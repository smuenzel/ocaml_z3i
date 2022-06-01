open Core

open Z3i_intf

module rec Context : Context 
  with module Types := Types
= struct
  type t = Types.Context.t

  let create ?(model = true) ?(proof = false) () =
    Z3.mk_context 
      [ "model", Bool.to_string model
      ; "proof", Bool.to_string proof 
      ]

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.context)
    let unsafe_of_native = (Obj.magic : Z3native.context -> t)
  end
end

and Expr : Expr
  with module Types := Types
= struct

  module ZExpr = Z3.Expr
  type t = Types.Expr.t

  let sexp_of_t t = [%sexp_of: string] (Expr.to_string t)

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.ast)
    let unsafe_of_native = (Obj.magic : Z3native.ast -> t)
  end

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

  let sort = ZExpr.get_sort

  let is_numeral = ZExpr.is_numeral

  let to_string = ZExpr.to_string

  let numeral_to_binary_string_exn t =
    if not (Expr.is_numeral t)
    then raise_s [%message "not a numeral" (t : t)];
    if not (Bitvector.is_bv t)
    then raise_s [%message "not a bitvector" (t : t)];
    let length = Bitvector.size (Expr.sort t) in
    let ctx = context t in
    let short_string =
      Z3native.get_numeral_binary_string
        (Context.Native.to_native ctx)
        (Expr.Native.to_native t)
    in
    String.init (length - String.length short_string) ~f:(Fn.const '0')
    ^ short_string

  let numeral_to_binary_array_exn t =
    numeral_to_binary_string_exn t
    |> String.to_array
    |> Array.map ~f:(Char.(=) '1')

  let const symbol sort = ZExpr.mk_const (Sort.context sort) symbol sort
  let const_s symbol sort = ZExpr.mk_const_s (Sort.context sort) symbol sort
  let const_i symbol sort =
    let context = Sort.context sort in
    ZExpr.mk_const context (Symbol.of_int context symbol) sort

  let mk_const = ZExpr.mk_const
  let mk_numeral_int = ZExpr.mk_numeral_int
end

and Sort : Sort
  with module Types := Types
= struct
  module Sort = Z3.Sort
  type t = Types.Sort.t

  let create_bitvector ctx ~bits =
    Z3.BitVector.mk_sort ctx bits

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.sort)
    let unsafe_of_native = (Obj.magic : Z3native.sort -> t)
  end

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

end

and Wrap : sig
  (* val list : (Context.t -> Expr.t list -> 'a) -> (Expr.t list -> 'a) *)
  val binary : (Context.t -> Expr.t -> Expr.t -> 'a) -> (Expr.t -> Expr.t -> 'a)
  val unary : (Context.t -> Expr.t -> 'a) -> (Expr.t -> 'a)
end =  struct
  (*
  let list f = fun expr_list ->
    match expr_list with
    | [] -> raise_s [%message "empty list"]
    | (expr :: _) as list ->
      f
        (Expr.context expr)
        list
     *)

  let unary f = fun a -> f (Expr.context a) a
  let binary f = fun a b -> f (Expr.context a) a b
end

and Bitvector : Bitvector
  with module Types := Types
= struct
  module ZBitvector = Z3.BitVector

  let is_bv = ZBitvector.is_bv
  let size = ZBitvector.get_size
  let size_e e = size (Expr.sort e)

  let and_ = Wrap.binary ZBitvector.mk_and
  let or_ = Wrap.binary ZBitvector.mk_or
  let xor = Wrap.binary ZBitvector.mk_xor
  let nand = Wrap.binary ZBitvector.mk_nand
  let nor = Wrap.binary ZBitvector.mk_nor
  let xnor = Wrap.binary ZBitvector.mk_xnor
  let not = Wrap.unary ZBitvector.mk_not

  let neg = Wrap.unary ZBitvector.mk_neg
  let add = Wrap.binary ZBitvector.mk_add
  let sub = Wrap.binary ZBitvector.mk_sub

  let concat = Wrap.binary ZBitvector.mk_concat
  let repeat expr ~count = ZBitvector.mk_repeat (Expr.context expr) count expr
  let extract expr ~high ~low = ZBitvector.mk_extract (Expr.context expr) high low expr
  let extract_single expr bit = extract expr ~high:bit ~low:bit
  let zero_extend expr ~extra_zeros = ZBitvector.mk_zero_ext (Expr.context expr) extra_zeros expr
  let sign_extend expr ~extra_bits = ZBitvector.mk_sign_ext (Expr.context expr) extra_bits expr

  let rotate_left_const expr n =
    ZBitvector.mk_rotate_left (Expr.context expr) n expr

  let popcount ?result_bit_size expr =
    let size = size_e expr in
    let result_bit_size = Option.value ~default:size result_bit_size in
    let context = Expr.context expr in
    let acc_bit_size = Int.ceil_log2 size in
    assert (acc_bit_size <= result_bit_size);
    List.init size
      ~f:(fun i ->
          (* CR smuenzel: With context *)
          ZBitvector.mk_extract context i i expr
          |> ZBitvector.mk_zero_ext context (acc_bit_size - 1)
        )
    |> List.reduce_balanced_exn
      ~f:(ZBitvector.mk_add context)
    |> ZBitvector.mk_zero_ext context (result_bit_size - acc_bit_size)

  let is_power_of_two_or_zero e =
    Bitvector.and_ e (Bitvector.sub e (Bitvector.Numeral.int_e e 1))
    |> Bitvector.is_zero

  let is_power_of_two e =
    Boolean.and_
     [ is_power_of_two_or_zero e
     ; Boolean.not (Bitvector.is_zero e)
     ]

  let is_zero e =
    Boolean.eq e (Bitvector.Numeral.int_e e 0)

  module Set = struct
    let const_empty ctx bits =
      Bitvector.Numeral.int
        (Sort.create_bitvector ctx ~bits)
        0

    let union = or_
    let inter = and_
    let complement = not
    let diff a b = inter a (complement b)
    let symmdiff = xor

    let is_empty = is_zero
    let is_subset a ~of_ = is_empty (diff a of_)

    let has_max_one_member = is_power_of_two_or_zero
    let has_single_member = is_power_of_two

  end

  module Numeral = struct
    let bool ctx bools =
      let length = List.length bools in
      Z3native.mk_bv_numeral
        (Context.Native.to_native ctx)
        length
        bools
      |> Expr.Native.unsafe_of_native

    let int sort i =
      Z3.Expr.mk_numeral_int (Sort.context sort) i sort

    let int_e expr i =
      int (Expr.sort expr) i
  end

end

and Model : Model
  with module Types := Types
= struct
  type t = Types.Model.t

  module ZModel = Z3.Model

  let to_string t = ZModel.to_string t
  let sexp_of_t t = Sexp.of_string (to_string t)

  let eval t expr ~apply_model_completion = ZModel.eval t expr apply_model_completion

  let const_interp_e = ZModel.get_const_interp_e

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.model)
    let unsafe_of_native = (Obj.magic : Z3native.model -> t)
  end

end

and Solver_result : Solver_result
  with module Types := Types
   and type 'a t = 'a Types.Solver_result.t
= struct

  type 'a t = 'a Types.Solver_result.t =
    | Unsatisfiable
    | Unknown of string
    | Satisfiable of 'a
  [@@deriving sexp]

  let satisfiable_exn = function
    | Unsatisfiable
    | Unknown _  as state -> raise_s [%message "not sat" (state : _ t)]
    | Satisfiable model -> model

  let map t ~f =
    match t with
    | Unsatisfiable -> Unsatisfiable
    | Unknown s -> Unknown s
    | Satisfiable x -> Satisfiable (f x)

end

and Solver : Solver
  with module Types := Types
= struct
  type t = Types.Solver.t

  let context t =
    Z3native.context_of_solver (Solver.Native.to_native t)
    |> Context.Native.unsafe_of_native

  module ZSolver = Z3.Solver

  let create context = ZSolver.mk_solver context None

  let to_string = ZSolver.to_string

  let add_list = ZSolver.add
  let add t expr = add_list t [ expr ]

  let check_and_get_model t exprs : _ Solver_result.t =
    match ZSolver.check t exprs with
    | UNSATISFIABLE -> Unsatisfiable
    | UNKNOWN -> Unknown (ZSolver.get_reason_unknown t)
    | SATISFIABLE ->
      let model = Option.value_exn (ZSolver.get_model t) in
      Satisfiable model

  let check_current_and_get_model t = check_and_get_model t []

  let push = ZSolver.push
  let pop = ZSolver.pop

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.solver)
    let unsafe_of_native = (Obj.magic : Z3native.solver -> t)
  end
end

and Optimize : Optimize
  with module Types := Types
= struct
  type t = Types.Optimize.t

  let context t =
    Z3native.context_of_optimize (Optimize.Native.to_native t)
    |> Context.Native.unsafe_of_native

  module ZOptimize = Z3.Optimize

  let create context = ZOptimize.mk_opt context

  let to_string t = ZOptimize.to_string t

  let add_list t list = ZOptimize.add t list
  let add t expr = add_list t [ expr ]

  let check_current_and_get_model t : _ Solver_result.t =
    match ZOptimize.check t with
    | UNSATISFIABLE -> Unsatisfiable
    | UNKNOWN -> Unknown (ZOptimize.get_reason_unknown t)
    | SATISFIABLE ->
      let model = Option.value_exn (ZOptimize.get_model t) in
      Satisfiable model

  let check_and_get_model t exprs : _ Solver_result.t =
    Optimize.push t;
    Optimize.add_list t exprs;
    let result = Optimize.check_current_and_get_model t in
    Optimize.pop t 1;
    result

  let push t = ZOptimize.push t
  let pop t scopes = for _ = 1 to scopes do ZOptimize.pop t done

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.optimize)
    let unsafe_of_native = (Obj.magic : Z3native.optimize -> t)
  end

  module Goal = struct
    type nonrec t =
      { optimize : t
      ; index : int
      }

    let lower t =
      Z3native.optimize_get_lower
        (Context.Native.to_native (context t.optimize)) 
        (Native.to_native t.optimize)
        t.index
      |> Expr.Native.unsafe_of_native

    let upper t =
      Z3native.optimize_get_upper
        (Context.Native.to_native (context t.optimize)) 
        (Native.to_native t.optimize)
        t.index
      |> Expr.Native.unsafe_of_native
  end

  let add_soft t expr ~weight symbol =
    let index =
      Z3native.optimize_assert_soft
        (context t |> Context.Native.to_native)
        (Optimize.Native.to_native t)
        (Expr.Native.to_native expr)
        (Int.to_string weight)
        (Symbol.Native.to_native symbol)
    in
    { Goal.
      optimize = t
    ; index
    }

end

and Symbol : Symbol
  with module Types := Types
= struct
  type t = Types.Symbol.t

  let of_int = Z3.Symbol.mk_int

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.symbol)
    let unsafe_of_native = (Obj.magic : Z3native.symbol -> t)
  end
end

and Boolean : Boolean
  with module Types := Types
= struct

  module ZBoolean = Z3.Boolean

  module With_context =  struct
    let and_ = ZBoolean.mk_and
    let not = ZBoolean.mk_not

    let eq = ZBoolean.mk_eq
    let neq ctx a b = not ctx (eq ctx a b)

    let wrap_unary f = fun a -> f (Expr.context a) a
    let wrap_binary f = fun a b -> f (Expr.context a) a b

    let wrap_list f = fun expr_list ->
      match expr_list with
      | [] -> raise_s [%message "empty list"]
      | (expr :: _) as list ->
        f
          (Expr.context expr)
          list
  end

  let and_ = With_context.(wrap_list and_)
  let not = With_context.(wrap_unary not)
  let eq = With_context.(wrap_binary eq)
  let neq = With_context.(wrap_binary neq)
end
