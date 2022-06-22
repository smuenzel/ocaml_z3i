open Core

open Z3i_intf

module S = S

module Make_raw(With_sort : With_sort) = struct
  type raw = With_sort.raw
  type 's t = 's With_sort.t
  type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

  let to_raw : _ t -> raw = Obj.magic
  let unsafe_of_raw : raw -> _ t = Obj.magic

  let to_raw_list : _ t list -> raw list = Obj.magic
  let unsafe_of_raw_list : raw list -> _ t list = Obj.magic

  let pack : _ t -> packed = Obj.magic
  let pack_list : _ t list -> packed list = Obj.magic

  let to_raw_unpack_list : packed list -> raw list = Obj.magic
end

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

  include Make_raw(Types.Expr)

  let sexp_of_t _ t = [%sexp_of: string] (Expr.to_string t)

  module Native = struct
    let to_native = (Obj.magic : _ t -> Z3native.ast)
    let unsafe_of_native = (Obj.magic : Z3native.ast -> _ t)
  end

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

  let sort (type s) (t : s t) : s Sort.t =
    ZExpr.get_sort (to_raw t)
    |> Sort.unsafe_of_raw

  let sort_kind t = Sort.sort_kind (sort t)

  let is_numeral = (ZExpr.is_numeral : raw -> bool :> _ t -> bool)

  let to_string = (ZExpr.to_string : raw -> string :> _ t -> string)

  let numeral_to_binary_string_exn t =
    if not (Expr.is_numeral t)
    then raise_s [%message "not a numeral" (t : _ t)];
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

  let const (type s) symbol (sort : s Sort.t) : s t =
    ZExpr.mk_const (Sort.context sort) symbol (sort : _ Sort.t :> Z3.Sort.sort)
    |> unsafe_of_raw

  let const_s (type s) symbol (sort : s Sort.t) : s t =
    ZExpr.mk_const_s (Sort.context sort) symbol (sort : _ Sort.t :> Z3.Sort.sort)
    |> unsafe_of_raw

  let const_i (type s) symbol (sort : s Sort.t) : s t=
    let context = Sort.context sort in
    ZExpr.mk_const context (Symbol.of_int context symbol) (sort : _ Sort.t :> Z3.Sort.sort)
    |> unsafe_of_raw

  let numeral_int (type s) int (sort : s Sort.t) : s t =
    let context = Sort.context sort in
    ZExpr.mk_numeral_int context int (sort : _ Sort.t :> Z3.Sort.sort)
    |> unsafe_of_raw
end

and Sort : Sort
  with module Types := Types
= struct
  module ZSort = Z3.Sort

  include Make_raw(Types.Sort)

  let sexp_of_t _ t =
    [%sexp_of: string] (ZSort.to_string (to_raw t))

  let create_bitvector ctx ~bits : S.bv t =
    Z3.BitVector.mk_sort ctx bits
    |> unsafe_of_raw

  module Native = struct
    let to_native = (Obj.magic : _ t -> Z3native.sort)
    let unsafe_of_native = (Obj.magic : Z3native.sort -> _ t)
  end

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

  let same (type a b) (a : a t) (b : b t) : (a,b) Type_equal.t option =
    if ZSort.equal (to_raw a) (to_raw b)
    then Obj.magic (Some Type_equal.T)
    else None

  let sort_kind (type s) (t : s t) : s S.kind =
    match ZSort.get_sort_kind (to_raw t) with
    | UNINTERPRETED_SORT -> Obj.magic S.Uninterpreted
    | BOOL_SORT -> Obj.magic S.Bool
    | INT_SORT -> Obj.magic S.Int
    | REAL_SORT -> Obj.magic S.Real
    | BV_SORT -> Obj.magic S.Bv
    | ARRAY_SORT -> Obj.magic S.Array
    | DATATYPE_SORT -> Obj.magic S.Datatype
    | RELATION_SORT -> Obj.magic S.Relation
    | FINITE_DOMAIN_SORT -> Obj.magic S.Finite_domain
    | FLOATING_POINT_SORT -> Obj.magic S.Floating_point
    | ROUNDING_MODE_SORT -> Obj.magic S.Rounding_mode
    | SEQ_SORT -> Obj.magic S.Seq
    | RE_SORT -> Obj.magic S.Re
    | CHAR_SORT -> Obj.magic S.Char
    | UNKNOWN_SORT -> Obj.magic S.Unknown

  let same_kind (type a b) (a : a t) (b : b t) : (a,b) Type_equal.t option =
    match sort_kind a, sort_kind b with
    | S.Uninterpreted, S.Uninterpreted -> Some T
    | S.Bool, S.Bool -> Some T
    | S.Int, S.Int -> Some T
    | S.Real, S.Real -> Some T
    | S.Bv, S.Bv -> Some T
    | S.Array, S.Array -> Some T
    | S.Datatype, S.Datatype -> Some T
    | S.Relation, S.Relation -> Some T
    | S.Finite_domain, S.Finite_domain -> Some T
    | S.Floating_point, S.Floating_point -> Some T
    | S.Rounding_mode, S.Rounding_mode -> Some T
    | S.Seq, S.Seq -> Some T
    | S.Re, S.Re -> Some T
    | S.Char, S.Char -> Some T
    | S.Unknown, S.Unknown -> Some T
    | _, _ -> None
end

and Wrap : sig
  module type T = sig
    type 'a wrap

    type 's ternary =
      { f : 'z . ('s Expr.t -> 'z Expr.t -> 'z Expr.t -> 'z Expr.t) wrap
      } [@@unboxed]

    val list : (Context.t -> Expr.raw list -> Expr.raw) -> ('s Expr.t list -> 's Expr.t) wrap
    val binary : (Context.t -> Expr.raw -> Expr.raw -> Expr.raw) -> (_ Expr.t -> _ Expr.t -> _ Expr.t) wrap
    val ternary : (Context.t -> Expr.raw -> Expr.raw -> Expr.raw -> Expr.raw) -> _ ternary
    val unary : (Context.t -> Expr.raw -> Expr.raw) -> (_ Expr.t -> _ Expr.t) wrap
  end

  include T with type 'a wrap = 'a

  module Noop : T with type 'a wrap = Context.t -> 'a
end =  struct
  module type T = Wrap.T

  type 'a wrap = 'a

  type 's ternary =
    { f : 'z . ('s Expr.t -> 'z Expr.t -> 'z Expr.t -> 'z Expr.t) wrap
    } [@@unboxed]

  let list f = fun expr_list ->
    match expr_list with
    | [] -> raise_s [%message "empty list"]
    | (expr :: _) as list ->
      f
        (Expr.context expr)
        (Expr.to_raw_list list)
      |> Expr.unsafe_of_raw

  let unary f = fun a -> f (Expr.context a) (Expr.to_raw a) |> Expr.unsafe_of_raw
  let binary f = fun a b -> f (Expr.context a) (Expr.to_raw a) (Expr.to_raw b) |> Expr.unsafe_of_raw
  let ternary f = 
    { f =
        fun a b c ->
          f (Expr.context a) (Expr.to_raw a) (Expr.to_raw b) (Expr.to_raw c) |> Expr.unsafe_of_raw
    }

  module Noop = struct
    type 'a wrap = Context.t -> 'a

    type 's ternary =
      { f : 'z . ('s Expr.t -> 'z Expr.t -> 'z Expr.t -> 'z Expr.t) wrap
      } [@@unboxed]

    let list f = Obj.magic f
    let unary f = Obj.magic f
    let binary f = Obj.magic f
    let ternary f = Obj.magic f
  end
end

and Bitvector : Bitvector
  with module Types := Types
= struct
  module ZBitvector = Z3.BitVector

  type t = S.bv Expr.t

  let is_bv (type s) (e : s Expr.t) : (s, S.bv) Type_equal.t option =
    match Expr.sort_kind e with
    | S.Bv -> Some T
    | _ -> None

  let size sort = ZBitvector.get_size (sort : _ Sort.t :> Z3.Sort.sort)
  let size_e e = size (Expr.sort e)

  let of_boolean e =
    let ctx = Expr.context e in
    Boolean.With_context.ite
      ctx
      e
      (Bitvector.Numeral.bit1 ctx)
      (Bitvector.Numeral.bit0 ctx)

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
  let mul = Wrap.binary ZBitvector.mk_mul

  let shift_left t ~count =
    ZBitvector.mk_shl (Expr.context t) (Expr.to_raw count) (Expr.to_raw count)
    |> Expr.unsafe_of_raw

  let is_add_overflow ~signed a b : Boolean.t =
    let ctx = Expr.context a in
    let a = (a : _ Expr.t :> Z3.Expr.expr) in
    let b = (b : _ Expr.t :> Z3.Expr.expr) in
    Expr.unsafe_of_raw (ZBitvector.mk_add_no_overflow ctx a b signed)
    |> Boolean.not

  let concat = Wrap.binary ZBitvector.mk_concat

  let repeat expr ~count =
    ZBitvector.mk_repeat (Expr.context expr) count (Expr.to_raw expr)
    |> Expr.unsafe_of_raw

  let broadcast_single single_bit target =
    let target_size = size target in
    assert (size_e single_bit = 1);
    repeat single_bit ~count:target_size

  let extract expr ~high ~low : t =
    ZBitvector.mk_extract (Expr.context expr) high low (Expr.to_raw expr)
    |> Expr.unsafe_of_raw

  let extract_single expr bit : t =
    extract expr ~high:bit ~low:bit

  let zero_extend expr ~extra_zeros =
    ZBitvector.mk_zero_ext (Expr.context expr) extra_zeros (Expr.to_raw expr)
    |> Expr.unsafe_of_raw

  let sign_extend expr ~extra_bits =
    ZBitvector.mk_sign_ext (Expr.context expr) extra_bits (Expr.to_raw expr)
    |> Expr.unsafe_of_raw

  let rotate_left_const expr n =
    ZBitvector.mk_rotate_left (Expr.context expr) n (Expr.to_raw expr)
    |> Expr.unsafe_of_raw

  let popcount ?result_bit_size expr =
    let size = size_e expr in
    let result_bit_size = Option.value ~default:size result_bit_size in
    let context = Expr.context expr in
    let acc_bit_size = Int.ceil_log2 size in
    assert (acc_bit_size <= result_bit_size);
    List.init size
      ~f:(fun i ->
          (* CR smuenzel: With context *)
          ZBitvector.mk_extract context i i (Expr.to_raw expr)
          |> ZBitvector.mk_zero_ext context (acc_bit_size - 1)
        )
    |> List.reduce_balanced_exn
      ~f:(ZBitvector.mk_add context)
    |> ZBitvector.mk_zero_ext context (result_bit_size - acc_bit_size)
    |> Expr.unsafe_of_raw

  let is_power_of_two_or_zero e =
    Bitvector.and_ e (Bitvector.sub e (Bitvector.Numeral.int_e e 1))
    |> Bitvector.is_zero

  let is_power_of_two e : S.bool Expr.t =
    Boolean.and_
     [ is_power_of_two_or_zero e
     ; Boolean.not (Bitvector.is_zero e)
     ]

  let is_zero e : S.bool Expr.t =
    Boolean.eq e (Bitvector.Numeral.int_e e 0)

  let sign a : t =
    extract_single a (size_e a - 1)

  let parity a =
    let size = size_e a in
    (* No mk_redxor *)
    List.init size ~f:(fun i -> extract_single a i)
    |> List.reduce_balanced_exn ~f:xor


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

    (* CR smuenzel: figure out how to make OP_BIT0 *)
    let bit0 ctx =
      bool ctx [ false ]

    let bit1 ctx =
      bool ctx [ true ]

    let bit0_e e = bit0 (Expr.context e)
    let bit1_e e = bit1 (Expr.context e)

    let int sort i =
      Z3.Expr.mk_numeral_int (Sort.context sort) i (Sort.to_raw sort)
      |> Expr.unsafe_of_raw

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
  let sexp_of_t t = Sexp.List (Sexp.of_string_many (to_string t))

  let eval (type s) t (expr : s Expr.t) ~apply_model_completion =
    ZModel.eval t (Expr.to_raw expr) apply_model_completion
    |> (Obj.magic : Expr.raw option -> s Expr.t option)

  let const_interp_e (type s) t (expr : s Expr.t) =
    ZModel.get_const_interp_e t (Expr.to_raw expr)
    |> (Obj.magic : Expr.raw option -> s Expr.t option)

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

  let add_list t exprs =
    ZSolver.add t (Expr.to_raw_list exprs)

  let add t expr = add_list t [ expr ]

  let check_and_get_model t exprs : _ Solver_result.t =
    match ZSolver.check t (Expr.to_raw_list exprs) with
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

  let add_list t list =
    ZOptimize.add t (Expr.to_raw_list list)

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
    type nonrec _ t =
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

  let of_string = Z3.Symbol.mk_string

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.symbol)
    let unsafe_of_native = (Obj.magic : Z3native.symbol -> t)
  end
end

and Boolean : Boolean
  with module Types := Types
= struct

  module ZBoolean = Z3.Boolean

  module Ops(Wrap : Wrap.T) = struct
    type t = S.bool Expr.t

    let and_ = Wrap.list ZBoolean.mk_and
    let or_ = Wrap.list ZBoolean.mk_or
    let not = Wrap.unary ZBoolean.mk_not
    let xor = Wrap.binary ZBoolean.mk_xor

    let iff = Wrap.binary ZBoolean.mk_iff
    let implies = Wrap.binary ZBoolean.mk_implies

    (*
    let eq = Wrap.binary ZBoolean.mk_eq
    let neq = Wrap.binary (fun ctx a b -> ZBoolean.mk_not ctx (ZBoolean.mk_eq ctx a b))
       *)

  end

  include Ops(Wrap)

  let is_bool (type s) (e : s Expr.t) : (s, S.bool) Type_equal.t option =
    match Expr.sort_kind e with
    | S.Bool -> Some T
    | _ -> None

  let eq a b =
    ZBoolean.mk_eq (Expr.context a) (Expr.to_raw a) (Expr.to_raw b)
    |> Expr.unsafe_of_raw

  let neq a b = not (eq a b)

  let ite a b c =
    (Wrap.ternary ZBoolean.mk_ite).f a b c

  module With_context = struct 
    include Ops(Wrap.Noop)

    let ite ctx a b c =
      (Wrap.Noop.ternary ZBoolean.mk_ite).f ctx a b c

    let eq ctx a b =
      ZBoolean.mk_eq ctx (Expr.to_raw a) (Expr.to_raw b)
      |> Expr.unsafe_of_raw

    let neq c a b = not c (eq c a b)
  end

  module Numeral = struct
    let false_ ctx = ZBoolean.mk_false ctx |> Expr.unsafe_of_raw
    let true_ ctx = ZBoolean.mk_true ctx |> Expr.unsafe_of_raw

    let bool ctx bool = ZBoolean.mk_val ctx bool |> Expr.unsafe_of_raw
  end
end

and Quantifier : Quantifier
  with module Types := Types
= struct

  module ZQuantifier = Z3.Quantifier

  include Make_raw(Types.Quantifier)

  let of_expr (type s) (e : s Expr.t) : s t =
    ZQuantifier.quantifier_of_expr (Expr.to_raw e)
    |> unsafe_of_raw

  let to_expr (type s) (t : s t) : s Expr.t =
    ZQuantifier.expr_of_quantifier (to_raw t)
    |> Expr.unsafe_of_raw

  let quantifier
      (type s)
      (kind : [ `forall| `exists ])
      ?weight
      ?quantifier_id
      ?skolem_id
      ?(patterns = [])
      ?(nopatterns = [])
      variables
      ~body
    : s t
    =
    let Sort.T head_sort, _ = List.hd_exn variables in
    let sorts, symbols = List.unzip variables in
    let ctx = Sort.context head_sort in
    ZQuantifier.mk_quantifier
      ctx
      (match kind with `forall -> true | `exists -> false)
      (Sort.to_raw_unpack_list sorts)
      symbols
      (Expr.to_raw body)
      weight
      (Pattern.to_raw_unpack_list patterns)
      (Expr.to_raw_unpack_list nopatterns)
      quantifier_id
      skolem_id
    |> unsafe_of_raw

  let quantifier_const
      (type s)
      (kind : [ `forall| `exists ])
      ?weight
      ?quantifier_id
      ?skolem_id
      ?(patterns = [])
      ?(nopatterns = [])
      variables
      ~body
    : s t
    =
    let Expr.T head_expr = List.hd_exn variables in
    let ctx = Expr.context head_expr in
    ZQuantifier.mk_quantifier_const
      ctx
      (match kind with `forall -> true | `exists -> false)
      (Expr.to_raw_unpack_list variables)
      (Expr.to_raw body)
      weight
      (Pattern.to_raw_unpack_list patterns)
      (Expr.to_raw_unpack_list nopatterns)
      quantifier_id
      skolem_id
    |> unsafe_of_raw

  let forall
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body
    =
    quantifier `forall
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body

  let forall_const
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body
    =
    quantifier_const `forall
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body

  let exists
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body
    =
    quantifier `exists
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body

  let exists_const
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body
    =
    quantifier_const `exists
      ?weight
      ?quantifier_id
      ?skolem_id
      ?patterns
      ?nopatterns
      variables
      ~body
end

and Pattern : Pattern
  with module Types := Types
= struct
  include Make_raw(Types.Pattern)

  let sexp_of_t _ t =
    [%sexp_of:string] (Z3.Quantifier.Pattern.to_string (to_raw t))

  let create exprs =
    Z3.Quantifier.mk_pattern
      (Expr.context (List.hd_exn exprs))
      (Expr.to_raw_list exprs)
    |> unsafe_of_raw

  module Native = struct
    let to_native = (Obj.magic : _ t -> Z3native.pattern)
    let unsafe_of_native = (Obj.magic : Z3native.pattern -> _ t)
  end
end
