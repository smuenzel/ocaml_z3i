(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2022-2025 Stefan Muenzel
 *)

open Core

open Z3i_intf

module S = S

module rec Context : Context 
= struct
  type t = Types.Context.t

   (*
  - proof  (Boolean)           Enable proof generation
  - debug_ref_count (Boolean)  Enable debug support for Z3_ast reference counting
  - trace  (Boolean)           Tracing support for VCC
  - trace_file_name (String)   Trace out file for VCC traces
  - timeout (unsigned)         default timeout (in milliseconds) used for solvers
  - well_sorted_check          type checker
  - auto_config                use heuristics to automatically select solver and configure it
  - model                      model generation for solvers, this parameter can be overwritten when creating a solver
  - model_validate             validate models produced by solvers
  - unsat_core                 unsat-core generation for solvers, this parameter can be overwritten when creating a solver
  - encoding                   the string encoding used internally (must be either "unicode" - 18 bit, "bmp" - 16 bit or "ascii" - 8 bit)
      *)

  let create
      ?(parallel = true)
      ?(model = true)
      ?(proof = false)
      ()
    =
    Z3.set_global_param "parallel.enable" (Bool.to_string parallel);
    Z3.mk_context 
      [ "model", Bool.to_string model
      ; "proof", Bool.to_string proof 
      ]

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.context)
    let to_native_list = (Obj.magic : t list -> Z3native.context list)
    let unsafe_of_native = (Obj.magic : Z3native.context -> t)
  end
end

and Ast : Ast
= struct
  module ZAst = Z3.AST

  type t = ZAst.ast

  module Kind = struct
    type t = Z3enums.ast_kind =
      | NUMERAL_AST
      | APP_AST
      | VAR_AST
      | QUANTIFIER_AST
      | SORT_AST
      | FUNC_DECL_AST
      | UNKNOWN_AST
    [@@deriving sexp]
  end

  let kind t = ZAst.get_ast_kind t

end

and Expr : Expr
= struct

  module ZExpr = Z3.Expr

  include Types.Expr

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

  let translate t context =
    Z3.Expr.translate (to_raw t) context |> unsafe_of_raw

  let sort (type s) (t : s t) : s Sort.t =
    ZExpr.get_sort (to_raw t)
    |> Sort.unsafe_of_raw

  let sort_kind t = Sort.sort_kind (sort t)

  let is_kind_exn (type a b) (t : a t) (sk : b Sort.Kind.t) : (a, b) Type_equal.t =
    match Sort.Kind.same (sort_kind t) sk with
    | None -> assert false
    | Some T -> T

  let is_numeral = (ZExpr.is_numeral : raw -> bool :> _ t -> bool)

  let to_string = (ZExpr.to_string : raw -> string :> _ t -> string)

  let to_ast t = ZExpr.ast_of_expr (to_raw t)

  let sexp_of_t _ t = [%sexp_of: string] (to_string t)

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

  let simplify e = 
    let context = context e in
    let params = Z3.Params.mk_params context in
    Z3.Params.add_bool params (Symbol.of_string context "bit-blast") true;
    ZExpr.simplify (to_raw e) (Some params)
    |> unsafe_of_raw

  (*
  let get_func_decl e =
    ZExpr.get_func_decl (to_raw e)
     *)

end

and Sort : sig
  include Sort

  val func_decl_domain : (_,_) Function_declaration.t -> S.packed_lambda_instance
  val func_decl_range : (_,'range) Function_declaration.t -> 'range Kind.t
end
= struct
  module ZSort = Z3.Sort

  include Types.Sort

  let sexp_of_t _ t =
    [%sexp_of: string] (ZSort.to_string (to_raw t))

  let tuple_elements s =
    let raw = to_raw s in
    Z3.Tuple.get_field_decls raw
    |> Base.List.map ~f:Z3.FuncDecl.get_range
    |> unsafe_of_raw_list
    |> pack_list

  let context t =
    Z3native.context_of_ast (Native.to_native t)
    |> Context.Native.unsafe_of_native

  module Kind = struct
    type 's t = 's S.kind

    let rec sexp_of_t : 'a 's . 'a -> 's t -> Sexp.t =
      fun (type s) _ (t : s t) ->
      match t with
      | Uninterpreted -> [%message "Uninterpreted"]
      | Bool -> [%message "Bool"]
      | Int -> [%message "Int"]
      | Real -> [%message "Real"]
      | Bv -> [%message "Bv"]
      | Datatype k -> [%message "Datatype" ~_:(k : _ datatype_kind)]
      | Relation -> [%message "Relation"]
      | Finite_domain -> [%message "Finite_domain"]
      | Floating_point -> [%message "Floating_point"]
      | Rounding_mode -> [%message "Rounding_mode"]
      | Seq -> [%message "Seq"]
      | Re -> [%message "Re"]
      | Char -> [%message "Char"]
      | Unknown -> [%message "Unknown"]
      | Array (lambda_instance, range) ->
        let domain = sexp_of_lambda_instance lambda_instance in
        [%message "Array"
            (domain : Sexp.t list)
            (range : _ t)
        ]
    and sexp_of_lambda_instance : 'a . 'a S.lambda_instance -> Sexp.t list =
      fun (type a) (i : a S.lambda_instance) ->
      match i with
      | [] -> []
      | x :: xs ->
        (sexp_of_t () x)
        :: sexp_of_lambda_instance xs
    and sexp_of_datatype_kind : 'a . _ -> 'a S.datatype_kind -> Sexp.t =
      fun (type a) _ (dt : a S.datatype_kind) ->
      match dt with
      | Tuple i ->
        let i = sexp_of_tuple_instance () i in
        [%message "Tuple" ~_:(i : Sexp.t list)]
      | Other -> [%message "Other"]
    and sexp_of_tuple_instance : 'a . _ -> 'a S.tuple_instance -> Sexp.t list =
      fun (type a) _ (a : a S.tuple_instance) ->
      match a with
      | [] -> []
      | x :: xs ->
        (sexp_of_t () x)
        :: sexp_of_tuple_instance () xs

    type 'a lambda_instance = 'a S.lambda_instance

    let sexp_of_lambda_instance _ li =
      Sexp.List (sexp_of_lambda_instance li)

    let rec kind_list_to_lambda_instance (list : S.packed_kind list) : S.packed_lambda_instance =
      match list with
      | [] -> S.A []
      | K x :: xs -> 
        let S.A xs = kind_list_to_lambda_instance xs in
        S.A (x :: xs)

    let rec kind_list_to_tuple_instance (list : S.packed_kind list) : S.packed_tuple_instance =
      match list with
      | [] -> S.TP []
      | K x :: xs -> 
        let S.TP xs = kind_list_to_tuple_instance xs in
        S.TP (x :: xs)

    let rec same_kind_internal : 'a 'b . 'a S.kind -> 'b S.kind -> ('a, 'b) Type_equal.t option =
      fun (type a b) (a : a S.kind) (b : b S.kind) : (a,b) Type_equal.t option ->
      match a, b with
      | S.Uninterpreted, S.Uninterpreted -> Some T
      | S.Bool, S.Bool -> Some T
      | S.Int, S.Int -> Some T
      | S.Real, S.Real -> Some T
      | S.Bv, S.Bv -> Some T
      | S.Array (a0,a1), S.Array (b0, b1) ->
        begin match same_kind_internal a1 b1 with
          | None -> None
          | Some T ->
            match same_array_kind_internal a0 b0 with
            | None -> None
            | Some T -> Some T
        end
      | S.Datatype a, S.Datatype b ->
        begin match same_datatype_kind a b with
          | None -> None
          | Some T -> Some T
        end
      | S.Relation, S.Relation -> Some T
      | S.Finite_domain, S.Finite_domain -> Some T
      | S.Floating_point, S.Floating_point -> Some T
      | S.Rounding_mode, S.Rounding_mode -> Some T
      | S.Seq, S.Seq -> Some T
      | S.Re, S.Re -> Some T
      | S.Char, S.Char -> Some T
      | S.Unknown, S.Unknown -> Some T
      | _, _ -> None
    and same_datatype_kind
      : 'a 'b . 'a S.datatype_kind -> 'b S.datatype_kind -> ('a, 'b) Type_equal.t option =
      fun (type a b) (a : a S.datatype_kind) (b : b S.datatype_kind) : (a,b) Type_equal.t option ->
      match a, b with
      | Other, Other -> Some T
      | Tuple a, Tuple b ->
        begin match same_tuple_instance a b with
          | None -> None
          | Some T -> Some T
        end
      | _,_ -> None
    and same_array_kind_internal
      : 'a0 'b0 .
          'a0 S.lambda_instance
        -> 'b0 S.lambda_instance
        -> ('a0, 'b0) Type_equal.t option =
      fun (type a0 b0)
        (a : a0 S.lambda_instance)
        (b : b0 S.lambda_instance)
        ->
          match a, b with
          | [], [] -> Some (Type_equal.T : (a0, b0) Type_equal.t)
          | _ :: _, [] -> None
          | [], _ :: _ -> None
          | x :: xs, y :: ys ->
            match same_kind_internal x y with
            | None -> None
            | Some T ->
              match same_array_kind_internal xs ys with
              | None -> None
              | Some T -> Some T
    and same_tuple_instance
      : 'a 'b . 'a S.tuple_instance -> 'b S.tuple_instance -> ('a, 'b) Type_equal.t option =
      fun (type a b) (a : a S.tuple_instance) (b : b S.tuple_instance) : (a, b) Type_equal.t option ->
      match a, b with
      | [], [] -> Some T
      | [], _ :: _ -> None
      | _ :: _, [] -> None
      | x :: xs, y :: ys ->
        match same_kind_internal x y with
        | None -> None
        | Some T ->
          match same_tuple_instance xs ys with
          | None -> None
          | Some T -> Some T

    let same = same_kind_internal

    let same_lambda_instance =  same_array_kind_internal

  end

  let same (type a b) (a : a t) (b : b t) : (a,b) Type_equal.t option =
    if ZSort.equal (to_raw a) (to_raw b)
    then Obj.magic (Some Type_equal.T)
    else None

  let rec sort_kind : 's . 's t -> 's S.kind =
    fun (type s) (t : s t) : s S.kind ->
    match ZSort.get_sort_kind (to_raw t) with
    | UNINTERPRETED_SORT -> Obj.magic S.Uninterpreted
    | BOOL_SORT -> Obj.magic S.Bool
    | INT_SORT -> Obj.magic S.Int
    | REAL_SORT -> Obj.magic S.Real
    | BV_SORT -> Obj.magic S.Bv
    | ARRAY_SORT ->
      (* CR smuenzel: too much magic *)
      let t = (Obj.magic : s t -> _ S.array t) t in
      let domain =
        Sort.List.higher (ZArray.domain t) 
        |> Typed_list.Lambda_higher.to_list_map
          { f = fun x -> Sort.(T !< x) }
      in
      let range = ZArray.range t in
      let all_array_domains =
        Base.List.map domain
          ~f:(fun (T s) ->
              S.K
                (sort_kind ((Obj.magic : _ t -> _ t) s)))
      in
      let A all_array_domains =
        Kind.kind_list_to_lambda_instance all_array_domains
      in
      S.Array ( all_array_domains
              , (Obj.magic : _ Kind.t -> _ Kind.t) (sort_kind range)
              )
      |> (Obj.magic : _ S.array Kind.t -> _ Kind.t)
    | DATATYPE_SORT ->
      begin match Z3.Tuple.get_num_fields (to_raw t) with
        | _ -> 
          let elements = tuple_elements t in
          let TP tuple_instance =
            Base.List.map elements
              ~f:(fun (T x) -> S.K (sort_kind x))
            |> Kind.kind_list_to_tuple_instance
          in
          Obj.magic (S.Datatype (Tuple tuple_instance))
        | exception (Z3.Error _) -> Obj.magic (S.Datatype Other)
      end
    | RELATION_SORT -> Obj.magic S.Relation
    | FINITE_DOMAIN_SORT -> Obj.magic S.Finite_domain
    | FLOATING_POINT_SORT -> Obj.magic S.Floating_point
    | ROUNDING_MODE_SORT -> Obj.magic S.Rounding_mode
    | SEQ_SORT -> Obj.magic S.Seq
    | RE_SORT -> Obj.magic S.Re
    | CHAR_SORT -> Obj.magic S.Char
    | UNKNOWN_SORT -> Obj.magic S.Unknown
    | TYPE_VAR -> Obj.magic (S.Datatype Other)

  let func_decl_domain t =
    let d = Z3.FuncDecl.get_domain (Function_declaration.to_raw t) in
    let kind_list = Base.List.map d ~f:(fun s -> S.K (sort_kind (unsafe_of_raw s))) in
    Kind.kind_list_to_lambda_instance kind_list

  let func_decl_range t =
    Z3.FuncDecl.get_range (Function_declaration.to_raw t)
    |> unsafe_of_raw
    |> sort_kind

  let same_kind (type a b) (a : a t) (b : b t) : (a,b) Type_equal.t option =
    Kind.same_kind_internal (sort_kind a) (sort_kind b)

  let equal a b =
    ZSort.equal (to_raw a) (to_raw b)
end

and Wrap : sig
  module type T = sig
    type 'a wrap

    type 's ternary =
      { f : 'z . ('s Expr.t -> 'z Expr.t -> 'z Expr.t -> 'z Expr.t) wrap
      } [@@unboxed]

    val list : (Context.t -> Expr.raw list -> Expr.raw) -> ('s Expr.t list -> 's Expr.t) wrap
    val binary : (Context.t -> Expr.raw -> Expr.raw -> Expr.raw) -> (_ Expr.t -> _ Expr.t -> _ Expr.t) wrap
    val binary_expr : (Context.t -> 'a Expr.t -> 'b Expr.t -> 'c Expr.t) -> ('a Expr.t -> 'b Expr.t -> 'c Expr.t) wrap
    val ternary : (Context.t -> Expr.raw -> Expr.raw -> Expr.raw -> Expr.raw) -> _ ternary
    val unary : (Context.t -> Expr.raw -> Expr.raw) -> (_ Expr.t -> _ Expr.t) wrap
  end

  include T with type 'a wrap = 'a

  val context_of_list_exn : 's Expr.t list -> Context.t
  val context_of_array_exn : 's Expr.t array -> Context.t

  module Noop : T with type 'a wrap = Context.t -> 'a
end =  struct
  module type T = Wrap.T

  type 'a wrap = 'a

  type 's ternary =
    { f : 'z . ('s Expr.t -> 'z Expr.t -> 'z Expr.t -> 'z Expr.t) wrap
    } [@@unboxed]

  let context_of_list_exn expr_list =
    match expr_list with
    | [] -> raise_s [%message "empty list"]
    | (expr :: _) ->
      Expr.context expr

  let context_of_array_exn expr_array =
    match expr_array with
    | [||] -> raise_s [%message "empty array"]
    | _ ->
      Expr.context expr_array.(0)

  let list f = fun expr_list ->
      f
        (context_of_list_exn expr_list)
        (Expr.to_raw_list expr_list)
      |> Expr.unsafe_of_raw

  let unary f = fun a -> f (Expr.context a) (Expr.to_raw a) |> Expr.unsafe_of_raw
  let binary f = fun a b -> f (Expr.context a) (Expr.to_raw a) (Expr.to_raw b) |> Expr.unsafe_of_raw
  let binary_expr f = fun a b -> f (Expr.context a) a b
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
    let binary_expr f = f
    let ternary f = Obj.magic f
  end
end

and Bitvector : Bitvector
= struct
  module ZBitvector = Z3.BitVector

  type t = S.bv Expr.t

  let create_sort ctx ~bits : S.bv Sort.t =
    Z3.BitVector.mk_sort ctx bits
    |> Sort.unsafe_of_raw

  let is_bv (type s) (e : s Expr.t) : (s, S.bv) Type_equal.t option =
    match Expr.sort_kind e with
    | S.Bv -> Some T
    | _ -> None

  let size sort = ZBitvector.get_size (sort : _ Sort.t :> Z3.Sort.sort)
  let size_e e = size (Expr.sort e)

  let const s ~bits = Expr.const s (Bitvector.create_sort (Symbol.context s) ~bits)
  let const_s ctx s ~bits = Expr.const_s s (Bitvector.create_sort ctx ~bits)
  let const_i ctx i ~bits = Expr.const_i i (Bitvector.create_sort ctx ~bits)

  let of_boolean e =
    let ctx = Expr.context e in
    Boolean.With_context.ite
      ctx
      e
      (Bitvector.Numeral.bit1 ctx)
      (Bitvector.Numeral.bit0 ctx)

  let make_list f =
    Wrap.list
      (fun ctx list ->
         List.reduce_balanced_exn list ~f:(f ctx)
      )

  let and_ = Wrap.binary ZBitvector.mk_and
  let and_list = make_list ZBitvector.mk_and
  let or_ = Wrap.binary ZBitvector.mk_or
  let or_list = make_list ZBitvector.mk_or
  let xor = Wrap.binary ZBitvector.mk_xor
  let xor_list = make_list ZBitvector.mk_xor
  let nand = Wrap.binary ZBitvector.mk_nand
  let nor = Wrap.binary ZBitvector.mk_nor
  let xnor = Wrap.binary ZBitvector.mk_xnor
  let not = Wrap.unary ZBitvector.mk_not

  let neg = Wrap.unary ZBitvector.mk_neg
  let add = Wrap.binary ZBitvector.mk_add
  let sub = Wrap.binary ZBitvector.mk_sub
  let mul = Wrap.binary ZBitvector.mk_mul

  let shift_left t ~count =
    ZBitvector.mk_shl (Expr.context t) (Expr.to_raw t) (Expr.to_raw count)
    |> Expr.unsafe_of_raw

  let shift_right t ~count =
    ZBitvector.mk_lshr (Expr.context t) (Expr.to_raw t) (Expr.to_raw count)
    |> Expr.unsafe_of_raw

  let shift_arithmetic_right t ~count =
    ZBitvector.mk_ashr (Expr.context t) (Expr.to_raw t) (Expr.to_raw count)
    |> Expr.unsafe_of_raw

  let is_add_overflow ~signed a b : Boolean.t =
    let ctx = Expr.context a in
    Expr.unsafe_of_raw (ZBitvector.mk_add_no_overflow ctx (Expr.to_raw a) (Expr.to_raw b) signed)
    |> Boolean.not

  let is_add_underflow a b : Boolean.t =
    let ctx = Expr.context a in
    Expr.unsafe_of_raw (ZBitvector.mk_add_no_underflow ctx (Expr.to_raw a) (Expr.to_raw b))
    |> Boolean.not

  let is_sub_overflow a b : Boolean.t =
    let ctx = Expr.context a in
    Expr.unsafe_of_raw (ZBitvector.mk_sub_no_overflow ctx (Expr.to_raw a) (Expr.to_raw b))
    |> Boolean.not

  let is_sub_underflow ~signed a b : Boolean.t =
    let ctx = Expr.context a in
    Expr.unsafe_of_raw (ZBitvector.mk_sub_no_underflow ctx (Expr.to_raw a) (Expr.to_raw b) signed)
    |> Boolean.not

  let concat = Wrap.binary ZBitvector.mk_concat

  let concat_list =
    Wrap.list
      (fun ctx list ->
         List.reduce_balanced_exn list ~f:(ZBitvector.mk_concat ctx)
      )

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
    Boolean.and_list
     [ is_power_of_two_or_zero e
     ; Boolean.not (Bitvector.is_zero e)
     ]

  let is_zero e : S.bool Expr.t =
    Boolean.eq e (Bitvector.Numeral.int_e e 0)

  let is_not_zero e : S.bool Expr.t =
    Boolean.neq e (Bitvector.Numeral.int_e e 0)

  let is_all_ones e : S.bool Expr.t =
    is_zero (not e)

  let sign a : t =
    extract_single a (size_e a - 1)

  let parity a =
    let size = size_e a in
    (* No mk_redxor *)
    List.init size ~f:(fun i -> extract_single a i)
    |> List.reduce_balanced_exn ~f:xor

  module Signed = struct
    let (<) = Wrap.binary ZBitvector.mk_slt
    let (<=) = Wrap.binary ZBitvector.mk_sle
    let (>) = Wrap.binary ZBitvector.mk_sgt
    let (>=) = Wrap.binary ZBitvector.mk_sge
  end

  module Unsigned = struct
    let (<) = Wrap.binary ZBitvector.mk_ult
    let (<=) = Wrap.binary ZBitvector.mk_ule
    let (>) = Wrap.binary ZBitvector.mk_ugt
    let (>=) = Wrap.binary ZBitvector.mk_uge
  end

  module Set = struct
    let const_empty ctx bits =
      Bitvector.Numeral.int
        (Bitvector.create_sort ctx ~bits)
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

    let to_binary_string_exn t =
      if Stdlib.not (Expr.is_numeral t)
      then raise_s [%message "not a numeral" (t : _ Expr.t)];
      let length = Bitvector.size (Expr.sort t) in
      let ctx = Expr.context t in
      let short_string =
        Z3native.get_numeral_binary_string
          (Context.Native.to_native ctx)
          (Expr.Native.to_native t)
      in
      String.init (length - String.length short_string) ~f:(Fn.const '0')
      ^ short_string

    let to_binary_array_exn t =
      to_binary_string_exn t
      |> String.to_array
      |> Array.map ~f:(Char.(=) '1')

    let to_int_exn t =
      if Stdlib.not (Expr.is_numeral t)
      then raise_s [%message "not a numeral" (t : _ Expr.t)];
      let ctx = Expr.context t in
      let success, result =
        Z3native.get_numeral_int
          (Context.Native.to_native ctx)
          (Expr.Native.to_native t)
      in
      if success
      then result
      else raise_s [%message "cannot fit in int" (t : _ Expr.t)]

    let to_int64_signed_exn t =
      if Stdlib.not (Expr.is_numeral t)
      then raise_s [%message "not a numeral" (t : _ Expr.t)];
      let ctx = Expr.context t in
      let success, result =
        Z3native.get_numeral_int64
          (Context.Native.to_native ctx)
          (Expr.Native.to_native t)
      in
      if success
      then result
      else raise_s [%message "cannot fit in int64" (t : _ Expr.t)]
  end

end

and Model : Model
= struct
  type t = Types.Model.t

  module ZModel = Z3.Model

  let to_string t = ZModel.to_string t
  let sexp_of_t t = Sexp.List (Sexp.of_string_many (to_string t))

  let eval (type s) ?(apply_model_completion=true) t (expr : s Expr.t) =
    ZModel.eval t (Expr.to_raw expr) apply_model_completion
    |> (Obj.magic : Expr.raw option -> s Expr.t option)

  let eval_exn ?apply_model_completion t expr =
    eval ?apply_model_completion t expr
    |> Option.value_exn ~message:"could not evaluate"

  let const_interp (type s) t (fdecl : (Nothing.t, s) Function_declaration.t) =
    ZModel.get_const_interp t (Function_declaration.to_raw fdecl)
    |> (Obj.magic : Expr.raw option -> s Expr.t option)

  let const_interp_exn (type s) t (fdecl : (Nothing.t, s) Function_declaration.t) =
    Option.value_exn (const_interp t fdecl)

  let const_interp_e (type s) t (expr : s Expr.t) =
    ZModel.get_const_interp_e t (Expr.to_raw expr)
    |> (Obj.magic : Expr.raw option -> s Expr.t option)

  let const_interp_e_exn (type s) t (expr : s Expr.t) =
    Option.value_exn (const_interp_e t expr)

  let num_consts t = ZModel.get_num_consts t
  let num_funcs t = ZModel.get_num_funcs t

  let const_decls t =
    ZModel.get_const_decls t
    |> Function_declaration.unsafe_of_raw_list
    |> Function_declaration.pack_list

  let func_decls t =
    ZModel.get_func_decls t
    |> Function_declaration.unsafe_of_raw_list
    |> Function_declaration.pack_list

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.model)
    let to_native_list = (Obj.magic : t list -> Z3native.model list)
    let unsafe_of_native = (Obj.magic : Z3native.model -> t)
  end

end

and Function_declaration : Function_declaration
= struct
  include Types.Function_declaration

  module ZFuncDecl = Z3.FuncDecl

  let to_string t = ZFuncDecl.to_string t

  let sexp_of_t _ _ t = Sexp.List (Sexp.of_string_many (to_string (to_raw t)))

  let context t =
    (* CR smuenzel: no get_context *)
    ZFuncDecl.get_name (to_raw t)
    |> Symbol.context

  let domain (type a) (t : (a, _) t) : a Sort.List.t =
    let L result =
      Z3.FuncDecl.get_domain (Function_declaration.to_raw t)
      |> Sort.unsafe_of_raw_list
      |> Sort.pack_list
      |> Sort.List.of_packed_list
    in
    (Obj.magic : _ Sort.List.t -> _ Sort.List.t) result

  let is_nullary_exn
      (type a body)
      (t : (a, body) t)
    : (a, Nothing.t) Type_equal.t
    =
    match domain t with
    | [] -> T
    | _ -> assert false

  let range t =
    Z3.FuncDecl.get_range (Function_declaration.to_raw t)
    |> Sort.unsafe_of_raw

  let sort_kind (type a final) (t : (a, final) t) : a S.lambda_instance * final Sort.Kind.t =
    let A domain = Sort.func_decl_domain t in
    let range = Sort.func_decl_range t in
    ((Obj.magic : _ S.lambda_instance -> _ S.lambda_instance) domain), range

  let same_witness
      (type a afinal b bfinal)
      (a : (a, afinal) t)
      (b : (b, bfinal) t)
      : (a*afinal, b*bfinal) Type_equal.t option
      =
      let adomain =
        Z3.FuncDecl.get_domain (Function_declaration.to_raw a)
      in
      let bdomain =
        Z3.FuncDecl.get_domain (Function_declaration.to_raw b)
      in
      let arange =
        Z3.FuncDecl.get_range (Function_declaration.to_raw a)
        |> Sort.unsafe_of_raw
      in
      let brange =
        Z3.FuncDecl.get_range (Function_declaration.to_raw b)
        |> Sort.unsafe_of_raw
      in
      if
        Sort.equal arange brange
        && Base.List.equal
          (fun a b ->
             Sort.equal (Sort.unsafe_of_raw a) (Sort.unsafe_of_raw b)
          )
          adomain bdomain
      then Obj.magic (Some Type_equal.T)
      else begin
        let ka,ra = Function_declaration.sort_kind a in 
        let kb,rb = Function_declaration.sort_kind b in 
        match Sort.Kind.same ra rb with
        | None -> None
        | Some T ->
          match Sort.Kind.same_lambda_instance ka kb with
          | None -> None
          | Some T -> Some (T : (a*afinal,b*bfinal) Type_equal.t)
      end

  let app
      (type inputs body)
      (t : (inputs,body) t)
      (ss : inputs Expr.List.t)
    : body Expr.t =
    let as_list =
      Expr.List.higher ss
      |> Typed_list.Lambda_higher.to_list_map
        { f = fun x -> Expr.(Native.to_native !< x)}
    in
    let length = Base.List.length as_list in
    let context = context t in
    Z3native.mk_app
      (Context.Native.to_native context)
      (Function_declaration.Native.to_native t)
      length
      as_list
    |> Expr.Native.unsafe_of_native

  let name t = ZFuncDecl.get_name (to_raw t)
end

and Function_interpretation : Function_interpretation
= struct
  type t = Types.Function_interpretation.t

  module ZFuncInterp = Z3.Model.FuncInterp

  let to_string t = ZFuncInterp.to_string t

  let sexp_of_t t = Sexp.List (Sexp.of_string_many (to_string t))
end

and Solver_result : Solver_result
   with type 'a t = 'a Types.Solver_result.t
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
= struct
  type t = Types.Solver.t

  let context t =
    Z3native.context_of_solver (Solver.Native.to_native t)
    |> Context.Native.unsafe_of_native

  module ZSolver = Z3.Solver

  let set_timeout t timeout =
    let context = context t in
    let p = Z3.Params.mk_params context in
    let timeout =
      Time_ns.Span.to_ms timeout
      |> Float.iround_up_exn
    in
    Z3.Params.add_int p (Symbol.of_string context "timeout") timeout;
    ZSolver.set_parameters t p


  let set_parameters
      ?timeout
      t
    =
    let context = context t in
    let p = Z3.Params.mk_params context in
    Option.iter timeout
      ~f:(fun timeout ->
          let timeout =
            Time_ns.Span.to_ms timeout
            |> Float.iround_up_exn
          in
          Z3.Params.add_int p (Symbol.of_string context "timeout") timeout;
        );
    Z3.Params.add_bool p (Symbol.of_string context "ctrl_c") false;
    ZSolver.set_parameters t p

  let create
      ?timeout
      context 
    =
    let s = ZSolver.mk_solver context None in
    set_parameters ?timeout s;
    s

  let to_string = ZSolver.to_string

  let sexp_of_t t =
    to_string t
    |> Sexp.of_string_many
    |> Sexp.List

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

  let enter_scope t f =
    push t;
    let result = f t in
    pop t 1;
    result

  let stats = ZSolver.get_statistics

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.solver)
    let to_native_list = (Obj.magic : t list -> Z3native.solver list)
    let unsafe_of_native = (Obj.magic : Z3native.solver -> t)
  end
end

and Optimize : Optimize
= struct
  type t = Types.Optimize.t

  let context t =
    Z3native.context_of_optimize (Optimize.Native.to_native t)
    |> Context.Native.unsafe_of_native

  module ZOptimize = Z3.Optimize

  let set_timeout t timeout =
    let context = context t in
    let p = Z3.Params.mk_params context in
    let timeout =
      Time_ns.Span.to_ms timeout
      |> Float.iround_up_exn
    in
    Z3.Params.add_int p (Symbol.of_string context "timeout") timeout;
    ZOptimize.set_parameters t p

  let create
      ?timeout
      context
    =
    let s = ZOptimize.mk_opt context in
    let p = Z3.Params.mk_params context in
    Option.iter timeout
      ~f:(fun timeout ->
          let timeout =
            Time_ns.Span.to_ms timeout
            |> Float.iround_up_exn
          in
          Z3.Params.add_int p (Symbol.of_string context "timeout") timeout;
        );
    Z3.Params.add_bool p (Symbol.of_string context "ctrl_c") false;
    ZOptimize.set_parameters s p;
    s

  let stats = ZOptimize.get_statistics

  let to_string t = ZOptimize.to_string t

  let sexp_of_t t =
    to_string t
    |> Sexp.of_string_many
    |> Sexp.List

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

  let enter_scope t f =
    push t;
    let result = f t in
    pop t 1;
    result

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.optimize)
    let to_native_list = (Obj.magic : t list -> Z3native.optimize list)
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
= struct
  type t = Types.Symbol.t

  let of_int = Z3.Symbol.mk_int

  let of_string = Z3.Symbol.mk_string

  let sexp_of_t t =
    Z3.Symbol.to_string t
    |> [%sexp_of: string]

  let context t =
    Z3native.context_of_symbol (Symbol.Native.to_native t)
    |> Context.Native.unsafe_of_native

  module Kind = Symbol_kind

  module Native = struct
    let to_native = (Obj.magic : t -> Z3native.symbol)
    let to_native_list = (Obj.magic : t list -> Z3native.symbol list)
    let unsafe_of_native = (Obj.magic : Z3native.symbol -> t)
  end

  module Contents = struct
    type nonrec _ t = t

    type packed = | T : _ t -> packed [@@unboxed]

    let kind (type a) (t : a t) : a Kind.t =
      match Z3.Symbol.kind t with
      | INT_SYMBOL -> Obj.magic Symbol_kind.Int
      | STRING_SYMBOL -> Obj.magic Symbol_kind.String

    let value (type a) (t : a t) : a =
      match (kind t : a Kind.t) with
      | Int -> Z3.Symbol.get_int t
      | String -> Z3.Symbol.get_string t

  end

  let contents t = Contents.T t
end

and Boolean : Boolean
= struct

  module ZBoolean = Z3.Boolean

  let create_sort ctx =
    Z3.Boolean.mk_sort ctx
    |> Sort.unsafe_of_raw

  (* CR smuenzel: this is not efficient *)
  let mk_and2 c a b =
    Z3native.mk_and (Context.Native.to_native c)
      2
      [ Expr.Native.to_native a
      ; Expr.Native.to_native b
      ]
    |> Expr.Native.unsafe_of_native

    (* CR smuenzel: this is not efficient *)
  let mk_or2 c a b =
    Z3native.mk_or (Context.Native.to_native c)
      2
      [ Expr.Native.to_native a 
      ; Expr.Native.to_native b
      ]
    |> Expr.Native.unsafe_of_native

  module Ops(Wrap : Wrap.T) = struct
    type t = S.bool Expr.t

    let and_list = Wrap.list ZBoolean.mk_and
    let and_ = Wrap.binary_expr mk_and2
    let or_list = Wrap.list ZBoolean.mk_or
    let or_ = Wrap.binary_expr mk_or2
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

  let of_single_bit_vector (e : S.bv Expr.t) =
    eq e (Bitvector.Numeral.bit1_e e)

  let neq a b = not (eq a b)

  let ite a b c =
    (Wrap.ternary ZBoolean.mk_ite).f a b c

  let distinct list =
    ZBoolean.mk_distinct
      (Wrap.context_of_list_exn list)
      (Expr.to_raw_list list)
    |> Expr.unsafe_of_raw

  let and_array array =
    let c = Wrap.context_of_array_exn array in
    Array.reduce_exn array ~f:(mk_and2 c)

  let or_array array =
    let c = Wrap.context_of_array_exn array in
    Array.reduce_exn array ~f:(mk_or2 c)

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
= struct

  module ZQuantifier = Z3.Quantifier

  include Types.Quantifier

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
    let _, Sort.T head_sort = Base.List.hd_exn variables in
    let symbols, sorts = Base.List.unzip variables in
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
    let Expr.T head_expr = Base.List.hd_exn variables in
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

  let lambda_const
      (type final inputs)
      (variables : inputs Expr.List.t)
      ~(body:final Expr.t)
    : (inputs, final) S.array t 
    =
    let variables =
      Typed_list.Lambda_higher.to_list_map
        { f = fun e -> Expr.(T !< e) }
        (Expr.List.higher variables)
    in
    ZQuantifier.mk_lambda_const
      (Expr.context body)
      (Expr.to_raw_unpack_list variables)
      (Expr.to_raw body)
    |> unsafe_of_raw

  let lambda_single
      symbol
      sort
      ~body
    =
    ZQuantifier.mk_lambda
      (Sort.context sort)
      [ symbol, Sort.to_raw sort
      ]
      (Expr.to_raw body)
    |> unsafe_of_raw

  let lambda_single_const
      variable
      ~body
    =
    ZQuantifier.mk_lambda_const
      (Expr.context variable)
      [ Expr.to_raw variable ]
      (Expr.to_raw body)
    |> unsafe_of_raw
end

and Pattern : Pattern
= struct
  include Types.Pattern

  let sexp_of_t _ t =
    [%sexp_of:string] (Z3.Quantifier.Pattern.to_string (to_raw t))

  let create exprs =
    Z3.Quantifier.mk_pattern
      (Expr.context (Base.List.hd_exn exprs))
      (Expr.to_raw_list exprs)
    |> unsafe_of_raw
end

and ZArray : ZArray
= struct

  type ('a, 'b) t = ('a, 'b) S.array Expr.t

  let array_domain_n (s : (_,_) S.array Sort.t) i =
    try
      let result =
        Z3native.get_array_sort_domain_n
          (Sort.context s |> Context.Native.to_native)
          (Sort.Native.to_native s)
          i
        |> Sort.Native.unsafe_of_native
      in
      Some (Sort.T result)
    with
    | Z3.Error _ -> None

  let rec all_array_domains acc s i =
    match array_domain_n s i with
    | None -> List.rev acc
    | Some packed ->
      all_array_domains (packed :: acc) s (i + 1)

  let all_array_domains (s : (_,_) S.array Sort.t) = all_array_domains [] s 0

  let domain (type a) (s : (a, _) S.array Sort.t) : a Sort.List.t =
    let L result =
      all_array_domains s
      |> Sort.List.of_packed_list
    in
    (Obj.magic : _ Sort.List.t -> _ Sort.List.t) result

  let range (type b c) (s : (c, b) S.array Sort.t) : b Sort.t =
    Z3.Z3Array.get_range (Sort.to_raw s)
    |> Sort.unsafe_of_raw

  let select_single (type a b) (ar : (a * Nothing.t, b) t) (s : a Expr.t) : b Expr.t =
    Z3.Z3Array.mk_select (Expr.context ar) (Expr.to_raw ar) (Expr.to_raw s)
    |> Expr.unsafe_of_raw

  let select
      (type inputs body)
      (ar : (inputs,body) S.array Expr.t)
      (ss : inputs Expr.List.t)
    : body Expr.t =
    let as_list =
      Expr.List.higher ss
      |> Typed_list.Lambda_higher.to_list_map
        { f = fun x -> Expr.(Native.to_native !< x) }
    in
    let length = List.length as_list in
    Z3native.mk_select_n
      (Expr.context ar |> Context.Native.to_native)
      (Expr.Native.to_native ar)
      length
      as_list
    |> Expr.Native.unsafe_of_native

end

and ZTuple : ZTuple
= struct

  module Symbol_sort_list =
    Typed_list.Make_lambda_lower(struct
      type 'a t = Symbol.t * 'a Sort.t
      include Higher_kinded_short.Make1(struct type nonrec 'a t = 'a t end)
    end)

  module Field_accessor = struct
    type ('arg, 'extra) t = ('extra * Nothing.t,'arg) Function_declaration.t 

    include Higher_kinded_short.Make2(struct
        type nonrec ('a, 'b) t = ('a, 'b) t
      end)
  end

  module Field_accessor_list = struct
    include Typed_list.Make_lambda_lower2(Field_accessor)
  end 

  module Z3Tuple = Z3.Tuple

  type 'a t = 'a S.tuple S.datatype Expr.t

  let rec unsafe_make_accessors : Function_declaration.packed list -> 'res Field_accessor_list.packed =
    fun list ->
    match (list : Function_declaration.packed list) with
    | [] -> Field_accessor_list.(L [])
    | T x :: xs ->
      let L rest = unsafe_make_accessors xs in
      let x = (Obj.magic : (_, _) Function_declaration.t -> (_,_) Field_accessor.t) x in
      Field_accessor_list.(L (x :: rest))

  let create_sort
    (type a)
    symbol
    (list : a Symbol_sort_list.t)
    : ( (a S.tuple S.datatype as 'res) Sort.t
        * (a,'res) Function_declaration.t
        * (a,'res) Field_accessor_list.t
      )
    =
    let list =
      Symbol_sort_list.higher list
      |> Typed_list.Lambda_higher.to_list_map
        { f = fun x ->
              let symbol, sort = Symbol_sort_list.Inner.project x in
              Symbol.Native.to_native symbol
            , Sort.Native.to_native sort
        }
    in
    let length = List.length list in
    let symbols, sorts = List.unzip list in
    let sort, constructor, accessors =
      Z3native.mk_tuple_sort
        (Symbol.context symbol |> Context.Native.to_native)
        (Symbol.Native.to_native symbol)
        length
        symbols
        sorts
    in
    let Field_accessor_list.L accessors =
      Function_declaration.Native.unsafe_of_native_list accessors
      |> unsafe_make_accessors
    in
    let accessors =
      (Obj.magic : (_,_) Field_accessor_list.t -> (a, a S.tuple S.datatype) Field_accessor_list.t) accessors
    in
    Sort.Native.unsafe_of_native sort
  , Function_declaration.Native.unsafe_of_native constructor
  , accessors

  let accessors
      (type a)
      (sort : (a S.tuple S.datatype as 'res) Sort.t)
    : (a, a S.tuple S.datatype) Field_accessor_list.t
    =
    let Field_accessor_list.L accessors =
      Z3Tuple.get_field_decls (Sort.to_raw sort)
      |> Function_declaration.unsafe_of_raw_list
      |> Function_declaration.pack_list
      |> unsafe_make_accessors
    in
    (Obj.magic : (_,_) Field_accessor_list.t -> (a, a S.tuple S.datatype) Field_accessor_list.t) accessors

  let constructor
      (type a)
      (sort : (a S.tuple S.datatype as 'res) Sort.t)
    : (a,a S.tuple S.datatype) Function_declaration.t
    =
    Z3Tuple.get_mk_decl (Sort.to_raw sort)
    |> Function_declaration.unsafe_of_raw
end

and Statistics : Statistics = struct
  type t = Types.Statistics.t

  let to_string = Z3.Statistics.to_string

end
