open Core

module type With_sort = sig
  type raw
  type 's t = private raw
  type packed = T : _ t -> packed [@@unboxed]
end

module type Types = sig
  module Context : T
  module Expr : With_sort
  module Sort : With_sort
  module Symbol : T
  module Model : T
  module Function_interpretation : T
  module Optimize : T
  module Solver : T
  module Solver_result : sig
    type 'a t =
      | Unsatisfiable
      | Unknown of string
      | Satisfiable of 'a
  end
  module Quantifier : With_sort
  module Pattern : With_sort
end

module S = struct
  type uninterpreted = [ `Uninterpreted ]
  type bool = [ `Bool ]
  type int = [ `Int ]
  type real = [ `Real ]
  type bv = [ `Bv ]
  type array = [ `Array ]
  type datatype = [ `Datatype ]
  type relation = [ `Relation ]
  type finite_domain = [ `Finite_domain ]
  type floating_point = [ `Floating_point ]
  type rounding_mode = [ `Rounding_mode ]
  type seq = [ `Seq ]
  type re = [ `Re ]
  type char = [ `Char ]
  type unknown = [ `Unknown ]

  type _ kind =
    | Uninterpreted : uninterpreted kind
    | Bool : bool kind
    | Int : int kind
    | Real : real kind
    | Bv : bv kind
    | Array : array kind
    | Datatype : datatype kind
    | Relation : relation kind
    | Finite_domain : finite_domain kind
    | Floating_point : floating_point kind
    | Rounding_mode : rounding_mode kind
    | Seq : seq kind
    | Re : re kind
    | Char : char kind
    | Unknown : unknown kind
end

module type Native = sig
  type t
  type native
  val to_native : t -> native
  val unsafe_of_native : native -> t
end

module type Native1 = sig
  type _ t
  type native
  val to_native : _ t -> native
  val unsafe_of_native : native -> _ t
end

module With_raw(With_sort : With_sort) = struct
  module type S = sig
    type raw = With_sort.raw
    type 's t = 's With_sort.t [@@deriving sexp_of]
    type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

    val to_raw : _ t -> raw
    val unsafe_of_raw : raw -> _ t

    val to_raw_list : _ t list -> raw list
    val unsafe_of_raw_list : raw list -> _ t list

    val pack : _ t -> packed
    val pack_list : _ t list -> packed list

    val to_raw_unpack_list : packed list -> raw list

    module Native : Native1 with type 's t := 's t
  end
end

module type Expr = sig
  module Types : Types
  open Types

  include With_raw(Expr).S with type Native.native := Z3native.ast

  val context : _ t -> Context.t
  val sort : 's t -> 's Sort.t
  val sort_kind : 's t -> 's S.kind

  val is_numeral : _ t -> bool

  val to_string : _ t -> string

  val const : Symbol.t -> 's Sort.t -> 's t
  val const_s : string -> 's Sort.t -> 's t
  val const_i : int -> 's Sort.t -> 's t
  val numeral_int : int -> 's Sort.t -> 's t

  val simplify : 's t -> 's t
end

module type Context = sig
  module Types : Types

  type t = Types.Context.t

  val create : ?model:bool -> ?proof:bool -> unit -> t

  module Native : Native with type t := t and type native := Z3native.context
end

module type Sort = sig
  module Types : Types
  open Types

  include With_raw(Sort).S with type Native.native := Z3native.sort

  val same : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_kind : 'a t -> 'b t -> ('a, 'b) Type_equal.t option

  val sort_kind : 's t -> 's S.kind

  val context : _ t -> Context.t

  val create_bitvector : Context.t -> bits:int -> S.bv t
end

module type Ordering = sig
  module Types : Types
  open Types
  type s
  val (>) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (>=) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (<) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (<=) : s Expr.t -> s Expr.t -> S.bool Expr.t
end

module type Bitvector = sig
  module Types : Types
  open Types

  type t = S.bv Expr.t  

  val is_bv : 's Expr.t -> ('s, S.bv) Type_equal.t option
  val size : S.bv Sort.t -> int
  val size_e : t -> int

  val const : Symbol.t -> bits:int -> t
  val const_s : Context.t -> string -> bits:int -> t
  val const_i : Context.t -> int -> bits:int -> t

  (* CR smuenzel: add With_context *)

  val of_boolean : S.bool Expr.t -> t

  val and_ : t -> t -> t
  val and_list : t list -> t
  val or_ : t -> t -> t
  val or_list : t list -> t
  val xor : t -> t -> t
  val xor_list : t list -> t
  val nand : t -> t -> t
  val nor : t -> t -> t
  val xnor : t -> t -> t
  val not : t -> t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t

  val shift_left : t -> count:t -> t

  val concat : t -> t -> t
  val concat_list : t list -> t
  val repeat : t -> count:int -> t
  val broadcast_single : t -> S.bv Sort.t -> t
  val extract : t -> high:int -> low:int -> t
  val extract_single : t -> int -> t
  val zero_extend : t -> extra_zeros:int -> t
  val sign_extend : t -> extra_bits:int -> t
  val rotate_left_const : t -> int -> t

  val popcount : ?result_bit_size:int -> t -> t

  val is_zero : t -> S.bool Expr.t
  val is_not_zero : t -> S.bool Expr.t
  val is_power_of_two : t -> S.bool Expr.t
  val is_power_of_two_or_zero : t -> S.bool Expr.t
  val is_add_overflow : signed:bool -> t -> t -> S.bool Expr.t

  val sign : t -> t
  val parity : t -> t

  module Signed : sig
    include Ordering with type s := S.bv and module Types := Types
  end

  module Unsigned : sig
    include Ordering with type s := S.bv and module Types := Types
  end

  module Set : sig
    val const_empty : Context.t -> int -> t

    val union : t -> t -> t
    val inter : t -> t -> t
    val complement : t -> t
    val diff : t -> t -> t
    val symmdiff : t -> t -> t

    val is_empty : t -> S.bool Expr.t
    val is_subset : t -> of_:t -> S.bool Expr.t
    val has_max_one_member : t -> S.bool Expr.t
    val has_single_member : t -> S.bool Expr.t
  end

  module Numeral : sig
    val bit0 : Context.t -> t
    val bit1 : Context.t -> t
    val bit0_e : _ Expr.t -> t
    val bit1_e : _ Expr.t -> t
    val bool : Context.t -> bool list -> t

    val int : S.bv Sort.t -> int -> t
    val int_e : t -> int -> t

    val to_binary_string_exn : t -> string
    val to_binary_array_exn : t -> bool array
  end

end

module type Model = sig
  module Types : Types
  open Types

  type t = Types.Model.t [@@deriving sexp_of]

  val to_string : t -> string

  val eval : ?apply_model_completion:bool -> t -> 's Expr.t -> 's Expr.t option
  val eval_exn : ?apply_model_completion:bool -> t -> 's Expr.t -> 's Expr.t

  val const_interp_e : t -> 's Expr.t -> 's Expr.t option
  val const_interp_e_exn : t -> 's Expr.t -> 's Expr.t

  module Native : Native with type t := t and type native := Z3native.model
end

module type Function_interpretation = sig
  module Types : Types
  open! Types

  type t = Types.Function_interpretation.t [@@deriving sexp_of]
end

module type Solver_result = sig
  module Types : Types

  type 'a t = 'a Types.Solver_result.t =
    | Unsatisfiable
    | Unknown of string
    | Satisfiable of 'a
  [@@deriving sexp]

  val satisfiable_exn : 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type Generic_solver = sig
  module Types : Types
  open Types

  type t
  type native

  val context : t -> Context.t

  val create : Context.t -> t

  val to_string : t -> string

  val add_list : t -> S.bool Expr.t list -> unit
  val add : t -> S.bool Expr.t -> unit

  val check_and_get_model : t -> S.bool Expr.t list -> Model.t Solver_result.t
  val check_current_and_get_model : t -> Model.t Solver_result.t

  val push : t -> unit
  val pop : t -> int -> unit

  module Native : Native with type t := t and type native := native
end

module type Solver = sig
  module Types : Types

  include Generic_solver
    with type t = Types.Solver.t
     and type native := Z3native.solver
     and module Types := Types
end

module type Optimize = sig
  module Types : Types
  open Types

  include Generic_solver
    with type t = Types.Optimize.t
     and type native := Z3native.optimize
     and module Types := Types

  module Goal : sig
    type 's t

    val lower : 's t -> 's Expr.t
    val upper : 's t -> 's Expr.t
  end

  val add_soft : t -> 's Expr.t -> weight:int -> Symbol.t -> 's Goal.t

end

module type Symbol = sig
  module Types : Types
  open Types

  type t = Symbol.t

  val context : t -> Context.t

  val of_int : Context.t -> int -> t
  val of_string : Context.t -> string -> t

  module Native : Native with type t := t and type native := Z3native.symbol
end

module type Boolean_ops = sig
  module Types : Types
  open Types
  type 'a wrap
  type t = S.bool Expr.t
  type n_ary := (t list -> t) wrap
  type binary := (t -> t -> t) wrap
  type unary := (t -> t) wrap

  val and_list : n_ary
  val or_list : n_ary
  val and_ : binary
  val or_ : binary
  val xor : binary
  val not : unary

  val iff : binary
  val implies : binary
  val ite : (t -> 'a Expr.t -> 'a Expr.t -> 'a Expr.t) wrap

  val eq : ('s1 Expr.t -> 's1 Expr.t -> t) wrap
  val neq : ('s1 Expr.t -> 's1 Expr.t -> t) wrap
end

module type Boolean = sig
  module Types : Types
  open Types

  module With_context :
    Boolean_ops with type 'a wrap := Context.t -> 'a and module Types := Types

  include Boolean_ops with type 'a wrap := 'a and module Types := Types

  val distinct : 's Expr.t list -> t

  val is_bool : 's Expr.t -> ('s, S.bool) Type_equal.t option
  val of_single_bit_vector : S.bv Expr.t -> t

  val and_array : t Array.t -> t
  val or_array : t Array.t -> t

  module Numeral : sig
    val false_ : Context.t -> S.bool Expr.t
    val true_ : Context.t -> S.bool Expr.t
    val bool : Context.t -> bool -> S.bool Expr.t
  end
end

module type Quantifier = sig
  module Types : Types
  open Types

  type 's t = 's Quantifier.t

  val to_expr : 's t -> 's Expr.t
  val of_expr : 's Expr.t -> 's t

  type 's create_quantifer
  :=   ?weight:int
    -> ?quantifier_id:Symbol.t
    -> ?skolem_id:Symbol.t
    -> ?patterns:Pattern.packed list
    -> ?nopatterns:Expr.packed list
    -> (Sort.packed * Symbol.t) list
    -> body:'s Expr.t
    -> 's t

  type 's create_quantifer_const
  :=   ?weight:int
    -> ?quantifier_id:Symbol.t
    -> ?skolem_id:Symbol.t
    -> ?patterns:Pattern.packed list
    -> ?nopatterns:Expr.packed list
    -> Expr.packed list
    -> body:'s Expr.t
    -> 's t

  val forall : 's create_quantifer
  val forall_const : 's create_quantifer_const
  val exists : 's create_quantifer
  val exists_const : 's create_quantifer_const
end

module type Pattern = sig
  module Types : Types
  open Types

  include With_raw(Pattern).S with type Native.native := Z3native.pattern

  val create : 's Expr.t list -> 's t
end

module rec Types : Types
  with type Context.t = Z3.context
   and type Sort.raw = Z3.Sort.sort
   and type Expr.raw = Z3.Expr.expr
   and type Symbol.t = Z3.Symbol.symbol
   and type Model.t = Z3.Model.model
   and type Function_interpretation.t = Z3.Model.FuncInterp.func_interp
   and type Solver.t = Z3.Solver.solver
   and type Optimize.t = Z3.Optimize.optimize
   and type Quantifier.raw = Z3.Quantifier.quantifier
   and type Pattern.raw = Z3.Quantifier.Pattern.pattern
  = Types

module type Z3i_internal = sig
  module Context : Context with module Types := Types
  module Expr : Expr with module Types := Types
  module Sort : Sort with module Types := Types
  module Bitvector : Bitvector with module Types := Types
  module Model : Model with module Types := Types
  module Function_interpretation : Function_interpretation with module Types := Types
  module Solver : Solver
    with module Types := Types
  module Solver_result : Solver_result
    with module Types := Types
     and type 'a t = 'a Types.Solver_result.t
  module Optimize : Optimize with module Types := Types
  module Symbol : Symbol with module Types := Types
  module Boolean : Boolean with module Types := Types
  module Quantifier : Quantifier with module Types := Types
  module Pattern : Pattern with module Types := Types

  module S = S
end

module type Mux = sig
  module Types : Types
  open Types

  type t =
    { selector : S.bv Expr.t
    ; output : S.bv Expr.t
    ; assertions : S.bool Expr.t list
    ; length : int
    }

  val create
    :  selector_symbol:Symbol.t
    -> S.bv Expr.t list
    -> t
  
  val selector_at : t -> int -> S.bool Expr.t

  val constraints : t -> S.bool Expr.t

  val model_selector : t -> Model.t -> int option
end

module type Symbol_builder = sig
  module Types : Types
  open Types

  type t

  val create : ?first_symbol:int -> ?use_name:bool -> Context.t -> t
  val sym : ?name:string -> t -> Symbol.t
  val sym_int : t -> int
  val context : t -> Context.t
end

module type Z3i = sig
  include Z3i_internal

  module Mux : Mux with module Types := Types
  module Symbol_builder : Symbol_builder with module Types := Types

end
