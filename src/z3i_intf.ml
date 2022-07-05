open Core
open! Typed_list

module type With_sort = sig
  type higher_kinded
  type raw
  type 's t = private raw
  type packed = T : _ t -> packed [@@unboxed]
end

module type With_sort2 = sig
  type raw
  type (_,_) t = private raw
  type packed = T : (_,_) t -> packed [@@unboxed]
end

module S = struct
  type uninterpreted = [ `Uninterpreted ]
  type bool = [ `Bool ]
  type int = [ `Int ]
  type real = [ `Real ]
  type bv = [ `Bv ]
  type ('a, 'b) array = [ `Array of 'a * 'b]
  type 'a datatype = [ `Datatype of 'a]
  type relation = [ `Relation ]
  type finite_domain = [ `Finite_domain ]
  type floating_point = [ `Floating_point ]
  type rounding_mode = [ `Rounding_mode ]
  type seq = [ `Seq ]
  type re = [ `Re ]
  type char = [ `Char ]
  type unknown = [ `Unknown ]

  type 'a tuple = [ `Tuple of 'a ]
  type other = [ `Other ]

  [@@@ocaml.warning "-30"]

  type _ kind =
    | Uninterpreted : uninterpreted kind
    | Bool : bool kind
    | Int : int kind
    | Real : real kind
    | Bv : bv kind
    | Array : 'a lambda_instance * 'final kind-> ('a, 'final) array kind
    | Datatype : 'a datatype_kind -> 'a datatype kind
    | Relation : relation kind
    | Finite_domain : finite_domain kind
    | Floating_point : floating_point kind
    | Rounding_mode : rounding_mode kind
    | Seq : seq kind
    | Re : re kind
    | Char : char kind
    | Unknown : unknown kind
  and 'inputs  lambda_instance =
    | [] : Nothing.t lambda_instance
    | (::)
      : 'arg kind * 'next_args lambda_instance
        -> ('arg * 'next_args) lambda_instance
  and packed_kind =
    | K : _ kind -> packed_kind [@@unboxed]
  and packed_lambda_instance =
    | A : _ lambda_instance -> packed_lambda_instance [@@unboxed]
  and 'a datatype_kind =
    | Tuple : 'a tuple_instance -> 'a tuple datatype_kind
    | Other : other datatype_kind
  and _ tuple_instance =
    | [] : Nothing.t tuple_instance
    | (::)
      : 'arg kind * 'next_arg tuple_instance
        -> ('arg * 'next_arg) tuple_instance
  and packed_tuple_instance =
    | TP : _ tuple_instance -> packed_tuple_instance [@@unboxed]
end

module type Native = sig
  type t
  type native
  val to_native : t -> native
  val to_native_list : t list -> native list
  val unsafe_of_native : native -> t
end

module type Native1 = sig
  type _ t
  type packed
  type native
  val to_native : _ t -> native
  val unsafe_of_native : native -> _ t
  val to_native_list : packed list -> native list
  val unsafe_of_native_list : native list -> packed list
end

module type Native2 = sig
  type (_,_) t
  type packed
  type native
  val to_native : (_,_) t -> native
  val unsafe_of_native : native -> (_,_) t
  val to_native_list : packed list -> native list
  val unsafe_of_native_list : native list -> packed list
end

module type Ast = sig
  open Types

  type t = Ast.t

  module Kind : sig
    type t = Z3enums.ast_kind [@@deriving sexp]
  end

  val kind : t -> Kind.t
end

module type Expr = sig
  open Types

  include module type of struct include Types.Expr end
  include sig type 'a t [@@deriving sexp_of] end with type 'a t := 'a t

  val context : _ t -> Context.t
  val translate : 'a t -> Context.t -> 'a t

  val sort : 's t -> 's Sort.t
  val sort_kind : 's t -> 's S.kind
  val is_kind_exn : 's t -> 'ss S.kind -> ('s, 'ss) Type_equal.t

  val is_numeral : _ t -> bool

  val to_string : _ t -> string
  val to_ast : _ t -> Ast.t

  val const : Symbol.t -> 's Sort.t -> 's t
  val const_s : string -> 's Sort.t -> 's t
  val const_i : int -> 's Sort.t -> 's t
  val numeral_int : int -> 's Sort.t -> 's t

  val simplify : 's t -> 's t
end

module type Context = sig
  type t = Types.Context.t

  val create
    :  ?parallel:bool
    -> ?model:bool
    -> ?proof:bool
    -> unit
    -> t

  module Native : Native with type t := t and type native := Z3native.context
end

module type Sort = sig
  open Types

  include module type of struct include Types.Sort end
  include sig type 'a t [@@deriving sexp_of] end with type 'a t := 'a t

  module Kind : sig
    type 's t = 's S.kind [@@deriving sexp_of]

    type 'a lambda_instance = 'a S.lambda_instance [@@deriving sexp_of]

    val same : 'a t -> 'b t -> ('a, 'b) Type_equal.t option

    val same_lambda_instance
      : 'a0 S.lambda_instance
      -> 'b0 S.lambda_instance
      -> ('a0, 'b0) Type_equal.t option
  end

  val equal : _ t -> _ t -> bool

  val same : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val same_kind : 'a t -> 'b t -> ('a, 'b) Type_equal.t option

  val sort_kind : 's t -> 's Kind.t

  val context : _ t -> Context.t
end

module type Ordering = sig
  open Types
  type s
  val (>) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (>=) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (<) : s Expr.t -> s Expr.t -> S.bool Expr.t
  val (<=) : s Expr.t -> s Expr.t -> S.bool Expr.t
end

module type Bitvector = sig
  open Types

  type t = S.bv Expr.t  

  val create_sort : Context.t -> bits:int -> S.bv Sort.t

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
  val shift_right : t -> count:t -> t
  val shift_arithmetic_right : t -> count:t -> t

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
  val is_sub_underflow : signed:bool -> t -> t -> S.bool Expr.t

  val sign : t -> t
  val parity : t -> t

  module Signed : sig
    include Ordering with type s := S.bv
  end

  module Unsigned : sig
    include Ordering with type s := S.bv
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
  open Types

  type t = Types.Model.t [@@deriving sexp_of]

  val to_string : t -> string

  val eval : ?apply_model_completion:bool -> t -> 's Expr.t -> 's Expr.t option
  val eval_exn : ?apply_model_completion:bool -> t -> 's Expr.t -> 's Expr.t

  val const_interp : t -> (Nothing.t, 's) Function_declaration.t -> 's Expr.t option
  val const_interp_exn : t -> (Nothing.t, 's) Function_declaration.t -> 's Expr.t

  val const_interp_e : t -> 's Expr.t -> 's Expr.t option
  val const_interp_e_exn : t -> 's Expr.t -> 's Expr.t

  val const_decls : t -> Function_declaration.packed list
  val func_decls : t -> Function_declaration.packed list

  val num_consts : t -> int
  val num_funcs : t -> int

  module Native : Native with type t := t and type native := Z3native.model
end

module type Function_declaration = sig
  open! Types

  include module type of struct include Types.Function_declaration end
  include sig type (_,_) t [@@deriving sexp_of] end with type ('a,'b) t := ('a,'b) t
      
  val context : (_,_) t -> Context.t

  val domain : ('d, _) t -> 'd Sort.List.t
  val range : (_, 'body) t -> 'body Sort.t

  val name : (_,_) t -> Symbol.t

  val is_nullary_exn
    : ('a, 'body) t
    -> ('a, Nothing.t) Type_equal.t

  val sort_kind
    : ('a, 'body) t 
    -> 'a S.lambda_instance * 'body S.kind

  val same_witness
    :  ('a, 'a_body) t
    -> ('b, 'b_body) t
    -> ('a * 'a_body, 'b * 'b_body) Type_equal.t option

  val app
    :  ('a, 'body) t
    -> 'a Expr.List.t
    -> 'body Expr.t
end

module type Function_interpretation = sig
  open! Types

  type t = Types.Function_interpretation.t [@@deriving sexp_of]
end

module type Solver_result = sig

  type 'a t = 'a Types.Solver_result.t =
    | Unsatisfiable
    | Unknown of string
    | Satisfiable of 'a
  [@@deriving sexp]

  val satisfiable_exn : 'a t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type Generic_solver = sig
  open Types

  type t [@@deriving sexp_of]
  type native

  val context : t -> Context.t

  val create
    :  ?timeout:Time_ns.Span.t
    -> Context.t
    -> t

  val stats : t -> Statistics.t

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

  include Generic_solver
    with type t = Types.Solver.t
     and type native := Z3native.solver
end

module type Optimize = sig
  open Types

  include Generic_solver
    with type t = Types.Optimize.t
     and type native := Z3native.optimize

  module Goal : sig
    type 's t

    val lower : 's t -> 's Expr.t
    val upper : 's t -> 's Expr.t
  end

  val add_soft : t -> 's Expr.t -> weight:int -> Symbol.t -> 's Goal.t

end

module type Symbol = sig
  open Types

  type t = Symbol.t [@@deriving sexp_of]

  val context : t -> Context.t

  val of_int : Context.t -> int -> t
  val of_string : Context.t -> string -> t

  module Native : Native with type t := t and type native := Z3native.symbol
end

module type Boolean_ops = sig
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
  open Types

  val create_sort : Context.t -> S.bool Sort.t

  module With_context :
    Boolean_ops with type 'a wrap := Context.t -> 'a

  include Boolean_ops with type 'a wrap := 'a

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
    -> (Symbol.t * Sort.packed) list
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

  val lambda_const
    :  'a Expr.List.t
    -> body:'body Expr.t
    -> ('a, 'body) S.array t

  val lambda_single
    :  Symbol.t
    -> 'a Sort.t
    -> body:'s Expr.t
    -> ('a * Nothing.t, 's) S.array t

  val lambda_single_const
    :  'a Expr.t
    -> body:'s Expr.t
    -> ('a * Nothing.t, 's) S.array t
end

module type Pattern = sig
  open Types

  include module type of struct include Types.Pattern end
  include sig type 'a t [@@deriving sexp_of] end with type 'a t := 'a t

  val create : 's Expr.t list -> 's t
end

module type ZArray = sig
  open Types

  type ('a, 'b) t = ('a, 'b) S.array Expr.t

  val domain : ('a, 'b) S.array Sort.t -> 'a Sort.List.t
  val range : (_,'b) S.array Sort.t -> 'b Sort.t

  val select_single : ('a * Nothing.t, 'b) t -> 'a Expr.t -> 'b Expr.t

  val select : ('a, 'b) t -> 'a Expr.List.t -> 'b Expr.t
end

module type ZTuple = sig
  open Types

  module Symbol_sort_list
    : Typed_list.Lambda_lower
      with type 'arg Inner.t = Symbol.t * 'arg Sort.t
       and module Lambda_higher := Lambda_higher

  module Field_accessor 
    : Higher_kinded_short.S2
      with type ('arg, 'extra) t = ('extra * Nothing.t, 'arg) Function_declaration.t

  module Field_accessor_list
    : Typed_list.Lambda_lower2
      with module Inner := Field_accessor
       and module Lambda_higher := Lambda_higher

  type 'a t = 'a S.tuple S.datatype Expr.t

  val create_sort
    :  Symbol.t
    -> 'a Symbol_sort_list.t
    -> ( ('a S.tuple S.datatype as 'res) Sort.t
         * ('a,'res) Function_declaration.t
         * ('a,'res) Field_accessor_list.t
         )

  val accessors
    : ('a S.tuple S.datatype as 'res) Sort.t
    -> ('a, 'res) Field_accessor_list.t

  val constructor
    : ('a S.tuple S.datatype as 'res) Sort.t
    -> ('a,'res) Function_declaration.t
end

module type Statistics = sig
  open Types
  type t = Statistics.t

  val to_string : t -> string
end

module type Z3i_internal = sig
  module Ast : Ast 
  module Context : Context 
  module Expr : Expr 

  module Sort : Sort 
  module Bitvector : Bitvector 
  module Model : Model 
  module Function_interpretation : Function_interpretation 
  module Function_declaration : Function_declaration 
  module Solver : Solver

  module Solver_result : Solver_result
    with type 'a t = 'a Types.Solver_result.t

  module Statistics : Statistics

  module Optimize : Optimize 
  module Symbol : Symbol 
  module Boolean : Boolean 
  module Quantifier : Quantifier 
  module Pattern : Pattern 
  module ZArray : ZArray 

  module ZTuple
    : ZTuple 

  module S = S
end

module type Mux = sig
  open Types

  type t =
    { selector : S.bv Expr.t
    ; output : S.bv Expr.t
    ; assertions : S.bool Expr.t list
    ; length : int
    }

  val create_with_selector_exn
    :  selector: S.bv Expr.t
    -> S.bv Expr.t list
    -> t

  val create
    :  selector_symbol:Symbol.t
    -> S.bv Expr.t list
    -> t
  
  val selector_at : t -> int -> S.bool Expr.t

  val constraints : t -> S.bool Expr.t

  val model_selector : t -> Model.t -> int option
end

module type Symbol_builder = sig
  open Types

  type t

  val create : ?first_symbol:int -> ?use_name:bool -> Context.t -> t
  val sym : ?name:string -> t -> Symbol.t
  val sym_int : t -> int
  val context : t -> Context.t
end

module type Z3i = sig
  include Z3i_internal

  module Mux : Mux 
  module Symbol_builder : Symbol_builder 

  module Typed_list = Typed_list

end
