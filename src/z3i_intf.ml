open Core

module type Types = sig
  module Context : T
  module Expr : T
  module Sort : T
  module Symbol : T
  module Model : T
  module Optimize : T
  module Solver : T
  module Solver_result : sig
    type 'a t =
      | Unsatisfiable
      | Unknown of string
      | Satisfiable of 'a
  end
end

module type Native = sig
  type t
  type native
  val to_native : t -> native
  val unsafe_of_native : native -> t
end

module type Expr = sig
  module Types : Types
  open Types

  type t = Types.Expr.t [@@deriving sexp_of]

  val context : t -> Context.t
  val sort : t -> Sort.t

  val is_numeral : t -> bool

  val to_string : t -> string
  val numeral_to_binary_string_exn : t -> string
  val numeral_to_binary_array_exn : t -> bool array

  val const : Symbol.t -> Sort.t -> t
  val const_s : string -> Sort.t -> t
  val const_i : int -> Sort.t -> t
  val mk_const : Context.t -> Symbol.t -> Sort.t -> t
  val mk_numeral_int : Context.t -> int -> Sort.t -> t

  module Native : Native with type t := t and type native := Z3native.ast
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

  type t = Types.Sort.t

  val context : t -> Context.t

  val create_bitvector : Context.t -> bits:int -> t

  module Native : Native with type t := t and type native := Z3native.sort
end

module type Bitvector = sig
  module Types : Types
  open Types

  val is_bv : Expr.t -> bool
  val size : Sort.t -> int

  val and_ : Expr.t -> Expr.t -> Expr.t
  val or_ : Expr.t -> Expr.t -> Expr.t
  val xor : Expr.t -> Expr.t -> Expr.t
  val nand : Expr.t -> Expr.t -> Expr.t
  val nor : Expr.t -> Expr.t -> Expr.t
  val xnor : Expr.t -> Expr.t -> Expr.t

  val neg : Expr.t -> Expr.t
  val add : Expr.t -> Expr.t -> Expr.t
  val sub : Expr.t -> Expr.t -> Expr.t

  val concat : Expr.t -> Expr.t -> Expr.t
  val repeat : Expr.t -> count:int -> Expr.t
  val extract : Expr.t -> high:int -> low:int -> Expr.t
  val extract_single : Expr.t -> int -> Expr.t
  val zero_extend : Expr.t -> extra_zeros:int -> Expr.t
  val sign_extend : Expr.t -> extra_bits:int -> Expr.t
  val rotate_left_const : Expr.t -> int -> Expr.t

  val popcount : ?result_bit_size:int -> Expr.t -> Expr.t

  val is_zero : Expr.t -> Expr.t
  val is_power_of_two : Expr.t -> Expr.t
  val is_power_of_two_or_zero : Expr.t -> Expr.t

  module Numeral : sig
    val bool : Context.t -> bool list -> Expr.t

    val int : Sort.t -> int -> Expr.t
    val int_e : Expr.t -> int -> Expr.t
  end

end

module type Model = sig
  module Types : Types
  open Types

  type t = Types.Model.t [@@deriving sexp_of]

  val to_string : t -> string

  val eval : t -> Expr.t -> apply_model_completion:bool -> Expr.t option

  val const_interp_e : t -> Expr.t -> Expr.t option

  module Native : Native with type t := t and type native := Z3native.model
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

  val add_list : t -> Expr.t list -> unit
  val add : t -> Expr.t -> unit

  val check_and_get_model : t -> Expr.t list -> Model.t Solver_result.t
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
    type t

    val lower : t -> Expr.t
    val upper : t -> Expr.t
  end

  val add_soft : t -> Expr.t -> weight:int -> Symbol.t -> Goal.t

end

module type Symbol = sig
  module Types : Types
  open Types

  type t = Symbol.t

  val of_int : Context.t -> int -> t

  module Native : Native with type t := t and type native := Z3native.symbol
end

module type Boolean_ops = sig
  module Types : Types
  open Types
  type 'a wrap
  val and_ : (Expr.t list -> Expr.t) wrap
  val not : (Expr.t -> Expr.t) wrap
  val eq : (Expr.t -> Expr.t -> Expr.t) wrap
  val neq : (Expr.t -> Expr.t -> Expr.t) wrap
end

module type Boolean = sig
  module Types : Types
  open Types

  module With_context :
    Boolean_ops with type 'a wrap := Context.t -> 'a and module Types := Types

  include Boolean_ops with type 'a wrap := 'a and module Types := Types
end

module rec Types : Types
  with type Context.t = Z3.context
   and type Sort.t = Z3.Sort.sort
   and type Expr.t = Z3.Expr.expr
   and type Symbol.t = Z3.Symbol.symbol
   and type Model.t = Z3.Model.model
   and type Solver.t = Z3.Solver.solver
   and type Optimize.t = Z3.Optimize.optimize
  = Types

module type Z3i = sig
  module Context : Context with module Types := Types
  module Expr : Expr with module Types := Types
  module Sort : Sort with module Types := Types
  module Bitvector : Bitvector with module Types := Types
  module Model : Model with module Types := Types
  module Solver : Solver
    with module Types := Types
  module Solver_result : Solver_result
    with module Types := Types
     and type 'a t = 'a Types.Solver_result.t
  module Optimize : Optimize with module Types := Types
  module Symbol : Symbol with module Types := Types
  module Boolean : Boolean with module Types := Types

end
