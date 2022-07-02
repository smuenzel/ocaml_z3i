module type With_sort = sig
  type native
  type raw
  type 's t = private raw
  type packed = T : _ t -> packed [@@unboxed]
end

module type With_sort2 = sig
  type native
  type raw
  type (_,_) t = private raw
  type packed = T : (_,_) t -> packed [@@unboxed]
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

module type With_raw = sig
  module With_sort : With_sort
  type raw = With_sort.raw
  type 's t = 's With_sort.t
  type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

  val to_raw : _ t -> raw
  val unsafe_of_raw : raw -> _ t

  val to_raw_list : _ t list -> raw list
  val unsafe_of_raw_list : raw list -> _ t list

  val pack : _ t -> packed
  val pack_list : _ t list -> packed list

  val to_raw_unpack_list : packed list -> raw list

  module Native
    : Native1 with type 's t := 's t
               and type packed := packed
               and type native = With_sort.native

  module Higher : Higher_kinded_short.S with type 'a t := 'a t
  include module type of struct include Higher end

  module List : sig
    include Typed_list.Lambda_lower
      with module Lambda_higher := Typed_list.Lambda_higher
       and type 'a Inner.t := 'a t
       and module Inner := Higher

    val of_packed_list : With_sort.packed list -> packed
  end
end

module Make_raw(With_sort : With_sort) : With_raw with module With_sort := With_sort
= struct
  type raw = With_sort.raw
  type 's t = 's With_sort.t
  type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

  external to_raw : _ t -> raw = "%identity"
  external unsafe_of_raw : raw -> _ t = "%identity"

  external to_raw_list : _ t list -> raw list = "%identity"
  external unsafe_of_raw_list : raw list -> _ t list = "%identity"

  external pack : _ t -> packed = "%identity"
  external pack_list : _ t list -> packed list = "%identity"

  external to_raw_unpack_list : packed list -> raw list = "%identity"

  module Higher = Higher_kinded_short.Make1(struct
      type nonrec 'a t = 'a t
    end)
  include Higher

  module List = struct
    include Typed_list.Make_lambda_lower(struct
      type nonrec 'a t = 'a t
      include Higher
    end)

    let rec of_packed_list (list : With_sort.packed list) =
      match list with
      | [] -> L []
      | (T x) :: xs ->
        let L xs = of_packed_list xs in
        L (x :: xs)
  end

  module Native = struct
    type native = With_sort.native

    external to_native : _ t -> native = "%identity"
    external unsafe_of_native : native -> _ t = "%identity"
    external to_native_list : packed list -> native list = "%identity"
    external unsafe_of_native_list : native list -> packed list = "%identity"
  end
end

module type With_raw2 = sig
  module With_sort : With_sort2
  type raw = With_sort.raw
  type ('a,'b) t = ('a,'b) With_sort.t
  type packed = With_sort.packed = T : (_,_) t -> packed [@@unboxed]

  val to_raw : (_, _) t -> raw
  val unsafe_of_raw : raw -> (_,_) t

  val to_raw_list : (_,_) t list -> raw list
  val unsafe_of_raw_list : raw list -> (_,_) t list

  val pack : (_,_) t -> packed
  val pack_list : (_,_) t list -> packed list

  val to_raw_unpack_list : packed list -> raw list

  module Native : Native2 with type ('a, 'b) t := ('a, 'b) t
                           and type packed := packed
                           and type native = With_sort.native
end

module Make_raw2(With_sort : With_sort2) : With_raw2 with module With_sort := With_sort
= struct
  type raw = With_sort.raw
  type ('a,'b) t = ('a,'b) With_sort.t
  type packed = With_sort.packed = T : (_, _) t -> packed [@@unboxed]

  external to_raw : (_, _) t -> raw = "%identity"
  external unsafe_of_raw : raw -> (_, _) t = "%identity"

  external to_raw_list : (_, _) t list -> raw list = "%identity"
  external unsafe_of_raw_list : raw list -> (_,_) t list = "%identity"

  external pack : (_,_) t -> packed = "%identity"
  external pack_list : (_,_) t list -> packed list = "%identity"

  external to_raw_unpack_list : packed list -> raw list = "%identity"

  module Native = struct
    type native = With_sort.native

    external to_native : _ t -> native = "%identity"
    external unsafe_of_native : native -> _ t = "%identity"
    external to_native_list : packed list -> native list = "%identity"
    external unsafe_of_native_list : native list -> packed list = "%identity"
  end
end

module Ast = struct
  type t = Z3.AST.ast
end

module Context = struct
  type t = Z3.context
end

module Expr = struct
  module rec T : With_sort with type raw = Z3.Expr.expr and type native = Z3native.ast = T
  include T
  include Make_raw(T)
end

module Sort = struct
  module rec T : With_sort with type raw = Z3.Sort.sort and type native = Z3native.sort = T
  include T
  include Make_raw(T)
end

module Symbol = struct
  type t = Z3.Symbol.symbol
end

module Model = struct
  type t = Z3.Model.model
end

module Function_declaration = struct
  module rec T : With_sort2
    with type raw = Z3.FuncDecl.func_decl
     and type native = Z3native.func_decl
    = T
  include T
  include Make_raw2(T)
end

module Function_interpretation = struct
  type t = Z3.Model.FuncInterp.func_interp
end

module Solver = struct
  type t = Z3.Solver.solver
end

module Optimize = struct
  type t = Z3.Optimize.optimize
end

module Solver_result = struct
  type 'a t =
    | Unsatisfiable
    | Unknown of string
    | Satisfiable of 'a
end

module Quantifier = struct
  module rec T : With_sort with type raw = Z3.Quantifier.quantifier
                            and type native = Z3native.ast
    = T
  include T
  include Make_raw(T)
end

module Pattern = struct
  module rec T : With_sort with type raw = Z3.Quantifier.Pattern.pattern
                            and type native = Z3native.pattern
    = T
  include T
  include Make_raw(T)
end
