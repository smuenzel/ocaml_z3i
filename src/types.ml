module type With_sort = sig
  type raw
  type 's t = private raw
  type packed = T : _ t -> packed [@@unboxed]
end

module type With_sort2 = sig
  type raw
  type (_,_) t = private raw
  type packed = T : (_,_) t -> packed [@@unboxed]
end

(*
module rec Types : Types
  with type Context.t = Z3.context
   and type Ast.t = Z3.AST.ast
   and type Sort.raw = Z3.Sort.sort
   and type Expr.raw = Z3.Expr.expr
   and type Symbol.t = Z3.Symbol.symbol
   and type Model.t = Z3.Model.model
   and type Function_declaration.raw = Z3.FuncDecl.func_decl
   and type Function_interpretation.t = Z3.Model.FuncInterp.func_interp
   and type Solver.t = Z3.Solver.solver
   and type Optimize.t = Z3.Optimize.optimize
   and type Quantifier.raw = Z3.Quantifier.quantifier
   and type Pattern.raw = Z3.Quantifier.Pattern.pattern
  = Types
   *)

module Ast = struct
  type t = Z3.AST.ast
end

module Context = struct
  type t = Z3.context
end

module Expr = struct
  module rec T : With_sort with type raw = Z3.Expr.expr = T
  include T
  include Higher_kinded_short.Make1(T)
end

module Sort = struct
  module rec T : With_sort with type raw = Z3.Sort.sort = T
  include T
  include Higher_kinded_short.Make1(T)
end

module Symbol = struct
  type t = Z3.Symbol.symbol
end

module Model = struct
  type t = Z3.Model.model
end

module Function_declaration = struct
  module rec T : With_sort2 with type raw = Z3.FuncDecl.func_decl = T
  include T
  include Higher_kinded_short.Make2(T)
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
  module rec T : With_sort with type raw = Z3.Quantifier.quantifier = T
  include T
  include Higher_kinded_short.Make1(T)
end

module Pattern = struct
  module rec T : With_sort with type raw = Z3.Quantifier.Pattern.pattern = T
  include T
  include Higher_kinded_short.Make1(T)
end
