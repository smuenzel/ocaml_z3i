open Types_helper
     
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
