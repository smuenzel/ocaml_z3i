open! Core

let%expect_test "popcount" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.popcount value) (Bitvector.Numeral.int sort 1)
  ; Boolean.not (Bitvector.is_power_of_two value)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| Unsatisfiable |}]

let%expect_test "parity" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.parity value) (Bitvector.Numeral.bit1 c)
  ; Boolean.eq (Bitvector.popcount value) (Bitvector.Numeral.int sort 3)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| (Satisfiable ((define-fun k!0 () (_ BitVec 64) #x0240000000000001))) |}]

let%expect_test "sign" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.sign value) (Bitvector.Numeral.bit1 c)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| (Satisfiable ((define-fun k!0 () (_ BitVec 64) #x8000000000000000))) |}]

let%expect_test "add overflow" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value0 = Expr.const_i 0 sort in
  let value1 = Expr.const_i 1 sort in
  [ Bitvector.is_add_overflow ~signed:true value0 value1
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {|
    (Satisfiable
     ((define-fun k!0 () (_ BitVec 64) #x7ffffffffffffffe)
      (define-fun k!1 () (_ BitVec 64) #x7ffffffffbfffffd))) |}]

let%expect_test "simplify" =
  let open Z3i in
  let c = Context.create () in
  let length = 1 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value0 = Expr.const_i 0 sort in
  Bitvector.is_not_zero value0
  |> Boolean.not
  |> [%sexp_of: _ Expr.t]
  |> print_s;
  [%expect {|
    "(not (not (= k!0 #b0)))" |}];
  Bitvector.is_not_zero value0
  |> Boolean.not
  |> Expr.simplify
  |> [%sexp_of: _ Expr.t]
  |> print_s;
  [%expect {|
    "(= k!0 #b0)" |}];
  ()

let%expect_test "mux" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let value0 = Expr.const_i 0 sort in
  let value1 = Expr.const_i 1 sort in
  let minus_01 = Bitvector.sub value0 value1 in
  let minus_10 = Bitvector.sub value1 value0 in
  let selector_symbol = Symbol.of_string c "sel" in
  let ({ Mux. selector = _ ; output; assertions; length = _ } as mux) =
    Mux.create ~selector_symbol [ minus_01; minus_10 ]
  in
  let select_minus_01 = Mux.selector_at mux 0 in
  let select_minus_10 = Mux.selector_at mux 1 in
  Solver.add_list s assertions;
  let output =
    Quantifier.forall_const
      [ T value0
      ; T value1
      ]
      ~body:
        (Boolean.eq output minus_01)
    |> Quantifier.to_expr
  in
  [ output
  ]
  |> Solver.add_list s;
  let result =
    Solver.check_current_and_get_model s
  in
  result
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {|
    (Satisfiable ((define-fun sel () (_ BitVec 2) #b01))) |}];
  let model = Solver_result.satisfiable_exn result in
  Model.eval_exn model select_minus_01
  |> Expr.to_string |> print_endline;
  [%expect {|
    true |}];
  Model.eval_exn model select_minus_10
  |> Expr.to_string |> print_endline;
  [%expect {|
    false |}];
  ()

let%expect_test "lambda" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Sort.create_bitvector c ~bits:length in
  let s10 = Symbol.of_int c 10 in
  let lambda =
    Quantifier.lambda
      [ s10, T sort ]
      ~body:
        (Bitvector.add
           (Expr.const s10 sort)
           (Bitvector.Numeral.int sort 4)
        )
  in
  Expr.to_ast (Quantifier.to_expr lambda)
  |> Ast.kind
  |> [%sexp_of: Ast.Kind.t]
  |> print_s;
  [%expect {|
    QUANTIFIER_AST |}]
  ;
  Expr.sort (Quantifier.to_expr lambda)
  |> Sort.sort_kind
  |> Sort.Kind.sexp_of_t [%sexp_of: _]
  |> print_s;
  [%expect {|
    (Array (domain Bv) (range Bv)) |}]
  ;
  let value0 = Expr.const_i 0 sort in
  let value1 = Expr.const_i 1 sort in
  [ Bitvector.is_add_overflow ~signed:true value0 value1
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {|
    (Satisfiable
     ((define-fun k!0 () (_ BitVec 64) #x7ffffffffffffffe)
      (define-fun k!1 () (_ BitVec 64) #x7ffffffffbfffffd))) |}]
