open! Core

let%expect_test "tuple (raw)" =
  let open Z3i in
  let c = Context.create () in
  let s = Symbol_builder.create c in
  let sort_symbol = Symbol_builder.sym s in
  let des0 = Symbol_builder.sym s in
  let des1 = Symbol_builder.sym s in
  let bool_s = Boolean.create_sort c in
  let bv_s = Bitvector.create_sort c ~bits:8 in
  let sort =
    Z3.Tuple.mk_sort c sort_symbol [ des0; des1 ] [ Sort.to_raw bool_s; Sort.to_raw bv_s ]
  in
  Sort.sort_kind (Sort.unsafe_of_raw sort)
  |> [%sexp_of: _ Sort.Kind.t]
  |> print_s;
  [%expect {| (Datatype (Tuple (Bool Bv))) |}];
  Z3.Tuple.get_field_decls sort
  |> List.iter
    ~f:(fun fdecl ->
        Function_declaration.sort_kind (Function_declaration.unsafe_of_raw fdecl)
        |> [%sexp_of : _ Sort.Kind.lambda_instance * _ Sort.Kind.t]
        |> print_s
      );
  [%expect {|
    (((Datatype (Tuple (Bool Bv)))) Bool)
    (((Datatype (Tuple (Bool Bv)))) Bv) |}];
  Z3.Tuple.get_mk_decl sort
  |> Function_declaration.unsafe_of_raw
  |> Function_declaration.sort_kind 
  |> [%sexp_of : _ Sort.Kind.lambda_instance * _ Sort.Kind.t]
  |> print_s;
  [%expect {| ((Bool Bv) (Datatype (Tuple (Bool Bv)))) |}]

let%expect_test "tuple" =
  let open Z3i in
  let c = Context.create () in
  let s = Symbol_builder.create c in
  let sort_symbol = Symbol_builder.sym s in
  let des0 = Symbol_builder.sym s in
  let des1 = Symbol_builder.sym s in
  let bool_s = Boolean.create_sort c in
  let bv_s = Bitvector.create_sort c ~bits:8 in
  let des0_var_s = Symbol_builder.sym s in
  let des1_var_s = Symbol_builder.sym s in
  let des0_var = Expr.const des0_var_s bool_s in
  let des1_var = Expr.const des1_var_s bv_s in
  let sort, constructor, accessors =
    ZTuple.create_sort
      sort_symbol
      [ des0, bool_s
      ; des1, bv_s
      ]
  in
  Sort.sort_kind sort
  |> [%sexp_of: _ Sort.Kind.t]
  |> print_s;
  [%expect {| (Datatype (Tuple (Bool Bv))) |}];
  let [ acc_des0; acc_des1 ] = accessors in
  let v = Function_declaration.app constructor [ des0_var; des1_var ] in
  print_s [%message "" (v : _ Expr.t)];
  [%expect {| (v "(k!0 k!3 k!4)") |}];
  let v = Expr.simplify v in
  let retr_0 = Function_declaration.app acc_des0 [ v ] in
  let retr_1 = Function_declaration.app acc_des1 [ v ] in
  print_s [%message ""
      (retr_0 : _ Expr.t)
      (retr_1 : _ Expr.t)
  ];
  [%expect {| ((retr_0 "(k!1 (k!0 k!3 k!4))") (retr_1 "(k!2 (k!0 k!3 k!4))")) |}];
  let retr_0 = Expr.simplify retr_0 in
  let retr_1 = Expr.simplify retr_1 in
  print_s [%message ""
      (retr_0 : _ Expr.t)
      (retr_1 : _ Expr.t)
  ];
  [%expect {| ((retr_0 k!3) (retr_1 k!4)) |}];
  ()

let%expect_test "popcount" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Bitvector.create_sort c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.popcount value) (Bitvector.Numeral.int sort 1)
  ; Boolean.not (Bitvector.is_power_of_two value)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| Unsatisfiable |}]

let%expect_test "shift_left" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Bitvector.create_sort c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.shift_left (Bitvector.Numeral.int sort 1) ~count:value)
      (Bitvector.Numeral.int sort 8)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| (Satisfiable ((define-fun k!0 () (_ BitVec 64) #x0000000000000003))) |}]

let%expect_test "parity" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Bitvector.create_sort c ~bits:length in
  let value = Expr.const_i 0 sort in
  [ Boolean.eq (Bitvector.parity value) (Bitvector.Numeral.bit1 c)
  ; Boolean.eq (Bitvector.popcount value) (Bitvector.Numeral.int sort 3)
  ]
  |> Solver.add_list s;
  Solver.check_current_and_get_model s
  |> [%sexp_of: Model.t Solver_result.t]
  |> print_s;
  [%expect {| (Satisfiable ((define-fun k!0 () (_ BitVec 64) #x020c000000000000))) |}]

let%expect_test "sign" =
  let open Z3i in
  let c = Context.create () in
  let s = Solver.create c in
  let length = 64 in
  let sort = Bitvector.create_sort c ~bits:length in
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
  let sort = Bitvector.create_sort c ~bits:length in
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
     ((define-fun k!0 () (_ BitVec 64) #x7ff5ffff07fe3ff5)
      (define-fun k!1 () (_ BitVec 64) #x7f43ffff027ffc96))) |}]

let%expect_test "simplify" =
  let open Z3i in
  let c = Context.create () in
  let length = 1 in
  let sort = Bitvector.create_sort c ~bits:length in
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
  let sort = Bitvector.create_sort c ~bits:length in
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
  let length = 64 in
  let sort = Bitvector.create_sort c ~bits:length in
  let s10 = Symbol.of_int c 10 in
  let s11 = Symbol.of_int c 11 in
  let s12 = Symbol.of_int c 12 in
  let lambda =
    let cs10 = Expr.const s10 sort in
    Quantifier.lambda_single_const
      cs10
      ~body:
        (Bitvector.add
           (Expr.const s10 sort)
           (Bitvector.Numeral.int sort 4)
        )
  in
  let lambda2 =
    let cs11 = Expr.const s11 sort in
    let cs12 = Expr.const s12 sort in
    Quantifier.lambda_const
        [ cs11
        ; cs12
        ]
      ~body:
        (Bitvector.add
           (Expr.const s11 sort)
           (Expr.const s12 sort)
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
    (Array (domain (Bv)) (range Bv)) |}]
  ;
  Expr.sort (Quantifier.to_expr lambda2)
  |> Sort.sort_kind
  |> Sort.Kind.sexp_of_t [%sexp_of: _]
  |> print_s;
  [%expect {|
    (Array (domain (Bv Bv)) (range Bv)) |}]
  ;
  let value0 = Expr.const_i 0 sort in
  let sel0 =
    ZArray.select_single
      (Quantifier.to_expr lambda)
      value0
  in
  sel0
  |> Expr.to_string
  |> print_endline;
  sel0
  |> Expr.simplify
  |> Expr.to_string
  |> print_endline;
  [%expect {|
    (select (lambda ((x!1 (_ BitVec 64))) (bvadd x!1 #x0000000000000004)) k!0)
    (bvadd #x0000000000000004 k!0) |}];
  ();
  let sel20 =
    ZArray.select
      (Quantifier.to_expr lambda2)
      [ value0
      ; value0
      ]
  in
  sel20
  |> Expr.to_string
  |> print_endline;
  sel20
  |> Expr.simplify
  |> Expr.to_string
  |> print_endline;
  [%expect {|
    (select (lambda ((x!1 (_ BitVec 64)) (x!2 (_ BitVec 64))) (bvadd x!1 x!2))
            k!0
            k!0)
    (bvmul #x0000000000000002 k!0) |}]
