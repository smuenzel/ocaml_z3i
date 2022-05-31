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
