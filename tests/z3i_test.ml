open! Core
open! Camlsoup_lib

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

(* {[
     let%expect_test "popcount" =
       let open Z3i in
       let c = Context.create () in
       let s = Solver.create c in
       let length = 64 in
       let sort = Sort.create_bitvector c ~bits:length in
       let value = Expr.const_i 0 sort in
       let popcount2 =
         Bitvector.extract
           ~high:6
           ~low:0
           (Bitvector.neg
              (List.reduce_balanced_exn ~f:Bitvector.add
                 (List.init 64
                    ~f:(fun i ->
                        Bitvector.rotate_left_const value i
                      )
                 )
              ))
       in
       [ Boolean.neq (Bitvector.popcount ~result_bit_size:7 value) popcount2
       ]
       |> Solver.add_list s;
       Solver.check_current_and_get_model s
       |> [%sexp_of: Model.t Solver_result.t]
       |> print_s;
       [%expect {| Unsatisfiable |}]
   ]}
*)
