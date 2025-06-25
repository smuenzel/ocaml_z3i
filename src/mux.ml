(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2022-2025 Stefan Muenzel
 *)

open Core
open Z3i_internal

type t =
  { selector : S.bv Expr.t
  ; output : S.bv Expr.t
  ; assertions : S.bool Expr.t list
  ; length : int
  }

let create_with_selector_exn
    ~selector
    inputs
  =
  let head = List.hd_exn inputs in
  let context = Expr.context head in
  let input_sort = Expr.sort head in
  let length = List.length inputs in
  let selector_sort = Bitvector.create_sort context ~bits:length in
  if not (Sort.equal (Expr.sort selector) selector_sort)
  then raise_s
      [%message "selector sort not equal"
          (Expr.sort selector : _ Sort.t)
          (selector_sort : _ Sort.t)
      ];
  let selector_restrict = Bitvector.Set.has_single_member selector in
  let individuals =
    List.mapi inputs
      ~f:(fun i input ->
          let select =
            Bitvector.broadcast_single
              (Bitvector.extract_single selector i)
              input_sort
          in
          Bitvector.and_
            select
            input
        )
  in
  let output = Bitvector.or_list individuals in
  { selector
  ; assertions = [ selector_restrict ]
  ; output
  ; length
  }

let create
    ~selector_symbol
    inputs
  =
  let head = List.hd_exn inputs in
  let context = Expr.context head in
  let length = List.length inputs in
  let selector_sort = Bitvector.create_sort context ~bits:length in
  let selector = Expr.const selector_symbol selector_sort in
  create_with_selector_exn
    ~selector
    inputs

let selector_at t i =
  Bitvector.extract_single t.selector i
  |> Boolean.of_single_bit_vector

let model_selector_i ~length model selector =
  Model.eval_exn model selector
  |> Bitvector.Numeral.to_binary_array_exn
  |> Array.findi ~f:(fun _i b -> b)
  |> Option.map ~f:(fun (i, _) -> length - i - 1)

let model_selector t model =
  model_selector_i ~length:t.length model t.selector

let constraints t =
  Boolean.and_list t.assertions
