open Core
open Z3i_internal

type t =
  { selector : S.bv Expr.t
  ; output : S.bv Expr.t
  ; assertions : S.bool Expr.t list
  }

let create
    ~selector_symbol
    inputs
  =
  let head = List.hd_exn inputs in
  let context = Expr.context head in
  let input_sort = Expr.sort head in
  let length = List.length inputs in
  let selector_sort = Sort.create_bitvector context ~bits:length in
  let selector = Expr.const selector_symbol selector_sort in
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
  }
