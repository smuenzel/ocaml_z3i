open Z3i_internal

type t =
  { mutable next : int
  ; context : Context.t
  }

let create
  ?(first_symbol=0)
  context
  =
  { next = first_symbol
  ; context
  }

let sym_int t =
  let s = t.next in
  t.next <- succ s;
  s

let sym t =
  let i = sym_int t in
  Symbol.of_int t.context i
