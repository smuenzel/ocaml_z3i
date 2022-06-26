open Z3i_internal

type t =
  { mutable next : int
  ; context : Context.t
  ; use_name : bool
  }

let create
  ?(first_symbol=0)
  ?(use_name = true)
  context
  =
  { next = first_symbol
  ; context
  ; use_name
  }

let sym_int t =
  let s = t.next in
  t.next <- succ s;
  s

let sym ?name t =
  let i = sym_int t in
  match name with
  | None ->
    Symbol.of_int t.context i
  | Some _ when t.use_name = false ->
    Symbol.of_int t.context i
  | Some name ->
    Symbol.of_string t.context (Printf.sprintf "%s_%i" name i)

let context t = t.context
