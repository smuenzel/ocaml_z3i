open! Core

include Typed_list_intf

module Lambda_higher = struct
  include Lambda_higher_types

  let [@tail_mod_cons] rec iter
    : 'inner1 'inputs .
      'inner1 f_iter
      -> ('inner1, 'inputs) t
      -> unit
    =
    fun (type inner1 inputs)
      ({ f } : inner1 f_iter)
      (t : (inner1, inputs) t)
      : unit
      ->
        match t with
        | [] -> ()
        | x :: xs ->
          f x;
          iter { f } xs

  let [@tail_mod_cons] rec map
    : 'inner1 'inner2 'inputs .
      ('inner1, 'inner2) f_map
      -> ('inner1, 'inputs) t
      -> ('inner2, 'inputs) t
    =
    fun (type inner1 inner2 inputs)
      ({ f } : (inner1, inner2) f_map)
      (t : (inner1, inputs) t)
      : (inner2, inputs) t
      ->
        match t with
        | [] -> []
        | x :: xs ->
          let x = f x in
          x :: map { f } xs

  let [@tail_mod_cons] rec map2
    : 'inner1a 'inner1b 'inner2 'inputs .
      ('inner1a, 'inner1b, 'inner2) f_map2
      -> ('inner1a, 'inputs) t
      -> ('inner1b, 'inputs) t
      -> ('inner2, 'inputs) t
    =
    fun (type inner1a inner1b inner2 inputs)
      ({ f } : (inner1a, inner1b, inner2) f_map2)
      (t1 : (inner1a, inputs) t)
      (t2 : (inner1b, inputs) t)
      : (inner2, inputs) t
      ->
        match t1, t2 with
        | [], [] -> []
        | x :: xs, y :: ys ->
          let r = f x y in
          r :: map2 { f } xs ys

  let [@tail_mod_cons] rec to_list_map
    : 'inner1 'out 'inputs .
      ('inner1, 'out) f_to_list
      -> ('inner1, 'inputs) t
      -> 'out list
    =
    fun (type inner1 out inputs)
      ({ f } : (inner1, out) f_to_list)
      (t : (inner1, inputs) t)
      : out list
      ->
        match t with
        | [] -> []
        | x :: xs ->
          let x = f x in
          x :: to_list_map { f } xs

  let [@tail_mod_cons] rec to_list_map2
    : 'inner1a 'inner1b 'out 'inputs .
      ('inner1a, 'inner1b, 'out) f_to_list2
      -> ('inner1a, 'inputs) t
      -> ('inner1b, 'inputs) t
      -> 'out list
    =
    fun (type inner1a inner1b out inputs)
      ({ f } : (inner1a, inner1b, out) f_to_list2)
      (t1 : (inner1a, inputs) t)
      (t2 : (inner1b, inputs) t)
      : out list
      ->
        match t1, t2 with
        | [], [] -> []
        | x :: xs, y :: ys ->
          let r = f x y in
          r :: to_list_map2 { f } xs ys
end

module Make_lambda_lower(Inner : Higher_kinded.S) = struct
  module Inner = Inner

  type 'inputs t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next_args t -> ('arg * 'next_args) t

  type packed = | L : _ t -> packed [@@unboxed]

  external higher : 'inputs t -> (Inner.higher_kinded, 'inputs) Lambda_higher.t = "%identity"
  external lower : (Inner.higher_kinded, 'inputs) Lambda_higher.t -> 'inputs t = "%identity"
end

module Make_lambda_lower2(Inner : Higher_kinded.S2) = struct
  module Inner = Inner

  type ('arg, 'extra) t =
    | [] : (Nothing.t, _) t
    | (::) : ('arg, 'extra) Inner.t * ('next_args, 'extra) t -> (('arg * 'next_args), 'extra) t

  type 'extra packed = | L : (_,'extra) t -> 'extra packed [@@unboxed]

  external higher : ('inputs, 'extra) t -> ('extra -> Inner.higher_kinded, 'inputs) Lambda_higher.t = "%identity"
  external lower : ('extra -> Inner.higher_kinded, 'inputs) Lambda_higher.t -> ('inputs, 'extra) t = "%identity"
end
