open! Core

include Typed_list_intf

module Make_simple0(Inner : Simple0_inner) = struct
  type _ t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next t -> ('arg * 'next) t

  let rec to_list : 'a . 'a t -> int * Inner.packed list =
    fun (type a) (t : a t) ->
    match t with
    | [] -> 0, ([] : _ list)
    | inner :: xs ->
      let length, rest = to_list xs in
      length + 1
    , Inner.pack inner :: rest
end

module Make_simple(Inner : Simple_inner) = struct
  type (_,_) t =
    | [] : (Nothing.t, _) t
    | (::) : ('arg, 'extra) Inner.t * ('next, 'extra) t -> (('arg * 'next), 'extra) t

  type _ packed =
    | S : (_, 'extra) t -> 'extra packed
  [@@unboxed]

  let rec to_list : 'a 'extra . ('a, 'extra) t -> int * 'extra Inner.packed list =
    fun (type a extra) (t : (a, extra) t) ->
    match t with
    | [] -> 0, ([] : _ list)
    | inner :: xs ->
      let length, rest = to_list xs in
      length + 1
    , Inner.pack inner :: rest

  let rec to_uniform_list_exn : 'a 'extra 'next. ('a * 'next, 'extra) t -> ('a, 'extra) Inner.t list =
    fun (type a next extra) (t : (a * next , extra) t) : (a, extra) Inner.t list ->
    match t with
    | inner :: [] ->
      [ inner ]
    | inner :: (x :: _) as xs ->
      let rest = to_uniform_list_exn xs in
      match Inner.same_witness inner x with
      | None -> raise_s [%message "List not uniform"]
      | Some T ->
        inner :: rest
end

module Make_lambda(Inner : Pack_inner) = struct
  module Inner = Inner

  type 'inputs t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next_args t -> ('arg * 'next_args) t

  let rec to_list : 'inputs . 'inputs t -> int * Inner.packed list =
    fun (type inputs) (t : inputs t) ->
    match t with
    | [] -> 0, ([] : _ list)
    | x :: xs ->
      let length, rest = to_list xs in
      length + 1
    , Inner.T x :: rest
end
