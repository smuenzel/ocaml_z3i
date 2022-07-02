open! Core

(* CR smuenzel: Simple0 is unused ? *)
module type Simple0_inner = sig
  include T1
  type packed
  val pack : _ t -> packed
end

module type Simple0_t1 = sig
  module Inner : T1
  type _ t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next t -> ('arg * 'next) t
end

module type Simple0 = sig
  module Inner : Simple0_inner
  include Simple0_t1 with module Inner := Inner

  val to_list : _ t -> int * Inner.packed list
end

module type Simple_inner = sig
  include T2
  type 'extra packed
  val pack : (_,'extra) t -> 'extra packed
  val same_witness : ('a, 'c) t -> ('b, 'c) t -> ('a, 'b) Type_equal.t option
end

module type Simple_t2 = sig
  module Inner : T2
  type (_,_) t =
    | [] : (Nothing.t, _) t
    | (::) : ('arg, 'extra) Inner.t * ('next, 'extra) t -> (('arg * 'next), 'extra) t
  type _ packed =
    | S : (_, 'extra) t -> 'extra packed [@@unboxed]
end

module type Simple = sig
  module Inner : Simple_inner
  include Simple_t2 with module Inner := Inner

  val to_list : (_, 'extra) t -> int * 'extra Inner.packed list

  val to_uniform_list_exn : ('a * 'next, 'extra) t -> ('a, 'extra) Inner.t list
end

module type Pack_inner = sig
  type raw
  type _ t = private raw
  type packed = | T : _ t -> packed [@@unboxed]
end

module type Lambda_list_t1 = sig
  module Inner : T1

  type 'inputs t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next_args t -> ('arg * 'next_args) t

  type packed = | T : _ t -> packed [@@unboxed]
end

module type Lambda_list = sig
  module Inner : Pack_inner
  include Lambda_list_t1 with module Inner := Inner

  val to_list_mapi : _ t -> f:(int -> Inner.packed -> 'a) -> int * 'a list
  val to_list_map : _ t -> f:(Inner.packed -> 'a) -> int * 'a list
  val to_list : _ t -> int * Inner.packed list
  val of_packed_list : Inner.packed list -> packed
end

module Lambda_higher_types = struct
  type ('inner, 'inputs) t =
    | [] : (_, Nothing.t) t
    | (::) : ('arg -> 'inner) Higher_kinded.t * ('inner, 'next_args) t -> ('inner, 'arg * 'next_args) t

  type 'inner packed =
    | L : ('inner, 'inputs) t -> 'inner packed
  [@@unboxed]

  type ('inner1, 'inner2) f_map =
    { f : 'arg . ('arg -> 'inner1) Higher_kinded.t -> ('arg -> 'inner2) Higher_kinded.t
    } [@@unboxed]

  type ('inner1, 'out) f_to_list =
    { f : 'arg . ('arg -> 'inner1) Higher_kinded.t -> 'out
    } [@@unboxed]
end

module type Lambda_higher = sig
  include module type of struct include Lambda_higher_types end

  val map
    :  ('inner1, 'inner2) f_map
    -> ('inner1, 'inputs) t
    -> ('inner2, 'inputs) t

  val to_list_map
    :  ('inner1, 'out) f_to_list
    -> ('inner1, 'inputs) t
    -> 'out list
end

module type Lambda_lower = sig
  module Lambda_higher : Lambda_higher
  module Inner : Higher_kinded.S

  type 'inputs t =
    | [] : Nothing.t t
    | (::) : 'arg Inner.t * 'next_args t -> ('arg * 'next_args) t

  type packed = | L : _ t -> packed [@@unboxed]

  val higher : 'inputs t -> (Inner.higher_kinded, 'inputs) Lambda_higher.t
  val lower : (Inner.higher_kinded, 'inputs) Lambda_higher.t -> 'inputs t
end

module type Lambda_lower2 = sig
  module Lambda_higher : Lambda_higher
  module Inner : Higher_kinded.S2

  type ('inputs, 'extra) t =
    | [] : (Nothing.t, _) t
    | (::) : ('arg, 'extra) Inner.t * ('next_args, 'extra) t -> (('arg * 'next_args), 'extra) t

  type 'extra packed = | L : (_, 'extra) t -> 'extra packed [@@unboxed]

  val higher : ('inputs, 'extra) t -> ('extra -> Inner.higher_kinded, 'inputs) Lambda_higher.t
  val lower : ('extra -> Inner.higher_kinded, 'inputs) Lambda_higher.t -> ('inputs, 'extra) t
end


module type Typed_list = sig
  module type Simple0_inner = Simple0_inner
  module type Simple0_t1 = Simple0_t1
  module type Simple0 = Simple0
  module Make_simple0(Inner : Simple0_inner) : Simple0 with module Inner := Inner

  module type Simple_inner = Simple_inner
  module type Simple_t2 = Simple_t2
  module type Simple = Simple
  module Make_simple(Inner : Simple_inner) : Simple with module Inner := Inner

  module type Pack_inner = Pack_inner
  module type Lambda_list_t1 = Lambda_list_t1
  module type Lambda_list = Lambda_list
  module Make_lambda(Inner : Pack_inner) : Lambda_list with module Inner = Inner

  module Lambda_higher : Lambda_higher
  module type Lambda_lower = Lambda_lower
  module Make_lambda_lower(Inner : Higher_kinded.S)
    : Lambda_lower with module Inner = Inner
                    and module Lambda_higher := Lambda_higher

  module type Lambda_lower2 = Lambda_lower2
  module Make_lambda_lower2(Inner : Higher_kinded.S2)
    : Lambda_lower2 with module Inner = Inner
                     and module Lambda_higher := Lambda_higher

end
