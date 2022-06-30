open! Core

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

  val to_list : _ t -> int * Inner.packed list
  val of_packed_list : Inner.packed list -> packed
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
end
