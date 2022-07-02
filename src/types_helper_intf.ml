
module T = struct
  module type With_sort = sig
    type native
    type raw
    type 's t = private raw
    type packed = T : _ t -> packed [@@unboxed]
  end

  module type With_sort2 = sig
    type native
    type raw
    type (_,_) t = private raw
    type packed = T : (_,_) t -> packed [@@unboxed]
  end

  module type Native = sig
    type t
    type native
    val to_native : t -> native
    val to_native_list : t list -> native list
    val unsafe_of_native : native -> t
  end

  module type Native1 = sig
    type _ t
    type packed
    type native
    val to_native : _ t -> native
    val unsafe_of_native : native -> _ t
    val to_native_list : packed list -> native list
    val unsafe_of_native_list : native list -> packed list
  end

  module type Native2 = sig
    type (_,_) t
    type packed
    type native
    val to_native : (_,_) t -> native
    val unsafe_of_native : native -> (_,_) t
    val to_native_list : packed list -> native list
    val unsafe_of_native_list : native list -> packed list
  end

  module type With_raw = sig
    module With_sort : With_sort
    type raw = With_sort.raw
    type 's t = 's With_sort.t
    type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

    val to_raw : _ t -> raw
    val unsafe_of_raw : raw -> _ t

    val to_raw_list : _ t list -> raw list
    val unsafe_of_raw_list : raw list -> _ t list

    val pack : _ t -> packed
    val pack_list : _ t list -> packed list

    val to_raw_unpack_list : packed list -> raw list

    module Native
      : Native1 with type 's t := 's t
                 and type packed := packed
                 and type native = With_sort.native

    module Higher : Higher_kinded_short.S with type 'a t := 'a t
    include module type of struct include Higher end

    module List : sig
      include Typed_list.Lambda_lower
        with module Lambda_higher := Typed_list.Lambda_higher
         and type 'a Inner.t := 'a t
         and module Inner := Higher

      val of_packed_list : With_sort.packed list -> packed
    end
  end

  module type With_raw2 = sig
    module With_sort : With_sort2
    type raw = With_sort.raw
    type ('a,'b) t = ('a,'b) With_sort.t
    type packed = With_sort.packed = T : (_,_) t -> packed [@@unboxed]

    val to_raw : (_, _) t -> raw
    val unsafe_of_raw : raw -> (_,_) t

    val to_raw_list : (_,_) t list -> raw list
    val unsafe_of_raw_list : raw list -> (_,_) t list

    val pack : (_,_) t -> packed
    val pack_list : (_,_) t list -> packed list

    val to_raw_unpack_list : packed list -> raw list

    module Higher : Higher_kinded_short.S2 with type ('a, 'b) t := ('a, 'b) t
    include module type of struct include Higher end

    module List : sig
      include Typed_list.Lambda_lower2
        with module Lambda_higher := Typed_list.Lambda_higher
         and type ('a, 'b) Inner.t := ('a, 'b) t
         and module Inner := Higher
    end

    module Native : Native2 with type ('a, 'b) t := ('a, 'b) t
                             and type packed := packed
                             and type native = With_sort.native
  end

end

module type Types_helper = sig
  include module type of struct include T end

  module Make_raw(With_sort : With_sort) : With_raw with module With_sort := With_sort
  module Make_raw2(With_sort : With_sort2) : With_raw2 with module With_sort := With_sort
end
