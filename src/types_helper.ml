include Types_helper_intf.T

module Make_raw(With_sort : With_sort) : With_raw with module With_sort := With_sort
= struct
  type raw = With_sort.raw
  type 's t = 's With_sort.t
  type packed = With_sort.packed = T : _ t -> packed [@@unboxed]

  external to_raw : _ t -> raw = "%identity"
  external unsafe_of_raw : raw -> _ t = "%identity"

  external to_raw_list : _ t list -> raw list = "%identity"
  external unsafe_of_raw_list : raw list -> _ t list = "%identity"

  external pack : _ t -> packed = "%identity"
  external pack_list : _ t list -> packed list = "%identity"

  external to_raw_unpack_list : packed list -> raw list = "%identity"

  module Higher = Higher_kinded_short.Make1(struct
      type nonrec 'a t = 'a t
    end)
  include Higher

  module List = struct
    include Typed_list.Make_lambda_lower(struct
      type nonrec 'a t = 'a t
      include Higher
    end)

    let rec of_packed_list (list : With_sort.packed list) =
      match list with
      | [] -> L []
      | (T x) :: xs ->
        let L xs = of_packed_list xs in
        L (x :: xs)
  end

  module Native = struct
    type native = With_sort.native

    external to_native : _ t -> native = "%identity"
    external unsafe_of_native : native -> _ t = "%identity"
    external to_native_list : packed list -> native list = "%identity"
    external unsafe_of_native_list : native list -> packed list = "%identity"
  end
end

module Make_raw2(With_sort : With_sort2) : With_raw2 with module With_sort := With_sort
= struct
  type raw = With_sort.raw
  type ('a,'b) t = ('a,'b) With_sort.t
  type packed = With_sort.packed = T : (_, _) t -> packed [@@unboxed]

  external to_raw : (_, _) t -> raw = "%identity"
  external unsafe_of_raw : raw -> (_, _) t = "%identity"

  external to_raw_list : (_, _) t list -> raw list = "%identity"
  external unsafe_of_raw_list : raw list -> (_,_) t list = "%identity"

  external pack : (_,_) t -> packed = "%identity"
  external pack_list : (_,_) t list -> packed list = "%identity"

  external to_raw_unpack_list : packed list -> raw list = "%identity"

  module Higher = Higher_kinded_short.Make2(struct
      type nonrec ('a, 'b) t = ('a, 'b) t
    end)
  include Higher

  module List = struct
    include Typed_list.Make_lambda_lower2(struct
        type nonrec ('a, 'b) t = ('a, 'b) t
        include Higher
      end)
  end

  module Native = struct
    type native = With_sort.native

    external to_native : _ t -> native = "%identity"
    external unsafe_of_native : native -> _ t = "%identity"
    external to_native_list : packed list -> native list = "%identity"
    external unsafe_of_native_list : native list -> packed list = "%identity"
  end
end

