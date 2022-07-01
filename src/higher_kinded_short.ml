
module type S = sig
  include Higher_kinded.S

  val (!>) : 'a t -> ('a -> higher_kinded) Higher_kinded.t
  val (!<) : ('a -> higher_kinded) Higher_kinded.t -> 'a t
end

module Make1(T : Core.T1) = struct
  include Higher_kinded.Make(T)

  let (!>) = inject
  let (!<) = project
end
