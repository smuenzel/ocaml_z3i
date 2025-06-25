(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2022-2025 Stefan Muenzel
 *)

open! Core

module type S = sig
  include Higher_kinded.S

  val (!>) : 'a t -> ('a -> higher_kinded) Higher_kinded.t
  val (!<) : ('a -> higher_kinded) Higher_kinded.t -> 'a t
end

module Make1(T : Core.T1) : S with type 'a t := 'a T.t = struct
  include Higher_kinded.Make(T)

  let (!>) = inject
  let (!<) = project
end

module type S2 = sig
  include Higher_kinded.S2

  val (!>) : ('a, 'z) t -> ('a -> 'z -> higher_kinded) Higher_kinded.t
  val (!<) : ('a -> 'z -> higher_kinded) Higher_kinded.t -> ('a, 'z) t
end

module Make2(T : Core.T2) = struct
  include Higher_kinded.Make2(T)

  let (!>) = inject
  let (!<) = project
end
