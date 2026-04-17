(**************************************************************************)
(*  This file is part of HQbricks.                                        *)
(*                                                                        *)
(*  Copyright (C) 2026                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    Université Paris-Saclay                                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.0.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3.0                 *)
(*  for more details (enclosed in the file LICENSE).                      *)
(*                                                                        *)
(**************************************************************************)

module Dyadic = struct
  type t = { num : Z.t; den_pow : int }

  let zero = { num = Z.zero; den_pow = 0 }

  let reduce d =
    if d.num = Z.zero || d.den_pow <= 0 then zero
    else
      let shift = Int.min (Z.trailing_zeros d.num) d.den_pow in
      { num = Z.shift_right d.num shift; den_pow = d.den_pow - shift }

  let add d1 d2 =
    let den_pow = Int.max d1.den_pow d2.den_pow in
    let p1 = den_pow - d1.den_pow in
    let p2 = den_pow - d2.den_pow in
    reduce
      { num = Z.((d1.num * (~$1 lsl p1)) + (d2.num * (~$1 lsl p2))); den_pow }

  let to_string d =
    let den_str =
      match d.den_pow with
      | 0 -> ""
      | 1 -> " / 2"
      | _ -> " / (2 ^ " ^ Int.to_string d.den_pow ^ ")"
    in
    Z.to_string d.num ^ den_str
end

type t = DyadicMod1 of Dyadic.t

let reduce (DyadicMod1 d) =
  Dyadic.(
    match reduce d with
    | { num = _; den_pow = d } when d <= 0 -> DyadicMod1 zero
    | { num = n; den_pow = dp } when Z.sign n = -1 ->
        let den = Z.(~$1 lsl dp) in
        DyadicMod1 { num = Z.((n mod den) + den); den_pow = dp }
    | { num = n; den_pow = dp } ->
        DyadicMod1 { num = Z.(n mod (~$1 lsl dp)); den_pow = dp })

let make num den_pow = reduce @@ DyadicMod1 Dyadic.{ num; den_pow }
let zero = DyadicMod1 Dyadic.zero

let add (DyadicMod1 d1) (DyadicMod1 d2) =
  reduce @@ DyadicMod1 (Dyadic.add d1 d2)

let mul_int i (DyadicMod1 d2) =
  reduce @@ DyadicMod1 { d2 with num = Z.(~$i * d2.num) }

let mul_z z (DyadicMod1 d2) =
  reduce @@ DyadicMod1 { d2 with num = Z.(z * d2.num) }

let mul_pow2 pow2 (DyadicMod1 d) =
  DyadicMod1 { d with den_pow = d.den_pow - pow2 }

let num (DyadicMod1 d) = d.num
let den_pow (DyadicMod1 d) = d.den_pow

(* This function computes the smallest integer p such that 1/2^p <= d *)
let min_inv_pow2_leq d =
  let d = reduce d in
  if den_pow d <= 0 then invalid_arg "d must be greater than 0"
  else if num d = Z.one then den_pow d
  else
    let inv_d = Q.make Z.(~$1 lsl den_pow d) (num d) in
    let rec loop p pow2 =
      if Q.(geq pow2 inv_d) then p else loop (p + 1) Q.(pow2 lsl 1)
    in
    loop 0 Q.one

let equal d1 d2 = reduce d1 = reduce d2
let to_q (DyadicMod1 d) = Q.make d.num Z.(~$1 lsl d.den_pow)

let to_float d =
  let q = to_q d in
  Mlmpfr.(
    div
      (make_from_str Q.(Z.to_string q.num))
      (make_from_str Q.(Z.to_string q.den)))

let to_string (DyadicMod1 d) = Dyadic.to_string d
