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

(** Dyadic modulo 1.

    This module implements Dyadic modulo 1, which involves rationals of the
    form:
    {math \frac{num}{2^{den\_pow}} \mod 1}
    where [num] is Zarith big integer of type [Z.t] representing the numerator
    and [den_pow] is an [int] representing the power of 2 of the denominator. *)

(** {1:types Types} *)

type t
(** Dyadic1 type. *)

(** {1:construction Construction} *)

val make : Z.t -> int -> t
(** [make num den_pow] returns a dyadic1 with the given [num] and [den_pow]. *)

val zero : t
(** The value 0. *)

(** {1:operations Operations} *)

val reduce : t -> t
(** Reduce. *)

val add : t -> t -> t
(** Addition. *)

val mul_int : int -> t -> t
(** Multiplication by an int. *)

val mul_z : Z.t -> t -> t
(** Multiplication by a Zarith big integer [Z.t]. *)

val mul_pow2 : int -> t -> t
(** [mul_pow2 pow d] multiplies [d] by [2^pow]. *)

(** {1:inspection Inspection} *)

val num : t -> Z.t
(** Get the numerator. *)

val den_pow : t -> int
(** Get the power of 2 of the denominator. *)

val min_inv_pow2_leq : t -> int
(** [min_inv_pow2_leq d] computes the smallest integer [p] such that
    [1 / 2^p <= d]. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:conversions Conversions} *)

val to_q : t -> Q.t
(** Convert to Zarith rational [Q.t]. *)

val to_float : t -> Mlmpfr.mpfr_float
(** Convert to Mlmpfr arbitrary precision float [Mlmpfr.mpfr_float]. *)

val to_string : t -> string
(** Convert to string. *)
