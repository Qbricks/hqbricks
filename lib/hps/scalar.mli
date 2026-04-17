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

(** Scalar.

    Constructible numbers with boolean variables {m x_i} and {m y_j}. *)

(** {1:types Types} *)

(** Scalar type. *)
type t =
  | SVar of string  (** Variable with the given name. *)
  | SBool of Hket.t  (** Boolean with the given value. *)
  | SFrac of Z.t * Z.t  (** [SFrac (i1 i2)] represents the fraction [i1/i2]. *)
  | SFloat of Mlmpfr.mpfr_float
      (** Arbitrary precision float with the given value. *)
  | Sqrt of t  (** Sqrt. *)
  | Cos of angle  (** Cosinus of the given angle. *)
  | Sin of angle  (** Sinus of the given angle. *)
  | SAdd of t * t  (** Addition. *)
  | SMul of t * t  (** Multiplication. *)
  | SPowB of t * Hket.t  (** [SpowB (s, ket)] represents [s^ket]. *)
  | SNeg of t  (** Negation. *)
  | SInv of t  (** Inverse. *)

(** Angle type represented as a fraction of 2Pi. *)
and angle =
  | AFrac of Z.t * Z.t
      (** [AFrac (i1, i2)] represents the fraction [2Pi * i1/i2]. *)
  | AFloat of Mlmpfr.mpfr_float
      (** Arbitrary precision float with the given value. *)
  | ArcCos of t  (** Arc cosinus of the given scalar. *)
  | ArcSin of t  (** Arc sinus of the given scalar. *)
  | AMulB of Hket.t * angle  (** [AMulB (ket, a)] represents [ket * a]. *)
  | AMulI of Z.t * angle  (** [AMulI (i, a)] represents [i * a]. *)
  | AAdd of angle * angle  (** Addition. *)
  | ANeg of angle  (** Negation. *)

(** {1:construction Construction} *)

val make_frac : Z.t -> Z.t -> t
(** [make_frac i1 i2] returns [SFrac (i1, i2)]. *)

val make_int : Z.t -> t
(** [make_frac i] returns [SFrac (i, 1)]. *)

val zero : t
(** The value 0. *)

val one : t
(** The value 1. *)

val sqrt_half : t
(** The value [sqrt(1/2)]. *)

val sqrt_half_pow_n : int -> t
(** [sqrt_half_pow_n n] returns [sqrt(1/2)^n]. *)

(** {1:operations Operations} *)

val simp : t -> t
(** Simplify the given scalar. *)

val simp_angle : angle -> angle
(** Simplify the given angle. *)

val square : t -> t
(** Return the square of the given scalar. *)

val complement : t -> t
(** [complement s] returns the complement of [s], [1-s]. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi s] returns a new scalar with [yi] set to 0 in [s]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_zero yi s] returns a new scalar with [yi] set to 1 in [s]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones s] returns a new scalar with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [s]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros s] returns a new scalar with all [yi] from [y_zeros] set
    to 0 and all other [yi] set to 1 in [s]. *)

val angle_set_y_to_zero : int -> angle -> angle
(** [angle_set_y_to_zero yi a] returns a new angle with [yi] set to 0 in [a]. *)

val angle_set_y_to_one : int -> angle -> angle
(** [angle_set_y_to_zero yi a] returns a new angle with [yi] set to 1 in [a]. *)

val angle_set_y_values : Y_set.t -> Y_set.t -> angle -> angle
(** [angle_set_y_values yi_zeros yi_ones a] returns a new angle with all [yi]
    from [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [a]. *)

val angle_set_all_y : Y_set.t -> angle -> angle
(** [angle_set_all_y y_zeros a] returns a new angle with all [yi] from [y_zeros]
    set to 0 and all other [yi] set to 1 in [a]. *)

val change_var : int -> Hket.t -> t -> t
(** [change_var yi new_val s] returns a new scalar with the path variable [yi]
    replaced by [new_val] in [s]. *)

val change_var_angle : int -> Hket.t -> angle -> angle
(** [change_var yi new_val a] returns a new angle with the path variable [yi]
    replaced by [new_val] in [a]. *)

(** {1:searching Searching} *)

val find_any_y : t -> int option
(** Return any path variable y from the given scalar, or [None] if the scalar
    contains no y. *)

val angle_find_any_y : angle -> int option
(** Return any path variable y from the given angle, or [None] if the angle
    contains no y. *)

val find_any_xy : t -> Var.t option
(** Return any input variable x or path variable y from the given scalar, or
    [None] if the scalar contains no x and y. *)

val angle_find_any_xy : angle -> Var.t option
(** Return any input variable x or path variable y from the given angle, or
    [None] if the angle contains no x and y. *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given scalar. *)

val angle_find_all_y : angle -> Y_set.t
(** Return a set of all the path variables y from the given angle. *)

val find_all_xy : t -> Var_set.t
(** Return a set of all the input variables x and path variables y from the
    given scalar. *)

val angle_find_all_xy : angle -> Var_set.t
(** Return a set of all the input variables x and path variables y from the
    given angle. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Scalar equality test. *)

val angle_equal : angle -> angle -> bool
(** Angle equality test. *)

val eq_zero : t -> bool
(** Check if the given scalar is 0. *)

val qleq : t -> t -> bool option
(** [qleq s1 s2] checks if [s1] is less than or equal to [s2] by converting [s1]
    and [s2] to rationals using {!to_q} and comparing them, or returns [None] if
    [s1] or [s2] cannot be converted. *)

val qgeq : t -> t -> bool option
(** [qgeq s1 s2] checks if [s1] is greater than or equal to [s2] by converting
    [s1] and [s2] to rationals using {!to_q} and comparing them, or returns
    [None] if [s1] or [s2] cannot be converted. *)

val qlt : t -> t -> bool option
(** [qlt s1 s2] checks if [s1] is less than [s2] by converting [s1] and [s2] to
    rationals using {!to_q} and comparing them, or returns [None] if [s1] or
    [s2] cannot be converted. *)

val qgt : t -> t -> bool option
(** [qgt s1 s2] checks if [s1] is greater than [s2] by converting [s1] and [s2]
    to rationals using {!to_q} and comparing them, or returns [None] if [s1] or
    [s2] cannot be converted. *)

val fleq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [fleq ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is less than or equal
    to [s2] up to an absolute epsilon [~epsilon_abs] and a relative epsilon
    [~epsilon_rel] by converting [s1] and [s2] to [Mlmpfr.mpfr_float] using
    {!to_float} and comparing them using {!Utils.float_approx_leq}. Returns
    [None] if [s1] of [s2] cannot be converted.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val fgeq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [fgeq ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is greater than or
    equal to [s2] up to an absolute epsilon [~epsilon_abs] and a relative
    epsilon [~epsilon_rel] by converting [s1] and [s2] to [Mlmpfr.mpfr_float]
    using {!to_float} and comparing them using {!Utils.float_approx_geq}.
    Returns [None] if [s1] of [s2] cannot be converted.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val flt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [flt ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is less than [s2] up to
    an absolute epsilon [~epsilon_abs] and a relative epsilon [~epsilon_rel] by
    converting [s1] and [s2] to [Mlmpfr.mpfr_float] using {!to_float} and
    comparing them using {!Utils.float_approx_lt}. Returns [None] if [s1] of
    [s2] cannot be converted.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val fgt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [fgt ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is greater than [s2] up
    to an absolute epsilon [~epsilon_abs] and a relative epsilon [~epsilon_rel]
    by converting [s1] and [s2] to [Mlmpfr.mpfr_float] using {!to_float} and
    comparing them using {!Utils.float_approx_gt}. Returns [None] if [s1] of
    [s2] cannot be converted.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [leq ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is less than or equal
    to [s2], first trying the exact comparison using {!qleq}, then if it returns
    [None] up to an absolute epsilon [~epsilon_abs] and a relative epsilon
    [~epsilon_rel] using {!fleq}. Returns [None] if [s1] of [s2] cannot be
    converted to rational and float.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [geq ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is greater than or
    equal to [s2], first trying the exact comparison using {!qgeq}, then if it
    returns [None] up to an absolute epsilon [~epsilon_abs] and a relative
    epsilon [~epsilon_rel] using {!fgeq}. Returns [None] if [s1] of [s2] cannot
    be converted to rational and float.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [lt ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is less than [s2], first
    trying the exact comparison using {!qlt}, then if it returns [None] up to an
    absolute epsilon [~epsilon_abs] and a relative epsilon [~epsilon_rel] using
    {!flt}. Returns [None] if [s1] of [s2] cannot be converted to rational and
    float.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

val gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  t ->
  t ->
  bool option
(** [gt ~epsilon_abs ~epsilon_rel s1 s2] checks if [s1] is greater than [s2],
    first trying the exact comparison using {!qgt}, then if it returns [None] up
    to an absolute epsilon [~epsilon_abs] and a relative epsilon [~epsilon_rel]
    using {!fgt}. Returns [None] if [s1] of [s2] cannot be converted to rational
    and float.

    [~epsilon_abs] and [~epsilon_rel] default value is [1e-6]. *)

(** {1:predicates Predicates} *)

val contains_any_x : t -> bool
(** Check if the given scalar contains any input variable x. *)

val angle_contains_any_x : angle -> bool
(** Check if the given angle contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given scalar contains any path variable y. *)

val angle_contains_any_y : angle -> bool
(** Check if the given angle contains any path variable y. *)

val contains_any_var : t -> bool
(** Check if the given scalar contains any input variable x or path variable y.
*)

val angle_contains_any_var : angle -> bool
(** Check if the given angle contains any input variable x or path variable y.
*)

val contains_y : int -> t -> bool
(** [contains_y yi s] checks if [s] contains the path variable [yi]. *)

val angle_contains_y : int -> angle -> bool
(** [angle_contains_y yi a] checks if [a] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys s] checks if [s] contains at least one path variable
    y from [ys]. *)

val angle_contains_y_from_set : Y_set.t -> angle -> bool
(** [angle_contains_y_from_set ys a] checks if [a] contains at least one path
    variable y from [ys]. *)

(** {1:conversions Conversions} *)

val of_dyadic1 : Dyadic1.t -> t
(** Convert the given dyadic1 to a scalar. *)

val angle_of_dyadic1 : Dyadic1.t -> angle
(** Convert the given dyadic1 to an angle. *)

val to_q : t -> Q.t option
(** Convert the given scalar to Zarith rational [Q.t], or return [None] if it
    cannot be converted (e.g. if there are variables, or if it is not a
    rational). *)

val to_float : t -> Mlmpfr.mpfr_float option
(** Convert the given scalar to Mlmpfr arbitrary precision float
    [Mlmpfr.mpfr_float], or return [None] if it cannot be converted (e.g. if
    there are variables). *)

val angle_to_float : angle -> Mlmpfr.mpfr_float option
(** Convert the given angle to Mlmpfr arbitrary precision float
    [Mlmpfr.mpfr_float], or return [None] if it cannot be converted (e.g. if
    there are variables). *)

val to_product_list : t -> t list
(** Convert the given scalar to a product of scalar represented by a list. All
    the top level SMul as well the ones inside Sqrt are splitted in the list. *)

val to_string : t -> string
(** Convert the given scalar to string. *)

val angle_to_string : angle -> string
(** Convert the given angle to string. *)

val to_raw_string : t -> string
(** Convert the given scalar to a raw string, usefull to see the exact structure
    of the variant. *)

val angle_to_raw_string : angle -> string
(** Convert the given angle to a raw string, usefull to see the exact structure
    of the variant. *)

(** {1:printing Printing} *)

val pp : Format.formatter -> t -> unit
(** Print the given scalar on the specified formatter (raw). *)

val pp_angle : Format.formatter -> angle -> unit
(** Print the given angle on the specified formatter (raw). *)
