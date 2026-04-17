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

(** Utility functions and types. *)

(** Map from string to value. *)
module String_map : sig
  include Map.S with type key = string
end

(** Map from variable name to value. *)
module Var_name_map : sig
  include Map.S with type key = string
end

(** Set of string. *)
module String_set : sig
  include Set.S with type elt = string
end

val string_equal_ci : string -> string -> bool
(** Case insensitive string equality test. *)

val n_string : string -> int -> string
(** [n_string str n] repeats str n times. *)

(** Map from int to value. *)
module Int_map : sig
  include Map.S with type key = int
end

(** Set of int. *)
module Int_set : sig
  include Set.S with type elt = int
end

val create_int_set : int -> int -> Int_set.t
(** [create_int_set i_start i_end] creates an int set containing values from
    i_start (included) to i_end (excluded). *)

val float_approx_equal :
  ?epsilon_abs:float -> ?epsilon_rel:float -> float -> float -> bool
(** [float_approx_equal ~epsilon_abs ~epsilon_rel x y] checks if [x] and [y] are
    equal up to [epsilon_abs] or up to [(max x y) *. epsilon_rel]. *)

val float_approx_leq :
  ?epsilon_abs:float -> ?epsilon_rel:float -> float -> float -> bool
(** [float_approx_leq ~epsilon_abs ~epsilon_rel x y] checks if [x] < [y] or
    [float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val float_approx_geq :
  ?epsilon_abs:float -> ?epsilon_rel:float -> float -> float -> bool
(** [float_approx_geq ~epsilon_abs ~epsilon_rel x y] checks if [x] > [y] or
    [float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val float_approx_lt :
  ?epsilon_abs:float -> ?epsilon_rel:float -> float -> float -> bool
(** [float_approx_lt ~epsilon_abs ~epsilon_rel x y] checks if [x] < [y] and not
    [float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val float_approx_gt :
  ?epsilon_abs:float -> ?epsilon_rel:float -> float -> float -> bool
(** [float_approx_gt ~epsilon_abs ~epsilon_rel x y] checks if [x] > [y] and not
    [float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val mpfr_float_approx_equal :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  bool
(** [mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y] checks if [x] and
    [y] are equal up to [epsilon_abs] or up to [(max x y) *. epsilon_rel]. *)

val mpfr_float_approx_leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  bool
(** [mpfr_float_approx_leq ~epsilon_abs ~epsilon_rel x y] checks if [x] < [y] or
    [mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val mpfr_float_approx_geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  bool
(** [mpfr_float_approx_geq ~epsilon_abs ~epsilon_rel x y] checks if [x] > [y] or
    [mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val mpfr_float_approx_lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  bool
(** [mpfr_float_approx_lt ~epsilon_abs ~epsilon_rel x y] checks if [x] < [y] and
    not [mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val mpfr_float_approx_gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  Mlmpfr.mpfr_float ->
  bool
(** [mpfr_float_approx_gt ~epsilon_abs ~epsilon_rel x y] checks if [x] > [y] and
    not [mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y]. *)

val mpfr_pi : Mlmpfr.mpfr_float
(** MPFR pi value. *)

val read_file : string -> string
(** Read file to string. *)

val write_json_to_file : string -> Yojson.Safe.t -> unit
(** Write json to file. *)
