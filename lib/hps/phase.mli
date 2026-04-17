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

(** Phase.

    Polynomial in input variables {m x_i} and path variables {m y_j} in the form
    of a {!Var_set} with {!Dyadic1} coefficients. *)

(** {1:types Types} *)

type t
(** Phase type. *)

(** {1:construction Construction} *)

val singleton : Var_set.t -> Dyadic1.t -> t
(** [singleton vs dy] returns a phase containing a single monomial with
    variables [vs] and coefficient [dy]. *)

val const : Dyadic1.t -> t
(** [const dy] returns the constant value [dy]. *)

val zero : t
(** The value 0. *)

val one_half : t
(** The value 1/2. *)

val lift_ket : Hket.t -> Dyadic1.t -> t
(** [lift_ket ket dy_fact] lifts [ket] into a phase with a [dy_fact] coefficient
    factor. *)

(** {1:operations Operations} *)

val add : Var_set.t -> Dyadic1.t -> t -> t
(** [add vs dy p] adds a monomial [vs] with coefficient [dy] to [p]. *)

val addl : Var.t list -> Dyadic1.t -> t -> t
(** [addl var_list dy p] adds a monomial with a var set corresponding to
    [var_list] with coefficient [dy] to [p]. *)

val remove : Var_set.t -> t -> t
(** Remove the given var set monomial if it exists. *)

val diff : t -> t -> t
(** [diff p1 p2] returns [p1] with it's common monomials with [p2] removed
    (monomials with the same var set and the same coefficients). *)

val filter : (Var_set.t -> Dyadic1.t -> bool) -> t -> t
(** [filter f p] returns the phase with all the monomials of [p] that satisfy
    the predicate f. *)

val partition : (Var_set.t -> Dyadic1.t -> bool) -> t -> t * t
(** [partition f p] returns a pair of phase [(p1, p2)], where [p1] contains all
    the monomials of [p] that satisfy the predicate [f], and [p2] is the phase
    with all the monomials of [p] that do not satisfy [f]. *)

val addp : t -> t -> t
(** Addition. *)

val muli : Z.t -> t -> t
(** Multiplication by a Zarith int [Z]. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi p] returns a new phase with [yi] set to 0 in [p]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi p] returns a new phase with [yi] set to 1 in [p]. *)

val set_ys_to_zero : Y_set.t -> t -> t
(** [set_ys_to_zero ys p] returns a new phase with all [yi] from [ys] set to 0
    in [p]. *)

val set_ys_to_one : Y_set.t -> t -> t
(** [set_ys_to_one ys p] returns a new phase with all [yi] from [ys] set to 1 in
    [p]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones p] returns a new phase with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [p]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros p] returns a new phase with all [yi] from [y_zeros] set
    to 0 and all other [yi] set to 1 in [p]. *)

val change_var : int -> Hket.t -> t -> t
(** [change_var yi new_val p] returns a new phase with the path variable [yi]
    replaced by [new_val] in [p]. *)

(** {1:inspection Inspection} *)

val cardinal : t -> int
(** Count the number of monomials. *)

val count_y : ?y_count_map:int Y_map.t -> t -> int Y_map.t
(** [count_y ~y_count_map p] counts the number of occurences of each path
    variables y in [p], [y_count_map] is empty by default and the count is added
    to it. *)

(** {1:searching Searching} *)

val find_opt : Var_set.t -> t -> Dyadic1.t option
(** [find_opt vs p] returns the coefficient associated with [vs] in [p] if it
    exists, and [None] otherwise. *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given phase. *)

val find_all_y_not_one_half : t -> Y_set.t
(** Return a set of all the path variables y that appear in a monomial with a
    coefficient that is not 1/2 in the given phase. *)

(** {1:iteration Iteration} *)

val iter : (Var_set.t -> Dyadic1.t -> unit) -> t -> unit
(** [iter f p] applies [f] in turn to all monomials of [p]. The elements of [p]
    are presented to [f] in increasing order with respect to the ordering over
    {!Var_set}. *)

val fold : (Var_set.t -> Dyadic1.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
(** [fold f p init] computes [(f vsN dyN ... (f vs1 dy1 init)...)], where
    [vs1 ... vsN] are the monomials var set in [p] (in increasing order), and
    [dy1 ... dyN] are the associated coefficients. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:predicates Predicates} *)

val for_all : (Var_set.t -> Dyadic1.t -> bool) -> t -> bool
(** [for_all f p] checks if all monomials of [p] satisfy the predicate [f]. *)

val exists : (Var_set.t -> Dyadic1.t -> bool) -> t -> bool
(** [exists f p] checks if at least one monomial of [p] satisfies the predicate
    [f]. *)

val eq_zero : t -> bool
(** Check if the given phase is 0. *)

val eq_one_half : t -> bool
(** Check if the given phase is 1/2. *)

val is_const : t -> bool
(** Check if the given phase is constant. *)

val contains_any_x : t -> bool
(** Check if the given phase contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given phase contains any path variable y. *)

val contains_y : int -> t -> bool
(** [contains_y yi p] checks if [p] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys p] checks if [p] contains at least one path variable
    y from [ys]. *)

val contains_y_not_one_half : int -> t -> bool
(** [contains_y_not_one_half yi p] checks if [p] contains a monomial containing
    the path variable [yi] with a coefficient that is not 1/2. *)

(** {1:conversions Conversions} *)

val to_string : t -> string
(** Convert to string. *)
