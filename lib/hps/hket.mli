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

(** Hybrid Ket.

    Boolean polynomial in input variables {m x_i} and path variables {m y_j} in
    the form of a {!Var_set}. *)

(** {1:types Types} *)

type t
(** Hket type. *)

(** {1:construction Construction} *)

val zero : t
(** The value 0. *)

val one : t
(** The value 1. *)

(** {1:operations Operations} *)

val add : Var_set.t -> t -> t
(** Addition of a var set monomial. *)

val remove : Var_set.t -> t -> t
(** Remove the given var set monomial if it exists. *)

val neg : t -> t
(** Negation, adds xor 1. *)

val xor : t -> t -> t
(** Xor. *)

val mul : t -> t -> t
(** And. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi ket] returns a new ket with [yi] set to 0 in [ket]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi ket] returns a new ket with [yi] set to 1 in [ket]. *)

val set_ys_to_zero : Y_set.t -> t -> t
(** [set_ys_to_zero ys ket] returns a new ket with all [yi] from [ys] set to 0
    in [ket]. *)

val set_ys_to_one : Y_set.t -> t -> t
(** [set_ys_to_one ys ket] returns a new ket with all [yi] from [ys] set to 1 in
    [ket]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones ket] returns a new ket with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [ket]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros ket] returns a new ket with all [yi] from [y_zeros] set
    to 0 and all other [yi] set to 1 in [ket]. *)

val change_var : int -> t -> t -> t
(** [change_var yi new_val ket] returns a new ket with the path variable [yi]
    replaced by [new_val] in [ket]. *)

(** {1:inspection Inspection} *)
val cardinal : t -> int
(** Count the number of monomials. *)

val choose : t -> Var_set.t
(** Return one monomial of the given ket or raise [Not_found] if the ket is
    zero. Which monomial is chosen is unspecified, but equal monomials will be
    chosen for equal kets. *)

(** {1:searching Searching} *)

val find_first : (Var_set.t -> bool) -> t -> Var_set.t
(** [find_first f ket], where f is a monotonically increasing function over
    {!Var_set}, returns the lowest monomial (according to [Set] comparison and
    {!Var.compare}) [vs] of [ket] such that [f vs], or raises [Not_found] if no
    such element exists. *)

val find_any_y : t -> int option
(** Return any path variable y from the given ket, or [None] if the ket contains
    no y. *)

val find_any_xy : t -> Var.t option
(** Return any input variable x or path variable y from the given ket, or [None]
    if the ket contains no x and y. *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given ket. *)

val find_all_xy : t -> Var_set.t
(** Return a set of all the input variables x and path variables y from the
    given ket. *)

val find_unique_y : t -> Y_set.t
(** Return a set of all the path variables y that appear alone in a monomial in
    the given ket. *)

(** {1:iteration Iteration} *)

val iter : (Var_set.t -> unit) -> t -> unit
(** [iter f ket] applies [f] in turn to all monomials of [ket]. The elements of
    [ket] are presented to [f] in increasing order with respect to the ordering
    over {!Var_set}. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

val compare : t -> t -> int
(** Total ordering between kets. It is a structural comparison, and should not
    be used as a semantic comparison. *)

(** {1:predicates Predicates} *)

val for_all : (Var_set.t -> bool) -> t -> bool
(** [for_all f ket] checks if all monomials of [ket] satisfy the predicate [f].
*)

val exists : (Var_set.t -> bool) -> t -> bool
(** [exists f ket] checks if at least one monomial of [ket] satisfies the
    predicate [f]. *)

val contains_monomial : Var_set.t -> t -> bool
(** [contains_monomial vs ket] checks if [ket] contains the monomial [vs]. *)

val contains_one : t -> bool
(** [contains_monomial ket] checks if [ket] contains 1. *)

val is_zero : t -> bool
(** Checks if the given ket is 0 (contains no monomial). *)

val is_one : t -> bool
(** Checks if the given ket is 1 (contains only 1 empty monomial). *)

val contains_any_x : t -> bool
(** Check if the given ket contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given ket contains any path variable y. *)

val contains_any_var : t -> bool
(** Check if the given ket contains any input variable x or path variable y. *)

val contains_y : int -> t -> bool
(** [contains_y yi ket] checks if [ket] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys ket] checks if [ket] contains at least one path
    variable y from [ys]. *)

(** {1:conversions Conversions} *)

val of_var : Var.t -> t
(** Return a ket containing one monomial with the given var. *)

val of_string : string -> t
(** Convert the given string to a ket. Input variables must be of the form [x0]
    or [X12], path variables of the form [y2] or [Y21], xor of the form [+] or
    [⊕], and and of the form [*] or [^]. Raise [Failure] if the string cannot be
    converted to a ket. *)

val of_string_opt : string -> t option
(** Convert the given string to a ket. Input variables must be of the form [x0]
    or [X12], path variables of the form [y2] or [Y21], xor of the form [+] or
    [⊕], and and of the form [*] or [^]. Return [None] if the string cannot be
    converted to a ket. *)

val to_string : t -> string
(** Convert to string. *)
