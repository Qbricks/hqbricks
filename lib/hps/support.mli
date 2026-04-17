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

(** Support.

    This module implements the support type, a set of [int] representing boolean
    path variables {m \{y_j\}}. *)

(** {1:types Types} *)

type t = Y_set.t
(** Support type. *)

(** {1:construction Construction} *)

val empty : t
(** The empty support. *)

(** {1:operations Operations} *)

val add : int -> t -> t
(** [add i su] returns a support containing all elements of [su], plus [i]. If
    [i] was already in [su], [su] is returned unchanged (the result of the
    function is then physically equal to [su]). *)

val remove : int -> t -> t
(** [remove i su] returns a set containing all elements of [su], except [i]. If
    [i] was not in [su], [su] is returned unchanged (the result of the function
    is then physically equal to [su]). *)

val diff : t -> t -> t
(** Support difference: [diff su1 su2] contains the elements of [su1] that are
    not in [su2]. *)

val partition : (int -> bool) -> t -> t * t
(** [partition f su] returns a pair of supports [(su1, su2)], where [su1] is the
    support with all the elements of [su] that satisfy the predicate [f], and
    [su2] is the support with all the elements of [su] that do not satisfy [f].
*)

val add_seq : int Stdlib.Seq.t -> t -> t
(** Add the given elements to the support, in order. *)

(** {1:inspection Inspection} *)

val cardinal : t -> int
(** Return the number of elements of the given support. *)

val min_elt : t -> int
(** Return the smallest element of the given support (smallest int), or raise
    [Not_found] if the support is empty. *)

(** {1:iteration Iteration} *)

val iter : (int -> unit) -> t -> unit
(** [iter f su] applies [f] in turn to all elements of [su]. The elements of
    [su] are presented to [f] in increasing order. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:predicates Predicates} *)

val subset : t -> t -> bool
(** [subset su1 su2] tests whether the support [su1] is a subset of the support
    [su2]. *)

val contains_y : int -> t -> bool
(** [contains_y yi su] checks if [su] contains the path variable [yi]. *)

(** {1:conversions Conversions} *)

val to_list : t -> int list
(** Return the list of all elements of the given support, sorted in increasing
    int order. *)

val to_string : t -> string
(** Convert to string. *)
