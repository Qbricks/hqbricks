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

(** Set of {!Var}. *)

(** {1:set Set} *)

include Set.S with type elt = Var.t

(** {1:operations Operations} *)

val partition_xset_yset : t -> X_set.t * Y_set.t
(** Partition the given var set into an x set and a y set. *)

(** {1:inspection Inspection} *)

val count_y : ?y_count_map:int Y_map.t -> t -> int Y_map.t
(** [count_y ~y_count_map vs] counts the number of occurences of each path
    variables y in [vs], [y_count_map] is empty by default and the count is
    added to it. *)

(** {1:predicates Predicates} *)

val contains_any_x : t -> bool
(** Check if the given var set contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given var set contains any path variable y. *)

(** {1:conversions Conversions} *)

val of_x_set : X_set.t -> t
(** Convert an x set into a var set. *)

val of_y_set : Y_set.t -> t
(** Convert a y set into a var set. *)

val to_string : t -> string
(** Convert to string. *)
