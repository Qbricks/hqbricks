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

(** Memory stack.

    This module implements the memory stack type, a list of {!Mem}. It is used
    to represent the classical memory of {!Output}, where the first element of
    the list is the present state of the classical memory, and the elements
    after are the past states. *)

(** {1:types Types} *)

type t = Mem.t list
(** Mem_stack type. *)

(** {1:construction Construction} *)

val empty : t
(** The empty memory stack. *)

(** {1:operations Operations} *)

val remove_leading_voids : t -> t
(** Remove the leading empty memory elements in the given mem stack. *)

val remove_trailing_voids : t -> t
(** Remove the trailing empty memory elements in the given mem stack. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi ms] returns a new memory stack with [yi] set to 0 in [ms].
*)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi ms] returns a new memory stack with [yi] set to 1 in [ms].
*)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones ms] returns a new memory stack with all [yi]
    from [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [ms]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros ms] returns a new memory stack with all [yi] from
    [y_zeros] set to 0 and all other [yi] set to 1 in [ms]. *)

val change_var : int -> Hket.t -> t -> t
(** [change_var yi new_val ms] returns a new memory stack with the path variable
    [yi] replaced by [new_val] in [ms]. *)

(** {1:searching Searching} *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given memory stack. *)

val find_unique_y : t -> Y_set.t
(** Return a set of all the path variables y that appear alone in a monomial of
    one of the kets in the given mem stack. *)

val find_y_in_regs : Reg_name_set.t -> t -> Y_set.t
(** [find_y_in_regs reg_name_set ms] returns a set of all the path variables y
    from [ms] appearing in a registers whose names are in [reg_name_set]. *)

val find_reg_names : t -> Reg_name_set.t
(** Returns a set of all register names in the given memory stack. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

val compare : t -> t -> int
(** Total ordering between memory stacks. It is a structural comparison, and
    should not be used as a semantic comparison. *)

(** {1:predicates Predicates} *)

val for_all : ('a -> bool) -> 'a list -> bool
(** [for_all f ms] checks if all memory elements of [ms] satisfy the predicate
    [f]. *)

val contains_any_x : t -> bool
(** Check if the given memory stack contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given memory stack contains any path variable y. *)

val contains_y : int -> t -> bool
(** [contains_y yi ms] checks if [ms] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys ms] checks if [ms] contains at least one path
    variable y from [ys]. *)

(** {1:conversions Conversions} *)

val to_string : string -> t -> string
(** Convert to string. *)
