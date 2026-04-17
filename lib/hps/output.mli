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

(** Output.

    This module implements the output type, composed of a quantum memory [qmem]
    of type {!Mem} and a classical memory [cmem_stack] of type {!Mem_stack}. *)

(** {1:types Types} *)

type t = {
  qmem : Mem.t;  (** Quantum memory. *)
  cmem_stack : Mem_stack.t;  (** Classical memory. *)
}
(** Output type. *)

(** {1:construction Construction} *)

val make : Mem.t -> Mem_stack.t -> t
(** [make qmem cmem_stack] returns an output with the given [qmem] and
    [cmem_stack]. *)

val empty : t
(** The empty output. *)

(** {1:operations Operations} *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi o] returns a new output with [yi] set to 0 in [o]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi o] returns a new output with [yi] set to 1 in [o]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones o] returns a new output with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [o]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros o] returns a new output with all [yi] from [y_zeros] set
    to 0 and all other [yi] set to 1 in [o]. *)

val change_var : int -> Hket.t -> t -> t
(** [change_var yi new_val o] returns a new output with the path variable [yi]
    replaced by [new_val] in [o]. *)

(** {1:searching Searching} *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given output. *)

val find_unique_y : t -> Y_set.t
(** Return a set of all the path variables y that appear alone in a monomial of
    one of the kets in the given output. *)

val find_y_in_regs : Reg_name_set.t -> t -> Y_set.t
(** [find_y_in_regs reg_name_set o] returns a set of all the path variables y
    from [o] appearing in a registers whose names are in [reg_name_set]. *)

val find_y_in_regs_past : Reg_name_set.t -> t -> Y_set.t
(** [find_y_in_regs_past reg_name_set o] returns a set of all the path variables
    y from the past of the classical memory of [o] (the classical memory except
    the first element) appearing in a registers whose names are in
    [reg_name_set]. *)

val find_reg_names : t -> Reg_name_set.t
(** Returns a set of all register names in the given output. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

val compare : t -> t -> int
(** Total ordering between outputs. It is a structural comparison, and should
    not be used as a semantic comparison. *)

(** {1:predicates Predicates} *)

val contains_any_x : t -> bool
(** Check if the given output contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given output contains any path variable y. *)

val contains_y : int -> t -> bool
(** [contains_y yi o] checks if [o] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys o] checks if [o] contains at least one path variable
    y from [ys]. *)

(** {1:conversions Conversions} *)

val to_string : string -> t -> string
(** Convert to string. *)
