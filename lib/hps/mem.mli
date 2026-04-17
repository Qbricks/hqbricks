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

(** Memory.

    This module implements the memory type, a map from register id as a
    [string * int] pair to {!Hket} value. It is used to represent the quantum
    memory of {!Output} and the elements of the classical memory of
    {!Mem_stack}. *)

(** {1:types Types} *)

type reg_id = string * int
(** Register identifier type, representing the address of the given register at
    the given index (the [string] is the register name and the [int] is the
    index) and used as the memory key. *)

type t
(** Mem type. *)

(** {1:construction Construction} *)

val empty : t
(** The empty memory. *)

(** {1:operations Operations} *)

val add : reg_id -> Hket.t -> t -> t
(** [add reg_id ket mem] adds a binding of [reg_id] to [ket] to [mem]. If
    [reg_id] was already bound in [mem], the previous binding disappears. *)

val remove : reg_id -> t -> t
(** [remove reg_id mem] removes the given [reg_id] from [mem] if it exists. *)

val partition : (reg_id -> Hket.t -> bool) -> t -> t * t
(** [partition f mem] returns a pair of memory [(mem1, mem2)], where [mem1]
    contains all the bindings of [m] that satisfy the predicate [f], and [mem2]
    is the memory with all the bindings of [mem] that do not satisfy [f]. *)

val add_vec : reg_id -> Hket.t list -> t -> t
(** [add_vec (reg_name, i_start) ket_list mem] adds bindings to [mem] for each
    element of [ket_list] from index 0 to the length of [ket_list] - 1, where
    the binding for the nth element is of the form
    [(reg_name, i_start + n) -> ket_list[n]] , and [ket_list[n]] is the nth
    element of [ket_list]. If some added register ids were already bound in
    [mem], the previous bindings disappear. *)

val add_vec_const : reg_id -> int -> Hket.t -> t -> t
(** [add_vec_const (reg_name, i_start) len ket mem] adds bindings to [mem] for
    each element from 0 to [len - 1], where the binding for the nth element is
    of the form [(reg_name, i_start + n) -> ket]. If some added register ids
    were already bound in [mem], the previous bindings disappear. *)

val add_vec_zero : reg_id -> int -> t -> t
(** [add_vec_zero (reg_name, i_start) len mem] adds bindings to [mem] for each
    element from 0 to [len - 1], where the binding for the nth element is of the
    form [(reg_name, i_start + n) -> Hket.zero]. If some added register ids were
    already bound in [mem], the previous bindings disappear. *)

val add_vec_one : reg_id -> int -> t -> t
(** [add_vec_one (reg_name, i_start) len mem] adds bindings to [mem] for each
    element from 0 to [len - 1], where the binding for the nth element is of the
    form [(reg_name, i_start + n) -> Hket.one]. If some added register ids were
    already bound in [mem], the previous bindings disappear. *)

val add_vec_x : reg_id -> int -> int -> t -> t
(** [add_vec_x (reg_name, i_start) len x_start mem] adds bindings to [mem] for
    each element from 0 to [len - 1], where the binding for the nth element is
    of the form [(reg_name, i_start + n) -> x[x_start + n]], and [x[i]] is the
    input variable x of index [i]. If some added register ids were already bound
    in [mem], the previous bindings disappear. *)

val add_int : reg_id -> int -> Z.t -> t -> t
(** [add_int (reg_name, i_start) len n mem] adds the [len] least significant
    bits of the binary representation of [n] to [mem] at binding [reg_name], for
    indices from [i_start] to [i_start + len - 1]. The value [n] is encoded in
    little-endian order in [mem], so index [i_start] contains the least
    significant bit. If some added register ids were already bound in [mem], the
    previous bindings disappear. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi mem] returns a new memory with [yi] set to 0 in [mem]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi mem] returns a new memory with [yi] set to 1 in [mem]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones mem] returns a new memory with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [mem]. *)

val set_all_y : Y_set.t -> t -> t
(** [set_all_y y_zeros mem] returns a new memory with all [yi] from [y_zeros]
    set to 0 and all other [yi] set to 1 in [mem]. *)

val change_var : int -> Hket.t -> t -> t
(** [change_var yi new_val mem] returns a new memory with the path variable [yi]
    replaced by [new_val] in [mem]. *)

(** {1:inspection Inspection} *)

val cardinal : t -> int
(** Count the number of reg_id. *)

(** {1:searching Searching} *)

val find : reg_id -> t -> Hket.t
(** [find_opt reg_id mem] returns the value of [reg_id] in [mem] if it exists,
    and raises [Not_found] otherwise. *)

val find_all_y : t -> Y_set.t
(** Return a set of all the path variables y from the given memory. *)

val find_unique_y : t -> Y_set.t
(** Return a set of all the path variables y that appear alone in a monomial of
    one of the kets in the given mem. *)

val find_y_in_regs : Reg_name_set.t -> t -> Y_set.t
(** [find_y_in_regs reg_name_set mem] returns a set of all the path variables y
    from [mem] appearing in a registers whose names are in [reg_name_set]. *)

val find_reg_names : t -> Reg_name_set.t
(** Returns a set of all register names in the given memory. *)

(** {1:iteration Iteration} *)

val iter : (reg_id -> Hket.t -> unit) -> t -> unit
(** [iter f mem] applies [f] in turn to all bindings of [mem]. The elements of
    [mem] are presented to [f] in increasing order with respect to the ordering
    over {!reg_id}. *)

val fold : (reg_id -> Hket.t -> 'b -> 'b) -> t -> 'b -> 'b
(** [fold f mem init] computes [(f reg_idN ketN ... (f reg_id1 ket1 init)...)],
    where [reg_id1 ... reg_idN] are the register identifiers in [mem] (in
    increasing order), and [ket1 ... ketN] are the associated ket values. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

val compare : t -> t -> int
(** Total ordering between memories. It is a structural comparison, and should
    not be used as a semantic comparison. *)

(** {1:predicates Predicates} *)

val is_empty : t -> bool
(** Check whether the given memory is empty or not. *)

val exists : (reg_id -> Hket.t -> bool) -> t -> bool
(** [exists f mem] checks if at least one binding of [mem] satisfies the
    predicate [f]. *)

val contains_reg : reg_id -> t -> bool
(** [contains_reg reg_id mem] checks if [mem] contains the register id [reg_id].
*)

val contains_any_x : t -> bool
(** Check if the given memory contains any input variable x. *)

val contains_any_y : t -> bool
(** Check if the given memory contains any path variable y. *)

val contains_y : int -> t -> bool
(** [contains_y yi mem] checks if [mem] contains the path variable [yi]. *)

val contains_y_from_set : Y_set.t -> t -> bool
(** [contains_y_from_set ys mem] checks if [mem] contains at least one path
    variable y from [ys]. *)

(** {1:conversions Conversions} *)
val reg_to_int : string -> int -> t -> Z.t
(** [reg_to_int name len mem] converts the register from [mem] with the given
    [name] and indices from [0] to [len - 1] to a Zarith big integer, or raises
    [Failure] if it cannot be converted (e.g. if it contains variables). The
    register is considered little-endian, so index [0] is the least significant
    bit. *)

val reg_to_int_big_endian : string -> int -> t -> Z.t
(** [reg_to_int_big_endian name len mem] converts the register from [mem] with
    the given [name] and indices from [0] to [len - 1] to a Zarith big integer,
    or raises [Failure] if it cannot be converted (e.g. if it contains
    variables). The register is considered big-endian, so index [0] is the most
    significant bit. *)

val reg_as_int_to_phase :
  ?k_fact:Hket.t -> Dyadic1.t -> string -> int -> t -> Phase.t
(** [reg_as_int_to_phase ~k_fact dy_fact reg_name reg_len mem] converts the
    register from [mem] with the given [reg_name] and indices from [0] to
    [reg_len - 1] to a phase by interpreting it as an [int] and lifting it using
    {!Phase.lift_ket} with the given [k_fact] and [dy_fact]. [k_fact] is 1 by
    default. The register is considered little-endian, so index [0] is the least
    significant bit. *)

val qmem_to_string : t -> string
(** Convert the given memory to string using quantum memory format. *)

val cmem_to_string : t -> string
(** Convert the given memory to string using classical memory format. *)
