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

(** Hybrid path-sums (HPS).

    This module implements HPS, a compact symbolic representation for hybrid
    classical/quantum program states that extends unbalanced path-sums to the
    hybrid case. This representation is used for the symbolic execution and the
    specification verification of hybrid programs.

    An HPS
    {m \left\langle\texttt{p},\texttt{s}\cdot\texttt{o}\right\rangle_\texttt{su}}
    is given by:

    - {{!Support}A path support} {m \texttt{su}}: a set of boolean path
      variables {m \{y_j\}}.
    - {{!Output}An output signature} {m \texttt{o}}: a hybrid memory containing
      {{!Hket}boolean polynomials} in input variables {m x_i} and path variables
      {m y_j}.
    - {{!Phase}A phase polynomial} {m \texttt{p}\in\mathbb D[x_i, y_j]} in
      {m x_i} and {m y_j} over the ring of {{!Dyadic1}dyadics}
      {m \mathbb D=\{\frac{n}{2^m} : n,m \in \mathbb Z\}}.
    - {{!Scalar}A scalar functions} {m \texttt{s}}: constructible numbers with
      boolean variables {m x_i} and {m y_j}.

    Hybrid path-sums can be produced by {{!Evaluator}evaluating} a program,
    rewritten using {{!Rewrite}rewriting rules}, {{!Concretization}concretized}
    into probabilistic hybrid state descriptions, and checked with
    {{!Assertion}assertions}. *)

(** {1:types Types} *)

module Y_map = Y_map
module X_set = X_set
module Y_set = Y_set
module Reg_name_set = Reg_name_set
module Var = Var
module Var_set = Var_set
module Dyadic1 = Dyadic1
module Hket = Hket
module Phase = Phase
module Scalar = Scalar
module Mem = Mem
module Mem_stack = Mem_stack
module Output = Output
module Support = Support

type t = {
  phase : Phase.t;  (** Phase. *)
  scalar : Scalar.t;  (** Scalar. *)
  output : Output.t;  (** Output. *)
  support : Support.t;  (** Support. *)
  y_count : int;
      (** Y count.

          Each time a new path variable y is added to an HPS, it gets the index
          [y_count] and [y_count] is incremented by 1. [y_count] counts the
          number of path variables that were added to the HPS, and not the
          number of path variables currently present in the HPS. *)
}
(** Hybrid path-sums (HPS) type. *)

(** {1:construction Construction} *)

val zero : t
(** The value 0. *)

val one : t
(** The value 1. *)

(** {1:operations Operations} *)

val add_phase : Var_set.elt list -> Dyadic1.t -> t -> t
(** [add_phase var_list dy hps] returns a new HPS with a monomial corresponding
    to [var_list] with coefficient [dy] added to [hps.phase] using
    {!Phase.addl}. *)

val set_phase : Phase.t -> t -> t
(** [set_phase p hps] returns a new HPS with [hps.phase] set to [p]. *)

val mul_scalar : Scalar.t -> t -> t
(** [mul_scalar s hps] returns a new HPS with [hps.scalar] multiplied by [s]. *)

val set_scalar : Scalar.t -> t -> t
(** [set_scalar s hps] returns a new HPS with [hps.scalar] set to [s]. *)

val set_qmem : Mem.t -> t -> t
(** [set_qmem qmem hps] returns a new HPS with [hps.output.qmem] set to [qmem].
*)

val add_qmem : Mem.reg_id -> Hket.t -> t -> t
(** [add_qmem qbit ket hps] returns a new HPS with a binding of [reg_id] to
    [ket] added to [hps.output.qmem] using {!Mem.add}. *)

val add_qmem_vec : Mem.reg_id -> Hket.t list -> t -> t
(** [add_qmem_vec reg_id_start ket_list hps] returns a new HPS with a vector
    [ket_list] added to [reg_id_start] in [hps.output.qmem] using
    {!Mem.add_vec}. *)

val add_qmem_vec_const : Mem.reg_id -> int -> Hket.t -> t -> t
(** [add_qmem_vec_const reg_id_start len ket hps] returns a new HPS with [ket]
    added to the first [len] register ids starting at [reg_id_start] in
    [hps.output.qmem] using {!Mem.add_vec_const}. *)

val add_qmem_vec_zero : Mem.reg_id -> int -> t -> t
(** [add_qmem_vec_zero reg_id_start len hps] returns a new HPS with [0] added to
    the first [len] register ids starting at [reg_id_start] in [hps.output.qmem]
    using {!Mem.add_vec_zero}. *)

val add_qmem_vec_one : Mem.reg_id -> int -> t -> t
(** [add_qmem_vec_one reg_id_start len hps] returns a new HPS with [1] added to
    the first [len] register ids starting at [reg_id_start] in [hps.output.qmem]
    using {!Mem.add_vec_one}. *)

val add_qmem_vec_x : Mem.reg_id -> int -> int -> t -> t
(** [add_qmem_vec_x reg_id_start len x_start hps] returns a new HPS with [x[n]]
    starting at [n = x_start] added to the first [len] register ids starting at
    [reg_id_start] in [hps.output.qmem] using {!Mem.add_vec_x}. *)

val add_qmem_int : Mem.reg_id -> int -> Z.t -> t -> t
(** [add_qmem_int reg_id_start len n hps] returns a new HPS with the [len] least
    significant bits of the binary representation of [n] (little-endian) added
    to the first [len] register ids starting at [reg_id_start] in
    [hps.output.qmem] using {!Mem.add_int}. *)

val set_cmem_stack : Mem_stack.t -> t -> t
(** [set_cmem_stack cmem_stack hps] returns a new HPS with
    [hps.output.cmem_stack] set to [cmem_stack]. *)

val add_cmem : Mem.reg_id -> Hket.t -> t -> t
(** [add_cmem qbit ket hps] returns a new HPS with a binding of [reg_id] to
    [ket] added to the first element (present classical memory) of
    [hps.output.cmem_stack] using {!Mem.add}. *)

val add_cmem_vec : Mem.reg_id -> Hket.t list -> t -> t
(** [add_cmem_vec reg_id_start ket_list hps] returns a new HPS with a vector
    [ket_list] added to [reg_id_start] in the first element (present classical
    memory) of [hps.output.cmem_stack] using {!Mem.add_vec}. *)

val add_cmem_vec_const : Mem.reg_id -> int -> Hket.t -> t -> t
(** [add_cmem_vec_const reg_id_start len ket hps] returns a new HPS with [ket]
    added to the first [len] register ids starting at [reg_id_start] in the
    first element (present classical memory) of [hps.output.cmem_stack] using
    {!Mem.add_vec_const}. *)

val add_cmem_vec_zero : Mem.reg_id -> int -> t -> t
(** [add_cmem_vec_zero reg_id_start len hps] returns a new HPS with [0] added to
    the first [len] register ids starting at [reg_id_start] in the first element
    (present classical memory) of [hps.output.cmem_stack] using
    {!Mem.add_vec_zero}. *)

val add_cmem_vec_one : Mem.reg_id -> int -> t -> t
(** [add_cmem_vec_one reg_id_start len hps] returns a new HPS with [1] added to
    the first [len] register ids starting at [reg_id_start] in the first element
    (present classical memory) of [hps.output.cmem_stack] using
    {!Mem.add_vec_one}. *)

val add_cmem_vec_x : Mem.reg_id -> int -> int -> t -> t
(** [add_cmem_vec_x reg_id_start len x_start hps] returns a new HPS with [x[n]]
    starting at [n = x_start] added to the first [len] register ids starting at
    [reg_id_start] in the first element (present classical memory) of
    [hps.output.cmem_stack] using {!Mem.add_vec_x}. *)

val add_empty_cmem_stack_elem : t -> t
(** [add_empty_cmem_stack_elem hps] returns a new HPS with an empty memory added
    as the first element (present classical memory) of [hps.output.cmem_stack].
*)

val add_support : int list -> t -> t
(** [add_support yi_list hps] returns a new HPS with all elements of [yi_list]
    added to [hps.support]. *)

val set_y_count : int -> t -> t
(** [set_y_count y_count hps] returns a new HPS with [hps.y_count] set to
    [y_count]. *)

val simp_scalar : t -> t
(** Simplify the scalar of the given HPS. *)

val set_y_to_zero : int -> t -> t
(** [set_y_to_zero yi hps] returns a new hps with [yi] set to 0 in [hps]. *)

val set_y_to_one : int -> t -> t
(** [set_y_to_one yi hps] returns a new hps with [yi] set to 1 in [hps]. *)

val set_y_values : Y_set.t -> Y_set.t -> t -> t
(** [set_y_values yi_zeros yi_ones hps] returns a new hps with all [yi] from
    [yi_zeros] set to 0 and all [yi] from [yi_ones] set to 1 in [hps]. *)

val remove_cmem_stack_trailing_voids : t -> t
(** [remove_cmem_stack_trailing_voids hps] removes the trailing empty memory
    elements in [hps.output.cmem_stack]. *)

(** {1:inspection Inspection} *)

val norm2 : t -> Scalar.t
(** [norm2 hps] returns the squared norm of [hps]. It only works some case where
    it can be computed efficiently (without instantiating all the path variables
    y):
    - The phase must contain no input variable x and path variable y.
    - The scalar must contain no path variable y.
    - Every path variable y appearing in the output must appear alone in at
      least one monomial of the output (without other x or y).

    Raise [Failure] if the squared norm cannot be computed. *)

val norm2_opt : t -> Scalar.t option
(** [norm2_opt hps] returns the squared norm of [hps]. It only works some case
    where it can be computed efficiently (without instantiating all the path
    variables y):
    - The phase must contain no input variable x and path variable y.
    - The scalar must contain no path variable y.
    - Every path variable y appearing in the output must appear alone in at
      least one monomial of the output (without other x or y).

    Return [None] if the squared norm cannot be computed. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:conversions Conversions} *)

val qmem_reg_to_int : string -> int -> t -> Z.t
(** [qmem_reg_to_int name len hps] converts the [len] first ids of register
    [name] from [hps.output.qmem] (little-endian) to a Zarith big integer using
    {!Mem.reg_to_int}. *)

val qmem_reg_to_int_big_endian : string -> int -> t -> Z.t
(** [qmem_reg_to_int name len hps] converts the [len] first ids of register
    [name] from [hps.output.qmem] (big-endian) to a Zarith big integer using
    {!Mem.reg_to_int}. *)

val cmem_reg_to_int : string -> int -> t -> Z.t
(** [cmem_reg_to_int name len hps] converts the [len] first ids of register
    [name] from the first element (present classical memory) of
    [hps.output.cmem_stack] (little-endian) to a Zarith big integer using
    {!Mem.reg_to_int}. *)

val cmem_reg_to_int_big_endian : string -> int -> t -> Z.t
(** [cmem_reg_to_int name len hps] converts the [len] first ids of register
    [name] from the first element (present classical memory) of
    [hps.output.cmem_stack] (big-endian) to a Zarith big integer using
    {!Mem.reg_to_int}. *)

val to_string : t -> string
(** Convert to string. *)
