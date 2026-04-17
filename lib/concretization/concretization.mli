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

(** Concretization of {!Hps} into {!Vector_map} or probability distribution. *)

(** Vector Map of an HPS. *)
module Vector_map : sig
  type t
  (** Type of Vector Maps. *)

  val empty : t
  (** The empty Vector Map. *)

  val add : Hps.Mem_stack.t -> Hps.Mem.t -> Hps.Phase.t * Hps.Scalar.t -> t -> t
  (** [add cmem_stack qmem (phase, scalar) vector_map] returns a new Vector Map
      with [(phase, scalar)] added to the [cmem_stack] [qmem] association of
      [vector_map]. *)

  val equal : t -> t -> bool
  (** Equality test. *)

  val of_hps : Hps.t -> t
  (** Convert an HPS to a Vector_map. *)

  val to_string : t -> string
  (** Convert to string. *)
end

val hps_proba_output : Hps.Output.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_output output_spec hps] returns the probability that [hps]
    satisfies [output_spec], that is, [hps.output] is equal to [output_spec]. *)

val hps_proba_qmem : Hps.Mem.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_qmem qmem_spec hps] returns the probability that [hps] satisfies
    [qmem_spec], that is, [hps.qmem] is equal to [qmem_spec]. *)

val hps_proba_qmem_fact :
  ?metrics:Metrics.t -> Hps.Mem.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_qmem qmem_spec ~metrics hps] returns the probability that [hps]
    satisfies [qmem_spec], that is, [hps.qmem] is equal to [qmem_spec].

    [hps] is factorized as much as possible using
    {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all} before computing the
    probability.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val hps_proba_cmem_stack : Hps.Mem_stack.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_cmem_stack cms_spec hps] returns the probability that [hps]
    satisfies [cms_spec], that is, [hps.cmem_stack] is equal to [cms_spec]. *)

val hps_proba_cmem : Hps.Mem.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_cmem cmem_spec hps] returns the probability that [hps] satisfies
    [cmem_spec], that is, the present of [hps.cmem_stack] is equal to
    [cmem_spec]. *)

val hps_proba_cmem_split :
  ?metrics:Metrics.t -> Hps.Mem.t -> Hps.t -> Hps.Scalar.t
(** [hps_proba_cmem_split ~metrics cmem_spec hps] returns the probability that
    [hps] satisfies [cmem_spec], that is, the present of [hps.cmem_stack] is
    equal to [cmem_spec].

    [hps] is splitted using {!Rewrite.Split_clear.apply_with_target_cmem} then
    reduced using {!Rewrite.reduce_hps} before computing the probability.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
