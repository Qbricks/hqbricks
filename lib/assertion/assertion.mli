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

(** Assertions for {!Hps} and {!Prog} specification verification. *)

(** {1:full_hybrid_state_description_assertions Full Hybrid State Description
    Assertions} *)

val hps_eq : Hps.t -> Hps.t -> bool
(** Check if two HPS are syntactically equal. *)

val assert_hps_eq : Hps.t -> Hps.t -> unit
(** Assert {!hps_eq}. *)

val prog_eq : ?metrics:Metrics.t -> Prog.t -> Prog.t -> Hps.t -> bool
(** [prog_eq ~metrics prog1 prog2 hps_input] checks if [prog1] and [prog2] are
    equivalent. This is done by evaluating both programs into an HPS (with
    automatic rewriting) using [hps_input] as input, and then comparing the two
    resulting HPS using {!hps_eq}.

    If [~metrics] is provided, it is updated in place to reflect the
    computation; it has no effect on the result (see {!Metrics}). *)

val assert_prog_eq : ?metrics:Metrics.t -> Prog.t -> Prog.t -> Hps.t -> unit
(** Assert {!prog_eq}. *)

(** {1:satisfaction_assertions Satisfaction Assertions} *)

val hps_satisfies : ?metrics:Metrics.t -> Hps.t -> Hps.t -> bool
(** [hps_satisfies ~metrics hps_spec hps] checks if [hps] satisfies [hps_spec],
    that is, [hps] is equal to [hps_spec] up to discard.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies : ?metrics:Metrics.t -> Hps.t -> Hps.t -> unit
(** Assert {!hps_satisfies}. *)

(** {1:probabilistic_satisfaction_assertions Probabilistic Satisfaction
    Assertions} *)

val hps_satisfies_proba_output :
  ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Output.t -> Hps.t -> bool
(** [hps_satisfies_proba_output ~metrics p output_spec hps] checks if the
    probability that [hps] satisfies [output_spec], that is, [hps.output] is
    equal to [output_spec], is equal to [p].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_output :
  ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Output.t -> Hps.t -> unit
(** Assert {!hps_satisfies_proba_output}. *)

val hps_satisfies_proba_output_leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_output_leq ~epsilon_abs ~epsilon_rel ~metrics p
     output_spec hps] checks if the probability that [hps] satisfies
    [output_spec], that is, [hps.output] is equal to [output_spec], is less than
    or equal to [p] using {!Hps.Scalar.leq}.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_output_leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_output_leq}. *)

val hps_satisfies_proba_output_geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_output_geq ~epsilon_abs ~epsilon_rel ~metrics p
     output_spec hps] checks if the probability that [hps] satisfies
    [output_spec], that is, [hps.output] is equal to [output_spec], is less
    greater or equal to [p] using {!Hps.Scalar.geq}.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_output_geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_output_geq}. *)

val hps_satisfies_proba_output_lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_output_lt ~epsilon_abs ~epsilon_rel ~metrics p
     output_spec hps] checks if the probability that [hps] satisfies
    [output_spec], that is, [hps.output] is equal to [output_spec], is less than
    [p] using {!Hps.Scalar.lt}.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_output_lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_output_lt}. *)

val hps_satisfies_proba_output_gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_output_gt ~epsilon_abs ~epsilon_rel ~metrics p
     output_spec hps] checks if the probability that [hps] satisfies
    [output_spec], that is, [hps.output] is equal to [output_spec], is greater
    than [p] using {!Hps.Scalar.gt}.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_output_gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Output.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_output_gt}. *)

val hps_satisfies_proba_qmem :
  ?fact:bool -> ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Mem.t -> Hps.t -> bool
(** [hps_satisfies_proba_qmem ~fact ~metrics p qmem_spec hps] checks if the
    probability that [hps] satisfies [qmem_spec], that is, [hps.qmem] is equal
    to [qmem_spec], is equal to p. If [fact] is true, [hps] is factorized as
    much as possible using {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all}
    before computing the probability, true by default.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_qmem :
  ?fact:bool -> ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Mem.t -> Hps.t -> unit
(** Assert {!hps_satisfies_proba_qmem}. *)

val hps_satisfies_proba_qmem_leq :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_qmem_leq ~fact ~epsilon_abs ~epsilon_rel ~metrics p
     qmem_spec hps] checks if the probability that [hps] satisfies [qmem_spec],
    that is, [hps.qmem] is equal to [qmem_spec], is less than or equal to p. If
    [fact] is true, [hps] is factorized as much as possible using
    {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all} before computing the
    probability, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_qmem_leq :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_qmem_leq}. *)

val hps_satisfies_proba_qmem_geq :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_qmem_geq ~fact ~epsilon_abs ~epsilon_rel ~metrics p
     qmem_spec hps] checks if the probability that [hps] satisfies [qmem_spec],
    that is, [hps.qmem] is equal to [qmem_spec], is greater than or equal to p.
    If [fact] is true, [hps] is factorized as much as possible using
    {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all} before computing the
    probability, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_qmem_geq :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_qmem_geq}. *)

val hps_satisfies_proba_qmem_lt :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_qmem_lt ~fact ~epsilon_abs ~epsilon_rel ~metrics p
     qmem_spec hps] checks if the probability that [hps] satisfies [qmem_spec],
    that is, [hps.qmem] is equal to [qmem_spec], is less than p. If [fact] is
    true, [hps] is factorized as much as possible using
    {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all} before computing the
    probability, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_qmem_lt :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_qmem_lt}. *)

val hps_satisfies_proba_qmem_gt :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_qmem_gt ~fact ~epsilon_abs ~epsilon_rel ~metrics p
     qmem_spec hps] checks if the probability that [hps] satisfies [qmem_spec],
    that is, [hps.qmem] is equal to [qmem_spec], is greater than p. If [fact] is
    true, [hps] is factorized as much as possible using
    {!Rewrite.Fact_distr.Xy_vars.find_and_apply_all} before computing the
    probability, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_qmem_gt :
  ?fact:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_qmem_gt}. *)

val hps_satisfies_proba_cmem_stack :
  ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Mem_stack.t -> Hps.t -> bool
(** [hps_satisfies_proba_cmem_stack ~metrics p cms_spec hps] checks if the
    probability that [hps] satisfies [cms_spec], that is, [hps.cmem_stack] is
    equal to [cms_spec], is equal to p.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_stack :
  ?metrics:Metrics.t -> Hps.Scalar.t -> Hps.Mem_stack.t -> Hps.t -> unit
(** Assert {!hps_satisfies_proba_cmem_stack}. *)

val hps_satisfies_proba_cmem_stack_leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_stack_leq ~epsilon_abs ~epsilon_rel ~metrics p
     cms_spec hps] checks if the probability that [hps] satisfies [cms_spec],
    that is, [hps.cmem_stack] is equal to [cms_spec], is less than or equal to
    p.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_stack_leq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_stack_leq}. *)

val hps_satisfies_proba_cmem_stack_geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_stack_geq ~epsilon_abs ~epsilon_rel ~metrics p
     cms_spec hps] checks if the probability that [hps] satisfies [cms_spec],
    that is, [hps.cmem_stack] is equal to [cms_spec], is greater than or equal
    to p.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_stack_geq :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_stack_geq}. *)

val hps_satisfies_proba_cmem_stack_lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_stack_lt ~epsilon_abs ~epsilon_rel ~metrics p
     cms_spec hps] checks if the probability that [hps] satisfies [cms_spec],
    that is, [hps.cmem_stack] is equal to [cms_spec], is less than p.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_stack_lt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_stack_lt}. *)

val hps_satisfies_proba_cmem_stack_gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_stack_gt ~epsilon_abs ~epsilon_rel ~metrics p
     cms_spec hps] checks if the probability that [hps] satisfies [cms_spec],
    that is, [hps.cmem_stack] is equal to [cms_spec], is greater than p.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_stack_gt :
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem_stack.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_stack_gt}. *)

val hps_satisfies_proba_cmem :
  ?split:bool ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool
(** [hps_satisfies_proba_cmem ~split ~metrics p cmem_spec hps] checks if the
    probability that [hps] satisfies [cmem_spec], that is, the present of
    [hps.cmem] is equal to [cmem_spec], is equal to p. If [split] is true, [hps]
    is splitted using {!Rewrite.Split_clear.apply_with_target_cmem} and then
    reduced using {!Rewrite.reduce_hps} first, true by default.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem :
  ?split:bool ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem}. *)

val hps_satisfies_proba_cmem_leq :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_leq ~split ~epsilon_abs ~epsilon_rel ~metrics p
     cmem_spec hps] checks if the probability that [hps] satisfies [cmem_spec],
    that is, the present of [hps.cmem] is equal to [cmem_spec], is less than or
    equal to p. If [split] is true, [hps] is splitted using
    {!Rewrite.Split_clear.apply_with_target_cmem} and then reduced using
    {!Rewrite.reduce_hps} first, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_leq :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_leq}. *)

val hps_satisfies_proba_cmem_geq :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_geq ~split ~epsilon_abs ~epsilon_rel ~metrics p
     cmem_spec hps] checks if the probability that [hps] satisfies [cmem_spec],
    that is, the present of [hps.cmem] is equal to [cmem_spec], is greater than
    or equal to p. If [split] is true, [hps] is splitted using
    {!Rewrite.Split_clear.apply_with_target_cmem} and then reduced using
    {!Rewrite.reduce_hps} first, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_geq :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_geq}. *)

val hps_satisfies_proba_cmem_lt :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_lt ~split ~epsilon_abs ~epsilon_rel ~metrics p
     cmem_spec hps] checks if the probability that [hps] satisfies [cmem_spec],
    that is, the present of [hps.cmem] is equal to [cmem_spec], is less than p.
    If [split] is true, [hps] is splitted using
    {!Rewrite.Split_clear.apply_with_target_cmem} and then reduced using
    {!Rewrite.reduce_hps} first, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_lt :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_lt}. *)

val hps_satisfies_proba_cmem_gt :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  bool option
(** [hps_satisfies_proba_cmem_gt ~split ~epsilon_abs ~epsilon_rel ~metrics p
     cmem_spec hps] checks if the probability that [hps] satisfies [cmem_spec],
    that is, the present of [hps.cmem] is equal to [cmem_spec], is greater than
    p. If [split] is true, [hps] is splitted using
    {!Rewrite.Split_clear.apply_with_target_cmem} and then reduced using
    {!Rewrite.reduce_hps} first, true by default.

    [~epsilon_abs] and [~epsilon_rel] are used in case of float comparison and
    their default value is [1e-6].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val assert_hps_satisfies_proba_cmem_gt :
  ?split:bool ->
  ?epsilon_abs:Mlmpfr.mpfr_float ->
  ?epsilon_rel:Mlmpfr.mpfr_float ->
  ?metrics:Metrics.t ->
  Hps.Scalar.t ->
  Hps.Mem.t ->
  Hps.t ->
  unit
(** Assert {!hps_satisfies_proba_cmem_gt}. *)

(** {1:circuit_equivalence_assertions Circuit Equivalence Assertions} *)

val unitary_eq : ?metrics:Metrics.t -> Prog.t -> Prog.t -> Hps.t -> bool
(** [unitary_eq ~metrics prog1 prog2 hps_input] checks if [prog1] and [prog2]
    are equivalent. This is done by sequencing [prog1] with the inverse of
    [prog2], then evaluating the resulting program with [hps_input] as input and
    comparing the resulting hps with [hps_input] using {!hps_eq}.

    The [hps_input] parameter makes the verification more flexible by allowing
    equivalence to be checked on concrete inputs, symbolic inputs, or a
    combination of both.

    [prog1] and [prog2] must be unitaries, that is, must only contain quantum
    instructions.

    If [~metrics] is provided, it is updated in place to reflect the
    computation; it has no effect on the result (see {!Metrics}). *)

val assert_unitary_eq : ?metrics:Metrics.t -> Prog.t -> Prog.t -> Hps.t -> unit
(** Assert {!unitary_eq}. *)
