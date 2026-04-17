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

(** Split Clear (SC) rewrite rule.

    This module implements functions for the Split Clear rule.

    The SC rule splits an HPS into cases (see {!Split}) and eliminates all cases
    but one, usually in order to analyse this specific case. It applies on a set
    of path variables [y_zeros] which will be set to 0 and a set of path
    variables [y_ones] which will be set to 1.

    The main use of this rule is with the {!apply_with_target_cmem} function,
    which filters worlds satisfying a given classical memory assignment by
    eliminating paths that do not satisfy this assignment in the HPS, and is
    generally used to improve the efficiency of probability computations. *)

val check : Hps.Y_set.t -> Hps.Y_set.t -> Hps.t -> (unit, string) Stdlib.result
(** [check y_zeros y_ones hps] checks if the Split rule can be applied to [hps]
    on [y_zeros] and [y_ones], that is, if:
    - [y_zeros] and [y_ones] are disjoint.
    - All path variables from [y_zeros] and [y_ones] are in [hps] support.

    Returns [Ok ()] if the check succeeds and [Error str] if it fails. *)

val apply_unchecked :
  ?metrics:Metrics.t -> Hps.Y_set.t -> Hps.Y_set.t -> Hps.t -> Hps.t
(** [apply_unchecked ~metrics y_zeros y_ones hps] returns a new HPS with the SC
    rule applied to [hps] with no checks. That is, all the path variables from
    [y_zeros] are set to 0 and all the path variables from [y_ones] are set to
    1. The behavior is undefined if the rule cannot be applied.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?metrics:Metrics.t ->
  Hps.Y_set.t ->
  Hps.Y_set.t ->
  Hps.t ->
  (Hps.t, string) Stdlib.result
(** [check_and_apply ~metrics y_zeros y_ones hps] checks if the SC rule can be
    applied to [hps] on [y_zeros] and [y_ones] using {!check}, then:
    - If {!check} returns [Ok ()], returns a new HPS with the SC rule applied to
      [hps] using {!apply_unchecked}, that is, with all the path variables from
      [y_zeros] set to 0 and all the path variables from [y_ones] set to 1.
    - If {!check} returns [Error str], returns [Error str].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val apply_with_target_cmem :
  ?metrics:Metrics.t -> Hps.Mem.t -> Hps.t -> (Hps.t, string) Stdlib.result
(** [apply_with_target_cmem ~metrics target_cmem hps] tries to find a pair of
    sets [(y_zeros, y_ones)] such that by replacing path variables from
    [y_zeros] by 0 and path variables from [y_one] by 1 in [hps], the present
    classical memory of [hps] is equal to the given target classical memory
    [target_cmem]. If a pair [(y_zeros, y_ones)] is found, returns a new HPS
    with the SC rule applied to [hps], that is, with all the path variables from
    [y_zeros] set to 0 and all the path variables from [y_ones] set to 1. If no
    pair is found, returns [Error str].

    The goal of this heuristic is to quickly analyse a specific case of an HPS
    satisfying a given present classical memory value by avoiding to instantiate
    and check all the possible combinations of values of path variables which
    grows exponentially, thus it only works in some cases where the pair of sets
    of path variables can be found efficiently.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
