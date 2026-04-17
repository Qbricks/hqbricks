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

(** Discard (Disc) rewrite rule.

    This module contains functions for the Disard rule, formally:
    {math
      \frac
      {
      \left|\texttt{h}_1\right| = 1
      \qquad
      \texttt{h}_1 \cap X = \emptyset
      }
      {
      \texttt{h}_1\otimes\texttt{h}_2 \Rrightarrow^{\texttt{P}} \texttt{h}_2
      } \text{(Disc)}
    }

    The Disc rule can be used to discard a subsystem and obtain a partial
    desciption of the state instead, by separating a state using the
    {!Fact_distr} rule and discarding one part of the tensor product of HPS. It
    applies on a set of registers [reg_name_set] which are discarded. *)

val check :
  ?past_only:bool ->
  Hps.Reg_name_set.t ->
  Hps.t ->
  (Hps.t * Hps.t, string) Stdlib.result
(** [check ~past_only reg_name_set hps] checks if the Disc rule can be applied
    to [hps] on [reg_name_set]. This is done by applying the {!Fact_distr} rule
    on [hps], and then checking if the part to discard contains no input
    variables and either is of norm 1 or it's norm can be computed in order to
    make it of norm 1 by applying a factor to the scalar of the kept part.
    Returns [Ok (hps_keep, hps_disc)] if the check succeeds and [Error str] if
    it fails. If [~past_only] is true, only the registers from [reg_name_set] in
    the past of the classical memory are discarded, [false] by default. *)

val apply_unchecked :
  ?past_only:bool ->
  ?metrics:Metrics.t ->
  Hps.Reg_name_set.t ->
  Hps.t ->
  Hps.t * Hps.t
(** [apply_unchecked ~past_only ~metrics reg_name_set hps] applies the
    {!Fact_distr} rule on [hps], and returns [(hps_keep, hps_disc)] without
    checking input variables and norm.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?past_only:bool ->
  ?metrics:Metrics.t ->
  Hps.Reg_name_set.t ->
  Hps.t ->
  (Hps.t * Hps.t, string) Stdlib.result
(** [check ~past_only ~metrics reg_name_set hps] checks if the Disc rule can be
    applied to [hps] on [reg_name_set]. This is done by applying the
    {!Fact_distr} rule on [hps], and then checking if the part to discard
    contains no input variables and either is of norm 1 or it's norm can be
    computed in order to make it of norm 1 by applying a factor to the scalar of
    the kept part. Returns [Ok (hps_keep, hps_disc)] if the check succeeds and
    [Error str] if it fails. If [~past_only] is true, only the registers from
    [reg_name_set] in the past of the classical memory are discarded, [false] by
    default.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
