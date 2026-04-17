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

(** Split rewrite rule.

    This module contains functions for the Split rule, formally:
    {math
      \frac
      {y \in \texttt{Pvar}(\texttt{h})}
      {
      \texttt{h} \equiv^{\texttt{p}} \texttt{h}[y:=0] \uplus \texttt{h}[y:=1]
      } \text{(Split)}
    }
    The Split rule can be used to do case analysis on a path variable {m y} by
    splitting the HPS into a union sum of two HPS, one where {m y} has been
    replaced by 0, and the other where {m y} has been replaced by 1. *)

val check : int -> Hps.t -> (unit, string) Stdlib.result
(** [check y hps] checks if the Split rule can be applied to [hps] on path
    variables [y], that is, if [y] is in [hps] support. Returns [Ok ()] if the
    check succeeds and [Error str] if it fails. *)

val apply_unchecked : ?metrics:Metrics.t -> int -> Hps.t -> Hps.t * Hps.t
(** [apply_unchecked ~metrics y hps] returns a pair of HPS [(hps0, hps1)] with
    the Split rule applied to [hps] on path variables [y] with no checks, where
    [hps0] is [hps] with [y] replaced by 0 and [hps1] is [hps] with [y] replaced
    by 1. The behavior is undefined if the rule cannot be applied.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?metrics:Metrics.t -> int -> Hps.t -> (Hps.t * Hps.t, string) Stdlib.result
(** [check_and_apply ~metrics y hps] checks if the Split rule can be applied to
    [hps] on path variables [y] using {!check}, then:
    - If {!check} returns [Ok ()], returns a pair of HPS [(hps0, hps1)] with the
      Split rule applied to [hps] on [y] using {!apply_unchecked}, where [hps0]
      is [hps] with [y] replaced by 0 and [hps1] is [hps] with [y] replaced by
      1.
    - If {!check} returns [Error str], returns [Error str].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
