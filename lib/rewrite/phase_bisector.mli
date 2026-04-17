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

(** Phase Bisector (PB) rewrite rule.

    This module implements functions for an iterative generalized version of the
    Phase Bisector rule.

    The PB rule removes a sum from the phase of the form:
    {math \sum_{k=0}^{n-1} 2^k\cdot y_{i+k}\cdot dy\cdot xs\quad (1)}
    then multiplies the scalar by:
    {math
      \frac{sin(\pi\cdot 2^n\cdot dy\cdot xs)}{sin(\pi\cdot dy\cdot xs)}\quad(2)
    }
    and finally adds a phase:
    {math (2^{n}-1)\cdot\frac{dy\cdot xs}{2}\quad(3)}
    ,it also removes all the {m y_k} involved. It applies on a start path
    variables [y_start] (included), an end path variable [y_end] (excluded), an
    {!Hps.Dyadic1} factor [dy], and an {!Hps.X_set} [xs], the associated sum
    {m (1)} is removed from the phase, the associated scalar {m (2)} is
    multyplied to the scalar, and the associated phase {m (3)} is added to the
    phase of the HPS . *)

val check :
  int ->
  int ->
  Hps.Dyadic1.t ->
  Hps.X_set.t ->
  Hps.t ->
  (unit, string) Stdlib.result
(** [check y_start y_end dy xs hps] checks if the VPB rule can be applied to
    [hps] on [y_start], [y_end], [dy], and [xs], that is, if:
    - [y_end - y_start >= 2].
    - [den_pow dy >= y_end - y_start].
    - [dy <> 0].
    - There is no y from [\[y_start,y_end)] in the output.
    - There is no y from [\[y_start,y_end)] in the scalar.
    - The sum pattern {m \sum_{k=0}^{n-1} 2^k\cdot y_{i+k}\cdot dy\cdot xs} is
      in the phase ([n] is the number of path variables and [i] is [y_start]).
    - There is no y from [\[y_start,y_end)] in the rest of the phase.

    Returns [Ok ()] if the check succeeds and [Error str] if it fails. *)

val apply_unchecked :
  ?metrics:Metrics.t ->
  int ->
  int ->
  Hps.Dyadic1.t ->
  Hps.X_set.t ->
  Hps.t ->
  Hps.t
(** [apply_unchecked ~metrics y_start y_end dy xs hps] returns a new HPS with
    the VPB rule applied to [hps] with no checks. That is, the sum
    {m \sum_{k=0}^{n-1} 2^k\cdot y_{i+k}\cdot dy\cdot xs} is removed from the
    phase, the scalar
    {m \frac{sin(\pi\cdot 2^n\cdot dy\cdot xs)}{sin(\pi\cdot dy\cdot xs)}} is
    multiplied to the scalar, and the phase
    {m (2^{n}-1)\cdot\frac{dy\cdot xs}{2}} is added to the phase of [hps] ([n]
    is the number of path variables and [i] is [y_start]). The behavior is
    undefined if the rule cannot be applied.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?metrics:Metrics.t ->
  int ->
  int ->
  Hps.Dyadic1.t ->
  Hps.X_set.t ->
  Hps.t ->
  (Hps.t, string) Stdlib.result
(** [check_and_apply ~metrics y_start y_end dy xs hps] checks if the VPB rule
    can be applied to [hps] on [y_start], [y_end], [dy], and [xs] using
    {!check}, then:
    - If {!check} returns [Ok ()], returns a new HPS with the VPB rule applied
      to [hps] using {!apply_unchecked}.
    - If {!check} returns [Error str], returns [Error str].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val find : Hps.t -> (Hps.Y_set.elt * int * Hps.Dyadic1.t * Hps.X_set.t) option
(** Return a valid tuple [Some (y_start, y_end, dy, xs)] if the VPB rule can be
    applied on some path variables in the given HPS, or [None] otherwise. *)

val find_all : Hps.t -> (Hps.Y_set.elt * int * Hps.Dyadic1.t * Hps.X_set.t) list
(** Return a list of all [(y_start, y_end, dy, xs)] tuples on which the VPB rule
    can be applied in the given HPS. *)

val find_and_apply :
  ?metrics:Metrics.t ->
  Hps.t ->
  (Hps.Y_set.elt * int * Hps.Dyadic1.t * Hps.X_set.t) option * Hps.t
(** [find_and_apply ~metrics hps] tries to find a valid tuple on which the VPS
    rule can be applied in [hps] using {!find}, then:
    - If {!find} returns [Some (y_start, y_end, dy, xs)], returns
      [Some (y_start, y_end, dy, xs)] and a new HPS with the VPB rule applied to
      [hps] on [y_start], [y_end], [dy], and [xs] using {!apply_unchecked}.
    - If {!find} returns [None], returns [(None, hps)].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val find_and_apply_all :
  ?metrics:Metrics.t ->
  Hps.t ->
  (Hps.Y_set.elt * int * Hps.Dyadic1.t * Hps.X_set.t) list * Hps.t
(** [find_and_apply_all hps] finds [(y_start, y_end, dy, xs)] tuples on which
    the VPB rule can be applied in [hps] and apply the rule repatedly until no
    further application is possible. Returns a list of all
    [(y_start, y_end, dy, xs)] tuples on which the VPB rule has been applied,
    and a new HPS with all the VPB rules applied in [hps].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
