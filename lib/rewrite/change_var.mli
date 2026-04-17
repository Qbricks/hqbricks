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

(** Change Variable (CV) rewrite rule.

    This module contains functions for the Change Variable rule, formally:
    {math
      \frac
      {\sigma\in\mathfrak S({\{0,1\}}^{\#(y)})\text{ bijective}}
      {\texttt{h}\equiv^{\texttt{p}}\texttt{h}[y\leftarrow\sigma(y)]}
      \text{(CV)}
    }
    The CV rule allows changing path variables, and is usually used to reduce
    the number of path variables or input variables. Currently, this module only
    handles a particular case of the CV rule, where there is a single path
    variable {m yi} updated such as
    {m yi:=yi\oplus\bigoplus y\oplus b,y\in\texttt{su}\setminus\{yi\},\texttt{Pvar}(b)=\emptyset},
    it applies on a path variable {m yi} with a new value [new_val] of type
    {!Hps.Hket} by which {m yi} will be replaced. *)

val check : int -> Hps.Hket.t -> Hps.t -> (unit, string) Stdlib.result
(** [check yi new_val hps] checks if the CV rule can be applied to [hps] on path
    variables on [yi] and [new_val], that is, if:
    - [yi] is in the support of [hps].
    - [yi] is in [new_val].
    - All path variables from [new_val] are in the support of [hps].
    - All monomials from [new_val] that contain a path variable don't contain
      any other variables.

    Returns [Ok ()] if the check succeeds and [Error str] if it fails. *)

val apply_unchecked : ?metrics:Metrics.t -> int -> Hps.Hket.t -> Hps.t -> Hps.t
(** [apply_unchecked ~metrics yi new_val hps] returns a new HPS with the CV rule
    applied to [hps] on path variables [yi] and [new_val] with no checks. That
    is, [yi] is replaced by [new_val] in [hps]. The behavior is undefined if the
    rule cannot be applied.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?metrics:Metrics.t ->
  int ->
  Hps.Hket.t ->
  Hps.t ->
  (Hps.t, string) Stdlib.result
(** [check_and_apply ~metrics yi new_val hps] checks if the CV rule can be
    applied to [hps] on [yi] and [new_val] using {!check}, then:
    - If {!check} returns [Ok ()], returns a new HPS with the CV rule applied to
      [hps] using {!apply_unchecked}.
    - If {!check} returns [Error str], returns [Error str].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val find_and_apply_all_y_xor_no_y :
  ?metrics:Metrics.t -> Hps.t -> Hps.Hket.t Utils.Int_map.t * Hps.t
(** [find_and_apply_all_y_xor_no_y ~metrics hps] finds all the patterns in [hps]
    where some path variable [yi] appears only in the form [yi + b] where b
    doesn't contain any path variable, and replace [yi] by [yi + b] by applying
    the CV rule in order to remove b. Returns a map containing all the [yi]
    (keys) [new_val] (values) pairs on which the CV rule has been applied, and a
    new HPS with all the CV rules applied in [hps].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
