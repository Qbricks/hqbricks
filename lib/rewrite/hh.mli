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

(** HH rewrite rule.

    This module contains functions for the HH rule, formally:
    {math
      \frac
      {
      y_0\cap\texttt{su}=\emptyset
      \qquad
      y_1\cap\texttt{Pvar}(f,\mathbf{o}_\texttt{Cl})=\emptyset
      }
      {
      \left\langle\texttt{p}+\frac{y_0\cdot(y_1+\overline{f})}{2}
      ,\texttt{s}\cdot\texttt{o}\right\rangle_{\texttt{su}\cup y_0 \cup y_1}
      \equiv^{\texttt{p}}
      \left\langle\texttt{p},{2^{\#y_0}}\texttt{s}\cdot\texttt{o}\right\rangle_\texttt{su}
      [y_1\leftarrow f]
      } \text{(HH)}
    }
    The HH rule is derived from the equality {m HH=I}, it applies on two path
    variables {m y_0} and {m y_1} and removes them. *)

val check : int -> int -> Hps.t -> (unit, string) Stdlib.result
(** [check y0 y1 hps] checks if the HH rule can be applied to [hps] on path
    variables [y0] and [y1], that is, if:
    - [y0] is in the support.
    - [y1] is in the support.
    - The phase contains the monomial [1/2*y0*y1].
    - The phase contains no other monomial containing both [y0] and [y1].
    - All the monomials from the phase containing [y0] are of coefficient [1/2].
    - [y0] is not in the scalar.
    - [y0] is not in the output.
    - [y1] is not in the cmem.

    Returns [Ok ()] if the check succeeds and [Error str] if it fails. *)

val apply_unchecked : ?metrics:Metrics.t -> int -> int -> Hps.t -> Hps.t
(** [apply_unchecked ~metrics y0 y1 hps] returns a new HPS with the HH rule
    applied to [hps] on path variables [y0] and [y1] with no checks. That is:
    - All the monomials from the phase that contain [y0] are removed.
    - [y1] is replaced by the sum of all monomials that contained [y0] but
      without [y0] and except the [1/2*y0*y1] monomial.
    - The scalar is multiplied by 2.
    - [y0] and [y1] are removed from the support.

    The behavior is undefined if the rule cannot be applied.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val check_and_apply :
  ?metrics:Metrics.t -> int -> int -> Hps.t -> (Hps.t, string) Stdlib.result
(** [check_and_apply ~metrics y0 y1 hps] checks if the HH rule can be applied to
    [hps] on path variables [y0] and [y1] using {!check}, then:
    - If {!check} returns [Ok ()], returns a new HPS with the HH rule applied to
      [hps] on [y0] and [y1] using {!apply_unchecked}.
    - If {!check} returns [Error str], returns [Error str].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val find : Hps.t -> (int * int) option
(** Return a valid pair [Some (y0, y1)] if the HH rule can be applied on some
    path variables in the given HPS, or [None] otherwise. *)

val find_all : Hps.t -> (int * int) list
(** Return a list of all [(y0, y1)] pairs on which the HH rule can be applied in
    the given HPS. *)

val find_and_apply : ?metrics:Metrics.t -> Hps.t -> (int * int) option * Hps.t
(** [find_and_apply ~metrics hps] tries to find a valid pair of path variables
    on which the HH rule can be applied in [hps] using {!find}, then:
    - If {!find} returns [Some (y0, y1)], returns [Some (y0, y1)] and a new HPS
      with the HH rule applied to [hps] on [y0] and [y1] using
      {!apply_unchecked}.
    - If {!find} returns [None], returns [(None, hps)].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)

val find_and_apply_all : ?metrics:Metrics.t -> Hps.t -> (int * int) list * Hps.t
(** [find_and_apply_all ~metrics hps] finds [(y0, y1)] pairs on which the HH
    rule can be applied in [hps] and apply the rule repatedly until no further
    application is possible. Returns a list of all [(y0, y1)] pairs on which the
    HH rule has been applied, and a new HPS with all the HH rules applied in
    [hps].

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
