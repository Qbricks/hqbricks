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

(** Factorize Distr (FD) rewrite rule.

    This module contains functions for the Factorize Distr rule, formally:
    {math
      \frac
      {
      \texttt{Pvar}(\texttt{p}_1,\texttt{o}_1,\texttt{s}_1,\texttt{su}_1)
      \cap\texttt{Pvar}(\texttt{p}_2,\texttt{o}_2,\texttt{s}_2,
      \texttt{su}_2) = \emptyset
      \qquad
      \texttt{0ad}_1\cap \texttt{0ad}_2 = \emptyset
      }
      {
      \left\langle\texttt{p}_1,\texttt{s}_1\cdot\texttt{o}_1\right\rangle_{\texttt{su}_1}
      \otimes
      \left\langle\texttt{p}_2,\texttt{s}_2\cdot\texttt{o}_2\right\rangle_{\texttt{su}_2}
      \equiv^{\texttt{p}}
      \left\langle\texttt{p}_1+\texttt{p}_2,\texttt{s}_1*\texttt{s}_2\cdot
      \texttt{o}_1\cup\texttt{o}_2\right\rangle_{\texttt{su}_1\cup\texttt{su}_2}
      }
      \text{(FD)}
    }
    The FD rule is a factorisation rule used to separate an HPS into a tensor
    product of two HPS. It has two implementation:
    - Register separation (see {!Reg}).
    - Variable separation (see {!Xy_vars}), mainly used to automatically
      factorize an HPS to the maximum using the {!Xy_vars.find_and_apply_all}
      heuristic. *)

(** {!Fact_distr} rewrite rule to separate registers.

    It applies on a set of registers [reg_name_set] and returns two HPS:
    - The first HPS contains the registers from [reg_name_set], and the parts of
      the phase, scalar, and support that either contain path variables
      appearing in the registers from [reg_name_set] or only input variables.
    - The second HPS contains the rest the registers which are not in
      [reg_name_set], and the parts of the phase, scalar, and support that
      contain either path variables that don't appear in the registers from
      [reg_name_set] or no variables at all. *)
module Reg : sig
  val check :
    ?past_only:bool ->
    Utils.String_set.t ->
    Hps.t ->
    (unit, string) Stdlib.result
  (** [check ~past_only reg_name_set hps] checks if the FD rule can be applied
      to [hps] on [reg_name_set]. This is done by checking if [hps] is separable
      (not entangled) into:
      - An HPS contaning the path variables appearing in registers from
        [reg_name_set]. If [~past_only] is [true], it only contains the path
        variables appearing registers from [reg_name_set] in the past of the
        classical memory.
      - An HPS containing the other path variables.

      Returns [Ok ()] if the check succeeds and [Error str] if it fails.
      [~past_only] is [false] by default. *)

  val apply_unchecked :
    ?past_only:bool ->
    ?metrics:Metrics.t ->
    Utils.String_set.t ->
    Hps.t ->
    Hps.t * Hps.t
  (** [apply_unchecked ~past_only ~metrics reg_name_set hps] applies the FD rule
      to [hps] on [reg_name_set] with no checks, and returns 2 new HPS:
      - The first HPS contains the registers from [reg_name_set], and the parts
        of the phase, scalar, and support that either contain path variables
        appearing in the registers from [reg_name_set] or only input variables.
        If [~past_only] is [true], it only contains the registers and path
        variables from [reg_name_set] from the past of the classical memory.
      - The second HPS contains the remaining parts of [hps].

      The behavior is undefined if the rule cannot be applied. [~past_only] is
      [false] by default.

      If [~metrics] is provided, it is updated in place to reflect the
      computation (only rewrite counters may be incremented); it has no effect
      on the result (see {!Metrics}). *)

  val check_and_apply :
    ?past_only:bool ->
    ?metrics:Metrics.t ->
    Utils.String_set.t ->
    Hps.t ->
    (Hps.t * Hps.t, string) Stdlib.result
  (** [check_and_apply ~past_only ~metrics reg_name_set hps] checks if the FD
      rule can be applied to [hps] on [reg_name_set] using {!check}, then:
      - If {!check} returns [Ok ()], returns a new HPS with the FD rule applied
        to [hps] on [reg_name_set] using {!apply_unchecked}.
      - If {!check} returns [Error str], returns [Error str].

      [~past_only] is [false] by default.

      If [~metrics] is provided, it is updated in place to reflect the
      computation (only rewrite counters may be incremented); it has no effect
      on the result (see {!Metrics}). *)
end

(** {!Fact_distr} rewrite rule to separate variables.

    It applies on a number of parts [n] to separate and a function [f] that
    assigns each variable to a part, and returns a product of [n] new HPS as an
    array, where the [i]-th element contains the parts of the original HPS
    containing only x and y variables [v] such that [f v = i]. *)
module Xy_vars : sig
  val check : int -> (Hps.Var.t -> int) -> Hps.t -> (unit, string) Stdlib.result
  (** [check n f hps] checks if the FD rule can be applied to [hps] on [n] and
      [f]. This is done by checking if [hps] is separable (not entangled) into
      [n] HPS (indexed from 0 to n - 1) such that the [i]-th HPS contains only x
      and y variables [v] such that [f v = i].

      Returns [Ok ()] if the check succeeds and [Error str] if it fails. *)

  val apply_unchecked :
    ?metrics:Metrics.t -> int -> (Hps.Var.t -> int) -> Hps.t -> Hps.t array
  (** [apply_unchecked ~metrics n f hps] applies the FD rule to [hps] on [n] and
      [f] with no checks, and returns a product of [n] new HPS as an array,
      where the [i]-th element contains the parts of [hps] containing only x and
      y variables [v] such that [f v = i]. The parts containing no x and y
      variables go to the first element of the array. The behavior is undefined
      if the rule cannot be applied.

      If [~metrics] is provided, it is updated in place to reflect the
      computation (only rewrite counters may be incremented); it has no effect
      on the result (see {!Metrics}). *)

  val check_and_apply :
    ?metrics:Metrics.t ->
    int ->
    (Hps.Var.t -> int) ->
    Hps.t ->
    (Hps.t array, string) Stdlib.result
  (** [check_and_apply ~metrics n f hps] checks if the FD rule can be applied to
      [hps] on [n] and [f] using {!check}, then:
      - If {!check} returns [Ok ()], returns a new HPS with the FD rule applied
        to [hps] on [n] and [f] using {!apply_unchecked}.
      - If {!check} returns [Error str], returns [Error str].

      If [~metrics] is provided, it is updated in place to reflect the
      computation (only rewrite counters may be incremented); it has no effect
      on the result (see {!Metrics}). *)

  val find_and_apply_all : ?metrics:Metrics.t -> Hps.t -> Hps.t array
  (** [find_and_apply_all ~metrics hps] finds the maximal factorizations of
      [hps] and returns the factorized HPS in the form of an array of HPS using
      the FD rule. If no factorization can be applied, it returns [[| hps |]].

      If [~metrics] is provided, it is updated in place to reflect the
      computation (only rewrite counters may be incremented); it has no effect
      on the result (see {!Metrics}). *)
end
