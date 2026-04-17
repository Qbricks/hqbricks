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

(** Evaluator from {!Prog} to {!Hps}.

    The parameter [var_val] of the evaluation functions is empty by default and
    is usually setup automatically during the evaluation, it is a map from
    [string] to [int] that can be used to give values to {!Prog.Base.pr_int}
    variables (for example to evaluate for loops). *)

module Base = Base

(** {1:rewrite_settings_flags Rewrite settings flags} *)

val no_rewrite : int
(** Rewrite settings flag to disable all rewrite. *)

val interactive_rewrite : int
(** Rewrite settings flag to enable interactive rewrite in command line after
    each evaluation step. *)

val auto_hh : int
(** Rewrite settings flag to enable automatic HH rewrite, the HH are applied
    before each measure and at the end of the evaluation. *)

val auto_change_var : int
(** Rewrite settings flag to enable automatic Change-var rewrite, the heuristic
    for automatic Change-var searches for y xor constant (x, true or false) and
    then replaces y by y xor constant, the Change-var are applied at the end of
    the evaluation. *)

val all_auto : int
(** Rewrite settings flag to enable all automatic rewrite flags. *)

val all_rewrite : int
(** Rewrite settings flag to enable all rewrite flags. *)

(** {1:evaluation Evaluation} *)

val evaluate_prog :
  ?var_val:int Utils.Var_name_map.t ->
  ?rewrite_settings:int ->
  ?print:bool ->
  ?metrics:Metrics.t ->
  Prog.t ->
  Hps.t ->
  Hps.t
(** [evaluate_prog ~var_val ~rewrite_settings ~print ~metrics prog hps]
    evaluates [prog] into an HPS using [hps] as input.

    [rewrite_settings] can be set using the {!rewrite_settings_flags}, rewrite
    flags can be combined using [+] (for example
    [interactive_rewrite + auto_hh]), {!no_rewrite} by default.

    [print] can be set to true/false to enable/disable the printing of details
    during the evaluation, enabled by default.

    If [~metrics] is provided, it is updated in place to reflect the
    computation; it has no effect on the result (see {!Metrics}). *)
