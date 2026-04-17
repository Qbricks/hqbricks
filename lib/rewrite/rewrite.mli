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

(** HPS rewrite rules.

    This module implements rewrite rules for {!Hps}. A rewrite rule usually
    provides some or all of the following functions:
    - [check]: checks weither the rule can be applied with the given parameters.
    - [apply_unchecked]: applies the rule on the given parameters without
      checking if it can be applied.
    - [check_and_apply]: checks weither the rule can be applied with the given
      parameters and apply it if it can.
    - [find]: finds one parameter group to which the rule can be applied.
    - [find_all]: finds all parameters groups to which the rule can be applied.
    - [find_and_apply]: finds one parameter group to which the rule can be
      applied and apply it.
    - [find_and_apply_all]: finds parameter groups to which the rule can be
      applied and apply the rule repeatedly until no further application is
      possible. *)

module Hh = Hh
module Change_var = Change_var
module Fact_distr = Fact_distr
module Discard = Discard
module Split = Split
module Split_clear = Split_clear
module Phase_bisector = Phase_bisector

val reduce_hps : ?metrics:Metrics.t -> Hps.t -> Hps.t
(** Reduce the given HPS using the Change_var, HH, and Phase_bisector rules.

    If [~metrics] is provided, it is updated in place to reflect the computation
    (only rewrite counters may be incremented); it has no effect on the result
    (see {!Metrics}). *)
