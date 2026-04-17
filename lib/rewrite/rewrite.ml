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

module Hh = Hh
module Change_var = Change_var
module Fact_distr = Fact_distr
module Discard = Discard
module Split = Split
module Split_clear = Split_clear
module Phase_bisector = Phase_bisector

let reduce_hps ?metrics hps =
  let _, hps = Change_var.find_and_apply_all_y_xor_no_y ?metrics hps in
  let _, hps = Hh.find_and_apply_all ?metrics hps in
  let _, hps = Phase_bisector.find_and_apply_all ?metrics hps in
  hps
