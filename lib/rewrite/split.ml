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

let check yi hps =
  if not Hps.(Support.contains_y yi hps.support) then
    Error (Printf.sprintf "y%d is not in support" yi)
  else Ok ()

let apply_unchecked ?metrics yi hps =
  Metrics.add_rewrites_opt metrics 1;
  Hps.(set_y_to_zero yi hps, Hps.set_y_to_one yi hps)

let check_and_apply ?metrics yi hps =
  match check yi hps with
  | Error s -> Error s
  | Ok () -> Ok (apply_unchecked yi hps ?metrics)
