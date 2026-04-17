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

open Hqbricks.Prog
open Hqbricks.Gate_set_impl.Clifford_k

let bell_state q0 q1 = (q0 |> h) -- (qbit_val q0 ~$0 => (q1 |> x))
let q0 = qreg "q0" ~$1
let q1 = qreg "q1" ~$1
let prog = bell_state q0 q1
let () = print_endline "Prog:"
let () = print prog
let () = print_endline ""
let prog_qbircks = Hqbricks.Qbircks.of_prog prog
let () = print_endline "Prog:"
let () = print prog
let prog_json = Hqbricks.Qbircks.to_yojson prog_qbircks
let () = print_endline "Prog json:"
let () = print_endline (Yojson.Safe.to_string prog_json)
