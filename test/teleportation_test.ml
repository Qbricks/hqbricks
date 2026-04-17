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

open Algo_verif
open Verif_handlers

let test_teleportation_1 () = Teleportation.verify ~handler:verbose 1
let test_teleportation_100 () = Teleportation.verify ~handler:quiet 100
let test_teleportation_10000 () = Teleportation.verify ~handler:quiet 10000

(* Tests *)
let () =
  let open Alcotest in
  run "Teleportation"
    [
      ( "Symbolic",
        [
          test_case "Teleportation 1" `Quick test_teleportation_1;
          test_case "Teleportation 100" `Quick test_teleportation_100;
          test_case "Teleportation 10000" `Slow test_teleportation_10000;
        ] );
    ]
