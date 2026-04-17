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

let test_ghz_1 () = Ghz.verify ~handler:verbose 1
let test_ghz_10 () = Ghz.verify ~handler:verbose 10
let test_ghz_100 () = Ghz.verify ~handler:quiet 100

(* Tests *)
let () =
  let open Alcotest in
  run "GHZ"
    [
      ( "Symbolic",
        [
          test_case "GHZ 1" `Quick test_ghz_1;
          test_case "GHZ 10" `Quick test_ghz_10;
          test_case "GHZ 100" `Quick test_ghz_100;
        ] );
    ]
