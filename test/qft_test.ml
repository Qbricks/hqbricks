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

(* QFT-QFTDAG *)
let test_qft_qftdag_5 () = Qft.verify_qft_qftdag ~handler:verbose 5
let test_qft_qftdag_50 () = Qft.verify_qft_qftdag ~handler:quiet 50
let test_qft_qftdag_500 () = Qft.verify_qft_qftdag ~handler:quiet 500

(* QFT-MQFT-Eq *)
let test_qft_mqft_eq_5 () = Qft.verify_qft_mqft_eq ~handler:verbose 5
let test_qft_mqft_eq_50 () = Qft.verify_qft_mqft_eq ~handler:verbose 50
let test_qft_mqft_eq_500 () = Qft.verify_qft_mqft_eq ~handler:verbose 500

(* Tests *)
let () =
  let open Alcotest in
  run "QFT"
    [
      ( "QFT-QFTDAG",
        [
          test_case "QFT-QFTDAG 5" `Quick test_qft_qftdag_5;
          test_case "QFT-QFTDAG 50" `Quick test_qft_qftdag_50;
          test_case "QFT-QFTDAG 500" `Slow test_qft_qftdag_500;
        ] );
      ( "QFT-MQFT-Eq",
        [
          test_case "QFT-MQFT equivalence 5" `Quick test_qft_mqft_eq_5;
          test_case "QFT-MQFT equivalence 50" `Quick test_qft_mqft_eq_50;
          test_case "QFT-MQFT equivalence 500" `Slow test_qft_mqft_eq_500;
        ] );
    ]
