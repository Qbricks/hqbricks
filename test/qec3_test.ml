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

open Hqbricks
open Algo_verif
open Verif_handlers

(* Symbolic-Input-Concrete-P *)
let test_qec3_l1_n0 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$0), Z.(~$1)) in
  let spec_success_p = Hps.Scalar.one in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

let test_qec3_l1_n1_d4 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$4)) in
  let spec_success_p = Hps.Scalar.SFrac (Z.(~$27), Z.(~$32)) in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

let test_qec3_l1_n1_d2 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$2)) in
  let spec_success_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$2)) in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

let test_qec3_l1_n3_d4 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$3), Z.(~$4)) in
  let spec_success_p = Hps.Scalar.SFrac (Z.(~$5), Z.(~$32)) in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

let test_qec3_l1_n1 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$1)) in
  let spec_success_p = Hps.Scalar.zero in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

let test_qec3_l100_n1_d3 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$3)) in
  let spec_success_p = Hps.Scalar.SFrac (Z.(~$20 ** 100), Z.(~$27 ** 100)) in
  Qec3.verify ~handler:quiet error_p spec_success_p 100

let test_qec3_l1000_n1_d5 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$1), Z.(~$5)) in
  let spec_success_p =
    Hps.Scalar.SFrac (Z.(~$112 ** 1000), Z.(~$125 ** 1000))
  in
  Qec3.verify ~handler:quiet error_p spec_success_p 1000

let test_qec3_l2000_n2_d5 () =
  let error_p = Hps.Scalar.SFrac (Z.(~$2), Z.(~$5)) in
  let spec_success_p = Hps.Scalar.SFrac (Z.(~$81 ** 2000), Z.(~$125 ** 2000)) in
  Qec3.verify ~handler:quiet error_p spec_success_p 2000

(* Symbolic-Input-Symbolic-P *)
let test_qec3_l1_p () =
  let error_p = Hps.Scalar.SVar "p" in
  let spec_success_p =
    Hps.Scalar.(
      SAdd
        ( SAdd
            ( SAdd
                ( SMul
                    ( SMul
                        ( SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")),
                          SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")) ),
                      SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")) ),
                  SMul
                    ( SMul
                        ( SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")),
                          SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")) ),
                      SVar "p" ) ),
              SMul
                ( SMul
                    (SVar "p", SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p"))),
                  SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")) ) ),
          SMul
            ( SMul (SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")), SVar "p"),
              SAdd (SFrac (Z.(~$1), Z.(~$1)), SNeg (SVar "p")) ) ))
  in
  Qec3.verify ~handler:verbose error_p spec_success_p 1

(* Tests *)
let () =
  let open Alcotest in
  run "QEC3"
    [
      ( "Symbolic-Input-Concrete-P",
        [
          test_case "QEC3 1 0" `Quick test_qec3_l1_n0;
          test_case "QEC3 1 1/4" `Quick test_qec3_l1_n1_d4;
          test_case "QEC3 1 1/2" `Quick test_qec3_l1_n1_d2;
          test_case "QEC3 1 3/4" `Quick test_qec3_l1_n3_d4;
          test_case "QEC3 1 1" `Quick test_qec3_l1_n1;
          test_case "QEC3 100 1/3" `Quick test_qec3_l100_n1_d3;
          test_case "QEC3 1000 1/5" `Slow test_qec3_l1000_n1_d5;
          test_case "QEC3 2000 2/5" `Slow test_qec3_l2000_n2_d5;
        ] );
      ( "Symbolic-Input-Symbolic-P",
        [ test_case "QEC3 1 p" `Quick test_qec3_l1_p ] );
    ]
