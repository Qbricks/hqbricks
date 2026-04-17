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

let () = Mlmpfr.set_default_prec 64

(* BRUS-H *)
let test_brus_h_k1 () = Brus.verify_brus_h ~handler:verbose Z.(Q.make ~$1 ~$2) 1

let test_brus_h_k10 () =
  Brus.verify_brus_h ~handler:verbose Z.(Q.make ~$1023 ~$1024) 10

let test_brus_h_k100 () =
  Brus.verify_brus_h ~handler:quiet
    Z.(
      Q.make
        (of_string "1267650600228229401496703205375")
        (of_string "1267650600228229401496703205376"))
    100

(* BRUS-H-Parallel *)
let test_brus_hp10_k1 () =
  let q_len = 10 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:verbose q_len Z.(Q.make ~$1 ~$1024) 1 mq_n

let test_brus_hp10_k10 () =
  let q_len = 10 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:verbose q_len
    Z.(
      Q.make
        (of_string "12325140160135610565932361727")
        (of_string "1267650600228229401496703205376"))
    10 mq_n

let test_brus_hp10_k100 () =
  let q_len = 10 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:quiet q_len
    Z.(
      Q.make
        (of_string
           "997388676291122345429342028281502710388492027041585762656534433869470769014336435456392852945014306778518572921121043190703473926013738219165855912623273693890191640173287465783187865335006500565269372084233350256687165082717252051903733906494256704659129819186512592224000238028122078535512779493375")
        (of_string
           "10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376"))
    100 mq_n

let test_brus_hp100_k100 () =
  let q_len = 100 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:quiet q_len
    (Brus.compute_brus_hp_spec_success_p 100 100)
    100 mq_n

let test_brus_hp100_k500 () =
  let q_len = 100 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:quiet q_len
    (Brus.compute_brus_hp_spec_success_p 100 500)
    500 mq_n

let test_brus_hp1000_k500 () =
  let q_len = 1000 in
  let mq_n = Test_helper.rand_zi q_len in
  Brus.verify_brus_hp ~handler:quiet q_len
    (Brus.compute_brus_hp_spec_success_p 1000 500)
    500 mq_n

(* BRUS-QPE *)
let test_brus_qpe_7_3_k1 () =
  Brus.verify_brus_qpe ~handler:verbose Z.(~$68) 7 3 1

let test_brus_qpe_7_3_k10 () =
  Brus.verify_brus_qpe ~handler:quiet Z.(~$67) 7 3 10

(* Tests *)
let () =
  let open Alcotest in
  run "BRUS"
    [
      ( "H",
        [
          test_case "BRUS-H-k1" `Quick test_brus_h_k1;
          test_case "BRUS-H-k10" `Quick test_brus_h_k10;
          test_case "BRUS-H-k100" `Quick test_brus_h_k100;
        ] );
      ( "H-Parallel",
        [
          test_case "BRUS-HP10-k1" `Quick test_brus_hp10_k1;
          test_case "BRUS-HP10-k10" `Quick test_brus_hp10_k10;
          test_case "BRUS-HP10-k100" `Quick test_brus_hp10_k100;
          test_case "BRUS-HP100-k100" `Quick test_brus_hp100_k100;
          test_case "BRUS-HP100-k500" `Quick test_brus_hp100_k500;
          test_case "BRUS-HP1000-k500" `Slow test_brus_hp1000_k500;
        ] );
      ( "QPE",
        [
          test_case "BRUS-QPE-7-3-k1" `Quick test_brus_qpe_7_3_k1;
          test_case "BRUS-QPE-7-3-k10" `Quick test_brus_qpe_7_3_k10;
        ] );
    ]
