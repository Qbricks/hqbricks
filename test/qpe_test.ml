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

let () = Mlmpfr.set_default_prec 64

(* QPE-Simple *)
let test_qpe_simple_exact_3 () =
  Qpe.verify_simple ~handler:quiet ~exact:true Z.(~$5) 3 3

let test_qpe_simple_exact_5 () =
  Qpe.verify_simple ~handler:quiet ~exact:true Z.(~$23) 5 5

let test_qpe_simple_approx_3 () = Qpe.verify_simple ~handler:quiet Z.(~$47) 6 3

let test_qpe_simple_approx_5 () =
  Qpe.verify_simple ~handler:quiet Z.(~$983) 10 5

let test_qpe_simple_rand100 () =
  for _i = 0 to 99 do
    let theta_den_pow = Random.int_in_range ~min:3 ~max:30 in
    let theta_num = Test_helper.rand_zi theta_den_pow in
    let c_len = Random.int_in_range ~min:2 ~max:(theta_den_pow - 1) in
    Qpe.verify_simple ~handler:quiet theta_num theta_den_pow c_len
  done

(* QPE-Concrete *)
let test_qpe_concrete_exact_rand100 () =
  for _i = 0 to 99 do
    let e_len = Random.int_in_range ~min:3 ~max:30 in
    let theta_num = Test_helper.rand_zi e_len in
    Qpe.verify_concrete ~handler:quiet ~exact:true theta_num e_len e_len;
    Qpe.verify_concrete ~handler:quiet ~gate:true ~exact:true theta_num e_len
      e_len
  done

let test_qpe_concrete_exact_e_gt_c () =
  Qpe.verify_concrete ~handler:verbose ~exact:true Z.(~$852) 10 8;
  Qpe.verify_concrete ~handler:quiet ~gate:true ~exact:true Z.(~$852) 10 8

let test_qpe_concrete_exact_smallest_rsa_number_not_factorized () =
  let e_len = 862 in
  let c_len = 862 in
  let theta_num =
    Z.of_string
      "22112825529529666435281085255026230927612089502470015394413748319128822941402001986512729726569746599085900330031400051170742204560859276357953757185954298838958709229238491006703034124620545784566413664540684214361293017694020846391065875914794251435144458199"
  in
  Qpe.verify_concrete ~handler:quiet ~exact:true theta_num e_len c_len

let test_qpe_concrete_approx_6e_3c () =
  let e_len = 6 in
  let c_len = 3 in
  let theta_num = Z.(~$47) in
  Qpe.verify_concrete ~handler:quiet theta_num e_len c_len;
  Qpe.verify_concrete ~handler:quiet ~gate:true theta_num e_len c_len

let test_qpe_concrete_approx_10e_5c () =
  let e_len = 10 in
  let c_len = 5 in
  let theta_num = Z.(~$983) in
  Qpe.verify_concrete ~handler:quiet theta_num e_len c_len;
  Qpe.verify_concrete ~handler:quiet ~gate:true theta_num e_len c_len

let test_qpe_concrete_approx_120e_90c () =
  Mlmpfr.set_default_prec 128;
  let e_len = 120 in
  let c_len = 90 in
  let theta_num = Z.of_string "1245176803961806747632845746442796629" in
  Qpe.verify_concrete ~handler:quiet theta_num e_len c_len;
  Qpe.verify_concrete ~handler:quiet ~gate:true theta_num e_len c_len;
  Mlmpfr.set_default_prec 64

let test_qpe_concrete_approx_1200e_900c () =
  Mlmpfr.set_default_prec 1024;
  let e_len = 1200 in
  let c_len = 900 in
  let theta_num =
    Z.of_string
      "13973184352426075455695557234933804639244448444313449912388001450482434636212528118447069671885645565743999989359759890041393950309299245532217637795499785694571731623239685011807592835605183113619882972792245554712875272922700068289503564674633207585420235049305384808298180204446965048629899838824900563063432454524441792835891351791830547706247901376306551053"
  in
  Qpe.verify_concrete ~handler:quiet theta_num e_len c_len;
  Mlmpfr.set_default_prec 64

let test_qpe_concrete_approx_rand100 () =
  for _i = 0 to 99 do
    let e_len = Random.int_in_range ~min:3 ~max:30 in
    let theta_num = Test_helper.rand_zi e_len in
    let c_len = Random.int_in_range ~min:2 ~max:(e_len - 1) in
    Qpe.verify_concrete ~handler:quiet theta_num e_len c_len;
    Qpe.verify_concrete ~handler:quiet ~gate:true theta_num e_len c_len
  done

(* QPE-Symbolic *)
let test_qpe_symbolic_exact_7 () =
  Qpe.verify_symbolic_exact ~handler:verbose 7;
  Qpe.verify_symbolic_exact ~handler:quiet ~gate:true 7

let test_qpe_symbolic_exact_70 () =
  Qpe.verify_symbolic_exact ~handler:quiet 70;
  Qpe.verify_symbolic_exact ~handler:quiet ~gate:true 70

let test_qpe_symbolic_exact_700 () =
  Qpe.verify_symbolic_exact ~handler:quiet 700

let test_qpe_symbolic_approx () =
  let e_len = 5 in
  let c_len = 3 in
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = Qpe.qpe_gate Qpe.u' e c mc in
  print_endline ("Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("E", 0) e_len 0) in
  print_endline ("Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  print_endline "Prog evaluation:";
  let hps =
    Evaluator.(evaluate_prog prog input_hps ~rewrite_settings:all_auto)
  in
  print_endline ("HPS:\n" ^ Hps.to_string hps ^ "\n")

(* Tests *)
let () =
  let open Alcotest in
  run "QPE"
    [
      ( "QPE-Simple",
        [
          test_case "QPE simple exact 3" `Quick test_qpe_simple_exact_3;
          test_case "QPE simple exact 5" `Quick test_qpe_simple_exact_5;
          test_case "QPE simple approx 3" `Quick test_qpe_simple_approx_3;
          test_case "QPE simple approx 5" `Quick test_qpe_simple_approx_5;
          test_case "QPE simple rand100" `Quick test_qpe_simple_rand100;
        ] );
      ( "QPE-Concrete",
        [
          test_case "QPE concrete exact rand100" `Quick
            test_qpe_concrete_exact_rand100;
          test_case "QPE concrete exact len E > len c" `Quick
            test_qpe_concrete_exact_e_gt_c;
          test_case "QPE concrete exact smallest RSA number not factorized"
            `Slow test_qpe_concrete_exact_smallest_rsa_number_not_factorized;
          test_case "QPE concrete approx 6e 3c" `Quick
            test_qpe_concrete_approx_6e_3c;
          test_case "QPE concrete approx 10e 5c" `Quick
            test_qpe_concrete_approx_10e_5c;
          test_case "QPE concrete approx 120e 90c" `Quick
            test_qpe_concrete_approx_120e_90c;
          test_case "QPE concrete approx 1200e 900c" `Slow
            test_qpe_concrete_approx_1200e_900c;
          test_case "QPE concrete approx rand100" `Quick
            test_qpe_concrete_approx_rand100;
        ] );
      ( "QPE-Symbolic",
        [
          test_case "QPE symbolic exact 7" `Quick test_qpe_symbolic_exact_7;
          test_case "QPE symbolic exact 70" `Quick test_qpe_symbolic_exact_70;
          test_case "QPE symbolic exact 700" `Slow test_qpe_symbolic_exact_700;
          test_case "QPE symbolic approx" `Quick test_qpe_symbolic_approx;
        ] );
    ]
