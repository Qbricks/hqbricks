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

let brus ?(handler = Verif_handler.default) u input_hps success_cmem delta k =
  let hps = ref input_hps in
  let success_p = ref Hps.Scalar.zero in
  let spec_fail_p = ref Hps.Scalar.one in
  for _ = 0 to k - 1 do
    let h = Evaluator.evaluate_prog u ~print:handler.print_eval_steps !hps in
    let step_success_p = Concretization.hps_proba_cmem_split success_cmem h in
    (success_p := Hps.Scalar.(simp (SAdd (!success_p, step_success_p))));
    let fail_p = Hps.Scalar.complement !success_p in
    handler.log_verbose (fun () ->
        "success_p: " ^ Hps.Scalar.to_string !success_p);
    handler.log_verbose (fun () ->
        "fail_p: " ^ Hps.Scalar.to_string fail_p ^ "\n");
    (spec_fail_p := Hps.Scalar.(simp (SMul (complement delta, !spec_fail_p))));
    let spec_success_p = Hps.Scalar.complement !spec_fail_p in
    (match Hps.Scalar.geq !success_p spec_success_p with
    | Some true -> ()
    | _ ->
        handler.fail
          (Printf.sprintf
             "geq success_p spec_success_p failed:\n\
              success_p: %s\n\
              spec_success_p: %s\n"
             (Hps.Scalar.to_string !success_p)
             (Hps.Scalar.to_string spec_success_p)));
    (match Hps.Scalar.leq fail_p !spec_fail_p with
    | Some true -> ()
    | _ ->
        handler.fail
          (Printf.sprintf
             "leq fail_p spec_fail_p failed:\nfail_p: %s\nspec_fail_p: %s\n"
             (Hps.Scalar.to_string fail_p)
             (Hps.Scalar.to_string !spec_fail_p)));
    let fail_scalar = Hps.Scalar.(simp (Sqrt fail_p)) in
    hps := Hps.{ input_hps with scalar = fail_scalar }
  done;
  !success_p

(* Parameters for BRUS-H *)
let h_circ =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$1 in
  let mq = creg "mq" ~$1 in
  init_qreg q -- (q_idx q ~$0 |> h) -- (q -@ mq)

let brus_h ?(handler = Verif_handler.default) =
  brus ~handler h_circ Hps.one
    Hps.Mem.(add ("mq", 0) Hps.Hket.one empty)
    (Hps.Scalar.SFrac (Z.(~$1), Z.(~$2)))

(* Parameters for BRUS-H-PARALLEL *)
let hp_circ len =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$len in
  let mq = creg "mq" ~$len in
  init_qreg q -- (q |> h) -- (q -@ mq)

let brus_hp ?(handler = Verif_handler.default) success_cmem len =
  brus ~handler (hp_circ len) Hps.one success_cmem
    (Hps.Scalar.SFrac (Z.(~$1), Z.(~$1 lsl len)))

(* Parameters for BRUS-QPE *)
let brus_qpe ?(handler = Verif_handler.default) u input_qmem success_cmem =
  let e_len = Hps.Mem.cardinal input_qmem in
  let c_len = Hps.Mem.cardinal success_cmem in
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "q" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  brus ~handler (Qpe.qpe u e c mc)
    Hps.(one |> set_qmem input_qmem)
    success_cmem
    (Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2)))

(* BRUS-H *)
let verify_brus_h ?(handler = Verif_handler.default) spec_success_p k =
  let success_p_scalar = brus_h k ~handler in
  handler.log (fun () -> "k: " ^ string_of_int k);
  handler.log (fun () -> "spec_success_p: " ^ Q.to_string spec_success_p);
  handler.log (fun () ->
      "success_p_scalar: " ^ Hps.Scalar.to_string success_p_scalar);
  let success_p =
    match Hps.Scalar.to_q success_p_scalar with
    | Some sp -> sp
    | None ->
        handler.fail "Hps.Scalar.to_q returned None";
        exit 1
  in
  handler.log_verbose (fun () -> "success_p: " ^ Q.to_string success_p ^ "\n");
  if not @@ Q.equal spec_success_p success_p then
    handler.fail
      ("Q.equal spec_success_p success_p:\nspec_success_p "
     ^ Q.to_string spec_success_p ^ "\nsuccess_p: " ^ Q.to_string success_p)

let compute_brus_hp_spec_success_p n k =
  let p_fail_1 = Q.(~$1 - Z.(~$1 /// (~$1 lsl n))) in
  let fail_p = Q.(Z.((p_fail_1.num ** k) /// (p_fail_1.den ** k))) in
  Q.(one - fail_p)

(* BRUS-H-Parallel *)
let verify_brus_hp ?(handler = Verif_handler.default) q_len spec_success_p k
    mq_n =
  handler.log (fun () -> "mq reg int value: " ^ Z.to_string mq_n ^ "\n");
  let success_cmem = Hps.Mem.(add_int ("mq", 0) q_len mq_n empty) in
  let success_p_scalar = brus_hp success_cmem q_len k ~handler in
  handler.log (fun () -> "k: " ^ string_of_int k);
  handler.log (fun () -> "spec_success_p: " ^ Q.to_string spec_success_p);
  handler.log (fun () ->
      "success_p_scalar: " ^ Hps.Scalar.to_string success_p_scalar);
  let success_p =
    match Hps.Scalar.to_q success_p_scalar with
    | Some sp -> sp
    | None ->
        handler.fail "Hps.Scalar.to_q returned None";
        exit 1
  in
  handler.log (fun () -> "success_p: " ^ Q.to_string success_p ^ "\n");
  if not @@ Q.equal spec_success_p success_p then
    handler.fail
      ("Q.equal spec_success_p success_p:\nspec_success_p "
     ^ Q.to_string spec_success_p ^ "\nsuccess_p: " ^ Q.to_string success_p)

(* BRUS-QPE *)
let verify_brus_qpe ?(handler = Verif_handler.default) theta_num e_len c_len k =
  let input_qmem = Hps.Mem.(add_int ("E", 0) e_len theta_num empty) in
  handler.log (fun () -> "e_len:\n" ^ string_of_int e_len ^ "\n");
  handler.log (fun () ->
      "input_qmem:\n" ^ Hps.Mem.qmem_to_string input_qmem ^ "\n");
  let theta'_num = Qpe.compute_theta' theta_num e_len c_len in
  handler.log (fun () -> "theta num: " ^ Z.to_string theta_num ^ "\n");
  handler.log (fun () -> "theta' num: " ^ Z.to_string theta'_num ^ "\n");
  let success_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  handler.log (fun () -> "c_len:\n" ^ string_of_int c_len ^ "\n");
  handler.log (fun () ->
      "success_cmem:\n" ^ Hps.Mem.cmem_to_string success_cmem ^ "\n");
  let success_p_scalar = brus_qpe Qpe.u_rz input_qmem success_cmem k ~handler in
  handler.log (fun () -> "k: " ^ string_of_int k);
  handler.log_verbose (fun () ->
      "success_p_scalar:\n" ^ Hps.Scalar.to_string success_p_scalar ^ "\n");
  let success_p =
    match Hps.Scalar.to_float success_p_scalar with
    | Some sp -> sp
    | None ->
        handler.fail "Hps.Scalar.to_float returned None";
        exit 1
  in
  let spec_success_inf =
    Mlmpfr.(
      int_sub 1 (pow_int (int_sub 1 (int_div 4 (pow_int Utils.mpfr_pi 2))) k))
  in
  handler.log (fun () ->
      Printf.sprintf "spec_success_inf: %s\n"
        (Mlmpfr.get_formatted_str ~size:11 spec_success_inf));
  handler.log (fun () ->
      Printf.sprintf "success_p: %s\n\n"
        (Mlmpfr.get_formatted_str ~size:11 success_p));
  if not @@ Utils.mpfr_float_approx_geq success_p spec_success_inf then
    handler.fail
      ("Utils.mpfr_float_approx_geq success_p spec_success_inf:\nsuccess_p "
      ^ Mlmpfr.get_formatted_str ~size:11 spec_success_inf
      ^ "\nspec_success_inf: "
      ^ Mlmpfr.get_formatted_str ~size:11 spec_success_inf)
