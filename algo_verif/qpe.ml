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

let qpe_simple u c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let e = qreg "E" ~$1 in
  init_qreg c -- init_qreg e -- (c |> h)
  -- ((c, "i") >> apply_gate e (u (ivar "i")))
  -- Qft.qft_dag c ~do_swaps:true
  -- (c -@ mc)

let qpe u e c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  init_qreg c -- (c |> h)
  -- ((c, "i") >> u (ivar "i") e)
  -- Qft.qft_dag c ~do_swaps:true
  -- (c -@ mc)

let qpe_gate u e c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  init_qreg c -- (c |> h)
  -- ((c, "i") >> apply_gate e (u (ivar "i")))
  -- Qft.qft_dag c ~do_swaps:true
  -- (c -@ mc)

(* This function computes the spec theta' numerator approximated by QPE
   for a given theta, and is used for QPE specification. *)
let compute_theta' theta_num theta_den_pow theta'_den_pow =
  if theta_den_pow = theta'_den_pow then theta_num
  else
    let reg_len_diff = theta_den_pow - theta'_den_pow in
    let round_bit_value =
      let shift = reg_len_diff - 1 in
      Z.((theta_num asr shift) land ~$1)
    in
    let mask = Z.(pred (~$1 lsl theta'_den_pow)) in
    Z.(((theta_num asr reg_len_diff) + round_bit_value) land mask)

(* Simple U gate for QPE, it represents a partial gate implementation
   and should only be applied on a specific vector *)
let evaluate_u theta_num theta_den_pow i _qreg ?(k = Hps.Hket.one)
    ?(var_val = Utils.Var_name_map.empty) ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let theta_num = evaluate_int theta_num ~var_val in
    let theta_den_pow = Z.to_int @@ evaluate_int theta_den_pow ~var_val in
    let i = Z.to_int @@ evaluate_int i ~var_val in
    let dy_fact =
      if i < 0 then
        Dyadic1.make Z.(theta_num * -(~$1 lsl Stdlib.( ~- ) i)) theta_den_pow
      else Dyadic1.make Z.(theta_num * (~$1 lsl i)) theta_den_pow
    in
    let phase =
      if Hket.is_one k then Phase.add Var_set.empty dy_fact hps.phase
      else Phase.addp hps.phase @@ Phase.lift_ket k dy_fact
    in
    Metrics.add_gates_opt metrics 1;
    { hps with phase }

let rec u theta_num theta_den_pow i qreg =
  let u =
    Prog.Gate.
      {
        name = "U";
        qreg_params = [ qreg ];
        params = [ Param.Int theta_num; Param.Int theta_den_pow; Param.Int i ];
        with_params = u_with_params;
        inverse = (fun _ -> failwith "Not implemented");
        evaluate = evaluate_u theta_num theta_den_pow i qreg;
      }
  in
  u

and u_with_params qreg_params params =
  let open Prog.Gate in
  match (qreg_params, params) with
  | ( qreg :: _,
      Param.Int theta_num :: Param.Int theta_den_pow :: Param.Int i :: _ ) ->
      u theta_num theta_den_pow i qreg
  | _ -> failwith "U needs 1 qreg parameter and 3 Int parameters"

(* U gate for QPE depending on the qreg value, it represents a partial
 * gate implementation and should only be applied on a specific vector *)
let evaluate_u' i qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let i = Z.to_int @@ evaluate_int i ~var_val in
    if i < 0 then failwith "i shouldn't be negative";
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    if i_start != 0 then failwith "qreg should start at 0";
    let dy_fact = Dyadic1.make Z.(~$1 lsl i) qreg_len in
    let phase_to_add =
      Mem.reg_as_int_to_phase dy_fact qreg_name qreg_len hps.output.qmem
        ~k_fact:k
    in
    let phase = Phase.addp hps.phase phase_to_add in
    Metrics.add_gates_opt metrics qreg_len;
    { hps with phase }

let rec u' i qreg =
  let u =
    Prog.Gate.
      {
        name = "U";
        qreg_params = [ qreg ];
        params = [ Param.Int i ];
        with_params = u_with_params';
        inverse = (fun _ -> failwith "Not implemented");
        evaluate = evaluate_u' i qreg;
      }
  in
  u

and u_with_params' qreg_params params =
  let open Prog.Gate in
  match (qreg_params, params) with
  | qreg :: _, Param.Int i :: _ -> u' i qreg
  | _ -> failwith "U needs 1 qreg parameter and 1 Int parameters"

(* U RZ circuit for QPE *)
let u_rz i q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  p_for "iu" ~$0 (len - i) (q_idxv q "iu" |> rz (len - i - ivar "iu"))

(* QPE-Simple *)
let verify_simple ?(handler = Verif_handler.default) ?(exact = false) theta_num
    theta_den_pow c_len =
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = qpe_simple Prog.(u (i_of_z theta_num) ~$theta_den_pow) c mc in
  handler.log_verbose (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.one in
  handler.log_verbose (fun () ->
      "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:no_rewrite
        ~print:handler.print_eval_steps)
  in
  handler.log_verbose (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let theta'_num = compute_theta' theta_num theta_den_pow c_len in
  let spec_p =
    if exact then Hps.Scalar.one
    else Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2))
  in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  handler.log (fun () -> "theta num: " ^ Z.to_string theta_num);
  handler.log (fun () -> "theta_den_pow: " ^ string_of_int theta_den_pow);
  handler.log (fun () -> "theta' num: " ^ Z.to_string theta'_num);
  handler.log (fun () -> "theta'_den_pow: " ^ string_of_int c_len);
  handler.log (fun () -> "spec_p: " ^ Hps.Scalar.to_string spec_p);
  handler.log_verbose (fun () ->
      "spec_cmem:\n" ^ Hps.Mem.cmem_to_string spec_cmem ^ "\n");
  if exact then (
    if not @@ Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps then
      handler.fail
        ("Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps:\nspec_p: "
        ^ Hps.Scalar.to_string spec_p
        ^ "\nspec_cmem:\n"
        ^ Hps.Mem.cmem_to_string spec_cmem
        ^ "\nhps:\n" ^ Hps.to_string hps))
  else if
    not
    @@ Option.value ~default:false
    @@ Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps
  then
    handler.fail
      ("Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps:\nspec_p: "
      ^ Hps.Scalar.to_string spec_p
      ^ "\nspec_cmem:\n"
      ^ Hps.Mem.cmem_to_string spec_cmem
      ^ "\nhps:\n" ^ Hps.to_string hps)

(* QPE-Concrete *)
let verify_concrete ?(handler = Verif_handler.default) ?(gate = false)
    ?(exact = false) theta_num e_len c_len =
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = if gate then qpe_gate u' e c mc else qpe u_rz e c mc in
  handler.log_verbose (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_int ("E", 0) e_len theta_num) in
  handler.log_verbose (fun () ->
      "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:no_rewrite
        ~print:handler.print_eval_steps)
  in
  handler.log_verbose (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let theta'_num = compute_theta' theta_num e_len c_len in
  let spec_p =
    if exact then Hps.Scalar.one
    else Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2))
  in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  handler.log (fun () -> "theta num: " ^ Z.to_string theta_num);
  handler.log (fun () -> "theta_den_pow: " ^ string_of_int e_len);
  handler.log (fun () -> "theta' num: " ^ Z.to_string theta'_num);
  handler.log (fun () -> "theta'_den_pow: " ^ string_of_int c_len);
  handler.log (fun () -> "spec_p: " ^ Hps.Scalar.to_string spec_p);
  handler.log_verbose (fun () ->
      "spec_cmem:\n" ^ Hps.Mem.cmem_to_string spec_cmem ^ "\n");
  if exact then (
    if not @@ Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps then
      handler.fail
        ("Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps:\nspec_p: "
        ^ Hps.Scalar.to_string spec_p
        ^ "\nspec_cmem:\n"
        ^ Hps.Mem.cmem_to_string spec_cmem
        ^ "\nhps:\n" ^ Hps.to_string hps))
  else if
    not
    @@ Option.value ~default:false
    @@ Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps
  then
    handler.fail
      ("Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps:\nspec_p: "
      ^ Hps.Scalar.to_string spec_p
      ^ "\nspec_cmem:\n"
      ^ Hps.Mem.cmem_to_string spec_cmem
      ^ "\nhps:\n" ^ Hps.to_string hps)

(* QPE-Symbolic *)
let verify_symbolic_exact ?(handler = Verif_handler.default) ?(gate = false) len
    =
  let e_len = len in
  let c_len = len in
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = if gate then qpe_gate u' e c mc else qpe u_rz e c mc in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("E", 0) e_len 0) in
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto
        ~print:handler.print_eval_steps)
  in
  handler.log (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let spec_p = Hps.Scalar.one in
  let spec_cmem = Hps.Mem.(empty |> add_vec_x ("mc", 0) e_len 0) in
  handler.log (fun () -> "spec_p: " ^ Hps.Scalar.to_string spec_p);
  handler.log_verbose (fun () ->
      "spec_cmem:\n" ^ Hps.Mem.cmem_to_string spec_cmem ^ "\n");
  if not @@ Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps then
    handler.fail
      ("Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps:\nspec_p: "
      ^ Hps.Scalar.to_string spec_p
      ^ "\nspec_cmem:\n"
      ^ Hps.Mem.cmem_to_string spec_cmem
      ^ "\nhps:\n" ^ Hps.to_string hps)
