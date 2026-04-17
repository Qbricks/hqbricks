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

let qec3 p =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let psi = qreg "psi" ~$1 in
  let q = qreg "q" ~$2 in
  let q0 = q_idx q ~$0 in
  let q1 = q_idx q ~$1 in
  let c = qreg "c" ~$2 in
  let mc = creg "mc" ~$2 in
  let b_mc0 = cbit_val mc ~$0 in
  let b_mc1 = cbit_val mc ~$1 in
  init_qreg q
  -- (qbit_val psi ~$0 => (q0 |> x))
  -- (qbit_val q ~$0 => (q1 |> x))
  -- (psi |> ie p)
  -- (q |> ie p)
  -- init_qreg c -- (c |> h)
  -- (qbit_val c ~$0 => (psi |> z) -- (q0 |> z))
  -- (qbit_val c ~$1 => (q0 |> z) -- (q1 |> z))
  -- (c |> h) -- (c -@ mc)
  -- ((b_mc0 & b_mc1) => (q0 |> x))
  -- ((b_mc0 & !b_mc1) => (psi |> x))
  -- ((!b_mc0 & b_mc1) => (q1 |> x))

let qec3' p =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let psi = qreg "psi" ~$1 in
  let q = qreg "q" ~$2 in
  let q0 = q_idx q ~$0 in
  let q1 = q_idx q ~$1 in
  let c = qreg "c" ~$2 in
  let mc = creg "mc" ~$2 in
  let b_mc0 = cbit_val mc ~$0 in
  let b_mc1 = cbit_val mc ~$1 in
  init_qreg q
  -- (qbit_val psi ~$0 => (q0 |> x))
  -- (qbit_val q ~$0 => (q1 |> x))
  -- (psi |> ie p)
  -- (q |> ie p)
  -- init_qreg c -- (c |> h)
  -- ((qbit_val c ~$0 => (psi |> z)) -- (qbit_val c ~$0 => (q0 |> z)))
  -- ((qbit_val c ~$1 => (q0 |> z)) -- (qbit_val c ~$1 => (q1 |> z)))
  -- (c |> h) -- (c -@ mc)
  -- ((b_mc0 & b_mc1) => (q0 |> x))
  -- ((b_mc0 & !b_mc1) => (psi |> x))
  -- ((!b_mc0 & b_mc1) => (q1 |> x))

let qec3'' p =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let psi = qreg "psi" ~$1 in
  let q = qreg "q" ~$2 in
  let q0 = q_idx q ~$0 in
  let q1 = q_idx q ~$1 in
  let c = qreg "c" ~$2 in
  let mc = creg "mc" ~$2 in
  let b_mc0 = cbit_val mc ~$0 in
  let b_mc1 = cbit_val mc ~$1 in
  init_qreg q
  -- (qbit_val psi ~$0 => (q0 |> x))
  -- (qbit_val q ~$0 => (q1 |> x))
  -- (psi |> ie p)
  -- (q |> ie p)
  -- init_qreg c -- (c |> h)
  -- (qbit_val c ~$0 => (psi |> z))
  -- (qbit_val c ~$0 => (q0 |> z))
  -- (qbit_val c ~$1 => (q0 |> z))
  -- (qbit_val c ~$1 => (q1 |> z))
  -- (c |> h) -- (c -@ mc)
  -- ((b_mc0 & b_mc1) => (q0 |> x))
  -- ((b_mc0 & !b_mc1) => (psi |> x))
  -- ((!b_mc0 & b_mc1) => (q1 |> x))

let qec3''' p psi q0 q1 =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let len = qreg_len psi in
  let c0 = qreg "c0" len in
  let c1 = qreg "c1" len in
  let mc0 = creg "mc0" len in
  let mc1 = creg "mc1" len in
  init_qreg q0 -- init_qreg q1
  -- ((psi, "i") >> (q_idxv q0 "i" |> x))
  -- ((q0, "i") >> (q_idxv q1 "i" |> x))
  -- (psi |> ie p)
  -- (q0 |> ie p)
  -- (q1 |> ie p)
  -- init_qreg c0 -- init_qreg c1 -- (c0 |> h) -- (c1 |> h)
  -- ((c0, "i") >> (q_idxv psi "i" |> z) -- (q_idxv q0 "i" |> z))
  -- ((c1, "i") >> (q_idxv q0 "i" |> z) -- (q_idxv q1 "i" |> z))
  -- (c0 |> h) -- (c1 |> h) -- (c0 -@ mc0) -- (c1 -@ mc1)
  -- p_for "i" ~$0 len
       ((cbit_valv mc0 "i" & cbit_valv mc1 "i") => (q_idxv q0 "i" |> x))
  -- p_for "i" ~$0 len
       ((cbit_valv mc0 "i" & !(cbit_valv mc1 "i")) => (q_idxv psi "i" |> x))
  -- p_for "i" ~$0 len
       ((!(cbit_valv mc0 "i") & cbit_valv mc1 "i") => (q_idxv q1 "i" |> x))

let hps_product_to_string hps_product =
  Array.mapi
    (fun i hps -> Printf.sprintf "hps %d:\n%s\n" i (Hps.to_string hps))
    hps_product
  |> Array.to_list |> String.concat "\n"

let verify ?(handler = Verif_handler.default) error_p spec_success_p len =
  let psi = Prog.(qreg "psi" ~$len) in
  let q0 = Prog.(qreg "q0" ~$len) in
  let q1 = Prog.(qreg "q1" ~$len) in
  let prog = qec3''' error_p psi q0 q1 in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) len 0) in
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto
        ~print:handler.print_eval_steps)
  in
  handler.log_verbose (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let spec_qmem =
    Hps.Mem.(
      empty
      |> add_vec_x ("psi", 0) len 0
      |> add_vec_x ("q0", 0) len 0
      |> add_vec_x ("q1", 0) len 0)
  in
  handler.log (fun () ->
      "Spec qmem:\n" ^ Hps.Mem.qmem_to_string spec_qmem ^ "\n");
  handler.log_verbose (fun () ->
      let success_p = Concretization.hps_proba_qmem spec_qmem hps in
      "Success probability:\n" ^ Hps.Scalar.to_string success_p ^ "\n");
  handler.log (fun () ->
      "Expected success probability:\n"
      ^ Hps.Scalar.to_string spec_success_p
      ^ "\n");
  handler.log_verbose (fun () ->
      let hps_product = Rewrite.Fact_distr.Xy_vars.find_and_apply_all hps in
      let success_p = Concretization.hps_proba_qmem_fact spec_qmem hps in
      "HPS product:\n"
      ^ hps_product_to_string hps_product
      ^ "\nSuccess probability:\n"
      ^ Hps.Scalar.to_string success_p);
  if not @@ Assertion.hps_satisfies_proba_qmem spec_success_p spec_qmem hps then
    handler.fail
      ("Assertion.hps_satisfies_proba_qmem spec_success_p spec_qmem hps:\n\
        spec_success_p:\n"
      ^ Hps.Scalar.to_string spec_success_p
      ^ "\nspec_qmem:\n"
      ^ Hps.Mem.qmem_to_string spec_qmem
      ^ "\nhps:\n" ^ Hps.to_string hps)
