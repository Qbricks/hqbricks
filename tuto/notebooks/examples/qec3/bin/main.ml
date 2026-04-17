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

let qec3_circ =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let p = Hps.Scalar.(SFrac (Z.(~$2), Z.(~$3))) in
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

let hps =
  let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) 1 0) in
  let open Evaluator in
  evaluate_prog qec3_circ input_hps ~rewrite_settings:interactive_rewrite

let x0 = Hps.Hket.of_var @@ X 0

let spec_qmem =
  Hps.Mem.(empty |> add ("psi", 0) x0 |> add ("q", 0) x0 |> add ("q", 1) x0)

let exp_p = Hps.Scalar.(SFrac (Z.(~$7), Z.(~$27)))

let () =
  if
    Assertion.hps_satisfies_proba_qmem exp_p spec_qmem hps
    && Hps.Support.cardinal hps.support == 3
  then print_endline "QEC3 with p = 2 / 3 Verified"
  else failwith "Verification failed, try again using HH rewrite rule"
