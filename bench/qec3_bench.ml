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

let qec3 n error_p =
  let open Prog in
  let open Gate_set_impl.Clifford_k_err in
  let psi = qreg "psi" ~$n in
  let q0 = qreg "q0" ~$n in
  let q1 = qreg "q1" ~$n in
  let c0 = qreg "c0" ~$n in
  let c1 = qreg "c1" ~$n in
  let mc0 = creg "mc0" ~$n in
  let mc1 = creg "mc1" ~$n in
  init_qreg q0 -- init_qreg q1
  -- ((psi, "i") >> (q_idxv q0 "i" |> x))
  -- ((q0, "i") >> (q_idxv q1 "i" |> x))
  -- (psi |> ie error_p)
  -- (q0 |> ie error_p)
  -- (q1 |> ie error_p)
  -- init_qreg c0 -- init_qreg c1 -- (c0 |> h) -- (c1 |> h)
  -- ((c0, "i") >> (q_idxv psi "i" |> z) -- (q_idxv q0 "i" |> z))
  -- ((c1, "i") >> (q_idxv q0 "i" |> z) -- (q_idxv q1 "i" |> z))
  -- (c0 |> h) -- (c1 |> h) -- (c0 -@ mc0) -- (c1 -@ mc1)
  -- p_for "i" ~$0 ~$n
       ((cbit_valv mc0 "i" & cbit_valv mc1 "i") => (q_idxv q0 "i" |> x))
  -- p_for "i" ~$0 ~$n
       ((cbit_valv mc0 "i" & !(cbit_valv mc1 "i")) => (q_idxv psi "i" |> x))
  -- p_for "i" ~$0 ~$n
       ((!(cbit_valv mc0 "i") & cbit_valv mc1 "i") => (q_idxv q1 "i" |> x))

let verify_qec3 ?metrics n error_p spec_success_p =
  let prog = qec3 n error_p in
  let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) n 0) in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto ~print:false
        ?metrics)
  in
  let spec_qmem =
    Hps.Mem.(
      empty
      |> add_vec_x ("psi", 0) n 0
      |> add_vec_x ("q0", 0) n 0
      |> add_vec_x ("q1", 0) n 0)
  in
  assert (
    Assertion.hps_satisfies_proba_qmem spec_success_p spec_qmem hps ?metrics)

let p_1_4 = Hps.Scalar.SFrac (Z.(~$1), Z.(~$4))
let p_2_3 = Hps.Scalar.SFrac (Z.(~$2), Z.(~$3))
let p_1_3 = Hps.Scalar.SFrac (Z.(~$1), Z.(~$3))
let p_2_5 = Hps.Scalar.SFrac (Z.(~$2), Z.(~$5))
let spec_success_p_1 = Hps.Scalar.SFrac (Z.(~$27), Z.(~$32))
let spec_success_p_7 = Hps.Scalar.SFrac (Z.(~$7 ** 7), Z.(~$27 ** 7))
let spec_success_p_100 = Hps.Scalar.SFrac (Z.(~$20 ** 100), Z.(~$27 ** 100))
let spec_success_p_2000 = Hps.Scalar.SFrac (Z.(~$81 ** 2000), Z.(~$125 ** 2000))

let benches =
  [
    ( "Quantum Error Correction 7",
      fun ?metrics () -> verify_qec3 1 p_1_4 spec_success_p_1 ?metrics );
    ( "Quantum Error Correction 49",
      fun ?metrics () -> verify_qec3 7 p_2_3 spec_success_p_7 ?metrics );
    ( "Quantum Error Correction 700",
      fun ?metrics () -> verify_qec3 100 p_1_3 spec_success_p_100 ?metrics );
    ( "Quantum Error Correction 14000",
      fun ?metrics () -> verify_qec3 2000 p_2_5 spec_success_p_2000 ?metrics );
  ]

let quick_benches = List.take 3 benches
