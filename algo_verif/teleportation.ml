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

let teleportation alice bob psi =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let m_alice = creg "m_alice" (qreg_len alice) in
  let m_psi = creg "m_psi" (qreg_len psi) in
  init_qreg alice -- init_qreg bob -- (alice |> h)
  -- ((alice, "i") >> (q_idxv bob "i" |> x))
  -- ((psi, "i") >> (q_idxv alice "i" |> x))
  -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
  -- C.((m_alice, "i") >> (q_idxv bob "i" |> x))
  -- C.((m_psi, "i") >> (q_idxv bob "i" |> z))

let teleportation' len =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let psi = qreg "psi" ~$len in
  let alice = qreg "alice" ~$len in
  let bob = qreg "bob" ~$len in
  let m_psi = creg "m_psi" ~$len in
  let m_alice = creg "m_alice" ~$len in
  init_qreg alice -- init_qreg bob -- (alice |> h)
  -- ((alice, "i") >> (q_idxv bob "i" |> x))
  -- ((psi, "i") >> (q_idxv alice "i" |> x))
  -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
  -- C.((m_alice, "i") >> (q_idxv bob "i" |> x))
  -- C.((m_psi, "i") >> (q_idxv bob "i" |> z))

let teleportation1 =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let psi = qreg "psi" ~$1 in
  let alice = qreg "alice" ~$1 in
  let bob = qreg "bob" ~$1 in
  let m_psi = creg "m_psi" ~$1 in
  let m_alice = creg "m_alice" ~$1 in
  init_qreg alice -- init_qreg bob -- (alice |> h)
  -- (qbit_val alice ~$0 => (bob |> x))
  -- (qbit_val psi ~$0 => (alice |> x))
  -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
  -- (cbit_val m_alice ~$0 => (bob |> x))
  -- (cbit_val m_psi ~$0 => (bob |> z))

let verify ?(handler = Verif_handler.default) len =
  let alice = Prog.(qreg "alice" ~$len) in
  let bob = Prog.(qreg "bob" ~$len) in
  let psi = Prog.(qreg "psi" ~$len) in
  let prog = teleportation alice bob psi in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) len 0) in
  handler.log_verbose (fun () ->
      "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto
        ~print:handler.print_eval_steps)
  in
  handler.log_verbose (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let spec_hps = Hps.(one |> add_qmem_vec_x ("bob", 0) len 0) in
  handler.log_verbose (fun () -> "Spec HPS:\n" ^ Hps.to_string spec_hps ^ "\n");
  if not @@ Assertion.hps_satisfies spec_hps hps then
    handler.fail
      ("Assertion.hps_satisfies spec_hps hps:\nspec_hps:\n"
     ^ Hps.to_string spec_hps ^ "\nhps:\n" ^ Hps.to_string hps)
