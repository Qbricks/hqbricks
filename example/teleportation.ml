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

(* Program implementation *)
let teleportation_prog =
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

(* Symbolic input HPS <0,1.|x0>psi> *)
let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) 1 0)

(* Metrics to count gates, measurements, and rewrites. *)
let metrics = Metrics.create ()

(* Evaluation of the Teleportation prog into the HPS symbolic
   representation *)
let hps =
  Evaluator.(
    evaluate_prog teleportation_prog input_hps ~rewrite_settings:all_auto
      ~print:true ~metrics)

(* Specification HPS <0,1.|x0>bob>, we want to verify that the input
   state of psi has been teleported into bob *)
let spec_hps = Hps.(one |> add_qmem_vec_x ("bob", 0) 1 0)

(* Specification verification *)
let () = assert (Assertion.hps_satisfies spec_hps hps ~metrics)
let () = print_endline ("Teleportation verified, " ^ Metrics.to_string metrics)
