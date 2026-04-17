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

let teleportation n =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let psi = qreg "psi" ~$n in
  let alice = qreg "alice" ~$n in
  let bob = qreg "bob" ~$n in
  let m_psi = creg "m_psi" ~$n in
  let m_alice = creg "m_alice" ~$n in
  init_qreg alice -- init_qreg bob -- (alice |> h)
  -- ((alice, "i") >> (q_idxv bob "i" |> x))
  -- ((psi, "i") >> (q_idxv alice "i" |> x))
  -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
  -- C.((m_alice, "i") >> (q_idxv bob "i" |> x))
  -- C.((m_psi, "i") >> (q_idxv bob "i" |> z))

let verify_teleportation ?metrics n =
  let prog = teleportation n in
  let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) n 0) in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto ~print:false
        ?metrics)
  in
  let spec_hps = Hps.(one |> add_qmem_vec_x ("bob", 0) n 0) in
  assert (Assertion.hps_satisfies spec_hps hps ?metrics)

let benches =
  [
    ( "Quantum Teleportation 5",
      fun ?metrics () -> verify_teleportation 1 ?metrics );
    ( "Quantum Teleportation 500",
      fun ?metrics () -> verify_teleportation 100 ?metrics );
    ( "Quantum Teleportation 50000",
      fun ?metrics () -> verify_teleportation 10000 ?metrics );
  ]

let quick_benches = benches
