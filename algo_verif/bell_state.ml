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

let bell_state =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q0 = qreg "q0" ~$1 in
  let q1 = qreg "q1" ~$1 in
  (q0 |> h) -- (qbit_val q0 ~$0 => (q1 |> x))

let bell_state' q0 q1 =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  (q0 |> h) -- (qbit_val q0 ~$0 => (q1 |> x))

let verify ?(handler = Verif_handler.default) input_hps spec_hps spec_vec_map =
  let q0 = Prog.(qreg "q0" ~$1) in
  let q1 = Prog.(qreg "q1" ~$1) in
  let prog = bell_state' q0 q1 in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(evaluate_prog ~print:handler.print_eval_steps prog input_hps)
  in
  handler.log (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  handler.log (fun () -> "Expected HPS:\n" ^ Hps.to_string spec_hps ^ "\n");
  if not @@ Assertion.hps_eq spec_hps hps then
    handler.fail
      ("Assertion.hps_eq spec_hps hps:\nspec_hps:\n" ^ Hps.to_string spec_hps
     ^ "\nhps:\n" ^ Hps.to_string hps);
  let vec_map = Concretization.Vector_map.of_hps hps in
  handler.log (fun () ->
      "Vector map:\n" ^ Concretization.Vector_map.to_string vec_map ^ "\n");
  handler.log (fun () ->
      "Expected vector map:\n"
      ^ Concretization.Vector_map.to_string spec_vec_map);
  if not @@ Concretization.Vector_map.equal spec_vec_map vec_map then
    handler.fail
      ("Concretization.Vector_map.equal spec_vec_map vec_map:\nspec_vec_map:\n"
      ^ Concretization.Vector_map.to_string spec_vec_map
      ^ "\nvec_map:\n"
      ^ Concretization.Vector_map.to_string vec_map)
