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

let ghz q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  (q_idx q ~$0 |> h)
  -- p_for "i" ~$1 (qreg_len q) (qbit_val q ~$0 => (q_idxv q "i" |> x))

let verify ?(handler = Verif_handler.default) len =
  let q = Prog.(qreg "q" ~$len) in
  let prog = ghz q in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("q", 0) len 0) in
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(evaluate_prog prog input_hps ~print:handler.print_eval_steps)
  in
  handler.log (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let sqrt_half = Hps.(Scalar.Sqrt (Scalar.SFrac (Z.(~$1), Z.(~$2)))) in
  let y0 = Hps.(Hket.of_var (Y 0)) in
  let spec_hps =
    Hps.(
      let rec add_qmem_loop i hps =
        if i >= len then hps
        else if i = 0 then add_qmem_loop (i + 1) (add_qmem ("q", i) y0 hps)
        else
          add_qmem_loop (i + 1)
            (add_qmem ("q", i) (Hket.xor y0 Hket.(of_var (X i))) hps)
      in
      one
      |> add_phase [ Y 0; X 0 ] (Dyadic1.make Z.one 1)
      |> set_scalar sqrt_half |> add_support [ 0 ] |> add_qmem_loop 0)
  in
  handler.log (fun () -> "Spec HPS:\n" ^ Hps.to_string spec_hps ^ "\n");
  if not @@ Assertion.hps_eq spec_hps hps then
    handler.fail
      ("Assertion.hps_eq spec_hps hps:\nspec_hps:\n" ^ Hps.to_string spec_hps
     ^ "\nhps:\n" ^ Hps.to_string hps);
  let vec_map = Concretization.Vector_map.of_hps hps in
  handler.log (fun () ->
      "Vector map:\n" ^ Concretization.Vector_map.to_string vec_map ^ "\n");
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q", 0) Hket.zero |> add_vec_x ("q", 1) len 0))
           Hps.(Phase.zero, sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(
             let rec add_qmem_loop i qmem =
               if i >= len then qmem
               else if i = 0 then
                 add_qmem_loop (i + 1) (Mem.add ("q", i) Hket.one qmem)
               else
                 add_qmem_loop (i + 1)
                   (Mem.add ("q", i)
                      (Hket.xor Hket.one Hket.(of_var (X i)))
                      qmem)
             in
             add_qmem_loop 0 Mem.empty)
           Hps.(Phase.(zero |> addl [ X 0 ] (Dyadic1.make Z.one 1)), sqrt_half))
  in
  handler.log (fun () ->
      "Spec vector map:\n" ^ Concretization.Vector_map.to_string spec_vec_map);
  if not @@ Concretization.Vector_map.equal spec_vec_map vec_map then
    handler.fail
      ("Concretization.Vector_map.equal spec_vec_map vec_map:\nspec_vec_map:\n"
      ^ Concretization.Vector_map.to_string spec_vec_map
      ^ "\nvec_map:\n"
      ^ Concretization.Vector_map.to_string vec_map)
