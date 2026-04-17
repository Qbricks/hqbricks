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

let qft len =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = ~$len in
  let q = qreg "q" len in
  let iv = ivar "i" in
  let i_inv = len - ~$1 - iv in
  let jv = ivar "j" in
  p_for "i" ~$0 len
    ((q_idx q i_inv |> h)
    -- p_for "j" ~$1 (len - iv)
         (qbit_val q (i_inv - jv) => (q_idx q i_inv |> rz (jv + ~$1))))

let qft' ?(do_swaps = false) q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  let iv = ivar "i" in
  let i_inv = len - ~$1 - iv in
  let jv = ivar "j" in
  let qft =
    p_for "i" ~$0 len
      ((q_idx q i_inv |> h)
      -- p_for "j" ~$1 (len - iv)
           (qbit_val q (i_inv - jv) => (q_idx q i_inv |> rz (jv + ~$1))))
  in
  if not do_swaps then qft
  else
    let _, _, qreg_len = Evaluator.Base.evaluate_qreg q in
    let half_len_floor = qreg_len / 2 in
    qft
    -- p_for "i" ~$0 ~$half_len_floor
         (Gate (swap (q_idx q iv) (q_idx q (len - ~$1 - iv))))

let qft5 =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$5 in
  (q_idx q ~$4 |> h)
  -- ((qbit_val q ~$3 => (q_idx q ~$4 |> rz ~$2))
     -- (qbit_val q ~$2 => (q_idx q ~$4 |> rz ~$3))
     -- (qbit_val q ~$1 => (q_idx q ~$4 |> rz ~$4))
     -- (qbit_val q ~$0 => (q_idx q ~$4 |> rz ~$5)))
  -- ((q_idx q ~$3 |> h)
     -- ((qbit_val q ~$2 => (q_idx q ~$3 |> rz ~$2))
        -- (qbit_val q ~$1 => (q_idx q ~$3 |> rz ~$3))
        -- (qbit_val q ~$0 => (q_idx q ~$3 |> rz ~$4))))
  -- ((q_idx q ~$2 |> h)
     -- ((qbit_val q ~$1 => (q_idx q ~$2 |> rz ~$2))
        -- (qbit_val q ~$0 => (q_idx q ~$2 |> rz ~$3))))
  -- ((q_idx q ~$1 |> h) -- (qbit_val q ~$0 => (q_idx q ~$1 |> rz ~$2)))
  -- ((q_idx q ~$0 |> h) -- skip)

let qft5' =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$5 in
  (q_idx q ~$4 |> h)
  -- (qbit_val q ~$3 => (q_idx q ~$4 |> rz ~$2))
  -- (qbit_val q ~$2 => (q_idx q ~$4 |> rz ~$3))
  -- (qbit_val q ~$1 => (q_idx q ~$4 |> rz ~$4))
  -- (qbit_val q ~$0 => (q_idx q ~$4 |> rz ~$5))
  -- (q_idx q ~$3 |> h)
  -- (qbit_val q ~$2 => (q_idx q ~$3 |> rz ~$2))
  -- (qbit_val q ~$1 => (q_idx q ~$3 |> rz ~$3))
  -- (qbit_val q ~$0 => (q_idx q ~$3 |> rz ~$4))
  -- (q_idx q ~$2 |> h)
  -- (qbit_val q ~$1 => (q_idx q ~$2 |> rz ~$2))
  -- (qbit_val q ~$0 => (q_idx q ~$2 |> rz ~$3))
  -- (q_idx q ~$1 |> h)
  -- (qbit_val q ~$0 => (q_idx q ~$1 |> rz ~$2))
  -- (q_idx q ~$0 |> h)

let qft_dag ?(do_swaps = false) q =
  Option.get @@ Prog.inverse_unitary @@ qft' q ~do_swaps

let qft_dag' q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  let iv = ivar "i" in
  let i_inv = len - ~$1 - iv in
  let jv = ivar "j" in
  p_for "i" ~$0 len
    (p_for "j" ~$1 (len - i_inv)
       (qbit_val q (iv - jv) => (q_idx q iv |> rz (~$(-1) * (jv + ~$1))))
    -- (q_idx q iv |> h))

(* Modified but equivalent version of QFT:
   - The order of RZ is reversed.
   - For each wire, two X, two Z, and two Hadamard gates have been inserted at
     various locations in the circuit. *)
let mqft q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  let iv = ivar "i" in
  let i_inv = len - ~$1 - iv in
  let jv = ivar "j" in
  let js = ~$1 in
  let je = len - iv in
  let j_inv = je - ~$1 - jv + js in
  (q |> x) -- (q |> x)
  -- p_for "i" ~$0 len
       ((q_idx q i_inv |> h)
       -- (q_idx q i_inv |> z)
       -- (q_idx q i_inv |> z)
       -- p_for "j" js je
            (qbit_val q (i_inv - j_inv) => (q_idx q i_inv |> rz (j_inv + ~$1)))
       )
  -- (q |> h) -- (q |> h)

let verify_qft_qftdag ?(handler = Verif_handler.default) len =
  let q = Prog.(qreg "q" ~$len) in
  let prog = Prog.(qft' q -- qft_dag' q) in
  handler.log (fun () -> "Prog:\n" ^ Prog.to_string prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("q", 0) len 0) in
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  handler.log_verbose (fun () -> "Prog evaluation:");
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto
        ~print:handler.print_eval_steps)
  in
  handler.log (fun () -> "HPS:\n" ^ Hps.to_string hps ^ "\n");
  let spec_hps = input_hps in
  handler.log (fun () -> "Spec HPS:\n" ^ Hps.to_string spec_hps ^ "\n");
  if not @@ Assertion.hps_eq spec_hps hps then
    handler.fail
      ("Assertion.hps_eq spec_hps hps:\nspec_hps:\n" ^ Hps.to_string spec_hps
     ^ "\nhps:\n" ^ Hps.to_string hps)

let verify_qft_mqft_eq ?(handler = Verif_handler.default) len =
  let q = Prog.(qreg "q" ~$len) in
  let qft_prog = qft' q in
  let mqft_prog = mqft q in
  handler.log (fun () -> "QFT Prog:\n" ^ Prog.to_string qft_prog ^ "\n");
  handler.log (fun () -> "MQFT Prog:\n" ^ Prog.to_string mqft_prog ^ "\n");
  let input_hps = Hps.(one |> add_qmem_vec_x ("q", 0) len 0) in
  handler.log (fun () -> "Input HPS:\n" ^ Hps.to_string input_hps ^ "\n");
  if not @@ Assertion.unitary_eq qft_prog mqft_prog input_hps then
    handler.fail
      ("Assertion.unitary_eq qft_prog mqft_prog input_hps:\nqft_prog:\n"
     ^ Prog.to_string qft_prog ^ "\nmqft_prog:\n" ^ Prog.to_string mqft_prog
     ^ "\ninput_hps:\n" ^ Hps.to_string input_hps)
