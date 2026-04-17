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

let qft n =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$n in
  let iv = ivar "i" in
  let i_inv = ~$n - ~$1 - iv in
  let jv = ivar "j" in
  p_for "i" ~$0 ~$n
    ((q_idx q i_inv |> h)
    -- p_for "j" ~$1 (~$n - iv)
         (qbit_val q (i_inv - jv) => (q_idx q i_inv |> rz (jv + ~$1))))

(* Modified but equivalent version of QFT:
   - The order of RZ is reversed.
   - For each wire, two X, two Z, and two Hadamard gates have been inserted at
     various locations in the circuit. *)
let mqft n =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$n in
  let iv = ivar "i" in
  let i_inv = ~$n - ~$1 - iv in
  let jv = ivar "j" in
  let js = ~$1 in
  let je = ~$n - iv in
  let j_inv = je - ~$1 - jv + js in
  (q |> x) -- (q |> x)
  -- p_for "i" ~$0 ~$n
       ((q_idx q i_inv |> h)
       -- (q_idx q i_inv |> z)
       -- (q_idx q i_inv |> z)
       -- p_for "j" js je
            (qbit_val q (i_inv - j_inv) => (q_idx q i_inv |> rz (j_inv + ~$1)))
       )
  -- (q |> h) -- (q |> h)

let verify_qft_mqft_equivalence ?metrics n =
  let qft_prog = qft n in
  let mqft_prog = mqft n in
  let input_hps = Hps.(one |> add_qmem_vec_x ("q", 0) n 0) in
  assert (Assertion.unitary_eq qft_prog mqft_prog input_hps ?metrics)

let benches =
  [
    ( "QFT Equivalence 16",
      fun ?metrics () -> verify_qft_mqft_equivalence 16 ?metrics );
    ( "QFT Equivalence 32",
      fun ?metrics () -> verify_qft_mqft_equivalence 32 ?metrics );
    ( "QFT Equivalence 75",
      fun ?metrics () -> verify_qft_mqft_equivalence 75 ?metrics );
    ( "QFT Equivalence 500",
      fun ?metrics () -> verify_qft_mqft_equivalence 500 ?metrics );
  ]

let quick_benches = List.take 3 benches
