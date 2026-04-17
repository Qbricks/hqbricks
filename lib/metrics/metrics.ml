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

type t = {
  mutable gate_count : int;
  mutable meas_count : int;
  mutable rewrite_count : int;
}

let create () = { gate_count = 0; meas_count = 0; rewrite_count = 0 }
let[@inline] add_gates m n = m.gate_count <- m.gate_count + n
let[@inline] add_measures m n = m.meas_count <- m.meas_count + n
let[@inline] add_rewrites m n = m.rewrite_count <- m.rewrite_count + n

let[@inline] add_inplace m1 m2 =
  m1.gate_count <- m1.gate_count + m2.gate_count;
  m1.meas_count <- m1.meas_count + m2.meas_count;
  m1.rewrite_count <- m1.rewrite_count + m2.rewrite_count

let[@inline] add_gates_opt m_opt n =
  match m_opt with None -> () | Some m -> add_gates m n

let[@inline] add_measures_opt m_opt n =
  match m_opt with None -> () | Some m -> add_measures m n

let[@inline] add_rewrites_opt m_opt n =
  match m_opt with None -> () | Some m -> add_rewrites m n

let[@inline] add_inplace_opt m1_opt m2_opt =
  match (m1_opt, m2_opt) with Some m1, Some m2 -> add_inplace m1 m2 | _ -> ()

let to_string m =
  Printf.sprintf "gates: %d, measures: %d, rewrites: %d" m.gate_count
    m.meas_count m.rewrite_count
