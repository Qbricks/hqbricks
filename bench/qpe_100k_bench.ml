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

let () = Random.self_init ()

(* Generate a random Zarith int value of on n bits *)
let rand_zi n =
  let rec loop i acc =
    if i >= n then acc else loop (i + 1) Z.((acc lsl 1) + ~$(Random.int 2))
  in
  loop 0 Z.zero

(* Robustness check for QPE calculation, it iterates 100 000 runs, with
   randomly chosen entries of 100 qubits and a correctness verification check
*)
let qpe_100k () =
  for _i = 0 to 99999 do
    let n = 100 in
    let theta_num = rand_zi n in
    Qpe_bench.verify_qpe_concrete_exact theta_num n n
  done

let () =
  Mlmpfr.set_default_prec 128;
  print_endline
    "Running 100 000 QPE exact verification checks with 100 qubits entries...\n\
     Expected duration ~44 min";
  let t_start = Unix.gettimeofday () in
  qpe_100k ();
  let t_end = Unix.gettimeofday () in
  let t = int_of_float (t_end -. t_start) in
  let min = t / 60 in
  let sec = t mod 60 in
  Printf.printf "Total time: %d min %d s\n" min sec
