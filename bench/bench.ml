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

open Bench_helper

let all_benches =
  Teleportation_bench.benches @ Qpe_bench.benches @ Brus_bench.benches
  @ Qec3_bench.benches @ Qft_bench.benches

let all_quick_benches =
  Teleportation_bench.quick_benches @ Qpe_bench.quick_benches
  @ Brus_bench.quick_benches @ Qec3_bench.quick_benches
  @ Qft_bench.quick_benches

let usage_msg =
  "bench [-q] [-m] [ALGO]\n"
  ^ "  ALGO: algorithm to benchmark, or 'all' for all algorithms (default)\n"
  ^ "        Available algorithms: teleportation, qpe, brus, qec3, qft"

let quick = ref false
let metrics = ref false
let algo = ref "all"
let anon_fun a = algo := a

let speclist =
  [
    ("-q", Arg.Set quick, "Quick benchmarks only");
    ("-m", Arg.Set metrics, "Include metrics output");
  ]

let run_bench_msg algo quick =
  "Running " ^ algo
  ^ (if quick then " quick" else "")
  ^ " benchmarks" ^ "..."
  ^
  if algo = "all" then
    " expected duration " ^ if quick then "~3 min" else "~12 min"
  else ""

let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline @@ run_bench_msg !algo !quick;
  match (!algo, !quick) with
  | "all", false -> run all_benches !metrics
  | "all", true -> run all_quick_benches !metrics
  | "teleportation", false -> run Teleportation_bench.benches !metrics
  | "teleportation", true -> run Teleportation_bench.quick_benches !metrics
  | "qpe", false -> run Qpe_bench.benches !metrics
  | "qpe", true -> run Qpe_bench.quick_benches !metrics
  | "brus", false -> run Brus_bench.benches !metrics
  | "brus", true -> run Brus_bench.quick_benches !metrics
  | "qec3", false -> run Qec3_bench.benches !metrics
  | "qec3", true -> run Qec3_bench.quick_benches !metrics
  | "qft", false -> run Qft_bench.benches !metrics
  | "qft", true -> run Qft_bench.quick_benches !metrics
  | _ -> raise (Arg.Bad ("Invalid ALGO: " ^ !algo))
