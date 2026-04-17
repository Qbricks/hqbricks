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

open Bechamel
open Toolkit

let bench_order_list =
  [
    "Quantum Teleportation 5";
    "Quantum Teleportation 500";
    "Quantum Teleportation 50000";
    "QPE exact symbolic 210";
    "QPE exact symbolic 2100";
    "QPE exact concrete RSA 260";
    "QPE approx concrete 7";
    "QPE approx concrete 11";
    "QPE approx concrete 101";
    "QPE approx concrete 300";
    "QPE approx concrete 1036";
    "QPE approx concrete 3000";
    "BRUS-H-parallel k100 w20";
    "BRUS-H-parallel k100 w200";
    "BRUS-H-parallel k500 w200";
    "BRUS-H-parallel k500 w2000";
    "BRUS-QPE k5 w300";
    "BRUS-QPE k5 w3000";
    "BRUS-QPE k10 w300";
    "BRUS-QPE k10 w3000";
    "Quantum Error Correction 7";
    "Quantum Error Correction 49";
    "Quantum Error Correction 700";
    "Quantum Error Correction 14000";
    "QFT Equivalence 16";
    "QFT Equivalence 32";
    "QFT Equivalence 75";
    "QFT Equivalence 500";
  ]

let benches_no_metrics benches =
  List.map (fun (name, f) -> (name, f ?metrics:None)) benches

let bench_order_map =
  Hqbricks.Utils.String_map.of_list
  @@ List.mapi (fun i name -> (name, i)) bench_order_list

let sort_bench s1 s2 =
  let open Hqbricks.Utils.String_map in
  let i1 = Option.value ~default:max_int (find_opt s1 bench_order_map) in
  let i2 = Option.value ~default:max_int (find_opt s2 bench_order_map) in
  Int.compare i1 i2

let results_to_list_sec results =
  let result = Hashtbl.find results "monotonic-clock" in
  let sorted_names =
    Hashtbl.fold (fun name _ acc -> name :: acc) result []
    |> List.sort sort_bench
  in
  List.fold_right
    (fun name acc ->
      let time =
        Hashtbl.find result name |> Analyze.OLS.estimates |> Option.get
        |> List.hd
      in
      (name, time *. 1e-9) :: acc)
    sorted_names []

let results_sec_to_csv results_sec =
  List.fold_left
    (fun acc (name, time) -> acc ^ Printf.sprintf "%s,%.6f\n" name time)
    "case,time_sec\n" results_sec

let results_sec_to_tex results_sec =
  List.fold_left
    (fun acc (name, time) -> acc ^ Printf.sprintf "%s&%.6f\\\\\n" name time)
    "\\begin{table}[htbp]\n\
     \\caption{Experimental evaluation result for HQbricks symbolic execution}\n\
     \\scriptsize\n\
     \\begin{tabular}{|l|c|}\n\
     \\hline\n\
     Case&Comp. time (sec.)\\\\\n\
     \\hline\n\
     \\hline\n"
    results_sec
  ^ "\\hline\n\\end{tabular}\n\\label{table:xps}\n\\end{table}"

let write_to_data_file filename content =
  let dir = "data" in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o775;
  let path = Filename.concat dir filename in
  let oc = open_out path in
  output_string oc content;
  close_out oc

let print_metrics benches =
  List.sort (fun (name1, _) (name2, _) -> sort_bench name1 name2) benches
  |> List.iter (fun (name, f) ->
         let metrics = Hqbricks.Metrics.create () in
         f ?metrics:(Some metrics) ();
         print_endline @@ name ^ ": " ^ Hqbricks.Metrics.to_string metrics);
  print_endline ""

let make_tests benches =
  List.map (fun (name, f) -> Test.make ~name (Staged.stage @@ f)) benches
  |> Test.make_grouped ~name:"" ~fmt:"%s%s"

let benchmark tests =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~quota:(Time.second 5.) ~stabilize:true () in
  let raw_results = Benchmark.all cfg instances tests in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  Analyze.merge ols instances results

let () =
  Bechamel_notty.Unit.add Instance.monotonic_clock
    (Measure.unit Instance.monotonic_clock)

let notty_print_results results =
  let open Notty_unix in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  Bechamel_notty.Multiple.image_of_ols_results ~sort:sort_bench ~rect:window
    ~predictor:Measure.run results
  |> eol |> output_image

let run benches with_metrics =
  let tests = benches_no_metrics benches |> make_tests in
  let results = benchmark tests in
  let results_sec = results_to_list_sec results in
  notty_print_results results;
  write_to_data_file "hqbricks_bench.csv" (results_sec_to_csv results_sec);
  write_to_data_file "hqbricks_bench.tex" (results_sec_to_tex results_sec);
  print_endline
    "Benches results in seconds have been saved in data/hqbricks_bench.csv and \
     data/hqbricks_bench.tex.";
  if with_metrics then (
    print_endline "\nComputing metrics...";
    print_metrics benches)
