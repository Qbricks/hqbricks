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

open Hps

let check_yi0_yi1_y yi0 yi1 hps =
  not
  @@ Phase.exists
       (fun vs _ ->
         Var_set.cardinal vs <> 2
         && Var_set.mem (Y yi0) vs && Var_set.mem (Y yi1) vs)
       hps.phase

let check yi0 yi1 hps =
  if not @@ Support.contains_y yi0 hps.support then
    Error (Printf.sprintf "y%d is not in support" yi0)
  else if not @@ Support.contains_y yi1 hps.support then
    Error (Printf.sprintf "y%d is not in support" yi1)
  else if
    Phase.find_opt Var_set.(empty |> add (Y yi0) |> add (Y yi1)) hps.phase
    <> Some (Dyadic1.make Z.one 1)
  then Error (Printf.sprintf "no 1 / 2 * y%dy%d term in phase" yi0 yi1)
  else if Scalar.contains_y yi0 hps.scalar then
    Error (Printf.sprintf "y%d is in scalar" yi0)
  else if Output.contains_y yi0 hps.output then
    Error (Printf.sprintf "y%d is in output" yi0)
  else if Mem_stack.contains_y yi1 hps.output.cmem_stack then
    Error (Printf.sprintf "y%d is in cmem_stack" yi1)
  else if Phase.contains_y_not_one_half yi0 hps.phase then
    Error (Printf.sprintf "a term containing y%d is not factor 1 / 2" yi0)
  else if not @@ check_yi0_yi1_y yi0 yi1 hps then
    Error
      (Printf.sprintf
         "there is a term containing y%d and y%d which is not the y%dy%d term"
         yi0 yi1 yi0 yi1)
  else Ok ()

let apply_unchecked ?metrics yi0 yi1 hps =
  let phase_yi0 = Phase.filter (fun vs _ -> Var_set.mem (Y yi0) vs) hps.phase in
  (* Remove all the elements containing yi0 from the phase *)
  let phase =
    Phase.fold (fun vs _ acc -> Phase.remove vs acc) phase_yi0 hps.phase
  in
  (* Compute the new value of yi1 for the change var *)
  let new_y1 =
    Phase.fold
      (fun vs _ acc -> Hket.add (Var_set.remove (Y yi0) vs) acc)
      (Phase.remove Var_set.(empty |> add (Y yi0) |> add (Y yi1)) phase_yi0)
      Hket.zero
  in
  (* Update scalar *)
  let scalar = Scalar.(simp @@ SMul (SFrac (Z.(~$2), Z.one), hps.scalar)) in
  (* Replace yi1 by the new value *)
  let phase = Phase.change_var yi1 new_y1 phase in
  let scalar = Scalar.change_var yi1 new_y1 scalar in
  let qmem = Mem.change_var yi1 new_y1 hps.output.qmem in
  let output = { hps.output with qmem } in
  (* Remove yi0 and yi1 from support *)
  let support = hps.support |> Support.remove yi0 |> Support.remove yi1 in
  Metrics.add_rewrites_opt metrics 1;
  { hps with phase; scalar; output; support }

let check_and_apply ?metrics yi0 yi1 hps =
  match check yi0 yi1 hps with
  | Error s -> Error s
  | Ok () -> Ok (apply_unchecked yi0 yi1 hps ?metrics)

let find_valid_y hps =
  let phase_not_one_half_y_set = Phase.find_all_y_not_one_half hps.phase in
  let scalar_y_set = Scalar.find_all_y hps.scalar in
  let qmem_y_set = Mem.find_all_y hps.output.qmem in
  let cmem_stack_y_set = Mem_stack.find_all_y hps.output.cmem_stack in
  let valid_yi0 =
    phase_not_one_half_y_set |> Y_set.union scalar_y_set
    |> Y_set.union qmem_y_set
    |> Y_set.union cmem_stack_y_set
    |> Y_set.diff hps.support
  in
  let valid_yi1 = Y_set.diff hps.support cmem_stack_y_set in
  (valid_yi0, valid_yi1)

let find_y_pair vs dy valid_yi0 valid_yi1 hps =
  if Var_set.cardinal vs <> 2 || Dyadic1.reduce dy <> Dyadic1.make Z.one 1 then
    None
  else
    let v0 = Var_set.choose vs in
    let v1 = Var_set.choose (Var_set.remove v0 vs) in
    match (v0, v1) with
    | Y yi0, Y yi1 ->
        if
          Y_set.mem yi0 valid_yi0 && Y_set.mem yi1 valid_yi1
          && check_yi0_yi1_y yi0 yi1 hps
        then Some (yi0, yi1)
        else if
          Y_set.mem yi1 valid_yi0 && Y_set.mem yi0 valid_yi1
          && check_yi0_yi1_y yi1 yi0 hps
        then Some (yi1, yi0)
        else None
    | _ -> None

let find hps =
  let valid_yi0, valid_yi1 = find_valid_y hps in
  let fold_fun vs dy = function
    | Some y_pair -> Some y_pair
    | None -> find_y_pair vs dy valid_yi0 valid_yi1 hps
  in
  Phase.fold fold_fun hps.phase None

let find_all hps =
  let valid_yi0, valid_yi1 = find_valid_y hps in
  let fold_fun vs dy acc =
    match find_y_pair vs dy valid_yi0 valid_yi1 hps with
    | Some y_pair -> y_pair :: acc
    | None -> acc
  in
  List.rev @@ Phase.fold fold_fun hps.phase []

let find_and_apply ?metrics hps =
  match find hps with
  | None -> (None, hps)
  | Some (yi0, yi1) -> (Some (yi0, yi1), apply_unchecked yi0 yi1 hps ?metrics)

let rec find_and_apply_all_aux ?metrics hh_y_pairs hps =
  match find hps with
  | None -> (hh_y_pairs, hps)
  | Some (yi0, yi1) ->
      find_and_apply_all_aux ((yi0, yi1) :: hh_y_pairs)
        (apply_unchecked yi0 yi1 hps ?metrics)
        ?metrics

let find_and_apply_all ?metrics hps =
  let hh_y_pairs, hps = find_and_apply_all_aux [] hps ?metrics in
  (List.rev hh_y_pairs, hps)
