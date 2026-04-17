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

(*
   Currently, this module only handles a particular case of the Change Variable rule
   It will be completed later with vectors of y and generalized check.
   The handled case is when there is only one path variable yi updated such as
   yi := yi + yxor + b, where:
   - yxor is a XOR of path variables excluding yi.
   - b is a boolean expression not depending on any path variable.
 *)

open Hps

let check yi new_val hps =
  if not @@ Support.contains_y yi hps.support then
    Error (Printf.sprintf "y%d is not in support" yi)
  else if not @@ Hket.contains_y yi new_val then
    Error (Printf.sprintf "y%d is not in the new value" yi)
  else if
    Hket.exists
      (fun vs ->
        Var_set.exists
          (fun v ->
            match v with
            | X _ -> false
            | Y yi -> not @@ Support.contains_y yi hps.support)
          vs)
      new_val
  then
    Error (Printf.sprintf "the new value contains an y that is not in support")
  else if
    Hket.exists
      (fun vs -> Var_set.cardinal vs > 1 && Var_set.contains_any_y vs)
      new_val
  then
    Error
      (Printf.sprintf
         "there is a monomial containing an y and at least one other variable \
          in the new value")
  else Ok ()

let apply_unchecked ?metrics yi new_val hps =
  Metrics.add_rewrites_opt metrics 1;
  {
    hps with
    phase = Phase.change_var yi new_val hps.phase;
    scalar = Scalar.change_var yi new_val hps.scalar;
    output = Output.change_var yi new_val hps.output;
  }

let check_and_apply ?metrics yi new_val hps =
  match check yi new_val hps with
  | Error s -> Error s
  | Ok () -> Ok (apply_unchecked yi new_val hps ?metrics)

let check_y_alone k =
  Hket.for_all
    (fun vs ->
      Var_set.cardinal vs < 2
      || Var_set.for_all (fun v -> match v with X _ -> true | Y _ -> false) vs)
    k

let check_cv_map_add yi k cv_map =
  match Utils.Int_map.find_opt yi cv_map with
  | Some (Some ket) when ket = k -> true
  | None -> true
  | _ -> false

let find_y_xor_no_y_ket k cv_map =
  let y_set = Hket.find_all_y k in
  if
    Y_set.cardinal y_set = 1
    && Hket.cardinal k <> 1
    && check_y_alone k
    && check_cv_map_add (Y_set.choose y_set) k cv_map
  then Utils.Int_map.add (Y_set.choose y_set) (Some k) cv_map
  else Y_set.fold (fun yi acc -> Utils.Int_map.add yi None acc) y_set cv_map

let find_y_xor_no_y_mem m cv_map =
  Mem.fold (fun _ k cv_map -> find_y_xor_no_y_ket k cv_map) m cv_map

let find_y_xor_no_y_mem_stack ms cv_map =
  List.fold_left (fun acc m -> find_y_xor_no_y_mem m acc) cv_map ms

let find_y_xor_no_y_output o cv_map =
  find_y_xor_no_y_mem Output.(o.qmem) cv_map
  |> find_y_xor_no_y_mem_stack Output.(o.cmem_stack)

let find_and_apply_all_y_xor_no_y ?metrics hps =
  let cv_map = find_y_xor_no_y_output hps.output Utils.Int_map.empty in
  let cv_map = Utils.Int_map.filter_map (fun _ k_opt -> k_opt) cv_map in
  let hps =
    Utils.Int_map.fold
      (fun yi k acc -> apply_unchecked yi k acc ?metrics)
      cv_map hps
  in
  (cv_map, hps)
