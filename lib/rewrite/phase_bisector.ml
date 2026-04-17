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

let check yi_start yi_end dy_fact xset_fact hps =
  let yset = Utils.create_int_set yi_start yi_end in
  let yi_count = Y_set.cardinal yset in
  let dy_fact = Dyadic1.reduce dy_fact in
  if yi_count < 2 then
    Error
      (Printf.sprintf
         "yi_end - yi_start must be at least 2; yi_start = %d, yi_end = %d"
         yi_start yi_end)
  else if Dyadic1.(den_pow dy_fact) < yi_count then
    Error
      (Printf.sprintf
         "dy_fact.den_pow must be at least yi_end - yi_start; den_pow = %d, \
          yi_start = %d, yi_end = %d"
         Dyadic1.(den_pow dy_fact)
         yi_start yi_end)
  else if Dyadic1.(equal dy_fact zero) then
    Error (Printf.sprintf "dy_fact should not be 0")
  else if Output.contains_y_from_set yset hps.output then
    Error
      (Printf.sprintf
         "No target y should be in the output; yi_start = %d, yi_end = %d\n\
          output:\n\
          %s"
         yi_start yi_end
         (Output.to_string "" hps.output))
  else if Scalar.contains_y_from_set yset hps.scalar then
    Error
      (Printf.sprintf
         "No target y should be in the scalar; yi_start = %d, yi_end = %d\n\
          scalar:\n\
          %s"
         yi_start yi_end
         (Scalar.to_string hps.scalar))
  else
    let xvar_set = Var_set.of_x_set xset_fact in
    (* Construction of the phase to bisect *)
    let rec loop acc dy_fact i =
      if i >= yi_end then acc
      else
        loop
          Phase.(add Var_set.(add (Y i) xvar_set) dy_fact acc)
          Dyadic1.(mul_pow2 1 dy_fact)
          (i + 1)
    in
    let bisect_phase = loop Phase.zero dy_fact yi_start in
    let remaining_phase = Phase.diff hps.phase bisect_phase in
    if Phase.cardinal hps.phase - Phase.cardinal remaining_phase <> yi_count
    then
      Error
        (Printf.sprintf
           "The phase to bisect is not present in the phase;\n\
            bisect_phase:\n\
            %s\n\
            phase:\n\
            %s"
           (Phase.to_string bisect_phase)
           (Phase.to_string hps.phase))
    else if Phase.contains_y_from_set yset remaining_phase then
      Error
        (Printf.sprintf
           "No target y should be in the remaining_phase; yi_start = %d, \
            yi_end = %d\n\
            remaining_phase:\n\
            %s"
           yi_start yi_end
           (Phase.to_string remaining_phase))
    else Ok ()

let apply_unchecked ?metrics yi_start yi_end dy_fact xset_fact hps =
  let yset = Utils.create_int_set yi_start yi_end in
  let yset_card = Y_set.cardinal yset in
  let dy_fact = Dyadic1.reduce dy_fact in
  if not @@ X_set.is_empty xset_fact then
    failwith "x in Phase-Bisector application is not implemented yet";
  let phase =
    let phase_rm =
      Phase.(
        filter
          (fun vs _ ->
            Var_set.for_all
              (fun v ->
                match v with X _ -> true | Y yi -> not @@ Y_set.mem yi yset)
              vs)
          hps.phase)
    in
    let vs = Var_set.of_x_set xset_fact in
    let dy =
      Dyadic1.(mul_pow2 (-1) dy_fact |> mul_z Z.((~$1 lsl yset_card) - ~$1))
    in
    Phase.add vs dy phase_rm
  in
  let scalar =
    let scalar_mul =
      Scalar.(
        SMul
          ( Sin (angle_of_dyadic1 @@ Dyadic1.mul_pow2 (yset_card - 1) dy_fact),
            SInv (Sin (angle_of_dyadic1 @@ Dyadic1.mul_pow2 (-1) dy_fact)) ))
    in
    Scalar.(SMul (scalar_mul, hps.scalar))
  in
  let support = Support.(diff hps.support yset) in
  Metrics.add_rewrites_opt metrics (yi_end - yi_start);
  { hps with phase; scalar; support }

let check_and_apply ?metrics yi_start yi_end dy_fact xset_fact hps =
  match check yi_start yi_end dy_fact xset_fact hps with
  | Error s -> Error s
  | Ok () -> Ok (apply_unchecked yi_start yi_end dy_fact xset_fact hps ?metrics)

let find_valid_y_in_phase p =
  let y_count_map = Phase.count_y p in
  Y_map.fold
    (fun yi count acc -> if count = 1 then Y_set.add yi acc else acc)
    y_count_map Y_set.empty

let find_valid_y hps =
  let valid_y = find_valid_y_in_phase hps.phase in
  Y_set.diff
    (Y_set.diff valid_y (Scalar.find_all_y hps.scalar))
    (Output.find_all_y hps.output)

let find_aux ?(all = false) hps =
  let valid_y = find_valid_y hps in
  if Y_set.cardinal valid_y < 2 then []
  else
    let fold_fun vs dy ((params_list, current_state, valid_y) as acc) =
      if
        (Option.is_none current_state && Y_set.cardinal valid_y < 2)
        || (params_list <> [] && not all)
      then acc
      else
        let xset, yset = Var_set.partition_xset_yset vs in
        (* x and y as factors are not handled for now *)
        if
          (not @@ X_set.is_empty xset)
          || Y_set.cardinal yset <> 1
          || not Y_set.(mem (choose yset) valid_y)
        then
          (* Invalid y case *)
          let valid_y = Y_set.diff valid_y yset in
          match current_state with
          | None -> (params_list, None, valid_y)
          | Some (yi_start, yi_end, _, _) when yi_end - yi_start < 2 ->
              (params_list, None, valid_y)
          | Some (yi_start, yi_end, dy_fact, xset_fact) ->
              ( (yi_start, yi_end, dy_fact, xset_fact) :: params_list,
                None,
                valid_y )
        else
          (* Valid y case *)
          let valid_y = Y_set.diff valid_y yset in
          let yi = Y_set.choose yset in
          match current_state with
          | None -> (params_list, Some (yi, yi + 1, dy, xset), valid_y)
          | Some (yi_start, yi_end, dy_fact, xset_fact)
            when yi = yi_end
                 && Dyadic1.(equal (mul_pow2 (yi_end - yi_start) dy_fact) dy)
                 && X_set.equal xset_fact xset ->
              if Y_set.is_empty valid_y then
                ( (yi_start, yi_end + 1, dy_fact, xset_fact) :: params_list,
                  None,
                  valid_y )
              else
                ( params_list,
                  Some (yi_start, yi_end + 1, dy_fact, xset_fact),
                  valid_y )
          | Some (yi_start, yi_end, dy_fact, xset_fact)
            when yi_start - yi_end >= 2 ->
              ( (yi_start, yi_end, dy_fact, xset_fact) :: params_list,
                None,
                valid_y )
          | Some _ -> (params_list, None, valid_y)
    in
    let params_list, _, _ = Phase.fold fold_fun hps.phase ([], None, valid_y) in
    List.rev params_list

let find hps = match find_aux hps with hd :: _ -> Some hd | [] -> None
let find_all hps = find_aux ~all:true hps

let find_and_apply ?metrics hps =
  match find hps with
  | None -> (None, hps)
  | Some (yi_start, yi_end, dy_fact, xset_fact) ->
      ( Some (yi_start, yi_end, dy_fact, xset_fact),
        apply_unchecked yi_start yi_end dy_fact xset_fact hps ?metrics )

let find_and_apply_all ?metrics hps =
  let params_list = find_all hps in
  let hps =
    List.fold_left
      (fun acc (yi_start, yi_end, dy_fact, xset_fact) ->
        apply_unchecked yi_start yi_end dy_fact xset_fact acc ?metrics)
      hps params_list
  in
  (params_list, hps)
