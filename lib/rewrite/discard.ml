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

let ( let* ) = Result.bind

let check_reg_name_set orns rns =
  if Reg_name_set.subset rns orns then Ok ()
  else Error "a reg from the reg name set is not in the output"

let check_hps_disc_phase p =
  if Phase.contains_any_x p then
    Error "the hps to discard contains an x in its phase"
  else Ok ()

let check_hps_disc_scalar s =
  if Scalar.contains_any_x s then
    Error "the hps to discard contains an x in its scalar"
  else Ok ()

let check_hps_disc_output o =
  if Output.contains_any_x o then
    Error "the hps to discard contains an x in its output"
  else Ok ()

let check_hps_disc_norm hps =
  match Hps.norm2_opt hps with
  | Some hps_norm2 -> Ok hps_norm2
  | None -> Error "the norm2 of the hps to discard couldn't be computed"

let check_hps_disc hps =
  let* () = check_hps_disc_phase hps.phase in
  let* () = check_hps_disc_scalar hps.scalar in
  let* () = check_hps_disc_output hps.output in
  check_hps_disc_norm hps

let check_aux ?(past_only = false) ?metrics reg_name_set hps =
  let out_reg_name_set = Output.find_reg_names hps.output in
  let* () = check_reg_name_set out_reg_name_set reg_name_set in
  let rns =
    if past_only then reg_name_set
    else Reg_name_set.diff out_reg_name_set reg_name_set
  in
  let* hps1, hps2 =
    Fact_distr.Reg.check_and_apply rns hps ~past_only ?metrics
  in
  let hps_keep, hps_disc = if past_only then (hps2, hps1) else (hps1, hps2) in
  let* hps_disc_norm2 = check_hps_disc hps_disc in
  let hps_keep =
    if Scalar.(equal hps_disc_norm2 one) then hps_keep
    else
      set_scalar
        Scalar.(simp (SMul (Sqrt hps_disc_norm2, hps_keep.scalar)))
        hps_keep
  in
  Ok (hps_keep, hps_disc)

let check ?(past_only = false) reg_name_set hps =
  check_aux ~past_only reg_name_set hps

let apply_unchecked ?(past_only = false) ?metrics reg_name_set hps =
  let out_reg_name_set = Output.find_reg_names hps.output in
  let rns =
    if past_only then reg_name_set
    else Reg_name_set.diff out_reg_name_set reg_name_set
  in
  match Fact_distr.Reg.check_and_apply rns hps ~past_only ?metrics with
  | Ok (hps1, hps2) ->
      Metrics.add_rewrites_opt metrics 1;
      if past_only then (hps2, hps1) else (hps1, hps2)
  | Error s -> failwith ("Discard.apply_unchecked failed: " ^ s)

let check_and_apply ?(past_only = false) ?metrics reg_name_set hps =
  let* hps_keep, hps_disc = check_aux reg_name_set hps ~past_only ?metrics in
  Metrics.add_rewrites_opt metrics 1;
  Ok (hps_keep, hps_disc)
