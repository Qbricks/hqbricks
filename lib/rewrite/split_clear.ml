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

let check yi_zeros yi_ones hps =
  if not (Hps.Y_set.disjoint yi_zeros yi_ones) then
    Error "yi_zeros and yi_ones are not disjoint"
  else if not Hps.(Support.subset yi_zeros hps.support) then
    Error "An element from yi_zeros is not in the support"
  else if not Hps.(Support.subset yi_ones hps.support) then
    Error "An element from yi_ones is not in the support"
  else Ok ()

let apply_unchecked ?metrics yi_zeros yi_ones hps =
  Metrics.add_rewrites_opt metrics 2;
  Hps.set_y_values yi_zeros yi_ones hps

let check_and_apply ?metrics yi_zeros yi_ones hps =
  match check yi_zeros yi_ones hps with
  | Error s -> Error s
  | Ok () -> Ok (apply_unchecked yi_zeros yi_ones hps ?metrics)

exception Internal_error of string

let apply_with_target_cmem_aux ?metrics target_cmem hps =
  let add_var_to_yi_zeros (var : Hps.Var.t) yi_zeros yi_ones =
    match var with
    | X _ -> raise (Internal_error "x input variables are not handled")
    | Y yi when Hps.Y_set.mem yi yi_ones ->
        raise
          (Internal_error
             "target_cmem and hps cmem cannot match, a path variable needs to \
              be false and true at the same time")
    | Y yi -> Hps.Y_set.add yi yi_zeros
  in
  let add_var_to_yi_ones (var : Hps.Var.t) yi_zeros yi_ones =
    match var with
    | X _ -> raise (Internal_error "x input variables are not handled")
    | Y yi when Hps.Y_set.mem yi yi_zeros ->
        raise
          (Internal_error
             "target_cmem and hps cmem cannot match, a path variable needs to \
              be false and true at the same time")
    | Y yi -> Hps.Y_set.add yi yi_ones
  in
  let add_vs_to_yi_zeros vs yi_zeros yi_ones =
    Hps.Var_set.fold
      (fun var acc -> add_var_to_yi_zeros var acc yi_ones)
      vs yi_zeros
  in
  let add_vs_to_yi_ones vs yi_zeros yi_ones =
    Hps.Var_set.fold
      (fun var acc -> add_var_to_yi_ones var yi_zeros acc)
      vs yi_ones
  in
  let f key tk (yi_zeros, yi_ones) =
    let k = Hps.Mem.find key Hps.(List.hd hps.output.cmem_stack) in
    if Hps.Hket.equal tk k then (yi_zeros, yi_ones)
    else if not Hps.Hket.(is_zero tk || is_one tk) then
      raise
        (Internal_error
           "Only true and false are handled in target_cmem unless both kets \
            exactly match")
    else
      match Hps.Hket.cardinal k with
      | 0 -> raise (Internal_error "target_cmem and hps cmem cannot match")
      | 1 when Hps.(Var_set.cardinal (Hket.choose k) = 0) ->
          raise (Internal_error "target_cmem and hps cmem cannot match")
      | 1 ->
          let vs = Hps.Hket.choose k in
          if Hps.Hket.is_one tk then
            (yi_zeros, add_vs_to_yi_ones vs yi_zeros yi_ones)
          else if Hps.Hket.is_zero tk && Hps.Var_set.cardinal vs = 1 then
            (add_vs_to_yi_zeros vs yi_zeros yi_ones, yi_ones)
          else raise (Internal_error "yi values cannot have multiple solutions")
      | 2 when Hps.Hket.contains_one k ->
          let vs =
            Hps.Hket.find_first (fun vs -> not Hps.Var_set.(is_empty vs)) k
          in
          if Hps.Hket.is_zero tk then
            (yi_zeros, add_vs_to_yi_ones vs yi_zeros yi_ones)
          else if Hps.Hket.is_one tk && Hps.Var_set.cardinal vs = 1 then
            (add_vs_to_yi_zeros vs yi_zeros yi_ones, yi_ones)
          else raise (Internal_error "yi values cannot have multiple solutions")
      | _ -> raise (Internal_error "yi values cannot have multiple solutions")
  in
  let yi_zeros, yi_ones = Hps.Mem.fold f target_cmem Hps.Y_set.(empty, empty) in
  check_and_apply yi_zeros yi_ones hps ?metrics

let apply_with_target_cmem ?metrics target_cmem hps =
  try apply_with_target_cmem_aux target_cmem hps ?metrics
  with Internal_error msg -> Error msg
