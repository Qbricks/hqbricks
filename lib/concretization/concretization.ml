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

module Vector_map = struct
  module Mem_stack_map = Map.Make (Mem_stack)

  module V = struct
    type t = Phase.t * Scalar.t

    let equal (p1, s1) (p2, s2) = Phase.equal p1 p2 && Scalar.equal s1 s2

    let to_string (p, s) =
      if Phase.eq_zero p then Scalar.to_string s
      else if Phase.eq_one_half p then Scalar.to_string Scalar.(simp @@ SNeg s)
      else "e^(2πi * " ^ Phase.to_string p ^ ") * " ^ Scalar.to_string s
  end

  module Mem_map = struct
    module M = Map.Make (Mem)

    type t = V.t M.t

    let empty = M.empty

    let add key (p, s) mm =
      M.update key
        (fun v2_opt ->
          match v2_opt with
          | Some (p2, s2) -> (
              match (p, p2) with
              | p, p2 when Phase.eq_zero p && Phase.eq_zero p2 ->
                  let scalar = Scalar.(simp @@ SAdd (s, s2)) in
                  if Scalar.eq_zero scalar then None
                  else Some (Phase.zero, scalar)
              | p, p2 when Phase.eq_zero p && Phase.eq_one_half p2 ->
                  let scalar = Scalar.(simp @@ SNeg (SAdd (s, SNeg s2))) in
                  if Scalar.eq_zero scalar then None
                  else Some (Phase.zero, scalar)
              | p, p2 when Phase.eq_one_half p && Phase.eq_zero p2 ->
                  let scalar = Scalar.(simp @@ SNeg (SAdd (SNeg s, s2))) in
                  if Scalar.eq_zero scalar then None
                  else Some (Phase.zero, scalar)
              | p, p2 when Phase.eq_one_half p && Phase.eq_one_half p2 ->
                  let scalar = Scalar.(simp @@ SNeg (SAdd (s, s2))) in
                  if Scalar.eq_zero scalar then None
                  else Some (Phase.zero, scalar)
              | _ ->
                  failwith
                    "Addition of vectors with complex phase not implemented yet"
              )
          | None -> Some (p, s))
        mm

    let find_opt qmem mm = M.find_opt qmem mm
    let is_empty mm = M.is_empty mm
    let equal = M.equal V.equal

    let proba mm =
      M.fold
        (fun _ (_, s) acc -> Scalar.(simp @@ SAdd (acc, square s)))
        mm Scalar.zero

    let to_string indent mm =
      let sep = "\n" ^ indent ^ "+ " in
      let str =
        M.fold
          (fun qmem v acc ->
            acc ^ V.to_string v ^ " * " ^ Mem.qmem_to_string qmem ^ sep)
          mm indent
      in
      String.sub str 0 (String.length str - String.length sep)
  end

  type t = Mem_map.t Mem_stack_map.t

  let empty = Mem_stack_map.empty

  let add ms qm v vm =
    Mem_stack_map.update ms
      (fun mm_opt ->
        match mm_opt with
        | Some mm ->
            let mm = Mem_map.add qm v mm in
            if Mem_map.is_empty mm then None else Some mm
        | None ->
            let mm = Mem_map.(empty |> add qm v) in
            if Mem_map.is_empty mm then None else Some mm)
      vm

  let find_opt ms vm = Mem_stack_map.find_opt ms vm
  let fold f vm acc = Mem_stack_map.fold f vm acc
  let equal = Mem_stack_map.equal Mem_map.equal

  (*
   * In order to generate all the possible combinations of values for the y path variables,
   * this functions uses the last {support cardinal} bits of a Zarith integer going from 0
   * to {support cardinal}
   * For a given combination, we only store the y path variables of value 0 as it is
   * sufficient to compute the vector map
   *)
  let of_hps hps =
    let support_card = Support.cardinal hps.support in
    let support_array = Support.to_list hps.support |> Array.of_list in
    let i_end = Z.(shift_left one support_card) in

    (* Function to transform a Zarith integer into its corresponding set of y of value 0 *)
    let i_to_y_zeros i =
      let rec fold_y_zeros i shift_len acc =
        if shift_len >= support_card then acc
        else if Z.((i asr shift_len) land one = zero) then
          fold_y_zeros i (shift_len + 1)
          @@ Y_set.add support_array.(shift_len) acc
        else fold_y_zeros i (shift_len + 1) acc
      in
      fold_y_zeros i 0 Y_set.empty
    in

    (* Function to compute all the elements of the vector map *)
    let rec fold hps i acc =
      if i >= i_end then acc
      else
        let y_zeros = i_to_y_zeros i in
        let phase = Phase.set_all_y y_zeros hps.phase in
        let scalar = Scalar.set_all_y y_zeros hps.scalar in
        let qmem = Mem.set_all_y y_zeros hps.output.qmem in
        let cms = Mem_stack.set_all_y y_zeros hps.output.cmem_stack in
        fold hps (Z.succ i) @@ add cms qmem (phase, scalar) acc
    in
    fold hps Z.zero empty

  let to_string vm =
    fold
      (fun cms mm acc ->
        acc ^ Mem_stack.to_string "" cms ^ ":\n" ^ Mem_map.to_string "  " mm
        ^ "\n")
      vm ""
end

let hps_proba_output output_spec hps =
  let vm = Vector_map.of_hps hps in
  match Vector_map.find_opt Output.(output_spec.cmem_stack) vm with
  | Some mm -> (
      match Vector_map.Mem_map.find_opt Output.(output_spec.qmem) mm with
      | Some (_, s) -> Scalar.(simp @@ square s)
      | None -> Scalar.zero)
  | None -> Scalar.zero

let hps_proba_qmem qmem_spec hps =
  let vm = Vector_map.of_hps hps in
  Vector_map.fold
    (fun _ mm acc ->
      if Vector_map.Mem_map.M.cardinal mm == 1 then
        match Vector_map.Mem_map.M.choose mm with
        | qmem, (_, s) when Mem.equal qmem qmem_spec ->
            Scalar.(simp @@ SAdd (acc, square s))
        | _ -> acc
      else acc)
    vm Scalar.zero

let split_qmem_spec qmem_spec hps_product =
  let qmem_spec = ref qmem_spec in
  let qmem_specs =
    Array.map
      (fun hps ->
        Mem.fold
          (fun reg_id ket acc ->
            if Mem.contains_reg reg_id hps.output.qmem then (
              qmem_spec := Mem.remove reg_id !qmem_spec;
              Mem.add reg_id ket acc)
            else acc)
          !qmem_spec Mem.empty)
      hps_product
  in
  if Mem.is_empty !qmem_spec then Some qmem_specs else None

let hps_proba_qmem_fact ?metrics qmem_spec hps =
  let hps_product =
    Rewrite.Fact_distr.Xy_vars.find_and_apply_all ?metrics hps
  in
  match split_qmem_spec qmem_spec hps_product with
  | None -> Scalar.zero
  | Some qmem_specs ->
      let len = Array.length hps_product in
      let rec loop i acc =
        if i >= len then acc
        else
          loop (i + 1)
            Scalar.(
              simp (SMul (hps_proba_qmem qmem_specs.(i) hps_product.(i), acc)))
      in
      loop 0 Scalar.one

let hps_proba_cmem_stack cms_spec hps =
  let vm = Vector_map.of_hps hps in
  match Vector_map.find_opt cms_spec vm with
  | Some mm -> Vector_map.Mem_map.proba mm
  | None -> Scalar.zero

let hps_proba_cmem cmem_spec hps =
  let vm = Vector_map.of_hps hps in
  Vector_map.fold
    (fun cms mm acc ->
      if (not (List.is_empty cms)) && Mem.equal (List.hd cms) cmem_spec then
        Scalar.(simp @@ SAdd (acc, Vector_map.Mem_map.proba mm))
      else acc)
    vm Scalar.zero

let hps_proba_cmem_split ?metrics cmem_spec hps =
  match Rewrite.Split_clear.apply_with_target_cmem ?metrics cmem_spec hps with
  | Ok hps -> (
      let hps = Rewrite.reduce_hps ?metrics hps in
      match Hps.norm2_opt hps with
      | Some s -> s
      | None -> hps_proba_cmem cmem_spec hps)
  | Error _ -> hps_proba_cmem cmem_spec hps
