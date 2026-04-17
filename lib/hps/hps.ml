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

module Y_map = Y_map
module X_set = X_set
module Y_set = Y_set
module Reg_name_set = Reg_name_set
module Var = Var
module Var_set = Var_set
module Dyadic1 = Dyadic1
module Hket = Hket
module Phase = Phase
module Scalar = Scalar
module Mem = Mem
module Mem_stack = Mem_stack
module Output = Output
module Support = Support

type t = {
  phase : Phase.t;
  scalar : Scalar.t;
  output : Output.t;
  support : Support.t;
  y_count : int;
}

let zero =
  {
    phase = Phase.zero;
    scalar = Scalar.zero;
    output = Output.empty;
    support = Support.empty;
    y_count = 0;
  }

let one =
  {
    phase = Phase.zero;
    scalar = Scalar.one;
    output = Output.empty;
    support = Support.empty;
    y_count = 0;
  }

let add_phase var_list dy hps =
  let phase = Phase.addl var_list dy hps.phase in
  { hps with phase }

let set_phase phase hps = { hps with phase }

let mul_scalar s hps =
  let scalar = Scalar.SMul (s, hps.scalar) in
  { hps with scalar }

let set_scalar scalar hps = { hps with scalar }

let set_qmem qmem hps =
  let output = Output.{ hps.output with qmem } in
  { hps with output }

let add_qmem reg_id ket hps =
  let qmem = Mem.add reg_id ket hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_vec reg_id_start ket_list hps =
  let qmem = Mem.add_vec reg_id_start ket_list hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_vec_const reg_id_start len ket hps =
  let qmem = Mem.add_vec_const reg_id_start len ket hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_vec_zero reg_id_start len hps =
  let qmem = Mem.add_vec_zero reg_id_start len hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_vec_one reg_id_start len hps =
  let qmem = Mem.add_vec_one reg_id_start len hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_vec_x reg_id_start len x_start hps =
  let qmem = Mem.add_vec_x reg_id_start len x_start hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let add_qmem_int reg_id_start len n hps =
  let qmem = Mem.add_int reg_id_start len n hps.output.qmem in
  { hps with output = { hps.output with qmem } }

let set_cmem_stack cmem_stack hps =
  let output = Output.{ hps.output with cmem_stack } in
  { hps with output }

let add_cmem reg_id ket hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add reg_id ket cmem :: cms
    | _ -> [ Mem.(empty |> add reg_id ket) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_cmem_vec reg_id_start ket_list hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add_vec reg_id_start ket_list cmem :: cms
    | _ -> [ Mem.(empty |> add_vec reg_id_start ket_list) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_cmem_vec_const reg_id_start len ket hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add_vec_const reg_id_start len ket cmem :: cms
    | _ -> [ Mem.(empty |> add_vec_const reg_id_start len ket) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_cmem_vec_zero reg_id_start len hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add_vec_zero reg_id_start len cmem :: cms
    | _ -> [ Mem.(empty |> add_vec_zero reg_id_start len) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_cmem_vec_one reg_id_start len hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add_vec_one reg_id_start len cmem :: cms
    | _ -> [ Mem.(empty |> add_vec_one reg_id_start len) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_cmem_vec_x reg_id_start len x_start hps =
  let cmem_stack =
    match hps.output.cmem_stack with
    | cmem :: cms -> Mem.add_vec_x reg_id_start len x_start cmem :: cms
    | _ -> [ Mem.(empty |> add_vec_x reg_id_start len x_start) ]
  in
  { hps with output = { hps.output with cmem_stack } }

let add_empty_cmem_stack_elem hps =
  let cmem_stack = Mem.empty :: hps.output.cmem_stack in
  { hps with output = { hps.output with cmem_stack } }

let add_support yi_list hps =
  match yi_list with
  | [] -> hps
  | hd :: tl ->
      let support = Support.add_seq (List.to_seq yi_list) hps.support in
      let yi_max = List.fold_left max hd tl in
      let y_count = if yi_max >= hps.y_count then yi_max + 1 else hps.y_count in
      { hps with support; y_count }

let set_y_count y_count hps = { hps with y_count }

let simp_scalar hps =
  let scalar = Scalar.simp hps.scalar in
  { hps with scalar }

let set_y_to_zero yi hps =
  {
    hps with
    phase = Phase.set_y_to_zero yi hps.phase;
    scalar = Scalar.set_y_to_zero yi hps.scalar;
    output = Output.set_y_to_zero yi hps.output;
    support = Support.remove yi hps.support;
  }

let set_y_to_one yi hps =
  {
    hps with
    phase = Phase.set_y_to_one yi hps.phase;
    scalar = Scalar.set_y_to_one yi hps.scalar;
    output = Output.set_y_to_one yi hps.output;
    support = Support.remove yi hps.support;
  }

let set_y_values yi_zeros yi_ones hps =
  {
    hps with
    phase = Phase.set_y_values yi_zeros yi_ones hps.phase;
    scalar = Scalar.set_y_values yi_zeros yi_ones hps.scalar;
    output = Output.set_y_values yi_zeros yi_ones hps.output;
    support = Support.(diff (diff hps.support yi_zeros) yi_ones);
  }

let remove_cmem_stack_trailing_voids hps =
  let cmem_stack = Mem_stack.remove_trailing_voids hps.output.cmem_stack in
  { hps with output = { hps.output with cmem_stack } }

(* The norm2 and norm2_opt functions are only meant to compute the squared norm
   in some of the cases where it can be computed efficiently, and it could be
   generalized more. *)
let norm2 hps =
  if Phase.is_const hps.phase && not (Scalar.contains_any_y hps.scalar) then
    let output_ys = Output.find_all_y hps.output in
    if Y_set.is_empty output_ys then
      Scalar.(
        square
          (SMul (hps.scalar, make_int Z.(~$1 lsl Support.cardinal hps.support))))
    else if Y_set.(equal output_ys (Output.find_unique_y hps.output)) then
      let y_support_only_count =
        Support.cardinal hps.support - Y_set.cardinal output_ys
      in
      let scalar =
        Scalar.(SMul (hps.scalar, make_int Z.(~$1 lsl y_support_only_count)))
      in
      Scalar.(
        simp
          (SMul (square scalar, make_int Z.(~$1 lsl Y_set.cardinal output_ys))))
    else
      failwith
        "norm2 with an output containing y not unique is not implemented yet"
  else
    failwith
      "norm2 with a phase containing x or y or a scalar containing y is not \
       implemented yet"

let norm2_opt hps =
  if Phase.is_const hps.phase && not (Scalar.contains_any_y hps.scalar) then
    let output_ys = Output.find_all_y hps.output in
    if Y_set.is_empty output_ys then
      Some
        Scalar.(
          square
            (SMul (hps.scalar, make_int Z.(~$1 lsl Support.cardinal hps.support))))
    else if Y_set.(equal output_ys (Output.find_unique_y hps.output)) then
      let y_support_only_count =
        Support.cardinal hps.support - Y_set.cardinal output_ys
      in
      let scalar =
        Scalar.(SMul (hps.scalar, make_int Z.(~$1 lsl y_support_only_count)))
      in
      Some
        Scalar.(
          simp
            (SMul (square scalar, make_int Z.(~$1 lsl Y_set.cardinal output_ys))))
    else None
  else None

let equal hps1 hps2 =
  Phase.equal hps1.phase hps2.phase
  && Output.equal hps1.output hps2.output
  && Support.equal hps1.support hps2.support
  && Scalar.equal (Scalar.simp hps1.scalar) (Scalar.simp hps2.scalar)

let qmem_reg_to_int name len hps = Mem.reg_to_int name len hps.output.qmem

let qmem_reg_to_int_big_endian name len hps =
  Mem.reg_to_int_big_endian name len hps.output.qmem

let cmem_reg_to_int name len hps =
  match hps.output.cmem_stack with
  | cmem :: _ -> Mem.reg_to_int name len cmem
  | _ -> failwith "cmem_stack is empty"

let cmem_reg_to_int_big_endian name len hps =
  match hps.output.cmem_stack with
  | cmem :: _ -> Mem.reg_to_int_big_endian name len cmem
  | _ -> failwith "cmem_stack is empty"

let to_string hps =
  let indent = "  " in
  "<\n" ^ indent ^ Phase.to_string hps.phase ^ "\n" ^ indent ^ ",\n" ^ indent
  ^ Scalar.to_string hps.scalar
  ^ "\n" ^ indent ^ ".\n" ^ indent
  ^ Output.to_string indent hps.output
  ^ "\n>"
  ^ Support.to_string hps.support
