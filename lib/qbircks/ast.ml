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

open Base

type t =
  | Skip
  | InitQReg of qreg
  | Seq of t * t
  | If of ir_bool * t
  | Meas of qreg * creg
  | Gate of Gate.t
  | SetCReg of creg * ir_int
[@@deriving yojson, eq, show { with_path = false }]

let to_string = show

let rec remove_skip_seq = function
  | (Skip | InitQReg _ | Meas _ | Gate _ | SetCReg _) as ir -> ir
  | Seq (ir1, ir2) -> (
      match (remove_skip_seq ir1, remove_skip_seq ir2) with
      | Skip, ir | ir, Skip -> ir
      | ir1, ir2 -> Seq (ir1, ir2))
  | If (b, ir) -> If (b, remove_skip_seq ir)

(* Read/write file *)
let write_to_file filename ir =
  to_yojson ir |> Utils.write_json_to_file filename

let read_from_file filename =
  let in_channel = open_in filename in
  let json =
    try
      let json = Yojson.Safe.from_channel in_channel in
      close_in in_channel;
      json
    with e ->
      close_in in_channel;
      raise e
  in
  match of_yojson json with Ok ir -> ir | Error e -> failwith e

(* Get control type *)
type control_type = Quantum | Classical | Hybrid | Unhandled

let rec get_control_type =
  let open Prog.Base in
  function
  | QBitVal _ -> Quantum
  | BVar _ | False | True | CBitVal _ -> Classical
  | Not b -> get_control_type b
  | Xor (b1, b2) | And (b1, b2) -> (
      match (get_control_type b1, get_control_type b2) with
      | Quantum, Quantum -> Quantum
      | Classical, Classical -> Classical
      | Unhandled, _ | _, Unhandled -> Unhandled
      | Quantum, Classical | Classical, Quantum | Hybrid, _ | _, Hybrid ->
          Hybrid)
  | Cond _ | BIter _ -> Unhandled

(* Functions to convert prog quantum control to ir *)
let get_ctrl_qregs b =
  let open Prog.Base in
  let rec aux not_count = function
    | BVar _ -> failwith "Cannot convert bvar to ir"
    | False -> failwith "false shouldn't appear in quantum control"
    | True -> failwith "true shouldn't appear in quantum control"
    | QBitVal (qreg, i) -> [ (qreg_of_prog qreg, int_of_prog i, not_count) ]
    | CBitVal _ -> failwith "cbit_val shoudn't appear in quantum control"
    | Not b -> aux Stdlib.(not_count + 1) b
    | Xor _ -> failwith "Cannot convert xor to ir"
    | And (b1, b2) -> aux not_count b1 @ aux not_count b2
    | Cond _ -> failwith "Cannot convert cond to ir"
    | BIter _ -> failwith "Cannot convert biter to ir"
  in
  aux 0 b

let ctrl_qregs_to_qreg_params ctrl_qregs =
  List.map
    (fun (qreg, i, _) ->
      match qreg with QCons reg_id | QIndex (reg_id, _) -> QIndex (reg_id, i))
    ctrl_qregs

let rec quantum_ctrl_to_ir ctrl_qreg_params = function
  | Prog.PVar _ -> failwith "Cannot convert pvar to ir"
  | Prog.Skip -> Skip
  | Prog.InitQReg _ ->
      failwith "InitQReg should not appear inside quantum control"
  | Prog.Seq (prog1, prog2) ->
      Seq
        ( quantum_ctrl_to_ir ctrl_qreg_params prog1,
          quantum_ctrl_to_ir ctrl_qreg_params prog2 )
  | Prog.For (_name, _i_start, _i_end, _prog) ->
      failwith "Cannot convert for inside quantum control to ir"
  | Prog.IfElse _ ->
      failwith "Cannot convert if_else inside quantum control to ir"
  | Prog.Meas _ -> failwith "Measure should not appear inside quantum control"
  | Prog.Gate gate ->
      let gate = Gate.of_prog gate in
      let c =
        let is_uppercase c = c >= 'A' && c <= 'Z' in
        if String.length gate.name > 0 && is_uppercase (String.get gate.name 0)
        then "C"
        else "c"
      in
      Gate
        Gate.
          {
            gate with
            name = Utils.n_string c (List.length ctrl_qreg_params) ^ gate.name;
            qreg_params = ctrl_qreg_params @ gate.qreg_params;
          }
  | Prog.SetCReg _ -> failwith "Cannot convert set_creg to ir"

(* Prog conversions *)
let rec of_prog = function
  | Prog.PVar _ -> failwith "Cannot convert pvar to ir"
  | Prog.Skip -> Skip
  | Prog.InitQReg qreg -> InitQReg (qreg_of_prog qreg)
  | Prog.Seq (prog1, prog2) -> Seq (of_prog prog1, of_prog prog2)
  | Prog.For (i_name, i_start, i_end, prog) ->
      let i_start = int_of_prog i_start in
      let i_end = int_of_prog i_end in
      if i_start >= i_end then Skip
      else
        let p =
          ref @@ of_prog @@ Prog.substitute_ivar i_name (Const i_start) prog
        in
        let i = ref @@ Z.succ i_start in
        while Z.(lt !i i_end) do
          p := Seq (!p, of_prog @@ Prog.substitute_ivar i_name (Const !i) prog);
          i := Z.succ !i
        done;
        !p
  | Prog.IfElse (b, prog1, prog2) -> (
      match get_control_type b with
      | Quantum ->
          let ctrl_qregs = get_ctrl_qregs b in
          if
            List.exists
              (fun (_, _, not_count) -> not_count mod 2 = 1)
              ctrl_qregs
          then failwith "Cannot convert not on quantum control to ir"
          else if prog2 <> Skip then
            failwith "Quantum control else should be skip to be converted to ir"
          else
            let ctrl_qreg_params = ctrl_qregs_to_qreg_params ctrl_qregs in
            quantum_ctrl_to_ir ctrl_qreg_params prog1
      | Classical -> (
          if prog2 <> Skip then
            failwith
              "Classical control else should be skip to be converted to ir"
          else
            match prog1 with
            | PVar _ | Seq _ | For _ | IfElse _ | SetCReg _ ->
                failwith
                  "Cannot convert if body to ir, only skip, init_qreg, meas \
                   and gate are allowed"
            | Skip | InitQReg _ | Meas _ | Gate _ ->
                If (bool_of_prog b, of_prog prog1))
      | Hybrid -> failwith "Cannot convert hybrid control to ir"
      | Unhandled -> failwith "Cannot convert b_cond and b_iter to ir")
  | Prog.Meas (qreg, creg) -> Meas (qreg_of_prog qreg, creg_of_prog creg)
  | Prog.Gate gate -> Gate (Gate.of_prog gate)
  | Prog.SetCReg (creg, i) -> SetCReg (creg_of_prog creg, int_of_prog i)

module Gate_name_map = Map.Make (String)

let rec to_prog gate_func_map = function
  | Skip -> Prog.Skip
  | InitQReg qreg -> Prog.InitQReg (qreg_to_prog qreg)
  | Seq (prog1, prog2) ->
      Prog.Seq (to_prog gate_func_map prog1, to_prog gate_func_map prog2)
  | If (b, prog) ->
      Prog.IfElse (bool_to_prog b, to_prog gate_func_map prog, Prog.Skip)
  | Meas (qreg, creg) -> Prog.Meas (qreg_to_prog qreg, creg_to_prog creg)
  | Gate gate -> (Gate_name_map.find Gate.(gate.name) gate_func_map) gate
  | SetCReg (creg, i) -> Prog.SetCReg (creg_to_prog creg, int_to_prog i)

(* Find regs *)
let rec find_regs_aux = function
  | Skip -> ([], [])
  | InitQReg qreg -> ([ get_qreg_id qreg ], [])
  | Seq (prog1, prog2) ->
      let qregs1, cregs1 = find_regs_aux prog1 in
      let qregs2, cregs2 = find_regs_aux prog2 in
      (qregs1 @ qregs2, cregs1 @ cregs2)
  | If (b, prog) ->
      let cregs1 = bool_find_cregs b in
      let qregs2, cregs2 = find_regs_aux prog in
      (qregs2, cregs1 @ cregs2)
  | Meas (qreg, creg) -> ([ get_qreg_id qreg ], [ get_creg_id creg ])
  | Gate gate -> (Gate.find_qregs gate, [])
  | SetCReg (creg, _) -> ([], [ get_creg_id creg ])

let find_regs prog =
  let qregs, cregs = find_regs_aux prog in
  (remove_reg_id_list_duplicates qregs, remove_reg_id_list_duplicates cregs)
