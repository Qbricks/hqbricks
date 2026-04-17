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

open Ast
open Base

let builtin_gates =
  [
    "CCNOT";
    "CNOT";
    "CSIGN";
    "H";
    "X";
    "Y";
    "Z";
    "I";
    "PH";
    "S";
    "T";
    "RX";
    "RY";
    "RZ";
    "SWAP";
    "iSWAP";
    "SQRTSWAP";
  ]

(* Ir of AQASM *)
let remove_blank_lines str =
  let lines = String.split_on_char '\n' str in
  let non_blank_lines =
    List.filter (fun line -> String.trim line <> "") lines
  in
  String.concat "\n" non_blank_lines

let of_aqasm str =
  (* Remove blank lines is needed for the parsing *)
  let str_no_blank = remove_blank_lines str in
  let lexbuf = Lexing.from_string str_no_blank in
  let ir = Aqasm_parser.main_program Aqasm_lexer.token lexbuf in
  remove_skip_seq ir

let of_aqasm_file file = of_aqasm @@ Utils.read_file file

(* Ir to AQASM *)
let build_reg_map regs =
  List.fold_left
    (fun (reg_map, cur_len) (name, len) ->
      (Utils.String_map.add name (cur_len, len) reg_map, Z.(cur_len + len)))
    (Utils.String_map.empty, Z.zero)
    regs

let get_i_from_reg_map name i reg_map =
  let i_start, len = Utils.String_map.find name reg_map in
  if Z.(geq i len) then
    failwith
    @@ Printf.sprintf "Index out of range: reg {%s} len {%s} i {%s}" name
         (Z.to_string len) (Z.to_string i)
  else Z.(i_start + i)

let qreg_to_aqasm qreg_map = function
  | QCons (name, _) ->
      let i_start, len = Utils.String_map.find name qreg_map in
      let str = ref "" in
      let i_end = Z.(i_start + len) in
      let i = ref i_start in
      while Z.lt !i i_end do
        str :=
          Printf.sprintf "%sq[%s]%s" !str (Z.to_string !i)
            (if !i = Z.(i_end - ~$1) then "" else ",");
        i := Z.succ !i
      done;
      !str
  | QIndex ((name, _), i) ->
      Printf.sprintf "q[%s]" @@ Z.to_string (get_i_from_reg_map name i qreg_map)

let creg_to_aqasm creg_map = function
  | CCons (name, _) ->
      let i_start, len = Utils.String_map.find name creg_map in
      let str = ref "" in
      let i_end = Z.(i_start + len) in
      let i = ref i_start in
      while Z.lt !i i_end do
        str :=
          Printf.sprintf "%sc[%s]%s" !str (Z.to_string !i)
            (if !i = Z.(i_end - ~$1) then "" else ",");
        i := Z.succ !i
      done;
      !str
  | CIndex ((name, _), i) ->
      Printf.sprintf "c[%s]" @@ Z.to_string (get_i_from_reg_map name i creg_map)

let rec bool_to_aqasm creg_map = function
  | False | True -> failwith "Cannot convert false/true to AQASM"
  | CBitVal ((name, _), i) ->
      Printf.sprintf "c[%s]" @@ Z.to_string (get_i_from_reg_map name i creg_map)
  | Not (And (b1, b2)) ->
      "~(" ^ bool_to_aqasm creg_map b1 ^ " & " ^ bool_to_aqasm creg_map b2 ^ ")"
  | Not b -> "~" ^ bool_to_aqasm creg_map b
  | And (b1, b2) ->
      bool_to_aqasm creg_map b1 ^ " & " ^ bool_to_aqasm creg_map b2

let angle_to_aqasm a =
  if Z.(geq a ~$0) then "PI/" ^ Z.(to_string @@ pow ~$2 (to_int a))
  else "-1*PI/" ^ Z.(to_string @@ pow ~$2 (to_int (-a)))

(* The given gate will be considered as a builtin gate in one of those cases:
   - If they have the same name case insensitive
   - If the gate name is the same as the building gate name with n leading
     'c' characters case insentitive and if the number of qreg parameters
     of the gate is superior than or equal to n + 1. In this case each 'c' is
     considered as a control, hense the need of having at least n + 1 qreg parameters *)
let find_builtin_gate_name gate =
  let aux builtin_gate =
    let open Gate in
    let open Str in
    let builtin_gate_len = String.length builtin_gate in
    let name_len = String.length gate.name in
    let qreg_params_len = List.length gate.qreg_params in
    if
      builtin_gate_len <> name_len
      && name_len - builtin_gate_len + 1 < qreg_params_len
    then false
    else
      let regex = regexp_case_fold @@ "^[c]*" ^ Str.quote builtin_gate ^ "$" in
      Str.string_match regex Gate.(gate.name) 0
  in
  List.find_opt aux builtin_gates

let gate_name_to_aqasm gate =
  let open Gate in
  match find_builtin_gate_name gate with
  | Some gate_name ->
      (gate_name, String.length gate.name - String.length gate_name)
  | None -> (gate.name, 0)

let param_to_aqasm = function
  | Gate.Param.Int i -> Z.to_string i
  | Gate.Param.Angle i -> angle_to_aqasm i
  | Gate.Param.Scalar (i1, i2) -> Z.to_string i1 ^ "/" ^ Z.to_string i2

let params_to_aqasm params =
  let rec aux acc = function
    | [] -> acc
    | [ i ] -> acc ^ param_to_aqasm i
    | i :: tl -> aux (acc ^ param_to_aqasm i ^ ",") tl
  in
  aux "" params

let get_qreg_params_len qreg_params =
  let len = ref Z.one in
  List.iter
    (fun qreg ->
      let l = match qreg with QCons (_, len) -> len | QIndex _ -> Z.one in
      if Z.(gt l ~$1) then
        if Z.(gt !len ~$1) && l <> !len then
          failwith
            "All qregs of size > 1 must have the same length in gate parralel \
             application"
        else len := l
      else ())
    qreg_params;
  !len

let qreg_param_to_aqasm i qreg_map = function
  | QCons (name, _) ->
      Printf.sprintf "q[%s]" @@ Z.to_string (get_i_from_reg_map name i qreg_map)
  | QIndex _ as qreg -> qreg_to_aqasm qreg_map qreg

let qreg_params_to_aqasm i qreg_map qregs =
  let rec aux acc = function
    | [] -> acc
    | [ qreg ] -> acc ^ qreg_param_to_aqasm i qreg_map qreg
    | qreg :: tl -> aux (acc ^ qreg_param_to_aqasm i qreg_map qreg ^ ",") tl
  in
  aux "" qregs

let gate_to_aqasm ?(prefix = "") qreg_map gate =
  let open Gate in
  let gate_name, ctrl_count = gate_name_to_aqasm gate in
  let ctrl_prefix = Utils.n_string "CTRL(" ctrl_count in
  let ctrl_suffix = Utils.n_string ")" ctrl_count in
  let params_str =
    match gate.params with
    | [] -> ""
    | _ as params -> "[" ^ params_to_aqasm params ^ "]"
  in
  if List.is_empty gate.qreg_params then failwith "Gate with no qreg params"
  else
    let qreg_params_len = get_qreg_params_len gate.qreg_params in
    let gate_aqasm = ref "" in
    let i = ref Z.zero in
    while Z.(lt !i qreg_params_len) do
      gate_aqasm :=
        !gate_aqasm
        ^ Printf.sprintf "%s%s%s%s%s %s\n" prefix ctrl_prefix gate_name
            params_str ctrl_suffix
            (qreg_params_to_aqasm !i qreg_map gate.qreg_params);
      i := Z.succ !i
    done;
    !gate_aqasm

let init_qreg_to_aqasm qreg_map qreg =
  Printf.sprintf "RESET %s\n" @@ qreg_to_aqasm qreg_map qreg

let meas_to_aqasm qreg_map creg_map qreg creg =
  Printf.sprintf "MEAS %s %s\n"
    (qreg_to_aqasm qreg_map qreg)
    (creg_to_aqasm creg_map creg)

let ir_find_non_builtin_gates ir =
  let rec aux = function
    | Skip | InitQReg _ | Meas _ | SetCReg _ -> []
    | If (_, ir) -> aux ir
    | Seq (ir1, ir2) -> aux ir1 @ aux ir2
    | Gate gate -> (
        match find_builtin_gate_name gate with Some _ -> [] | None -> [ gate ])
  in
  aux ir |> Gate.remove_gate_list_duplicates

let gate_param_to_type_aqasm = function
  | Gate.Param.Int _ -> "int"
  | Gate.Param.Angle _ | Gate.Param.Scalar _ -> "float"

let gate_params_to_types_aqasm params =
  String.concat " " (List.map gate_param_to_type_aqasm params)

let non_builtin_gates_to_aqasm non_builtin_gates =
  let aux acc gate =
    acc
    ^ Gate.(
        Printf.sprintf "DEFINE PARAM %s%s : %d\n" gate.name
          (if List.is_empty gate.params then ""
           else " " ^ gate_params_to_types_aqasm gate.params)
          (List.length gate.qreg_params))
  in
  List.fold_left aux "" non_builtin_gates

let bool_find_and_not b =
  let aux = function
    | False | True | CBitVal _ -> false
    | Not _ | And _ -> true
  in
  aux @@ bool_simp_false_true b

let rec ir_find_and_not = function
  | Skip | InitQReg _ | Meas _ | Gate _ | SetCReg _ -> false
  | Seq (ir1, ir2) -> ir_find_and_not ir1 || ir_find_and_not ir2
  | If (b, ir) -> bool_find_and_not b || ir_find_and_not ir

let rec to_aqasm_aux qreg_map creg_map last_cbit_i = function
  | Skip -> ""
  | InitQReg qreg -> init_qreg_to_aqasm qreg_map qreg
  | Seq (prog1, prog2) ->
      to_aqasm_aux qreg_map creg_map last_cbit_i prog1
      ^ to_aqasm_aux qreg_map creg_map last_cbit_i prog2
  | If (cond, ir) -> (
      match (bool_simp_false_true cond, ir) with
      | False, _ -> ""
      | True, ir -> to_aqasm_aux qreg_map creg_map last_cbit_i ir
      | CBitVal ((name, _), i), Gate gate ->
          let prefix =
            Printf.sprintf "? c[%s] : "
            @@ Z.to_string (get_i_from_reg_map name i creg_map)
          in
          gate_to_aqasm ~prefix qreg_map gate
      | b, Gate gate ->
          let prefix =
            Printf.sprintf "LOGIC c[%s] (%s)\n" (Z.to_string last_cbit_i)
              (bool_to_aqasm creg_map b)
            ^ Printf.sprintf "? c[%s] : " (Z.to_string last_cbit_i)
          in
          gate_to_aqasm ~prefix qreg_map gate
      | _ ->
          failwith
            "Only classical if containing a gate can be converted to AQASM")
  | Meas (qreg, creg) -> meas_to_aqasm qreg_map creg_map qreg creg
  | Gate gate -> gate_to_aqasm qreg_map gate
  | SetCReg _ -> failwith "Cannot convert set_creg to AQASM"

let to_aqasm ir =
  let non_builtin_gates = ir_find_non_builtin_gates ir in
  let header =
    match non_builtin_gates_to_aqasm non_builtin_gates with
    | "" -> ""
    | s -> s ^ "\n"
  in
  let and_not = ir_find_and_not ir in
  let qregs, cregs = find_regs ir in
  let qreg_map, qtotal_len = build_reg_map qregs in
  let creg_map, ctotal_len = build_reg_map cregs in
  (* If there are boolean expressions with operators,
     we will need an extra cbit to compute them in AQASM *)
  let ctotal_len = if and_not then Z.succ ctotal_len else ctotal_len in
  let qbits_decl = "qubits " ^ Z.to_string qtotal_len ^ "\n" in
  let cbits_decl = "cbits " ^ Z.to_string ctotal_len ^ "\n" in
  header ^ "BEGIN\n"
  ^ (if qtotal_len = Z.zero then "" else qbits_decl)
  ^ (if ctotal_len = Z.zero then "" else cbits_decl)
  ^ "\n"
  ^ to_aqasm_aux qreg_map creg_map Z.(pred ctotal_len) ir
  ^ "\nEND\n"
