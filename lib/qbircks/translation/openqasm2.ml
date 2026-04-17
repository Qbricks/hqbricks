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

(* U and CX are builtin gates, the others are in qelib1.inc
   which is systematically included *)
let builtin_gates =
  [
    "U";
    "CX";
    "u3";
    "u2";
    "u1";
    "id";
    "x";
    "y";
    "z";
    "h";
    "s";
    "sdg";
    "t";
    "tdg";
    "rx";
    "ry";
    "rz";
    "cz";
    "cy";
    "ch";
    "ccx";
    "crz";
    "cu1";
    "cu3";
  ]

(* Ir of OpenQASM2 *)
let of_openqasm2 str =
  let lexbuf = Lexing.from_string str in
  let ir = Openqasm2_parser.main_program Openqasm2_lexer.token lexbuf in
  remove_skip_seq ir

let of_openqasm2_file file =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let ir = Openqasm2_parser.main_program Openqasm2_lexer.token lexbuf in
  remove_skip_seq ir

(* Ir to OpenQASM2 *)
let qreg_to_openqasm2 = function
  | QCons (name, _) -> name
  | QIndex ((name, _), i) -> name ^ "[" ^ Z.to_string i ^ "]"

let creg_to_openqasm2 = function
  | CCons (name, _) -> name
  | CIndex ((name, _), i) -> name ^ "[" ^ Z.to_string i ^ "]"

let angle_to_openqasm2 a =
  if Z.(geq a ~$0) then "pi/" ^ Z.(to_string @@ pow ~$2 (to_int a))
  else "-pi/" ^ Z.(to_string @@ pow ~$2 (to_int (-a)))

(* The given gate will be considered as a builtin gate
   if they have the same name case insensitive *)
let find_builtin_gate_name gate =
  List.find_opt (Utils.string_equal_ci Gate.(gate.name)) builtin_gates

let gate_name_to_openqasm2 gate =
  match find_builtin_gate_name gate with
  | Some gate_name -> gate_name
  | None -> String.lowercase_ascii Gate.(gate.name)

let param_to_openqasm2 = function
  | Gate.Param.Int i -> Z.to_string i
  | Gate.Param.Angle i -> angle_to_openqasm2 i
  | Gate.Param.Scalar (i1, i2) -> Z.to_string i1 ^ "/" ^ Z.to_string i2

let params_to_openqasm2 angles =
  let rec aux acc = function
    | [] -> acc
    | [ i ] -> acc ^ param_to_openqasm2 i
    | i :: tl -> aux (acc ^ param_to_openqasm2 i ^ ",") tl
  in
  aux "" angles

let qregs_to_openqasm2 qregs =
  let rec aux acc = function
    | [] -> acc
    | [ qreg ] -> acc ^ qreg_to_openqasm2 qreg
    | qreg :: tl -> aux (acc ^ qreg_to_openqasm2 qreg ^ ",") tl
  in
  aux "" qregs

let gate_to_openqasm2 gate =
  let open Gate in
  let name_str = gate_name_to_openqasm2 gate in
  let angles_str =
    match gate.params with
    | [] -> ""
    | _ as params -> "(" ^ params_to_openqasm2 params ^ ")"
  in
  if List.is_empty gate.qreg_params then failwith "Gate with no qreg params"
  else name_str ^ angles_str ^ " " ^ qregs_to_openqasm2 gate.qreg_params ^ ";\n"

let init_qreg_to_openqasm2 qreg = "reset " ^ qreg_to_openqasm2 qreg ^ ";\n"

let meas_to_openqasm2 qreg creg =
  "measure " ^ qreg_to_openqasm2 qreg ^ " -> " ^ creg_to_openqasm2 creg ^ ";\n"

module Cbit = struct
  type t = reg_id * Z.t

  let raw = (("", Z.zero), Z.zero)

  let compare ((name1, len1), i1) ((name2, len2), i2) =
    let cmp_name = compare name1 name2 in
    if cmp_name <> 0 then cmp_name
    else
      let cmp_len = Z.compare len1 len2 in
      if cmp_len <> 0 then cmp_len else Z.compare i1 i2
end

module Cbit_map = Map.Make (Cbit)

let get_ctrl_cbit_map b =
  let rec aux not_count b acc =
    if Cbit_map.find_opt Cbit.raw acc = Some true then acc
    else
      match b with
      | False -> Cbit_map.add Cbit.raw true acc
      | True -> Cbit_map.add Cbit.raw false acc
      | CBitVal (reg_id, i) -> (
          let is_not = not_count mod 2 = 1 in
          match Cbit_map.find_opt (reg_id, i) acc with
          | Some is_not2 when is_not = is_not2 -> acc
          | Some _ -> Cbit_map.singleton Cbit.raw true
          | None -> Cbit_map.add (reg_id, i) is_not acc)
      | Not b -> aux (not_count + 1) b acc
      | And (b1, b2) -> aux not_count b1 acc |> aux not_count b2
  in
  aux 0 b Cbit_map.empty

let get_cbit_map_unique_reg_id cbit_map =
  match
    Cbit_map.find_first_opt
      (fun ((name, _), _) -> String.length name > 0)
      cbit_map
  with
  | None -> None
  | Some (((name, len), _), _) ->
      if
        Cbit_map.for_all
          (fun ((n, l), _) _ ->
            String.length n = 0 || (String.equal name n && l = len))
          cbit_map
      then Some (name, len)
      else None

let cbit_arr_of_cbit_map cbit_map (name, len) =
  let cbit_arr = Array.make (Z.to_int len) None in
  Cbit_map.iter
    (fun ((n, l), i) is_not ->
      if String.equal n name && l = len then
        cbit_arr.(Z.to_int i) <- Some is_not)
    cbit_map;
  cbit_arr

let cbit_arr_to_int cbit_arr =
  Array.fold_right
    (fun is_not_opt acc ->
      Z.((acc lsl 1) + if Option.get is_not_opt then zero else one))
    cbit_arr Z.zero

let cif_else_to_openqasm2 creg_name creg_int =
  let if_str = "if(" ^ creg_name ^ "==" ^ Z.to_string creg_int ^ ") " in
  function
  | Skip -> ""
  | InitQReg qreg -> if_str ^ init_qreg_to_openqasm2 qreg
  | Seq _ -> failwith "Seq should not appear in if"
  | If _ -> failwith "Imbricated if should not appear"
  | Meas (qreg, creg) -> if_str ^ meas_to_openqasm2 qreg creg
  | Gate gate -> if_str ^ gate_to_openqasm2 gate
  | SetCReg _ -> failwith "Cannot convert set_creg to OpenQASM2"

let ir_find_non_builtin_gates ir =
  let rec aux = function
    | Skip | InitQReg _ | Meas _ | SetCReg _ -> []
    | If (_, ir) -> aux ir
    | Seq (ir1, ir2) -> aux ir1 @ aux ir2
    | Gate gate -> (
        match find_builtin_gate_name gate with Some _ -> [] | None -> [ gate ])
  in
  aux ir |> Gate.remove_gate_list_duplicates

let count_gate_params_per_type params =
  List.fold_left
    (fun (i, a, s) param ->
      match param with
      | Gate.Param.Int _ -> (i + 1, a, s)
      | Gate.Param.Angle _ -> (i, a + 1, s)
      | Gate.Param.Scalar _ -> (i, a, s + 1))
    (0, 0, 0) params

let gate_params_to_names_openqasm2 params =
  let int_tot_count, angle_tot_count, scalar_tot_count =
    count_gate_params_per_type params
  in
  let int_count = ref 0 in
  let angle_count = ref 0 in
  let scalar_count = ref 0 in
  String.concat ","
    (List.map
       (fun param ->
         match param with
         | Gate.Param.Int _ ->
             "int_val"
             ^
             if int_tot_count > 1 then (
               let count_str = string_of_int !int_count in
               int_count := !int_count + 1;
               count_str)
             else ""
         | Gate.Param.Angle _ ->
             "theta"
             ^
             if angle_tot_count > 1 then (
               let count_str = string_of_int !angle_count in
               angle_count := !angle_count + 1;
               count_str)
             else ""
         | Gate.Param.Scalar _ ->
             "frac"
             ^
             if scalar_tot_count > 1 then (
               let count_str = string_of_int !scalar_count in
               scalar_count := !scalar_count + 1;
               count_str)
             else "")
       params)

let gate_qreg_params_to_names_openqasm2 qreg_params =
  match qreg_params with
  | [] -> failwith "Gate with no qreg params"
  | [ _ ] -> "q"
  | _ ->
      String.concat ","
        (List.mapi (fun i _ -> "q" ^ string_of_int i) qreg_params)

let non_builtin_gates_to_openqasm2 non_builtin_gates =
  let aux acc gate =
    let open Gate in
    acc
    ^ Printf.sprintf "opaque %s%s %s;\n"
        (String.lowercase_ascii gate.name)
        (if List.is_empty gate.params then ""
         else "(" ^ gate_params_to_names_openqasm2 gate.params ^ ")")
        (gate_qreg_params_to_names_openqasm2 gate.qreg_params)
  in
  List.fold_left aux "" non_builtin_gates

let rec to_openqasm2_aux = function
  | Skip -> ""
  | InitQReg qreg -> init_qreg_to_openqasm2 qreg
  | Seq (prog1, prog2) -> to_openqasm2_aux prog1 ^ to_openqasm2_aux prog2
  | If (cond, prog) -> (
      let cbit_map = get_ctrl_cbit_map cond in
      if Cbit_map.is_empty cbit_map then
        failwith "No cbit found in classical control"
      else if
        Cbit_map.cardinal cbit_map = 1
        && Cbit_map.find_opt Cbit.raw cbit_map = Some false
      then to_openqasm2_aux prog (* Condition true *)
      else if Cbit_map.find_opt Cbit.raw cbit_map = Some true then ""
      (* Condition false *)
        else
        match get_cbit_map_unique_reg_id cbit_map with
        | None ->
            failwith
              "Cannot convert classical control with multiple creg name/len to \
               OpenQASM2"
        | Some (name, len) ->
            let cbit_arr = cbit_arr_of_cbit_map cbit_map (name, len) in
            if Array.exists (fun is_not_opt -> is_not_opt = None) cbit_arr then
              failwith
                "Cannot convert classical control on a partial creg to \
                 OpenQASM2"
            else
              let creg_int = cbit_arr_to_int cbit_arr in
              cif_else_to_openqasm2 name creg_int prog)
  | Meas (qreg, creg) -> meas_to_openqasm2 qreg creg
  | Gate gate -> gate_to_openqasm2 gate
  | SetCReg _ -> failwith "Cannot convert set_creg to OpenQASM2"

let to_openqasm2 prog =
  let non_builtin_gates = ir_find_non_builtin_gates prog in
  let gates_declaration = non_builtin_gates_to_openqasm2 non_builtin_gates in
  let qregs, cregs = find_regs prog in
  let qregs_declaration =
    List.fold_left
      (fun acc (name, len) ->
        acc ^ "qreg " ^ name ^ "[" ^ Z.to_string len ^ "];\n")
      "" qregs
  in
  let cregs_declaration =
    List.fold_left
      (fun acc (name, len) ->
        acc ^ "creg " ^ name ^ "[" ^ Z.to_string len ^ "];\n")
      "" cregs
  in
  "OPENQASM 2.0;\ninclude \"qelib1.inc\";\n\n"
  ^ (if String.length gates_declaration = 0 then ""
     else gates_declaration ^ "\n")
  ^ (if String.length qregs_declaration = 0 then ""
     else qregs_declaration ^ "\n")
  ^ (if String.length cregs_declaration = 0 then ""
     else cregs_declaration ^ "\n")
  ^ to_openqasm2_aux prog
