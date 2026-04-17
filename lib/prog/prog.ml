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

module Base = Base
module Gate = Gate
include Base

type t =
  | PVar of string
  | Skip
  | InitQReg of qreg
  | Seq of t * t
  | For of string * pr_int * pr_int * t
  | IfElse of pr_bool * t * t
  | Meas of qreg * creg
  | Gate of Gate.t
  | SetCReg of creg * pr_int
[@@deriving eq, show { with_path = false }]

(* prog *)
let pvar var_name = PVar var_name
let skip = Skip
let init_qreg qreg = InitQReg qreg
let seq prog1 prog2 = Seq (prog1, prog2)
let if_else b prog1 prog2 = IfElse (b, prog1, prog2)
let if_b_then_p b p = IfElse (b, p, skip)
let p_for s i1 i2 p = For (s, i1, i2, p)

let for_ctrl_prog (ctrl_qreg, s) p =
  For (s, ~$0, qreg_len ctrl_qreg, if_b_then_p (qbit_val ctrl_qreg (ivar s)) p)

let c_for_ctrl_prog (ctrl_creg, s) p =
  For (s, ~$0, creg_len ctrl_creg, if_b_then_p (cbit_val ctrl_creg (ivar s)) p)

let meas qreg creg = Meas (qreg, creg)
let apply_gate qreg gate = Gate (gate qreg)
let set_creg creg i = SetCReg (creg, i)
let ( -- ) = seq
let ( >> ) = for_ctrl_prog

module C = struct
  let ( >> ) = c_for_ctrl_prog
end

let ( |> ) = apply_gate
let ( => ) = if_b_then_p
let ( -@ ) = meas
let indent_len = 4

let rec to_string_aux prog indent_level =
  let open Stdlib in
  let indent_str = String.make (indent_level * indent_len) ' ' in
  match prog with
  | PVar var_name -> indent_str ^ var_name ^ ";"
  | Skip -> indent_str ^ "skip;"
  | InitQReg qreg -> indent_str ^ "init(" ^ qreg_to_string qreg ^ ");"
  | Seq (prog1, prog2) ->
      to_string_aux prog1 indent_level ^ "\n" ^ to_string_aux prog2 indent_level
  | For (var_name, i_start, i_end, prog) ->
      indent_str ^ "for " ^ var_name ^ " = " ^ pr_int_to_string i_start ^ " to "
      ^ pr_int_to_string i_end ^ " do\n"
      ^ to_string_aux prog (indent_level + 1)
      ^ "\n" ^ indent_str ^ "done"
  | IfElse (cond, prog1, prog2) ->
      indent_str ^ "if " ^ pr_bool_to_string cond ^ " then\n"
      ^ to_string_aux prog1 (indent_level + 1)
      ^ "\n" ^ indent_str ^ "else\n"
      ^ to_string_aux prog2 (indent_level + 1)
      ^ "\n" ^ indent_str ^ "end"
  | Meas (qreg, creg) ->
      indent_str ^ qreg_to_string qreg ^ " -@ " ^ creg_to_string creg ^ ";"
  | Gate gate -> indent_str ^ Gate.to_string gate ^ ";"
  | SetCReg (creg, i) ->
      indent_str ^ "set(" ^ creg_to_string creg ^ ", " ^ pr_int_to_string i
      ^ ");"

let to_string prog = to_string_aux prog 0
let print prog = print_endline (to_string prog)

let rec substitute_ivar var_name i_sub prog =
  match prog with
  | (PVar _ | Skip) as p -> p
  | InitQReg qreg -> InitQReg (qreg_substitute_ivar var_name i_sub qreg)
  | Seq (prog1, prog2) ->
      Seq
        ( substitute_ivar var_name i_sub prog1,
          substitute_ivar var_name i_sub prog2 )
  | For (vn, i_start, i_end, prog) ->
      For
        ( vn,
          pr_int_substitute_ivar var_name i_sub i_start,
          pr_int_substitute_ivar var_name i_sub i_end,
          substitute_ivar var_name i_sub prog )
  | IfElse (cond, prog1, prog2) ->
      IfElse
        ( pr_bool_substitute_ivar var_name i_sub cond,
          substitute_ivar var_name i_sub prog1,
          substitute_ivar var_name i_sub prog2 )
  | Meas (qreg, creg) ->
      Meas
        ( qreg_substitute_ivar var_name i_sub qreg,
          creg_substitute_ivar var_name i_sub creg )
  | Gate gate -> Gate (Gate.substitute_ivar var_name i_sub gate)
  | SetCReg (creg, i) ->
      SetCReg
        ( creg_substitute_ivar var_name i_sub creg,
          pr_int_substitute_ivar var_name i_sub i )

let rec inverse_unitary = function
  | PVar _ -> None
  | Skip -> Some Skip
  | InitQReg _ -> None
  | Seq (prog1, prog2) -> (
      match (inverse_unitary prog1, inverse_unitary prog2) with
      | Some p1, Some p2 -> Some (Seq (p2, p1))
      | _ -> None)
  | For (var_name, i_start, i_end, prog) -> (
      match inverse_unitary prog with
      | Some p ->
          Some
            (For
               ( var_name,
                 i_start,
                 i_end,
                 substitute_ivar var_name
                   (i_start + i_end - ivar var_name - ~$1)
                   p ))
      | _ -> None)
  | IfElse (cond, prog1, prog2) -> (
      match (inverse_unitary prog1, inverse_unitary prog2) with
      | Some p1, Some p2 -> Some (IfElse (cond, p1, p2))
      | _ -> None)
  | Meas _ -> None
  | Gate gate -> Some (Gate (gate.inverse ()))
  | SetCReg _ -> None
