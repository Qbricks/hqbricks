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

type ir_int = Z.t [@@deriving eq]

let show_ir_int = Z.to_string
let pp_ir_int fmt i = Format.fprintf fmt "Z.(~$%s)" @@ Z.to_string i
let ir_int_to_yojson ir_int = `Intlit (Z.to_string ir_int)

let ir_int_of_yojson = function
  | `Int i -> Ok (Z.of_int i)
  | `Intlit s -> (
      try Ok (Z.of_string s)
      with _ -> Error "Invalid string format for a big integer")
  | _ -> Error "Expected a JSON int or big integer string"

type reg_id = string * ir_int
[@@deriving yojson, eq, show { with_path = false }]

type qreg = QCons of reg_id | QIndex of reg_id * ir_int
[@@deriving yojson, eq, show { with_path = false }]

type creg = CCons of reg_id | CIndex of reg_id * ir_int
[@@deriving yojson, eq, show { with_path = false }]

type ir_bool =
  | False
  | True
  | CBitVal of reg_id * ir_int
  | Not of ir_bool
  | And of ir_bool * ir_bool
[@@deriving yojson, eq, show { with_path = false }]

let reg_id_to_string = show_reg_id
let qreg_to_string = show_qreg
let creg_to_string = show_creg
let ir_bool_to_string = show_ir_bool
let ir_int_to_string = show_ir_int

(* Prog conversions *)
let rec qreg_of_prog qreg =
  let rec aux = function
    | Prog.Base.QCons (name, len) ->
        (name, int_of_prog len, Z.zero, int_of_prog len)
    | Prog.Base.QSlice (qreg, i_start, i_end) ->
        let i_start = int_of_prog i_start in
        let i_end = int_of_prog i_end in
        let slice_len = Z.(i_end - i_start) in
        let name, len, is, _ = aux qreg in
        (name, len, Z.(is + i_start), slice_len)
  in
  let name, len, i_start, slice_len = aux qreg in
  if len = slice_len then
    match (len, qreg) with
    | i, Prog.Base.QSlice _ when i = Z.one -> QIndex ((name, len), Z.zero)
    | _ -> QCons (name, len)
  else if slice_len = Z.(~$1) then QIndex ((name, len), i_start)
  else failwith "Cannot convert qreg slices to ir"

and creg_of_prog creg =
  let rec aux = function
    | Prog.Base.CCons (name, len) ->
        (name, int_of_prog len, Z.zero, int_of_prog len)
    | Prog.Base.CSlice (creg, i_start, i_end) ->
        let i_start = int_of_prog i_start in
        let i_end = int_of_prog i_end in
        let slice_len = Z.(i_end - i_start) in
        let name, len, is, _ = aux creg in
        (name, len, Z.(is + i_start), slice_len)
  in
  let name, len, i_start, slice_len = aux creg in
  if len = slice_len then CCons (name, len)
  else if slice_len = Z.one then CIndex ((name, len), i_start)
  else failwith "Cannot convert creg slices to ir"

and bool_of_prog = function
  | Prog.Base.BVar _ -> failwith "Cannot convert bvar to ir"
  | Prog.Base.False -> False
  | Prog.Base.True -> True
  | Prog.Base.QBitVal _ -> failwith "Cannot convert qbit_val to ir"
  | Prog.Base.CBitVal (creg, i) -> (
      match creg_of_prog creg with
      | CCons reg_id | CIndex (reg_id, _) -> CBitVal (reg_id, int_of_prog i))
  | Prog.Base.Not b -> Not (bool_of_prog b)
  | Prog.Base.Xor _ -> failwith "Cannot convert xor to ir"
  | Prog.Base.And (b1, b2) -> And (bool_of_prog b1, bool_of_prog b2)
  | Prog.Base.Cond _ -> failwith "Cannot convert cond to ir"
  | Prog.Base.BIter _ -> failwith "Cannot convert biter to ir"

and int_of_prog i = Evaluator.Base.evaluate_int i

and scalar_of_prog = function
  | Hps.Scalar.SFrac (i1, i2) -> (i1, i2)
  | _ -> failwith "Cannot convert scalar other than SFrac to ir"

let rec qreg_to_prog = function
  | QCons (name, len) -> Prog.Base.(QCons (name, int_to_prog len))
  | QIndex ((name, len), i) ->
      Prog.Base.QSlice
        ( Prog.Base.QCons (name, int_to_prog len),
          Prog.Base.Const i,
          Prog.Base.(Const i + ~$1) )

and creg_to_prog = function
  | CCons (name, len) -> Prog.Base.(CCons (name, int_to_prog len))
  | CIndex ((name, len), i) ->
      Prog.Base.CSlice
        ( Prog.Base.CCons (name, int_to_prog len),
          Prog.Base.Const i,
          Prog.Base.(Const i + ~$1) )

and bool_to_prog = function
  | False -> Prog.Base.False
  | True -> Prog.Base.True
  | CBitVal ((name, len), i) ->
      Prog.Base.(CBitVal (CCons (name, int_to_prog len), int_to_prog i))
  | Not b -> Prog.Base.Not (bool_to_prog b)
  | And (b1, b2) -> Prog.Base.And (bool_to_prog b1, bool_to_prog b2)

and int_to_prog i = Prog.Base.Const i
and scalar_to_prog (num, den) = Hps.Scalar.SFrac (num, den)

(* Find creg *)
let get_qreg_id = function QCons reg_id | QIndex (reg_id, _) -> reg_id
let get_creg_id = function CCons reg_id | CIndex (reg_id, _) -> reg_id

let remove_reg_id_list_duplicates l =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | (name, _) :: tl when Utils.String_set.mem name seen -> aux seen acc tl
    | (name, len) :: tl ->
        aux (Utils.String_set.add name seen) ((name, len) :: acc) tl
  in
  aux Utils.String_set.empty [] l

let bool_find_cregs b =
  let rec aux = function
    | False | True -> []
    | CBitVal (creg, _) -> [ creg ]
    | Not b -> aux b
    | And (b1, b2) -> aux b1 @ aux b2
  in
  remove_reg_id_list_duplicates @@ aux b

(* bool_simp_false_true *)
let rec bool_simp_false_true = function
  | (False | True | CBitVal _) as b -> b
  | Not b ->
      Not
        (match bool_simp_false_true b with
        | True -> False
        | False -> True
        | (CBitVal _ | Not _ | And _) as b -> b)
  | And (b1, b2) -> (
      match (bool_simp_false_true b1, bool_simp_false_true b2) with
      | False, _ | _, False -> False
      | True, b | b, True -> b
      | b1, b2 -> And (b1, b2))
