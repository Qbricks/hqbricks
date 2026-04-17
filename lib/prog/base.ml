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

type qreg = QCons of string * pr_int | QSlice of qreg * pr_int * pr_int
[@@deriving eq, show { with_path = false }]

and creg = CCons of string * pr_int | CSlice of creg * pr_int * pr_int
[@@deriving eq, show { with_path = false }]

and pr_bool =
  | BVar of string
  | False
  | True
  | QBitVal of qreg * pr_int
  | CBitVal of creg * pr_int
  | Not of pr_bool
  | Xor of pr_bool * pr_bool
  | And of pr_bool * pr_bool
  | Cond of pr_bool * pr_bool * pr_bool
  | BIter of string * pr_bool * string * pr_int * pr_int * pr_bool
[@@deriving eq, show { with_path = false }]

and pr_int =
  | IVar of string
  | Const of Z.t
      [@printer fun fmt z -> fprintf fmt "(Const %s)" (Z.to_string z)]
  | QGet of qreg
  | CGet of creg
  | Add of pr_int * pr_int
  | Mul of pr_int * pr_int
  | Pow of pr_int * pr_int
  | QRegLen of qreg
  | CRegLen of creg
  | IIter of string * pr_int * string * pr_int * pr_int * pr_int
[@@deriving eq, show { with_path = false }]

(* pr_bool *)
let bvar var_name = BVar var_name
let b_true = True
let b_false = False
let qbit_val qreg i = QBitVal (qreg, i)
let qbit_valv qreg var_name = QBitVal (qreg, IVar var_name)
let cbit_val creg i = CBitVal (creg, i)
let cbit_valv creg var_name = CBitVal (creg, IVar var_name)
let b_not b = Not b
let b_xor b1 b2 = Xor (b1, b2)
let b_and b1 b2 = And (b1, b2)
let b_cond b1 b2 b3 = Cond (b1, b2, b3)
let b_iter s1 b1 s2 i1 i2 b2 = BIter (s1, b1, s2, i1, i2, b2)
let ( ! ) = b_not
let ( ^ ) = b_xor
let ( & ) = b_and

(* pr_int *)
let ivar var_name = IVar var_name
let i_of_z zi = Const zi
let i_of_int i = Const (Z.of_int i)
let i_of_string str = Const (Z.of_string str)
let i_of_qreg qreg = QGet qreg
let i_of_creg creg = CGet creg
let i_add i1 i2 = Add (i1, i2)
let i_sub i1 i2 = Add (i1, Mul (i_of_int (-1), i2))
let i_mul i1 i2 = Mul (i1, i2)
let i_pow i1 i2 = Pow (i1, i2)
let qreg_len qreg = QRegLen qreg
let creg_len creg = CRegLen creg
let i_iter s1 i1 s2 i2 i3 i4 = IIter (s1, i1, s2, i2, i3, i4)
let ( ~$ ) = i_of_int
let ( + ) = i_add
let ( - ) = i_sub
let ( * ) = i_mul
let ( ** ) = i_pow

(* qreg *)
let qreg qreg_name qreg_len = QCons (qreg_name, qreg_len)
let q_slice qreg i_start i_end = QSlice (qreg, i_start, i_end)
let q_slice_from qreg i_start = QSlice (qreg, i_start, QRegLen qreg)
let q_slice_to qreg i_end = QSlice (qreg, ~$0, i_end)
let q_idx qreg i = QSlice (qreg, i, i + ~$1)

let q_idxv qreg var_name =
  let iv = ivar var_name in
  QSlice (qreg, iv, iv + ~$1)

(* creg *)
let creg creg_name creg_len = CCons (creg_name, creg_len)
let c_slice creg i_start i_end = CSlice (creg, i_start, i_end)
let c_slice_from creg i_start = CSlice (creg, i_start, CRegLen creg)
let c_slice_to creg i_end = CSlice (creg, ~$0, i_end)
let c_idx creg i = CSlice (creg, i, i + ~$1)

let c_idxv creg var_name =
  let iv = ivar var_name in
  CSlice (creg, iv, iv + ~$1)

(* substitute_ivar *)
let rec qreg_substitute_ivar var_name i_sub qreg =
  match qreg with
  | QCons (name, len) -> QCons (name, pr_int_substitute_ivar var_name i_sub len)
  | QSlice (qreg, i_start, i_end) ->
      QSlice
        ( qreg_substitute_ivar var_name i_sub qreg,
          pr_int_substitute_ivar var_name i_sub i_start,
          pr_int_substitute_ivar var_name i_sub i_end )

and creg_substitute_ivar var_name i_sub creg =
  match creg with
  | CCons (name, len) -> CCons (name, pr_int_substitute_ivar var_name i_sub len)
  | CSlice (creg, i_start, i_end) ->
      CSlice
        ( creg_substitute_ivar var_name i_sub creg,
          pr_int_substitute_ivar var_name i_sub i_start,
          pr_int_substitute_ivar var_name i_sub i_end )

and pr_bool_substitute_ivar var_name i_sub b =
  match b with
  | (BVar _ | False | True) as b -> b
  | QBitVal (qreg, i) ->
      QBitVal
        ( qreg_substitute_ivar var_name i_sub qreg,
          pr_int_substitute_ivar var_name i_sub i )
  | CBitVal (creg, i) ->
      CBitVal
        ( creg_substitute_ivar var_name i_sub creg,
          pr_int_substitute_ivar var_name i_sub i )
  | Not b -> Not (pr_bool_substitute_ivar var_name i_sub b)
  | Xor (b1, b2) ->
      Xor
        ( pr_bool_substitute_ivar var_name i_sub b1,
          pr_bool_substitute_ivar var_name i_sub b2 )
  | And (b1, b2) ->
      And
        ( pr_bool_substitute_ivar var_name i_sub b1,
          pr_bool_substitute_ivar var_name i_sub b2 )
  | Cond (b1, b2, b3) ->
      Cond
        ( pr_bool_substitute_ivar var_name i_sub b1,
          pr_bool_substitute_ivar var_name i_sub b2,
          pr_bool_substitute_ivar var_name i_sub b3 )
  | BIter (bvar_name, b_start, ivar_name, i_start, i_end, b_expr) ->
      BIter
        ( bvar_name,
          pr_bool_substitute_ivar var_name i_sub b_start,
          ivar_name,
          pr_int_substitute_ivar var_name i_sub i_start,
          pr_int_substitute_ivar var_name i_sub i_end,
          pr_bool_substitute_ivar var_name i_sub b_expr )

and pr_int_substitute_ivar var_name i_sub i =
  match i with
  | IVar vn when String.equal vn var_name -> i_sub
  | (IVar _ | Const _) as i -> i
  | QGet qreg -> QGet (qreg_substitute_ivar var_name i_sub qreg)
  | CGet creg -> CGet (creg_substitute_ivar var_name i_sub creg)
  | Add (i1, i2) ->
      Add
        ( pr_int_substitute_ivar var_name i_sub i1,
          pr_int_substitute_ivar var_name i_sub i2 )
  | Mul (i1, i2) ->
      Mul
        ( pr_int_substitute_ivar var_name i_sub i1,
          pr_int_substitute_ivar var_name i_sub i2 )
  | Pow (i1, i2) ->
      Pow
        ( pr_int_substitute_ivar var_name i_sub i1,
          pr_int_substitute_ivar var_name i_sub i2 )
  | QRegLen qreg -> QRegLen (qreg_substitute_ivar var_name i_sub qreg)
  | CRegLen creg -> CRegLen (creg_substitute_ivar var_name i_sub creg)
  | IIter (acc_var_name, acc_i_start, ivar_name, i_start, i_end, i_expr) ->
      IIter
        ( acc_var_name,
          pr_int_substitute_ivar var_name i_sub acc_i_start,
          ivar_name,
          pr_int_substitute_ivar var_name i_sub i_start,
          pr_int_substitute_ivar var_name i_sub i_end,
          pr_int_substitute_ivar var_name i_sub i_expr )

(* to_string *)
let rec qreg_to_string qreg =
  Stdlib.(
    match qreg with
    | QCons (qreg_name, len) -> qreg_name ^ "(" ^ pr_int_to_string len ^ ")"
    | QSlice (qreg, start_i, end_i) -> (
        match (start_i, end_i) with
        | Const i1, Const i2 when Z.(equal (i1 + one) i2) ->
            qreg_to_string qreg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | i1, Add (i2, Const i3) when i1 = i2 && Z.(equal i3 one) ->
            qreg_to_string qreg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | i1, Add (Const i2, i3) when i1 = i3 && Z.(equal i2 one) ->
            qreg_to_string qreg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | _ ->
            qreg_to_string qreg ^ "[" ^ pr_int_to_string start_i ^ " : "
            ^ pr_int_to_string end_i ^ "]"))

and creg_to_string qreg =
  Stdlib.(
    match qreg with
    | CCons (creg_name, len) -> creg_name ^ "(" ^ pr_int_to_string len ^ ")"
    | CSlice (creg, start_i, end_i) -> (
        match (start_i, end_i) with
        | Const i1, Const i2 when Z.(equal (i1 + one) i2) ->
            creg_to_string creg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | i1, Add (i2, Const i3) when i1 = i2 && Z.(equal i3 one) ->
            creg_to_string creg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | i1, Add (Const i2, i3) when i1 = i3 && Z.(equal i2 one) ->
            creg_to_string creg ^ "[" ^ pr_int_to_string start_i ^ "]"
        | _ ->
            creg_to_string creg ^ "[" ^ pr_int_to_string start_i ^ " : "
            ^ pr_int_to_string end_i))

and pr_bool_to_string b =
  Stdlib.(
    match b with
    | BVar var_name -> var_name
    | False -> "false"
    | True -> "true"
    | QBitVal (qreg, i) -> qreg_to_string qreg ^ "[" ^ pr_int_to_string i ^ "]"
    | CBitVal (creg, i) -> creg_to_string creg ^ "[" ^ pr_int_to_string i ^ "]"
    | Not b -> "!" ^ pr_bool_to_string b
    | Xor (b1, b2) -> pr_bool_to_string b1 ^ " ⊕ " ^ pr_bool_to_string b2
    | And (b1, b2) -> pr_bool_to_string b1 ^ " ^ " ^ pr_bool_to_string b2
    | Cond (b1, b2, b3) ->
        pr_bool_to_string b1 ^ " ? " ^ pr_bool_to_string b2 ^ " : "
        ^ pr_bool_to_string b3
    | BIter (bvar_name, start_b, ivar_name, start_i, end_i, b_expr) ->
        "(iter " ^ ivar_name ^ " = " ^ pr_int_to_string start_i ^ " to "
        ^ pr_int_to_string end_i ^ " (" ^ pr_bool_to_string b_expr ^ ")["
        ^ bvar_name ^ "])" ^ pr_bool_to_string start_b)

and pr_int_to_string i =
  Stdlib.(
    match i with
    | IVar var_name -> var_name
    | Const i -> Z.to_string i
    | QGet qreg -> "get(" ^ qreg_to_string qreg ^ ")"
    | CGet creg -> "get(" ^ creg_to_string creg ^ ")"
    | Add (i1, i2) -> pr_int_to_string i1 ^ " + " ^ pr_int_to_string i2
    | Mul (i1, i2) -> pr_int_to_string i1 ^ " * " ^ pr_int_to_string i2
    | Pow (i1, i2) -> pr_int_to_string i1 ^ " ** " ^ pr_int_to_string i2
    | QRegLen qreg -> "len(" ^ qreg_to_string qreg ^ ")"
    | CRegLen creg -> "len(" ^ creg_to_string creg ^ ")"
    | IIter (acc_var_name, acc_start_i, ivar_name, start_i, end_i, i_expr) ->
        "(iter " ^ ivar_name ^ " = " ^ pr_int_to_string start_i ^ " to "
        ^ pr_int_to_string end_i ^ " (" ^ pr_int_to_string i_expr ^ ")["
        ^ acc_var_name ^ "])"
        ^ pr_int_to_string acc_start_i)
