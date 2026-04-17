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

let rec evaluate_bool ?(var_val = Utils.Var_name_map.empty)
    (b : Prog.Base.pr_bool) hps =
  match b with
  | BVar _str -> failwith "Cannot evaluate BVar into an hps"
  | False -> Hket.zero
  | True -> Hket.one
  | QBitVal (qreg, i) ->
      let qreg_name, i_start, _ = evaluate_qreg qreg ~var_val in
      let i = Z.to_int (evaluate_int i ~var_val) in
      Mem.find (qreg_name, i_start + i) hps.output.qmem
  | CBitVal (creg, i) ->
      let creg_name, i_start, _ = evaluate_creg creg ~var_val in
      let i = Z.to_int (evaluate_int i ~var_val) in
      Mem.find (creg_name, i_start + i) (List.hd hps.output.cmem_stack)
  | Not b -> Hket.neg (evaluate_bool b hps ~var_val)
  | Xor (b1, b2) ->
      Hket.xor (evaluate_bool b1 hps ~var_val) (evaluate_bool b2 hps ~var_val)
  | And (b1, b2) ->
      Hket.mul (evaluate_bool b1 hps ~var_val) (evaluate_bool b2 hps ~var_val)
  | Cond _ -> failwith "Not implemented yet"
  | BIter _ -> failwith "Not implemented yet"

and evaluate_int ?(var_val = Utils.Var_name_map.empty) (i : Prog.Base.pr_int) =
  match i with
  | IVar name -> Z.of_int @@ Utils.Var_name_map.find name var_val
  | Const c -> c
  | QGet _qreg -> failwith "Not implemented yet"
  | CGet _creg -> failwith "Not implemented yet"
  | Add (i1, i2) -> Z.add (evaluate_int i1 ~var_val) (evaluate_int i2 ~var_val)
  | Mul (i1, i2) -> Z.mul (evaluate_int i1 ~var_val) (evaluate_int i2 ~var_val)
  | Pow (i1, i2) ->
      Z.pow (evaluate_int i1 ~var_val) (Z.to_int (evaluate_int i2 ~var_val))
  | QRegLen qreg -> (
      match qreg with
      | QCons (_, qreg_len) -> evaluate_int qreg_len ~var_val
      | QSlice (_, i_start, i_end) ->
          Z.sub (evaluate_int i_end ~var_val) (evaluate_int i_start ~var_val))
  | CRegLen creg -> (
      match creg with
      | CCons (_, creg_len) -> evaluate_int creg_len ~var_val
      | CSlice (_, i_start, i_end) ->
          Z.sub (evaluate_int i_end ~var_val) (evaluate_int i_start ~var_val))
  | IIter (_s1, _i1, _s2, _i2, _i3, _i4) -> failwith "Not implemented yet"

and evaluate_qreg ?(var_val = Utils.Var_name_map.empty) (qreg : Prog.Base.qreg)
    =
  match qreg with
  | QCons (name, len) -> (name, 0, Z.to_int (evaluate_int len ~var_val))
  | QSlice (qreg, i_start, i_end) ->
      let i_start = Z.to_int (evaluate_int i_start ~var_val) in
      let i_end = Z.to_int (evaluate_int i_end ~var_val) in
      let len = i_end - i_start in
      let name, is, _ = evaluate_qreg qreg ~var_val in
      (name, is + i_start, len)

and evaluate_creg ?(var_val = Utils.Var_name_map.empty) (creg : Prog.Base.creg)
    =
  match creg with
  | CCons (name, len) -> (name, 0, Z.to_int (evaluate_int len ~var_val))
  | CSlice (creg, i_start, i_end) ->
      let i_start = Z.to_int (evaluate_int i_start ~var_val) in
      let i_end = Z.to_int (evaluate_int i_end ~var_val) in
      let len = i_end - i_start in
      let name, is, _ = evaluate_creg creg ~var_val in
      (name, is + i_start, len)
