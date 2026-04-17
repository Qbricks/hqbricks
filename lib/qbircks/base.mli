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

(** This module contains the base types for QbIRcks. *)

type reg_id = string * ir_int
and qreg = QCons of reg_id | QIndex of reg_id * ir_int
and creg = CCons of reg_id | CIndex of reg_id * ir_int

and ir_bool =
  | False
  | True
  | CBitVal of reg_id * ir_int
  | Not of ir_bool
  | And of ir_bool * ir_bool

and ir_int = Z.t

val equal_reg_id : reg_id -> reg_id -> bool
val equal_qreg : qreg -> qreg -> bool
val equal_creg : creg -> creg -> bool
val equal_ir_bool : ir_bool -> ir_bool -> bool
val equal_ir_int : ir_int -> ir_int -> bool
val reg_id_to_string : reg_id -> string
val qreg_to_string : qreg -> string
val creg_to_string : creg -> string
val ir_bool_to_string : ir_bool -> string
val ir_int_to_string : ir_int -> string
val pp_reg_id : Format.formatter -> reg_id -> unit
val pp_qreg : Format.formatter -> qreg -> unit
val pp_creg : Format.formatter -> creg -> unit
val pp_ir_bool : Format.formatter -> ir_bool -> unit
val pp_ir_int : Format.formatter -> ir_int -> unit
val reg_id_to_yojson : reg_id -> Yojson.Safe.t
val reg_id_of_yojson : Yojson.Safe.t -> (reg_id, string) result
val qreg_to_yojson : qreg -> Yojson.Safe.t
val qreg_of_yojson : Yojson.Safe.t -> (qreg, string) result
val creg_to_yojson : creg -> Yojson.Safe.t
val creg_of_yojson : Yojson.Safe.t -> (creg, string) result
val ir_bool_to_yojson : ir_bool -> Yojson.Safe.t
val ir_bool_of_yojson : Yojson.Safe.t -> (ir_bool, string) result
val ir_int_to_yojson : ir_int -> Yojson.Safe.t
val ir_int_of_yojson : Yojson.Safe.t -> (ir_int, string) result
val qreg_of_prog : Prog.Base.qreg -> qreg
val creg_of_prog : Prog.Base.creg -> creg
val bool_of_prog : Prog.Base.pr_bool -> ir_bool
val int_of_prog : Prog.Base.pr_int -> ir_int
val scalar_of_prog : Hps.Scalar.t -> ir_int * ir_int
val qreg_to_prog : qreg -> Prog.Base.qreg
val creg_to_prog : creg -> Prog.Base.creg
val bool_to_prog : ir_bool -> Prog.Base.pr_bool
val int_to_prog : ir_int -> Prog.Base.pr_int
val scalar_to_prog : ir_int * ir_int -> Hps.Scalar.t
val get_qreg_id : qreg -> reg_id
val get_creg_id : creg -> reg_id
val remove_reg_id_list_duplicates : reg_id list -> reg_id list
val bool_find_cregs : ir_bool -> reg_id list
val bool_simp_false_true : ir_bool -> ir_bool
