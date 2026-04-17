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

(** Clifford-k gate set. *)

val h : Prog.Base.qreg -> Prog.Gate.t
(** Hadamard gate. *)

val x : Prog.Base.qreg -> Prog.Gate.t
(** Pauli X gate. *)

val z : Prog.Base.qreg -> Prog.Gate.t
(** Pauli Z gate. *)

val rz : Prog.Base.pr_int -> Prog.Base.qreg -> Prog.Gate.t
(** Z Rotation gate. *)

val swap : Prog.Base.qreg -> Prog.Base.qreg -> Prog.Gate.t
(** SWAP gate. *)

val ir_gate_func_map : (Qbircks.Gate.t -> Prog.t) Qbircks.Ast.Gate_name_map.t
(** Map from gate name (string) to a function to convert {!Qbircks.Gate.t} to
    {!Prog.Gate.t}, it can be used as parameter of the function
    {!Qbircks.Ast.to_prog} to convert ir to prog using the Clifford-k gate set.
*)
