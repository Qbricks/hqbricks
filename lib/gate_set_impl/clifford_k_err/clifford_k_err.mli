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

(** Clifford-k-err gate set.

    Clifford_k gate set with the addition of an identity error gate IE. *)

include module type of Clifford_k
(** Clifford_k gates. *)

val ie : Hps.Scalar.t -> Prog.Base.qreg -> Prog.Gate.t
(** Identity error gate, noisy Identity gate with the given probability of
    success. *)

val ir_gate_func_map : (Qbircks.Gate.t -> Prog.t) Qbircks.Ast.Gate_name_map.t
(** Map from gate name (string) to a function to convert {!Qbircks.Gate.t} to
    {!Prog.Gate.t}, it can be used as parameter of the function
    {!Qbircks.Ast.to_prog} to convert ir to prog using the Clifford-k-err gate
    set. *)
