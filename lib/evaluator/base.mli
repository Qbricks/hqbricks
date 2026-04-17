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

(** Evaluation functions for {!Prog.Base}. *)

val evaluate_bool :
  ?var_val:int Utils.Var_name_map.t -> Prog.Base.pr_bool -> Hps.t -> Hps.Hket.t
(** [evaluate_bool ~var_val b hps] evaluates [b] into an HPS ket, [hps] is
    needed to evaluate {!Prog.Base.pr_bool} with qbit or cbit values. *)

val evaluate_int : ?var_val:int Utils.Var_name_map.t -> Prog.Base.pr_int -> Z.t
(** [evaluate_int ~var_val i] evaluates [i] into a Zarith big integer. *)

val evaluate_qreg :
  ?var_val:int Utils.Var_name_map.t -> Prog.Base.qreg -> string * int * int
(** [evaluate_qreg ~var_val qreg] evaluates [qreg] into
    [(name, start_i, end_i)], respectively the name, the start index, and the
    end index of [qreg]. *)

val evaluate_creg :
  ?var_val:int Utils.Var_name_map.t -> Prog.Base.creg -> string * int * int
(** [evaluate_creg ~var_val creg] evaluates [creg] into
    [(name, start_i, end_i)], respectively the name, the start index, and the
    end index of [creg]. *)
