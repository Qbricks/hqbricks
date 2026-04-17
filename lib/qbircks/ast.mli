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

(** QbIRcks AST. *)

type t =
  | Skip
  | InitQReg of Base.qreg
  | Seq of t * t
  | If of Base.ir_bool * t
  | Meas of Base.qreg * Base.creg
  | Gate of Gate.t
  | SetCReg of Base.creg * Base.ir_int

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val remove_skip_seq : t -> t
val write_to_file : string -> t -> unit
val read_from_file : string -> t
val of_prog : Prog.t -> t

module Gate_name_map : sig
  include Map.S with type key = string
end

val to_prog : (Gate.t -> Prog.t) Gate_name_map.t -> t -> Prog.t
val find_regs : t -> Base.reg_id list * Base.reg_id list
