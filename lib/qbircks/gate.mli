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

(** Gate. *)

(** Gate parameter. *)
module Param : sig
  (** Gate parameter type. *)
  type t =
    | Int of Base.ir_int  (** int parameter. *)
    | Angle of Base.ir_int  (** angle parameter, interpreted as 1/2^i. *)
    | Scalar of (Base.ir_int * Base.ir_int)
        (** scalar parameter, interpreted as i1 / i2. *)

  val equal : t -> t -> bool
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val of_prog : Prog.Gate.Param.t -> t
end

type t = {
  name : string;  (** Name. *)
  qreg_params : Base.qreg list;  (** qreg parameters. *)
  params : Param.t list;  (** parameters. *)
}
(** Gate type. *)

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val of_prog : Prog.Gate.t -> t
val find_qregs : t -> Base.reg_id list
val remove_gate_list_duplicates : t list -> t list
