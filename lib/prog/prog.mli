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

(** HQbricks programs. *)

(** {1:types Types} *)

module Base = Base
module Gate = Gate

(** Program.

    A subset of these constructors are also quantum program constructors, a
    program containing only those constructors is a quantum program:
    - [PVar] if it contains a quantum program.
    - [Skip].
    - [InitQReg].
    - [Seq] if both prog fields are quantum programs.
    - [For] if prog is a quantum program.
    - [IfElse] if both prog fields are quantum programs.
    - [Gate]. *)
type t =
  | PVar of string
      (** Program variable with the given name.
          - The name must not be empty. *)
  | Skip  (** Skip. *)
  | InitQReg of Base.qreg  (** Init the given quantum register. *)
  | Seq of t * t  (** Sequence of programs. *)
  | For of string * Base.pr_int * Base.pr_int * t
      (** [For (s, i1, i2, prog)] is for [ivar s] between [i2] and [i2] do
          [prog]. *)
  | IfElse of Base.pr_bool * t * t
      (** [IfElse (b, prog1, prog2)] is if [b] then [prog1] else [prog2]. *)
  | Meas of Base.qreg * Base.creg
      (** [Meas (qreg, creg)] measures [qreg] in [creg]. *)
  | Gate of Gate.t  (** Gate application. *)
  | SetCReg of Base.creg * Base.pr_int
      (** [SetCReg (creg, i)] sets [creg] to [i]. *)

(** {1:syntactic_sugar Syntactic Sugar} *)

val pvar : string -> t
(** Variable with the given name. *)

val skip : t
(** Skip. *)

val init_qreg : Base.qreg -> t
(** Initialize [qreg]. *)

val seq : t -> t -> t
(** Sequence. *)

val if_else : Base.pr_bool -> t -> t -> t
(** [if_else cond prog1 prog2] is if [cond] then [prog1] else [prog2]. *)

val if_b_then_p : Base.pr_bool -> t -> t
(** [if_b_then_p cond prog] is if [cond] then [prog] else [skip]. *)

val p_for : string -> Base.pr_int -> Base.pr_int -> t -> t
(** [p_for s i1 i2 prog] is for [ivar s] between [i1] and [i2] do [prog] done.
*)

val for_ctrl_prog : Base.qreg * string -> t -> t
(** [for_ctrl_prog (ctrl_qreg, s) prog] is for [ivar s] between [O] and
    [qreg_len qreg] do [prog[ivar s]] controlled by [ctrl_qreg[ivar s]]. *)

val c_for_ctrl_prog : Base.creg * string -> t -> t
(** [for_ctrl_prog (ctrl_creg, s) prog] is for [ivar s] between [O] and
    [creg_len creg] do [prog[ivar s]] controlled by [ctrl_creg[ivar s]]. *)

val meas : Base.qreg -> Base.creg -> t
(** [meas qreg creg] measures [qreg] in [creg]. *)

val apply_gate : 'a -> ('a -> Gate.t) -> t
(** [apply gate qreg] applies [gate] to [qreg]. *)

val set_creg : Base.creg -> Base.pr_int -> t
(** [set_creg creg i] sets [creg] to the value of [i]. *)

val ( -- ) : t -> t -> t
(** {!seq} operator. *)

val ( >> ) : Base.qreg * string -> t -> t
(** {!for_ctrl_prog} operator. *)

(** Module for the classical version of the >> operator. *)
module C : sig
  val ( >> ) : Base.creg * string -> t -> t
  (** {!c_for_ctrl_prog} operator. *)
end

val ( |> ) : 'a -> ('a -> Gate.t) -> t
(** {!apply_gate} operator. *)

val ( => ) : Base.pr_bool -> t -> t
(** {!if_b_then_p} operator. *)

val ( -@ ) : Base.qreg -> Base.creg -> t
(** {!meas} operator. *)

(** {1:operations Operations} *)

val substitute_ivar : string -> Base.pr_int -> t -> t
(** [substitute_ivar var_name i_sub prog] returns a new prog with the pr_int
    variable [var_name] substituted by [i_sub] in [prog]. *)

val inverse_unitary : t -> t option
(** Inverse the given unitary prog. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:conversions Conversions} *)

val to_string : t -> string
(** Convert to string. *)

(** {1:printing Printing} *)

val pp : Format.formatter -> t -> unit
(** Print the given prog on the specified formatter. *)

val print : t -> unit
(** Print the given prog. *)

(** {1:base_include Base Include} *)

include module type of Base
(** Include {!Base} so it is available through [open Prog]. *)
