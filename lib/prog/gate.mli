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

(** Gate parameters. *)
module Param : sig
  (** Gate parameters type. *)
  type t =
    | Int of Base.pr_int  (** pr_int parameter. *)
    | Angle of Base.pr_int  (** angle parameter, interpreted as 1/2^i. *)
    | Scalar of Hps.Scalar.t  (** Scalar parameter. *)

  val equal : t -> t -> bool
  (** Equality test. *)

  val substitute_ivar : string -> Base.pr_int -> t -> t
  (** [substitute_ivar var_name i_sub param] returns a new param with the pr_int
      variable [var_name] substituted by [i_sub] in [param]. *)

  val to_string : t -> string
  (** Convert to string. *)

  val pp : Format.formatter -> t -> unit
  (** Print the given param on the specified formatter. *)
end

type t = {
  name : string;  (** Name. *)
  qreg_params : Base.qreg list;  (** qreg parameters. *)
  params : Param.t list;  (** parameters. *)
  with_params : Base.qreg list -> Param.t list -> t;
      (** Function to create a new gate with updated parameters. *)
  inverse : unit -> t;
      (** Function to create a gate that is the inverse of the gate. *)
  evaluate :
    ?k:Hps.Hket.t ->
    ?var_val:int Utils.Var_name_map.t ->
    ?metrics:Metrics.t ->
    Hps.t ->
    Hps.t;
      (** [evaluate ~k ~var_val ~metrics hps] evaluates the gate into an HPS
          using [hps] as input.

          [k] is a factor set to one by default that can be modified by if else
          controls.

          [var_val] is a map from [string] to [int] that can be used to give
          values to {!Prog.Base.pr_int} variables (for example to evaluate for
          loops).

          If [~metrics] is provided, it is updated in place to reflect the
          computation (only gate counters may be incremented); it has no effect
          on the result (see {!Metrics}). *)
}
(** Gate type. *)

val equal : t -> t -> bool
(** Equality test. *)

val substitute_ivar : string -> Base.pr_int -> t -> t
(** [substitute_ivar var_name i_sub gate] returns a new gate with the pr_int
    variable [var_name] substituted by [i_sub] in [gate]. *)

val to_string : t -> string
(** Convert to string. *)

val pp : Format.formatter -> t -> unit
(** Print the given gate on the specified formatter. *)
