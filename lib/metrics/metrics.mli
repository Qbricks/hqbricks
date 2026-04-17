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

(** Metrics for programs and verification.

    This module provides optional metrics to track the number of gates,
    measurements and rewrites during program evaluation and verification. The
    [?metrics] optional argument can be ignored if metrics are not needed.

    Metrics are created using {!create} and passed to functions that update
    metrics in {!Evaluator}, {!Assertion}, and {!Concretization} via their
    [?metrics] optional argument. The metrics record is updated in place,
    reflecting the program evaluation and verification process. *)

type t = {
  mutable gate_count : int;  (** Number of gates. *)
  mutable meas_count : int;  (** Number of measurements. *)
  mutable rewrite_count : int;  (** Number of rewrite rules applied. *)
}
(** Metrics type. *)

val create : unit -> t
(** Create empty metrics. *)

val add_gates : t -> int -> unit
(** [add_gates metrics n] adds [n] to [metrics.gate_count]. *)

val add_measures : t -> int -> unit
(** [add_measures metrics n] adds [n] to [metrics.meas_count]. *)

val add_rewrites : t -> int -> unit
(** [add_rewrites metrics n] adds [n] to [metrics.rewite_count]. *)

val add_inplace : t -> t -> unit
(** [add_inplace metrics1 metrics2] adds the fields of [metrics2] into
    [metrics1] in place. *)

val add_gates_opt : t option -> int -> unit
(** [add_gates_opt metrics_opt n] adds [n] to [metrics.gate_count] if present,
    does nothing if [None]. *)

val add_measures_opt : t option -> int -> unit
(** [add_measures_opt metrics_opt n] adds [n] to [metrics.meas_count] if
    present, does nothing if [None]. *)

val add_rewrites_opt : t option -> int -> unit
(** [add_rewrites_opt metrics_opt n] adds [n] to [metrics.rewite_count] if
    present, does nothing if [None]. *)

val add_inplace_opt : t option -> t option -> unit
(** [add_inplace_opt metrics1_opt metrics2_opt] adds the fields of
    [metrics2_opt] into [metrics1_opt] in place if both are [Some], does nothing
    otherwise. *)

val to_string : t -> string
(** Convert to string. *)
