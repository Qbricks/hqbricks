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

(** Boolean input variables {m x_i} and path variables {m y_j}. *)

(** Var type. *)
type t =
  | X of int  (** Input variable x with the given index. *)
  | Y of int  (** Path variable y with the given index. *)

val compare : t -> t -> int
(** [compare v1 v2] is 0 if [v1] equals [v2], strictly negative if [v1] is
    smaller than [v2], and stricly positive if [v1] is greater than [v2]. [X] is
    considered greater than [Y], and for variables of the same type the varible
    with the greater index is considered greater. *)

val to_string : t -> string
(** Convert to string. *)
