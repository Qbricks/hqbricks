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

module S = Y_set

type t = S.t

let empty = S.empty
let add i su = S.add i su
let remove i su = S.remove i su
let diff = S.diff
let partition f su = S.partition f su
let add_seq = S.add_seq
let cardinal su = S.cardinal su
let min_elt = S.min_elt
let iter = S.iter
let equal su1 su2 = S.equal su1 su2
let subset = S.subset
let contains_y yi su = S.mem yi su
let to_list su = S.elements su

let to_string su =
  "{"
  ^ String.concat ", "
      (List.map (fun i -> "y" ^ Int.to_string i) (S.elements su))
  ^ "}"
