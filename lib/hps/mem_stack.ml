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

type t = Mem.t list

let empty = [ Mem.empty ]

let rec remove_leading_voids = function
  | [] -> []
  | hd :: tl when Mem.is_empty hd -> remove_leading_voids tl
  | l -> l

let remove_trailing_voids ms =
  List.rev @@ [ Mem.empty ] @ remove_leading_voids @@ List.rev ms

let set_y_to_zero yi ms = List.map (fun m -> Mem.set_y_to_zero yi m) ms
let set_y_to_one yi ms = List.map (fun m -> Mem.set_y_to_one yi m) ms

let set_y_values yi_zeros yi_ones ms =
  List.map (fun m -> Mem.set_y_values yi_zeros yi_ones m) ms

let set_all_y y_zeros ms = List.map (fun mem -> Mem.set_all_y y_zeros mem) ms

let change_var yi new_val ms =
  List.map (fun mem -> Mem.change_var yi new_val mem) ms

let find_all_y ms =
  List.fold_left
    (fun acc m -> Y_set.union acc (Mem.find_all_y m))
    Y_set.empty ms

let find_y_in_regs reg_name_set ms =
  List.fold_left
    (fun acc m -> Y_set.union acc (Mem.find_y_in_regs reg_name_set m))
    Y_set.empty ms

let find_unique_y ms =
  List.fold_left
    (fun acc m -> Y_set.union acc (Mem.find_unique_y m))
    Y_set.empty ms

let find_reg_names ms =
  List.fold_left
    (fun acc m -> Reg_name_set.union acc (Mem.find_reg_names m))
    Reg_name_set.empty ms

let equal ms1 ms2 =
  List.equal Mem.equal (remove_trailing_voids ms1) (remove_trailing_voids ms2)

let compare ms1 ms2 =
  List.compare Mem.compare
    (remove_trailing_voids ms1)
    (remove_trailing_voids ms2)

let for_all f ms = List.for_all f ms
let contains_any_x ms = List.exists (fun m -> Mem.contains_any_x m) ms
let contains_any_y ms = List.exists (fun m -> Mem.contains_any_y m) ms
let contains_y yi ms = List.exists (fun m -> Mem.contains_y yi m) ms

let contains_y_from_set yset ms =
  List.exists (fun m -> Mem.contains_y_from_set yset m) ms

let to_string indent cms =
  let elem_indent = indent ^ "  " in
  indent ^ "[\n" ^ elem_indent
  ^ String.concat (",\n" ^ elem_indent) (List.map Mem.cmem_to_string cms)
  ^ "\n" ^ indent ^ "]"
