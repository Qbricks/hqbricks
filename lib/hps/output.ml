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

type t = { qmem : Mem.t; cmem_stack : Mem_stack.t }

let make qmem cmem_stack = { qmem; cmem_stack }
let empty = make Mem.empty Mem_stack.empty

let set_y_to_zero yi o =
  {
    qmem = Mem.set_y_to_zero yi o.qmem;
    cmem_stack = Mem_stack.set_y_to_zero yi o.cmem_stack;
  }

let set_y_to_one yi o =
  {
    qmem = Mem.set_y_to_one yi o.qmem;
    cmem_stack = Mem_stack.set_y_to_one yi o.cmem_stack;
  }

let set_y_values yi_zeros yi_ones o =
  {
    qmem = Mem.set_y_values yi_zeros yi_ones o.qmem;
    cmem_stack = Mem_stack.set_y_values yi_zeros yi_ones o.cmem_stack;
  }

let set_all_y y_zeros o =
  {
    qmem = Mem.set_all_y y_zeros o.qmem;
    cmem_stack = Mem_stack.set_all_y y_zeros o.cmem_stack;
  }

let change_var yi new_val o =
  {
    qmem = Mem.change_var yi new_val o.qmem;
    cmem_stack = Mem_stack.change_var yi new_val o.cmem_stack;
  }

let find_all_y o =
  Y_set.union (Mem.find_all_y o.qmem) (Mem_stack.find_all_y o.cmem_stack)

let find_y_in_regs reg_name_set o =
  Y_set.union
    (Mem.find_y_in_regs reg_name_set o.qmem)
    (Mem_stack.find_y_in_regs reg_name_set o.cmem_stack)

let find_y_in_regs_past reg_name_set o =
  Mem_stack.find_y_in_regs reg_name_set @@ List.drop 1 o.cmem_stack

let find_unique_y o =
  Y_set.union (Mem.find_unique_y o.qmem) (Mem_stack.find_unique_y o.cmem_stack)

let find_reg_names o =
  Reg_name_set.union
    (Mem.find_reg_names o.qmem)
    (Mem_stack.find_reg_names o.cmem_stack)

let equal o1 o2 =
  Mem.equal o1.qmem o2.qmem && Mem_stack.equal o1.cmem_stack o2.cmem_stack

let compare o1 o2 =
  match Mem_stack.compare o1.cmem_stack o2.cmem_stack with
  | 0 -> Mem.compare o1.qmem o2.qmem
  | i -> i

let contains_any_x o =
  Mem.contains_any_x o.qmem || Mem_stack.contains_any_x o.cmem_stack

let contains_any_y o =
  Mem.contains_any_y o.qmem || Mem_stack.contains_any_y o.cmem_stack

let contains_y yi o =
  Mem.contains_y yi o.qmem || Mem_stack.contains_y yi o.cmem_stack

let contains_y_from_set yset o =
  Mem.contains_y_from_set yset o.qmem
  || Mem_stack.contains_y_from_set yset o.cmem_stack

let to_string indent o =
  Mem.qmem_to_string o.qmem ^ "\n" ^ Mem_stack.to_string indent o.cmem_stack
