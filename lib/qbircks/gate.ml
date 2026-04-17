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

open Base

module Param = struct
  type t = Int of ir_int | Angle of ir_int | Scalar of (ir_int * ir_int)
  [@@deriving yojson, eq, show { with_path = false }]

  let to_string = show

  let of_prog = function
    | Prog.Gate.Param.Int i -> Int (int_of_prog i)
    | Prog.Gate.Param.Angle i -> Angle (int_of_prog i)
    | Prog.Gate.Param.Scalar s -> Scalar (scalar_of_prog s)
end

type t = { name : string; qreg_params : Base.qreg list; params : Param.t list }
[@@deriving yojson, eq, show { with_path = false }]

let to_string = show

let of_prog gate =
  {
    name = Prog.Gate.(gate.name);
    qreg_params = List.map qreg_of_prog Prog.Gate.(gate.qreg_params);
    params = List.map Param.of_prog Prog.Gate.(gate.params);
  }

let find_qregs gate =
  remove_reg_id_list_duplicates
  @@ List.fold_right
       (fun qreg acc -> get_qreg_id qreg :: acc)
       gate.qreg_params []

let remove_gate_list_duplicates l =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | gate :: tl when Utils.String_set.mem gate.name seen -> aux seen acc tl
    | gate :: tl -> aux (Utils.String_set.add gate.name seen) (gate :: acc) tl
  in
  aux Utils.String_set.empty [] l
