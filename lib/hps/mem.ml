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

type reg_id = string * int

module Reg_id = struct
  type t = reg_id

  let compare (name1, i1) (name2, i2) =
    let name_c = compare name1 name2 in
    if name_c = 0 then Int.compare i1 i2 else name_c
end

module M = Map.Make (Reg_id)

type t = Hket.t M.t

let empty = M.empty
let add reg_id ket mem = M.add reg_id ket mem
let remove reg_id mem = M.remove reg_id mem
let partition f mem = M.partition f mem

let add_vec (name, i_start) ket_list mem =
  let _, m =
    List.fold_left
      (fun (i, acc) ket -> (i + 1, add (name, i) ket acc))
      (i_start, mem) ket_list
  in
  m

let add_vec_const (name, i_start) len ket mem =
  let rec loop i acc =
    if i < len then loop (i + 1) (add (name, i) ket acc) else acc
  in
  loop i_start mem

let add_vec_zero reg_id_start len mem =
  add_vec_const reg_id_start len Hket.zero mem

let add_vec_one reg_id_start len mem =
  add_vec_const reg_id_start len Hket.one mem

let add_vec_x (name, i_start) len x_start mem =
  let rec loop i acc =
    if i < len then
      loop (i + 1) (add (name, i) (Hket.of_var @@ X (x_start + i)) acc)
    else acc
  in
  loop i_start mem

let add_int (name, i_start) len n mem =
  let rec loop i n acc =
    if i >= len then acc
    else
      loop (i + 1)
        Z.(n asr 1)
        (add
           (name, i_start + i)
           (if Z.(n land ~$1 = ~$1) then Hket.one else Hket.zero)
           acc)
  in
  loop 0 n mem

let set_y_to_zero yi m = M.map (fun ket -> Hket.set_y_to_zero yi ket) m
let set_y_to_one yi m = M.map (fun ket -> Hket.set_y_to_one yi ket) m

let set_y_values yi_zeros yi_ones m =
  M.map (fun ket -> Hket.set_y_values yi_zeros yi_ones ket) m

let set_all_y y_zeros mem = M.map (fun ket -> Hket.set_all_y y_zeros ket) mem

let change_var yi new_val mem =
  M.map (fun ket -> Hket.change_var yi new_val ket) mem

let cardinal = M.cardinal
let find reg_id mem = M.find reg_id mem

let find_all_y mem =
  M.fold
    (fun _ ket acc -> Y_set.union (Hket.find_all_y ket) acc)
    mem Y_set.empty

let find_y_in_regs reg_name_set mem =
  M.fold
    (fun (qreg_name, _) ket acc ->
      if Reg_name_set.mem qreg_name reg_name_set then
        Y_set.union (Hket.find_all_y ket) acc
      else acc)
    mem Y_set.empty

let find_unique_y mem =
  M.fold
    (fun _ ket acc -> Y_set.union acc (Hket.find_unique_y ket))
    mem Y_set.empty

let find_reg_names mem =
  M.fold
    (fun (qreg_name, _) _ acc -> Reg_name_set.add qreg_name acc)
    mem Reg_name_set.empty

let iter = M.iter
let fold f mem acc = M.fold f mem acc
let equal mem1 mem2 = M.equal Hket.equal mem1 mem2
let compare mem1 mem2 = M.compare Hket.compare mem1 mem2
let is_empty mem = M.is_empty mem
let exists f mem = M.exists f mem
let contains_reg = M.mem
let contains_any_x mem = M.exists (fun _ ket -> Hket.contains_any_x ket) mem
let contains_any_y mem = M.exists (fun _ ket -> Hket.contains_any_y ket) mem
let contains_y yi mem = M.exists (fun _ ket -> Hket.contains_y yi ket) mem

let contains_y_from_set yset mem =
  M.exists (fun _ ket -> Hket.contains_y_from_set yset ket) mem

let reg_to_int name len mem =
  let rec loop i acc =
    if i < 0 then acc
    else
      let to_add =
        match find (name, i) mem with
        | ket when Hket.is_zero ket -> Z.zero
        | ket when Hket.is_one ket -> Z.one
        | _ -> failwith "Cannot read int from mem reg containing variables"
      in
      loop (i - 1) Z.((acc lsl 1) + to_add)
  in
  loop (len - 1) Z.zero

let reg_to_int_big_endian name len mem =
  let rec loop i acc =
    if i >= len then acc
    else
      let to_add =
        match find (name, i) mem with
        | ket when Hket.is_zero ket -> Z.zero
        | ket when Hket.is_one ket -> Z.one
        | _ -> failwith "Cannot read int from mem reg containing variables"
      in
      loop (i + 1) Z.((acc lsl 1) + to_add)
  in
  loop 0 Z.zero

let reg_as_int_to_phase ?(k_fact = Hket.one) dy_fact reg_name reg_len m =
  if Hket.is_zero k_fact then Phase.zero
  else
    let rec loop i dy_fact acc =
      if Dyadic1.(equal dy_fact zero) || i >= reg_len then acc
      else
        loop (i + 1)
          (Dyadic1.mul_int 2 dy_fact)
          Phase.(
            addp (lift_ket (Hket.mul (find (reg_name, i) m) k_fact) dy_fact) acc)
    in
    loop 0 dy_fact Phase.zero

let qmem_to_string qm =
  if M.cardinal qm = 0 then "_"
  else
    let str =
      M.fold
        (fun (qreg_name, i) ket acc ->
          acc
          ^ Printf.sprintf "|%s>_%s[%s] " (Hket.to_string ket) qreg_name
              (Int.to_string i))
        qm ""
    in
    String.sub str 0 (String.length str - 1)

let cmem_to_string cm =
  if M.cardinal cm = 0 then "_"
  else
    let str =
      M.fold
        (fun (creg_name, i) ket acc ->
          acc
          ^ Printf.sprintf "[%s]_%s[%s] " (Hket.to_string ket) creg_name
              (Int.to_string i))
        cm ""
    in
    String.sub str 0 (String.length str - 1)
