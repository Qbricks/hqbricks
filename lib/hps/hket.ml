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

module S = Set.Make (Var_set)

type t = S.t

let empty = S.empty
let const_term_elt = Var_set.empty
let zero = empty
let one = empty |> S.add const_term_elt
let add vs ket = if S.mem vs ket then S.remove vs ket else S.add vs ket
let remove vs ket = S.remove vs ket
let union ket1 ket2 = S.union ket1 ket2
let diff ket1 ket2 = S.diff ket1 ket2
let contains_monomial vs ket = S.mem vs ket
let contains_one ket = contains_monomial const_term_elt ket
let is_zero ket = S.cardinal ket = 0
let is_one ket = S.cardinal ket = 1 && contains_one ket

let neg ket =
  match contains_one ket with
  | true -> ket |> remove const_term_elt
  | false -> ket |> add const_term_elt

let xor ket1 ket2 = union (diff ket1 ket2) (diff ket2 ket1)

let mul ket1 ket2 =
  if is_zero ket1 || is_zero ket2 then zero
  else if is_one ket1 then ket2
  else if is_one ket2 then ket1
  else
    let fold_ket1_fun vs1 acc =
      let fold_ket2_fun vs2 acc =
        xor (empty |> add (Var_set.union vs1 vs2)) acc
      in
      S.fold fold_ket2_fun ket2 acc
    in
    S.fold fold_ket1_fun ket1 empty

let set_y_to_zero yi k = S.filter (fun vs -> not (Var_set.mem (Y yi) vs)) k

let set_y_to_one yi k =
  S.fold (fun vs acc -> add (Var_set.remove (Y yi) vs) acc) k zero

let set_ys_to_zero ys k =
  S.filter (fun vs -> Var_set.disjoint (Var_set.of_y_set ys) vs) k

let set_ys_to_one ys k =
  S.fold (fun vs acc -> add (Var_set.diff vs (Var_set.of_y_set ys)) acc) k zero

let set_y_values yi_zeros yi_ones k =
  k |> set_ys_to_zero yi_zeros |> set_ys_to_one yi_ones

let set_all_y y_zeros ket =
  let fold_fun vs acc =
    match
      Var_set.exists
        (fun v ->
          match v with Y yi when Y_set.mem yi y_zeros -> true | _ -> false)
        vs
    with
    | false ->
        add
          (Var_set.filter
             (fun v -> match v with X _ -> true | Y _ -> false)
             vs)
          acc
    | true -> acc
  in
  S.fold fold_fun ket empty

let change_var yi new_val ket =
  let fold_fun vs acc =
    match Var_set.mem (Y yi) vs with
    | false -> add vs acc
    | true ->
        let vs = Var_set.remove (Y yi) vs in
        xor
          (mul (if vs = Var_set.empty then one else empty |> add vs) new_val)
          acc
  in
  S.fold fold_fun ket empty

let cardinal ket = S.cardinal ket
let choose = S.choose
let for_all f ket = S.for_all f ket
let exists f ket = S.exists f ket
let contains_any_x ket = exists (fun vs -> Var_set.contains_any_x vs) ket
let contains_any_y ket = exists (fun vs -> Var_set.contains_any_y vs) ket
let contains_any_var ket = exists (fun vs -> not @@ Var_set.is_empty vs) ket
let contains_y yi ket = exists (fun vs -> Var_set.mem (Y yi) vs) ket

let contains_y_from_set yset ket =
  exists
    (fun vs ->
      Var_set.exists
        (fun v -> match v with X _ -> false | Y yi -> Y_set.mem yi yset)
        vs)
    ket

let find_first = S.find_first

let find_any_y ket =
  let yi_opt = ref None in
  S.iter
    (fun vs ->
      match !yi_opt with
      | Some _ -> ()
      | None -> (
          match
            Var_set.find_last_opt
              (fun v -> match v with X _ -> false | Y _ -> true)
              vs
          with
          | Some (Y yi) -> yi_opt := Some yi
          | _ -> ()))
    ket;
  !yi_opt

let find_any_xy ket =
  let xy_opt = ref None in
  S.iter
    (fun vs ->
      match !xy_opt with
      | Some _ -> ()
      | None -> xy_opt := Var_set.choose_opt vs)
    ket;
  !xy_opt

let find_all_y ket =
  S.fold
    (fun vs acc ->
      Var_set.fold
        (fun v acc -> match v with X _ -> acc | Y i -> Y_set.add i acc)
        vs acc)
    ket Y_set.empty

let find_all_xy ket = S.fold Var_set.union ket Var_set.empty

let find_unique_y ket =
  if cardinal ket > 2 || contains_any_x ket then Y_set.empty
  else
    let y_set = find_all_y ket in
    if Y_set.cardinal y_set = 1 then y_set else Y_set.empty

let iter = S.iter
let equal ket1 ket2 = S.equal ket1 ket2
let compare vs1 vs2 = S.compare vs1 vs2
let of_var var = empty |> add Var_set.(empty |> add var)

let of_string_ff_aux acc str =
  if str = "" then failwith "Hket.of_string"
  else
    match str.[0] with
    | 'x' | 'X' -> (
        match int_of_string_opt (Str.string_after str 1) with
        | Some xi -> mul acc (of_var @@ X xi)
        | None -> failwith "Hket.of_string")
    | 'y' | 'Y' -> (
        match int_of_string_opt (Str.string_after str 1) with
        | Some yi -> mul acc (of_var @@ Y yi)
        | None -> failwith "Hket.of_string")
    | '0' -> zero
    | '1' -> acc
    | _ -> failwith "Hket.of_string"

let of_string_f_aux acc str =
  let open Str in
  let and_list = split (regexp "[*^]") str in
  xor acc (List.fold_left of_string_ff_aux one and_list)

let of_string str =
  let open Str in
  let str = global_replace (regexp "[ \t]") "" str in
  let xor_list = split (regexp "[+⊕]") str in
  List.fold_left of_string_f_aux zero xor_list

let of_string_opt str = try Some (of_string str) with Failure _ -> None

let to_string ket =
  if is_zero ket then "0"
  else if is_one ket then "1"
  else
    let fold_fun vs acc =
      if Var_set.cardinal vs = 0 then acc ^ "1 ⊕ "
      else
        let str =
          Var_set.fold (fun v acc -> acc ^ Var.to_string v ^ " ^ ") vs acc
        in
        let str = String.sub str 0 (String.length str - 3) in
        str ^ " ⊕ "
    in
    let str = S.fold fold_fun ket "" in
    String.sub str 0 (String.length str - String.length " ⊕ ")
