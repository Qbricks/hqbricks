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

module M = Map.Make (Var_set)

type t = Dyadic1.t M.t

let empty = M.empty
let const_term_elt = Var_set.empty
let singleton = M.singleton
let const dy = singleton const_term_elt dy
let zero = empty
let one_half = zero |> M.add const_term_elt @@ Dyadic1.make Z.one 1

let add vs dy p =
  match M.find_opt vs p with
  | Some dy2 ->
      let d = Dyadic1.add dy dy2 in
      if Dyadic1.den_pow d = 0 then M.remove vs p else M.add vs d p
  | None -> M.add vs dy p

let addl var_list dy p = add (Var_set.of_list var_list) dy p
let remove vs p = M.remove vs p

let diff p1 p2 =
  M.fold
    (fun vs dy acc ->
      match M.find_opt vs acc with
      | Some d when Dyadic1.equal d dy -> remove vs acc
      | _ -> acc)
    p2 p1

let filter f p = M.filter f p
let filter_map = M.filter_map
let partition f p = M.partition f p

let lift_ket ket dy_fact =
  if Dyadic1.equal dy_fact Dyadic1.zero then zero
  else
    let ket_card = Hket.cardinal ket in
    (*
     * Since phase is modulo 1 and every combinations of more than
     * [Dyadic1.min_inv_pow2_leq dy_fact] elements will have a whole number
     * as factor, we can avoid computing those combinations
     *)
    let comb_len_max = Int.min (Dyadic1.min_inv_pow2_leq dy_fact) ket_card in
    (* Array to index ket elements *)
    let ket_array =
      let a = Array.make ket_card Var_set.empty in
      let i = ref 0 in
      Hket.iter
        (fun vs ->
          a.(!i) <- vs;
          incr i)
        ket;
      a
    in
    (* Loop over comb_len and dy_fact * (-2)^(comb_len - 1) *)
    let rec loop comb_len dy_fact acc =
      if comb_len > comb_len_max then acc
      else
        loop (comb_len + 1) (Dyadic1.mul_int (-2) dy_fact)
        @@
        (* Loop to add combinations to the resulting phase *)
        let rec loop_add_comb i elt_count elts acc =
          if i >= ket_card then acc
          else
            (* Loop over index *)
            loop_add_comb (i + 1) elt_count elts
            @@
            let elts = i :: elts and elt_count = elt_count + 1 and i = i + 1 in
            if elt_count >= comb_len then
              (* Add combination to the resulting phase *)
              let vs =
                List.fold_left
                  (fun acc i -> Var_set.union acc ket_array.(i))
                  Var_set.empty elts
              in
              add vs dy_fact acc
            else (* Loop over elements of the combination *)
              loop_add_comb i elt_count elts acc
        in
        loop_add_comb 0 0 [] acc
    in
    loop 1 dy_fact zero

let addp p1 p2 =
  let union_fun _ d1 d2 =
    let d = Dyadic1.add d1 d2 in
    if Dyadic1.den_pow d = 0 then None else Some d
  in
  M.union union_fun p1 p2

let muli i p =
  filter_map
    (fun _ dy ->
      let dy = Dyadic1.(mul_z i dy) in
      if Dyadic1.(equal dy zero) then None else Some dy)
    p

let set_y_to_zero yi p = filter (fun vs _ -> not (Var_set.mem (Y yi) vs)) p

let set_y_to_one yi p =
  M.fold (fun vs dy acc -> add (Var_set.remove (Y yi) vs) dy acc) p zero

let set_ys_to_zero ys p =
  filter (fun vs _ -> Var_set.disjoint (Var_set.of_y_set ys) vs) p

let set_ys_to_one ys p =
  M.fold
    (fun vs dy acc -> add (Var_set.diff vs (Var_set.of_y_set ys)) dy acc)
    p zero

let set_y_values yi_zeros yi_ones p =
  p |> set_ys_to_zero yi_zeros |> set_ys_to_one yi_ones

let set_all_y y_zeros p =
  let fold_fun vs dy acc =
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
          dy acc
    | true -> acc
  in
  M.fold fold_fun p empty

let change_var yi new_val p =
  let fold_fun vs dy acc =
    match Var_set.mem (Y yi) vs with
    | false -> add vs dy acc
    | true ->
        let vs = Var_set.remove (Y yi) vs in
        addp
          (M.fold
             (fun v d acc -> add (Var_set.union vs v) d acc)
             (lift_ket new_val dy) zero)
          acc
  in
  M.fold fold_fun p empty

let cardinal = M.cardinal

let count_y ?(y_count_map = Y_map.empty) p =
  M.fold (fun vs _ acc -> Var_set.count_y ~y_count_map:acc vs) p y_count_map

let iter = M.iter
let find_opt vs p = M.find_opt vs p

let find_all_y p =
  M.fold
    (fun vs _ acc ->
      Var_set.fold
        (fun v acc -> match v with X _ -> acc | Y i -> Y_set.add i acc)
        vs acc)
    p Y_set.empty

let find_all_y_not_one_half p =
  M.fold
    (fun vs dy acc ->
      if Dyadic1.reduce dy = Dyadic1.make Z.one 1 then acc
      else
        Var_set.fold
          (fun v acc -> match v with X _ -> acc | Y i -> Y_set.add i acc)
          vs acc)
    p Y_set.empty

let fold f p init = M.fold f p init
let equal mem1 mem2 = M.equal Dyadic1.equal mem1 mem2
let for_all = M.for_all
let is_empty p = M.is_empty p
let mem vs p = M.mem vs p
let exists f p = M.exists f p
let eq_zero = is_empty
let eq_one_half mem = equal mem one_half
let is_const p = M.cardinal p = 0 || (M.cardinal p = 1 && mem const_term_elt p)
let contains_any_x p = exists (fun vs _ -> Var_set.contains_any_x vs) p
let contains_any_y p = exists (fun vs _ -> Var_set.contains_any_y vs) p
let contains_y yi p = exists (fun vs _ -> Var_set.mem (Y yi) vs) p

let contains_y_from_set yset p =
  exists
    (fun vs _ ->
      Var_set.exists
        (fun v -> match v with X _ -> false | Y yi -> Y_set.mem yi yset)
        vs)
    p

let contains_y_not_one_half yi p =
  exists
    (fun vs dy ->
      Var_set.mem (Y yi) vs && Dyadic1.reduce dy <> Dyadic1.make Z.one 1)
    p

let to_string p =
  if M.cardinal p = 0 then "0"
  else
    let fold_fun vs d acc =
      if Var_set.cardinal vs = 0 then
        Printf.sprintf "%s%s + " acc (Dyadic1.to_string d)
      else
        let str =
          Var_set.fold
            (fun v acc -> acc ^ Var.to_string v ^ " * ")
            vs
            (acc ^ Dyadic1.to_string d ^ " * ")
        in
        let str = String.sub str 0 (String.length str - 3) in
        str ^ " + "
    in
    let str = fold fold_fun p "" in
    String.sub str 0 (String.length str - 3)
