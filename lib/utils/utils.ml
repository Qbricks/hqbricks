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

(* String *)
module String_map = Map.Make (String)
module Var_name_map = Map.Make (String)
module String_set = Set.Make (String)

let string_equal_ci s1 s2 =
  String.lowercase_ascii s1 = String.lowercase_ascii s2

let rec n_string str n = if n = 0 then "" else str ^ n_string str (n - 1)

(* Int *)
module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)

let create_int_set i_start i_end =
  let rec aux acc i =
    if i >= i_end then acc else aux (Int_set.add i acc) (i + 1)
  in
  aux Int_set.empty i_start

(* Float *)
let float_approx_equal ?(epsilon_abs = 1e-6) ?(epsilon_rel = 1e-6) x y =
  let abs_diff = Float.abs (x -. y) in
  let max = Float.(max (abs x) (abs y)) in
  abs_diff <= epsilon_abs || abs_diff <= max *. epsilon_rel

let float_approx_leq ?(epsilon_abs = 1e-6) ?(epsilon_rel = 1e-6) x y =
  x < y || float_approx_equal ~epsilon_abs ~epsilon_rel x y

let float_approx_geq ?(epsilon_abs = 1e-6) ?(epsilon_rel = 1e-6) x y =
  x > y || float_approx_equal ~epsilon_abs ~epsilon_rel x y

let float_approx_lt ?(epsilon_abs = 1e-6) ?(epsilon_rel = 1e-6) x y =
  x < y && not (float_approx_equal ~epsilon_abs ~epsilon_rel x y)

let float_approx_gt ?(epsilon_abs = 1e-6) ?(epsilon_rel = 1e-6) x y =
  x > y && not (float_approx_equal ~epsilon_abs ~epsilon_rel x y)

(* MPFR Float *)
let mpfr_float_approx_equal ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") x y =
  let abs_diff = Mlmpfr.(sub x y |> abs) in
  let max_val = Mlmpfr.(max (abs x) (abs y)) in
  Mlmpfr.(
    lessequal_p abs_diff epsilon_abs
    || lessequal_p abs_diff (mul max_val epsilon_rel))

let mpfr_float_approx_leq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") x y =
  Mlmpfr.less_p x y || mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y

let mpfr_float_approx_geq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") x y =
  Mlmpfr.greater_p x y || mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y

let mpfr_float_approx_lt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") x y =
  Mlmpfr.less_p x y
  && not (mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y)

let mpfr_float_approx_gt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") x y =
  Mlmpfr.greater_p x y
  && not (mpfr_float_approx_equal ~epsilon_abs ~epsilon_rel x y)

let mpfr_pi = Mlmpfr.(const_pi (get_default_prec ()))

(* File *)
let read_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_json_to_file filename json =
  let out_channel = open_out filename in
  Yojson.Safe.pretty_to_channel out_channel json;
  output_string out_channel "\n";
  close_out out_channel
