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

type t =
  | SVar of string
  | SBool of Hket.t
      [@printer fun fmt ket -> fprintf fmt "(SBool(%s))" (Hket.to_string ket)]
  | SFrac of Z.t * Z.t
      [@printer
        fun fmt (i1, i2) ->
          fprintf fmt "(SFrac (Z.(~$%s), Z.(~$%s)))" (Z.to_string i1)
            (Z.to_string i2)]
  | SFloat of Mlmpfr.mpfr_float
      [@printer
        fun fmt f ->
          fprintf fmt "(SFloat(%s))" Mlmpfr.(get_formatted_str ~size:11 f)]
  | Sqrt of t
  | Cos of angle
  | Sin of angle
  | SAdd of t * t
  | SMul of t * t
  | SPowB of t * Hket.t
      [@printer
        fun fmt (s, ket) ->
          fprintf fmt "(SPowB (%s, %s))" (show s) (Hket.to_string ket)]
  | SNeg of t
  | SInv of t
[@@deriving show { with_path = false }]

and angle =
  | AFrac of Z.t * Z.t
      [@printer
        fun fmt (i1, i2) ->
          fprintf fmt "(AFrac (Z.(~$%s), Z.(~$%s)))" (Z.to_string i1)
            (Z.to_string i2)]
  | AFloat of Mlmpfr.mpfr_float
      [@printer
        fun fmt f ->
          fprintf fmt "(SFloat(%s))" Mlmpfr.(get_formatted_str ~size:11 f)]
  | ArcCos of t
  | ArcSin of t
  | AMulB of Hket.t * angle
      [@printer
        fun fmt (ket, a) ->
          fprintf fmt "(AMulB (%s, %s))" (Hket.to_string ket) (show_angle a)]
  | AMulI of Z.t * angle
      [@printer
        fun fmt (i, a) ->
          fprintf fmt "(AMulI (Z.(~$%s), %s))" (Z.to_string i) (show_angle a)]
  | AAdd of angle * angle
  | ANeg of angle
[@@deriving show { with_path = false }]

let make_frac i1 i2 = SFrac (i1, i2)
let make_int i = SFrac (i, Z.one)
let zero = SFrac (Z.zero, Z.one)
let one = SFrac (Z.one, Z.one)
let sqrt_half = Sqrt (SFrac (Z.(~$1), Z.(~$2)))
let sqrt_half_pow_n n = Sqrt (SFrac (Z.(~$1), Z.(~$2 ** n)))

let rec simp s =
  match s with
  | SVar _ as s -> s
  | SBool _ as s -> s
  | SFrac (i1, i2) ->
      let q = Q.make i1 i2 in
      SFrac (q.num, q.den)
  | SFloat f when Mlmpfr.integer_p f ->
      let frac, exp = Mlmpfr.(get_str f) in
      let i = Z.of_string (Str.first_chars frac (Z.to_int (Z.of_string exp))) in
      SFrac (i, Z.one)
  | SFloat f -> SFloat f
  | Sqrt s -> (
      match simp s with
      | SFrac (n, d) when Z.perfect_square n && Z.perfect_square d ->
          let n = Z.sqrt n in
          let d = Z.sqrt d in
          simp @@ SFrac (n, d)
      | SMul (SFrac (n, d), s) when Z.perfect_square n && Z.perfect_square d ->
          let n = Z.sqrt n in
          let d = Z.sqrt d in
          simp @@ SMul (SFrac (n, d), Sqrt s)
      | SFloat f -> SFloat (Mlmpfr.sqrt f)
      | _ as s -> Sqrt s)
  | Cos a -> (
      match simp_angle a with
      | ArcCos s -> s
      | AFloat f -> SFloat (Mlmpfr.cos f)
      | a -> Cos a)
  | Sin a -> (
      match simp_angle a with
      | ArcSin s -> s
      | AFloat f -> SFloat (Mlmpfr.sin f)
      | a -> Sin a)
  | SAdd (s1, s2) -> (
      match (simp s1, simp s2) with
      | SBool b, s when Hket.is_zero b -> s
      | s, SBool b when Hket.is_zero b -> s
      | SFrac (i1, _), s when Z.(equal i1 Z.zero) -> s
      | s, SFrac (i1, _) when Z.(equal i1 Z.zero) -> s
      | SFrac (i1, i2), SFrac (i3, i4) ->
          let q = Q.add (Q.make i1 i2) (Q.make i3 i4) in
          SFrac (q.num, q.den)
      | SFloat f1, SFloat f2 -> SFloat Mlmpfr.(add f1 f2)
      | s1, s2 -> SAdd (s1, s2))
  | SMul (s1, s2) -> (
      match (simp s1, simp s2) with
      | SBool b, s when Hket.is_one b -> s
      | s, SBool b when Hket.is_one b -> s
      | SFrac (sn, sd), s when sn = Z.one && sd = Z.one -> s
      | s, SFrac (sn, sd) when sn = Z.one && sd = Z.one -> s
      | SBool b, _ when Hket.is_zero b -> SBool Hket.zero
      | _, SBool b when Hket.is_zero b -> SBool Hket.zero
      | Sqrt s1, Sqrt s2 when s1 = s2 -> s1
      | Sqrt s1, Sqrt s2 -> Sqrt (SMul (s1, s2))
      | Sqrt s1, SMul (Sqrt s2, s)
      | Sqrt s1, SMul (s, Sqrt s2)
      | SMul (Sqrt s1, s), Sqrt s2
      | SMul (s, Sqrt s1), Sqrt s2 ->
          if s1 = s2 then simp @@ SMul (s, s1)
          else simp @@ SMul (s, Sqrt (SMul (s1, s2)))
      | SFrac (i1, i2), SFrac (i3, i4) ->
          let q = Q.mul (Q.make i1 i2) (Q.make i3 i4) in
          SFrac (q.num, q.den)
      | SFrac (i1, i2), SMul (SFrac (i3, i4), s)
      | SFrac (i1, i2), SMul (s, SFrac (i3, i4))
      | SMul (SFrac (i1, i2), s), SFrac (i3, i4)
      | SMul (s, SFrac (i1, i2)), SFrac (i3, i4) ->
          let q = Q.mul (Q.make i1 i2) (Q.make i3 i4) in
          simp @@ SMul (SFrac (q.num, q.den), s)
      | SFloat f1, SFloat f2 -> SFloat Mlmpfr.(mul f1 f2)
      | s1, s2 -> SMul (s1, s2))
  | SPowB (s, b) -> (
      match (simp s, b) with
      | s, b when Hket.is_one b -> s
      | _, b when Hket.is_zero b -> one
      | SFrac (n, d), _ when n = d && n <> Z.zero -> one
      | s, b -> SPowB (s, b))
  | SNeg s -> (
      match simp s with
      | SFrac (i1, i2) ->
          let i1 = Z.neg i1 in
          SFrac (i1, i2)
      | SFloat f -> SFloat Mlmpfr.(neg f)
      | s -> SNeg s)
  | SInv s -> (
      match simp s with
      | SBool b when Hket.is_one b -> SBool Hket.one
      | SFrac (i1, i2) -> SFrac (i2, i1)
      | SFloat f -> SFloat Mlmpfr.(int_div 1 f)
      | s -> SInv s)

and simp_angle a =
  match a with
  | AFrac (i1, i2) ->
      let q = Q.make i1 i2 in
      AFrac (q.num, q.den)
  | AFloat f when Mlmpfr.integer_p f ->
      let frac, exp = Mlmpfr.(get_str f) in
      let i = Z.of_string (Str.first_chars frac (Z.to_int (Z.of_string exp))) in
      AFrac (i, Z.one)
  | AFloat f -> AFloat f
  | ArcCos s -> (
      match simp s with
      | Cos a -> a
      | SFloat f -> AFloat Mlmpfr.(acos f)
      | s -> ArcCos s)
  | ArcSin s -> (
      match simp s with
      | Sin a -> a
      | SFloat f -> AFloat Mlmpfr.(asin f)
      | s -> ArcSin s)
  | AMulB (b, a) -> (
      match (b, simp_angle a) with
      | b, a when Hket.is_one b -> a
      | b, _ when Hket.is_zero b -> AFrac (Z.zero, Z.one)
      | _, AFrac (n, _) when n = Z.zero -> AFrac (Z.zero, Z.one)
      | b, a -> AMulB (b, a))
  | AMulI (i, a) -> (
      match (i, simp_angle a) with
      | i, a when i = Z.one -> a
      | i, _ when i = Z.zero -> AFrac (Z.zero, Z.one)
      | i, AFrac (n, d) when n = Z.zero && d = Z.zero -> AFrac (i, Z.one)
      | _, AFrac (n, _) when n = Z.zero -> AFrac (Z.zero, Z.one)
      | i, AFloat f -> AFloat Mlmpfr.(mul f (make_from_str (Z.to_string i)))
      | i, a -> AMulI (i, a))
  | AAdd (a1, a2) -> (
      match (simp_angle a1, simp_angle a2) with
      | AFrac (i1, i2), AFrac (i3, i4) ->
          let q = Q.add (Q.make i1 i2) (Q.make i3 i4) in
          AFrac (q.num, q.den)
      | AFloat f1, AFloat f2 -> AFloat Mlmpfr.(add f1 f2)
      | a1, a2 -> AAdd (a1, a2))
  | ANeg a -> (
      match simp_angle a with
      | AFrac (i1, i2) ->
          let i1 = Z.neg i1 in
          AFrac (i1, i2)
      | AFloat f -> AFloat Mlmpfr.(neg f)
      | a -> ANeg a)

let rec square = function
  | (SVar _ | SBool _ | Cos _ | Sin _ | SAdd _ | SPowB _) as s -> SMul (s, s)
  | SFrac (s1, s2) -> simp (SFrac (Z.mul s1 s1, Z.mul s2 s2))
  | SFloat f -> SFloat Mlmpfr.(pow_int f 2)
  | Sqrt s -> s
  | SMul (s1, s2) -> simp (SMul (square s1, square s2))
  | SNeg s -> square s
  | SInv s -> simp (SInv (square s))

let complement s = simp (SAdd (one, SNeg s))

let rec set_y_to_zero_aux yi = function
  | (SVar _ | SFrac _ | SFloat _) as s -> s
  | SBool b -> SBool (Hket.set_y_to_zero yi b)
  | Sqrt s -> Sqrt (set_y_to_zero_aux yi s)
  | Cos a -> Cos (angle_set_y_to_zero_aux yi a)
  | Sin a -> Sin (angle_set_y_to_zero_aux yi a)
  | SAdd (s1, s2) -> SAdd (set_y_to_zero_aux yi s1, set_y_to_zero_aux yi s2)
  | SMul (s1, s2) -> SMul (set_y_to_zero_aux yi s1, set_y_to_zero_aux yi s2)
  | SPowB (s, b) -> SPowB (set_y_to_zero_aux yi s, Hket.set_y_to_zero yi b)
  | SNeg s -> SNeg (set_y_to_zero_aux yi s)
  | SInv s -> SInv (set_y_to_zero_aux yi s)

and angle_set_y_to_zero_aux yi = function
  | (AFrac _ | AFloat _) as a -> a
  | ArcCos s -> ArcCos (set_y_to_zero_aux yi s)
  | ArcSin s -> ArcSin (set_y_to_zero_aux yi s)
  | AMulB (b, a) -> AMulB (Hket.set_y_to_zero yi b, angle_set_y_to_zero_aux yi a)
  | AMulI (i, a) -> AMulI (i, angle_set_y_to_zero_aux yi a)
  | AAdd (a1, a2) ->
      AAdd (angle_set_y_to_zero_aux yi a1, angle_set_y_to_zero_aux yi a2)
  | ANeg a -> ANeg (angle_set_y_to_zero_aux yi a)

let rec set_y_to_one_aux yi = function
  | (SVar _ | SFrac _ | SFloat _) as s -> s
  | SBool b -> SBool (Hket.set_y_to_one yi b)
  | Sqrt s -> Sqrt (set_y_to_one_aux yi s)
  | Cos a -> Cos (angle_set_y_to_one_aux yi a)
  | Sin a -> Sin (angle_set_y_to_one_aux yi a)
  | SAdd (s1, s2) -> SAdd (set_y_to_one_aux yi s1, set_y_to_one_aux yi s2)
  | SMul (s1, s2) -> SMul (set_y_to_one_aux yi s1, set_y_to_one_aux yi s2)
  | SPowB (s, b) -> SPowB (set_y_to_one_aux yi s, Hket.set_y_to_one yi b)
  | SNeg s -> SNeg (set_y_to_one_aux yi s)
  | SInv s -> SInv (set_y_to_one_aux yi s)

and angle_set_y_to_one_aux yi = function
  | (AFrac _ | AFloat _) as a -> a
  | ArcCos s -> ArcCos (set_y_to_one_aux yi s)
  | ArcSin s -> ArcSin (set_y_to_one_aux yi s)
  | AMulB (b, a) -> AMulB (Hket.set_y_to_one yi b, angle_set_y_to_one_aux yi a)
  | AMulI (i, a) -> AMulI (i, angle_set_y_to_one_aux yi a)
  | AAdd (a1, a2) ->
      AAdd (angle_set_y_to_one_aux yi a1, angle_set_y_to_one_aux yi a2)
  | ANeg a -> ANeg (angle_set_y_to_one_aux yi a)

let rec set_y_values_aux yi_zeros yi_ones = function
  | (SVar _ | SFrac _ | SFloat _) as s -> s
  | SBool b -> SBool (Hket.set_y_values yi_zeros yi_ones b)
  | Sqrt s -> Sqrt (set_y_values_aux yi_zeros yi_ones s)
  | Cos a -> Cos (angle_set_y_values_aux yi_zeros yi_ones a)
  | Sin a -> Sin (angle_set_y_values_aux yi_zeros yi_ones a)
  | SAdd (s1, s2) ->
      SAdd
        ( set_y_values_aux yi_zeros yi_ones s1,
          set_y_values_aux yi_zeros yi_ones s2 )
  | SMul (s1, s2) ->
      SMul
        ( set_y_values_aux yi_zeros yi_ones s1,
          set_y_values_aux yi_zeros yi_ones s2 )
  | SPowB (s, b) ->
      SPowB
        ( set_y_values_aux yi_zeros yi_ones s,
          Hket.set_y_values yi_zeros yi_ones b )
  | SNeg s -> SNeg (set_y_values_aux yi_zeros yi_ones s)
  | SInv s -> SInv (set_y_values_aux yi_zeros yi_ones s)

and angle_set_y_values_aux yi_zeros yi_ones = function
  | (AFrac _ | AFloat _) as a -> a
  | ArcCos s -> ArcCos (set_y_values_aux yi_zeros yi_ones s)
  | ArcSin s -> ArcSin (set_y_values_aux yi_zeros yi_ones s)
  | AMulB (b, a) ->
      AMulB
        ( Hket.set_y_values yi_zeros yi_ones b,
          angle_set_y_values_aux yi_zeros yi_ones a )
  | AMulI (i, a) -> AMulI (i, angle_set_y_values_aux yi_zeros yi_ones a)
  | AAdd (a1, a2) ->
      AAdd
        ( angle_set_y_values_aux yi_zeros yi_ones a1,
          angle_set_y_values_aux yi_zeros yi_ones a2 )
  | ANeg a -> ANeg (angle_set_y_values_aux yi_zeros yi_ones a)

let rec set_all_y_aux y_zeros s =
  match s with
  | SVar _ as s -> s
  | SBool b -> SBool (Hket.set_all_y y_zeros b)
  | SFrac _ as s -> s
  | SFloat _ as s -> s
  | Sqrt s -> Sqrt (set_all_y_aux y_zeros s)
  | Cos a -> Cos (angle_set_all_y_aux y_zeros a)
  | Sin a -> Sin (angle_set_all_y_aux y_zeros a)
  | SAdd (s1, s2) -> SAdd (set_all_y_aux y_zeros s1, set_all_y_aux y_zeros s2)
  | SMul (s1, s2) -> SMul (set_all_y_aux y_zeros s1, set_all_y_aux y_zeros s2)
  | SPowB (s, b) -> SPowB (set_all_y_aux y_zeros s, Hket.set_all_y y_zeros b)
  | SNeg s -> SNeg (set_all_y_aux y_zeros s)
  | SInv s -> SInv (set_all_y_aux y_zeros s)

and angle_set_all_y_aux y_zeros a =
  match a with
  | (AFrac _ | AFloat _) as a -> a
  | ArcCos s -> ArcCos (set_all_y_aux y_zeros s)
  | ArcSin s -> ArcSin (set_all_y_aux y_zeros s)
  | AMulB (b, a) ->
      AMulB (Hket.set_all_y y_zeros b, angle_set_all_y_aux y_zeros a)
  | AMulI (i, a) -> AMulI (i, angle_set_all_y_aux y_zeros a)
  | AAdd (a1, a2) ->
      AAdd (angle_set_all_y_aux y_zeros a1, angle_set_all_y_aux y_zeros a2)
  | ANeg a -> ANeg (angle_set_all_y_aux y_zeros a)

let set_y_to_zero yi s = simp (set_y_to_zero_aux yi s)
let set_y_to_one yi s = simp (set_y_to_one_aux yi s)
let set_y_values yi_zeros yi_ones s = simp (set_y_values_aux yi_zeros yi_ones s)
let set_all_y y_zeros s = simp @@ set_all_y_aux y_zeros s
let angle_set_y_to_zero yi s = simp_angle (angle_set_y_to_zero_aux yi s)
let angle_set_y_to_one yi s = simp_angle (angle_set_y_to_one_aux yi s)

let angle_set_y_values yi_zeros yi_ones s =
  simp_angle (angle_set_y_values_aux yi_zeros yi_ones s)

let angle_set_all_y y_zeros a = simp_angle @@ angle_set_all_y_aux y_zeros a

let rec change_var_aux yi new_val s =
  match s with
  | SVar _ as s -> s
  | SBool b -> SBool (Hket.change_var yi new_val b)
  | SFrac _ as s -> s
  | SFloat _ as s -> s
  | Sqrt s -> Sqrt (change_var_aux yi new_val s)
  | Cos a -> Cos (change_var_angle_aux yi new_val a)
  | Sin a -> Sin (change_var_angle_aux yi new_val a)
  | SAdd (s1, s2) ->
      SAdd (change_var_aux yi new_val s1, change_var_aux yi new_val s2)
  | SMul (s1, s2) ->
      SMul (change_var_aux yi new_val s1, change_var_aux yi new_val s2)
  | SPowB (s, b) ->
      SPowB (change_var_aux yi new_val s, Hket.change_var yi new_val b)
  | SNeg s -> SNeg (change_var_aux yi new_val s)
  | SInv s -> SInv (change_var_aux yi new_val s)

and change_var_angle_aux yi new_val a =
  match a with
  | (AFrac _ | AFloat _) as a -> a
  | ArcCos s -> ArcCos (change_var_aux yi new_val s)
  | ArcSin s -> ArcSin (change_var_aux yi new_val s)
  | AMulB (b, a) ->
      AMulB (Hket.change_var yi new_val b, change_var_angle_aux yi new_val a)
  | AMulI (i, a) -> AMulI (i, change_var_angle_aux yi new_val a)
  | AAdd (a1, a2) ->
      AAdd
        (change_var_angle_aux yi new_val a1, change_var_angle_aux yi new_val a2)
  | ANeg a -> ANeg (change_var_angle_aux yi new_val a)

let change_var yi new_val s = simp @@ change_var_aux yi new_val s

let change_var_angle yi new_val a =
  simp_angle @@ change_var_angle_aux yi new_val a

let rec find_any_y s =
  match s with
  | SVar _ -> None
  | SBool b -> Hket.find_any_y b
  | SFrac _ -> None
  | SFloat _ -> None
  | Sqrt s -> find_any_y s
  | Cos a -> angle_find_any_y a
  | Sin a -> angle_find_any_y a
  | SAdd (s1, s2) -> (
      match find_any_y s1 with Some yi -> Some yi | None -> find_any_y s2)
  | SMul (s1, s2) -> (
      match find_any_y s1 with Some yi -> Some yi | None -> find_any_y s2)
  | SPowB (s, b) -> (
      match find_any_y s with Some yi -> Some yi | None -> Hket.find_any_y b)
  | SNeg s -> find_any_y s
  | SInv s -> find_any_y s

and angle_find_any_y a =
  match a with
  | AFrac _ | AFloat _ -> None
  | ArcCos s -> find_any_y s
  | ArcSin s -> find_any_y s
  | AMulB (b, a) -> (
      match Hket.find_any_y b with
      | Some yi -> Some yi
      | None -> angle_find_any_y a)
  | AMulI (_, a) -> angle_find_any_y a
  | AAdd (a1, a2) -> (
      match angle_find_any_y a1 with
      | Some yi -> Some yi
      | None -> angle_find_any_y a2)
  | ANeg a -> angle_find_any_y a

let rec find_any_xy s =
  match s with
  | SVar _ -> None
  | SBool b -> Hket.find_any_xy b
  | SFrac _ -> None
  | SFloat _ -> None
  | Sqrt s -> find_any_xy s
  | Cos a -> angle_find_any_xy a
  | Sin a -> angle_find_any_xy a
  | SAdd (s1, s2) -> (
      match find_any_xy s1 with Some yi -> Some yi | None -> find_any_xy s2)
  | SMul (s1, s2) -> (
      match find_any_xy s1 with Some yi -> Some yi | None -> find_any_xy s2)
  | SPowB (s, b) -> (
      match find_any_xy s with Some yi -> Some yi | None -> Hket.find_any_xy b)
  | SNeg s -> find_any_xy s
  | SInv s -> find_any_xy s

and angle_find_any_xy a =
  match a with
  | AFrac _ | AFloat _ -> None
  | ArcCos s -> find_any_xy s
  | ArcSin s -> find_any_xy s
  | AMulB (b, a) -> (
      match Hket.find_any_xy b with
      | Some yi -> Some yi
      | None -> angle_find_any_xy a)
  | AMulI (_, a) -> angle_find_any_xy a
  | AAdd (a1, a2) -> (
      match angle_find_any_xy a1 with
      | Some yi -> Some yi
      | None -> angle_find_any_xy a2)
  | ANeg a -> angle_find_any_xy a

let rec find_all_y s =
  match s with
  | SVar _ -> Y_set.empty
  | SBool b -> Hket.find_all_y b
  | SFrac _ -> Y_set.empty
  | SFloat _ -> Y_set.empty
  | Sqrt s -> find_all_y s
  | Cos a -> angle_find_all_y a
  | Sin a -> angle_find_all_y a
  | SAdd (s1, s2) -> Y_set.union (find_all_y s1) (find_all_y s2)
  | SMul (s1, s2) -> Y_set.union (find_all_y s1) (find_all_y s2)
  | SPowB (s, b) -> Y_set.union (find_all_y s) (Hket.find_all_y b)
  | SNeg s -> find_all_y s
  | SInv s -> find_all_y s

and angle_find_all_y a =
  match a with
  | AFrac _ | AFloat _ -> Y_set.empty
  | ArcCos s -> find_all_y s
  | ArcSin s -> find_all_y s
  | AMulB (b, a) -> Y_set.union (Hket.find_all_y b) (angle_find_all_y a)
  | AMulI (_, a) -> angle_find_all_y a
  | AAdd (a1, a2) -> Y_set.union (angle_find_all_y a1) (angle_find_all_y a2)
  | ANeg a -> angle_find_all_y a

let rec find_all_xy s =
  match s with
  | SVar _ -> Var_set.empty
  | SBool b -> Hket.find_all_xy b
  | SFrac _ -> Var_set.empty
  | SFloat _ -> Var_set.empty
  | Sqrt s -> find_all_xy s
  | Cos a -> angle_find_all_xy a
  | Sin a -> angle_find_all_xy a
  | SAdd (s1, s2) -> Var_set.union (find_all_xy s1) (find_all_xy s2)
  | SMul (s1, s2) -> Var_set.union (find_all_xy s1) (find_all_xy s2)
  | SPowB (s, b) -> Var_set.union (find_all_xy s) (Hket.find_all_xy b)
  | SNeg s -> find_all_xy s
  | SInv s -> find_all_xy s

and angle_find_all_xy a =
  match a with
  | AFrac _ | AFloat _ -> Var_set.empty
  | ArcCos s -> find_all_xy s
  | ArcSin s -> find_all_xy s
  | AMulB (b, a) -> Var_set.union (Hket.find_all_xy b) (angle_find_all_xy a)
  | AMulI (_, a) -> angle_find_all_xy a
  | AAdd (a1, a2) -> Var_set.union (angle_find_all_xy a1) (angle_find_all_xy a2)
  | ANeg a -> angle_find_all_xy a

let rec contains_any_x s =
  match s with
  | SVar _ -> false
  | SBool b -> Hket.contains_any_x b
  | SFrac _ -> false
  | SFloat _ -> false
  | Sqrt s -> contains_any_x s
  | Cos a -> angle_contains_any_x a
  | Sin a -> angle_contains_any_x a
  | SAdd (s1, s2) -> contains_any_x s1 || contains_any_x s2
  | SMul (s1, s2) -> contains_any_x s1 || contains_any_x s2
  | SPowB (s, b) -> contains_any_x s || Hket.contains_any_x b
  | SNeg s -> contains_any_x s
  | SInv s -> contains_any_x s

and angle_contains_any_x a =
  match a with
  | AFrac _ | AFloat _ -> false
  | ArcCos s -> contains_any_x s
  | ArcSin s -> contains_any_x s
  | AMulB (b, a) -> Hket.contains_any_x b || angle_contains_any_x a
  | AMulI (_, a) -> angle_contains_any_x a
  | AAdd (a1, a2) -> angle_contains_any_x a1 || angle_contains_any_x a2
  | ANeg a -> angle_contains_any_x a

let rec contains_any_y s =
  match s with
  | SVar _ -> false
  | SBool b -> Hket.contains_any_y b
  | SFrac _ -> false
  | SFloat _ -> false
  | Sqrt s -> contains_any_y s
  | Cos a -> angle_contains_any_y a
  | Sin a -> angle_contains_any_y a
  | SAdd (s1, s2) -> contains_any_y s1 || contains_any_y s2
  | SMul (s1, s2) -> contains_any_y s1 || contains_any_y s2
  | SPowB (s, b) -> contains_any_y s || Hket.contains_any_y b
  | SNeg s -> contains_any_y s
  | SInv s -> contains_any_y s

and angle_contains_any_y a =
  match a with
  | AFrac _ | AFloat _ -> false
  | ArcCos s -> contains_any_y s
  | ArcSin s -> contains_any_y s
  | AMulB (b, a) -> Hket.contains_any_y b || angle_contains_any_y a
  | AMulI (_, a) -> angle_contains_any_y a
  | AAdd (a1, a2) -> angle_contains_any_y a1 || angle_contains_any_y a2
  | ANeg a -> angle_contains_any_y a

let rec contains_any_var = function
  | SVar _ -> true
  | SBool b -> Hket.contains_any_var b
  | SFrac _ -> false
  | SFloat _ -> false
  | Sqrt s -> contains_any_var s
  | Cos a -> angle_contains_any_var a
  | Sin a -> angle_contains_any_var a
  | SAdd (s1, s2) -> contains_any_var s1 || contains_any_var s2
  | SMul (s1, s2) -> contains_any_var s1 || contains_any_var s2
  | SPowB (s, b) -> contains_any_var s || Hket.contains_any_var b
  | SNeg s -> contains_any_var s
  | SInv s -> contains_any_var s

and angle_contains_any_var a =
  match a with
  | AFrac _ | AFloat _ -> false
  | ArcCos s -> contains_any_var s
  | ArcSin s -> contains_any_var s
  | AMulB (b, a) -> Hket.contains_any_var b || angle_contains_any_var a
  | AMulI (_, a) -> angle_contains_any_var a
  | AAdd (a1, a2) -> angle_contains_any_var a1 || angle_contains_any_var a2
  | ANeg a -> angle_contains_any_var a

let rec contains_y yi s =
  match s with
  | SVar _ -> false
  | SBool b -> Hket.contains_y yi b
  | SFrac _ -> false
  | SFloat _ -> false
  | Sqrt s -> contains_y yi s
  | Cos a -> angle_contains_y yi a
  | Sin a -> angle_contains_y yi a
  | SAdd (s1, s2) -> contains_y yi s1 || contains_y yi s2
  | SMul (s1, s2) -> contains_y yi s1 || contains_y yi s2
  | SPowB (s, b) -> contains_y yi s || Hket.contains_y yi b
  | SNeg s -> contains_y yi s
  | SInv s -> contains_y yi s

and angle_contains_y yi a =
  match a with
  | AFrac _ | AFloat _ -> false
  | ArcCos s -> contains_y yi s
  | ArcSin s -> contains_y yi s
  | AMulB (b, a) -> Hket.contains_y yi b || angle_contains_y yi a
  | AMulI (_, a) -> angle_contains_y yi a
  | AAdd (a1, a2) -> angle_contains_y yi a1 || angle_contains_y yi a2
  | ANeg a -> angle_contains_y yi a

let rec contains_y_from_set yset s =
  match s with
  | SVar _ -> false
  | SBool b -> Hket.contains_y_from_set yset b
  | SFrac _ -> false
  | SFloat _ -> false
  | Sqrt s -> contains_y_from_set yset s
  | Cos a -> angle_contains_y_from_set yset a
  | Sin a -> angle_contains_y_from_set yset a
  | SAdd (s1, s2) -> contains_y_from_set yset s1 || contains_y_from_set yset s2
  | SMul (s1, s2) -> contains_y_from_set yset s1 || contains_y_from_set yset s2
  | SPowB (s, b) ->
      contains_y_from_set yset s || Hket.contains_y_from_set yset b
  | SNeg s -> contains_y_from_set yset s
  | SInv s -> contains_y_from_set yset s

and angle_contains_y_from_set yset a =
  match a with
  | AFrac _ | AFloat _ -> false
  | ArcCos s -> contains_y_from_set yset s
  | ArcSin s -> contains_y_from_set yset s
  | AMulB (b, a) ->
      Hket.contains_y_from_set yset b || angle_contains_y_from_set yset a
  | AMulI (_, a) -> angle_contains_y_from_set yset a
  | AAdd (a1, a2) ->
      angle_contains_y_from_set yset a1 || angle_contains_y_from_set yset a2
  | ANeg a -> angle_contains_y_from_set yset a

let to_q s =
  match simp s with
  | SBool b when Hket.is_zero b -> Some Q.zero
  | SBool b when Hket.is_one b -> Some Q.one
  | SFrac (i1, i2) -> Some (Q.make i1 i2)
  | SVar _ | SBool _ | SFloat _ | Sqrt _ | Cos _ | Sin _ | SAdd _ | SMul _
  | SPowB _ | SNeg _ | SInv _ ->
      None

let rec to_float_aux = function
  | SVar _ -> failwith "SVar cannot be converted to float"
  | SBool b when Hket.is_zero b -> Mlmpfr.make_from_int 0
  | SBool b when Hket.is_one b -> Mlmpfr.make_from_int 1
  | SBool _ -> failwith "bool containing variables cannot be converted to float"
  | SFrac (i1, i2) ->
      Mlmpfr.(
        div (make_from_str (Z.to_string i1)) (make_from_str (Z.to_string i2)))
  | SFloat f -> f
  | Sqrt s -> Mlmpfr.(sqrt @@ to_float_aux s)
  | Cos a -> Mlmpfr.(cospi @@ mul_int (angle_to_float_aux a) 2)
  | Sin a -> Mlmpfr.(sinpi @@ mul_int (angle_to_float_aux a) 2)
  | SAdd (s1, s2) -> Mlmpfr.add (to_float_aux s1) (to_float_aux s2)
  | SMul (s1, s2) -> Mlmpfr.mul (to_float_aux s1) (to_float_aux s2)
  | SPowB (_, b) when Hket.is_zero b -> Mlmpfr.make_from_int 1
  | SPowB (s, b) when Hket.is_one b -> to_float_aux s
  | SPowB _ -> failwith "bool containing variables cannot be converted to float"
  | SNeg s -> Mlmpfr.neg (to_float_aux s)
  | SInv s -> Mlmpfr.(int_div 1 (to_float_aux s))

and angle_to_float_aux = function
  | AFrac (i1, i2) ->
      Mlmpfr.(
        div (make_from_str (Z.to_string i1)) (make_from_str (Z.to_string i2)))
  | AFloat f -> f
  | ArcCos s -> Mlmpfr.acos @@ to_float_aux s
  | ArcSin s -> Mlmpfr.asin @@ to_float_aux s
  | AMulB (b, _) when Hket.is_zero b -> Mlmpfr.make_from_int 0
  | AMulB (b, a) when Hket.is_one b -> angle_to_float_aux a
  | AMulB _ -> failwith "bool containing variables cannot be converted to float"
  | AMulI (i, a) ->
      Mlmpfr.(mul (angle_to_float_aux a) (make_from_str (Z.to_string i)))
  | AAdd (a1, a2) ->
      Mlmpfr.(add (angle_to_float_aux a1) (angle_to_float_aux a2))
  | ANeg a -> Mlmpfr.neg (angle_to_float_aux a)

let to_float s = if contains_any_var s then None else Some (to_float_aux s)

let angle_to_float a =
  if angle_contains_any_var a then None else Some (angle_to_float_aux a)

let rec equal_aux s1 s2 =
  match (simp s1, simp s2) with
  | SVar str1, SVar str2 -> str1 = str2
  | SBool b1, SBool b2 -> Hket.equal b1 b2
  | SFrac (i1, i2), SFrac (i3, i4) -> Z.equal i1 i3 && Z.equal i2 i4
  | SFloat f1, SFloat f2 -> f1 = f2
  | Sqrt s1, Sqrt s2 -> equal_aux s1 s2
  | Cos a1, Cos a2 -> angle_equal_aux a1 a2
  | Sin a1, Sin a2 -> angle_equal_aux a1 a2
  | SAdd (s1, s2), SAdd (s3, s4) -> equal_aux s1 s3 && equal_aux s2 s4
  | SMul (s1, s2), SMul (s3, s4) -> equal_aux s1 s3 && equal_aux s2 s4
  | SPowB (s1, b1), SPowB (s2, b2) -> equal_aux s1 s2 && Hket.equal b1 b2
  | SNeg s1, SNeg s2 -> equal_aux s1 s2
  | SInv s1, SInv s2 -> equal_aux s1 s2
  | _, _ -> false

and angle_equal_aux a1 a2 =
  match (a1, a2) with
  | AFrac (i1, i2), AFrac (i3, i4) -> Z.equal i1 i3 && Z.equal i2 i4
  | AFloat f1, AFloat f2 -> f1 = f2
  | ArcCos s1, ArcCos s2 -> equal_aux s1 s2
  | ArcSin s1, ArcSin s2 -> equal_aux s1 s2
  | AMulB (b1, a1), AMulB (b2, a2) -> Hket.equal b1 b2 && angle_equal_aux a1 a2
  | AMulI (i1, a1), AMulI (i2, a2) -> Z.equal i1 i2 && angle_equal_aux a1 a2
  | AAdd (a1, a2), AAdd (a3, a4) ->
      angle_equal_aux a1 a3 && angle_equal_aux a2 a4
  | ANeg a1, ANeg a2 -> angle_equal_aux a1 a2
  | _, _ -> false

let equal s1 s2 = equal_aux (simp s1) (simp s2)
let angle_equal a1 a2 = angle_equal_aux (simp_angle a1) (simp_angle a2)

let eq_zero s =
  match s with
  | SFrac (z, _) when Z.(equal z zero) -> true
  | SBool b when Hket.is_zero b -> true
  | SFloat f when Mlmpfr.(equal_p f (make_from_int 0)) -> true
  | _ -> false

let ( let* ) = Option.bind

let qleq s1 s2 =
  let* q1 = to_q s1 in
  let* q2 = to_q s2 in
  Some (Q.leq q1 q2)

let qgeq s1 s2 =
  let* q1 = to_q s1 in
  let* q2 = to_q s2 in
  Some (Q.geq q1 q2)

let qlt s1 s2 =
  let* q1 = to_q s1 in
  let* q2 = to_q s2 in
  Some (Q.lt q1 q2)

let qgt s1 s2 =
  let* q1 = to_q s1 in
  let* q2 = to_q s2 in
  Some (Q.gt q1 q2)

let fleq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  let* f1 = to_float s1 in
  let* f2 = to_float s2 in
  Some (Utils.mpfr_float_approx_leq ~epsilon_abs ~epsilon_rel f1 f2)

let fgeq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  let* f1 = to_float s1 in
  let* f2 = to_float s2 in
  Some (Utils.mpfr_float_approx_geq ~epsilon_abs ~epsilon_rel f1 f2)

let flt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  let* f1 = to_float s1 in
  let* f2 = to_float s2 in
  Some (Utils.mpfr_float_approx_lt ~epsilon_abs ~epsilon_rel f1 f2)

let fgt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  let* f1 = to_float s1 in
  let* f2 = to_float s2 in
  Some (Utils.mpfr_float_approx_gt ~epsilon_abs ~epsilon_rel f1 f2)

let leq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  match qleq s1 s2 with
  | Some b -> Some b
  | None -> fleq ~epsilon_abs ~epsilon_rel s1 s2

let geq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  match qgeq s1 s2 with
  | Some b -> Some b
  | None -> fgeq ~epsilon_abs ~epsilon_rel s1 s2

let lt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  match qlt s1 s2 with
  | Some b -> Some b
  | None -> flt ~epsilon_abs ~epsilon_rel s1 s2

let gt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") s1 s2 =
  match qgt s1 s2 with
  | Some b -> Some b
  | None -> fgt ~epsilon_abs ~epsilon_rel s1 s2

let of_dyadic1 dy = Dyadic1.(SFrac (num dy, Z.(~$1 lsl den_pow dy)))
let angle_of_dyadic1 dy = Dyadic1.(AFrac (num dy, Z.(~$1 lsl den_pow dy)))
let rec repeat_sqrt n s = if n <= 0 then s else Sqrt (repeat_sqrt (n - 1) s)

let rec to_product_list_aux sqrt_count = function
  | Sqrt s -> to_product_list_aux (sqrt_count + 1) s
  | SMul (s1, s2) ->
      to_product_list_aux sqrt_count s1 @ to_product_list_aux sqrt_count s2
  | _ as s -> [ repeat_sqrt sqrt_count s ]

let to_product_list s = to_product_list_aux 0 s

(*
 * Type used for parenthesis in to_string:
 * - OpNone: all surrounding parenthesis
 * - OpAdd: surrounding parenthesis except for Add
 * - OpMul: surrounding parenthesis except for Mul
 * - OpFun: no surrounding parenthesis
 *)
type op = OpNone | OpAdd | OpMul | OpFun

let rec to_string_aux prev_op s =
  match s with
  | SVar str -> str
  | SBool b -> Hket.to_string b
  | SFrac (i1, i2) -> (
      if i2 = Z.one then Z.to_string i1
      else
        let str = Z.to_string i1 ^ " / " ^ Z.to_string i2 in
        match prev_op with OpFun -> str | _ -> "(" ^ str ^ ")")
  | SFloat f -> Mlmpfr.get_formatted_str ~size:11 f
  | Sqrt s -> "sqrt(" ^ to_string_aux OpFun s ^ ")"
  | Cos a -> "cos(" ^ angle_to_string_aux OpFun a ^ ")"
  | Sin a -> "sin(" ^ angle_to_string_aux OpFun a ^ ")"
  | SAdd (s1, SNeg s2) -> (
      let str = to_string_aux OpAdd s1 ^ " - " ^ to_string_aux OpNone s2 in
      match prev_op with OpAdd | OpFun -> str | _ -> "(" ^ str ^ ")")
  | SAdd (s1, s2) -> (
      let str = to_string_aux OpAdd s1 ^ " + " ^ to_string_aux OpAdd s2 in
      match prev_op with OpAdd | OpFun -> str | _ -> "(" ^ str ^ ")")
  | SMul (s1, s2) -> (
      let str = to_string_aux OpMul s1 ^ " * " ^ to_string_aux OpMul s2 in
      match prev_op with OpMul | OpFun -> str | _ -> "(" ^ str ^ ")")
  | SPowB (s, b) -> (
      let str = to_string_aux OpNone s ^ " ^ (" ^ Hket.to_string b ^ ")" in
      match prev_op with OpFun -> str | _ -> "(" ^ str ^ ")")
  | SNeg s -> "-" ^ to_string_aux OpNone s
  | SInv s -> (
      let str = "1 / " ^ to_string_aux OpNone s in
      match prev_op with OpFun -> str | _ -> "(" ^ str ^ ")")

and angle_to_string_aux prev_op a =
  match a with
  | AFrac (i1, i2) -> (
      let str = "2pi * " ^ Z.to_string i1 ^ " / " ^ Z.to_string i2 in
      match prev_op with OpFun -> str | _ -> "(" ^ str ^ ")")
  | AFloat f -> Mlmpfr.get_formatted_str ~size:11 f
  | ArcCos s -> "arccos(" ^ to_string_aux OpFun s ^ ")"
  | ArcSin s -> "arcsin(" ^ to_string_aux OpFun s ^ ")"
  | AMulB (b, a) -> (
      let str = Hket.to_string b ^ " * " ^ angle_to_string_aux OpMul a in
      match prev_op with OpMul | OpFun -> str | _ -> "(" ^ str ^ ")")
  | AMulI (i, a) -> (
      let str = Z.to_string i ^ " * " ^ angle_to_string_aux OpMul a in
      match prev_op with OpMul | OpFun -> str | _ -> "(" ^ str ^ ")")
  | AAdd (a1, a2) -> (
      let str =
        angle_to_string_aux OpAdd a1 ^ " + " ^ angle_to_string_aux OpAdd a2
      in
      match prev_op with OpAdd | OpFun -> str | _ -> "(" ^ str ^ ")")
  | ANeg a -> "-" ^ angle_to_string_aux OpNone a

let to_string s = to_string_aux OpFun s
let angle_to_string a = angle_to_string_aux OpFun a
let to_raw_string s = show s
let angle_to_raw_string a = show_angle a
