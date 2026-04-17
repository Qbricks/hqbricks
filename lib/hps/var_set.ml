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

include Set.Make (Var)

let partition_xset_yset vs =
  fold
    (fun v (xset, yset) ->
      match v with
      | X xi -> (X_set.add xi xset, yset)
      | Y yi -> (xset, Y_set.add yi yset))
    vs (X_set.empty, Y_set.empty)

let count_y ?(y_count_map = Y_map.empty) vs =
  fold
    (fun v acc ->
      match v with
      | X _ -> acc
      | Y yi ->
          Y_map.update yi
            (fun opt_count ->
              match opt_count with
              | Some count -> Some (count + 1)
              | None -> Some 1)
            acc)
    vs y_count_map

let contains_any_x vs =
  exists (fun v -> match v with X _ -> true | Y _ -> false) vs

let contains_any_y vs =
  exists (fun v -> match v with X _ -> false | Y _ -> true) vs

let of_x_set xs = Y_set.fold (fun xi acc -> add (X xi) acc) xs empty
let of_y_set ys = Y_set.fold (fun yi acc -> add (Y yi) acc) ys empty

let to_string vs =
  let elts_str = elements vs |> List.map Var.to_string |> String.concat ", " in
  "{ " ^ elts_str ^ " }"
