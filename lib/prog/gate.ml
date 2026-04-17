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

module Param = struct
  type t = Int of Base.pr_int | Angle of Base.pr_int | Scalar of Hps.Scalar.t
  [@@deriving eq, show { with_path = false }]

  let substitute_ivar var_name i_sub = function
    | Int i -> Int (Base.pr_int_substitute_ivar var_name i_sub i)
    | Angle i -> Angle (Base.pr_int_substitute_ivar var_name i_sub i)
    | Scalar _ as p -> p

  let to_string = function
    | Int i | Angle i -> Base.pr_int_to_string i
    | Scalar s -> Hps.Scalar.to_string s
end

type t = {
  name : string;
  qreg_params : Base.qreg list;
  params : Param.t list;
  with_params : Base.qreg list -> Param.t list -> t; [@equal fun _ _ -> true]
  inverse : unit -> t; [@equal fun _ _ -> true]
  evaluate :
    ?k:Hps.Hket.t ->
    ?var_val:int Utils.Var_name_map.t ->
    ?metrics:Metrics.t ->
    Hps.t ->
    Hps.t;
      [@equal fun _ _ -> true]
}
[@@deriving eq, show { with_path = false }]

let substitute_ivar var_name i_sub gate =
  let qreg_params =
    List.map (Base.qreg_substitute_ivar var_name i_sub) gate.qreg_params
  in
  let params = List.map (Param.substitute_ivar var_name i_sub) gate.params in
  gate.with_params qreg_params params

let to_string gate =
  let sep = ", " in
  let qreg_params_str =
    match List.length gate.qreg_params with
    | 0 -> ""
    | 1 -> Base.qreg_to_string @@ List.hd gate.qreg_params
    | _ ->
        "("
        ^ Str.string_after
            (List.fold_left
               (fun acc qreg -> acc ^ sep ^ Base.qreg_to_string qreg)
               "" gate.qreg_params)
            (String.length sep)
        ^ ")"
  in
  let params_str =
    match List.length gate.params with
    | 0 -> ""
    | 1 -> Param.to_string @@ List.hd gate.params
    | _ ->
        "("
        ^ Str.string_after
            (List.fold_left
               (fun acc p -> acc ^ sep ^ Param.to_string p)
               "" gate.params)
            (String.length sep)
        ^ ")"
  in
  (if String.length qreg_params_str = 0 then ""
   else qreg_params_str ^ " |> " ^ gate.name)
  ^ if String.length params_str = 0 then "" else "(" ^ params_str ^ ")"
