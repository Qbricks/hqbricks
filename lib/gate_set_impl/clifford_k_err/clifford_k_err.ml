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

open Prog.Gate

(* Clifford_k gates *)
include Clifford_k

(* Identity error gate *)
let evaluate_ie p qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    if not @@ Hket.is_one k then failwith "Cannot put an Ie gate in an IfElse"
    else
      let start_cmem = List.hd hps.output.cmem_stack in
      let rec loop i hps cmem =
        if i < qreg_len then
          let yvar = Hket.of_var (Y hps.y_count) in
          let qmem_key = (qreg_name, i_start + i) in
          (* update support *)
          let support = Support.add hps.y_count hps.support in
          (* update scalar *)
          let scalar =
            Scalar.(
              SMul
                ( hps.scalar,
                  Sqrt
                    (SMul
                       ( SPowB (p, yvar),
                         SPowB
                           (SAdd (SFrac (Z.one, Z.one), SNeg p), Hket.neg yvar)
                       )) ))
          in
          (* update output.qmem *)
          let qmem_val = Hket.xor (Mem.find qmem_key hps.output.qmem) yvar in
          let qmem = Mem.add qmem_key qmem_val hps.output.qmem in
          let output = Output.{ hps.output with qmem } in
          (* update cmem *)
          let cmem = Mem.add ("w" ^ Int.to_string hps.y_count, 0) yvar cmem in
          (* update y-count *)
          let y_count = hps.y_count + 1 in
          loop (i + 1) { hps with support; scalar; output; y_count } cmem
        else (hps, cmem)
      in
      let hps, cmem = loop 0 hps start_cmem in
      (* update cmem_stack *)
      let output =
        Output.{ hps.output with cmem_stack = cmem :: hps.output.cmem_stack }
      in
      let hps = { hps with output } in
      (* simp scalar *)
      let hps = simp_scalar hps in
      Metrics.add_gates_opt metrics qreg_len;
      hps

let rec ie p qreg =
  {
    name = "IE";
    qreg_params = [ qreg ];
    params = [ Param.Scalar p ];
    with_params = ie_with_params;
    inverse = (fun () -> failwith "inverse not implemented for Ie");
    evaluate = evaluate_ie p qreg;
  }

and ie_with_params qreg_params params =
  match (qreg_params, params) with
  | qreg :: _, Param.Scalar p :: _ -> ie p qreg
  | _ -> failwith "Ie needs 1 qreg parameter and 1 scalar parameter"

(* IR convertions *)

let ie_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | qreg :: _, Qbircks.Gate.Param.Scalar (i1, i2) :: _ ->
      Prog.Gate
        (ie (Hps.Scalar.SFrac (i1, i2)) @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "ie_of_ir gate needs 1 qreg param and 1 scalar param"

let ir_gate_func_map =
  Qbircks.Ast.Gate_name_map.(
    Clifford_k.ir_gate_func_map |> add "ie" ie_of_ir |> add "Ie" ie_of_ir
    |> add "IE" ie_of_ir)
