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

(* Identity bitflip error gate *)
let evaluate_ie_x p qreg ?(k = Hps.Hket.one)
    ?(var_val = Utils.Var_name_map.empty) ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    if not @@ Hket.is_one k then failwith "Cannot put an Ie_X gate in an IfElse"
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
          let cmem = Mem.add ("w", Mem.cardinal_reg_name "w" cmem) yvar cmem in
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

let rec ie_x p qreg =
  {
    name = "IE_X";
    qreg_params = [ qreg ];
    params = [ Param.Scalar p ];
    with_params = ie_x_with_params;
    inverse = (fun () -> failwith "inverse not implemented for Ie_X");
    evaluate = evaluate_ie_x p qreg;
  }

and ie_x_with_params qreg_params params =
  match (qreg_params, params) with
  | qreg :: _, Param.Scalar p :: _ -> ie_x p qreg
  | _ -> failwith "Ie_X needs 1 qreg parameter and 1 scalar parameter"

(* Identity phaseflip error gate *)
let evaluate_ie_z p qreg ?(k = Hps.Hket.one)
    ?(var_val = Utils.Var_name_map.empty) ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    if not @@ Hket.is_one k then failwith "Cannot put an Ie_Z gate in an IfElse"
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
          (* update phase *)
          let phase_to_add_b = Mem.find qmem_key hps.output.qmem in
          let phase =
            Phase.addp hps.phase
              (Phase.lift_ket (Hket.mul phase_to_add_b yvar)
              @@ Dyadic1.make Z.one 1)
          in
          (* update cmem *)
          let cmem = Mem.add ("v", Mem.cardinal_reg_name "v" cmem) yvar cmem in
          (* update y-count *)
          let y_count = hps.y_count + 1 in
          loop (i + 1) { hps with support; scalar; phase; y_count } cmem
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

let rec ie_z p qreg =
  {
    name = "IE_Z";
    qreg_params = [ qreg ];
    params = [ Param.Scalar p ];
    with_params = ie_z_with_params;
    inverse = (fun () -> failwith "inverse not implemented for Ie_Z");
    evaluate = evaluate_ie_z p qreg;
  }

and ie_z_with_params qreg_params params =
  match (qreg_params, params) with
  | qreg :: _, Param.Scalar p :: _ -> ie_z p qreg
  | _ -> failwith "Ie_Z needs 1 qreg parameter and 1 scalar parameter"

(* Identity phaseflip error gate *)
let evaluate_ie p_w p_v qreg ?(k = Hps.Hket.one)
    ?(var_val = Utils.Var_name_map.empty) ?metrics hps =
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
          let yvar_w = Hket.of_var (Y hps.y_count) in
          let yvar_v = Hket.of_var (Y (hps.y_count + 1)) in
          let qmem_key = (qreg_name, i_start + i) in
          (* update support *)
          let support =
            Support.add (hps.y_count + 1) (Support.add hps.y_count hps.support)
          in
          (* update scalar *)
          let scalar =
            Scalar.(
              SMul
                ( hps.scalar,
                  SMul
                    ( Sqrt
                        (SMul
                           ( SPowB (p_w, yvar_w),
                             SPowB
                               ( SAdd (SFrac (Z.one, Z.one), SNeg p_w),
                                 Hket.neg yvar_w ) )),
                      Sqrt
                        (SMul
                           ( SPowB (p_v, yvar_v),
                             SPowB
                               ( SAdd (SFrac (Z.one, Z.one), SNeg p_v),
                                 Hket.neg yvar_v ) )) ) ))
          in
          (* update output.qmem *)
          let qmem_val = Hket.xor (Mem.find qmem_key hps.output.qmem) yvar_w in
          let qmem = Mem.add qmem_key qmem_val hps.output.qmem in
          let output = Output.{ hps.output with qmem } in
          (* update phase *)
          let phase_to_add_b = Mem.find qmem_key output.qmem in
          let phase =
            Phase.addp hps.phase
              (Phase.lift_ket (Hket.mul phase_to_add_b yvar_v)
              @@ Dyadic1.make Z.one 1)
          in
          (* update cmem *)
          let cmem =
            Mem.add ("w", Mem.cardinal_reg_name "w" cmem) yvar_w cmem
          in
          let cmem =
            Mem.add ("v", Mem.cardinal_reg_name "v" cmem) yvar_v cmem
          in
          (* update y-count *)
          let y_count = hps.y_count + 2 in
          loop (i + 1) { support; scalar; output; phase; y_count } cmem
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

let rec ie p_w p_v qreg =
  {
    name = "IE";
    qreg_params = [ qreg ];
    params = [ Param.Scalar p_w; Param.Scalar p_v ];
    with_params = ie_with_params;
    inverse = (fun () -> failwith "inverse not implemented for Ie");
    evaluate = evaluate_ie p_w p_v qreg;
  }

and ie_with_params qreg_params params =
  match (qreg_params, params) with
  | qreg :: _, Param.Scalar p_w :: Param.Scalar p_v :: _ -> ie p_w p_v qreg
  | _ -> failwith "Ie needs 1 qreg parameter and 2 scalar parameters"

(* IR convertions *)

let ie_x_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | qreg :: _, Qbircks.Gate.Param.Scalar (i1, i2) :: _ ->
      Prog.Gate
        (ie_x (Hps.Scalar.SFrac (i1, i2)) @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "ie_x_of_ir gate needs 1 qreg param and 1 scalar param"

let ie_z_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | qreg :: _, Qbircks.Gate.Param.Scalar (i1, i2) :: _ ->
      Prog.Gate
        (ie_z (Hps.Scalar.SFrac (i1, i2)) @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "ie_z_of_ir gate needs 1 qreg param and 1 scalar param"

let ie_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | ( qreg :: _,
      Qbircks.Gate.Param.Scalar (i1, i2)
      :: Qbircks.Gate.Param.Scalar (i3, i4)
      :: _ ) ->
      Prog.Gate
        (ie (Hps.Scalar.SFrac (i1, i2)) (Hps.Scalar.SFrac (i3, i4))
        @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "ie_of_ir gate needs 1 qreg param and 2 scalar params"

let ir_gate_func_map =
  Qbircks.Ast.Gate_name_map.(
    Clifford_k.ir_gate_func_map |> add "ie_x" ie_x_of_ir
    |> add "Ie_X" ie_x_of_ir |> add "IE_X" ie_x_of_ir |> add "ie_z" ie_z_of_ir
    |> add "Ie_Z" ie_z_of_ir |> add "IE_Z" ie_z_of_ir |> add "Ie" ie_of_ir
    |> add "IE" ie_of_ir |> add "ie" ie_of_ir)
