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

(* Hadamard gate *)
let evaluate_h qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    let rec loop i hps =
      if i < qreg_len then
        let yvar = Hket.of_var (Y hps.y_count) in
        let qmem_key = (qreg_name, i_start + i) in
        (* update support *)
        let support = Support.add hps.y_count hps.support in
        (* update phase *)
        let phase_to_add_b =
          Hket.mul (Mem.find qmem_key hps.output.qmem) yvar
        in
        let phase =
          Phase.addp hps.phase
            Hket.(
              if is_one k then
                Phase.lift_ket phase_to_add_b @@ Dyadic1.make Z.one 1
              else Phase.lift_ket (mul phase_to_add_b k) @@ Dyadic1.make Z.one 1)
        in
        (* update output *)
        let prev_qmem_val = Mem.find qmem_key hps.output.qmem in
        let new_qmem_val = yvar in
        let qmem_val =
          if Hket.is_one k then new_qmem_val
          else Hket.(xor (mul k new_qmem_val) (mul (neg k) prev_qmem_val))
        in
        let qmem = Mem.add qmem_key qmem_val hps.output.qmem in
        let output = { hps.output with qmem } in
        (* update y-count *)
        let y_count = hps.y_count + 1 in
        loop (i + 1) { hps with support; phase; output; y_count }
      else hps
    in
    let hps = loop 0 hps in
    (* update scalar *)
    let scalar_to_mul = Scalar.(Sqrt (SFrac (Z.one, Z.(~$2 ** qreg_len)))) in
    let scalar =
      Scalar.(
        simp
        @@ SMul
             ( (if Hket.is_one k then scalar_to_mul
                else SAdd (SMul (SBool k, scalar_to_mul), SBool (Hket.neg k))),
               hps.scalar ))
    in
    Metrics.add_gates_opt metrics qreg_len;
    { hps with scalar }

let rec h qreg =
  let rec g =
    {
      name = "H";
      qreg_params = [ qreg ];
      params = [];
      with_params = h_with_params;
      inverse = (fun () -> g);
      evaluate = evaluate_h qreg;
    }
  in
  g

and h_with_params qreg_params _ =
  match qreg_params with
  | qreg :: _ -> h qreg
  | [] -> failwith "H needs 1 qreg parameter"

(* X gate *)
let evaluate_x qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    let rec loop i hps =
      if i < qreg_len then
        let qmem_key = (qreg_name, i_start + i) in
        let prev_qmem_val = Mem.find qmem_key hps.output.qmem in
        let new_qmem_val = Hket.neg prev_qmem_val in
        let qmem_val =
          if Hket.is_one k then new_qmem_val else Hket.xor prev_qmem_val k
        in
        let qmem = Mem.add qmem_key qmem_val hps.output.qmem in
        let output = { hps.output with qmem } in
        loop (i + 1) { hps with output }
      else hps
    in
    Metrics.add_gates_opt metrics qreg_len;
    loop 0 hps

let rec x qreg =
  let rec x =
    {
      name = "X";
      qreg_params = [ qreg ];
      params = [];
      with_params = x_with_params;
      inverse = (fun () -> x);
      evaluate = evaluate_x qreg;
    }
  in
  x

and x_with_params qreg_params _ =
  match qreg_params with
  | qreg :: _ -> x qreg
  | [] -> failwith "X needs 1 qreg parameter"

(* Z gate *)
let evaluate_z qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    let rec loop i hps =
      if i < qreg_len then
        let qmem_key = (qreg_name, i_start + i) in
        let phase_to_add_b = Mem.find qmem_key hps.output.qmem in
        let phase =
          Phase.addp hps.phase
            Hket.(
              if is_one k then
                Phase.lift_ket phase_to_add_b @@ Dyadic1.make Z.one 1
              else Phase.lift_ket (mul phase_to_add_b k) @@ Dyadic1.make Z.one 1)
        in
        loop (i + 1) { hps with phase }
      else hps
    in
    Metrics.add_gates_opt metrics qreg_len;
    loop 0 hps

let rec z qreg =
  let rec z =
    {
      name = "Z";
      qreg_params = [ qreg ];
      params = [];
      with_params = z_with_params;
      inverse = (fun () -> z);
      evaluate = evaluate_z qreg;
    }
  in
  z

and z_with_params qreg_params _ =
  match qreg_params with
  | qreg :: _ -> z qreg
  | [] -> failwith "Z needs 1 qreg parameter"

(* Rz gate *)
let evaluate_rz t qreg ?(k = Hps.Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name, i_start, qreg_len = evaluate_qreg qreg ~var_val in
    let t = Z.to_int @@ Evaluator.Base.evaluate_int t ~var_val in
    let rec loop i hps =
      if i < qreg_len then
        let qmem_key = (qreg_name, i_start + i) in
        let phase_to_add_b = Mem.find qmem_key hps.output.qmem in
        let dy_fact =
          if t < 0 then Dyadic1.make Z.(neg one) (-t) else Dyadic1.make Z.one t
        in
        let phase =
          Phase.addp hps.phase
            Hket.(
              if is_one k then Phase.lift_ket phase_to_add_b dy_fact
              else Phase.lift_ket (mul phase_to_add_b k) dy_fact)
        in
        loop (i + 1) { hps with phase }
      else hps
    in
    Metrics.add_gates_opt metrics qreg_len;
    loop 0 hps

let rec rz i qreg =
  let rec rz =
    {
      name = "RZ";
      qreg_params = [ qreg ];
      params = [ Param.Angle i ];
      with_params = rz_with_params;
      inverse = (fun () -> rz_inv);
      evaluate = evaluate_rz i qreg;
    }
  and rz_inv =
    let i_inv = Prog.Base.(Mul (Const Z.(~$(-1)), i)) in
    {
      name = "RZ";
      qreg_params = [ qreg ];
      params = [ Param.Angle i_inv ];
      with_params = rz_with_params;
      inverse = (fun () -> rz);
      evaluate = evaluate_rz i_inv qreg;
    }
  in
  rz

and rz_with_params qreg_params params =
  match (qreg_params, params) with
  | qreg :: _, Param.Angle i :: _ -> rz i qreg
  | _ -> failwith "Rz needs 1 qreg parameter and 1 pr_int angle parameter"

(* Swap gate *)
let evaluate_swap qreg1 qreg2 ?(k = Hps.Hket.one)
    ?(var_val = Utils.Var_name_map.empty) ?metrics hps =
  let open Hps in
  let open Evaluator.Base in
  if Hket.is_zero k then hps
  else
    let qreg_name1, i_start1, qreg_len1 = evaluate_qreg qreg1 ~var_val in
    let qreg_name2, i_start2, qreg_len2 = evaluate_qreg qreg2 ~var_val in
    if qreg_len1 != qreg_len2 then
      failwith "Both qreg should have the same size";
    let rec loop i hps =
      if i >= qreg_len1 then hps
      else
        let qbit1 = (qreg_name1, i_start1 + i) in
        let qbit2 = (qreg_name2, i_start2 + i) in
        let ket1 = Mem.find qbit1 hps.output.qmem in
        let ket2 = Mem.find qbit2 hps.output.qmem in
        loop (i + 1)
          (hps
          |> add_qmem qbit1 Hket.(xor (mul ket2 k) (mul ket1 (neg k)))
          |> add_qmem qbit2 Hket.(xor (mul ket1 k) (mul ket2 (neg k))))
    in
    Metrics.add_gates_opt metrics qreg_len1;
    loop 0 hps

let rec swap qreg1 qreg2 =
  let rec g =
    {
      name = "SWAP";
      qreg_params = [ qreg1; qreg2 ];
      params = [];
      with_params = swap_with_params;
      inverse = (fun () -> g);
      evaluate = evaluate_swap qreg1 qreg2;
    }
  in
  g

and swap_with_params qreg_params _ =
  match qreg_params with
  | qreg1 :: qreg2 :: _ -> swap qreg1 qreg2
  | _ -> failwith "SWAP needs 2 qreg parameters"

(* IR convertions *)

let h_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg :: _ -> Prog.Gate (h @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "h_of_ir gate needs 1 qreg param"

let x_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg :: _ -> Prog.Gate (x @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "x_of_ir gate needs 1 qreg param"

let z_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg :: _ -> Prog.Gate (z @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "z_of_ir gate needs 1 qreg param"

let rz_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | qreg :: _, Qbircks.Gate.Param.Angle i :: _ ->
      Prog.Gate (rz Prog.Base.(Const i) @@ Qbircks.Base.qreg_to_prog qreg)
  | _ -> failwith "rz_of_ir gate needs 1 qreg param and 1 angle param"

let ir_qreg_to_qbit_val = function
  | Qbircks.Base.QCons (name, len) ->
      Prog.Base.(qbit_val (qreg name (Const len)) ~$0)
  | Qbircks.Base.QIndex ((name, len), i) ->
      Prog.Base.(qbit_val (qreg name (Const len)) (Const i))

let ch_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg1 :: qreg2 :: _ ->
      Prog.(
        ir_qreg_to_qbit_val qreg1 => Gate (h @@ Qbircks.Base.qreg_to_prog qreg2))
  | _ -> failwith "ch_of_ir gate needs 2 qreg params"

let cx_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg1 :: qreg2 :: _ ->
      Prog.(
        ir_qreg_to_qbit_val qreg1 => Gate (x @@ Qbircks.Base.qreg_to_prog qreg2))
  | _ -> failwith "cx_of_ir gate needs 2 qreg params"

let cz_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg1 :: qreg2 :: _ ->
      Prog.(
        ir_qreg_to_qbit_val qreg1 => Gate (z @@ Qbircks.Base.qreg_to_prog qreg2))
  | _ -> failwith "cz_of_ir gate needs 2 qreg params"

let crz_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params, gate.params) with
  | qreg1 :: qreg2 :: _, Qbircks.Gate.Param.Angle i :: _ ->
      Prog.(
        ir_qreg_to_qbit_val qreg1
        => Gate (rz (Const i) @@ Qbircks.Base.qreg_to_prog qreg2))
  | _ -> failwith "crz_of_ir gate needs 2 qreg params and 1 angle param"

let ccx_of_ir gate =
  match Qbircks.Gate.(gate.qreg_params) with
  | qreg1 :: qreg2 :: qreg3 :: _ ->
      Prog.(
        ir_qreg_to_qbit_val qreg1
        => (ir_qreg_to_qbit_val qreg2
           => Gate (x @@ Qbircks.Base.qreg_to_prog qreg3)))
  | _ -> failwith "ccx_of_ir gate needs 3 qreg params"

let ir_gate_func_map =
  Qbircks.Ast.Gate_name_map.(
    empty |> add "h" h_of_ir |> add "H" h_of_ir |> add "x" x_of_ir
    |> add "X" x_of_ir |> add "not" x_of_ir |> add "Not" x_of_ir
    |> add "NOT" x_of_ir |> add "z" z_of_ir |> add "Z" z_of_ir
    |> add "rz" rz_of_ir |> add "Rz" rz_of_ir |> add "RZ" rz_of_ir
    |> add "ch" ch_of_ir |> add "Ch" ch_of_ir |> add "CH" ch_of_ir
    |> add "cx" cx_of_ir |> add "Cx" cx_of_ir |> add "CX" cx_of_ir
    |> add "cnot" cx_of_ir |> add "Cnot" cx_of_ir |> add "CNot" cx_of_ir
    |> add "CNOT" cx_of_ir |> add "cz" cz_of_ir |> add "Cz" cz_of_ir
    |> add "CZ" cz_of_ir |> add "crz" crz_of_ir |> add "Crz" crz_of_ir
    |> add "CRz" crz_of_ir |> add "CRZ" crz_of_ir |> add "ccx" ccx_of_ir
    |> add "Ccx" ccx_of_ir |> add "CCX" ccx_of_ir |> add "toffoli" ccx_of_ir
    |> add "Toffoli" ccx_of_ir |> add "TOFFOLI" ccx_of_ir)
