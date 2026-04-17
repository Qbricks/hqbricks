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

open Hqbricks

(* --- Dependencies --- *)
(* QFT with swaps *)
let qft_swap q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  let iv = ivar "i" in
  let i_inv = len - ~$1 - iv in
  let jv = ivar "j" in
  let swap_len =
    let _, _, l = Evaluator.Base.evaluate_qreg q in
    l / 2
  in
  p_for "i" ~$0 len
    ((q_idx q i_inv |> h)
    -- p_for "j" ~$1 (len - iv)
         (qbit_val q (i_inv - jv) => (q_idx q i_inv |> rz (jv + ~$1))))
  -- p_for "i" ~$0 ~$swap_len
       (Gate (swap (q_idx q iv) (q_idx q (len - ~$1 - iv))))

(* QFT dagger *)
let qft_dag q = Option.get @@ Prog.inverse_unitary @@ qft_swap q

(* --- QPE --- *)
(* QPE with u circuit *)
let qpe u e c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  init_qreg c -- (c |> h)
  -- ((c, "i") >> u (ivar "i") e)
  -- qft_dag c -- (c -@ mc)

(* QPE with u gate *)
let qpe_gate u c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let e = qreg "E" ~$1 in
  init_qreg c -- init_qreg e -- (c |> h)
  -- ((c, "i") >> apply_gate e (u (ivar "i")))
  -- qft_dag c -- (c -@ mc)

(* U RZ circuit for QPE *)
let u_rz i q =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let len = qreg_len q in
  p_for "iu" ~$0 (len - i) (q_idxv q "iu" |> rz (len - i - ivar "iu"))

(* This function computes the spec theta' numerator approximated by QPE
   for a given theta, and is used for QPE specification. *)
let compute_theta' theta_num theta_den_pow theta'_den_pow =
  if theta_den_pow = theta'_den_pow then theta_num
  else
    let reg_len_diff = theta_den_pow - theta'_den_pow in
    let round_bit_value =
      let shift = reg_len_diff - 1 in
      Z.((theta_num asr shift) land ~$1)
    in
    let mask = Z.(pred (~$1 lsl theta'_den_pow)) in
    Z.(((theta_num asr reg_len_diff) + round_bit_value) land mask)

(* QPE-Concrete *)
let verify_qpe_concrete ?metrics theta_num e_len c_len =
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = qpe u_rz e c mc in
  let input_hps = Hps.(one |> add_qmem_int ("E", 0) e_len theta_num) in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:no_rewrite ~print:false
        ?metrics)
  in
  let theta'_num = compute_theta' theta_num e_len c_len in
  let spec_p = Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2)) in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  assert (
    Option.value ~default:false
    @@ Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps ?metrics)

(* QPE-Concrete exact *)
let verify_qpe_concrete_exact ?metrics theta_num e_len c_len =
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = qpe u_rz e c mc in
  let input_hps = Hps.(one |> add_qmem_int ("E", 0) e_len theta_num) in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:no_rewrite ~print:false
        ?metrics)
  in
  let theta'_num = compute_theta' theta_num e_len c_len in
  let spec_p = Hps.Scalar.one in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  assert (Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps ?metrics)

(* QPE-Symbolic *)
let verify_qpe_symbolic_exact ?metrics n =
  let e_len = n in
  let c_len = n in
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog = qpe u_rz e c mc in
  let input_hps = Hps.(one |> add_qmem_vec_x ("E", 0) e_len 0) in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:all_auto ~print:false
        ?metrics)
  in
  let spec_p = Hps.Scalar.one in
  let spec_cmem = Hps.Mem.(empty |> add_vec_x ("mc", 0) e_len 0) in
  assert (Assertion.hps_satisfies_proba_cmem spec_p spec_cmem hps ?metrics)

(* QPE-Gate-Concrete *)
let verify_qpe_gate_concrete ?metrics theta_num theta_den_pow c_len =
  let c = Prog.(qreg "c" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  let prog =
    qpe_gate Prog.(Algo_verif.Qpe.u (i_of_z theta_num) ~$theta_den_pow) c mc
  in
  let input_hps = Hps.one in
  let hps =
    Evaluator.(
      evaluate_prog prog input_hps ~rewrite_settings:no_rewrite ~print:false
        ?metrics)
  in
  let theta'_num = compute_theta' theta_num theta_den_pow c_len in
  let spec_p = Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2)) in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  assert (
    Option.value ~default:false
    @@ Assertion.hps_satisfies_proba_cmem_geq spec_p spec_cmem hps ?metrics)

let rsa_260_theta_num =
  Z.of_string
    "22112825529529666435281085255026230927612089502470015394413748319128822941402001986512729726569746599085900330031400051170742204560859276357953757185954298838958709229238491006703034124620545784566413664540684214361293017694020846391065875914794251435144458199"

let theta_num_7 = Z.(~$47)
let theta_num_11 = Z.(~$983)
let theta_num_101 = Z.(~$1940650529537570856)
let theta_num_300 = Z.of_string "1245176803961806747632845746442796629"

let theta_num_1036 =
  Z.of_string
    "143979989637958370046578322075827794472763057933216667995760023490738554892484534814220498855869881036299815427390759130068467280054878370488456254085265627040566992215941939915768435467491784659992661421290619091967970414852685662571590924090284562242006420156052549361240664095080352561848159390157265653413"

let theta_num_3000 =
  Z.of_string
    "13973184352426075455695557234933804639244448444313449912388001450482434636212528118447069671885645565743999989359759890041393950309299245532217637795499785694571731623239685011807592835605183113619882972792245554712875272922700068289503564674633207585420235049305384808298180204446965048629899838824900563063432454524441792835891351791830547706247901376306551053"

let () = Mlmpfr.set_default_prec 64

let benches =
  [
    ( "QPE exact symbolic 210",
      fun ?metrics () -> verify_qpe_symbolic_exact 70 ?metrics );
    ( "QPE exact concrete RSA 260",
      fun ?metrics () ->
        verify_qpe_concrete_exact rsa_260_theta_num 862 862 ?metrics );
    ( "QPE approx concrete 7",
      fun ?metrics () -> verify_qpe_gate_concrete theta_num_7 6 3 ?metrics );
    ( "QPE approx concrete 11",
      fun ?metrics () -> verify_qpe_gate_concrete theta_num_11 10 5 ?metrics );
    ( "QPE approx concrete 101",
      fun ?metrics () -> verify_qpe_gate_concrete theta_num_101 61 50 ?metrics
    );
    ( "QPE approx concrete 300",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 128;
        verify_qpe_concrete theta_num_300 120 90 ?metrics;
        Mlmpfr.set_default_prec 64 );
    ( "QPE approx concrete 1036",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 1024;
        verify_qpe_concrete theta_num_1036 1024 6 ?metrics;
        Mlmpfr.set_default_prec 64 );
    ( "QPE approx concrete 3000",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 1024;
        verify_qpe_concrete theta_num_3000 1200 900 ?metrics;
        Mlmpfr.set_default_prec 64 );
    ( "QPE exact symbolic 2100",
      fun ?metrics () -> verify_qpe_symbolic_exact 700 ?metrics );
  ]

let quick_benches = List.take 8 benches
