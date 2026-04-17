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

(* QPE *)
let qpe u e c mc =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  init_qreg c -- (c |> h)
  -- ((c, "i") >> u (ivar "i") e)
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

(* --- BRUS --- *)
let brus ?metrics u input_hps spec_cmem delta k =
  let hps = ref input_hps in
  let succ_p = ref Hps.Scalar.zero in
  let spec_fail_p = ref Hps.Scalar.one in
  for _ = 0 to k - 1 do
    let h = Evaluator.evaluate_prog u !hps ~print:false ?metrics in
    let step_succ_p =
      Concretization.hps_proba_cmem_split spec_cmem h ?metrics
    in
    (succ_p := Hps.Scalar.(simp (SAdd (!succ_p, step_succ_p))));
    let fail_p = Hps.Scalar.complement !succ_p in
    (spec_fail_p := Hps.Scalar.(simp (SMul (complement delta, !spec_fail_p))));
    let spec_succ_p = Hps.Scalar.complement !spec_fail_p in
    assert (Option.value ~default:false @@ Hps.Scalar.geq !succ_p spec_succ_p);
    assert (Option.value ~default:false @@ Hps.Scalar.leq fail_p !spec_fail_p);
    hps := Hps.{ input_hps with scalar = Scalar.(simp (Sqrt fail_p)) }
  done;
  !succ_p

(* Parameters for BRUS-H-PARALLEL *)
let hp_circ len =
  let open Prog in
  let open Gate_set_impl.Clifford_k in
  let q = qreg "q" ~$len in
  let mq = creg "mq" ~$len in
  init_qreg q -- (q |> h) -- (q -@ mq)

let brus_hp ?metrics spec_cmem len =
  brus (hp_circ len) Hps.one spec_cmem
    (Hps.Scalar.SFrac (Z.(~$1), Z.(~$1 lsl len)))
    ?metrics

(* Parameters for BRUS-QPE *)
let brus_qpe ?metrics u input_qmem spec_cmem =
  let e_len = Hps.Mem.cardinal input_qmem in
  let c_len = Hps.Mem.cardinal spec_cmem in
  let e = Prog.(qreg "E" ~$e_len) in
  let c = Prog.(qreg "q" ~$c_len) in
  let mc = Prog.(creg "mc" ~$c_len) in
  brus (qpe u e c mc)
    Hps.(one |> set_qmem input_qmem)
    spec_cmem
    (Hps.Scalar.SFloat Mlmpfr.(int_div 4 (pow_int Utils.mpfr_pi 2)))
    ?metrics

(* BRUS HP specification success probability *)
let compute_brus_hp_spec_success_p n k =
  let p_fail_1 = Q.(~$1 - Z.(~$1 /// (~$1 lsl n))) in
  let fail_p = Q.(Z.((p_fail_1.num ** k) /// (p_fail_1.den ** k))) in
  Q.(one - fail_p)

(* BRUS-H-Parallel *)
let verify_brus_hp ?metrics q_len spec_success_p k mq_n =
  let spec_cmem = Hps.Mem.(add_int ("mq", 0) q_len mq_n empty) in
  let success_p_scalar = brus_hp spec_cmem q_len k ?metrics in
  let success_p = Option.get @@ Hps.Scalar.to_q success_p_scalar in
  assert (Q.equal spec_success_p success_p)

(* BRUS-QPE *)
let verify_brus_qpe ?metrics theta_num e_len c_len k =
  let input_qmem = Hps.Mem.(add_int ("E", 0) e_len theta_num empty) in
  let theta'_num = compute_theta' theta_num e_len c_len in
  let spec_cmem = Hps.Mem.(add_int ("mc", 0) c_len theta'_num empty) in
  let success_p_scalar = brus_qpe u_rz input_qmem spec_cmem k ?metrics in
  let success_p = Option.get @@ Hps.Scalar.to_float success_p_scalar in
  let spec_success_inf =
    Mlmpfr.(
      int_sub 1 (pow_int (int_sub 1 (int_div 4 (pow_int Utils.mpfr_pi 2))) k))
  in
  assert (Utils.mpfr_float_approx_geq success_p spec_success_inf)

let spec_success_p_hp_k100_w20 = compute_brus_hp_spec_success_p 10 100
let spec_success_p_hp_k100_w200 = compute_brus_hp_spec_success_p 100 100
let spec_success_p_hp_k500_w200 = compute_brus_hp_spec_success_p 100 500
let spec_success_p_hp_k500_w2000 = compute_brus_hp_spec_success_p 1000 500
let target_n_hp_k100_w20 = Z.of_string "731"
let target_n_hp_k100_w200 = Z.of_string "65226428224224192775288391415"
let target_n_hp_k500_w200 = Z.of_string "1166860789811860759849167921719"

let target_n_hp_k500_w2000 =
  Z.of_string
    "10124173172020146536085019160033384108287693291570485069316922591572063413775736330671456629263536711843001231004214606991633334478276235527557175750962252869122491331069426820780517174312306782947023696008785998955397213738650606844270491472453431290850751256371639150565242345646937936413484806347893"

let theta_num_qpe_k5_w300 = Z.of_string "1245176803961806747632845746442796629"
let theta_num_qpe_k10_w300 = Z.of_string "942636469360688026348382703775336167"

let theta_num_qpe_k5_w3000 =
  Z.of_string
    "13973184352426075455695557234933804639244448444313449912388001450482434636212528118447069671885645565743999989359759890041393950309299245532217637795499785694571731623239685011807592835605183113619882972792245554712875272922700068289503564674633207585420235049305384808298180204446965048629899838824900563063432454524441792835891351791830547706247901376306551053"

let theta_num_qpe_k10_w3000 =
  Z.of_string
    "2082449113422047710057489993145374889221329834934972075252578952207743655826953042032601536139596848681185590259018499372721411793596147185816704737342075975154560777679036404643710248731475267632123811943312729839988714190523280420689106011039011211135154210378739656566988171606993341851591171138119278859555414322005981491823407329829739214592178162622004477"

let benches =
  [
    ( "BRUS-H-parallel k100 w20",
      fun ?metrics () ->
        verify_brus_hp 10 spec_success_p_hp_k100_w20 100 target_n_hp_k100_w20
          ?metrics );
    ( "BRUS-H-parallel k100 w200",
      fun ?metrics () ->
        verify_brus_hp 100 spec_success_p_hp_k100_w200 100 target_n_hp_k100_w200
          ?metrics );
    ( "BRUS-H-parallel k500 w200",
      fun ?metrics () ->
        verify_brus_hp 100 spec_success_p_hp_k500_w200 500 target_n_hp_k500_w200
          ?metrics );
    ( "BRUS-H-parallel k500 w2000",
      fun ?metrics () ->
        verify_brus_hp 1000 spec_success_p_hp_k500_w2000 500
          target_n_hp_k500_w2000 ?metrics );
    ( "BRUS-QPE k5 w300",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 128;
        verify_brus_qpe theta_num_qpe_k5_w300 120 90 5 ?metrics );
    ( "BRUS-QPE k10 w300",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 128;
        verify_brus_qpe theta_num_qpe_k10_w300 120 90 10 ?metrics );
    ( "BRUS-QPE k5 w3000",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 1024;
        verify_brus_qpe theta_num_qpe_k5_w3000 1200 900 5 ?metrics );
    ( "BRUS-QPE k10 w3000",
      fun ?metrics () ->
        Mlmpfr.set_default_prec 1024;
        verify_brus_qpe theta_num_qpe_k10_w3000 1200 900 10 ?metrics );
  ]

let quick_benches = List.take 6 benches
