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

open Hps

(* Full hybrid state description assertions. *)
let hps_eq hps1 hps2 = equal hps1 hps2
let assert_hps_eq hps1 hps2 = assert (hps_eq hps1 hps2)

let prog_eq ?metrics prog1 prog2 hps_input =
  let open Evaluator in
  let hps1 =
    evaluate_prog prog1 hps_input ~rewrite_settings:all_auto ~print:false
      ?metrics
  in
  let hps2 =
    evaluate_prog prog2 hps_input ~rewrite_settings:all_auto ~print:false
      ?metrics
  in
  hps_eq hps1 hps2

let assert_prog_eq ?metrics prog1 prog2 hps_input =
  assert (prog_eq ?metrics prog1 prog2 hps_input)

(* Satisfaction assertions. *)
let hps_satisfies ?metrics hps_spec hps =
  let rns_cond = Output.find_reg_names hps_spec.output in
  let rns = Output.find_reg_names hps.output in
  if rns_cond = rns then equal hps_spec hps
  else
    let rns_disc = Reg_name_set.diff rns rns_cond in
    match Rewrite.Discard.check_and_apply rns_disc hps ?metrics with
    | Error _ -> false
    | Ok (hps_keep, _) -> equal hps_spec hps_keep

let assert_hps_satisfies ?metrics hps_spec hps =
  assert (hps_satisfies ?metrics hps_spec hps)

(* Probabilistic satisfaction assertions. *)
let hps_satisfies_proba_output ?metrics:_ p output_spec hps =
  Scalar.(equal (Concretization.hps_proba_output output_spec hps) (simp p))

let assert_hps_satisfies_proba_output ?metrics p output_spec hps =
  assert (hps_satisfies_proba_output ?metrics p output_spec hps)

let hps_satisfies_proba_output_leq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p output_spec hps =
  Scalar.(
    leq ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_output output_spec hps)
      (simp p))

let assert_hps_satisfies_proba_output_leq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p output_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_output_leq ~epsilon_abs ~epsilon_rel ?metrics p
         output_spec hps)

let hps_satisfies_proba_output_geq ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p output_spec hps =
  Scalar.(
    geq ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_output output_spec hps)
      (simp p))

let assert_hps_satisfies_proba_output_geq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p output_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_output_geq ~epsilon_abs ~epsilon_rel ?metrics p
         output_spec hps)

let hps_satisfies_proba_output_lt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p output_spec hps =
  Scalar.(
    lt ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_output output_spec hps)
      (simp p))

let assert_hps_satisfies_proba_output_lt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p output_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_output_lt ~epsilon_abs ~epsilon_rel ?metrics p
         output_spec hps)

let hps_satisfies_proba_output_gt ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p output_spec hps =
  Scalar.(
    gt ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_output output_spec hps)
      (simp p))

let assert_hps_satisfies_proba_output_gt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p output_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_output_gt ~epsilon_abs ~epsilon_rel ?metrics p
         output_spec hps)

let hps_satisfies_proba_qmem ?(fact = true) ?metrics p qmem_spec hps =
  let p_qmem =
    if fact then Concretization.hps_proba_qmem_fact qmem_spec hps ?metrics
    else Concretization.hps_proba_qmem qmem_spec hps
  in
  Scalar.(equal p_qmem (simp p))

let assert_hps_satisfies_proba_qmem ?(fact = true) ?metrics p qmem_spec hps =
  assert (hps_satisfies_proba_qmem ~fact ?metrics p qmem_spec hps)

let hps_satisfies_proba_qmem_leq ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  let p_qmem =
    if fact then Concretization.hps_proba_qmem_fact qmem_spec hps ?metrics
    else Concretization.hps_proba_qmem qmem_spec hps
  in
  Scalar.(leq ~epsilon_abs ~epsilon_rel p_qmem (simp p))

let assert_hps_satisfies_proba_qmem_leq ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_qmem_leq ~fact ~epsilon_abs ~epsilon_rel ?metrics p
         qmem_spec hps)

let hps_satisfies_proba_qmem_geq ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  let p_qmem =
    if fact then Concretization.hps_proba_qmem_fact qmem_spec hps ?metrics
    else Concretization.hps_proba_qmem qmem_spec hps
  in
  Scalar.(geq ~epsilon_abs ~epsilon_rel p_qmem (simp p))

let assert_hps_satisfies_proba_qmem_geq ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_qmem_geq ~fact ~epsilon_abs ~epsilon_rel ?metrics p
         qmem_spec hps)

let hps_satisfies_proba_qmem_lt ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  let p_qmem =
    if fact then Concretization.hps_proba_qmem_fact qmem_spec hps ?metrics
    else Concretization.hps_proba_qmem qmem_spec hps
  in
  Scalar.(lt ~epsilon_abs ~epsilon_rel p_qmem (simp p))

let assert_hps_satisfies_proba_qmem_lt ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_qmem_lt ~fact ~epsilon_abs ~epsilon_rel ?metrics p
         qmem_spec hps)

let hps_satisfies_proba_qmem_gt ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  let p_qmem =
    if fact then Concretization.hps_proba_qmem_fact qmem_spec hps ?metrics
    else Concretization.hps_proba_qmem qmem_spec hps
  in
  Scalar.(gt ~epsilon_abs ~epsilon_rel p_qmem (simp p))

let assert_hps_satisfies_proba_qmem_gt ?(fact = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p qmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_qmem_gt ~fact ~epsilon_abs ~epsilon_rel ?metrics p
         qmem_spec hps)

let hps_satisfies_proba_cmem_stack ?metrics:_ p cms_spec hps =
  Scalar.(equal (Concretization.hps_proba_cmem_stack cms_spec hps) (simp p))

let assert_hps_satisfies_proba_cmem_stack ?metrics p cms_spec hps =
  assert (hps_satisfies_proba_cmem_stack ?metrics p cms_spec hps)

let hps_satisfies_proba_cmem_stack_leq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p cmem_stack_spec
    hps =
  Scalar.(
    leq ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_cmem_stack cmem_stack_spec hps)
      (simp p))

let assert_hps_satisfies_proba_cmem_stack_leq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_stack_spec hps
    =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_stack_leq ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_stack_spec hps)

let hps_satisfies_proba_cmem_stack_geq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p cmem_stack_spec
    hps =
  Scalar.(
    geq ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_cmem_stack cmem_stack_spec hps)
      (simp p))

let assert_hps_satisfies_proba_cmem_stack_geq
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_stack_spec hps
    =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_stack_geq ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_stack_spec hps)

let hps_satisfies_proba_cmem_stack_lt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p cmem_stack_spec
    hps =
  Scalar.(
    lt ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_cmem_stack cmem_stack_spec hps)
      (simp p))

let assert_hps_satisfies_proba_cmem_stack_lt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_stack_spec hps
    =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_stack_lt ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_stack_spec hps)

let hps_satisfies_proba_cmem_stack_gt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics:_ p cmem_stack_spec
    hps =
  Scalar.(
    gt ~epsilon_abs ~epsilon_rel
      (Concretization.hps_proba_cmem_stack cmem_stack_spec hps)
      (simp p))

let assert_hps_satisfies_proba_cmem_stack_gt
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_stack_spec hps
    =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_stack_gt ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_stack_spec hps)

let hps_satisfies_proba_cmem ?(split = true) ?metrics p cmem_spec hps =
  let p_cmem =
    if split then Concretization.hps_proba_cmem_split cmem_spec hps ?metrics
    else Concretization.hps_proba_cmem cmem_spec hps
  in
  Scalar.(equal p_cmem (simp p))

let assert_hps_satisfies_proba_cmem ?(split = true) ?metrics p cmem_spec hps =
  assert (hps_satisfies_proba_cmem ~split ?metrics p cmem_spec hps)

let hps_satisfies_proba_cmem_leq ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  let p_cmem =
    if split then Concretization.hps_proba_cmem_split cmem_spec hps ?metrics
    else Concretization.hps_proba_cmem cmem_spec hps
  in
  Scalar.(leq ~epsilon_abs ~epsilon_rel p_cmem (simp p))

let assert_hps_satisfies_proba_cmem_leq ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_leq ~split ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_spec hps)

let hps_satisfies_proba_cmem_geq ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  let p_cmem =
    if split then Concretization.hps_proba_cmem_split cmem_spec hps ?metrics
    else Concretization.hps_proba_cmem cmem_spec hps
  in
  Scalar.(geq ~epsilon_abs ~epsilon_rel p_cmem (simp p))

let assert_hps_satisfies_proba_cmem_geq ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_geq ~split ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_spec hps)

let hps_satisfies_proba_cmem_lt ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  let p_cmem =
    if split then Concretization.hps_proba_cmem_split cmem_spec hps ?metrics
    else Concretization.hps_proba_cmem cmem_spec hps
  in
  Scalar.(lt ~epsilon_abs ~epsilon_rel p_cmem (simp p))

let assert_hps_satisfies_proba_cmem_lt ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_lt ~split ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_spec hps)

let hps_satisfies_proba_cmem_gt ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  let p_cmem =
    if split then Concretization.hps_proba_cmem_split cmem_spec hps ?metrics
    else Concretization.hps_proba_cmem cmem_spec hps
  in
  Scalar.(gt ~epsilon_abs ~epsilon_rel p_cmem (simp p))

let assert_hps_satisfies_proba_cmem_gt ?(split = true)
    ?(epsilon_abs = Mlmpfr.make_from_str "1e-6")
    ?(epsilon_rel = Mlmpfr.make_from_str "1e-6") ?metrics p cmem_spec hps =
  assert (
    Option.value ~default:false
    @@ hps_satisfies_proba_cmem_gt ~split ~epsilon_abs ~epsilon_rel ?metrics p
         cmem_spec hps)

(* Unitary equivalence assertions. *)
let unitary_eq ?metrics prog1 prog2 hps_input =
  match Prog.inverse_unitary prog2 with
  | Some p2i ->
      let p1_p2i = Prog.seq prog1 p2i in
      let hps =
        Evaluator.(
          evaluate_prog p1_p2i hps_input ~rewrite_settings:all_auto ~print:false
            ?metrics)
      in
      hps_eq hps hps_input
  | None -> false

let assert_unitary_eq ?metrics prog1 prog2 hps_input =
  assert (unitary_eq ?metrics prog1 prog2 hps_input)
