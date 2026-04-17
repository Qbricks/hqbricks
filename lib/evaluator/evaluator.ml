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

module Base = Base
open Hps
open Base

let print_step prog hps =
  Prog.print prog;
  print_endline (Hps.to_string hps ^ "\n")

let auto_hh_rewrite ?(print = true) ?metrics hps =
  if print then print_endline "Automatic hh rewrite:";
  let hh_y_pairs, hps = Rewrite.Hh.find_and_apply_all hps ?metrics in
  if hh_y_pairs = [] then (
    if print then print_endline "No hh found\n";
    (hps, false))
  else (
    if print then (
      List.iter
        (fun (yi0, yi1) ->
          Printf.printf "hh applied between y%d and y%d\n" yi0 yi1)
        hh_y_pairs;
      print_endline (Hps.to_string hps ^ "\n"));
    (hps, true))

let auto_change_var_rewrite ?(print = true) ?metrics hps =
  Rewrite.Change_var.(
    if print then print_endline "Automatic change_var rewrite:";
    let cv_map, hps = find_and_apply_all_y_xor_no_y hps ?metrics in
    if Utils.Int_map.is_empty cv_map then (
      if print then print_endline "No change_var found\n";
      (hps, false))
    else (
      if print then (
        Utils.Int_map.iter
          (fun yi k ->
            Printf.printf "change_var applied: changed y%d -> %s\n" yi
              (Hket.to_string k))
          cv_map;
        print_endline (Hps.to_string hps ^ "\n"));
      (hps, true)))

let split_command cmd_str =
  (* A leading space is added to cmd_str for the first match *)
  let cmd_str = " " ^ cmd_str in
  (* Regex to match either a non-space token or a quoted token *)
  let match_token_reg = Str.regexp "[ \t]+\\([^ \t\"][^ \t]*\\|\"[^\"]*\"\\)" in
  try
    let rec aux acc i =
      (* Search for the next token *)
      if Str.string_match match_token_reg cmd_str i then
        let token = Str.matched_group 1 cmd_str in
        (* Remove the surrounding quotes if it's a quoted token *)
        let token =
          if String.length token >= 2 && token.[0] = '"' then
            String.sub token 1 (String.length token - 2)
          else token
        in
        aux (token :: acc) (Str.match_end ())
      else if Str.string_match (Str.regexp "[ \t]*[^ \t]") cmd_str i then
        (* Exit if there are still things in cmd_str that we couldn't match *)
        raise Exit
      else List.rev acc
    in
    Some (aux [] 0)
  with Exit -> None

let rec prompt_rewrite ?metrics hps =
  print_string "Rewrite: ";
  let cmd_str = read_line () in
  match split_command cmd_str with
  | None ->
      print_endline "There is an error in the command format\n";
      prompt_rewrite hps ?metrics
  | Some [] ->
      print_endline "";
      hps
  | Some (cmd :: args) -> exec_cmd cmd args hps ?metrics

and exec_cmd ?metrics cmd args hps =
  let open Rewrite in
  match cmd with
  | "hh" -> (
      match args with
      | [ s1; s2 ] -> (
          let s1 =
            if String.length s1 > 0 && s1.[0] = 'y' then Str.string_after s1 1
            else s1
          in
          let s2 =
            if String.length s2 > 0 && s2.[0] = 'y' then Str.string_after s2 1
            else s2
          in
          match (int_of_string_opt s1, int_of_string_opt s2) with
          | Some yi0, Some yi1 -> (
              match Hh.check_and_apply yi0 yi1 hps ?metrics with
              | Ok hps ->
                  Printf.printf "hh applied between y%d and y%d:\n" yi0 yi1;
                  print_endline (Hps.to_string hps ^ "\n");
                  prompt_rewrite hps ?metrics
              | Error s ->
                  print_endline ("Failed to apply hh: " ^ s ^ "\n");
                  prompt_rewrite hps ?metrics)
          | _ ->
              print_endline "hh arguments must be int or yint\n";
              prompt_rewrite hps ?metrics)
      | [ "-all" ] ->
          let hps, _ = auto_hh_rewrite hps ?metrics in
          prompt_rewrite hps ?metrics
      | [ "-find" ] -> (
          match Hh.find_all hps with
          | [] ->
              print_endline "No possible hh found\n";
              prompt_rewrite hps ?metrics
          | y_pairs ->
              print_endline "Possible hh:";
              List.iter
                (fun (yi0, yi1) -> Printf.printf "(y%d, y%d)\n" yi0 yi1)
                y_pairs;
              print_endline "\n";
              prompt_rewrite hps ?metrics)
      | _ ->
          print_endline "hh needs 2 arguments or -[all|find] option\n";
          prompt_rewrite hps ?metrics)
  | "change_var" | "cv" -> (
      match args with
      | [ s1; s2 ] -> (
          let s1 =
            if String.length s1 > 0 && s1.[0] = 'y' then Str.string_after s1 1
            else s1
          in
          match (int_of_string_opt s1, Hket.of_string_opt s2) with
          | Some yi, Some new_val -> (
              match Change_var.check_and_apply yi new_val hps ?metrics with
              | Ok hps ->
                  Printf.printf "change_var applied: changed y%d -> %s:\n" yi
                    (Hket.to_string new_val);
                  print_endline (Hps.to_string hps ^ "\n");
                  prompt_rewrite hps ?metrics
              | Error s ->
                  print_endline ("Failed to apply change_var: " ^ s ^ "\n");
                  prompt_rewrite hps ?metrics)
          | None, _ ->
              print_endline "change_var first argument must be int or yint\n";
              prompt_rewrite hps ?metrics
          | _, None ->
              print_endline
                "change_var second argument must be a new ket val of the form \
                 \"y1+x3+1+y3+x2*x4...\"\n";
              prompt_rewrite hps ?metrics)
      | [ "-all" ] ->
          let hps, _ = auto_change_var_rewrite hps ?metrics in
          prompt_rewrite hps ?metrics
      | _ ->
          print_endline "change_var needs 2 arguments or -all option\n";
          prompt_rewrite hps ?metrics)
  | "fact_distr" | "fd" -> (
      if args = [] then (
        print_endline "fact_distr needs at least 1 register name argument\n";
        prompt_rewrite hps ?metrics)
      else
        let reg_name_set = Reg_name_set.of_list args in
        match Fact_distr.Reg.check_and_apply reg_name_set hps with
        | Ok (hps1, hps2) ->
            print_endline "fact_distr:";
            print_endline (Hps.to_string hps1);
            print_endline "⊗";
            print_endline (Hps.to_string hps2 ^ "\n");
            prompt_rewrite hps ?metrics
        | Error s ->
            print_endline ("Failed to apply fact_distr: " ^ s ^ "\n");
            prompt_rewrite hps ?metrics)
  | "discard" | "disc" -> (
      if args = [] then (
        print_endline "discard needs at least 1 register name argument\n";
        prompt_rewrite hps ?metrics)
      else
        let reg_name_set = Reg_name_set.of_list args in
        match Discard.check_and_apply reg_name_set hps ?metrics with
        | Ok (hps, hps_disc) ->
            print_endline "discard applied:";
            print_endline ("Discarded:\n" ^ Hps.to_string hps_disc);
            print_endline ("HPS:\n" ^ Hps.to_string hps ^ "\n");
            prompt_rewrite hps ?metrics
        | Error s ->
            print_endline ("Failed to apply discard: " ^ s ^ "\n");
            prompt_rewrite hps ?metrics)
  | "exit" ->
      print_endline "";
      hps
  | _ ->
      print_endline "Unknow command\n";
      prompt_rewrite hps ?metrics

(* rewrite settings flags *)
let no_rewrite = 0b000
let interactive_rewrite = 0b001
let auto_hh = 0b010
let auto_change_var = 0b100
let all_auto = 0b110
let all_rewrite = 0b111

let auto_hh_with_settings ?(print = true) ?metrics rewrite_settings hps =
  if rewrite_settings land auto_hh <> 0 then auto_hh_rewrite hps ~print ?metrics
  else (hps, false)

let auto_change_var_with_settings ?(print = true) ?metrics rewrite_settings hps
    =
  if rewrite_settings land auto_change_var <> 0 then
    auto_change_var_rewrite hps ~print ?metrics
  else (hps, false)

let auto_rewrite_with_settings ?(print = true) ?metrics rewrite_settings hps =
  let hps, change_var_applied =
    auto_change_var_with_settings rewrite_settings hps ~print ?metrics
  in
  let hps, hh_applied =
    auto_hh_with_settings rewrite_settings hps ~print ?metrics
  in
  (hps, change_var_applied || hh_applied)

let interactive_rewrite_with_settings ?metrics rewrite_settings hps =
  if rewrite_settings land interactive_rewrite <> 0 then
    prompt_rewrite hps ?metrics
  else hps

let rec evaluate_prog_aux ?(k = Hket.one) ?(var_val = Utils.Var_name_map.empty)
    ?(rewrite_settings = no_rewrite) ?(print = true) ?metrics prog hps =
  if Hket.is_zero k then hps
  else
    match prog with
    | Prog.PVar _ -> failwith "Cannot evaluate PVar into an hps"
    | Prog.Skip -> hps
    | Prog.InitQReg qreg ->
        if not @@ Hket.is_one k then
          (* In order to handle InitQReg in IfElse, we need to check that
           * both sides init the same qregs
           *)
          failwith "InitQReg in IfElse is not supported yet"
        else
          let qreg_name, qreg_i_start, qreg_len = evaluate_qreg qreg ~var_val in
          let rec loop i hps =
            if i < qreg_len then
              let qreg_i = qreg_i_start + i in
              let qmem =
                Mem.add (qreg_name, qreg_i) Hket.zero hps.output.qmem
              in
              let output = { hps.output with qmem } in
              loop (i + 1) { hps with output }
            else hps
          in
          let hps = loop 0 hps in
          if print then print_step prog hps;
          interactive_rewrite_with_settings rewrite_settings hps ?metrics
    | Prog.Seq (prog1, prog2) ->
        let hps =
          evaluate_prog_aux prog1 hps ~k ~var_val ~rewrite_settings ~print
            ?metrics
        in
        evaluate_prog_aux prog2 hps ~k ~var_val ~rewrite_settings ~print
          ?metrics
    | Prog.For (name, i_start, i_end, prog) ->
        let i_start = Z.to_int @@ evaluate_int i_start ~var_val in
        let i_end = Z.to_int @@ evaluate_int i_end ~var_val in
        let rec loop i hps =
          if i < i_end then (
            if print then Printf.printf "%s = %d:\n" name i;
            loop (i + 1)
              (evaluate_prog_aux prog hps ~k
                 ~var_val:(Utils.Var_name_map.add name i var_val)
                 ~rewrite_settings ~print ?metrics))
          else hps
        in
        loop i_start hps
    | Prog.IfElse (cond, prog1, prog2) ->
        if print then print_endline (Prog.to_string prog ^ ":");
        let cond = evaluate_bool cond hps ~var_val in
        let hps =
          evaluate_prog_aux prog1 hps ~k:(Hket.mul k cond) ~var_val
            ~rewrite_settings ~print ?metrics
        in
        evaluate_prog_aux prog2 hps
          ~k:Hket.(mul k (neg cond))
          ~var_val ~rewrite_settings ~print ?metrics
    | Prog.Meas (qreg, creg) ->
        let hps, _ =
          auto_hh_with_settings rewrite_settings hps ~print ?metrics
        in
        if not @@ Hket.is_one k then
          (*
           * In order to handle Meas in IfElse, we need to check that
           * both sides measure the same qregs
           *)
          failwith "Meas in IfElse is not supported yet"
        else
          let qreg_name, qreg_i_start, qreg_len = evaluate_qreg qreg ~var_val in
          let creg_name, creg_i_start, creg_len = evaluate_creg creg ~var_val in
          if qreg_len <> creg_len then
            failwith
              "Cannot evaluate Meas if qreg len and creg len are not equal"
          else if qreg_len = 0 then (
            if print then print_step prog hps;
            interactive_rewrite_with_settings rewrite_settings hps ?metrics)
          else
            let start_cmem = List.hd hps.output.cmem_stack in
            let rec loop i hps cmem =
              if i < qreg_len then
                let qreg_i = qreg_i_start + i in
                let creg_i = creg_i_start + i in
                let cmem =
                  Mem.add (creg_name, creg_i)
                    (Mem.find (qreg_name, qreg_i) hps.output.qmem)
                    cmem
                in
                let qmem = Mem.remove (qreg_name, qreg_i) hps.output.qmem in
                let output = Output.{ hps.output with qmem } in
                loop (i + 1) { hps with output } cmem
              else (hps, cmem)
            in
            let hps, cmem = loop 0 hps start_cmem in
            let output =
              Output.
                { hps.output with cmem_stack = cmem :: hps.output.cmem_stack }
            in
            let hps = { hps with output } in
            if print then print_step prog hps;
            Metrics.add_measures_opt metrics qreg_len;
            interactive_rewrite_with_settings rewrite_settings hps ?metrics
    | Prog.Gate gate ->
        let hps = gate.evaluate ~k ~var_val ?metrics hps in
        if print then print_step prog hps;
        interactive_rewrite_with_settings rewrite_settings hps ?metrics
    | Prog.SetCReg (_creg, _i) ->
        failwith "SetCReg evaluation not implemented yet"

let evaluate_prog ?(var_val = Utils.Var_name_map.empty)
    ?(rewrite_settings = no_rewrite) ?(print = true) ?metrics prog hps =
  let hps =
    evaluate_prog_aux ~var_val ~rewrite_settings ~print ?metrics prog hps
  in
  let hps, rule_applied =
    auto_rewrite_with_settings rewrite_settings hps ~print ?metrics
  in
  if rule_applied then
    interactive_rewrite_with_settings rewrite_settings hps ?metrics
  else hps
