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

open Cmdliner
open Cmdliner.Term.Syntax
open Hqbricks

type translation =
  | Openqasm2_qbircks
  | Qbircks_openqasm2
  | Aqasm_qbircks
  | Qbircks_aqasm
[@@deriving show { with_path = false }]

let exit_with_error_msg msg =
  Printf.eprintf "Error: %s\n" msg;
  exit 123

let read_file file =
  let read file ic =
    try Ok (In_channel.input_all ic)
    with Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
  try
    match file with
    | "-" ->
        binary_stdin ();
        read file In_channel.stdin
    | file -> In_channel.with_open_bin file (read file)
  with Sys_error e -> Error e

let write_file file s =
  let write file s oc =
    try Ok (Out_channel.output_string oc s)
    with Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = Out_channel.(set_binary_mode stdout true) in
  try
    match file with
    | "-" ->
        binary_stdout ();
        write file s Out_channel.stdout
    | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

let translate translation infile outfile =
  let input_str =
    match read_file infile with
    | Ok input_str -> input_str
    | Error e -> exit_with_error_msg e
  in
  match translation with
  | Openqasm2_qbircks -> (
      let ir =
        try Qbircks.Translation.of_openqasm2 input_str
        with exn ->
          exit_with_error_msg
          @@ "translation from OpenQASM2 to QBricks failed: "
          ^ Printexc.to_string exn
      in
      let ir_json =
        try Qbircks.to_yojson ir
        with exn ->
          exit_with_error_msg
          @@ "translation from QBricks to json file failed failed: "
          ^ Printexc.to_string exn
      in
      match
        write_file outfile @@ Yojson.Safe.pretty_to_string ir_json ^ "\n"
      with
      | Ok () -> ()
      | Error e -> exit_with_error_msg e)
  | Qbircks_openqasm2 -> (
      let ir =
        try
          match Qbircks.of_yojson @@ Yojson.Safe.from_string input_str with
          | Ok ir -> ir
          | Error e -> exit_with_error_msg @@ "invalid QBricks file: " ^ e
        with exn ->
          exit_with_error_msg @@ "invalid QBricks file: "
          ^ Printexc.to_string exn
      in
      let oqasm2 =
        try Qbircks.Translation.to_openqasm2 ir
        with exn ->
          exit_with_error_msg
          @@ "translation from QBricks to OpenQASM2 failed: "
          ^ Printexc.to_string exn
      in
      match write_file outfile oqasm2 with
      | Ok () -> ()
      | Error e -> exit_with_error_msg e)
  | Aqasm_qbircks -> (
      let ir =
        try Qbircks.Translation.of_aqasm input_str
        with exn ->
          exit_with_error_msg @@ "translation from AQASM to QBricks failed: "
          ^ Printexc.to_string exn
      in
      let ir_json =
        try Qbircks.to_yojson ir
        with exn ->
          exit_with_error_msg
          @@ "translation from QBricks to json file failed failed: "
          ^ Printexc.to_string exn
      in
      match
        write_file outfile @@ Yojson.Safe.pretty_to_string ir_json ^ "\n"
      with
      | Ok () -> ()
      | Error e -> exit_with_error_msg e)
  | Qbircks_aqasm -> (
      let ir =
        try
          match Qbircks.of_yojson @@ Yojson.Safe.from_string input_str with
          | Ok ir -> ir
          | Error e -> exit_with_error_msg @@ "invalid QBricks file: " ^ e
        with exn ->
          exit_with_error_msg @@ "invalid QBricks file: "
          ^ Printexc.to_string exn
      in
      let aqasm =
        try Qbircks.Translation.to_aqasm ir
        with exn ->
          exit_with_error_msg @@ "translation from QBricks to AQASM failed: "
          ^ Printexc.to_string exn
      in
      match write_file outfile aqasm with
      | Ok () -> ()
      | Error e -> exit_with_error_msg e)

let translation =
  let doc =
    "Specify the translation direction, must be one of: \
     $(b,openqasm2_to_qbircks) | $(b,qbircks_to_openqasm2) | \
     $(b,aqasm_to_qbircks) | $(b,qbircks_to_aqasm)"
  in
  Arg.(
    required
    & pos 0
        (some
           (enum
              [
                ("openqasm2_to_qbircks", Openqasm2_qbircks);
                ("qbircks_to_openqasm2", Qbircks_openqasm2);
                ("aqasm_to_qbircks", Aqasm_qbircks);
                ("qbircks_to_aqasm", Qbircks_aqasm);
              ]))
        None
    & info [] ~doc ~docv:"TRANSLATION")

let infile =
  let doc = "$(docv) is the file to read from. Use $(b,-) for $(b,stdin)" in
  Arg.(value & opt string "-" & info [ "i"; "input-file" ] ~doc ~docv:"FILE")

let outfile =
  let doc = "$(docv) is the file to write to. Use $(b,-) for $(b,stdout)" in
  Arg.(value & opt string "-" & info [ "o"; "output-file" ] ~doc ~docv:"FILE")

let translate_cmd =
  let doc = "Translate between quantum intermediate representations" in
  let man =
    [
      `S Manpage.s_examples;
      `P
        "qbircks_translate openqasm2_to_qbircks -i input.qasm -o output.qbircks";
      `P "qbircks_translate qbircks_to_aqasm -i input.qbircks -o output.aqasm";
    ]
  in
  Cmd.v (Cmd.info "qbircks-translate" ~version:"1.0.0" ~doc ~man)
  @@
  let+ translation = translation and+ infile = infile and+ outfile = outfile in
  translate translation infile outfile

let main () = Cmd.eval translate_cmd
let () = if !Sys.interactive then () else exit (main ())
