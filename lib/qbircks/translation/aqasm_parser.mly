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

%{
open Ast
open Base

let qbit_count = ref Z.zero
let cbit_count = ref Z.zero
let cbit_bool_assoc = ref []

let get_qbit_index = function
  | QCons _ -> failwith "qbit shouldn't be a qcons"
  | QIndex (_, i) -> i

let get_cbit_index = function
  | CCons _ -> failwith "cbit shouldn't be a ccons"
  | CIndex (_, i) -> i

let cbit_to_val cbit = CBitVal (("c", !cbit_count), get_cbit_index cbit)

let add_cbit_bool_assoc cbit b =
  cbit_bool_assoc := (get_cbit_index cbit, b) :: !cbit_bool_assoc

let find_cbit_bool_assoc cbit =
  match List.assoc_opt (get_cbit_index cbit) !cbit_bool_assoc with
  | Some b -> b
  | None -> cbit_to_val cbit

let meas_to_ir qbit_list cbit_list =
  let cbit_list =
    if List.length cbit_list = 0 then
      List.map
        (fun qbit -> CIndex (("c", !cbit_count), get_qbit_index qbit))
        qbit_list
    else cbit_list
  in
  let len = List.length qbit_list in
  if len != List.length cbit_list then
    failwith "MEAS qbit list and cbit list must have the same length"
  else
    List.fold_left2
      (fun acc qbit cbit ->
        cbit_bool_assoc :=
          List.remove_assoc (get_cbit_index cbit) !cbit_bool_assoc;
        if acc = Skip then Meas (qbit, cbit) else Seq (acc, Meas (qbit, cbit)))
      Skip qbit_list cbit_list

let reset_to_ir qbit_list =
  List.fold_left
    (fun acc qbit ->
      if acc = Skip then InitQReg qbit else Seq (acc, InitQReg qbit))
    Skip qbit_list

let int_to_2_pow_n i =
  if not Z.(equal (i land (i - one)) zero)
  then failwith "Only angle with 2^n denominator are handled"
  else Z.log2 i |> Z.of_int
%}

%token LINEENDING
%token BEGIN END
%token QBITS CBITS
%token GATE PARAM MEAS RESET LOGIC
%token IF THEN
%token QBIT CBIT
%token CTRL
%token PI NEG MUL DIV AND NOT
%token LBRACKET RBRACKET LPAREN RPAREN
%token COMMA
%token <string> ID
%token <Z.t> INT
%token EOF

%left MUL DIV AND
%nonassoc NEG NOT

%start main_program

%type <Ast.t> main_program

%%

main_program:
  | BEGIN LINEENDING qubits cbits program endl EOF { $5 }

endl:
  | END {}
  | END LINEENDING {}

qubits:
  | QBITS INT LINEENDING { qbit_count := $2 }

cbits:
  | CBITS INT LINEENDING { cbit_count := $2 }
  | { cbit_count := Z.zero }

program:
  | statement { $1 }
  | program statement { Seq ($1, $2) }

statement:
  | qop { $1 }
  | IF cbit THEN uop { If (find_cbit_bool_assoc $2, $4) }
  | LOGIC cbit boolexp LINEENDING { add_cbit_bool_assoc $2 $3; Skip }

qop:
  | uop { $1 }
  | MEAS qbitlist cbitlist LINEENDING { meas_to_ir $2 $3 }
  | MEAS qbitlist LINEENDING { meas_to_ir $2 [] }
  | RESET qbitlist LINEENDING { reset_to_ir $2 }
  | RESET cbitlist LINEENDING { failwith "Cannot reset cbits" }
  | RESET qbitlist cbitlist LINEENDING { failwith "Cannot reset cbits" }

uop:
  | gate qbitlist LINEENDING { Gate Gate.{ $1 with qreg_params = $2 }}

gate:
  | ID {
      Gate.{
        name = $1;
        qreg_params = [];
        params = [];
    }}
  | ID LBRACKET explist RBRACKET {
      Gate.{
        name = $1;
        qreg_params = [];
        params = $3;
    }}
  | GATE ID {
      Gate.{
        name = $2;
        qreg_params = [];
        params = [];
    }}
  | GATE ID LBRACKET explist RBRACKET {
      Gate.{
        name = $2;
        qreg_params = [];
        params = $4;
    }}
  | CTRL LPAREN gate RPAREN { Gate.{ $3 with name = "C" ^ $3.name }}

boolexp:
  | cbit { cbit_to_val $1 }
  | NOT boolexp { Not $2 }
  | boolexp AND boolexp { And ($1, $3) }
  | LPAREN boolexp RPAREN { $2 }

qbitlist:
  | qbit { [ $1 ] }
  | qbitlist COMMA qbit { $1 @ [ $3 ] }

qbit:
  | QBIT LBRACKET INT RBRACKET { QIndex (("q", !qbit_count), $3) }

cbitlist:
  | cbit { [ $1 ] }
  | cbitlist COMMA cbit { $1 @ [ $3 ] }

cbit:
  | CBIT LBRACKET INT RBRACKET { CIndex (("c", !cbit_count), $3) }

explist:
  | exp { [ $1 ] }
  | explist COMMA exp { $1 @ [ $3 ] }

exp:
  | PI { Gate.Param.Angle Z.zero }
  | PI DIV INT { Gate.Param.Angle (int_to_2_pow_n $3) }
  | INT MUL PI {
      if Z.equal $1 Z.one then Gate.Param.Angle Z.zero
      else failwith "Only PI and -1*PI are handled as angle numerator"
    }
  | INT MUL PI DIV INT {
      if Z.equal $1 Z.one then Gate.Param.Angle (int_to_2_pow_n $5)
      else failwith "Only PI and -1*PI are handled as angle numerator"
    }
  | NEG INT MUL PI DIV INT {
      if Z.equal $2 Z.one then Gate.Param.Angle (Z.(-int_to_2_pow_n $6))
      else failwith "Only PI and -1*PI are handled as angle numerator"
    }
  | INT { Gate.Param.Int $1 }
  | NEG INT { Gate.Param.Int Z.(-$2) }
  | INT DIV INT { Gate.Param.Scalar ($1, $3) }
  | INT DIV NEG INT { Gate.Param.Scalar ($1, Z.(-$4)) }
  | NEG INT DIV INT { Gate.Param.Scalar (Z.(-$2), $4) }
  | NEG INT DIV NEG INT { Gate.Param.Scalar (Z.(-$2), Z.(-$5)) }
