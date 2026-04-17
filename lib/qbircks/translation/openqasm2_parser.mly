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

let qregs_id = ref []
let cregs_id = ref []
let insert_qreg_id name len = qregs_id := (name, len) :: !qregs_id
let insert_creg_id name len = cregs_id := (name, len) :: !cregs_id
let get_qreg_id name = (name, List.assoc name !qregs_id)
let get_creg_id name = (name, List.assoc name !cregs_id)

let bool_of_creg_eq_int i_eq = function
  | CCons (name, len) ->
      let rec aux i =
        let cbit =
          let cb = CBitVal ((name, len), i) in
          if Z.(equal ((i_eq asr to_int i) mod (of_int 2)) zero) then Not cb
          else cb
        in
        if Z.(geq i (pred len)) then cbit else And (cbit, aux (Z.succ i))
      in
      aux Z.zero
  | CIndex _ -> failwith "creg in creg==int should not be indexed"

let int_to_2_pow_n i =
  if not Z.(equal (i land (i - one)) zero)
  then failwith "Only angle with 2^n denominator are handled"
  else Z.log2 i |> Z.of_int
%}

%token OPENQASM
%token QREG CREG
%token MEASURE RESET
%token IF
%token U CX
%token PI NEG MUL DIV
%token EQ
%token LBRACKET RBRACKET LPAREN RPAREN
%token ARROW COMMA SEMICOLON
%token <string> ID REAL
%token <Z.t> INT
%token EOF

%left MUL DIV
%nonassoc NEG

%start main_program

%type <Ast.t> main_program

%%

main_program:
  | OPENQASM REAL SEMICOLON program EOF { $4 }

program:
  | statement { $1 }
  | program statement { Seq ($1, $2) }

statement:
  | decl { Skip }
  | qop { $1 }
  | IF LPAREN creg EQ INT RPAREN qop { If (bool_of_creg_eq_int $5 $3, $7) }

decl:
  | QREG ID LBRACKET INT RBRACKET SEMICOLON { insert_qreg_id $2 $4 }
  | CREG ID LBRACKET INT RBRACKET SEMICOLON { insert_creg_id $2 $4 }

qop:
  | uop { $1 }
  | MEASURE qreg ARROW creg SEMICOLON { Meas ($2, $4) }
  | RESET qreg SEMICOLON { InitQReg $2 }

uop:
  | U LPAREN explist RPAREN qreg SEMICOLON {
    Gate Gate.{
      name = "U";
      qreg_params = [ $5 ];
      params = $3;
    }}
  | CX qreg COMMA qreg SEMICOLON {
      Gate Gate.{
        name = "CX";
        qreg_params = [ $2; $4 ];
        params = [];
    }}
  | ID qreglist SEMICOLON {
      Gate Gate.{
        name = $1;
        qreg_params = $2;
        params = [];
    }}
  | ID LPAREN RPAREN qreglist SEMICOLON {
      Gate Gate.{
        name = $1;
        qreg_params = $4;
        params = [];
    }}
  | ID LPAREN explist RPAREN qreglist SEMICOLON {
      Gate Gate.{
        name = $1;
        qreg_params = $5;
        params = $3;
    }}

qreglist:
  | qreg { [ $1 ] }
  | qreglist COMMA qreg { $1 @ [ $3 ] }

qreg:
  | ID { QCons (get_qreg_id $1) }
  | ID LBRACKET INT RBRACKET { QIndex (get_qreg_id $1, $3) }

creg:
  | ID { CCons (get_creg_id $1) }
  | ID LBRACKET INT RBRACKET { CIndex (get_creg_id $1, $3) }

explist:
  | exp { [ $1 ] }
  | explist COMMA exp { $1 @ [ $3 ] }

exp:
  | PI { Gate.Param.Angle Z.zero }
  | PI DIV INT { Gate.Param.Angle (int_to_2_pow_n $3) }
  | NEG PI DIV INT { Gate.Param.Angle (Z.(-int_to_2_pow_n $4)) }
  | INT MUL PI {
      if Z.equal $1 Z.one then Gate.Param.Angle Z.zero
      else failwith "Only pi and -pi are handled as angle numerator"
    }
  | INT MUL PI DIV INT {
      if Z.equal $1 Z.one then Gate.Param.Angle (int_to_2_pow_n $5)
      else failwith "Only pi and -pi are handled as angle numerator"
    }
  | NEG INT MUL PI DIV INT {
      if Z.equal $2 Z.one then Gate.Param.Angle (Z.(-int_to_2_pow_n $6))
      else failwith "Only pi and -pi are handled as angle numerator"
    }
  | INT { Gate.Param.Int ($1) }
  | NEG INT { Gate.Param.Int (Z.(-$2)) }
  | INT DIV INT { Gate.Param.Scalar ($1, $3) }
  | INT DIV NEG INT { Gate.Param.Scalar ($1, Z.(-$4)) }
  | NEG INT DIV INT { Gate.Param.Scalar (Z.(-$2), $4) }
  | NEG INT DIV NEG INT { Gate.Param.Scalar (Z.(-$2), Z.(-$5)) }
