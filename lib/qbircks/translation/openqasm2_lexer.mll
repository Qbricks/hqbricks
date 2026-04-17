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

{
open Openqasm2_parser
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "//" { comment lexbuf }
  | "OPENQASM" { OPENQASM }
  | "include" [' ' '\t' '\r' '\n']+ '"' [^'"']* '"' { ignore_statement lexbuf }
  | "qreg" { QREG }
  | "creg" { CREG }
  | "gate" { ignore_gate_decl lexbuf }
  | "opaque" { ignore_statement lexbuf }
  | "measure" { MEASURE }
  | "reset" { RESET }
  | "if" { IF }
  | "barrier" { ignore_statement lexbuf }

  | 'U' { U }
  | "CX" { CX }

  | "pi" { PI }
  | '+' { failwith "+ is not handled" }
  | '-' { NEG }
  | '*' { MUL }
  | '/' { DIV }
  | '^' { failwith "^ is not handled"  }
  | "sin" { failwith "sin is not handled" }
  | "cos" { failwith "cos is not handled" }
  | "tan" { failwith "tan is not handled" }
  | "exp" { failwith "exp is not handded" }
  | "ln" { failwith "ln is not handled" }
  | "sqrt" { failwith "sqrt is not handled" }

  | "==" { EQ }

  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "->" { ARROW }
  | ',' { COMMA }
  | ';' { SEMICOLON }

  | ['a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as id { ID id }
  | (['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)(['e''E']['-''+']?['0'-'9']+)? as r { REAL r }
  | ['1'-'9']+['0'-'9']*|'0' as i { INT(Z.of_string i) }

  | eof { EOF }
  | _ { failwith "Unknown token" }

and comment = parse
  | '\n' { token lexbuf }
  | _ { comment lexbuf }

and ignore_gate_decl = parse
  | '}' { token lexbuf }
  | _ { ignore_gate_decl lexbuf }

and ignore_statement = parse
  | ';' { token lexbuf }
  | _ { ignore_statement lexbuf }
