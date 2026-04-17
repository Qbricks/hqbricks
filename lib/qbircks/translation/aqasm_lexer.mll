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
open Aqasm_parser
}

rule token = parse
  | '\r'? '\n' { LINEENDING }
  | [' ' '\t'] { token lexbuf }
  | "DEFINE" { ignore_statement lexbuf }
  | "BEGIN" { BEGIN }
  | "END" { END }
  | "qubits" { QBITS }
  | "cbits" { CBITS }
  | "GATE" { GATE }
  | "MEAS" { MEAS }
  | "?" { IF }
  | ":" { THEN }
  | "RESET" { RESET }
  | "LOGIC" { LOGIC }
  | "BREAK" { failwith "BREAK is not handled" }

  | 'q' { QBIT }
  | 'c' { CBIT }

  | "CTRL" { CTRL }
  | "DAG" { failwith "DAG is not handled" }
  | "CONJ" { failwith "CONJ is not handled" }
  | "TRANS" { failwith "TRANS is not handled" }

  | "PI" { PI }
  | '+' { failwith "+ is not handled" }
  | '-' { NEG }
  | '*' { MUL }
  | '/' { DIV }
  | '&' { AND }
  | '|' { failwith "| is not handled" }
  | '^' { failwith "^ is not handled" }
  | '~' { NOT }

  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }

  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as id { ID id }
  | (['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)(['e''E']['-''+']?['0'-'9']+)? { failwith "Real numbers are not handled" }
  | ['1'-'9']+['0'-'9']*|'0' as i { INT(Z.of_string i) }

  | eof { EOF }
  | _ { failwith "Unknown token" }

and ignore_statement = parse
  | '\n' { token lexbuf }
  | _ { ignore_statement lexbuf }
