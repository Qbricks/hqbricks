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

(** This module contains the base types to construct a {!Prog}. *)

(** {1:base_types Types} *)

(** Quantum register type, list of qbits identified by a register name of type
    string and a length of type pr_int, that can be sliced with two pr_int. *)
type qreg =
  | QCons of string * pr_int
      (** Quantum register with given name and length.
          - The name must not be empty.
          - The length must be strictly positive.
          - If this constructor is called multiple times with the same name, the
            length must be the same each time as well. *)
  | QSlice of qreg * pr_int * pr_int
      (** Slice of the given quantum register in between the given indices
          (first start included, then end excluded).
          - Both start and end must be non negative.
          - End must be strictly greater than start and lesser than or equal to
            the length of the quantum register. *)

(** Classical register, list of bits identified by a register name of type
    string and a length of type pr_int, that can be sliced with two pr_int. *)
and creg =
  | CCons of string * pr_int
      (** Classical register with the given name and length.
          - The name must not be empty.
          - The length must be strictly positive.
          - If this constructor is called multiple times with the same name, the
            length must be the same each time as well. *)
  | CSlice of creg * pr_int * pr_int
      (** Slice of the given classical register in between the given indices
          (first start included, then end excluded).
          - Both start and end must be non negative.
          - End must be strictly greater than start and lesser than or equal to
            the length of the classical register. *)

(** Boolean *)
and pr_bool =
  | BVar of string
      (** Boolean variable with the given name.
          - The name must not be empty. *)
  | False  (** False value. *)
  | True  (** True value. *)
  | QBitVal of qreg * pr_int
      (** qbit value in the given qreg at the given index.
          - The index must non negative and lesser than or equal to the qreg
            length. *)
  | CBitVal of creg * pr_int
      (** bit value in the given creg at the given index.
          - The index must non negative and lesser than or equal to the creg
            length. *)
  | Not of pr_bool  (** Not b. *)
  | Xor of pr_bool * pr_bool  (** b1 xor b2. *)
  | And of pr_bool * pr_bool  (** b1 and b2. *)
  | Cond of pr_bool * pr_bool * pr_bool
      (** Conditional expression: b1?(b2;b3). *)
  | BIter of string * pr_bool * string * pr_int * pr_int * pr_bool
      (** Boolean iterator.
          - s1 is a BVar string identifier, BVar s1 is the accumulator.
          - b1 is the value of BVar s1 during the first iteration.
          - s2 is an IVar string identifier, IVar s2 is the iterated variable.
          - i1 is the start value of IVar s2 (included).
          - i2 is the end value of IVar s2 (excluded).
          - b2 is the boolean expression depending on BVar s1 and IVar s2 which
            applies at each step. *)

(** Int. *)
and pr_int =
  | IVar of string
      (** Integer variable with the given name.
          - The name must not be empty. *)
  | Const of Z.t  (** Zarith integer constant. *)
  | QGet of qreg  (** Get the int value of qreg. *)
  | CGet of creg  (** Get the int value of creg. *)
  | Add of pr_int * pr_int  (** i1 + i2. *)
  | Mul of pr_int * pr_int  (** i1 * i2. *)
  | Pow of pr_int * pr_int  (** i1 ** i2. *)
  | QRegLen of qreg  (** Length of qreg. *)
  | CRegLen of creg  (** Length of creg. *)
  | IIter of string * pr_int * string * pr_int * pr_int * pr_int
      (** Int iterator.
          - s1 is an IVar string identifier, IVar s1 is the accumulator.
          - i1 is the value of IVar s1 during the first iteration.
          - s2 is an IVar string identifier, IVar s2 is the iterated variable.
          - i2 is the start value of IVar s2 (included).
          - i3 is the end value of IVar s2 (excluded).
          - i4 is the int expression depending on IVar s1 and IVar s2 which
            applies at each step. *)

(** {1:base_syntactic_sugar Syntactic Sugar} *)

(* pr_bool *)
val bvar : string -> pr_bool
(** pr_bool variable with the given name. *)

val b_true : pr_bool
(** pr_bool true. *)

val b_false : pr_bool
(** pr_bool false. *)

val qbit_val : qreg -> pr_int -> pr_bool
(** pr_bool value of the given qreg at the given index. *)

val qbit_valv : qreg -> string -> pr_bool
(** pr_bool value of the given qreg at the given var index. *)

val cbit_val : creg -> pr_int -> pr_bool
(** pr_bool value of the given creg at the given index. *)

val cbit_valv : creg -> string -> pr_bool
(** pr_bool value of the given creg at the given var index. *)

val b_not : pr_bool -> pr_bool
(** pr_bool not. *)

val b_xor : pr_bool -> pr_bool -> pr_bool
(** pr_bool xor. *)

val b_and : pr_bool -> pr_bool -> pr_bool
(** pr_bool and. *)

val b_cond : pr_bool -> pr_bool -> pr_bool -> pr_bool
(** [b_cond b1 b2 b3] returns a pr_bool corresponding to the conditional
    expression [b1 ? b2 : b3]. *)

val b_iter :
  string -> pr_bool -> string -> pr_int -> pr_int -> pr_bool -> pr_bool
(** [b_iter s1 b1 s2 i1 i2 b2] returns a pr_bool iterator where:
    - [s1] is the string identifier of the pr_bool variable used as accumulator.
    - [b1] is the initial value of the pr_bool variable [s1].
    - [s2] is the string identifier of the iterated pr_int variable.
    - [i1] is the start value of the iterated pr_int variable [s2] (included).
    - [i2] is the end value of the iterated pr_int variable [s2] (excluded).
    - [b2] is the pr_bool expression applied at each step. *)

val ( ! ) : pr_bool -> pr_bool
(** pr_bool {!b_not} operator. *)

val ( ^ ) : pr_bool -> pr_bool -> pr_bool
(** pr_bool {!b_xor} operator. *)

val ( & ) : pr_bool -> pr_bool -> pr_bool
(** pr_bool {!b_and} operator. *)

(* pr_int *)
val ivar : string -> pr_int
(** pr_int variable with the given name. *)

val i_of_z : Z.t -> pr_int
(** Convert a Zarith big integer to a pr_int. *)

val i_of_int : int -> pr_int
(** Convert an int to a pr_int. *)

val i_of_string : string -> pr_int
(** Convert a string to a pr_int. The string must be composed of only (-)\[0-9\]
    characters. *)

val i_of_qreg : qreg -> pr_int
(** Convert a qreg to a pr_int. *)

val i_of_creg : creg -> pr_int
(** Convert a qreg to a pr_int. *)

val i_add : pr_int -> pr_int -> pr_int
(** pr_int addition. *)

val i_sub : pr_int -> pr_int -> pr_int
(** pr_int subtraction. *)

val i_mul : pr_int -> pr_int -> pr_int
(** pr_int multiplication. *)

val i_pow : pr_int -> pr_int -> pr_int
(** pr_int power. *)

val qreg_len : qreg -> pr_int
(** Length of qreg. *)

val creg_len : creg -> pr_int
(** Length of creg. *)

val i_iter : string -> pr_int -> string -> pr_int -> pr_int -> pr_int -> pr_int
(** [i_iter s1 i1 s2 i2 i3 i4] returns a pr_int iterator where:
    - [s1] is the string identifier of the pr_int variable used as accumulator.
    - [i1] is the initial value of the pr_int variable [s1].
    - [s2] is the string identifier of the iterated pr_int variable.
    - [i2] is the start value of the iterated pr_int variable [s2] (included).
    - [i3] is the end value of the iterated pr_int variable [s2] (excluded).
    - [i4] is the pr_int expression applied at each step. *)

val ( ~$ ) : int -> pr_int
(** pr_int {!i_of_int} operator. *)

val ( + ) : pr_int -> pr_int -> pr_int
(** pr_int {!i_add} operator. *)

val ( - ) : pr_int -> pr_int -> pr_int
(** pr_int {!i_sub} operator. *)

val ( * ) : pr_int -> pr_int -> pr_int
(** pr_int {!i_mul} operator. *)

val ( ** ) : pr_int -> pr_int -> pr_int
(** pr_int {!i_pow} operator. *)

(* qreg *)
val qreg : string -> pr_int -> qreg
(** qreg with the given name and length. *)

val q_slice : qreg -> pr_int -> pr_int -> qreg
(** [q_slice qreg i_start i_end] returns the slice of the quantum register
    [qreg] starting at index [i_start] (included) and ending at index [i_end]
    (excluded). *)

val q_slice_from : qreg -> pr_int -> qreg
(** [q_slice_from qreg i_start] returns the slice of the quantum register [qreg]
    starting at index [i_start] (included) until the end of the register. *)

val q_slice_to : qreg -> pr_int -> qreg
(** [q_slice_to qreg i_end] returns the slice of the quantum register [qreg]
    starting at index [0] (included) and ending at index [i_end] (excluded). *)

val q_idx : qreg -> pr_int -> qreg
(** [q_idx qreg i] returns the slice of the quantum register [qreg] containing
    only the element at index [i]. *)

val q_idxv : qreg -> string -> qreg
(** [q_idxv qreg var_name] returns the slice of the quantum register [qreg]
    containing only the element at index [ivar var_name]. *)

(* creg *)
val creg : string -> pr_int -> creg
(** creg with the given name and length. *)

val c_slice : creg -> pr_int -> pr_int -> creg
(** [c_slice creg i_start i_end] returns the slice of the classical register
    [creg] starting at index [i_start] (included) and ending at index [i_end]
    (excluded). *)

val c_slice_from : creg -> pr_int -> creg
(** [c_slice_from creg i_start] returns the slice of the classical register
    [creg] starting at index [i_start] (included) until the end of the register.
*)

val c_slice_to : creg -> pr_int -> creg
(** [c_slice_to creg i_end] returns the slice of the classical register [creg]
    starting at index [0] (included) and ending at index [i_end] (excluded). *)

val c_idx : creg -> pr_int -> creg
(** [c_idx creg i] returns the slice of the classical register [creg] containing
    only the element at index [i]. *)

val c_idxv : creg -> string -> creg
(** [c_idxv creg var_name] returns the slice of the classical register [creg]
    containing only the element at index [ivar var_name]. *)

(** {1:base_operations Operations} *)

(* substitute_ivar *)
val pr_bool_substitute_ivar : string -> pr_int -> pr_bool -> pr_bool
(** [pr_bool_substitute_ivar var_name i_sub b] returns a new pr_bool with the
    pr_int variable [var_name] substituted by [i_sub] in [b]. *)

val pr_int_substitute_ivar : string -> pr_int -> pr_int -> pr_int
(** [pr_int_substitute_ivar var_name i_sub i] returns a new pr_int with the
    pr_int variable [var_name] substituted by [i_sub] in [i]. *)

val qreg_substitute_ivar : string -> pr_int -> qreg -> qreg
(** [qreg_substitute_ivar var_name i_sub qreg] returns a new qreg with the
    pr_int variable [var_name] substituted by [i_sub] in [qreg]. *)

val creg_substitute_ivar : string -> pr_int -> creg -> creg
(** [creg_substitute_ivar var_name i_sub creg] returns a new creg with the
    pr_int variable [var_name] substituted by [i_sub] in [creg]. *)

(** {1:base_comparisons Comparisons} *)

(* Equal *)
val equal_qreg : qreg -> qreg -> bool
(** qreg equality test. *)

val equal_creg : creg -> creg -> bool
(** creg equality test. *)

val equal_pr_bool : pr_bool -> pr_bool -> bool
(** pr_bool equality test. *)

val equal_pr_int : pr_int -> pr_int -> bool
(** pr_int equality test. *)

(** {1:base_conversions Conversions} *)

(* to_string *)
val pr_bool_to_string : pr_bool -> string
(** Convert the given pr_bool to string. *)

val pr_int_to_string : pr_int -> string
(** Convert the given pr_int to string. *)

val qreg_to_string : qreg -> string
(** Convert the given qreg to string. *)

val creg_to_string : creg -> string
(** Convert the given creg to string. *)

(** {1:base_printing Printing} *)

(* pretty print *)
val pp_qreg : Format.formatter -> qreg -> unit
(** Print the given qreg on the specified formatter. *)

val pp_creg : Format.formatter -> creg -> unit
(** Print the given creg on the specified formatter. *)

val pp_pr_bool : Format.formatter -> pr_bool -> unit
(** Print the given pr_bool on the specified formatter. *)

val pp_pr_int : Format.formatter -> pr_int -> unit
(** Print the given pr_int on the specified formatter. *)
