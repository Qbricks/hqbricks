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

(** HQbricks programs. *)

(** {1:types Types} *)

module Base = Base
module Gate = Gate

(** Program.

    A subset of these constructors are also quantum program constructors, a
    program containing only those constructors is a quantum program:
    - [PVar] if it contains a quantum program.
    - [Skip].
    - [InitQReg].
    - [Seq] if both prog fields are quantum programs.
    - [For] if prog is a quantum program.
    - [IfElse] if both prog fields are quantum programs.
    - [Gate]. *)
type t =
  | PVar of string
      (** Program variable with the given name.
          - The name must not be empty. *)
  | Skip  (** Skip. *)
  | InitQReg of Base.qreg  (** Init the given quantum register. *)
  | Seq of t * t  (** Sequence of programs. *)
  | For of string * Base.pr_int * Base.pr_int * t
      (** [For (s, i1, i2, prog)] is for [ivar s] between [i2] and [i2] do
          [prog]. *)
  | IfElse of Base.pr_bool * t * t
      (** [IfElse (b, prog1, prog2)] is if [b] then [prog1] else [prog2]. *)
  | Meas of Base.qreg * Base.creg
      (** [Meas (qreg, creg)] measures [qreg] in [creg]. *)
  | Gate of Gate.t  (** Gate application. *)
  | SetCReg of Base.creg * Base.pr_int
      (** [SetCReg (creg, i)] sets [creg] to [i]. *)

(** {1:syntactic_sugar Syntactic Sugar} *)

val pvar : string -> t
(** Variable with the given name. *)

val skip : t
(** Skip. *)

val init_qreg : Base.qreg -> t
(** Initialize [qreg]. *)

val seq : t -> t -> t
(** Sequence. *)

val if_else : Base.pr_bool -> t -> t -> t
(** [if_else cond prog1 prog2] is if [cond] then [prog1] else [prog2]. *)

val if_b_then_p : Base.pr_bool -> t -> t
(** [if_b_then_p cond prog] is if [cond] then [prog] else [skip]. *)

val p_for : string -> Base.pr_int -> Base.pr_int -> t -> t
(** [p_for s i1 i2 prog] is for [ivar s] between [i1] and [i2] do [prog] done.
*)

val for_ctrl_prog : Base.qreg * string -> t -> t
(** [for_ctrl_prog (ctrl_qreg, s) prog] is for [ivar s] between [O] and
    [qreg_len qreg] do [prog[ivar s]] controlled by [ctrl_qreg[ivar s]]. *)

val c_for_ctrl_prog : Base.creg * string -> t -> t
(** [for_ctrl_prog (ctrl_creg, s) prog] is for [ivar s] between [O] and
    [creg_len creg] do [prog[ivar s]] controlled by [ctrl_creg[ivar s]]. *)

val meas : Base.qreg -> Base.creg -> t
(** [meas qreg creg] measures [qreg] in [creg]. *)

val apply_gate : 'a -> ('a -> Gate.t) -> t
(** [apply gate qreg] applies [gate] to [qreg]. *)

val set_creg : Base.creg -> Base.pr_int -> t
(** [set_creg creg i] sets [creg] to the value of [i]. *)

val ( -- ) : t -> t -> t
(** {!seq} operator. *)

val ( >> ) : Base.qreg * string -> t -> t
(** {!for_ctrl_prog} operator. *)

(** Module for the classical version of the >> operator. *)
module C : sig
  val ( >> ) : Base.creg * string -> t -> t
  (** {!c_for_ctrl_prog} operator. *)
end

val ( |> ) : 'a -> ('a -> Gate.t) -> t
(** {!apply_gate} operator. *)

val ( => ) : Base.pr_bool -> t -> t
(** {!if_b_then_p} operator. *)

val ( -@ ) : Base.qreg -> Base.creg -> t
(** {!meas} operator. *)

(** {1:operations Operations} *)

val substitute_ivar : string -> Base.pr_int -> t -> t
(** [substitute_ivar var_name i_sub prog] returns a new prog with the pr_int
    variable [var_name] substituted by [i_sub] in [prog]. *)

val inverse_unitary : t -> t option
(** Inverse the given unitary prog. *)

(** {1:comparisons Comparisons} *)

val equal : t -> t -> bool
(** Equality test. *)

(** {1:conversions Conversions} *)

val to_string : t -> string
(** Convert to string. *)

val to_latex : t -> string
(** Convert to LaTeX string. *)

val for_header_to_string : string -> Base.pr_int -> Base.pr_int -> string
(** [for_header_to_string var_name i_start i_end] converts the given parameters
    to a for header string [for var_name = i_start, i_end do], where [var_name]
    is the name of the iterated variable, [i_start] is the start value, and
    [i_end] is the end value. *)

val for_header_to_latex : string -> Base.pr_int -> Base.pr_int -> string
(** [for_header_to_latex var_name i_start i_end] converts the given parameters
    to a for header LaTeX string [for var_name = i_start, i_end do], where
    [var_name] is the name of the iterated variable, [i_start] is the start
    value, and [i_end] is the end value. *)

val for_iteration_to_string : string -> int -> string
(** [for_iteration_to_string var_name i] converts the given parameters to a for
    iteration string [var_name = n], where [var_name] is the name of the
    iterated variable and [i] is its the current value. *)

val for_iteration_to_latex : string -> int -> string
(** [for_iteration_to_latex var_name i] converts the given parameters to a for
    iteration LaTeX string [var_name = n], where [var_name] is the name of the
    iterated variable and [i] is its the current value. *)

val if_cond_to_string : Base.pr_bool -> string
(** Convert the given condition to an if string. *)

val if_cond_to_latex : Base.pr_bool -> string
(** Convert the given condition to an if LaTeX string. *)

val for_unfolding_str : string
(** For [unfolding...] string. *)

val for_done_str : string
(** For [done] string. *)

val else_str : string
(** [else] string. *)

val if_end_str : string
(** If [end] string. *)

(** {1:printing Printing} *)

(** {2:prog_pretty_printers Prog Pretty-Printers} *)

val pp : Format.formatter -> t -> unit
(** Print the given prog on the specified formatter. *)

val print : t -> unit
(** Print the given prog. *)

(** {2:prog_evaluation_traces Prog Evaluation Traces} *)

(** Functions for printing to an output channel with an indentation, using the
    [to_string] functions and [str] values defined in {!conversions}. They are
    used by the {!Evaluator} to display the evaluation steps. *)

val output : ?indent_level:int -> out_channel -> t -> unit
(** Print the given prog to the specified output channel, with an optional
    indentation level (default: 0). *)

val output_for_unfolding : ?indent_level:int -> out_channel -> unit
(** Print for [unfolding...] to the specified output channel, with an optional
    indentation level (default: 0). *)

val output_for_header :
  ?indent_level:int -> out_channel -> string * Base.pr_int * Base.pr_int -> unit
(** Print the given for header to the specified output channel, with an optional
    indentation level (default: 0). *)

val output_for_iteration :
  ?indent_level:int -> out_channel -> string * int -> unit
(** Print the given for iteration to the specified output channel, with an
    optional indentation level (default: 0). *)

val output_if_cond : ?indent_level:int -> out_channel -> Base.pr_bool -> unit
(** Print the given if cond to the specified output channel, with an optional
    indentation level (default: 0). *)

val output_for_done : ?indent_level:int -> out_channel -> unit
(** Print for [done] to the specified output channel, with an optional
    indentation level (default: 0). *)

val output_else : ?indent_level:int -> out_channel -> unit
(** Print [else] to the specified output channel, with an optional indentation
    level (default: 0). *)

val output_if_end : ?indent_level:int -> out_channel -> unit
(** Print if [end] to the specified output channel, with an optional indentation
    level (default: 0). *)

val output_latex : ?indent_level:int -> out_channel -> t -> unit
(** Print the given prog in LaTeX format to the specified output channel, with
    an optional indentation level (default: 0). *)

val output_latex_for_header :
  ?indent_level:int -> out_channel -> string * Base.pr_int * Base.pr_int -> unit
(** Print the given for header in LaTeX format to the specified output channel,
    with an optional indentation level (default: 0). *)

val output_latex_for_iteration :
  ?indent_level:int -> out_channel -> string * int -> unit
(** Print the given for iteration in LaTeX format to the specified output
    channel, with an optional indentation level (default: 0). *)

val output_latex_if_cond :
  ?indent_level:int -> out_channel -> Base.pr_bool -> unit
(** Print the given if cond in LaTeX format to the specified output channel,
    with an optional indentation level (default: 0). *)

val output_latex_for_unfolding : ?indent_level:int -> out_channel -> unit
(** Print for [unfolding...] in LaTeX format to the specified output channel,
    with an optional indentation level (default: 0). *)

val output_latex_for_done : ?indent_level:int -> out_channel -> unit
(** Print for [done] in LaTeX format to the specified output channel, with an
    optional indentation level (default: 0). *)

val output_latex_else : ?indent_level:int -> out_channel -> unit
(** Print [else] in LaTeX format to the specified output channel, with an
    optional indentation level (default: 0). *)

val output_latex_if_end : ?indent_level:int -> out_channel -> unit
(** Print if [end] in LaTeX format to the specified output channel, with an
    optional indentation level (default: 0). *)

(** {1:base_include Base Include} *)

include module type of Base
(** Include {!Base} so it is available through [open Prog]. *)
