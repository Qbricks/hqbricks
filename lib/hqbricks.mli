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

(** HQbricks is a library for the symbolic execution and specification
    verification of hybrid programs (classical/quantum). *)

(** {1:getting_started Getting Started} *)

(** This guide shows how to use the library through a simple Quantum
    Teleportation verification example. It is divided into four parts:
    - {b Implementation} of the Teleportation program.
    - {b Evaluation} of this program into the HPS symbolic representation.
    - {b Verification} of the program, we verify that the input state has been
      correctly teleported into bob.
    - {b Execution} of the full example.

    {2:teleportation-implementation Teleportation Implementation}

    First, we implement the Telportation program using the {!Prog} module for
    program instructions and the {!Gate_set_impl.Clifford_k} module for gates
    (containing the gates H, X, Z, Rz, and SWAP, the library also allows users
    to implement their own gate sets).

    Syntactic sugar from the Prog module is used to simply implement the program
    (-- for sequence, |> for gate application, => for control, -@ for measure,
    ...), and is documented in {!Prog.syntactic_sugar} and
    {{!Prog.Base.base_syntactic_sugar} Base Syntactic Sugar}.

    {[
      (* Program implementation *)
      let teleportation_prog =
        let open Prog in
        let open Gate_set_impl.Clifford_k in
        let psi = qreg "psi" ~$1 in
        let alice = qreg "alice" ~$1 in
        let bob = qreg "bob" ~$1 in
        let m_psi = creg "m_psi" ~$1 in
        let m_alice = creg "m_alice" ~$1 in
        init_qreg alice -- init_qreg bob -- (alice |> h)
        -- (qbit_val alice ~$0 => (bob |> x))
        -- (qbit_val psi ~$0 => (alice |> x))
        -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
        -- (cbit_val m_alice ~$0 => (bob |> x))
        -- (cbit_val m_psi ~$0 => (bob |> z))
    ]}

    {2:teleportation-evaluation Teleportation Evaluation}

    Then, we evaluate the Teleportation program into the HPS symbolic
    representation using the {!Evaluator} module, with a symbolic input HPS
    constructed using the {!Hps} module:
    {math \left\langle 0,1 \cdot\vert x_0\rangle_{\psi} \right\rangle}

    [~rewrite_settings:all_auto] enables automatic rewriting during the
    evaluation, in this example, the {!Rewrite.Change_var} rule is applied
    automatically. Evaluation rewrite settings are documented in
    {!Evaluator.rewrite_settings_flags}, and rewrite rules in the {!Rewrite}
    module.

    [~print:true] enables the display of evaluation steps and rewritings.

    The optional argument [~metrics] is updated in place to record the number of
    gates, measurements, and rewrite rules applied during the evaluation. See
    the {!Metrics} module for more details.

    {[
      (* Symbolic input HPS <0,1.|x0>psi> *)
      let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) 1 0)

      (* Metrics to count gates, measurements, and rewrites. *)
      let metrics = Metrics.create ()

      (* Evaluation of the Teleportation prog into the HPS symbolic
         representation *)
      let hps =
        Evaluator.(
          evaluate_prog teleportation_prog input_hps ~rewrite_settings:all_auto
            ~print:true ~metrics)
    ]}

    {2:teleportation-verification Teleportation verification}

    Finally, we verify the Teleporation program by asserting that the state of
    bob after the symbolic execution is the same as the input state of psi:
    {math
      \texttt{hps}\preccurlyeq_{\texttt{r}}\langle 0,1 \cdot\vert x_0
      \rangle_{\texttt{bob}}\rangle
    }

    [Assertion.hps_satisfies] also automatically uses the {!Rewrite.Discard} and
    thus the {!Rewrite.Fact_distr} rules for the specification verification.

    Assertions are documented in {!Assertion}, and the specification HPS is
    constructed using the {!Hps} module.

    The optional argument [~metrics] is updated in place to increment the count
    of rewrites by the number of automatically applied rewrite rules during the
    verification (see {!Metrics}).

    {[
      (* Specification HPS <0,1.|x0>bob>, we want to verify that the input
         state of psi has been teleported into bob *)
      let spec_hps = Hps.(one |> add_qmem_vec_x ("bob", 0) 1 0)

      (* Specification verification *)
      let () = assert (Assertion.hps_satisfies spec_hps hps ~metrics)
    ]}

    {2:full-example-execution Full Example Execution}

    We will now execute the full Teleportation example:

    {[
      open Hqbricks

      (* Program implementation *)
      let teleportation_prog =
        let open Prog in
        let open Gate_set_impl.Clifford_k in
        let psi = qreg "psi" ~$1 in
        let alice = qreg "alice" ~$1 in
        let bob = qreg "bob" ~$1 in
        let m_psi = creg "m_psi" ~$1 in
        let m_alice = creg "m_alice" ~$1 in
        init_qreg alice -- init_qreg bob -- (alice |> h)
        -- (qbit_val alice ~$0 => (bob |> x))
        -- (qbit_val psi ~$0 => (alice |> x))
        -- (psi |> h) -- (psi -@ m_psi) -- (alice -@ m_alice)
        -- (cbit_val m_alice ~$0 => (bob |> x))
        -- (cbit_val m_psi ~$0 => (bob |> z))

      (* Symbolic input HPS <0,1.|x0>psi> *)
      let input_hps = Hps.(one |> add_qmem_vec_x ("psi", 0) 1 0)

      (* Metrics to count gates, measurements, and rewrites. *)
      let metrics = Metrics.create ()

      (* Evaluation of the Teleportation prog into the HPS symbolic
         representation *)
      let hps =
        Evaluator.(
          evaluate_prog teleportation_prog input_hps ~rewrite_settings:all_auto
            ~print:true ~metrics)

      (* Specification HPS <0,1.|x0>bob>, we want to verify that the input
         state of psi has been teleported into bob *)
      let spec_hps = Hps.(one |> add_qmem_vec_x ("bob", 0) 1 0)

      (* Specification verification *)
      let () = assert (Assertion.hps_satisfies spec_hps hps ~metrics)

      let () =
        print_endline ("Teleportation verified, " ^ Metrics.to_string metrics)
    ]}

    In the output below ("..." parts have been skipped to make it shorter), we
    can see that the {b specification} has been {b verified}, along with the
    associated {b metrics}. We can also see the state of the HPS after the first
    initialization, after the end of the program, and after the {b Change_var}
    rule application:

    {v
      $ dune exec example/teleportation.exe
      init(alice(1));
      <
        0
        ,
        1
        .
        |0>_alice[0] |x[0]>_psi[0]
        [
          _
        ]
      >{}
      ...
      <
        0
        ,
        1 / 2
        .
        |x[0]>_bob[0]
        [
          [y[0] ⊕ x[0]]_m_alice[0] [y[1]]_m_psi[0],
          [y[1]]_m_psi[0],
          _
        ]
      >{y0, y1}
      ...
      change_var applied: changed y0 -> y[0] ⊕ x[0]
      <
        0
        ,
        1 / 2
        .
        |x[0]>_bob[0]
        [
          [y[0]]_m_alice[0] [y[1]]_m_psi[0],
          [y[1]]_m_psi[0],
          _
        ]
      >{y0, y1}
      ...
      Teleportation verified, gates: 6, measures: 2, rewrites: 3
    v} *)

(** {1:program_implementation Program Implementation} *)

module Prog = Prog
module Gate_set_impl = Gate_set_impl

(** {1:hps_symbolic_representation HPS Symbolic Representation} *)

module Hps = Hps
module Rewrite = Rewrite
module Concretization = Concretization

(** {1:program_evaluation Program Evaluation} *)

module Evaluator = Evaluator

(** {1:assertions Assertions} *)

module Assertion = Assertion

(** {1:metrics Metrics} *)

module Metrics = Metrics

(** {1:qbircks_intermediate_representation QbIRcks Intermediate Representation}
*)

module Qbircks = Qbircks

(** {1:utilities Utilities} *)

module Utils = Utils
