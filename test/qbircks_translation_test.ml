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

open Hqbricks
open Algo_verif

module To_test = struct
  let to_prog = Qbircks.Ast.to_prog
  let of_prog = Qbircks.Ast.of_prog
  let to_openqasm2 = Qbircks.Translation.to_openqasm2
  let of_openqasm2 = Qbircks.Translation.of_openqasm2
  let to_aqasm = Qbircks.Translation.to_aqasm
  let of_aqasm = Qbircks.Translation.of_aqasm
end

let bell_state_oqasm2 =
  "OPENQASM 2.0;\n\
   include \"qelib1.inc\";\n\n\
   qreg q0[1];\n\
   qreg q1[1];\n\n\
   h q0;\n\
   CX q0[0],q1;\n"

let teleportation_oqasm2 =
  "OPENQASM 2.0;\n\
   include \"qelib1.inc\";\n\n\
   qreg alice[1];\n\
   qreg bob[1];\n\
   qreg psi[1];\n\n\
   creg m_psi[1];\n\
   creg m_alice[1];\n\n\
   reset alice;\n\
   reset bob;\n\
   h alice;\n\
   CX alice[0],bob;\n\
   CX psi[0],alice;\n\
   h psi;\n\
   measure psi -> m_psi;\n\
   measure alice -> m_alice;\n\
   if(m_alice==1) x bob;\n\
   if(m_psi==1) z bob;\n"

let qec3_oqasm2 =
  "OPENQASM 2.0;\n\
   include \"qelib1.inc\";\n\n\
   opaque ie(frac) q;\n\n\
   qreg q[2];\n\
   qreg psi[1];\n\
   qreg c[2];\n\n\
   creg mc[2];\n\n\
   reset q;\n\
   CX psi[0],q[0];\n\
   CX q[0],q[1];\n\
   ie(2/3) psi;\n\
   ie(2/3) q;\n\
   reset c;\n\
   h c;\n\
   cz c[0],psi;\n\
   cz c[0],q[0];\n\
   cz c[1],q[0];\n\
   cz c[1],q[1];\n\
   h c;\n\
   measure c -> mc;\n\
   if(mc==3) x q[0];\n\
   if(mc==1) x psi;\n\
   if(mc==2) x q[1];\n"

let qft_oqasm2 =
  "OPENQASM 2.0;\n\
   include \"qelib1.inc\";\n\n\
   qreg q[5];\n\n\
   h q[4];\n\
   crz(pi/4) q[3],q[4];\n\
   crz(pi/8) q[2],q[4];\n\
   crz(pi/16) q[1],q[4];\n\
   crz(pi/32) q[0],q[4];\n\
   h q[3];\n\
   crz(pi/4) q[2],q[3];\n\
   crz(pi/8) q[1],q[3];\n\
   crz(pi/16) q[0],q[3];\n\
   h q[2];\n\
   crz(pi/4) q[1],q[2];\n\
   crz(pi/8) q[0],q[2];\n\
   h q[1];\n\
   crz(pi/4) q[0],q[1];\n\
   h q[0];\n"

let bell_state_aqasm = "BEGIN\nqubits 2\n\nH q[0]\nCTRL(X) q[0],q[1]\n\nEND\n"

let teleportation_aqasm =
  "BEGIN\n\
   qubits 3\n\
   cbits 2\n\n\
   RESET q[0]\n\
   RESET q[1]\n\
   H q[0]\n\
   CTRL(X) q[0],q[1]\n\
   CTRL(X) q[2],q[0]\n\
   H q[2]\n\
   MEAS q[2] c[0]\n\
   MEAS q[0] c[1]\n\
   ? c[1] : X q[1]\n\
   ? c[0] : Z q[1]\n\n\
   END\n"

let qec3_aqasm =
  "DEFINE PARAM IE float : 1\n\n\
   BEGIN\n\
   qubits 5\n\
   cbits 3\n\n\
   RESET q[0],q[1]\n\
   CTRL(X) q[2],q[0]\n\
   CTRL(X) q[0],q[1]\n\
   IE[2/3] q[2]\n\
   IE[2/3] q[0]\n\
   IE[2/3] q[1]\n\
   RESET q[3],q[4]\n\
   H q[3]\n\
   H q[4]\n\
   CTRL(Z) q[3],q[2]\n\
   CTRL(Z) q[3],q[0]\n\
   CTRL(Z) q[4],q[0]\n\
   CTRL(Z) q[4],q[1]\n\
   H q[3]\n\
   H q[4]\n\
   MEAS q[3],q[4] c[0],c[1]\n\
   LOGIC c[2] (c[0] & c[1])\n\
   ? c[2] : X q[0]\n\
   LOGIC c[2] (c[0] & ~c[1])\n\
   ? c[2] : X q[2]\n\
   LOGIC c[2] (~c[0] & c[1])\n\
   ? c[2] : X q[1]\n\n\
   END\n"

let qft_aqasm =
  "BEGIN\n\
   qubits 5\n\n\
   H q[4]\n\
   CTRL(RZ[PI/4]) q[3],q[4]\n\
   CTRL(RZ[PI/8]) q[2],q[4]\n\
   CTRL(RZ[PI/16]) q[1],q[4]\n\
   CTRL(RZ[PI/32]) q[0],q[4]\n\
   H q[3]\n\
   CTRL(RZ[PI/4]) q[2],q[3]\n\
   CTRL(RZ[PI/8]) q[1],q[3]\n\
   CTRL(RZ[PI/16]) q[0],q[3]\n\
   H q[2]\n\
   CTRL(RZ[PI/4]) q[1],q[2]\n\
   CTRL(RZ[PI/8]) q[0],q[2]\n\
   H q[1]\n\
   CTRL(RZ[PI/4]) q[0],q[1]\n\
   H q[0]\n\n\
   END\n"

(* to_prog *)
let test_to_prog exp_prog gate_func_map ir =
  Alcotest.check Prog_helper.prog_testable
    ("to_prog ir:\n" ^ Qbircks.to_string ir)
    exp_prog
    (To_test.to_prog gate_func_map ir)

let test_to_prog_bell_state () =
  test_to_prog Bell_state.bell_state Gate_set_impl.Clifford_k.ir_gate_func_map
    Qbircks_helper.bell_state;
  test_to_prog Bell_state.bell_state Gate_set_impl.Clifford_k.ir_gate_func_map
    Qbircks_helper.bell_state'

let test_to_prog_teleportation () =
  test_to_prog Teleportation.teleportation1
    Gate_set_impl.Clifford_k.ir_gate_func_map Qbircks_helper.teleportation;
  test_to_prog Teleportation.teleportation1
    Gate_set_impl.Clifford_k.ir_gate_func_map Qbircks_helper.teleportation'

let test_to_prog_qec3 () =
  test_to_prog
    (Qec3.qec3' (Hqbricks.Hps.Scalar.SFrac (Z.(~$2), Z.(~$3))))
    Gate_set_impl.Clifford_k_err.ir_gate_func_map
    (Qbircks_helper.qec3 (Z.(~$2), Z.(~$3)));
  test_to_prog
    (Qec3.qec3'' (Hqbricks.Hps.Scalar.SFrac (Z.(~$39), Z.(~$47))))
    Gate_set_impl.Clifford_k_err.ir_gate_func_map
    (Qbircks_helper.qec3' (Z.(~$39), Z.(~$47)))

let test_to_prog_qft () =
  test_to_prog Qft.qft5 Gate_set_impl.Clifford_k.ir_gate_func_map
    Qbircks_helper.qft5;
  test_to_prog Qft.qft5' Gate_set_impl.Clifford_k.ir_gate_func_map
    Qbircks_helper.qft5'

(* of_prog *)
let test_of_prog exp_ir prog =
  Alcotest.check Qbircks_helper.qbircks_testable
    ("of_prog prog:\n" ^ Prog.to_string prog)
    exp_ir (To_test.of_prog prog)

let test_of_prog_bell_state () =
  test_of_prog Qbircks_helper.bell_state Bell_state.bell_state

let test_of_prog_teleportation () =
  test_of_prog Qbircks_helper.teleportation Teleportation.teleportation1

let test_of_prog_qec3 () =
  test_of_prog
    (Qbircks_helper.qec3 (Z.(~$2), Z.(~$3)))
    (Qec3.qec3 (Hqbricks.Hps.Scalar.SFrac (Z.(~$2), Z.(~$3))));
  test_of_prog
    (Qbircks_helper.qec3 (Z.(~$39), Z.(~$47)))
    (Qec3.qec3 (Hqbricks.Hps.Scalar.SFrac (Z.(~$39), Z.(~$47))))

let test_of_prog_qft () = test_of_prog Qbircks_helper.qft5 (Qft.qft 5)

(* to_openqasm2 *)
let test_to_openqasm2 exp_oqasm2 ir =
  Alcotest.(check string)
    ("to_openqasm2 ir:\n" ^ Qbircks.Ast.to_string ir)
    exp_oqasm2 (To_test.to_openqasm2 ir)

let test_to_openqasm2_bell_state () =
  test_to_openqasm2 bell_state_oqasm2 Qbircks_helper.bell_state

let test_to_openqasm2_teleportation () =
  test_to_openqasm2 teleportation_oqasm2 Qbircks_helper.teleportation

let test_to_openqasm2_qec3 () =
  test_to_openqasm2 qec3_oqasm2 (Qbircks_helper.qec3 (Z.(~$2), Z.(~$3)))

let test_to_openqasm2_qft () = test_to_openqasm2 qft_oqasm2 Qbircks_helper.qft5

(* of_openqasm2 *)
let test_of_openqasm2 exp_ir oqasm2 =
  Alcotest.check Qbircks_helper.qbircks_testable
    ("of_openqasm2 oqasm2_str:\n" ^ oqasm2)
    exp_ir
    (To_test.of_openqasm2 oqasm2)

let test_of_openqasm2_bell_state () =
  test_of_openqasm2 Qbircks_helper.bell_state' bell_state_oqasm2

let test_of_openqasm2_teleportation () =
  test_of_openqasm2 Qbircks_helper.teleportation' teleportation_oqasm2

let test_of_openqasm2_qec3 () =
  test_of_openqasm2 (Qbircks_helper.qec3' (Z.(~$2), Z.(~$3))) qec3_oqasm2

let test_of_openqasm2_qft () = test_of_openqasm2 Qbircks_helper.qft5' qft_oqasm2

(* to_aqasm *)
let test_to_aqasm exp_aqasm ir =
  Alcotest.(check string)
    ("to_aqasm ir:\n" ^ Qbircks.Ast.to_string ir)
    exp_aqasm (To_test.to_aqasm ir)

let test_to_aqasm_bell_state () =
  test_to_aqasm bell_state_aqasm Qbircks_helper.bell_state

let test_to_aqasm_teleportation () =
  test_to_aqasm teleportation_aqasm Qbircks_helper.teleportation

let test_to_aqasm_qec3 () =
  test_to_aqasm qec3_aqasm (Qbircks_helper.qec3 (Z.(~$2), Z.(~$3)))

let test_to_aqasm_qft () = test_to_aqasm qft_aqasm Qbircks_helper.qft5

(* of_aqasm *)
let test_of_aqasm exp_ir aqasm =
  Alcotest.check Qbircks_helper.qbircks_testable
    ("of_aqasm aqasm_str:\n" ^ aqasm)
    exp_ir (To_test.of_aqasm aqasm)

let test_of_aqasm_bell_state () =
  test_of_aqasm Qbircks_helper.bell_state'' bell_state_aqasm

let test_of_aqasm_teleportation () =
  test_of_aqasm Qbircks_helper.teleportation'' teleportation_aqasm

let test_of_aqasm_qec3 () =
  test_of_aqasm (Qbircks_helper.qec3'' (Z.(~$2), Z.(~$3))) qec3_aqasm

let test_of_aqasm_qft () = test_of_aqasm Qbircks_helper.qft5'' qft_aqasm

(* Tests *)
let () =
  let open Alcotest in
  run "QbIRcks-translation"
    [
      ( "to_prog",
        [
          test_case "Bell state" `Quick test_to_prog_bell_state;
          test_case "Teleportation" `Quick test_to_prog_teleportation;
          test_case "QEC3" `Quick test_to_prog_qec3;
          test_case "QFT" `Quick test_to_prog_qft;
        ] );
      ( "of_prog",
        [
          test_case "Bell state" `Quick test_of_prog_bell_state;
          test_case "Teleportation" `Quick test_of_prog_teleportation;
          test_case "QEC3" `Quick test_of_prog_qec3;
          test_case "QFT" `Quick test_of_prog_qft;
        ] );
      ( "to_openqasm2",
        [
          test_case "Bell state" `Quick test_to_openqasm2_bell_state;
          test_case "Teleportation" `Quick test_to_openqasm2_teleportation;
          test_case "QEC3" `Quick test_to_openqasm2_qec3;
          test_case "QFT" `Quick test_to_openqasm2_qft;
        ] );
      ( "of_openqasm2",
        [
          test_case "Bell state" `Quick test_of_openqasm2_bell_state;
          test_case "Teleportation" `Quick test_of_openqasm2_teleportation;
          test_case "QEC3" `Quick test_of_openqasm2_qec3;
          test_case "QFT" `Quick test_of_openqasm2_qft;
        ] );
      ( "to_aqasm",
        [
          test_case "Bell state" `Quick test_to_aqasm_bell_state;
          test_case "Teleportation" `Quick test_to_aqasm_teleportation;
          test_case "QEC3" `Quick test_to_aqasm_qec3;
          test_case "QFT" `Quick test_to_aqasm_qft;
        ] );
      ( "of_aqasm",
        [
          test_case "Bell state" `Quick test_of_aqasm_bell_state;
          test_case "Teleportation" `Quick test_of_aqasm_teleportation;
          test_case "QEC3" `Quick test_of_aqasm_qec3;
          test_case "QFT" `Quick test_of_aqasm_qft;
        ] );
    ]
