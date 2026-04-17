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
open Verif_handlers

(* Concrete *)
let test_bell_state_00 () =
  let input_hps =
    Hps.(one |> add_qmem ("q0", 0) Hket.zero |> add_qmem ("q1", 0) Hket.zero)
  in
  let spec_hps =
    Hps.(
      one
      |> set_scalar Scalar.sqrt_half
      |> add_qmem ("q0", 0) (Hket.of_var (Y 0))
      |> add_qmem ("q1", 0) (Hket.of_var (Y 0))
      |> add_support [ 0 ])
  in
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.zero |> add ("q1", 0) Hket.zero))
           Hps.(Phase.zero, Scalar.sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(Mem.(empty |> add ("q0", 0) Hket.one |> add ("q1", 0) Hket.one))
           Hps.(Phase.zero, Scalar.sqrt_half))
  in
  Bell_state.verify ~handler:verbose input_hps spec_hps spec_vec_map

let test_bell_state_01 () =
  let input_hps =
    Hps.(one |> add_qmem ("q0", 0) Hket.zero |> add_qmem ("q1", 0) Hket.one)
  in
  let spec_hps =
    Hps.(
      one
      |> set_scalar Scalar.sqrt_half
      |> add_qmem ("q0", 0) (Hket.of_var (Y 0))
      |> add_qmem ("q1", 0) Hket.(xor one (of_var (Y 0)))
      |> add_support [ 0 ])
  in
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.zero |> add ("q1", 0) Hket.one))
           Hps.(Phase.zero, Scalar.sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.one |> add ("q1", 0) Hket.zero))
           Hps.(Phase.zero, Scalar.sqrt_half))
  in
  Bell_state.verify ~handler:verbose input_hps spec_hps spec_vec_map

let test_bell_state_10 () =
  let input_hps =
    Hps.(one |> add_qmem ("q0", 0) Hket.one |> add_qmem ("q1", 0) Hket.zero)
  in
  let spec_hps =
    Hps.(
      one
      |> add_phase [ Y 0 ] (Dyadic1.make Z.one 1)
      |> set_scalar Scalar.sqrt_half
      |> add_qmem ("q0", 0) (Hket.of_var (Y 0))
      |> add_qmem ("q1", 0) (Hket.of_var (Y 0))
      |> add_support [ 0 ])
  in
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.zero |> add ("q1", 0) Hket.zero))
           Hps.(Phase.zero, Scalar.sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(Mem.(empty |> add ("q0", 0) Hket.one |> add ("q1", 0) Hket.one))
           Hps.(Phase.one_half, Scalar.sqrt_half))
  in
  Bell_state.verify ~handler:verbose input_hps spec_hps spec_vec_map

let test_bell_state_11 () =
  let input_hps =
    Hps.(one |> add_qmem ("q0", 0) Hket.one |> add_qmem ("q1", 0) Hket.one)
  in
  let spec_hps =
    Hps.(
      one
      |> add_phase [ Y 0 ] (Dyadic1.make Z.one 1)
      |> set_scalar Scalar.sqrt_half
      |> add_qmem ("q0", 0) (Hket.of_var (Y 0))
      |> add_qmem ("q1", 0) Hket.(xor one (of_var (Y 0)))
      |> add_support [ 0 ])
  in
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.zero |> add ("q1", 0) Hket.one))
           Hps.(Phase.zero, Scalar.sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(empty |> add ("q0", 0) Hket.one |> add ("q1", 0) Hket.zero))
           Hps.(Phase.one_half, Scalar.sqrt_half))
  in
  Bell_state.verify ~handler:verbose input_hps spec_hps spec_vec_map

(* Symbolic *)
let test_bell_state_symbolic () =
  let x0 = Hps.(Hket.of_var (X 0)) in
  let x1 = Hps.(Hket.of_var (X 1)) in
  let input_hps = Hps.(one |> add_qmem ("q0", 0) x0 |> add_qmem ("q1", 0) x1) in
  let y0 = Hps.(Hket.of_var (Y 0)) in
  let spec_hps =
    Hps.(
      one
      |> add_phase [ Y 0; X 0 ] (Dyadic1.make Z.one 1)
      |> set_scalar Scalar.sqrt_half
      |> add_qmem ("q0", 0) y0
      |> add_qmem ("q1", 0) (Hket.xor y0 x1)
      |> add_support [ 0 ])
  in
  let spec_vec_map =
    Concretization.Vector_map.(
      empty
      |> add Hps.Mem_stack.empty
           Hps.(Mem.(empty |> add ("q0", 0) Hket.zero |> add ("q1", 0) x1))
           Hps.(Phase.zero, Scalar.sqrt_half)
      |> add Hps.Mem_stack.empty
           Hps.(
             Mem.(
               empty
               |> add ("q0", 0) Hket.one
               |> add ("q1", 0) Hket.(xor one x1)))
           Hps.
             ( Phase.(zero |> addl [ X 0 ] (Dyadic1.make Z.one 1)),
               Scalar.sqrt_half ))
  in
  Bell_state.verify ~handler:verbose input_hps spec_hps spec_vec_map

(* Tests *)
let () =
  let open Alcotest in
  run "Bell-state"
    [
      ( "Concrete",
        [
          test_case "Bell State 00" `Quick test_bell_state_00;
          test_case "Bell State 01" `Quick test_bell_state_01;
          test_case "Bell State 10" `Quick test_bell_state_10;
          test_case "Bell State 11" `Quick test_bell_state_11;
        ] );
      ( "Symbolic",
        [ test_case "Bell State symbolic" `Quick test_bell_state_symbolic ] );
    ]
