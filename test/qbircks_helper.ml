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

let qbircks_testable = Alcotest.testable Qbircks.Ast.pp Qbircks.Ast.equal
let h qreg = Qbircks.Gate.{ name = "H"; qreg_params = [ qreg ]; params = [] }
let h' qreg = Qbircks.Gate.{ name = "h"; qreg_params = [ qreg ]; params = [] }

let cx ctrl_qreg qreg =
  Qbircks.Gate.{ name = "CX"; qreg_params = [ ctrl_qreg; qreg ]; params = [] }

let bell_state =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Gate (h (QCons ("q0", Z.(~$1)))),
      Gate (cx (QIndex (("q0", Z.(~$1)), Z.(~$0))) (QCons ("q1", Z.(~$1)))) )

let bell_state' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Gate (h' (QCons ("q0", Z.(~$1)))),
      Gate (cx (QIndex (("q0", Z.(~$1)), Z.(~$0))) (QCons ("q1", Z.(~$1)))) )

let bell_state'' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Gate (h (QIndex (("q", Z.(~$2)), Z.(~$0)))),
      Gate
        (cx
           (QIndex (("q", Z.(~$2)), Z.(~$0)))
           (QIndex (("q", Z.(~$2)), Z.(~$1)))) )

let teleportation =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( InitQReg (QCons ("alice", Z.(~$1))),
                                      InitQReg (QCons ("bob", Z.(~$1))) ),
                                  Gate
                                    {
                                      name = "H";
                                      qreg_params = [ QCons ("alice", Z.(~$1)) ];
                                      params = [];
                                    } ),
                              Gate
                                {
                                  name = "CX";
                                  qreg_params =
                                    [
                                      QIndex (("alice", Z.(~$1)), Z.(~$0));
                                      QCons ("bob", Z.(~$1));
                                    ];
                                  params = [];
                                } ),
                          Gate
                            {
                              name = "CX";
                              qreg_params =
                                [
                                  QIndex (("psi", Z.(~$1)), Z.(~$0));
                                  QCons ("alice", Z.(~$1));
                                ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "H";
                          qreg_params = [ QCons ("psi", Z.(~$1)) ];
                          params = [];
                        } ),
                  Meas (QCons ("psi", Z.(~$1)), CCons ("m_psi", Z.(~$1))) ),
              Meas (QCons ("alice", Z.(~$1)), CCons ("m_alice", Z.(~$1))) ),
          If
            ( CBitVal (("m_alice", Z.(~$1)), Z.(~$0)),
              Gate
                {
                  name = "X";
                  qreg_params = [ QCons ("bob", Z.(~$1)) ];
                  params = [];
                } ) ),
      If
        ( CBitVal (("m_psi", Z.(~$1)), Z.(~$0)),
          Gate
            {
              name = "Z";
              qreg_params = [ QCons ("bob", Z.(~$1)) ];
              params = [];
            } ) )

let teleportation' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( InitQReg (QCons ("alice", Z.(~$1))),
                                      InitQReg (QCons ("bob", Z.(~$1))) ),
                                  Gate
                                    {
                                      name = "h";
                                      qreg_params = [ QCons ("alice", Z.(~$1)) ];
                                      params = [];
                                    } ),
                              Gate
                                {
                                  name = "CX";
                                  qreg_params =
                                    [
                                      QIndex (("alice", Z.(~$1)), Z.(~$0));
                                      QCons ("bob", Z.(~$1));
                                    ];
                                  params = [];
                                } ),
                          Gate
                            {
                              name = "CX";
                              qreg_params =
                                [
                                  QIndex (("psi", Z.(~$1)), Z.(~$0));
                                  QCons ("alice", Z.(~$1));
                                ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "h";
                          qreg_params = [ QCons ("psi", Z.(~$1)) ];
                          params = [];
                        } ),
                  Meas (QCons ("psi", Z.(~$1)), CCons ("m_psi", Z.(~$1))) ),
              Meas (QCons ("alice", Z.(~$1)), CCons ("m_alice", Z.(~$1))) ),
          If
            ( CBitVal (("m_alice", Z.(~$1)), Z.(~$0)),
              Gate
                {
                  name = "x";
                  qreg_params = [ QCons ("bob", Z.(~$1)) ];
                  params = [];
                } ) ),
      If
        ( CBitVal (("m_psi", Z.(~$1)), Z.(~$0)),
          Gate
            {
              name = "z";
              qreg_params = [ QCons ("bob", Z.(~$1)) ];
              params = [];
            } ) )

let teleportation'' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( InitQReg (QIndex (("q", Z.(~$3)), Z.(~$0))),
                                      InitQReg
                                        (QIndex (("q", Z.(~$3)), Z.(~$1))) ),
                                  Gate
                                    {
                                      name = "H";
                                      qreg_params =
                                        [ QIndex (("q", Z.(~$3)), Z.(~$0)) ];
                                      params = [];
                                    } ),
                              Gate
                                {
                                  name = "CX";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$3)), Z.(~$0));
                                      QIndex (("q", Z.(~$3)), Z.(~$1));
                                    ];
                                  params = [];
                                } ),
                          Gate
                            {
                              name = "CX";
                              qreg_params =
                                [
                                  QIndex (("q", Z.(~$3)), Z.(~$2));
                                  QIndex (("q", Z.(~$3)), Z.(~$0));
                                ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "H";
                          qreg_params = [ QIndex (("q", Z.(~$3)), Z.(~$2)) ];
                          params = [];
                        } ),
                  Meas
                    ( QIndex (("q", Z.(~$3)), Z.(~$2)),
                      CIndex (("c", Z.(~$2)), Z.(~$0)) ) ),
              Meas
                ( QIndex (("q", Z.(~$3)), Z.(~$0)),
                  CIndex (("c", Z.(~$2)), Z.(~$1)) ) ),
          If
            ( CBitVal (("c", Z.(~$2)), Z.(~$1)),
              Gate
                {
                  name = "X";
                  qreg_params = [ QIndex (("q", Z.(~$3)), Z.(~$1)) ];
                  params = [];
                } ) ),
      If
        ( CBitVal (("c", Z.(~$2)), Z.(~$0)),
          Gate
            {
              name = "Z";
              qreg_params = [ QIndex (("q", Z.(~$3)), Z.(~$1)) ];
              params = [];
            } ) )

let qec3 p =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( Seq
                                        ( Seq
                                            ( Seq
                                                ( Seq
                                                    ( InitQReg
                                                        (QCons ("q", Z.(~$2))),
                                                      Gate
                                                        {
                                                          name = "CX";
                                                          qreg_params =
                                                            [
                                                              QIndex
                                                                ( ( "psi",
                                                                    Z.(~$1) ),
                                                                  Z.(~$0) );
                                                              QIndex
                                                                ( ("q", Z.(~$2)),
                                                                  Z.(~$0) );
                                                            ];
                                                          params = [];
                                                        } ),
                                                  Gate
                                                    {
                                                      name = "CX";
                                                      qreg_params =
                                                        [
                                                          QIndex
                                                            ( ("q", Z.(~$2)),
                                                              Z.(~$0) );
                                                          QIndex
                                                            ( ("q", Z.(~$2)),
                                                              Z.(~$1) );
                                                        ];
                                                      params = [];
                                                    } ),
                                              Gate
                                                {
                                                  name = "IE";
                                                  qreg_params =
                                                    [ QCons ("psi", Z.(~$1)) ];
                                                  params = [ Scalar p ];
                                                } ),
                                          Gate
                                            {
                                              name = "IE";
                                              qreg_params =
                                                [ QCons ("q", Z.(~$2)) ];
                                              params = [ Scalar p ];
                                            } ),
                                      InitQReg (QCons ("c", Z.(~$2))) ),
                                  Gate
                                    {
                                      name = "H";
                                      qreg_params = [ QCons ("c", Z.(~$2)) ];
                                      params = [];
                                    } ),
                              Seq
                                ( Gate
                                    {
                                      name = "CZ";
                                      qreg_params =
                                        [
                                          QIndex (("c", Z.(~$2)), Z.(~$0));
                                          QCons ("psi", Z.(~$1));
                                        ];
                                      params = [];
                                    },
                                  Gate
                                    {
                                      name = "CZ";
                                      qreg_params =
                                        [
                                          QIndex (("c", Z.(~$2)), Z.(~$0));
                                          QIndex (("q", Z.(~$2)), Z.(~$0));
                                        ];
                                      params = [];
                                    } ) ),
                          Seq
                            ( Gate
                                {
                                  name = "CZ";
                                  qreg_params =
                                    [
                                      QIndex (("c", Z.(~$2)), Z.(~$1));
                                      QIndex (("q", Z.(~$2)), Z.(~$0));
                                    ];
                                  params = [];
                                },
                              Gate
                                {
                                  name = "CZ";
                                  qreg_params =
                                    [
                                      QIndex (("c", Z.(~$2)), Z.(~$1));
                                      QIndex (("q", Z.(~$2)), Z.(~$1));
                                    ];
                                  params = [];
                                } ) ),
                      Gate
                        {
                          name = "H";
                          qreg_params = [ QCons ("c", Z.(~$2)) ];
                          params = [];
                        } ),
                  Meas (QCons ("c", Z.(~$2)), CCons ("mc", Z.(~$2))) ),
              If
                ( And
                    ( CBitVal (("mc", Z.(~$2)), Z.(~$0)),
                      CBitVal (("mc", Z.(~$2)), Z.(~$1)) ),
                  Gate
                    {
                      name = "X";
                      qreg_params = [ QIndex (("q", Z.(~$2)), Z.(~$0)) ];
                      params = [];
                    } ) ),
          If
            ( And
                ( CBitVal (("mc", Z.(~$2)), Z.(~$0)),
                  Not (CBitVal (("mc", Z.(~$2)), Z.(~$1))) ),
              Gate
                {
                  name = "X";
                  qreg_params = [ QCons ("psi", Z.(~$1)) ];
                  params = [];
                } ) ),
      If
        ( And
            ( Not (CBitVal (("mc", Z.(~$2)), Z.(~$0))),
              CBitVal (("mc", Z.(~$2)), Z.(~$1)) ),
          Gate
            {
              name = "X";
              qreg_params = [ QIndex (("q", Z.(~$2)), Z.(~$1)) ];
              params = [];
            } ) )

let qec3' p =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( Seq
                                        ( Seq
                                            ( Seq
                                                ( Seq
                                                    ( Seq
                                                        ( Seq
                                                            ( InitQReg
                                                                (QCons
                                                                   ("q", Z.(~$2))),
                                                              Gate
                                                                {
                                                                  name = "CX";
                                                                  qreg_params =
                                                                    [
                                                                      QIndex
                                                                        ( ( "psi",
                                                                            Z.(
                                                                              ~$1)
                                                                          ),
                                                                          Z.(
                                                                            ~$0)
                                                                        );
                                                                      QIndex
                                                                        ( ( "q",
                                                                            Z.(
                                                                              ~$2)
                                                                          ),
                                                                          Z.(
                                                                            ~$0)
                                                                        );
                                                                    ];
                                                                  params = [];
                                                                } ),
                                                          Gate
                                                            {
                                                              name = "CX";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$2)
                                                                      ),
                                                                      Z.(~$0) );
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$2)
                                                                      ),
                                                                      Z.(~$1) );
                                                                ];
                                                              params = [];
                                                            } ),
                                                      Gate
                                                        {
                                                          name = "ie";
                                                          qreg_params =
                                                            [
                                                              QCons
                                                                ("psi", Z.(~$1));
                                                            ];
                                                          params = [ Scalar p ];
                                                        } ),
                                                  Gate
                                                    {
                                                      name = "ie";
                                                      qreg_params =
                                                        [ QCons ("q", Z.(~$2)) ];
                                                      params = [ Scalar p ];
                                                    } ),
                                              InitQReg (QCons ("c", Z.(~$2))) ),
                                          Gate
                                            {
                                              name = "h";
                                              qreg_params =
                                                [ QCons ("c", Z.(~$2)) ];
                                              params = [];
                                            } ),
                                      Gate
                                        {
                                          name = "cz";
                                          qreg_params =
                                            [
                                              QIndex (("c", Z.(~$2)), Z.(~$0));
                                              QCons ("psi", Z.(~$1));
                                            ];
                                          params = [];
                                        } ),
                                  Gate
                                    {
                                      name = "cz";
                                      qreg_params =
                                        [
                                          QIndex (("c", Z.(~$2)), Z.(~$0));
                                          QIndex (("q", Z.(~$2)), Z.(~$0));
                                        ];
                                      params = [];
                                    } ),
                              Gate
                                {
                                  name = "cz";
                                  qreg_params =
                                    [
                                      QIndex (("c", Z.(~$2)), Z.(~$1));
                                      QIndex (("q", Z.(~$2)), Z.(~$0));
                                    ];
                                  params = [];
                                } ),
                          Gate
                            {
                              name = "cz";
                              qreg_params =
                                [
                                  QIndex (("c", Z.(~$2)), Z.(~$1));
                                  QIndex (("q", Z.(~$2)), Z.(~$1));
                                ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "h";
                          qreg_params = [ QCons ("c", Z.(~$2)) ];
                          params = [];
                        } ),
                  Meas (QCons ("c", Z.(~$2)), CCons ("mc", Z.(~$2))) ),
              If
                ( And
                    ( CBitVal (("mc", Z.(~$2)), Z.(~$0)),
                      CBitVal (("mc", Z.(~$2)), Z.(~$1)) ),
                  Gate
                    {
                      name = "x";
                      qreg_params = [ QIndex (("q", Z.(~$2)), Z.(~$0)) ];
                      params = [];
                    } ) ),
          If
            ( And
                ( CBitVal (("mc", Z.(~$2)), Z.(~$0)),
                  Not (CBitVal (("mc", Z.(~$2)), Z.(~$1))) ),
              Gate
                {
                  name = "x";
                  qreg_params = [ QCons ("psi", Z.(~$1)) ];
                  params = [];
                } ) ),
      If
        ( And
            ( Not (CBitVal (("mc", Z.(~$2)), Z.(~$0))),
              CBitVal (("mc", Z.(~$2)), Z.(~$1)) ),
          Gate
            {
              name = "x";
              qreg_params = [ QIndex (("q", Z.(~$2)), Z.(~$1)) ];
              params = [];
            } ) )

let qec3'' p =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( Seq
                                        ( Seq
                                            ( Seq
                                                ( Seq
                                                    ( Seq
                                                        ( Seq
                                                            ( Seq
                                                                ( Seq
                                                                    ( Seq
                                                                        ( Seq
                                                                            ( InitQReg
                                                                                (
                                                                                QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$0)
                                                                                )),
                                                                              InitQReg
                                                                                (
                                                                                QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$1)
                                                                                ))
                                                                            ),
                                                                          Gate
                                                                            {
                                                                              name =
                                                                                "CX";
                                                                              qreg_params =
                                                                                [
                                                                                QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$2)
                                                                                );
                                                                                QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$0)
                                                                                );
                                                                                ];
                                                                              params =
                                                                                [];
                                                                            } ),
                                                                      Gate
                                                                        {
                                                                          name =
                                                                            "CX";
                                                                          qreg_params =
                                                                            [
                                                                              QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$0)
                                                                                );
                                                                              QIndex
                                                                                ( 
                                                                                ( 
                                                                                "q",
                                                                                Z.(
                                                                                ~$5)
                                                                                ),
                                                                                Z.(
                                                                                ~$1)
                                                                                );
                                                                            ];
                                                                          params =
                                                                            [];
                                                                        } ),
                                                                  Gate
                                                                    {
                                                                      name =
                                                                        "IE";
                                                                      qreg_params =
                                                                        [
                                                                          QIndex
                                                                            ( ( "q",
                                                                                Z.(
                                                                                ~$5)
                                                                              ),
                                                                              Z.(
                                                                                ~$2)
                                                                            );
                                                                        ];
                                                                      params =
                                                                        [
                                                                          Scalar
                                                                            ( Z.(
                                                                                ~$2),
                                                                              Z.(
                                                                                ~$3)
                                                                            );
                                                                        ];
                                                                    } ),
                                                              Gate
                                                                {
                                                                  name = "IE";
                                                                  qreg_params =
                                                                    [
                                                                      QIndex
                                                                        ( ( "q",
                                                                            Z.(
                                                                              ~$5)
                                                                          ),
                                                                          Z.(
                                                                            ~$0)
                                                                        );
                                                                    ];
                                                                  params =
                                                                    [ Scalar p ];
                                                                } ),
                                                          Gate
                                                            {
                                                              name = "IE";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$1) );
                                                                ];
                                                              params =
                                                                [ Scalar p ];
                                                            } ),
                                                      Seq
                                                        ( InitQReg
                                                            (QIndex
                                                               ( ("q", Z.(~$5)),
                                                                 Z.(~$3) )),
                                                          InitQReg
                                                            (QIndex
                                                               ( ("q", Z.(~$5)),
                                                                 Z.(~$4) )) ) ),
                                                  Gate
                                                    {
                                                      name = "H";
                                                      qreg_params =
                                                        [
                                                          QIndex
                                                            ( ("q", Z.(~$5)),
                                                              Z.(~$3) );
                                                        ];
                                                      params = [];
                                                    } ),
                                              Gate
                                                {
                                                  name = "H";
                                                  qreg_params =
                                                    [
                                                      QIndex
                                                        (("q", Z.(~$5)), Z.(~$4));
                                                    ];
                                                  params = [];
                                                } ),
                                          Gate
                                            {
                                              name = "CZ";
                                              qreg_params =
                                                [
                                                  QIndex
                                                    (("q", Z.(~$5)), Z.(~$3));
                                                  QIndex
                                                    (("q", Z.(~$5)), Z.(~$2));
                                                ];
                                              params = [];
                                            } ),
                                      Gate
                                        {
                                          name = "CZ";
                                          qreg_params =
                                            [
                                              QIndex (("q", Z.(~$5)), Z.(~$3));
                                              QIndex (("q", Z.(~$5)), Z.(~$0));
                                            ];
                                          params = [];
                                        } ),
                                  Gate
                                    {
                                      name = "CZ";
                                      qreg_params =
                                        [
                                          QIndex (("q", Z.(~$5)), Z.(~$4));
                                          QIndex (("q", Z.(~$5)), Z.(~$0));
                                        ];
                                      params = [];
                                    } ),
                              Gate
                                {
                                  name = "CZ";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$4));
                                      QIndex (("q", Z.(~$5)), Z.(~$1));
                                    ];
                                  params = [];
                                } ),
                          Gate
                            {
                              name = "H";
                              qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$3)) ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "H";
                          qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$4)) ];
                          params = [];
                        } ),
                  Seq
                    ( Meas
                        ( QIndex (("q", Z.(~$5)), Z.(~$3)),
                          CIndex (("c", Z.(~$3)), Z.(~$0)) ),
                      Meas
                        ( QIndex (("q", Z.(~$5)), Z.(~$4)),
                          CIndex (("c", Z.(~$3)), Z.(~$1)) ) ) ),
              If
                ( And
                    ( CBitVal (("c", Z.(~$3)), Z.(~$0)),
                      CBitVal (("c", Z.(~$3)), Z.(~$1)) ),
                  Gate
                    {
                      name = "X";
                      qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$0)) ];
                      params = [];
                    } ) ),
          If
            ( And
                ( CBitVal (("c", Z.(~$3)), Z.(~$0)),
                  Not (CBitVal (("c", Z.(~$3)), Z.(~$1))) ),
              Gate
                {
                  name = "X";
                  qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$2)) ];
                  params = [];
                } ) ),
      If
        ( And
            ( Not (CBitVal (("c", Z.(~$3)), Z.(~$0))),
              CBitVal (("c", Z.(~$3)), Z.(~$1)) ),
          Gate
            {
              name = "X";
              qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$1)) ];
              params = [];
            } ) )

let qft5 =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Gate
                        {
                          name = "H";
                          qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$4)) ];
                          params = [];
                        },
                      Seq
                        ( Seq
                            ( Seq
                                ( Gate
                                    {
                                      name = "CRZ";
                                      qreg_params =
                                        [
                                          QIndex (("q", Z.(~$5)), Z.(~$3));
                                          QIndex (("q", Z.(~$5)), Z.(~$4));
                                        ];
                                      params = [ Angle Z.(~$2) ];
                                    },
                                  Gate
                                    {
                                      name = "CRZ";
                                      qreg_params =
                                        [
                                          QIndex (("q", Z.(~$5)), Z.(~$2));
                                          QIndex (("q", Z.(~$5)), Z.(~$4));
                                        ];
                                      params = [ Angle Z.(~$3) ];
                                    } ),
                              Gate
                                {
                                  name = "CRZ";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$1));
                                      QIndex (("q", Z.(~$5)), Z.(~$4));
                                    ];
                                  params = [ Angle Z.(~$4) ];
                                } ),
                          Gate
                            {
                              name = "CRZ";
                              qreg_params =
                                [
                                  QIndex (("q", Z.(~$5)), Z.(~$0));
                                  QIndex (("q", Z.(~$5)), Z.(~$4));
                                ];
                              params = [ Angle Z.(~$5) ];
                            } ) ),
                  Seq
                    ( Gate
                        {
                          name = "H";
                          qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$3)) ];
                          params = [];
                        },
                      Seq
                        ( Seq
                            ( Gate
                                {
                                  name = "CRZ";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$2));
                                      QIndex (("q", Z.(~$5)), Z.(~$3));
                                    ];
                                  params = [ Angle Z.(~$2) ];
                                },
                              Gate
                                {
                                  name = "CRZ";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$1));
                                      QIndex (("q", Z.(~$5)), Z.(~$3));
                                    ];
                                  params = [ Angle Z.(~$3) ];
                                } ),
                          Gate
                            {
                              name = "CRZ";
                              qreg_params =
                                [
                                  QIndex (("q", Z.(~$5)), Z.(~$0));
                                  QIndex (("q", Z.(~$5)), Z.(~$3));
                                ];
                              params = [ Angle Z.(~$4) ];
                            } ) ) ),
              Seq
                ( Gate
                    {
                      name = "H";
                      qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$2)) ];
                      params = [];
                    },
                  Seq
                    ( Gate
                        {
                          name = "CRZ";
                          qreg_params =
                            [
                              QIndex (("q", Z.(~$5)), Z.(~$1));
                              QIndex (("q", Z.(~$5)), Z.(~$2));
                            ];
                          params = [ Angle Z.(~$2) ];
                        },
                      Gate
                        {
                          name = "CRZ";
                          qreg_params =
                            [
                              QIndex (("q", Z.(~$5)), Z.(~$0));
                              QIndex (("q", Z.(~$5)), Z.(~$2));
                            ];
                          params = [ Angle Z.(~$3) ];
                        } ) ) ),
          Seq
            ( Gate
                {
                  name = "H";
                  qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$1)) ];
                  params = [];
                },
              Gate
                {
                  name = "CRZ";
                  qreg_params =
                    [
                      QIndex (("q", Z.(~$5)), Z.(~$0));
                      QIndex (("q", Z.(~$5)), Z.(~$1));
                    ];
                  params = [ Angle Z.(~$2) ];
                } ) ),
      Seq
        ( Gate
            {
              name = "H";
              qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$0)) ];
              params = [];
            },
          Skip ) )

let qft5' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( Seq
                                        ( Seq
                                            ( Seq
                                                ( Seq
                                                    ( Seq
                                                        ( Gate
                                                            {
                                                              name = "h";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$4) );
                                                                ];
                                                              params = [];
                                                            },
                                                          Gate
                                                            {
                                                              name = "crz";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$3) );
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$4) );
                                                                ];
                                                              params =
                                                                [
                                                                  Angle Z.(~$2);
                                                                ];
                                                            } ),
                                                      Gate
                                                        {
                                                          name = "crz";
                                                          qreg_params =
                                                            [
                                                              QIndex
                                                                ( ("q", Z.(~$5)),
                                                                  Z.(~$2) );
                                                              QIndex
                                                                ( ("q", Z.(~$5)),
                                                                  Z.(~$4) );
                                                            ];
                                                          params =
                                                            [ Angle Z.(~$3) ];
                                                        } ),
                                                  Gate
                                                    {
                                                      name = "crz";
                                                      qreg_params =
                                                        [
                                                          QIndex
                                                            ( ("q", Z.(~$5)),
                                                              Z.(~$1) );
                                                          QIndex
                                                            ( ("q", Z.(~$5)),
                                                              Z.(~$4) );
                                                        ];
                                                      params = [ Angle Z.(~$4) ];
                                                    } ),
                                              Gate
                                                {
                                                  name = "crz";
                                                  qreg_params =
                                                    [
                                                      QIndex
                                                        (("q", Z.(~$5)), Z.(~$0));
                                                      QIndex
                                                        (("q", Z.(~$5)), Z.(~$4));
                                                    ];
                                                  params = [ Angle Z.(~$5) ];
                                                } ),
                                          Gate
                                            {
                                              name = "h";
                                              qreg_params =
                                                [
                                                  QIndex
                                                    (("q", Z.(~$5)), Z.(~$3));
                                                ];
                                              params = [];
                                            } ),
                                      Gate
                                        {
                                          name = "crz";
                                          qreg_params =
                                            [
                                              QIndex (("q", Z.(~$5)), Z.(~$2));
                                              QIndex (("q", Z.(~$5)), Z.(~$3));
                                            ];
                                          params = [ Angle Z.(~$2) ];
                                        } ),
                                  Gate
                                    {
                                      name = "crz";
                                      qreg_params =
                                        [
                                          QIndex (("q", Z.(~$5)), Z.(~$1));
                                          QIndex (("q", Z.(~$5)), Z.(~$3));
                                        ];
                                      params = [ Angle Z.(~$3) ];
                                    } ),
                              Gate
                                {
                                  name = "crz";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$0));
                                      QIndex (("q", Z.(~$5)), Z.(~$3));
                                    ];
                                  params = [ Angle Z.(~$4) ];
                                } ),
                          Gate
                            {
                              name = "h";
                              qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$2)) ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "crz";
                          qreg_params =
                            [
                              QIndex (("q", Z.(~$5)), Z.(~$1));
                              QIndex (("q", Z.(~$5)), Z.(~$2));
                            ];
                          params = [ Angle Z.(~$2) ];
                        } ),
                  Gate
                    {
                      name = "crz";
                      qreg_params =
                        [
                          QIndex (("q", Z.(~$5)), Z.(~$0));
                          QIndex (("q", Z.(~$5)), Z.(~$2));
                        ];
                      params = [ Angle Z.(~$3) ];
                    } ),
              Gate
                {
                  name = "h";
                  qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$1)) ];
                  params = [];
                } ),
          Gate
            {
              name = "crz";
              qreg_params =
                [
                  QIndex (("q", Z.(~$5)), Z.(~$0));
                  QIndex (("q", Z.(~$5)), Z.(~$1));
                ];
              params = [ Angle Z.(~$2) ];
            } ),
      Gate
        {
          name = "h";
          qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$0)) ];
          params = [];
        } )

let qft5'' =
  let open Qbircks.Ast in
  let open Qbircks.Base in
  Seq
    ( Seq
        ( Seq
            ( Seq
                ( Seq
                    ( Seq
                        ( Seq
                            ( Seq
                                ( Seq
                                    ( Seq
                                        ( Seq
                                            ( Seq
                                                ( Seq
                                                    ( Seq
                                                        ( Gate
                                                            {
                                                              name = "H";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$4) );
                                                                ];
                                                              params = [];
                                                            },
                                                          Gate
                                                            {
                                                              name = "CRZ";
                                                              qreg_params =
                                                                [
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$3) );
                                                                  QIndex
                                                                    ( ( "q",
                                                                        Z.(~$5)
                                                                      ),
                                                                      Z.(~$4) );
                                                                ];
                                                              params =
                                                                [
                                                                  Angle Z.(~$2);
                                                                ];
                                                            } ),
                                                      Gate
                                                        {
                                                          name = "CRZ";
                                                          qreg_params =
                                                            [
                                                              QIndex
                                                                ( ("q", Z.(~$5)),
                                                                  Z.(~$2) );
                                                              QIndex
                                                                ( ("q", Z.(~$5)),
                                                                  Z.(~$4) );
                                                            ];
                                                          params =
                                                            [ Angle Z.(~$3) ];
                                                        } ),
                                                  Gate
                                                    {
                                                      name = "CRZ";
                                                      qreg_params =
                                                        [
                                                          QIndex
                                                            ( ("q", Z.(~$5)),
                                                              Z.(~$1) );
                                                          QIndex
                                                            ( ("q", Z.(~$5)),
                                                              Z.(~$4) );
                                                        ];
                                                      params = [ Angle Z.(~$4) ];
                                                    } ),
                                              Gate
                                                {
                                                  name = "CRZ";
                                                  qreg_params =
                                                    [
                                                      QIndex
                                                        (("q", Z.(~$5)), Z.(~$0));
                                                      QIndex
                                                        (("q", Z.(~$5)), Z.(~$4));
                                                    ];
                                                  params = [ Angle Z.(~$5) ];
                                                } ),
                                          Gate
                                            {
                                              name = "H";
                                              qreg_params =
                                                [
                                                  QIndex
                                                    (("q", Z.(~$5)), Z.(~$3));
                                                ];
                                              params = [];
                                            } ),
                                      Gate
                                        {
                                          name = "CRZ";
                                          qreg_params =
                                            [
                                              QIndex (("q", Z.(~$5)), Z.(~$2));
                                              QIndex (("q", Z.(~$5)), Z.(~$3));
                                            ];
                                          params = [ Angle Z.(~$2) ];
                                        } ),
                                  Gate
                                    {
                                      name = "CRZ";
                                      qreg_params =
                                        [
                                          QIndex (("q", Z.(~$5)), Z.(~$1));
                                          QIndex (("q", Z.(~$5)), Z.(~$3));
                                        ];
                                      params = [ Angle Z.(~$3) ];
                                    } ),
                              Gate
                                {
                                  name = "CRZ";
                                  qreg_params =
                                    [
                                      QIndex (("q", Z.(~$5)), Z.(~$0));
                                      QIndex (("q", Z.(~$5)), Z.(~$3));
                                    ];
                                  params = [ Angle Z.(~$4) ];
                                } ),
                          Gate
                            {
                              name = "H";
                              qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$2)) ];
                              params = [];
                            } ),
                      Gate
                        {
                          name = "CRZ";
                          qreg_params =
                            [
                              QIndex (("q", Z.(~$5)), Z.(~$1));
                              QIndex (("q", Z.(~$5)), Z.(~$2));
                            ];
                          params = [ Angle Z.(~$2) ];
                        } ),
                  Gate
                    {
                      name = "CRZ";
                      qreg_params =
                        [
                          QIndex (("q", Z.(~$5)), Z.(~$0));
                          QIndex (("q", Z.(~$5)), Z.(~$2));
                        ];
                      params = [ Angle Z.(~$3) ];
                    } ),
              Gate
                {
                  name = "H";
                  qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$1)) ];
                  params = [];
                } ),
          Gate
            {
              name = "CRZ";
              qreg_params =
                [
                  QIndex (("q", Z.(~$5)), Z.(~$0));
                  QIndex (("q", Z.(~$5)), Z.(~$1));
                ];
              params = [ Angle Z.(~$2) ];
            } ),
      Gate
        {
          name = "H";
          qreg_params = [ QIndex (("q", Z.(~$5)), Z.(~$0)) ];
          params = [];
        } )
