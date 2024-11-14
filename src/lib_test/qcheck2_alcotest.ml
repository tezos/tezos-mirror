(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

let qcheck_wrap ?verbose ?long ?rand =
  List.map (QCheck_alcotest.to_alcotest ?verbose ?long ?rand)

let qcheck_wrap_lwt ?verbose ?long ?rand =
  List.map (fun test ->
      let name, speed, f =
        QCheck_alcotest.to_alcotest ?verbose ?long ?rand test
      in
      (name, speed, fun arg -> Lwt.return (f arg)))
