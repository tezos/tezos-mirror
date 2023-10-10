(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Tickets, direct spending from implicit accounts
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                   -- --file test_ticket_direct_spending.ml
    Subject:      Test direct spending of tickets from implicit accounts
*)

let test_spending () =
  let open Lwt_result_syntax in
  return_unit

let tests =
  [
    Tztest.tztest
      "Test ticket spending from implicit accounts."
      `Quick
      test_spending;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("ticket direct spending", tests)]
  |> Lwt_main.run
