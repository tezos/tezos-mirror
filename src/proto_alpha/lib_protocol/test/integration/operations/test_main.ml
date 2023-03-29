(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe
    Subject:      Entrypoint
*)

let () =
  Alcotest_lwt.run
    "protocol > integration > operations"
    [
      (Protocol.name ^ ": voting", Test_voting.tests);
      (Protocol.name ^ ": origination", Test_origination.tests);
      (Protocol.name ^ ": revelation", Test_reveal.tests);
      (Protocol.name ^ ": transfer", Test_transfer.tests);
      (Protocol.name ^ ": activation", Test_activation.tests);
      ( Protocol.name ^ ": paid storage increase",
        Test_paid_storage_increase.tests );
      (Protocol.name ^ ": combined", Test_combined_operations.tests);
      (Protocol.name ^ ": failing_noop operation", Test_failing_noop.tests);
      (Protocol.name ^ ": sc rollup", Test_sc_rollup.tests);
      (Protocol.name ^ ": sc rollup transfer", Test_sc_rollup_transfer.tests);
      (Protocol.name ^ ": zk rollup", Test_zk_rollup.tests);
      (Protocol.name ^ ": transfer ticket", Test_transfer_ticket.tests);
    ]
  |> Lwt_main.run
