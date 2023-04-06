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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe
    Subject:      Entrypoint
*)

let () =
  Alcotest_lwt.run
    ~__FILE__
    "protocol > integration > consensus"
    [
      (Protocol.name ^ ": endorsement", Test_endorsement.tests);
      (Protocol.name ^ ": preendorsement", Test_preendorsement.tests);
      (Protocol.name ^ ": double endorsement", Test_double_endorsement.tests);
      ( Protocol.name ^ ": double preendorsement",
        Test_double_preendorsement.tests );
      (Protocol.name ^ ": double baking", Test_double_baking.tests);
      (Protocol.name ^ ": seed", Test_seed.tests);
      (Protocol.name ^ ": baking", Test_baking.tests);
      (Protocol.name ^ ": delegation", Test_delegation.tests);
      (Protocol.name ^ ": deactivation", Test_deactivation.tests);
      (Protocol.name ^ ": helpers rpcs", Test_helpers_rpcs.tests);
      (Protocol.name ^ ": participation monitoring", Test_participation.tests);
      (Protocol.name ^ ": frozen deposits", Test_frozen_deposits.tests);
      (Protocol.name ^ ": consensus key", Test_consensus_key.tests);
    ]
  |> Lwt_main.run
