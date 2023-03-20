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
    Invocation:   dune runtest src/proto_alpha/lib_protocol/test/integration/michelson
    Subject:      Integration > Michelson
*)

let () =
  Alcotest_lwt.run
    ~__FILE__
    "protocol > integration > michelson"
    [
      ( Protocol.name ^ ": global table of constants",
        Test_global_constants_storage.tests );
      (Protocol.name ^ ": interpretation", Test_interpretation.tests);
      (Protocol.name ^ ": lazy storage diff", Test_lazy_storage_diff.tests);
      (Protocol.name ^ ": sapling", Test_sapling.tests);
      (Protocol.name ^ ": script typed ir size", Test_script_typed_ir_size.tests);
      (Protocol.name ^ ": temp big maps", Test_temp_big_maps.tests);
      (Protocol.name ^ ": ticket balance key", Test_ticket_balance_key.tests);
      (Protocol.name ^ ": ticket scanner", Test_ticket_scanner.tests);
      (Protocol.name ^ ": ticket storage", Test_ticket_storage.tests);
      ( Protocol.name ^ ": ticket lazy storage diff",
        Test_ticket_lazy_storage_diff.tests );
      ( Protocol.name ^ ": ticket operations diff",
        Test_ticket_operations_diff.tests );
      (Protocol.name ^ ": ticket accounting", Test_ticket_accounting.tests);
      (Protocol.name ^ ": ticket balance", Test_ticket_balance.tests);
      (Protocol.name ^ ": ticket manager", Test_ticket_manager.tests);
      (Protocol.name ^ ": typechecking", Test_typechecking.tests);
      (Protocol.name ^ ": script cache", Test_script_cache.tests);
      ( Protocol.name ^ ": block time instructions",
        Test_block_time_instructions.tests );
      (Protocol.name ^ ": annotations", Test_annotations.tests);
      (Protocol.name ^ ": event logging", Test_contract_event.tests);
      (Protocol.name ^ ": patched contracts", Test_patched_contracts.tests);
      (Protocol.name ^ ": lambda normalization", Test_lambda_normalization.tests);
    ]
  |> Lwt_main.run
