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
    "protocol > integration > michelson"
    [
      ("global table of constants", Test_global_constants_storage.tests);
      ("interpretation", Test_interpretation.tests);
      ("lazy storage diff", Test_lazy_storage_diff.tests);
      ("sapling", Test_sapling.tests);
      ("script typed ir size", Test_script_typed_ir_size.tests);
      ("temp big maps", Test_temp_big_maps.tests);
      ("ticket balance key", Test_ticket_balance_key.tests);
      ("ticket scanner", Test_ticket_scanner.tests);
      ("ticket storage", Test_ticket_storage.tests);
      ("ticket lazy storage diff", Test_ticket_lazy_storage_diff.tests);
      ("ticket operations diff", Test_ticket_operations_diff.tests);
      ("ticket accounting", Test_ticket_accounting.tests);
      ("ticket balance", Test_ticket_balance.tests);
      ("ticket manager", Test_ticket_manager.tests);
      ("timelock", Test_timelock.tests);
      ("typechecking", Test_typechecking.tests);
      ("script cache", Test_script_cache.tests);
      ("block time instructions", Test_block_time_instructions.tests);
      ("annotations", Test_annotations.tests);
    ]
  |> Lwt_main.run
