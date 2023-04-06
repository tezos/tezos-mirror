(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

let () =
  Alcotest_lwt.run
    ~__FILE__
    (Protocol.name ^ " > unit")
    [
      ("Tez_repr.ml", Test_tez_repr.tests);
      ("Contract_repr.ml", Test_contract_repr.tests);
      ("Destination_repr.ml", Test_destination_repr.tests);
      ("Operation_repr.ml", Test_operation_repr.tests);
      ("Global_constants_storage.ml", Test_global_constants_storage.tests);
      ("fitness", Test_fitness.tests);
      ("fixed point computation", Test_fixed_point.tests);
      ("level module", Test_level_module.tests);
      ("qty", Test_qty.tests);
      ("round", Test_round_repr.tests);
      ("time", Test_time_repr.tests);
      ("receipt encodings", Test_receipt.tests);
      ("saturation arithmetic", Test_saturation.tests);
      ("gas monad", Test_gas_monad.tests);
      ("sc rollup storage", Test_sc_rollup_storage.tests);
      ("sc rollup game", Test_sc_rollup_game.tests);
      ("liquidity baking", Test_liquidity_baking_repr.tests);
      ("sc rollup wasm", Test_sc_rollup_wasm.tests);
      ("sc rollup arith", Test_sc_rollup_arith.tests);
      ("merkle list", Test_merkle_list.tests);
      ("sc rollup inbox", Test_sc_rollup_inbox.tests);
      ("skip list", Test_skip_list_repr.tests);
      ("sc rollup management protocol", Test_sc_rollup_management_protocol.tests);
      ("Bond_id_repr.ml", Test_bond_id_repr.tests);
      ("zk rollup storage", Test_zk_rollup_storage.tests);
      ("Delegate_consensus_key.ml", Test_consensus_key.tests);
      ("local_contexts", Test_local_contexts.tests);
      ("dal slot proof", Test_dal_slot_proof.tests);
    ]
  |> Lwt_main.run
