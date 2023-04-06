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
    "protocol > unit"
    [
      (Protocol.name ^ ": Tez_repr.ml", Test_tez_repr.tests);
      (Protocol.name ^ ": Contract_repr.ml", Test_contract_repr.tests);
      (Protocol.name ^ ": Destination_repr.ml", Test_destination_repr.tests);
      (Protocol.name ^ ": Operation_repr.ml", Test_operation_repr.tests);
      ( Protocol.name ^ ": Global_constants_storage.ml",
        Test_global_constants_storage.tests );
      (Protocol.name ^ ": fitness", Test_fitness.tests);
      (Protocol.name ^ ": fixed point computation", Test_fixed_point.tests);
      (Protocol.name ^ ": level module", Test_level_module.tests);
      (Protocol.name ^ ": qty", Test_qty.tests);
      (Protocol.name ^ ": round", Test_round_repr.tests);
      (Protocol.name ^ ": time", Test_time_repr.tests);
      (Protocol.name ^ ": receipt encodings", Test_receipt.tests);
      (Protocol.name ^ ": saturation arithmetic", Test_saturation.tests);
      (Protocol.name ^ ": gas monad", Test_gas_monad.tests);
      (Protocol.name ^ ": sc rollup storage", Test_sc_rollup_storage.tests);
      (Protocol.name ^ ": sc rollup game", Test_sc_rollup_game.tests);
      (Protocol.name ^ ": tx rollup l2", Test_tx_rollup_l2.tests);
      (Protocol.name ^ ": tx rollup l2 apply", Test_tx_rollup_l2_apply.tests);
      (Protocol.name ^ ": liquidity baking", Test_liquidity_baking_repr.tests);
      (Protocol.name ^ ": sc rollup wasm", Test_sc_rollup_wasm.tests);
      (Protocol.name ^ ": sc rollup arith", Test_sc_rollup_arith.tests);
      (Protocol.name ^ ": merkle list", Test_merkle_list.tests);
      (Protocol.name ^ ": sc rollup inbox", Test_sc_rollup_inbox.tests);
      (Protocol.name ^ ": skip list", Test_skip_list_repr.tests);
      ( Protocol.name ^ ": sc rollup management protocol",
        Test_sc_rollup_management_protocol.tests );
      (Protocol.name ^ ": Bond_id_repr.ml", Test_bond_id_repr.tests);
      (Protocol.name ^ ": zk rollup storage", Test_zk_rollup_storage.tests);
      (Protocol.name ^ ": Delegate_consensus_key.ml", Test_consensus_key.tests);
      (Protocol.name ^ ": local_contexts", Test_local_contexts.tests);
      (Protocol.name ^ ": dal slot proof", Test_dal_slot_proof.tests);
    ]
  |> Lwt_main.run
