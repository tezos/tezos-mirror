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
module Unit_test : sig
  (**
   * Example: [spec "Alpha_context.ml" Test_alpha_context.test_cases]
   * Unit tests needs tag in log (like "[UNIT] some test description here...")
   * This function handles such meta data *)
  val spec :
    string ->
    unit Alcotest_lwt.test_case list ->
    string * unit Alcotest_lwt.test_case list

  (** Tests with description string without [Unit] are skipped *)
  val skip :
    string ->
    unit Alcotest_lwt.test_case list ->
    string * unit Alcotest_lwt.test_case list
end = struct
  let spec unit_name test_cases = ("[Unit] " ^ unit_name, test_cases)

  let skip unit_name test_cases = ("[SKIPPED] " ^ unit_name, test_cases)
end

let () =
  Alcotest_lwt.run
    "protocol > unit"
    [
      Unit_test.spec (Protocol.name ^ ": Tez_repr.ml") Test_tez_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": Contract_repr.ml")
        Test_contract_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": Destination_repr.ml")
        Test_destination_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": Operation_repr.ml")
        Test_operation_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": Global_constants_storage.ml")
        Test_global_constants_storage.tests;
      Unit_test.spec (Protocol.name ^ ": fitness") Test_fitness.tests;
      Unit_test.spec
        (Protocol.name ^ ": fixed point computation")
        Test_fixed_point.tests;
      Unit_test.spec (Protocol.name ^ ": level module") Test_level_module.tests;
      Unit_test.spec (Protocol.name ^ ": qty") Test_qty.tests;
      Unit_test.spec (Protocol.name ^ ": round") Test_round_repr.tests;
      Unit_test.spec (Protocol.name ^ ": time") Test_time_repr.tests;
      Unit_test.spec (Protocol.name ^ ": receipt encodings") Test_receipt.tests;
      Unit_test.spec
        (Protocol.name ^ ": saturation arithmetic")
        Test_saturation.tests;
      Unit_test.spec (Protocol.name ^ ": gas monad") Test_gas_monad.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup storage")
        Test_sc_rollup_storage.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup game")
        Test_sc_rollup_game.tests;
      Unit_test.spec
        (Protocol.name ^ ": liquidity baking")
        Test_liquidity_baking_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup wasm")
        Test_sc_rollup_wasm.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup arith")
        Test_sc_rollup_arith.tests;
      Unit_test.spec (Protocol.name ^ ": merkle list") Test_merkle_list.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup inbox")
        Test_sc_rollup_inbox.tests;
      Unit_test.spec (Protocol.name ^ ": skip list") Test_skip_list_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": sc rollup management protocol")
        Test_sc_rollup_management_protocol.tests;
      Unit_test.spec
        (Protocol.name ^ ": Bond_id_repr.ml")
        Test_bond_id_repr.tests;
      Unit_test.spec
        (Protocol.name ^ ": zk rollup storage")
        Test_zk_rollup_storage.tests;
      Unit_test.spec
        (Protocol.name ^ ": Delegate_consensus_key.ml")
        Test_consensus_key.tests;
      Unit_test.spec
        (Protocol.name ^ ": local_contexts")
        Test_local_contexts.tests;
      Unit_test.spec
        (Protocol.name ^ ": dal slot proof")
        Test_dal_slot_proof.tests;
    ]
  |> Lwt_main.run
