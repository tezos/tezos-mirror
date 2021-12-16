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
    Component:  Protocol (baking)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^constants$"
    Subject:    the consistency of parametric constants
 *)

open Test_tez

let test_constants_consistency () =
  let open Default_parameters in
  List.iter_es
    Block.check_constants_consistency
    [constants_mainnet; constants_sandbox; constants_test]

let test_max_operations_ttl () =
  let open Protocol in
  (* We check the rationale that the value for [max_operations_time_to_live] is the following:

     [minimal_time_between_blocks *  max_operations_time_to_live = 3600] *)
  let constants = Default_parameters.constants_mainnet in
  Environment.wrap_tzresult
    (Alpha_context.Period.mult
       (Int32.of_int constants.max_operations_time_to_live)
       constants.minimal_block_delay)
  >>?= fun result ->
  Assert.equal
    ~loc:__LOC__
    (fun x y -> Alpha_context.Period.compare x y = 0)
    "max_operations_ttl"
    Alpha_context.Period.pp
    Alpha_context.Period.one_hour
    result

(** Test that the amount of the liquidity baking subsidy is epsilon smaller than
   1/16th of the maximum reward. *)
let liquidity_baking_subsidy_param () =
  let constants = Default_parameters.constants_mainnet in
  constants.baking_reward_bonus_per_slot
  *? Int64.of_int (constants.consensus_committee_size / 3)
  >>?= fun baking_reward_bonus ->
  constants.baking_reward_fixed_portion +? baking_reward_bonus
  >>?= fun baking_rewards ->
  constants.endorsing_reward_per_slot
  *? Int64.of_int constants.consensus_committee_size
  >>?= fun validators_rewards ->
  baking_rewards +? validators_rewards >>?= fun total_rewards ->
  total_rewards /? 16L >>?= fun expected_subsidy ->
  constants.liquidity_baking_subsidy -? expected_subsidy >>?= fun diff ->
  let max_diff = 1000 (* mutez *) in
  Assert.leq_int ~loc:__LOC__ (Int64.to_int (to_mutez diff)) max_diff

let tests =
  [
    Tztest.tztest "constants consistency" `Quick test_constants_consistency;
    Tztest.tztest "max_operations_ttl" `Quick test_max_operations_ttl;
    Tztest.tztest
      "test liquidity_baking_subsidy parameter is 1/16th of total baking \
       rewards"
      `Quick
      liquidity_baking_subsidy_param;
  ]
