(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trilitech <contact@trili.tech>                         *)
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
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/integration/main.exe
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

(* Check that
    [sc_rollup_challenge_window_in_blocks < sc_rollup_max_lookahead_in_blocks]

    Otherwise committers would be forced to commit at an artificially slow rate, affecting
    the throughput of the rollup. *)
let test_sc_rollup_challenge_window_lt_max_lookahead () =
  let constants = Default_parameters.constants_mainnet in
  let max_lookahead = constants.sc_rollup.max_lookahead_in_blocks in
  let challenge_window =
    Int32.of_int constants.sc_rollup.challenge_window_in_blocks
  in
  Assert.lt_int32 ~loc:__LOC__ challenge_window max_lookahead

(* Check that
    [commitment_storage_cost * max_lookahead / commitment_period < stake_amount]

   Otherwise storage could be overallocated - since backtracking is not allowed, a staker
   can allocated at most [d] nodes (where [d] is the tree depth) - the maximum storage cost
   of these commitments must be at most the size of the staker's deposit. *)
let test_sc_rollup_max_commitment_storage_cost_lt_deposit () =
  let constants = Default_parameters.constants_mainnet in
  let open Protocol in
  let cost_per_byte_mutez =
    Alpha_context.Tez.to_mutez constants.cost_per_byte
  in
  let commitment_storage_size =
    Int64.of_int Sc_rollup_stake_storage.commitment_storage_size_in_bytes
  in
  let commitment_storage_cost =
    Int64.mul cost_per_byte_mutez commitment_storage_size
  in
  let max_lookahead =
    Int64.of_int32 constants.sc_rollup.max_lookahead_in_blocks
  in
  let commitment_period =
    Int64.of_int constants.sc_rollup.commitment_period_in_blocks
  in
  let stake_amount =
    Alpha_context.Tez.to_mutez constants.sc_rollup.stake_amount
  in
  Assert.leq_int64
    ~loc:__LOC__
    (Int64.mul
       commitment_storage_cost
       (Int64.div max_lookahead commitment_period))
    stake_amount

(* Check that
   [{!Sc_rollup_stake_storage.commitment_storage_size_in_bytes} =
   commitments_entry_size + commitment_stake_count_entry_size +
   commitment_added_entry_size]

   Required to ensure [sc_rollup_stake_amount] and [sc_rollup_max_lookahead] are
   correctly scaled with respect to each other - see
   {!test_sc_rollup_max_commitment_storage_cost_lt_deposit}
*)
let test_sc_rollup_commitment_storage_size () =
  let open Protocol in
  Assert.get_some
    ~loc:__LOC__
    (Sc_rollup_repr.Number_of_ticks.of_value 1232909L)
  >>=? fun number_of_ticks ->
  let commitment =
    Sc_rollup_commitment_repr.to_versioned
      {
        predecessor = Sc_rollup_commitment_repr.Hash.zero;
        inbox_level = Raw_level_repr.of_int32_exn 21l;
        number_of_ticks;
        compressed_state = Sc_rollup_repr.State_hash.zero;
      }
  in
  let commitment_bytes =
    Data_encoding.Binary.to_bytes_exn
      Sc_rollup_commitment_repr.versioned_encoding
      commitment
  in
  let level = Alpha_context.Raw_level.of_int32_exn 5l in
  let level_bytes =
    Data_encoding.Binary.to_bytes_exn Alpha_context.Raw_level.encoding level
  in
  (* commitment stake count is encoded in Int32, but is not exposed here *)
  let commitment_stake_count = 300l in
  let commitment_stake_count_bytes =
    Data_encoding.Binary.to_bytes_exn Data_encoding.int32 commitment_stake_count
  in
  Assert.equal_int
    ~loc:__LOC__
    Sc_rollup_stake_storage.commitment_storage_size_in_bytes
    (Bytes.length commitment_bytes
    + Bytes.length level_bytes
    + Bytes.length commitment_stake_count_bytes)

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
      "sc rollup challenge window less than max lookahead"
      `Quick
      test_sc_rollup_challenge_window_lt_max_lookahead;
    Tztest.tztest
      "sc rollup max commitment storage cost less than deposit"
      `Quick
      test_sc_rollup_max_commitment_storage_cost_lt_deposit;
    Tztest.tztest
      "sc rollup commitment storage size correct"
      `Quick
      test_sc_rollup_commitment_storage_size;
    Tztest.tztest
      "test liquidity_baking_subsidy parameter is 1/16th of total baking \
       rewards"
      `Quick
      liquidity_baking_subsidy_param;
  ]
