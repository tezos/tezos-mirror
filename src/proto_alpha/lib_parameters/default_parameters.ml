(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Protocol.Alpha_context

let constants_mainnet =
  let consensus_committee_size = 7000 in
  let block_time = 30 in
  let Constants.Generated.
        {
          consensus_threshold;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 60; denominator = block_time}
  in
  {
    Constants.preserved_cycles = 5;
    blocks_per_cycle = 8192l;
    blocks_per_commitment = 64l;
    blocks_per_stake_snapshot = 512l;
    cycles_per_voting_period = 5l;
    hard_gas_limit_per_operation = Gas.Arith.(integral_of_int_exn 1_040_000);
    hard_gas_limit_per_block = Gas.Arith.(integral_of_int_exn 5_200_000);
    proof_of_work_threshold = Int64.(sub (shift_left 1L 46) 1L);
    tokens_per_roll = Tez.(mul_exn one 6_000);
    seed_nonce_revelation_tip =
      (match Tez.(one /? 8L) with Ok c -> c | Error _ -> assert false);
    origination_size = 257;
    baking_reward_fixed_portion (* 10_000_000 mutez *);
    baking_reward_bonus_per_slot (* 4_286 mutez *);
    endorsing_reward_per_slot (* 2_857 mutez *);
    hard_storage_limit_per_operation = Z.of_int 60_000;
    cost_per_byte = Tez.of_mutez_exn 250L;
    quorum_min = 20_00l;
    quorum_max = 70_00l;
    min_proposal_quorum = 5_00l;
    (* liquidity_baking_subsidy is 1/16th of maximum total rewards for a block *)
    liquidity_baking_subsidy = Tez.of_mutez_exn 2_500_000L;
    (* level after protocol activation when liquidity baking shuts off:
         about 6 months after first activation on mainnet *)
    liquidity_baking_sunset_level = 3_063_809l;
    (* 1/3 window size of 2000 blocks with precision of 1000 for integer computation *)
    liquidity_baking_escape_ema_threshold = 666_667l;
    (* The rationale behind the value of this constant is that an
       operation should be considered alive for about one hour:

       minimal_block_delay context *  max_operations_ttl = 3600

       The unit for this value is a block.
    *)
    max_operations_time_to_live = 120;
    minimal_block_delay = Period.of_seconds_exn (Int64.of_int block_time);
    delay_increment_per_round = Period.of_seconds_exn 15L;
    consensus_committee_size;
    consensus_threshold;
    (* 4667 slots *)
    minimal_participation_ratio = {numerator = 2; denominator = 3};
    max_slashing_period = 2;
    frozen_deposits_percentage = 10;
    double_baking_punishment = Tez.(mul_exn one 640);
    ratio_of_frozen_deposits_slashed_per_double_endorsement =
      {numerator = 1; denominator = 2};
    initial_seed = None;
    (* A cache for contract source code and storage. Its size has been
       chosen not too exceed 100 000 000 bytes. *)
    cache_script_size = 100_000_000;
    (* A cache for the stake distribution for all cycles stored at any
       moment: preserved_cycles + max_slashing_period + 1 = 8 currently. *)
    cache_stake_distribution_cycles = 8;
    (* One for the sampler state for all cycles stored at any moment (as above). *)
    cache_sampler_state_cycles = 8;
    tx_rollup_enable = false;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2152
       Transaction rollups parameters need to be refined,
       currently the following values are merely placeholders. *)
    tx_rollup_origination_size = 60_000;
    (* Transaction rollup’s size limits are expressed in number of bytes *)
    tx_rollup_hard_size_limit_per_inbox = 100_000;
    tx_rollup_hard_size_limit_per_message = 5_000;
    tx_rollup_commitment_bond = Tez.of_mutez_exn 10_000_000_000L;
    tx_rollup_finality_period = 2_000;
    tx_rollup_max_unfinalized_levels = 2_100;
    (* [60_000] blocks is about two weeks. *)
    tx_rollup_withdraw_period = 60_000;
    sc_rollup_enable = false;
    (* The following value is chosen to prevent spam. *)
    sc_rollup_origination_size = 6_314;
  }

let constants_sandbox =
  let consensus_committee_size = 256 in
  let block_time = 1 in
  let Constants.Generated.
        {
          consensus_threshold = _;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 60; denominator = block_time}
  in
  {
    constants_mainnet with
    Constants.preserved_cycles = 2;
    blocks_per_cycle = 8l;
    blocks_per_commitment = 4l;
    blocks_per_stake_snapshot = 4l;
    cycles_per_voting_period = 8l;
    proof_of_work_threshold = Int64.of_int (-1);
    liquidity_baking_sunset_level = 128l;
    minimal_block_delay = Period.of_seconds_exn (Int64.of_int block_time);
    delay_increment_per_round = Period.one_second;
    consensus_committee_size = 256;
    consensus_threshold = 0;
    baking_reward_fixed_portion (* 333_333 mutez *);
    baking_reward_bonus_per_slot (* 3_921 mutez *);
    endorsing_reward_per_slot (* 2_604 mutez *);
    max_slashing_period = 2;
    frozen_deposits_percentage = 5;
  }

let constants_test =
  let consensus_committee_size = 25 in
  let Constants.Generated.
        {
          consensus_threshold;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 2; denominator = 1}
  in
  {
    constants_mainnet with
    Constants.preserved_cycles = 3;
    blocks_per_cycle = 12l;
    blocks_per_commitment = 4l;
    blocks_per_stake_snapshot = 4l;
    cycles_per_voting_period = 2l;
    proof_of_work_threshold = Int64.of_int (-1);
    liquidity_baking_sunset_level = 4096l;
    consensus_committee_size;
    consensus_threshold (* 17 slots *);
    max_slashing_period = 2;
    baking_reward_fixed_portion (* 10 tez *);
    baking_reward_bonus_per_slot (* 1.25 tez *);
    endorsing_reward_per_slot (* 0.8 tez *);
    frozen_deposits_percentage =
      5
      (* not 10 so that multiplication and
         divisions do not easily get
         intermingled *);
  }

let test_commitments =
  lazy
    (List.map
       (fun (bpkh, amount) ->
         let blinded_public_key_hash =
           Protocol.Blinded_public_key_hash.of_b58check_exn bpkh
         in
         let amount = Protocol.Alpha_context.Tez.of_mutez_exn amount in
         {Protocol.Alpha_context.Commitment.blinded_public_key_hash; amount})
       [
         ("btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", 23932454669343L);
         ("btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", 72954577464032L);
         ("btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", 217487035428348L);
         ("btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", 4092742372031L);
         ("btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", 17590039016550L);
         ("btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", 26322312350555L);
         ("btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", 244951387881443L);
         ("btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", 80065050465525L);
         ("btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", 3569618927693L);
         ("btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", 9034781424478L);
       ])

let bootstrap_accounts_strings =
  [
    "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
    "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9";
    "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV";
    "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
    "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
  ]

let bootstrap_balance = Tez.of_mutez_exn 4_000_000_000_000L

let compute_accounts =
  List.map (fun s ->
      let public_key = Signature.Public_key.of_b58check_exn s in
      let public_key_hash = Signature.Public_key.hash public_key in
      Parameters.
        {
          public_key_hash;
          public_key = Some public_key;
          amount = bootstrap_balance;
        })

let bootstrap_accounts = compute_accounts bootstrap_accounts_strings

let make_bootstrap_account (pkh, pk, amount) =
  Parameters.{public_key_hash = pkh; public_key = Some pk; amount}

let parameters_of_constants ?(bootstrap_accounts = bootstrap_accounts)
    ?(bootstrap_contracts = []) ?(commitments = []) constants =
  Parameters.
    {
      bootstrap_accounts;
      bootstrap_contracts;
      commitments;
      constants;
      security_deposit_ramp_up_cycles = None;
      no_reward_cycles = None;
    }

let json_of_parameters parameters =
  Data_encoding.Json.construct Parameters.encoding parameters
