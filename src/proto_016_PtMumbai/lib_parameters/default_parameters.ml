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

let tx_rollup_finality_period = 40_000

(** The challenge window is about two weeks with 15s block-time,
    (4 * 60 * 24 * 14).
    WARNING: changing this value also impacts
    [sc_rollup_max_active_outbox_levels]. See below. *)
let sc_rollup_challenge_window_in_blocks = 80_640

(** Number of active levels kept for executing outbox messages.

    WARNING: Changing this value impacts the storage charge for
    applying messages from the outbox. It also requires migration for
    remapping existing active outbox levels to new indices. *)
let sc_rollup_max_active_outbox_levels =
  Int32.of_int sc_rollup_challenge_window_in_blocks

(** Maximum number of outbox messages per level.

    WARNING: changing this value impacts the storage size a rollup has to
    pay for at origination time. *)
let sc_rollup_max_outbox_messages_per_level = 100

(** The timeout period is about a week with 15s block-time,
    (4 * 60 * 24 * 7).

    It suffers from the same risk of censorship as
    {!sc_rollup_challenge_windows_in_blocks} so we use the same value.
*)
let sc_rollup_timeout_period_in_blocks = 40_320

(** We want to allow a max lookahead in blocks of 4 weeks, so the rollup
    can still move forward even if its impossible to cement commitments.

    As there is a challenge window of 2 weeks, and because the maximum
    duration of a game is 2 weeks, the hypothetical maximum time
    to cement a block is a month, (4 * 60 * 24 * 30).

    Be careful, this constant has an impact of the maximum cost of
    a rollup on the storage:
    [maximum_cost_in_storage =
       (sc_rollup_max_lookahead_in_blocks / commitment_period) *
       max_commitment_storage_size_in_bytes *
       cost_per_byte]

    With the current values:
    [maximum_cost_in_storage = 348.3 tez]
*)
let sc_rollup_max_lookahead_in_blocks = 172_800l

(* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3177

   Think harder about those values. *)
let default_cryptobox_parameters =
  {
    Dal.page_size = 4096;
    slot_size = 1 lsl 20;
    redundancy_factor = 16;
    number_of_shards = 2048;
  }

let default_dal =
  Constants.Parametric.
    {
      feature_enable = false;
      number_of_slots = 256;
      attestation_lag = 1;
      availability_threshold = 50;
      cryptobox_parameters = default_cryptobox_parameters;
    }

let constants_mainnet =
  let consensus_committee_size = 7000 in
  let block_time = 15 in
  let Constants.Generated.
        {
          consensus_threshold;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
          liquidity_baking_subsidy;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 60; denominator = block_time}
  in
  {
    Constants.Parametric.preserved_cycles = 5;
    blocks_per_cycle = 16384l;
    blocks_per_commitment = 128l;
    nonce_revelation_threshold = 512l;
    blocks_per_stake_snapshot = 1024l;
    cycles_per_voting_period = 5l;
    hard_gas_limit_per_operation = Gas.Arith.(integral_of_int_exn 1_040_000);
    hard_gas_limit_per_block = Gas.Arith.(integral_of_int_exn 2_600_000);
    proof_of_work_threshold = Int64.(sub (shift_left 1L 46) 1L);
    minimal_stake = Tez.(mul_exn one 6_000);
    (* VDF's difficulty must be a multiple of `nonce_revelation_threshold` times
       the block time. At the moment it is equal to 8B = 8000 * 5 * .2M with
          - 8000 ~= 512 * 15 that is nonce_revelation_threshold * block time
          - .2M  ~= number of modular squaring per second on benchmark machine
         with 2.8GHz CPU
          - 5: security factor (strictly higher than the ratio between highest CPU
         clock rate and benchmark machine that is 8.43/2.8 ~= 3 *)
    vdf_difficulty = 8_000_000_000L;
    seed_nonce_revelation_tip =
      (match Tez.(one /? 8L) with Ok c -> c | Error _ -> assert false);
    origination_size = 257;
    baking_reward_fixed_portion (* 5_000_000 mutez *);
    baking_reward_bonus_per_slot (* 2_143 mutez *);
    endorsing_reward_per_slot (* 1_428 mutez *);
    hard_storage_limit_per_operation = Z.of_int 60_000;
    cost_per_byte = Tez.of_mutez_exn 250L;
    quorum_min = 20_00l;
    quorum_max = 70_00l;
    min_proposal_quorum = 5_00l;
    (* liquidity_baking_subsidy is 1/16th of maximum total rewards for a block *)
    liquidity_baking_subsidy (* 1_250_000 mutez *);
    (* 1/2 window size of 2000 blocks with precision of 1_000_000
       for integer computation *)
    liquidity_baking_toggle_ema_threshold = 1_000_000_000l;
    (* The rationale behind the value of this constant is that an
       operation should be considered alive for about one hour:

         minimal_block_delay * max_operations_time_to_live = 3600

       The unit for this value is a block.
    *)
    max_operations_time_to_live = 240;
    minimal_block_delay = Period.of_seconds_exn (Int64.of_int block_time);
    delay_increment_per_round = Period.of_seconds_exn 8L;
    consensus_committee_size;
    consensus_threshold;
    (* 4667 slots *)
    minimal_participation_ratio = {numerator = 2; denominator = 3};
    max_slashing_period = 2;
    frozen_deposits_percentage = 10;
    double_baking_punishment = Tez.(mul_exn one 640);
    ratio_of_frozen_deposits_slashed_per_double_endorsement =
      {numerator = 1; denominator = 2};
    (* The `testnet_dictator` should absolutely be None on mainnet *)
    testnet_dictator = None;
    initial_seed = None;
    (* A cache for contract source code and storage. Its size has been
       chosen not too exceed 100 000 000 bytes. *)
    cache_script_size = 100_000_000;
    (* A cache for the stake distribution for all cycles stored at any
       moment: preserved_cycles + max_slashing_period + 1 = 8 currently. *)
    cache_stake_distribution_cycles = 8;
    (* One for the sampler state for all cycles stored at any moment (as above). *)
    cache_sampler_state_cycles = 8;
    tx_rollup =
      {
        enable = false;
        (* Based on how storage burn is implemented for
           transaction rollups, this means that a rollup operator
           can create 100 inboxes (40 bytes per inbox) before
           having to pay storage burn. *)
        origination_size = 4_000;
        (* Considering an average size of layer-2 operations of
           20, this gives a TPS per rollup higher than 400, and
           the capability to have two rollups at full speed on
           mainnet (as long as they do not reach scalability
           issues related to proof size). *)
        hard_size_limit_per_inbox = 500_000;
        hard_size_limit_per_message = 5_000;
        commitment_bond = Tez.of_mutez_exn 10_000_000_000L;
        finality_period = tx_rollup_finality_period;
        max_inboxes_count = tx_rollup_finality_period + 100;
        (* [60_000] blocks is about two weeks. *)
        withdraw_period = tx_rollup_finality_period;
        max_messages_per_inbox = 1_010;
        (* Must be greater than the withdraw period. *)
        max_commitments_count = (2 * tx_rollup_finality_period) + 100;
        cost_per_byte_ema_factor = 120;
        (* Tickets are transmitted in batches in the
           [Tx_rollup_dispatch_tickets] operation.

           The semantics is that this operation is used to
           concretize the withdraw orders emitted by the layer-2,
           one layer-1 operation per messages of an
           inbox. Therefore, it is of significant importance that
           a valid batch does not produce a list of withdraw
           orders which could not fit in a layer-1 operation.

           With these values, at least 2048 bytes remain available
           to store the rest of the operands of
           [Tx_rollup_dispatch_tickets] (in practice, even more,
           because we overapproximate the size of tickets). So we
           are safe. *)
        max_withdrawals_per_batch = 15;
        max_ticket_payload_size = 2_048;
        (* Must be smaller than maximum limit of a manager operation
           (minus overhead), since we need to limit our proofs to those
           that can fit in an operation. *)
        rejection_max_proof_size = 30000;
        (* This is the first block of cycle 618, which is expected to
           be about one year after the activation of protocol J.  See
           https://tzstats.com/cycle/618 *)
        sunset_level = 3_473_409l;
      };
    dal = default_dal;
    sc_rollup =
      {
        enable = true;
        arith_pvm_enable = false;
        (* The following value is chosen to prevent spam. *)
        origination_size = 6_314;
        challenge_window_in_blocks = sc_rollup_challenge_window_in_blocks;
        commitment_period_in_blocks = 60;
        stake_amount = Tez.of_mutez_exn 10_000_000_000L;
        max_lookahead_in_blocks = sc_rollup_max_lookahead_in_blocks;
        max_active_outbox_levels = sc_rollup_max_active_outbox_levels;
        max_outbox_messages_per_level = sc_rollup_max_outbox_messages_per_level;
        (* The default number of required sections in a dissection *)
        number_of_sections_in_dissection = 32;
        timeout_period_in_blocks = sc_rollup_timeout_period_in_blocks;
        (* We store multiple cemented commitments because we want to
            allow the execution of outbox messages against cemented
            commitments that are older than the last cemented commitment.
            The execution of an outbox message is a manager operation,
            and manager operations are kept in the mempool for one
            hour. Hence we only need to ensure that an outbox message
            can be validated against a cemented commitment produced in the
            last hour. If we assume that the rollup is operating without
            issues, that is no commitments are being refuted and commitments
            are published and cemented regularly by one rollup node, we can
            expect commitments to be cemented approximately every 15
            minutes, or equivalently we can expect 5 commitments to be
            published in one hour (at minutes 0, 15, 30, 45 and 60).
            Therefore, we need to keep 5 cemented commitments to guarantee
            that the execution of an outbox operation can always be
            validated against a cemented commitment while it is in the
            mempool. *)
        max_number_of_stored_cemented_commitments = 5;
        max_number_of_parallel_games = 32;
      };
    zk_rollup =
      {
        enable = false;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/3726
           The following constants need to be refined. *)
        origination_size = 4_000;
        min_pending_to_process = 10;
      };
  }

(* Sandbox and test networks's Dal cryptobox are computed by this function:
   - Redundancy_factor is provided as a parameter;
   - The other fields are derived from mainnet's values, as divisions by the
     provided factor. *)
let derive_cryptobox_parameters ~redundancy_factor ~mainnet_constants_divider =
  let m = default_cryptobox_parameters in
  {
    Dal.redundancy_factor;
    page_size = m.page_size / mainnet_constants_divider;
    slot_size = m.slot_size / mainnet_constants_divider;
    number_of_shards = m.number_of_shards / mainnet_constants_divider;
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
          liquidity_baking_subsidy;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 60; denominator = block_time}
  in
  {
    constants_mainnet with
    dal =
      Constants.Parametric.
        {
          constants_mainnet.dal with
          number_of_slots = 16;
          cryptobox_parameters =
            derive_cryptobox_parameters
              ~redundancy_factor:8
              ~mainnet_constants_divider:32;
        };
    Constants.Parametric.preserved_cycles = 2;
    blocks_per_cycle = 8l;
    blocks_per_commitment = 4l;
    nonce_revelation_threshold = 4l;
    blocks_per_stake_snapshot = 4l;
    cycles_per_voting_period = 8l;
    proof_of_work_threshold = Int64.of_int (-1);
    vdf_difficulty = 50_000L;
    liquidity_baking_subsidy;
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
          liquidity_baking_subsidy;
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~blocks_per_minute:{numerator = 2; denominator = 1}
  in
  {
    constants_mainnet with
    dal =
      Constants.Parametric.
        {
          constants_mainnet.dal with
          number_of_slots = 8;
          cryptobox_parameters =
            derive_cryptobox_parameters
              ~redundancy_factor:4
              ~mainnet_constants_divider:64;
        };
    Constants.Parametric.preserved_cycles = 3;
    blocks_per_cycle = 12l;
    blocks_per_commitment = 4l;
    nonce_revelation_threshold = 4l;
    blocks_per_stake_snapshot = 4l;
    cycles_per_voting_period = 2l;
    proof_of_work_threshold = Int64.of_int (-1);
    vdf_difficulty = 50_000L;
    liquidity_baking_subsidy;
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
          delegate_to = None;
          consensus_key = None;
        })

let bootstrap_accounts = compute_accounts bootstrap_accounts_strings

let make_bootstrap_account (pkh, pk, amount, delegate_to, consensus_key) =
  Parameters.
    {
      public_key_hash = pkh;
      public_key = Some pk;
      amount;
      delegate_to;
      consensus_key;
    }

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
