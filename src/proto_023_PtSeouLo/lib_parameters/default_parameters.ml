(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

(* Individual constants should be documented here so that the
   documentation is right next to its mainnet value. However, note
   that some constants are currently documented in
   {!Protocol.Constants_parametric_repr} instead.

   See {!Protocol.Constants_parametric_repr} for documentation on
   constant management in general, and instructions on how to
   add/modify a constant.
*)

open Protocol.Alpha_context

let seconds_in_a_day = 60 * 60 * 24

let seconds_in_a_week = seconds_in_a_day * 7

let make_sc_rollup_parameter ~dal_activation_level
    ~dal_attested_slots_validity_lag block_time =
  (* Maximum number of outbox messages per level.

      WARNING: changing this value impacts the storage size a rollup has to
      pay for at origination time. *)
  let max_outbox_messages_per_level = 100 in

  (* The commitment period in blocks is about 15 minutes. *)
  let commitment_period_in_blocks = 60 * 15 / block_time in

  (* The challenge window is about two weeks.  WARNING: changing this
     value also impacts [sc_rollup_max_active_outbox_levels].  See
     below. *)
  let challenge_window_in_blocks = seconds_in_a_week * 2 / block_time in

  (* Number of active levels kept for executing outbox messages.

      WARNING: Changing this value impacts the storage charge for
      applying messages from the outbox. It also requires migration for
      remapping existing active outbox levels to new indices. *)
  let max_active_outbox_levels = Int32.of_int challenge_window_in_blocks in

  (* The timeout period is about a week.  It suffers from the same
     risk of censorship as {!sc_rollup_challenge_windows_in_blocks} so
     we use the same value. *)
  let timeout_period_in_blocks = seconds_in_a_week / block_time in

  (* We want to allow a max lookahead in blocks of 4 weeks, so the
     rollup can still move forward even if its impossible to cement
     commitments.

     As there is a challenge window of 2 weeks, and because the maximum
     duration of a game is 2 weeks, the hypothetical maximum time
     to cement a block is a month.

     Be careful, this constant has an impact of the maximum cost of
     a rollup on the storage:
     [maximum_cost_in_storage =
        (sc_rollup_max_lookahead_in_blocks / commitment_period) *
        max_commitment_storage_size_in_bytes *
        cost_per_byte]

     With the current values:
     [maximum_cost_in_storage = 348.3 tez]
  *)
  let max_lookahead_in_blocks =
    let seconds_in_a_month = Int32.of_int (seconds_in_a_day * 30) in
    let block_time = Int32.of_int block_time in
    Int32.div seconds_in_a_month block_time
  in
  Constants.Parametric.
    {
      arith_pvm_enable = false;
      (* The following value is chosen to prevent spam. *)
      origination_size = 6_314;
      challenge_window_in_blocks;
      commitment_period_in_blocks;
      stake_amount = Tez.of_mutez_exn 10_000_000_000L;
      max_lookahead_in_blocks;
      max_active_outbox_levels;
      max_outbox_messages_per_level;
      (* The default number of required sections in a dissection *)
      number_of_sections_in_dissection = 32;
      timeout_period_in_blocks;
      (* We store multiple cemented commitments because we want to
         allow the execution of outbox messages against cemented
         commitments that are older than the last cemented commitment.
         The execution of an outbox message is a manager operation,
         and manager operations are kept in the mempool for one
         hour. Hence we only need to ensure that an outbox message can
         be validated against a cemented commitment produced in the
         last hour. If we assume that the rollup is operating without
         issues, that is no commitments are being refuted and
         commitments are published and cemented regularly by one
         rollup node, we can expect commitments to be cemented
         approximately every 15 minutes, or equivalently we can expect
         5 commitments to be published in one hour (at minutes 0, 15,
         30, 45 and 60).  Therefore, we need to keep 5 cemented
         commitments to guarantee that the execution of an outbox
         operation can always be validated against a cemented
         commitment while it is in the mempool. *)
      max_number_of_stored_cemented_commitments = 5;
      max_number_of_parallel_games = 32;
      reveal_activation_level =
        {
          raw_data = {blake2B = Raw_level.root};
          metadata = Raw_level.root;
          dal_page = dal_activation_level;
          dal_parameters = dal_activation_level;
          dal_attested_slots_validity_lag;
        };
      private_enable = true;
      riscv_pvm_enable = false;
    }

let default_cryptobox_parameters =
  {
    Dal.page_size = 3967;
    slot_size = 126_944;
    redundancy_factor = 8;
    number_of_shards = 512;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
       Changing these values will be tricky. (The issue suggests a way to deal
       with such changes.) *)
  }

let default_dal =
  Constants.Parametric.
    {
      feature_enable = true;
      incentives_enable = true;
      number_of_slots = 32;
      attestation_lag = 8;
      attestation_threshold = 66;
      cryptobox_parameters = default_cryptobox_parameters;
      minimal_participation_ratio = Q.(64 // 100);
      (* Note that other values may make tests in tezt/tests/mockup.ml
         fail. Indeed, some tests modify the constants' values a bit and then
         perform some checks on the modified values. In case of [Q.t] values,
         the numerator and the denominator are increased by 1. For instance,
         when minimal_attestation_ratio is 60%, we have that the new value is
         2/3 = 4/6 = (3+1)/(5+1). However, the test fails because it does not
         realize that 2/3 = 4/6...
         That is why a value x of [minimal_participation_ratio] was chosen such
         that we have x = a/b with a and b smallest such that they are relatively
         prime, and (a+1, b+1) are relatively prime as well. The value x = 63%
         works as well. *)
      rewards_ratio = Q.(1 // 10);
      (* This value determines the value of
         [issuance_weights.dal_rewards_weight]. When computing the actual
         rewards, [dal_rewards_weight] is ignored when [incentives_enable =
         false]. *)
      traps_fraction = Q.(5 // 10000);
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7742
         Further changes (both in the protocol and in the DAL node) need to be
         made if this value changes. *)
    }

let constants_mainnet : Constants.Parametric.t =
  let block_time = 8 in
  let consensus_committee_size = 7000 in
  let Constants.Generated.
        {
          consensus_threshold_size;
          issuance_weights =
            {
              base_total_issued_per_minute;
              baking_reward_fixed_portion_weight;
              baking_reward_bonus_weight;
              attesting_reward_weight;
              seed_nonce_revelation_tip_weight;
              vdf_revelation_tip_weight;
              dal_rewards_weight;
            };
        } =
    Constants.Generated.generate
      ~consensus_committee_size
      ~dal_rewards_ratio:default_dal.rewards_ratio
  in
  let dal_activation_level =
    if default_dal.feature_enable then Raw_level.succ Raw_level.root
    else
      (* Deactivate the reveal if the dal is not enabled. *)
      (* https://gitlab.com/tezos/tezos/-/issues/5968
         Encoding error with Raw_level

         We set the activation level to [pred max_int] to deactivate
         the feature. The [pred] is needed to not trigger an encoding
         exception with the value [Int32.int_min] (see tezt/tests/mockup.ml). *)
      Raw_level.of_int32_exn Int32.(pred max_int)
  in
  let dal_attested_slots_validity_lag =
    (* A rollup node shouldn't import a page of an attested slot whose attested
       level is too far in the past w.r.t. the current level. Importation window
       is fixed to 241_920 levels below. It is the number of blocks produced
       during 28 days (4 weeks) with a block time of 10 seconds. *)
    241_920
  in
  let sc_rollup =
    make_sc_rollup_parameter
      ~dal_activation_level
      ~dal_attested_slots_validity_lag
      block_time
  in
  {
    (* [consensus_rights_delay] is the number of cycles between the
       computation of consensus rights and their actual use for baking
       and attesting. That is, at the end of cycle [n] (dawn of cycle
       [n+1]), the consensus rights for cycle
       [n+1+consensus_rights_delay] are sampled based on the current
       baking power of bakers.

       Last updated in protocol P. *)
    consensus_rights_delay = 2;
    blocks_preservation_cycles = 1;
    (* [delegate_parameters_activation_delay] is the number of full
       cycles after which submitted delegate parameters are actually
       used. That is, parameters updated during cycle [n] take effect
       at the end of cycle [n + delegate_parameters_activation_delay]
       and beginning of cycle [n +
       delegate_parameters_activation_delay + 1].

       This should translate into a sufficient duration in days for
       delegators to manually react to a notification that their
       delegate's parameters are going to be modified. This should
       also be longer than the delay for unstake requests to become
       finalizable, to make possible for delegators to finish
       unstaking before the new parameters take effect if they are
       unhappy with the change. *)
    delegate_parameters_activation_delay = 5;
    (* At the end of a cycle, a delegate gets deactivated if the chain
       has witnessed no consensus activity (baking, attesting) from it
       during the past [tolerated_inactivity_period] cycles, including
       the currently ending cycle.

       [tolerated_inactivity_period = 1] means that if a delegate does
       not participate in consensus at all during a full cycle, then
       it gets deactivated at the end of that cycle.

       Note that there is an extra grace period of
       [consensus_rights_delay] cycles when a delegate has just
       registered or has just been reactivated. This accounts for the
       fact that it will not receive consensus rights yet during the
       first [consensus_rights_delay] cycles, so of course the chain
       will not witness any activity from it then.

       Last updated in protocol R. *)
    tolerated_inactivity_period = 2;
    (* [blocks_per_cycle] is the duration of a cycle in number of
       blocks. Multiply it by [minimal_block_delay] to get the minimal
       duration of a cycle in seconds.

       [blocks_per_cycle = 10800l] has been chosen so that cycles last
       one day (24h) (plus drift from non-zero rounds) on mainnet
       where [minimal_block_delay] is 8s, and half a day (12h) on
       ghostnet where [minimal_block_delay] is 4s.

       Last updated in protocol R. *)
    blocks_per_cycle = 10800l;
    (* Each [blocks_per_commitment] blocks, the block producer has to commit on
       a nonce. Currently we target 128 nonces per cycle in order to ensure the
       nonces are produced by sufficiently many different bakers.
       (blocks_per_commitment = blocks_per_cycle / 128)

       Don't forget to update cycles-eras when updating this parameter.
       Last updated in protocol S.
    *)
    blocks_per_commitment = 84l;
    (* Duration in levels of the nonce revelation phase (which precedes the VDF
       phase).
       Last updated in protocol S. *)
    nonce_revelation_threshold = 300l;
    (* [cycles_per_voting_period] is the duration of any voting period.

       Last updated in protocol R. *)
    cycles_per_voting_period = 14l;
    hard_gas_limit_per_operation = Gas.Arith.(integral_of_int_exn 1_040_000);
    hard_gas_limit_per_block = Gas.Arith.(integral_of_int_exn 1_386_666);
    (* When reducing blocks time, consider adapting this constant so
       the block production's overhead is not too important. *)
    proof_of_work_threshold = Int64.(sub (shift_left 1L 48) 1L);
    minimal_stake = Tez.(mul_exn one 6_000);
    minimal_frozen_stake = Tez.(mul_exn one 600);
    (* VDF's difficulty must be a multiple of `nonce_revelation_threshold` times
       the block time. At the moment it is equal to 2400M = 2400 * 5 * 0.2M with
          - 2400 ~= 300 * 8 that is nonce_revelation_threshold * block time
          - 0.2M  ~= number of modular squaring per second on benchmark machine
         with 2.8GHz CPU
          - 5: security factor (strictly higher than the ratio between highest CPU
         clock rate and benchmark machine that is 9.12/2.8 ~= 3.
       Last updated in protocol S. *)
    vdf_difficulty = 2_400_000_000L;
    origination_size = 257;
    issuance_weights =
      {
        base_total_issued_per_minute;
        (* 80.007812 tez/minute *)
        baking_reward_fixed_portion_weight;
        (* 1/4th of total block rewards *)
        baking_reward_bonus_weight;
        (* all bonus rewards = fixed rewards *)
        attesting_reward_weight;
        (* all baking rewards = all attesting rewards *)
        seed_nonce_revelation_tip_weight;
        (* 1/20480 of block rewards *)
        vdf_revelation_tip_weight;
        (* 1/20480 of block rewards *)
        dal_rewards_weight;
        (* 2275; it depends on the value of [dal.rewards_ratio] *)
      };
    hard_storage_limit_per_operation = Z.of_int 60_000;
    cost_per_byte = Tez.of_mutez_exn 250L;
    quorum_min = 20_00l;
    quorum_max = 70_00l;
    min_proposal_quorum = 5_00l;
    (* [liquidity_baking_subsidy] is the amount to mint for the
       constant product market making (CPMM) contract, in tez/minute.
       Last updated in protocol P (it was then changed from tez/block
       to tez/minute.)  *)
    liquidity_baking_subsidy = Tez.(mul_exn one 5);
    (* 1/2 window size of 2000 blocks with precision of 1_000_000
       for integer computation *)
    liquidity_baking_toggle_ema_threshold = 1_000_000_000l;
    (* The rationale behind the value of this constant is that an
       operation should be considered alive for about one hour:

         minimal_block_delay * max_operations_time_to_live = 3600

       The unit for this value is a block.
    *)
    max_operations_time_to_live = 450;
    (* Round [k] lasts [minimal_block_delay + k * delay_increment_per_round]. *)
    minimal_block_delay = Period.of_seconds_exn (Int64.of_int block_time);
    (* [delay_increment_per_round] must be strictly positive to ensure
       strictly increasing round durations, as required by Tenderbake. *)
    delay_increment_per_round = Period.of_seconds_exn 4L;
    consensus_committee_size;
    consensus_threshold_size;
    (* 4667 slots *)
    minimal_participation_ratio = {numerator = 2; denominator = 3};
    limit_of_delegation_over_baking = 9;
    percentage_of_frozen_deposits_slashed_per_double_baking =
      Protocol.Percentage.p5;
    max_slashing_per_block = Protocol.Percentage.p100;
    (* When slashing happens, if the power of the misbehaving consensus
       exceeds this threshold (as a ratio of the total power), then the maximum value
       for slashing (defined in the previous line) is applied.
       It corresponds to 2334 slots on mainnet.
       It must correspond to (1 - minimal_participation_ratio) *)
    max_slashing_threshold = {numerator = 1; denominator = 3};
    (* The `testnet_dictator` should absolutely be None on mainnet *)
    testnet_dictator = None;
    initial_seed = None;
    (* A cache for contract source code and storage. Its size has been
       chosen not too exceed 100 000 000 bytes. *)
    cache_script_size = 100_000_000;
    (* A cache for the stake distribution for all cycles stored at any moment:
       consensus_rights_delay + max_slashing_period + 1 = 2 + 2 + 1 = 5
       currently. *)
    cache_stake_distribution_cycles = 5;
    (* One for the sampler state for all cycles stored at any moment (as above). *)
    cache_sampler_state_cycles = 5;
    dal = default_dal;
    sc_rollup;
    zk_rollup =
      {
        enable = false;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/3726
           The following constants need to be refined. *)
        origination_size = 4_000;
        min_pending_to_process = 10;
        max_ticket_payload_size = 2_048;
      };
    adaptive_issuance =
      {
        global_limit_of_staking_over_baking = 9;
        edge_of_staking_over_delegation = 3;
        adaptive_rewards_params =
          {
            issuance_ratio_final_min = Q.(0_25 // 100_00);
            issuance_ratio_final_max = Q.(10 // 100);
            issuance_ratio_initial_min = Q.(45 // 1000);
            issuance_ratio_initial_max = Q.(55 // 1000);
            initial_period = 10;
            transition_period = 50;
            max_bonus =
              Protocol.Issuance_bonus_repr.max_bonus_parameter_of_Q_exn
                Q.(5 // 100);
            (* 0.01% per 1% per day *)
            growth_rate = Q.(1 // 100);
            center_dz = Q.(1 // 2);
            radius_dz = Q.(1 // 50);
          };
      };
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6668
       Enable once at least the following is done:
       - Split [allow_forged] into [allow_tickets] and [allow_lazy_storage_id]: #2964
       - Introduce a new Ticket constructor in Michelson: #6643 *)
    direct_ticket_spending_enable = false;
    (* attestation aggregation feature flag *)
    aggregate_attestation = true;
    allow_tz4_delegate_enable = true;
    all_bakers_attest_activation_level = None;
  }

let constants_sandbox =
  let consensus_committee_size = 301 in
  let block_time = 1 in
  let Constants.Generated.{consensus_threshold_size = _; issuance_weights} =
    Constants.Generated.generate
      ~consensus_committee_size
      ~dal_rewards_ratio:default_dal.rewards_ratio
  in
  {
    constants_mainnet with
    dal =
      Constants.Parametric.
        {
          constants_mainnet.dal with
          number_of_slots = 16;
          cryptobox_parameters =
            {default_cryptobox_parameters with number_of_shards = 256};
        };
    issuance_weights;
    blocks_preservation_cycles = 1;
    consensus_rights_delay = 2;
    delegate_parameters_activation_delay = 2;
    blocks_per_cycle = 8l;
    blocks_per_commitment = 4l;
    nonce_revelation_threshold = 4l;
    cycles_per_voting_period = 8l;
    proof_of_work_threshold = Int64.(sub (shift_left 1L 62) 1L);
    vdf_difficulty = 50_000L;
    minimal_block_delay = Period.of_seconds_exn (Int64.of_int block_time);
    delay_increment_per_round = Period.one_second;
    consensus_committee_size = 256;
    consensus_threshold_size = 0;
    limit_of_delegation_over_baking = 19;
    max_operations_time_to_live = 8;
    allow_tz4_delegate_enable = true;
  }

let constants_test =
  let consensus_committee_size = 67 in
  let Constants.Generated.{consensus_threshold_size; issuance_weights} =
    Constants.Generated.generate
      ~consensus_committee_size
      ~dal_rewards_ratio:default_dal.rewards_ratio
  in
  {
    constants_mainnet with
    dal =
      Constants.Parametric.
        {
          constants_mainnet.dal with
          number_of_slots = 8;
          cryptobox_parameters =
            {
              default_cryptobox_parameters with
              number_of_shards = 64;
              redundancy_factor = 2;
            };
        };
    issuance_weights;
    consensus_rights_delay = 2;
    delegate_parameters_activation_delay = 3;
    blocks_preservation_cycles = 1;
    blocks_per_cycle = 12l;
    blocks_per_commitment = 4l;
    nonce_revelation_threshold = 4l;
    cycles_per_voting_period = 2l;
    proof_of_work_threshold =
      Int64.(sub (shift_left 1L 62) 1L) (* 1/4 of nonces are accepted *);
    vdf_difficulty = 50_000L;
    consensus_committee_size;
    consensus_threshold_size (* 17 slots *);
    limit_of_delegation_over_baking =
      19
      (* Not 9 so that multiplication by a percentage and
         divisions by a limit do not easily get intermingled. *);
    max_operations_time_to_live = 8;
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
    ?(bootstrap_contracts = []) ?(bootstrap_smart_rollups = [])
    ?(commitments = []) constants =
  Parameters.
    {
      bootstrap_accounts;
      bootstrap_contracts;
      bootstrap_smart_rollups;
      commitments;
      constants;
      security_deposit_ramp_up_cycles = None;
      no_reward_cycles = None;
    }

module Protocol_parameters_overrides = struct
  type t = {parameters : Parameters.t; chain_id : Chain_id.t option}

  let encoding =
    let open Data_encoding in
    conv
      (fun {parameters; chain_id} -> (parameters, chain_id))
      (fun (parameters, chain_id) -> {parameters; chain_id})
      (merge_objs Parameters.encoding (obj1 (opt "chain_id" Chain_id.encoding)))
end

let json_of_parameters ?chain_id parameters =
  Data_encoding.Json.construct
    Protocol_parameters_overrides.encoding
    Protocol_parameters_overrides.{parameters; chain_id}

module Internal_for_tests = struct
  let bootstrap_balance = bootstrap_balance

  let make_sc_rollup_parameter = make_sc_rollup_parameter
end
