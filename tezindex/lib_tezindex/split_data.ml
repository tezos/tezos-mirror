(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* --- TzKT-compatible sub-types --- *)

type tzkt_quote = {
  btc : float;
  eur : float;
  usd : float;
  cny : float;
  jpy : float;
  krw : float;
  eth : float;
  gbp : float;
}

let empty_tzkt_quote =
  {
    btc = 0.;
    eur = 0.;
    usd = 0.;
    cny = 0.;
    jpy = 0.;
    krw = 0.;
    eth = 0.;
    gbp = 0.;
  }

type tzkt_delegator = {
  del_address : string;
  delegated_balance : int64;
  emptied : bool;
}

type tzkt_staker_entry = {
  stk_address : string;
  staked_pseudotokens : string;
  staked_balance : int64;
}

type tzkt_actual_staker = {
  as_address : string;
  initial_stake : int64;
  final_stake : int64;
  as_rewards : int64;
}

type stake_split = {
  ss_delegated : int64;
  staked_own : int64;
  staked_edge : int64;
  staked_shared : int64;
}

let empty_stake_split =
  {ss_delegated = 0L; staked_own = 0L; staked_edge = 0L; staked_shared = 0L}

type double_op_info = {
  dbl_rewards : int64;
  lost_staked : int64;
  lost_unstaked : int64;
  lost_external_staked : int64;
  lost_external_unstaked : int64;
}

let empty_double_op =
  {
    dbl_rewards = 0L;
    lost_staked = 0L;
    lost_unstaked = 0L;
    lost_external_staked = 0L;
    lost_external_unstaked = 0L;
  }

(* --- TzKT-compatible main type --- *)
type tzkt_baker_rewards = {
  tzkt_cycle : int32;
  own_delegated_balance : int64;
  external_delegated_balance : int64;
  delegators_count : int;
  own_staked_balance : int64;
  external_staked_balance : int64;
  stakers_count : int;
  issued_pseudotokens : string;
  baking_power : int64;
  total_baking_power : int64;
  expected_blocks : int;
  future_blocks : int;
  future_block_rewards : int64;
  num_blocks : int;
  block_rewards_split : stake_split;
  missed_blocks : int;
  missed_block_rewards : int64;
  expected_attestations : int;
  future_attestations : int;
  future_attestation_rewards : int64;
  num_attestations : int;
  attestation_rewards_split : stake_split;
  missed_attestations : int;
  missed_attestation_rewards : int64;
  expected_dal_attestations : int;
  future_dal_attestation_rewards : int64;
  dal_attestation_rewards_split : stake_split;
  missed_dal_attestation_rewards : int64;
  tzkt_block_fees : int64;
  missed_block_fees : int64;
  double_baking : double_op_info;
  double_consensus : double_op_info;
  vdf_revelation_rewards : stake_split;
  nonce_revelation_rewards : stake_split;
  nonce_revelation_losses : int64;
  tzkt_quote : tzkt_quote;
  expected_dal_shards : int;
  expected_endorsements : int;
  future_endorsements : int;
  future_endorsement_rewards : int64;
  num_endorsements : int;
  endorsement_rewards : stake_split;
  missed_endorsements : int;
  missed_endorsement_rewards : int64;
  double_endorsing : double_op_info;
  double_preendorsing : double_op_info;
  tzkt_delegators : tzkt_delegator list;
  tzkt_stakers : tzkt_staker_entry list;
  tzkt_actual_stakers : tzkt_actual_staker list;
}

let empty_tzkt_baker_rewards ~cycle =
  {
    tzkt_cycle = cycle;
    own_delegated_balance = 0L;
    external_delegated_balance = 0L;
    delegators_count = 0;
    own_staked_balance = 0L;
    external_staked_balance = 0L;
    stakers_count = 0;
    issued_pseudotokens = "0";
    baking_power = 0L;
    total_baking_power = 0L;
    expected_blocks = 0;
    future_blocks = 0;
    future_block_rewards = 0L;
    num_blocks = 0;
    block_rewards_split = empty_stake_split;
    missed_blocks = 0;
    missed_block_rewards = 0L;
    expected_attestations = 0;
    future_attestations = 0;
    future_attestation_rewards = 0L;
    num_attestations = 0;
    attestation_rewards_split = empty_stake_split;
    missed_attestations = 0;
    missed_attestation_rewards = 0L;
    expected_dal_attestations = 0;
    future_dal_attestation_rewards = 0L;
    dal_attestation_rewards_split = empty_stake_split;
    missed_dal_attestation_rewards = 0L;
    tzkt_block_fees = 0L;
    missed_block_fees = 0L;
    double_baking = empty_double_op;
    double_consensus = empty_double_op;
    vdf_revelation_rewards = empty_stake_split;
    nonce_revelation_rewards = empty_stake_split;
    nonce_revelation_losses = 0L;
    tzkt_quote = empty_tzkt_quote;
    expected_dal_shards = 0;
    expected_endorsements = 0;
    future_endorsements = 0;
    future_endorsement_rewards = 0L;
    num_endorsements = 0;
    endorsement_rewards = empty_stake_split;
    missed_endorsements = 0;
    missed_endorsement_rewards = 0L;
    double_endorsing = empty_double_op;
    double_preendorsing = empty_double_op;
    tzkt_delegators = [];
    tzkt_stakers = [];
    tzkt_actual_stakers = [];
  }

let update_stake_split split (result : Data.Balance_update.result) total =
  match result with
  | Baker_own_stake | Contract ->
      {split with staked_own = Int64.add split.staked_own total}
  | Baker_edge -> {split with staked_edge = Int64.add split.staked_edge total}
  | Staker -> {split with staked_shared = Int64.add split.staked_shared total}
  | Delegate -> {split with ss_delegated = Int64.add split.ss_delegated total}
  | Lost -> split

let tzkt_baker_rewards_of_entries ~cycle ?(num_blocks = 0)
    ?(expected_blocks = 0) entries =
  let base = empty_tzkt_baker_rewards ~cycle in
  let base = {base with num_blocks; expected_blocks} in
  List.fold_left
    (fun r ((cat : Data.Balance_update.category), res, total) ->
      match cat with
      | Baking_rewards | Baking_bonuses -> (
          match (res : Data.Balance_update.result) with
          | Lost ->
              {
                r with
                missed_block_rewards = Int64.add r.missed_block_rewards total;
              }
          | _ ->
              {
                r with
                block_rewards_split =
                  update_stake_split r.block_rewards_split res total;
              })
      | Attestation_rewards -> (
          match (res : Data.Balance_update.result) with
          | Lost ->
              {
                r with
                missed_attestation_rewards =
                  Int64.add r.missed_attestation_rewards total;
              }
          | _ ->
              {
                r with
                attestation_rewards_split =
                  update_stake_split r.attestation_rewards_split res total;
              })
      | Dal_attestation_rewards -> (
          match (res : Data.Balance_update.result) with
          | Lost ->
              {
                r with
                missed_dal_attestation_rewards =
                  Int64.add r.missed_dal_attestation_rewards total;
              }
          | _ ->
              {
                r with
                dal_attestation_rewards_split =
                  update_stake_split r.dal_attestation_rewards_split res total;
              })
      | Block_fees -> (
          match (res : Data.Balance_update.result) with
          | Lost ->
              {r with missed_block_fees = Int64.add r.missed_block_fees total}
          | _ -> {r with tzkt_block_fees = Int64.add r.tzkt_block_fees total}))
    base
    entries

(* --- Data_encoding for TzKT types --- *)

let tzkt_quote_encoding =
  let open Data_encoding in
  conv
    (fun {btc; eur; usd; cny; jpy; krw; eth; gbp} ->
      (btc, eur, usd, cny, jpy, krw, eth, gbp))
    (fun (btc, eur, usd, cny, jpy, krw, eth, gbp) ->
      {btc; eur; usd; cny; jpy; krw; eth; gbp})
    (obj8
       (req "btc" float)
       (req "eur" float)
       (req "usd" float)
       (req "cny" float)
       (req "jpy" float)
       (req "krw" float)
       (req "eth" float)
       (req "gbp" float))

let tzkt_delegator_encoding =
  let open Data_encoding in
  conv
    (fun {del_address; delegated_balance; emptied} ->
      (del_address, delegated_balance, emptied))
    (fun (del_address, delegated_balance, emptied) ->
      {del_address; delegated_balance; emptied})
    (obj3
       (req "address" string)
       (req "delegatedBalance" int64)
       (req "emptied" bool))

let tzkt_staker_entry_encoding =
  let open Data_encoding in
  conv
    (fun {stk_address; staked_pseudotokens; staked_balance} ->
      (stk_address, staked_pseudotokens, staked_balance))
    (fun (stk_address, staked_pseudotokens, staked_balance) ->
      {stk_address; staked_pseudotokens; staked_balance})
    (obj3
       (req "address" string)
       (req "stakedPseudotokens" string)
       (req "stakedBalance" int64))

let tzkt_actual_staker_encoding =
  let open Data_encoding in
  conv
    (fun {as_address; initial_stake; final_stake; as_rewards} ->
      (as_address, initial_stake, final_stake, as_rewards))
    (fun (as_address, initial_stake, final_stake, as_rewards) ->
      {as_address; initial_stake; final_stake; as_rewards})
    (obj4
       (req "address" string)
       (req "initialStake" int64)
       (req "finalStake" int64)
       (req "rewards" int64))

let stake_split_enc prefix =
  let open Data_encoding in
  conv
    (fun {ss_delegated; staked_own; staked_edge; staked_shared} ->
      (ss_delegated, staked_own, staked_edge, staked_shared))
    (fun (ss_delegated, staked_own, staked_edge, staked_shared) ->
      {ss_delegated; staked_own; staked_edge; staked_shared})
    (obj4
       (req (prefix ^ "Delegated") int64)
       (req (prefix ^ "StakedOwn") int64)
       (req (prefix ^ "StakedEdge") int64)
       (req (prefix ^ "StakedShared") int64))

let double_op_enc prefix =
  let open Data_encoding in
  conv
    (fun {
           dbl_rewards;
           lost_staked;
           lost_unstaked;
           lost_external_staked;
           lost_external_unstaked;
         }
       ->
      ( dbl_rewards,
        lost_staked,
        lost_unstaked,
        lost_external_staked,
        lost_external_unstaked ))
    (fun ( dbl_rewards,
           lost_staked,
           lost_unstaked,
           lost_external_staked,
           lost_external_unstaked )
       ->
      {
        dbl_rewards;
        lost_staked;
        lost_unstaked;
        lost_external_staked;
        lost_external_unstaked;
      })
    (obj5
       (req (prefix ^ "Rewards") int64)
       (req (prefix ^ "LostStaked") int64)
       (req (prefix ^ "LostUnstaked") int64)
       (req (prefix ^ "LostExternalStaked") int64)
       (req (prefix ^ "LostExternalUnstaked") int64))

(* Main encoding: 9 sections merged into a flat JSON object *)
let tzkt_baker_rewards_encoding =
  let open Data_encoding in
  let overview =
    obj10
      (req "cycle" int32)
      (req "ownDelegatedBalance" int64)
      (req "externalDelegatedBalance" int64)
      (req "delegatorsCount" int31)
      (req "ownStakedBalance" int64)
      (req "externalStakedBalance" int64)
      (req "stakersCount" int31)
      (req "issuedPseudotokens" string)
      (req "bakingPower" int64)
      (req "totalBakingPower" int64)
  in
  let blocks =
    merge_objs
      (obj4
         (req "expectedBlocks" int31)
         (req "futureBlocks" int31)
         (req "futureBlockRewards" int64)
         (req "blocks" int31))
      (merge_objs
         (stake_split_enc "blockRewards")
         (obj4
            (req "missedBlocks" int31)
            (req "missedBlockRewards" int64)
            (req "blockFees" int64)
            (req "missedBlockFees" int64)))
  in
  let attestations =
    merge_objs
      (obj4
         (req "expectedAttestations" int31)
         (req "futureAttestations" int31)
         (req "futureAttestationRewards" int64)
         (req "attestations" int31))
      (merge_objs
         (stake_split_enc "attestationRewards")
         (obj2
            (req "missedAttestations" int31)
            (req "missedAttestationRewards" int64)))
  in
  let dal =
    merge_objs
      (obj2
         (req "expectedDalAttestations" int31)
         (req "futureDalAttestationRewards" int64))
      (merge_objs
         (stake_split_enc "dalAttestationRewards")
         (obj2
            (req "missedDalAttestationRewards" int64)
            (req "expectedDalShards" int31)))
  in
  let double_ops =
    merge_objs (double_op_enc "doubleBaking") (double_op_enc "doubleConsensus")
  in
  let revelations =
    merge_objs
      (stake_split_enc "vdfRevelationRewards")
      (merge_objs
         (stake_split_enc "nonceRevelationRewards")
         (obj1 (req "nonceRevelationLosses" int64)))
  in
  let quotes = obj1 (req "quote" tzkt_quote_encoding) in
  let deprecated_endorsements =
    merge_objs
      (merge_objs
         (obj4
            (req "expectedEndorsements" int31)
            (req "futureEndorsements" int31)
            (req "futureEndorsementRewards" int64)
            (req "endorsements" int31))
         (stake_split_enc "endorsementRewards"))
      (merge_objs
         (obj2
            (req "missedEndorsements" int31)
            (req "missedEndorsementRewards" int64))
         (merge_objs
            (double_op_enc "doubleEndorsing")
            (double_op_enc "doublePreendorsing")))
  in
  let participants =
    obj3
      (req "delegators" (list tzkt_delegator_encoding))
      (req "stakers" (list tzkt_staker_entry_encoding))
      (req "actualStakers" (list tzkt_actual_staker_encoding))
  in
  conv
    (fun r ->
      ( (* overview *)
        ( r.tzkt_cycle,
          r.own_delegated_balance,
          r.external_delegated_balance,
          r.delegators_count,
          r.own_staked_balance,
          r.external_staked_balance,
          r.stakers_count,
          r.issued_pseudotokens,
          r.baking_power,
          r.total_baking_power ),
        ( (* blocks *)
          ( ( r.expected_blocks,
              r.future_blocks,
              r.future_block_rewards,
              r.num_blocks ),
            ( r.block_rewards_split,
              ( r.missed_blocks,
                r.missed_block_rewards,
                r.tzkt_block_fees,
                r.missed_block_fees ) ) ),
          ( (* attestations *)
            ( ( r.expected_attestations,
                r.future_attestations,
                r.future_attestation_rewards,
                r.num_attestations ),
              ( r.attestation_rewards_split,
                (r.missed_attestations, r.missed_attestation_rewards) ) ),
            ( (* dal *)
              ( (r.expected_dal_attestations, r.future_dal_attestation_rewards),
                ( r.dal_attestation_rewards_split,
                  (r.missed_dal_attestation_rewards, r.expected_dal_shards) ) ),
              ( (* double_ops *)
                (r.double_baking, r.double_consensus),
                ( (* revelations *)
                  ( r.vdf_revelation_rewards,
                    (r.nonce_revelation_rewards, r.nonce_revelation_losses) ),
                  ( (* quotes *)
                    r.tzkt_quote,
                    ( (* deprecated_endorsements *)
                      ( ( ( r.expected_endorsements,
                            r.future_endorsements,
                            r.future_endorsement_rewards,
                            r.num_endorsements ),
                          r.endorsement_rewards ),
                        ( (r.missed_endorsements, r.missed_endorsement_rewards),
                          (r.double_endorsing, r.double_preendorsing) ) ),
                      (* participants *)
                      (r.tzkt_delegators, r.tzkt_stakers, r.tzkt_actual_stakers)
                    ) ) ) ) ) ) ) ))
    (fun ( ( tzkt_cycle,
             own_delegated_balance,
             external_delegated_balance,
             delegators_count,
             own_staked_balance,
             external_staked_balance,
             stakers_count,
             issued_pseudotokens,
             baking_power,
             total_baking_power ),
           ( ( (expected_blocks, future_blocks, future_block_rewards, num_blocks),
               ( block_rewards_split,
                 ( missed_blocks,
                   missed_block_rewards,
                   tzkt_block_fees,
                   missed_block_fees ) ) ),
             ( ( ( expected_attestations,
                   future_attestations,
                   future_attestation_rewards,
                   num_attestations ),
                 ( attestation_rewards_split,
                   (missed_attestations, missed_attestation_rewards) ) ),
               ( ( (expected_dal_attestations, future_dal_attestation_rewards),
                   ( dal_attestation_rewards_split,
                     (missed_dal_attestation_rewards, expected_dal_shards) ) ),
                 ( (double_baking, double_consensus),
                   ( ( vdf_revelation_rewards,
                       (nonce_revelation_rewards, nonce_revelation_losses) ),
                     ( tzkt_quote,
                       ( ( ( ( expected_endorsements,
                               future_endorsements,
                               future_endorsement_rewards,
                               num_endorsements ),
                             endorsement_rewards ),
                           ( (missed_endorsements, missed_endorsement_rewards),
                             (double_endorsing, double_preendorsing) ) ),
                         (tzkt_delegators, tzkt_stakers, tzkt_actual_stakers) )
                     ) ) ) ) ) ) )
       ->
      {
        tzkt_cycle;
        own_delegated_balance;
        external_delegated_balance;
        delegators_count;
        own_staked_balance;
        external_staked_balance;
        stakers_count;
        issued_pseudotokens;
        baking_power;
        total_baking_power;
        expected_blocks;
        future_blocks;
        future_block_rewards;
        num_blocks;
        block_rewards_split;
        missed_blocks;
        missed_block_rewards;
        tzkt_block_fees;
        missed_block_fees;
        expected_attestations;
        future_attestations;
        future_attestation_rewards;
        num_attestations;
        attestation_rewards_split;
        missed_attestations;
        missed_attestation_rewards;
        expected_dal_attestations;
        future_dal_attestation_rewards;
        dal_attestation_rewards_split;
        missed_dal_attestation_rewards;
        expected_dal_shards;
        double_baking;
        double_consensus;
        vdf_revelation_rewards;
        nonce_revelation_rewards;
        nonce_revelation_losses;
        tzkt_quote;
        expected_endorsements;
        future_endorsements;
        future_endorsement_rewards;
        num_endorsements;
        endorsement_rewards;
        missed_endorsements;
        missed_endorsement_rewards;
        double_endorsing;
        double_preendorsing;
        tzkt_delegators;
        tzkt_stakers;
        tzkt_actual_stakers;
      })
    (merge_objs
       overview
       (merge_objs
          blocks
          (merge_objs
             attestations
             (merge_objs
                dal
                (merge_objs
                   double_ops
                   (merge_objs
                      revelations
                      (merge_objs
                         quotes
                         (merge_objs deprecated_endorsements participants))))))))

let tzkt_baker_rewards_to_json r =
  Data_encoding.Json.construct tzkt_baker_rewards_encoding r
