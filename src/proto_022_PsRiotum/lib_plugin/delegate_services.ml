(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Environment
open Environment.Error_monad
open Protocol
open Protocol.Alpha_context
open Services_registration_plugin

type error += Balance_rpc_non_delegate of public_key_hash

type error += (* `Temporary *) Not_registered of Signature.Public_key_hash.t

let () =
  register_error_kind
    `Temporary
    ~id:"delegate.not_registered"
    ~title:"Not a registered delegate"
    ~description:
      "The provided public key hash is not the address of a registered \
       delegate."
    ~pp:(fun ppf pkh ->
      Format.fprintf
        ppf
        "The provided public key hash (%a) is not the address of a registered \
         delegate. If you own this account and want to register it as a \
         delegate, use a delegation operation to delegate the account to \
         itself."
        Signature.Public_key_hash.pp
        pkh)
    Data_encoding.(obj1 (req "pkh" Signature.Public_key_hash.encoding))
    (function Not_registered pkh -> Some pkh | _ -> None)
    (fun pkh -> Not_registered pkh)

let () =
  register_error_kind
    `Temporary
    ~id:"delegate_service.balance_rpc_on_non_delegate"
    ~title:"Balance request for an unregistered delegate"
    ~description:"The account whose balance was requested is not a delegate."
    ~pp:(fun ppf pkh ->
      Format.fprintf
        ppf
        "The implicit account (%a) whose balance was requested is not a \
         registered delegate. To get the balance of this account you can use \
         the ../context/contracts/%a/balance RPC."
        Signature.Public_key_hash.pp
        pkh
        Signature.Public_key_hash.pp
        pkh)
    Data_encoding.(obj1 (req "pkh" Signature.Public_key_hash.encoding))
    (function Balance_rpc_non_delegate pkh -> Some pkh | _ -> None)
    (fun pkh -> Balance_rpc_non_delegate pkh)

let stakers_encoding =
  let open Data_encoding in
  let staker_enconding =
    obj2
      (req "staker" Alpha_context.Contract.implicit_encoding)
      (req "frozen_deposits" Tez.encoding)
  in
  list staker_enconding

type consensus_key = {
  consensus_key_pkh : Signature.Public_key_hash.t;
  consensus_key_pk : Signature.Public_key.t;
}

let consensus_key_encoding =
  let open Data_encoding in
  conv
    (fun {consensus_key_pkh; consensus_key_pk} ->
      (consensus_key_pkh, consensus_key_pk))
    (fun (consensus_key_pkh, consensus_key_pk) ->
      {consensus_key_pkh; consensus_key_pk})
    (obj2
       (req "pkh" Signature.Public_key_hash.encoding)
       (req "pk" Signature.Public_key.encoding))

type consensus_keys_info = {
  active : consensus_key;
  pendings : (Cycle.t * consensus_key) list;
}

let consensus_key_info_encoding =
  let open Data_encoding in
  conv
    (fun {active; pendings} -> (active, pendings))
    (fun (active, pendings) -> {active; pendings})
    (obj2
       (req "active" consensus_key_encoding)
       (dft
          "pendings"
          (list
             (merge_objs
                (obj1 (req "cycle" Cycle.encoding))
                consensus_key_encoding))
          []))

let participation_info_encoding =
  let open Data_encoding in
  conv
    (fun Delegate.For_RPC.
           {
             expected_cycle_activity;
             minimal_cycle_activity;
             missed_slots;
             missed_levels;
             remaining_allowed_missed_slots;
             expected_attesting_rewards;
           }
       ->
      ( expected_cycle_activity,
        minimal_cycle_activity,
        missed_slots,
        missed_levels,
        remaining_allowed_missed_slots,
        expected_attesting_rewards ))
    (fun ( expected_cycle_activity,
           minimal_cycle_activity,
           missed_slots,
           missed_levels,
           remaining_allowed_missed_slots,
           expected_attesting_rewards )
       ->
      {
        expected_cycle_activity;
        minimal_cycle_activity;
        missed_slots;
        missed_levels;
        remaining_allowed_missed_slots;
        expected_attesting_rewards;
      })
    (obj6
       (req "expected_cycle_activity" int31)
       (req "minimal_cycle_activity" int31)
       (req "missed_slots" int31)
       (req "missed_levels" int31)
       (req "remaining_allowed_missed_slots" int31)
       (req "expected_attesting_rewards" Tez.encoding))

let dal_participation_info_encoding =
  let open Data_encoding in
  conv
    (fun Delegate.For_RPC.
           {
             expected_assigned_shards_per_slot;
             delegate_attested_dal_slots;
             delegate_attestable_dal_slots;
             expected_dal_rewards;
             sufficient_dal_participation;
             denounced;
           }
       ->
      ( expected_assigned_shards_per_slot,
        delegate_attested_dal_slots,
        delegate_attestable_dal_slots,
        expected_dal_rewards,
        sufficient_dal_participation,
        denounced ))
    (fun ( expected_assigned_shards_per_slot,
           delegate_attested_dal_slots,
           delegate_attestable_dal_slots,
           expected_dal_rewards,
           sufficient_dal_participation,
           denounced )
       ->
      {
        expected_assigned_shards_per_slot;
        delegate_attested_dal_slots;
        delegate_attestable_dal_slots;
        expected_dal_rewards;
        sufficient_dal_participation;
        denounced;
      })
    (obj6
       (req "expected_assigned_shards_per_slot" int31)
       (req "delegate_attested_dal_slots" int31)
       (req "delegate_attestable_dal_slots" int31)
       (req "expected_dal_rewards" Tez.encoding)
       (req "sufficient_dal_participation" bool)
       (req "denounced" bool))

type deposit_per_cycle = {cycle : Cycle.t; deposit : Tez.t}

let deposit_per_cycle_encoding : deposit_per_cycle Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {cycle; deposit} -> (cycle, deposit))
    (fun (cycle, deposit) -> {cycle; deposit})
    (obj2 (req "cycle" Cycle.encoding) (req "deposit" Tez.encoding))

type unstaked_per_cycle = deposit_per_cycle list

let unstaked_per_cycle_encoding = Data_encoding.list deposit_per_cycle_encoding

type pending_staking_parameters = Cycle.t * Staking_parameters_repr.t

let pending_staking_parameters_encoding :
    pending_staking_parameters Data_encoding.t =
  let open Data_encoding in
  obj2
    (req "cycle" Cycle.encoding)
    (req "parameters" Staking_parameters_repr.encoding)

let min_delegated_in_current_cycle_encoding =
  let open Data_encoding in
  conv
    (fun (min_delegated, anchor) -> (min_delegated, anchor))
    (fun (min_delegated, anchor) -> (min_delegated, anchor))
    (obj2 (req "amount" Tez.encoding) (opt "level" Level_repr.encoding))

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7369
   update tests to use new_info instead, then remove this type *)
type info = {
  full_balance : Tez.t;
  current_frozen_deposits : Tez.t;
  frozen_deposits : Tez.t;
  staking_balance : Tez.t;
  frozen_deposits_limit : Tez.t option;
  delegated_contracts : Alpha_context.Contract.t list;
  delegated_balance : Tez.t;
  min_delegated_in_current_cycle : Tez.t * Level_repr.t option;
  total_delegated_stake : Tez.t;
  staking_denominator : Staking_pseudotoken.t;
  deactivated : bool;
  grace_period : Cycle.t;
  pending_denunciations : bool;
  voting_info : Vote.delegate_info;
  active_consensus_key : Signature.Public_key_hash.t;
  pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
}

type new_info = {
  (*
     General baking information *)
  deactivated : bool;
  is_forbidden : bool;
  participation : Delegate.For_RPC.participation_info;
  (* The dal_participation is an optional value for the case where DAL
     incentives are disabled. *)
  dal_participation : Delegate.For_RPC.dal_participation_info option;
  grace_period : Cycle.t;
  active_staking_parameters : Staking_parameters_repr.t;
  pending_staking_parameters : pending_staking_parameters list;
  (*
     Baking rights *)
  baking_power : int64;
  total_staked (* old name: current_frozen_deposits *) : Tez.t;
  total_delegated (* new *) : Tez.t;
  min_delegated_in_current_cycle : Tez.t * Level_repr.t option;
  own_full_balance (* old name: full_balance *) : Tez.t;
  own_staked (* new *) : Tez.t;
  own_delegated (* new *) : Tez.t;
  external_staked (* old name: total_delegated_stake *) : Tez.t;
  external_delegated (* new *) : Tez.t;
  total_unstaked_per_cycle
    (* old RPC name: unstaked_frozen_deposits; was not in info *) :
    unstaked_per_cycle;
  denunciations (* replaces pending_denunciations *) : Denunciations_repr.t;
  estimated_shared_pending_slashed_amount (* new *) : Tez.t;
  staking_denominator : Staking_pseudotoken.t;
  (*
    Voting *)
  current_voting_power : int64;
  voting_power : int64;
  voting_info : Vote.delegate_info;
  (*
    Consensus key *)
  consensus_key
    (* corresponds to old active_consensus_key and pending_consensus_keys *) :
    consensus_keys_info;
  (*
    Chunked RPCs at the end, because they might be arbitrarily large *)
  stakers : (public_key_hash * Tez.t) list;
  delegators (* old name: delegated_contracts *) : Contract.t list;
}
(* Removed:
   - frozen_deposits_limit (has no effects)
   - frozen_deposits (equals total_staked on last block three cycles ago)
   - staking_balance (equals total_staked + total_delegated
   - delegated_balance (equals external_staked + external_delegated) *)

let conv26 ty =
  Data_encoding.conv
    (fun ( x0,
           x1,
           x2,
           x3,
           x4,
           x5,
           x6,
           x7,
           x8,
           x9,
           x10,
           x11,
           x12,
           x13,
           x14,
           x15,
           x16,
           x17,
           x18,
           x19,
           x20,
           x21,
           x22,
           x23,
           x24,
           x25 )
       ->
      ( (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9),
        ( (x10, x11, x12, x13, x14, x15, x16, x17, x18, x19),
          (x20, x21, x22, x23, x24, x25) ) ))
    (fun ( (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9),
           ( (x10, x11, x12, x13, x14, x15, x16, x17, x18, x19),
             (x20, x21, x22, x23, x24, x25) ) )
       ->
      ( x0,
        x1,
        x2,
        x3,
        x4,
        x5,
        x6,
        x7,
        x8,
        x9,
        x10,
        x11,
        x12,
        x13,
        x14,
        x15,
        x16,
        x17,
        x18,
        x19,
        x20,
        x21,
        x22,
        x23,
        x24,
        x25 ))
    ty

let obj26 f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19
    f20 f21 f22 f23 f24 f25 =
  conv26
    Data_encoding.(
      merge_objs
        (obj10 f0 f1 f2 f3 f4 f5 f6 f7 f8 f9)
        (merge_objs
           (obj10 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19)
           (obj6 f20 f21 f22 f23 f24 f25)))

let info_encoding =
  let open Data_encoding in
  conv
    (fun {
           (* General baking information *)
           deactivated;
           is_forbidden;
           participation;
           dal_participation;
           grace_period;
           active_staking_parameters;
           pending_staking_parameters;
           (* Baking rights *)
           baking_power;
           total_staked;
           total_delegated;
           min_delegated_in_current_cycle;
           own_full_balance;
           own_staked;
           own_delegated;
           external_staked;
           external_delegated;
           total_unstaked_per_cycle;
           denunciations;
           estimated_shared_pending_slashed_amount;
           staking_denominator;
           (* Voting *)
           current_voting_power;
           voting_power;
           voting_info;
           (* Consensus key *)
           consensus_key;
           (* Chunked RPCs *)
           stakers;
           delegators;
         }
       ->
      ( (* General baking information *)
        deactivated,
        is_forbidden,
        participation,
        dal_participation,
        grace_period,
        active_staking_parameters,
        pending_staking_parameters,
        (* Baking rights *)
        baking_power,
        total_staked,
        total_delegated,
        min_delegated_in_current_cycle,
        own_full_balance,
        own_staked,
        own_delegated,
        external_staked,
        external_delegated,
        total_unstaked_per_cycle,
        denunciations,
        estimated_shared_pending_slashed_amount,
        staking_denominator,
        (* Voting *)
        current_voting_power,
        voting_power,
        voting_info,
        (* Consensus key *)
        consensus_key,
        (* Chunked RPCs *)
        stakers,
        delegators ))
    (fun ( (* General baking information *)
           deactivated,
           is_forbidden,
           participation,
           dal_participation,
           grace_period,
           active_staking_parameters,
           pending_staking_parameters,
           (* Baking rights *)
           baking_power,
           total_staked,
           total_delegated,
           min_delegated_in_current_cycle,
           own_full_balance,
           own_staked,
           own_delegated,
           external_staked,
           external_delegated,
           total_unstaked_per_cycle,
           denunciations,
           estimated_shared_pending_slashed_amount,
           staking_denominator,
           (* Voting *)
           current_voting_power,
           voting_power,
           voting_info,
           (* Consensus key *)
           consensus_key,
           (* Chunked RPCs *)
           stakers,
           delegators )
       ->
      {
        (* General baking information *)
        deactivated;
        is_forbidden;
        participation;
        dal_participation;
        grace_period;
        active_staking_parameters;
        pending_staking_parameters;
        (* Baking rights *)
        baking_power;
        total_staked;
        total_delegated;
        min_delegated_in_current_cycle;
        own_full_balance;
        own_staked;
        own_delegated;
        external_staked;
        external_delegated;
        total_unstaked_per_cycle;
        denunciations;
        estimated_shared_pending_slashed_amount;
        staking_denominator;
        (* Voting *)
        current_voting_power;
        voting_power;
        voting_info;
        (* Consensus key *)
        consensus_key;
        (* Chunked RPCs *)
        stakers;
        delegators;
      })
    (obj26
       (* General baking information *)
       (req "deactivated" bool)
       (req "is_forbidden" bool)
       (req "participation" participation_info_encoding)
       (opt "dal_participation" dal_participation_info_encoding)
       (req "grace_period" Cycle.encoding)
       (req "active_staking_parameters" Staking_parameters_repr.encoding)
       (req
          "pending_staking_parameters"
          (list pending_staking_parameters_encoding))
       (* Baking rights *)
       (req "baking_power" int64)
       (req "total_staked" Tez.encoding)
       (req "total_delegated" Tez.encoding)
       (req
          "min_delegated_in_current_cycle"
          min_delegated_in_current_cycle_encoding)
       (req "own_full_balance" Tez.encoding)
       (req "own_staked" Tez.encoding)
       (req "own_delegated" Tez.encoding)
       (req "external_staked" Tez.encoding)
       (req "external_delegated" Tez.encoding)
       (req "total_unstaked_per_cycle" unstaked_per_cycle_encoding)
       (req "denunciations" Denunciations_repr.encoding)
       (req "estimated_shared_pending_slashed_amount" Tez.encoding)
       (req "staking_denominator" Staking_pseudotoken.For_RPC.encoding)
       (* Voting *)
       (req "current_voting_power" int64)
       (req "voting_power" int64)
       (req "voting_info" Vote.delegate_info_encoding)
       (* Consensus key *)
       (req "consensus_key" consensus_key_info_encoding)
       (* Chunked RPCs *)
       (req "stakers" stakers_encoding)
       (req "delegators" (list Contract.encoding)))

module S = struct
  let context_path :
      ( Tezos_protocol_environment__Environment_context.rpc_context,
        Tezos_protocol_environment__Environment_context.rpc_context )
      Resto.Path.t =
    RPC_path.(open_root / "context")

  let raw_path = RPC_path.(open_root / "context" / "delegates")

  open Data_encoding

  type list_query = {
    active : bool;
    inactive : bool;
    with_minimal_stake : bool;
    without_minimal_stake : bool;
  }

  let list_query : list_query RPC_query.t =
    let open RPC_query in
    query (fun active inactive with_minimal_stake without_minimal_stake ->
        {active; inactive; with_minimal_stake; without_minimal_stake})
    |+ flag "active" (fun t -> t.active)
    |+ flag "inactive" (fun t -> t.inactive)
    |+ flag "with_minimal_stake" (fun t -> t.with_minimal_stake)
    |+ flag "without_minimal_stake" (fun t -> t.without_minimal_stake)
    |> seal

  let list_delegate =
    RPC_service.get_service
      ~description:
        "Lists all registered delegates by default. The arguments `active`, \
         `inactive`, `with_minimal_stake`, and `without_minimal_stake` allow \
         to enumerate only the delegates that are active, inactive, have at \
         least a minimal stake to participate in consensus and in governance, \
         or do not have such a minimal stake, respectively. Note, setting \
         these arguments to false has no effect."
      ~query:list_query
      ~output:(list Signature.Public_key_hash.encoding)
      raw_path

  let total_currently_staked =
    RPC_service.get_service
      ~description:
        "Returns the amount of staked tez by delegates, delegators or \
         overstaked."
      ~query:RPC_query.empty
      ~output:
        Data_encoding.(
          obj3
            (req "delegates" Tez_repr.encoding)
            (req "delegators" Tez_repr.encoding)
            (req "overstaked" Tez_repr.encoding))
      RPC_path.(context_path / "total_currently_staked")

  let path = RPC_path.(raw_path /: Signature.Public_key_hash.rpc_arg)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7383 *)
  module Deprecated = struct
    let full_balance =
      RPC_service.get_service
        ~description:"DEPRECATED; use own_full_balance instead."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "full_balance")

    let current_frozen_deposits =
      RPC_service.get_service
        ~description:"DEPRECATED; use total_staked instead."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "current_frozen_deposits")

    let staking_balance =
      RPC_service.get_service
        ~description:
          "DEPRECATED; to get this value, you can call RPCs total_staked and \
           total_delegated, and add their outputs together."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "staking_balance")

    let total_delegated_stake =
      RPC_service.get_service
        ~description:"DEPRECATED; use external_staked instead."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "total_delegated_stake")

    let delegated_balance =
      RPC_service.get_service
        ~description:
          "DEPRECATED; to get this value, you can call RPCs external_staked \
           and external_delegated, and add their outputs together."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "delegated_balance")

    let frozen_deposits =
      RPC_service.get_service
        ~description:
          "DEPRECATED; call RPC total_staked on the last block of \
           (current_cycle - 3) instead. Returns the total amount (in mutez) \
           that was staked for the baker by all stakers (including the baker \
           itself) at the time the staking rights for the current cycle were \
           computed."
        ~query:RPC_query.empty
        ~output:Tez.encoding
        RPC_path.(path / "frozen_deposits")

    let frozen_deposits_limit =
      RPC_service.get_service
        ~description:
          "DEPRECATED; the frozen deposits limit has no effects since the \
           activation of Adaptive Issuance and Staking during the Paris \
           protocol."
        ~query:RPC_query.empty
        ~output:(Data_encoding.option Tez.encoding)
        RPC_path.(path / "frozen_deposits_limit")

    let current_baking_power =
      RPC_service.get_service
        ~description:"DEPRECATED; use baking_power instead."
        ~query:RPC_query.empty
        ~output:Data_encoding.int64
        RPC_path.(path / "current_baking_power")

    let delegated_contracts =
      RPC_service.get_service
        ~description:"DEPRECATED; use delegators instead."
        ~query:RPC_query.empty
        ~output:(list Contract.encoding)
        RPC_path.(path / "delegated_contracts")

    let unstaked_frozen_deposits =
      RPC_service.get_service
        ~description:"DEPRECATED; use total_unstaked_per_cycle instead."
        ~query:RPC_query.empty
        ~output:unstaked_per_cycle_encoding
        RPC_path.(path / "unstaked_frozen_deposits")
  end

  let is_forbidden =
    RPC_service.get_service
      ~description:
        "Returns true if the delegate is forbidden to participate in consensus."
      ~query:RPC_query.empty
      ~output:Data_encoding.bool
      RPC_path.(path / "is_forbidden")

  let stakers =
    RPC_service.get_service
      ~description:
        "Returns the list of accounts that stake to a given delegate together \
         with their share of the frozen deposits."
      ~query:RPC_query.empty
      ~output:stakers_encoding
      RPC_path.(path / "stakers")

  let own_full_balance =
    RPC_service.get_service
      ~description:
        "The full balance (in mutez) of tokens owned by the delegate itself. \
         Includes its spendable balance, staked tez, unstake requests, and \
         frozen bonds. Does not include any tokens owned by external \
         delegators. This RPC fails when the pkh is not a delegate. When it is \
         a delegate, this RPC outputs the same amount as \
         ../<block_id>/context/contracts/<delegate_contract_id>/full_balance."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "own_full_balance")

  let total_staked =
    RPC_service.get_service
      ~description:
        "The total amount (in mutez) currently staked for the baker, both by \
         the baker itself and by external stakers. This is the staked amount \
         before applying the baker's 'limit_of_staking_over_baking'; in other \
         words, it includes overstaked tez if there are any."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "total_staked")

  let total_unstaked_per_cycle =
    RPC_service.get_service
      ~description:
        "For each cycle, returns the total amount (in mutez) contained in all \
         unstake requests created during this cycle by all delegators, \
         including the baker itself. Note that these tokens count as delegated \
         to the baker for the purpose of computing baking rights, and are \
         included in the amount returned by the total_delegated RPC."
      ~query:RPC_query.empty
      ~output:unstaked_per_cycle_encoding
      RPC_path.(path / "total_unstaked_per_cycle")

  let total_delegated =
    RPC_service.get_service
      ~description:
        "All tokens (in mutez) that currently count as delegated for the \
         purpose of computing the baker's rights; they weigh half as much as \
         staked tez in the rights. Limits such as overstaking and \
         overdelegation have not been applied yet. This corresponds to all \
         non-staked tez owned by the baker's delegators (including the baker \
         itself): spendable balances, frozen bonds, and unstaked requests, \
         except for any unstake requests that have been created before the \
         delegator changed its delegate to the current baker (because they \
         still count as delegated for the old delegate instead)."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "total_delegated")

  let delegators =
    RPC_service.get_service
      ~description:
        "The list of all contracts that are currently delegating to the \
         delegate. Includes both user accounts and smart contracts. Includes \
         the delegate itself."
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      RPC_path.(path / "delegators")

  let own_staked =
    RPC_service.get_service
      ~description:
        "The amount (in mutez) currently owned and staked by the baker itself. \
         Returns the same value as \
         ../<block_id>/context/contracts/<delegate_contract_id>/staked_balance \
         (except for the fact that the present RPC fails if the \
         public_key_hash in the path is not a delegate)."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "own_staked")

  let own_delegated =
    RPC_service.get_service
      ~description:
        "The amount (in mutez) currently owned by the baker itself and \
         counting as delegated for the purpose of baking rights. This \
         corresponds to all non-staked tokens owned by the baker: spendable \
         balance, frozen bonds, and unstake requests. (Note: There is one \
         exception: if the baker still has unstake requests created at a time \
         when it was delegating to a different delegate, then these unstake \
         requests still count as delegated to the former delegate. Any such \
         unstake requests are excluded from the amount returned by the present \
         RPC, despite being non-staked tokens owned by the baker.)"
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "own_delegated")

  let external_staked =
    RPC_service.get_service
      ~description:
        "The sum (in mutez) of all tokens currently staked by the baker's \
         external delegators. This excludes the baker's own staked tokens."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "external_staked")

  let external_delegated =
    RPC_service.get_service
      ~description:
        "The sum (in mutez) of non-staked tokens that currently count as \
         delegated to the baker, excluding those owned by the baker iself. \
         Does not take limits such as overstaking or overdelegation into \
         account. This includes the spendable balances and frozen bonds of all \
         the baker's external delegators. It also includes unstake requests of \
         contracts other than the baker, on the condition that the contract \
         was delegating to the baker at the time of the unstake operation. So \
         this includes most but not all unstake requests from current \
         delegators, and might include some unstake requests from old \
         delegators. Limits such as overstaking and overdelegation have not \
         been applied yet."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "external_delegated")

  let staking_denominator =
    RPC_service.get_service
      ~description:
        "Returns an abstract representation of the total delegated stake."
      ~query:RPC_query.empty
      ~output:Staking_pseudotoken.For_RPC.encoding
      RPC_path.(path / "staking_denominator")

  let deactivated =
    RPC_service.get_service
      ~description:
        "Tells whether the delegate is currently tagged as deactivated or not."
      ~query:RPC_query.empty
      ~output:bool
      RPC_path.(path / "deactivated")

  let grace_period =
    RPC_service.get_service
      ~description:
        "Returns the cycle by the end of which the delegate might be \
         deactivated if she fails to execute any delegate action. A \
         deactivated delegate might be reactivated (without loosing any stake) \
         by simply re-registering as a delegate. For deactivated delegates, \
         this value contains the cycle at which they were deactivated."
      ~query:RPC_query.empty
      ~output:Cycle.encoding
      RPC_path.(path / "grace_period")

  let current_voting_power =
    RPC_service.get_service
      ~description:
        "The voting power of a given delegate, as computed from its current \
         stake."
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "current_voting_power")

  let voting_power =
    RPC_service.get_service
      ~description:"The voting power in the vote listings for a given delegate."
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "voting_power")

  let baking_power =
    RPC_service.get_service
      ~description:
        "The current baking power of a delegate, using the current staked and \
         delegated balances of the baker and its delegators. In other words, \
         the baking rights that the baker would get for a future cycle if the \
         current cycle ended right at the current block."
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "baking_power")

  let voting_info =
    RPC_service.get_service
      ~description:
        "Returns the delegate info (e.g. voting power) found in the listings \
         of the current voting period."
      ~query:RPC_query.empty
      ~output:Vote.delegate_info_encoding
      RPC_path.(path / "voting_info")

  let consensus_key =
    RPC_service.get_service
      ~description:
        "The active consensus key for a given delegate and the pending \
         consensus keys."
      ~query:RPC_query.empty
      ~output:consensus_key_info_encoding
      RPC_path.(path / "consensus_key")

  let participation =
    RPC_service.get_service
      ~description:
        "Returns cycle and level participation information. In particular this \
         indicates, in the field 'expected_cycle_activity', the number of \
         slots the delegate is expected to have in the cycle based on its \
         active stake. The field 'minimal_cycle_activity' indicates the \
         minimal attesting slots in the cycle required to get attesting \
         rewards. It is computed based on 'expected_cycle_activity. The fields \
         'missed_slots' and 'missed_levels' indicate the number of missed \
         attesting slots and missed levels (for attesting) in the cycle so \
         far. 'missed_slots' indicates the number of missed attesting slots in \
         the cycle so far. The field 'remaining_allowed_missed_slots' \
         indicates the remaining amount of attesting slots that can be missed \
         in the cycle before forfeiting the rewards. Finally, \
         'expected_attesting_rewards' indicates the attesting rewards that \
         will be distributed at the end of the cycle if activity at that point \
         will be greater than the minimal required; if the activity is already \
         known to be below the required minimum, then the rewards are zero."
      ~query:RPC_query.empty
      ~output:participation_info_encoding
      RPC_path.(path / "participation")

  let dal_participation =
    RPC_service.get_service
      ~description:
        "Returns information about the delegate's participation in the \
         attestation of slots published into the Data Availability Layer (DAL) \
         during the current cycle. The field \
         'expected_assigned_shards_per_slot' indicates the expected number of \
         shards assigned to the delegate in the cycle per DAL slot. The field \
         'delegate_attested_dal_slots' represents the number of attested DAL \
         slots which are also attested by the delegate, while \
         'delegate_attestable_dal_slots' provides the total number of DAL \
         slots attested during the cycle for which the delegate had shards \
         assigned. The 'expected_dal_rewards' field specifies the expected \
         amount of rewards for the delegate based on DAL participation, \
         provided the delegate meets the required participation. Whether this \
         threshold is currently met is determined by the \
         'sufficient_dal_participation' flag, which is true if currently the \
         delegate has sufficiently participated in attesting DAL slots \
         declared to be attested by the protocol. Note that this flag may \
         evolve during the cycle. Also note, in particular, that if no DAL no \
         DAL slots have been globally attested during the cycle (i.e., when \
         'delegate_attestable_dal_slots' is zero), the flag is true. The \
         'denounced' field specifies whether the delegate was denounced for \
         not detecting traps during the current cycle."
      ~query:RPC_query.empty
      ~output:dal_participation_info_encoding
      RPC_path.(path / "dal_participation")

  let active_staking_parameters =
    RPC_service.get_service
      ~description:
        "Returns the currently active staking parameters for the given \
         delegate."
      ~query:RPC_query.empty
      ~output:Staking_parameters_repr.encoding
      RPC_path.(path / "active_staking_parameters")

  let pending_staking_parameters =
    RPC_service.get_service
      ~description:
        "Returns the pending values for the given delegate's staking \
         parameters."
      ~query:RPC_query.empty
      ~output:(list pending_staking_parameters_encoding)
      RPC_path.(path / "pending_staking_parameters")

  let pending_denunciations =
    RPC_service.get_service
      ~description:"Returns the pending denunciations for the given delegate."
      ~query:RPC_query.empty
      ~output:(list Denunciations_repr.item_encoding)
      RPC_path.(path / "denunciations")

  let estimated_shared_pending_slashed_amount =
    RPC_service.get_service
      ~description:
        "Returns the estimated shared pending slashed amount (in mutez) of a \
         given delegate."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "estimated_shared_pending_slashed_amount")

  let min_delegated_in_current_cycle =
    RPC_service.get_service
      ~description:
        "Returns the minimum of delegated tez (in mutez) during the current \
         cycle and the block level at the end of which the minimum was \
         reached. This only takes into account the value of `total_delegated` \
         at the end of each block, not in the middle of applying operations. \
         This is the delegated amount that would be used to compute the \
         delegate's future baking rights if the cycle ended at the current \
         block. If the minimum was reached multiple times, the returned level \
         is the earliest level of the current cycle that reached this minimum. \
         For instance, if `total_delegated` hasn't changed at all since the \
         beginning of the current cycle, returns the first level of the \
         current cycle. (If the contract is not registered as a delegate, \
         returns 0 mutez and omits the level.)"
      ~query:RPC_query.empty
      ~output:min_delegated_in_current_cycle_encoding
      RPC_path.(path / "min_delegated_in_current_cycle")

  let info =
    RPC_service.get_service
      ~description:
        "Everything about a delegate. Gathers the outputs of all RPCs with the \
         ../delegates/<pkh> prefix."
      ~query:RPC_query.empty
      ~output:info_encoding
      path
end

let check_delegate_registered ctxt pkh =
  let open Lwt_result_syntax in
  let*! is_registered = Delegate.registered ctxt pkh in
  match is_registered with
  | true -> return_unit
  | false -> tzfail (Not_registered pkh)

let consensus_key ctxt pkh =
  let open Lwt_result_syntax in
  let* {consensus_pk = consensus_key_pk; consensus_pkh = consensus_key_pkh; _} =
    Delegate.Consensus_key.active_pubkey ctxt pkh
  in
  let* pendings = Delegate.Consensus_key.pending_updates ctxt pkh in
  let pendings =
    List.map
      (fun (cycle, consensus_key_pkh, consensus_key_pk) ->
        (cycle, {consensus_key_pk; consensus_key_pkh}))
      pendings
  in
  return {active = {consensus_key_pk; consensus_key_pkh}; pendings}

let contract_stake ctxt ~delegator_contract ~delegate =
  let open Alpha_context in
  let open Lwt_result_syntax in
  let* staked_balance =
    Staking_pseudotokens.For_RPC.staked_balance
      ctxt
      ~contract:delegator_contract
      ~delegate
  in
  if not Tez.(staked_balance = zero) then
    let delegator_pkh =
      match delegator_contract with
      | Contract.Implicit pkh -> pkh
      | Contract.Originated _ -> assert false
      (* Originated contracts cannot stake *)
    in
    return @@ Some (delegator_pkh, staked_balance)
  else return_none

let delegators ctxt pkh = Delegate.delegated_contracts ctxt pkh

let stakers ctxt pkh =
  let open Lwt_result_syntax in
  let*! delegators = delegators ctxt pkh in
  List.filter_map_es
    (fun delegator_contract ->
      contract_stake ctxt ~delegator_contract ~delegate:pkh)
    delegators

let f_own_full_balance ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () =
    trace (Balance_rpc_non_delegate pkh) (check_delegate_registered ctxt pkh)
  in
  Delegate.For_RPC.full_balance ctxt pkh

let total_staked ctxt pkh = Delegate.current_frozen_deposits ctxt pkh

let f_total_staked ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  total_staked ctxt pkh

let total_staked_and_delegated ctxt pkh =
  Delegate.For_RPC.staking_balance ctxt pkh

let total_delegated ctxt pkh =
  let open Lwt_result_syntax in
  let* total_staked = total_staked ctxt pkh in
  let* total_staked_and_delegated = total_staked_and_delegated ctxt pkh in
  let*? total_delegated = Tez.(total_staked_and_delegated -? total_staked) in
  return total_delegated

let f_total_delegated ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  total_delegated ctxt pkh

let own_staked ctxt pkh =
  let open Lwt_result_syntax in
  let* own_staked_opt =
    Contract.For_RPC.get_staked_balance ctxt (Implicit pkh)
  in
  return (Option.value own_staked_opt ~default:Tez.zero)

let own_staked_and_delegated ctxt pkh =
  let open Lwt_result_syntax in
  let* own_full_balance = Delegate.For_RPC.full_balance ctxt pkh in
  let* own_unstake_requests =
    Contract_services.Implem.unstake_requests ctxt (Implicit pkh)
  in
  let* own_unstaked_from_other_delegates =
    match own_unstake_requests with
    | None -> return Tez.zero
    | Some {finalizable; unfinalizable} ->
        let* finalizable_sum =
          List.fold_left_es
            (fun acc (delegate, _, (amount : Tez.t)) ->
              if Signature.Public_key_hash.(delegate <> pkh) then
                Lwt.return Tez.(acc +? amount)
              else return acc)
            Tez.zero
            finalizable
        in
        let* unfinalizable_sum =
          if Signature.Public_key_hash.(unfinalizable.delegate <> pkh) then
            List.fold_left_es
              (fun acc (_, (amount : Tez.t)) -> Lwt.return Tez.(acc +? amount))
              Tez.zero
              unfinalizable.requests
          else return Tez.zero
        in
        Lwt.return Tez.(finalizable_sum +? unfinalizable_sum)
  in
  let*? own_staked_and_delegated =
    Tez.(own_full_balance -? own_unstaked_from_other_delegates)
  in
  return own_staked_and_delegated

let own_delegated ctxt pkh =
  let open Lwt_result_syntax in
  let* own_staked_and_delegated = own_staked_and_delegated ctxt pkh in
  let* own_staked = own_staked ctxt pkh in
  let*? own_delegated = Tez.(own_staked_and_delegated -? own_staked) in
  return own_delegated

let external_staked ctxt pkh =
  Staking_pseudotokens.For_RPC.get_frozen_deposits_staked_tez ctxt ~delegate:pkh

let f_external_staked ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  external_staked ctxt pkh

let external_staked_and_delegated ctxt pkh =
  let open Lwt_result_syntax in
  let* total_staked_and_delegated = total_staked_and_delegated ctxt pkh in
  let* own_staked_and_delegated = own_staked_and_delegated ctxt pkh in
  let*? external_staked_and_delegated =
    Tez.(total_staked_and_delegated -? own_staked_and_delegated)
  in
  return external_staked_and_delegated

let external_delegated ctxt pkh : Tez.t Environment.Error_monad.tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* external_staked_and_delegated = external_staked_and_delegated ctxt pkh in
  let* external_staked = external_staked ctxt pkh in
  let*? external_delegated =
    Tez.(external_staked_and_delegated -? external_staked)
  in
  return external_delegated

let f_external_delegated ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  external_delegated ctxt pkh

let total_unstaked_per_cycle ctxt pkh =
  let open Lwt_result_syntax in
  let ctxt_cycle = Cycle.current ctxt in
  let last_unslashable_cycle =
    Option.value ~default:Cycle.root
    @@ Cycle.greatest_unstake_finalizable_cycle ctxt
  in
  let cycles = Cycle.(last_unslashable_cycle ---> ctxt_cycle) in
  List.map_es
    (fun cycle ->
      let* deposit = Unstaked_frozen_deposits.balance ctxt pkh cycle in
      return {cycle; deposit})
    cycles

let f_baking_power ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  Stake_distribution.For_RPC.delegate_current_baking_power ctxt pkh

let f_delegators ctxt pkh () () =
  let open Lwt_result_syntax in
  let* () = check_delegate_registered ctxt pkh in
  let*! contracts = delegators ctxt pkh in
  return contracts

let f_total_currently_staked ctxt =
  let open Lwt_result_syntax in
  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  Stake_storage.fold_on_active_delegates_with_minimal_stake_es
    ctxt
    ~order:`Undefined
    ~init:(Tez_repr.zero, Tez_repr.zero, Tez_repr.zero)
    ~f:(fun delegate (own, delegated, overstaked) ->
      let* full_staking_balance =
        Stake_storage.get_full_staking_balance ctxt delegate
      in
      let delegate_own =
        Full_staking_balance_repr.own_frozen full_staking_balance
      in
      let delegate_delegated =
        Full_staking_balance_repr.staked_frozen full_staking_balance
      in
      let* limits = Delegate_staking_parameters.of_delegate ctxt delegate in
      let allowed =
        Full_staking_balance_repr.allowed_staked_frozen
          ~adaptive_issuance_global_limit_of_staking_over_baking:5
          ~delegate_limit_of_staking_over_baking_millionth:
            limits.limit_of_staking_over_baking_millionth
          full_staking_balance
      in
      let*? own = Tez_repr.(own +? delegate_own) in
      let*? delegated =
        Tez_repr.(delegated +? min delegate_delegated allowed)
      in
      let*? delegate_overstaked =
        Tez_repr.(max delegate_delegated allowed -? allowed)
      in
      let*? overstaked = Tez_repr.(overstaked +? delegate_overstaked) in
      return (own, delegated, overstaked))

let info ctxt pkh =
  let open Lwt_result_syntax in
  (* General baking information *)
  let* deactivated = Delegate.deactivated ctxt pkh in
  let is_forbidden = Delegate.is_forbidden_delegate ctxt pkh in
  let* participation = Delegate.For_RPC.participation_info ctxt pkh in
  let* dal_participation =
    let dal_incentives_enable =
      Constants.(parametric ctxt).dal.incentives_enable
    in
    if dal_incentives_enable then
      let* dal_participation =
        Delegate.For_RPC.dal_participation_info ctxt pkh
      in
      return_some dal_participation
    else return_none
  in
  let* grace_period = Delegate.last_cycle_before_deactivation ctxt pkh in
  let* active_staking_parameters =
    Delegate.Staking_parameters.of_delegate ctxt pkh
  in
  let* pending_staking_parameters =
    Delegate.Staking_parameters.pending_updates ctxt pkh
  in
  (* Baking rights *)
  let* baking_power =
    Stake_distribution.For_RPC.delegate_current_baking_power ctxt pkh
  in
  let* total_staked = total_staked ctxt pkh in
  let* total_delegated = total_delegated ctxt pkh in
  let* min_delegated_in_current_cycle =
    Delegate.For_RPC.min_delegated_in_current_cycle ctxt pkh
  in
  let* own_full_balance = Delegate.For_RPC.full_balance ctxt pkh in
  let* own_staked = own_staked ctxt pkh in
  let* own_delegated = own_delegated ctxt pkh in
  let* external_staked = external_staked ctxt pkh in
  let* external_delegated = external_delegated ctxt pkh in
  let* total_unstaked_per_cycle = total_unstaked_per_cycle ctxt pkh in
  let* denunciations = Delegate.For_RPC.pending_denunciations ctxt pkh in
  let* estimated_shared_pending_slashed_amount =
    Delegate.For_RPC.get_estimated_shared_pending_slashed_amount ctxt pkh
  in
  let* staking_denominator =
    Staking_pseudotokens.For_RPC.get_frozen_deposits_pseudotokens
      ctxt
      ~delegate:pkh
  in
  (* Voting *)
  let* current_voting_power = Vote.get_current_voting_power_free ctxt pkh in
  let* voting_power = Vote.get_voting_power_free ctxt pkh in
  let* voting_info = Vote.get_delegate_info ctxt pkh in
  (* Consensus key *)
  let* consensus_key = consensus_key ctxt pkh in
  (* Chunked RPCs *)
  let* stakers = stakers ctxt pkh in
  let*! delegators = delegators ctxt pkh in
  return
    {
      (* General baking information *)
      deactivated;
      is_forbidden;
      participation;
      dal_participation;
      grace_period;
      active_staking_parameters;
      pending_staking_parameters;
      (* Baking rights *)
      baking_power;
      total_staked;
      total_delegated;
      min_delegated_in_current_cycle;
      own_full_balance;
      own_staked;
      own_delegated;
      external_staked;
      external_delegated;
      total_unstaked_per_cycle;
      denunciations;
      estimated_shared_pending_slashed_amount;
      staking_denominator;
      (* Voting *)
      current_voting_power;
      voting_power;
      voting_info;
      (* Consensus key *)
      consensus_key;
      (* Chunked RPCs *)
      stakers;
      delegators;
    }

let wrap_check_registered ~chunked s f =
  register1 ~chunked s (fun ctxt pkh () () ->
      let open Lwt_result_syntax in
      let* () = check_delegate_registered ctxt pkh in
      f ctxt pkh)

module Implem = struct
  let check_delegate_registered = check_delegate_registered

  let total_delegated = total_delegated

  let own_delegated = own_delegated

  let delegators = delegators
end

let register () =
  let open Lwt_result_syntax in
  register0 ~chunked:true S.list_delegate (fun ctxt q () ->
      let*! delegates = Delegate.list ctxt in
      let* delegates =
        match q with
        | {active = true; inactive = false; _} ->
            List.filter_es
              (fun pkh ->
                let+ deactivated = Delegate.deactivated ctxt pkh in
                not deactivated)
              delegates
        | {active = false; inactive = true; _} ->
            List.filter_es (fun pkh -> Delegate.deactivated ctxt pkh) delegates
        | {active = false; inactive = false; _}
        (* This case is counter-intuitive, but it represents the default behavior, when no arguments are given *)
        | {active = true; inactive = true; _} ->
            return delegates
      in
      let minimal_stake = Constants.minimal_stake ctxt in
      match q with
      | {with_minimal_stake = true; without_minimal_stake = false; _} ->
          List.filter_es
            (fun pkh ->
              let+ staking_balance =
                Delegate.For_RPC.staking_balance ctxt pkh
              in
              Tez.(staking_balance >= minimal_stake))
            delegates
      | {with_minimal_stake = false; without_minimal_stake = true; _} ->
          List.filter_es
            (fun pkh ->
              let+ staking_balance =
                Delegate.For_RPC.staking_balance ctxt pkh
              in
              Tez.(staking_balance < minimal_stake))
            delegates
      | {with_minimal_stake = true; without_minimal_stake = true; _}
      | {with_minimal_stake = false; without_minimal_stake = false; _} ->
          return delegates) ;
  register1 ~chunked:false S.is_forbidden (fun ctxt pkh () () ->
      return @@ Delegate.is_forbidden_delegate ctxt pkh) ;
  wrap_check_registered ~chunked:true S.stakers stakers ;
  register1 ~chunked:false S.Deprecated.full_balance f_own_full_balance ;
  register1 ~chunked:false S.own_full_balance f_own_full_balance ;
  register1 ~chunked:false S.Deprecated.current_frozen_deposits f_total_staked ;
  register1 ~chunked:false S.total_staked f_total_staked ;
  register1 ~chunked:false S.Deprecated.frozen_deposits (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.initial_frozen_deposits ctxt pkh) ;
  wrap_check_registered
    ~chunked:false
    S.Deprecated.unstaked_frozen_deposits
    total_unstaked_per_cycle ;
  wrap_check_registered
    ~chunked:false
    S.total_unstaked_per_cycle
    total_unstaked_per_cycle ;
  register1 ~chunked:false S.total_delegated f_total_delegated ;
  register1 ~chunked:false S.Deprecated.staking_balance (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.staking_balance ctxt pkh) ;
  register1
    ~chunked:false
    S.Deprecated.frozen_deposits_limit
    (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.frozen_deposits_limit ctxt pkh) ;
  register1 ~chunked:true S.Deprecated.delegated_contracts f_delegators ;
  register1 ~chunked:true S.delegators f_delegators ;
  register1 ~chunked:false S.Deprecated.total_delegated_stake f_external_staked ;
  wrap_check_registered ~chunked:false S.own_staked own_staked ;
  wrap_check_registered ~chunked:false S.own_delegated own_delegated ;
  register1 ~chunked:false S.external_staked f_external_staked ;
  register1 ~chunked:false S.external_delegated f_external_delegated ;
  register1 ~chunked:false S.Deprecated.delegated_balance (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      external_staked_and_delegated ctxt pkh) ;
  register1 ~chunked:false S.staking_denominator (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Staking_pseudotokens.For_RPC.get_frozen_deposits_pseudotokens
        ctxt
        ~delegate:pkh) ;
  register1 ~chunked:false S.deactivated (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.deactivated ctxt pkh) ;
  register1 ~chunked:false S.grace_period (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.last_cycle_before_deactivation ctxt pkh) ;
  register1 ~chunked:false S.current_voting_power (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Vote.get_current_voting_power_free ctxt pkh) ;
  register1 ~chunked:false S.voting_power (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Vote.get_voting_power_free ctxt pkh) ;
  register1 ~chunked:false S.Deprecated.current_baking_power f_baking_power ;
  register1 ~chunked:false S.baking_power f_baking_power ;
  register1 ~chunked:false S.voting_info (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Vote.get_delegate_info ctxt pkh) ;
  register1 ~chunked:false S.consensus_key (fun ctxt pkh () () ->
      consensus_key ctxt pkh) ;
  register1 ~chunked:false S.participation (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.participation_info ctxt pkh) ;
  register1 ~chunked:false S.dal_participation (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.dal_participation_info ctxt pkh) ;
  register1 ~chunked:false S.active_staking_parameters (fun ctxt pkh () () ->
      Delegate.Staking_parameters.of_delegate ctxt pkh) ;
  register1 ~chunked:false S.pending_staking_parameters (fun ctxt pkh () () ->
      Delegate.Staking_parameters.pending_updates ctxt pkh) ;
  register1 ~chunked:false S.pending_denunciations (fun ctxt pkh () () ->
      Delegate.For_RPC.pending_denunciations ctxt pkh) ;
  register1
    ~chunked:false
    S.estimated_shared_pending_slashed_amount
    (fun ctxt delegate () () ->
      let* () = check_delegate_registered ctxt delegate in
      Delegate.For_RPC.get_estimated_shared_pending_slashed_amount ctxt delegate) ;
  register1
    ~chunked:false
    S.min_delegated_in_current_cycle
    (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.min_delegated_in_current_cycle ctxt pkh) ;
  wrap_check_registered ~chunked:true S.info info ;
  register0 ~chunked:false S.total_currently_staked (fun ctxt () () ->
      f_total_currently_staked ctxt)

let list ctxt block ?(active = true) ?(inactive = false)
    ?(with_minimal_stake = true) ?(without_minimal_stake = false) () =
  RPC_context.make_call0
    S.list_delegate
    ctxt
    block
    {active; inactive; with_minimal_stake; without_minimal_stake}
    ()

let is_forbidden ctxt block pkh =
  RPC_context.make_call1 S.is_forbidden ctxt block pkh () ()

let full_balance ctxt block pkh =
  RPC_context.make_call1 S.own_full_balance ctxt block pkh () ()

let current_frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.total_staked ctxt block pkh () ()

let frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.Deprecated.frozen_deposits ctxt block pkh () ()

let unstaked_frozen_deposits ctxt block pkh =
  RPC_context.make_call1
    S.Deprecated.unstaked_frozen_deposits
    ctxt
    block
    pkh
    ()
    ()

let staking_balance ctxt block pkh =
  RPC_context.make_call1 S.Deprecated.staking_balance ctxt block pkh () ()

let frozen_deposits_limit ctxt block pkh =
  RPC_context.make_call1 S.Deprecated.frozen_deposits_limit ctxt block pkh () ()

let delegated_contracts ctxt block pkh =
  RPC_context.make_call1 S.delegators ctxt block pkh () ()

let total_delegated_stake ctxt block pkh =
  RPC_context.make_call1 S.external_staked ctxt block pkh () ()

let staking_denominator ctxt block pkh =
  RPC_context.make_call1 S.staking_denominator ctxt block pkh () ()

let deactivated ctxt block pkh =
  RPC_context.make_call1 S.deactivated ctxt block pkh () ()

let grace_period ctxt block pkh =
  RPC_context.make_call1 S.grace_period ctxt block pkh () ()

let voting_power ctxt block pkh =
  RPC_context.make_call1 S.voting_power ctxt block pkh () ()

let current_voting_power ctxt block pkh =
  RPC_context.make_call1 S.current_voting_power ctxt block pkh () ()

let current_baking_power ctxt block pkh =
  RPC_context.make_call1 S.baking_power ctxt block pkh () ()

let voting_info ctxt block pkh =
  RPC_context.make_call1 S.voting_info ctxt block pkh () ()

let consensus_key ctxt block pkh =
  RPC_context.make_call1 S.consensus_key ctxt block pkh () ()

let participation ctxt block pkh =
  RPC_context.make_call1 S.participation ctxt block pkh () ()

let dal_participation ctxt block pkh =
  RPC_context.make_call1 S.dal_participation ctxt block pkh () ()

let active_staking_parameters ctxt block pkh =
  RPC_context.make_call1 S.active_staking_parameters ctxt block pkh () ()

let pending_staking_parameters ctxt block pkh =
  RPC_context.make_call1 S.pending_staking_parameters ctxt block pkh () ()

let pending_denunciations ctxt block pkh =
  RPC_context.make_call1 S.pending_denunciations ctxt block pkh () ()

let estimated_shared_pending_slashed_amount ctxt block pkh =
  RPC_context.make_call1
    S.estimated_shared_pending_slashed_amount
    ctxt
    block
    pkh
    ()
    ()

let info ctxt block pkh =
  let open Lwt_result_syntax in
  let* {
         (* General baking information *)
         deactivated;
         is_forbidden = _;
         participation = _;
         dal_participation = _;
         grace_period;
         active_staking_parameters = _;
         pending_staking_parameters = _;
         (* Baking rights *)
         baking_power = _;
         total_staked;
         total_delegated;
         min_delegated_in_current_cycle;
         own_full_balance;
         own_staked = _;
         own_delegated = _;
         external_staked;
         external_delegated;
         total_unstaked_per_cycle = _;
         denunciations;
         estimated_shared_pending_slashed_amount = _;
         staking_denominator;
         (* Voting *)
         current_voting_power = _;
         voting_power = _;
         voting_info;
         (* Consensus key *)
         consensus_key;
         (* Chunked RPCs *)
         stakers = _;
         delegators;
       } =
    RPC_context.make_call1 S.info ctxt block pkh () ()
  in
  let* frozen_deposits = frozen_deposits ctxt block pkh in
  let*? staking_balance =
    Environment.wrap_tzresult Tez.(total_staked +? total_delegated)
  in
  let* frozen_deposits_limit = frozen_deposits_limit ctxt block pkh in
  let*? delegated_balance =
    Environment.wrap_tzresult Tez.(external_staked +? external_delegated)
  in
  return
    {
      full_balance = own_full_balance;
      current_frozen_deposits = total_staked;
      frozen_deposits;
      staking_balance;
      frozen_deposits_limit;
      delegated_contracts = delegators;
      delegated_balance;
      min_delegated_in_current_cycle;
      total_delegated_stake = external_staked;
      staking_denominator;
      deactivated;
      grace_period;
      pending_denunciations = not (List.is_empty denunciations);
      voting_info;
      active_consensus_key = consensus_key.active.consensus_key_pkh;
      pending_consensus_keys =
        List.map
          (fun (cyc, ck) -> (cyc, ck.consensus_key_pkh))
          consensus_key.pendings;
    }
