(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Alpha_context

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

type info = {
  full_balance : Tez.t;
  current_frozen_deposits : Tez.t;
  frozen_deposits : Tez.t;
  staking_balance : Tez.t;
  frozen_deposits_limit : Tez.t option;
  delegated_contracts : Contract.t list;
  delegated_balance : Tez.t;
  total_delegated_stake : Tez.t;
  staking_denominator : Staking_pseudotoken.t;
  deactivated : bool;
  grace_period : Cycle.t;
  voting_info : Vote.delegate_info;
  active_consensus_key : Signature.Public_key_hash.t;
  pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
}

let info_encoding =
  let open Data_encoding in
  conv
    (fun {
           full_balance;
           current_frozen_deposits;
           frozen_deposits;
           staking_balance;
           frozen_deposits_limit;
           delegated_contracts;
           delegated_balance;
           total_delegated_stake;
           staking_denominator;
           deactivated;
           grace_period;
           voting_info;
           active_consensus_key;
           pending_consensus_keys;
         } ->
      ( ( full_balance,
          current_frozen_deposits,
          frozen_deposits,
          staking_balance,
          frozen_deposits_limit,
          delegated_contracts,
          delegated_balance,
          deactivated,
          grace_period ),
        ( (total_delegated_stake, staking_denominator),
          (voting_info, (active_consensus_key, pending_consensus_keys)) ) ))
    (fun ( ( full_balance,
             current_frozen_deposits,
             frozen_deposits,
             staking_balance,
             frozen_deposits_limit,
             delegated_contracts,
             delegated_balance,
             deactivated,
             grace_period ),
           ( (total_delegated_stake, staking_denominator),
             (voting_info, (active_consensus_key, pending_consensus_keys)) ) ) ->
      {
        full_balance;
        current_frozen_deposits;
        frozen_deposits;
        staking_balance;
        frozen_deposits_limit;
        delegated_contracts;
        delegated_balance;
        total_delegated_stake;
        staking_denominator;
        deactivated;
        grace_period;
        voting_info;
        active_consensus_key;
        pending_consensus_keys;
      })
    (merge_objs
       (obj9
          (req "full_balance" Tez.encoding)
          (req "current_frozen_deposits" Tez.encoding)
          (req "frozen_deposits" Tez.encoding)
          (req "staking_balance" Tez.encoding)
          (opt "frozen_deposits_limit" Tez.encoding)
          (req "delegated_contracts" (list Contract.encoding))
          (req "delegated_balance" Tez.encoding)
          (req "deactivated" bool)
          (req "grace_period" Cycle.encoding))
       (merge_objs
          (obj2
             (req "total_delegated_stake" Tez.encoding)
             (req "staking_denominator" Staking_pseudotoken.For_RPC.encoding))
          (merge_objs
             Vote.delegate_info_encoding
             (obj2
                (req "active_consensus_key" Signature.Public_key_hash.encoding)
                (dft
                   "pending_consensus_keys"
                   (list
                      (obj2
                         (req "cycle" Cycle.encoding)
                         (req "pkh" Signature.Public_key_hash.encoding)))
                   [])))))

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
           } ->
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
           expected_attesting_rewards ) ->
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

type deposit_per_cycle = {cycle : Cycle.t; deposit : Tez.t}

let deposit_per_cycle_encoding : deposit_per_cycle Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {cycle; deposit} -> (cycle, deposit))
    (fun (cycle, deposit) -> {cycle; deposit})
    (obj2 (req "cycle" Cycle.encoding) (req "deposit" Tez.encoding))

type pending_staking_parameters = Cycle.t * Staking_parameters_repr.t

let pending_staking_parameters_encoding :
    pending_staking_parameters Data_encoding.t =
  let open Data_encoding in
  obj2
    (req "cycle" Cycle.encoding)
    (req "parameters" Staking_parameters_repr.encoding)

module S = struct
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

  let path = RPC_path.(raw_path /: Signature.Public_key_hash.rpc_arg)

  let info =
    RPC_service.get_service
      ~description:"Everything about a delegate."
      ~query:RPC_query.empty
      ~output:info_encoding
      path

  let full_balance =
    RPC_service.get_service
      ~description:
        "Returns the full balance (in mutez) of a given delegate, including \
         the frozen deposits and the frozen bonds. It does not include its \
         delegated balance."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "full_balance")

  let current_frozen_deposits =
    RPC_service.get_service
      ~description:
        "Returns the current amount of the frozen deposits (in mutez). That is \
         the frozen deposits at beginning of cycle plus rewards minus unstaked \
         and slashing. It doesn't count unstaked frozen deposits."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "current_frozen_deposits")

  let frozen_deposits =
    RPC_service.get_service
      ~description:
        "Returns the amount of the frozen deposits (in mutez) at the beginning \
         of the current cycle. It doesn't count frozen deposits unstaked \
         before the current cycle."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "frozen_deposits")

  let unstaked_frozen_deposits =
    RPC_service.get_service
      ~description:
        "Returns, for each cycle, the sum of unstaked-but-frozen deposits for \
         this cycle. Cycles go from the last unslashable cycle to the current \
         cycle."
      ~query:RPC_query.empty
      ~output:(Data_encoding.list deposit_per_cycle_encoding)
      RPC_path.(path / "unstaked_frozen_deposits")

  let staking_balance =
    RPC_service.get_service
      ~description:
        "Returns the total amount of tokens (in mutez) delegated to a given \
         delegate. This includes the balances of all the contracts that \
         delegate to it, but also the balance of the delegate itself, its \
         frozen deposits, and its frozen bonds."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "staking_balance")

  let frozen_deposits_limit =
    RPC_service.get_service
      ~description:
        "Returns the frozen deposits limit for the given delegate or none if \
         no limit is set."
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Tez.encoding)
      RPC_path.(path / "frozen_deposits_limit")

  let delegated_contracts =
    RPC_service.get_service
      ~description:
        "Returns the list of contracts that delegate to a given delegate."
      ~query:RPC_query.empty
      ~output:(list Contract.encoding)
      RPC_path.(path / "delegated_contracts")

  let total_delegated_stake =
    RPC_service.get_service
      ~description:
        "Returns the sum (in mutez) of all tokens staked by the delegators of \
         a given delegate. This excludes the delegate's own staked tokens."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "total_delegated_stake")

  let staking_denominator =
    RPC_service.get_service
      ~description:
        "Returns an abstract representation of the total delegated stake."
      ~query:RPC_query.empty
      ~output:Staking_pseudotoken.For_RPC.encoding
      RPC_path.(path / "staking_denominator")

  let delegated_balance =
    RPC_service.get_service
      ~description:
        "Returns the sum (in mutez) of all balances of all the contracts that \
         delegate to a given delegate. This excludes the delegate's own \
         balance, its frozen deposits and its frozen bonds."
      ~query:RPC_query.empty
      ~output:Tez.encoding
      RPC_path.(path / "delegated_balance")

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

  let current_baking_power =
    RPC_service.get_service
      ~description:
        "The baking power of a delegate, as computed from its current stake. \
         This value is not used for computing baking rights but only reflects \
         the baking power that the delegate would have if a snapshot was taken \
         at the current block."
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "current_baking_power")

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
end

let check_delegate_registered ctxt pkh =
  let open Lwt_result_syntax in
  let*! is_registered = Delegate.registered ctxt pkh in
  match is_registered with
  | true -> return_unit
  | false -> tzfail (Not_registered pkh)

let register () =
  let open Services_registration in
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
  register1 ~chunked:false S.info (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      let* full_balance = Delegate.For_RPC.full_balance ctxt pkh in
      let* current_frozen_deposits =
        Delegate.current_frozen_deposits ctxt pkh
      in
      let* frozen_deposits = Delegate.initial_frozen_deposits ctxt pkh in
      let* staking_balance = Delegate.For_RPC.staking_balance ctxt pkh in
      let* frozen_deposits_limit = Delegate.frozen_deposits_limit ctxt pkh in
      let*! delegated_contracts = Delegate.delegated_contracts ctxt pkh in
      let* delegated_balance = Delegate.For_RPC.delegated_balance ctxt pkh in
      let* total_delegated_stake =
        Staking_pseudotokens.For_RPC.get_frozen_deposits_staked_tez
          ctxt
          ~delegate:pkh
      in
      let* staking_denominator =
        Staking_pseudotokens.For_RPC.get_frozen_deposits_pseudotokens
          ctxt
          ~delegate:pkh
      in
      let* deactivated = Delegate.deactivated ctxt pkh in
      let* grace_period = Delegate.last_cycle_before_deactivation ctxt pkh in
      let* voting_info = Vote.get_delegate_info ctxt pkh in
      let* consensus_key = Delegate.Consensus_key.active_pubkey ctxt pkh in
      let+ pendings = Delegate.Consensus_key.pending_updates ctxt pkh in
      let pending_consensus_keys =
        List.map (fun (cycle, pkh, _) -> (cycle, pkh)) pendings
      in
      {
        full_balance;
        current_frozen_deposits;
        frozen_deposits;
        staking_balance;
        frozen_deposits_limit;
        delegated_contracts;
        delegated_balance;
        total_delegated_stake;
        staking_denominator;
        deactivated;
        grace_period;
        voting_info;
        active_consensus_key = consensus_key.consensus_pkh;
        pending_consensus_keys;
      }) ;
  register1 ~chunked:false S.full_balance (fun ctxt pkh () () ->
      let* () =
        trace
          (Balance_rpc_non_delegate pkh)
          (check_delegate_registered ctxt pkh)
      in
      Delegate.For_RPC.full_balance ctxt pkh) ;
  register1 ~chunked:false S.current_frozen_deposits (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.current_frozen_deposits ctxt pkh) ;
  register1 ~chunked:false S.frozen_deposits (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.initial_frozen_deposits ctxt pkh) ;
  register1 ~chunked:false S.unstaked_frozen_deposits (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      let ctxt_cycle = (Alpha_context.Level.current ctxt).cycle in
      let csts = (Constants.all ctxt).parametric in
      let last_unslashable_cycle =
        Option.value ~default:Cycle.root
        @@ Cycle.sub
             ctxt_cycle
             (csts.preserved_cycles + Constants_repr.max_slashing_period)
      in
      let cycles = Cycle.(last_unslashable_cycle ---> ctxt_cycle) in
      let* requests =
        List.map_es
          (fun cycle ->
            let* deposit = Unstaked_frozen_deposits.balance ctxt pkh cycle in
            return (cycle, deposit))
          cycles
      in
      let* slashed_requests =
        Alpha_context.Unstake_requests.For_RPC
        .apply_slash_to_unstaked_unfinalizable
          ctxt
          ~delegate:pkh
          ~requests
      in
      List.map_es
        (fun (cycle, deposit) -> return {cycle; deposit})
        slashed_requests) ;
  register1 ~chunked:false S.staking_balance (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.staking_balance ctxt pkh) ;
  register1 ~chunked:false S.frozen_deposits_limit (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.frozen_deposits_limit ctxt pkh) ;
  register1 ~chunked:true S.delegated_contracts (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      let*! contracts = Delegate.delegated_contracts ctxt pkh in
      return contracts) ;
  register1 ~chunked:false S.delegated_balance (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.delegated_balance ctxt pkh) ;
  register1 ~chunked:false S.total_delegated_stake (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Staking_pseudotokens.For_RPC.get_frozen_deposits_staked_tez
        ctxt
        ~delegate:pkh) ;
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
  register1 ~chunked:false S.current_baking_power (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Stake_distribution.For_RPC.delegate_current_baking_power ctxt pkh) ;
  register1 ~chunked:false S.voting_info (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Vote.get_delegate_info ctxt pkh) ;
  register1 ~chunked:false S.consensus_key (fun ctxt pkh () () ->
      let* {
             consensus_pk = consensus_key_pk;
             consensus_pkh = consensus_key_pkh;
             _;
           } =
        Delegate.Consensus_key.active_pubkey ctxt pkh
      in
      let* pendings = Delegate.Consensus_key.pending_updates ctxt pkh in
      let pendings =
        List.map
          (fun (cycle, consensus_key_pkh, consensus_key_pk) ->
            (cycle, {consensus_key_pk; consensus_key_pkh}))
          pendings
      in
      return {active = {consensus_key_pk; consensus_key_pkh}; pendings}) ;
  register1 ~chunked:false S.participation (fun ctxt pkh () () ->
      let* () = check_delegate_registered ctxt pkh in
      Delegate.For_RPC.participation_info ctxt pkh) ;
  register1 ~chunked:false S.active_staking_parameters (fun ctxt pkh () () ->
      Delegate.Staking_parameters.of_delegate ctxt pkh) ;
  register1 ~chunked:false S.pending_staking_parameters (fun ctxt pkh () () ->
      Delegate.Staking_parameters.pending_updates ctxt pkh)

let list ctxt block ?(active = true) ?(inactive = false)
    ?(with_minimal_stake = true) ?(without_minimal_stake = false) () =
  RPC_context.make_call0
    S.list_delegate
    ctxt
    block
    {active; inactive; with_minimal_stake; without_minimal_stake}
    ()

let info ctxt block pkh = RPC_context.make_call1 S.info ctxt block pkh () ()

let full_balance ctxt block pkh =
  RPC_context.make_call1 S.full_balance ctxt block pkh () ()

let current_frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.current_frozen_deposits ctxt block pkh () ()

let frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.frozen_deposits ctxt block pkh () ()

let unstaked_frozen_deposits ctxt block pkh =
  RPC_context.make_call1 S.unstaked_frozen_deposits ctxt block pkh () ()

let staking_balance ctxt block pkh =
  RPC_context.make_call1 S.staking_balance ctxt block pkh () ()

let frozen_deposits_limit ctxt block pkh =
  RPC_context.make_call1 S.frozen_deposits_limit ctxt block pkh () ()

let delegated_contracts ctxt block pkh =
  RPC_context.make_call1 S.delegated_contracts ctxt block pkh () ()

let delegated_balance ctxt block pkh =
  RPC_context.make_call1 S.delegated_balance ctxt block pkh () ()

let total_delegated_stake ctxt block pkh =
  RPC_context.make_call1 S.total_delegated_stake ctxt block pkh () ()

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
  RPC_context.make_call1 S.current_baking_power ctxt block pkh () ()

let voting_info ctxt block pkh =
  RPC_context.make_call1 S.voting_info ctxt block pkh () ()

let consensus_key ctxt block pkh =
  RPC_context.make_call1 S.consensus_key ctxt block pkh () ()

let participation ctxt block pkh =
  RPC_context.make_call1 S.participation ctxt block pkh () ()

let active_staking_parameters ctxt block pkh =
  RPC_context.make_call1 S.active_staking_parameters ctxt block pkh () ()

let pending_staking_parameters ctxt block pkh =
  RPC_context.make_call1 S.pending_staking_parameters ctxt block pkh () ()
