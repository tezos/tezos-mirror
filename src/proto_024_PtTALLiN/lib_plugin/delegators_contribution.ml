(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Protocol

(* Cf {!Services_registration.rpc_init}. *)
let init_ctxt
    ( (block_header : Block_header.shell_header),
      (context : Environment.Context.t) ) =
  let open Lwt_result_syntax in
  let timestamp = block_header.timestamp in
  let level = block_header.level in
  let* ctxt, _, _ =
    Alpha_context.prepare
      ~level
      ~predecessor_timestamp:timestamp
      ~timestamp
      context
  in
  return ctxt

(* Checks whether [ctxt]'s current level is the level at which the
   baking rights for [cycle] have been sampled, and if not, returns
   information needed to find it.

   - Returns [`Already_at_sampling_level] if [ctxt]'s current level is
     the sampling level for [cycle].

   - Returns [`Cycle_too_far_in_future] if the sampling level for
     [cycle] is higher than [ctxt]'s current level.

   - Returns [`Retry_at_level retry_level] if the sampling level for
     [cycle] is lower than [ctxt]'s current level. [retry_level] is the
     sampling level for [cycle] if it belongs to the current protocol;
     otherwise it is the highest level of the previous protocol: indeed,
     in that case we cannot determine the actual sampling level here,
     since [blocks_per_cycle] may be different for ealier protocols. In
     both cases, the following holds:

     [actual_sampling_level_for_cycle <= retry_level < current_level]

   (Here, the protocol that a level "belongs" to is the protocol
   associated with its resulting context.)
*)
let find_sampling_level ctxt cycle =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let consensus_rights_delay = Constants.consensus_rights_delay ctxt in
  let blocks_per_cycle = Constants.blocks_per_cycle ctxt in
  let current_level = Level.current ctxt in
  let current_cycle = Cycle.to_int32 current_level.cycle in
  let current_raw = Raw_level.to_int32 current_level.level in
  let sampling_level =
    if Compare.Int32.(cycle <= Int32.of_int consensus_rights_delay) then
      (* This means that rights for [cycle] have been initialized during migration from Genesis *)
      1l
    else
      let open Int32 in
      let sampling_cycle = sub cycle (of_int (consensus_rights_delay + 1)) in
      assert (Compare.Int32.(sampling_cycle >= 0l)) ;
      let last_level_of_current_cycle =
        add
          (sub current_raw current_level.cycle_position)
          (pred blocks_per_cycle)
      in
      add
        last_level_of_current_cycle
        (mul blocks_per_cycle (sub sampling_cycle current_cycle))
  in
  if Compare.Int32.(current_raw = sampling_level) then
    return `Already_at_sampling_level
  else if Compare.Int32.(current_raw < sampling_level) then
    return `Cycle_too_far_in_future
  else
    let* first_level = First_level_of_protocol.get ctxt in
    let first_level = Raw_level.to_int32 first_level in
    if Compare.Int32.(first_level <= sampling_level) then
      return (`Retry_at_level sampling_level)
    else (
      assert (Compare.Int32.(first_level >= 1l)) ;
      return (`Retry_at_level (Int32.pred first_level)))

(* See description in [src/lib_validation/protocol_plugin.mli]. *)
type delegated_breakdown_at_sampling = {
  min_delegated_amount : int64;
  min_delegated_level : int32;
  overstaked : int64;
  total_delegated_including_overdelegated : int64;
  total_delegated_after_limits : int64;
  overdelegated : int64;
}

(* Returns the overstaking- and overdelegation-related breakdown of
   delegated contribution to baking rights as of [ctxt].

   Intended to be called on the [ctxt] that corresponds to the level
   at which the protocol sampled the baking rights for the queried
   cycle.

   Contributions from delegated balances of both the delegate and
   delegators are totaled in [min_delegated_amount]; breaking them
   down further requires accessing the context at
   [min_delegated_level].

   Mostly based on the implementation of
   {!Stake_context.apply_limits}. *)
let delegated_breakdown_from_sampling_level_ctxt ctxt pkh =
  let open Lwt_result_syntax in
  let* () = Delegate_services.Implem.check_delegate_registered ctxt pkh in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in
  let* staking_balance = Stake_storage.get_full_staking_balance raw_ctxt pkh in
  let* staking_parameters =
    Delegate_staking_parameters.of_delegate raw_ctxt pkh
  in
  let current_level = Raw_context.current_level raw_ctxt in
  let cycle_eras = Raw_context.cycle_eras raw_ctxt in
  let own_frozen = Full_staking_balance_repr.own_frozen staking_balance in
  let staked_frozen = Full_staking_balance_repr.staked_frozen staking_balance in
  let allowed_staked_frozen =
    Full_staking_balance_repr.allowed_staked_frozen
      ~adaptive_issuance_global_limit_of_staking_over_baking:
        (Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking
           raw_ctxt)
      ~delegate_limit_of_staking_over_baking_millionth:
        staking_parameters
          .Staking_parameters_repr.limit_of_staking_over_baking_millionth
      staking_balance
  in
  let min_delegated_amount, min_delegated_level =
    Full_staking_balance_repr.Internal_for_tests_and_RPCs
    .min_delegated_and_level
      ~cycle_eras
      ~current_level
      staking_balance
  in
  let limit_of_delegation_over_baking =
    Int64.of_int (Constants_storage.limit_of_delegation_over_baking raw_ctxt)
  in
  let*? overstaked = Tez_repr.(staked_frozen -? allowed_staked_frozen) in
  let*? total_delegated_including_overdelegated =
    Tez_repr.(min_delegated_amount +? overstaked)
  in
  let total_delegated_after_limits =
    match Tez_repr.(own_frozen *? limit_of_delegation_over_baking) with
    | Ok max_allowed_delegated ->
        Tez_repr.min
          max_allowed_delegated
          total_delegated_including_overdelegated
    | Error _max_allowed_delegated_overflows ->
        total_delegated_including_overdelegated
  in
  let total_delegated_including_overdelegated =
    Tez_repr.to_mutez total_delegated_including_overdelegated
  in
  let total_delegated_after_limits =
    Tez_repr.to_mutez total_delegated_after_limits
  in
  let overdelegated =
    Int64.sub
      total_delegated_including_overdelegated
      total_delegated_after_limits
  in
  return
    {
      min_delegated_amount = Tez_repr.to_mutez min_delegated_amount;
      min_delegated_level = Raw_level_repr.to_int32 min_delegated_level.level;
      overstaked = Tez_repr.to_mutez overstaked;
      total_delegated_including_overdelegated;
      total_delegated_after_limits;
      overdelegated;
    }

(* See description in [src/lib_validation/protocol_plugin.mli]. *)
let delegated_breakdown_at_sampling context ~cycle ~delegate_pkh =
  let open Lwt_result_syntax in
  let* ctxt = init_ctxt context in
  let* sampling_level = find_sampling_level ctxt cycle in
  match sampling_level with
  | `Already_at_sampling_level ->
      let* breakdown =
        delegated_breakdown_from_sampling_level_ctxt ctxt delegate_pkh
      in
      return (`Ok breakdown)
  | (`Retry_at_level _ | `Cycle_too_far_in_future) as x -> return x

(* Computes [delegator]'s delegated contribution to its current
   delegate.

   It is equal to [delegator]'s delegated_balance (that is, its full
   balance minus its staked balance) minus the amounts in any unstaked
   requests that [delegator] might still have associated with older
   delegates.

   Precondition: [delegate_pkh] is [delegator]'s current delegate
   (checked by the function). *)
let delegator_contribution ctxt ~delegate_pkh delegator =
  let open Lwt_result_syntax in
  let* current_delegate = Alpha_context.Contract.Delegate.find ctxt delegator in
  assert (
    match current_delegate with
    | None -> false
    | Some current_delegate ->
        Signature.Public_key_hash.(current_delegate = delegate_pkh)) ;
  let* full_balance =
    Alpha_context.Contract.For_RPC.get_full_balance ctxt delegator
  in
  let* staked_balance_opt =
    Alpha_context.Contract.For_RPC.get_staked_balance ctxt delegator
  in
  let staked_balance =
    Option.value staked_balance_opt ~default:Alpha_context.Tez.zero
  in
  let* unstake_requests =
    Contract_services.Implem.unstake_requests ctxt delegator
  in
  let*? unstaked_counting_for_former_delegates =
    let open Result_syntax in
    match unstake_requests with
    | None -> return Alpha_context.Tez.zero
    | Some {finalizable; unfinalizable} ->
        let* finalizable_sum =
          List.fold_left_e
            (fun acc
                 (request_delegate, _cycle, (amount : Alpha_context.Tez.t))
               ->
              if Signature.Public_key_hash.(request_delegate <> delegate_pkh)
              then Alpha_context.Tez.(acc +? amount)
              else return acc)
            Alpha_context.Tez.zero
            finalizable
        in
        let* unfinalizable_sum =
          if Signature.Public_key_hash.(unfinalizable.delegate <> delegate_pkh)
          then
            List.fold_left_e
              (fun acc (_cycle, amount) -> Alpha_context.Tez.(acc +? amount))
              Alpha_context.Tez.zero
              unfinalizable.requests
          else return Alpha_context.Tez.zero
        in
        Alpha_context.Tez.(finalizable_sum +? unfinalizable_sum)
  in
  let*? delegated_balance =
    Alpha_context.Tez.(full_balance -? staked_balance)
  in
  let*? delegated_contribution_to_delegate_pkh =
    Alpha_context.Tez.(
      delegated_balance -? unstaked_counting_for_former_delegates)
  in
  return
    ( Format.asprintf "%a" Alpha_context.Contract.pp delegator,
      Alpha_context.Tez.to_mutez delegated_contribution_to_delegate_pkh )

(* See description in [src/lib_validation/protocol_plugin.mli]. *)
type min_delegated_breakdown = {
  total_delegated : int64;
  own_delegated : int64;
  delegators_contributions : (string * int64) list;
  former_delegators_unstake_requests : int64;
}

(* See description in [src/lib_validation/protocol_plugin.mli]. *)
let min_delegated_breakdown context ~delegate_pkh =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ctxt = init_ctxt context in
  let* total_delegated =
    Delegate_services.Implem.total_delegated ctxt delegate_pkh
  in
  let total_delegated = Tez.to_mutez total_delegated in
  let* own_delegated =
    Delegate_services.Implem.own_delegated ctxt delegate_pkh
  in
  let own_delegated = Tez.to_mutez own_delegated in
  let*! delegators = Delegate_services.Implem.delegators ctxt delegate_pkh in
  let external_delegators =
    List.filter
      (function
        | Contract.Implicit pkh ->
            Signature.Public_key_hash.(pkh <> delegate_pkh)
        | Originated _ -> true)
      delegators
  in
  let* delegators_contributions =
    List.map_es (delegator_contribution ctxt ~delegate_pkh) external_delegators
  in
  let total_external_delegators =
    List.fold_left
      (fun acc (_delegator_str, amount) -> Int64.add acc amount)
      0L
      delegators_contributions
  in
  let former_delegators_unstake_requests =
    Int64.(sub (sub total_delegated own_delegated) total_external_delegators)
  in
  return
    {
      total_delegated;
      own_delegated;
      delegators_contributions;
      former_delegators_unstake_requests;
    }

let wrap_tzresult_lwt x =
  let open Lwt_syntax in
  let* result = x in
  return (Environment.wrap_tzresult result)

let delegated_breakdown_at_sampling context ~cycle ~delegate_pkh =
  delegated_breakdown_at_sampling context ~cycle ~delegate_pkh
  |> wrap_tzresult_lwt

let min_delegated_breakdown context ~delegate_pkh =
  min_delegated_breakdown context ~delegate_pkh |> wrap_tzresult_lwt
