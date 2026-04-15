(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* CLST stake allocation — cycle-end computation of per-delegate allocations.

   At each cycle boundary, allocations are recomputed from scratch:

   1. Clear all previous allocations (read stez_frozen from staking balances).
   2. Compute each registered delegate's target allocation via [compute_cap].
   3. Greedily allocate from the deposits in public-key-hash order.

   Allocations are pure accounting: no tez is physically moved from
   [CLST_deposits]. The per-delegate [stez_frozen] field in the staking balance
   is used by the baking-rights computation to include CLST stake, but the tez
   remains in the deposits so that stakers can always redeem their tokens.

   Slashing of CLST allocations is deferred to a later MR. *)

(* --- Cap computation --- *)

let allocated_rights_of_delegate ctxt delegate =
  let open Lwt_result_syntax in
  let* staking_balance = Stake_storage.get_full_staking_balance ctxt delegate in
  return (Full_staking_balance_repr.stez_frozen staking_balance)

(* [compute_cap] determines the maximum tez that CLST can allocate to a
   delegate.  The cap is the minimum of two independent bounds:

   1. **Ratio bound**: own_frozen * global_limit * ratio / 1_000_000_000
      where [ratio] is [ratio_of_clst_staking_over_direct_staking_billionth]
      and [global_limit] is [global_limit_of_staking_over_baking].

   2. **Global-limit bound**: global_limit * own_frozen - allowed_staked_frozen
      where [global_limit] is [global_limit_of_staking_over_baking] (the
      protocol-wide cap on total external stake per unit of own stake) and
      [allowed_staked_frozen] is the delegate's existing direct-staker stake.

   Direct staking takes priority: if a delegate is already overstaked
   (allowed_staked_frozen >= global_limit * own_frozen), the global-limit bound is
   zero and no CLST is allocated regardless of the ratio.

   NOTE: this is behind a feature flag and not production-ready.  The greedy
   allocation order (pkh) is intentionally simple and may change later. *)
let compute_cap ~own_frozen ~allowed_staked_frozen
    ~global_limit_of_staking_over_baking
    ~ratio_of_clst_staking_over_direct_staking_billionth =
  let open Result_syntax in
  let* max_external =
    Tez_repr.(own_frozen *? Int64.of_int global_limit_of_staking_over_baking)
  in
  (* Ratio bound: own_frozen * global_limit * ratio / 1_000_000_000 *)
  let* ratio_cap =
    Tez_repr.mul_ratio_z
      ~rounding:`Down
      max_external
      ~num:(Z.of_int32 ratio_of_clst_staking_over_direct_staking_billionth)
      ~den:(Z.of_int64 1_000_000_000L)
  in
  (* Global-limit bound: own_frozen * global_limit - allowed_staked_frozen *)
  let* global_cap =
    if Tez_repr.(allowed_staked_frozen >= max_external) then
      (* delegate is overstaked *)
      return Tez_repr.zero
    else Tez_repr.(max_external -? allowed_staked_frozen)
  in
  return (Tez_repr.min ratio_cap global_cap)

(* [compute_target ctxt delegate params] returns the target allocation for a
   registered delegate based on the global staking limit, existing direct
   stakers, and the delegate's CLST registration parameters. *)
let compute_target ctxt delegate params =
  let open Lwt_result_syntax in
  let* staking_balance = Stake_storage.get_full_staking_balance ctxt delegate in
  let own_frozen = Full_staking_balance_repr.own_frozen staking_balance in
  let global_limit_of_staking_over_baking =
    Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking ctxt
  in
  let* staking_parameters =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  let allowed_staked_frozen =
    Full_staking_balance_repr.allowed_staked_frozen
      ~adaptive_issuance_global_limit_of_staking_over_baking:
        global_limit_of_staking_over_baking
      ~delegate_limit_of_staking_over_baking_millionth:
        staking_parameters
          .Staking_parameters_repr.limit_of_staking_over_baking_millionth
      staking_balance
  in
  let ratio_of_clst_staking_over_direct_staking_billionth =
    params
      .Clst_delegates_parameters_repr
       .ratio_of_clst_staking_over_direct_staking_billionth
  in
  Lwt.return
    (compute_cap
       ~own_frozen
       ~allowed_staked_frozen
       ~global_limit_of_staking_over_baking
       ~ratio_of_clst_staking_over_direct_staking_billionth)

(* --- Rebalancing --- *)

(* [rebalance_at_cycle_end ctxt ~new_cycle] recomputes CLST allocations for all
   registered delegates.  Called at cycle end.

   Allocations are pure accounting: the per-delegate [stez_frozen] field in the
   staking balance records how much of the deposits is attributed to each
   delegate for baking rights, but no tez is transferred.

   NOTE: the allocation algorithm (greedy in pkh order) is not definitive and
   may be replaced by a fairer or more balanced strategy in the future. *)
let rebalance_at_cycle_end ctxt ~new_cycle:_ =
  let open Lwt_result_syntax in
  (* Step 1: deallocate everything *)
  let* ctxt =
    Delegate_storage.fold
      ctxt
      ~order:`Undefined
      ~init:(Ok ctxt)
      ~f:(fun delegate ctxt ->
        let*? ctxt = ctxt in
        let* old_amount = allocated_rights_of_delegate ctxt delegate in
        if Tez_repr.(old_amount = zero) then return ctxt
        else Stake_storage.clear_stez_frozen_stake ctxt delegate)
  in
  (* Step 2: compute targets and greedily allocate from deposits (pkh order) *)
  let* available = Clst_storage.get_deposits_balance ctxt in
  let* ctxt, _remaining =
    Storage.Clst.Registered_delegates.fold
      ctxt
      ~order:`Sorted
      ~init:(Ok (ctxt, available))
      ~f:(fun contract params acc ->
        let*? ctxt, remaining = acc in
        match contract with
        | Contract_repr.Implicit delegate ->
            let* target = compute_target ctxt delegate params in
            let allocation = Tez_repr.min target remaining in
            if Tez_repr.(allocation = zero) then return (ctxt, remaining)
            else
              let* ctxt =
                Stake_storage.set_stez_frozen_stake ctxt delegate allocation
              in
              let*? remaining = Tez_repr.(remaining -? allocation) in
              return (ctxt, remaining)
        | Contract_repr.Originated _ -> return (ctxt, remaining))
  in
  return ctxt

module For_RPC = struct
  let allocated_rights_of_delegate = allocated_rights_of_delegate

  let total_allocated_rights ctxt =
    let open Lwt_result_syntax in
    Storage.Clst.Registered_delegates.fold
      ctxt
      ~order:`Undefined
      ~init:(Ok Tez_repr.zero)
      ~f:(fun contract _params acc ->
        let*? total = acc in
        match contract with
        | Contract_repr.Implicit delegate ->
            let* amount = allocated_rights_of_delegate ctxt delegate in
            Lwt.return Tez_repr.(total +? amount)
        | Contract_repr.Originated _ -> return total)
end
