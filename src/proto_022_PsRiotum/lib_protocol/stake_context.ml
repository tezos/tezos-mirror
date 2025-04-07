(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let apply_limits ctxt staking_parameters staking_balance =
  let open Result_syntax in
  let current_level = Raw_context.current_level ctxt in
  let cycle_eras = Raw_context.cycle_eras ctxt in
  let own_frozen = Full_staking_balance_repr.own_frozen staking_balance in
  let staked_frozen = Full_staking_balance_repr.staked_frozen staking_balance in
  let allowed_staked_frozen =
    Full_staking_balance_repr.allowed_staked_frozen
      ~adaptive_issuance_global_limit_of_staking_over_baking:
        (Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking
           ctxt)
      ~delegate_limit_of_staking_over_baking_millionth:
        staking_parameters
          .Staking_parameters_repr.limit_of_staking_over_baking_millionth
      staking_balance
  in
  let delegated =
    Full_staking_balance_repr.min_delegated_in_cycle
      ~cycle_eras
      ~current_level
      staking_balance
  in
  let limit_of_delegation_over_baking =
    Int64.of_int (Constants_storage.limit_of_delegation_over_baking ctxt)
  in
  (* Overstaked tez count as delegated.
     Note that, unlike delegated tez, overstaked tez may not have been staked
     the whole cycle to contribute to rights, but they are going to be frozen
     for several cycles. *)
  let* overstaked = Tez_repr.(staked_frozen -? allowed_staked_frozen) in
  let* delegated = Tez_repr.(delegated +? overstaked) in
  (* Overdelegated tez don't count. *)
  let delegated =
    match Tez_repr.(own_frozen *? limit_of_delegation_over_baking) with
    | Ok max_allowed_delegated -> Tez_repr.min max_allowed_delegated delegated
    | Error _max_allowed_delegated_overflows -> delegated
  in
  let* weighted_delegated =
    if Constants_storage.adaptive_issuance_enable ctxt then
      let edge_of_staking_over_delegation =
        Int64.of_int
          (Constants_storage.adaptive_issuance_edge_of_staking_over_delegation
             ctxt)
      in
      Tez_repr.(delegated /? edge_of_staking_over_delegation)
    else return delegated
  in
  let+ frozen = Tez_repr.(own_frozen +? allowed_staked_frozen) in
  Stake_repr.make ~frozen ~weighted_delegated

let optimal_frozen_wrt_delegated_without_ai ctxt full_staking_balance =
  let open Result_syntax in
  let limit_of_delegation_over_baking =
    Int64.of_int (Constants_storage.limit_of_delegation_over_baking ctxt)
  in
  (* Without AI, frozen deposit is optimal when `delegated =
     limit_of_delegation_over_baking * frozen`. Freezing more would
     unnecessarily freeze tokens, freezing less would under exploit delegated
     rights due to over-delegation limit.

     With AI the optimum is to freeze as much as possible, this computation
     would make no sense. *)
  let delegated =
    Full_staking_balance_repr.current_delegated full_staking_balance
  in
  let own_frozen = Full_staking_balance_repr.own_frozen full_staking_balance in
  let* power = Tez_repr.(delegated +? own_frozen) in
  let* opti_frozen =
    Tez_repr.mul_ratio
      ~rounding:`Up
      power
      ~num:1L
      ~den:(Int64.add limit_of_delegation_over_baking 1L)
  in
  let min_frozen = Constants_storage.minimal_frozen_stake ctxt in
  return (Tez_repr.max opti_frozen min_frozen)

let baking_weight ctxt staking_parameters f =
  let open Result_syntax in
  let+ s = apply_limits ctxt staking_parameters f in
  Stake_repr.staking_weight s
