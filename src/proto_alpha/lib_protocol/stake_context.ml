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
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let own_frozen = Full_staking_balance_repr.own_frozen staking_balance in
  let staked_frozen = Full_staking_balance_repr.staked_frozen staking_balance in
  let delegated =
    Full_staking_balance_repr.min_delegated_in_cycle
      ~current_cycle
      staking_balance
  in
  let limit_of_delegation_over_baking =
    Int64.of_int (Constants_storage.limit_of_delegation_over_baking ctxt)
  in
  let global_limit_of_staking_over_baking_millionth =
    Int64.(
      mul
        1_000_000L
        (of_int
           (Constants_storage
            .adaptive_issuance_global_limit_of_staking_over_baking
              ctxt)))
  in
  let {Staking_parameters_repr.limit_of_staking_over_baking_millionth; _} =
    staking_parameters
  in
  let limit_of_staking_over_baking_millionth =
    let delegate_limit_of_staking_over_baking_millionth =
      Int64.of_int32 limit_of_staking_over_baking_millionth
    in
    Compare.Int64.min
      global_limit_of_staking_over_baking_millionth
      delegate_limit_of_staking_over_baking_millionth
  in
  let allowed_staked_frozen =
    match
      Tez_repr.mul_ratio
        ~rounding:`Down
        own_frozen
        ~num:limit_of_staking_over_baking_millionth
        ~den:1_000_000L
    with
    | Ok max_allowed_staked_frozen ->
        Tez_repr.min staked_frozen max_allowed_staked_frozen
    | Error _max_allowed_staked_frozen_overflows -> staked_frozen
  in
  (* Overstaked tez count as delegated. *)
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
