(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let of_delegate ctxt delegate =
  let open Lwt_result_syntax in
  let* t =
    Storage.Contract.Staking_parameters.find
      ctxt
      (Contract_repr.Implicit delegate)
  in
  match t with
  | None -> return Staking_parameters_repr.default
  | Some t -> return t

let find ctxt delegate =
  Storage.Contract.Staking_parameters.find
    ctxt
    (Contract_repr.Implicit delegate)

let pending_updates ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let to_cycle = Cycle_repr.add current_cycle (preserved_cycles + 1) in
  List.filter_map_es
    (fun cycle ->
      let open Lwt_result_syntax in
      let+ param_opt =
        Storage.Pending_staking_parameters.find (ctxt, cycle) contract
      in
      Option.map (fun param -> (cycle, param)) param_opt)
    Cycle_repr.(current_cycle ---> to_cycle)

let of_delegate_for_cycle ctxt delegate cycle =
  Storage.Pending_staking_parameters.get (ctxt, cycle) (Implicit delegate)

let register_update ctxt delegate t =
  let open Lwt_result_syntax in
  let update_cycle =
    let current_level = Raw_context.current_level ctxt in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    Cycle_repr.add current_level.cycle (preserved_cycles + 1)
  in
  let*! ctxt =
    Storage.Pending_staking_parameters.add
      (ctxt, update_cycle)
      (Contract_repr.Implicit delegate)
      t
  in
  return ctxt

let activate ctxt ~new_cycle =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Pending_staking_parameters.fold
      (ctxt, new_cycle)
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun delegate t ctxt ->
        Storage.Contract.Staking_parameters.add ctxt delegate t)
  in
  Storage.Pending_staking_parameters.clear (ctxt, new_cycle)

type reward_distrib = {to_frozen : Tez_repr.t; to_spendable : Tez_repr.t}

(** Compute the reward distribution between frozen and spendable according to:
    - the [stake] of the delegate composed of the [frozen] deposits and the
      [delegated] tokens.
    - the [edge_of_baking_over_staking_billionth] parameter set by the baker in 1_000_000_000th
    - the [edge_of_staking_over_delegation] constant.
    - the [rewards] to be distributed

Preconditions:
 - [edge_of_staking_over_delegation] > 0
 - 0 <= [edge_of_baking_over_staking_billionth]  <= 1_000_000_000
*)
let compute_reward_distrib ~stake ~edge_of_baking_over_staking_billionth
    ~edge_of_staking_over_delegation ~(rewards : Tez_repr.t) =
  let ({frozen; delegated} : Stake_repr.t) = stake in
  (* convert into Q *)
  let delegated = (* >= 0 *) Q.of_int64 @@ Tez_repr.to_mutez delegated in
  let frozen = (* >= 0 *) Q.of_int64 @@ Tez_repr.to_mutez frozen in
  let baking_over_staking_edge (* 0 <= baking_over_staking_edge <= 1 *) =
    Q.(of_int32 edge_of_baking_over_staking_billionth / of_int 1_000_000_000)
  in
  let edge_of_staking_over_delegation (* > 0 ? *) =
    Q.of_int edge_of_staking_over_delegation
  in
  let rewards_q = Q.of_int64 @@ Tez_repr.to_mutez rewards in
  (* compute in Q *)
  let to_frozen =
    let open Q in
    let weighted_delegated = delegated / edge_of_staking_over_delegation in
    let total_stake = weighted_delegated + frozen in
    if total_stake <= zero then zero
    else
      let non_delegated_ratio = frozen / total_stake in
      let non_delegated_rewards = rewards_q * non_delegated_ratio in
      non_delegated_rewards * (one - baking_over_staking_edge)
  in
  (* finish computation into mutez *)
  let rewards = Tez_repr.to_mutez rewards in
  let to_frozen = Q.to_int64 to_frozen in
  (* todo: is there any risk to overflow here ? *)
  let to_spendable = Int64.(sub rewards to_frozen) in
  (* convert back to tez *)
  (* Preconditions prevents to_frozen to be negative or greater than
     rewards. Thus we can use to_mutez_exn *)
  let to_frozen = Tez_repr.of_mutez_exn to_frozen in
  let to_spendable = Tez_repr.of_mutez_exn to_spendable in
  ok {to_frozen; to_spendable}

let compute_reward_distrib ctxt delegate stake rewards =
  let open Lwt_result_syntax in
  let* (delegate_parameter : Staking_parameters_repr.t) =
    of_delegate ctxt delegate
  in
  let edge_of_baking_over_staking_billionth =
    delegate_parameter.edge_of_baking_over_staking_billionth
  in
  let edge_of_staking_over_delegation =
    if Constants_storage.adaptive_issuance_enable ctxt then
      Constants_storage.adaptive_issuance_edge_of_staking_over_delegation ctxt
    else 1
  in
  Lwt.return
  @@ compute_reward_distrib
       ~stake
       ~edge_of_baking_over_staking_billionth
       ~edge_of_staking_over_delegation
       ~rewards

let pay_rewards ctxt ?active_stake ~source ~delegate rewards =
  let open Lwt_result_syntax in
  let*? active_stake =
    let open Result_syntax in
    match active_stake with
    | Some active_stake -> ok active_stake
    | None ->
        let+ stake_distrib =
          Raw_context.stake_distribution_for_current_cycle ctxt
        in
        Option.value
          (Signature.Public_key_hash.Map.find delegate stake_distrib)
          ~default:Stake_repr.zero
  in
  let* {to_frozen; to_spendable} =
    compute_reward_distrib ctxt delegate active_stake rewards
  in
  let* ctxt, balance_updates_frozen_rewards =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Stake_repr.Shared delegate))
      to_frozen
  in
  let+ ctxt, balance_updates_spendable_rewards =
    Token.transfer
      ctxt
      source
      (`Contract (Contract_repr.Implicit delegate))
      to_spendable
  in
  (ctxt, balance_updates_frozen_rewards @ balance_updates_spendable_rewards)
