(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

include Selected_distribution_storage

let get_full_staking_balance = Storage.Stake.Staking_balance.get

let has_minimal_stake ctxt staking_balance =
  let minimal_stake = Constants_storage.minimal_stake ctxt in
  Full_staking_balance_repr.has_minimal_stake_to_be_considered
    ~minimal_stake
    staking_balance

let initialize_delegate ctxt delegate ~delegated =
  let open Lwt_result_syntax in
  let current_level = Raw_context.current_level ctxt in
  let balance =
    Full_staking_balance_repr.init
      ~own_frozen:Tez_repr.zero
      ~staked_frozen:Tez_repr.zero
      ~delegated
      ~current_level
  in
  let* ctxt = Storage.Stake.Staking_balance.init ctxt delegate balance in
  if has_minimal_stake ctxt balance then
    let*! ctxt =
      Storage.Stake.Active_delegates_with_minimal_stake.add ctxt delegate
    in
    return ctxt
  else return ctxt

let update_stake ~f ctxt delegate =
  let open Lwt_result_syntax in
  let* staking_balance_before = get_full_staking_balance ctxt delegate in
  let*? staking_balance = f staking_balance_before in
  let* ctxt =
    Storage.Stake.Staking_balance.update ctxt delegate staking_balance
  in
  (* Since the staking balance has changed, the delegate might have
     moved across the minimal stake barrier. If so we may need to
     update the set of active delegates with minimal stake. *)
  let had_minimal_stake_before =
    has_minimal_stake ctxt staking_balance_before
  in
  let has_minimal_stake_after = has_minimal_stake ctxt staking_balance in
  match (had_minimal_stake_before, has_minimal_stake_after) with
  | true, false ->
      (* Decrease below the minimal stake. *)
      let* inactive = Delegate_activation_storage.is_inactive ctxt delegate in
      if inactive then
        (* The delegate is inactive so it wasn't in the set and we
           don't need to update it. *)
        return ctxt
      else
        let*! ctxt =
          Storage.Stake.Active_delegates_with_minimal_stake.remove ctxt delegate
        in
        return ctxt
  | false, true ->
      (* Increase above the minimal stake. *)
      let* inactive = Delegate_activation_storage.is_inactive ctxt delegate in
      if inactive then
        (* The delegate is inactive so we don't need to add it to the
           set. *)
        return ctxt
      else
        let*! ctxt =
          Storage.Stake.Active_delegates_with_minimal_stake.add ctxt delegate
        in
        return ctxt
  | false, false | true, true -> return ctxt

let remove_delegated_stake ctxt delegate amount =
  let current_level = Raw_context.current_level ctxt in
  let cycle_eras = Raw_context.cycle_eras ctxt in
  let f =
    Full_staking_balance_repr.remove_delegated
      ~cycle_eras
      ~current_level
      ~amount
  in
  update_stake ctxt delegate ~f

let remove_own_frozen_stake ctxt delegate amount =
  let f = Full_staking_balance_repr.remove_own_frozen ~amount in
  update_stake ctxt delegate ~f

let remove_staked_frozen_stake ctxt delegate amount =
  let f = Full_staking_balance_repr.remove_staked_frozen ~amount in
  update_stake ctxt delegate ~f

let remove_frozen_stake_only_call_from_token ctxt staker amount =
  match staker with
  | Frozen_staker_repr.Baker delegate ->
      remove_own_frozen_stake ctxt delegate amount
  | Frozen_staker_repr.Baker_edge delegate ->
      (* This case should not happen because [Baker_edge] is only
         intended to be used for rewards. *)
      remove_own_frozen_stake ctxt delegate amount
  | Single_staker {staker = _; delegate} | Shared_between_stakers {delegate} ->
      remove_staked_frozen_stake ctxt delegate amount

let add_delegated_stake ctxt delegate amount =
  let current_level = Raw_context.current_level ctxt in
  let cycle_eras = Raw_context.cycle_eras ctxt in
  let f =
    Full_staking_balance_repr.add_delegated ~cycle_eras ~current_level ~amount
  in
  update_stake ctxt delegate ~f

let add_own_frozen_stake ctxt delegate amount =
  let f = Full_staking_balance_repr.add_own_frozen ~amount in
  update_stake ctxt delegate ~f

let add_staked_frozen_stake ctxt delegate amount =
  let f = Full_staking_balance_repr.add_staked_frozen ~amount in
  update_stake ctxt delegate ~f

let add_frozen_stake_only_call_from_token ctxt staker amount =
  match staker with
  | Frozen_staker_repr.Baker delegate ->
      add_own_frozen_stake ctxt delegate amount
  | Frozen_staker_repr.Baker_edge delegate ->
      add_own_frozen_stake ctxt delegate amount
  | Single_staker {staker = _; delegate} | Shared_between_stakers {delegate} ->
      add_staked_frozen_stake ctxt delegate amount

let set_inactive ctxt delegate =
  let open Lwt_syntax in
  let* ctxt = Delegate_activation_storage.set_inactive ctxt delegate in
  Storage.Stake.Active_delegates_with_minimal_stake.remove ctxt delegate

let set_active ctxt delegate =
  let open Lwt_result_syntax in
  let* ctxt, inactive = Delegate_activation_storage.set_active ctxt delegate in
  if not inactive then return ctxt
  else
    let* staking_balance = get_full_staking_balance ctxt delegate in
    if has_minimal_stake ctxt staking_balance then
      let*! ctxt =
        Storage.Stake.Active_delegates_with_minimal_stake.add ctxt delegate
      in
      return ctxt
    else return ctxt

let fold_on_active_delegates_with_minimal_stake_es ctxt ~f ~order ~init =
  let open Lwt_result_syntax in
  Storage.Stake.Active_delegates_with_minimal_stake.fold
    ctxt
    ~order
    ~init:(Ok init)
    ~f:(fun delegate acc ->
      let*? acc in
      f delegate acc)

let clear_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  match Cycle_storage.cycle_to_clear_of_sampling_data ~new_cycle with
  | None -> return ctxt
  | Some cycle_to_clear ->
      let* ctxt =
        Storage.Stake.Total_active_stake.remove_existing ctxt cycle_to_clear
      in
      Selected_distribution_for_cycle.remove_existing ctxt cycle_to_clear

let fold_on_active_delegates_with_minimal_stake_s =
  Storage.Stake.Active_delegates_with_minimal_stake.fold

let remove_contract_delegated_stake ctxt contract amount =
  let open Lwt_result_syntax in
  let* delegate_opt = Contract_delegate_storage.find ctxt contract in
  match delegate_opt with
  | None -> return ctxt
  | Some delegate -> remove_delegated_stake ctxt delegate amount

let add_contract_delegated_stake ctxt contract amount =
  let open Lwt_result_syntax in
  let* delegate_opt = Contract_delegate_storage.find ctxt contract in
  match delegate_opt with
  | None -> return ctxt
  | Some delegate -> add_delegated_stake ctxt delegate amount

module For_RPC = struct
  let get_staking_balance ctxt delegate =
    let open Lwt_result_syntax in
    let* staking_balance = Storage.Stake.Staking_balance.get ctxt delegate in
    Lwt.return (Full_staking_balance_repr.current_total staking_balance)
end

module Internal_for_tests = struct
  let get ctxt delegate =
    let open Lwt_result_syntax in
    let*! result =
      Storage.Stake.Active_delegates_with_minimal_stake.mem ctxt delegate
    in
    match result with
    | true -> For_RPC.get_staking_balance ctxt delegate
    | false -> return Tez_repr.zero
end
