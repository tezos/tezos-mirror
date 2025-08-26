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

(** Tolerated inactivity period depends on the delegate's stake ratio
    over the total active stake.

    If the stake is unknown, it means that the baker had no staking
    rights for the current cycle. In this case, the call to
    [set_active] is subsequent to a delegate registration or
    reactivation. As we do not know its future weight in the
    consensus, we take a conservative approach and assign it a low
    tolerance [tolerated_inactivity_period_low].

    If the ratio is greater than the threshold (expressed in 'per
    thousand'), we apply a low tolerance. Otherwise, we apply a high
    tolerance. *)
let tolerated_inactivity_period ctxt delegate =
  let open Lwt_result_syntax in
  let tolerance_threshold =
    Constants_storage.tolerated_inactivity_period_threshold ctxt
  in
  let tolerance_low = Constants_storage.tolerated_inactivity_period_low ctxt in
  let tolerance_high =
    Constants_storage.tolerated_inactivity_period_high ctxt
  in
  let current_cycle = Cycle_storage.current ctxt in
  if Cycle_repr.(current_cycle = root) then
    (* There is no selected distribution at chain initialisation, so
       we give a low tolerance *)
    return tolerance_low
  else
    let* delegates_stakes =
      Selected_distribution_storage.get_selected_distribution ctxt current_cycle
    in
    match
      List.assoc
        ~equal:Signature.Public_key_hash.equal
        delegate
        delegates_stakes
    with
    | None ->
        (* There is no stake registered at
           - a first delegate registration
           - a delegate reactivation
           - after decreasing its stake below [minimal_stake]
           so we give a low tolerance *)
        return tolerance_low
    | Some delegate_stake ->
        let+ total_stake =
          Selected_distribution_storage.get_total_active_stake
            ctxt
            current_cycle
        in
        let compare_stake_ratio_with_threshold =
          Int64.(
            compare
              (div
                 (mul 1000L (Stake_repr.staking_weight delegate_stake))
                 (Stake_repr.staking_weight total_stake))
              (of_int tolerance_threshold))
        in
        if Compare.Int.(compare_stake_ratio_with_threshold > 0) then
          tolerance_low
        else tolerance_high

let is_inactive ctxt delegate =
  let open Lwt_result_syntax in
  let*! inactive =
    Storage.Contract.Inactive_delegate.mem
      ctxt
      (Contract_repr.Implicit delegate)
  in
  if inactive then Lwt.return_ok inactive
  else
    let* cycle_opt =
      Storage.Contract.Delegate_last_cycle_before_deactivation.find
        ctxt
        (Contract_repr.Implicit delegate)
    in
    let+ tolerance = tolerated_inactivity_period ctxt delegate in
    match cycle_opt with
    | Some last_active_cycle ->
        let ({Level_repr.cycle = current_cycle; _} : Level_repr.t) =
          Raw_context.current_level ctxt
        in
        Cycle_repr.(add last_active_cycle tolerance < current_cycle)
    | None ->
        (* This case is only when called from `set_active`, when creating
             a contract. *)
        false

let last_cycle_before_deactivation ctxt delegate =
  let open Lwt_result_syntax in
  let* tolerance = tolerated_inactivity_period ctxt delegate in
  let contract = Contract_repr.Implicit delegate in
  let+ cycle =
    Storage.Contract.Delegate_last_cycle_before_deactivation.get ctxt contract
  in
  (* we give [tolerance] cycles to the delegate after its last active
     cycle before it can be deactivated *)
  Cycle_repr.add cycle tolerance

let set_inactive ctxt delegate =
  Storage.Contract.Inactive_delegate.add ctxt (Contract_repr.Implicit delegate)

let set_active ctxt delegate =
  let open Lwt_result_syntax in
  let* inactive = is_inactive ctxt delegate in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let consensus_rights_delay = Constants_storage.consensus_rights_delay ctxt in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* current_last_active_cycle =
    Storage.Contract.Delegate_last_cycle_before_deactivation.find
      ctxt
      delegate_contract
  in
  let last_active_cycle =
    (* if the delegate is new or inactive, we give it additionally
       [consensus_rights_delay] because the delegate needs this number
       of cycles to receive the rights *)
    match current_last_active_cycle with
    | None -> Cycle_repr.add current_cycle consensus_rights_delay
    | Some current_last_active_cycle ->
        let updated =
          if inactive then Cycle_repr.add current_cycle consensus_rights_delay
          else current_cycle
        in
        Cycle_repr.max current_last_active_cycle updated
  in
  let*! ctxt =
    Storage.Contract.Delegate_last_cycle_before_deactivation.add
      ctxt
      delegate_contract
      last_active_cycle
  in
  if not inactive then return (ctxt, inactive)
  else
    let*! ctxt =
      Storage.Contract.Inactive_delegate.remove ctxt delegate_contract
    in
    return (ctxt, inactive)
