(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let is_forbidden ctxt delegate =
  let forbidden_delegates = Raw_context.Consensus.forbidden_delegates ctxt in
  Signature.Public_key_hash.Set.mem delegate forbidden_delegates

let forbid ctxt delegate =
  let ctxt = Raw_context.Consensus.forbid_delegate ctxt delegate in
  let new_forbidden_delegates =
    Raw_context.Consensus.forbidden_delegates ctxt
  in
  Storage.Tenderbake.Forbidden_delegates.add ctxt new_forbidden_delegates

let should_forbid ~current_cycle slash_history =
  let slashed_this_cycle =
    Storage.Slashed_deposits_history.get current_cycle slash_history
  in
  let slashed_previous_cycle =
    match Cycle_repr.pred current_cycle with
    | Some previous_cycle ->
        Storage.Slashed_deposits_history.get previous_cycle slash_history
    | None -> Int_percentage.p0
  in
  let slashed_both_cycles =
    Int_percentage.add_bounded slashed_this_cycle slashed_previous_cycle
  in
  Compare.Int.((slashed_both_cycles :> int) >= 100)

let may_forbid ctxt delegate ~current_cycle slash_history =
  let open Lwt_syntax in
  if should_forbid ~current_cycle slash_history then
    let+ ctxt = forbid ctxt delegate in
    (ctxt, true)
  else return (ctxt, false)

let load ctxt =
  let open Lwt_result_syntax in
  let* forbidden_delegates_opt =
    Storage.Tenderbake.Forbidden_delegates.find ctxt
  in
  let ctxt =
    match forbidden_delegates_opt with
    | Some forbidden_delegates ->
        Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
    | None ->
        Raw_context.Consensus.set_forbidden_delegates
          ctxt
          Signature.Public_key_hash.Set.empty
  in
  return ctxt

let set_forbidden_delegates ctxt forbidden_delegates =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Tenderbake.Forbidden_delegates.add ctxt forbidden_delegates
  in
  let ctxt =
    Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
  in
  return ctxt

let reset ctxt =
  if
    Signature.Public_key_hash.Set.is_empty
      (Raw_context.Consensus.forbidden_delegates ctxt)
  then Lwt.return ctxt
  else set_forbidden_delegates ctxt Signature.Public_key_hash.Set.empty

let update_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let*! ctxt = reset ctxt in
  let* selection_for_new_cycle =
    Stake_storage.get_selected_distribution ctxt new_cycle
  in
  List.fold_left_es
    (fun ctxt (delegate, _stake) ->
      let* current_deposits =
        Delegate_storage.current_frozen_deposits ctxt delegate
      in
      if Tez_repr.(current_deposits = zero) then
        (* If the delegate's current deposit remains at zero then we add it to
           the forbidden set. *)
        let*! ctxt = forbid ctxt delegate in
        return ctxt
      else return ctxt)
    ctxt
    selection_for_new_cycle

let init_for_genesis_and_oxford ctxt =
  Storage.Tenderbake.Forbidden_delegates.init
    ctxt
    Signature.Public_key_hash.Set.empty
