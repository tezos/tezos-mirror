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
  let since_cycle =
    Cycle_repr.pred current_cycle |> Option.value ~default:Cycle_repr.root
  in
  let slashed_since =
    List.fold_left
      (fun slashed_since (cycle, slashed) ->
        if Cycle_repr.(cycle >= since_cycle) then
          Percentage.add_bounded slashed_since slashed
        else slashed_since)
      Percentage.p0
      slash_history
  in
  Percentage.(Compare.(slashed_since >= p51))

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

let update_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let forbidden_delegates = Raw_context.Consensus.forbidden_delegates ctxt in
  if Signature.Public_key_hash.Set.is_empty forbidden_delegates then return ctxt
  else
    let* selection_for_new_cycle =
      Stake_storage.get_selected_distribution_as_map ctxt new_cycle
    in
    let* forbidden_delegates =
      Signature.Public_key_hash.Set.fold_es
        (fun delegate acc ->
          let* slash_history_opt =
            Storage.Contract.Slashed_deposits.find ctxt (Implicit delegate)
          in
          let slash_history = Option.value slash_history_opt ~default:[] in
          (* To be unforbidden in the new cycle, a delegate must not meet the
             criterion used on denunciation anymore... *)
          if should_forbid ~current_cycle:new_cycle slash_history then
            return acc
          else
            (* ...and must have, either no rights (in which case there is no reason
               to keep it forbidden), or have at least half the frozen deposits it
               had when rights were computed (probably coming from autostaking). *)
            let+ unforbid =
              match
                Signature.Public_key_hash.Map.find
                  delegate
                  selection_for_new_cycle
              with
              | None -> return_true
              | Some {frozen; _} ->
                  let+ current_deposits =
                    Delegate_storage.current_frozen_deposits ctxt delegate
                  in
                  Tez_repr.(current_deposits >= div2 frozen)
            in
            if unforbid then
              `Changed
                (Signature.Public_key_hash.Set.remove
                   delegate
                   (match acc with
                   | `Unchanged -> forbidden_delegates
                   | `Changed forbidden_delegates -> forbidden_delegates))
            else acc)
        forbidden_delegates
        `Unchanged
    in
    match forbidden_delegates with
    | `Unchanged -> return ctxt
    | `Changed forbidden_delegates ->
        let*! ctxt = set_forbidden_delegates ctxt forbidden_delegates in
        return ctxt

let init_for_genesis ctxt =
  Storage.Tenderbake.Forbidden_delegates.init
    ctxt
    Signature.Public_key_hash.Set.empty
