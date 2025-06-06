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

let should_unforbid ctxt delegate ~selection_for_new_cycle =
  let open Lwt_result_syntax in
  (* A delegate who has pending denunciations for which slashing has
     not been applied yet should stay forbidden, because their frozen
     deposits are going to decrease by a yet unknown amount. *)
  let*! has_pending_denunciations =
    Pending_denunciations_storage.has_pending_denunciations ctxt delegate
  in
  if has_pending_denunciations then return_false
  else
    (* To get unforbidden, a delegate's current frozen deposits must
       be high enough to insure the next cycle's baking rights. More
       precisely, their [current_frozen_deposits] must at least match
       [frozen], where:

       - [current_frozen_deposits] is the sum of the delegate's own
       frozen funds and their stakers'; it doesn't necessarily observe
       overstaking limits.

       - [frozen] is the frozen stake that was used in the past to
       compute the baking rights for the new cycle. It includes past
       frozen balances from the delegate and their stakers, but
       excludes any overstaked funds (as enforced by
       {!Stake_context.apply_limits}). *)
    match
      Signature.Public_key_hash.Map.find delegate selection_for_new_cycle
    with
    | None -> return_true
    | Some {Stake_repr.frozen; _} ->
        let* current_frozen_deposits =
          Delegate_storage.current_frozen_deposits ctxt delegate
        in
        return Tez_repr.(current_frozen_deposits >= frozen)

let update_at_cycle_end_after_slashing ctxt ~new_cycle =
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
          let* should_unforbid =
            should_unforbid ctxt delegate ~selection_for_new_cycle
          in
          if should_unforbid then
            let old_forbidden =
              match acc with
              | `Unchanged -> forbidden_delegates
              | `Changed forbidden_delegates -> forbidden_delegates
            in
            let new_forbidden =
              Signature.Public_key_hash.Set.remove delegate old_forbidden
            in
            return (`Changed new_forbidden)
          else return acc)
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
