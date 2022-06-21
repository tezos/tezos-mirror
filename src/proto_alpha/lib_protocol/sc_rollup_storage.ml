(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Sc_rollup_errors
module Store = Storage.Sc_rollup
module Commitment = Sc_rollup_commitment_repr
module Commitment_hash = Commitment.Hash

let originate ctxt ~kind ~boot_sector ~parameters_ty =
  Raw_context.increment_origination_nonce ctxt >>?= fun (ctxt, nonce) ->
  let level = Raw_context.current_level ctxt in
  Sc_rollup_repr.Address.from_nonce nonce >>?= fun address ->
  Store.PVM_kind.add ctxt address kind >>= fun ctxt ->
  Store.Initial_level.add ctxt address level.level >>= fun ctxt ->
  Store.Boot_sector.add ctxt address boot_sector >>= fun ctxt ->
  Store.Parameters_type.add ctxt address parameters_ty
  >>=? fun (ctxt, param_ty_size_diff, _added) ->
  let inbox = Sc_rollup_inbox_repr.empty address level.level in
  Store.Inbox.init ctxt address inbox >>=? fun (ctxt, inbox_size_diff) ->
  Store.Last_cemented_commitment.init ctxt address Commitment_hash.zero
  >>=? fun (ctxt, lcc_size_diff) ->
  Store.Staker_count.init ctxt address 0l >>=? fun (ctxt, stakers_size_diff) ->
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let boot_sector_size =
    Data_encoding.Binary.length Data_encoding.string boot_sector
  in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size
     + inbox_size_diff + lcc_size_diff + stakers_size_diff + param_ty_size_diff
      )
  in
  return (address, size, ctxt)

let kind ctxt address = Store.PVM_kind.find ctxt address

let list ctxt = Store.PVM_kind.keys ctxt >|= Result.return

let initial_level ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* level = Store.Initial_level.find ctxt rollup in
  match level with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some level -> return level

let get_boot_sector ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* boot_sector = Storage.Sc_rollup.Boot_sector.find ctxt rollup in
  match boot_sector with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some boot_sector -> return boot_sector

let parameters_type ctxt rollup =
  let open Lwt_result_syntax in
  let+ ctxt, res = Store.Parameters_type.find ctxt rollup in
  (res, ctxt)

module Outbox = struct
  let level_index ctxt level =
    let max_active_levels =
      Constants_storage.sc_rollup_max_active_outbox_levels ctxt
    in
    Int32.rem (Raw_level_repr.to_int32 level) max_active_levels

  let record_applied_message ctxt rollup level ~message_index =
    let open Lwt_tzresult_syntax in
    (* Check that the 0 <= message index < maximum number of outbox messages per
       level. *)
    let*? () =
      let max_outbox_messages_per_level =
        Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt
      in
      error_unless
        Compare.Int.(
          0 <= message_index && message_index < max_outbox_messages_per_level)
        Sc_rollup_invalid_outbox_message_index
    in
    let level_index = level_index ctxt level in
    let* ctxt, level_and_bitset_opt =
      Store.Applied_outbox_messages.find (ctxt, rollup) level_index
    in
    let*? bitset, ctxt =
      let open Tzresult_syntax in
      let* bitset, ctxt =
        match level_and_bitset_opt with
        | Some (existing_level, bitset)
          when Raw_level_repr.(existing_level = level) ->
            (* The level at the index is the same as requested. Fail if the
               message has been applied already. *)
            let* already_applied = Bitset.mem bitset message_index in
            let* () =
              error_when
                already_applied
                Sc_rollup_outbox_message_already_applied
            in
            return (bitset, ctxt)
        | Some (existing_level, _bitset)
          when Raw_level_repr.(level < existing_level) ->
            fail Sc_rollup_outbox_level_expired
        | Some _ | None ->
            (* The old level is outdated or there is no previous bitset at
               this index. *)
            return (Bitset.empty, ctxt)
      in
      let* bitset = Bitset.add bitset message_index in
      return (bitset, ctxt)
    in
    let+ ctxt, size_diff, _is_new =
      Store.Applied_outbox_messages.add
        (ctxt, rollup)
        level_index
        (level, bitset)
    in
    (Z.of_int size_diff, ctxt)
end

module Dal_slot = struct
  let slot_of_int_e n =
    let open Tzresult_syntax in
    match Dal_slot_repr.Index.of_int n with
    | None -> fail Dal_errors_repr.Dal_slot_index_above_hard_limit
    | Some slot_index -> return slot_index

  let fail_if_slot_index_invalid ctxt slot_index =
    let open Lwt_tzresult_syntax in
    let*? max_slot_index =
      slot_of_int_e @@ ((Raw_context.constants ctxt).dal.number_of_slots - 1)
    in
    if
      Compare.Int.(
        Dal_slot_repr.Index.compare slot_index max_slot_index > 0
        || Dal_slot_repr.Index.compare slot_index Dal_slot_repr.Index.zero < 0)
    then
      fail
        Dal_errors_repr.(
          Dal_subscribe_rollup_invalid_slot_index
            {given = slot_index; maximum = max_slot_index})
    else return slot_index

  let all_indexes ctxt =
    let max_slot_index = (Raw_context.constants ctxt).dal.number_of_slots - 1 in
    Misc.(0 --> max_slot_index) |> List.map slot_of_int_e |> all_e

  let subscribed_slots_at_level ctxt rollup level =
    let open Lwt_tzresult_syntax in
    let current_level = (Raw_context.current_level ctxt).level in
    if Raw_level_repr.(level > current_level) then
      fail
        (Sc_rollup_requested_dal_slot_subscriptions_of_future_level
           (current_level, level))
    else
      let*! subscription_levels =
        Store.Slot_subscriptions.keys (ctxt, rollup)
      in
      (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3170
         Improve code efficiency. *)
      let relevant_subscription_levels =
        subscription_levels
        |> List.filter (fun subscription_level ->
               Raw_level_repr.(subscription_level <= level))
      in
      let last_subscription_level_opt =
        List.fold_left
          (fun max_level level ->
            match max_level with
            | None -> Some level
            | Some max_level ->
                Some
                  (if Raw_level_repr.(max_level > level) then max_level
                  else level))
          None
          relevant_subscription_levels
      in
      match last_subscription_level_opt with
      | None -> return Bitset.empty
      | Some subscription_level ->
          Store.Slot_subscriptions.get (ctxt, rollup) subscription_level

  let subscribe ctxt rollup ~slot_index =
    let open Lwt_tzresult_syntax in
    let* _slot_index = fail_if_slot_index_invalid ctxt slot_index in
    (* Check if the rollup exists by looking for the initial level *)
    let* _initial_level = initial_level ctxt rollup in
    let {Level_repr.level; _} = Raw_context.current_level ctxt in
    let* subscribed_slots = subscribed_slots_at_level ctxt rollup level in
    let*? slot_already_subscribed =
      Bitset.mem subscribed_slots (Dal_slot_repr.Index.to_int slot_index)
    in
    if slot_already_subscribed then
      fail (Sc_rollup_dal_slot_already_registered (rollup, slot_index))
    else
      let*? subscribed_slots =
        Bitset.add subscribed_slots (Dal_slot_repr.Index.to_int slot_index)
      in
      let*! ctxt =
        (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3248
           remove too old entries and charge for storage. *)
        Store.Slot_subscriptions.add (ctxt, rollup) level subscribed_slots
      in
      return (slot_index, level, ctxt)

  let subscribed_slot_indices ctxt rollup level =
    let all_indexes = all_indexes ctxt in
    let to_dal_slot_index_list bitset =
      let open Result_syntax in
      let* all_indexes = all_indexes in
      let+ slot_indexes =
        all_indexes
        |> List.map (fun i ->
               let+ is_index_present =
                 Bitset.mem bitset (Dal_slot_repr.Index.to_int i)
               in
               if is_index_present then [i] else [])
        |> all_e
      in
      List.concat slot_indexes
    in
    let open Lwt_tzresult_syntax in
    (* Check if the rollup exists by looking for the initial level *)
    let* _initial_level = initial_level ctxt rollup in
    let* subscribed_slots = subscribed_slots_at_level ctxt rollup level in
    let*? result = to_dal_slot_index_list subscribed_slots in
    return result
end
