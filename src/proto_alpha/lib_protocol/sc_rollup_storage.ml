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

(** [address_from_nonce ctxt nonce] produces an address completely determined by
    an operation hash and an origination counter, and accounts for gas spent. *)
let address_from_nonce ctxt nonce =
  let open Tzresult_syntax in
  let* ctxt =
    Raw_context.consume_gas ctxt Sc_rollup_costs.Constants.cost_serialize_nonce
  in
  match Data_encoding.Binary.to_bytes_opt Origination_nonce.encoding nonce with
  | None -> error Sc_rollup_address_generation
  | Some nonce_bytes ->
      let bytes_len = Bytes.length nonce_bytes in
      let+ ctxt =
        Raw_context.consume_gas
          ctxt
          (Sc_rollup_costs.cost_hash_bytes ~bytes_len)
      in
      (ctxt, Sc_rollup_repr.Address.hash_bytes [nonce_bytes])

let originate ctxt ~kind ~boot_sector ~parameters_ty ~genesis_commitment =
  let open Lwt_tzresult_syntax in
  let*? ctxt, genesis_commitment_hash =
    Sc_rollup_commitment_storage.hash ctxt genesis_commitment
  in
  let*? ctxt, nonce = Raw_context.increment_origination_nonce ctxt in
  let level = Raw_context.current_level ctxt in
  let*? ctxt, address = address_from_nonce ctxt nonce in
  let* ctxt, pvm_kind_size, _kind_existed =
    Store.PVM_kind.add ctxt address kind
  in
  let* ctxt, genesis_info_size, _info_existed =
    Store.Genesis_info.add
      ctxt
      address
      {commitment_hash = genesis_commitment_hash; level = level.level}
  in
  let* ctxt, boot_sector_size, _sector_existed =
    Store.Boot_sector.add ctxt address boot_sector
  in
  let* ctxt, param_ty_size_diff, _added =
    Store.Parameters_type.add ctxt address parameters_ty
  in
  let*! inbox =
    Sc_rollup_inbox_repr.empty (Raw_context.recover ctxt) address level.level
  in
  let* ctxt, inbox_size_diff = Store.Inbox.init ctxt address inbox in
  let* ctxt, lcc_size_diff =
    Store.Last_cemented_commitment.init ctxt address genesis_commitment_hash
  in
  let* ctxt, commitment_size_diff, _was_bound =
    Store.Commitments.add
      (ctxt, address)
      genesis_commitment_hash
      genesis_commitment
  in
  (* This store [Store.Commitment_added] is going to be used to look this
     bootstrap commitment. This commitment is added here so the
     [sc_rollup_state_storage.deallocate] function does not have to handle a
     edge case. *)
  let* ctxt, commitment_added_size_diff, _commitment_existed =
    Store.Commitment_added.add
      (ctxt, address)
      genesis_commitment_hash
      level.level
  in
  (* This store [Store.Commitment_added] is going to be used to look this
     bootstrap commitment. This commitment is added here so the
     [sc_rollup_state_storage.deallocate] function does not have to handle a
     edge case.

     There is no staker for the genesis_commitment. *)
  let* ctxt, commitment_staker_count_size_diff, _commitment_staker_existed =
    Store.Commitment_stake_count.add
      (ctxt, address)
      genesis_commitment_hash
      Int32.zero
  in
  let* ctxt, stakers_size_diff = Store.Staker_count.init ctxt address 0l in
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size
     + inbox_size_diff + lcc_size_diff + commitment_size_diff
     + commitment_added_size_diff + commitment_staker_count_size_diff
     + stakers_size_diff + param_ty_size_diff + pvm_kind_size
     + genesis_info_size)
  in
  return (address, size, genesis_commitment_hash, ctxt)

let kind ctxt address =
  let open Lwt_tzresult_syntax in
  let* ctxt, kind_opt = Store.PVM_kind.find ctxt address in
  match kind_opt with
  | Some k -> return (ctxt, k)
  | None -> fail (Sc_rollup_errors.Sc_rollup_does_not_exist address)

let list_unaccounted ctxt =
  let open Lwt_syntax in
  let+ res = Store.PVM_kind.keys_unaccounted ctxt in
  Result.return res

let genesis_info ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* ctxt, genesis_info = Store.Genesis_info.find ctxt rollup in
  match genesis_info with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some genesis_info -> return (ctxt, genesis_info)

let get_boot_sector ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* ctxt, boot_sector = Storage.Sc_rollup.Boot_sector.find ctxt rollup in
  match boot_sector with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some boot_sector -> return (ctxt, boot_sector)

let parameters_type ctxt rollup =
  let open Lwt_result_syntax in
  let+ ctxt, res = Store.Parameters_type.find ctxt rollup in
  (res, ctxt)

module Dal_slot = struct
  open Dal_errors_repr

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
      fail (Dal_requested_subscriptions_at_future_level (current_level, level))
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
    let* _initial_level = genesis_info ctxt rollup in
    let {Level_repr.level; _} = Raw_context.current_level ctxt in
    let* subscribed_slots = subscribed_slots_at_level ctxt rollup level in
    let*? slot_already_subscribed =
      Bitset.mem subscribed_slots (Dal_slot_repr.Index.to_int slot_index)
    in
    if slot_already_subscribed then
      fail (Dal_rollup_already_registered_to_slot (rollup, slot_index))
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
    let* _initial_level = genesis_info ctxt rollup in
    let* subscribed_slots = subscribed_slots_at_level ctxt rollup level in
    let*? result = to_dal_slot_index_list subscribed_slots in
    return result
end
