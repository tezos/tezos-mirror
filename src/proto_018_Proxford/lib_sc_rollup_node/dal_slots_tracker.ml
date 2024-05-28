(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
open Alpha_context

let ancestor_hash ~number_of_levels
    ({Node_context.genesis_info; _} as node_ctxt) head =
  let genesis_level = genesis_info.level in
  let rec go number_of_levels (Layer1.{hash; level} as head) =
    let open Lwt_result_syntax in
    if level < genesis_level then return_none
    else if number_of_levels = 0 then return_some hash
    else
      let* pred_head = Node_context.get_predecessor_opt node_ctxt head in
      match pred_head with
      | None -> return_none
      | Some pred_head -> go (number_of_levels - 1) pred_head
  in
  go number_of_levels head

(* Values of type `confirmations_info` are used to catalog the status of slots
   published in a given block hash. These values record whether
   the slot has been confirmed after the attestation_lag has passed. *)
type confirmations_info = {
  (* The hash of the block in which the slots have been published. *)
  published_block_hash : Block_hash.t;
  (* The indexes of slots that have beenp published in block
     with hash `published_block_hash`, and have later been confirmed. *)
  confirmed_slots_indexes : Bitset.t;
}

(** [slots_info constants node_ctxt head] gathers information about the slot confirmations
    of slot indexes. It reads the slot indexes that have been declared available
    from [head]'s block receipt. It then returns the hash of
    the block where the slot headers have been published and the list of
    slot indexes that have been confirmed for that block.  *)
let slots_info constants node_ctxt (Layer1.{hash; _} as head) =
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3722
     The case for protocol migrations when the lag constant has
     been changed is tricky, especially if the lag is reduced.
     Suppose that a slot header is published at the second last level of a
     cycle, and the lag is 2. The block is expected to be confirmed at the
     first level of the new cycle. However, if during the protocol migration
     we reduce the lag to 1, then the slots header will never be confirmed.
  *)
  let open Lwt_result_syntax in
  let lag = constants.Rollup_constants.dal.attestation_lag in
  (* we are downloading attestation for slots at level [level], so
     we need to download the data at level [level - lag].
  *)
  let* published_slots_block_hash =
    ancestor_hash ~number_of_levels:lag node_ctxt head
  in
  match published_slots_block_hash with
  | None ->
      (* Less then lag levels have passed from the rollup origination, and
         confirmed slots should not be applied *)
      return_none
  | Some published_block_hash ->
      let* {metadata; _} =
        Layer1_helpers.fetch_tezos_block node_ctxt.Node_context.l1_ctxt hash
      in
      let*? metadata =
        Option.to_result
          ~none:(TzTrace.make @@ Layer1_services.Cannot_read_block_metadata hash)
          metadata
      in
      (* `metadata.protocol_data.dal_attestation` is `None` if we are behind the
         `Dal feature flag`: in this case we return an empty slot
         attestation. *)
      let confirmed_slots =
        Option.value
          ~default:Dal.Attestation.empty
          metadata.protocol_data.dal_attestation
      in
      let* published_slots_indexes =
        Node_context.get_slot_indexes
          node_ctxt
          ~published_in_block_hash:published_block_hash
      in
      let number_of_slots = constants.dal.number_of_slots in
      let confirmed_slots_indexes_list =
        List.filter
          (Dal.Attestation.is_attested confirmed_slots)
          (List.filter_map
             (Dal.Slot_index.of_int_opt ~number_of_slots)
             published_slots_indexes)
      in
      let*? confirmed_slots_indexes =
        Environment.wrap_tzresult
          (confirmed_slots_indexes_list
          |> List.map Dal.Slot_index.to_int
          |> Bitset.from_list)
      in
      return @@ Some {published_block_hash; confirmed_slots_indexes}

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3884
   avoid going back and forth between bitsets and lists of slot indexes. *)
let to_slot_index_list (constants : Rollup_constants.protocol_constants) bitset
    =
  let all_slots = Misc.(0 --> (constants.dal.number_of_slots - 1)) in
  List.filter_e (Bitset.mem bitset) all_slots

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/4139.
   Use a shared storage between dal and rollup node to store slots data.
*)

let download_and_save_slots constants (node_context : _ Node_context.t)
    ~current_block_hash {published_block_hash; confirmed_slots_indexes} =
  let open Lwt_result_syntax in
  let*? all_slots =
    Bitset.fill ~length:constants.Rollup_constants.dal.number_of_slots
    |> Environment.wrap_tzresult
  in
  let*? not_confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list constants
    @@ Bitset.diff all_slots confirmed_slots_indexes
  in
  let*? confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list constants confirmed_slots_indexes
  in
  (* The contents of each slot index are written to a different location on
     disk, therefore calls to store contents for different slot indexes can
     be parallelized. *)
  let* () =
    List.iter_ep
      (fun s_slot ->
        Node_context.save_slot_status
          node_context
          current_block_hash
          s_slot
          `Unconfirmed)
      not_confirmed
  in
  List.iter_ep
    (fun s_slot ->
      let* () =
        Node_context.save_slot_status
          node_context
          current_block_hash
          s_slot
          `Confirmed
      in
      let*! () =
        Dal_slots_tracker_event.slot_has_been_confirmed
          (Sc_rollup_proto_types.Dal.Slot_index.of_octez
             ~number_of_slots:constants.dal.number_of_slots
             s_slot)
          published_block_hash
          current_block_hash
      in
      return_unit)
    confirmed

let process_head node_ctxt (Layer1.{hash = head_hash; level} as head) =
  let open Lwt_result_syntax in
  let* constants = Protocol_plugins.get_constants_of_level node_ctxt level in
  let* confirmation_info = slots_info constants node_ctxt head in
  match confirmation_info with
  | None -> return_unit
  | Some confirmation_info ->
      download_and_save_slots
        ~current_block_hash:head_hash
        constants
        node_ctxt
        confirmation_info
