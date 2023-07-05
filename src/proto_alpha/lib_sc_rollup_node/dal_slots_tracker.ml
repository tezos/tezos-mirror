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

(** [slots_info node_ctxt head] gathers information about the slot confirmations
    of slot indexes. It reads the slot indexes that have been declared available
    from [head]'s block receipt. It then returns the hash of
    the block where the slot headers have been published and the list of
    slot indexes that have been confirmed for that block.  *)
let slots_info node_ctxt (Layer1.{hash; _} as head) =
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3722
     The case for protocol migrations when the lag constant has
     been changed is tricky, especially if the lag is reduced.
     Suppose that a slot header is published at the second last level of a
     cycle, and the lag is 2. The block is expected to be confirmed at the
     first level of the new cycle. However, if during the protocol migration
     we reduce the lag to 1, then the slots header will never be confirmed.
  *)
  let open Lwt_result_syntax in
  let lag =
    node_ctxt.Node_context.current_protocol.constants.dal.attestation_lag
  in
  (* we are downloading endorsemented for slots at level [level], so
     we need to download the data at level [level - lag].
  *)
  let* published_slots_block_hash =
    ancestor_hash ~number_of_levels:lag node_ctxt head
  in
  match published_slots_block_hash with
  | None ->
      (* Less then lag levels have passed from the rollup origination, and
         confirmed slots should not be applied *)
      return None
  | Some published_block_hash ->
      let* {metadata; _} =
        Layer1_helpers.fetch_tezos_block node_ctxt.Node_context.l1_ctxt hash
      in
      let*? metadata =
        Option.to_result
          ~none:(TzTrace.make @@ Layer1_services.Cannot_read_block_metadata hash)
          metadata
      in
      (* `metadata.protocol_data.dal_attestation` is `None` if we are behind
          the `Dal feature flag`: in this case we return an empty slot endorsement.
      *)
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
      let number_of_slots =
        node_ctxt.Node_context.current_protocol.constants.dal.number_of_slots
      in
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

let download_and_save_slots (node_context : _ Node_context.t)
    ~current_block_hash {published_block_hash; confirmed_slots_indexes} =
  let open Lwt_result_syntax in
  let*? all_slots =
    Bitset.fill
      ~length:node_context.current_protocol.constants.dal.number_of_slots
    |> Environment.wrap_tzresult
  in
  let*? not_confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list node_context.current_protocol.constants
    @@ Bitset.diff all_slots confirmed_slots_indexes
  in
  let*? confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list
         node_context.current_protocol.constants
         confirmed_slots_indexes
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
             ~number_of_slots:
               node_context.current_protocol.constants.dal.number_of_slots
             s_slot)
          published_block_hash
          current_block_hash
      in
      return_unit)
    confirmed

module Confirmed_slots_history = struct
  (** [confirmed_slots_with_headers node_ctxt confirmations_info] returns the
      headers of confirmed slot indexes for the block with hash
      [confirmations_info.published_block_hash]. *)
  let confirmed_slots_with_headers node_ctxt
      {published_block_hash; confirmed_slots_indexes; _} =
    let open Lwt_result_syntax in
    let*? relevant_slots_indexes =
      Environment.wrap_tzresult
      @@ to_slot_index_list
           node_ctxt.Node_context.current_protocol.constants
           confirmed_slots_indexes
    in
    List.map_ep
      (fun slot_index ->
        let+ h =
          Node_context.get_slot_header
            node_ctxt
            ~published_in_block_hash:published_block_hash
            slot_index
        in
        Sc_rollup_proto_types.Dal.Slot_header.of_octez
          ~number_of_slots:
            node_ctxt.current_protocol.constants.dal.number_of_slots
          h)
      relevant_slots_indexes

  let read_slots_history_from_l1 {Node_context.cctxt; _} block =
    let open Lwt_result_syntax in
    (* We return the empty Slots_history if DAL is not enabled. *)
    let* slots_list_opt =
      RPC.Dal.dal_confirmed_slots_history
        (new Protocol_client_context.wrap_full cctxt)
        (cctxt#chain, `Hash (block, 0))
    in
    return @@ Option.value slots_list_opt ~default:Dal.Slots_history.genesis

  (** Depending on the rollup's origination level and on the DAL's endorsement
      lag, the rollup node should start processing confirmed slots and update its
      slots_history and slots_history's cache entries in the store after
      [origination_level + attestation_lag] blocks. This function checks if
      that level is reached or not.  *)
  let should_process_dal_slots node_ctxt block_level =
    let open Node_context in
    let lag =
      Int32.of_int
        node_ctxt.Node_context.current_protocol.constants.dal.attestation_lag
    in
    let block_level = Raw_level.to_int32 block_level in
    let genesis_level = node_ctxt.genesis_info.level in
    Int32.(block_level >= add lag genesis_level)

  let dal_entry_of_block_hash node_ctxt
      Layer1.{hash = block_hash; level = block_level} ~entry_kind ~find ~default
      =
    let open Lwt_result_syntax in
    let* confirmed_slots_history_opt = find node_ctxt block_hash in
    let block_level = Raw_level.of_int32_exn block_level in
    let should_process_dal_slots =
      should_process_dal_slots node_ctxt block_level
    in
    match (confirmed_slots_history_opt, should_process_dal_slots) with
    | Some confirmed_dal_slots, true -> return confirmed_dal_slots
    | None, false -> default node_ctxt block_hash
    | Some _confirmed_dal_slots, false ->
        failwith
          "The confirmed DAL %S for block hash %a (level = %a) is not expected \
           to be found in the store, but is exists."
          entry_kind
          Block_hash.pp
          block_hash
          Raw_level.pp
          block_level
    | None, true ->
        failwith
          "The confirmed DAL %S for block hash %a (level = %a) is expected to \
           be found in the store, but is missing."
          entry_kind
          Block_hash.pp
          block_hash
          Raw_level.pp
          block_level

  let slots_history_of_hash node_ctxt block =
    let find node_ctxt block =
      let open Lwt_result_syntax in
      let+ hist = Node_context.find_confirmed_slots_history node_ctxt block in
      Option.map Sc_rollup_proto_types.Dal.Slot_history.of_octez hist
    in
    dal_entry_of_block_hash
      node_ctxt
      block
      ~entry_kind:"slots history"
      ~find
      ~default:read_slots_history_from_l1

  let slots_history_cache_of_hash node_ctxt block =
    let find node_ctxt block =
      let open Lwt_result_syntax in
      let+ hist = Node_context.find_confirmed_slots_histories node_ctxt block in
      Option.map Sc_rollup_proto_types.Dal.Slot_history_cache.of_octez hist
    in
    dal_entry_of_block_hash
      node_ctxt
      block
      ~entry_kind:"slots history cache"
      ~find
      ~default:(fun node_ctxt _block ->
        let num_slots =
          node_ctxt.Node_context.current_protocol.constants.dal.number_of_slots
        in
        (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3788
           Put an accurate value for capacity. The value
                  `num_slots * 60000` below is chosen based on:
               - The number of remembered L1 inboxes in their corresponding
                 cache (60000),
               - The (max) number of slots (num_slots) that could be attested
                 per L1 block,
               - The way the Slots_history.t skip list is implemented (one slot
                 per cell). *)
        return
        @@ Dal.Slots_history.History_cache.empty
             ~capacity:(Int64.of_int @@ (num_slots * 60000)))

  let update node_ctxt Layer1.({hash = head_hash; _} as head) confirmation_info
      =
    let open Lwt_result_syntax in
    let* slots_to_save =
      confirmed_slots_with_headers node_ctxt confirmation_info
    in
    let slots_to_save =
      let open Dal in
      List.fast_sort
        (fun Slot.Header.{id = {index = a; _}; _} {id = {index = b; _}; _} ->
          Slot_index.compare a b)
        slots_to_save
    in
    let* pred = Node_context.get_predecessor node_ctxt head in
    let* slots_history = slots_history_of_hash node_ctxt pred in
    let* slots_cache = slots_history_cache_of_hash node_ctxt pred in
    let*? slots_history, slots_cache =
      Dal.Slots_history.add_confirmed_slot_headers
        slots_history
        slots_cache
        slots_to_save
      |> Environment.wrap_tzresult
    in
    (* The value of [slots_history] computed here is supposed to be equal to the
       one computed stored for block [head_hash] on L1, we basically re-do the
       computation here because we need to build/maintain the [slots_cache]
       bounded cache in case we need it for refutation game. *)
    (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/3856
       Attempt to improve this process. *)
    let* () =
      Node_context.save_confirmed_slots_history
        node_ctxt
        head_hash
        (Sc_rollup_proto_types.Dal.Slot_history.to_octez slots_history)
    in
    let* () =
      Node_context.save_confirmed_slots_histories
        node_ctxt
        head_hash
        (Sc_rollup_proto_types.Dal.Slot_history_cache.to_octez slots_cache)
    in
    return ()
end

let process_head node_ctxt (Layer1.{hash = head_hash; _} as head) =
  let open Lwt_result_syntax in
  let* confirmation_info = slots_info node_ctxt head in
  match confirmation_info with
  | None -> return_unit
  | Some confirmation_info ->
      let* () =
        download_and_save_slots
          ~current_block_hash:head_hash
          node_ctxt
          confirmation_info
      in
      Confirmed_slots_history.update node_ctxt head confirmation_info

let slots_history_of_hash = Confirmed_slots_history.slots_history_of_hash

let slots_history_cache_of_hash =
  Confirmed_slots_history.slots_history_cache_of_hash
