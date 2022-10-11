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
module Block_services = Block_services.Make (Protocol) (Protocol)

type error += Cannot_read_block_metadata of Block_hash.t

let () =
  register_error_kind
    ~id:"sc_rollup.node.cannot_read_receipt_of_block"
    ~title:"Cannot read receipt of block from L1"
    ~description:"The receipt of a block could not be read."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Could not read block receipt for block with hash %a."
        Block_hash.pp
        hash)
    `Temporary
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_read_block_metadata hash -> Some hash | _ -> None)
    (fun hash -> Cannot_read_block_metadata hash)

let get_slot_subscriptions cctxt head rollup =
  let open Lwt_result_syntax in
  let*? level = Environment.wrap_tzresult @@ Raw_level.of_int32 (snd head) in
  Plugin.RPC.Sc_rollup.dal_slot_subscriptions
    cctxt
    (cctxt#chain, cctxt#block)
    rollup
    level

(* [fetch_and_save_subscribed_slot_headers cctxt head rollup] fetches the slot
   indices to which [rollup] is subscribed to at [head], and stores them. *)
let fetch_and_save_subscribed_slot_headers
    Node_context.{cctxt; rollup_address; store; _}
    Layer1.{level; hash = head_hash} =
  let open Lwt_result_syntax in
  let* res = get_slot_subscriptions cctxt (head_hash, level) rollup_address in
  let*! () = Store.Dal_slot_subscriptions.add store head_hash res in
  return_unit

let ancestor_hash ~number_of_levels {Node_context.genesis_info; l1_ctxt; _} head
    =
  let genesis_level = genesis_info.level in
  let rec go number_of_levels (Layer1.{hash; level} as head) =
    let open Lwt_result_syntax in
    if level < Raw_level.to_int32 genesis_level then return_none
    else if number_of_levels = 0 then return_some hash
    else
      let* pred_head = Layer1.get_predecessor_opt l1_ctxt head in
      match pred_head with
      | None -> return_none
      | Some pred_head -> go (number_of_levels - 1) pred_head
  in
  go number_of_levels head

(* Values of type `confirmations_info` are used to catalog the status of slots
   published in a given block hash. These values record whether the rollup was
   subscribed to a given slot index when the block was published, and whether
   the slot has been confirmed after the endorsement_lag has passed. *)
type confirmations_info = {
  (* The hash of the block in which the slots have been published. *)
  published_block_hash : Block_hash.t;
  (* The slot indexes to which the rollup is subscribed to in
     the block with hash `published_block_hash. *)
  subscribed_slots_indexes : Bitset.t;
  (* The indexes of slots that have beenp published in block
     with hash `published_block_hash`, and have later been confirmed. *)
  confirmed_slots_indexes : Bitset.t;
}

(** [slots_info node_ctxt head] gathers information about the slot subscriptions
    and confirmations of slot indexes. It reads the slot indexes that have been
    declared available from [head]'s block receipt, and the list of slot index
    subscriptions from the block to which the confirmations refer to.
    It then returns the hash of the block where the slot headers have been
    published, the list of slots indexes to which the rollup was
    subscribed to in that block, and the list of slot indexes that have
    been confirmed for that block. There is no relationship between
    the confirmed and subscribed slot indexes returned by this function. *)
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
    node_ctxt.Node_context.protocol_constants.parametric.dal.endorsement_lag
  in
  (* we are downloading endorsemented for slots at level [level], so
     we need to download the data at level [level - lag].
     Therefore, we need to check only the slots to which the rollup node
     was subscribed to at level `level - lag`
  *)
  let* published_slots_block_hash =
    ancestor_hash ~number_of_levels:lag node_ctxt head
  in
  match published_slots_block_hash with
  | None ->
      (* Less then lag levels have passed from the rollup origination, and confirmed slots should not be applied *)
      return None
  | Some published_block_hash ->
      let* {metadata; _} =
        Layer1.fetch_tezos_block node_ctxt.Node_context.l1_ctxt hash
      in
      let*? metadata =
        Option.to_result
          ~none:(TzTrace.make @@ Cannot_read_block_metadata hash)
          metadata
      in
      (* `metadata.protocol_data.dal_slot_availability` is `None` if we are behind
          the `Dal feature flag`: in this case we return an empty slot endorsement.
      *)
      let confirmed_slots =
        Option.value
          ~default:Dal.Endorsement.empty
          metadata.protocol_data.dal_slot_availability
      in
      let*! subscribed_slots_indexes_list =
        Store.Dal_slot_subscriptions.get node_ctxt.store published_block_hash
      in
      let*? subscribed_slots_indexes =
        Environment.wrap_tzresult
          (subscribed_slots_indexes_list
          |> List.map Dal.Slot_index.to_int
          |> Bitset.from_list)
      in
      let*! published_slots_indexes =
        Store.Dal_slots_headers.list_secondary_keys
          node_ctxt.store
          ~primary_key:published_block_hash
      in
      let confirmed_slots_indexes_list =
        List.filter
          (Dal.Endorsement.is_available confirmed_slots)
          published_slots_indexes
      in
      let*? confirmed_slots_indexes =
        Environment.wrap_tzresult
          (confirmed_slots_indexes_list
          |> List.map Dal.Slot_index.to_int
          |> Bitset.from_list)
      in
      return
      @@ Some
           {
             published_block_hash;
             subscribed_slots_indexes;
             confirmed_slots_indexes;
           }

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3884
   avoid going back and forth between bitsets and lists of slot indexes. *)
let to_slot_index_list (constants : Constants.Parametric.t) bitset =
  let open Result_syntax in
  let all_slots = Misc.(0 --> (constants.dal.number_of_slots - 1)) in
  let+ filtered = List.filter_e (Bitset.mem bitset) all_slots in
  (* Because the maximum slot index is smaller than the number_of_slots protocol
     constants, and this value is smaller than the hard limit imposed for slots,
     then Dal.Slot_index.to_int will always return a defined value. See
     `src/proto_alpha/lib_protocol/constants_repr.ml`.
  *)
  List.filter_map Dal.Slot_index.of_int filtered

let save_unconfirmed_slots store current_block_hash slot_index number_of_pages =
  (* Adding multiple entries with the same primary key amounts to updating the
     contents of an in-memory map, hence pages must be added sequentially. *)
  List.iter_s
    (fun page_number ->
      Store.Dal_slot_pages.add
        store
        ~primary_key:current_block_hash
        ~secondary_key:(slot_index, page_number)
        None)
    Misc.(0 --> (number_of_pages - 1))

let save_confirmed_slot store current_block_hash slot_index pages =
  (* Adding multiple entries with the same primary key amounts to updating the
     contents of an in-memory map, hence pages must be added sequentially. *)
  List.iteri_s
    (fun page_number page ->
      Store.Dal_slot_pages.add
        store
        ~primary_key:current_block_hash
        ~secondary_key:(slot_index, page_number)
        (Some page))
    pages

let download_and_save_slots
    {Node_context.store; dal_cctxt; protocol_constants; _} ~current_block_hash
    {published_block_hash; subscribed_slots_indexes; confirmed_slots_indexes} =
  let open Lwt_result_syntax in
  let*? subscribed_not_confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list protocol_constants.parametric
    @@ Bitset.diff subscribed_slots_indexes confirmed_slots_indexes
  in
  let*? subscribed_and_confirmed =
    Environment.wrap_tzresult
    @@ to_slot_index_list protocol_constants.parametric
    @@ Bitset.inter subscribed_slots_indexes confirmed_slots_indexes
  in
  let params = protocol_constants.parametric.dal.cryptobox_parameters in
  let number_of_pages = params.slot_size / params.page_size in
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/2766.
     As part of the clean rollup storage workflow, we should make sure that
     pages for old slots are removed from the storage when not needed anymore.
  *)
  (* The contents of each slot index are written to a different location on
     disk, therefore calls to store contents for different slot indexes can
     be parallelized. *)
  let*! () =
    subscribed_not_confirmed
    |> List.iter_p (fun s_slot ->
           save_unconfirmed_slots
             store
             current_block_hash
             s_slot
             number_of_pages)
  in
  let* () =
    subscribed_and_confirmed
    |> List.iter_ep (fun s_slot ->
           (* slot_header is missing but the slot with index s_slot has been
                confirmed. This scenario should not be possible. *)
           let*! slot_header_is_stored =
             Store.Dal_slots_headers.mem
               store
               ~primary_key:published_block_hash
               ~secondary_key:s_slot
           in
           if not slot_header_is_stored then
             failwith "Slot header was not found in store"
           else
             let*! {commitment; _} =
               Store.Dal_slots_headers.get
                 store
                 ~primary_key:published_block_hash
                 ~secondary_key:s_slot
             in
             let* pages = Dal_node_client.get_slot_pages dal_cctxt commitment in
             let*! () =
               save_confirmed_slot store current_block_hash s_slot pages
             in
             (* The slot with index s_slot is subscribed to and endorsed,
                we can proceed retrieving it from the dal node and save it
                in the store. *)
             let*! () =
               Dal_slots_tracker_event.slot_has_been_confirmed
                 s_slot
                 published_block_hash
                 current_block_hash
             in
             return_unit)
  in
  return_unit

module Confirmed_slots_history = struct
  (** [confirmed_slots_with_headers node_ctxt confirmations_info] returns
    the headers of confirmed (but not necessarily subscribed to) slot indexes
    for the block with hash [confirmations_info.published_block_hash]. *)
  let confirmed_slots_with_headers node_ctxt
      {published_block_hash; confirmed_slots_indexes; _} =
    let open Lwt_result_syntax in
    let*? relevant_slots_indexes =
      Environment.wrap_tzresult
      @@ to_slot_index_list
           node_ctxt.Node_context.protocol_constants.parametric
           confirmed_slots_indexes
    in
    let*! confirmed_slots_indexes_with_header =
      List.map_p
        (fun slot_index ->
          Store.Dal_slots_headers.get
            node_ctxt.Node_context.store
            ~primary_key:published_block_hash
            ~secondary_key:slot_index)
        relevant_slots_indexes
    in
    return confirmed_slots_indexes_with_header

  let read_slots_history_from_l1 {Node_context.l1_ctxt = {cctxt; _}; _} block =
    let open Lwt_result_syntax in
    (* We return the empty Slots_history if DAL is not enabled. *)
    let* slots_list_opt =
      RPC.Dal.dal_confirmed_slots_history cctxt (cctxt#chain, `Hash (block, 0))
    in
    return @@ Option.value slots_list_opt ~default:Dal.Slots_history.genesis

  let slots_history_of_hash node_ctxt
      Layer1.{hash = block_hash; level = block_level} =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! confirmed_slots_history_opt =
      Store.Dal_confirmed_slots_history.find node_ctxt.store block_hash
    in
    match confirmed_slots_history_opt with
    | Some confirmed_dal_slots -> return confirmed_dal_slots
    | None ->
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          (* We won't find "dal slots history" for blocks before the
             rollup origination level in the node's store. In this case, we get
             the value of the skip list form L1 in case DAL is enabled, or the
             genesis DAL skip list otherwise. *)
          read_slots_history_from_l1 node_ctxt block_hash
        else
          (* We won't find "dal slots history" for blocks after the rollup
             origination level. This should not happen in normal circumstances. *)
          failwith
            "The confirmed DAL slots history for block hash %a (level = %a) is \
             missing."
            Block_hash.pp
            block_hash
            Raw_level.pp
            block_level

  let slots_history_cache_of_hash node_ctxt
      Layer1.{hash = block_hash; level = block_level} =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! confirmed_slots_history_cache_opt =
      Store.Dal_confirmed_slots_histories.find node_ctxt.store block_hash
    in
    match confirmed_slots_history_cache_opt with
    | Some cache -> return cache
    | None ->
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          (* We won't find "dal slots history cache" for blocks before the
             rollup origination level. In this case, we initialize with the
             empty cache. *)
          let num_slots =
            node_ctxt.Node_context.protocol_constants.parametric.dal
              .number_of_slots
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
               ~capacity:(Int64.of_int @@ (num_slots * 60000))
        else
          (* We won't find "dal slots history cache" for blocks after the rollup
             origination level. This should not happen in normal circumstances. *)
          failwith
            "The confirmed DAL slots history cache for block hash %a (level = \
             %a) is missing."
            Block_hash.pp
            block_hash
            Raw_level.pp
            block_level

  let update (Node_context.{store; l1_ctxt; _} as node_ctxt)
      Layer1.({hash = head_hash; _} as head) confirmation_info =
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
    let* pred = Layer1.get_predecessor l1_ctxt head in
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
    let*! () =
      Store.Dal_confirmed_slots_history.add store head_hash slots_history
    in
    let*! () =
      Store.Dal_confirmed_slots_histories.add store head_hash slots_cache
    in
    return ()
end

let process_head node_ctxt (Layer1.{hash = head_hash; _} as head) =
  let open Lwt_result_syntax in
  let* () = fetch_and_save_subscribed_slot_headers node_ctxt head in
  let* confirmation_info = slots_info node_ctxt head in
  match confirmation_info with
  | None -> return_unit
  | Some confirmation_info ->
      (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3867.
         Pre-fetch the slots to which the rollup node is subscribed to.
         Note that subscriptions are not used anymore to determine the slot pages
         that will be applied in the PVM. However, we currently use them to
         determine the slot indexes that will be pre-fetched by the rollup node.
      *)
      let* () =
        download_and_save_slots
          ~current_block_hash:head_hash
          node_ctxt
          confirmation_info
      in
      Confirmed_slots_history.update node_ctxt head confirmation_info
