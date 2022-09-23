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
    Layer1.(Head {level; hash = head_hash}) =
  let open Lwt_result_syntax in
  let* res = get_slot_subscriptions cctxt (head_hash, level) rollup_address in
  let*! () = Store.Dal_slot_subscriptions.add store head_hash res in
  return_unit

let rec predecessor_hash ~levels node_ctxt (Layer1.Head {hash; level} as head) =
  let open Lwt_option_syntax in
  let genesis_level = node_ctxt.Node_context.genesis_info.level in
  if level < Raw_level.to_int32 genesis_level then fail
  else if levels = 0 then return hash
  else
    let*! pred_hash = Layer1.predecessor node_ctxt.store head in
    let pred_head =
      Layer1.Head {hash = pred_hash; level = Int32.(pred level)}
    in
    predecessor_hash ~levels:(levels - 1) node_ctxt pred_head

(* Intersect two sorted lists. *)
module Slot_set = Set.Make (Dal.Slot_index)

let common_slot_indexes (l : Dal.Slot_index.t list) (r : Dal.Slot_index.t list)
    =
  let l_set = Slot_set.of_list l in
  let r_set = Slot_set.of_list r in
  Slot_set.elements @@ Slot_set.inter l_set r_set

(** [confirmed_slots node_ctxt head] reads the slot indexes that have been
    declared available from [head]'s block receipt, and returns the list of
    confirmed slots to which the rollup is subscribed to, with the corresponding
    slot header. *)
let confirmed_slots node_ctxt (Layer1.Head {hash; _} as head) =
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
  let*! published_slots_block_hash =
    predecessor_hash ~levels:lag node_ctxt head
  in
  match published_slots_block_hash with
  | None -> return []
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
      let*! subscribed_slots_indexes =
        Store.Dal_slot_subscriptions.get node_ctxt.store published_block_hash
      in
      let*! published_slots_indexes =
        Store.Dal_slots.list_secondary_keys
          node_ctxt.store
          ~primary_key:published_block_hash
      in
      (* The list of slot_indexes l and r are guaranteed to be sorted.
         List l is sorted because the protocol computes it from a bitset,
         scanning the values 1 by 1.
         List r is sorted because of the internal logic of list_inner_keys. *)
      let relevant_slots_indexes =
        common_slot_indexes subscribed_slots_indexes published_slots_indexes
        |> List.filter (Dal.Endorsement.is_available confirmed_slots)
      in
      let*! () =
        List.iter_s
          (fun s ->
            Dal_slots_tracker_event.slot_has_been_confirmed
              s
              published_block_hash
              hash)
          relevant_slots_indexes
      in
      let*! confirmed_slot_indexes_with_header =
        List.map_p
          (fun slot_index ->
            Store.Dal_slots.get
              node_ctxt.store
              ~primary_key:published_block_hash
              ~secondary_key:slot_index)
          relevant_slots_indexes
      in

      return confirmed_slot_indexes_with_header

let save_confirmed_slot_headers node_ctxt hash slots_to_save =
  List.iter_s
    (fun (Dal.Slot.{id = {index; _}; _} as slot) ->
      Store.Dal_confirmed_slots.add
        node_ctxt.Node_context.store
        ~primary_key:hash
        ~secondary_key:index
        slot)
    slots_to_save
  >|= ok

module Confirmed_slots_history = struct
  let read_slots_history_from_l1 {Node_context.l1_ctxt = {cctxt; _}; _} block =
    let open Lwt_result_syntax in
    (* We return the empty Slots_history if DAL is not enabled. *)
    let* slots_list_opt =
      RPC.Dal.dal_confirmed_slots_history cctxt (cctxt#chain, `Hash (block, 0))
    in
    return @@ Option.value slots_list_opt ~default:Dal.Slots_history.genesis

  let slots_history_of_hash node_ctxt block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! confirmed_slots_history_opt =
      Store.Dal_confirmed_slots_history.find node_ctxt.store block_hash
    in
    match confirmed_slots_history_opt with
    | Some confirmed_dal_slots -> return confirmed_dal_slots
    | None ->
        let*! block_level = Layer1.level_of_hash node_ctxt.store block_hash in
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

  let slots_history_cache_of_hash node_ctxt block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! confirmed_slots_history_cache_opt =
      Store.Dal_confirmed_slots_histories.find node_ctxt.store block_hash
    in
    match confirmed_slots_history_cache_opt with
    | Some cache -> return cache
    | None ->
        let*! block_level = Layer1.level_of_hash node_ctxt.store block_hash in
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

  let update (Node_context.{store; _} as node_ctxt)
      Layer1.(Head {hash = head_hash; _} as head) slots_to_save =
    let open Lwt_result_syntax in
    let slots_to_save =
      let open Dal in
      List.fast_sort
        (fun Slot.{id = {index = a; _}; _} {id = {index = b; _}; _} ->
          Slot_index.compare a b)
        slots_to_save
    in
    let*! pred = Layer1.predecessor store head in
    let* slots_history = slots_history_of_hash node_ctxt pred in
    let* slots_cache = slots_history_cache_of_hash node_ctxt pred in
    let*? slots_history, slots_cache =
      Dal.Slots_history.add_confirmed_slots
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

let process_head node_ctxt (Layer1.Head {hash = head_hash; _} as head) =
  let open Lwt_result_syntax in
  let* () = fetch_and_save_subscribed_slot_headers node_ctxt head in
  let* slots_to_save = confirmed_slots node_ctxt head in
  let* () = save_confirmed_slot_headers node_ctxt head_hash slots_to_save in
  Confirmed_slots_history.update node_ctxt head slots_to_save
