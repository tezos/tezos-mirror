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

(* [confirmed_slots node_ctxt head] reads the slot indexes that have been
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

let compute_and_save_confirmed_slot_headers node_ctxt
    (Layer1.Head {hash; _} as head) =
  let open Lwt_result_syntax in
  let* slots_to_save = confirmed_slots node_ctxt head in
  let*! () =
    List.iter_s
      (fun ({Dal.Slot.index; _} as slot) ->
        Store.Dal_confirmed_slots.add
          node_ctxt.store
          ~primary_key:hash
          ~secondary_key:index
          slot)
      slots_to_save
  in
  return_unit

let process_head node_ctxt head =
  let open Lwt_result_syntax in
  let* () = fetch_and_save_subscribed_slot_headers node_ctxt head in
  compute_and_save_confirmed_slot_headers node_ctxt head

let get_slots_history_of_hash = X.State.slots_history_of_hash
