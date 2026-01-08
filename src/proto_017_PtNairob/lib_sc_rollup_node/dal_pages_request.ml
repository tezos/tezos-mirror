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

(** If a slot, published at some level L, is expected to be confirmed at level
    L+D then, once the confirmation level is over, the rollup node is supposed to:
    - Download and save the content of the slot's pages in the store, if the slot
      is confirmed;
    - Add entries [None] for the slot's pages in the store, if the slot
      is not confirmed. *)

type error +=
  | Dal_slot_not_found_in_store of Dal.Slot.Header.id
  | Dal_invalid_page_for_slot of Dal.Page.t

let () =
  Sc_rollup_node_errors.register_error_kind
    `Permanent
    ~id:"dal_pages_request.dal_slot_not_found_in_store"
    ~title:"Dal slot not found in store"
    ~description:"The Dal slot whose ID is given is not found in the store"
    ~pp:(fun ppf ->
      Format.fprintf ppf "Dal slot not found in store %a" Dal.Slot.Header.pp_id)
    Data_encoding.(obj1 (req "slot_id" Dal.Slot.Header.id_encoding))
    (function Dal_slot_not_found_in_store slot_id -> Some slot_id | _ -> None)
    (fun slot_id -> Dal_slot_not_found_in_store slot_id) ;
  Sc_rollup_node_errors.register_error_kind
    `Permanent
    ~id:"dal_pages_request.dal_invalid_page_for_slot"
    ~title:"Invalid Dal page requested for slot"
    ~description:"The requested Dal page for a given slot is invalid"
    ~pp:(fun ppf ->
      Format.fprintf ppf "Invalid Dal page requested %a" Dal.Page.pp)
    Data_encoding.(obj1 (req "page_id" Dal.Page.encoding))
    (function Dal_invalid_page_for_slot page_id -> Some page_id | _ -> None)
    (fun page_id -> Dal_invalid_page_for_slot page_id)

let store_entry_from_published_level ~dal_attestation_lag ~published_level
    node_ctxt =
  Node_context.hash_of_level node_ctxt
  @@ Int32.(
       add (of_int dal_attestation_lag) (Raw_level.to_int32 published_level))

module Slot_id = struct
  include Tezos_dal_node_services.Types.Slot_id

  let equal id1 id2 = Comparable.compare id1 id2 = 0

  let hash id = Hashtbl.hash id
end

(* The cache allows to not fetch pages on the DAL node more than necessary. *)
module Pages_cache =
  Aches_lwt.Lache.Make (Aches.Rache.Transfer (Aches.Rache.LRU) (Slot_id))

let get_slot_pages =
  let pages_cache =
    Pages_cache.create 16
    (* 130MB *)
  in
  fun dal_cctxt commitment ->
    Pages_cache.bind_or_put
      pages_cache
      commitment
      (Dal_node_client.get_slot_pages dal_cctxt)
      Lwt.return

let download_confirmed_slot_pages {Node_context.dal_cctxt; _} ~published_level
    ~index =
  let dal_cctxt = WithExceptions.Option.get ~loc:__LOC__ dal_cctxt in
  (* DAL must be configured for this point to be reached *)
  get_slot_pages
    dal_cctxt
    {slot_level = Raw_level.to_int32 published_level; slot_index = index}

let storage_invariant_broken published_level index =
  failwith
    "Internal error: [Node_context.find_slot_status] is supposed to have \
     registered the status of the slot %d published at level %a in the store"
    index
    Raw_level.pp
    published_level

let slot_pages ~dal_attestation_lag node_ctxt
    Dal.Slot.Header.{published_level; index} =
  let open Lwt_result_syntax in
  let* confirmed_in_block_hash =
    store_entry_from_published_level
      ~dal_attestation_lag
      ~published_level
      node_ctxt
  in
  let index = Dal.Slot_index.to_int index in
  let* processed =
    Node_context.find_slot_status node_ctxt ~confirmed_in_block_hash index
  in
  match processed with
  | Some `Confirmed ->
      let* pages =
        download_confirmed_slot_pages node_ctxt ~published_level ~index
      in
      return (Some pages)
  | Some `Unconfirmed -> return None
  | None -> storage_invariant_broken published_level index

let page_content ~dal_attestation_lag node_ctxt page_id =
  let open Lwt_result_syntax in
  let Dal.Page.{slot_id; page_index} = page_id in
  let Dal.Slot.Header.{published_level; index} = slot_id in
  let* confirmed_in_block_hash =
    store_entry_from_published_level
      ~dal_attestation_lag
      ~published_level
      node_ctxt
  in
  let index = Dal.Slot_index.to_int index in
  let* processed =
    Node_context.find_slot_status node_ctxt ~confirmed_in_block_hash index
  in
  match processed with
  | Some `Confirmed -> (
      let* pages =
        download_confirmed_slot_pages node_ctxt ~published_level ~index
      in
      match List.nth_opt pages page_index with
      | Some page -> return @@ Some page
      | None -> tzfail @@ Dal_invalid_page_for_slot page_id)
  | Some `Unconfirmed -> return None
  | None -> storage_invariant_broken published_level index
