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

type Environment.Error_monad.error += Dal_invalid_page_for_slot of Dal.Page.t

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Permanent
    ~id:"dal_pages_request.dal_invalid_page_for_slot"
    ~title:"Invalid Dal page requested for slot"
    ~description:"The requested Dal page for a given slot is invalid"
    ~pp:(fun ppf ->
      Format.fprintf ppf "Invalid Dal page requested %a" Dal.Page.pp)
    Data_encoding.(obj1 (req "page_id" Dal.Page.encoding))
    (function Dal_invalid_page_for_slot page_id -> Some page_id | _ -> None)
    (fun page_id -> Dal_invalid_page_for_slot page_id)

module Event = struct
  include Internal_event.Simple

  let section = [Protocol.name; "smart_rollup_node"; "dal_pages"]

  let pp_content_elipsis ppf c =
    let len = Bytes.length c in
    if len <= 8 then Hex.pp ppf (Hex.of_bytes c)
    else
      let start = Bytes.sub c 0 4 in
      let end_ = Bytes.sub c (len - 4) 4 in
      Format.fprintf
        ppf
        "%a...%a"
        Hex.pp
        (Hex.of_bytes start)
        Hex.pp
        (Hex.of_bytes end_)

  let page_reveal =
    declare_5
      ~section
      ~name:"dal_page_reveal"
      ~msg:
        "Reveal page {page_index} for slot {slot_index} published at \
         {published_level} for inbox level {inbox_level}: {content}"
      ~level:Debug
      ("slot_index", Data_encoding.int31)
      ("page_index", Data_encoding.int31)
      ("published_level", Data_encoding.int32)
      ("inbox_level", Data_encoding.int32)
      ("content", Data_encoding.bytes)
      ~pp5:pp_content_elipsis
end

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
  fun dal_cctxt slot_id ->
    Pages_cache.bind_or_put
      pages_cache
      slot_id
      (Dal_node_client.get_slot_pages dal_cctxt)
      Lwt.return

let download_confirmed_slot_pages {Node_context.dal_cctxt; _} ~published_level
    ~index =
  let dal_cctxt = WithExceptions.Option.get ~loc:__LOC__ dal_cctxt in
  (* DAL must be configured for this point to be reached *)
  get_slot_pages
    dal_cctxt
    {slot_level = Raw_level.to_int32 published_level; slot_index = index}

module Slots_statuses_cache =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
    (struct
      type t = Dal.Slot.Header.id

      let equal = Dal.Slot.Header.slot_id_equal

      let hash id = Hashtbl.hash id
    end)

let download_skip_list_cells_of_level node_ctxt ~attested_level =
  Plugin.RPC.Dal.skip_list_cells_of_level
    (new Protocol_client_context.wrap_full node_ctxt.Node_context.cctxt)
    (`Main, `Level attested_level)
    ()

(* We have currently 32 slots per level. We retain the information for 32 levels
   (assuming no reorgs with different payload occur). *)
let skip_list_cells_content_cache =
  (* Attestation lag * number of slots * 32 retention levels, assuming no
     reorgs. *)
  let attestation_lag = 8 in
  let number_of_slots = 32 in
  let retention_levels = 32 in
  Slots_statuses_cache.create
    (attestation_lag * number_of_slots * retention_levels)

let dal_skip_list_cell_content_of_slot_id node_ctxt
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants) slot_id
    =
  let open Lwt_result_syntax in
  let attested_level =
    Int32.add
      (Raw_level.to_int32 slot_id.Dal.Slot.Header.published_level)
      (Int32.of_int dal_constants.attestation_lag)
  in
  let* attested_level_hash =
    Node_context.hash_of_level node_ctxt attested_level
  in
  match Slots_statuses_cache.find_opt skip_list_cells_content_cache slot_id with
  | Some (cell_content, block_hash)
    when Block_hash.equal attested_level_hash block_hash ->
      return cell_content
  | None | Some _ -> (
      let* hash_with_cells =
        download_skip_list_cells_of_level node_ctxt ~attested_level
      in
      List.iter
        (fun (_hash, cell) ->
          let content = Dal.Slots_history.content cell in
          Slots_statuses_cache.replace
            skip_list_cells_content_cache
            (Dal.Slots_history.content_id content)
            (content, attested_level_hash))
        hash_with_cells ;
      (* The [find] below validates the fact that we fetched the info of
         commitments published at level [slot_id.published_level]. *)
      match
        Slots_statuses_cache.find_opt skip_list_cells_content_cache slot_id
      with
      | Some (cell_content, block_hash)
        when Block_hash.equal attested_level_hash block_hash ->
          return cell_content
      | None ->
          Stdlib.failwith
            (Format.asprintf
               "Unreachable: We expect to find some data for slot %a, but got \
                none"
               Dal.Slot.Header.pp_id
               slot_id)
      | Some (_, got_hash) ->
          Stdlib.failwith
            (Format.asprintf
               "Unreachable: We expect to find some data for slot %a \
                associated to block hash %a, but got data associated to block \
                hash %a"
               Dal.Slot.Header.pp_id
               slot_id
               Block_hash.pp_short
               attested_level_hash
               Block_hash.pp_short
               got_hash))

let slot_attestation_status ?attestation_threshold_percent
    ?restricted_commitments_publishers node_ctxt dal_constants slot_id =
  let open Lwt_result_syntax in
  let* cell_content =
    dal_skip_list_cell_content_of_slot_id node_ctxt dal_constants slot_id
  in
  let commitment_res =
    Dal.Slots_history.is_commitment_attested
      ~attestation_threshold_percent
      ~restricted_commitments_publishers
      cell_content
  in
  return @@ if Option.is_some commitment_res then `Attested else `Unattested

let get_page node_ctxt ~inbox_level page_id =
  let open Environment.Error_monad.Lwt_result_syntax in
  let Dal.Page.{slot_id; page_index} = page_id in
  let Dal.Slot.Header.{published_level; index} = slot_id in
  let index = Sc_rollup_proto_types.Dal.Slot_index.to_octez index in
  let* pages =
    download_confirmed_slot_pages node_ctxt ~published_level ~index
  in
  let*! res =
    match List.nth_opt pages page_index with
    | None -> tzfail @@ Dal_invalid_page_for_slot page_id
    | Some page ->
        let*! () =
          Event.(emit page_reveal)
            ( index,
              page_index,
              Raw_level.to_int32 published_level,
              inbox_level,
              page )
        in
        return (Some page)
  in
  Lwt.return @@ Environment.wrap_tzresult res

let shadownet = Chain_id.of_b58check_exn "NetXsqzbfFenSTS"

let slot_id_is_valid chain_id
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~origination_level ~inbox_level slot_id
    ~dal_attested_slots_validity_lag =
  let open Alpha_context in
  let dal_activation_level =
    if Chain_id.(chain_id = shadownet) then Some (Raw_level.of_int32_exn 1l)
    else dal_activation_level
  in
  Result.is_ok
    (Dal.Slot_index.check_is_in_range
       ~number_of_slots:dal_constants.number_of_slots
       slot_id.Dal.index)
  &&
  let origination_level_res = Raw_level.of_int32 origination_level in
  let commit_inbox_level_res = Raw_level.of_int32 inbox_level in
  match (origination_level_res, commit_inbox_level_res) with
  | Ok origination_level, Ok commit_inbox_level ->
      Alpha_context.Sc_rollup.Proof.Dal_helpers.import_level_is_valid
        ~dal_activation_level
        ~dal_attestation_lag:dal_constants.attestation_lag
        ~origination_level
        ~commit_inbox_level
        ~dal_attested_slots_validity_lag
        ~published_level:slot_id.published_level
  | _ -> false

let page_id_is_valid chain_id
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~origination_level ~inbox_level
    Dal.Page.{slot_id; page_index} ~dal_attested_slots_validity_lag =
  Result.is_ok
    (Dal.Page.Index.check_is_in_range
       ~number_of_pages:
         (Dal.Page.pages_per_slot dal_constants.cryptobox_parameters)
       page_index)
  && slot_id_is_valid
       chain_id
       dal_constants
       ~dal_activation_level
       ~origination_level
       ~inbox_level
       slot_id
       ~dal_attested_slots_validity_lag

let slot_pages
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~inbox_level node_ctxt slot_id
    ~dal_attested_slots_validity_lag =
  let open Lwt_result_syntax in
  let Node_context.{genesis_info = {level = origination_level; _}; l1_ctxt; _} =
    node_ctxt
  in
  let* chain_id = Layer1.get_chain_id l1_ctxt in
  let Dal.Slot.Header.{published_level; index} = slot_id in
  if
    not
    @@ slot_id_is_valid
         chain_id
         dal_constants
         ~dal_activation_level
         ~origination_level
         ~inbox_level
         ~dal_attested_slots_validity_lag
         slot_id
  then return_none
  else
    let* status = slot_attestation_status node_ctxt dal_constants slot_id in
    match status with
    | `Attested ->
        let index = Sc_rollup_proto_types.Dal.Slot_index.to_octez index in
        let* pages =
          download_confirmed_slot_pages node_ctxt ~published_level ~index
        in
        return (Some pages)
    | `Unattested -> return_none

let page_content
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~inbox_level node_ctxt ~attestation_threshold_percent
    ~restricted_commitments_publishers page_id ~dal_attested_slots_validity_lag
    =
  let open Lwt_result_syntax in
  let Node_context.{genesis_info = {level = origination_level; _}; l1_ctxt; _} =
    node_ctxt
  in
  let* chain_id = Layer1.get_chain_id l1_ctxt in
  if
    not
    @@ page_id_is_valid
         chain_id
         dal_constants
         ~dal_activation_level
         ~origination_level
         ~inbox_level
         ~dal_attested_slots_validity_lag
         page_id
  then return_none
  else
    let* status =
      slot_attestation_status
        ?attestation_threshold_percent
        ?restricted_commitments_publishers
        node_ctxt
        dal_constants
        page_id.Dal.Page.slot_id
    in
    match status with
    | `Attested -> get_page node_ctxt ~inbox_level page_id
    | `Unattested -> return_none
