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
module Slot_id = Tezos_dal_node_services.Types.Slot_id

(** If a slot, published at some level L, is expected to be confirmed at level
    L+D then, once the confirmation level is over, the rollup node is supposed to:
    - Download and save the content of the slot's pages in the store, if the slot
      is confirmed;
    - Add entries [None] for the slot's pages in the store, if the slot
      is not confirmed. *)

type Environment.Error_monad.error +=
  | No_dal_node_provided
  | Dal_invalid_page_for_slot of Dal.Page.t
  | Dal_attestation_status_not_found of {
      published_level : int32;
      slot_index : int;
    }

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

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Temporary
    ~id:"dal_pages_request.attestation_status_not_found"
    ~title:"DAL attestation status is not found"
    ~description:
      "The DAL attestation status for the requested slot is not found."
    ~pp:(fun ppf (published_level, slot_index) ->
      Format.fprintf
        ppf
        "DAL attestation status not found for slot %d published at level %ld."
        slot_index
        published_level)
    Data_encoding.(
      obj2
        (req "published_level" Data_encoding.int32)
        (req "slot_index" Data_encoding.uint8))
    (function
      | Dal_attestation_status_not_found {published_level; slot_index} ->
          Some (published_level, slot_index)
      | _ -> None)
    (fun (published_level, slot_index) ->
      Dal_attestation_status_not_found {published_level; slot_index})

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Permanent
    ~id:"dal_pages_request.no_dal_node_provided"
    ~title:"No DAL node provided"
    ~description:"No DAL node was provided"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "No DAL node provided to fetch slot pages.")
    Data_encoding.unit
    (function No_dal_node_provided -> Some () | _ -> None)
    (fun () -> No_dal_node_provided)

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

  let pages_reveal_failed =
    declare_4
      ~section
      ~name:"dal_pages_reveal_failed"
      ~msg:
        "Fetching pages at inbox level {inbox_level} of slot index \
         {slot_index} published at level {published_level} failed with {error}"
      ~level:Error
      ("inbox_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("published_level", Data_encoding.int32)
      ~pp4:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)
end

module Slot_id_cache =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong) (Slot_id)

let download_confirmed_slot_pages =
  let open Lwt_result_syntax in
  (* The size below was chosen arbitrarily, but it should be sufficient for
     Etherlink, given that slots are pulled and processed in some order and only
     once. In general a kernel may request data it previously processed during
     normal operation (i.e., not for refutation games). This size can then be
     adapted as needed. *)
  let cache = Slot_id_cache.create 16 in
  fun dal_cctxt ~published_level ~index ->
    let slot_id =
      Slot_id.
        {slot_level = Raw_level.to_int32 published_level; slot_index = index}
    in
    match Slot_id_cache.find_opt cache slot_id with
    | Some pages -> return pages
    | None ->
        let+ res = Dal_node_client.get_slot_pages dal_cctxt slot_id in
        Slot_id_cache.replace cache slot_id res ;
        res

let slot_statuses_cache :
    ([`Attested of int | `Unattested | `Unpublished] * int32 * Block_hash.t)
    Slot_id_cache.t =
  let number_of_slots = 32 in
  let max_number_of_dynamic_lags = 5 in
  let tb_finality = 2 in
  Slot_id_cache.create
  @@ (number_of_slots * max_number_of_dynamic_lags * tb_finality)

module SI = Set.Make (Int)

(* This function returns the (final) attestation status of [slot_id] (published
   level, index) as seen from L1 at the inbox level identified by
   [inbox_level_block_hash].

   We deliberately do not rely on DAL nodes here: DAL nodes typically update their
   context only after blocks finalization, while rollups advance optimistically
   and may need this information earlier. Instead, we read the DAL skip-list
   cells from L1 ([Dal.skip_list_cells_of_level] at `Hash
   (inbox_level_block_hash, 0)), and derive the slot status from those cells.

   The function maintains a small in-memory cache keyed by
   (inbox_level_block_hash, slot_id) to avoid refetching/decoding the same
   skip-list data multiple times within a run. The use of the inbox_level's
   block hash protects from the use of wrong/reverted statuses in case of L1
   reorg.

   Note 1: this limitation only affects fetching header/attestation
   metadata. Fetching the *slot contents* can still rely on a DAL node when the
   data is already stored, even if the corresponding header attestation status
   is not (yet) available in the DAL node context.

   Note 2: Adaptive DAL is not supported. We check attestation status as
   declared by the L1 protocol. *)
let get_slot_header_attestation_info (Node_context.{cctxt; _} as node_ctxt)
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants) slot_id
    =
  let open Lwt_result_syntax in
  let find_opt () =
    match Slot_id_cache.find_opt slot_statuses_cache slot_id with
    | Some (status, indexed_block_level, indexed_block_hash) ->
        let* current_block_hash =
          Node_context.hash_of_level node_ctxt indexed_block_level
        in
        if Block_hash.equal indexed_block_hash current_block_hash then
          return_some status
        else
          (* There was a reorg, we should re-fetch statuses from L1 *)
          return_none
    | None -> return_none
  in
  let* immediate_res = find_opt () in
  match immediate_res with
  | Some res -> return res
  | None -> (
      let init_cache_for_potential_attestation_lag ~lag =
        let potential_attested_level =
          Int32.(add slot_id.slot_level (of_int lag))
        in
        let* potential_attested_level_hash =
          Node_context.hash_of_level node_ctxt potential_attested_level
        in
        let* cells =
          let cctxt = new Protocol_client_context.wrap_full cctxt in
          Plugin.RPC.Dal.skip_list_cells_of_level
            cctxt
            (cctxt#chain, `Hash (potential_attested_level_hash, 0))
            ()
        in
        List.iter
          (fun (_hash, cell) ->
            let content = Dal.Slots_history.content cell in
            let Dal.Slots_history.{header_id = {published_level; index}; _} =
              Dal.Slots_history.content_id content
            in
            let published_level = Raw_level.to_int32 published_level in
            let index = Dal.Slot_index.to_int index in
            let slot_id =
              Slot_id.{slot_level = published_level; slot_index = index}
            in
            let final_status =
              match content with
              | Unpublished _ -> `Unpublished
              | Published {is_proto_attested = false; _} -> `Unattested
              | Published {attestation_lag; _} ->
                  `Attested
                    (Dal.Slots_history.attestation_lag_value attestation_lag)
            in
            Slot_id_cache.replace
              slot_statuses_cache
              slot_id
              ( final_status,
                potential_attested_level,
                potential_attested_level_hash ))
          cells ;
        return_unit
      in
      let attestation_lags =
        (* init with dynamic lags, if enabled *)
        (if dal_constants.dynamic_lag_enable then
           SI.of_list dal_constants.attestation_lags
         else SI.empty)
        |>
        (* Add current lag if not already present *)
        SI.add dal_constants.attestation_lag
        |>
        (* Add legacy lag if not already present *)
        SI.add Dal.Slots_history.legacy_attestation_lag
      in
      let* res =
        List.fold_left_es
          (fun res lag ->
            if Option.is_some res then return res
            else
              let* () = init_cache_for_potential_attestation_lag ~lag in
              find_opt ())
          None
          (SI.elements attestation_lags)
      in
      match res with
      | Some status -> return status
      | None ->
          let open Environment.Error_monad.Lwt_result_syntax in
          let*! res =
            tzfail
            @@ Dal_attestation_status_not_found
                 {
                   published_level = slot_id.slot_level;
                   slot_index = slot_id.slot_index;
                 }
          in
          Environment.wrap_tzresult res |> Lwt.return)

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

let slot_id_is_valid chain_id ~dal_attestation_lag ~number_of_slots
    ~dal_activation_level ~origination_level ~inbox_level slot_id
    ~dal_attested_slots_validity_lag =
  let open Alpha_context in
  let dal_activation_level =
    if Chain_id.(chain_id = Constants_repr.shadownet_id) then
      (* Tezos L1 Shadownet was created with a DAL activation level hard-coded
         as 5,726,209, which is the value for the Mainnet network. It should
         have been smaller, but it means strictly speaking it is not possible
         for a rollup on Shadownet to use DAL. We overwrite this value to
         overcome this mistake, and to let kernel developers experiment with
         DAL on Shadownet. *)
      Some (Raw_level.of_int32_exn 1l)
    else dal_activation_level
  in
  Result.is_ok
    (Dal.Slot_index.check_is_in_range ~number_of_slots slot_id.Dal.index)
  &&
  let origination_level_res = Raw_level.of_int32 origination_level in
  let import_inbox_level_res = Raw_level.of_int32 inbox_level in
  match (origination_level_res, import_inbox_level_res) with
  | Ok origination_level, Ok import_inbox_level ->
      Alpha_context.Sc_rollup.Proof.Dal_helpers.import_level_is_valid
        ~dal_activation_level
        ~dal_attestation_lag
        ~origination_level
        ~import_inbox_level
        ~dal_attested_slots_validity_lag
        ~published_level:slot_id.published_level
  | _ -> false

let page_id_is_valid chain_id ~dal_attestation_lag ~number_of_slots
    ~number_of_pages ~dal_activation_level ~origination_level ~inbox_level
    Dal.Page.{slot_id; page_index} ~dal_attested_slots_validity_lag =
  Result.is_ok (Dal.Page.Index.check_is_in_range ~number_of_pages page_index)
  && slot_id_is_valid
       chain_id
       ~dal_attestation_lag
       ~number_of_slots
       ~dal_activation_level
       ~origination_level
       ~inbox_level
       slot_id
       ~dal_attested_slots_validity_lag

let get_dal_node cctxt_opt =
  let open Environment.Error_monad.Lwt_result_syntax in
  match cctxt_opt with
  | Some res -> return res
  | None ->
      let*! err = tzfail No_dal_node_provided in
      Environment.wrap_tzresult err |> Lwt.return

(* Check whether [page_id] could be valid for at least one admissible DAL
   attestation lag.

   This function mirrors the pre-check done in DAL refutations'
   [produce_proof_repr]. At this point we do not know the exact DAL
   attestation lag, so we over-approximate the admissible range by the whole
   interval [0, legacy_attestation_lag] and reuse [page_id_is_valid] on both
   extremal values:

     - If [page_id_is_valid] is [false] for both lags (0 and
       [legacy_attestation_lag]), then [page_id] is definitely invalid for any
       admissible lag. In that case, any import of the corresponding slot/page
       is known to be invalid and we can reject it immediately, without ever
       querying the DAL node.

     - Otherwise, there exists at least one admissible lag for which
       [page_id] may still be valid, and the DAL node must be consulted to
       decide.

   This early check protects the rollup node from pathological situations
   where the DAL node cannot answer about a slot/page whose published level is
   very far in the past or future: when the result is [false], we already know
   that no import could be valid.  *)
let may_be_valid_in_our_range_of_lags chain_id ~number_of_slots ~number_of_pages
    ~dal_activation_level ~origination_level ~inbox_level page_id
    ~dal_attested_slots_validity_lag =
  let page_id_is_valid =
    page_id_is_valid
      chain_id
      ~number_of_slots
      ~number_of_pages
      ~dal_activation_level
      ~origination_level
      ~inbox_level
      ~dal_attested_slots_validity_lag
      page_id
  in
  (* It is important to check that both [dal_attestation_lag = 0] and
     [dal_attestation_lag = max_dal_attestation_lag] below in order
     to be sure that the page id is not invalid.

     In addition to the checks that do not depend on the lag value:

     - The test with [dal_attestation_lag = 0] checks that the import level is
       not before the slot's published level (however, it does not guarantee
       that enough blocks have elapsed since publication).

     - The test with [dal_attestation_lag = max_dal_attestation_lag] checks that
       the import level is not beyond the [dal_attested_slots_validity_lag]
       validity window.

     Overall, this function returns an over-approximation: if it returns [true]
     only because the import level is close to the validity boundaries, a more
     precise check will be performed later using the exact attestation lag. The
     exact lag will be fetched from DAL (either already indexed, or indexed
     within at most [max_dal_attestation_lag] L1 blocks). *)
  let max_dal_attestation_lag = Dal.Slots_history.legacy_attestation_lag in
  page_id_is_valid ~dal_attestation_lag:0
  || page_id_is_valid ~dal_attestation_lag:max_dal_attestation_lag

let page_content_int
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~inbox_level node_ctxt page_id
    ~dal_attested_slots_validity_lag =
  let open Lwt_result_syntax in
  let Node_context.{genesis_info = {level = origination_level; _}; l1_ctxt; _} =
    node_ctxt
  in
  let* chain_id = Layer1.get_chain_id l1_ctxt in
  let may_be_valid_id =
    may_be_valid_in_our_range_of_lags
      chain_id
      ~number_of_slots:dal_constants.number_of_slots
      ~number_of_pages:
        (Dal.Page.pages_per_slot dal_constants.cryptobox_parameters)
      ~dal_activation_level
      ~origination_level
      ~inbox_level
      page_id
      ~dal_attested_slots_validity_lag
  in
  if not may_be_valid_id then return_none
  else
    let* dal_cctxt = get_dal_node node_ctxt.dal_cctxt in
    let Dal.Slot.Header.{published_level; index} = page_id.Dal.Page.slot_id in
    let published_level = Raw_level.to_int32 published_level in
    let index = Dal.Slot_index.to_int index in
    let slot_id = Slot_id.{slot_level = published_level; slot_index = index} in

    let* status =
      get_slot_header_attestation_info node_ctxt dal_constants slot_id
    in
    match status with
    | `Attested attestation_lag ->
        if
          not
          @@ page_id_is_valid
               chain_id
               ~dal_attestation_lag:attestation_lag
               ~number_of_slots:dal_constants.number_of_slots
               ~number_of_pages:
                 (Dal.Page.pages_per_slot dal_constants.cryptobox_parameters)
               ~dal_activation_level
               ~origination_level
               ~inbox_level
               ~dal_attested_slots_validity_lag
               page_id
        then return_none
        else get_page dal_cctxt ~inbox_level page_id
    | `Unattested | `Unpublished -> return_none

let with_errors_logging ~inbox_level slot_id f =
  let open Lwt_syntax in
  let* res = f in
  let+ () =
    match res with
    | Ok _ -> return_unit
    | Error err ->
        Event.(emit pages_reveal_failed)
          ( inbox_level,
            Dal.(Slot_index.to_int slot_id.index),
            Raw_level.to_int32 slot_id.published_level,
            err )
  in
  res

let page_content
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~inbox_level node_ctxt page_id
    ~dal_attested_slots_validity_lag =
  with_errors_logging ~inbox_level page_id.Dal.Page.slot_id
  @@ page_content_int
       dal_constants
       ~dal_activation_level
       ~inbox_level
       node_ctxt
       page_id
       ~dal_attested_slots_validity_lag
