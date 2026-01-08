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
  | Dal_attestation_status_not_final of {
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
    ~id:"dal_pages_request.attestation_status_not_final"
    ~title:"DAL attestation status is not final"
    ~description:
      "The DAL attestation status for the requested slot is not final yet. We \
       cannot decide whether it is legitimate for the rollup node to import \
       it."
    ~pp:(fun ppf (published_level, slot_index) ->
      Format.fprintf
        ppf
        "DAL attestation status not final for slot %d published at level %ld. \
         We cannot decide yet if it is legitimate for the rollup node to \
         import it."
        slot_index
        published_level)
    Data_encoding.(
      obj2
        (req "published_level" Data_encoding.int32)
        (req "slot_index" Data_encoding.uint8))
    (function
      | Dal_attestation_status_not_final {published_level; slot_index} ->
          Some (published_level, slot_index)
      | _ -> None)
    (fun (published_level, slot_index) ->
      Dal_attestation_status_not_final {published_level; slot_index})

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

  let final_slot_header_attestation_status_failure =
    declare_4
      ~section
      ~name:"final_slot_header_attestation_status_failure"
      ~msg:
        "Failed to get a final status for slot at index {slot_index} published \
         at level {published_level}. Reason: {reason}, {allowed_retries_count} \
         retries left."
      ~level:Warning
      ("published_level", Data_encoding.int32)
      ("slot_index", Data_encoding.int31)
      ("allowed_retries_count", Data_encoding.int31)
      ("reason", Data_encoding.string)
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

(* Adaptive DAL is not supported anymore *)
let get_slot_header_attestation_info =
  let open Lwt_result_syntax in
  (* The size was chosen arbitrarily, but it should be sufficient for Etherlink,
     given that slots statuses are pulled and processed in some order and only
     once. In general a kernel may request data it previously processed during
     normal operation (i.e., not for refutation games). This size can then be
     adapted as needed. *)
  let cache = Slot_id_cache.create 16 in
  fun dal_cctxt ~published_level ~index ~dal_slot_status_max_fetch_attempts ->
    let published_level = Raw_level.to_int32 published_level in
    let index = Dal.Slot_index.to_int index in
    let slot_id = Slot_id.{slot_level = published_level; slot_index = index} in
    let may_retry n f res ~reason =
      let*! () =
        Event.(emit final_slot_header_attestation_status_failure)
          (published_level, index, n, reason)
      in
      if n <= 0 then Lwt.return res else f (n - 1)
    in
    let rec aux allowed_retries_count =
      match Slot_id_cache.find_opt cache slot_id with
      | Some status -> return status
      | None -> (
          let*! res = Dal_node_client.get_slot_status dal_cctxt slot_id in
          match res with
          | Ok ((`Attested _ | `Unattested | `Unpublished) as final_status) ->
              Slot_id_cache.replace cache slot_id final_status ;
              Lwt.return res
          | Ok `Waiting_attestation ->
              may_retry
                allowed_retries_count
                aux
                res
                ~reason:"waiting for attestation"
          | Error err ->
              may_retry
                allowed_retries_count
                aux
                res
                ~reason:(Format.asprintf "%a" Error_monad.pp_print_trace err))
    in
    aux dal_slot_status_max_fetch_attempts

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
  let commit_inbox_level_res = Raw_level.of_int32 inbox_level in
  match (origination_level_res, commit_inbox_level_res) with
  | Ok origination_level, Ok commit_inbox_level ->
      Alpha_context.Sc_rollup.Proof.Dal_helpers.import_level_is_valid
        ~dal_activation_level
        ~dal_attestation_lag
        ~origination_level
        ~commit_inbox_level
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

let attestation_status_not_final published_level slot_index =
  let open Environment.Error_monad.Lwt_result_syntax in
  let*! res =
    tzfail
    @@ Dal_attestation_status_not_final
         {
           published_level = Raw_level.to_int32 published_level;
           slot_index = Sc_rollup_proto_types.Dal.Slot_index.to_octez slot_index;
         }
  in
  Environment.wrap_tzresult res |> Lwt.return

let get_dal_node cctxt_opt =
  let open Environment.Error_monad.Lwt_result_syntax in
  match cctxt_opt with
  | Some res -> return res
  | None ->
      let*! err = tzfail No_dal_node_provided in
      Environment.wrap_tzresult err |> Lwt.return

let page_content_int
    (dal_constants : Octez_smart_rollup.Rollup_constants.dal_constants)
    ~dal_activation_level ~inbox_level node_ctxt page_id
    ~dal_attested_slots_validity_lag =
  let open Lwt_result_syntax in
  let Node_context.{genesis_info = {level = origination_level; _}; l1_ctxt; _} =
    node_ctxt
  in
  let* chain_id = Layer1.get_chain_id l1_ctxt in
  let* dal_cctxt = get_dal_node node_ctxt.dal_cctxt in
  let Dal.Slot.Header.{published_level; index} = page_id.Dal.Page.slot_id in
  let dal_slot_status_max_fetch_attempts =
    node_ctxt.config.dal_slot_status_max_fetch_attempts
  in

  let* status =
    get_slot_header_attestation_info
      dal_cctxt
      ~published_level
      ~index
      ~dal_slot_status_max_fetch_attempts
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
  | `Waiting_attestation -> attestation_status_not_final published_level index

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
