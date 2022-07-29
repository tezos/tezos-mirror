(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Request = struct
  type view = New_head of Block_hash.t | New_branch of Block_hash.t * int

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"New_head"
          (obj2
             (req "request" (constant "new_head"))
             (req "block" Block_hash.encoding))
          (function New_head h -> Some ((), h) | _ -> None)
          (fun ((), h) -> New_head h);
        case
          (Tag 1)
          ~title:"New_branch"
          (obj3
             (req "request" (constant "new_branch"))
             (req "block" Block_hash.encoding)
             (req "locators" int31))
          (function New_branch (h, l) -> Some ((), h, l) | _ -> None)
          (fun ((), h, l) -> New_branch (h, l));
      ]

  let pp ppf = function
    | New_head hash -> Format.fprintf ppf "New head %a" Block_hash.pp hash
    | New_branch (hash, len) ->
        Format.fprintf
          ppf
          "New branch %a, locator length %d"
          Block_hash.pp
          hash
          len
end

module Event = struct
  type block_received = {peer : P2p_peer.Id.t; hash : Block_hash.t}

  type t =
    | Request of
        (Request.view * Worker_types.request_status * error list option)
    | Validating_new_branch of {peer : P2p_peer.Id.t; nb_blocks : int}
    | New_branch_validated of block_received
    | Fetching_operations_for_head of block_received
    | Requesting_new_head_validation of block_received
    | New_head_validation_end of block_received
    | Ignoring_head of block_received
    | Ignoring_previously_validated_block of block_received
    | Ignoring_prechecked_block of block_received
    | Ignoring_invalid_block of block_received
    | Missing_new_head_predecessor of block_received
    | Ignoring_branch_with_invalid_locator of block_received
    | Ignoring_branch_without_common_ancestor of block_received
    | No_new_head_from_peer of {peer : P2p_peer.Id.t; timespan : float}
    | Processing_new_head of block_received
    | Processing_new_branch of block_received
    | Terminating_worker of {peer : P2p_peer.Id.t; reason : string}
    | Ignoring_prechecked_invalid_block of block_received

  type view = t

  let view t = t

  let level req =
    match req with
    | Validating_new_branch _ | New_branch_validated _
    | Fetching_operations_for_head _ | Requesting_new_head_validation _
    | New_head_validation_end _ | Ignoring_head _
    | Ignoring_previously_validated_block _ | Ignoring_prechecked_block _
    | Ignoring_invalid_block _ | Missing_new_head_predecessor _
    | Ignoring_branch_with_invalid_locator _
    | Ignoring_branch_without_common_ancestor _ | No_new_head_from_peer _
    | Processing_new_head _ | Processing_new_branch _ | Terminating_worker _
    | Ignoring_prechecked_invalid_block _ ->
        Internal_event.Debug
    | Request (_, _, Some _) -> Internal_event.Notice
    | Request (Request.New_head _, _, None) -> Internal_event.Debug
    | Request (Request.New_branch (_, _), _, None) -> Internal_event.Info

  let block_received_encoding =
    let open Data_encoding in
    obj2 (req "peer" P2p_peer.Id.encoding) (req "block" Block_hash.encoding)

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Request"
          (obj2
             (req "request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Request (req, t, None) -> Some (req, t) | _ -> None)
          (fun (req, t) -> Request (req, t, None));
        case
          (Tag 1)
          ~title:"Failed request"
          (obj3
             (req "error" RPC_error.encoding)
             (req "failed_request" Request.encoding)
             (req "status" Worker_types.request_status_encoding))
          (function
            | Request (req, t, Some errs) -> Some (errs, req, t) | _ -> None)
          (fun (errs, req, t) -> Request (req, t, Some errs));
        case
          (Tag 2)
          ~title:"validating_new_branch"
          (obj2 (req "peer" P2p_peer.Id.encoding) (req "nb_blocks" int31))
          (function
            | Validating_new_branch {peer; nb_blocks} -> Some (peer, nb_blocks)
            | _ -> None)
          (fun (peer, nb_blocks) -> Validating_new_branch {peer; nb_blocks});
        case
          (Tag 3)
          ~title:"new_branch_validated"
          block_received_encoding
          (function
            | New_branch_validated {peer; hash} -> Some (peer, hash) | _ -> None)
          (fun (peer, hash) -> New_branch_validated {peer; hash});
        case
          (Tag 4)
          ~title:"fetching_operations_for_head"
          block_received_encoding
          (function
            | Fetching_operations_for_head {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Fetching_operations_for_head {peer; hash});
        case
          (Tag 5)
          ~title:"Requesting_new_head_validation"
          block_received_encoding
          (function
            | Requesting_new_head_validation {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Requesting_new_head_validation {peer; hash});
        case
          (Tag 6)
          ~title:"new_head_validation_end"
          block_received_encoding
          (function
            | New_head_validation_end {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> New_head_validation_end {peer; hash});
        case
          (Tag 7)
          ~title:"ignoring_head"
          block_received_encoding
          (function
            | Ignoring_head {peer; hash} -> Some (peer, hash) | _ -> None)
          (fun (peer, hash) -> Ignoring_head {peer; hash});
        case
          (Tag 8)
          ~title:"ignoring_previously_validated_block"
          block_received_encoding
          (function
            | Ignoring_previously_validated_block {peer; hash} ->
                Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Ignoring_previously_validated_block {peer; hash});
        case
          (Tag 9)
          ~title:"ignoring_prechecked_block"
          block_received_encoding
          (function
            | Ignoring_prechecked_block {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Ignoring_prechecked_block {peer; hash});
        case
          (Tag 10)
          ~title:"ignoring_invalid_block"
          block_received_encoding
          (function
            | Ignoring_invalid_block {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Ignoring_invalid_block {peer; hash});
        case
          (Tag 11)
          ~title:"missing_new_head_predecessor"
          block_received_encoding
          (function
            | Missing_new_head_predecessor {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Missing_new_head_predecessor {peer; hash});
        case
          (Tag 12)
          ~title:"ignoring_invalid_locator_branch"
          block_received_encoding
          (function
            | Ignoring_branch_with_invalid_locator {peer; hash} ->
                Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) ->
            Ignoring_branch_with_invalid_locator {peer; hash});
        case
          (Tag 13)
          ~title:"ignore_branch_without_common_ancestor"
          block_received_encoding
          (function
            | Ignoring_branch_without_common_ancestor {peer; hash} ->
                Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) ->
            Ignoring_branch_without_common_ancestor {peer; hash});
        case
          (Tag 14)
          ~title:"no_new_head_from_peer"
          (obj2 (req "peer" P2p_peer.Id.encoding) (req "timespan" float))
          (function
            | No_new_head_from_peer {peer; timespan} -> Some (peer, timespan)
            | _ -> None)
          (fun (peer, timespan) -> No_new_head_from_peer {peer; timespan});
        case
          (Tag 15)
          ~title:"processing_new_head"
          block_received_encoding
          (function
            | Processing_new_head {peer; hash} -> Some (peer, hash) | _ -> None)
          (fun (peer, hash) -> Processing_new_head {peer; hash});
        case
          (Tag 16)
          ~title:"processing_new_branch"
          block_received_encoding
          (function
            | Processing_new_branch {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Processing_new_branch {peer; hash});
        case
          (Tag 17)
          ~title:"terminating_worker"
          (obj2 (req "peer" P2p_peer.Id.encoding) (req "reason" string))
          (function
            | Terminating_worker {peer; reason} -> Some (peer, reason)
            | _ -> None)
          (fun (peer, reason) -> Terminating_worker {peer; reason});
        case
          (Tag 18)
          ~title:"ignoring_prechecked_invalid_block"
          block_received_encoding
          (function
            | Ignoring_prechecked_invalid_block {peer; hash} -> Some (peer, hash)
            | _ -> None)
          (fun (peer, hash) -> Ignoring_prechecked_invalid_block {peer; hash});
      ]

  let pp_block_received ppf {peer; hash} =
    Format.fprintf ppf "%a from %a" Block_hash.pp hash P2p_peer.Id.pp peer

  let pp ppf = function
    | Validating_new_branch {peer; nb_blocks} ->
        Format.fprintf
          ppf
          "validating new branch from peer %a (approx. %d blocks)"
          P2p_peer.Id.pp
          peer
          nb_blocks
    | New_branch_validated block_received ->
        Format.fprintf
          ppf
          "new branch %a validated"
          pp_block_received
          block_received
    | Fetching_operations_for_head block_received ->
        Format.fprintf
          ppf
          "fetching operations for head %a"
          pp_block_received
          block_received
    | Requesting_new_head_validation block_received ->
        Format.fprintf
          ppf
          "requesting new head validation %a"
          pp_block_received
          block_received
    | New_head_validation_end block_received ->
        Format.fprintf
          ppf
          "new head validation ended %a"
          pp_block_received
          block_received
    | Ignoring_head block_received ->
        Format.fprintf
          ppf
          "ignoring head with non-increasing fitness %a"
          pp_block_received
          block_received
    | Ignoring_previously_validated_block block_received ->
        Format.fprintf
          ppf
          "ignoring previously validated head %a"
          pp_block_received
          block_received
    | Ignoring_prechecked_block block_received ->
        Format.fprintf
          ppf
          "ignoring prechecked head %a"
          pp_block_received
          block_received
    | Ignoring_invalid_block block_received ->
        Format.fprintf
          ppf
          "ignoring invalid block %a"
          pp_block_received
          block_received
    | Missing_new_head_predecessor block_received ->
        Format.fprintf
          ppf
          "missing new head's predecessor %a"
          pp_block_received
          block_received
    | Ignoring_branch_with_invalid_locator block_received ->
        Format.fprintf
          ppf
          "ignoring branch with invalid locator %a"
          pp_block_received
          block_received
    | Ignoring_branch_without_common_ancestor block_received ->
        Format.fprintf
          ppf
          "ignoring branch without common ancestor %a"
          pp_block_received
          block_received
    | Processing_new_head block_received ->
        Format.fprintf
          ppf
          "processing new head %a"
          pp_block_received
          block_received
    | Processing_new_branch block_received ->
        Format.fprintf
          ppf
          "processing new branch %a"
          pp_block_received
          block_received
    | No_new_head_from_peer {peer; timespan} ->
        Format.fprintf
          ppf
          "no new head from peer %a for %g seconds"
          P2p_peer.Id.pp
          peer
          timespan
    | Terminating_worker {peer; reason} ->
        Format.fprintf
          ppf
          "terminating the validation worker for peer %a (%s)"
          P2p_peer.Id.pp
          peer
          reason
    | Ignoring_prechecked_invalid_block block_received ->
        Format.fprintf
          ppf
          "ignoring invalid block %a"
          pp_block_received
          block_received
    | Request (view, {pushed; treated; completed}, None) ->
        Format.fprintf
          ppf
          "@[<v 0>%a@, %a@]"
          Request.pp
          view
          Worker_types.pp_status
          {pushed; treated; completed}
    | Request (view, {pushed; treated; completed}, Some errors) ->
        Format.fprintf
          ppf
          "@[<v 0>%a@, %a, %a@]"
          Request.pp
          view
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errors
end

type pipeline_length = {fetched_header_length : int; fetched_block_length : int}

let pipeline_length_encoding =
  let open Data_encoding in
  conv
    (function
      | {fetched_header_length; fetched_block_length} ->
          (fetched_header_length, fetched_block_length))
    (function
      | fetched_header_length, fetched_block_length ->
          {fetched_header_length; fetched_block_length})
    (obj2 (req "fetched_headers" int31) (req "fetched_blocks" int31))
