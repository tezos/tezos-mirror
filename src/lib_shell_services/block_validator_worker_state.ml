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
  type validation_view = {
    chain_id : Chain_id.t;
    block : Block_hash.t;
    peer : P2p_peer.Id.t option;
  }

  let validation_view_encoding =
    let open Data_encoding in
    conv
      (fun {block; chain_id; peer} -> (block, chain_id, peer))
      (fun (block, chain_id, peer) -> {block; chain_id; peer})
      (obj3
         (req "block" Block_hash.encoding)
         (req "chain_id" Chain_id.encoding)
         (opt "peer" P2p_peer.Id.encoding))

  type preapplication_view = {chain_id : Chain_id.t; level : int32}

  let preapplication_view_encoding =
    let open Data_encoding in
    conv
      (fun {chain_id; level} -> (chain_id, level))
      (fun (chain_id, level) -> {chain_id; level})
      (obj2 (req "chain_id" Chain_id.encoding) (req "level" int32))

  type view =
    | Validation of validation_view
    | Preapplication of preapplication_view

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"validation"
          (Tag 0)
          validation_view_encoding
          (function
            | Validation {block; chain_id; peer} -> Some {block; chain_id; peer}
            | _ -> None)
          (fun {block; chain_id; peer} -> Validation {block; chain_id; peer});
        case
          ~title:"preapplication"
          (Tag 1)
          preapplication_view_encoding
          (function
            | Preapplication {chain_id; level} -> Some {chain_id; level}
            | _ -> None)
          (fun {chain_id; level} -> Preapplication {chain_id; level});
      ]

  let pp ppf = function
    | Validation {chain_id; block; peer} -> (
        Format.fprintf
          ppf
          "Validation of %a (chain: %a)"
          Block_hash.pp
          block
          Chain_id.pp_short
          chain_id ;
        match peer with
        | None -> ()
        | Some peer ->
            Format.fprintf ppf "from peer %a" P2p_peer.Id.pp_short peer)
    | Preapplication {chain_id; level} ->
        Format.fprintf
          ppf
          "Pre-application at level %ld (chain: %a)"
          level
          Chain_id.pp_short
          chain_id
end

module Event = struct
  type t =
    | Validation_success of
        Request.validation_view * Worker_types.request_status
    | Validation_failure of
        Request.validation_view * Worker_types.request_status * error trace
    | Preapplication_success of
        Request.preapplication_view * Worker_types.request_status
    | Preapplication_failure of
        Request.preapplication_view * Worker_types.request_status * error trace
    | Validation_failure_after_precheck of
        Request.validation_view * Worker_types.request_status * error trace
    | Precheck_failure of
        Request.validation_view * Worker_types.request_status * error trace
    | Could_not_find_context of Block_hash.t
    | Previously_validated of Block_hash.t
    | Validating_block of Block_hash.t
    | Prechecking_block of Block_hash.t
    | Prechecked_block of Block_hash.t

  type view = t

  let view t = t

  let level req =
    match req with
    | Validation_success _ | Validation_failure _ | Preapplication_success _
    | Preapplication_failure _ | Precheck_failure _ ->
        Internal_event.Notice
    | Validation_failure_after_precheck _ | Prechecked_block _ ->
        Internal_event.Info
    | Could_not_find_context _ | Previously_validated _ | Validating_block _
    | Prechecking_block _ ->
        Internal_event.Debug

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"validation_success"
          (obj2
             (req "successful_validation" Request.validation_view_encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Validation_success (r, s) -> Some (r, s) | _ -> None)
          (fun (r, s) -> Validation_success (r, s));
        case
          (Tag 1)
          ~title:"validation_failure"
          (obj3
             (req "failed_validation" Request.validation_view_encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" RPC_error.encoding []))
          (function
            | Validation_failure (r, s, err) -> Some (r, s, err) | _ -> None)
          (fun (r, s, err) -> Validation_failure (r, s, err));
        case
          (Tag 2)
          ~title:"validation_failure_after_precheck"
          (obj3
             (req
                "failed_validation_after_precheck"
                Request.validation_view_encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" RPC_error.encoding []))
          (function
            | Validation_failure_after_precheck (r, s, err) -> Some (r, s, err)
            | _ -> None)
          (fun (r, s, err) -> Validation_failure_after_precheck (r, s, err));
        case
          (Tag 3)
          ~title:"precheck_failure"
          (obj3
             (req "failed_precheck" Request.validation_view_encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" RPC_error.encoding []))
          (function
            | Precheck_failure (r, s, err) -> Some (r, s, err) | _ -> None)
          (fun (r, s, err) -> Precheck_failure (r, s, err));
        case
          (Tag 4)
          ~title:"could_not_find_context"
          (obj1 (req "block" Block_hash.encoding))
          (function Could_not_find_context block -> Some block | _ -> None)
          (fun block -> Could_not_find_context block);
        case
          (Tag 5)
          ~title:"previously_validated"
          (obj1 (req "block" Block_hash.encoding))
          (function Previously_validated block -> Some block | _ -> None)
          (fun block -> Previously_validated block);
        case
          (Tag 6)
          ~title:"validating_block"
          (obj1 (req "block" Block_hash.encoding))
          (function Validating_block block -> Some block | _ -> None)
          (fun block -> Validating_block block);
        case
          (Tag 7)
          ~title:"preapplication_success"
          (obj2
             (req "preapplication_success" Request.preapplication_view_encoding)
             (req "status" Worker_types.request_status_encoding))
          (function Preapplication_success (r, s) -> Some (r, s) | _ -> None)
          (fun (r, s) -> Preapplication_success (r, s));
        case
          (Tag 8)
          ~title:"preapplication_failure"
          (obj3
             (req "preapplication_failure" Request.preapplication_view_encoding)
             (req "status" Worker_types.request_status_encoding)
             (dft "errors" RPC_error.encoding []))
          (function
            | Preapplication_failure (r, s, e) -> Some (r, s, e) | _ -> None)
          (fun (r, s, e) -> Preapplication_failure (r, s, e));
        case
          (Tag 9)
          ~title:"prechecking_block"
          (obj1 (req "block" Block_hash.encoding))
          (function Prechecking_block block -> Some block | _ -> None)
          (fun block -> Prechecking_block block);
        case
          (Tag 10)
          ~title:"prechecked_block"
          (obj2
             (req "kind" Data_encoding.string)
             (req "block" Block_hash.encoding))
          (function
            | Prechecked_block block -> Some ("prechecked", block) | _ -> None)
          (fun (_, block) -> Prechecked_block block);
      ]

  let pp ppf = function
    | Validation_success (req, {pushed; treated; completed}) ->
        Format.fprintf
          ppf
          "@[<v 0>block %a successfully validated@,%a@]"
          Block_hash.pp
          req.block
          Worker_types.pp_status
          {pushed; treated; completed}
    | Validation_failure (req, {pushed; treated; completed}, errs) ->
        Format.fprintf
          ppf
          "@[<v 0>validation of block %a failed@,%a, %a@]"
          Block_hash.pp
          req.block
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errs
    | Preapplication_success (req, {pushed; treated; completed}) ->
        Format.fprintf
          ppf
          "@[<v 0>block at level %ld successfully pre-applied@,%a@]"
          req.level
          Worker_types.pp_status
          {pushed; treated; completed}
    | Preapplication_failure (req, {pushed; treated; completed}, errs) ->
        Format.fprintf
          ppf
          "@[<v 0>pre-application of block at level %ld failed@,%a, %a@]"
          req.level
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errs
    | Validation_failure_after_precheck (req, {pushed; treated; completed}, errs)
      ->
        Format.fprintf
          ppf
          "@[<v 0>validation of block %a failed but precheck succeeded@,\
           %a, %a@]"
          Block_hash.pp
          req.block
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errs
    | Precheck_failure (req, {pushed; treated; completed}, errs) ->
        Format.fprintf
          ppf
          "@[<v 0>precheck of block %a failed@,%a, %a@]"
          Block_hash.pp
          req.block
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errs
    | Could_not_find_context block ->
        Format.fprintf
          ppf
          "could not find context for block %a"
          Block_hash.pp
          block
    | Previously_validated block ->
        Format.fprintf
          ppf
          "previously validated block %a (after pipe)"
          Block_hash.pp
          block
    | Validating_block block ->
        Format.fprintf ppf "validating block %a" Block_hash.pp block
    | Prechecking_block block ->
        Format.fprintf ppf "prechecking block %a" Block_hash.pp block
    | Prechecked_block block ->
        Format.fprintf ppf "prechecked block %a" Block_hash.pp block
end
