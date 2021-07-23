(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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
  type 'a t =
    | Flush : Block_hash.t * Block_hash.Set.t * Operation_hash.Set.t -> unit t
    | Notify : P2p_peer.Id.t * Mempool.t -> unit t
    | Leftover : unit t
    | Inject : Operation.t -> unit t
    | Arrived : Operation_hash.t * Operation.t -> unit t
    | Advertise : unit t
    | Ban : Operation_hash.t -> unit t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Flush"
          (obj2
             (req "request" (constant "flush"))
             (req "block" Block_hash.encoding))
          (function View (Flush (hash, _, _)) -> Some ((), hash) | _ -> None)
          (fun ((), hash) ->
            View (Flush (hash, Block_hash.Set.empty, Operation_hash.Set.empty)));
        case
          (Tag 1)
          ~title:"Notify"
          (obj3
             (req "request" (constant "notify"))
             (req "peer" P2p_peer.Id.encoding)
             (req "mempool" Mempool.encoding))
          (function
            | View (Notify (peer, mempool)) -> Some ((), peer, mempool)
            | _ -> None)
          (fun ((), peer, mempool) -> View (Notify (peer, mempool)));
        case
          (Tag 2)
          ~title:"Inject"
          (obj2
             (req "request" (constant "inject"))
             (req "operation" Operation.encoding))
          (function View (Inject op) -> Some ((), op) | _ -> None)
          (fun ((), op) -> View (Inject op));
        case
          (Tag 3)
          ~title:"Arrived"
          (obj3
             (req "request" (constant "arrived"))
             (req "operation_hash" Operation_hash.encoding)
             (req "operation" Operation.encoding))
          (function
            | View (Arrived (oph, op)) -> Some ((), oph, op) | _ -> None)
          (fun ((), oph, op) -> View (Arrived (oph, op)));
        case
          (Tag 4)
          ~title:"Advertise"
          (obj1 (req "request" (constant "advertise")))
          (function View Advertise -> Some () | _ -> None)
          (fun () -> View Advertise);
        case
          (Tag 5)
          ~title:"Leftover"
          (obj1 (req "request" (constant "leftover")))
          (function View Leftover -> Some () | _ -> None)
          (fun () -> View Leftover);
        case
          (Tag 6)
          ~title:"Ban"
          (obj2
             (req "request" (constant "ban"))
             (req "operation_hash" Operation_hash.encoding))
          (function View (Ban oph) -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> View (Ban oph));
      ]

  let pp ppf (View r) =
    match r with
    | Flush (hash, _, _) ->
        Format.fprintf ppf "switching to new head %a" Block_hash.pp hash
    | Notify (id, {Mempool.known_valid; pending}) ->
        Format.fprintf
          ppf
          "@[<v 2>notified by %a of operations"
          P2p_peer.Id.pp
          id ;
        List.iter
          (fun oph -> Format.fprintf ppf "@,%a (applied)" Operation_hash.pp oph)
          known_valid ;
        List.iter
          (fun oph -> Format.fprintf ppf "@,%a (pending)" Operation_hash.pp oph)
          (Operation_hash.Set.elements pending) ;
        Format.fprintf ppf "@]"
    | Leftover -> Format.fprintf ppf "process next batch of operation"
    | Inject op ->
        Format.fprintf
          ppf
          "injecting operation %a"
          Operation_hash.pp
          (Operation.hash op)
    | Arrived (oph, _) ->
        Format.fprintf ppf "operation %a arrived" Operation_hash.pp oph
    | Advertise -> Format.fprintf ppf "advertising pending operations"
    | Ban oph -> Format.fprintf ppf "banning operation %a" Operation_hash.pp oph
end

module Operation_encountered = struct
  type situation = Injected | Arrived | Notified of P2p_peer_id.t | Other

  type t = situation * Operation_hash.t

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"injected"
          (obj2
             (req "situation" (constant "injected"))
             (req "operation" Operation_hash.encoding))
          (function (Injected, oph) -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> (Injected, oph));
        case
          (Tag 1)
          ~title:"arrived"
          (obj2
             (req "situation" (constant "arrived"))
             (req "operation" Operation_hash.encoding))
          (function (Arrived, oph) -> Some ((), oph) | _ -> None)
          (fun ((), oph) -> (Arrived, oph));
        case
          (Tag 2)
          ~title:"notified"
          (obj3
             (req "situation" (constant "notified"))
             (req "operation" Operation_hash.encoding)
             (req "peer" P2p_peer_id.encoding))
          (function (Notified peer, oph) -> Some ((), oph, peer) | _ -> None)
          (fun ((), oph, peer) -> (Notified peer, oph));
        case
          (Tag 3)
          ~title:"other"
          (obj3
             (req "situation" (constant "other"))
             (req "operation" Operation_hash.encoding)
             (req "peer" P2p_peer_id.encoding))
          (function (Notified peer, oph) -> Some ((), oph, peer) | _ -> None)
          (fun ((), oph, peer) -> (Notified peer, oph));
      ]

  let situation_pp ppf = function
    | Injected -> Format.fprintf ppf "injected"
    | Arrived -> Format.fprintf ppf "arrived"
    | Notified pid -> Format.fprintf ppf "notified from %a" P2p_peer_id.pp pid
    | Other -> Format.fprintf ppf "encountered"

  let pp ppf (situation, oph) =
    Format.fprintf
      ppf
      "operation %a: %a"
      situation_pp
      situation
      Operation_hash.pp
      oph
end

module Event = struct
  type t =
    | Request of
        (Request.view * Worker_types.request_status * error list option)
    | Invalid_mempool_filter_configuration
    | Unparsable_operation of Operation_hash.t
    | Processing_n_operations of int
    | Fetching_operation of Operation_hash.t
    | Operation_included of Operation_hash.t
    | Operations_not_flushed of int
    | Operation_not_fetched of Operation_hash.t
    | Banned_operation_encountered of Operation_encountered.t

  type view = t

  let view t = t

  let level req =
    let open Request in
    match req with
    | Request (View (Flush _), _, _) -> Internal_event.Notice
    | Request (View (Notify _), _, _) -> Internal_event.Debug
    | Request (View Leftover, _, _) -> Internal_event.Debug
    | Request (View (Inject _), _, _) -> Internal_event.Notice
    | Request (View (Arrived _), _, _) -> Internal_event.Debug
    | Request (View Advertise, _, _) -> Internal_event.Debug
    | Request (View (Ban _), _, _) -> Internal_event.Notice
    | Invalid_mempool_filter_configuration | Unparsable_operation _
    | Processing_n_operations _ | Fetching_operation _ | Operation_included _
    | Operations_not_flushed _ | Operation_not_fetched _ ->
        Internal_event.Debug
    | Banned_operation_encountered _ -> Internal_event.Notice

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
          ~title:"invalid_mempool_configuration"
          empty
          (function
            | Invalid_mempool_filter_configuration -> Some () | _ -> None)
          (fun () -> Invalid_mempool_filter_configuration);
        case
          (Tag 3)
          ~title:"unparsable_operation"
          Operation_hash.encoding
          (function Unparsable_operation oph -> Some oph | _ -> None)
          (fun oph -> Unparsable_operation oph);
        case
          (Tag 4)
          ~title:"processing_n_operations"
          int31
          (function Processing_n_operations n -> Some n | _ -> None)
          (fun n -> Processing_n_operations n);
        case
          (Tag 5)
          ~title:"fetching_operation"
          (obj1 (req "fetching_operation" Operation_hash.encoding))
          (function Fetching_operation oph -> Some oph | _ -> None)
          (fun oph -> Fetching_operation oph);
        case
          (Tag 6)
          ~title:"operation_included"
          Operation_hash.encoding
          (function Operation_included oph -> Some oph | _ -> None)
          (fun oph -> Operation_included oph);
        case
          (Tag 7)
          ~title:"operations_not_flushed"
          int31
          (function Operations_not_flushed n -> Some n | _ -> None)
          (fun n -> Operations_not_flushed n);
        case
          (Tag 8)
          ~title:"operation_not_fetched"
          (obj1 (req "operation_not_fetched" Operation_hash.encoding))
          (function Operation_not_fetched oph -> Some oph | _ -> None)
          (fun oph -> Operation_not_fetched oph);
        case
          (Tag 9)
          ~title:"banned_operation_encountered"
          (obj1
             (req "banned_operation_encountered" Operation_encountered.encoding))
          (function
            | Banned_operation_encountered op_enc -> Some op_enc | _ -> None)
          (fun op_enc -> Banned_operation_encountered op_enc);
      ]

  let pp ppf = function
    | Invalid_mempool_filter_configuration ->
        Format.fprintf ppf "invalid mempool filter configuration"
    | Unparsable_operation oph ->
        Format.fprintf ppf "unparsable operation %a" Operation_hash.pp oph
    | Processing_n_operations n ->
        Format.fprintf ppf "processing %d operations" n
    | Fetching_operation oph ->
        Format.fprintf ppf "fetching operation %a" Operation_hash.pp oph
    | Operation_included oph ->
        Format.fprintf
          ppf
          "operation %a included before being prevalidated"
          Operation_hash.pp
          oph
    | Operations_not_flushed n ->
        Format.fprintf ppf "%d operations were not washed by the flush" n
    | Banned_operation_encountered op_enc ->
        Format.fprintf ppf "banned %a" Operation_encountered.pp op_enc
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
    | Operation_not_fetched oph ->
        Format.fprintf ppf "operation %a was not fetched" Operation_hash.pp oph
end
