(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error += Timeout of (float * Uri.t)

let () =
  register_error_kind
    ~id:"preblocks_monitor.timeout"
    ~title:"Timeout reached when waiting for the next preblock"
    ~description:"Timeout reached when waiting for the next preblock"
    ~pp:(fun ppf (duration, endpoint) ->
      Format.fprintf
        ppf
        "Timeout reached when waiting for a preblock from: %a. No preblock was \
         received after %1.2f ms"
        Uri.pp
        endpoint
        duration)
    `Temporary
    Data_encoding.(
      obj2
        (req "duration" float)
        (req "endpoint" (conv Uri.to_string Uri.of_string string)))
    (function
      | Timeout duration_with_endpoint -> Some duration_with_endpoint
      | _ -> None)
    (fun duration_with_endpoint -> Timeout duration_with_endpoint)

type preblock_notification =
  | No_preblock
  | Preblock of Threshold_encryption_types.preblock

type t = {
  endpoint : Uri.t;
  preblocks_stream : preblock_notification Lwt_stream.t;
  (* https://gitlab.com/tezos/tezos/-/issues/7245.
     Remove lock once the produceBlock RPC is made asynchronous. *)
  (* The stream is going to be shared by the main threshold encryption
     sequencer loop, and by the handler of the produce_block RPC. Therefore, we
     need to ensure that only one of these components access the stream at any
     given time.*)
  preblocks_stream_lock : Lwt_mutex.t;
}

let init endpoint =
  let open Lwt_result_syntax in
  let* preblocks_from_sidecar_stream =
    Threshold_encryption_services.monitor_preblocks
      ~sidecar_endpoint:endpoint
      ()
  in
  let preblocks_from_sidecar_stream =
    Lwt_stream.map
      (fun preblock -> Preblock preblock)
      preblocks_from_sidecar_stream
  in
  let no_preblocks_stream, notify_no_preblock = Lwt_stream.create () in
  let preblocks_stream =
    Lwt_stream.choose [preblocks_from_sidecar_stream; no_preblocks_stream]
  in
  let notify_no_preblock () = notify_no_preblock @@ Some No_preblock in
  let preblocks_stream_lock = Lwt_mutex.create () in
  return
    ({endpoint; preblocks_stream; preblocks_stream_lock}, notify_no_preblock)

(* We do not lock on the stream in this function, hence it is unsafe to use it
   on its own as it is prone to data races. Rather, we require callers of
   this function to handle locking and unlocking [t.preblocks_stream] before
   and after accessing [t.preblocks_stream]. This gives users of the function
   the freedom to decide wether they need fine-grained (e.g. as in [next]) or
   coarse-grained access (e.g. as in [submit_and_fetch]) to the stream of
   preblocks.
   This function is intended to be used for internal use in this module only
   and is not exposed outside of the module. *)
let next_unsafe ?timeout t =
  let open Lwt_result_syntax in
  let timeout =
    match timeout with
    | None ->
        let p, _ = Lwt.task () in
        p
    | Some d ->
        let*! () = Lwt_unix.sleep d in
        tzfail @@ Timeout (d, t.endpoint)
  in
  let next_preblock () =
    let*! preblock = Lwt_stream.next t.preblocks_stream in
    return preblock
  in
  Lwt.pick [next_preblock (); timeout]

let next ?timeout t =
  Lwt_mutex.with_lock t.preblocks_stream_lock @@ fun () ->
  next_unsafe ?timeout t

let submit_and_fetch ?timeout ~force ~timestamp t =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock t.preblocks_stream_lock @@ fun () ->
  let* () =
    Threshold_encryption_proposals_handler.add_proposal_request timestamp force
  in
  next_unsafe ?timeout t
