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
  return ({endpoint; preblocks_stream}, notify_no_preblock)

let next ?timeout t =
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
