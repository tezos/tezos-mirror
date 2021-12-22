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

module Events = Signer_events.Http_daemon

let run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes
    ~check_high_watermark ~require_auth mode =
  let open Lwt_result_syntax in
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register1 dir Signer_services.sign (fun pkh signature data ->
        Handler.sign
          ?magic_bytes
          ~check_high_watermark
          ~require_auth
          cctxt
          {pkh; data; signature})
  in
  let dir =
    RPC_directory.register1 dir Signer_services.public_key (fun pkh () () ->
        Handler.public_key cctxt pkh)
  in
  let dir =
    RPC_directory.register0 dir Signer_services.authorized_keys (fun () () ->
        if require_auth then
          let* keys = Handler.Authorized_key.load cctxt in
          let hashes =
            List.map (fun (_, k) -> Signature.Public_key.hash k) keys
          in
          return_some hashes
        else return_none)
  in
  Lwt.catch
    (fun () ->
      List.map
        (fun host ->
          let*! () = Events.(emit listening) host in
          let*! _server =
            RPC_server.launch
              ~host:(Ipaddr.V6.to_string host)
              mode
              dir
              ~media_types:Media_type.all_media_types
          in
          Lwt_utils.never_ending ())
        hosts
      |> Lwt.choose)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          failwith "Port already in use."
      | exn -> fail_with_exn exn)

let run_https ~host ~port ~cert ~key ?magic_bytes ~check_high_watermark
    ~require_auth (cctxt : #Client_context.wallet) =
  let open Lwt_syntax in
  let* points =
    Lwt_utils_unix.getaddrinfo
      ~passive:true
      ~node:host
      ~service:(string_of_int port)
  in
  match points with
  | [] -> failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = List.map fst points in
      let* () = Events.(emit accepting_requests) ("HTTPS", port) in
      let mode : Conduit_lwt_unix.server =
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port)
      in
      run
        (cctxt : #Client_context.wallet)
        ~hosts
        ?magic_bytes
        ~check_high_watermark
        ~require_auth
        mode

let run_http ~host ~port ?magic_bytes ~check_high_watermark ~require_auth
    (cctxt : #Client_context.wallet) =
  let open Lwt_syntax in
  let* points =
    Lwt_utils_unix.getaddrinfo
      ~passive:true
      ~node:host
      ~service:(string_of_int port)
  in
  match points with
  | [] -> failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = List.map fst points in
      let* () = Events.(emit accepting_requests) ("HTTP", port) in
      let mode : Conduit_lwt_unix.server = `TCP (`Port port) in
      run
        (cctxt : #Client_context.wallet)
        ~hosts
        ?magic_bytes
        ~check_high_watermark
        ~require_auth
        mode
