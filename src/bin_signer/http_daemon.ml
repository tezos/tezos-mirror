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

let run (cctxt : #Client_context.wallet) ~hosts ?signing_version ?magic_bytes
    ?(allow_list_known_keys = false) ?(allow_to_prove_possession = false)
    ~check_high_watermark ~require_auth mode =
  let open Lwt_result_syntax in
  let dir = Tezos_rpc.Directory.empty in
  let dir =
    Tezos_rpc.Directory.register1
      dir
      Signer_services.sign
      (fun pkh (signature, req_version) data ->
        let pkh =
          match req_version with
          | Some version -> Signer_messages.Pkh_with_version (pkh, version)
          | None -> Signer_messages.Pkh pkh
        in
        Handler.sign
          ?signing_version
          ?magic_bytes
          ~check_high_watermark
          ~require_auth
          cctxt
          {pkh; data; signature})
  in
  let dir =
    Tezos_rpc.Directory.register1
      dir
      Signer_services.public_key
      (fun pkh () () -> Handler.public_key cctxt pkh)
  in
  let dir =
    if allow_to_prove_possession then
      Tezos_rpc.Directory.register1
        dir
        Signer_services.bls_prove_possession
        (fun pkh override_pk () ->
          Handler.bls_prove_possession cctxt ?override_pk pkh)
    else
      Tezos_rpc.Directory.register1
        dir
        Signer_services.bls_prove_possession
        (fun _pkh _override_pk () ->
          failwith "Request to prove possession is not allowed.")
  in
  let dir =
    Tezos_rpc.Directory.register0
      dir
      Signer_services.authorized_keys
      (fun () () ->
        if require_auth then
          let* keys = Handler.Authorized_key.load cctxt in
          let hashes =
            List.map
              (fun (_, k) -> Tezos_crypto.Signature.Public_key.hash k)
              keys
          in
          return_some hashes
        else return_none)
  in
  let dir =
    if allow_list_known_keys then
      Tezos_rpc.Directory.register0 dir Signer_services.known_keys (fun () () ->
          Handler.known_keys cctxt)
    else
      Tezos_rpc.Directory.register0 dir Signer_services.known_keys (fun () () ->
          failwith "List known keys request not allowed.")
  in
  let server =
    RPC_server.init_server ~media_types:Media_type.all_media_types dir
  in
  Lwt.catch
    (fun () ->
      List.map
        (fun host ->
          let*! () = Events.(emit listening) host in
          let*! () =
            RPC_server.launch
              ~host:(Ipaddr.V6.to_string host)
              server
              ~callback:(RPC_server.resto_callback server)
              mode
          in
          Lwt_utils.never_ending ())
        hosts
      |> Lwt.choose)
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          failwith "Port already in use."
      | exn -> fail_with_exn exn)

let run_https ~host ~port ~cert ~key ?signing_version ?magic_bytes
    ?allow_list_known_keys ?allow_to_prove_possession ~check_high_watermark
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
        ?signing_version
        ?magic_bytes
        ?allow_list_known_keys
        ?allow_to_prove_possession
        ~check_high_watermark
        ~require_auth
        mode

let run_http ~host ~port ?signing_version ?magic_bytes ?allow_list_known_keys
    ?allow_to_prove_possession ~check_high_watermark ~require_auth
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
        ?signing_version
        ?magic_bytes
        ?allow_list_known_keys
        ?allow_to_prove_possession
        ~check_high_watermark
        ~require_auth
        mode
