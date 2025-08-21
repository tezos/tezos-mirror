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

open Signer_messages
module Events = Signer_events.Socket_daemon

let handle_client_step ?signing_version ?magic_bytes ?timeout
    ?(allow_list_known_keys = false) ?(allow_to_prove_possession = false)
    ~check_high_watermark ~require_auth cctxt fd =
  let open Lwt_result_syntax in
  let* recved = Tezos_base_unix.Socket.recv ?timeout fd Request.encoding in
  match recved with
  | Sign req ->
      let encoding = result_encoding Sign.Response.encoding in
      let*! res =
        Handler.sign
          ?signing_version
          cctxt
          req
          ?magic_bytes
          ~check_high_watermark
          ~require_auth
      in
      Tezos_base_unix.Socket.send fd encoding res
  | Deterministic_nonce req ->
      let encoding = result_encoding Deterministic_nonce.Response.encoding in
      let*! res = Handler.deterministic_nonce cctxt req ~require_auth in
      Tezos_base_unix.Socket.send fd encoding res
  | Deterministic_nonce_hash req ->
      let encoding =
        result_encoding Deterministic_nonce_hash.Response.encoding
      in
      let*! res = Handler.deterministic_nonce_hash cctxt req ~require_auth in
      Tezos_base_unix.Socket.send fd encoding res
  | Supports_deterministic_nonces req ->
      let encoding =
        result_encoding Supports_deterministic_nonces.Response.encoding
      in
      let*! res = Handler.supports_deterministic_nonces cctxt req in
      Tezos_base_unix.Socket.send fd encoding res
  | Public_key pkh ->
      let encoding = result_encoding Public_key.Response.encoding in
      let*! res = Handler.public_key cctxt pkh in
      Tezos_base_unix.Socket.send fd encoding res
  | Authorized_keys ->
      let encoding = result_encoding Authorized_keys.Response.encoding in
      let*! res =
        if require_auth then
          let* keys = Handler.Authorized_key.load cctxt in
          let hashes =
            List.map
              (fun (_, k) -> Tezos_crypto.Signature.Public_key.hash k)
              keys
          in
          return (Authorized_keys.Response.Authorized_keys hashes)
        else return Authorized_keys.Response.No_authentication
      in
      Tezos_base_unix.Socket.send fd encoding res
  | Known_keys ->
      let encoding = result_encoding Known_keys.Response.encoding in
      let*! res =
        if allow_list_known_keys then Handler.known_keys cctxt
        else failwith "List known keys request not allowed."
      in
      Tezos_base_unix.Socket.send fd encoding res
  | Bls_prove_possession (pkh, override_pk) ->
      let encoding = result_encoding Bls_prove_possession.Response.encoding in
      let*! res =
        if allow_to_prove_possession then
          Handler.bls_prove_possession cctxt ?override_pk pkh
        else failwith "Request to prove possession is not allowed"
      in
      Tezos_base_unix.Socket.send fd encoding res

let handle_client_loop ?signing_version ?magic_bytes ?timeout
    ?allow_list_known_keys ?allow_to_prove_possession ~check_high_watermark
    ~require_auth cctxt fd =
  let rec loop () =
    let open Lwt_result_syntax in
    let* () =
      handle_client_step
        ?signing_version
        ?magic_bytes
        ?timeout
        ?allow_list_known_keys
        ?allow_to_prove_possession
        ~check_high_watermark
        ~require_auth
        cctxt
        fd
    in
    loop ()
  in
  loop ()

let run ?signing_version ?magic_bytes ?timeout ?allow_list_known_keys
    ?allow_to_prove_possession ~check_high_watermark ~require_auth
    (cctxt : #Client_context.wallet) path =
  let open Lwt_result_syntax in
  let open Tezos_base_unix.Socket in
  let*! () =
    match path with
    | Tcp (host, service, _opts) ->
        Events.(emit accepting_tcp_requests) (host, service)
    | Unix path ->
        let signal_handle =
          Sys.Signal_handle
            (fun _ ->
              Format.printf "Removing the local socket file and quitting.@." ;
              Unix.unlink path ;
              exit 0)
        in
        Sys.set_signal Sys.sigint signal_handle ;
        Sys.set_signal Sys.sigterm signal_handle ;
        Events.(emit accepting_unix_requests) path
  in
  let* fds = bind path in
  let rec loop fd =
    let open Lwt_syntax in
    let* cfd, _ = Lwt_unix.accept fd in
    Lwt.dont_wait
      (fun () ->
        Unit.catch_s (fun () ->
            Lwt.finalize
              (fun () ->
                let* (_ : unit tzresult) =
                  handle_client_loop
                    ?signing_version
                    ?magic_bytes
                    ?timeout
                    ?allow_list_known_keys
                    ?allow_to_prove_possession
                    ~check_high_watermark
                    ~require_auth
                    cctxt
                    cfd
                in
                Lwt.return_unit)
              (fun () ->
                let+ r = Lwt_utils_unix.safe_close cfd in
                Result.iter_error
                  (Format.eprintf "Uncaught error: %a\n%!" pp_print_trace)
                  r)))
      (fun exc ->
        Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc)) ;
    loop fd
  in
  Lwt_result.ok @@ List.map_p loop fds
