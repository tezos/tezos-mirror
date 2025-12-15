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

module Events = Signer_events
open Client_keys
open Signer_messages

let tcp_scheme = "tcp"

let unix_scheme = "unix"

module Make (P : sig
  val authenticate :
    Tezos_crypto.Signature.Public_key_hash.t list ->
    Bytes.t ->
    Tezos_crypto.Signature.t tzresult Lwt.t
end) =
struct
  open P

  type request_type =
    | Sign_request
    | Deterministic_nonce_request
    | Deterministic_nonce_hash_request

  let build_request ?version (pkh : Tezos_crypto.Signature.public_key_hash) data
      signature request =
    match request with
    | Sign_request ->
        let pkh =
          match (pkh, version) with
          | _, None -> Pkh pkh
          | (Ed25519 _ | Secp256k1 _ | P256 _), Some _ -> Pkh pkh
          | Bls _, Some version -> Pkh_with_version (pkh, version)
          | Mldsa44 _, Some version -> Pkh_with_version (pkh, version)
        in
        Request.Sign {Sign.Request.pkh; data; signature}
    | Deterministic_nonce_request ->
        Request.Deterministic_nonce
          {Deterministic_nonce.Request.pkh; data; signature}
    | Deterministic_nonce_hash_request ->
        Request.Deterministic_nonce_hash
          {Deterministic_nonce_hash.Request.pkh; data; signature}

  let maybe_authenticate pkh msg conn =
    let open Lwt_result_syntax in
    let* () =
      Tezos_base_unix.Socket.send conn Request.encoding Request.Authorized_keys
    in
    let* authorized_keys =
      Tezos_base_unix.Socket.recv
        conn
        (result_encoding Authorized_keys.Response.encoding)
    in
    let* v = Lwt.return authorized_keys in
    match v with
    | No_authentication -> return_none
    | Authorized_keys authorized_keys ->
        let* signature =
          authenticate authorized_keys (Sign.Request.to_sign ~pkh ~data:msg)
        in
        return_some signature

  let with_signer_operation ?version path pkh msg request_type enc =
    let open Lwt_result_syntax in
    let f () =
      Tezos_base_unix.Socket.with_connection path (fun conn ->
          let* signature = maybe_authenticate pkh msg conn in
          let req = build_request ?version pkh msg signature request_type in
          let* () = Tezos_base_unix.Socket.send conn Request.encoding req in
          Tezos_base_unix.Socket.recv conn (result_encoding enc))
    in
    let rec loop n =
      let*! r = protect (fun () -> f ()) in
      match r with
      | Error trace as e
        when List.exists
               (function Exn Lwt_unix.Timeout -> true | _ -> false)
               trace ->
          if n = 0 then Lwt.return e
          else
            let*! () = Events.(emit Socket.signer_timeout) (pred n) in
            loop (pred n)
      | Error _ as e -> Lwt.return e
      | Ok v -> Lwt.return v
    in
    loop 3

  let sign ?version ?watermark path pkh msg =
    let msg =
      match watermark with
      | None -> msg
      | Some watermark ->
          Bytes.cat (Tezos_crypto.Signature.bytes_of_watermark watermark) msg
    in
    with_signer_operation
      ?version
      path
      pkh
      msg
      Sign_request
      Sign.Response.encoding

  let deterministic_nonce path pkh msg =
    with_signer_operation
      path
      pkh
      msg
      Deterministic_nonce_request
      Deterministic_nonce.Response.encoding

  let deterministic_nonce_hash path pkh msg =
    with_signer_operation
      path
      pkh
      msg
      Deterministic_nonce_hash_request
      Deterministic_nonce_hash.Response.encoding

  let supports_deterministic_nonces path pkh =
    let open Lwt_result_syntax in
    Tezos_base_unix.Socket.with_connection path (fun conn ->
        let* () =
          Tezos_base_unix.Socket.send
            conn
            Request.encoding
            (Request.Supports_deterministic_nonces pkh)
        in
        let* supported =
          Tezos_base_unix.Socket.recv
            conn
            (result_encoding Supports_deterministic_nonces.Response.encoding)
        in
        Lwt.return supported)

  let list_known_keys path =
    let open Lwt_result_syntax in
    Tezos_base_unix.Socket.with_connection path (fun conn ->
        let* () =
          Tezos_base_unix.Socket.send conn Request.encoding Request.Known_keys
        in
        let encoding = result_encoding Known_keys.Response.encoding in
        let* pkhs = Tezos_base_unix.Socket.recv conn encoding in
        match pkhs with Error _ as e -> Lwt.return e | Ok pkhs -> return pkhs)

  let bls_prove_possession path ?override_pk pkh =
    let open Lwt_result_syntax in
    Tezos_base_unix.Socket.with_connection path (fun conn ->
        let* () =
          Tezos_base_unix.Socket.send
            conn
            Request.encoding
            (Request.Bls_prove_possession (pkh, override_pk))
        in
        let encoding = result_encoding Bls_prove_possession.Response.encoding in
        let* proof_of_possession = Tezos_base_unix.Socket.recv conn encoding in
        match proof_of_possession with
        | Error _ as e -> Lwt.return e
        | Ok proof_of_possession -> return proof_of_possession)

  let public_key path pkh =
    let open Lwt_result_syntax in
    Tezos_base_unix.Socket.with_connection path (fun conn ->
        let* () =
          Tezos_base_unix.Socket.send
            conn
            Request.encoding
            (Request.Public_key pkh)
        in
        let encoding = result_encoding Public_key.Response.encoding in
        let* pk = Tezos_base_unix.Socket.recv conn encoding in
        Lwt.return pk)

  module Unix = struct
    let scheme = unix_scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded unix socket."

    let description =
      "Valid locators are of the form\n - unix:/path/to/socket?pkh=tz1..."

    include Client_keys.Signature_type

    let parse_aux uri =
      let open Result_syntax in
      assert (Uri.scheme uri = Some scheme) ;
      return (Tezos_base_unix.Socket.Unix (Uri.path uri))

    let parse uri =
      let open Result_syntax in
      match Uri.get_query_param uri "pkh" with
      | None -> error_with "Missing the query parameter: 'pkh=tz1...'"
      | Some key ->
          let* key = Tezos_crypto.Signature.Public_key_hash.of_b58check key in
          let+ path = parse_aux uri in
          (path, key)

    let parse_aux uri =
      parse_aux uri |> record_trace (Invalid_uri uri) |> Lwt.return

    let parse uri = parse uri |> record_trace (Invalid_uri uri) |> Lwt.return

    let public_key uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : pk_uri :> Uri.t) in
      public_key path pkh

    let neuterize uri =
      let open Lwt_result_syntax in
      let*? v = Client_keys.make_pk_uri (uri : sk_uri :> Uri.t) in
      return v

    let public_key_hash uri =
      let open Lwt_result_syntax in
      let* pk = public_key uri in
      return (Tezos_crypto.Signature.Public_key.hash pk, Some pk)

    let import_secret_key ~io:_ = public_key_hash

    let list_known_keys uri =
      let open Lwt_result_syntax in
      let* path = parse_aux uri in
      list_known_keys path

    let sign ?version ?watermark uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      sign ?version ?watermark path pkh msg

    let deterministic_nonce uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      deterministic_nonce path pkh msg

    let deterministic_nonce_hash uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      deterministic_nonce_hash path pkh msg

    let supports_deterministic_nonces uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      supports_deterministic_nonces path pkh

    let bls_prove_possession ?override_pk uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      bls_prove_possession path ?override_pk pkh
  end

  module Tcp = struct
    let scheme = tcp_scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded tcp socket."

    let description =
      "Valid locators are of the form\n - tcp://host:port/tz1..."

    include Client_keys.Signature_type

    let parse_aux uri =
      let open Result_syntax in
      assert (Uri.scheme uri = Some scheme) ;
      match (Uri.host uri, Uri.port uri) with
      | None, _ -> error_with "Missing host address"
      | _, None -> error_with "Missing host port"
      | Some path, Some port ->
          let tcp_socket =
            Tezos_base_unix.Socket.Tcp
              (path, string_of_int port, [Lwt_unix.AI_SOCKTYPE SOCK_STREAM])
          in
          return tcp_socket

    let parse uri =
      let open Result_syntax in
      let* tcp_socket = parse_aux uri in
      let pkh = Uri.path uri in
      let pkh = try String.(sub pkh 1 (length pkh - 1)) with _ -> "" in
      let+ pkh = Tezos_crypto.Signature.Public_key_hash.of_b58check pkh in
      (tcp_socket, pkh)

    let parse_aux uri =
      parse_aux uri |> record_trace (Invalid_uri uri) |> Lwt.return

    let parse uri = parse uri |> record_trace (Invalid_uri uri) |> Lwt.return

    let list_known_keys uri =
      let open Lwt_result_syntax in
      let* path = parse_aux uri in
      list_known_keys path

    let public_key uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : pk_uri :> Uri.t) in
      public_key path pkh

    let neuterize uri =
      let open Lwt_result_syntax in
      let*? v = Client_keys.make_pk_uri (uri : sk_uri :> Uri.t) in
      return v

    let public_key_hash uri =
      let open Lwt_result_syntax in
      let* pk = public_key uri in
      return (Tezos_crypto.Signature.Public_key.hash pk, Some pk)

    let import_secret_key ~io:_ = public_key_hash

    let sign ?version ?watermark uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      sign ?version ?watermark path pkh msg

    let deterministic_nonce uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      deterministic_nonce path pkh msg

    let deterministic_nonce_hash uri msg =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      deterministic_nonce_hash path pkh msg

    let supports_deterministic_nonces uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      supports_deterministic_nonces path pkh

    let bls_prove_possession ?override_pk uri =
      let open Lwt_result_syntax in
      let* path, pkh = parse (uri : sk_uri :> Uri.t) in
      bls_prove_possession path ?override_pk pkh
  end
end

let make_unix_base path = Uri.make ~scheme:unix_scheme ~path ()

let make_tcp_base host port = Uri.make ~scheme:tcp_scheme ~host ~port ()
