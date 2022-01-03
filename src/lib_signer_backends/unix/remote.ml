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

open Client_keys

let scheme = "remote"

module Make
    (RPC_client : RPC_client.S) (S : sig
      val default : Uri.t

      val authenticate :
        Signature.Public_key_hash.t list ->
        Bytes.t ->
        Signature.t tzresult Lwt.t

      val logger : RPC_client.logger
    end) =
struct
  let scheme = scheme

  let title = "Built-in tezos-signer using remote wallet."

  let description =
    "Valid locators are of the form\n\
    \ - remote://tz1...\n\
     The key will be queried to current remote signer, which can be configured \
     with the `--remote-signer` or `-R` options, or by defining the following \
     environment variables:\n\
    \ - $TEZOS_SIGNER_UNIX_PATH,\n\
    \ - $TEZOS_SIGNER_TCP_HOST and $TEZOS_SIGNER_TCP_PORT (default: 7732),\n\
    \ - $TEZOS_SIGNER_HTTP_HOST and $TEZOS_SIGNER_HTTP_PORT (default: 6732),\n\
    \ - $TEZOS_SIGNER_HTTPS_HOST and $TEZOS_SIGNER_HTTPS_PORT (default: 443)."

  include Client_keys.Signature_type
  module Socket = Socket.Make (S)
  module Http = Http.Make (RPC_client) (S)
  module Https = Https.Make (RPC_client) (S)

  let get_remote () =
    match Uri.scheme S.default with
    | Some "unix" -> (module Socket.Unix : SIGNER)
    | Some "tcp" -> (module Socket.Tcp : SIGNER)
    | Some "http" -> (module Http : SIGNER)
    | Some "https" -> (module Https : SIGNER)
    | _ -> assert false

  module Remote = (val get_remote () : SIGNER)

  let key =
    match Uri.scheme S.default with
    | Some "unix" ->
        fun uri ->
          let key = Uri.path uri in
          Uri.add_query_param' S.default ("pkh", key)
    | Some "tcp" ->
        fun uri ->
          let key = Uri.path uri in
          Uri.with_path S.default key
    | Some ("https" | "http") -> (
        fun uri ->
          let key = Uri.path uri in
          match Uri.path S.default with
          | "" -> Uri.with_path S.default key
          | path -> Uri.with_path S.default (path ^ "/" ^ key))
    | _ -> assert false

  let public_key pk_uri =
    let open Lwt_result_syntax in
    let*? v = Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)) in
    Remote.public_key v

  let public_key_hash pk_uri =
    let open Lwt_result_syntax in
    let*? v = Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)) in
    Remote.public_key_hash v

  let import_secret_key ~io:_ = public_key_hash

  let neuterize sk_uri =
    let open Lwt_result_syntax in
    let*? v = Client_keys.make_pk_uri (sk_uri : sk_uri :> Uri.t) in
    return v

  let sign ?watermark sk_uri msg =
    let open Lwt_result_syntax in
    let*? sk_uri = Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)) in
    Remote.sign ?watermark sk_uri msg

  let deterministic_nonce sk_uri msg =
    let open Lwt_result_syntax in
    let*? sk_uri = Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)) in
    Remote.deterministic_nonce sk_uri msg

  let deterministic_nonce_hash sk_uri msg =
    let open Lwt_result_syntax in
    let*? sk_uri = Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)) in
    Remote.deterministic_nonce_hash sk_uri msg

  let supports_deterministic_nonces sk_uri =
    let open Lwt_result_syntax in
    let*? v = Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)) in
    Remote.supports_deterministic_nonces v
end

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())

let read_base_uri_from_env () =
  let open Lwt_tzresult_syntax in
  match
    ( Sys.getenv_opt "TEZOS_SIGNER_UNIX_PATH",
      Sys.getenv_opt "TEZOS_SIGNER_TCP_HOST",
      Sys.getenv_opt "TEZOS_SIGNER_HTTP_HOST",
      Sys.getenv_opt "TEZOS_SIGNER_HTTPS_HOST" )
  with
  | (None, None, None, None) -> return_none
  | (Some path, None, None, None) -> return_some (Socket.make_unix_base path)
  | (None, Some host, None, None) -> (
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_TCP_PORT" with
          | None -> 7732
          | Some port -> int_of_string port
        in
        return_some (Socket.make_tcp_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_TCP_PORT.@.")
  | (None, None, Some host, None) -> (
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_HTTP_PORT" with
          | None -> 6732
          | Some port -> int_of_string port
        in
        return_some (Http.make_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_HTTP_PORT.@.")
  | (None, None, None, Some host) -> (
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_PORT" with
          | None -> 443
          | Some port -> int_of_string port
        in
        return_some (Https.make_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_HTTPS_PORT.@.")
  | (_, _, _, _) ->
      failwith
        "Only one the following environment variable must be defined: \
         TEZOS_SIGNER_UNIX_PATH, TEZOS_SIGNER_TCP_HOST, \
         TEZOS_SIGNER_HTTP_HOST, TEZOS_SIGNER_HTTPS_HOST@."

type error += Invalid_remote_signer of string

let () =
  register_error_kind
    `Branch
    ~id:"invalid_remote_signer"
    ~title:"Unexpected URI fot remote signer"
    ~description:"The provided remote signer is invalid."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "@[<v 0>Value '%s' is not a valid URI for a remote signer.@,\
         Supported URIs for remote signers are of the form:@,\
        \ - unix:///path/to/socket/file@,\
        \ - tcp://host:port@,\
        \ - http://host[:port][/prefix]@,\
        \ - https://host[:port][/prefix]@]"
        s)
    Data_encoding.(obj1 (req "uri" string))
    (function Invalid_remote_signer s -> Some s | _ -> None)
    (fun s -> Invalid_remote_signer s)

let parse_base_uri s =
  (* FIXME: documentation for [Uri.of_string] doesn't mention any exception.
      However, from reading the code it seems like [Not_found] can be raised
      (via some internal call to [Re.exec]. *)
  match Uri.of_string s with
  (* We keep [Invalid_argument] but this needs investigation because of the
      above comment *)
  | exception Invalid_argument msg -> error_with "Malformed URI: %s" msg
  | exception Not_found -> error_with "Malformed URI"
  | uri -> (
      match Uri.scheme uri with
      | Some "http" -> Ok uri
      | Some "https" -> Ok uri
      | Some "tcp" -> Ok uri
      | Some "unix" -> Ok uri
      | Some scheme -> error_with "Unknown scheme: %s" scheme
      | None -> error_with "Unknown scheme: <empty>")

let parse_base_uri s =
  parse_base_uri s |> record_trace (Invalid_remote_signer s) |> Lwt.return
