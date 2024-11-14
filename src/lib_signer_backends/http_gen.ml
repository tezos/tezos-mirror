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

module Make (N : sig
  val scheme : string
end) =
struct
  let scheme = N.scheme

  module Make
      (RPC_client : RPC_client.S)
      (P : sig
        val authenticate :
          Tezos_crypto.Signature.Public_key_hash.t list ->
          Bytes.t ->
          Tezos_crypto.Signature.t tzresult Lwt.t

        val logger : RPC_client.logger
      end) =
  struct
    include Client_keys.Signature_type

    let scheme = scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded " ^ scheme
      ^ " requests."

    let description =
      "Valid locators are of this form:\n" ^ " - " ^ scheme ^ "://host/tz1...\n"
      ^ " - " ^ scheme ^ "://host:port/path/to/service/tz1...\n"
      ^ "Environment variable TEZOS_SIGNER_HTTP_HEADERS can be specified to \
         add headers to the requests (only 'host' and custom 'x-...' headers \
         are supported)."

    let headers =
      match Sys.getenv_opt "TEZOS_SIGNER_HTTP_HEADERS" with
      | None -> None
      | Some contents ->
          let lines = String.split_on_char '\n' contents in
          Some
            (List.fold_left
               (fun acc line ->
                 match String.index_opt line ':' with
                 | None ->
                     Stdlib.failwith
                       "Http signer: invalid TEZOS_SIGNER_HTTP_HEADERS \
                        environment variable, missing colon"
                 | Some pos ->
                     let header = String.trim (String.sub line 0 pos) in
                     let header = String.lowercase_ascii header in
                     if
                       header <> "host"
                       && (String.length header < 2
                          || String.sub header 0 2 <> "x-")
                     then
                       Stdlib.failwith
                         "Http signer: invalid TEZOS_SIGNER_HTTP_HEADERS \
                          environment variable, only 'host' or 'x-' headers \
                          are supported" ;
                     let value =
                       String.trim
                         (String.sub
                            line
                            (pos + 1)
                            (String.length line - pos - 1))
                     in
                     (header, value) :: acc)
               []
               lines)

    let parse uri =
      (* extract `tz1..` from the last component of the path *)
      let open Lwt_result_syntax in
      assert (Uri.scheme uri = Some scheme) ;
      let path = Uri.path uri in
      let* base, pkh =
        match String.rindex_opt path '/' with
        | None -> failwith "Invalid locator %a" Uri.pp_hum uri
        | Some i ->
            let pkh =
              try String.sub path (i + 1) (String.length path - i - 1)
              with _ -> ""
            in
            let path = String.sub path 0 i in
            return (Uri.with_path uri path, pkh)
      in
      let* pkh =
        Lwt.return (Tezos_crypto.Signature.Public_key_hash.of_b58check pkh)
      in
      return (base, pkh)

    let public_key uri =
      let open Lwt_result_syntax in
      let* base, pkh = parse (uri : pk_uri :> Uri.t) in
      RPC_client.call_service
        ~logger:P.logger
        ?headers
        Media_type.all_media_types
        ~base
        Signer_services.public_key
        ((), pkh)
        ()
        ()

    let neuterize uri =
      let open Lwt_result_syntax in
      let*? v = Client_keys.make_pk_uri (uri : sk_uri :> Uri.t) in
      return v

    let public_key_hash uri =
      let open Lwt_result_syntax in
      let* pk = public_key uri in
      return (Tezos_crypto.Signature.Public_key.hash pk, Some pk)

    let import_secret_key ~io:_ = public_key_hash

    let get_signature base pkh msg =
      let open Lwt_result_syntax in
      let* o =
        RPC_client.call_service
          ~logger:P.logger
          ?headers
          Media_type.all_media_types
          ~base
          Signer_services.authorized_keys
          ()
          ()
          ()
      in
      match o with
      | Some authorized_keys ->
          let* signature =
            P.authenticate
              authorized_keys
              (Signer_messages.Sign.Request.to_sign ~pkh ~data:msg)
          in
          return_some signature
      | None -> return_none

    let sign ?watermark uri msg =
      let open Lwt_result_syntax in
      let* base, pkh = parse (uri : sk_uri :> Uri.t) in
      let msg =
        match watermark with
        | None -> msg
        | Some watermark ->
            Bytes.cat (Tezos_crypto.Signature.bytes_of_watermark watermark) msg
      in
      let* signature = get_signature base pkh msg in
      RPC_client.call_service
        ~logger:P.logger
        ?headers
        Media_type.all_media_types
        ~base
        Signer_services.sign
        ((), pkh)
        signature
        msg

    let deterministic_nonce uri msg =
      let open Lwt_result_syntax in
      let* base, pkh = parse (uri : sk_uri :> Uri.t) in
      let* signature = get_signature base pkh msg in
      RPC_client.call_service
        ~logger:P.logger
        ?headers
        Media_type.all_media_types
        ~base
        Signer_services.deterministic_nonce
        ((), pkh)
        signature
        msg

    let deterministic_nonce_hash uri msg =
      let open Lwt_result_syntax in
      let* base, pkh = parse (uri : sk_uri :> Uri.t) in
      let* signature = get_signature base pkh msg in
      RPC_client.call_service
        ~logger:P.logger
        ?headers
        Media_type.all_media_types
        ~base
        Signer_services.deterministic_nonce_hash
        ((), pkh)
        signature
        msg

    let supports_deterministic_nonces uri =
      let open Lwt_result_syntax in
      let* base, pkh = parse (uri : sk_uri :> Uri.t) in
      let*! r =
        RPC_client.call_service
          ~logger:P.logger
          ?headers
          Media_type.all_media_types
          ~base
          Signer_services.supports_deterministic_nonces
          ((), pkh)
          ()
          ()
      in
      match r with
      | Ok ans -> return ans
      | Error (Tezos_rpc.Context.Not_found _ :: _) -> return_false
      | Error _ as res -> Lwt.return res
  end

  let make_base host port = Uri.make ~scheme ~host ~port ()
end
