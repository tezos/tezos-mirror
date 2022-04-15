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

let scheme = "unencrypted"

let title = "Built-in signer using raw unencrypted keys."

let description =
  "Please DO NOT USE this signer outside of test environments.\n\
   Valid secret key URIs are of the form\n\
  \ - unencrypted:<key>\n\
   where <key> is the secret key in Base58.\n\
   Valid public key URIs are of the form\n\
  \ - unencrypted:<public_key>\n\
   where <public_key> is the public key in Base58."

let make_sapling_key sk =
  let path =
    Base58.simple_encode
      Tezos_sapling.Core.Wallet.Spending_key.b58check_encoding
      sk
  in
  Client_keys.make_sapling_uri (Uri.make ~scheme ~path ())

module Make_common (S : sig
  include S.COMMON_SIGNATURE

  type public_key_hash = Public_key_hash.t

  type public_key = Public_key.t

  type secret_key = Secret_key.t

  type sk_uri = private Uri.t

  type pk_uri = private Uri.t

  val make_sk_uri : Uri.t -> sk_uri tzresult

  val make_pk_uri : Uri.t -> pk_uri tzresult

  val scheme : string
end) =
struct
  include S

  let scheme = S.scheme

  let title = title

  let description = description

  let secret_key sk_uri =
    Secret_key.of_b58check (Uri.path (sk_uri : sk_uri :> Uri.t)) |> Lwt.return

  let make_sk sk : sk_uri tzresult =
    make_sk_uri (Uri.make ~scheme ~path:(S.Secret_key.to_b58check sk) ())

  let public_key pk_uri =
    Public_key.of_b58check (Uri.path (pk_uri : pk_uri :> Uri.t)) |> Lwt.return

  let make_pk pk : pk_uri tzresult =
    make_pk_uri (Uri.make ~scheme ~path:(Public_key.to_b58check pk) ())

  let neuterize sk_uri =
    let open Lwt_result_syntax in
    let* sk = secret_key sk_uri in
    let*? v = make_pk (Secret_key.to_public_key sk) in
    return v

  let public_key_hash pk_uri =
    let open Lwt_result_syntax in
    let* pk = public_key pk_uri in
    return (Public_key.hash pk, Some pk)

  let import_secret_key ~io:_ = public_key_hash
end

include Make_common (struct
  include Signature
  include Client_keys.Signature_type

  let make_sk_uri = Client_keys.make_sk_uri

  let make_pk_uri = Client_keys.make_pk_uri

  let scheme = scheme
end)

let sign ?watermark sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  return (Signature.sign ?watermark sk buf)

let deterministic_nonce sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  return (Signature.deterministic_nonce sk buf)

let deterministic_nonce_hash sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  return (Signature.deterministic_nonce_hash sk buf)

let supports_deterministic_nonces _ = Lwt_result_syntax.return_true

module Aggregate = struct
  include Make_common (struct
    include Aggregate_signature
    include Client_keys.Aggregate_type

    let make_sk_uri = Client_keys.make_aggregate_sk_uri

    let make_pk_uri = Client_keys.make_aggregate_pk_uri

    let scheme = "aggregate_" ^ scheme
  end)

  let sign sk_uri buf =
    let open Lwt_result_syntax in
    let+ sk = secret_key sk_uri in
    Aggregate_signature.sign sk buf
end
