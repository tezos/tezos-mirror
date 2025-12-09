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

include Client_keys.Signature_type

let scheme = "unencrypted"

let aggregate_scheme = "aggregate_unencrypted"

let title = "Built-in signer using raw unencrypted keys."

let description =
  "Please DO NOT USE this signer outside of test environments.\n\
   Valid secret key URIs are of the form\n\
  \ - unencrypted:<key>\n\
   where <key> is the secret key in Base58.\n\
   Valid public key URIs are of the form\n\
  \ - unencrypted:<public_key>\n\
   where <public_key> is the public key in Base58."

let secret_key sk_uri =
  Lwt.return
    (Signature.Secret_key.of_b58check (Uri.path (sk_uri : sk_uri :> Uri.t)))

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let make_sapling_key sk =
  let path =
    Tezos_crypto.Base58.simple_encode
      Tezos_sapling.Core.Wallet.Spending_key.b58check_encoding
      sk
  in
  Client_keys.make_sapling_uri (Uri.make ~scheme ~path ())

let public_key pk_uri =
  Lwt.return
    (Signature.Public_key.of_b58check (Uri.path (pk_uri : pk_uri :> Uri.t)))

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())

let neuterize sk_uri =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  let*? v = make_pk (Signature.Secret_key.to_public_key sk) in
  return v

let public_key_hash pk_uri =
  let open Lwt_result_syntax in
  let* pk = public_key pk_uri in
  return (Signature.Public_key.hash pk, Some pk)

let import_secret_key ~io:_ = public_key_hash

let list_known_keys path = Client_keys.list_known_keys path

let sign ?version ?watermark sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  match version with
  | Some Tezos_crypto.Signature.Version_0 -> (
      match Tezos_crypto.Signature.V0.Of_V_latest.secret_key sk with
      | Some sk ->
          let s = Tezos_crypto.Signature.V0.sign ?watermark sk buf in
          return (Signature.V_latest.Of_V0.signature s)
      | None ->
          Error_monad.failwith
            "Failed to handle secret key in Signature version 0")
  | Some Version_1 -> (
      match Tezos_crypto.Signature.V1.Of_V_latest.secret_key sk with
      | Some sk ->
          let s = Tezos_crypto.Signature.V1.sign ?watermark sk buf in
          return (Signature.V_latest.Of_V1.signature s)
      | None ->
          Error_monad.failwith
            "Failed to handle secret key in Signature version 1")
  | Some Version_2 -> (
      match Tezos_crypto.Signature.V2.Of_V_latest.secret_key sk with
      | Some sk ->
          let s = Tezos_crypto.Signature.V2.sign ?watermark sk buf in
          return (Signature.V_latest.Of_V2.signature s)
      | None ->
          Error_monad.failwith
            "Failed to handle secret key in Signature version 2")
  | Some Version_3 -> (
      match Tezos_crypto.Signature.V3.Of_V_latest.secret_key sk with
      | Some sk ->
          let s = Tezos_crypto.Signature.V3.sign ?watermark sk buf in
          return s
      | None ->
          Error_monad.failwith
            "Failed to handle secret key in Signature version 3")
  | None -> return (Tezos_crypto.Signature.V_latest.sign ?watermark sk buf)

let deterministic_nonce sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  return (Signature.deterministic_nonce sk buf)

let deterministic_nonce_hash sk_uri buf =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  return (Signature.deterministic_nonce_hash sk buf)

let supports_deterministic_nonces _ = Lwt_result_syntax.return_true

let bls_prove_possession ?override_pk sk_uri =
  let open Lwt_result_syntax in
  let* sk = secret_key sk_uri in
  match sk with
  | Bls sk ->
      return
      @@ Signature.Bls.of_bytes_exn
           (Signature.Bls.pop_prove ?msg:override_pk sk)
  | _ ->
      Error_monad.failwith
        "Proof of possession can only be requested for BLS keys."
