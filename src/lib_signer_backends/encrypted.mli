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

module Make (C : sig
  val cctxt : Client_context.io_wallet
end) : Client_keys.SIGNER

module Make_aggregate (C : sig
  val cctxt : Client_context.io_wallet
end) : Client_keys.AGGREGATE_SIGNER

val decrypt :
  #Client_context.io_wallet ->
  ?name:string ->
  Client_keys.sk_uri ->
  Signature.secret_key tzresult Lwt.t

val decrypt_aggregate :
  #Client_context.io_wallet ->
  ?name:string ->
  Client_keys.aggregate_sk_uri ->
  Aggregate_signature.secret_key tzresult Lwt.t

val decrypt_all : #Client_context.io_wallet -> unit tzresult Lwt.t

val decrypt_list :
  #Client_context.io_wallet -> string list -> unit tzresult Lwt.t

(** Encrypts a secret key using the given password *)
val encrypt : Signature.secret_key -> bytes -> Client_keys.sk_uri tzresult Lwt.t

(** Prompts password twice to user for confirmation and returns
    the corresponding encrypted secret key *)
val prompt_twice_and_encrypt :
  #Client_context.io ->
  Signature.secret_key ->
  Client_keys.sk_uri tzresult Lwt.t

(** [prompt_twice_and_encrypt_aggregate cctxt sk] Prompts password twice to user
    for confirmation and returns the corresponding encrypted aggregate secret
    key *)
val prompt_twice_and_encrypt_aggregate :
  #Client_context.io ->
  Aggregate_signature.secret_key ->
  Client_keys.aggregate_sk_uri tzresult Lwt.t

val encrypt_sapling_key :
  #Client_context.io ->
  Tezos_sapling.Core.Wallet.Spending_key.t ->
  Client_keys.sapling_uri tzresult Lwt.t

val decrypt_sapling_key :
  #Client_context.io ->
  Client_keys.sapling_uri ->
  Tezos_sapling.Core.Wallet.Spending_key.t tzresult Lwt.t

val encrypt_pvss_key :
  #Client_context.io ->
  Pvss_secp256k1.Secret_key.t ->
  Client_keys.pvss_sk_uri tzresult Lwt.t
