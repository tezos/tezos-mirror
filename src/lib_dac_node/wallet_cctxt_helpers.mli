(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** Module that implements Account wallet functionalities. *)

(** [get_keys cctxt pkh] returns the keys associated with the
    given public key hash [pkh] in the wallet context [cctxt]. *)
val get_keys :
  #Client_context.wallet ->
  Tezos_crypto.Aggregate_signature.public_key_hash ->
  (Tezos_crypto.Aggregate_signature.public_key_hash
  * Tezos_crypto.Aggregate_signature.public_key option
  * Client_keys.aggregate_sk_uri option)
  tzresult
  Lwt.t

(** [get_public_key cctxt pkh] returns the public key associated with the given [pkh] if it can
      be found in [cctxt].
  *)
val get_public_key :
  #Client_context.wallet ->
  Tezos_crypto.Aggregate_signature.public_key_hash ->
  Tezos_crypto.Aggregate_signature.public_key option tzresult Lwt.t

(** [can_verify (pkh, pk_opt, sk_uri_opt)] checks whether the public key
    [pk_opt] of an account is defined. *)
val can_verify :
  Tezos_crypto.Aggregate_signature.public_key_hash
  * Tezos_crypto.Aggregate_signature.public_key option
  * Client_keys.aggregate_sk_uri option ->
  bool

(** [can_sign (pkh, pk_opt, sk_uri_opt)] checks whether the secret key URI
    [sk_uri_opt] of an account is defined. *)
val can_sign :
  Tezos_crypto.Aggregate_signature.public_key_hash
  * Tezos_crypto.Aggregate_signature.public_key option tzresult
  * Client_keys.aggregate_sk_uri option ->
  bool
