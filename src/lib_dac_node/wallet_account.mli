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

(** Module containing the definition of wallet and functions operating on
    wallets exclusive to [Coordinator] nodes. *)
module Coordinator : sig
  (** The type of an account wallet for [Coordinator] nodes. An account wallet
      contains the public key hash and an optional public key for each account
      wallet. [Coordinator] nodes can operate even if the public key of a
      committee member is not defined. Secret keys URIs are not included in
      [Coordinator] account wallets, as [Coordinator] nodes never sign any
      data. *)
  type t = {
    public_key_hash : Tezos_crypto.Aggregate_signature.public_key_hash;
    public_key_opt : Tezos_crypto.Aggregate_signature.public_key option;
  }

  (** [of_committee_member_public_key public_key] constructs a value of
      type [Coordinator.t] from the public key [public_key]. *)
  val of_committee_member_public_key :
    Tezos_crypto.Aggregate_signature.public_key -> t tzresult Lwt.t
end

(** Module containing the definition of account wallet and functions operating
    on account wallets exclusive to [Committee_member] nodes. *)
module Committee_member : sig
  (** The type of a wallet of a [Committee_member] node. It contains the public
      key hash of the single committee member associated with the
      [Committee_member] node, and its corresponding secret key URI. The
      wallet does not contain the public key of the committee member, as a
      [Committee_member] node never verifies the signatures of other
      committee members. *)
  type t = {
    public_key_hash : Tezos_crypto.Aggregate_signature.public_key_hash;
    secret_key_uri : Client_keys.aggregate_sk_uri;
  }

  (** [of_committee_member_address pkh wallet_cctxt] constructs a value of
      type [Committee_member.t] from the public key hash [pkh], by using the
      wallet context [wallet_cctxt] . *)
  val of_committee_member_address :
    Tezos_crypto.Aggregate_signature.public_key_hash ->
    #Client_context.wallet ->
    t tzresult Lwt.t
end

(** Module containing the definition of account wallet and functions operating
    on accunt wallets exclusive to [Legacy] nodes. *)
module Legacy : sig
  (** The type of a wallet of a [Legacy] node. It contains both the public key
      hash, optional public key, and optional secret key URI, of a committee
      member, as [Legacy] nodes are responsible both for signing and verifying
      signatures on behalf of the whole data availability committee. A [Legacy]
      node can keep operating even if the public key or secret key URI of a
      committee member are not defined. *)
  type t = {
    public_key_hash : Tezos_crypto.Aggregate_signature.public_key_hash;
    public_key_opt : Tezos_crypto.Aggregate_signature.public_key option;
    secret_key_uri_opt : Client_keys.aggregate_sk_uri option;
  }

  (** [of_committee_member_address pkh wallet_cctxt] constructs a value of
      type [Legacy.t] from the public key hash [pkh], by using the
      wallet context [wallet_cctxt] . *)
  val of_committee_member_address :
    Tezos_crypto.Aggregate_signature.public_key_hash ->
    #Client_context.wallet ->
    t tzresult Lwt.t
end
