(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This RPCs use Signature.V2 *)
module Bls = Tezos_crypto.Signature.Bls

module S : sig
  type public_key_with_proof = {pk : Bls.Public_key.t; proof : Bls.t}

  type public_key_and_public_key_hash = {
    pk : Bls.Public_key.t;
    pkh : Bls.Public_key_hash.t;
  }

  type public_key_with_proofs = {pk : Bls.Public_key.t; proofs : Bls.t list}

  type threshold_signature_share = {id : int; signature : Bls.t}

  type threshold_signature = {
    pk : Signature.Bls.Public_key.t;
    msg : Bytes.t;
    signature_shares : threshold_signature_share list;
  }

  val aggregate_signatures :
    ([`POST], unit, unit, unit, Bls.t list, Bls.t option) Tezos_rpc.Service.t

  val check_proof :
    ([`POST], unit, unit, unit, public_key_with_proof, bool) Tezos_rpc.Service.t

  val aggregate_public_keys :
    ( [`POST],
      unit,
      unit,
      unit,
      public_key_with_proof list,
      public_key_and_public_key_hash option )
    Tezos_rpc.Service.t

  val aggregate_proofs :
    ( [`POST],
      unit,
      unit,
      unit,
      public_key_with_proofs,
      Bls.t option )
    Tezos_rpc.Service.t

  val threshold_signatures :
    ( [`POST],
      unit,
      unit,
      unit,
      threshold_signature,
      Bls.t option )
    Tezos_rpc.Service.t
end

val aggregate_signatures :
  #Tezos_rpc.Context.simple -> Bls.t list -> Bls.t option tzresult Lwt.t

val check_proof :
  #Tezos_rpc.Context.simple -> S.public_key_with_proof -> bool tzresult Lwt.t

val aggregate_public_keys :
  #Tezos_rpc.Context.simple ->
  S.public_key_with_proof list ->
  S.public_key_and_public_key_hash option tzresult Lwt.t

val aggregate_proofs :
  #Tezos_rpc.Context.simple ->
  S.public_key_with_proofs ->
  Bls.t option tzresult Lwt.t

val threshold_signatures :
  #Tezos_rpc.Context.simple ->
  S.threshold_signature ->
  Bls.t option tzresult Lwt.t
