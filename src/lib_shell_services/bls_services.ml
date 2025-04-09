(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This RPCs use Signature.V2 *)
module Bls = Tezos_crypto.Signature.Bls

module S = struct
  open Data_encoding

  let path = Tezos_rpc.Path.(root / "bls")

  type public_key_with_proof = {pk : Bls.Public_key.t; proof : Bls.t}

  let public_key_with_proof_encoding =
    let open Data_encoding in
    conv
      (fun {pk; proof} -> (pk, proof))
      (fun (pk, proof) -> {pk; proof})
      (obj2
         (req "public_key" Bls.Public_key.encoding)
         (req "proof" Bls.encoding))

  type public_key_and_public_key_hash = {
    pk : Bls.Public_key.t;
    pkh : Bls.Public_key_hash.t;
  }

  let public_key_and_public_key_hash_encoding =
    let open Data_encoding in
    conv
      (fun {pk; pkh} -> (pk, pkh))
      (fun (pk, pkh) -> {pk; pkh})
      (obj2
         (req "public_key" Bls.Public_key.encoding)
         (req "public_key_hash" Bls.Public_key_hash.encoding))

  type threshold_signature = {id : int; signature : Bls.t}

  let threshold_signature_encoding =
    let open Data_encoding in
    conv
      (fun {id; signature} -> (id, signature))
      (fun (id, signature) -> {id; signature})
      (obj2 (req "id" Data_encoding.int8) (req "signature" Bls.encoding))

  let aggregate_signatures =
    Tezos_rpc.Service.post_service
      ~description:"Aggregate BLS signatures"
      ~query:Tezos_rpc.Query.empty
      ~input:(list Bls.encoding)
      ~output:(option Bls.encoding)
      Tezos_rpc.Path.(path / "aggregate_signatures")

  let check_proof =
    Tezos_rpc.Service.post_service
      ~description:"Check a BLS proof"
      ~query:Tezos_rpc.Query.empty
      ~input:public_key_with_proof_encoding
      ~output:Data_encoding.bool
      Tezos_rpc.Path.(path / "check_proof")

  let aggregate_public_keys =
    Tezos_rpc.Service.post_service
      ~description:"Aggregate BLS public keys after checking their BLS proofs"
      ~query:Tezos_rpc.Query.empty
      ~input:(list public_key_with_proof_encoding)
      ~output:(option public_key_and_public_key_hash_encoding)
      Tezos_rpc.Path.(path / "aggregate_public_keys")

  let threshold_signatures =
    Tezos_rpc.Service.post_service
      ~description:"Threshold BLS signatures"
      ~query:Tezos_rpc.Query.empty
      ~input:(list threshold_signature_encoding)
      ~output:(option Bls.encoding)
      Tezos_rpc.Path.(path / "threshold_signatures")
end

let aggregate_signatures ctxt sigs =
  Tezos_rpc.Context.make_call S.aggregate_signatures ctxt () () sigs

let check_proof ctxt pk_with_proof =
  Tezos_rpc.Context.make_call S.check_proof ctxt () () pk_with_proof

let aggregate_public_keys ctxt pks_with_proofs =
  Tezos_rpc.Context.make_call S.aggregate_public_keys ctxt () () pks_with_proofs

let threshold_signatures ctxt sigs =
  Tezos_rpc.Context.make_call S.threshold_signatures ctxt () () sigs
