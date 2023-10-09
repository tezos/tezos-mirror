(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Services_legacy
open Types

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Tezos_rpc.Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

let post_commitment :
    < meth : [`POST]
    ; input : Cryptobox.slot
    ; output : Cryptobox.commitment
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.post_service
    ~description:
      "Add a slot in the node's context if not already present. The \
       corresponding commitment is returned."
    ~query:Tezos_rpc.Query.empty
    ~input:Types.slot_encoding
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(open_root / "commitments")

let patch_commitment :
    < meth : [`PATCH]
    ; input : Types.slot_id
    ; output : unit
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:"Associate a commitment to a level and a slot index."
    ~query:Tezos_rpc.Query.empty
    ~input:Types.slot_id_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "commitments" /: Cryptobox.Commitment.rpc_arg)

let get_commitment_slot :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.slot
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve the content of the slot associated with the given commitment."
    ~query:Tezos_rpc.Query.empty
    ~output:Types.slot_encoding
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "slot")

let get_commitment_proof :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.commitment_proof
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Compute the proof associated with a commitment"
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment_proof.encoding
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "proof")

let put_commitment_shards :
    < meth : [`PUT]
    ; input : Types.with_proof
    ; output : unit
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.put_service
    ~description:
      "Compute and save the shards of the slot associated to the given \
       commitment. If the input's flag is true, the proofs associated with \
       each given shards are also computed."
    ~query:Tezos_rpc.Query.empty
    ~input:Types.with_proof_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "shards")

let get_commitment_by_published_level_and_index :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.commitment
    ; prefix : unit
    ; params : (unit * Types.level) * Types.slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the accepted commitment associated to the given slot index and \
       published at the given level."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slot_indices"
      /: Tezos_rpc.Arg.int / "commitment")

let get_commitment_headers :
    < meth : [`GET]
    ; input : unit
    ; output : Types.slot_header list
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : Types.level option * Types.slot_index option >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the known headers for the slot whose commitment is given."
    ~query:Types.slot_id_query
    ~output:(Data_encoding.list Types.slot_header_encoding)
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "headers")

let get_published_level_headers :
    < meth : [`GET]
    ; input : unit
    ; output : Types.slot_header list
    ; prefix : unit
    ; params : unit * Types.level
    ; query : Types.header_status option >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the known headers for the given published level."
    ~query:Types.opt_header_status_query
    ~output:(Data_encoding.list Types.slot_header_encoding)
    Tezos_rpc.Path.(open_root / "levels" /: Tezos_rpc.Arg.int32 / "headers")

let patch_profiles :
    < meth : [`PATCH]
    ; input : Types.operator_profiles
    ; output : unit
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:
      "Update the list of profiles tracked by the DAL node. Note that it does \
       not take the bootstrap profile as it is incompatible with other \
       profiles."
    ~query:Tezos_rpc.Query.empty
    ~input:(Data_encoding.list Types.operator_profile_encoding)
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "profiles")

let get_profiles :
    < meth : [`GET]
    ; input : unit
    ; output : Types.profiles
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the list of current profiles tracked by the DAL node"
    ~query:Tezos_rpc.Query.empty
    ~output:Types.profiles_encoding
    Tezos_rpc.Path.(open_root / "profiles")

let get_assigned_shard_indices :
    < meth : [`GET]
    ; input : unit
    ; output : Types.shard_index list
    ; prefix : unit
    ; params : (unit * Tezos_crypto.Signature.public_key_hash) * Types.level
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the shard indexes assigned to the given public key hash at the \
       given level."
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.(list int16)
    Tezos_rpc.Path.(
      open_root / "profiles" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "assigned_shard_indices")

let get_attestable_slots :
    < meth : [`GET]
    ; input : unit
    ; output : Types.attestable_slots
    ; prefix : unit
    ; params : (unit * Tezos_crypto.Signature.public_key_hash) * Types.level
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the currently attestable slots at the given attested level by \
       the given public key hash. A slot is attestable at level [l] if it is \
       published at level [l - attestation_lag] and *all* the shards assigned \
       at level [l] to the given public key hash are available in the DAL \
       node's store."
    ~query:Tezos_rpc.Query.empty
    ~output:Types.attestable_slots_encoding
    Tezos_rpc.Path.(
      open_root / "profiles" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "attestable_slots")

let monitor_shards :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.Commitment.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Monitor put shards"
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(open_root / "monitor_shards")

module P2P = struct
  open Tezos_rpc.Path

  let open_root = open_root / "p2p"

  module Gossipsub = struct
    let open_root = open_root / "gossipsub"

    let get_topics :
        < meth : [`GET]
        ; input : unit
        ; output : Types.Topic.t list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"get the topics this node is currently subscribed to"
        ~query:Tezos_rpc.Query.empty
        ~output:(Data_encoding.list Types.Topic.encoding)
        (open_root / "topics")
  end
end
