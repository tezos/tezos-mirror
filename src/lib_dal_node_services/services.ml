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
open Tezos_crypto_dal

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

module Types = struct
  type level = int32

  type slot_index = int

  (* Declaration of types used as inputs and/or outputs. *)
  type slot_id = {slot_level : level; slot_index : slot_index}

  (** A set of slots, represented by a list of booleans (false for not in the
      set). It is used for instance to record which slots are deemed available
      by an attestor. *)
  type slot_set = bool list

  type attestable_slots = Attestable_slots of slot_set | Not_in_committee

  type header_status =
    [`Waiting_attestation | `Attested | `Unattested | `Not_selected | `Unseen]

  type shard_index = int

  type slot_header = {
    slot_id : slot_id;
    commitment : Cryptobox.Commitment.t;
    status : header_status;
  }

  type profile =
    | Attestor of Tezos_crypto.Signature.public_key_hash
    | Producer of {slot_index : int}

  type profiles = profile list

  type with_proof = {with_proof : bool}

  (* Auxiliary functions.  *)

  (* Encodings associated  to the types. *)

  let slot_id_encoding =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4396
        Reuse protocol encodings. *)
    let open Data_encoding in
    conv
      (fun {slot_level; slot_index} -> (slot_level, slot_index))
      (fun (slot_level, slot_index) -> {slot_level; slot_index})
      (obj2 (req "slot_level" int32) (req "slot_index" uint8))

  let slot_encoding = Data_encoding.bytes

  let attestable_slots_encoding : attestable_slots Data_encoding.t =
    let open Data_encoding in
    union
      [
        case
          ~title:"attestable_slots_set"
          (Tag 0)
          (obj2
             (req "kind" (constant "attestable_slots_set"))
             (req "attestable_slots_set" Data_encoding.(list bool)))
          (function Attestable_slots slots -> Some ((), slots) | _ -> None)
          (function (), slots -> Attestable_slots slots);
        case
          ~title:"not_in_committee"
          (Tag 1)
          (obj1 (req "kind" (constant "not_in_committee")))
          (function Not_in_committee -> Some () | _ -> None)
          (function () -> Not_in_committee);
      ]

  let header_status_encoding : header_status Data_encoding.t =
    let open Data_encoding in
    union
      [
        case
          ~title:"waiting_attestation"
          (Tag 0)
          (obj1 (req "status" (constant "waiting_attestation")))
          (function `Waiting_attestation -> Some () | _ -> None)
          (function () -> `Waiting_attestation);
        case
          ~title:"attested"
          (Tag 1)
          (obj1 (req "status" (constant "attested")))
          (function `Attested -> Some () | _ -> None)
          (function () -> `Attested);
        case
          ~title:"unattested"
          (Tag 2)
          (obj1 (req "status" (constant "unattested")))
          (function `Unattested -> Some () | _ -> None)
          (function () -> `Unattested);
        case
          ~title:"not_selected"
          (Tag 3)
          (obj1 (req "status" (constant "not_selected")))
          (function `Not_selected -> Some () | _ -> None)
          (function () -> `Not_selected);
        case
          ~title:"unseen"
          (Tag 4)
          (obj1 (req "status" (constant "unseen")))
          (function `Unseen -> Some () | _ -> None)
          (function () -> `Unseen);
      ]

  let slot_header_encoding =
    let open Data_encoding in
    conv
      (fun {slot_id; commitment; status} -> (slot_id, (commitment, status)))
      (fun (slot_id, (commitment, status)) -> {slot_id; commitment; status})
      (merge_objs
         slot_id_encoding
         (merge_objs
            (obj1 (req "commitment" Cryptobox.Commitment.encoding))
            header_status_encoding))

  let equal_profile prof1 prof2 =
    match (prof1, prof2) with
    | Attestor p1, Attestor p2 ->
        Tezos_crypto.Signature.Public_key_hash.equal p1 p2
    | Producer {slot_index = s1}, Producer {slot_index = s2} -> Int.equal s1 s2
    | Attestor _, _ | Producer _, _ -> false

  let profile_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Attestor with pkh"
          (Tag 0)
          (obj2
             (req "kind" (constant "attestor"))
             (req
                "public_key_hash"
                Tezos_crypto.Signature.Public_key_hash.encoding))
          (function Attestor attest -> Some ((), attest) | _ -> None)
          (function (), attest -> Attestor attest);
        case
          ~title:"Slot producer"
          (Tag 1)
          (obj2 (req "kind" (constant "producer")) (req "slot_index" int31))
          (function
            | Producer {slot_index} -> Some ((), slot_index) | _ -> None)
          (function (), slot_index -> Producer {slot_index});
      ]

  let profiles_encoding =
    let open Data_encoding in
    list profile_encoding

  let with_proof_encoding =
    let open Data_encoding in
    conv
      (fun {with_proof} -> with_proof)
      (fun with_proof -> {with_proof})
      (obj1 (req "with_proof" bool))

  (* String parameters queries. *)

  let header_status_arg =
    let destruct s =
      let s = `O [("status", `String s)] in
      try Ok (Data_encoding.Json.destruct header_status_encoding s)
      with _ -> Error "Cannot parse header status value"
    in
    let construct = Data_encoding.Binary.to_string_exn header_status_encoding in
    Tezos_rpc.Arg.make ~name:"header_status" ~destruct ~construct ()

  let slot_id_query =
    let open Tezos_rpc in
    let open Query in
    query (fun slot_level slot_index -> (slot_level, slot_index))
    |+ opt_field "slot_level" Arg.int32 fst
    |+ opt_field "slot_index" Arg.int snd
    |> seal

  let opt_header_status_query =
    let open Tezos_rpc in
    let open Query in
    query (fun header_status -> header_status)
    |+ opt_field "status" header_status_arg (fun hs -> hs)
    |> seal
end

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
    ; input : Types.profiles
    ; output : unit
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:"Update the list of profiles tracked by the DAL node"
    ~query:Tezos_rpc.Query.empty
    ~input:Types.profiles_encoding
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
