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

  type header_status =
    [`Waiting_attestation | `Attested | `Unattested | `Not_selected | `Unseen]

  type slot_header = {
    slot_id : slot_id;
    commitment : Cryptobox.Commitment.t;
    status : header_status;
  }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4442
     Add missing profiles. *)
  type profile = Attestor of Tezos_crypto.Signature.public_key_hash

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

  let equal_profile (Attestor p1) (Attestor p2) =
    Tezos_crypto.Signature.Public_key_hash.( = ) p1 p2

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
          (function Attestor attest -> Some ((), attest))
          (function (), attest -> Attestor attest);
      ]

  (* String parameters queries. *)

  let slot_id_query =
    let open Tezos_rpc in
    let open Query in
    query (fun slot_level slot_index -> (slot_level, slot_index))
    |+ opt_field "slot_level" Arg.int32 fst
    |+ opt_field "slot_index" Arg.int snd
    |> seal
end

let post_slots :
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
    Tezos_rpc.Path.(open_root / "slots")

let patch_slot :
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
    Tezos_rpc.Path.(open_root / "slots" /: Cryptobox.Commitment.rpc_arg)

let get_slot :
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
    Tezos_rpc.Path.(open_root / "slots" /: Cryptobox.Commitment.rpc_arg)

let get_slot_commitment_proof :
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
      open_root / "slots" /: Cryptobox.Commitment.rpc_arg / "proof")

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
      "Return the known headers for the the slot whose commitment is given."
    ~query:Types.slot_id_query
    ~output:(Data_encoding.list Types.slot_header_encoding)
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "headers")

let patch_profile :
    < meth : [`PATCH]
    ; input : Types.profile
    ; output : unit
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:"Update the list of profiles tracked by the DAL node"
    ~query:Tezos_rpc.Query.empty
    ~input:Types.profile_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "profiles")

let get_profiles :
    < meth : [`GET]
    ; input : unit
    ; output : Types.profile list
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the list of current profiles tracked by the DAL node"
    ~query:Tezos_rpc.Query.empty
    ~output:(Data_encoding.list Types.profile_encoding)
    Tezos_rpc.Path.(open_root / "profiles")
