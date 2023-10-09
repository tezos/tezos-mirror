(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Cryptobox = Tezos_crypto_dal.Cryptobox

type level = int32

type slot_index = int

module Topic = struct
  type t = {slot_index : int; pkh : Signature.Public_key_hash.t}

  type topic = t

  let compare topic {slot_index; pkh} =
    let c = Int.compare topic.slot_index slot_index in
    if c <> 0 then c else Signature.Public_key_hash.compare topic.pkh pkh

  module Cmp = struct
    type nonrec t = t

    let compare = compare
  end

  include Compare.Make (Cmp)
  module Set = Set.Make (Cmp)
  module Map = Map.Make (Cmp)

  let pp fmt {pkh; slot_index} =
    Format.fprintf
      fmt
      "{ pkh=%a; slot_index=%d }"
      Signature.Public_key_hash.pp
      pkh
      slot_index

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5607

     Bound / add checks for bounds of these encodings *)
  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun ({slot_index; pkh} : topic) -> (slot_index, pkh))
      (fun (slot_index, pkh) -> {slot_index; pkh})
      (obj2
         (req "slot_index" uint8)
         (req "pkh" Signature.Public_key_hash.encoding))
end

(* Declaration of types used as inputs and/or outputs. *)
type slot_id = {slot_level : level; slot_index : slot_index}

type slot_set = {slots : bool list; published_level : int32}

type attestable_slots = Attestable_slots of slot_set | Not_in_committee

type header_status =
  [ `Waiting_attestation
  | `Attested
  | `Unattested
  | `Not_selected
  | `Unseen_or_not_finalized ]

type shard_index = int

type slot_header = {
  slot_id : slot_id;
  commitment : Cryptobox.Commitment.t;
  status : header_status;
}

type operator_profile =
  | Attester of Tezos_crypto.Signature.public_key_hash
  | Producer of {slot_index : int}

type operator_profiles = operator_profile list

type profiles = Bootstrap | Operator of operator_profiles

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
        (obj3
           (req "kind" (constant "attestable_slots_set"))
           (req "attestable_slots_set" Data_encoding.(list bool))
           (req "published_level" int32))
        (function
          | Attestable_slots {slots; published_level} ->
              Some ((), slots, published_level)
          | _ -> None)
        (function
          | (), slots, published_level ->
              Attestable_slots {slots; published_level});
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
        ~title:"unseen_or_not_finalized"
        (Tag 4)
        (obj1 (req "status" (constant "unseen")))
        (function `Unseen_or_not_finalized -> Some () | _ -> None)
        (function () -> `Unseen_or_not_finalized);
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

let operator_profile_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Attester with pkh"
        (Tag 0)
        (obj2
           (req "kind" (constant "attester"))
           (req
              "public_key_hash"
              Tezos_crypto.Signature.Public_key_hash.encoding))
        (function Attester attest -> Some ((), attest) | _ -> None)
        (function (), attest -> Attester attest);
      case
        ~title:"Slot producer"
        (Tag 1)
        (obj2 (req "kind" (constant "producer")) (req "slot_index" int31))
        (function Producer {slot_index} -> Some ((), slot_index) | _ -> None)
        (function (), slot_index -> Producer {slot_index});
    ]

let profiles_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Boostrap node"
        (Tag 1)
        (obj1 (req "kind" (constant "bootstrap")))
        (function Bootstrap -> Some () | _ -> None)
        (function () -> Bootstrap);
      case
        ~title:"Operator"
        (Tag 2)
        (obj2
           (req "kind" (constant "operator"))
           (req "operator_profiles" (list operator_profile_encoding)))
        (function
          | Operator operator_profiles -> Some ((), operator_profiles)
          | _ -> None)
        (function (), operator_profiles -> Operator operator_profiles);
    ]

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

module Store = struct
  (** Data kind stored in DAL. *)
  type kind = Commitment | Header_status | Slot_id | Slot | Profile

  let encoding : kind Data_encoding.t =
    Data_encoding.string_enum
      [
        ("commitment", Commitment);
        ("header_status", Header_status);
        ("slot_id", Slot_id);
        ("slot", Slot);
        ("profile", Profile);
      ]

  let to_string data_kind =
    Data_encoding.Binary.to_string_exn encoding data_kind
end
