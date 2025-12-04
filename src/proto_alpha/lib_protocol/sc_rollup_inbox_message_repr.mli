(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module exposes a type {!t} that represents inbox messages. Inbox
    messages are produced by the Layer 1 protocol and are encoded using the
    {!serialize} function, before being added to a smart-contract rollup's inbox.

    They are part of the [Rollup Management Protocol] that defines the
    communication protocol for exchanging messages between Layer 1 and Layer 2
    for a smart-contract rollup.

    There are two types of inbox messages: external and internal.

     Internal messages originate from Layer 1 smart-contract and consist of:
     - [payload] the parameters passed to the smart-contract rollup.
     - [sender] the Layer 1 contract caller.
     - [source] the public key hash used for originating the transaction.

    External messages originate from the [Sc_rollup_add_messages]
    manager-operation and consists of strings. The Layer 2 node is responsible
    for decoding and interpreting these messages.
  *)

(** [internal_inbox_message] represent an internal message in a inbox (L1 ->
    L2). This is not inline so it can easily be used by
    {!Sc_rollup_costs.cost_serialize_internal_inbox_message}. *)
type internal_inbox_message =
  | Transfer of {
      payload : Script_repr.expr;
          (** A Micheline value containing the parameters passed to the rollup. *)
      sender : Contract_hash.t;
          (** The contract hash of an Layer 1 originated contract sending a message
          to the rollup. *)
      source : Signature.public_key_hash;
          (** The implicit account that originated the transaction. *)
      destination : Sc_rollup_repr.Address.t;
          (** The destination, as a rollup address, for the message. *)
    }
  | Start_of_level
      (** Internal message put at the beginning of each inbox's level. *)
  | End_of_level  (** Internal message put at the end of each inbox's level. *)
  | Info_per_level of {
      predecessor_timestamp : Time.t;
          (** Timestamp of the predecessor block where this message is
              pushed. *)
      predecessor : Block_hash.t;
          (** Predecessor of the block this message is pushed. *)
    }
  | Protocol_migration of string
  | Dal_attested_slots of {
      published_level : Raw_level_repr.t;
      number_of_slots : int;
      slot_size : int;
      page_size : int;
      slots_by_publisher :
        Dal_slot_index_repr.t list Signature.Public_key_hash.Map.t;
    }
      (** Internal message communicating the attested slots for the given
          [published_level]. Attested slots are grouped by
          [slots_by_publisher]. A slot is attested if and only if it appears in
          the map's codomain. *)

(** A type representing messages from Layer 1 to Layer 2. Internal ones are
    originated from Layer 1 smart-contracts and external ones are messages from
    an external manager operation. *)
type t = Internal of internal_inbox_message | External of string

type serialized = private string

(** Encoding for messages from Layer 1 to Layer 2 *)
val encoding : t Data_encoding.t

(** [serialize msg] encodes the inbox message [msg] in binary format. *)
val serialize : t -> serialized tzresult

(** [deserialize bs] decodes [bs] as an inbox_message [t]. *)
val deserialize : serialized -> t tzresult

val unsafe_of_string : string -> serialized

val unsafe_to_string : serialized -> string

module Hash : S.HASH

(** [hash_serialized_message payload] is the hash of [payload]. It is used by
    {!Sc_rollup_inbox_merkelized_payload_hashes_repr.t}. *)
val hash_serialized_message : serialized -> Hash.t

(** {!serialized} representation of [Internal [Start_of_level]]. *)
val start_of_level_serialized : serialized

(** {!serialized} representation of [Internal [End_of_level]]. *)
val end_of_level_serialized : serialized

(** {!info_per_level_serialized ~predecessor_timestamp ~predecessor} is the serialized representation of the internal message for {!Info_per_level}. *)
val info_per_level_serialized :
  predecessor_timestamp:Time.t -> predecessor:Block_hash.t -> serialized

(** [dal_attested_slots_serialized ~published_level ~number_of_slots ~slot_size
    ~page_size ~slots_by_publisher] is the serialized representation of the
    internal message for {!Dal_attested_slots}. *)
val dal_attested_slots_serialized :
  published_level:Raw_level_repr.t ->
  number_of_slots:int ->
  slot_size:int ->
  page_size:int ->
  slots_by_publisher:Dal_slot_index_repr.t list Signature.Public_key_hash.Map.t ->
  serialized
