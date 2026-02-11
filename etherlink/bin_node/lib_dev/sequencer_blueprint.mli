(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Not_a_blueprint
  | Bad_chunk_index of {expected : int; actual : int}
  | Bad_nb_chunks of {expected : int; actual : int; chunk_index : int}

type unsigned_chunk = private {
  value : bytes;
  number : Ethereum_types.quantity;
  nb_chunks : int;
  chunk_index : int;
}

type chunked_blueprint

(** [nb_chunks blueprint] computes in constant time the number of
    chunks of [blueprint]. *)
val nb_chunks : chunked_blueprint -> int

(* Invariants:
   - the nb_chunks field of each unsigned chunk is the length of the list,
   - the chunk_index field of the ith chunk of the list is i.
*)
type unsigned_chunked_blueprint = private unsigned_chunk list

(** [unsafe_drop_signatures chunks] gives back the content of [chunks]
    {e without checking if their signatures are valid}.  See
    {!check_signatures} if you want to get the unsigned content iff the
    signature is correct. *)
val unsafe_drop_signatures : chunked_blueprint -> unsigned_chunked_blueprint

(** [check_signatures pubkey chunks] will return the (unsigned) chunk content in
    the case that they were indeed signed for [pubkey]. Otherwise it returns an
    error. See {!unsafe_drop_signatures} if you want to skip the signature
    verification and just get the unsigned content. *)
val check_signatures :
  Signature.public_key ->
  chunked_blueprint ->
  unsigned_chunked_blueprint tzresult

val unsigned_chunked_blueprint_encoding :
  unsigned_chunked_blueprint Data_encoding.t

val chunked_blueprint_encoding : chunked_blueprint Data_encoding.t

(** [chunks_of_external_messages payload] attempts to decode payload as a chunked blueprint. *)
val chunks_of_external_messages :
  Blueprint_types.payload -> chunked_blueprint tzresult

(** [sign ~signer ~chunks] serializes and signs a list of chunks. *)
val sign :
  signer:Signer.t ->
  chunks:unsigned_chunked_blueprint ->
  chunked_blueprint tzresult Lwt.t

(** [create_inbox_payload ~smart_rollup_address ~chunks] encodes the chunks into
    message(s) that can be read from the inbox by the kernel. *)
val create_inbox_payload :
  smart_rollup_address:string ->
  chunks:chunked_blueprint ->
  Blueprint_types.payload

(** [create_dal_payloads chunks] encodes the chunks into messages that
    can be read from DAL slots by the kernel. The chunks are prefixed
    by a tag. *)
val create_dal_payloads : chunked_blueprint -> string list

(** [maximum_usable_size_in_blueprint chunks_count] returns the available space
    for transactions in a blueprint composed of [chunks_count] chunks. *)
val maximum_usable_space_in_blueprint : int -> int

(* [maximum_chunks_per_l1_level] is the maximum number of chunks the inbox of a L1 block can
   hold at once. *)
val maximum_chunks_per_l1_level : int

type blueprint_version = Legacy

type kernel_blueprint = {
  version : blueprint_version;
  parent_hash : Ethereum_types.block_hash;
  delayed_transactions : Ethereum_types.hash list;
  transactions : string list;
  timestamp : Time.Protocol.t;
}

(** [make_blueprint_chunks ~number kernel_blueprint] serializes the
    [kernel_blueprint] whose number is [number] and splits the result into
    chunks small enough to fit in inbox messages. *)
val make_blueprint_chunks :
  number:Ethereum_types.quantity ->
  kernel_blueprint ->
  unsigned_chunked_blueprint

(** [kernel_blueprint_parent_hash_of_payload sequencer bytes]
    partially decodes fields of a {!kernel_blueprint} and return the
    {!kernel_blueprint.parent_hash}. Verify the signature of each
    chunk against public key [sequencer]. *)
val kernel_blueprint_parent_hash_of_payload :
  Signature.public_key ->
  Blueprint_types.payload ->
  Ethereum_types.block_hash option tzresult

val to_rlp : Blueprint_types.payload -> Rlp.item tzresult
