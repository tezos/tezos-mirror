(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type unsigned_chunk = private {
  value : bytes;
  number : Ethereum_types.quantity;
  nb_chunks : int;
  chunk_index : int;
}

(* A signed blueprint chunk *)
type t

(** [unsafe_drop_signature chunk] gives back the content of [chunk]
    {e without checking if its signature is valid}.  See {!check_signature} if
    you want to get the unsigned content iff the signature is correct. *)
val unsafe_drop_signature : t -> unsigned_chunk

(** [check_signature pubkey chunk] will return the (unsigned) chunk content in
    the case that it was indeed signed for [pubkey]. Otherwise it returns an
    error. See {!unsafe_drop_signature} if you want to skip the signature
    verification and just get the unsigned content. *)
val check_signature : Signature.public_key -> t -> unsigned_chunk tzresult

val chunk_encoding : t Data_encoding.t

(** [chunk_to_rlp chunk] encodes a chunk into its RLP format. *)
val chunk_to_rlp : t -> Rlp.item

(** [chunk_of_external_message msg] attempts to decode [msg] as a blueprint
    chunk. *)
val chunk_of_external_message : [`External of string] -> t tzresult

(** [sign ~signer ~chunks] serializes and signs a list of chunks. *)
val sign :
  signer:Signer.t -> chunks:unsigned_chunk list -> t list tzresult Lwt.t

(** [create_inbox_payload ~smart_rollup_address ~chunks] encodes the chunks into
    message(s) that can be read from the inbox by the kernel. *)
val create_inbox_payload :
  smart_rollup_address:string -> chunks:t list -> Blueprint_types.payload

(** [create_dal_payloads chunks] encodes the chunks into messages that
    can be read from DAL slots by the kernel. The chunks are prefixed
    by a tag. *)
val create_dal_payloads : t list -> string list

(** [maximum_usable_size_in_blueprint chunks_count] returns the available space
    for transactions in a blueprint composed of [chunks_count] chunks. *)
val maximum_usable_space_in_blueprint : int -> int

(* [maximum_chunks_per_l1_level] is the maximum number of chunks the inbox of a L1 block can
   hold at once. *)
val maximum_chunks_per_l1_level : int

type kernel_blueprint = {
  parent_hash : Ethereum_types.block_hash;
  delayed_transactions : Ethereum_types.hash list;
  transactions : string list;
  timestamp : Time.Protocol.t;
}

(** [make_blueprint_chunks ~number kernel_blueprint] serializes the
    [kernel_blueprint] whose number is [number] and splits the result into
    chunks small enough to fit in inbox messages. *)
val make_blueprint_chunks :
  number:Ethereum_types.quantity -> kernel_blueprint -> unsigned_chunk list

(** [kernel_blueprint_parent_hash_of_payload sequencer bytes]
    partially decodes fields of a {!kernel_blueprint} and return the
    {!kernel_blueprint.parent_hash}. Verify the signature of each
    chunk against public key [sequencer]. *)
val kernel_blueprint_parent_hash_of_payload :
  Signature.public_key ->
  Blueprint_types.payload ->
  Ethereum_types.block_hash option

type error += Not_a_blueprint
