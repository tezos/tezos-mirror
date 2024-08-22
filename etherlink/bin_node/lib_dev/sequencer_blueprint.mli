(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* A signed blueprint chunk *)
type t

val chunk_encoding : t Data_encoding.t

(** [chunk_to_rlp chunk] encodes a chunk into its RLP format. *)
val chunk_to_rlp : t -> Rlp.item

(** [prepare ~secret_key ~timestamp ~number ~parent_hash ~delayed_transactions
    ~transactions] creates a sequencer blueprint at [timestamp] with a given
    [number] containing [transactions], signed with [secret_key]. Returns the
    list of prepared chunks.
*)
val prepare :
  cctxt:#Client_context.wallet ->
  sequencer_key:Client_keys.sk_uri ->
  timestamp:Time.Protocol.t ->
  number:Ethereum_types.quantity ->
  parent_hash:Ethereum_types.block_hash ->
  delayed_transactions:Ethereum_types.hash list ->
  transactions:string list ->
  t list tzresult Lwt.t

(** [create_inbox_payload ~smart_rollup_address ~chunks] encodes the chunks into
    message(s) that can be read from the inbox by the kernel. *)
val create_inbox_payload :
  smart_rollup_address:string -> chunks:t list -> Blueprint_types.payload

(** [create_dal_payload chunks] encodes the chunks into a message that can be
    read from a DAL slot by the kernel. The chunks are prefixed by a tag
    and concatenated. *)
val create_dal_payload : t list -> string

(** [maximum_usable_size_in_blueprint chunks_count] returns the available space
    for transactions in a blueprint composed of [chunks_count] chunks. *)
val maximum_usable_space_in_blueprint : int -> int

(* [maximum_chunks_per_l1_level] is the maximum number of chunks the inbox of a L1 block can
   hold at once. *)
val maximum_chunks_per_l1_level : int

(* [maximum_unsigned_chunks_per_dal_slot] is the maximum number of unsigned
   chunks a DAL slot can hold at once. *)
val maximum_unsigned_chunks_per_dal_slot : int
