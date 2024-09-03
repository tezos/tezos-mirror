(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Chunk of {
      value : bytes;
      number : Ethereum_types.quantity;
      nb_chunks : int;
      chunk_index : int;
      signature : Signature.t;
    }

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

(** [create ~smart_rollup_address ~rlp_chunks] encodes the chunks into
    message(s) that can be read by the kernel. *)
val create :
  smart_rollup_address:string -> chunks:t list -> Blueprint_types.payload

(** [maximum_usable_size_in_blueprint chunks_count] returns the available space
    for transactions in a blueprint composed of [chunks_count] chunks. *)
val maximum_usable_space_in_blueprint : int -> int

(* [maximum_chunks_per_l1_level] is the maximum number of chunks a L1 block can
   hold at once. *)
val maximum_chunks_per_l1_level : int
