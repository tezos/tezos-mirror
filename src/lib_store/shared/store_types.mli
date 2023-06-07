(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** {1 Global types used in the store library} *)

(** The type used to describe a block pointer i.e. its hash and level. *)
type block_descriptor = Block_hash.t * int32

(** Equality function for {!block_descriptor}. *)
val block_descriptor_equal : block_descriptor -> block_descriptor -> bool

(** Encoding for {!block_descriptor}. *)
val block_descriptor_encoding : block_descriptor Data_encoding.t

(** The type for chain configuration. *)
type chain_config = {
  history_mode : History_mode.t;
  genesis : Genesis.t;
  expiration : Time.Protocol.t option;
}

(** Equality function for {!chain_config}. *)
val chain_config_equal : chain_config -> chain_config -> bool

(** Encoding for {!chain_config}. *)
val chain_config_encoding : chain_config Data_encoding.t

(** Pretty-printer for [block_descriptor] *)
val pp_block_descriptor : Format.formatter -> block_descriptor -> unit

(** The type used to store an invalid block's value. We only retain
    the level and the errors encountered during validation. These
    values should be indexed by the block's hash. *)
type invalid_block = {level : int32; errors : Error_monad.error list}

(** Equality function on {!invalid_block}. Warning: uses polymorphic comparison on errors. *)
val invalid_block_equal : invalid_block -> invalid_block -> bool

(** Encoding for {!invalid_block}. *)
val invalid_block_encoding : invalid_block Data_encoding.t

(** Module [Block_lru_cache] implements a lwt LRU cache map indexed by
    block hashes. *)
module Block_lru_cache : Aches_lwt.Lache.MAP_OPTION with type key = Block_hash.t

(** Module [Protocol_levels] represents an associative map of protocol
    levels to corresponding blocks which supposedly activate new
    protocols, that is to say blocks that acknowledge a protocol
    change in the next block. *)
module Protocol_levels : sig
  include Map.S with type key = int

  (** The type for protocol info.

    [expect_predecessor_context] is a flag which reflects what is
    referenced by the context hash of a block, depending on its
    protocol. If the flag is true, the block contains the predecessors
    context (context before the application of the block). Otherwise,
    it contains the resulting context hash of the application of the
    block. An activation block refers to a block which activated a new
    protocol [N+1]. Thus, the block header related to that activation
    block will have a protocol level which differs from its
    predecessor. As this block aims to activate a new protocol, the
    [next_protocol] and [protocol] fields from the metadata differs:
    [next_protocol] refers to the hash of the activated protocol. As a
    consequence, this block handled by protocol [N] and is the last
    block of that protocol.
*)
  type protocol_info = {
    protocol : Protocol_hash.t;
    activation_block : block_descriptor;
    expect_predecessor_context : bool;
  }

  (** Equality on protocol levels maps. *)
  val equal : protocol_info t -> protocol_info t -> bool

  (** Encoding for the protocol level's association map. *)
  val encoding : protocol_info t Data_encoding.t

  module Legacy : sig
    type commit_info = {
      author : string;
      message : string;
      test_chain_status : Test_chain_status.t;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      data_merkle_root : Context_hash.t;
      parents_contexts : Context_hash.t list;
    }

    type activation_block = {
      block : block_descriptor;
      protocol : Protocol_hash.t;
      commit_info : commit_info option;
    }

    include Map.S with type key = int

    (** Equality on legacy protocol levels maps. *)
    val equal : activation_block t -> activation_block t -> bool

    (** Encoding for the protocol level's association map. *)
    val encoding : activation_block t Data_encoding.t
  end
end
