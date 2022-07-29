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

(** Encoding for {!block_descriptor}. *)
val block_descriptor_encoding : block_descriptor Data_encoding.t

(** The type for chain configuration. *)
type chain_config = {
  history_mode : History_mode.t;
  genesis : Genesis.t;
  expiration : Time.Protocol.t option;
}

(** Encoding for {!chain_config}. *)
val chain_config_encoding : chain_config Data_encoding.t

(** Pretty-printer for [block_descriptor] *)
val pp_block_descriptor : Format.formatter -> block_descriptor -> unit

(** The type used to store an invalid block's value. We only retain
    the level and the errors encountered during validation. These
    values should be indexed by the block's hash. *)
type invalid_block = {level : int32; errors : Error_monad.error list}

(** Encoding for {!invalid_block}. *)
val invalid_block_encoding : invalid_block Data_encoding.t

(** Module [Block_lru_cache] implements a lwt LRU cache map indexed by
    block hashes. *)
module Block_lru_cache :
  Ringo_lwt.Sigs.CACHE_MAP_OPT with type key = Block_hash.t

(** Module [Protocol_levels] represents an associative map of protocol
    levels to corresponding blocks which supposedly activate new
    protocols, that is to say blocks that acknowledge a protocol
    change in the next block. *)
module Protocol_levels : sig
  include Map.S with type key = int

  (** The type representing a subset of the commit information. These
      are used to easily check that a given [Context_hash.t], with the
      associated context not present on disk, is consistent.  It is
      used to verify that an announced protocol is indeed the one that
      was committed on disk. Fields are:
      - [author] is the commit's author;
      - [message] is the commit's message;
      - [test_chain_status] is the status of the test chain at commit
        time;
      - [data_merkle_root] is the merkle root of the context's data
        main node;
      - [parents_contexts] are the context hashes of this commit's
        parents.

      This structure should be populated with the result of
      {!Tezos_context.Context.retrieve_commit_info}. The consistency
      check is done by
      {!Tezos_context.Context.check_protocol_commit_consistency} when
      a context in imported in the leger state, for example, when
      importing a snapshot. *)
  type commit_info = {
    author : string;
    message : string;
    test_chain_status : Test_chain_status.t;
    predecessor_block_metadata_hash : Block_metadata_hash.t option;
    predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
    data_merkle_root : Context_hash.t;
    parents_contexts : Context_hash.t list;
  }

  val commit_info_of_tuple :
    Protocol_hash.t
    * string
    * string
    * Time.Protocol.t
    * Test_chain_status.t
    * Context_hash.t
    * Block_metadata_hash.t option
    * Operation_metadata_list_list_hash.t option
    * Context_hash.t list ->
    commit_info

  (** Encoding for {!commit_info}. *)
  val commit_info_encoding : commit_info Data_encoding.t

  (** The type for activation blocks.

      An activation block refers to a block which activated a new
     protocol [N+1]. Thus, the block header related to that activation
     block will have a protocol level which differs from its
     predecessor. As this block aims to activate a new protocol, the
     [next_protocol] and [protocol] fields from the metadata differs:
     [next_protocol] refers to the hash of the activated protocol. As
     a consequence, this block handled by protocol [N] and is the last
     block of that protocol.


      {b WARNING.} Commit information are optional to allow
     retro-compatibility: the LMDB legacy store does not contain such
     information. Thus, populating the protocol levels' map while
     upgrading the storage would prevent us from storing an activation
     block which is used to retrieve the [protocol] to load, and
     therefore, being unable to decode stored blocks. In the future,
     when a sufficient number of nodes have fully migrated, we can
     stitch the missing commit information by hard-coding them,
     allowing us to remove the option. *)
  type activation_block = {
    block : block_descriptor;
    protocol : Protocol_hash.t;
    commit_info : commit_info option;
  }

  (** Encoding for the protocol level's association map. *)
  val encoding : activation_block t Data_encoding.t
end
