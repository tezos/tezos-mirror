(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** Describes the different representations that can be stored persistently. *)

(** {2 Signatures} *)

(** A store composed of a single file on disk *)
module type SINGLETON_STORE = sig
  (** The type of the singleton store. *)
  type t

  (** The type of values stored in this singleton store. *)
  type value

  (** Reads the current value from the disk. Returns [None] if the
      file does not exist or if it is corrupted. *)
  val read : t -> value option Lwt.t

  (** Write the value to disk. *)
  val write : t -> value -> unit tzresult Lwt.t

  (** Deletes the value from the disk. *)
  val delete : t -> unit Lwt.t
end

(** An index store mapping keys to values. It is composed of an index only. *)
module type INDEXABLE_STORE = sig
  (** The type of store build in indexes *)
  type t

  (** The type of keys for the *)
  type key

  (** The type of values stored in the index *)
  type value

  (** Returns [true] if the key has a value associated in
      the store. *)
  val mem : t -> key -> bool Lwt.t

  (** Returns the value associated to a key in the store,
      or [None] otherwise. *)
  val find : t -> key -> value option Lwt.t

  (** Add an association from a key to a value in the
      store. If [flush] (default to [true]) is set, the index is written on disk
      right away. *)
  val add : ?flush:bool -> t -> key -> value -> unit Lwt.t
end

(** An index store mapping keys to values. Keys are associated to optional
    values in the index which allows them to be removed. *)
module type INDEXABLE_REMOVABLE_STORE = sig
  include INDEXABLE_STORE

  (** Removes an association from the store. Does nothing if the key was not
      registered. *)
  val remove : ?flush:bool -> t -> key -> unit Lwt.t
end

(** {2 Indexed stores}  *)

(** A persistent store on disk for storing L2 blocks. It is composed of an index
    file and a data file which contains the actual blocks. The keys of the index
    are the L2 block hashes. *)
module L2_block_store : sig
  (** The type of store for L2 blocks *)
  type t

  (** Returns [true] if the L2 block hash exists in the index, i.e. if the block
      exists in the store. *)
  val mem : t -> L2block.hash -> bool Lwt.t

  (** Returns the predecessor of the block (by only querying the index, without
      reading the block data). *)
  val predecessor : t -> L2block.hash -> L2block.hash option Lwt.t

  (** Returns the context hash of the block (by only querying the index, without
      reading the block data). *)
  val context :
    t -> L2block.hash -> Protocol.Tx_rollup_l2_context_hash.t option Lwt.t

  (** Read a block from the file on disk, given a L2 block hash. Returns [None]
      if the block is not stored. *)
  val read_block : t -> L2block.hash -> L2block.t option Lwt.t

  (** [append_block ?flush store block] stores the [block] in [store] updating
      its index and flushing if [flush] is set to [true] (defaults to [true]).*)
  val append_block : ?flush:bool -> t -> L2block.t -> unit Lwt.t
end

(** {2 Pure index stores}  *)

(** An index store to map Tezos block hashes to L2 block hashes. It is composed
    of an index only. This store is used to remember which Tezos blocks have been
    processed by the Tx rollup node. When there is no inbox for a Tezos block, we
    associate to it the L2 block of its predecessor. *)
module Tezos_block_store : sig
  type value = {
    l2_block : L2block.hash;
    level : int32;
    predecessor : Block_hash.t;
  }

  include INDEXABLE_STORE with type key := Block_hash.t and type value := value
end

(** An index store to map L2 block level to L2 block hashes. It is composed
    of an index only. *)
module Level_store :
  INDEXABLE_REMOVABLE_STORE
    with type key := L2block.level
     and type value := L2block.hash

(** An index store to map commitment hashes to their inclusion information. *)
module Commitment_store : sig
  type value = {
    block : Block_hash.t;
        (** Tezos block in which the commitment is included. *)
    operation : Operation_hash.t;
        (** Operation of the block in which the commitment is included. *)
  }

  include
    INDEXABLE_REMOVABLE_STORE
      with type key := Protocol.Alpha_context.Tx_rollup_commitment_hash.t
       and type value := value
end

(** {2 Singleton stores}  *)

(** A store composed of a single file on disk to store the current head *)
module Head_store : SINGLETON_STORE with type value := L2block.hash

(** A store composed of a single file on disk to store the current Tezos head *)
module Tezos_head_store : SINGLETON_STORE with type value := Block_hash.t

(** Type for on disk information about a rollup *)
type rollup_info = {
  rollup_id : Protocol.Alpha_context.Tx_rollup.t;
  origination_block : Block_hash.t;
  origination_level : int32;
}

(** A store composed of a single file on disk to store the rollup
    information. This is used to guarantee consistency between several runs of
    the Tx rollup node. *)
module Rollup_info_store : SINGLETON_STORE with type value := rollup_info

(** A store composed of a single file on disk to store the last finalized rollup
    level (on L1) *)
module Finalized_level_store :
  SINGLETON_STORE with type value := Protocol.Alpha_context.Tx_rollup_level.t

(** The type of all stores of the Tx rollup node. *)
type t = {
  blocks : L2_block_store.t;
  tezos_blocks : Tezos_block_store.t;
  levels : Level_store.t;
  commitments : Commitment_store.t;
  head : Head_store.t;
  tezos_head : Tezos_head_store.t;
  rollup_info : Rollup_info_store.t;
  finalized_level : Finalized_level_store.t;
}

(** [init ~data_dir ~readonly ~blocks_cache_size] creates or loads existing
    stores located in the directory [data_dir]. If [readonly] (defaults to
    [false]) is set, the stores can only be read. An LRU cache of size
    [blocks_cache_size] is used for reading L2 blocks. *)
val init : data_dir:string -> readonly:bool -> blocks_cache_size:int -> t Lwt.t

(** [close store] closes all the stores by closing the indexes and the
    associated opened file descriptors. *)
val close : t -> unit Lwt.t
