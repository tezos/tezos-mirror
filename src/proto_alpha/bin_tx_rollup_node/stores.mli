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
  (** The type of store for Tezos block hashes *)
  type t

  (** Returns [true] if the Tezos block hash has a L2 block hash associated in
      the store. *)
  val mem : t -> Block_hash.t -> bool Lwt.t

  (** Returns the L2 block hash associated to a Tezos block hash in the store,
      or [None] otherwise. *)
  val find : t -> Block_hash.t -> L2block.hash option Lwt.t

  (** Add an association from a Tezos block hash to an L2 block hash in the
      store. If [flush] (default to [true]) is set, the index is written on disk
      right away. *)
  val add : ?flush:bool -> t -> Block_hash.t -> L2block.hash -> unit Lwt.t
end

(** An index store to map L2 block level to L2 block hashes. It is composed
    of an index only. *)
module Level_store : sig
  (** The type of store for L2 block levels *)
  type t

  (** Returns [true] if the L2 block level exists in the store. *)
  val mem : t -> L2block.level -> bool Lwt.t

  (** Returns the L2 block hash associated to a L2 block level in the store,
      or [None] otherwise. *)
  val find : t -> L2block.level -> L2block.hash option Lwt.t

  (** Add an association from a L2 block level to an L2 block hash in the
      store. If [flush] (default to [true]) is set, the index is written on disk
      right away. *)
  val add : ?flush:bool -> t -> L2block.level -> L2block.hash -> unit Lwt.t
end

module Head_store : sig
  (** The type of store for the head. *)
  type t

  (** Reads the current L2 head block hash from the disk. Returns [None] if the
      file does not exist or if it is corrupted. *)
  val read : t -> L2block.hash option Lwt.t

  (** Write the head block hash to disk. *)
  val write : t -> L2block.hash -> unit tzresult Lwt.t
end
