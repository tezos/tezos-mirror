(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Module type for new durable storage (NDS) backends.

    Any concrete NDS implementation (in-memory, prove, verify) must satisfy
    this signature. The structure mirrors
    {!Octez_riscv_nds_common.Intf.NORMAL}: a [Registry] sub-module for
    registry-level operations and a [Database] sub-module for per-database
    key-value operations.

    Concrete backends such as [Octez_riscv_nds_memory.Normal] satisfy this
    signature directly, with no adapter needed. *)
module type S = sig
  module Registry : sig
    (** Abstract handle to the storage backend. *)
    type t

    (** [hash registry] computes the Merkle root hash of [registry]. *)
    val hash : t -> bytes tzresult

    (** [size registry] returns the number of databases currently held in
        the registry. *)
    val size : t -> int64 tzresult

    (** [resize registry n] adjusts the registry to contain exactly [n]
        databases. *)
    val resize : t -> int64 -> unit tzresult

    (** [copy_database registry ~src ~dst] duplicates all contents of the
        database at index [src] to index [dst]. *)
    val copy_database : t -> src:int64 -> dst:int64 -> unit tzresult

    (** [move_database registry ~src ~dst] transfers the database at index
        [src] to index [dst]. *)
    val move_database : t -> src:int64 -> dst:int64 -> unit tzresult

    (** [clear registry db_index] removes all entries from database
        [db_index]. *)
    val clear : t -> int64 -> unit tzresult
  end

  module Database : sig
    (** [exists registry ~db_index ~key] checks whether [key] exists in
        the database at [db_index]. *)
    val exists : Registry.t -> db_index:int64 -> key:bytes -> bool tzresult

    (** [set registry ~db_index ~key ~value] replaces the entire value
        associated with [key] in database [db_index]. *)
    val set :
      Registry.t -> db_index:int64 -> key:bytes -> value:bytes -> unit tzresult

    (** [write registry ~db_index ~key ~offset ~value] writes [value]
        starting at [offset] into the value associated with [key]. *)
    val write :
      Registry.t ->
      db_index:int64 ->
      key:bytes ->
      offset:int64 ->
      value:bytes ->
      int64 tzresult

    (** [read registry ~db_index ~key ~offset ~len] reads up to [len]
        bytes starting at [offset] from the value associated with [key]. *)
    val read :
      Registry.t ->
      db_index:int64 ->
      key:bytes ->
      offset:int64 ->
      len:int64 ->
      bytes tzresult

    (** [value_length registry ~db_index ~key] returns the length of the
        value associated with [key] in database [db_index]. *)
    val value_length :
      Registry.t -> db_index:int64 -> key:bytes -> int64 tzresult

    (** [delete registry ~db_index ~key] removes [key] and its associated
        value from database [db_index]. *)
    val delete : Registry.t -> db_index:int64 -> key:bytes -> unit tzresult

    (** [hash registry ~db_index] returns the Merkle root hash of the
        database at [db_index]. *)
    val hash : Registry.t -> db_index:int64 -> bytes tzresult
  end
end
