(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type-erased NDS handle.

    Wraps any concrete backend satisfying {!Nds_sig.S} into a single
    existential type so that callers can work with NDS generically. *)

(** Opaque NDS handle that erases the concrete backend type. *)
type t

(** [wrap impl value] packages a concrete NDS backend [impl] and its
    state [value] into an opaque {!t} handle. *)
val wrap : (module Nds_sig.S with type Registry.t = 'a) -> 'a -> t

(** {2 Dispatchers}

    Each function below unpacks the existential and delegates to the
    corresponding operation in the wrapped {!Nds_sig.S} implementation. *)

(** [size t] returns the number of databases in the registry. *)
val size : t -> int64 tzresult

(** [resize t n] adjusts the registry to contain exactly [n] databases. *)
val resize : t -> int64 -> unit tzresult

(** [copy_database t ~src ~dst] duplicates database [src] to [dst]. *)
val copy_database : t -> src:int64 -> dst:int64 -> unit tzresult

(** [move_database t ~src ~dst] moves database [src] to [dst]. *)
val move_database : t -> src:int64 -> dst:int64 -> unit tzresult

(** [clear t db_index] removes all entries from database [db_index]. *)
val clear : t -> int64 -> unit tzresult

(** [registry_hash t] returns the Merkle root hash of the registry. *)
val registry_hash : t -> bytes tzresult

(** [exists t ~db_index ~key] checks whether [key] exists in database [db_index]. *)
val exists : t -> db_index:int64 -> key:bytes -> bool tzresult

(** [read t ~db_index ~key ~offset ~len] reads bytes from database [db_index]. *)
val read :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  len:int64 ->
  bytes tzresult

(** [write t ~db_index ~key ~offset ~value] writes bytes to database [db_index].
    Returns the new total length. *)
val write :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  value:bytes ->
  int64 tzresult

(** [set t ~db_index ~key ~value] replaces the value of [key] in database [db_index]. *)
val set : t -> db_index:int64 -> key:bytes -> value:bytes -> unit tzresult

(** [delete t ~db_index ~key] removes [key] from database [db_index]. *)
val delete : t -> db_index:int64 -> key:bytes -> unit tzresult

(** [value_length t ~db_index ~key] returns the length of the value for [key]
    in database [db_index]. *)
val value_length : t -> db_index:int64 -> key:bytes -> int64 tzresult

(** [hash t ~db_index] returns the Merkle root hash of database [db_index]. *)
val hash : t -> db_index:int64 -> bytes tzresult
