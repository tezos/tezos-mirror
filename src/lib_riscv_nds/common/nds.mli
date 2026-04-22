(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type-erased NDS handle.

    Wraps any concrete backend satisfying {!Intf.NORMAL} into a single
    existential type so that callers can work with NDS generically.
    Since all backends share the common
    {!Nds_errors.invalid_argument_error} type in their results, no error
    conversion is required. *)

(** Opaque NDS handle that erases the concrete backend type. *)
type t

(** [wrap impl value] packages a concrete NDS backend [impl] and its
    state [value] into an opaque {!t} handle. *)
val wrap : (module Intf.NORMAL with type Registry.t = 'a) -> 'a -> t

(** {2 Dispatchers}

    Each function below unpacks the existential and delegates to the
    wrapped backend. *)

(** [size t] returns the number of databases in the registry. *)
val size : t -> (int64, Nds_errors.invalid_argument_error) result

(** [resize t n] adjusts the registry to contain exactly [n] databases. *)
val resize : t -> int64 -> (unit, Nds_errors.invalid_argument_error) result

(** [copy_database t ~src ~dst] duplicates database [src] to [dst]. *)
val copy_database :
  t ->
  src:int64 ->
  dst:int64 ->
  (unit, Nds_errors.invalid_argument_error) result

(** [move_database t ~src ~dst] moves database [src] to [dst]. *)
val move_database :
  t ->
  src:int64 ->
  dst:int64 ->
  (unit, Nds_errors.invalid_argument_error) result

(** [clear t db_index] removes all entries from database [db_index]. *)
val clear : t -> int64 -> (unit, Nds_errors.invalid_argument_error) result

(** [registry_hash t] returns the Merkle root hash of the registry. *)
val registry_hash : t -> (bytes, Nds_errors.invalid_argument_error) result

(** [exists t ~db_index ~key] checks whether [key] exists in database
    [db_index]. *)
val exists :
  t ->
  db_index:int64 ->
  key:bytes ->
  (bool, Nds_errors.invalid_argument_error) result

(** [read t ~db_index ~key ~offset ~len] reads bytes from database
    [db_index]. *)
val read :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  len:int64 ->
  (bytes, Nds_errors.invalid_argument_error) result

(** [write t ~db_index ~key ~offset ~value] writes [value] starting at
    [offset] into the value associated with [key] in database
    [db_index]. Returns the number of bytes written (i.e.
    [Bytes.length value]). *)
val write :
  t ->
  db_index:int64 ->
  key:bytes ->
  offset:int64 ->
  value:bytes ->
  (int64, Nds_errors.invalid_argument_error) result

(** [set t ~db_index ~key ~value] replaces the value of [key] in
    database [db_index]. *)
val set :
  t ->
  db_index:int64 ->
  key:bytes ->
  value:bytes ->
  (unit, Nds_errors.invalid_argument_error) result

(** [delete t ~db_index ~key] removes [key] from database [db_index]. *)
val delete :
  t ->
  db_index:int64 ->
  key:bytes ->
  (unit, Nds_errors.invalid_argument_error) result

(** [value_length t ~db_index ~key] returns the length of the value for
    [key] in database [db_index]. *)
val value_length :
  t ->
  db_index:int64 ->
  key:bytes ->
  (int64, Nds_errors.invalid_argument_error) result

(** [hash t ~db_index] returns the Merkle root hash of database
    [db_index]. *)
val hash :
  t -> db_index:int64 -> (bytes, Nds_errors.invalid_argument_error) result
