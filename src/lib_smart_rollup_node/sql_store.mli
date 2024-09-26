(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Type of store (a handler to the underlying database). *)
type 'a t

(** Read/write store {!t}. *)
type rw = Store_sigs.rw t

(** Read only store {!t}. *)
type ro = Store_sigs.ro t

(** Name of SQLite file in data directory. *)
val sqlite_file_name : string

(** [init mode ~data_dir] initializes the store and returns it. *)
val init : 'a Store_sigs.mode -> data_dir:string -> 'a t tzresult Lwt.t

(** Close the store by freeing all resources and closing database
    connections. *)
val close : _ t -> unit Lwt.t

(** Returns a read-only version of the store. *)
val readonly : _ t -> ro
