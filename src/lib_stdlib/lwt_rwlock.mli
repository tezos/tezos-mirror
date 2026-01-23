(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A read-write lock for Lwt with writer priority.

    This module provides a read-write lock that allows multiple concurrent
    readers or a single exclusive writer. Writers have priority: when a writer
    is waiting for the lock, new readers will block until the writer has
    acquired and released the lock.

    {b Warning: this lock is not re-entrant.} An Lwt thread that tries to
    acquire a lock it already holds can deadlock. *)

(** The type of read-write locks. *)
type t

(** [create ()] creates a new read-write lock. *)
val create : unit -> t

(** [read_lock t] acquires a read lock. Multiple readers can hold the lock
    concurrently. Blocks if a writer holds the lock or is waiting for it
    (writer priority). *)
val read_lock : t -> unit Lwt.t

(** [read_unlock t] releases a read lock. *)
val read_unlock : t -> unit

(** [write_lock t] acquires an exclusive write lock. Blocks until all readers
    and any current writer release the lock. Has priority over new readers:
    once a writer starts waiting, new readers will block. *)
val write_lock : t -> unit Lwt.t

(** [write_unlock t] releases the write lock. *)
val write_unlock : t -> unit

(** [with_read_lock t f] executes [f ()] while holding the read lock.
    The lock is released when [f ()] completes, even if it raises an
    exception. *)
val with_read_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** [with_write_lock t f] executes [f ()] while holding the write lock.
    The lock is released when [f ()] completes, even if it raises an
    exception. *)
val with_write_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
