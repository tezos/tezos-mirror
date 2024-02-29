(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [init_store ()] advertizes that the store is being initialized for the
    first time. *)
val init_store : unit -> unit Lwt.t

(** [applied_migration name] advertizes that the migration [name] is applied
    on a store that was previously missing it. *)
val applied_migration : string -> unit Lwt.t

(** [assume_old_store ()] advertizes that the EVM node is promoting its store
    to be compatible with the migrations table. It does that when its store is
    missing the migrations table, but has the tables expected from the first
    migration. *)
val assume_old_store : unit -> unit Lwt.t

(** [migration_from_the_future ~applied ~known] advertizes that there is more
    migrations applied to the store than known from the EVM node, which
    suggests the EVM node is outdated. *)
val migrations_from_the_future : applied:int -> known:int -> unit Lwt.t
