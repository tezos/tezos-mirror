(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [init_store ()] advertizes that the store is being initialized for the
    first time. *)
val init_store : unit -> unit Lwt.t

(** [applied_migration name time] advertizes that the migration [name] is
    applied in [time] on a store that was previously missing it. *)
val applied_migration : string -> Time.System.Span.t -> unit Lwt.t

(** [migration_from_the_future ~applied ~known] advertizes that there is more
    migrations applied to the store than known from the EVM node, which
    suggests the EVM node is outdated. *)
val migrations_from_the_future : applied:int -> known:int -> unit Lwt.t

(** [no_l1_latest_level_to_catch_up ()] advertizes that the EVM node
    is missing l1 latest level in its store. This means the evm node
    can't catch up on evm events and might misses some. *)
val no_l1_latest_level_to_catch_up : unit -> unit Lwt.t
