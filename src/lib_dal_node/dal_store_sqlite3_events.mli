(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [applied_migration ~name ~duration] advertizes that the migration
    [name] is applied on a store that was previously missing it in
    [duration] seconds. *)
val applied_migration : name:string -> duration:float -> unit Lwt.t

(** [migration_from_the_future ~applied ~known] advertizes that there are more
    migrations applied to the store than known from the DAL node, which
    suggests the DAL node is outdated. *)
val migrations_from_the_future : applied:int -> known:int -> unit Lwt.t
