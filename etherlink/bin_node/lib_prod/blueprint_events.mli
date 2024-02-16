(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Events related to the publication of blueprints. *)

val section : string list

(** [publisher_is_ready ()] advertises that the worker is ready to accept
    requests. *)
val publisher_is_ready : unit -> unit Lwt.t

(** [publisher_shutdown ()] advertises that the worker has been shutdown and
    will not accept requests anymore. *)
val publisher_shutdown : unit -> unit Lwt.t

(** [blueprint_applied level] advertizes that a blueprint for level
    [level] has been applied onto the local state. *)
val blueprint_applied : Z.t -> unit Lwt.t

(** [blueprint_injected level] advertizes that a blueprint for level
    [level] has been forwarded to a rollup node  *)
val blueprint_injected : Z.t -> unit Lwt.t

(** [blueprint_injection_failed level] advertizes that a blueprint could not be
    injected for level [level]. *)
val blueprint_injection_failed : Z.t -> unit Lwt.t

(** [invalid_blueprint_produced level] advertizes that the sequencer has tried
    to produce a blueprint which does not result in the publication of a new
    Ethereum block. *)
val invalid_blueprint_produced : Z.t -> unit Lwt.t

(** [missing_blueprint level] advertizes that a sequencer has tried to fetch
    the blueprint for level [level] from its store and failed. This means the
    sequencer store is inconsistent. *)
val missing_blueprint : Z.t -> unit Lwt.t

(** [catching_up min max] advertizes that the sequencer is reinjecting
    blueprints from level [min] to [max] because the rollup node is lagging. *)
val catching_up : Z.t -> Z.t -> unit Lwt.t
