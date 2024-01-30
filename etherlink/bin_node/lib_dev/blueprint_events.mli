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

(** [entered_degraded_mode level] advertizes that the worker will
    no longer forward blueprints to the rollup node. *)
val entered_degraded_mode : Z.t -> unit Lwt.t

(** [invalid_blueprint_produced level] advertizes that the sequencer has tried
    to produce a blueprint which does not result in the publication of a new
    Ethereum block. *)
val invalid_blueprint_produced : Z.t -> unit Lwt.t
