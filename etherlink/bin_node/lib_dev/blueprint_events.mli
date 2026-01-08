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

(** [blueprint_applied block execution_gas duration] advertizes that a blueprint
    leading to [block] requiring [execution_gas] to be executed has been
    applied in [duration] time onto the local state. *)
val blueprint_applied :
  'transaction_object L2_types.block -> Z.t -> Time.System.Span.t -> unit Lwt.t

(** [blueprint_replayed ~execution_gas ~process_time ~diverged number]
    advertises that the node was able to replay the blueprint for level [number]. *)
val blueprint_replayed :
  execution_gas:Ethereum_types.quantity ->
  process_time:Time.System.Span.t ->
  diverged:bool ->
  Ethereum_types.quantity ->
  unit Lwt.t

(** [blueprint_injected level] advertizes that a blueprint for level
    [level] has been forwarded to a rollup node  *)
val blueprint_injected : Z.t -> unit Lwt.t

(** [blueprint_injected_on_inbox level] advertizes that a blueprint
    for level [level] has been forwarded to a rollup node for
    injection on the shared inbox *)
val blueprint_injected_on_inbox : Z.t -> unit Lwt.t

(** [blueprint_injected_on_DAL level nb_chunks] advertizes that a blueprint for
    level [level] and containing [nb_chunks] chunks has been forwarded to a
    rollup node for injection on the DAL *)
val blueprint_injected_on_DAL : level:Z.t -> nb_chunks:int -> unit Lwt.t

(** [blueprint_injection_failed level trace] advertizes that a blueprint could
    not be injected for level [level]. *)
val blueprint_injection_failed : Z.t -> tztrace -> unit Lwt.t

(** [invalid_blueprint_produced level] advertizes that the sequencer has tried
    to produce a blueprint which does not result in the publication of a new
    Ethereum block. *)
val invalid_blueprint_produced : Z.t -> unit Lwt.t

(** [invalid_blueprint_applied level] advertizes that the observer received a blueprint
    for level [level]. *)
val invalid_blueprint_applied : Z.t -> unit Lwt.t

(** [missing_blueprints count from to_] advertizes that a sequencer has detect
    it is missing [count] blueprints in the provided range. This means the
    sequencer store is inconsistent. *)
val missing_blueprints :
  int -> Ethereum_types.quantity -> Ethereum_types.quantity -> unit Lwt.t

(** [catching_up min max] advertizes that the sequencer is reinjecting
    blueprints from level [min] to [max] because the rollup node is lagging. *)
val catching_up : Z.t -> Z.t -> unit Lwt.t

(** [blueprint_production level duration] advertizes the sequencer has fully
    produced a blueprint for [level] in [duration] time. *)
val blueprint_production :
  Ethereum_types.quantity -> Time.System.Span.t -> unit Lwt.t

(** [unexpected_blueprint_from_remote_node ~received ~expected] advertizes the node has
    received a blueprint [number] while expected for [expected]. *)
val unexpected_blueprint_from_remote_node :
  received:Ethereum_types.quantity ->
  expected:Ethereum_types.quantity ->
  unit Lwt.t

(** [worker_request_failed request_view state errs] warns that the
    blueprints_publisher worker encountered errors [errs]. *)
val worker_request_failed :
  Blueprints_publisher_types.Request.view -> Error_monad.tztrace -> unit Lwt.t

val follower_failed : tztrace -> unit Lwt.t
