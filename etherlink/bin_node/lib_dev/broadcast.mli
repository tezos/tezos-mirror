(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [create_blueprint_stream ()] returns a new stream that can be used to be 
    notified when a new blueprint is being successfully applied on top of the 
    local state. *)
val create_blueprint_stream :
  unit -> Blueprint_types.with_events Lwt_stream.t * Lwt_watcher.stopper

(** Type of messages that are broadcasted to all evm nodes. *)
type message = Blueprint of Blueprint_types.with_events

val message_encoding : message Data_encoding.t

(** [create_broadcast_stream ()] returns a new stream that can be used to be 
    notified of all messages, including a new blueprint being successfully 
    applied on top of the local state. *)
val create_broadcast_stream : unit -> message Lwt_stream.t * Lwt_watcher.stopper

(** [notify message] advertizes [message] to every relevant streams created
    with {!create_stream} or {!create_blueprint_stream} still active. *)
val notify : message -> unit
