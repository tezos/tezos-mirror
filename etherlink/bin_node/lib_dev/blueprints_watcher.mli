(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [create_stream ()] returns a new stream that can be used to be notified
    when a new blueprint is being successfully applied on top of the local
    state. *)
val create_stream :
  unit -> Blueprint_types.with_events Lwt_stream.t * Lwt_watcher.stopper

(** [notify blueprint] advertizes to every streams created with
    {!create_stream} still active that a new blueprint has been applied on top
    of the local state. *)
val notify : Blueprint_types.with_events -> unit
