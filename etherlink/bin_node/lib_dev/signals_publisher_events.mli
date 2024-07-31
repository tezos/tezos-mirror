(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(** Events related to the publication of signals. *)

val section : string list

(** [publisher_is_ready ()] advertises that the worker is ready to
    accept requests. *)
val publisher_is_ready : unit -> unit Lwt.t

(** [publisher_shutdown ()] advertises that the worker has been
    shutdown and will not accept requests anymore. *)
val publisher_shutdown : unit -> unit Lwt.t
