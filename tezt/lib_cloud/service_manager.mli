(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A service manager is a small lwt worker that checks regularly that
    some processes, given their name and pid, are alive and emits errors
    when it is not anymore the case. *)
type t

(** [init] creates a new instance of a service manager *)
val init : unit -> t

(** [register_service ~executable ~pid t] register a new service with the
    manager [t].
    Automatically start the loop on the first service. *)
val register_service : name:string -> executable:string -> t -> unit

(** [notify_start_service name pid] notifies service [name] was started with [pid] *)
val notify_start_service : name:string -> pid:int -> t -> unit

(** [notify_stop_service name pid] notifies service [name] was stopped *)
val notify_stop_service : name:string -> t -> unit

(** [shutdown t] terminates the service manager [t] *)
val shutdown : t -> unit
