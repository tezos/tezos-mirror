(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** The type for a process_monitor
    Handles a list of processes for prometheus-process-monitor to watch *)
type t

val encoding : t Data_encoding.t

(** [init ~listening_port] initializes a new prometheus process monitor
    listening on port [listening_port] *)
val init : listening_port:int -> t

(** [add_binary process_monitor ~group ~name] : adds a binary [name] to the
    monitored processes, in a group [group]. Returns true if the binary
    and associated group was added, false if it was already existing *)
val add_binary : t -> group:string -> name:string -> bool

(** [get_port process_monitor] returns the listening port of
    prometheus-process-monitor *)
val get_port : t -> int

(** [get_binaries process_monitor] returns the list of binaries with their
    group *)
val get_binaries : t -> (string * string) list

(** [reload cmd_wrapper cmd args] reload the prometheus process monitor *)
val reload :
  t -> (detach:bool -> string -> string list -> Process.t) -> unit Lwt.t
