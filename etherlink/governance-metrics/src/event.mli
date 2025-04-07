(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Event to notify about the binary starting its execution. *)
val starting_observer : unit -> unit Lwt.t

(** Event to notify about the metric server starting its execution. *)
val starting_metrics_server : unit -> unit Lwt.t

(** Event to notify that head monitoring is re-starting its execution. *)
val monitor_head_restart : unit -> unit Lwt.t

(** Debug event to give information on when a contract metric was successfully
    set. *)
val contract_metrics : string -> unit Lwt.t

(** Error event to give information when there is an issue related to processing
    the storage of a smart contract. *)
val storage_state_error : string -> unit Lwt.t

(** Error event to give information when there is an issue related to processing
    the operations of a smart contract. *)
val contract_operations_error : string -> unit Lwt.t
