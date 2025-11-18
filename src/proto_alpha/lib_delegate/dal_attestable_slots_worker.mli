(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t

(** [create ~attestation_lag ~number_of_slots] creates a new worker state. *)
val create : attestation_lag:int -> number_of_slots:int -> t

(** [shutdown_worker state] stops all active delegate subscriptions and clears
    the worker’s in-memory state. The worker will no longer hold any references
    to live streams and the cache will become empty. *)
val shutdown_worker : t -> unit Lwt.t
