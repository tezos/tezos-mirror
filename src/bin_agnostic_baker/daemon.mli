(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Daemon handling the bakers life cycle. *)

type t

(** [create binaries_directory node_endpoint baker_args] returns a non
    initialized daemon.*)
val create :
  binaries_directory:string option ->
  node_endpoint:string ->
  baker_args:string trace ->
  t

(** [run t] Runs the daemon responsible for the spawn/stop of the
    baker daemons.  *)
val run : t -> unit tzresult Lwt.t
