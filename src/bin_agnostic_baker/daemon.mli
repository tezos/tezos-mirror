(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Daemon handling the baker's life cycle.

    It is used to [create] and [run] a protocol-agnostic process which uses the existing
    baking binaries in an adaptive way, depending on the current protocol obtained
    from the chain.

    It relies on a [state] which contains the [endpoint] to contact the running node,
    together with the current baker which is being run.

    To do so, it also spawns a "monitoring" process which follows the heads of the
    chain, as reported by the node from the [state], more precisely which monitors
    the voting period. By doing that, it decides when to swap to a different baking
    binary.
*)

type 'a t

(** [create ~binaries_directory ~node_endpoint ~baker_args] returns a non
    initialized daemon. *)
val create :
  binaries_directory:string option ->
  node_endpoint:string ->
  baker_args:string trace ->
  'a t

(** [run t] Runs the daemon responsible for the spawn/stop of the
    baker daemons. *)
val run : 'a t -> unit tzresult Lwt.t
