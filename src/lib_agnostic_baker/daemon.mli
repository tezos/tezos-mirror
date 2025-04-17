(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Daemon handling the an agent's life cycle.

    The current possible agents are the accuser and the baker, and the corresponding
    daemons are given by [Accuser] and [Baker], respectively.

    It is used to [create] and [run] a protocol-agnostic process which uses the existing
    protocol processes in an adaptive way, depending on the current protocol obtained
    from the chain.

    It relies on a [state] which contains the [endpoint] to contact the running node,
    together with the current agent which is being run.

    To do so, it also spawns a "monitoring" process which follows the heads of the
    chain, as reported by the node from the [state], more precisely which monitors
    the voting period. By doing that, it decides when to swap to a different baking
    process.
*)

module type AGNOSTIC_DAEMON = sig
  type t

  (** [create ~node_endpoint ~keep_alive] returns a non initialized daemon. *)
  val create : node_endpoint:string -> keep_alive:bool -> t

  (** [run daemon] Runs the daemon responsible for the spawn/stop of the daemons. *)
  val run : t -> unit tzresult Lwt.t
end

module Baker : AGNOSTIC_DAEMON

module Accuser : AGNOSTIC_DAEMON
