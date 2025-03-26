(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Daemon handling the baker's life cycle.

    It is used to [create] and [run] a protocol-agnostic process which uses the existing
    baking processes in an adaptive way, depending on the current protocol obtained
    from the chain.

    It relies on a [state] which contains the [endpoint] to contact the running node,
    together with the current baker which is being run.

    To do so, it also spawns a "monitoring" process which follows the heads of the
    chain, as reported by the node from the [state], more precisely which monitors
    the voting period. By doing that, it decides when to swap to a different baking
    process.
*)

type t

(** [create ~node_endpoint] returns a non initialized daemon. *)
val create : node_endpoint:string -> t

(** [run daemon] Runs the daemon responsible for the spawn/stop of the
    baker daemons. *)
val run : t -> unit tzresult Lwt.t
