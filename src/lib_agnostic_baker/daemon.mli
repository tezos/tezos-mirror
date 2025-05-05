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

  type command

  (** [run ~keep_alive ~command cctxt] Runs the daemon
      responsible for the spawn/stop of the daemons. *)
  val run :
    keep_alive:bool ->
    command:command ->
    Tezos_client_base.Client_context.full ->
    unit tzresult Lwt.t
end

type baker_command =
  | Run_with_local_node of {
      data_dir : string;
      args : Configuration.t;
      sources : Signature.public_key_hash trace;
    }
  | Run_remotely of {
      args : Configuration.t;
      sources : Signature.public_key_hash trace;
    }
  | Run_vdf of {pidfile : string option; keep_alive : bool}
  | Run_accuser of {
      pidfile : string option;
      preserved_levels : int;
      keep_alive : bool;
    }

module Baker : AGNOSTIC_DAEMON with type command = baker_command

type accuser_command =
  | Run_accuser of {
      pidfile : string option;
      preserved_levels : int;
      keep_alive : bool;
    }

module Accuser : AGNOSTIC_DAEMON with type command = accuser_command
