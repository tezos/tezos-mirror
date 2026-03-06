(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type evm_services_methods = Evm_ro_context.evm_services_methods

type finalizer = unit -> unit Lwt.t

type block_production = [`Single_node | `Disabled]

(** [start_private_server ~block_production config ctxt] starts the private RPC
    server with low-level, internal services which should not be exposed to the
    Internet. If [config] does not provide the necessary information to start a
    private RPC server, then [start_private_server] is a no-op.

    [block_production] is used to tailor the private RPC server to the
    particular need of the EVM node mode at use, more particularly
    which methods are available wrt. block production. [`Single_node]
    starts the default private RPC server, as used in the single node
    sequencer setup, [`Disabled] means no block production method is
    available. *)
val start_private_server :
  mode:'f Mode.t ->
  rpc_server_family:'f Rpc_types.rpc_server_family ->
  tick:(unit -> unit tzresult Lwt.t) ->
  ?block_production:block_production ->
  Configuration.t ->
  Evm_ro_context.t ->
  finalizer tzresult Lwt.t

(** [start_public_server config ctxt] starts the RPC servers as per specified
    in [config].

    The optional argument [evm_services_methods] can be used to install
    the EVM services.

    If the host provides the necessary binaries, performance metrics
    are enabled. *)
val start_public_server :
  mode:'f Mode.t ->
  rpc_server_family:'f Rpc_types.rpc_server_family ->
  l2_chain_id:L2_types.chain_id option ->
  tick:(unit -> unit tzresult Lwt.t) ->
  ?evm_services:evm_services_methods ->
  Configuration.t ->
  Evm_ro_context.t ->
  finalizer tzresult Lwt.t
