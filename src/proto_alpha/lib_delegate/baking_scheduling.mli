(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state
open Baking_state_types
open Protocol.Alpha_context

(** [create_global_state ?canceler ?dal_node_rpc_ctxt ?constants ~chain context
    baking_configuration consensus_keys] creates a
    {!type-Baking_state.global_state}: [round_durations] and a DAL attestable
    slots worker are created from [constants], and a forge worker is started. If
    [constants] is not provided, an RPC is called to recover them from
    [context]. If [canceler] is provided, cancelling it will trigger a shutdown
    of the DAL attestable slots worker. *)
val create_global_state :
  ?canceler:Lwt_canceler.t ->
  ?dal_node_rpc_ctxt:Tezos_rpc.Context.generic ->
  ?constants:Constants.t ->
  chain:Chain_services.chain ->
  Protocol_client_context.full ->
  Baking_configuration.t ->
  Baking_state_types.Key.t list ->
  global_state tzresult Lwt.t

(** [create_initial_state ?canceler context ?synchronize
    ?monitor_node_operations ~global_state current_proposal consensus_keys]
    creates an initial {!Baking_state.t} for a single node connection by
    initializing a {!type-Baking_state.automaton_state}, a
    {!type-Baking_state.level_state} and a {!type-Baking_state.round_state}
    from an existing [global_state].

    - For the [automaton_state] initialization, a validation mode is set based
    on the baking configuration and an operation worker is started. If
    [canceler] is provided, cancelling it will also trigger a shutdown of the
    operation worker. See {!Baking_automaton.create_automaton_state}.

    - For the [level_state] initialization, information regarding the current
    and next levels is retrieved (the current level being that of the
    [current_proposal]) and delegate slots are computed for the given
    [consensus_keys] at both levels. The DAL included attestations cache
    committees are also initialized for both levels.

    - For the [round_state] initialization, current round is computed by calling
    {!Baking_actions.compute_round} with [current_proposal] information if
    [synchronize] is set to [true] (which is the default). *)
val create_initial_state :
  ?canceler:Lwt_canceler.t ->
  Protocol_client_context.full ->
  ?synchronize:bool ->
  ?monitor_node_operations:bool ->
  global_state:global_state ->
  current_proposal:proposal ->
  Baking_state_types.Key.t list ->
  state tzresult Lwt.t

(** [run context ?canceler ?stop_on_event ?on_error ?constants chain
    baking_configuration consensus_keys] is the entry point of the baker
    automaton. This function performs the following tasks:

    - perform a sanity check that check the location of the baking files,
    nonces, highwatermarks and state files, also verifying that the files are
    loadable

    - create the streams for valid blocks, new heads, and operations from the
    node's mempool's

    - create an initial state, see {!Baking_automaton.create_initial_state}

    - register dal profiles by calling the [register_dal_profiles] node RPC

    - start a revelation worker for nonces by calling
    {!Baking_nonces.start_revelation_worker}

    - create the automaton state, see {!Baking_automaton.create_loop_state}

    - run the automaton loop, see {!Baking_automaton.automaton_loop} *)
val run :
  Protocol_client_context.full ->
  extra_nodes:Protocol_client_context.full list ->
  ?dal_node_rpc_ctxt:Tezos_rpc.Context.generic ->
  ?canceler:Lwt_canceler.t ->
  ?stop_on_event:(event -> bool) ->
  ?on_error:(tztrace -> unit tzresult Lwt.t) ->
  ?constants:Constants.t ->
  chain:Chain_services.chain ->
  Baking_configuration.t ->
  Baking_state_types.Key.t list ->
  unit tzresult Lwt.t
