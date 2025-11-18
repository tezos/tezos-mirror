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

(** {2 Scheduler state type}  *)

(** the automaton's state *)
type loop_state

(** {2 Functions used by the baker} *)

(** [retry ctxt ~delay ?max_delay ~factor ~tries ?msg f x] retries applying [f
    x] [tries] until it succeeds or returns an error different from
    [Connection_failed], at most [tries] number of times. After each try it
    waits for a number of seconds, but not more than [max_delay], if given. The
    wait time between tries is given by the initial [delay], multiplied by
    [factor] at each subsequent try. At each failure, [msg] together with the
    current delay is printed using [ctxt#message].*)
val retry :
  #Protocol_client_context.full ->
  ?max_delay:float ->
  delay:float ->
  factor:float ->
  ?tries:int ->
  ?msg:(tztrace -> string) ->
  ('a -> 'b tzresult Lwt.t) ->
  'a ->
  'b tzresult Lwt.t

val try_resolve_consensus_keys :
  #Protocol_client_context.rpc_context -> Key.t -> public_key_hash Lwt.t

(** [run context ?canceler ?stop_on_event ?on_error ?constants chain
    baking_configuration consensus_keys] is the entry point of the baker
    automaton. This function performs the following tasks:

    - perform a sanity check that check the location of the baking files,
    nonces, highwatermarks and state files, also verifying that the files are
    loadable

    - create the streams for valid blocks, new heads, and operations from the
    node's mempool's

    - create an initial state, see {!create_initial_state}

    - register dal profiles by calling the [register_dal_profiles] node RPC

    - start a revelation worker for nonces by calling
    {!Baking_nonces.start_revelation_worker}

    - create the automaton state, see {!create_loop_state}

    - run the automaton loop, see {!automaton_loop} *)
val run :
  Protocol_client_context.full ->
  ?dal_node_rpc_ctxt:Tezos_rpc.Context.generic ->
  ?canceler:Lwt_canceler.t ->
  ?stop_on_event:(event -> bool) ->
  ?on_error:(tztrace -> unit tzresult Lwt.t) ->
  ?constants:Constants.t ->
  chain:Chain_services.chain ->
  Baking_configuration.t ->
  Baking_state_types.Key.t list ->
  unit tzresult Lwt.t

(** {2 Functions only needed for the baking_lib} *)

(** [sleep_until time] is blocking until it is [time]. *)
val sleep_until : Time.Protocol.t -> unit Lwt.t option

(** [first_potential_round_at_next_level] Returns the first round at the next
    level, at or after [earliest_round], whose baking slot belongs to one of our
    own delegates; also returns the corresponding delegate. Or returns [None] if
    no such round exists. *)
val first_potential_round_at_next_level :
  state ->
  earliest_round:Round.t ->
  (Round.t * Baking_state_types.Delegate.t) option

(** [first_potential_round_at_current_level] Returns the first round at current
    level, at or after [earliest_round], whose baking slot belongs to one of our
    own delegates; also returns the corresponding delegate. Or returns [None] if
    no such round exists. *)
val first_potential_round_at_current_level :
  state ->
  earliest_round:Round.t ->
  (Round.t * Baking_state_types.Delegate.t) option

(** [compute_next_potential_baking_time state] From the current [state], the
    function returns an optional association pair, which consists of the next
    baking timestamp and its baking round. In that case, an elected block must
    exist. *)
val compute_next_potential_baking_time_at_next_level :
  state -> (Time.Protocol.t * Round.t) option Lwt.t

(** [compute_bootstrap_event state] emits the first event. If the latest
    proposal is for the current round, then trigger the new proposal event to
    possibly preattest. Otherwise, trigger the end of round event (for the
    previous round) to check whether we need to propose at this level or not. *)
val compute_bootstrap_event : state -> event tzresult

(** [create_loop_state ?get_valid_blocks_stream heads_stream forge_event_stream
    operation_worker] creates a loop state with the streams of valid blocks, new
    heads, forged events and operations from the node's mempool. *)
val create_loop_state :
  ?get_valid_blocks_stream:proposal Lwt_stream.t Lwt.t ->
  heads_stream:proposal Lwt_stream.t ->
  forge_event_stream:forge_event Lwt_stream.t ->
  Operation_worker.t ->
  loop_state

(** [create_initial_state context ?synchronize chain baking_configuration
    operation_worker dal_attestable_slots_worker current_proposal ?constants consensus_keys]
    creates an initial {!Baking_state.t} by initializing a
    {!type-Baking_state.global_state}, a {!type-Baking_state.level_state}
    and a {!type-Baking_state.round_state}.

    - For the [global_state] initialization, a validation mode is set based on
    the [baking_configuration] and a forge worker is started. If [constants] is
    not provided, an RPC is called to recover them from the [context].

    - For the [level_state] initialization, information regarding the current
    level is retrieved (the current level being that of the [current_proposal])
    and delegates slots are computed for the given [consensus_keys].

    - For the [round_state] initialization, current round is compute by calling
    {!Baking_actions.compute_round} with [current_proposal] information if
    [synchronize] is set to [true] (which is the default). *)
val create_initial_state :
  Protocol_client_context.full ->
  ?dal_node_rpc_ctxt:Tezos_rpc.Context.generic ->
  ?synchronize:bool ->
  chain:Chain_services.chain ->
  Baking_configuration.t ->
  Operation_worker.t ->
  Dal_attestable_slots_worker.t ->
  Round.round_durations ->
  current_proposal:proposal ->
  ?constants:Constants.t ->
  Baking_state_types.Key.t list ->
  state tzresult Lwt.t

(** [automaton_loop ?stop_on_event baking_configuration on_error loop_state
    state event]:

    - calls {!State_transition.step} with the [state] and [event] and recover a
    new state and an action to perform

    - calls {!Baking_actions.perform_action} on this new state and action

    - records the new state on the disk if the [baking_configuration] is not
    {!Baking_configuration.Memory}

    - computes the next timeouts from the current [state] using a function that
    returns an Lwt promise that fulfills once the nearest timeout (between those
    computed by [wait_end_of_round] and [wait_baking_time_next_level]) is
    expired (at that moment the state machine will react)

    - waits for the next event from the events streams (new valid proposal, new
    heads, new forged event, (pre)quorum reached), or a timeout (end of round,
    or a baking time)

    - stops if [stop_on_event] matches the given [event]; recursively calls
    itself with the new event otherwise *)
val automaton_loop :
  ?stop_on_event:(event -> bool) ->
  config:Baking_configuration.t ->
  on_error:(tztrace -> (unit, tztrace) result Lwt.t) ->
  loop_state ->
  state ->
  event ->
  event option tzresult Lwt.t
