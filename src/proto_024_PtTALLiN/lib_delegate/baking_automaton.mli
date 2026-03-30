(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state
open Baking_state_types
open Protocol.Alpha_context

module Profiler : sig
  include Tezos_profiler.Profiler.GLOBAL_PROFILER

  val reset_block_section : event * Tezos_profiler.Profiler.metadata -> unit
end

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

(** [try_resolve_consensus_keys cctxt key] attempts to resolve the primary
    delegate associated with the given [key].

    - If [key] is a primary baking key, it is returned directly.
    - If [key] is a consensus key, attestation rights over the past 50 levels
      are inspected to retrieve the associated primary delegate.
    - Falls back to returning the PKH of [key] if no rights are found, or if
      [key] is neither a registered delegate nor a consensus key. *)
val try_resolve_consensus_keys :
  #Protocol_client_context.rpc_context -> Key.t -> public_key_hash Lwt.t

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

val create_automaton_state :
  ?canceler:Lwt_canceler.t ->
  ?monitor_node_operations:bool ->
  global_state:global_state ->
  Protocol_client_context.full ->
  automaton_state tzresult Lwt.t
