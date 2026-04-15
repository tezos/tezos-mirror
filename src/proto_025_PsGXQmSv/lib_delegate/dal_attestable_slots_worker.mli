(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** The DAL attestable slots worker keeps per-delegate subscriptions to
    the DAL node's streaming RPC [GET /profiles/<pkh>/monitor/attestable_slots)].
    Each stream emits attestable slots events as soon as they are available from
    the DAL node. Check {!Tezos_dal_node_services.Types.Attestable_event.t} for
    the possible types of events available.

    Incoming events are folded into two caches:
    - [slots_cache] : keyed by published level and delegate id that maintains for
                      the given keys a bitset of currently known attestable slots;
    - [committee_cache] : keyed by delegate id that maintains the current set of
                          known committee levels where the delegate is not in the
                          committee.
    These cache are filled continuously and independently of the baker's main loop
    so that consensus code never waits on the network. There can be a little
    overhead for the backfill of the cache, which is done at stream subscription
    (usually at startup).

    The worker's purpose is to decouple the critical consensus path from DAL
    RPC latency: streams advance in the background, therefore the cache can serve
    DAL information instantly.

    When a consumer asks for attestable slots at level L for a set of delegates,
    the worker returns an immediate "snapshot" from the cache (defaulting to an all-false
    bitset if nothing has been observed yet).
*)

type t

(** [update_streams_subscriptions state dal_node_rpc_ctxt ~delegate_ids] reconciles
    the active set of streams using [~delegate_ids] by computing the list of streams
    to subscribe to. *)
val update_streams_subscriptions :
  t ->
  Tezos_rpc.Context.generic ->
  delegate_ids:Baking_state_types.Delegate_id.t list ->
  unit Lwt.t

(** Status of a single DAL slot for a given delegate and published level. *)
type slot_attestation_status = Attestable | Trap | Unknown

(** [get_dal_attestable_slots t ctxt ~delegate_id ~published_level]
    returns for [~delegate_id], the current bitset for [~published_level], if
    found in the cache. *)
val get_dal_attestable_slots :
  t ->
  delegate_id:Baking_state_types.Delegate_id.t ->
  published_level:int32 ->
  slot_attestation_status list option Lwt.t

(** [is_not_in_committee state ~delegate_id ~committee_level] returns [false] if
    [~delegate_id] is in the committee at the given [~committee_level].
    Returns [false] if we have no information (assume in committee). *)
val is_not_in_committee :
  t ->
  delegate_id:Baking_state_types.Delegate_id.t ->
  committee_level:int32 ->
  bool

(** [create ~automaton_name ~attestation_lag ~attestation_lags ~number_of_slots] creates a new worker
    state. This does not start any background thread, as streams are opened via
    [update_streams_subscriptions]. *)
val create :
  automaton_name:string ->
  attestation_lag:int ->
  attestation_lags:int list ->
  number_of_slots:int ->
  t

(** [shutdown_worker state] stops all active delegate subscriptions and clears
    the worker's in-memory state. The worker will no longer hold any references
    to live streams and the cache will become empty. *)
val shutdown_worker : t -> unit Lwt.t

(** [get_automaton_name state] returns the automaton name associated with this worker. *)
val get_automaton_name : t -> string

(** [emit_dal_worker_started automaton_name dal_endpoint] emits an event indicating
    the DAL worker has started for the given automaton with the specified endpoint. *)
val emit_dal_worker_started : string -> string -> unit Lwt.t
