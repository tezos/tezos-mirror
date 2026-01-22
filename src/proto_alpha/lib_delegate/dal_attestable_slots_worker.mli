(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** The DAL attestable slots worker keeps per-delegate subscriptions to
    the DAL node’s streaming RPC [GET /profiles/<pkh>/monitor/attestable_slots)].
    Each stream emits attestable slots events as soon as they are available from
    the DAL node. Check {!Tezos_dal_node_services.Types.Attestable_event.t} for
    the possible types of events available.

    Incoming events are folded into an in-memory cache keyed by attestation level
    and delegate id. For each (attestation_level, delegate), the worker maintains a
    boolean bitset of attestable slots. This cache is filled continuously and independently
    of the baker’s main loop so that consensus code never waits on the network. There can
    be a little overhead for the backfill of the cache, which is done at stream subscription
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

(** [get_dal_attestable_slots t ctxt ~delegate_id ~attestation_level]
    returns for [~delegate_id], the current bitset for [published_level] derived
    from [~attestation_level], if found in the cache. *)
val get_dal_attestable_slots :
  t ->
  delegate_id:Baking_state_types.Delegate_id.t ->
  attestation_level:int32 ->
  Tezos_dal_node_services.Types.attestable_slots option Lwt.t

(** [create ~attestation_lag ~attestation_lags ~number_of_slots] creates a new worker
    state. This does not start any background thread, as streams are opened via
    [update_streams_subscriptions]. *)
val create :
  attestation_lag:int -> attestation_lags:int list -> number_of_slots:int -> t

(** [shutdown_worker state] stops all active delegate subscriptions and clears
    the worker’s in-memory state. The worker will no longer hold any references
    to live streams and the cache will become empty. *)
val shutdown_worker : t -> unit Lwt.t
