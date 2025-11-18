(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t

(** [update_streams_subscriptions state dal_node_rpc_ctxt ~delegate_ids] reconciles
    the active set of streams using [~delegate_ids] by computing the list of streams
    to subscribe to. *)
val update_streams_subscriptions :
  t ->
  Tezos_rpc.Context.generic ->
  delegate_ids:Baking_state_types.Delegate_id.t list ->
  unit Lwt.t

(** [create ~attestation_lag ~number_of_slots] creates a new worker state. This
    does not start any background thread, as streams are opened via
    [update_streams_subscriptions]. *)
val create : attestation_lag:int -> number_of_slots:int -> t

(** [shutdown_worker state] stops all active delegate subscriptions and clears
    the worker’s in-memory state. The worker will no longer hold any references
    to live streams and the cache will become empty. *)
val shutdown_worker : t -> unit Lwt.t
