(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module is about shard amplification, a feature allowing DAL
    nodes which receive enough shards of a given slot to contribute to
    the DAL reliability by reconstructing the slot, recomputing all the
    shards, and republishing the missing shards on the DAL network.

    The reconstructed slot is not stored but all the reconstructed
    shards are stored.
*)

(** An amplificator process.  *)
type t

(** [try_amplification node_ctxt commitment slot_metrics slot_id amplificator]
    triggers an amplification, ie the reconstruction and publication of a
    partial set of shards. It is called each time a new shard is received by an
    observer node, after being added to the shard store
    [node_store.shard_store]. The argument [commitment] is the commitment of
    the received shard. The argument [slot_metrics] is the slot_metric
    concerning the slot at [slot_id], containing various timing, eg. the time
    the first shard was received. This function enqueues an amplification task
    for the crypto process worker in the following case:
    - the prover SRS is available,
    - enough shards have been received to reconstruct the slot,
    - not all shards for the given commitment are stored (in
      particular, the slot id has not yet been amplified),
    - an amplification for the same slot id is not already ongoing.

    The amplification is cancelled if all the shards are received in a
    short time.

    To avoid freezing the DAL node, the reconstruction is not handled by the
    main process of the DAL node but by the process provided in the
    [amplificator] argument. *)
val try_amplification :
  Cryptobox.Commitment.t ->
  Dal_metrics.slot_metrics ->
  Types.slot_id ->
  t ->
  unit tzresult Lwt.t

(** Creates a new amplificator process *)
val make : Node_context.t -> t tzresult Lwt.t

(** [restart amplificator] restarts the process worker.
    Should be called if the node context cryptobox or shard proof
    precomputations changed. *)
val restart : t -> unit tzresult Lwt.t
