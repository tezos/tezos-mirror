(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The proposal handler is a worker which is responsible for receiving and
    processing requests to submit proposals to the DSN node, which in turn
    will construct preblocks which are then streamed to the
    [Threshold_encryption_preblocks_monitor.t] instance used by the EVM node.
    Proposal submission requests consist of a `timestamp` and a boolean `force`
    argument. Requests to submit proposals are sent to the
    [Threshold_encryption_proposals_handler] by two different components:
    the main loop of [Threshold_encryption_sequencer], and the handler of the
    private rpc `produceBlock`. The worker maintains a queue of
    pending proposal submission requests, which are processed by the
    worker in a first-in-first-out order. When the
    [Threshold_encryption_proposals_handler] processes a new proposal
    submission, it determines whether the [Tx_pool] is locked or
    it is empty with the `force` argument of the submission request being
    `false`, in which case no proposal is submitted to the `Dsn node`, and
    instead a notification to the instance of
    [Threshold_encryption_preblocks_monitor]
    using a function `notify_no_preblock` provided by the former.
    Otherwise, the [Threshold_encryption_proposals_handler] removes
    transactions from the [Tx_pool], constructs a new proposal and sends it to
    the `Dsn node`. It then waits for a preblock to be delivered by the
    [Threshold_encryption_preblocks_monitor], and for such preblock to be
    processed, e.g. that the preblock has been converted into a blueprint and
    it has been applied on top of the `EVM state` of the node.
    When this happens, a request that a proposal has been processed is sent to
    the [Threshold_encryption_proposals_handler], which starts processing the
    next pending proposal in the queue (if any). *)

type parameters = {
  sidecar_endpoint : Uri.t;  (** The Uri of the Dsn node sequencer sicecar. *)
  maximum_number_of_chunks : int;
      (** The maximum number of chunks a blueprint can be split into. *)
  keep_alive : bool;
  notify_no_preblock : unit -> unit;
      (** A function to notify that a submission proposal request will not
          lead to a preblock being published by the Dsn node. *)
}

(** [start parameters] starts the events follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the events follower. *)
val shutdown : unit -> unit Lwt.t

(** [add_proposal_request timestamp force] Notifies the
    [Threshold_encryption_proposals_handler] tha a new proposal with the given
    [timestamp] should be processed. A proposal
    will be submitted only once it gets notified by the EVM node that it has
    finished processing the previous proposal, that is either it produced and applied
    a blueprint, or it determined that the proposal will not be turned into a blueprint
    (for example if the [Tx_pool] is locked).  *)
val add_proposal_request : Time.Protocol.t -> bool -> unit tzresult Lwt.t

(** [notify_proposal_processed ()] Notifies the blueprint_producer that
    a proposal has been processed, and if it led to a blueprint to
    be produced, it has been applied on top of the EVM state. The
    blueprint producer can start processing the next proposal, if any. *)
val notify_proposal_processed : unit -> Time.Protocol.t tzresult Lwt.t
