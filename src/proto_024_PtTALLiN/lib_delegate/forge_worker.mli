(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Concurrent worker for consensus operations and block forging *)

(**
   {2 Description}

   This component allows the concurrent production of consensus
   operations and blocks. It's meant to be used by pushing specific
   requests as tasks and waiting for their completions on a dedicated
   event stream.

   Block forging implies the retrieval of current operations and context
   validation (and application if needed) and then producing a
   signature. For consensus operations, the heavy work is mostly on
   producing a signature. Signatures are expected to be concurrent in
   order not to block the main execution thread.

   {2 Concurrency}

   Each task is associated to a delegate. This worker is designed to
   work concurrently on each delegate's tasks. However, if a request
   is pushed for a delegate while an existing one is active, this new
   request will be enqueued and only be executed after the completion
   of the first one. Hence, only one request may be active per
   delegate at any time.

   {2 Cancellation}

   It is possible to cancel all pending tasks but it is not possible
   to cancel each delegate's active tasks. This is explained by the
   fact that we do not have control over the different signers scheme
   and, in particular, some are not cancellable at all (e.g.,
   ledger).
*)

open Baking_state

(** Worker type *)
type worker

(** [push_request worker request] pushes the [request] to the worker
    to be treated. Each [forge_request] is associated to a specific
    delegate. The request will be treated whenever the delegate's
    associated queue is available. If, the delegate's queue did not
    previously exist, it will be created. Returns an error if the
    worker queue is closed or unavailable. *)
val push_request : worker -> forge_request -> unit tzresult Lwt.t

(** [cancel_all_pending_tasks worker] cancels all the worker's
    delegate queues pending tasks. *)
val cancel_all_pending_tasks : worker -> unit

(** [shutdown worker] triggers the [worker] shutdown. This function
    cancels all pending tasks but still waits for each active one to
    complete. *)
val shutdown : worker -> unit Lwt.t

(** [start global_state] creates and runs a worker based on a baker's
    [global_state]. *)
val start : global_state -> (worker, tztrace) result Lwt.t

(** Internal module exposed for testing purposes only.
    Do not use outside of tests. *)
module Internal_for_tests : sig
  (** Internal module for delegate signing queues. *)
  module Delegate_signing_queue : sig
    type t

    val create : Baking_state_types.Delegate.t -> t
  end

  (** Internal types module. *)
  module Types : sig
    type state = {
      delegate_signing_queues :
        Delegate_signing_queue.t Baking_state_types.Key_id.Table.t;
      baking_state : global_state;
      forge_consensus_vote_hook : (unit -> unit Lwt.t) option;
    }
  end

  (** Get or create a delegate signing queue. This function is synchronous
      and therefore thread-safe in Lwt's cooperative concurrency model. *)
  val get_or_create_queue :
    Types.state -> Baking_state_types.Delegate.t -> Delegate_signing_queue.t

  (** Create a minimal test state for unit testing. *)
  val create_test_state : unit -> Types.state

  (** Get the number of queues in the state. *)
  val queue_count : Types.state -> int

  (** Check if a queue exists for a given delegate. *)
  val has_queue : Types.state -> Baking_state_types.Delegate.t -> bool

  (** Start a forge worker with optional test hook.
      The [forge_consensus_vote_hook] function will be called inside the consensus vote forging task
      before forge_and_sign_consensus_vote is executed. This allows tests to inject custom behavior
      such as delays, logging, or error simulation.
      
      In production code, use the main [start] function which has no hook (None). *)
  val start :
    ?forge_consensus_vote_hook:(unit -> unit Lwt.t) ->
    global_state ->
    (worker, tztrace) result Lwt.t
end
