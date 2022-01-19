(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Tezos Shell - Prevalidation of pending operations (a.k.a Mempool) *)

(** The prevalidator is in charge of the [mempool] (a.k.a. the
    set of known not-invalid-for-sure operations that are not yet
    included in the blockchain).

    The prevalidator also maintains a sorted subset of the mempool that
    might correspond to a valid block on top of the current head. The
    "in-progress" context produced by the application of those
    operations is called the (pre)validation context.

    Before including an operation into the mempool, the prevalidation
    worker tries to append (in application mode)/evaluate (precheck mode)
    the operation to/in the prevalidation context. Only an operation that passes
    the application/precheck will be broadcast. If the operation is ill-formed,
    it will not be added into the mempool and then it will
    be ignored by the node and will never be broadcast. If the operation is
    only [branch_refused] or [branch_delayed], it may be added to the mempool
    if it passes the application/precheck in the future.
*)

(** An (abstract) prevalidator context. Separate prevalidator contexts should be
    used for separate chains (e.g., mainchain vs testchain). *)
type t

(** This record contains the differents limits and settings that can be updated
    from a node configuration for a prevalidator *)
type limits = {
  max_refused_operations : int;
      (** The maximum number of operations tracked by the mempool for each of
          the [refused], [branch delayed], [branch refused] and [outdated]
          operation classifications. Default is [1000] *)
  operation_timeout : Time.System.Span.t;
      (** The maximum time allowed to fetch the contents of an operation
          advertised by a remote peer. Default is [10] seconds *)
  operations_batch_size : int;
      (** Maximum number of pending operations processed (or classified) at the
          end of each request to the prevalidator worker. Default is [50] *)
  disable_precheck : bool;
      (** If [disable_precheck] is [true] (default is [false]) operations are
          executed by the protocol before being propagated. This flag is
          intended to be used for testing and debugging. *)
}

(** Sane default values for {!limits} *)
val default_limits : limits

(** Creates/tear-down a new prevalidator context. *)
val create :
  limits ->
  (module Prevalidator_filters.FILTER) ->
  Distributed_db.chain_db ->
  t tzresult Lwt.t

val shutdown : t -> unit Lwt.t

(** Notify the prevalidator that the identified peer has sent a bunch of
    operations relevant to the specified context. *)
val notify_operations : t -> P2p_peer.Id.t -> Mempool.t -> unit Lwt.t

(** [inject_operation t ~force op] notifies the prevalidator worker of a new
    injected operation. If [force] is set to [true] the operation is injected
    without any check. [force] should be used for test purpose only. *)
val inject_operation : t -> force:bool -> Operation.t -> unit tzresult Lwt.t

(** Notify the prevalidator that a new head has been selected.
    [update] is used as an optimisation to know which operations
    previously classified require to be prevalidated again. *)
val flush :
  t ->
  Chain_validator_worker_state.Event.update ->
  Block_hash.t ->
  Block_hash.Set.t ->
  Operation_hash.Set.t ->
  unit tzresult Lwt.t

(** Returns the list of prevalidation contexts running and their associated
    chain *)
val running_workers : unit -> (Chain_id.t * Protocol_hash.t * t) list

(** Worker status and events *)

(* None indicates the there are no workers for the current protocol. *)
val status : t -> Worker_types.worker_status

val pending_requests :
  t -> (Time.System.t * Prevalidator_worker_state.Request.view) list

val current_request :
  t ->
  (Time.System.t * Time.System.t * Prevalidator_worker_state.Request.view)
  option

val information : t -> Worker_types.worker_information

val pipeline_length : t -> int

val rpc_directory : t option RPC_directory.t

(**/**)

module Internal_for_tests : sig
  (** Documented in {!Prevalidator}, because this is only exported for tests. *)
  type 'prevalidation_t tools = {
    advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
    chain_tools : Store.Block.t Prevalidator_classification.chain_tools;
    create :
      predecessor:Store.Block.t ->
      live_operations:Operation_hash.Set.t ->
      timestamp:Time.Protocol.t ->
      unit ->
      'prevalidation_t tzresult Lwt.t;
    fetch :
      ?peer:P2p_peer.Id.t ->
      ?timeout:Time.System.Span.t ->
      Operation_hash.t ->
      Operation.t tzresult Lwt.t;
    read_block : Block_hash.t -> Store.Block.t tzresult Lwt.t;
    send_get_current_head : ?peer:P2p_peer_id.t -> unit -> unit;
    set_mempool : head:Block_hash.t -> Mempool.t -> unit tzresult Lwt.t;
  }

  (** Documented in {!Prevalidator}, because this is only exported for tests. *)
  type worker_tools = {
    push_request : unit Prevalidator_worker_state.Request.t -> unit Lwt.t;
    push_request_now : unit Prevalidator_worker_state.Request.t -> unit;
  }

  (** The corresponding internal type of the mempool (see {!Prevalidator.S}),
      that is independent from the protocol. *)
  type ('a, 'b) types_state_shell

  (** Create a pristine value of {!type_state_shell} *)
  val mk_types_state_shell :
    predecessor:Store.Block.t ->
    tools:'prevalidation_t tools ->
    worker:worker_tools ->
    ('protocol_data, 'prevalidation_t) types_state_shell

  module Make
      (Filter : Prevalidator_filters.FILTER)
      (Prevalidation_t : Prevalidation.T
                           with type validation_state =
                                 Filter.Proto.validation_state
                            and type protocol_operation = Filter.Proto.operation
                            and type operation_receipt =
                                 Filter.Proto.operation_receipt) : sig
    (** The corresponding internal type of the mempool (see {!Prevalidator.S}),
        that depends on the protocol *)
    type types_state

    (** Create a pristine value of {!type_state} *)
    val mk_types_state :
      shell:
        ( Prevalidation_t.protocol_operation,
          Prevalidation_t.t )
        types_state_shell ->
      validation_state:Prevalidation_t.t ->
      types_state Lwt.t

    (** [to_shell pv] returns the shell part of [pv] *)
    val to_shell :
      types_state ->
      (Prevalidation_t.protocol_operation, Prevalidation_t.t) types_state_shell

    (** Documented in {!Prevalidator.S} *)
    val handle_unprocessed : types_state -> unit Lwt.t

    (** Documented in {!Prevalidator.S} (as are all the functions of this module) *)
    module Requests : sig
      val on_advertise : _ types_state_shell -> unit

      val on_arrived :
        types_state -> Operation_hash.t -> Operation.t -> unit tzresult Lwt.t

      val on_ban : types_state -> Operation_hash.t -> unit tzresult Lwt.t

      val on_flush :
        handle_branch_refused:bool ->
        types_state ->
        Store.Block.t ->
        Block_hash.Set.t ->
        Operation_hash.Set.t ->
        unit tzresult Lwt.t

      val on_inject :
        types_state -> force:bool -> Operation.t -> unit tzresult Lwt.t

      val on_notify :
        _ types_state_shell -> P2p_peer_id.t -> Mempool.t -> unit Lwt.t
    end
  end
end
