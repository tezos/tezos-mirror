(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** The prevalidator is the worker in charge of the [mempool], that
    is, the operations that are not yet included in a block. It keeps
    track of which operations are valid (according to the economic
    protocol), maybe-valid-later, or invalid. The prevalidator is also
    responsible for deciding which operations are appropriate to
    broadcast to peers.

    Before including an operation into the mempool, the prevalidator
    performs a few preliminary checks: [unparsable] operations are
    noted down so that they will never be considered again; already
    handled operations or operations that are not branched on any
    [live_block] are simply ignored; operations which do not meet some
    configurable requirements (e.g. minimal fees) are [refused] at this
    point.

    If the operation passes these preliminary checks, the prevalidator
    then asks the protocol to validate the operation in its maintained
    mempool context. Only an operation that passes this validation gets
    added to the mempool context and broadcast. If the operation is
    flat-out [refused] by the protocol, it is noted down to be ignored
    by the node from now on. If the operation is only [branch_refused]
    or [branch_delayed], it is kept for later: it may be evaluated
    again in the future when the mempool's head (a.k.a. the last block
    it is built on) changes, then added to the mempool and broadcast if
    it passes this new validation.

    See the {{!page-prevalidator} prevalidator implementation overview} to
    learn more.
*)

(** An (abstract) prevalidator context. Separate prevalidator contexts should be
    used for separate chains (e.g., mainchain vs testchain). *)
type t

(** Creates/tear-down a new prevalidator context. *)
val create :
  Shell_limits.prevalidator_limits ->
  (module Protocol_plugin.T) ->
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
  Chain_validator_worker_state.update ->
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

val rpc_directory : t option Tezos_rpc.Directory.t

module Internal_for_tests : sig
  module Tools : sig
    type tools = {
      advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
      chain_tools : Store.Block.t Prevalidator_classification.chain_tools;
      fetch :
        ?peer:P2p_peer.Id.t ->
        ?timeout:Time.System.Span.t ->
        Operation_hash.t ->
        Operation.t tzresult Lwt.t;
      read_block : Block_hash.t -> Store.Block.t tzresult Lwt.t;
      send_get_current_head : ?peer:P2p_peer_id.t -> unit -> unit;
      set_mempool : head:Block_hash.t -> Mempool.t -> unit tzresult Lwt.t;
    }
  end

  val mk_chain_tools :
    Distributed_db.chain_db ->
    Store.Block.t Prevalidator_classification.chain_tools

  val create :
    Tools.tools ->
    Shell_limits.prevalidator_limits ->
    (module Protocol_plugin.T) ->
    Distributed_db.chain_db ->
    t tzresult Lwt.t

  val advertise_mempool : t -> unit Lwt.t
end
