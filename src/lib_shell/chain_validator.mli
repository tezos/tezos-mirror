(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t

(** Constants parameterizing the bootstrap heuristics. *)
type bootstrap_conf = {
  max_latency : int;
      (** [max_latency] is the time interval (seconds) used to determine if a node is
          synchronized with a chain. For instance, a node that knows head
          with timestamp T is synchronized if T >= now - max_latency. This
          parameter depends on the baking rate and the latency of the network. *)
  chain_stuck_delay : int;
      (** [chain_stuck_delay] is the delay (seconds) after which we consider
          the chain is stuck if we don't get new heads from peers. *)
  sync_polling_period : int;
      (** [sync_polling_period] is the polling period (seconds) used to check for
          synchronisation. *)
  bootstrap_threshold : int;
}

type limits = {
  bootstrap_conf : bootstrap_conf;
  worker_limits : Worker_types.limits;
}

val create :
  start_prevalidator:bool ->
  start_testchain:bool ->
  active_chains:t Chain_id.Table.t ->
  block_validator_process:Block_validator_process.t ->
  Peer_validator.limits ->
  Prevalidator.limits ->
  Block_validator.t ->
  State.Block.t Lwt_watcher.input ->
  (Chain_id.t * bool) Lwt_watcher.input ->
  Distributed_db.t ->
  State.Chain.t ->
  limits ->
  t tzresult Lwt.t

val chain_id : t -> Chain_id.t

val chain_state : t -> State.Chain.t

(** - If there are at least `boostrap_threshold` active peers, which validated
      at least one block, we consider the peers with the most recent
      heads:
    . if all heads are *recent enough*, returns [`Sync]. Recent mean timestamp
      is later than [now() - max_latency]
    . if all heads have the same time, but haven't been updated for
      [chain_stuck_delay] returns [`Stuck]
    . otherwise, returns [`Unsync]
    - If there are less than `bootstrap_threshold` such peers
    . if bootstrap_threshold = 0, returns [`Sync] else [`Unsync`] *)
val sync_state : t -> [`Sync | `Stuck | `Unsync]

(** Poll synchronization state until it is `Sync or `Stuck.
    Subsequent calls return immediately. In other words, once a node is
    bootstrapped, it remains bootstrapped until it terminates. *)
val bootstrapped : t -> unit Lwt.t

val is_bootstrapped : t -> bool

val prevalidator : t -> Prevalidator.t option

val chain_db : t -> Distributed_db.chain_db

val child : t -> t option

val validate_block :
  t ->
  ?force:bool ->
  Block_hash.t ->
  Block_header.t ->
  Operation.t list list ->
  State.Block.t option tzresult Lwt.t

val shutdown : t -> unit Lwt.t

val valid_block_watcher : t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

val new_head_watcher : t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

val running_workers : unit -> (Chain_id.t * t) list

val status : t -> Worker_types.worker_status

val information : t -> Worker_types.worker_information

val pending_requests :
  t -> (Time.System.t * Chain_validator_worker_state.Request.view) list

val pending_requests_length : t -> int

val current_request :
  t ->
  (Time.System.t * Time.System.t * Chain_validator_worker_state.Request.view)
  option

val last_events :
  t -> (Internal_event.level * Chain_validator_worker_state.Event.t list) list

val ddb_information :
  t -> Chain_validator_worker_state.Distributed_db_state.view
