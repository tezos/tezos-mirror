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

type t

val create :
  start_prevalidator:bool ->
  start_testchain:bool ->
  active_chains:t Chain_id.Table.t ->
  block_validator_process:Block_validator_process.t ->
  Shell_limits.peer_validator_limits ->
  Shell_limits.prevalidator_limits ->
  Block_validator.t ->
  Store.Block.t Lwt_watcher.input ->
  (Chain_id.t * bool) Lwt_watcher.input ->
  Distributed_db.t ->
  Store.chain_store ->
  Shell_limits.chain_validator_limits ->
  t tzresult Lwt.t

val chain_id : t -> Chain_id.t

val chain_store : t -> Store.chain_store

val sync_status : t -> Synchronisation_heuristic.status

(** Wait for the `synchronisation_status` to be
   `Synchronised`. Subsequent calls return immediately. In other
   words, once a node is bootstrapped, it remains bootstrapped until
   it terminates (except if [force_bootstrapped] is used). *)
val bootstrapped : t -> unit Lwt.t

val is_bootstrapped : t -> bool

val force_bootstrapped : t -> bool -> unit Lwt.t

val prevalidator : t -> Prevalidator.t option

val chain_db : t -> Distributed_db.chain_db

val child : t -> t option

val reconfigure_event_logging :
  t -> Internal_event_unix.Configuration.t -> unit tzresult Lwt.t

val validate_block :
  t ->
  ?force:bool ->
  Block_hash.t ->
  Block_header.t ->
  Operation.t list list ->
  unit tzresult Lwt.t

val shutdown : t -> unit Lwt.t

val valid_block_watcher : t -> Store.Block.t Lwt_stream.t * Lwt_watcher.stopper

val new_head_watcher :
  t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Lwt_watcher.stopper

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

val ddb_information :
  t -> Chain_validator_worker_state.Distributed_db_state.view
