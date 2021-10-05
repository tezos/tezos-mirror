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

(** Tezos Shell - Main entry point of the validation scheduler. *)

type t

val create :
  Store.t ->
  Distributed_db.t ->
  Peer_validator.limits ->
  Block_validator.limits ->
  Block_validator_process.t ->
  Prevalidator.limits ->
  Chain_validator.limits ->
  start_testchain:bool ->
  t tzresult Lwt.t

val shutdown : t -> unit Lwt.t

(** Start the validation scheduler of a given chain. *)
val activate :
  t ->
  start_prevalidator:bool ->
  validator_process:Block_validator_process.t ->
  Store.chain_store ->
  Chain_validator.t tzresult Lwt.t

val get : t -> Chain_id.t -> Chain_validator.t tzresult

val get_active_chains : t -> Chain_id.t list

(** Force the validation of a block. *)
val validate_block :
  t ->
  ?force:bool ->
  ?chain_id:Chain_id.t ->
  Bytes.t ->
  Operation.t list list ->
  (Block_hash.t * unit tzresult Lwt.t) tzresult Lwt.t

(** Monitor all the valid block (for all activate chains). *)
val watcher : t -> Store.Block.t Lwt_stream.t * Lwt_watcher.stopper

val chains_watcher : t -> (Chain_id.t * bool) Lwt_stream.t * Lwt_watcher.stopper

(** [inject_operation t ?chain_id ~force op] notifies the prevalidator worker
    associated with the [chain_id] of a new injected operation.
    Fails if the branch in [op] is not known in the given [chain_id] or if no
    prevalidator is associated with the [chain_id].
    If no [chain_id] is provided, try to recover a chain_id from the branch
    in the operation data. Fails if no chain_id can be recovered unless [force] is set.
    If force is set, notify all the known prevalidator workers. *)
val inject_operation :
  t -> ?chain_id:Chain_id.t -> force:bool -> Operation.t -> unit tzresult Lwt.t

val distributed_db : t -> Distributed_db.t
