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

(** This module is the main entry point to valide blocks and protocols. *)

type t

type limits = {
  protocol_timeout : Time.System.Span.t;
  operation_metadata_size_limit : int option;
}

(** [create limits ddb bvp start_testchain] creates a
   [Block_validator].

    - [limits] contains various [timeout] limits.

    - [ddb] is used to commit a block on the storage and get the state
   of the chain for which the block is submitted to validation.

    - [bvp] is an instance of the [Block_validator_process]. [bvp] is
   a proxy between the shell and the validation part related to the
   economic protocol (See [Block_validator_process]).

    - [start_testchain] if set to true allows to run the [testchain].


    This function is not supposed to fail. It is implemented this way
   because of the interface implemented by the [Worker] module. *)
val create :
  limits ->
  Distributed_db.t ->
  Block_validator_process.t ->
  start_testchain:bool ->
  t tzresult Lwt.t

type block_validity =
  | Valid
  | Invalid_after_precheck of error trace (* precheck succeeded but validation failed *)
  | Invalid of error trace
(* Invalid (precheck failed) *)

(** [validate ?precheck_and_notify validator ddb hash header ops] validates a block
   [header] [ops] of hash [hash]. It is a no-op in the following
   cases:

    - If the block has already been validated.

    - If the block level is before the [savepoint]

    Otherwise it calls the [Block_validator_process] process
   associated to the current [validator].

    - [canceler] is trigerred when the validation of a block fails.

    - [peer] is the peer which sent the block.

    If the validation succeeded it processes as follows:

    1. The [ddb] commits the block on the storage.

    2. If the next block requires a switch of protocol, it tries to
   fetch and precompile the next protocol.

    3. Call [notify_new_block] with the committed [block].


    An error is raised if the validation failed or if the block was
   already known as invalid. However, if the first [validation]
   attempt failed because the protocol was missing, it tries to
   [fetch] and [download] the protocol before trying to validate the
   block a second time.

 *)
val validate :
  t ->
  ?canceler:Lwt_canceler.t ->
  ?peer:P2p_peer.Id.t ->
  ?notify_new_block:(Store.Block.t -> unit) ->
  ?precheck_and_notify:bool ->
  Distributed_db.chain_db ->
  Block_hash.t ->
  Block_header.t ->
  Operation.t list list ->
  block_validity Lwt.t

(** [preapply validator canceler chains_store predecessor timestamp
    protocol_data operations] creates a new block and returns it.  It
    may call the [Block_validator_process] process associated to the
    current [validator]. If the preapply is a succeeded, the
    application resulted is cached to avoid re-apply the block if the
    next call block validation, through [validate], targets the same
    block.

    An error is raised if the pre-apply failed. However, if the first
    [pre-apply] attempt failed because the protocol was missing, it
    tries to [fetch] and [download] the protocol before trying to
    pre-apply the block a second time. *)
val preapply :
  t ->
  ?canceler:Lwt_canceler.t ->
  Store.chain_store ->
  predecessor:Store.Block.t ->
  timestamp:Time.Protocol.t ->
  protocol_data:bytes ->
  Operation.t list list ->
  (Block_header.shell_header * error Preapply_result.t trace) tzresult Lwt.t

val fetch_and_compile_protocol :
  t ->
  ?peer:P2p_peer.Id.t ->
  ?timeout:Time.System.Span.t ->
  Protocol_hash.t ->
  Registered_protocol.t tzresult Lwt.t

val shutdown : t -> unit Lwt.t

val running_worker : unit -> t

val status : t -> Worker_types.worker_status

val pending_requests :
  t -> (Time.System.t * Block_validator_worker_state.Request.view) list

val current_request :
  t ->
  (Time.System.t * Time.System.t * Block_validator_worker_state.Request.view)
  option
