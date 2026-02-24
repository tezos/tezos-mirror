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

(** Returns [true] if the error represents a cancellation, handling
    all forms: [Canceled], [Exn Lwt.Canceled], and
    [Exn (Failure "Lwt.Resolution_loop.Canceled")] (the latter
    resulting from Data_encoding round-trips through IPC). *)
val is_canceled_error : error -> bool

type t

(** Type of a validated block *)
type new_block = {
  block : Store.Block.t;  (** The block itself. *)
  resulting_context_hash : Context_hash.t;
      (** The context hash resulting of [block]'s application.

          It may be the same one as contained in its header depending
          on the protocol expected semantics. *)
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
  Shell_limits.block_validator_limits ->
  Distributed_db.t ->
  Block_validator_process.t ->
  start_testchain:bool ->
  t Lwt.t

type block_validity =
  | Valid
  | Inapplicable_after_validation of
      error trace (* validation succeeded but application failed *)
  | Invalid of error trace
(* Invalid (validation failed) *)

(** [validate_and_apply ?canceler ?peer ?notify_new_block
    ~advertise_after_validation validator ddb hash header ops] validates a block
    [header] [ops] of hash [hash]. It is a no-op in the following cases:

    - If the block has already been validated.

    - If the block level is before the [savepoint]

    Otherwise it calls the [Block_validator_process] process
   associated to the current [validator].

    - [advertise_after_validation] if set to true, the block is
    advertised to its peers after its validation. It aims to be set to
    false during bootstrapping.

    - [canceler] is triggered when the validation of a block fails.

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
val validate_and_apply :
  t ->
  ?canceler:Lwt_canceler.t ->
  ?peer:P2p_peer.Id.t ->
  ?notify_new_block:(new_block -> unit) ->
  advertise_after_validation:bool ->
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

(** [context_garbage_collection bv index chain_store context_hash
    ~gc_lockfile_path] moves the contexts below the give
    [context_hash] from the upper layer to the lower layer. For full
    and rolling nodes, this is considered as a garbage
    collection. When a garbage collection occurs in another process, a
    lock, located at [gc_lockfile_path], is taken to ensure
    synchronous GC calls. *)
val context_garbage_collection :
  t ->
  Context_ops.index ->
  Context_hash.t ->
  gc_lockfile_path:string ->
  unit tzresult Lwt.t

(** [context_split bv index] finishes and then starts a new chunk in
    the context storage layout. This aims to be called at the dawn of
    each cycle, to improve the disk footprint when running a garbage
    collection. *)
val context_split : t -> Context_ops.index -> unit tzresult Lwt.t

val shutdown : t -> unit Lwt.t

val running_worker : unit -> t

val status : t -> Worker_types.worker_status

val pending_requests :
  t -> (Time.System.t * Block_validator_worker_state.Request.view) list

val current_request :
  t ->
  (Time.System.t * Time.System.t * Block_validator_worker_state.Request.view)
  option
