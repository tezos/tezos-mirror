(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** The rollup node stores and publishes commitments for the PVM
    every `Commitment.sc_rollup_commitment_period` levels.

    Every time a finalized block is processed  by the rollup node,
    the latter determines whether the last commitment that the node
    has produced referred to `Commitment.sc_rollup_commitment_period` blocks
    earlier. In this case, it computes and stores a new commitment in a
    level-indexed map.

    Stored commitments are signed by the rollup node operator
    and published on the layer1 chain. To ensure that commitments
    produced by the rollup node are eventually published,
    storing and publishing commitments are decoupled. Every time
    a new head is processed, the node tries to publish the oldest
    commitment that was not published already.
*)

(** [process_head plugin node_ctxt ~predecessor head ctxt] builds a new
    commitment if needed, by looking at the level of [head] and checking whether
    it is a multiple of `Commitment.sc_rollup_commitment_period` levels away
    from [node_ctxt.initial_level]. It uses the functionalities of [PVM] to
    compute the hash of to be included in the commitment.  *)
val process_head :
  (module Protocol_plugin_sig.S) ->
  Node_context.rw ->
  predecessor:Block_hash.t ->
  Layer1.header ->
  Context.rw ->
  Commitment.Hash.t option tzresult Lwt.t

(** [create_commitment_if_necessary plugin node_ctxt ~predecessor level ctxt]
    returns the commitment for inbox level [level] if there needs to be
    one. [ctxt] should be the context checkouted for [level]. *)
val create_commitment_if_necessary :
  (module Protocol_plugin_sig.S) ->
  'a Node_context.t ->
  predecessor:Block_hash.t ->
  int32 ->
  'a Context.t ->
  (Commitment.t option, tztrace) result Lwt.t

(** [publish_single_commitment node_ctxt commitment] publishes a single
    [commitment] if it is missing. This function is meant to be used by the {e
    accuser} mode to sparingly publish commitments when it detects a
    conflict. *)
val publish_single_commitment :
  _ Node_context.t -> Commitment.t -> unit tzresult Lwt.t

(** [recover_bond node_ctxt] publishes a recover bond operator for the
    Operating key. The submitter is either the operator or another
    address depending of the rollup node configuration. This function
    is intended to be used by the {e bailout} mode. *)
val recover_bond : _ Node_context.t -> unit tzresult Lwt.t

(** Initialize worker for publishing and cementing commitments, if the
    rollup node mode supports it. *)
val init : Node_context.rw -> unit tzresult Lwt.t

(** [publish_commitments] publishes the commitments that were not yet
    published up to the finalized head and which are after the last cemented
    commitment. This is a no-op if the rollup node is not in the appropriate
    mode. *)
val publish_commitments : unit -> unit tzresult Lwt.t

(** [cement_commitments] cements the commitments that can be cemented,
    i.e. the commitments that are after the current last cemented commitment and
    which have [sc_rollup_challenge_period] levels on top of them since they
    were originally published. This is a no-op if the rollup node is not in the
    appropriate mode. *)
val cement_commitments : unit -> unit tzresult Lwt.t

(** [execute_outbox] execute pending outbox messages on L1. *)
val execute_outbox : unit -> unit tzresult Lwt.t

(** Stop worker for publishing and cementing commitments. *)
val shutdown : unit -> unit Lwt.t

(** Returns the status of the publisher worker. *)
val worker_status : unit -> [`Running | `Not_running | `Crashed of exn]
