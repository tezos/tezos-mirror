(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
module type S = sig
  module PVM : Pvm.S

  (** [process_head node_ctxt ~predecessor head ctxt] builds a new commitment if
      needed, by looking at the level of [head] and checking whether it is a
      multiple of `Commitment.sc_rollup_commitment_period` levels away from
      [node_ctxt.initial_level]. It uses the functionalities of [PVM] to compute
      the hash of to be included in the commitment.  *)
  val process_head :
    Node_context.rw ->
    predecessor:Block_hash.t ->
    Layer1.head ->
    Context.rw ->
    Protocol.Alpha_context.Sc_rollup.Commitment.Hash.t option tzresult Lwt.t

  (** [publish_single_commitment node_ctxt commitment] publishes a single
      [commitment] if it is missing. This function is meant to be used by the {e
      accuser} mode to sparingly publish commitments when it detects a
      conflict. *)
  val publish_single_commitment :
    _ Node_context.t ->
    Protocol.Alpha_context.Sc_rollup.Commitment.t ->
    unit tzresult Lwt.t

  (** Worker for publishing and cementing commitments. *)
  module Publisher : sig
    val init : _ Node_context.t -> unit tzresult Lwt.t

    (** [publish_commitments node_ctxt] publishes the commitments that were not
      yet published up to the finalized head and which are after the last
      cemented commitment. *)
    val publish_commitments : unit -> unit tzresult Lwt.t

    (** [cement_commitments node_ctxt] cements the commitments that can be
      cemented, i.e. the commitments that are after the current last cemented
      commitment and which have [sc_rollup_challenge_period] levels on top of
      them since they were originally published.  *)
    val cement_commitments : unit -> unit tzresult Lwt.t

    val shutdown : unit -> unit Lwt.t
  end
end
