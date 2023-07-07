(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Protocol specific RPC directory.  *)
module type RPC_DIRECTORY = sig
  (** The RPC directory for the rollup node of this protocol *)
  val directory : Node_context.rw -> unit Tezos_rpc.Directory.t
end

(** Protocol specific functions to track endorsed DAL slots of L1 blocks. *)
module type DAL_SLOTS_TRACKER = sig
  (** [process_head node_ctxt head] performs the following operations for a
      given protocol:
      {ul
        {li it reads the endorsements for headers published attestation_lag
        levels preceding [head] from the block metadata, determines which
        ones the rollup node will download, and stores the results in
        [Store.Dal_confirmed_slots].}
      }  *)
  val process_head : Node_context.rw -> Layer1.head -> unit tzresult Lwt.t
end

(** Protocol specific functions to reconstruct inboxes from L1 blocks. *)
module type INBOX = sig
  (** [process_head node_ctxt ~predecessor head] changes the state of
      the inbox on disk to react to the operations contained in the block [head]
      (where [predecessor] is the predecessor of [head] in the L1 chain). It
      returns a tuple [(inbox_hash, inbox, payload_hash, messages)] where
      [inbox] is the new inbox and [inbox_hash] its hash, [payload_hash] is the
      hash of the merkelized payload for this inbox and [messages] are the
      serialized messages present in the block (with the internal messages added
      by the protocol). *)
  val process_head :
    Node_context.rw ->
    predecessor:Layer1.header ->
    Layer1.header ->
    (Octez_smart_rollup.Inbox.Hash.t
    * Octez_smart_rollup.Inbox.t
    * Merkelized_payload_hashes_hash.t
    * string list)
    tzresult
    Lwt.t

  (** [same_as_layer_1 node_ctxt block node_inbox] ensures that the rollup node
      agrees with the L1 node that inbox for [block] is [node_inbox]. *)
  val same_as_layer_1 :
    _ Node_context.t ->
    Block_hash.t ->
    Octez_smart_rollup.Inbox.t ->
    unit tzresult Lwt.t

  (**/**)

  module Internal_for_tests : sig
    (** Only for tests. [process_messages node_ctxt ~is_first_block ~predecessor
        head messages] reconstructs the inbox on disk for the [messages] as if
        they appeared in [head]. See {!val:process_head} for the return
        values. *)
    val process_messages :
      Node_context.rw ->
      is_first_block:bool ->
      predecessor:Layer1.header ->
      Layer1.header ->
      string list ->
      (Octez_smart_rollup.Inbox.Hash.t
      * Octez_smart_rollup.Inbox.t
      * Merkelized_payload_hashes_hash.t
      * string list)
      tzresult
      Lwt.t
  end
end

(** Protocol specific functions to interpret inbox and messages of L1
    blocks. *)
module type INTERPRETER = sig
  (** [process_head node_ctxt ctxt ~predecessor head (inbox, messages)] interprets
      the [messages] associated with a [head] (where [predecessor] is the
      predecessor of [head] in the L1 chain). This requires the [inbox] to be
      updated beforehand. It returns [(ctxt, num_messages, num_ticks, tick)]
      where [ctxt] is the updated layer 2 context (with the new PVM state),
      [num_messages] is the number of [messages], [num_ticks] is the number of
      ticks taken by the PVM for the evaluation and [tick] is the tick reached
      by the PVM after the evaluation. *)
  val process_head :
    Node_context.rw ->
    'a Context.t ->
    predecessor:Layer1.header ->
    Layer1.header ->
    Octez_smart_rollup.Inbox.t * string trace ->
    ('a Context.t * int * int64 * Z.t) tzresult Lwt.t

  (** [state_of_head node_ctxt ctxt head] returns the state corresponding to the
      block [head], or the state at rollup genesis if the block is before the
      rollup origination. *)
  val state_of_head :
    'a Node_context.t ->
    'a Context.t ->
    Layer1.head ->
    ('a Context.t * Context.tree) tzresult Lwt.t
end

(** Protocol specific commitments publisher. NOTE: The publisher has to be
    stopped and the new one restarted on protocol change. *)
module type PUBLISHER = sig
  (** [process_head node_ctxt ~predecessor head ctxt] builds a new commitment if
      needed, by looking at the level of [head] and checking whether it is a
      multiple of `Commitment.sc_rollup_commitment_period` levels away from
      [node_ctxt.initial_level]. It uses the functionalities of [PVM] to compute
      the hash to be included in the commitment. *)
  val process_head :
    Node_context.rw ->
    predecessor:Block_hash.t ->
    Layer1.header ->
    Context.rw ->
    Commitment.Hash.t option tzresult Lwt.t

  (** [init node_ctxt] initializes the worker for publishing and cementing
      commitments. *)
  val init : _ Node_context.t -> unit tzresult Lwt.t

  (** [publish_commitments ()] publishes the commitments that were not
      yet published up to the finalized head and which are after the last
      cemented commitment. *)
  val publish_commitments : unit -> unit tzresult Lwt.t

  (** [cement_commitments ()] cements the commitments that can be
      cemented, i.e. the commitments that are after the current last cemented
      commitment and which have [sc_rollup_challenge_period] levels on top of
      them since they were originally published.  *)
  val cement_commitments : unit -> unit tzresult Lwt.t

  (** [shutdown ()] stops the worker for publishing and cementing
      commitments. *)
  val shutdown : unit -> unit Lwt.t
end

(** Protocol specific refutation coordinator. NOTE: The refutation coordinator
    has to be stopped and the new one restarted on protocol change. *)
module type REFUTATION_COORDINATOR = sig
  (** [init node_ctxt] initializes the refutation coordinator worker. *)
  val init : Node_context.rw -> unit tzresult Lwt.t

  (** [process head] processes a new l1 head. This means that the coordinator
      will:
      {ol
        {li Gather all existing conflicts}
        {li Launch new refutation players for each conflict concerning
            the operator that doesn't have a player in this node}
        {li Kill all players whose conflict has disappeared from L1}
        {li Make all players play a step in the refutation}
      }
  *)
  val process : Layer1.head -> unit tzresult Lwt.t

  (** [shutdown ()] stops the refutation coordinator. *)
  val shutdown : unit -> unit Lwt.t
end

(** Protocol specific batcher. NOTE: The batcher has to be stopped and the new
    one restarted on protocol change. *)
module type BATCHER = sig
  (** [init config ~signer node_ctxt] initializes and starts the batcher for
      [signer]. If [config.simulation] is [true] (the default), messages added
      to the batcher are simulated in an incremental simulation context. *)
  val init :
    Configuration.batcher ->
    signer:Signature.public_key_hash ->
    _ Node_context.t ->
    unit tzresult Lwt.t

  (** [new_head head] create batches of L2 messages from the queue and pack each
      batch in an L1 operation. The L1 operations (i.e. L2 batches) are queued
      in the injector for injection on the Tezos node.  *)
  val new_head : Layer1.head -> unit tzresult Lwt.t

  (** [shutdown ()] stops the batcher, waiting for the ongoing request to be
      processed. *)
  val shutdown : unit -> unit Lwt.t
end

(** Protocol specific functions to interact with the L1 node. *)
module type LAYER1_HELPERS = sig
  (** [prefetch_tezos_blocks l1_ctxt blocks] prefetches the blocks
      asynchronously. NOTE: the number of blocks to prefetch must not be greater
      than the size of the blocks cache otherwise they will be lost. *)
  val prefetch_tezos_blocks : Layer1.t -> Layer1.head list -> unit

  val get_last_cemented_commitment :
    #Client_context.full -> Address.t -> Node_context.lcc tzresult Lwt.t

  val get_last_published_commitment :
    #Client_context.full ->
    Address.t ->
    Signature.public_key_hash ->
    Commitment.t option tzresult Lwt.t

  val get_kind : #Client_context.full -> Address.t -> Kind.t tzresult Lwt.t

  val genesis_inbox :
    #Client_context.full ->
    genesis_level:int32 ->
    Octez_smart_rollup.Inbox.t tzresult Lwt.t

  (** Retrieve protocol agnotic constants for the head of the chain. *)
  val retrieve_constants :
    ?block:Tezos_shell_services.Block_services.block ->
    #Client_context.full ->
    Rollup_constants.protocol_constants tzresult Lwt.t

  val retrieve_genesis_info :
    #Client_context.full ->
    Address.t ->
    Node_context.genesis_info tzresult Lwt.t
end

(** Protocol specific functions for processing L1 blocks. *)
module type L1_PROCESSING = sig
  (** Ensure that the initial state hash of the PVM as defined by the rollup
      node matches the one of the PVM on the L1 node.  *)
  val check_pvm_initial_state_hash : _ Node_context.t -> unit tzresult Lwt.t

  (** React to L1 operations included in a block of the chain. *)
  val process_l1_block_operations :
    Node_context.rw -> Layer1.header -> unit tzresult Lwt.t
end

(** Signature of protocol plugins for the rollup node. NOTE: the plugins have to
    be registered to be made available to the rollup node.  *)
module type S = sig
  (** The protocol for which this plugin is. *)
  val protocol : Protocol_hash.t

  module RPC_directory : RPC_DIRECTORY

  module Dal_slots_tracker : DAL_SLOTS_TRACKER

  module Inbox : INBOX

  module Interpreter : INTERPRETER

  module Publisher : PUBLISHER

  module Refutation_coordinator : REFUTATION_COORDINATOR

  module Batcher : BATCHER

  module Layer1_helpers : LAYER1_HELPERS

  module L1_processing : L1_PROCESSING
end
