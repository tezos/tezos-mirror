(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Representation of a block in the simulator. *)
type block = {
  rpc_context : Tezos_protocol_environment.rpc_context;
  protocol_data : Protocol.Alpha_context.Block_header.protocol_data;
  raw_protocol_data : Bytes.t;
  operations : Mockup.M.Block_services.operation list list;
  resulting_context_hash : Context_hash.t;
}

(** Chain is a list of blocks. *)
type chain = block list

(** How an operation or block should propagate through the network. *)
type propagation =
  | Block  (** Block the operation/block, it'll never be delivered. *)
  | Pass  (** Pass the operation/block as is. *)
  | Delay of float
      (** Delay the operation/block for the given number of seconds. *)

(** Values of this type specify to which bakers a block or operation should
   be delivered. *)
type propagation_vector = propagation list

(** The way to control behavior of a mockup node. *)
module type Hooks = sig
  (** This function is called on injection of a block by a particular baker.
     It allows us to inspect, change, or discard the block. Calling the
     injection RPC and actually updating the state of the mockup node are
     two different operations. Normally the first entails the latter, but
     not always. In particular, the [propagation_vector] controls what
     bakers will see and incorporate the block. *)
  val on_inject_block :
    level:int32 ->
    round:int32 ->
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    operations:Operation.t list list ->
    protocol_data:Alpha_context.Block_header.protocol_data ->
    (Block_hash.t * Block_header.t * Operation.t list list * propagation_vector)
    tzresult
    Lwt.t

  (** This function is called on injection of an operation. It is similar
      to [on_inject_block], which see. *)
  val on_inject_operation :
    op_hash:Operation_hash.t ->
    op:Alpha_context.packed_operation ->
    (Operation_hash.t * Alpha_context.packed_operation * propagation_vector)
    tzresult
    Lwt.t

  (** This is called when a new validated block is going to be sent as
      the response to a "monitor validated blocks" RPC call. Returning
      [None] here terminates the process for the baker. *)
  val on_new_validated_block :
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    operations:Operation.t list list ->
    (Block_hash.t * Block_header.t * Operation.t list list) option Lwt.t

  (** This is called when a new head is going to be sent as the response to
     a "monitor heads" RPC call. Returning [None] here terminates the
     process for the baker. *)
  val on_new_head :
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    (Block_hash.t * Block_header.t) option Lwt.t

  (** This is called when a new operation is going to be sent as the
      response to a "monitor operations" RPC call. Returning [None] here
      indicates that the node has advanced to the next level. *)
  val on_new_operation :
    Operation_hash.t * Alpha_context.packed_operation ->
    (Operation_hash.t * Alpha_context.packed_operation) option Lwt.t

  (** Check a block before processing it in the mockup. *)
  val check_block_before_processing :
    level:int32 ->
    round:int32 ->
    block_hash:Block_hash.t ->
    block_header:Block_header.t ->
    protocol_data:Alpha_context.Block_header.protocol_data ->
    unit tzresult Lwt.t

  (** Check the chain after processing a proposal. *)
  val check_chain_after_processing :
    level:int32 -> round:int32 -> chain:chain -> unit tzresult Lwt.t

  (** Check operations in the mempool after injecting an operation. *)
  val check_mempool_after_processing :
    mempool:(Operation_hash.t * Mockup.M.Protocol.operation) list ->
    unit tzresult Lwt.t

  (** This hook is used to decide when the baker is supposed to shut down.
      It is triggered by receiving an event. *)
  val stop_on_event : Baking_state.event -> bool

  (** This hook is used to gather information on the baker when it is
     started (usually recording for later use). The first argument
     [baker_position], is the position of the baker in the list of
     bakers that were started for this run. *)
  val on_start_baker :
    baker_position:int ->
    delegates:Baking_state_types.Key.t list ->
    cctxt:Protocol_client_context.full ->
    unit Lwt.t

  (** Check to run on the chain upon successful termination. *)
  val check_chain_on_success : chain:chain -> unit tzresult Lwt.t
end

(** The default hook implementation. *)
module Default_hooks : Hooks

(** Simulation configuration. *)
type config = {
  round0 : int64;  (** Duration of the round 0 in seconds. *)
  round1 : int64;  (** Duration of the round 1 in seconds. *)
  timeout : int;
      (** Maximal duration of the test. If the test takes
                     longer to terminate it'll be aborted with an
                     error. *)
  delegate_selection : (int32 * (int32 * Signature.public_key_hash) list) list;
      (** Desired selection of delegates per level/round *)
  initial_seed : State_hash.t option;
      (** Optional initial seed for protocol (used to control delegate selection) *)
  consensus_committee_size : int;
      (** Size of the committee for tenderbake in number of slots *)
  consensus_threshold_size : int;
      (** Threshold, in number of slots, for the quorum to be considered
     reached. Should be [2 * consensus_committee_size / 3 + 1] in
     usual setting for tenderbake. *)
}

(** Default configuration. *)
val default_config : config

(** [run spec] runs a simulation according to the [spec]. Elements of [spec]
   describe bakers: how many delegate each baker has and how it behaves. The
   total number of delegates cannot exceed 5 for now (it is easy to increase
   this limit). The delegates are assigned in order, gradually exhausting
   the standard bootstrap accounts. For example, if the first baker has 3
   delegates and the second one has 2 delegates, we have the following
   distribution of bootstrap accounts:

    Baker no. 1: bootstrap1, bootstrap2, bootstrap3

    Baker no. 2: bootstrap4, bootstrap5

   A simulation continues till all nodes finish either with an error or
   successfully. If at least one node finishes with an error, it propagates
   to the final result. *)
val run : ?config:config -> (int * (module Hooks)) list -> unit tzresult Lwt.t

val bootstrap1 : Signature.public_key

val bootstrap2 : Signature.public_key

val bootstrap3 : Signature.public_key

val bootstrap4 : Signature.public_key

val bootstrap5 : Signature.public_key

(** Check if a block header is signed by a given delegate. *)
val check_block_signature :
  block_hash:Block_hash.t ->
  block_header:Block_header.t ->
  public_key:Signature.public_key ->
  unit tzresult Lwt.t

(** A shortcut type for predicates on operations. *)
type op_predicate =
  Operation_hash.t -> Alpha_context.packed_operation -> bool tzresult Lwt.t

(** Count the number of operations in the mempool that satisfy the given
    predicate. *)
val mempool_count_ops :
  mempool:(Operation_hash.t * Mockup.M.Protocol.operation) list ->
  predicate:op_predicate ->
  int tzresult Lwt.t

(** Check if the mempool has at least one operation that satisfies the given
   predicate. *)
val mempool_has_op :
  mempool:(Operation_hash.t * Mockup.M.Protocol.operation) list ->
  predicate:op_predicate ->
  bool tzresult Lwt.t

(** Similar to [mempool_has_op] but instead of returning a [bool] it sets
    the given [bool ref]. *)
val mempool_has_op_ref :
  mempool:(Operation_hash.t * Mockup.M.Protocol.operation) list ->
  predicate:op_predicate ->
  var:bool ref ->
  unit tzresult Lwt.t

(** Check if an operation is signed by the given delegate. *)
val op_is_signed_by : public_key:Signature.public_key -> op_predicate

(** Check that an operation is a preattestation. *)
val op_is_preattestation : ?level:int32 -> ?round:int32 -> op_predicate

(** Check that an operation is an attestation. *)
val op_is_attestation : ?level:int32 -> ?round:int32 -> op_predicate

(** Combine two predicates. *)
val op_is_both : op_predicate -> op_predicate -> op_predicate

(** Set the given variable to save payload hash and payload round. *)
val save_proposal_payload :
  protocol_data:Alpha_context.Block_header.protocol_data ->
  var:(Block_payload_hash.t * Alpha_context.Round.t) option ref ->
  unit tzresult Lwt.t

(** Check that payload hashes match, fail if it is not the case. *)
val verify_payload_hash :
  protocol_data:Alpha_context.Block_header.protocol_data ->
  original_proposal:(Block_payload_hash.t * Alpha_context.Round.t) option ref ->
  message:string ->
  unit tzresult Lwt.t

(** Parse protocol data. *)
val parse_protocol_data :
  Bytes.t -> Alpha_context.Block_header.protocol_data tzresult Lwt.t

(** Get round of a block. *)
val get_block_round : block -> int32 tzresult Lwt.t
