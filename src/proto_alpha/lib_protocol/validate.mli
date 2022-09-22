(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The purpose of this module is to provide the {!validate_operation}
    function, that decides quickly whether an operation may safely be
    included in a block. See the function's description for further
    information.

    This module also provide functions to check the validity of a
    block and the consistency of its block_header.

    Most elements in this module are either used or wrapped in the
    {!Main} module. *)

open Alpha_context
open Validate_errors

(** Static information required to validate blocks and operations. *)
type info

(** State used to register operations effects used to establish
    potential conflicts. This state is serializable which allows it to
    be exchanged with another source. See {Mempool_validation} *)
type operation_conflict_state

(** Encoding for the [operation_conflict_state]. *)
val operation_conflict_state_encoding : operation_conflict_state Data_encoding.t

(** State used to register global block validity dependent
    effects. This state is used and updated by the
    [validate_operation] function and will also be used during the
    [finalize_block]. For instance, it registers inter-operations
    checks (e.g. total gas used in the block so far). *)
type block_state

(** Validation state *)
type validation_state = {
  info : info;
  operation_state : operation_conflict_state;
  block_state : block_state;
}

(** Return the context stored in the state. Note that this is the
    context at the beginning of the block / mempool: indeed, it is not
    modified by [validate_operation]. *)
val get_initial_ctxt : validation_state -> context

(** Initialize the {!info} and {!state} for the validation of an
    existing block (in preparation for its future application). *)
val begin_application :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_timestamp:Time.t ->
  Block_header.t ->
  Fitness.t ->
  validation_state tzresult Lwt.t

(** Initialize the {!info} and {!state} for the partial validation of
    an existing block.

    Note that the given context may be based on an ancestor
    block. Indeed, we may not have access to the predecessor context
    when trying to quickly assess a series of blocks in a cousin branch
    (multipass validation). *)
val begin_partial_validation :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_timestamp:Time.t ->
  Block_header.t ->
  Fitness.t ->
  validation_state tzresult Lwt.t

(** Initialize the {!info} and {!state} for the full
    construction of a fresh block. *)
val begin_full_construction :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_timestamp:Time.t ->
  predecessor_hash:Block_hash.t ->
  Round.t ->
  Block_header.contents ->
  validation_state tzresult Lwt.t

(** Initialize the {!info} and {!state} for the partial
    construction use mainly to implement the mempool. *)
val begin_partial_construction :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  grandparent_round:Round.t ->
  validation_state

(** Initialize the {!info} and {!state} without providing any
   predecessor information. This will cause any preendorsement or
   endorsement operation to fail, since we lack the information needed
   to validate it. *)
val begin_no_predecessor_info : context -> Chain_id.t -> validation_state

(** Check the validity of the given operation; return an updated
    {!state}.

    An operation is valid if it may be included in a block without
    causing the block's application to fail. The purpose of this
    function is to decide validity quickly, that is, without trying to
    actually apply the operation (ie. compute modifications to the
    context: see {!Apply.apply_operation}) and see whether it causes
    an error.

    An operation's validity may be checked in different situations:
    when we receive a block from a peer or we are constructing a fresh
    block, we validate each operation in the block right before trying
    to apply it; when a mempool receives an operation, it validates it
    to decide whether the operation should be propagated (note that
    for now, this only holds for manager operations, since
    [validate_operation] is not implemented yet for other operations:
    see below). See {!type:mode}.

    The [info] contains every information we need
    about the status of the chain to validate an operation, notably the
    context (of type {!Alpha_context.t}) at the end of the previous
    block. This context is never updated by the validation of
    operations, since validation is separate from application. Yet
    sometimes, the presence of some previous operations in a block or a
    mempool may render the current operation invalid. E.g. the
    one-operation-per-manager-per-block restriction (1M) states that a
    block is invalid if it contains two separate operations from the
    same manager; therefore the validation of an operation will return
    [Error Manager_restriction] if another operation by the same
    manager has already been validated in the same block or mempool. In
    order to track this kind of operation incompatibilities, we use a
    [state] with minimal information that gets
    updated during validation.

    For a manager operation, validity is solvability, ie. it must be
    well-formed, and we need to be able to take its fees. Indeed, this
    is sufficient for the safe inclusion of the operation in a block:
    even if there is an error during the subsequent application of the
    manager operation, this will cause the operation to have no further
    effects, but won't impact the success of the block's
    application. The solvability of a manager operation notably
    includes it being correctly signed: indeed, we can't take anything
    from a manager without having checked their signature.

    For non-manager operations, any error during the operation
    application causes the whole block to fail. Therefore, the
    validation of such an operation must ensure that its application
    will fully succeed.

    @param check_signature indicates whether the signature
    check should happen. It defaults to [true] because the signature
    needs to be correct for the operation to be valid. This argument
    exists for special cases where it is acceptable to bypass this
    check, e.g.:

    - The mempool may keep track of operations whose signatures have
      already been checked: if such an operation needs to be validated
      again (typically when the head block changes), then the mempool may
      call [validate_operation] with [check_signature:false].

    - The [run_operation] RPC provided by the plugin explicitly
      excludes signature checks: see its documentation in
      [lib_plugin/RPC.Scripts.S.run_operation]. *)
val validate_operation :
  ?check_signature:bool ->
  validation_state ->
  Operation_hash.t ->
  packed_operation ->
  validation_state tzresult Lwt.t

(** Check the operation validity, see {!validate_operation} for
    more information

    Note: Should only be called in mempool mode *)
val check_operation :
  ?check_signature:bool -> info -> 'kind operation -> unit tzresult Lwt.t

(** Check that the operation does not conflict with other operations
    already validated and included in the {!operation_conflict_state}

    Note: Should only be called in mempool mode *)
val check_operation_conflict :
  operation_conflict_state ->
  Operation_hash.t ->
  'kind operation ->
  (unit, operation_conflict) result

(** Add the operation in the {!operation_conflict_state}. The
    operation should be validated before being added

    Note: Should only be called in mempool mode *)
val add_valid_operation :
  operation_conflict_state ->
  Operation_hash.t ->
  'kind operation ->
  operation_conflict_state

(** Remove the operation from the {!operation_conflict_state}.

    Hypothesis:
    - the [operation] has been validated and added to
      [operation_conflict_state];
    - this function is only valid for the mempool mode. *)
val remove_operation :
  operation_conflict_state -> 'kind operation -> operation_conflict_state

(** Check the consistency of the block_header information with the one
    computed (Endorsement power, payload hash, etc) while validating
    the block operations. Checks vary depending on the mode. *)
val finalize_block : validation_state -> unit tzresult Lwt.t
