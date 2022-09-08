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

(** The purpose of this module is to provide, that decides quickly
   whether an operation may safely be included in a block or whether a
   block can be advertised through the network.

    Most elements in this module are either used or wrapped in the
   {!Main} module. *)

(** Static information needed by {!validate_operation} or for a block
   validation.

    It lives in memory, not in the storage. *)
type info

(** State used and modified by {!validate_operation} or by a block
   validation.

    It lives in memory, not in the storage. *)
type state

type validation_state = {info : info; state : state}

open Alpha_context

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
    an existing block. *)
val begin_partial_application :
  ancestor_context:context ->
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
  predecessor_hash:Block_hash.t ->
  grandparent_round:Round.t ->
  validation_state tzresult Lwt.t

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

    @param should_check_signature indicates whether the signature
    check should happen. It defaults to [true] because the signature
    needs to be correct for the operation to be valid. This argument
    exists for special cases where it is acceptable to bypass this
    check, e.g.:

    - The mempool may keep track of operations whose signatures have
      already been checked: if such an operation needs to be validated
      again (typically when the head block changes), then the mempool may
      call [validate_operation] with [should_check_signature:false].

    - The [run_operation] RPC provided by the plugin explicitly
      excludes signature checks: see its documentation in
      [lib_plugin/RPC.Scripts.S.run_operation]. *)
val validate_operation :
  validation_state ->
  ?should_check_signature:bool ->
  Operation_hash.t ->
  Alpha_context.packed_operation ->
  state tzresult Lwt.t

(** Check the consistency of the block_header information with the one
    computed (Endorsement power, payload hash, etc) while validating
    the block operations. Checks vary depending on the mode. *)
val finalize_block : validation_state -> unit tzresult Lwt.t

(** Remove a manager operation from the {!validate_operation_state}.

    This function is intended for a mempool: when an operation A has
    already been validated, but another operation B conflicts with A
    (e.g. they have the same manager) and is more desirable than A
    (e.g. better fees/gas ratio), then the mempool may remove A in
    order to validate B instead.

    This function will be replaced with a generic function
    [remove_operation] in the future. *)
val remove_manager_operation :
  validation_state -> 'a Kind.manager operation -> state
