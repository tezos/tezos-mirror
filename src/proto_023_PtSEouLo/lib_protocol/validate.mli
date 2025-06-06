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

(** This module provides functions pertaining to the validation of
    blocks and operations. Most elements in this module are either used
    or wrapped in the {!Main} module (though some of them are also
    directly used by the plugin).

    The purpose of validation is to decide quickly whether a block or
    an operation is valid, with minimal computations and without
    writing anything in the storage. A block is considered valid if it
    can be applied without failure (see {!Apply}). An operation is
    valid if it can be safely included in a block without causing it to
    fail. Therefore, the current module is responsible for ensuring
    that calling functions from {!Apply} on validated blocks and
    operations will not fail.

    {2 Block validation}

    The process of validation of a block may be started by calling one
    of the following functions, depending on the circumstances (aka
    mode):

    - [begin_application] is used for the validation of a preexisting
      block, typically received through the network, and usually in
      preparation for its future application.

    - [begin_partial_validation] is used to quickly but partially
      validate an existing block. It is intended for quickly assessing a
      series of blocks in an alternate branch (multipass validation). For
      this reason, in this mode, the initial {!Alpha_context.t} may be
      based on an ancestor block of the block to validate, instead of
      necessarily its predecessor as in other modes.

    - [begin_full_construction] is used for the construction of a new
      block, typically by a baker.

    Then, [validate_operation] should be called on every operation in
    the block (in order of validation pass: see
    {!Operation_repr.acceptable_pass}). Lastly, [finalize_block]
    performs final checks on the block; if this function succeeds then
    the block is valid.

    {2 Validation state}

    The process of block validation relies on a [validation_state]
    transmitted throughout the aforementioned function calls. More
    precisely, this immutable functional state is initialized by the
    [begin_...]  functions, read and updated by [validate_operation]
    (as in, a slightly different [validation_state] is returned), and
    required by [finalize_block]. It consists in three fields:

    - [info] contains static information required by
      [validate_operation] and [finalize_block], notably the initial
      {!Alpha_context.t}. It is fully filled in by the [begin_...]
      functions, then only read, never updated.

    - [operation_conflict_state] keeps track of every validated
      operation in the block, so that it can detect any conflict between
      operations (e.g. two manager operations from the same
      source). Consequently, it is both filled in and read by
      [validate_operation], but not used at all by [finalize_block].

    - [block_state] registers global block metrics such as total gas
      used or attestation power. It is filled in by [validate_operation],
      which also uses it, e.g. to immediately return an error if the
      block gas limit is exceeded. It is also essential to several checks
      in [finalize_block].

    The immutability of the [validation_state] allows the caller to
    pause, replay, or backtrack throughout the steps of the validation
    process.

    {2 Operation validation}

    Operations may be validated either as part of the validation of a
    block in which they are included (see above), or on their own:

    - [begin_partial_construction] allows to initialize a
      [validation_state] for the validation of operations outside of the
      process of validation of a block. It is intended for mempools (see
      {!Mempool_validation}) and for some RPCs. The global block
      properties such as total block gas and attestation power are not
      checked. Calling [finalize_block] on such a [validation_state] does
      not make much sense and simply returns unit.

    - [begin_no_predecessor_info] is a special weaker version of
      [begin_partial_construction]: see its own documentation below.

    Even outside of the context of a given block validation, the
    validation of operations aims at deciding whether they could
    theoretically be included in a future block. Indeed, for a mempool,
    this means that they are worth transmitting to a baker and
    propagating to peers; or for the caller of an RPC, it means that
    the tested operations may be injected in the node.

    An important property to maintain is that applying (see
    {!Apply.apply_operation}) any subset of validated operations should
    always succeed, even if they are not applied in the same order as
    they were validated (as long as the order of application respects
    the validation passes ordering). In other words, for all operations
    A and B that have both been validated: if A has an earlier or the
    same validation pass as B, then applying A then B must succeed; and
    if B has an earlier or the same validation pass as A, then applying
    B then A must succeed. Some restrictions, such as
    one-operation-per-manager-per-block (1M), have been introduced to
    preserve this property, and are enforced with the help of the
    [operation_conflict_state]. An important consequence of this
    property is that a baker may select any subset of validated
    operations to bake into a new block, which is then guaranteed to be
    applicable (provided that it verifies some additional global
    properties such as including enough (pre)attesting power; the
    baker is responsible for ensuring this).

    For a manager operation, validity is mainly solvability, ie. the
    operation must be well-formed and we must be able to take its
    fees. Indeed, this is sufficient for the safe inclusion of the
    operation in a block: even if there is an error during the
    subsequent application of the manager operation, this will cause
    the operation to have no further effects, but won't impact the
    success of the block's application. The solvability of a manager
    operation notably requires that it is correctly signed: indeed, we
    can't take anything from a manager without having checked their
    signature.

    A non-manager operation is only valid if its effects can be fully
    applied in an {!Alpha_context.t} without failure. Indeed, any error
    during the application of such an operation would cause the whole
    block to fail; unlike manager operations, there is no notion of
    failing to have an effect without impacting the application of the
    whole block. More detailled documentation on checks performed and
    potential errors can be found in the [validate.ml] file for some
    non-manager operations. *)

open Alpha_context
open Validate_errors

(** Static information required to validate blocks and operations. *)
type info

(** State used to keep track of previously validated operations and
    detect potential conflicts. This state is serializable which allows
    it to be exchanged with another source. See {!Mempool_validation}. *)
type operation_conflict_state

(** Encoding for the [operation_conflict_state]. *)
val operation_conflict_state_encoding : operation_conflict_state Data_encoding.t

(** State used to register global block properties which are relevant
    to the validity of a block, e.g. the total gas used in the block so
    far. This state is both used and updated by the [validate_operation]
    function, and is also required by [finalize_block]. *)
type block_state

(** Validation state (see above). *)
type validation_state = {
  info : info;
  operation_state : operation_conflict_state;
  block_state : block_state;
}

(** Return the context stored in the state.

    Note that this is the context at the beginning of the block /
    mempool: indeed, it is not modified by [validate_operation]. *)
val get_initial_ctxt : validation_state -> context

(** Initialize the {!validation_state} for the validation of an
    existing block, usually in preparation for its future application. *)
val begin_application :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_timestamp:Time.t ->
  Block_header.t ->
  Fitness.t ->
  validation_state tzresult Lwt.t

(** Initialize the {!validation_state} for the partial validation of
    an existing block.

    The partial validation mode is intended for quickly assessing a
    series of blocks in a cousin branch (multipass
    validation). Therefore, it is the only mode in which the given
    {!type-context} may be based on any recent ancestor block of the
    block to validate, instead of only its predecessor (where recent
    means having a greater level than the [last_allowed_fork_level] of
    the current head). *)
val begin_partial_validation :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_timestamp:Time.t ->
  Block_header.t ->
  Fitness.t ->
  validation_state tzresult Lwt.t

(** Initialize the {!validation_state} for the full construction of a
    fresh block. *)
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

(** Initialize the {!validation_state} for the validation of
    operations outside of the process of validation of a block. The
    partial construction mode is mainly used to implement the mempool
    (see {!Mempool_validation}), but may also be used by some RPCs. *)
val begin_partial_construction :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  validation_state

(** Similar to [begin_partial_construction] but do not require
    predecessor information that is essential to the validation of
    preattestation and attestation operations. As a consequence, the
    validation of these operations will always fail.

    This function is used by the plugin RPC [run_operation], which
    does not support consensus operations anyway. *)
val begin_no_predecessor_info : context -> Chain_id.t -> validation_state

(** Check the validity of the given operation and return the updated
    {!validation_state}.

    See the documentation at the top of this module on operation validation.

    @param check_signature indicates whether the signature check
    should happen. It defaults to [true] because the signature needs to
    be correct for the operation to be valid. This argument exists for
    special cases where it is acceptable to bypass this check, e.g.:

    - A mempool implementation may keep track of operations whose
      signatures have already been checked: if such an operation needs to
      be validated again (typically when the head block changes), then
      the mempool may call [validate_operation] with
      [check_signature:false].

    - The [run_operation] RPC provided by the plugin explicitly
      excludes signature checks: see its documentation in
      [lib_plugin/RPC.Scripts.S.run_operation]. *)
val validate_operation :
  ?check_signature:bool ->
  validation_state ->
  Operation_hash.t ->
  packed_operation ->
  validation_state tzresult Lwt.t

(** Finish the validation of a block.

    This function should only be used after {!validate_operation} has
    been called on every operation in the block. It checks the
    consistency of the block_header with the information computed while
    validating the block's operations (Attestation power, payload hash,
    etc.) Checks vary depending on the mode (ie. which of the
    [begin_...] functions above was used to initialize the
    [validation_state]). *)
val finalize_block : validation_state -> unit tzresult Lwt.t

(** The remaining functions are intended for the mempool.
    See {!Mempool_validation}. *)

(** Check the operation validity, similarly to {!validate_operation}.

    It returns a (potentially empty) list of commutative checks that MUST be ran
    in order to complete check the operation check. Note that, for now, the
    returned list contains only the signature check or an empty list. It might
    be augmented in the future.

    However, this function does not check for conflicts with previously
    validated operations, nor global block properties such as the respect of the
    block gas limit. This allows the function to only take an {!type-info} as
    input rather than a full {!type-validation_state}.

    This function is intended for {!Mempool_validation} exclusively. *)
val check_operation :
  ?check_signature:bool ->
  info ->
  'kind operation ->
  (unit -> unit tzresult) list tzresult Lwt.t

(** Check that the operation does not conflict with other operations
    already validated and recorded in the {!operation_conflict_state}.

    Two {!Dal_entrapment_evidence} are in conflict if their consensus
    attestations have the same level and the same slot.

    This function is intended for {!Mempool_validation} exclusively. *)
val check_operation_conflict :
  operation_conflict_state ->
  Operation_hash.t ->
  'kind operation ->
  (unit, operation_conflict) result

(** Add a valid operation to the {!operation_conflict_state}.

    The operation should have been previously validated by calling
    both {!check_operation} and {!check_operation_conflict}.

    This function is intended for {!Mempool_validation} exclusively. *)
val add_valid_operation :
  operation_conflict_state ->
  Operation_hash.t ->
  'kind operation ->
  operation_conflict_state

(** Remove a valid operation from the {!operation_conflict_state}.

    Preconditions:
    - The operation has already been validated and added to the
      [operation_conflict_state].
    - The [operation_conflict_state] and other states used to validate
      the operation have been initialized by calling
      {!begin_partial_construction}.

    This function is intended for {!Mempool_validation}, though it is
    also called by the plugin. *)
val remove_operation :
  operation_conflict_state -> 'kind operation -> operation_conflict_state
