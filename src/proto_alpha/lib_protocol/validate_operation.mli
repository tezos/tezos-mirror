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

    Most elements in this module are either used or wrapped in the
    {!Main} module. *)

(** Static information needed in {!validate_operation}.

    It lives in memory, not in the storage. *)
type validate_operation_info

(** State used and modified by {!validate_operation}.

    It lives in memory, not in the storage. *)
type validate_operation_state

open Alpha_context

(** Initialize the {!validate_operation_info} and
    {!validate_operation_state} for the validation of an existing block
    (in preparation for its future application). *)
val begin_block_validation :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_hash:Block_hash.t ->
  Fitness.t ->
  Block_payload_hash.t ->
  validate_operation_info * validate_operation_state

(** Initialize the {!validate_operation_info} and
    {!validate_operation_state} for the construction of a fresh
    block. *)
val begin_block_construction :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_hash:Block_hash.t ->
  Round.t ->
  Block_payload_hash.t ->
  validate_operation_info * validate_operation_state

(** Initialize the {!validate_operation_info} and
    {!validate_operation_state} for a mempool. *)
val begin_mempool :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_hash:Block_hash.t ->
  grandparent_round:Round.t ->
  validate_operation_info * validate_operation_state

(** Initialize the {!validate_operation_info} and
    {!validate_operation_state} without providing any predecessor
    information. This will cause any preendorsement or endorsement
    operation to fail, since we lack the information needed to validate
    it. *)
val begin_no_predecessor_info :
  context -> Chain_id.t -> validate_operation_info * validate_operation_state

(** A receipt to guarantee that an operation is always validated
    before it is applied.

    Indeed, some functions in {!Apply} require a value of this type,
    which may only be created by calling {!validate_operation} (or a
    function in {!TMP_for_plugin}). *)
type stamp

(** Check the validity of the given operation; return an updated
    {!validate_operation_state}, and a {!stamp} attesting that the
    operation has been validated.

    An operation is valid if it may be included in a block without
    causing the block's application to fail. The purpose of this
    function is to decide validity quickly, that is, without trying to
    actually apply the operation (ie. compute modifications to the
    context: see {!Apply.apply_operation}) and see whether it causes an
    error.

    An operation's validity may be checked in different situations:
    when we receive a block from a peer or we are constructing a fresh
    block, we validate each operation in the block right before trying
    to apply it; when a mempool receives an operation, it validates it
    to decide whether the operation should be propagated (note that for
    now, this only holds for manager operations, since
    [validate_operation] is not impleted yet for other operations: see
    below). See {!type:mode}.

    The [validate_operation_info] contains every information we need
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
    [validate_operation_state] with minimal information that gets
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
  validate_operation_info ->
  validate_operation_state ->
  ?should_check_signature:bool ->
  Operation_hash.t ->
  'kind operation ->
  (validate_operation_state * stamp) tzresult Lwt.t

(** Functions for the plugin.

    These functions are temporary.

    TODO: https://gitlab.com/tezos/tezos/-/issues/3245
    Update the plugin to call directly {!validate_operation} then
    remove these functions. *)
module TMP_for_plugin : sig
  (** Indicate whether the signature should be checked in
      {!precheck_manager}; if so, provide the raw operation.

      We could have used an [option], but this makes calls to
      {!precheck_manager} more readable. *)
  type 'a should_check_signature =
    | Check_signature of 'a operation
    | Skip_signature_check

  (** Similar to {!validate_operation}, but do not check the
      one-operation-per-manager-per-block restriction (1M).

      Indeed, 1M is already handled by the plugin. This function is
      purposefully close to the former
      [Apply.precheck_manager_contents_list], so that few changes are
      needed in the plugin.

      The signature is only checked if the [should_check_signature]
      argument is [Check_signature _].

      The {!validate_operation_state} does not need to be updated
      because:

      + 1M is not handled here anyway.

      + In mempool mode, the block gas limit is not tracked.

      This function is called by {!Main.precheck_manager}, which is
      called in [lib_plugin/mempool.ml]. *)
  val precheck_manager :
    validate_operation_info ->
    validate_operation_state ->
    'a Kind.manager contents_list ->
    'a Kind.manager should_check_signature ->
    stamp tzresult Lwt.t
end
