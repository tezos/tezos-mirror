(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(** This module implements a mempool structure meant to be used by a
    shell and bakers in order to incrementally accumulate commutative
    operations which could then be safely used to bake a new
    block. These mempool components guarantee a set of properties
    useful for these purposes:

    - Every operation contained in a mempool is valid;

    - All the mempool's operations can safely be included (and
      applicable) in a block in an arbitrary order which means
      operations commutes. However, to build a valid block with these
      operations:

      - Operations must be reorganized with regards to their validation
        passes.
      - Block's operations quota are ignored, it is the baker's
        responsability to ensure that the set of selected operations
        does not exceed gas/size operations quota.
      - The baker must also include the required preattestations and
        attestations.

    - The merging of two mempools also maintains the aforementioned
      properties.

    Mempools do not depend on local data and therefore are
    serializable. This is useful when a node needs to send a mempool
    to another (remote-)process (e.g. the baker).
*)

open Alpha_context

(** Mempool type *)
type t

(** Validation info type required to validate and add operations to a
    mempool. *)
type validation_info

(** Type of the function that may be provided in order to resolve a
    potential conflict when adding an operation to an existing mempool
    or when merging two mempools. This handler may be defined as a
    simple order relation over operations (e.g. prioritize the most
    profitable operations) or an arbitrary one (e.g. prioritize
    operations where the source is a specific manager).

    Returning [`Keep] will leave the mempool unchanged and retain the
    [existing_operation] while returning [`Replace] will remove
    [existing_operation] and add [new_operation] instead. *)
type conflict_handler =
  existing_operation:Operation_hash.t * packed_operation ->
  new_operation:Operation_hash.t * packed_operation ->
  [`Keep | `Replace]

(** Return type when adding an operation to the mempool *)
type add_result =
  | Added
      (** [Added] means that an operation was successfully added to
          the mempool without any conflict. *)
  | Replaced of {removed : Operation_hash.t}
      (** [Replaced {removed}] means that an operation was
          successfully added but there was a conflict with the [removed]
          operation which was removed from the mempool. *)
  | Unchanged
      (** [Unchanged] means that there was a conflict with an existing
          operation which was considered better by the
          [conflict_handler], therefore the new operation is discarded
          and the mempool remains unchanged. *)

type operation_conflict = Validate_errors.operation_conflict =
  | Operation_conflict of {
      existing : Operation_hash.t;
      new_operation : Operation_hash.t;
    }

(** Error type returned when adding an operation to the mempool fails. *)
type add_error =
  | Validation_error of error trace
      (** [Validation_error _] means that the operation is invalid. *)
  | Add_conflict of operation_conflict
      (** [Add_conflict _] means that an operation conflicts with an
          existing one. This error will only be obtained when no
          [conflict_handler] was provided. Moreover, [Validation_error _]
          takes precedence over [Add_conflict _] which implies that
          we have the implicit invariant that the operation would be
          valid if there was no conflict. Therefore, if
          [add_operation] would have to be called again, it would be
          redondant to check the operation's signature. *)

(** Error type returned when the merge of two mempools fails. *)
type merge_error =
  | Incompatible_mempool
      (** [Incompatible_mempool _] means that the two mempools are not built
          ontop of the same head and therefore cannot be considered. *)
  | Merge_conflict of operation_conflict
      (** [Merge_conflict _] arises when two mempools contain conflicting
          operations and no [conflict_handler] was provided. *)

(** Mempool encoding *)
val encoding : t Data_encoding.t

(** Initialize a static [validation_info] and [mempool], required to validate and add
    operations, and an incremental and serializable [mempool]. *)
val init :
  context ->
  Chain_id.t ->
  predecessor_level:Level.t ->
  predecessor_round:Round.t ->
  predecessor_hash:Block_hash.t ->
  validation_info * t

(** This function partially checks the validity of an operation (see
    {!Validate.check_operation}) that is candidate to be added to the mempool.

    If a validation error is triggered, the result will be a [Validation_error].

    It returns a list of comutative checks that MUST be ran in order to fully
    validate the operation. It includes the signature verification if relevant
    and if [check_signature] has NOT been set to [false]. *)
val partial_op_validation :
  ?check_signature:bool ->
  validation_info ->
  packed_operation ->
  (unit -> unit tzresult) list tzresult Lwt.t

(** Adds a valid operation to a [mempool] if it does not conflict with
    previously added operation.

    Operation should have been validated through [partial_op_validation] and
    the list of commutative checks MUST have been ran.

    If a conflict with a previous operation exists, the result will be
    [Add_conflict] (see {!Validate.check_operation_conflict}). Important: no
    [Add_conflict] will be raised if a [conflict_handler] is provided (see
    [add_result]).

    If no error is raised the operation is potentially added to the [mempool]
    depending on the [add_result] value. *)
val add_valid_operation :
  ?conflict_handler:conflict_handler ->
  t ->
  Operation_hash.t * packed_operation ->
  (t * add_result, add_error) result

(** Legacy function that combines checks of [partial_op_validation] and action
    of [add_valid_operation]. This is not expected to be used by protocols using
    this environment. *)
val add_operation :
  ?check_signature:bool ->
  ?conflict_handler:conflict_handler ->
  validation_info ->
  t ->
  Operation_hash.t * packed_operation ->
  (t * add_result, add_error) result Lwt.t

(** [remove_operation mempool oph] removes the operation [oph] from
    the [mempool]. The [mempool] remains unchanged when [oph] is not
    present in the [mempool] *)
val remove_operation : t -> Operation_hash.t -> t

(** [merge ?conflict_handler existing_mempool new_mempool] merges [new_mempool]
    {b into} [existing_mempool].

    Mempools may only be merged if they are compatible: i.e. both have
    been initialised with the same predecessor block. Otherwise, the
    [Incompatible_mempool] error is returned.

    Conflicts between operations from the two mempools can
    occur. Similarly as [add_operation], a [Merge_conflict] error
    may be raised when no [conflict_handler] is provided.

    [existing_operation] in [conflict_handler ~existing_operation ~new_operation]
    references operations present in [existing_mempool] while
    [new_operation] will reference operations present in
    [new_mempool]. *)
val merge :
  ?conflict_handler:conflict_handler -> t -> t -> (t, merge_error) result

(** [operations mempool] returns the map of operations present in
    [mempool]. *)
val operations : t -> packed_operation Operation_hash.Map.t
