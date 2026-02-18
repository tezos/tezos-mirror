(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides functions pertaining to the validation of Tezlink
    operations. Validity is checked in several steps: before adding the
    operation in the tx_queue, and then before adding the operation to a
    blueprint.

    To be added to the tx_queue, an operation must be valid on its own
    (independently of other operations), but it can be valid "in the future".
    We check that each field of the operation is valid, in particular:
    - the operation is supported by Tezlink,
    - the public key of the source is either already revealed, or the operation
    is it's reveal,
    - the source can pay the fees,
    - the gas limit is valid: it's enough to cover deserialisation, signature
    verification, no operation goes over the maximum set by the protocol,
    neither does the whole batch,
    - the counter is valid: when receiving the operation, the counter should be
    greater than the current counter of the source; when inserting in a
    blueprint the counter should be the successor of the current counter.
    - the signature is valid,
    - the size of the operation is not bigger then the maximum set by the
    protocol.

    In addition, for a batch:
    - there is only one reveal if any, and it can only be the first operation
    of the batch,
    - the counters are incremented by one by each operation (starting from the
    first counter),
    - the sum of the gas limit doesn't exceed the maximum set for a block by
    the protocol,
    - all the operations have the same source.

    To be added to a blueprint, an operation must be valid when taking into
    account the operations already included in the blueprint:
    - the counter must be exactly the one expected for the source,
    - the balance must be enough to pay all fees.

    Contrary to L1 *we don't enforce the 1M constrait* (applying any subset of
    validated operations should always succeed, even if they are not applied in
    the same order as they were validated). The L2 uses a queue and a single
    sequencer, instead of having a mempool in which bakers can choose
    operations to add. We follow instead the approach taken by Etherlink: the
    queue orders operations. In particular, for a given source the operations
    are ordered using the counter. When creating a blueprint the operation are
    poped in order, and are dropped if they are not valid for the blueprint.

    The errors sent by the functions in this module are taken from the protocol
    as much as possible. This helps with consistency with external tooling,
    e.g. octez-client.
*)

(** [parse_and_validate_for_queue read ~data_model raw_op]
    parses [raw_op] into an operation, and checks that the operation
    is valid on its own. [read] is used to check information about the
    source in the current context (counter, balance, public key);
    [data_model] is used to know which data model (path-based
    or RLP-based) to use when checking information. This first
    validation pass should be done at insertion in the [tx_queue].

    At this point operations are valid if they could be inserted into a future
    blueprint, i.e. it is either valid or could be valid in the future. For
    example, the counter provided in the operation is either the next counter
    (it is valid as the next operation from the source) or a counter further
    away in the future. However the source must be able to pay the fees in the
    current context.

    Another pass of validation will be done at insertion into a blueprint (see
    in this module {!validate_for_blueprint}).

    The optionnal [check_signature] is used to bypass signature verifications
    during simulation.

    The value returned, of type {!Tezos_types.Operation.t}, contains more
    information than just the parsed operation, used by the node notably at
    insertion into a blueprint to validate the transaction in the context of
    the blueprint. In particular, the total fee, the length of the operation
    (one, unless it's a batch) etc. Therefore, any {!Tezos_types.Operation.t}
    value should be created and validated at the same time. *)
val parse_and_validate_for_queue :
  ?check_signature:bool ->
  read:(string -> bytes option tzresult Lwt.t) ->
  data_model:Tezlink_durable_storage.implicit_account_data_model ->
  string ->
  (Tezos_types.Operation.t, string) result tzresult Lwt.t

(** [gas_limit_could_fit state operation] returns [true] if the operationb,
    fits into the blueprint being created. Can lead to a new blueprint. *)
val gas_limit_could_fit :
  Validation_types.validation_state -> Tezos_types.Operation.t -> bool

(** [init_blueprint_validation read ~data_model ()] creates an empty
    validation state, from the context the [read] returns info
    about. *)
val init_blueprint_validation :
  (string -> bytes option tzresult Lwt.t) ->
  data_model:Tezlink_durable_storage.implicit_account_data_model ->
  unit ->
  Validation_types.validation_state

(** [validate_for_blueprint state operation] finishes the validation of
    [operation] and checks that it can it be added to the blueprint. Returns
    either the update validation state, or an error if the operation is now
    invalid.

    Supposes that the operation was first pre-validated at insertion into the
    [tx_queue].

    Returns validation errors as string, following Etherlink convention, to
    make it easier to insert that part of the validation in the current control
    flow. *)
val validate_for_blueprint :
  Validation_types.validation_state ->
  Tezos_types.Operation.t ->
  (Validation_types.validation_state, string) result tzresult Lwt.t
