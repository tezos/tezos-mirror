(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Environment - Protocol updater. *)

(** Validation result: the record returned by the protocol
    on the successful validation of a block. *)
type validation_result = {
  context : Context.t;
      (** The resulting context, it will be used for the next block. *)
  fitness : Fitness.t;
      (** The effective fitness of the block (to be compared with the one
          'announced' in the block header). *)
  message : string option;
      (** An optional informative message, akin to a 'git commit' message,
          which can be attached to the [context] when it's being commited. *)
  max_operations_ttl : int;
      (** The "time-to-live" of operations for the next block: any
          operation whose 'branch' is older than 'ttl' blocks in the past
          cannot be included in the next block. *)
  last_finalized_block_level : Int32.t;
      (** The level of the last block for which the node might
          consider an alternate branch. The shell should consider as
          invalid any branch whose fork point is older (has a lower
          level) than the given value. *)
  last_preserved_block_level : Int32.t;
      (** The level of the oldest block that is considered as
          preserved. The shell uses it as an hint to perform
          internal maintenance operations. *)
}

type quota = {
  max_size : int;
      (** The maximum size (in bytes) of the serialized list of
          operations. *)
  max_op : int option;
      (** The maximum number of operations in a block.
          [None] means no limit. *)
}

type rpc_context = {
  block_hash : Block_hash.t;
  block_header : Block_header.shell_header;
  context : Context.t;
}

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig
  (** The maximum size of a block header in bytes. *)
  val max_block_length : int

  (** The maximum size of an {!operation} in bytes. This value is bigger than the size
      of the bytes required for {!operation_data}, because this value accounts
      for the shell header. *)
  val max_operation_data_length : int

  (** Operations quota for each validation pass. The length of the
     list denotes the number of validation passes. *)
  val validation_passes : quota list

  (** The economic protocol-specific type of blocks. *)
  type block_header_data

  (** Encoding for economic protocol-specific part of block headers. *)
  val block_header_data_encoding : block_header_data Data_encoding.t

  (** A fully parsed block header. *)
  type block_header = {
    shell : Block_header.shell_header;
    protocol_data : block_header_data;
  }

  (** Economic protocol-specific side information computed by the
      protocol during the validation of a block. Should not include
      information about the evaluation of operations which is handled
      separately by {!operation_metadata}. To be used as an execution
      trace by tools (client, indexer). Not necessary for
      validation. *)
  type block_header_metadata

  (** Encoding for economic protocol-specific block metadata. This encoding uses
      the attestation legacy name: endorsement. *)
  val block_header_metadata_encoding_with_legacy_attestation_name :
    block_header_metadata Data_encoding.t

  (** Encoding for economic protocol-specific block metadata. *)
  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  (** The economic protocol-specific type of operations. *)
  type operation_data

  (** Economic protocol-specific side information computed by the
      protocol during the validation of each operation, to be used
      conjointly with {!block_header_metadata}. *)
  type operation_receipt

  (** A fully parsed operation. *)
  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  (** Encoding for protocol-specific operation data. *)
  val operation_data_encoding : operation_data Data_encoding.t

  (** Encoding for protocol-specific operation data. This encoding uses the
      attestation legacy name: endorsement. *)
  val operation_data_encoding_with_legacy_attestation_name :
    operation_data Data_encoding.t

  (** Encoding for protocol-specific operation receipts. *)
  val operation_receipt_encoding : operation_receipt Data_encoding.t

  (** Encoding for protocol-specific operation receipts. This encoding uses the
      attestation legacy name: endorsement. *)
  val operation_receipt_encoding_with_legacy_attestation_name :
    operation_receipt Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. *)
  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. This encoding uses
      the attestation legacy name: endorsement. *)
  val operation_data_and_receipt_encoding_with_legacy_attestation_name :
    (operation_data * operation_receipt) Data_encoding.t

  (** [acceptable_pass op] gives the validation pass in which the
      input operation [op] can appear. For instance, it results in
      [Some 0] if [op] only belongs to the first pass. When [op] is
      ill-formed, [acceptable_pass op] returns [None]. *)
  val acceptable_pass : operation -> int option

  (** [compare_operations (oph1,op1) (oph2,op2)] defines a total
      ordering relation on valid operations.

      The following requirements must be satisfied: [oph1] is the
      [Operation.hash.p1], [oph2] is [Operation.hash op2] and that
      [op1] and [op2] are valid in the same context.

      [compare_operations (oph1,op1) (oph2,op2) = 0] happens only if
      [Operation_hash.compare oph1 oph2 = 0], meaning [op1 = op2] only
      when [op1] and [op2] are structurally identical.

      Two operations of different validation_passes are compared in the
      reverse order of their [validation_pass]: the one with the
      smaller [validation_pass] is compared as being the greater.

      When belonging to the same validation_pass, two operations
      comparison depends on their static parameters. An abstract weight
      is computed for each operation based on its static parameters.
      When two operations' weights are compared as equal,
      [compare_operation (oph1,op1) (oph2,op2)] is
      [Operation_hash.compare oph1 oph2].

      [compare_operations] can be used as a [compare] component of an
      {!Stdlib.Map.OrderedType}, or any such collection which relies on
      a total comparison function. *)
  val compare_operations :
    Operation_hash.t * operation -> Operation_hash.t * operation -> int

  (** {2 Block (and operation) validation and application}

      The following functions may be used when an existing block is
      received through the network, when a new block is created, or
      when operations are considered on their own e.g. in a mempool or
      during an RPC call.

      Validation aims at deciding quickly whether a block or
      an operation is valid, with minimal computations and without
      writing anything in the storage. A block is valid if it can be
      applied without failure. An operation is valid if it can be
      safely included in a block without causing it to fail.

      The application of an operation updates the {!Context.t} with
      regards to its semantics (e.g. updating balances after a
      transaction). The application of a block updates the context
      with all its operations and some additional global
      effects. Isolated operations may be applied as part of an RPC
      call to simulate their effects.

      Blocks and operations must always be validated before they are
      applied. Indeed, the application assumes their validity as a
      precondition, meaning that the application of an invalid block
      might yield incorrect results instead of failing cleanly.

      Note that in protocol versions <= K, where the validation
      functions do not yet exist, the validation of existing blocks is
      done by trying to apply it using the [Partial_validation] mode
      below. Therefore, the application of a validated block may still
      fail in these protocols. *)

  (** The mode indicates the circumstances in which a block and/or
      operations are validated or applied, and contains specific
      information. It must be provided as an argument to
      [begin_validation] and [begin_application]. *)
  type mode =
    | Application of block_header
        (** Standard validation or application of a preexisting block. *)
    | Partial_validation of block_header
        (** Partial validation of a preexisting block. This mode is
            meant to quickly reject obviously invalid alternate
            branches by only performing a subset of checks.
            Therefore, application of blocks or operations makes no
            sense in this mode: calling [begin_application] with this
            mode returns an error. *)
    | Construction of {
        predecessor_hash : Block_hash.t;
        timestamp : Time.t;
        block_header_data : block_header_data;
      }
        (** Construction of a new block. The main difference with the
            previous modes is that we cannot provide the block header to the
            [begin_] functions, since the block does not exist yet. Note that
            the [begin_] functions may be called in this mode without knowing
            yet which operations will be included in the future block.

            The provided [block_header_data] is not expected to be the final
            value of the field of the same type in the {!type-block_header} of
            the constructed block. Instead, it should be a protocol-specific,
            good enough, "prototype" of the final value. E.g. if the
            {!block_header_data} type for the current economic protocol includes
            a signature, then the provided [block_header_data] should contain a
            fake signature (since providing a correct signature is not possible
            at this stage). *)
    | Partial_construction of {
        predecessor_hash : Block_hash.t;
        timestamp : Time.t;
      }
        (** Minimal construction of a new virtual block, with the purpose of
            being able to validate/apply operations of interest. This mode may
            be used by the mempool (though the [Mempool] module below is better
            suited for this) or by some RPCs
            e.g. [preapply/operations]. Calling the [finalize_] functions makes
            no sense in this mode. *)

  (** A functional state that is transmitted throughout the validation
      of a block (or during the lifetime of a mempool or RPC). It is
      created by [begin_validation] below, updated by
      [validate_operation], and required by [finalize_validation].
      This state is immutable thus validator or baker implementations
      are allowed to pause, replay or backtrack throughout validation
      steps. *)
  type validation_state

  (** Similar to {!validation_state}, but for the application process. *)
  type application_state

  (** [begin_validation predecessor_context chain_id mode
      ~predecessor] initializes the {!validation_state} for the
      validation process of an existing or new block.

      [predecessor_context] and [predecessor] are the resulting
      context and shell header of the predecessor block. Exceptionally
      in {!Partial_validation} mode, they may instead come from any
      ancestor block that is more recent (i.e. has a greater level)
      than the current head's "last_finalized_block_level".

      [mode] specifies the circumstances of validation and also
      carries additional information: see {!mode}.

      Note that for protocol versions <= K where [begin_validation]
      does not exist yet, this calls the old [begin_application] by
      necessity. However, in [Application] mode, this calls the old
      [begin_application] in [Partial_validation] mode in order to run
      more quickly. This preserves the behavior of [precheck] in
      [lib_validation/block_validation.ml] for old protocols. It does
      mean that the application of a validated block may fail in these
      protocols. *)
  val begin_validation :
    Context.t ->
    Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    validation_state tzresult Lwt.t

  (** Validate an operation. If successful, return the updated
      {!validation_state}.

      [check_signature] indicates whether the signature should be
      checked. It defaults to [true] because the signature needs to be
      correct for the operation to be valid. This argument exists for
      special cases where it is acceptable to bypass this check,
      e.g. if we know that the operation has already been successfully
      validated in another context. *)
  val validate_operation :
    ?check_signature:bool ->
    validation_state ->
    Operation_hash.t ->
    operation ->
    validation_state tzresult Lwt.t

  (** Run final and global checks on the block that must come after
      the validation of all its operations to establish its
      validity. *)
  val finalize_validation : validation_state -> unit tzresult Lwt.t

  (** Initialize the {!application_state} for the application process
      of an existing or new block. See {!begin_validation} for details
      on the arguments.

      In protocol versions > K, calling this function with the
      {!Partial_validation} mode returns an error. *)
  val begin_application :
    Context.t ->
    Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    application_state tzresult Lwt.t

  (** Apply an operation. If successful, return the updated
      {!application_state} and the corresponding {!operation_receipt}.

      This should be called for all operations in a block, after
      {!begin_application} and before
      {!finalize_application}. Moreover, the operation should have
      already been validated by {!validate_operation}. *)
  val apply_operation :
    application_state ->
    Operation_hash.t ->
    operation ->
    (application_state * operation_receipt) tzresult Lwt.t

  (** Finalize the context resulting from the application of the
      contents of the block.

      If there is no protocol migration, i.e. if the block being
      applied is not the last block of the current economic protocol,
      then the resulting context can be used in the future as input for
      the validation and application of its successor blocks.

      In {!Construction} mode, the [Block_header.shell_header option]
      argument must contain a value, which will be used to compute the
      [cache_nonce]. In other modes, it can as well be [None] since it
      will not be used. *)
  val finalize_application :
    application_state ->
    Block_header.shell_header option ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  (** [rpc_services] provides the list of remote procedures exported
      by this protocol implementation. *)
  val rpc_services : rpc_context RPC_directory.t

  (** [init chain_id ctxt hd] initializes the context, or upgrades the
      context after a protocol amendment. This function receives as
      arguments the [chain_id] of the current chain and the context
      [ctxt] resulting from the application of the block that triggered
      the amendment, as well as its header [hd]. This function should
      fail if the "protocol stitching", i.e., the transition from a
      valid previous protocol to the one being activated, has not been
      implemented. *)
  val init :
    Chain_id.t ->
    Context.t ->
    Block_header.shell_header ->
    validation_result tzresult Lwt.t

  (** [value_of_key chain_id predecessor_context
      predecessor_timestamp predecessor_level predecessor_fitness
      predecessor timestamp] returns a function to build one value of
      the cache from its key.

      This function is used to restore all or part of the cache, for
      instance when booting a validator to preheat the cache, or when a
      reorganization happens. This function should never fail, returned
      errors are fatal.

      The generated function is passed to [Context.Cache.load_caches]
      which will use it either immediately a cache-loading time or
      on-demand, when a given cached value is accessed. *)
  val value_of_key :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.t ->
    (Context.Cache.key -> Context.Cache.value tzresult Lwt.t) tzresult Lwt.t

  module Mempool : sig
    (** Mempool type. This immutable functional state keeps track of
        operations added to the mempool, and allows to detect conflicts
        between them and a new candidate operation. *)
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
      existing_operation:Operation_hash.t * operation ->
      new_operation:Operation_hash.t * operation ->
      [`Keep | `Replace]

    type operation_conflict =
      | Operation_conflict of {
          existing : Operation_hash.t;
          new_operation : Operation_hash.t;
        }

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
              and the mempool remains unchanged.*)

    (** Error type returned when adding an operation to the mempool fails. *)
    type add_error =
      | Validation_error of error trace
          (** [Validation_error _] means that the operation is invalid. *)
      | Add_conflict of operation_conflict
          (** [Add_conflict _] means that an operation conflicts with
              an existing one. This error will only be obtained when
              no [conflict_handler] was provided. Moreover,
              [Validation_error _] takes precedence over [Add_conflict
              _] which implies that we have the implicit invariant
              that the operation would be valid if there was no
              conflict. Therefore, if [add_operation] would have to be
              called again, it would be redondant to check the
              operation's signature. *)

    (** Error type returned when the merge of two mempools fails. *)
    type merge_error =
      | Incompatible_mempool
          (** [Incompatible_mempool _] means that the two mempools are not built
              ontop of the same head and therefore cannot be considered. *)
      | Merge_conflict of operation_conflict
          (** [Merge_conflict _] arises when two mempool contains conflicting
              operations and no [conflict_handler] was provided.*)

    (** Initialize a static [validation_info] and [mempool], required
        to validate and add operations, and an incremental and
        serializable {!mempool}. *)
    val init :
      Context.t ->
      Chain_id.t ->
      head_hash:Block_hash.t ->
      head:Block_header.shell_header ->
      (validation_info * t) tzresult Lwt.t

    (** Mempool encoding *)
    val encoding : t Data_encoding.t

    (** Adds an operation to a [mempool] if and only if it is valid and
        does not conflict with previously added operation.

        This function checks the validity of an operation and tries to
        add it to the mempool.

        If a validation error is triggered, the result will be a
        [Validation_error].  If a conflict with a previous operation
        exists, the result will be [Add_conflict] is then checked.
        Important: no [Add_conflict] will be raised if a
        [conflict_handler] is provided (see [add_result]).

        If no error is raised the operation is potentially added to the
        [mempool] depending on the [add_result] value. *)
    val add_operation :
      ?check_signature:bool ->
      ?conflict_handler:conflict_handler ->
      validation_info ->
      t ->
      Operation_hash.t * operation ->
      (t * add_result, add_error) result Lwt.t

    (** [remove_operation mempool oph] removes the operation [oph] from
        the [mempool]. The [mempool] remains unchanged when [oph] is not
        present in the [mempool] *)
    val remove_operation : t -> Operation_hash.t -> t

    (** [merge ?conflict_handler mempool mempool'] merges [mempool']
        {b into} [mempool].

        Mempools may only be merged if they are compatible: i.e. both have
        been initialised with the same predecessor block. Otherwise, the
        [Incompatible_mempool] error is returned.

        Conflicts between operations from the two mempools can
        occur. Similarly as [add_operation], a [Merge_conflict] error
        may be raised when no [conflict_handler] is provided.

        [existing_operation] in [conflict_handler ~existing_operation ~new_operation]
        references operations present in [mempool] while
        [new_operation] will reference operations present in
        [mempool']. *)
    val merge :
      ?conflict_handler:conflict_handler -> t -> t -> (t, merge_error) result

    (** [operations mempool] returns the map of operations present in
        [mempool]. *)
    val operations : t -> operation Operation_hash.Map.t
  end
end

(** [activate ctxt ph] activates an economic protocol (given by its
    hash [ph]) from the context [ctxt]. The resulting context is still
    a context for the current economic protocol, and the migration is
    not complete until [init] in invoked. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t
