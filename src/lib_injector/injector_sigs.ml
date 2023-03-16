(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type tez = {mutez : int64}

type fee_parameter = {
  minimal_fees : tez;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : tez;
  burn_cap : tez;
}

type injection_strategy =
  [ `Each_block  (** Inject pending operations after each new L1 block *)
  | `Delay_block of float
    (** [`Delay_block f] strategy waits for [f] * the next block time to be
        elapsed before injecting pending operations. This strategy allows for
        maximizing the number of the same kind of operations to include in a
        block. *)
  ]

(** Explanation for unsuccessful operations (that are included in a block). *)
type unsuccessful_status =
  | Other_branch
      (** The operation is included in a block that is not on the main chain
          anymore, because of a reorganization. *)
  | Backtracked
      (** The operation is backtracked because of a further failing operation in
          the same batch. *)
  | Skipped
      (** The operation is skipped because of a previous failing operation in
          the same batch. *)
  | Failed of error trace  (** The operation failed with the provided error. *)

type operation_status = Successful | Unsuccessful of unsuccessful_status

type simulation_status = {index_in_batch : int; status : operation_status}

type 'unsigned_op simulation_result = {
  operations_statuses : simulation_status list;
  unsigned_operation : 'unsigned_op;
}

(** Action to be taken for unsuccessful operation. *)
type retry_action =
  | Retry  (** The operation is retried by being re-queued for injection. *)
  | Forget  (** The operation is forgotten without error. *)
  | Abort of error trace
      (** The error for the failing operation should be propagated at a higher
          level. *)

(** Signature for tags used in injector  *)
module type TAG = sig
  include Stdlib.Set.OrderedType

  include Stdlib.Hashtbl.HashedType with type t := t

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

module type PARAM_OPERATION = sig
  (** The abstract type of operations to inject *)
  type t

  (** An encoding for injector's operations *)
  val encoding : t Data_encoding.t

  (** Pretty-printing injector's operations *)
  val pp : Format.formatter -> t -> unit
end

(** Internal representation of injector operations. *)
module type INJECTOR_OPERATION = sig
  type operation

  (** Hash with b58check encoding iop(53), for hashes of injector operations *)
  module Hash : Tezos_crypto.Intfs.HASH

  (** Alias for L1 operations hashes *)
  type hash = Hash.t

  (** The type of L1 operations that are injected on Tezos. These have a hash
      attached to them that allows tracking and retrieving their status. *)
  type t = private {hash : hash; operation : operation}

  (** [make op] returns an L1 operation with the corresponding hash. *)
  val make : operation -> t

  (** Encoding for L1 operations *)
  val encoding : t Data_encoding.t

  (** Pretty printer for L1 operations. Only the relevant part for the rollup node
      is printed. *)
  val pp : Format.formatter -> t -> unit
end

(** Module type for parameter of functor {!Injector_functor.Make}. *)
module type PARAMETERS = sig
  (** The type of the state that the injector can access *)
  type state

  (** A module which contains the different tags for the injector *)
  module Tag : TAG

  (** A module for the injector operations *)
  module Operation : PARAM_OPERATION

  (** Where to put the events for this injector  *)
  val events_section : string list

  (** Coarse approximation for the number of operation of each tag we expect to
      inject for each block. *)
  val table_estimated_size : Tag.t -> int

  (** Action (see {!retry_action}) to be taken on unsuccessful operation (see
      {!unsuccessful_status}). *)
  val retry_unsuccessful_operation :
    state -> Operation.t -> unsuccessful_status -> retry_action Lwt.t

  (** The tag of a manager operation. This is used to send operations to the
      correct queue automatically (when signer is not provided) and to recover
      persistent information. *)
  val operation_tag : Operation.t -> Tag.t

  (** Returns the fee_parameter (to compute fee w.r.t. gas, size, etc.) and the
      caps of fee and burn for each operation. *)
  val fee_parameter : state -> Operation.t -> fee_parameter

  (** When injecting the given [operations] in an L1 batch, if
     [batch_must_succeed operations] returns [`All] then all the operations must
     succeed in the simulation of injection. If it returns [`At_least_one], at
     least one operation in the list [operations] must be successful in the
     simulation. In any case, only operations which are known as successful will
     be included in the injected L1 batch. {b Note}: Returning [`At_least_one]
     allows to incrementally build "or-batches" by iteratively removing
     operations that fail from the desired batch. *)
  val batch_must_succeed : Operation.t list -> [`All | `At_least_one]
end

module type PROTOCOL_CLIENT = sig
  type state

  type operation

  type unsigned_operation

  val max_operation_data_length : int

  (** The validation pass of manager operations. *)
  val manager_pass : int

  (** [operation_status block oph ~index] returns the status of the operation at
      position [index] in the L1 batch [oph] included in the block [block]. It
      returns [None] if the operation with the given index is not in the
      block. *)
  val operation_status :
    state ->
    Block_hash.t ->
    Operation_hash.t ->
    index:int ->
    operation_status option tzresult Lwt.t

  (** Size of an operation in bytes according to the protocol. This only
      accounts for the actual content of the corresponding manager operation
      (and not its fees, gas, etc.). *)
  val operation_size : operation -> int

  (** An upper bound of the overhead added to manager operations in
      bytes. Typically, this would include the source, fees, counter, gas limit,
      and storage limit. *)
  val operation_size_overhead : int

  (** Simulating a batch of operations. This function returns the simulation
      result for each of these operations (with its associated index in the
      batch, in case there is a revelation operation added) together with a
      Tezos raw unsigned operation that can be directly injected on a node if
      one wishes to. *)
  val simulate_operations :
    #Client_context.full ->
    force:bool ->
    source:Signature.public_key_hash ->
    src_pk:Signature.public_key ->
    successor_level:bool ->
    fee_parameter:fee_parameter ->
    operation list ->
    ( unsigned_operation simulation_result,
      [`Exceeds_quotas of tztrace | `TzError of tztrace] )
    result
    Lwt.t

  (** Sign an unsigned operation an return the serialized signed operation,
      ready for injection. *)
  val sign_operation :
    #Client_context.full ->
    Client_keys.sk_uri ->
    unsigned_operation ->
    bytes tzresult Lwt.t

  (** [time_until_next_block state block_header] computes the time until the
      block following [block_header], with respect to the current time. *)
  val time_until_next_block :
    state -> Tezos_base.Block_header.t option -> Ptime.span
end

(** Output signature for functor {!Injector_functor.Make}. *)
module type S = sig
  type state

  type tag

  type operation

  module Inj_operation : INJECTOR_OPERATION with type operation = operation

  (** Information stored about an L1 operation that was injected on a Tezos
    node. *)
  type injected_info = {
    op : Inj_operation.t;  (** The injector operation. *)
    oph : Operation_hash.t;
        (** The hash of the operation which contains [op] (this can be an L1 batch
          of several manager operations). *)
    op_index : int;
        (** The index of the operation [op] in the L1 batch corresponding to [oph]. *)
  }

  (** Information stored about an L1 operation that was included in a Tezos
    block. *)
  type included_info = {
    op : Inj_operation.t;  (** The injector operation. *)
    oph : Operation_hash.t;
        (** The hash of the operation which contains [op] (this can be an L1 batch
          of several manager operations). *)
    op_index : int;
        (** The index of the operation [op] in the L1 batch corresponding to [oph]. *)
    l1_block : Block_hash.t;
        (** The hash of the L1 block in which the operation was included. *)
    l1_level : int32;  (** The level of [l1_block]. *)
  }

  (** Status of an operation in the injector. *)
  type status =
    | Pending of operation  (** The operation is pending injection. *)
    | Injected of injected_info
        (** The operation has been injected successfully in the node. *)
    | Included of included_info
        (** The operation has been included in a L1 block. *)

  val injected_info_encoding : injected_info Data_encoding.t

  val included_info_encoding : included_info Data_encoding.t

  (** Initializes the injector with the rollup node state, for a list of
      signers, and start the workers. Each signer has its own worker with a
      queue of operations to inject.

      [retention_period] is the number of blocks for which the injector keeps
      the included information for, must be positive or zero. By default (when
      [0]), the injector will not keep information longer than necessary. It can
      be useful to set this value to something [> 0] if we want to retrieve
      information about operations included on L1 for a given period.

      The injector monitors L1 heads to update the statuses of its operations
      accordingly. The argument [reconnection_delay] gives an initial value for
      the delay before attempting a reconnection (see {!Layer_1.init}).
  *)
  val init :
    #Client_context.full ->
    data_dir:string ->
    ?retention_period:int ->
    ?reconnection_delay:float ->
    state ->
    signers:(Signature.public_key_hash * injection_strategy * tag list) list ->
    unit tzresult Lwt.t

  (** Add an operation as pending injection in the injector. If the source is
      not provided, the operation is queued to the worker which handles the
      corresponding tag. It returns the hash of the operation in the injector
      queue. *)
  val add_pending_operation :
    ?source:Signature.public_key_hash ->
    operation ->
    Inj_operation.Hash.t tzresult Lwt.t

  (** Trigger an injection of the pending operations for all workers. If [tags]
      is given, only the workers which have a tag in [tags] inject their pending
      operations. [header] must be provided for the [`Delay_block] strategy to
      compute the next block timestamp. *)
  val inject :
    ?tags:tag list -> ?header:Tezos_base.Block_header.t -> unit -> unit Lwt.t

  (** Shutdown the injectors, waiting for the ongoing request to be processed. *)
  val shutdown : unit -> unit Lwt.t

  (** The status of an operation in the injector.  *)
  val operation_status : Inj_operation.Hash.t -> status option
end
