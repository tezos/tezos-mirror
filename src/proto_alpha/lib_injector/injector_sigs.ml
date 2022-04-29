(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context

(** Type to represent {e appoximate upper-bounds} for the fee and limits, used
    to compute an upper bound on the size (in bytes) of an operation. *)
type approximate_fee_bound = {
  fee : Tez.t;
  counter : Z.t;
  gas_limit : Gas.Arith.integral;
  storage_limit : Z.t;
}

type injection_strategy =
  [ `Each_block  (** Inject pending operations after each new L1 block *)
  | `Delay_block
    (** Wait for some time after the L1 block is produced to inject pending
        operations. This strategy allows for maximizing the number of the same
        kind of operations to include in a block. *)
  ]

(** Signature for tags used in injector  *)
module type TAG = sig
  include Stdlib.Set.OrderedType

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

(** Module type for parameter of functor {!Injector_functor.Make}. *)
module type PARAMETERS = sig
  (** The type of the state for the rollup node that the injector can access *)
  type rollup_node_state

  (** A module which contains the different tags for the injector *)
  module Tag : TAG

  (** Where to put the events for this injector  *)
  val events_section : string list

  (** Coarse approximation for the number of operation of each tag we expect to
      inject for each block. *)
  val table_estimated_size : Tag.t -> int

  (** [requeue_reverted_operation state op] should return [true] if an included
      operation should be re-queued for injection when the block in which it is
      included is reverted (due to a reorganization). *)
  val requeue_reverted_operation :
    rollup_node_state -> 'a manager_operation -> bool Lwt.t

  (** [ignore_failing_operation op] specifies if the injector should
      ignore this operation when its simulation fails when trying to inject.
      Returns:
      - [`Ignore_keep] if the operation should be ignored but kept in the
        pending queue,
      - [`Ignore_drop] if the operation should be ignored and dropped from the
        pending queue,
      - [`Don't_ignore] if the failing operation should not be ignored and the
        failure reported.
  *)
  val ignore_failing_operation :
    'a manager_operation -> [`Ignore_keep | `Ignore_drop | `Don't_ignore]

  (** Returns the {e appoximate upper-bounds} for the fee and limits of an
      operation, used to compute an upper bound on the size (in bytes) for this
      operation. *)
  val approximate_fee_bound : 'a manager_operation -> approximate_fee_bound

  (** Returns the fee_parameter (to compute fee w.r.t. gas, size, etc.) and the
      caps of fee and burn for each operation. *)
  val fee_parameter :
    rollup_node_state -> 'a manager_operation -> Injection.fee_parameter

  (** When injecting the given [operations] in an L1 batch, if
     [batch_must_succeed operations] returns [`All] then all the operations must
     succeed in the simulation of injection. If it returns [`At_least_one], at
     least one operation in the list [operations] must be successful in the
     simulation. In any case, only operations which are known as successful will
     be included in the injected L1 batch. {b Note}: Returning [`At_least_one]
     allows to incrementally build "or-batches" by iteratively removing
     operations that fail from the desired batch. *)
  val batch_must_succeed :
    packed_manager_operation list -> [`All | `At_least_one]
end

(** Output signature for functor {!Injector_functor.Make}. *)
module type S = sig
  type rollup_node_state

  type tag

  (** Initializes the injector with the rollup node state, for a list of
      signers, and start the workers. Each signer has its own worker with a
      queue of operations to inject. *)
  val init :
    #Protocol_client_context.full ->
    rollup_node_state ->
    signers:(public_key_hash * injection_strategy * tag list) list ->
    unit tzresult Lwt.t

  (** Add an operation as pending injection in the injector. *)
  val add_pending_operation :
    source:public_key_hash -> 'a manager_operation -> unit tzresult Lwt.t

  (** Notify the injector of a new Tezos head. The injector marks the operations
      appropriately (for instance reverted operations that are part of a
      reorganization are put back in the pending queue). When an operation is
      considered as {e confirmed}, it disappears from the injector. *)
  val new_tezos_head :
    Protocol_client_context.Alpha_block_services.block_info ->
    Protocol_client_context.Alpha_block_services.block_info Common.reorg ->
    unit Lwt.t

  (** Trigger an injection of the pending operations for all workers. If [tags]
      is given, only the workers which have a tag in [tags] inject their pending
      operations. If [strategy] is given, only workers which have this strategy
      inject their pending operations. *)
  val inject :
    ?tags:tag list -> ?strategy:injection_strategy -> unit -> unit Lwt.t
end
