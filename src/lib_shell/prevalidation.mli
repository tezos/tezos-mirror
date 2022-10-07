(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4113

   This file is part of the implementation of the new mempool, which
   uses features of the protocol that only exist since Lima.

   When you modify this file, consider whether you should also change
   the files that implement the legacy mempool for Kathmandu. They all
   start with the "legacy" prefix and will be removed when Lima is
   activated on Mainnet. *)

(** A newly received block is validated by replaying locally the block
    creation, applying each operation and its finalization to ensure their
    consistency. This module is stateless and creates and manipulates the
    prevalidation_state. *)

module type T = sig
  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL.operation} *)
  type protocol_operation

  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type validation_state

  (** Type {!Shell_plugin.FILTER.Mempool.state}. *)
  type filter_state

  (** Type {!Shell_plugin.FILTER.Mempool.config}. *)
  type filter_config

  (** The type implemented by {!Tezos_store.Store.chain_store} in
      production, and mocked in tests *)
  type chain_store

  (** The type used internally by this module. Created by {!create} and
      then passed back and possibly updated by {!apply_operation}. *)
  type t

  (** Creates a new prevalidation context w.r.t. the protocol associated with
      the head block. *)
  val create :
    chain_store ->
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    unit ->
    t tzresult Lwt.t

  (** If an old operation has been replaced by a newly added
      operation, then this type contains its hash and its new
      classification. If there is no replaced operation, this is [None]. *)
  type replacement =
    (Tezos_crypto.Operation_hash.t
    * Prevalidator_classification.error_classification)
    option

  (** Result of {!add_operation}.

      Contain the updated (or unchanged) state {!t} and
      {!filter_state}, the operation (in which
      [count_successful_prechecks] has been incremented if
      appropriate), its classification, and the potential
      {!replacement}.

      Invariant: [replacement] can only be [Some _] when the
      classification is [`Prechecked]. *)
  type add_result =
    t
    * filter_state
    * protocol_operation Shell_operation.operation
    * Prevalidator_classification.classification
    * replacement

  (** Call the protocol [Mempool.add_operation] function, providing it
      with the [conflict_handler] from the plugin.

      Then if the protocol accepts the operation, call the plugin
      [add_operation_and_enforce_mempool_bound], which is responsible
      for bounding the number of manager operations in the mempool.

      See {!add_result} for a description of the output. *)
  val add_operation :
    t ->
    filter_state ->
    filter_config ->
    protocol_operation Shell_operation.operation ->
    add_result Lwt.t

  (** [validation_state t] returns the subset of [t] corresponding
      to the type {!validation_state} of the protocol. *)
  val validation_state : t -> validation_state

  module Internal_for_tests : sig
    (** Return the map of operations currently present in the protocol
        representation of the mempool. *)
    val get_valid_operations :
      t -> protocol_operation Tezos_crypto.Operation_hash.Map.t
  end
end

(** How-to obtain an instance of this module's main module type: {!T} *)
module Make : functor (Filter : Shell_plugin.FILTER) ->
  T
    with type protocol_operation = Filter.Proto.operation
     and type validation_state = Filter.Proto.validation_state
     and type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type chain_store = Store.chain_store

(**/**)

module Internal_for_tests : sig
  module type CHAIN_STORE = sig
    (** The [chain_store] type. Implemented by
        {!Tezos_store.Store.chain_store} in production and mocked in
        tests *)
    type chain_store

    (** [context store block] checkouts and returns the context of [block] *)
    val context :
      chain_store ->
      Store.Block.t ->
      Tezos_protocol_environment.Context.t tzresult Lwt.t

    (** [chain_id store] returns the {!Tezos_crypto.Chain_id.t} to which [store]
        corresponds *)
    val chain_id : chain_store -> Tezos_crypto.Chain_id.t
  end

  (** A variant of [Make] above that is parameterized by {!CHAIN_STORE},
      for mocking purposes. *)
  module Make : functor
    (Chain_store : CHAIN_STORE)
    (Filter : Shell_plugin.FILTER)
    ->
    T
      with type protocol_operation = Filter.Proto.operation
       and type validation_state = Filter.Proto.validation_state
       and type filter_state = Filter.Mempool.state
       and type filter_config = Filter.Mempool.config
       and type chain_store = Chain_store.chain_store
end
