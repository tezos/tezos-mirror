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

(** This module is used by the prevalidator worker to determine
    whether an operation should be added to the mempool. To do so, it
    calls on the protocol's [Mempool] to validate the operation, as
    well as the {!Prevalidator_bounding} to ensure that the mempool
    remains bounded. It also retrieves relevant information from the
    protocol plugin. *)

module type T = sig
  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL.operation} *)
  type protocol_operation

  (** Mempool configuration that groups both the plugin config
      (e.g. minimal fee to pass the {!pre_filter}) and the bounding
      config (e.g. max number of valid operations in the mempool). *)
  type config

  (** Default mempool configuration. *)
  val default_config : config

  (** Encoding for {!config}.

      Internally an object without any variable fields. *)
  val config_encoding : config Data_encoding.t

  (** The type implemented by {!Tezos_store.Store.chain_store} in
      production, and mocked in tests *)
  type chain_store

  (** The state used internally by this module. Created by {!val-create}
      and then passed back and possibly updated by {!add_operation} and
      {!remove_operation}.

      This state notably contains a representation of the protocol
      mempool, as well as the bounding state. *)
  type t

  (** Create an empty state based on the [head] block.

      Called only once when a prevalidator starts. *)
  val create :
    chain_store ->
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    t tzresult Lwt.t

  (** Create a new empty state based on the [head] block.

      The previous state must be provided (even when it was based on a
      different block). Indeed, parts of it are recycled to make this
      function more efficient than [create]. *)
  val flush :
    chain_store ->
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    t ->
    t tzresult Lwt.t

  (** Light preliminary checks that should be performed on arrival of
      an operation and after a flush of the prevalidator.

      See [Shell_plugin.FILTER.Mempool.pre_filter]. *)
  val pre_filter :
    t ->
    config ->
    protocol_operation Shell_operation.operation ->
    [ `Passed_prefilter of Prevalidator_pending_operations.priority
    | Prevalidator_classification.error_classification ]
    Lwt.t

  (** Contain the hash and new classification of any operations that
      had to be removed to make room for a newly added operation. *)
  type replacements =
    (Operation_hash.t * Prevalidator_classification.error_classification) list

  (** Result of {!add_operation}.

      Contain the updated (or unchanged) state {!t}, the operation (in
      which the [signature_checked] field has been updated if
      appropriate), its classification, and the potential {!replacements}.

      Invariant: [replacements] can only be non-empty when the
      classification is [`Prechecked]. *)
  type add_result =
    t
    * protocol_operation Shell_operation.operation
    * Prevalidator_classification.classification
    * replacements

  (** Try and add an operation to the protocol's mempool; also ensure
      that this mempool remains bounded (in terms of both operation
      count and total byte size; the bounds are specified in the [config]).

      See {!add_result} for a description of the output. *)
  val add_operation :
    t ->
    config ->
    protocol_operation Shell_operation.operation ->
    add_result Lwt.t

  (** Remove an operation from the state.

      The state remains unchanged when the operation was not
      present (though not physically equal to the input state). *)
  val remove_operation : t -> Operation_hash.t -> t

  module Internal_for_tests : sig
    (** Return the map of operations currently present in the protocol
        representation of the mempool. *)
    val get_mempool_operations : t -> protocol_operation Operation_hash.Map.t

    (** Type {!Tezos_protocol_environment.PROTOCOL.Mempool.t}. *)
    type mempool

    (** Modify the [mempool] field of the internal state [t]. *)
    val set_mempool : t -> mempool -> t

    (** Type [Prevalidator_bounding.T.state]. *)
    type bounding_state

    (** Return the [bounding_state] component of the state. *)
    val get_bounding_state : t -> bounding_state

    (** Modify the [bounding_state] component of the state. *)
    val set_bounding_state : t -> bounding_state -> t
  end
end

(** How-to obtain an instance of this module's main module type: {!T} *)
module Make : functor (Filter : Shell_plugin.FILTER) ->
  T
    with type protocol_operation = Filter.Proto.operation
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

    (** [chain_id store] returns the {!Chain_id.t} to which [store]
        corresponds *)
    val chain_id : chain_store -> Chain_id.t
  end

  (** A variant of [Make] above that is parameterized by {!CHAIN_STORE},
      for mocking purposes. *)
  module Make : functor
    (Chain_store : CHAIN_STORE)
    (Filter : Shell_plugin.FILTER)
    (Bounding : Prevalidator_bounding.T
                  with type protocol_operation = Filter.Proto.operation)
    ->
    T
      with type protocol_operation = Filter.Proto.operation
       and type chain_store = Chain_store.chain_store
       and type Internal_for_tests.mempool = Filter.Proto.Mempool.t
       and type Internal_for_tests.bounding_state = Bounding.state
end
