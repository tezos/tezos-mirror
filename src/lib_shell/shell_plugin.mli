(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

(** Type of a protocol-specific mempool filter plugin. *)
module type FILTER = sig
  module Proto : Registered_protocol.T

  module Mempool : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    (** Internal state of the prevalidator filter *)
    type state

    (** Create an empty [state].

        Called only once when a prevalidator starts. *)
    val init :
      Tezos_protocol_environment.Context.t ->
      head:Tezos_base.Block_header.shell_header ->
      state tzresult Lwt.t

    (** Create a new empty [state] based on [head].

        Parts of the old [state] are recycled, so that this function
        is more efficient than [init] and does not need a
        [Tezos_protocol_environment.Context.t] argument. *)
    val flush :
      state -> head:Tezos_base.Block_header.shell_header -> state tzresult Lwt.t

    (** [remove ~filter_state oph] removes the operation manager linked to
        [oph] from the state of the filter *)
    val remove : filter_state:state -> Operation_hash.t -> state

    (** [pre_filter config ~filter_state operation_data]
        is called on arrival of an operation and after a flush of
        the prevalidator. This function calls the [pre_filter] in the protocol
        plugin and returns [`Passed_prefilter priority] if no error occurs during
        checking of the [operation_data], where priority is the priority computed by
        the protocol filter plug-in. More tests are done using the
        [filter_state]. We classify an operation that passes the prefilter as
        [`Passed_prefilter] since we do not know yet if the operation is
        applicable or not. If an error occurs during the checks, this function
        returns an error corresponding to the kind of the error returned by the
        protocol. *)
    val pre_filter :
      config ->
      filter_state:state ->
      Proto.operation ->
      [ `Passed_prefilter of Prevalidator_pending_operations.priority
      | Prevalidator_classification.error_classification ]
      Lwt.t

    (** Add an operation to the filter {!state}.

        The operation should have been previously validated by the protocol.

        This function is responsible for bounding the number of
        manager operations in the mempool. If the mempool is full and
        the input operation is a manager operation, then it is compared
        with the already present operation with minimal weight. Then
        either the minimal operation is replaced, or the new operation
        is rejected.

        If successful, return the updated state and possibly the
        replaced minimal operation, otherwise return the error
        classification for the new operation.

        If [replace] is provided, then it is removed from the state
        before processing the new operation (in which case the mempool
        can no longer be full, so this function will succeed and return
        [`No_replace]). *)
    val add_operation_and_enforce_mempool_bound :
      ?replace:Operation_hash.t ->
      config ->
      state ->
      Operation_hash.t * Proto.operation ->
      ( state
        * [ `No_replace
          | `Replace of
            Operation_hash.t * Prevalidator_classification.error_classification
          ],
        Prevalidator_classification.error_classification )
      result
      Lwt.t

    (** Return a conflict handler for [Proto.Mempool.add_operation].

        See the documentation of type [Mempool.conflict_handler] in
        e.g. [lib_protocol_environment/sigs/v8/updater.mli]. *)
    val conflict_handler : config -> Proto.Mempool.conflict_handler
  end
end

(** Type of a protocol-specific RPC plug-in. *)
module type RPC = sig
  module Proto : Registered_protocol.T

  val rpc_services :
    Tezos_protocol_environment.rpc_context Tezos_rpc.Directory.directory
end

(** Dummy filter that does nothing *)
module No_filter (Proto : Registered_protocol.T) :
  FILTER with module Proto = Proto and type Mempool.state = unit

(** This is a protocol specific module that is used to collect all the
   * protocol-specific metrics. This module
   * allows to decode protocol data payload and provide back basic
   * types that can be used as metrics. *)
module type METRICS = sig
  val hash : Protocol_hash.t

  val update_metrics :
    protocol_metadata:bytes ->
    Fitness.t ->
    (cycle:float -> consumed_gas:float -> round:float -> unit) ->
    unit Lwt.t
end

(** Emtpy metrics module. All metrics are -1. *)
module Undefined_metrics_plugin (P : sig
  val hash : Protocol_hash.t
end) : METRICS

(** Register a mempool filter plugin for a specific protocol
    (according to its [Proto.hash]). *)
val register_filter : (module FILTER) -> unit

(** Registers a RPC plug-in for a specific protocol *)
val register_rpc : (module RPC) -> unit

(** Register a metrics plugin module *)
val register_metrics : (module METRICS) -> unit

(** Looks for a mempool filter plug-in for a specific protocol. *)
val find_filter : Protocol_hash.t -> (module FILTER) option

(** Looks for an rpc plug-in for a specific protocol. *)
val find_rpc : Protocol_hash.t -> (module RPC) option

(** Looks for a metrics plugin module for a specific protocol *)
val find_metrics : Protocol_hash.t -> (module METRICS) option

(** Same as [find_metrics] but returns [Undefined_metrics_plugin] if not found *)
val safe_find_metrics : Protocol_hash.t -> (module METRICS) Lwt.t
