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

(** Type of a protocol-specific mempool filter plugin.

    This is compatible with the plugins of protocols Lima and up. For
    Kathmandu and older protocols, see {!Legacy_mempool_plugin.FILTER}. *)
module type FILTER = sig
  module Proto : Registered_protocol.T

  module Mempool : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    (** Internal state of the prevalidator filter *)
    type state

    (** [init config ?validation_state ~predecessor] is called once when
        a prevalidator starts. *)
    val init :
      config ->
      ?validation_state:Proto.validation_state ->
      predecessor:Tezos_base.Block_header.t ->
      unit ->
      state tzresult Lwt.t

    (** [on_flush config state ?validation_state ~predecessor] is called when
        a flush in the prevalidator is triggered. It resets part of the
        [state]. *)
    val on_flush :
      config ->
      state ->
      ?validation_state:Proto.validation_state ->
      predecessor:Tezos_base.Block_header.t ->
      unit ->
      state tzresult Lwt.t

    (** [remove ~filter_state oph] removes the operation manager linked to
        [oph] from the state of the filter *)
    val remove : filter_state:state -> Tezos_crypto.Operation_hash.t -> state

    (** [precheck config ~filter_state ~validation_state oph op
        ~nb_successful_prechecks]
        should be used to decide whether an operation can be gossiped to the
        network without executing it. This is a wrapper around
        [Proto.precheck_manager] and [Proto.check_signature]. This
        function hereby has a similar return type.

        Returns [`Passed_precheck `No_replace] if the operation was successfully
        prechecked. In case the operation is successfully prechecked
        but replaces an already prechecked operation [old_oph], the
        result [`Passed_precheck (`Replace (old_oph, clasification))] is
        returned, where [classification] is the new classifiation of the
        replaced operation. If the function returns [`Undecided] it means that
        [apply_operation] should be called.

        This function takes a filter [state] and a [Proto.validation_state]
        as parameters, and returns them updated if the operation has been
        successfully [prechecked]. It also takes an under-approximation
        [nb_successful_prechecks] of the number of times the given operation
        has been successfully prechecked. *)
    val precheck :
      config ->
      filter_state:state ->
      validation_state:Proto.validation_state ->
      Tezos_crypto.Operation_hash.t ->
      Proto.operation ->
      nb_successful_prechecks:int ->
      [ `Passed_precheck of
        state
        * Proto.validation_state
        * [ `No_replace
          | `Replace of
            Tezos_crypto.Operation_hash.t
            * Prevalidator_classification.error_classification ]
      | `Undecided
      | Prevalidator_classification.error_classification ]
      Lwt.t

    (** [pre_filter config ~filter_state ?validation_state_before operation_data]
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
      ?validation_state_before:Proto.validation_state ->
      Proto.operation ->
      [ `Passed_prefilter of Prevalidator_pending_operations.priority
      | Prevalidator_classification.error_classification ]
      Lwt.t

    (** [post_filter config ~filter_state ~validation_state_before
        ~validation_state_after (operation_data, operation_receipt)]
        is called after a call to [Prevalidation.apply_operation] in the
        prevalidator, on operations that did not fail. It returns
        [`Passed_postfilter] if the operation passes the filter. It returns
        [`Refused] otherwise. This function both takes a [filter_state] as
        parameter and returns a [filter_state], because it can update it while
        executing. *)
    val post_filter :
      config ->
      filter_state:state ->
      validation_state_before:Proto.validation_state ->
      validation_state_after:Proto.validation_state ->
      Proto.operation * Proto.operation_receipt ->
      [`Passed_postfilter of state | `Refused of tztrace] Lwt.t

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
      ?replace:Tezos_crypto.Operation_hash.t ->
      Proto.validation_state ->
      config ->
      state ->
      Tezos_crypto.Operation_hash.t * Proto.operation ->
      ( state
        * [ `No_replace
          | `Replace of
            Tezos_crypto.Operation_hash.t
            * Prevalidator_classification.error_classification ],
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
  val hash : Tezos_crypto.Protocol_hash.t

  val update_metrics :
    protocol_metadata:bytes ->
    Fitness.t ->
    (cycle:float -> consumed_gas:float -> round:float -> unit) ->
    unit Lwt.t
end

(** Emtpy metrics module. All metrics are -1. *)
module Undefined_metrics_plugin (P : sig
  val hash : Tezos_crypto.Protocol_hash.t
end) : METRICS

(** Juggling between recent filter version {!FILTER}, designed for
    Lima (environment V7) and newer protocols, and legacy filter
    version {!Legacy_mempool_plugin.FILTER}. *)
type filter_t =
  | Recent of (module FILTER)
  | Legacy of (module Legacy_mempool_plugin.FILTER)

(** Dummy filter that does nothing. *)
val no_filter : (module Registered_protocol.T) -> filter_t

(** Register a mempool filter plugin for a specific protocol
    (according to its [Proto.hash]). The protocol must be Lima or a
    more recent one. *)
val register_filter : (module FILTER) -> unit

(** Register a mempool filter plugin for a specific protocol
    (according to its [Proto.hash]). The protocol must be Kathmandu
    or older. *)
val register_legacy_filter : (module Legacy_mempool_plugin.FILTER) -> unit

(** Registers a RPC plug-in for a specific protocol *)
val register_rpc : (module RPC) -> unit

(** Register a metrics plugin module *)
val register_metrics : (module METRICS) -> unit

(** Looks for a mempool filter plug-in for a specific protocol. *)
val find_filter : Tezos_crypto.Protocol_hash.t -> filter_t option

(** Looks for an rpc plug-in for a specific protocol. *)
val find_rpc : Tezos_crypto.Protocol_hash.t -> (module RPC) option

(** Looks for a metrics plugin module for a specific protocol *)
val find_metrics : Tezos_crypto.Protocol_hash.t -> (module METRICS) option

(** Same as [find_metrics] but returns [Undefined_metrics_plugin] if not found *)
val safe_find_metrics : Tezos_crypto.Protocol_hash.t -> (module METRICS) Lwt.t
