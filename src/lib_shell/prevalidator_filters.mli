(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

(** Type of a protocol-specific mempool filter plug-in. *)
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
    val remove : filter_state:state -> Operation_hash.t -> state

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

        This function takes a [state] as parameter and returns it updated if the
        operation has been [prechecked]. It also takes an under-approximation
        [nb_successful_prechecks] of the number of times the given operation
        has been successfully prechecked. *)
    val precheck :
      config ->
      filter_state:state ->
      validation_state:Proto.validation_state ->
      Operation_hash.t ->
      Proto.operation ->
      nb_successful_prechecks:int ->
      [ `Passed_precheck of
        state
        * [ `No_replace
          | `Replace of
            Operation_hash.t * Prevalidator_classification.error_classification
          ]
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
  end

  module RPC : sig
    val rpc_services : Environment_context.rpc_context RPC_directory.directory
  end
end

(** Dummy filter that does nothing *)
module No_filter (Proto : Registered_protocol.T) :
  FILTER with module Proto = Proto

(** Registers a mempool plug-in for a specific protocol (according to its [Proto.hash]). *)
val register : (module FILTER) -> unit

(** Looks for a mempool plug-in for a specific protocol. *)
val find : Protocol_hash.t -> (module FILTER) option
