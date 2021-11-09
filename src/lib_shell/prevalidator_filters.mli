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

    (** [precheck validation_state shell_header op] should be used to
        decide whether an operation can be propagated over the gossip
        network without executing it. This is a wrapper around
        [Proto.precheck_manager] and [Proto.check_signature]. This
        function hereby has a similar return type.

        Returns [`Prechecked] if the operation was successfully
        prechecked. If the function returns [`Undecided] it means that
        [apply_operation] should be called.

        Note: Currently this function directly returns [`Undecided] for
        non-manager operations.
        This function can return [`Prechecked] only for manager operations. *)
    val precheck :
      validation_state:Proto.validation_state ->
      Tezos_base.Operation.shell_header ->
      Proto.operation_data ->
      [ `Prechecked
      | `Branch_delayed of tztrace
      | `Branch_refused of tztrace
      | `Refused of tztrace
      | `Outdated of tztrace
      | `Undecided ]
      Lwt.t

    (** [pre_filter config ~filter_state ?validation_state_before operation_data]
        is called on arrival of an operation and after a flush of
        the prevalidator. This function calls the [pre_filter] in the protocol
        plugin and returns [`Undecided] if no error occurs during checking of
        the [operation_data]. We classify an operation that pass the prefilter
        as [`Undecided] since we do not know yet if the operation is applicable
        or not. If an error occurs during the checks, this function returns an error
        corresponding to the kind of the error returned by the protocol.
        This function both takes a [state] as parameter and
        returns a [state], because it can update it while executing. *)
    val pre_filter :
      config ->
      filter_state:state ->
      ?validation_state_before:Proto.validation_state ->
      Proto.operation_data ->
      ([ `Undecided
       | `Branch_delayed of tztrace
       | `Branch_refused of tztrace
       | `Refused of tztrace
       | `Outdated of tztrace ]
      * state)
      Lwt.t

    (** [post_filter config ~filter_state ~validation_state_before
        ~validation_state_after (operation_data, operation_receipt)]
        is called after a call to
        [Prevalidation.apply_operation] in the prevalidator, on operations that
        did not fail. It calls the [post_filter] function in the protocol
        plugin and returns [`Applied] if no error occurs during the checking of
        the [operation_receipt]. If an error occurs during the checks, returns
        an error corresponding to the kind of the error returned by the
        protocol. This function both takes a [state] as parameter and
        returns a [state], because it can update it while executing. *)
    val post_filter :
      config ->
      filter_state:state ->
      validation_state_before:Proto.validation_state ->
      validation_state_after:Proto.validation_state ->
      Proto.operation_data * Proto.operation_receipt ->
      (bool * state) Lwt.t
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
