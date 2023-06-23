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

    Implementations for such a plugin can be found in
    [src/proto_xxx/lib_plugin/mempool.ml]. *)
module type FILTER = sig
  module Proto : Registered_protocol.T

  module Mempool : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    (** Static internal information needed by {!pre_filter}.

        It depends on the [head] block upon which a mempool is built. *)
    type filter_info

    (** Create a {!filter_info} based on the [head] block.

        Should be called only once when a new prevalidator is started
        for a new protocol. Subsequent {!filter_info}s should be
        created using {!flush}. *)
    val init :
      Tezos_protocol_environment.Context.t ->
      head:Tezos_base.Block_header.shell_header ->
      filter_info tzresult Lwt.t

    (** Create a new {!filter_info} based on the [head] block.

        Parts of the old {!filter_info} (which may have been built on
        a different block) are recycled, so that this function is more
        efficient than {!init} and does not need a
        {!Tezos_protocol_environment.Context.t} argument. *)
    val flush :
      filter_info ->
      head:Tezos_base.Block_header.shell_header ->
      filter_info tzresult Lwt.t

    (** Perform some light preliminary checks on the operation.

        If successful, return [`Passed_prefilter] with the priority of the
        operation, based on the operation kind and potentially its
        fee, gas, and size. If not, return a classification containing
        the encountered error.

        Should be called on arrival of an operation and after a flush
        of the prevalidator. *)
    val pre_filter :
      filter_info ->
      config ->
      Proto.operation ->
      [ `Passed_prefilter of Prevalidator_pending_operations.priority
      | Prevalidator_classification.error_classification ]
      Lwt.t

    (** Return a conflict handler for [Proto.Mempool.add_operation].

        See the documentation of type [Mempool.conflict_handler] in
        e.g. [lib_protocol_environment/sigs/v8/updater.mli].

        Precondition: both operations must be individually valid
        (required by the protocol's operation comparison on which the
        implementation of this function relies). *)
    val conflict_handler : config -> Proto.Mempool.conflict_handler

    (** The purpose of this module is to provide the
        [fee_needed_to_replace_by_fee] function. For this function to
        be correct, the caller must maintain the state of type [t] by
        calling [update] on each successfully validated operation and
        its induced replacements. *)
    module Conflict_map : sig
      (** Internal state needed by [fee_needed_to_replace_by_fee]. *)
      type t

      (** Initial state. *)
      val empty : t

      (** Removes all the [replacements] from the state then adds
          [new_operation]. *)
      val update :
        t ->
        new_operation:Proto.operation ->
        replacements:Proto.operation list ->
        t

      (** This function should be called when
          [Proto.Mempool.add_operation] has returned [Unchanged]. This
          means that the [candidate_op] has been rejected because there
          was a conflict with an pre-existing operation and the
          {!val-conflict_handler} has returned [`Keep]. This function
          returns the minimal fee (in mutez) that [candidate_op] would
          need so that the {!val-conflict_handler} would return
          [`Replace] instead. If no such fee exists, then the function
          returns [None]. *)
      val fee_needed_to_replace_by_fee :
        config -> candidate_op:Proto.operation -> conflict_map:t -> int64 option
    end

    (** Compute the minimal fee (expressed in mutez) that [candidate_op]
        would need to have in order to be strictly greater than
        [op_to_overtake] according to {!Proto.compare_operations}.

        Return [None] when at least one operation is not a manager operation.

        Also return [None] if both operations are manager operations but
        there was an error while computing the needed fee. However,
        note that this cannot happen when both manager operations have
        been successfully validated by the protocol. *)
    val fee_needed_to_overtake :
      op_to_overtake:Proto.operation ->
      candidate_op:Proto.operation ->
      int64 option
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
  FILTER with module Proto = Proto

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
