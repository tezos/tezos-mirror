(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Type of a protocol accompanied with its main plugin (aka the
    validation & mempool plugin).

    Implementations for such a plugin can be found in
    [src/proto_xxx/lib_plugin/mempool.ml]. *)
module type T = sig
  include Registered_protocol.T

  module Plugin : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    (** Static internal information needed by {!pre_filter}.

        It depends on the [head] block upon which a mempool is built. *)
    type info

    (** Create an {!info} based on the [head] block.

        Should be called only once when a new prevalidator is started
        for a new protocol. Subsequent {!info}s should be
        created using {!flush}. *)
    val init :
      Tezos_protocol_environment.Context.t ->
      head:Tezos_base.Block_header.shell_header ->
      info tzresult Lwt.t

    (** Create a new {!info} based on the [head] block.

        Parts of the old {!info} (which may have been built on
        a different block) are recycled, so that this function is more
        efficient than {!init} and does not need a
        {!Tezos_protocol_environment.Context.t} argument. *)
    val flush :
      info -> head:Tezos_base.Block_header.shell_header -> info tzresult Lwt.t

    (** Perform some syntactic checks on the operation.

        To be used mostly as an exceptional mechanism to prevent
        ill-formed operations to block block application.

        Note that the functions exposed in the output of
        {!proto_with_validation_plugin} already call [syntactic_check]
        when appropriate. *)
    val syntactic_check : operation -> [`Well_formed | `Ill_formed] Lwt.t

    (** Perform some light preliminary checks on the operation.

        If successful, return [`Passed_prefilter] with the priority of the
        operation, based on the operation kind and potentially its
        fee, gas, and size. If not, return a classification containing
        the encountered error.

        Should be called on arrival of an operation and after a flush
        of the prevalidator. *)
    val pre_filter :
      info ->
      config ->
      operation ->
      [ `Passed_prefilter of [`High | `Medium | `Low of Q.t list]
      | `Branch_delayed of tztrace
      | `Branch_refused of tztrace
      | `Refused of tztrace
      | `Outdated of tztrace ]
      Lwt.t

    (** Return a conflict handler for {!Mempool.add_operation}.

        See the documentation of type {!Mempool.conflict_handler} in
        e.g. [lib_protocol_environment/sigs/v8/updater.mli].

        Precondition: both operations must be individually valid
        (required by the protocol's operation comparison on which the
        implementation of this function relies). *)
    val conflict_handler : config -> Mempool.conflict_handler

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
        t -> new_operation:operation -> replacements:operation list -> t

      (** This function should be called when
          {!Mempool.add_operation} has returned [Unchanged]. This
          means that the [candidate_op] has been rejected because there
          was a conflict with an pre-existing operation and the
          {!val-conflict_handler} has returned [`Keep]. This function
          returns the minimal fee (in mutez) that [candidate_op] would
          need so that the {!val-conflict_handler} would return
          [`Replace] instead. If no such fee exists, then the function
          returns [None]. *)
      val fee_needed_to_replace_by_fee :
        config -> candidate_op:operation -> conflict_map:t -> int64 option
    end

    (** Compute the minimal fee (expressed in mutez) that [candidate_op]
        would need to have in order to be strictly greater than
        [op_to_overtake] according to {!compare_operations}.

        Return [None] when at least one operation is not a manager operation.

        Also return [None] if both operations are manager operations but
        there was an error while computing the needed fee. However,
        note that this cannot happen when both manager operations have
        been successfully validated by the protocol. *)
    val fee_needed_to_overtake :
      op_to_overtake:operation -> candidate_op:operation -> int64 option

    (** Protocol context *)
    type ctxt

    (** Return the protocol context *)
    val get_context :
      Tezos_protocol_environment.Context.t ->
      head:Block_header.shell_header ->
      ctxt tzresult Lwt.t

    (** Return the sources from the operation *)
    val sources_from_operation :
      ctxt -> operation -> Signature.public_key_hash list Lwt.t
  end
end

(** Type of a protocol-specific RPC plug-in. *)
module type RPC = sig
  module Proto : Registered_protocol.T

  val rpc_services :
    Tezos_protocol_environment.rpc_context Tezos_rpc.Directory.directory

  (** Return the number of blocks preservation cycles from protocol constants. *)
  val get_blocks_preservation_cycles :
    get_context:(unit -> Tezos_protocol_environment.Context.t Lwt.t) ->
    int option Lwt.t
end

(** To use when no registered plugin is found. This module is
    functional; it just misses on the smarter logic that a plugin can
    add on top of the protocol. *)
module No_plugin (Proto : Registered_protocol.T) :
  T
    with type operation_data = Proto.operation_data
     and type operation = Proto.operation
     and type Mempool.t = Proto.Mempool.t
     and type Plugin.info = unit

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

(** Protocol specific plugin to expose helper functions in the
    implementation of Http cache headers RPC middleware. *)
module type HTTP_CACHE_HEADERS = sig
  val hash : Protocol_hash.t

  (** [get_round_end_time ctx curr_header] gets the time at which the 
    current round ends which is the time at which the next round starts. 
    Useful to get an estimate of when the next block should arrive.
*)
  val get_round_end_time :
    get_context:(unit -> Tezos_protocol_environment.Context.t Lwt.t) ->
    Tezos_base.Block_header.shell_header ->
    Time.System.t option Lwt.t
end

(** Emtpy metrics module. All metrics are -1. *)
module Undefined_metrics_plugin (P : sig
  val hash : Protocol_hash.t
end) : METRICS

(** Register a validation plugin for a specific protocol
    (according to its [Proto.hash]). *)
val register_validation_plugin : (module T) -> unit

(** Registers a RPC plug-in for a specific protocol *)
val register_rpc : (module RPC) -> unit

(** Register a metrics plugin module *)
val register_metrics : (module METRICS) -> unit

val register_http_cache_headers_plugin : (module HTTP_CACHE_HEADERS) -> unit

(** Retrieves the registered protocol with the provided hash and wraps it
    together with its validation plugin.

    If no validation plugin has been registered for the protocol, then
    uses {!No_plugin} which is functional, but not as smart as a
    protocol-specific plugin.

    Returns the error [Block_validator_errors.Unavailable_protocol]
    when there is no registered protocol with the given hash.

    The [block_hash] argument is only used as additional information
    for the potential aforementioned error. *)
val proto_with_validation_plugin :
  block_hash:Block_hash.t -> Protocol_hash.t -> (module T) tzresult Lwt.t

(** Looks for an rpc plug-in for a specific protocol. *)
val find_rpc : Protocol_hash.t -> (module RPC) option

(** Looks for a metrics plugin module for a specific protocol *)
val find_metrics : Protocol_hash.t -> (module METRICS) option

(** Same as [find_metrics] but returns [Undefined_metrics_plugin] if not found *)
val safe_find_metrics : Protocol_hash.t -> (module METRICS) Lwt.t

(** Looks for a http cache headers plugin module for a specific protocol *)
val find_http_cache_headers :
  Protocol_hash.t -> (module HTTP_CACHE_HEADERS) option
