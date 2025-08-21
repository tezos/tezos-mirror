(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
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

(** Empty metrics module. All metrics are -1. *)
module Undefined_metrics_plugin (P : sig
  val hash : Protocol_hash.t
end) : METRICS

(** Protocol specific plugin to expose some helpers function that are
    helpful in scope of the Shell. *)
module type SHELL_HELPERS = sig
  val hash : Protocol_hash.t

  (** [get_blocks_per_cycle ctxt] returns the blocks_per_cycle
      protocol constant. *)
  val get_blocks_per_cycle :
    Tezos_protocol_environment.Context.t -> int32 option Lwt.t
end

(** Register a validation plugin for a specific protocol
    (according to its [Proto.hash]). *)
val register_validation_plugin : (module T) -> unit

(** Registers a RPC plug-in for a specific protocol *)
val register_rpc : (module RPC) -> unit

(** Register a metrics plugin module *)
val register_metrics : (module METRICS) -> unit

(** Register a Http_cache_headers plugin module *)
val register_http_cache_headers_plugin : (module HTTP_CACHE_HEADERS) -> unit

(** Register a Shell_helpers plugin module *)
val register_shell_helpers : (module SHELL_HELPERS) -> unit

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

(** Looks for a shell helpers plugin module for a specific protocol *)
val find_shell_helpers : Protocol_hash.t -> (module SHELL_HELPERS) option

(** The types below are used to implement the
    {!Shell_services.Chain.S.delegators_contribution} RPC in
    [src/lib_shell/chain_directory.ml]. *)

(** Partial breakdown of delegated tez contribution to baking rights.

    Contains the data available from the context at the level when the
    baking rights for the cycle of interest were sampled. In
    particular, only the total sum [min_delegated_amount] is available
    for the contributions from delegated balances of both the delegate
    and its delegators; breaking it down further requires access to
    the context at [min_delegated_level].

    All int64 values are mutez amount. *)
type delegated_breakdown_at_sampling = {
  min_delegated_amount : int64;
      (** Sum of the contribution to the delegate's baking rights from
          the delegated balances of the delegate and its (current and
          former) delegators. Includes the delegate and current
          delegators' spendable balances, frozen bonds, and unstake
          requests associated with this delegate. Also includes
          unstaked requests that belong to former delegators but are
          still associated with this delegate. Excludes delegators'
          lingering unstake requests associated with an older
          delegate. *)
  min_delegated_level : int32;
      (** Level of min-delegated-in-current-cycle, whose context
          contains the information needed to break down
          [min_delegated_amount]. *)
  overstaked : int64;
      (** Tez that are part of external stakers' staked balances, but
          contribute to the delegated portion of baking power because
          of overstaking. *)
  total_delegated_including_overdelegated : int64;
      (** Total contribution to the delegated portion of baking power,
          before computing overdelegation.

          Invariant: [total_delegated_including_overdelegated =
          min_delegated_amount + overstaked] *)
  total_delegated_after_limits : int64;
      (** Actual total contribution to the delegated portion of baking
          power, constrained by overdelegation.

          Invariant: [total_delegated_after_limits <=
          total_delegated_including_overdelegated] *)
  overdelegated : int64;
      (** Amount that is delegated to the delegate but contributes
          nothing to its baking power because of overdelegation.

          Invariant: [total_delegated_including_overdelegated =
          total_delegated_after_limits + overdelegated] *)
}

(** Breakdown of {!field-min_delegated_amount}.

    Invariant:
    [total_delegated = own_delegated +
    sum_amounts(delegators_contributions) +
    former_delegators_unstake_requests]
*)
type min_delegated_breakdown = {
  total_delegated : int64;
      (** Sum of all contributions to the delegate's baking power from
          delegated balances. Same as {!field-min_delegated_amount}. *)
  own_delegated : int64;
      (** Contribution from the delegate's own delegated
          balance. Includes the delegate's spendable balance, frozen
          bonds, and unstake requests associated with itself. Exclude
          any unstake requests associated with an older delegate. *)
  delegators_contributions : (string * int64) list;
      (** Contract hash (pkh or KT) and contribution for each current
          external delegator. Includes the delegator's spendable
          balance, frozen bonds, and unstake requests associated with
          the delegate. Exclude any unstake requests associated with
          an older delegate. *)
  former_delegators_unstake_requests : int64;
      (** Sum of the unstake requests that belong to the delegate's
          former delegators but are still associated with the
          delegate. *)
}

module type DELEGATORS_CONTRIBUTION = sig
  val hash : Protocol_hash.t

  (** - Returns [`Ok delegated_breakdown_at_sampling] if the provided
        context's current level is the level at which the baking rights
        for [cycle] have been sampled. See
        {!type-delegated_breakdown_at_sampling}.

      - Returns [`Cycle_too_far_in_future] if the sampling level for
        [cycle] is higher than the context's current level.

      - Returns [`Retry_at_level retry_level] if the sampling level
        for [cycle] is lower than the context's current level,
        guaranteing that:

        [actual_sampling_level_for_cycle <= retry_level < current_level]

      This function should be called initially on the context of the
      current head of the chain, then again on the context of
      [retry_level] in the [`Retry_at_level] case. The inequality on
      [retry_level] ensures that this will eventually return another
      result than [`Retry_at_level], and that this will only return
      [`Cycle_too_far_in_future] if the sampling level is higher than
      the level of current head of the chain. *)
  val delegated_breakdown_at_sampling :
    Block_header.shell_header * Tezos_protocol_environment.Context.t ->
    cycle:int32 ->
    delegate_pkh:Signature.public_key_hash ->
    [ `Ok of delegated_breakdown_at_sampling
    | `Retry_at_level of int32
    | `Cycle_too_far_in_future ]
    Error_monad.tzresult
    Lwt.t

  (** When called on the context of {!field-min_delegated_level},
      returns the breakdown of {!field-min_delegated_amount}. See
      {!type-min_delegated_breakdown}. *)
  val min_delegated_breakdown :
    Block_header.shell_header * Tezos_protocol_environment.Context.t ->
    delegate_pkh:Signature.public_key_hash ->
    min_delegated_breakdown Error_monad.tzresult Lwt.t
end

val register_delegators_contribution : (module DELEGATORS_CONTRIBUTION) -> unit

val find_delegators_contribution :
  Protocol_hash.t -> (module DELEGATORS_CONTRIBUTION) option
