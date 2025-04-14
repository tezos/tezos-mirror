(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(*                                                                           *)
(*****************************************************************************)

module type T = sig
  include Registered_protocol.T

  module Plugin : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    type info

    val init :
      Tezos_protocol_environment.Context.t ->
      head:Tezos_base.Block_header.shell_header ->
      info tzresult Lwt.t

    val flush :
      info -> head:Tezos_base.Block_header.shell_header -> info tzresult Lwt.t

    val syntactic_check : operation -> [`Well_formed | `Ill_formed] Lwt.t

    val pre_filter :
      info ->
      config ->
      operation ->
      [ `Passed_prefilter of [`High | `Medium | `Low of Q.t list]
      | `Branch_delayed of tztrace
      | `Branch_refused of tztrace
      | `Refused of tztrace
      | `Outdated of tztrace ]

    val conflict_handler : config -> Mempool.conflict_handler

    module Conflict_map : sig
      type t

      val empty : t

      val update :
        t -> new_operation:operation -> replacements:operation list -> t

      val fee_needed_to_replace_by_fee :
        config -> candidate_op:operation -> conflict_map:t -> int64 option
    end

    val fee_needed_to_overtake :
      op_to_overtake:operation -> candidate_op:operation -> int64 option

    type ctxt

    val get_context :
      Tezos_protocol_environment.Context.t ->
      head:Block_header.shell_header ->
      ctxt tzresult Lwt.t

    val sources_from_operation :
      ctxt -> operation -> Signature.public_key_hash list Lwt.t
  end
end

module type RPC = sig
  module Proto : Registered_protocol.T

  val rpc_services :
    Tezos_protocol_environment.rpc_context Tezos_rpc.Directory.directory

  val get_blocks_preservation_cycles :
    get_context:(unit -> Tezos_protocol_environment.Context.t Lwt.t) ->
    int option Lwt.t
end

module No_plugin (Proto : Registered_protocol.T) :
  T
    with type operation_data = Proto.operation_data
     and type operation = Proto.operation
     and type Mempool.t = Proto.Mempool.t
     and type Plugin.info = unit = struct
  include Proto

  module Plugin = struct
    type config = unit

    let config_encoding = Data_encoding.empty

    let default_config = ()

    type info = unit

    let init _ ~head:_ = Lwt_result_syntax.return_unit

    let flush _ ~head:_ = Lwt_result_syntax.return_unit

    let syntactic_check _ = Lwt.return `Well_formed

    let pre_filter _ _ _ = `Passed_prefilter (`Low [])

    let conflict_handler _ ~existing_operation ~new_operation =
      if compare_operations existing_operation new_operation < 0 then `Replace
      else `Keep

    module Conflict_map = struct
      type t = unit

      let empty = ()

      let update _t ~new_operation:_ ~replacements:_ = ()

      let fee_needed_to_replace_by_fee _config ~candidate_op:_ ~conflict_map:_ =
        None
    end

    let fee_needed_to_overtake ~op_to_overtake:_ ~candidate_op:_ = None

    type ctxt = unit

    let get_context _ ~head:_ = Lwt_result_syntax.return_unit

    let sources_from_operation _ _ = Lwt_syntax.return_nil
  end
end

module type METRICS = sig
  val hash : Protocol_hash.t

  val update_metrics :
    protocol_metadata:bytes ->
    Fitness.t ->
    (cycle:float -> consumed_gas:float -> round:float -> unit) ->
    unit Lwt.t
end

module type HTTP_CACHE_HEADERS = sig
  val hash : Protocol_hash.t

  val get_round_end_time :
    get_context:(unit -> Tezos_protocol_environment.Context.t Lwt.t) ->
    Tezos_base.Block_header.shell_header ->
    Time.System.t option Lwt.t
end

module Undefined_metrics_plugin (Proto : sig
  val hash : Protocol_hash.t
end) =
struct
  let hash = Proto.hash

  let update_metrics ~protocol_metadata:_ _ _ = Lwt.return_unit
end

module type SHELL_HELPERS = sig
  val hash : Protocol_hash.t

  val get_blocks_per_cycle :
    Tezos_protocol_environment.Context.t -> int32 option Lwt.t
end

let rpc_table : (module RPC) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let metrics_table : (module METRICS) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let http_cache_headers_table : (module HTTP_CACHE_HEADERS) Protocol_hash.Table.t
    =
  Protocol_hash.Table.create 5

let shell_helpers_table : (module SHELL_HELPERS) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let register_rpc (module Rpc : RPC) =
  assert (not (Protocol_hash.Table.mem rpc_table Rpc.Proto.hash)) ;
  Protocol_hash.Table.add rpc_table Rpc.Proto.hash (module Rpc)

let register_metrics (module Metrics : METRICS) =
  Protocol_hash.Table.replace metrics_table Metrics.hash (module Metrics)

let register_http_cache_headers_plugin
    (module Http_cache_headers : HTTP_CACHE_HEADERS) =
  assert (
    not
      (Protocol_hash.Table.mem http_cache_headers_table Http_cache_headers.hash)) ;
  Protocol_hash.Table.add
    http_cache_headers_table
    Http_cache_headers.hash
    (module Http_cache_headers)

let register_shell_helpers (module Shell_helpers : SHELL_HELPERS) =
  assert (not (Protocol_hash.Table.mem shell_helpers_table Shell_helpers.hash)) ;
  Protocol_hash.Table.add
    shell_helpers_table
    Shell_helpers.hash
    (module Shell_helpers)

let find_rpc = Protocol_hash.Table.find rpc_table

let find_metrics = Protocol_hash.Table.find metrics_table

let find_http_cache_headers = Protocol_hash.Table.find http_cache_headers_table

let find_shell_helpers = Protocol_hash.Table.find shell_helpers_table

let safe_find_metrics hash =
  match find_metrics hash with
  | Some proto_metrics -> Lwt.return proto_metrics
  | None ->
      let module Metrics = Undefined_metrics_plugin (struct
        let hash = hash
      end) in
      Lwt.return (module Metrics : METRICS)

let validation_plugin_table : (module T) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let add_to_validation_plugin_table proto_hash proto_with_plugin =
  assert (not (Protocol_hash.Table.mem validation_plugin_table proto_hash)) ;
  Protocol_hash.Table.add validation_plugin_table proto_hash proto_with_plugin

let register_validation_plugin (module Proto_with_plugin : T) =
  add_to_validation_plugin_table
    Proto_with_plugin.hash
    (module Proto_with_plugin)

type error += Ill_formed_operation of Operation_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"validation.plugin.ill_formed_operation"
    ~title:"Ill_formed_operation"
    ~description:"Ill-formed operation filtered"
    ~pp:(fun ppf oph ->
      Format.fprintf
        ppf
        "Ill-formed operation filtered: %a."
        Operation_hash.pp
        oph)
    Data_encoding.(obj1 (req "operation_hash" Operation_hash.encoding))
    (function Ill_formed_operation oph -> Some oph | _ -> None)
    (fun oph -> Ill_formed_operation oph)

module Patch_T (Proto : T) : T = struct
  include Proto

  let validate_operation ?check_signature validation_state oph op =
    let open Lwt_syntax in
    let* status = Proto.Plugin.syntactic_check op in
    match status with
    | `Ill_formed -> Lwt_result_syntax.tzfail (Ill_formed_operation oph)
    | `Well_formed ->
        Proto.validate_operation ?check_signature validation_state oph op

  module Mempool = struct
    include Proto.Mempool

    let add_operation ?check_signature ?conflict_handler validation_info
        mempool_state (oph, op) =
      let open Lwt_syntax in
      let* status = Proto.Plugin.syntactic_check op in
      match status with
      | `Ill_formed ->
          Lwt.return_error
            (Proto.Mempool.Validation_error
               (TzTrace.make (Ill_formed_operation oph)))
      | `Well_formed ->
          Proto.Mempool.add_operation
            ?check_signature
            ?conflict_handler
            validation_info
            mempool_state
            (oph, op)
  end
end

let proto_with_validation_plugin ~block_hash protocol_hash =
  let open Lwt_result_syntax in
  let* (module Proto_with_plugin) =
    match Protocol_hash.Table.find validation_plugin_table protocol_hash with
    | Some proto_with_plugin -> return proto_with_plugin
    | None -> (
        match Registered_protocol.get protocol_hash with
        | None ->
            tzfail
              (Block_validator_errors.Unavailable_protocol
                 {block = block_hash; protocol = protocol_hash})
        | Some (module Proto : Registered_protocol.T) ->
            return (module No_plugin (Proto) : T))
  in
  return (module Patch_T (Proto_with_plugin) : T)

type delegated_breakdown_at_sampling = {
  min_delegated_amount : int64;
  min_delegated_level : int32;
  overstaked : int64;
  total_delegated_including_overdelegated : int64;
  total_delegated_after_limits : int64;
  overdelegated : int64;
}

type min_delegated_breakdown = {
  total_delegated : int64;
  own_delegated : int64;
  delegators_contributions : (string * int64) list;
  former_delegators_unstake_requests : int64;
}

module type DELEGATORS_CONTRIBUTION = sig
  val hash : Protocol_hash.t

  val delegated_breakdown_at_sampling :
    Tezos_base.Block_header.shell_header * Tezos_protocol_environment.Context.t ->
    cycle:int32 ->
    delegate_pkh:Signature.public_key_hash ->
    [ `Ok of delegated_breakdown_at_sampling
    | `Retry_at_level of int32
    | `Cycle_too_far_in_future ]
    Error_monad.tzresult
    Lwt.t

  val min_delegated_breakdown :
    Block_header.shell_header * Tezos_protocol_environment.Context.t ->
    delegate_pkh:Signature.public_key_hash ->
    min_delegated_breakdown Error_monad.tzresult Lwt.t
end

let delegators_contribution_table :
    (module DELEGATORS_CONTRIBUTION) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let register_delegators_contribution (module M : DELEGATORS_CONTRIBUTION) =
  assert (not (Protocol_hash.Table.mem delegators_contribution_table M.hash)) ;
  Protocol_hash.Table.add delegators_contribution_table M.hash (module M)

let find_delegators_contribution =
  Protocol_hash.Table.find delegators_contribution_table
