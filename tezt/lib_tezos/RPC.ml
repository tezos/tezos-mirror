(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Query_arg = struct
  let opt name f = function None -> [] | Some x -> [(name, f x)]

  let opt_list name f = function None -> [] | Some l -> List.map (f name) l

  let opt_bool name b = opt name string_of_bool b

  let switch name b = if b then [(name, "")] else []
end

module Decode = struct
  let mutez json = json |> JSON.as_int |> Tez.of_mutez_int

  (* - Returns [Some <n>] if the input is an integer.
     - Returns [None] if the input is [`Null].
     - Raises an exception in all other cases.

     The difference with {!JSON.as_int_opt} is that {!JSON.as_int_opt}
     never fails, but instead returns [None] whenever the input is
     anything else than an integer. *)
  let int_option json =
    if JSON.is_null json then None else Some (JSON.as_int json)
end

type 'result t = 'result RPC_core.t

type data = RPC_core.data

let make = RPC_core.make

let get_config = make GET ["config"] Fun.id

let get_config_history_mode = make GET ["config"; "history_mode"] Fun.id

let get_config_network_dal = make GET ["config"; "network"; "dal"] Fun.id

let get_network_connections =
  make GET ["network"; "connections"] @@ fun json ->
  let decode_connection json =
    let open JSON in
    let id_point = json |-> "id_point" in
    ( id_point |-> "addr" |> as_string,
      id_point |-> "port" |> as_int,
      json |-> "peer_id" |> as_string )
  in
  List.map decode_connection (JSON.as_list json)

let get_network_connection peer_id =
  make GET ["network"; "connections"; peer_id] @@ fun json ->
  let id_point = JSON.(json |-> "id_point") in
  (JSON.(id_point |-> "addr" |> as_string), JSON.(id_point |-> "port" |> as_int))

let post_private_injection_operations ?(use_tmp_file = false) ?(force = false)
    ?(async = false) ~ops () =
  let query_string =
    [("async", string_of_bool async); ("force", string_of_bool force)]
  in
  let data : RPC_core.data =
    if use_tmp_file then (
      let filename = Temp.file "injection_operations.json" in
      with_open_out filename (fun out ->
          let open Format in
          let fmt = formatter_of_out_channel out in
          fprintf
            fmt
            "[%a]"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",")
               (fun fmt (`Hex op) -> fprintf fmt {|"%s"|} op))
            ops) ;
      File filename)
    else Data (`A (List.map (fun (`Hex op) -> `String op) ops))
  in
  make ~data ~query_string POST ["private"; "injection"; "operations"]
  @@ fun json ->
  JSON.(json |> as_list |> List.map (fun json -> `OpHash (JSON.as_string json)))

let get_network_self = make GET ["network"; "self"] JSON.as_string

let get_network_greylist_ips = make GET ["network"; "greylist"; "ips"] Fun.id

let get_network_peers =
  make GET ["network"; "peers"] @@ fun json ->
  JSON.(json |> as_list |> List.map @@ fun p -> (p |=> 0 |> as_string, p |=> 1))

let patch_network_peer peer_id data =
  make
    PATCH
    ["network"; "peers"; peer_id]
    ~data:(Data (JSON.unannotate data))
    ignore

let get_network_peer peer_id = make GET ["network"; "peers"; peer_id] Fun.id

let get_network_peer_banned peer_id =
  make GET ["network"; "peers"; peer_id; "banned"] Fun.id

let get_chain_blocks ?(chain = "main") () =
  make GET ["chains"; chain; "blocks"] Fun.id

let get_chain_invalid_blocks ?(chain = "main") () =
  make GET ["chains"; chain; "invalid_blocks"] Fun.id

let get_chain_block_header_raw ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"; "raw"] Fun.id

let get_chain_block_live_blocks ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "live_blocks"] Fun.id

let decode_operation_hashes json = JSON.(json |> as_list |> List.map as_string)

let get_chain_block_operation_hashes ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "operation_hashes"] (fun json ->
      JSON.(json |> as_list |> List.map @@ decode_operation_hashes))

let get_chain_block_operation_hashes_of_validation_pass ?(chain = "main")
    ?(block = "head") validation_pass =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "operation_hashes";
      string_of_int validation_pass;
    ]
    decode_operation_hashes

let get_chain_block_operation_hash ?(chain = "main") ?(block = "head")
    ~validation_pass ~operation_offset () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "operation_hashes";
      string_of_int validation_pass;
      string_of_int operation_offset;
    ]
    JSON.as_string

let get_chain_block_helper_complete ?(chain = "main") ?(block = "head") prefix =
  make
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "complete"; prefix]
    Fun.id

let get_chain_block_helper_round ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "helpers"; "round"] JSON.as_int

let get_chain_block_context_nonce ?(chain = "main") ?(block = "head")
    block_level =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "nonces";
      string_of_int block_level;
    ]
    Fun.id

let get_chain_block_context_seed ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "context"; "seed"] Fun.id

let get_chain_block_context_seed_computation ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "seed_computation"]
    Fun.id

let get_chain_block_context_liquidity_baking_cpmm_address ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "liquidity_baking";
      "cpmm_address";
    ]
    JSON.as_string

let get_network_points =
  make GET ["network"; "points"] @@ fun json ->
  JSON.(json |> as_list |> List.map @@ fun p -> (p |=> 0 |> as_string, p |=> 1))

let get_network_point point_id = make GET ["network"; "points"; point_id] Fun.id

let patch_network_point point_id data =
  make
    PATCH
    ["network"; "points"; point_id]
    ~data:(Data (JSON.unannotate data))
    ignore

let get_network_point_banned point_id =
  make GET ["network"; "points"; point_id; "banned"] Fun.id

let get_network_stat = make GET ["network"; "stat"] Fun.id

let put_network_points point =
  make PUT ["network"; "points"; point] ~data:(Data (`O [])) Fun.id

let delete_network_greylist = make DELETE ["network"; "greylist"] Fun.id

let get_version = make GET ["version"] Fun.id

let post_injection_operation ?(async = false) data =
  make
    POST
    ["injection"; "operation"]
    ~query_string:(Query_arg.switch "async" async)
    ~data
    Fun.id

let post_private_injection_operation ?(async = false) data =
  make
    POST
    ["private"; "injection"; "operation"]
    ~query_string:(Query_arg.switch "async" async)
    ~data
    Fun.id

let post_chain_block_helpers_scripts_run_operation ?(chain = "main")
    ?(block = "head") ?version ?(async = false) data =
  let query_string =
    Query_arg.opt "version" Fun.id version @ Query_arg.switch "async" async
  in
  make
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "run_operation"]
    ~query_string
    ~data
    Fun.id

let post_chain_block_helpers_scripts_pack_data ?(chain = "main")
    ?(block = "head") ~data ~ty ?gas () =
  let body =
    `O
      (List.filter_map
         Fun.id
         [
           Some ("data", data);
           Some ("type", ty);
           Option.map (fun g -> ("gas", `String (string_of_int g))) gas;
         ])
  in
  make
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "pack_data"]
    ~data:(Data body)
    Fun.id

let get_chain_chain_id ?(chain = "main") () =
  make GET ["chains"; chain; "chain_id"] JSON.as_string

let get_chain_block ?(chain = "main") ?(block = "head") ?version
    ?(force_metadata = false) ?metadata () =
  let query_string =
    Query_arg.opt "version" Fun.id version
    @
    if force_metadata then [("force_metadata", "")]
    else
      []
      @
      match metadata with
      | Some `Never -> [("metadata", "never")]
      | Some `Always -> [("metadata", "always")]
      | None -> []
  in
  make ~query_string GET ["chains"; chain; "blocks"; block] Fun.id

type balance_update = {kind : string; category : string option}

type block_metadata = {
  protocol : string;
  next_protocol : string;
  proposer : string;
  max_operations_ttl : int;
  dal_attestation : string option;
  balance_updates : balance_update list;
}

let get_chain_block_metadata ?(chain = "main") ?(block = "head") ?version () =
  let query_string = Query_arg.opt "version" Fun.id version in
  make ~query_string GET ["chains"; chain; "blocks"; block; "metadata"]
  @@ fun json ->
  let dal_attestation = JSON.(json |-> "dal_attestation" |> as_string_opt) in
  let protocol = JSON.(json |-> "protocol" |> as_string) in
  let next_protocol = JSON.(json |-> "next_protocol" |> as_string) in
  let proposer =
    match JSON.(json |-> "proposer" |> as_string_opt) with
    | None -> (* This should be only for tests protocols *) ""
    | Some proposer -> proposer
  in
  let max_operations_ttl = JSON.(json |-> "max_operations_ttl" |> as_int) in
  let balance_updates = JSON.(json |-> "balance_updates" |> as_list) in
  let balance_updates =
    List.map
      (fun json ->
        let kind = JSON.(json |-> "kind" |> as_string) in
        let category = JSON.(json |-> "category" |> as_string_opt) in
        {kind; category})
      balance_updates
  in
  {
    dal_attestation;
    protocol;
    next_protocol;
    proposer;
    max_operations_ttl;
    balance_updates;
  }

let get_chain_block_metadata_raw ?(chain = "main") ?(block = "head") ?version ()
    =
  let query_string = Query_arg.opt "version" Fun.id version in
  make ~query_string GET ["chains"; chain; "blocks"; block; "metadata"] Fun.id

let get_chain_block_protocols ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "protocols"] Fun.id

let get_chain_block_hash ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "hash"] JSON.as_string

let get_chain_block_header ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"] Fun.id

let get_chain_block_header_shell ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"; "shell"] Fun.id

let patch_chain_bootstrapped ?(chain = "main") bootstrapped =
  make
    PATCH
    ["chains"; chain]
    ~data:(Data (`O [("bootstrapped", `Bool bootstrapped)]))
    ignore

type sync_state = Synced | Unsynced | Stuck

type is_bootstrapped = {bootstrapped : bool; sync_state : sync_state}

let get_chain_is_bootstrapped ?(chain = "main") () =
  make GET ["chains"; chain; "is_bootstrapped"] @@ fun json ->
  JSON.
    {
      sync_state =
        ( json |-> "sync_state" |> as_string |> function
          | "synced" -> Synced
          | "unsynced" -> Unsynced
          | "stuck" -> Stuck
          | state ->
              Test.fail
                "/chains/%s/is_bootstrapped returned unexpected sync_state: %s"
                chain
                state );
      bootstrapped = json |-> "bootstrapped" |> as_bool;
    }

type block_descriptor = {block_hash : string; level : int}

let parse_block_descriptor json =
  JSON.
    {
      block_hash = json |-> "block_hash" |> as_string;
      level = json |-> "level" |> as_int;
    }

let get_chain_level_checkpoint ?(chain = "main") () =
  make GET ["chains"; chain; "levels"; "checkpoint"] parse_block_descriptor

let get_chain_level_savepoint ?(chain = "main") () =
  make GET ["chains"; chain; "levels"; "savepoint"] parse_block_descriptor

let get_chain_level_caboose ?(chain = "main") () =
  make GET ["chains"; chain; "levels"; "caboose"] parse_block_descriptor

let get_worker_block_validator = make GET ["workers"; "block_validator"] Fun.id

let get_workers_chain_validators =
  make GET ["workers"; "chain_validators"] Fun.id

let get_worker_chain_validator ?(chain = "main") () =
  make GET ["workers"; "chain_validators"; chain] Fun.id

let get_worker_chain_validator_ddb ?(chain = "main") () =
  make GET ["workers"; "chain_validators"; chain; "ddb"] Fun.id

let get_worker_chain_validator_peers_validators ?(chain = "main") () =
  make GET ["workers"; "chain_validators"; chain; "peers_validators"] Fun.id

let get_workers_prevalidators = make GET ["workers"; "prevalidators"] Fun.id

let get_worker_prevalidator ?(chain = "main") () =
  make GET ["workers"; "prevalidators"; chain] Fun.id

let get_errors = make GET ["errors"] Fun.id

let get_protocol protocol_hash = make GET ["protocols"; protocol_hash] Fun.id

let get_protocols =
  make
    GET
    ["protocols"]
    JSON.(fun json -> json |> as_list |> List.map as_string)

let get_fetch_protocol protocol_hash =
  make GET ["fetch_protocol"; protocol_hash] Fun.id

let get_stats_gc = make GET ["stats"; "gc"] Fun.id

let get_stats_memory = make GET ["stats"; "memory"] Fun.id

let post_injection_block ~data = make POST ["injection"; "block"] ~data Fun.id

let get_chain_block_header_protocol_data_raw ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "header"; "protocol_data"; "raw"]
    JSON.as_string

let get_chain_block_header_protocol_data ?(chain = "main") ?(block = "head")
    ?(offset = 0) () =
  let query_string = [("offset", string_of_int offset)] in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "header"; "protocol_data"]
    Fun.id

let get_chain_block_operations ?(chain = "main") ?(block = "head") ?version
    ?(force_metadata = false) ?(metadata = true) () =
  let query_string =
    Query_arg.opt "version" Fun.id version
    @
    if force_metadata then [("force_metadata", "")]
    else [] @ if metadata then [] else [("metadata", "never")]
  in
  make ~query_string GET ["chains"; chain; "blocks"; block; "operations"] Fun.id

let get_chain_block_operations_validation_pass ?(chain = "main")
    ?(block = "head") ?version ?(force_metadata = false) ?(metadata = true)
    ?operation_offset ~validation_pass () =
  let path =
    [
      "chains";
      chain;
      "blocks";
      block;
      "operations";
      string_of_int validation_pass;
    ]
    @ match operation_offset with None -> [] | Some m -> [string_of_int m]
  in
  let query_string =
    Query_arg.opt "version" Fun.id version
    @
    if force_metadata then [("force_metadata", "")]
    else [] @ if metadata then [] else [("metadata", "never")]
  in
  make ~query_string GET path Fun.id

let get_chain_mempool_pending_operations ?(chain = "main") ?version ?validated
    ?branch_delayed ?branch_refused ?refused ?outdated ?validation_passes
    ?sources ?operation_hash () =
  let query_string =
    Query_arg.opt "version" Fun.id version
    @ Query_arg.opt_bool "validated" validated
    @ Query_arg.opt_bool "refused" refused
    @ Query_arg.opt_bool "outdated" outdated
    @ Query_arg.opt_bool "branch_delayed" branch_delayed
    @ Query_arg.opt_bool "branch_refused" branch_refused
    @ Query_arg.opt_list
        "validation_pass"
        (fun name vp -> (name, string_of_int vp))
        validation_passes
    @ Query_arg.opt_list "source" (fun name d -> (name, d)) sources
    @ Query_arg.opt_list
        "operation_hash"
        (fun name h -> (name, h))
        operation_hash
  in
  make
    ~query_string
    GET
    ["chains"; chain; "mempool"; "pending_operations"]
    Fun.id

let get_chain_mempool_monitor_operations ?(chain = "main") ?version ?validated
    ?branch_delayed ?branch_refused ?refused ?outdated ?validation_passes
    ?sources () =
  let query_string =
    Query_arg.opt "version" Fun.id version
    @ Query_arg.opt_bool "validated" validated
    @ Query_arg.opt_bool "refused" refused
    @ Query_arg.opt_bool "outdated" outdated
    @ Query_arg.opt_bool "branch_delayed" branch_delayed
    @ Query_arg.opt_bool "branch_refused" branch_refused
    @ Query_arg.opt_list
        "validation_pass"
        (fun name vp -> (name, string_of_int vp))
        validation_passes
    @ Query_arg.opt_list "sources" (fun name source -> (name, source)) sources
  in
  make
    ~query_string
    GET
    ["chains"; chain; "mempool"; "monitor_operations"]
    Fun.id

let post_chain_mempool_request_operations ?(chain = "main") ?peer () =
  make
    ~query_string:(Query_arg.opt "peer_id" Fun.id peer)
    POST
    ["chains"; chain; "mempool"; "request_operations"]
    Fun.id

let post_chain_mempool_ban_operation ?(chain = "main") ~data () =
  make ~data POST ["chains"; chain; "mempool"; "ban_operation"] Fun.id

let post_chain_mempool_unban_operation ?(chain = "main") ~data () =
  make ~data POST ["chains"; chain; "mempool"; "unban_operation"] Fun.id

let post_chain_mempool_unban_all_operations ?(chain = "main") () =
  make POST ["chains"; chain; "mempool"; "unban_all_operations"] Fun.id

let get_chain_mempool_filter ?(chain = "main") ?include_default () =
  let query_string =
    Option.map
      (fun b -> [("include_default", string_of_bool b)])
      include_default
  in
  make ?query_string GET ["chains"; chain; "mempool"; "filter"] Fun.id

let post_chain_mempool_filter ?(chain = "main") ~data () =
  make ~data POST ["chains"; chain; "mempool"; "filter"] Fun.id

let post_chain_block_helpers_preapply_block ?(chain = "main") ?(block = "head")
    ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "preapply"; "block"]
    Fun.id

let post_chain_block_helpers_preapply_operations ?(chain = "main")
    ?(block = "head") ?version ~data () =
  let query_string = Query_arg.opt "version" Fun.id version in
  make
    ~query_string
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "preapply"; "operations"]
    Fun.id

let post_chain_block_helpers_forge_operations ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "forge"; "operations"]
    Fun.id

let post_chain_block_helpers_forge_signed_operations ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "forge"; "signed_operations"]
    Fun.id

let post_chain_block_helpers_forge_bls_consensus_operations ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    [
      "chains";
      chain;
      "blocks";
      block;
      "helpers";
      "forge";
      "bls_consensus_operations";
    ]
    Fun.id

let post_chain_block_helpers_forge_block_header ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "forge_block_header"]
    Fun.id

let post_chain_block_helpers_parse_operations ?(chain = "main")
    ?(block = "head") ?(check_signature = true) ?version ops =
  let query_string = Query_arg.opt "version" Fun.id version in
  let data : RPC_core.data =
    Data (`O [("operations", ops); ("check_signature", `Bool check_signature)])
  in
  make
    ~query_string
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "parse"; "operations"]
    Fun.id

let post_chain_block_helpers_scripts_simulate_operation ?(chain = "main")
    ?(block = "head") ?version ~data () =
  let query_string = Query_arg.opt "version" Fun.id version in
  make
    ~query_string
    ~data
    POST
    [
      "chains";
      chain;
      "blocks";
      block;
      "helpers";
      "scripts";
      "simulate_operation";
    ]
    Fun.id

let post_chain_block_helpers_scripts_event_address ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "event_address"]
    Fun.id

type ctxt_type = Bytes | Json

let ctxt_type_to_string = function Bytes -> "bytes" | Json -> "json"

let get_chain_block_context_raw ?(chain = "main") ?(block = "head")
    ?(ctxt_type = Json) ?depth ~value_path () =
  make
    ~query_string:(Query_arg.opt "depth" string_of_int depth)
    GET
    ([
       "chains";
       chain;
       "blocks";
       block;
       "context";
       "raw";
       ctxt_type_to_string ctxt_type;
     ]
    @ value_path)
    Fun.id

let get_chain_block_context_raw_bytes ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "context"; "raw"; "bytes"] Fun.id

let get_chain_block_context_cache_contracts_all ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "cache"; "contracts"; "all"]
    Fun.id

let get_chain_block_context_constants ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "context"; "constants"] Fun.id

let get_chain_block_context_constants_errors ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "constants"; "errors"]
    Fun.id

let get_chain_block_context_constants_parametric ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "constants"; "parametric"]
    Fun.id

let get_chain_block_context_contract_storage_used_space ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "storage";
      "used_space";
    ]
    JSON.as_int

let get_chain_block_context_contract_storage_paid_space ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "storage";
      "paid_space";
    ]
    JSON.as_int

let get_chain_block_context_contract_staking_numerator ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "staking_numerator";
    ]
    JSON.as_int

let get_chain_block_context_contract_staked_balance ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "staked_balance";
    ]
    JSON.as_int

let get_chain_block_context_contract_unstake_requests ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "unstake_requests";
    ]
    Fun.id

let get_chain_block_context_contract_unstaked_finalizable_balance
    ?(chain = "main") ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "unstaked_finalizable_balance";
    ]
    JSON.as_int

let get_chain_block_context_contract_unstaked_frozen_balance ?(chain = "main")
    ?(block = "head") contract =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract;
      "unstaked_frozen_balance";
    ]
    JSON.as_int

let get_chain_block_helper_baking_rights ?(chain = "main") ?(block = "head")
    ?delegate ?level ?cycle ?max_round () =
  let query_string =
    Query_arg.opt "delegate" Fun.id delegate
    @ Query_arg.opt "level" Int.to_string level
    @ Query_arg.opt "cycle" string_of_int cycle
    @ Query_arg.opt "max_round" Int.to_string max_round
  in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "baking_rights"]
    Fun.id

type level = {
  level : int;
  level_position : int;
  cycle : int;
  cycle_position : int;
  expected_commitment : bool;
}

let get_chain_block_helper_current_level ?(chain = "main") ?(block = "head")
    ?(offset = 0) () =
  let query_string = [("offset", string_of_int offset)] in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "current_level"]
  @@ fun json ->
  let level = JSON.(json |-> "level" |> as_int) in
  let level_position = JSON.(json |-> "level_position" |> as_int) in
  let cycle = JSON.(json |-> "cycle" |> as_int) in
  let cycle_position = JSON.(json |-> "cycle_position" |> as_int) in
  let expected_commitment = JSON.(json |-> "expected_commitment" |> as_bool) in
  {level; level_position; cycle; cycle_position; expected_commitment}

let get_chain_block_helper_attestation_rights ?(chain = "main")
    ?(block = "head") ?level ?cycle ?delegate ?consensus_key () =
  let query_string =
    Query_arg.opt "delegate" Fun.id delegate
    @ Query_arg.opt "consensus_key" Fun.id consensus_key
    @ Query_arg.opt "level" string_of_int level
    @ Query_arg.opt "cycle" string_of_int cycle
  in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "attestation_rights"]
    Fun.id

let get_chain_block_helper_validators ?(chain = "main") ?(block = "head")
    ?delegate ?consensus_key ?level () =
  let query_string =
    Query_arg.opt "delegate" Fun.id delegate
    @ Query_arg.opt "level" Int.to_string level
    @ Query_arg.opt "consensus_key" Fun.id consensus_key
  in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "validators"]
    Fun.id

type cycle_levels = {first : int; last : int}

let get_chain_block_helper_levels_in_current_cycle ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "levels_in_current_cycle"]
  @@ fun json ->
  let first = JSON.(json |-> "first" |> as_int) in
  let last = JSON.(json |-> "last" |> as_int) in
  {first; last}

let get_chain_block_helper_total_baking_power ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "total_baking_power"]
    Fun.id

let post_bls_aggregate_signatures ~pk ~msg sigs =
  let signatures = List.map (fun signature -> `String signature) sigs in
  let pk_msg_sig =
    [
      ("public_key", `String pk);
      ("message", `String msg);
      ("signature_shares", `A signatures);
    ]
  in
  let data = `O pk_msg_sig in
  make ~data:(Data data) POST ["bls"; "aggregate_signatures"] JSON.as_string

let post_bls_check_proof ~pk ~proof () =
  let data = `O [("public_key", `String pk); ("proof", `String proof)] in
  make ~data:(Data data) POST ["bls"; "check_proof"] JSON.as_bool

let post_bls_aggregate_public_keys pks_with_proofs =
  let data =
    `A
      (List.map
         (fun (pk, proof) ->
           `O [("public_key", `String pk); ("proof", `String proof)])
         pks_with_proofs)
  in
  make ~data:(Data data) POST ["bls"; "aggregate_public_keys"] @@ fun json ->
  let group_pk = JSON.(json |-> "public_key" |> as_string) in
  let group_pkh = JSON.(json |-> "public_key_hash" |> as_string) in
  (group_pk, group_pkh)

let post_bls_aggregate_proofs ~pk proofs =
  let proofs = List.map (fun proof -> `String proof) proofs in
  let pk_with_proofs = [("public_key", `String pk); ("proofs", `A proofs)] in
  let data = `O pk_with_proofs in
  make ~data:(Data data) POST ["bls"; "aggregate_proofs"] JSON.as_string

let post_bls_threshold_signatures ~pk ~msg id_signatures =
  let id_signatures =
    List.map
      (fun (id, signature) ->
        `O [("id", `Float (float_of_int id)); ("signature", `String signature)])
      id_signatures
  in
  let pk_msg_sig =
    [
      ("public_key", `String pk);
      ("message", `String msg);
      ("signature_shares", `A id_signatures);
    ]
  in
  let data = `O pk_msg_sig in
  make ~data:(Data data) POST ["bls"; "threshold_signatures"] JSON.as_string

let get_chain_block_context_big_map ?(chain = "main") ?(block = "head") ~id
    ~key_hash () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "big_maps"; id; key_hash]
    Fun.id

let get_chain_block_context_big_maps ?(chain = "main") ?(block = "head") ~id
    ?offset ?length () =
  let query_string =
    Query_arg.opt "offset" Int.to_string offset
    @ Query_arg.opt "length" Int.to_string length
  in
  make
    GET
    ~query_string
    ["chains"; chain; "blocks"; block; "context"; "big_maps"; id]
    Fun.id

let get_chain_block_context_raw_json_big_maps_index ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "raw";
      "json";
      "big_maps";
      "index";
      id;
    ]
    Fun.id

let get_chain_block_context_contracts ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "context"; "contracts"] Fun.id

let get_chain_block_context_contract ?(chain = "main") ?(block = "head") ~id ()
    =
  make GET ["chains"; chain; "blocks"; block; "context"; "contracts"; id] Fun.id

let get_chain_block_context_contract_balance ?(chain = "main") ?(block = "head")
    ~id () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "contracts"; id; "balance"]
    Decode.mutez

let get_chain_block_context_contract_frozen_bonds ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      id;
      "frozen_bonds";
    ]
    Decode.mutez

let get_chain_block_context_contract_balance_and_frozen_bonds ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      id;
      "balance_and_frozen_bonds";
    ]
    Decode.mutez

let post_chain_block_context_contract_big_map_get ?(chain = "main")
    ?(block = "head") ~id ~data () =
  make
    ~data
    POST
    [
      "chains"; chain; "blocks"; block; "context"; "contracts"; id; "big_map_get";
    ]
    Fun.id

let get_chain_block_context_contract_counter ?(chain = "main") ?(block = "head")
    ~id () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "contracts"; id; "counter"]
    Fun.id

let get_chain_block_context_contract_delegate ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "contracts"; id; "delegate"]
    Fun.id

let get_chain_block_context_contract_entrypoints ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains"; chain; "blocks"; block; "context"; "contracts"; id; "entrypoints";
    ]
    Fun.id

let get_chain_block_context_contract_manager_key ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains"; chain; "blocks"; block; "context"; "contracts"; id; "manager_key";
    ]
    Fun.id

let get_chain_block_context_contract_script ?(chain = "main") ?(block = "head")
    ~id () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "contracts"; id; "script"]
    Fun.id

let get_chain_block_context_contract_storage ?(chain = "main") ?(block = "head")
    ~id () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "contracts"; id; "storage"]
    Fun.id

let post_chain_block_context_contract_ticket_balance ?(chain = "main")
    ?(block = "head") ~id ~data () =
  make
    POST
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      id;
      "ticket_balance";
    ]
    ~data
    JSON.as_int

let get_chain_block_context_contract_all_ticket_balances ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      id;
      "all_ticket_balances";
    ]
    Fun.id

let get_chain_block_context_smart_rollups_all ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "smart_rollups"; "all"]
    (fun rollups -> JSON.as_list rollups |> List.map JSON.as_string)

let get_chain_block_context_smart_rollups_smart_rollup_staker_games
    ?(chain = "main") ?(block = "head") ~staker sc_rollup () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "staker";
      staker;
      "games";
    ]
    Fun.id

let get_chain_block_context_smart_rollups_smart_rollup_consumed_outputs
    ?(chain = "main") ?(block = "head") ~sc_rollup ~outbox_level () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "consumed_outputs";
      string_of_int outbox_level;
    ]
  @@ fun json -> JSON.(as_list json |> List.map as_int)

type smart_rollup_inbox = {
  old_levels_messages : string;
  level : int;
  current_messages_hash : string option;
}

let smart_rollup_inbox_from_json json =
  let open JSON in
  let old_levels_messages =
    json |-> "old_levels_messages" |-> "content" |-> "hash" |> as_string
  in
  let level = json |-> "level" |> as_int in
  let current_messages_hash =
    json |-> "current_messages_hash" |> as_string_opt
  in
  {old_levels_messages; level; current_messages_hash}

let get_chain_block_context_smart_rollups_all_inbox ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "all";
      "inbox";
    ]
    smart_rollup_inbox_from_json

let get_chain_block_context_smart_rollups_smart_rollup_genesis_info
    ?(chain = "main") ?(block = "head") sc_rollup =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "genesis_info";
    ]
    Fun.id

let get_chain_block_context_smart_rollups_smart_rollup_last_cemented_commitment_hash_with_level
    ?(chain = "main") ?(block = "head") sc_rollup =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "last_cemented_commitment_hash_with_level";
    ]
    Fun.id

type smart_rollup_commitment = {
  compressed_state : string;
  inbox_level : int;
  predecessor : string;
  number_of_ticks : int;
}

let smart_rollup_commitment_from_json json =
  let compressed_state = JSON.as_string @@ JSON.get "compressed_state" json in
  let inbox_level = JSON.as_int @@ JSON.get "inbox_level" json in
  let predecessor = JSON.as_string @@ JSON.get "predecessor" json in
  let number_of_ticks = JSON.as_int @@ JSON.get "number_of_ticks" json in
  {compressed_state; inbox_level; predecessor; number_of_ticks}

let get_chain_block_context_smart_rollups_smart_rollup_commitment
    ?(chain = "main") ?(block = "head") ~sc_rollup ~hash () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "commitment";
      hash;
    ]
    (fun json -> smart_rollup_commitment_from_json json)

let get_chain_block_context_smart_rollups_smart_rollup_staker_staked_on_commitment
    ?(chain = "main") ?(block = "head") ~sc_rollup staker =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "staker";
      staker;
      "staked_on_commitment";
    ]
    Fun.id

let post_chain_block_context_smart_rollups_smart_rollup_ticket_balance
    ?(chain = "main") ?(block = "head") ~sc_rollup ~data () =
  make
    POST
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "ticket_balance";
    ]
    ~data
    JSON.as_int

let get_chain_block_context_smart_rollups_smart_rollup_whitelist
    ?(chain = "main") ?(block = "head") sc_rollup =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "smart_rollup";
      sc_rollup;
      "whitelist";
    ]
    (fun whitelist ->
      match JSON.(whitelist |> as_opt |> Option.map as_list) with
      | Some l -> Some (List.map JSON.as_string l)
      | None -> None)

let get_chain_block_context_delegates ?(chain = "main") ?(block = "head")
    ?query_string () =
  make
    GET
    ?query_string
    ["chains"; chain; "blocks"; block; "context"; "delegates"]
    (fun contracts -> JSON.as_list contracts |> List.map JSON.as_string)

let get_chain_block_context_delegate ?(chain = "main") ?(block = "head") pkh =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh]
    Fun.id

let get_chain_block_context_delegate_active_staking_parameters ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "active_staking_parameters";
    ]
    Fun.id

let get_chain_block_context_delegate_pending_staking_parameters
    ?(chain = "main") ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "pending_staking_parameters";
    ]
    Fun.id

let get_chain_block_context_delegate_current_frozen_deposits ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "current_frozen_deposits";
    ]
    Decode.mutez

let get_chain_block_context_delegate_deactivated ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "deactivated";
    ]
    Fun.id

let get_chain_block_context_delegate_delegated_balance ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "delegated_balance";
    ]
    Decode.mutez

let get_chain_block_context_delegate_delegated_contracts ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "delegated_contracts";
    ]
    Fun.id

let get_chain_block_context_delegate_stakers ?(chain = "main") ?(block = "head")
    pkh =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh; "stakers"]
    Fun.id

let get_chain_block_context_delegate_is_forbidden ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "is_forbidden";
    ]
    JSON.as_bool

let get_chain_block_context_delegate_total_delegated_stake ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "total_delegated_stake";
    ]
    Fun.id

let get_chain_block_context_delegate_staking_denominator ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "staking_denominator";
    ]
    Fun.id

let get_chain_block_context_delegate_frozen_deposits ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "frozen_deposits";
    ]
    Decode.mutez

let get_chain_block_context_delegate_frozen_deposits_limit ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "frozen_deposits_limit";
    ]
    (fun json -> json |> JSON.as_opt |> Option.map Decode.mutez)

let get_chain_block_context_delegate_full_balance ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "full_balance";
    ]
    Decode.mutez

let get_chain_block_context_delegate_grace_period ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "grace_period";
    ]
    Fun.id

let get_chain_block_context_delegate_min_delegated_in_current_cycle
    ?(chain = "main") ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "min_delegated_in_current_cycle";
    ]
    Fun.id

let get_chain_block_context_delegate_participation_raw ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "participation";
    ]
    Fun.id

type participation = {
  expected_cycle_activity : int;
  minimal_cycle_activity : int;
  missed_slots : int;
  missed_levels : int;
  remaining_allowed_missed_slots : int;
  expected_attesting_rewards : Tez.t;
}

let get_chain_block_context_delegate_participation ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "participation";
    ]
  @@ fun json ->
  let open JSON in
  let expected_cycle_activity = json |-> "expected_cycle_activity" |> as_int in
  let minimal_cycle_activity = json |-> "minimal_cycle_activity" |> as_int in
  let missed_slots = json |-> "missed_slots" |> as_int in
  let missed_levels = json |-> "missed_levels" |> as_int in
  let remaining_allowed_missed_slots =
    json |-> "remaining_allowed_missed_slots" |> as_int
  in
  let expected_attesting_rewards =
    json |-> "expected_attesting_rewards" |> as_int64 |> Tez.of_mutez_int64
  in
  {
    expected_cycle_activity : int;
    minimal_cycle_activity : int;
    missed_slots : int;
    missed_levels : int;
    remaining_allowed_missed_slots : int;
    expected_attesting_rewards : Tez.t;
  }

let get_chain_block_context_delegate_dal_participation_raw ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "dal_participation";
    ]
    Fun.id

type dal_participation = {
  expected_assigned_shards_per_slot : int;
  delegate_attested_dal_slots : int;
  delegate_attestable_dal_slots : int;
  expected_dal_rewards : Tez.t;
  sufficient_dal_participation : bool;
  denounced : bool;
}

let get_chain_block_context_delegate_dal_participation ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "dal_participation";
    ]
  @@ fun json ->
  let open JSON in
  let expected_assigned_shards_per_slot =
    json |-> "expected_assigned_shards_per_slot" |> as_int
  in
  let delegate_attested_dal_slots =
    json |-> "delegate_attested_dal_slots" |> as_int
  in
  let delegate_attestable_dal_slots =
    json |-> "delegate_attestable_dal_slots" |> as_int
  in
  let expected_dal_rewards =
    json |-> "expected_dal_rewards" |> as_int64 |> Tez.of_mutez_int64
  in
  let sufficient_dal_participation =
    json |-> "sufficient_dal_participation" |> as_bool
  in
  let denounced = json |-> "denounced" |> as_bool in
  {
    expected_assigned_shards_per_slot;
    delegate_attested_dal_slots;
    delegate_attestable_dal_slots;
    expected_dal_rewards;
    sufficient_dal_participation;
    denounced;
  }

let get_chain_block_context_delegate_frozen_balance ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "frozen_balance";
    ]
    Decode.mutez

let get_chain_block_context_delegate_frozen_balance_by_cycle ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "frozen_balance_by_cycle";
    ]
    Fun.id

let get_chain_block_context_delegate_staking_balance ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "staking_balance";
    ]
    Decode.mutez

let get_chain_block_context_delegate_balance ?(chain = "main") ?(block = "head")
    pkh =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh; "balance"]
    Decode.mutez

let get_chain_block_context_delegate_voting_info ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "voting_info";
    ]
    Fun.id

let get_chain_block_context_delegate_voting_power ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "voting_power";
    ]
    Fun.id

let get_chain_block_context_delegate_consensus_key ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "consensus_key";
    ]
    Fun.id

let get_chain_block_context_delegate_companion_key ?(chain = "main")
    ?(block = "head") pkh =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "delegates";
      pkh;
      "companion_key";
    ]
    Fun.id

let get_chain_block_context_total_supply ?(chain = "main") ?(block = "head") ()
    =
  make GET ["chains"; chain; "blocks"; block; "context"; "total_supply"] Fun.id

let get_chain_block_context_total_frozen_stake ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "total_frozen_stake"]
    Fun.id

let get_chain_block_context_total_currently_staked ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "total_currently_staked"]
    Fun.id

let get_chain_block_context_issuance_current_yearly_rate ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "issuance";
      "current_yearly_rate";
    ]
    Fun.id

let get_chain_block_context_issuance_current_yearly_rate_exact ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "issuance";
      "current_yearly_rate_exact";
    ]
    Fun.id

let get_chain_block_context_issuance_current_yearly_rate_details
    ?(chain = "main") ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "issuance";
      "current_yearly_rate_details";
    ]
    Fun.id

let get_chain_block_context_issuance_issuance_per_minute ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "issuance";
      "issuance_per_minute";
    ]
    Fun.id

let get_chain_block_context_adaptive_issuance_launch_cycle ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "adaptive_issuance_launch_cycle";
    ]
    Decode.int_option

let get_chain_block_context_issuance_expected_issuance ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "issuance";
      "expected_issuance";
    ]
    Fun.id

let get_chain_block_context_dal_commitments_history ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "dal"; "commitments_history"]
    Fun.id

let get_chain_block_context_dal_cells_of_level ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "dal";
      "skip_list_cells_of_level";
    ]
    Fun.id

let get_chain_block_context_raw_json ?(chain = "main") ?(block = "head")
    ?(path = []) () =
  make
    GET
    (["chains"; chain; "blocks"; block; "context"; "raw"; "json"] @ path)
    Fun.id

let get_chain_block_votes_ballot_list ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "votes"; "ballot_list"] Fun.id

let get_chain_block_votes_ballots ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "votes"; "ballots"] Fun.id

let get_chain_block_votes_current_period ?(chain = "main") ?(block = "head") ()
    =
  make GET ["chains"; chain; "blocks"; block; "votes"; "current_period"] Fun.id

let get_chain_block_votes_current_proposal ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "votes"; "current_proposal"]
    Fun.id

let get_chain_block_votes_current_quorum ?(chain = "main") ?(block = "head") ()
    =
  make GET ["chains"; chain; "blocks"; block; "votes"; "current_quorum"] Fun.id

let get_chain_block_votes_listings ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "votes"; "listings"] Fun.id

let get_chain_block_votes_proposals ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "votes"; "proposals"] Fun.id

let get_chain_block_votes_successor_period ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "votes"; "successor_period"]
    Fun.id

let get_chain_block_votes_total_voting_power ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    ["chains"; chain; "blocks"; block; "votes"; "total_voting_power"]
    Fun.id

let get_chain_block_context_dal_shards ?(chain = "main") ?(block = "head")
    ?level ?delegates () =
  let query_string =
    Query_arg.opt_list "delegates" (fun x y -> (x, y)) delegates
    @ Query_arg.opt "level" Int.to_string level
  in
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "dal"; "shards"]
    ~query_string
    Fun.id

let get_monitor_applied_blocks = make GET ["monitor"; "applied_blocks"] Fun.id

let get_monitor_heads_chain ?(chain = "main") () =
  make GET ["monitor"; "heads"; chain] Fun.id

let get_monitor_validated_blocks =
  make GET ["monitor"; "validated_blocks"] Fun.id

let nonexistent_path = make GET ["nonexistent"; "path"] Fun.id

let get_chain_block_context_denunciations ?(chain = "main") ?(block = "head") ()
    =
  make GET ["chains"; chain; "blocks"; block; "context"; "denunciations"] Fun.id

type baker_with_power = {
  delegate : string;
  staked : int;
  weighted_delegated : int;
}

let get_stake_distribution ?(chain = "main") ?(block = "head") ~cycle () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "raw";
      "json";
      "cycle";
      string_of_int cycle;
      "selected_stake_distribution";
    ]
  @@ fun json ->
  let bakers_with_pow = JSON.(json |> as_list) in
  List.map
    JSON.(
      fun baker_with_pow ->
        let active_stake = baker_with_pow |-> "active_stake" in
        {
          delegate = baker_with_pow |-> "baker" |> as_string;
          staked = active_stake |-> "frozen" |> as_int;
          weighted_delegated = active_stake |-> "delegated" |> as_int;
        })
    bakers_with_pow

let get_baking_power_distribution_for_current_cycle ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "helpers";
      "baking_power_distribution_for_current_cycle";
    ]
    Fun.id

let get_tz4_baker_number_ratio ?(chain = "main") ?(block = "head") ?cycle () =
  let query_string = Option.map (fun c -> [("cycle", Int.to_string c)]) cycle in
  make
    ?query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "tz4_baker_number_ratio"]
    Fun.id

let get_abaab_activation_level ?(chain = "main") ?(block = "head") () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "helpers";
      "all_bakers_attest_activation_level";
    ]
    Fun.id

let get_chain_block_context_destination_index ?(chain = "main")
    ?(block = "head") destination =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "destination";
      destination;
      "index";
    ]
  @@ JSON.as_int_opt

let get_chain_block_context_clst_contract_hash ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "clst"; "contract_hash"]
    JSON.as_string

let get_chain_block_context_clst_total_supply ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "clst"; "total_supply"]
    Fun.id

let get_chain_block_context_clst_total_amount_of_tez ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "clst"; "total_amount_of_tez"]
    Fun.id

let get_chain_block_context_clst_exchange_rate ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "clst"; "exchange_rate"]
    Fun.id

let get_chain_block_context_contract_clst_balance ?(chain = "main")
    ?(block = "head") ~id () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      id;
      "clst_balance";
    ]
    Fun.id

let get_chain_block_context_address_registry ?(chain = "main") ?(block = "head")
    () =
  make
    GET
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "raw";
      "json";
      "contracts";
      "address_registry";
      "next";
    ]
  @@ fun json -> JSON.as_string json |> int_of_string

let get_chain_block_helpers_swrr_credits ?(chain = "main") ?(block = "head") ()
    =
  make GET ["chains"; chain; "blocks"; block; "helpers"; "swrr_credits"] Fun.id

let get_chain_block_helpers_swrr_selected_bakers ?(chain = "main")
    ?(block = "head") ?cycle () =
  let query_string = Option.map (fun c -> [("cycle", Int.to_string c)]) cycle in
  make
    ?query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "swrr_selected_bakers"]
    Fun.id
