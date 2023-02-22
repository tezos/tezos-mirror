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

include RPC_core
include RPC_legacy

type 'a t = (Node.t, 'a) RPC_core.t

module Query_arg = struct
  let opt name f = function None -> [] | Some x -> [(name, f x)]

  let opt_list name f = function None -> [] | Some l -> List.map (f name) l

  let opt_bool name b = opt name string_of_bool b

  let switch name b = if b then [(name, "")] else []
end

let make ?data ?query_string =
  make
    ?data
    ?query_string
    ~get_host:Node.rpc_host
    ~get_port:Node.rpc_port
    ~get_scheme:Node.rpc_scheme

module Decode = struct
  let mutez json = json |> JSON.as_int |> Tez.of_mutez_int
end

let get_config = make GET ["config"] Fun.id

let get_network_connections =
  make GET ["network"; "connections"] @@ fun json ->
  let decode_connection json =
    let id_point = JSON.(json |-> "id_point") in
    ( JSON.(id_point |-> "addr" |> as_string),
      JSON.(id_point |-> "port" |> as_int) )
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

let get_network_greylist_clear =
  make GET ["network"; "greylist"; "clear"] Fun.id

let get_network_peers =
  make GET ["network"; "peers"] @@ fun json ->
  JSON.(json |> as_list |> List.map @@ fun p -> (p |=> 0 |> as_string, p |=> 1))

let get_network_peer peer_id = make GET ["network"; "peers"; peer_id] Fun.id

let get_network_peer_ban peer_id =
  make GET ["network"; "peers"; peer_id; "ban"] Fun.id

let get_network_peer_banned peer_id =
  make GET ["network"; "peers"; peer_id; "banned"] Fun.id

let get_network_peer_unban peer_id =
  make GET ["network"; "peers"; peer_id; "unban"] Fun.id

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

let get_network_peer_untrust peer_id =
  make GET ["network"; "peers"; peer_id; "untrust"] Fun.id

let get_network_peer_trust peer_id =
  make GET ["network"; "peers"; peer_id; "trust"] Fun.id

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

let get_network_point_ban point_id =
  make GET ["network"; "points"; point_id; "ban"] Fun.id

let get_network_point_banned point_id =
  make GET ["network"; "points"; point_id; "banned"] Fun.id

let get_network_point_unban point_id =
  make GET ["network"; "points"; point_id; "unban"] Fun.id

let get_network_point_untrust point_id =
  make GET ["network"; "points"; point_id; "untrust"] Fun.id

let get_network_point_trust point_id =
  make GET ["network"; "points"; point_id; "trust"] Fun.id

let get_network_stat = make GET ["network"; "stat"] Fun.id

let get_network_version = make GET ["network"; "version"] Fun.id

let get_network_versions = make GET ["network"; "versions"] Fun.id

let put_network_points point =
  make PUT ["network"; "points"; point] ~data:(Data (`O [])) Fun.id

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
    ?(block = "head") ?(async = false) data =
  make
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "run_operation"]
    ~query_string:(Query_arg.switch "async" async)
    ~data
    Fun.id

let get_chain_chain_id ?(chain = "main") () =
  make GET ["chains"; chain; "chain_id"] JSON.as_string

let get_chain_block ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block] Fun.id

type block_metadata = {
  protocol : string;
  next_protocol : string;
  proposer : string;
  max_operations_ttl : int;
  dal_attestation : bool Array.t option;
}

let get_chain_block_metadata ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "metadata"] @@ fun json ->
  let dal_attestation =
    match JSON.(json |-> "dal_attestation" |> as_string_opt) with
    | None -> None
    | Some slots ->
        let attestation = Z.of_string slots in
        let length = Z.numbits attestation in
        let array = Array.make length false in
        List.iter
          (fun i -> if Z.testbit attestation i then array.(i) <- true)
          (range 0 (length - 1)) ;
        Some array
  in
  let protocol = JSON.(json |-> "protocol" |> as_string) in
  let next_protocol = JSON.(json |-> "next_protocol" |> as_string) in
  let proposer =
    match JSON.(json |-> "proposer" |> as_string_opt) with
    | None -> (* This should be only for tests protocols *) ""
    | Some proposer -> proposer
  in
  let max_operations_ttl = JSON.(json |-> "max_operations_ttl" |> as_int) in
  {dal_attestation; protocol; next_protocol; proposer; max_operations_ttl}

let get_chain_block_hash ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "hash"] JSON.as_string

let get_chain_block_header ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"] Fun.id

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
        (json |-> "sync_state" |> as_string |> function
         | "synced" -> Synced
         | "unsynced" -> Unsynced
         | "stuck" -> Stuck
         | state ->
             Test.fail
               "/chains/%s/is_bootstrapped returned unexpected sync_state: %s"
               chain
               state);
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

let get_chain_block_operations ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "operations"] Fun.id

let get_chain_block_operations_validation_pass ?(chain = "main")
    ?(block = "head") ?(force_metadata = false) ?operation_offset
    ~validation_pass () =
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
  let query_string = if force_metadata then [("force_metadata", "")] else [] in
  make ~query_string GET path Fun.id

let get_chain_mempool_pending_operations ?(chain = "main") ?version ?applied
    ?branch_delayed ?branch_refused ?refused ?outdated ?validation_passes () =
  let query_string =
    Query_arg.opt "version" Fun.id version
    @ Query_arg.opt_bool "applied" applied
    @ Query_arg.opt_bool "refused" refused
    @ Query_arg.opt_bool "outdated" outdated
    @ Query_arg.opt_bool "branch_delayed" branch_delayed
    @ Query_arg.opt_bool "branch_refused" branch_refused
    @ Query_arg.opt_list
        "validation_pass"
        (fun name vp -> (name, string_of_int vp))
        validation_passes
  in
  make
    ~query_string
    GET
    ["chains"; chain; "mempool"; "pending_operations"]
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

let post_chain_block_helpers_forge_operations ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "forge"; "operations"]
    Fun.id

let post_chain_block_helpers_forge_block_header ?(chain = "main")
    ?(block = "head") ~data () =
  make
    ~data
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "forge_block_header"]
    Fun.id

let post_chain_block_helpers_scripts_simulate_operation ?(chain = "main")
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

let get_chain_block_helper_baking_rights ?(chain = "main") ?(block = "head")
    ?delegate ?level () =
  let query_string =
    Query_arg.opt "delegate" Fun.id delegate
    @ Query_arg.opt "level" Int.to_string level
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

let get_chain_block_helper_endorsing_rights ?(chain = "main") ?(block = "head")
    ?delegate () =
  let query_string = Query_arg.opt "delegate" Fun.id delegate in
  make
    ~query_string
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "endorsing_rights"]
    Fun.id

let get_chain_block_helper_levels_in_current_cycle ?(chain = "main")
    ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "helpers"; "levels_in_current_cycle"]
    Fun.id

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
    Fun.id

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
    Fun.id

let post_chain_block_context_smart_rollups_all_origination_proof
    ?(chain = "main") ?(block = "head") ~kind ~boot_sector () =
  let data : RPC_core.data =
    Data (`O [("kind", `String kind); ("kernel", `String boot_sector)])
  in
  make
    ~data
    POST
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "smart_rollups";
      "all";
      "origination_proof";
    ]
    Fun.id

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
    Fun.id

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

let get_chain_block_context_delegates ?(chain = "main") ?(block = "head") () =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "delegates"]
    (fun contracts -> JSON.as_list contracts |> List.map JSON.as_string)

let get_chain_block_context_delegate ?(chain = "main") ?(block = "head") pkh =
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh]
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
    Fun.id

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

let get_chain_block_context_dal_confirmed_slot_headers_history ?(chain = "main")
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
      "confirmed_slot_headers_history";
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
    ?level () =
  let query_string =
    match level with
    | None -> []
    | Some offset -> [("level", string_of_int offset)]
  in
  make
    GET
    ["chains"; chain; "blocks"; block; "context"; "dal"; "shards"]
    ~query_string
    Fun.id

let make = RPC_core.make
