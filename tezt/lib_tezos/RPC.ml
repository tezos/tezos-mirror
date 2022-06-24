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

let post_private_injection_operations ?(force = false) ?(async = false) ~ops ()
    =
  let query_string =
    [("async", string_of_bool async); ("force", string_of_bool force)]
  in
  let data = `A (List.map (fun (`Hex op) -> `String op) ops) in
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

let get_network_peer_untrust peer_id =
  make GET ["network"; "peers"; peer_id; "untrust"] Fun.id

let get_network_peer_trust peer_id =
  make GET ["network"; "peers"; peer_id; "trust"] Fun.id

let get_network_points =
  make GET ["network"; "points"] @@ fun json ->
  JSON.(json |> as_list |> List.map @@ fun p -> (p |=> 0 |> as_string, p |=> 1))

let get_network_point point_id = make GET ["network"; "points"; point_id] Fun.id

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

let post_injection_operation ?(async = false) data =
  make
    POST
    ["injection"; "operation"]
    ~query_string:(if async then [("async", "")] else [])
    ~data
    Fun.id

let post_private_injection_operation ?(async = false) data =
  make
    POST
    ["private"; "injection"; "operation"]
    ~query_string:(if async then [("async", "")] else [])
    ~data
    Fun.id

let post_run_operation ?(chain = "main") ?(block = "head") ?(async = false) data
    =
  make
    POST
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "run_operation"]
    ~query_string:(if async then [("async", "")] else [])
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
  dal_slot_availability : bool Array.t option;
}

let get_chain_block_metadata ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "metadata"] @@ fun json ->
  let dal_slot_availability =
    match JSON.(json |-> "dal_slot_availability" |> as_string_opt) with
    | None -> None
    | Some slots ->
        let slot_availability = Z.of_string slots in
        let length = Z.numbits slot_availability in
        let array = Array.make length false in
        List.iter
          (fun i -> if Z.testbit slot_availability i then array.(i) <- true)
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
  {dal_slot_availability; protocol; next_protocol; proposer; max_operations_ttl}

let get_chain_block_hash ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "hash"] JSON.as_string

let get_chain_block_header ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"] Fun.id

let patch_chain_bootstrapped ?(chain = "main") bootstrapped =
  make
    PATCH
    ["chains"; chain]
    ~data:(`O [("bootstrapped", `Bool bootstrapped)])
    ignore

let get_chain_is_bootstrapped ?(chain = "main") () =
  make GET ["chains"; chain; "is_bootstrapped"] @@ fun json ->
  JSON.(json |-> "sync_state" |> as_string)

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

module Sc_rollup = struct
  let root_path ~chain ~block =
    ["chains"; chain; "blocks"; block; "context"; "sc_rollup"]

  let list ?(chain = "main") ?(block = "head") () =
    make GET (root_path ~chain ~block) Fun.id

  let path ~chain ~block sc_rollup_address =
    root_path ~chain ~block @ [sc_rollup_address]

  let get_inbox ?(chain = "main") ?(block = "head") sc_rollup_address =
    make GET (path ~chain ~block sc_rollup_address @ ["inbox"]) Fun.id

  let get_initial_level ?(chain = "main") ?(block = "head") sc_rollup_address =
    make GET (path ~chain ~block sc_rollup_address @ ["initial_level"]) Fun.id

  let get_boot_sector ?(chain = "main") ?(block = "head") sc_rollup_address =
    make GET (path ~chain ~block sc_rollup_address @ ["boot_sector"]) Fun.id

  let get_last_cemented_commitment_hash_with_level ?(chain = "main")
      ?(block = "head") sc_rollup_address =
    make
      GET
      (path ~chain ~block sc_rollup_address
      @ ["last_cemented_commitment_hash_with_level"])
      Fun.id
end
