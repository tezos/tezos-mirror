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

let get_chain_chain_id ?(chain = "main") () =
  make GET ["chains"; chain; "chain_id"] JSON.as_string

let get_chain_block ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block] Fun.id

let get_chain_block_metadata ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "metadata"] Fun.id

let get_chain_block_hash ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "hash"] JSON.as_string

let get_chain_block_header ?(chain = "main") ?(block = "head") () =
  make GET ["chains"; chain; "blocks"; block; "header"] Fun.id

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
