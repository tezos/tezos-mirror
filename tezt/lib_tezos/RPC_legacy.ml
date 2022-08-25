(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

let mempool_ban_operation ?endpoint ?(chain = "main") ~data client =
  let path = ["chains"; chain; "mempool"; "ban_operation"] in
  Client.rpc ?endpoint ~data POST path client

let mempool_unban_operation ?endpoint ?(chain = "main") ~data client =
  let path = ["chains"; chain; "mempool"; "unban_operation"] in
  Client.rpc ?endpoint ~data POST path client

let mempool_unban_all_operations ?endpoint ?(chain = "main") client =
  let path = ["chains"; chain; "mempool"; "unban_all_operations"] in
  Client.rpc ?endpoint POST path client

let get_mempool_filter ?endpoint ?hooks ?(chain = "main") ?include_default
    client =
  let path = ["chains"; chain; "mempool"; "filter"] in
  let query_string =
    Option.map
      (fun b -> [("include_default", string_of_bool b)])
      include_default
  in
  Client.rpc ?endpoint ?hooks ?query_string GET path client

let post_mempool_filter ?endpoint ?hooks ?(chain = "main") ~data client =
  let path = ["chains"; chain; "mempool"; "filter"] in
  Client.rpc ?endpoint ?hooks ~data POST path client

let preapply_block ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~data
    client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "preapply"; "block"]
  in
  Client.rpc ?endpoint ?hooks ~data POST path client

let get_constants ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
  let path = ["chains"; chain; "blocks"; block; "context"; "constants"] in
  Client.rpc ?endpoint ?hooks GET path client

let get_constants_errors ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    client =
  let path =
    ["chains"; chain; "blocks"; block; "context"; "constants"; "errors"]
  in
  Client.rpc ?endpoint ?hooks GET path client

type ctxt_type = Bytes | Json

let ctxt_type_to_string = function Bytes -> "bytes" | Json -> "json"

let get_context_value ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ?(ctxt_type = Json) ~value_path client =
  let path =
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "raw";
      ctxt_type_to_string ctxt_type;
    ]
    @ value_path
  in
  Client.rpc ?endpoint ?hooks GET path client

let get_baking_rights ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ?delegate client =
  let path = ["chains"; chain; "blocks"; block; "helpers"; "baking_rights"] in
  let query_string = Option.map (fun d -> [("delegate", d)]) delegate in
  Client.rpc ?endpoint ?hooks ?query_string GET path client

let get_current_level ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ?(offset = 0) client =
  let path = ["chains"; chain; "blocks"; block; "helpers"; "current_level"] in
  let query_string = [("offset", string_of_int offset)] in
  Client.rpc ?endpoint ?hooks ~query_string GET path client

let get_endorsing_rights ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ?delegate client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "endorsing_rights"]
  in
  let query_string = Option.map (fun d -> [("delegate", d)]) delegate in
  Client.rpc ?endpoint ?hooks ?query_string GET path client

let get_levels_in_current_cycle ?endpoint ?hooks ?(chain = "main")
    ?(block = "head") client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "levels_in_current_cycle"]
  in
  Client.rpc ?endpoint ?hooks GET path client

let post_forge_operations ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ~data client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "forge"; "operations"]
  in
  Client.rpc ?endpoint ?hooks ~data POST path client

let post_simulate_operation ?endpoint ?hooks ?(chain = "main") ?(block = "head")
    ~data client =
  let path =
    [
      "chains";
      chain;
      "blocks";
      block;
      "helpers";
      "scripts";
      "simulate_operation";
    ]
  in
  Client.rpc ?endpoint ?hooks ~data POST path client

let post_compute_event_address ?endpoint ?hooks ?(chain = "main")
    ?(block = "head") ~data client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "scripts"; "event_address"]
  in
  Client.rpc ?endpoint ?hooks ~data POST path client

module Big_maps = struct
  let get ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~id ~key_hash
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "big_maps"; id; key_hash]
    in
    Client.rpc ?endpoint ?hooks GET path client

  let get_all ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~big_map_id
      ?offset ?length client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "big_maps"; big_map_id]
    in
    let query_string =
      [
        Option.map (fun offset -> ("offset", Int.to_string offset)) offset;
        Option.map (fun length -> ("length", Int.to_string length)) length;
      ]
      |> List.filter_map Fun.id
    in
    Client.Spawn.rpc ?endpoint ?hooks ~query_string GET path client
end

module Contracts = struct
  let get_all ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "contracts"] in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let get ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~contract_id
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "contracts"; contract_id]
    in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let sub_path ~chain ~block ~contract_id field =
    [
      "chains";
      chain;
      "blocks";
      block;
      "context";
      "contracts";
      contract_id;
      field;
    ]

  let get_sub ?endpoint ?hooks ~chain ~block ~contract_id field client =
    let path = sub_path ~chain ~block ~contract_id field in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let get_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "balance" client

  let get_frozen_bonds ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "frozen_bonds" client

  let get_balance_and_frozen_bonds ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~contract_id client =
    get_sub
      ?endpoint
      ?hooks
      ~chain
      ~block
      ~contract_id
      "balance_and_frozen_bonds"
      client

  let big_map_get ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id ~data client =
    let path = sub_path ~chain ~block ~contract_id "big_map_get" in
    Client.Spawn.rpc ?endpoint ?hooks ~data POST path client

  let get_counter ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "counter" client

  let get_delegate ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "delegate" client

  let get_entrypoints ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "entrypoints" client

  let get_manager_key ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "manager_key" client

  let get_script ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "script" client

  let get_storage ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~contract_id client =
    get_sub ?endpoint ?hooks ~chain ~block ~contract_id "storage" client
end

module Delegates = struct
  let spawn_get_all ?endpoint ?hooks ?(chain = "main") ?(block = "head") client
      =
    let path = ["chains"; chain; "blocks"; block; "context"; "delegates"] in
    Client.spawn_rpc ?endpoint ?hooks GET path client

  let get_all ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "delegates"] in
    let* contracts = Client.rpc ?endpoint ?hooks GET path client in
    return (JSON.as_list contracts |> List.map JSON.as_string)

  let spawn_get ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh client
      =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh]
    in
    Client.spawn_rpc ?endpoint ?hooks GET path client

  let get ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh]
    in
    Client.rpc ?endpoint ?hooks GET path client

  let sub_path ~chain ~block ~pkh field =
    ["chains"; chain; "blocks"; block; "context"; "delegates"; pkh; field]

  let spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh field client =
    let path = sub_path ~chain ~block ~pkh field in
    Client.spawn_rpc ?endpoint ?hooks GET path client

  let get_sub ?endpoint ?hooks ~chain ~block ~pkh field client =
    let path = sub_path ~chain ~block ~pkh field in
    Client.rpc ?endpoint ?hooks GET path client

  let spawn_get_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "balance" client

  let get_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh
      client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "balance" client

  let spawn_get_full_balance ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "full_balance" client

  let get_full_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh
      client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "full_balance" client

  let spawn_get_frozen_deposits ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "frozen_deposits" client

  let get_frozen_deposits ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "frozen_deposits" client

  let spawn_get_deactivated ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "deactivated" client

  let get_deactivated ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh
      client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "deactivated" client

  let spawn_get_delegated_balance ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "delegated_balance" client

  let get_delegated_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "delegated_balance" client

  let spawn_get_delegated_contracts ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub
      ?endpoint
      ?hooks
      ~chain
      ~block
      ~pkh
      "delegated_contracts"
      client

  let get_delegated_contracts ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "delegated_contracts" client

  let spawn_get_frozen_balance ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "frozen_balance" client

  let get_frozen_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "frozen_balance" client

  let spawn_get_frozen_balance_by_cycle ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub
      ?endpoint
      ?hooks
      ~chain
      ~block
      ~pkh
      "frozen_balance_by_cycle"
      client

  let get_frozen_balance_by_cycle ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "frozen_balance_by_cycle" client

  let spawn_get_grace_period ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "grace_period" client

  let get_grace_period ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh
      client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "grace_period" client

  let spawn_get_staking_balance ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "staking_balance" client

  let get_staking_balance ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~pkh client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "staking_balance" client

  let spawn_get_voting_power ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~pkh client =
    spawn_get_sub ?endpoint ?hooks ~chain ~block ~pkh "voting_power" client

  let get_voting_power ?endpoint ?hooks ?(chain = "main") ?(block = "head") ~pkh
      client =
    get_sub ?endpoint ?hooks ~chain ~block ~pkh "voting_power" client
end

module Seed = struct
  let get_seed ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "seed"] in
    let* json = Client.rpc ?endpoint ?hooks POST path client in
    return (JSON.as_string json)

  let get_seed_status ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "seed_computation"]
    in
    Client.rpc ?endpoint ?hooks GET path client
end

module Votes = struct
  let sub_path ~chain ~block sub =
    ["chains"; chain; "blocks"; block; "votes"; sub]

  let get_ballot_list ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path = sub_path ~chain ~block "ballot_list" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_ballots ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = sub_path ~chain ~block "ballots" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_current_proposal ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path = sub_path ~chain ~block "current_proposal" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_current_quorum ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path = sub_path ~chain ~block "current_quorum" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_listings ?endpoint ?hooks ?(chain = "main") ?(block = "head") client =
    let path = sub_path ~chain ~block "listings" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_proposals ?endpoint ?hooks ?(chain = "main") ?(block = "head") client
      =
    let path = sub_path ~chain ~block "proposals" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_current_period ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path = sub_path ~chain ~block "current_period" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_successor_period ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path = sub_path ~chain ~block "successor_period" in
    Client.rpc ?endpoint ?hooks GET path client

  let get_total_voting_power ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") client =
    let path = sub_path ~chain ~block "total_voting_power" in
    Client.rpc ?endpoint ?hooks GET path client
end

module Script_cache = struct
  let get_cached_contracts ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "cache"; "contracts"; "all"]
    in
    Client.rpc ?endpoint ?hooks GET path client
end

module Tx_rollup = struct
  let sub_path ?(chain = "main") ?(block = "head") ~rollup sub =
    ["chains"; chain; "blocks"; block; "context"; "tx_rollup"; rollup] @ sub

  let get_state ?endpoint ?hooks ?chain ?block ~rollup client =
    let path = sub_path ?chain ?block ~rollup ["state"] in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let get_inbox ?endpoint ?hooks ?chain ?block ~rollup ~level client =
    let path =
      sub_path ?chain ?block ~rollup ["inbox"; Format.sprintf "%d" level]
    in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let get_commitment ?endpoint ?hooks ?(chain = "main") ?(block = "head")
      ~rollup ~level client =
    let path =
      sub_path ~chain ~block ~rollup ["commitment"; Format.sprintf "%d" level]
    in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  let get_pending_bonded_commitments ?endpoint ?hooks ?(chain = "main")
      ?(block = "head") ~rollup ~pkh client =
    let path =
      sub_path ~chain ~block ~rollup ["pending_bonded_commitments"; pkh]
    in
    Client.Spawn.rpc ?endpoint ?hooks GET path client

  module Forge = struct
    module Inbox = struct
      let message_hash ?endpoint ?hooks ?(chain = "main") ?(block = "head")
          ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "inbox";
            "message_hash";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client

      let merkle_tree_hash ?endpoint ?hooks ?(chain = "main") ?(block = "head")
          ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "inbox";
            "merkle_tree_hash";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client

      let merkle_tree_path ?endpoint ?hooks ?(chain = "main") ?(block = "head")
          ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "inbox";
            "merkle_tree_path";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client
    end

    module Commitment = struct
      let merkle_tree_hash ?endpoint ?hooks ?(chain = "main") ?(block = "head")
          ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "commitment";
            "merkle_tree_hash";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client

      let merkle_tree_path ?endpoint ?hooks ?(chain = "main") ?(block = "head")
          ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "commitment";
            "merkle_tree_path";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client

      let message_result_hash ?endpoint ?hooks ?(chain = "main")
          ?(block = "head") ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "commitment";
            "message_result_hash";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client
    end

    module Withdraw = struct
      let withdraw_list_hash ?endpoint ?hooks ?(chain = "main")
          ?(block = "head") ~data client =
        let path =
          [
            "chains";
            chain;
            "blocks";
            block;
            "helpers";
            "forge";
            "tx_rollup";
            "withdraw";
            "withdraw_list_hash";
          ]
        in
        Client.Spawn.rpc ?endpoint ?hooks ~data POST path client
    end
  end
end

let raw_bytes ?endpoint ?hooks ?(chain = "main") ?(block = "head") ?(path = [])
    client =
  let path =
    ["chains"; chain; "blocks"; block; "context"; "raw"; "bytes"] @ path
  in
  Client.rpc ?endpoint ?hooks GET path client

module Curl = struct
  let curl_path_cache = ref None

  let get () =
    Process.(
      try
        let* curl_path =
          match !curl_path_cache with
          | Some curl_path -> return curl_path
          | None ->
              let* curl_path =
                run_and_read_stdout "sh" ["-c"; "command -v curl"]
              in
              let curl_path = String.trim curl_path in
              curl_path_cache := Some curl_path ;
              return curl_path
        in
        return
        @@ Some
             (fun ~url ->
               let* output = run_and_read_stdout curl_path ["-s"; url] in
               return (JSON.parse ~origin:url output))
      with _ -> return @@ None)

  let post () =
    Process.(
      try
        let* curl_path =
          match !curl_path_cache with
          | Some curl_path -> return curl_path
          | None ->
              let* curl_path =
                run_and_read_stdout "sh" ["-c"; "command -v curl"]
              in
              let curl_path = String.trim curl_path in
              curl_path_cache := Some curl_path ;
              return curl_path
        in
        return
        @@ Some
             (fun ~url data ->
               let* output =
                 run_and_read_stdout
                   curl_path
                   [
                     "-X";
                     "POST";
                     "-H";
                     "Content-Type: application/json";
                     "-s";
                     url;
                     "-d";
                     JSON.encode data;
                   ]
               in
               return (JSON.parse ~origin:url output))
      with _ -> return @@ None)
end
