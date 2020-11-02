(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let get_connections ?node ?hooks ?peer_id client =
  match peer_id with
  | None ->
      let path = ["network"; "connections"] in
      Client.rpc ?node ?hooks GET path client
  | Some peer_id ->
      let path = ["network"; "connections"; peer_id] in
      Client.rpc ?node ?hooks GET path client

let get_chain_id ?node ?hooks ?(chain = "main") client =
  let path = ["chains"; chain; "chain_id"] in
  Client.rpc ?node ?hooks GET path client

let force_bootstrapped ?node ?hooks ?(chain = "main") ?(bootstrapped = true)
    client =
  let path = ["chains"; chain] in
  let data = `O [("bootstrapped", `Bool bootstrapped)] in
  Client.rpc ?node ?hooks ~data PATCH path client

let get_checkpoint ?node ?hooks ?(chain = "main") client =
  let path = ["chains"; chain; "checkpoint"] in
  Client.rpc ?node ?hooks GET path client

let get_baking_rights ?node ?hooks ?(chain = "main") ?(block = "head")
    ~delegate client =
  let path = ["chains"; chain; "blocks"; block; "helpers"; "baking_rights"] in
  let query_string = [("delegate", delegate)] in
  Client.rpc ?node ?hooks ~query_string GET path client

let get_current_level ?node ?hooks ?(chain = "main") ?(block = "head")
    ?(offset = 0) client =
  let path = ["chains"; chain; "blocks"; block; "helpers"; "current_level"] in
  let query_string = [("offset", string_of_int offset)] in
  Client.rpc ?node ?hooks ~query_string GET path client

let get_protocol_data ?node ?hooks ?(chain = "main") ?(block = "head")
    ?(offset = 0) client =
  let path = ["chains"; chain; "blocks"; block; "header"; "protocol_data"] in
  let query_string = [("offset", string_of_int offset)] in
  Client.rpc ?node ?hooks GET path ~query_string client

let get_levels_in_curent_cycle ?node ?hooks ?(chain = "main") ?(block = "head")
    client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "levels_in_current_cycle"]
  in
  Client.rpc ?node ?hooks GET path client

let get_operations ?node ?hooks ?(chain = "main") ?(block = "head") client =
  let path = ["chains"; chain; "blocks"; block; "operations"] in
  Client.rpc ?node ?hooks GET path client

let get_mempool_pending_operations ?node ?hooks ?(chain = "main") client =
  let path = ["chains"; chain; "mempool"; "pending_operations"] in
  Client.rpc ?node ?hooks GET path client

let preapply_block ?node ?hooks ?(chain = "main") ?(block = "head") ~data
    client =
  let path =
    ["chains"; chain; "blocks"; block; "helpers"; "preapply"; "block"]
  in
  Client.rpc ?node ?hooks ~data POST path client

let inject_block ?node ?hooks ~data client =
  let path = ["injection"; "block"] in
  Client.rpc ?node ?hooks ~data POST path client

module Proto_alpha = struct
  let get_constants ?node ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "constants"] in
    Client.rpc ?node ?hooks GET path client

  let get_constants_errors ?node ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "constants"; "errors"]
    in
    Client.rpc ?node ?hooks GET path client

  module Contract = struct
    let spawn_get_all ?node ?hooks ?(chain = "main") ?(block = "head") client =
      let path = ["chains"; chain; "blocks"; block; "context"; "contracts"] in
      Client.spawn_rpc ?node ?hooks GET path client

    let get_all ?node ?hooks ?(chain = "main") ?(block = "head") client =
      let path = ["chains"; chain; "blocks"; block; "context"; "contracts"] in
      let* contracts = Client.rpc ?node ?hooks GET path client in
      return (JSON.as_list contracts |> List.map JSON.as_string)

    let spawn_get ?node ?hooks ?(chain = "main") ?(block = "head") ~contract_id
        client =
      let path =
        ["chains"; chain; "blocks"; block; "context"; "contracts"; contract_id]
      in
      Client.spawn_rpc ?node ?hooks GET path client

    let get ?node ?hooks ?(chain = "main") ?(block = "head") ~contract_id
        client =
      let path =
        ["chains"; chain; "blocks"; block; "context"; "contracts"; contract_id]
      in
      Client.rpc ?node ?hooks GET path client

    let sub_path ~chain ~block ~contract_id field =
      [ "chains";
        chain;
        "blocks";
        block;
        "context";
        "contracts";
        contract_id;
        field ]

    let spawn_get_sub ?node ?hooks ~chain ~block ~contract_id field client =
      let path = sub_path ~chain ~block ~contract_id field in
      Client.spawn_rpc ?node ?hooks GET path client

    let get_sub ?node ?hooks ~chain ~block ~contract_id field client =
      let path = sub_path ~chain ~block ~contract_id field in
      Client.rpc ?node ?hooks GET path client

    let spawn_get_balance ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "balance" client

    let get_balance ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "balance" client

    let spawn_big_map_get ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id ~data client =
      let path = sub_path ~chain ~block ~contract_id "big_map_get" in
      Client.spawn_rpc ?node ?hooks ~data POST path client

    let big_map_get ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id ~data client =
      let path = sub_path ~chain ~block ~contract_id "big_map_get" in
      Client.rpc ?node ?hooks ~data POST path client

    let spawn_get_counter ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "counter" client

    let get_counter ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "counter" client

    let spawn_get_delegate ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "delegate" client

    let get_delegate ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "delegate" client

    let spawn_get_entrypoints ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub
        ?node
        ?hooks
        ~chain
        ~block
        ~contract_id
        "entrypoints"
        client

    let get_entrypoints ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "entrypoints" client

    let spawn_get_manager_key ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub
        ?node
        ?hooks
        ~chain
        ~block
        ~contract_id
        "manager_key"
        client

    let get_manager_key ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "manager_key" client

    let spawn_get_script ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "script" client

    let get_script ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "script" client

    let spawn_get_storage ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "storage" client

    let get_storage ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "storage" client
  end
end

module Proto_007 = struct
  let get_constants ?node ?hooks ?(chain = "main") ?(block = "head") client =
    let path = ["chains"; chain; "blocks"; block; "context"; "constants"] in
    Client.rpc ?node ?hooks GET path client

  let get_constants_errors ?node ?hooks ?(chain = "main") ?(block = "head")
      client =
    let path =
      ["chains"; chain; "blocks"; block; "context"; "constants"; "errors"]
    in
    Client.rpc ?node ?hooks GET path client

  module Contract = struct
    let spawn_get_all ?node ?hooks ?(chain = "main") ?(block = "head") client =
      let path = ["chains"; chain; "blocks"; block; "context"; "contracts"] in
      Client.spawn_rpc ?node ?hooks GET path client

    let get_all ?node ?hooks ?(chain = "main") ?(block = "head") client =
      let path = ["chains"; chain; "blocks"; block; "context"; "contracts"] in
      let* contracts = Client.rpc ?node ?hooks GET path client in
      return (JSON.as_list contracts |> List.map JSON.as_string)

    let spawn_get ?node ?hooks ?(chain = "main") ?(block = "head") ~contract_id
        client =
      let path =
        ["chains"; chain; "blocks"; block; "context"; "contracts"; contract_id]
      in
      Client.spawn_rpc ?node ?hooks GET path client

    let get ?node ?hooks ?(chain = "main") ?(block = "head") ~contract_id
        client =
      let path =
        ["chains"; chain; "blocks"; block; "context"; "contracts"; contract_id]
      in
      Client.rpc ?node ?hooks GET path client

    let sub_path ~chain ~block ~contract_id field =
      [ "chains";
        chain;
        "blocks";
        block;
        "context";
        "contracts";
        contract_id;
        field ]

    let spawn_get_sub ?node ?hooks ~chain ~block ~contract_id field client =
      let path = sub_path ~chain ~block ~contract_id field in
      Client.spawn_rpc ?node ?hooks GET path client

    let get_sub ?node ?hooks ~chain ~block ~contract_id field client =
      let path = sub_path ~chain ~block ~contract_id field in
      Client.rpc ?node ?hooks GET path client

    let spawn_get_balance ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "balance" client

    let get_balance ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "balance" client

    let spawn_big_map_get ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id ~data client =
      let path = sub_path ~chain ~block ~contract_id "big_map_get" in
      Client.spawn_rpc ?node ?hooks ~data POST path client

    let big_map_get ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id ~data client =
      let path = sub_path ~chain ~block ~contract_id "big_map_get" in
      Client.rpc ?node ?hooks ~data POST path client

    let spawn_get_counter ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "counter" client

    let get_counter ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "counter" client

    let spawn_get_delegate ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "delegate" client

    let get_delegate ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "delegate" client

    let spawn_get_entrypoints ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub
        ?node
        ?hooks
        ~chain
        ~block
        ~contract_id
        "entrypoints"
        client

    let get_entrypoints ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "entrypoints" client

    let spawn_get_manager_key ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub
        ?node
        ?hooks
        ~chain
        ~block
        ~contract_id
        "manager_key"
        client

    let get_manager_key ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "manager_key" client

    let spawn_get_script ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "script" client

    let get_script ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "script" client

    let spawn_get_storage ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      spawn_get_sub ?node ?hooks ~chain ~block ~contract_id "storage" client

    let get_storage ?node ?hooks ?(chain = "main") ?(block = "head")
        ~contract_id client =
      get_sub ?node ?hooks ~chain ~block ~contract_id "storage" client
  end
end
