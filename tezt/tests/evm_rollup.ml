(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  make -f kernels.mk build
                 npm install eth-cli
   Invocation:   dune exec tezt/tests/main.exe -- --file evm_rollup.ml
*)
open Sc_rollup_helpers

let pvm_kind = "wasm_2_0_0"

let kernel_inputs_path = "tezt/tests/evm_kernel_inputs"

let exchanger_path () =
  Base.(project_root // "src/kernel_evm/l1_bridge/exchanger.tz")

let bridge_path () =
  Base.(project_root // "src/kernel_evm/l1_bridge/evm_bridge.tz")

let admin_path () = Base.(project_root // "src/kernel_evm/l1_bridge/admin.tz")

type l1_contracts = {exchanger : string; bridge : string; admin : string}

type full_evm_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_client : Sc_rollup_client.t;
  sc_rollup_address : string;
  originator_key : string;
  rollup_operator_key : string;
  evm_proxy_server : Evm_proxy_server.t;
  endpoint : string;
  l1_contracts : l1_contracts option;
}

let hex_256_of n = Printf.sprintf "%064x" n

let hex_256_of_address acc =
  let s = acc.Eth_account.address in
  (* strip 0x and convert to lowercase *)
  let n = String.length s in
  let s = String.lowercase_ascii @@ String.sub s 2 (n - 2) in
  (* prepend 24 leading zeros *)
  String.("0x" ^ make 24 '0' ^ s)

let evm_proxy_server_version proxy_server =
  let endpoint = Evm_proxy_server.endpoint proxy_server in
  let get_version_url = endpoint ^ "/version" in
  Curl.get get_version_url

let get_transaction_status ~endpoint ~tx =
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx in
  match receipt with
  | None ->
      failwith "no transaction receipt, probably it hasn't been mined yet."
  | Some r -> return r.status

let check_tx_succeeded ~endpoint ~tx =
  let* status = get_transaction_status ~endpoint ~tx in
  Check.(is_true status) ~error_msg:"Expected transaction to succeed." ;
  unit

let check_tx_failed ~endpoint ~tx =
  let* status = get_transaction_status ~endpoint ~tx in
  Check.(is_false status) ~error_msg:"Expected transaction to fail." ;
  unit

let check_status_n_logs ~endpoint ~status ~logs ~tx =
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx in
  match receipt with
  | None ->
      failwith "no transaction receipt, probably it hasn't been mined yet."
  | Some r ->
      Check.(
        (r.status = status)
          bool
          ~__LOC__
          ~error_msg:"Unexpected transaction status, expected: %R but got: %L") ;
      let received_logs = List.map Transaction.extract_log_body r.logs in
      Check.(
        (received_logs = logs)
          (list (tuple3 string (list string) string))
          ~__LOC__
          ~error_msg:"Unexpected transaction logs, expected:\n%R but got:\n%L") ;
      unit

(** [get_value_in_storage client addr nth] fetch the [nth] value in the storage
    of account [addr]  *)
let get_value_in_storage sc_rollup_client address nth =
  Sc_rollup_client.inspect_durable_state_value
    ~hooks
    sc_rollup_client
    ~pvm_kind
    ~operation:Sc_rollup_client.Value
    ~key:(Durable_storage_path.storage address ~key:(hex_256_of nth) ())

let check_str_in_storage ~evm_setup ~address ~nth ~expected =
  let*! value = get_value_in_storage evm_setup.sc_rollup_client address nth in
  Check.((value = Some expected) (option string))
    ~error_msg:"Unexpected value in storage, should be %R, but got %L" ;
  unit

let check_nb_in_storage ~evm_setup ~address ~nth ~expected =
  check_str_in_storage ~evm_setup ~address ~nth ~expected:(hex_256_of expected)

let get_storage_size sc_rollup_client ~address =
  let*! storage =
    Sc_rollup_client.inspect_durable_state_value
      ~hooks
      sc_rollup_client
      ~pvm_kind
      ~operation:Sc_rollup_client.Subkeys
      ~key:(Durable_storage_path.storage address ())
  in
  return (List.length storage)

let check_storage_size sc_rollup_client ~address size =
  (* check storage size *)
  let* storage_size = get_storage_size sc_rollup_client ~address in
  Check.((storage_size = size) int)
    ~error_msg:"Unexpected storage size, should be %R, but is %L" ;
  unit

(** [next_evm_level ~sc_rollup_node ~node ~client] moves [sc_rollup_node] to
    the [node]'s next level. *)
let next_evm_level ~sc_rollup_node ~node ~client =
  let* () = Client.bake_for_and_wait client in
  let* level = Node.get_level node in
  Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level

(** [wait_for_transaction_receipt ~evm_proxy_server ~transaction_hash] takes an
    transaction_hash and returns only when the receipt is non null, or [count]
    blocks have passed and the receipt is still not available. *)
let wait_for_transaction_receipt ?(count = 3) ~evm_proxy_server
    ~transaction_hash () =
  let rec loop count =
    let* () = Lwt_unix.sleep 5. in
    let* receipt =
      Evm_proxy_server.(
        call_evm_rpc
          evm_proxy_server
          {
            method_ = "eth_getTransactionReceipt";
            parameters = `A [`String transaction_hash];
          })
    in
    if receipt |> Evm_proxy_server.extract_result |> JSON.is_null then
      if count > 0 then loop (count - 1)
      else Test.fail "Transaction still hasn't been included"
    else
      receipt |> Evm_proxy_server.extract_result
      |> Transaction.transaction_receipt_of_json |> return
  in
  loop count

let wait_for_application ~sc_rollup_node ~node ~client apply () =
  let* start_level = Client.level client in
  let max_iteration = 10 in
  let application_result = apply () in
  let rec loop () =
    let* () = Lwt_unix.sleep 5. in
    let* new_level = next_evm_level ~sc_rollup_node ~node ~client in
    if start_level + max_iteration < new_level then
      Test.fail
        "Baked more than %d blocks and the operation's application is still \
         pending"
        max_iteration ;
    if Lwt.state application_result = Lwt.Sleep then loop () else unit
  in
  (* Using [Lwt.both] ensures that any exception thrown in [tx_hash] will be
     thrown by [Lwt.both] as well. *)
  let* result, () = Lwt.both application_result (loop ()) in
  return result

let send_and_wait_until_tx_mined ~sc_rollup_node ~node ~client
    ~source_private_key ~to_public_key ~value ~evm_proxy_server_endpoint ?data
    () =
  let send =
    Eth_cli.transaction_send
      ~source_private_key
      ~to_public_key
      ~value
      ~endpoint:evm_proxy_server_endpoint
      ?data
  in
  wait_for_application ~sc_rollup_node ~node ~client send ()

(* sending more than ~300 tx could fail, because or curl *)
let send_n_transactions ~sc_rollup_node ~node ~client ~evm_proxy_server txs =
  let requests =
    List.map
      (fun tx ->
        Evm_proxy_server.
          {method_ = "eth_sendRawTransaction"; parameters = `A [`String tx]})
      txs
  in
  let* hashes = Evm_proxy_server.batch_evm_rpc evm_proxy_server requests in
  let hashes =
    hashes |> JSON.as_list
    |> List.map (fun json ->
           Evm_proxy_server.extract_result json |> JSON.as_string)
  in
  let first_hash = List.hd hashes in
  (* Let's wait until one of the transactions is injected into a block, and
      test this block contains the `n` transactions as expected. *)
  let* receipt =
    wait_for_application
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt
         ~evm_proxy_server
         ~transaction_hash:first_hash)
      ()
  in
  return (requests, receipt, hashes)

let setup_l1_contracts ~admin client =
  (* Originates the exchanger. *)
  let* exchanger =
    Client.originate_contract
      ~alias:"exchanger"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:(exchanger_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait client in

  (* Originates the bridge. *)
  let* bridge =
    Client.originate_contract
      ~alias:"evm-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "Pair %S None" exchanger)
      ~prg:(bridge_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait client in

  (* Originates the administrator contract. *)
  let* admin =
    Client.originate_contract
      ~alias:"evm-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "%S" admin.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait client in

  return {exchanger; bridge; admin}

let default_bootstrap_account_balance = Wei.of_eth_int 9999

let make_config ?bootstrap_accounts ?ticketer ?administrator () =
  let open Sc_rollup_helpers.Installer_kernel_config in
  let ticketer =
    Option.fold
      ~some:(fun ticketer ->
        let value = Hex.(of_string ticketer |> show) in
        let to_ = Durable_storage_path.ticketer in
        [Set {value; to_}])
      ~none:[]
      ticketer
  in
  let bootstrap_accounts =
    Option.fold
      ~some:
        (Array.fold_left
           (fun acc Eth_account.{address; _} ->
             let value =
               Wei.(to_le_bytes default_bootstrap_account_balance)
               |> Hex.of_bytes |> Hex.show
             in
             let to_ = Durable_storage_path.balance address in
             Set {value; to_} :: acc)
           [])
      ~none:[]
      bootstrap_accounts
  in
  let administrator =
    Option.fold
      ~some:(fun administrator ->
        let to_ = Durable_storage_path.admin in
        let value = Hex.(of_string administrator |> show) in
        [Set {value; to_}])
      ~none:[]
      administrator
  in
  match ticketer @ bootstrap_accounts @ administrator with
  | [] -> None
  | res -> Some (`Config res)

type kernel_installee = {base_installee : string; installee : string}

let setup_evm_kernel ?config ?kernel_installee
    ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(rollup_operator_key = Constant.bootstrap1.public_key_hash)
    ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    ?(with_administrator = true) ~admin protocol =
  let* node, client = setup_l1 protocol in
  let* l1_contracts =
    match admin with
    | Some admin ->
        let* res = setup_l1_contracts ~admin client in
        return (Some res)
    | None -> return None
  in
  (* If a L1 bridge was set up, we make the kernel aware of the address. *)
  let config =
    match config with
    | Some config -> Some config
    | None ->
        let ticketer =
          Option.map (fun {exchanger; _} -> exchanger) l1_contracts
        in
        let administrator =
          if with_administrator then
            Option.map (fun {admin; _} -> admin) l1_contracts
          else None
        in
        make_config ~bootstrap_accounts ?ticketer ?administrator ()
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:rollup_operator_key
  in
  (* Start a rollup node *)
  let* boot_sector =
    let base_installee, installee =
      match kernel_installee with
      | Some {base_installee; installee} -> (base_installee, installee)
      | None -> ("./", "evm_kernel")
    in
    prepare_installer_kernel
      ~base_installee
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      ?config
      installee
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector
      ~parameters_ty:"pair (pair bytes (ticket unit)) (pair nat bytes)"
      ~src:originator_key
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address ["--log-kernel-debug"]
  in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  (* EVM Kernel installation level. *)
  let* () = Client.bake_for_and_wait client in
  let* level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
  let* evm_proxy_server =
    Evm_proxy_server.init ~mode:`Development sc_rollup_node
  in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  return
    {
      node;
      client;
      sc_rollup_node;
      sc_rollup_client;
      sc_rollup_address;
      originator_key;
      rollup_operator_key;
      evm_proxy_server;
      endpoint;
      l1_contracts;
    }

let setup_past_genesis ?config ?with_administrator ?kernel_installee
    ?originator_key ?bootstrap_accounts ?rollup_operator_key ~admin protocol =
  let* ({node; client; sc_rollup_node; _} as full_setup) =
    setup_evm_kernel
      ?config
      ?kernel_installee
      ?originator_key
      ?bootstrap_accounts
      ?rollup_operator_key
      ?with_administrator
      ~admin
      protocol
  in
  (* Force a level to got past the genesis block *)
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  return full_setup

let setup_mockup () =
  let evm_proxy_server = Evm_proxy_server.mockup () in
  let* () = Evm_proxy_server.run evm_proxy_server in
  return evm_proxy_server

type contract = {label : string; abi : string; bin : string}

let deploy ~contract ~sender full_evm_setup =
  let {node; client; sc_rollup_node; evm_proxy_server; _} = full_evm_setup in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* () = Eth_cli.add_abi ~label:contract.label ~abi:contract.abi () in
  let send_deploy () =
    Eth_cli.deploy
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint:evm_proxy_server_endpoint
      ~abi:contract.label
      ~bin:contract.bin
  in
  wait_for_application ~sc_rollup_node ~node ~client send_deploy ()

let send ~sender ~receiver ~value ?data full_evm_setup =
  let {node; client; sc_rollup_node; evm_proxy_server; _} = full_evm_setup in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let send =
    Eth_cli.transaction_send
      ~source_private_key:sender.Eth_account.private_key
      ~to_public_key:receiver.Eth_account.address
      ~value
      ~endpoint:evm_proxy_server_endpoint
      ?data
  in
  wait_for_application ~sc_rollup_node ~node ~client send ()

let send_external_message_and_wait ~sc_rollup_node ~node ~client ~sender
    ~hex_msg =
  let* () =
    Client.Sc_rollup.send_message
      ~src:sender
      ~msg:("hex:[ \"" ^ hex_msg ^ "\" ]")
      client
  in
  let* _ = next_evm_level ~sc_rollup_node ~node ~client in
  unit

let check_block_progression ~sc_rollup_node ~node ~client ~endpoint
    ~expected_block_level =
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let* block_number = Eth_cli.block_number ~endpoint in
  return
  @@ Check.((block_number = expected_block_level) int)
       ~error_msg:"Unexpected block number, should be %%R, but got %%L"

let test_evm_proxy_server_connection =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~title:"EVM proxy server connection"
  @@ fun protocol ->
  let* tezos_node, tezos_client = setup_l1 protocol in
  let* sc_rollup =
    originate_sc_rollup
      ~kind:"wasm_2_0_0"
      ~parameters_ty:"string"
      ~src:Constant.bootstrap1.alias
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Observer
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:Constant.bootstrap1.alias
  in
  let evm_proxy = Evm_proxy_server.create sc_rollup_node in
  (* Tries to start the EVM proxy server without a listening rollup node. *)
  let process = Evm_proxy_server.spawn_run evm_proxy in
  let* () = Process.check ~expect_failure:true process in
  (* Starts the rollup node. *)
  let* _ = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  (* Starts the EVM proxy server and asks its version. *)
  let* () = Evm_proxy_server.run evm_proxy in
  let*? process = evm_proxy_server_version evm_proxy in
  let* () = Process.check process in
  unit

let test_originate_evm_kernel =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~title:"Originate EVM kernel with installer"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; sc_rollup_client; _} =
    setup_evm_kernel ~admin:None protocol
  in
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* () = Client.bake_for_and_wait client in
  let* first_evm_run_level = Node.get_level node in
  let* level =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      first_evm_run_level
  in
  Check.(level = first_evm_run_level)
    Check.int
    ~error_msg:"Current level has moved past first EVM run (%L = %R)" ;
  let evm_key = "evm" in
  let*! storage_root_keys =
    Sc_rollup_client.inspect_durable_state_value
      ~hooks
      sc_rollup_client
      ~pvm_kind
      ~operation:Sc_rollup_client.Subkeys
      ~key:""
  in
  Check.(
    list_mem
      string
      evm_key
      storage_root_keys
      ~error_msg:"Expected %L to be initialized by the EVM kernel.") ;
  unit

let test_rpc_getBalance =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_balance"]
    ~title:"RPC method eth_getBalance"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* balance =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:evm_proxy_server_endpoint
  in
  Check.((balance = default_bootstrap_account_balance) Wei.typ)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Eth_account.bootstrap_accounts.(0).address) ;
  unit

let test_rpc_getBlockByNumber =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_block_by_number"]
    ~title:"RPC method eth_getBlockByNumber"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  unit

let test_rpc_getBlockByHash =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_block_by_hash"]
    ~title:"RPC method eth_getBlockByHash"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  let* block' =
    Eth_cli.get_block
      ~block_id:(Option.get block.hash)
      ~endpoint:evm_proxy_server_endpoint
  in
  assert (block = block') ;
  unit

let test_l2_block_size_non_zero =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "block"; "size"]
    ~title:"Block size is greater than zero"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  Check.((block.size > 0l) int32)
    ~error_msg:"Unexpected block size, should be > 0, but got %%L" ;
  unit

let transaction_count_request address =
  Evm_proxy_server.
    {
      method_ = "eth_getTransactionCount";
      parameters = `A [`String address; `String "latest"];
    }

let get_transaction_count proxy_server address =
  let* transaction_count =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (transaction_count_request address)
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

let test_rpc_getTransactionCount =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_transaction_count"]
    ~title:"RPC method eth_getTransactionCount"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let* transaction_count =
    get_transaction_count
      evm_proxy_server
      Eth_account.bootstrap_accounts.(0).address
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  unit

let test_rpc_getTransactionCountBatch =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_transaction_count_as_batch"]
    ~title:"RPC method eth_getTransactionCount in batch"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let* transaction_count =
    get_transaction_count
      evm_proxy_server
      Eth_account.bootstrap_accounts.(0).address
  in
  let* transaction_count_batch =
    let* transaction_count =
      Evm_proxy_server.batch_evm_rpc
        evm_proxy_server
        [transaction_count_request Eth_account.bootstrap_accounts.(0).address]
    in
    match JSON.as_list transaction_count with
    | [transaction_count] ->
        return JSON.(transaction_count |-> "result" |> as_int64)
    | _ -> Test.fail "Unexpected result from batching one request"
  in
  Check.((transaction_count = transaction_count_batch) int64)
    ~error_msg:"Nonce from a single request is %L, but got %R from batching it" ;
  unit

let test_rpc_batch =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "rpc"; "batch"]
    ~title:"RPC batch requests"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let* transaction_count, chain_id =
    let transaction_count =
      transaction_count_request Eth_account.bootstrap_accounts.(0).address
    in
    let chain_id =
      Evm_proxy_server.{method_ = "eth_chainId"; parameters = `Null}
    in
    let* results =
      Evm_proxy_server.batch_evm_rpc
        evm_proxy_server
        [transaction_count; chain_id]
    in
    match JSON.as_list results with
    | [transaction_count; chain_id] ->
        return
          ( JSON.(transaction_count |-> "result" |> as_int64),
            JSON.(chain_id |-> "result" |> as_int64) )
    | _ -> Test.fail "Unexpected result from batching two requests"
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  (* Default chain id for Ethereum custom networks, not chosen randomly. *)
  let default_chain_id = 1337L in
  Check.((chain_id = default_chain_id) int64)
    ~error_msg:"Expected a chain_id of %R, but got %L" ;
  unit

let test_l2_blocks_progression =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_blocks_progression"]
    ~title:"Check L2 blocks progression"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; endpoint; _} =
    setup_evm_kernel ~admin:None protocol
  in
  let* () =
    check_block_progression
      ~sc_rollup_node
      ~node
      ~client
      ~endpoint
      ~expected_block_level:1
  in
  let* () =
    check_block_progression
      ~sc_rollup_node
      ~node
      ~client
      ~endpoint
      ~expected_block_level:2
  in
  unit

let test_consistent_block_hashes =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_blocks"]
    ~title:"Check L2 blocks consistency of hashes"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; endpoint; _} =
    setup_evm_kernel ~admin:None protocol
  in
  let new_block () =
    let* _level = next_evm_level ~sc_rollup_node ~node ~client in
    let* number = Eth_cli.block_number ~endpoint in
    Eth_cli.get_block ~block_id:(string_of_int number) ~endpoint
  in

  let* block0 = new_block () in
  let* block1 = new_block () in
  let* block2 = new_block () in
  let* block3 = new_block () in

  let check_parent_hash parent block =
    let parent_hash = Option.value ~default:"" parent.Block.hash in
    Check.((block.Block.parent = parent_hash) string)
      ~error_msg:"Unexpected parent hash, should be %%R, but got %%L"
  in

  (* Check consistency accross blocks. *)
  check_parent_hash block0 block1 ;
  check_parent_hash block1 block2 ;
  check_parent_hash block2 block3 ;

  let block_hashes, parent_hashes =
    List.map
      (fun Block.{hash; parent; _} -> (hash, parent))
      [block0; block1; block2; block3]
    |> List.split
  in
  let block_hashes_uniq = List.sort_uniq compare block_hashes in
  let parent_hashes_uniq = List.sort_uniq compare parent_hashes in

  (* Check unicity of hashes and parent hashes. *)
  Check.(List.(length block_hashes = length block_hashes_uniq) int)
    ~error_msg:"The list of block hashes must be unique" ;
  Check.(List.(length parent_hashes = length parent_hashes_uniq) int)
    ~error_msg:"The list of block parent hashes must be unique" ;

  unit

(** The info for the "storage.sol" contract.
    See [src/kernel_evm/solidity_examples] *)
let simple_storage =
  {
    label = "simpleStorage";
    abi = kernel_inputs_path ^ "/storage.abi";
    bin = kernel_inputs_path ^ "/storage.bin";
  }

(** The info for the "erc20tok.sol" contract.
    See [src/kernel_evm/solidity_examples] *)
let erc20 =
  {
    label = "erc20tok";
    abi = kernel_inputs_path ^ "/erc20tok.abi";
    bin = kernel_inputs_path ^ "/erc20tok.bin";
  }

(** The info for the "loop.sol" contract.
    See [src/kernel_evm/benchmarks/scripts/benchmarks/contracts/loop.sol] *)
let loop =
  {
    label = "loop";
    abi = kernel_inputs_path ^ "/loop.abi";
    bin = kernel_inputs_path ^ "/loop.bin";
  }

(** Test that the contract creation works.  *)
let test_l2_deploy_simple_storage =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"]
    ~title:"Check L2 contract deployment"
  @@ fun protocol ->
  let* ({sc_rollup_client; evm_proxy_server; _} as full_evm_setup) =
    setup_past_genesis ~admin:None protocol
  in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address, tx =
    deploy ~contract:simple_storage ~sender full_evm_setup
  in
  let address = String.lowercase_ascii contract_address in
  Check.(
    (address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
      string
      ~error_msg:"Expected address to be %R but was %L.") ;

  let* code_in_kernel =
    Evm_proxy_server.fetch_contract_code evm_proxy_server contract_address
  in
  (* The same deployment has been reproduced on the Sepolia testnet, resulting
     on this specific code. *)
  let expected_code =
    "0x608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033"
  in
  Check.((code_in_kernel = expected_code) string)
    ~error_msg:"Unexpected code %L, it should be %R" ;
  (* The transaction was a contract creation, the transaction object
     must not contain the [to] field. *)
  let* tx_object = Eth_cli.transaction_get ~endpoint ~tx_hash:tx in
  (match tx_object with
  | Some tx_object ->
      Check.((tx_object.to_ = None) (option string))
        ~error_msg:
          "The transaction object of a contract creation should not have the \
           [to] field present"
  | None -> Test.fail "The transaction object of %s should be available" tx) ;

  let*! accounts =
    Sc_rollup_client.inspect_durable_state_value
      ~hooks
      sc_rollup_client
      ~pvm_kind
      ~operation:Sc_rollup_client.Subkeys
      ~key:Durable_storage_path.eth_accounts
  in
  (* check tx status*)
  let* () = check_tx_succeeded ~endpoint ~tx in

  (* check contract account was created *)
  Check.(
    list_mem
      string
      (Helpers.normalize contract_address)
      (List.map String.lowercase_ascii accounts)
      ~error_msg:"Expected %L account to be initialized by contract creation.") ;
  unit

let send_call_set_storage_simple contract_address sender n
    {sc_rollup_node; node; client; endpoint; _} =
  let call_set (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:simple_storage.label
      ~address:contract_address
      ~method_call:(Printf.sprintf "set(%d)" n)
  in
  wait_for_application ~sc_rollup_node ~node ~client (call_set sender n) ()

(** Test that a contract can be called,
    and that the call can modify the storage.  *)
let test_l2_call_simple_storage =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"; "l2_call"]
    ~title:"Check L2 contract call"
  @@ fun protocol ->
  (* setup *)
  let* ({evm_proxy_server; sc_rollup_client; _} as evm_setup) =
    setup_past_genesis ~admin:None protocol
  in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let sender = Eth_account.bootstrap_accounts.(0) in

  (* deploy contract *)
  let* address, _tx = deploy ~contract:simple_storage ~sender evm_setup in

  (* set 42 *)
  let* tx = send_call_set_storage_simple address sender 42 evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_client ~address 1 in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:42 in

  (* set 24 by another user *)
  let* tx =
    send_call_set_storage_simple
      address
      Eth_account.bootstrap_accounts.(1)
      24
      evm_setup
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_client ~address 1 in
  (* value stored has changed *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:24 in

  (* set -1 *)
  (* some environments prevent sending a negative value, as the value is
     unsigned (eg remix) but it is actually the expected result *)
  let* tx = send_call_set_storage_simple address sender (-1) evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_client ~address 1 in
  (* value stored has changed *)
  let* () =
    check_str_in_storage
      ~evm_setup
      ~address
      ~nth:0
      ~expected:
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  in
  unit

let test_l2_deploy_erc20 =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"; "erc20"; "l2_call"]
    ~title:"Check L2 erc20 contract deployment"
  @@ fun protocol ->
  (* setup *)
  let* ({sc_rollup_client; evm_proxy_server; node; client; sc_rollup_node; _} as
       evm_setup) =
    setup_past_genesis ~admin:None protocol
  in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let player = Eth_account.bootstrap_accounts.(1) in

  (* deploy the contract *)
  let* address, tx = deploy ~contract:erc20 ~sender evm_setup in
  Check.(
    (String.lowercase_ascii address
    = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
      string
      ~error_msg:"Expected address to be %R but was %L.") ;

  (* check tx status *)
  let* () = check_tx_succeeded ~endpoint ~tx in

  (* check account was created *)
  let*! accounts =
    Sc_rollup_client.inspect_durable_state_value
      ~hooks
      sc_rollup_client
      ~pvm_kind
      ~operation:Sc_rollup_client.Subkeys
      ~key:Durable_storage_path.eth_accounts
  in
  Check.(
    list_mem
      string
      (Helpers.normalize address)
      (List.map String.lowercase_ascii accounts)
      ~error_msg:"Expected %L account to be initialized by contract creation.") ;

  (* minting / burning *)
  let call_mint (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20.label
      ~address
      ~method_call:(Printf.sprintf "mint(%d)" n)
  in
  let call_burn ?(expect_failure = false) (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20.label
      ~address
      ~method_call:(Printf.sprintf "burn(%d)" n)
  in
  let transfer_event_topic =
    let h =
      Tezos_crypto.Hacl.Hash.Keccak_256.digest
        (Bytes.of_string "Transfer(address,address,uint256)")
    in
    "0x" ^ Hex.show (Hex.of_bytes h)
  in
  let zero_address = "0x" ^ String.make 64 '0' in
  let mint_logs sender amount =
    [
      ( address,
        [transfer_event_topic; zero_address; hex_256_of_address sender],
        "0x" ^ hex_256_of amount );
    ]
  in
  let burn_logs sender amount =
    [
      ( address,
        [transfer_event_topic; hex_256_of_address sender; zero_address],
        "0x" ^ hex_256_of amount );
    ]
  in
  (* sender mints 42 *)
  let* tx =
    wait_for_application ~sc_rollup_node ~node ~client (call_mint sender 42) ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs sender 42) ~tx
  in

  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:42 in

  (* player mints 100 *)
  let* tx =
    wait_for_application ~sc_rollup_node ~node ~client (call_mint player 100) ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs player 100) ~tx
  in
  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 100, should fail *)
  let* _tx =
    wait_for_application
      ~sc_rollup_node
      ~node
      ~client
      (call_burn ~expect_failure:true sender 100)
      ()
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 42, should succeed *)
  let* tx =
    wait_for_application ~sc_rollup_node ~node ~client (call_burn sender 42) ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(burn_logs sender 42) ~tx
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:100 in
  unit

(* TODO: add internal parameters here (e.g the kernel version) *)
type config_result = {chain_id : int64}

let config_setup evm_setup =
  let web3_clientVersion =
    Evm_proxy_server.{method_ = "web3_clientVersion"; parameters = `A []}
  in
  let chain_id =
    Evm_proxy_server.{method_ = "eth_chainId"; parameters = `Null}
  in
  let* results =
    Evm_proxy_server.batch_evm_rpc
      evm_setup.evm_proxy_server
      [web3_clientVersion; chain_id]
  in
  match JSON.as_list results with
  | [web3_clientVersion; chain_id] ->
      (* We don't need to return the web3_clientVersion because,
         it might change after the upgrade.
         The only thing that we need to look out for is,
         are we able to retrieve it and deserialize it. *)
      let _sanity_check = JSON.(web3_clientVersion |-> "result" |> as_string) in
      return {chain_id = JSON.(chain_id |-> "result" |> as_int64)}
  | _ -> Test.fail "Unexpected result from batching two requests"

let ensure_config_setup_integrity ~config_result evm_setup =
  let* upcoming_config_setup = config_setup evm_setup in
  assert (config_result = upcoming_config_setup) ;
  unit

let ensure_block_integrity ~block_result evm_setup =
  let* block =
    Eth_cli.get_block
      ~block_id:(Int32.to_string block_result.Block.number)
      ~endpoint:evm_setup.endpoint
  in
  (* only the relevant fields *)
  assert (block.number = block_result.number) ;
  assert (block.hash = block_result.hash) ;
  assert (block.timestamp = block_result.timestamp) ;
  assert (block.transactions = block_result.transactions) ;
  unit

let get_block_by_number ?(full_tx_objects = false) evm_setup block_param =
  let* latest_block =
    Evm_proxy_server.(
      call_evm_rpc
        evm_setup.evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters = `A [`String block_param; `Bool full_tx_objects];
        })
  in
  return @@ (latest_block |> Evm_proxy_server.extract_result |> Block.of_json)

let latest_block ?(full_tx_objects = false) evm_setup =
  get_block_by_number ~full_tx_objects evm_setup "latest"

type transfer_result = {
  sender_balance_before : Wei.t;
  sender_balance_after : Wei.t;
  sender_nonce_before : int64;
  sender_nonce_after : int64;
  value : Wei.t;
  tx_hash : string;
  tx_object : Transaction.transaction_object;
  tx_receipt : Transaction.transaction_receipt;
  receiver_balance_before : Wei.t;
  receiver_balance_after : Wei.t;
}

let get_tx_object ~endpoint ~tx_hash =
  let* tx_object = Eth_cli.transaction_get ~endpoint ~tx_hash in
  match tx_object with
  | Some tx_object -> return tx_object
  | None -> Test.fail "The transaction object of %s should be available" tx_hash

let get_transaction_receipt ~full_evm_setup ~tx_hash =
  let* json =
    Evm_proxy_server.call_evm_rpc
      full_evm_setup.evm_proxy_server
      {method_ = "eth_getTransactionReceipt"; parameters = `A [`String tx_hash]}
  in
  return JSON.(json |-> "result" |> Transaction.transaction_receipt_of_json)

let ensure_transfer_result_integrity ~transfer_result ~sender ~receiver
    full_evm_setup =
  let endpoint = Evm_proxy_server.endpoint full_evm_setup.evm_proxy_server in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance = balance sender.Eth_account.address in
  assert (sender_balance = transfer_result.sender_balance_after) ;
  let* receiver_balance = balance receiver.Eth_account.address in
  assert (receiver_balance = transfer_result.receiver_balance_after) ;
  let* sender_nonce =
    get_transaction_count full_evm_setup.evm_proxy_server sender.address
  in
  assert (sender_nonce = transfer_result.sender_nonce_after) ;
  let* tx_object = get_tx_object ~endpoint ~tx_hash:transfer_result.tx_hash in
  assert (tx_object = transfer_result.tx_object) ;
  let* tx_receipt =
    get_transaction_receipt ~full_evm_setup ~tx_hash:transfer_result.tx_hash
  in
  assert (tx_receipt = transfer_result.tx_receipt) ;
  unit

let make_transfer ?data ~value ~sender ~receiver full_evm_setup =
  let endpoint = Evm_proxy_server.endpoint full_evm_setup.evm_proxy_server in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance_before = balance sender.Eth_account.address in
  let* receiver_balance_before = balance receiver.Eth_account.address in
  let* sender_nonce_before =
    get_transaction_count full_evm_setup.evm_proxy_server sender.address
  in
  let* tx_hash = send ~sender ~receiver ~value ?data full_evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx:tx_hash in
  let* sender_balance_after = balance sender.address in
  let* receiver_balance_after = balance receiver.address in
  let* sender_nonce_after =
    get_transaction_count full_evm_setup.evm_proxy_server sender.address
  in
  let* tx_object = get_tx_object ~endpoint ~tx_hash in
  let* tx_receipt = get_transaction_receipt ~full_evm_setup ~tx_hash in
  return
    {
      sender_balance_before;
      sender_balance_after;
      sender_nonce_before;
      sender_nonce_after;
      value;
      tx_hash;
      tx_object;
      tx_receipt;
      receiver_balance_before;
      receiver_balance_after;
    }

let transfer ?data protocol =
  let* full_evm_setup = setup_past_genesis ~admin:None protocol in
  let sender, receiver =
    (Eth_account.bootstrap_accounts.(0), Eth_account.bootstrap_accounts.(1))
  in
  let* {
         sender_balance_before;
         sender_balance_after;
         sender_nonce_before;
         sender_nonce_after;
         value;
         tx_object;
         receiver_balance_before;
         receiver_balance_after;
         _;
       } =
    make_transfer
      ?data
      ~value:Wei.(default_bootstrap_account_balance - one)
      ~sender
      ~receiver
      full_evm_setup
  in
  let* receipt =
    Eth_cli.get_receipt ~endpoint:full_evm_setup.endpoint ~tx:tx_object.hash
  in
  let fees =
    match receipt with
    | Some Transaction.{status = true; gasUsed; effectiveGasPrice; _} ->
        Int32.(to_string (mul gasUsed effectiveGasPrice)) |> Wei.of_string
    | _ -> Test.fail "Transaction didn't succeed"
  in
  Check.(
    Wei.(sender_balance_after = sender_balance_before - value - fees) Wei.typ)
    ~error_msg:
      "Unexpected sender balance after transfer, should be %R, but got %L" ;
  Check.(Wei.(receiver_balance_after = receiver_balance_before + value) Wei.typ)
    ~error_msg:
      "Unexpected receiver balance after transfer, should be %R, but got %L" ;
  Check.((sender_nonce_after = Int64.succ sender_nonce_before) int64)
    ~error_msg:
      "Unexpected sender nonce after transfer, should be %R, but got %L" ;
  (* Perform some sanity checks on the transaction object produced by the
     kernel. *)
  Check.((tx_object.from = sender.address) string)
    ~error_msg:"Unexpected transaction's sender" ;
  Check.((tx_object.to_ = Some receiver.address) (option string))
    ~error_msg:"Unexpected transaction's receiver" ;
  Check.((tx_object.value = value) Wei.typ)
    ~error_msg:"Unexpected transaction's value" ;
  unit

let test_l2_transfer =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_transfer"]
    ~title:"Check L2 transfers are applied"
    transfer

let test_chunked_transaction =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_transfer"; "chunked"]
    ~title:"Check L2 chunked transfers are applied"
  @@ transfer ~data:("0x" ^ String.make 12_000 'a')

let test_rpc_txpool_content =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "txpool_content"]
    ~title:"Check RPC txpool_content is available"
  @@ fun _protocol ->
  let* evm_proxy_server = setup_mockup () in
  (* The content of the txpool is not relevant for now, this test only checks
     the the RPC is correct, i.e. an object containing both the `pending` and
     `queued` fields, containing the correct objects: addresses pointing to a
     mapping of nonces to transactions. *)
  let* _result = Evm_proxy_server.txpool_content evm_proxy_server in
  unit

let test_rpc_web3_clientVersion =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "client_version"]
    ~title:"Check RPC web3_clientVersion"
  @@ fun _protocol ->
  let* evm_proxy_server = setup_mockup () in
  let* web3_clientVersion =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {method_ = "web3_clientVersion"; parameters = `A []})
  in
  let* server_version =
    evm_proxy_server_version evm_proxy_server |> Runnable.run
  in
  Check.(
    (JSON.(web3_clientVersion |-> "result" |> as_string)
    = JSON.as_string server_version)
      string)
    ~error_msg:"Expected version %%R, got %%L." ;
  unit

let test_rpc_web3_sha3 =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sha3"]
    ~title:"Check RPC web3_sha3"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  (* From the example provided in
     https://ethereum.org/en/developers/docs/apis/json-rpc/#web3_sha3 *)
  let input_data = "0x68656c6c6f20776f726c64" in
  let expected_reply =
    "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
  in
  let* web3_sha3 =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {method_ = "web3_sha3"; parameters = `A [`String input_data]})
  in
  Check.((JSON.(web3_sha3 |-> "result" |> as_string) = expected_reply) string)
    ~error_msg:"Expected hash %%R, got %%L." ;
  unit

let test_simulate =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "simulate"]
    ~title:"A block can be simulated in the rollup node"
    (fun protocol ->
      let* {evm_proxy_server; sc_rollup_client; _} =
        setup_past_genesis ~admin:None protocol
      in
      let* json =
        Evm_proxy_server.call_evm_rpc
          evm_proxy_server
          {method_ = "eth_blockNumber"; parameters = `A []}
      in
      let block_number =
        JSON.(json |-> "result" |> as_string |> int_of_string)
      in
      let*! simulation_result =
        Sc_rollup_client.simulate
          ~insight_requests:
            [`Durable_storage_key ["evm"; "blocks"; "current"; "number"]]
          sc_rollup_client
          []
      in
      let simulated_block_number =
        match simulation_result.insights with
        | [insight] ->
            Option.map
              (fun hex -> `Hex hex |> Hex.to_string |> Z.of_bits |> Z.to_int)
              insight
        | _ -> None
      in
      Check.((simulated_block_number = Some (block_number + 1)) (option int))
        ~error_msg:"The simulation should advance one L2 block" ;
      unit)

let read_tx_from_file () =
  read_file (kernel_inputs_path ^ "/100-inputs-for-proxy")
  |> String.trim |> String.split_on_char '\n'
  |> List.map (fun line ->
         match String.split_on_char ' ' line with
         | [tx_raw; tx_hash] -> (tx_raw, tx_hash)
         | _ -> failwith "Unexpected tx_raw and tx_hash.")

let test_full_blocks =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "full_blocks"]
    ~title:
      "Check `evm_getBlockByNumber` with full blocks returns the correct \
       informations"
  @@ fun protocol ->
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  let* {evm_proxy_server; sc_rollup_node; node; client; _} =
    setup_past_genesis ~config ~admin:None protocol
  in
  let txs =
    read_tx_from_file ()
    |> List.filteri (fun i _ -> i < 5)
    |> List.map (fun (tx, _hash) -> tx)
  in
  let* _requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_proxy_server txs
  in
  let* block =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool true];
        })
  in
  let block = block |> Evm_proxy_server.extract_result |> Block.of_json in
  let block_hash =
    match block.hash with
    | Some hash -> hash
    | None -> Test.fail "Expected a hash for the block"
  in
  let block_number = block.number in
  (match block.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full transactions ->
      List.iteri
        (fun index
             ({blockHash; blockNumber; transactionIndex; _} :
               Transaction.transaction_object) ->
          Check.((block_hash = blockHash) string)
            ~error_msg:
              (sf "The transaction should be in block %%L but found %%R") ;
          Check.((block_number = blockNumber) int32)
            ~error_msg:
              (sf "The transaction should be in block %%L but found %%R") ;
          Check.((Int32.of_int index = transactionIndex) int32)
            ~error_msg:
              (sf "The transaction should be at index %%L but found %%R"))
        transactions
  | Block.Hash _ -> Test.fail "Block is supposed to contain transaction objects") ;
  unit

let test_latest_block =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "blocks"; "latest"]
    ~title:
      "Check `evm_getBlockByNumber` works correctly when asking for the \
       `latest`"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  (* The first execution of the kernel actually builds two blocks: the genesis
     block and the block for the current inbox. As such, the latest block is
     always of level 1. *)
  let* latest_block =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters = `A [`String "latest"; `Bool false];
        })
  in
  let latest_block =
    latest_block |> Evm_proxy_server.extract_result |> Block.of_json
  in
  Check.((latest_block.Block.number = 1l) int32)
    ~error_msg:"Expected latest being block number %R, but got %L" ;
  unit

let test_eth_call_nullable_recipient =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_call"; "null"]
    ~title:"Check `eth_call.to` input can be null"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let* call_result =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_call";
          parameters = `A [`O [("to", `Null)]; `String "latest"];
        })
  in
  (* Check the RPC returns a `result`. *)
  let _result = call_result |> Evm_proxy_server.extract_result in
  unit

let test_inject_100_transactions =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "bigger_blocks"]
    ~title:"Check blocks can contain more than 64 transactions"
  @@ fun protocol ->
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  let* {evm_proxy_server; sc_rollup_node; node; client; _} =
    setup_past_genesis ~config ~admin:None protocol
  in
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_proxy_server txs
  in
  let* block_with_100tx =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A
              [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool false];
        })
  in
  let block_with_100tx =
    block_with_100tx |> Evm_proxy_server.extract_result |> Block.of_json
  in
  (match block_with_100tx.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the latest block, got %L") ;

  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let* latest_evm_level =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {method_ = "eth_blockNumber"; parameters = `A []})
  in
  let latest_evm_level =
    latest_evm_level |> Evm_proxy_server.extract_result |> JSON.as_int32
  in
  (* At each loop, the kernel reads the previous block. Until the patch, the
     kernel failed to read the previous block if there was more than 64 hash,
     this test ensures it works by assessing new blocks are produced. *)
  Check.((latest_evm_level >= Int32.succ block_with_100tx.Block.number) int32)
    ~error_msg:
      "Expected a new block after the one with 100 transactions, but level \
       hasn't changed" ;
  unit

let test_eth_call_large =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_call"; "simulate"; "large"]
    ~title:"eth_estimateGas with a large amount of data"
    (fun protocol ->
      (* setup *)
      let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
      let sender = Eth_account.bootstrap_accounts.(0) in

      (* large request *)
      let eth_call =
        [
          ("to", Ezjsonm.encode_string sender.address);
          ("data", Ezjsonm.encode_string ("0x" ^ String.make 12_000 'a'));
        ]
      in

      (* make call to proxy *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {
              method_ = "eth_call";
              parameters = `A [`O eth_call; `String "latest"];
            })
      in

      (* Check the RPC returns a `result`. *)
      let r = call_result |> Evm_proxy_server.extract_result in
      Check.((JSON.as_string r = "0x") string)
        ~error_msg:"Expected result %R, but got %L" ;

      unit)

let test_estimate_gas =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_estimategas"; "simulate"]
    ~title:"eth_estimateGas for contract creation"
    (fun protocol ->
      (* setup *)
      let* {evm_proxy_server; _} = setup_past_genesis protocol ~admin:None in

      (* large request *)
      let data = read_file simple_storage.bin in
      let eth_call = [("data", Ezjsonm.encode_string @@ "0x" ^ data)] in

      (* make call to proxy *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {method_ = "eth_estimateGas"; parameters = `A [`O eth_call]})
      in

      (* Check the RPC returns a `result`. *)
      let r = call_result |> Evm_proxy_server.extract_result in
      Check.((JSON.as_int r = 23423) int)
        ~error_msg:"Expected result greater than %R, but got %L" ;

      unit)

let test_estimate_gas_additionnal_field =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_estimategas"; "simulate"; "remix"]
    ~title:"eth_estimateGas allows additional fields"
    (fun protocol ->
      (* setup *)
      let* {evm_proxy_server; _} = setup_past_genesis protocol ~admin:None in

      (* large request *)
      let data = read_file simple_storage.bin in
      let eth_call =
        [
          ( "from",
            Ezjsonm.encode_string
            @@ "0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c" );
          ("data", Ezjsonm.encode_string @@ "0x" ^ data);
          ("value", Ezjsonm.encode_string @@ "0x0");
          (* for some reason remix adds the "type" field *)
          ("type", Ezjsonm.encode_string @@ "0x1");
        ]
      in

      (* make call to proxy *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {method_ = "eth_estimateGas"; parameters = `A [`O eth_call]})
      in

      (* Check the RPC returns a `result`. *)
      let r = call_result |> Evm_proxy_server.extract_result in
      Check.((JSON.as_int r = 23423) int)
        ~error_msg:"Expected result greater than %R, but got %L" ;

      unit)

let test_eth_call_storage_contract_rollup_node =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_call"; "simulate"]
    ~title:"Try to call a view (directly through proxy)"
    (fun protocol ->
      (* setup *)
      let* ({evm_proxy_server; endpoint; _} as evm_setup) =
        setup_past_genesis ~admin:None protocol
      in

      let sender = Eth_account.bootstrap_accounts.(0) in

      (* deploy contract *)
      let* address, tx = deploy ~contract:simple_storage ~sender evm_setup in
      let* () = check_tx_succeeded ~endpoint ~tx in
      Check.(
        (String.lowercase_ascii address
        = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
          string
          ~error_msg:"Expected address to be %R but was %L.") ;

      (* craft request *)
      let data = "0x4e70b1dc" in
      let eth_call =
        [
          ("to", Ezjsonm.encode_string address);
          ("data", Ezjsonm.encode_string data);
        ]
      in

      (* make call to proxy *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {
              method_ = "eth_call";
              parameters = `A [`O eth_call; `String "latest"];
            })
      in

      let r = call_result |> Evm_proxy_server.extract_result in
      Check.(
        (JSON.as_string r
       = "0x0000000000000000000000000000000000000000000000000000000000000000")
          string)
        ~error_msg:"Expected result %R, but got %L" ;

      let* tx = send_call_set_storage_simple address sender 42 evm_setup in
      let* () = check_tx_succeeded ~endpoint ~tx in

      (* make call to proxy *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {
              method_ = "eth_call";
              parameters = `A [`O eth_call; `String "latest"];
            })
      in
      let r = call_result |> Evm_proxy_server.extract_result in
      Check.(
        (JSON.as_string r
       = "0x000000000000000000000000000000000000000000000000000000000000002a")
          string)
        ~error_msg:"Expected result %R, but got %L" ;
      unit)

let test_eth_call_storage_contract_proxy =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "simulate"]
    ~title:"Try to call a view (directly through rollup node)"
    (fun protocol ->
      let* ({sc_rollup_client; evm_proxy_server; _} as evm_setup) =
        setup_past_genesis ~admin:None protocol
      in

      let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
      let sender = Eth_account.bootstrap_accounts.(0) in

      (* deploy contract *)
      let* address, tx = deploy ~contract:simple_storage ~sender evm_setup in
      let* () = check_tx_succeeded ~endpoint ~tx in

      Check.(
        (String.lowercase_ascii address
        = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
          string
          ~error_msg:"Expected address to be %R but was %L.") ;

      let*! simulation_result =
        Sc_rollup_client.simulate
          ~insight_requests:
            [
              `Durable_storage_key ["evm"; "simulation_result"];
              `Durable_storage_key ["evm"; "simulation_status"];
            ]
          sc_rollup_client
          [
            Hex.to_string @@ `Hex "ff";
            Hex.to_string
            @@ `Hex
                 "ff0100e68094d77420f73b4612a7a99dba8c2afd30a1886b03448857040000000000008080844e70b1dc";
          ]
      in
      let expected_insights =
        [
          Some "0000000000000000000000000000000000000000000000000000000000000000";
          Some "01";
        ]
      in
      Check.(
        (simulation_result.insights = expected_insights) (list @@ option string))
        ~error_msg:"Expected result %R, but got %L" ;
      unit)

let test_eth_call_storage_contract_eth_cli =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_call"; "simulate"]
    ~title:"Try to call a view through an ethereum client"
    (fun protocol ->
      (* setup *)
      let* ({evm_proxy_server; endpoint; sc_rollup_node; client; node; _} as
           evm_setup) =
        setup_past_genesis ~admin:None protocol
      in

      (* sanity *)
      let* call_result =
        Evm_proxy_server.(
          call_evm_rpc
            evm_proxy_server
            {
              method_ = "eth_call";
              parameters = `A [`O [("to", `Null)]; `String "latest"];
            })
      in
      (* Check the RPC returns a `result`. *)
      let _result = call_result |> Evm_proxy_server.extract_result in

      let sender = Eth_account.bootstrap_accounts.(0) in

      (* deploy contract send send 42 *)
      let* address, _tx = deploy ~contract:simple_storage ~sender evm_setup in
      let* tx = send_call_set_storage_simple address sender 42 evm_setup in
      let* () = check_tx_succeeded ~endpoint ~tx in

      (* make a call to proxy through eth-cli *)
      let call_num =
        Eth_cli.contract_call
          ~endpoint
          ~abi_label:simple_storage.label
          ~address
          ~method_call:"num()"
      in
      let* res =
        wait_for_application ~sc_rollup_node ~node ~client call_num ()
      in

      Check.((String.trim res = "42") string)
        ~error_msg:"Expected result %R, but got %L" ;
      unit)

let test_preinitialized_evm_kernel =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "administrator"; "config"]
    ~title:"Creates a kernel with an initialized administrator key"
  @@ fun protocol ->
  let administrator_key_path = Durable_storage_path.admin in
  let administrator_key = Eth_account.bootstrap_accounts.(0).address in
  let config =
    `Config
      Sc_rollup_helpers.Installer_kernel_config.
        [
          Set
            {
              value = Hex.(of_string administrator_key |> show);
              to_ = administrator_key_path;
            };
        ]
  in
  let* {sc_rollup_client; _} = setup_evm_kernel ~config ~admin:None protocol in
  let*! found_administrator_key_hex =
    Sc_rollup_client.inspect_durable_state_value
      sc_rollup_client
      ~pvm_kind:"wasm_2_0_0"
      ~operation:Sc_rollup_client.Value
      ~key:administrator_key_path
  in
  let found_administrator_key =
    Option.map
      (fun administrator -> Hex.to_string (`Hex administrator))
      found_administrator_key_hex
  in
  Check.((Some administrator_key = found_administrator_key) (option string))
    ~error_msg:
      (sf "Expected to read %%L as administrator key, but found %%R instead") ;
  unit

let deposit ~amount_mutez ~bridge ~depositor ~receiver ~sc_rollup_node
    ~sc_rollup_address ~node client =
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S %s" sc_rollup_address receiver)
      ~amount:amount_mutez
      ~giver:depositor.Account.public_key_hash
      ~receiver:bridge
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait client in

  let* _ = next_evm_level ~sc_rollup_node ~node ~client in
  unit

let check_balance ~receiver ~endpoint expected_balance =
  let* balance = Eth_cli.balance ~account:receiver ~endpoint in
  let balance = Wei.truncate_to_mutez balance in
  Check.((balance = Tez.to_mutez expected_balance) int)
    ~error_msg:(sf "Expected balance of %s should be %%R, but got %%L" receiver) ;
  unit

let test_deposit =
  Protocol.register_test ~__FILE__ ~tags:["evm"; "deposit"] ~title:"Deposit tez"
  @@ fun protocol ->
  let admin = Constant.bootstrap5 in
  let* {
         client;
         sc_rollup_address;
         l1_contracts;
         sc_rollup_node;
         node;
         endpoint;
         _;
       } =
    setup_evm_kernel ~admin:(Some admin) protocol
  in
  let {bridge; admin = _; exchanger = _} =
    match l1_contracts with
    | Some x -> x
    | None -> Test.fail ~__LOC__ "The test needs the L1 bridge"
  in

  let amount_mutez = Tez.of_mutez_int 100_000_000 in
  let receiver = "0x119811f34EF4491014Fbc3C969C426d37067D6A4" in

  let* () =
    deposit
      ~amount_mutez
      ~sc_rollup_address
      ~bridge
      ~depositor:admin
      ~receiver
      ~sc_rollup_node
      ~node
      client
  in
  check_balance ~receiver ~endpoint amount_mutez

let get_kernel_boot_wasm ~sc_rollup_client =
  let hooks : Process_hooks.t =
    let on_spawn _command _arguments = () in
    let on_log _output = Regression.capture "<boot.wasm>" in
    {on_spawn; on_log}
  in
  let*! kernel_boot_opt =
    Sc_rollup_client.inspect_durable_state_value
      sc_rollup_client
      ~hooks
      ~log_output:false
      ~pvm_kind:"wasm_2_0_0"
      ~operation:Sc_rollup_client.Value
      ~key:Durable_storage_path.kernel_boot_wasm
  in
  match kernel_boot_opt with
  | Some boot_wasm -> return boot_wasm
  | None -> failwith "Kernel `boot.wasm` should be accessible/readable."

let gen_test_kernel_upgrade ?evm_setup ?rollup_address ?(should_fail = false)
    ?(nonce = 2) ~base_installee ~installee ?with_administrator
    ?expect_l1_failure ?(admin = Constant.bootstrap1) ?(upgrador = admin)
    protocol =
  let* {
         node;
         client;
         sc_rollup_node;
         sc_rollup_client;
         sc_rollup_address;
         evm_proxy_server;
         l1_contracts;
         _;
       } =
    match evm_setup with
    | Some evm_setup -> return evm_setup
    | None -> setup_evm_kernel ?with_administrator ~admin:(Some admin) protocol
  in
  let l1_contracts =
    match l1_contracts with
    | Some x -> x
    | None -> Test.fail "The test requires the l1 contracts."
  in
  let sc_rollup_address =
    Option.value ~default:sc_rollup_address rollup_address
  in
  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let* _, preimage_root_hash_opt =
    Sc_rollup_helpers.prepare_installer_kernel_gen
      ~preimages_dir
      ~base_installee
      ~display_root_hash:true
      installee
  in
  let preimage_root_hash_bytes =
    match preimage_root_hash_opt with
    | Some preimage_root_hash -> Hex.to_string @@ `Hex preimage_root_hash
    | None ->
        failwith
          "Couldn't obtain the root hash of the preimages of the chunked \
           kernel."
  in
  let upgrade_nonce_bytes = Helpers.u16_to_bytes nonce in
  let upgrade_payload =
    upgrade_nonce_bytes ^ preimage_root_hash_bytes |> Hex.of_string |> Hex.show
  in
  let* kernel_boot_wasm_before_upgrade =
    get_kernel_boot_wasm ~sc_rollup_client
  in
  let* expected_kernel_boot_wasm =
    if should_fail then return kernel_boot_wasm_before_upgrade
    else
      return @@ Hex.show @@ Hex.of_string
      @@ read_file (project_root // base_installee // (installee ^ ".wasm"))
  in
  let* () =
    let* () =
      Client.transfer
        ?expect_failure:expect_l1_failure
        ~amount:Tez.zero
        ~giver:upgrador.public_key_hash
        ~receiver:l1_contracts.admin
        ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address upgrade_payload)
        ~burn_cap:Tez.one
        client
    in
    let* _ = next_evm_level ~sc_rollup_node ~node ~client in
    unit
  in
  let* kernel_boot_wasm_after_upgrade =
    get_kernel_boot_wasm ~sc_rollup_client
  in
  Check.((expected_kernel_boot_wasm = kernel_boot_wasm_after_upgrade) string)
    ~error_msg:(sf "Unexpected `boot.wasm`.") ;
  return
    ( sc_rollup_node,
      sc_rollup_client,
      node,
      client,
      evm_proxy_server,
      kernel_boot_wasm_before_upgrade )

let test_kernel_upgrade_to_debug =
  Protocol.register_test
    ~__FILE__
    ~tags:["debug"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade integrity to a debug kernel"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "debug_kernel" in
  let* _ = gen_test_kernel_upgrade ~base_installee ~installee protocol in
  unit

let test_kernel_upgrade_evm_to_evm =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade integrity to itself"
  @@ fun protocol ->
  let base_installee = "./" in
  let installee = "evm_kernel" in
  let* sc_rollup_node, _, node, client, evm_proxy_server, _ =
    gen_test_kernel_upgrade ~base_installee ~installee protocol
  in
  (* We ensure the upgrade went well by checking if the kernel still produces
     blocks. *)
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  check_block_progression
    ~sc_rollup_node
    ~node
    ~client
    ~endpoint
    ~expected_block_level:2

let test_kernel_upgrade_wrong_key =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade fails with a wrong administrator key"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "debug_kernel" in
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~should_fail:true
      ~base_installee
      ~installee
      ~admin:Constant.bootstrap1
      ~upgrador:Constant.bootstrap2
      protocol
  in
  unit

let test_kernel_upgrade_wrong_nonce =
  Protocol.register_test
    ~__FILE__
    ~tags:["nonce"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade fails with a wrong upgrade nonce"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "debug_kernel" in
  let* _ =
    gen_test_kernel_upgrade
      ~nonce:3
      ~should_fail:true
      ~base_installee
      ~installee
      protocol
  in
  unit

let test_kernel_upgrade_wrong_rollup_address =
  Protocol.register_test
    ~__FILE__
    ~tags:["address"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade fails with a wrong rollup address"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "debug_kernel" in
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~rollup_address:"sr1T13qeVewVm3tudQb8dwn8qRjptNo7KVkj"
      ~should_fail:true
      ~base_installee
      ~installee
      protocol
  in
  unit

let test_kernel_upgrade_no_administrator =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade fails if there is no administrator"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "debug_kernel" in
  let* _ =
    gen_test_kernel_upgrade
      ~should_fail:true
      ~base_installee
      ~installee
      ~with_administrator:false
      protocol
  in
  unit

let test_kernel_upgrade_failing_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["migration"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade rollback when migration fails"
  @@ fun protocol ->
  let base_installee = "src/kernel_evm/kernel/tests/resources" in
  let installee = "failed_migration" in
  let* ( sc_rollup_node,
         sc_rollup_client,
         node,
         client,
         evm_proxy_server,
         original_kernel_boot_wasm ) =
    gen_test_kernel_upgrade ~base_installee ~installee protocol
  in
  (* Fallback mechanism is triggered, no block is produced at that level. *)
  let* _ = next_evm_level ~sc_rollup_node ~node ~client in
  (* We make sure that we can't read under the tmp file, after migration failed,
     everything is reverted. *)
  let*! tmp_dummy =
    Sc_rollup_client.inspect_durable_state_value
      sc_rollup_client
      ~pvm_kind:"wasm_2_0_0"
      ~operation:Sc_rollup_client.Value
      ~key:"/tmp/__dummy"
  in
  (match tmp_dummy with
  | Some _ -> failwith "Nothing should be readable under the temporary dir."
  | None -> ()) ;
  let* kernel_after_migration_failed = get_kernel_boot_wasm ~sc_rollup_client in
  (* The upgrade succeeded, but the fallback mechanism was activated, so the kernel
     after the upgrade/migration is still the previous one. *)
  Check.((original_kernel_boot_wasm = kernel_after_migration_failed) string)
    ~error_msg:(sf "Unexpected `boot.wasm` after migration failed.") ;
  (* We ensure that the fallback mechanism went well by checking if the
     kernel still produces blocks since it has booted back to the previous,
     original kernel. *)
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  check_block_progression
    ~sc_rollup_node
    ~node
    ~client
    ~endpoint
    ~expected_block_level:2

let test_check_kernel_upgrade_nonce =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "nonce"]
    ~title:"Ensures EVM kernel's upgrade nonce is bumped"
  @@ fun protocol ->
  let base_installee = "./" in
  let installee = "evm_kernel" in
  let* _, _, _, _, evm_proxy_server, _ =
    gen_test_kernel_upgrade ~base_installee ~installee protocol
  in
  let* upgrade_nonce_result =
    Evm_proxy_server.call_evm_rpc
      evm_proxy_server
      Evm_proxy_server.{method_ = "tez_upgradeNonce"; parameters = `Null}
  in
  let upgrade_nonce =
    upgrade_nonce_result |> Evm_proxy_server.extract_result |> JSON.as_int32
  in
  Check.((upgrade_nonce = Int32.of_int 2) int32)
    ~error_msg:(sf "Expected upgrade nonce should be %%R, but got %%L") ;
  unit

let send_raw_transaction_request raw_tx =
  Evm_proxy_server.
    {method_ = "eth_sendRawTransaction"; parameters = `A [`String raw_tx]}

let send_raw_transaction proxy_server raw_tx =
  let* response =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (send_raw_transaction_request raw_tx)
  in
  let hash =
    response |> Evm_proxy_server.extract_result |> JSON.as_string_opt
  in
  let error_message =
    response |> Evm_proxy_server.extract_error_message |> JSON.as_string_opt
  in
  match (hash, error_message) with
  | Some hash, _ -> return (Ok hash)
  | _, Some error_code -> return (Error error_code)
  | _ -> failwith "invalid response from eth_sendRawTransaction"

let test_rpc_sendRawTransaction =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "tx_hash"]
    ~title:
      "Ensure EVM proxy returns appropriate hash for any given transactions."
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 5) in
  let* hashes =
    Lwt_list.map_p
      (fun (tx_raw, _) ->
        let* hash = send_raw_transaction evm_proxy_server tx_raw in
        let hash = Result.get_ok hash in
        return hash)
      txs
  in
  let expected_hashes =
    List.map (fun (_, expected_hash) -> expected_hash) txs
  in
  Check.((hashes = expected_hashes) (list string))
    ~error_msg:"Unexpected returned hash, should be %R, but got %L" ;
  unit

let by_block_arg_string by =
  match by with `Hash -> "Hash" | `Number -> "Number"

let get_transaction_by_block_arg_and_index_request ~by arg index =
  let by = by_block_arg_string by in
  Evm_proxy_server.
    {
      method_ = "eth_getTransactionByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_transaction_by_block_arg_and_index ~by proxy_server block_hash index =
  let* transaction_object =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (get_transaction_by_block_arg_and_index_request ~by block_hash index)
  in
  return
    JSON.(
      transaction_object |-> "result" |> Transaction.transaction_object_of_json)

let test_rpc_getTransactionByBlockArgAndIndex ~by protocol =
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  let* {evm_proxy_server; sc_rollup_node; node; client; _} =
    setup_past_genesis ~config ~admin:None protocol
  in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let* _, _, hashes =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_proxy_server
      (List.map fst txs)
  in
  Lwt_list.iter_s
    (fun transaction_hash ->
      let* receipt =
        wait_for_application
          ~sc_rollup_node
          ~node
          ~client
          (wait_for_transaction_receipt ~evm_proxy_server ~transaction_hash)
          ()
      in
      let block_arg, index =
        ( (match by with
          | `Hash -> receipt.blockHash
          | `Number -> Int32.to_string receipt.blockNumber),
          receipt.transactionIndex )
      in
      let* transaction_object =
        get_transaction_by_block_arg_and_index
          ~by
          evm_proxy_server
          block_arg
          (Int32.to_string index)
      in
      Check.(
        ((transaction_object.hash = transaction_hash) string)
          ~error_msg:"Incorrect transaction hash, should be %R, but got %L.") ;
      unit)
    hashes

let test_rpc_getTransactionByBlockHashAndIndex =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_transaction_by"; "block_hash_and_index"]
    ~title:"RPC method eth_getTransactionByBlockHashAndIndex"
  @@ test_rpc_getTransactionByBlockArgAndIndex ~by:`Hash

let test_rpc_getTransactionByBlockNumberAndIndex =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_transaction_by"; "block_number_and_index"]
    ~title:"RPC method eth_getTransactionByBlockNumberAndIndex"
  @@ test_rpc_getTransactionByBlockArgAndIndex ~by:`Number

type storage_migration_results = {
  transfer_result : transfer_result;
  block_result : Block.t;
  config_result : config_result;
}

(* This is the test generator that will trigger the sanity checks for migration
   tests.
   Note that:
   - it uses the latest version of the ghostnet EVM rollup as a starter kernel.
   - the upgrade of the kernel during the test will always target the latest one
     on master.
   - everytime a new path/rpc/object is stored in the kernel, a new sanity check
     MUST be generated. *)
let gen_kernel_migration_test ?(admin = Constant.bootstrap5) ~scenario_prior
    ~scenario_after protocol =
  let current_kernel_base_installee = "src/kernel_evm/kernel/tests/resources" in
  let current_kernel_installee = "ghostnet_evm_kernel" in
  let* evm_setup =
    setup_past_genesis
      ~kernel_installee:
        {
          base_installee = current_kernel_base_installee;
          installee = current_kernel_installee;
        }
      ~admin:(Some admin)
      protocol
  in
  (* Load the EVM rollup's storage and sanity check results. *)
  let* evm_proxy_server =
    Evm_proxy_server.init ~mode:`Production evm_setup.sc_rollup_node
  in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* sanity_check =
    scenario_prior ~evm_setup:{evm_setup with evm_proxy_server; endpoint}
  in
  (* Upgrade the kernel. *)
  let next_kernel_base_installee = "./" in
  let next_kernel_installee = "evm_kernel" in
  let* _ =
    gen_test_kernel_upgrade
      ~evm_setup
      ~base_installee:next_kernel_base_installee
      ~installee:next_kernel_installee
      ~admin
      protocol
  in
  let* _ =
    (* wait for the migration to be processed *)
    next_evm_level
      ~sc_rollup_node:evm_setup.sc_rollup_node
      ~node:evm_setup.node
      ~client:evm_setup.client
  in
  (* Check the values after the upgrade with [sanity_check] results. *)
  scenario_after ~evm_setup ~sanity_check

let test_kernel_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "upgrade"]
    ~title:"Ensures EVM kernel's upgrade succeed with potential migration(s)."
  @@ fun protocol ->
  let sender, receiver =
    (Eth_account.bootstrap_accounts.(0), Eth_account.bootstrap_accounts.(1))
  in
  let scenario_prior ~evm_setup =
    let* transfer_result =
      make_transfer
        ~value:Wei.(default_bootstrap_account_balance - one)
        ~sender
        ~receiver
        evm_setup
    in
    let* block_result = latest_block evm_setup in
    let* config_result = config_setup evm_setup in
    return {transfer_result; block_result; config_result}
  in
  let scenario_after ~evm_setup ~sanity_check =
    let* () =
      ensure_transfer_result_integrity
        ~sender
        ~receiver
        ~transfer_result:sanity_check.transfer_result
        evm_setup
    in
    let* () =
      ensure_block_integrity ~block_result:sanity_check.block_result evm_setup
    in
    ensure_config_setup_integrity
      ~config_result:sanity_check.config_result
      evm_setup
  in
  gen_kernel_migration_test ~scenario_prior ~scenario_after protocol

let test_deposit_dailynet =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "deposit"; "dailynet"]
    ~title:"deposit on dailynet"
  @@ fun protocol ->
  let bridge_address = "KT1QwBaLj5TRaGU3qkU4ZKKQ5mvNvyyzGBFv" in
  let exchanger_address = "KT1FHqsvc7vRS3u54L66DdMX4gb6QKqxJ1JW" in
  let rollup_address = "sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf" in

  let mockup_client = Client.create_with_mode Mockup in
  let make_bootstrap_contract ~address ~code ~storage ?typecheck () =
    let* code_json = Client.convert_script_to_json ~script:code mockup_client in
    let* storage_json =
      Client.convert_data_to_json ~data:storage ?typecheck mockup_client
    in
    let script : Ezjsonm.value =
      `O [("code", code_json); ("storage", storage_json)]
    in
    return
      Protocol.
        {delegate = None; amount = Tez.of_int 0; script; hash = Some address}
  in

  (* Creates the exchanger contract. *)
  let* exchanger_contract =
    make_bootstrap_contract
      ~address:exchanger_address
      ~code:(exchanger_path ())
      ~storage:"Unit"
      ()
  in
  (* Creates the bridge contract initialized with exchanger contract. *)
  let* bridge_contract =
    make_bootstrap_contract
      ~address:bridge_address
      ~code:(bridge_path ())
      ~storage:(sf "Pair %S None" exchanger_address)
      ()
  in

  (* Creates the EVM rollup that listens to the bootstrap smart contract exchanger. *)
  let* {
         bootstrap_smart_rollup = evm;
         smart_rollup_node_data_dir;
         smart_rollup_node_extra_args;
       } =
    setup_bootstrap_smart_rollup
      ~name:"evm"
      ~address:rollup_address
      ~parameters_ty:"pair (pair bytes (ticket unit)) (pair nat bytes)"
      ~base_installee:"./"
      ~installee:"evm_kernel"
      ~config:
        (`Path Base.(project_root // "src/kernel_evm/config/dailynet.yaml"))
      ()
  in

  (* Setup a chain where the EVM rollup, the exchanger contract and the bridge
     are all originated. *)
  let* node, client =
    setup_l1
      ~bootstrap_smart_rollups:[evm]
      ~bootstrap_contracts:[exchanger_contract; bridge_contract]
      protocol
  in

  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~data_dir:smart_rollup_node_data_dir
      ~base_dir:(Client.base_dir client)
      ~default_operator:Constant.bootstrap1.public_key_hash
  in
  let* () = Client.bake_for_and_wait client in

  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      rollup_address
      smart_rollup_node_extra_args
  in

  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  let endpoint = Evm_proxy_server.endpoint evm_proxy_server in

  (* Deposit tokens to the EVM rollup. *)
  let amount_mutez = Tez.of_mutez_int 100_000_000 in
  let receiver = "0x119811f34EF4491014Fbc3C969C426d37067D6A4" in

  let* () =
    deposit
      ~amount_mutez
      ~bridge:bridge_address
      ~depositor:Constant.bootstrap2
      ~receiver
      ~sc_rollup_node
      ~sc_rollup_address:rollup_address
      ~node
      client
  in

  (* Check the balance in the EVM rollup. *)
  check_balance ~receiver ~endpoint amount_mutez

let test_rpc_sendRawTransaction_nonce_too_low =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "nonce"]
    ~title:"Returns an error if the nonce is too low"
  @@ fun protocol ->
  let* {evm_proxy_server; sc_rollup_node; node; client; _} =
    setup_past_genesis ~admin:None protocol
  in
  (* Nonce: 0 *)
  let raw_tx =
    "0xf86c80825208831e8480940000000000000000000000000000000000000000888ac7230489e8000080820a96a038294f867266c767aee6c3b54a0c444368fb8d5e90353219bce1da78de16aea4a018a7d3c58ddb1f6b33bad5dde106843acfbd6467e5df181d22270229dcfdf601"
  in
  let* result = send_raw_transaction evm_proxy_server raw_tx in
  let transaction_hash = Result.get_ok result in
  let* _ =
    wait_for_application
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_proxy_server ~transaction_hash)
      ()
  in
  let* result = send_raw_transaction evm_proxy_server raw_tx in
  let error_message = Result.get_error result in
  Check.(
    ((error_message = "Nonce too low.") string)
      ~error_msg:"The transaction should fail") ;
  unit

let test_rpc_sendRawTransaction_nonce_too_high =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "nonce"]
    ~title:"Returns an error if the nonce is too high."
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  (* Nonce: 1 *)
  let raw_tx =
    "0xf86c01825208831e8480940000000000000000000000000000000000000000888ac7230489e8000080820a95a0a349864bedc9b84aea88cda197e96538c62c242286ead58eb7180a611f850237a01206525ff16ae5b708ee02b362f9b4d7565e0d7e9b4c536d7ef7dec81cda3ac7"
  in
  let* result = send_raw_transaction evm_proxy_server raw_tx in
  let error_message = Result.get_error result in
  Check.(
    ((error_message = "Nonce too high.") string)
      ~error_msg:"The transaction should fail") ;
  unit

let test_deposit_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "deposit"]
    ~title:"Deposit before and after migration"
  @@ fun protocol ->
  let admin = Constant.bootstrap5 in
  let receiver = "0x119811f34EF4491014Fbc3C969C426d37067D6A4" in
  let amount_mutez = Tez.of_mutez_int 50_000_000 in

  let scenario_prior
      ~evm_setup:
        {
          l1_contracts;
          sc_rollup_node;
          sc_rollup_address;
          node;
          client;
          endpoint;
          _;
        } =
    let {bridge; _} =
      match l1_contracts with Some x -> x | None -> assert false
    in
    let* () =
      deposit
        ~amount_mutez
        ~bridge
        ~depositor:admin
        ~receiver
        ~sc_rollup_node
        ~sc_rollup_address
        ~node
        client
    in
    check_balance ~receiver ~endpoint amount_mutez
  in
  let scenario_after
      ~evm_setup:
        {
          l1_contracts;
          sc_rollup_node;
          sc_rollup_address;
          node;
          client;
          endpoint;
          _;
        } ~sanity_check:_ =
    let {bridge; _} =
      match l1_contracts with Some x -> x | None -> assert false
    in
    let* () =
      deposit
        ~amount_mutez
        ~bridge
        ~depositor:admin
        ~receiver
        ~sc_rollup_node
        ~sc_rollup_address
        ~node
        client
    in
    check_balance ~receiver ~endpoint Tez.(amount_mutez + amount_mutez)
  in
  gen_kernel_migration_test ~admin ~scenario_prior ~scenario_after protocol

let test_block_storage_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "block"; "storage"]
    ~title:"Block storage before and after migration"
  @@ fun protocol ->
  let block_id = "1" in
  let scenario_prior ~evm_setup:{endpoint; _} =
    let* (block : Block.t) = Eth_cli.get_block ~block_id ~endpoint in
    return block
  in
  let scenario_after ~evm_setup:{endpoint; _} ~(sanity_check : Block.t) =
    let* (block : Block.t) = Eth_cli.get_block ~block_id ~endpoint in
    (* Compare fields stored before migration *)
    assert (block.number = sanity_check.number) ;
    assert (block.hash = sanity_check.hash) ;
    assert (block.timestamp = sanity_check.timestamp) ;
    assert (block.transactions = sanity_check.transactions) ;
    unit
  in
  gen_kernel_migration_test ~scenario_prior ~scenario_after protocol

let test_rpc_sendRawTransaction_invalid_chain_id =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "chain_id"]
    ~title:"Returns an error if the chainId is not correct."
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  (* Nonce: 0, chainId: 4242*)
  let raw_tx =
    "0xf86a8080831e8480940000000000000000000000000000000000000000888ac7230489e8000080822148a0e09f1fb4920f2e64a274b83d925890dd0b109fdf31f2811a781e918118daf34aa00f425e9a93bd92d710d3d323998b093a8c7d497d2af688c062a8099b076813e8"
  in
  let* result = send_raw_transaction evm_proxy_server raw_tx in
  let error_message = Result.get_error result in
  Check.(
    ((error_message = "Invalid chain id.") string)
      ~error_msg:"The transaction should fail") ;
  unit

let register_evm_migration ~protocols =
  test_kernel_migration protocols ;
  test_deposit_before_and_after_migration protocols ;
  test_block_storage_before_and_after_migration protocols

(* Flakyness: the rollup node batches the transactions before receiving all of them
   issue for the fix: #6438 *)
let test_reboot =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "reboot"; "loop"; Tag.flaky]
    ~title:"Check that the kernel can handle too many txs for a single run"
  @@ fun protocol ->
  let* {evm_proxy_server; sc_rollup_node; node; client; sc_rollup_client; _} =
    setup_past_genesis ~admin:None protocol
  in
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let transfers =
    read_file (kernel_inputs_path ^ "/100-loops-transfers")
    |> String.trim |> String.split_on_char '\n'
  in
  let txs =
    read_file (kernel_inputs_path ^ "/100-loops")
    |> String.trim |> String.split_on_char '\n'
  in
  let* _, _transfer_receipt, _ =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_proxy_server
      transfers
  in
  let* requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_proxy_server txs
  in
  let* block_with_many_txs =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A
              [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool false];
        })
  in
  let block_with_many_txs =
    block_with_many_txs |> Evm_proxy_server.extract_result |> Block.of_json
  in
  (match block_with_many_txs.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the latest block, got %L") ;
  let*! tick_number = Sc_rollup_client.ticks sc_rollup_client in
  let max_tick =
    Tezos_protocol_alpha.Protocol.Sc_rollup_wasm.V2_0_0.ticks_per_snapshot
  in
  Check.((tick_number > Z.to_int max_tick) int)
    ~error_msg:
      "Expected %L to be greater than %R, max nb of ticks per kernel run" ;
  (* a single run can't do more than 11_000_000_000 / 2000 gas units *)
  (* TODO: #6278
     RPC to fetch tick model / tick per gas *)
  Check.((block_with_many_txs.gasUsed > 5500000l) int32)
    ~error_msg:"Expected gas used %L to be superior to %R" ;
  unit

let block_transaction_count_by ~by arg =
  let method_ = "eth_getBlockTransactionCountBy" ^ by_block_arg_string by in
  Evm_proxy_server.{method_; parameters = `A [`String arg]}

let get_block_transaction_count_by proxy_server ~by arg =
  let* transaction_count =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (block_transaction_count_by ~by arg)
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

let test_rpc_getBlockTransactionCountBy =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_block_transaction_count_by"]
    ~title:
      "RPC methods eth_getBlockTransactionCountByHash and \
       eth_getBlockTransactionCountByNumber"
  @@ fun protocol ->
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  let* {evm_proxy_server; sc_rollup_node; node; client; _} =
    setup_past_genesis ~config ~admin:None protocol
  in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 5) in
  let* _, receipt, _ =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_proxy_server
      (List.map fst txs)
  in
  let* block =
    Eth_cli.get_block
      ~block_id:receipt.blockHash
      ~endpoint:evm_proxy_server_endpoint
  in
  let expected_count =
    match block.transactions with
    | Empty -> 0L
    | Hash l -> Int64.of_int @@ List.length l
    | Full l -> Int64.of_int @@ List.length l
  in
  let* transaction_count =
    get_block_transaction_count_by evm_proxy_server ~by:`Hash receipt.blockHash
  in
  Check.((transaction_count = expected_count) int64)
    ~error_msg:
      "Expected %R transactions with eth_getBlockTransactionCountByHash, but \
       got %L" ;
  let* transaction_count =
    get_block_transaction_count_by
      evm_proxy_server
      ~by:`Number
      (Int32.to_string receipt.blockNumber)
  in
  Check.((transaction_count = expected_count) int64)
    ~error_msg:
      "Expected %R transactions with eth_getBlockTransactionCountByNumber, but \
       got %L" ;
  unit

let uncle_count_by_block_arg_request ~by arg =
  let method_ = "eth_getUncleCountByBlock" ^ by_block_arg_string by in
  Evm_proxy_server.{method_; parameters = `A [`String arg]}

let get_uncle_count_by_block_arg proxy_server ~by arg =
  let* uncle_count =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (uncle_count_by_block_arg_request ~by arg)
  in
  return JSON.(uncle_count |-> "result" |> as_int64)

let test_rpc_getUncleCountByBlock =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_uncle_count_by_block"]
    ~title:
      "RPC methods eth_getUncleCountByBlockHash and \
       eth_getUncleCountByBlockNumber"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  let* uncle_count =
    get_uncle_count_by_block_arg
      evm_proxy_server
      ~by:`Hash
      (Option.get block.hash)
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockHash, but got %L" ;
  let* uncle_count =
    get_uncle_count_by_block_arg
      evm_proxy_server
      ~by:`Number
      (Int32.to_string block.number)
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockNumber, but got %L" ;
  unit

let uncle_by_block_arg_and_index_request ~by arg index =
  let by = by_block_arg_string by in
  Evm_proxy_server.
    {
      method_ = "eth_getUncleByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_uncle_by_block_arg_and_index ~by proxy_server arg index =
  let* block =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      (uncle_by_block_arg_and_index_request ~by arg index)
  in
  let result = JSON.(block |-> "result") in
  if JSON.is_null result then return None
  else return @@ Some (result |> Block.of_json)

let test_rpc_getUncleByBlockArgAndIndex =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_uncle_by_block_arg_and_index"]
    ~title:
      "RPC methods eth_getUncleByBlockHashAndIndex and \
       eth_getUncleByBlockNumberAndIndex"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let block_id = "0" in
  let* block =
    Eth_cli.get_block ~block_id ~endpoint:evm_proxy_server_endpoint
  in
  let* uncle =
    get_uncle_by_block_arg_and_index
      ~by:`Hash
      evm_proxy_server
      (Option.get block.hash)
      block_id
  in
  assert (Option.is_none uncle) ;
  let* uncle =
    get_uncle_by_block_arg_and_index
      ~by:`Number
      evm_proxy_server
      (Int32.to_string block.number)
      block_id
  in
  assert (Option.is_none uncle) ;
  unit

let test_simulation_eip2200 =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "loop"; "simulation"; "eip2200"]
    ~title:"Simulation is EIP2200 resilient"
  @@ fun protocol ->
  let* ({sc_rollup_node; node; client; endpoint; _} as full_evm_setup) =
    setup_past_genesis ~admin:None protocol
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* loop_address, _tx = deploy ~contract:loop ~sender full_evm_setup in
  (* If we support EIP-2200, the simulation gives an amount of gas
     insufficient for the execution. As we do the simulation with an
     enormous gas limit, we never trigger EIP-2200. *)
  let call =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:loop.label
      ~address:loop_address
      ~method_call:"loop(5)"
  in
  let* _tx = wait_for_application ~sc_rollup_node ~node ~client call () in
  unit

let test_cover_fees =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "validity"]
    ~title:"Transaction is invalid if sender cannot cover the fees"
  @@ fun protocol ->
  (* No bootstrap accounts, so no one has funds. *)
  let* {evm_proxy_server; endpoint; sc_rollup_node; node; client; _} =
    setup_past_genesis ~bootstrap_accounts:[||] ~admin:None protocol
  in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1).  We do not use eth-cli in
     this test because we want the results of the simulation. *)
  let raw_transfer =
    "0xf86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  (* This should fail when the tx-pool check this. *)
  let* hash_result = send_raw_transaction evm_proxy_server raw_transfer in
  let hash =
    match hash_result with
    | Ok hash -> hash
    | Error _ -> Test.fail "The proxy did not accept the transaction."
  in
  (* No receipt should be produced for an invalid transaction. *)
  let rec check_for_receipt blocks_left =
    if blocks_left = 0 then unit
    else
      let* receipt = Eth_cli.get_receipt ~endpoint ~tx:hash in
      match receipt with
      | Some _ -> Test.fail "An invalid transaction should not have a receipt."
      | None ->
          let* _ = next_evm_level ~sc_rollup_node ~node ~client in
          check_for_receipt (blocks_left - 1)
  in
  check_for_receipt 6

let test_rpc_gasPrice =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "gas_price"]
    ~title:"RPC methods eth_gasPrice"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis ~admin:None protocol in
  let expected_gas_price = 21_000l in
  let* gas_price =
    Evm_proxy_server.(
      let* price =
        call_evm_rpc
          evm_proxy_server
          {method_ = "eth_gasPrice"; parameters = `A []}
      in
      return JSON.(price |-> "result" |> as_int64))
  in
  Check.((gas_price = Int64.of_int32 expected_gas_price) int64)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let register_evm_proxy_server ~protocols =
  test_originate_evm_kernel protocols ;
  test_evm_proxy_server_connection protocols ;
  test_consistent_block_hashes protocols ;
  test_rpc_getBalance protocols ;
  test_rpc_getBlockByNumber protocols ;
  test_rpc_getBlockByHash protocols ;
  test_rpc_getTransactionCount protocols ;
  test_rpc_getTransactionCountBatch protocols ;
  test_rpc_batch protocols ;
  test_l2_block_size_non_zero protocols ;
  test_l2_blocks_progression protocols ;
  test_l2_transfer protocols ;
  test_chunked_transaction protocols ;
  test_rpc_txpool_content protocols ;
  test_rpc_web3_clientVersion protocols ;
  test_rpc_web3_sha3 protocols ;
  test_simulate protocols ;
  test_full_blocks protocols ;
  test_latest_block protocols ;
  test_eth_call_nullable_recipient protocols ;
  test_l2_deploy_simple_storage protocols ;
  test_l2_call_simple_storage protocols ;
  test_l2_deploy_erc20 protocols ;
  test_inject_100_transactions protocols ;
  test_eth_call_storage_contract_rollup_node protocols ;
  test_eth_call_storage_contract_proxy protocols ;
  test_eth_call_storage_contract_eth_cli protocols ;
  test_eth_call_large protocols ;
  test_preinitialized_evm_kernel protocols ;
  test_deposit protocols ;
  test_estimate_gas protocols ;
  test_estimate_gas_additionnal_field protocols ;
  test_kernel_upgrade_to_debug protocols ;
  test_kernel_upgrade_evm_to_evm protocols ;
  test_kernel_upgrade_wrong_key protocols ;
  test_kernel_upgrade_wrong_nonce protocols ;
  test_kernel_upgrade_wrong_rollup_address protocols ;
  test_kernel_upgrade_no_administrator protocols ;
  test_kernel_upgrade_failing_migration protocols ;
  test_check_kernel_upgrade_nonce protocols ;
  test_rpc_sendRawTransaction protocols ;
  test_deposit_dailynet protocols ;
  test_rpc_sendRawTransaction_nonce_too_low protocols ;
  test_rpc_sendRawTransaction_nonce_too_high protocols ;
  test_rpc_sendRawTransaction_invalid_chain_id protocols ;
  test_rpc_getTransactionByBlockHashAndIndex protocols ;
  test_rpc_getTransactionByBlockNumberAndIndex protocols ;
  test_reboot protocols ;
  test_rpc_getBlockTransactionCountBy protocols ;
  test_rpc_getUncleCountByBlock protocols ;
  test_rpc_getUncleByBlockArgAndIndex protocols ;
  test_simulation_eip2200 protocols ;
  test_cover_fees protocols ;
  test_rpc_gasPrice protocols

let register ~protocols =
  register_evm_proxy_server ~protocols ;
  register_evm_migration ~protocols
