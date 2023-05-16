(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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
   Requirement:  make -f kernels.mk build-kernels
                 npm install eth-cli
   Invocation:   dune exec tezt/tests/main.exe -- --file evm_rollup.ml
*)

open Sc_rollup_helpers

let pvm_kind = "wasm_2_0_0"

let kernel_inputs_path = "tezt/tests/evm_kernel_inputs"

type full_evm_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_client : Sc_rollup_client.t;
  sc_rollup_address : string;
  originator_key : string;
  rollup_operator_key : string;
  evm_proxy_server : Evm_proxy_server.t;
}

let hex_encode (input : string) : string =
  match Hex.of_string input with `Hex s -> s

let evm_proxy_server_version proxy_server =
  let endpoint = Evm_proxy_server.endpoint proxy_server in
  let get_version_url = endpoint ^ "/version" in
  RPC.Curl.get get_version_url

(** [next_evm_level ~sc_rollup_node ~node ~client] moves [sc_rollup_node] to
    the [node]'s next level. *)
let next_evm_level ~sc_rollup_node ~node ~client =
  let* () = Client.bake_for_and_wait client in
  Sc_rollup_node.wait_for_level
    ~timeout:30.
    sc_rollup_node
    (Node.get_level node)

(** [wait_for_application ~sc_rollup_node ~node ~client apply ()] tries to
[apply] an operation and in parallel moves [sc_rollup_node] to the [node]'s next
level until either the operation succeeded (in which case it stops) or a given
number of level has passed (in which case it fails). *)
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

let setup_evm_kernel ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(rollup_operator_key = Constant.bootstrap1.public_key_hash) protocol =
  let* node, client = setup_l1 protocol in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:rollup_operator_key
  in
  (* Start a rollup node *)
  let* boot_sector =
    prepare_installer_kernel
      ~base_installee:"./"
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      "evm_kernel"
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      ~src:originator_key
      client
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  (* EVM Kernel installation level. *)
  let* () = Client.bake_for_and_wait client in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      (Node.get_level node)
  in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
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
    }

let setup_past_genesis ?originator_key ?rollup_operator_key protocol =
  let* ({node; client; sc_rollup_node; _} as full_setup) =
    setup_evm_kernel ?originator_key ?rollup_operator_key protocol
  in
  (* Force a level to got past the genesis block *)
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  return full_setup

let setup_mockup () =
  let evm_proxy_server = Evm_proxy_server.mockup () in
  let* () = Evm_proxy_server.run evm_proxy_server in
  return evm_proxy_server

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
      ~protocol
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
    setup_evm_kernel protocol
  in
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* () = Client.bake_for_and_wait client in
  let first_evm_run_level = Node.get_level node in
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
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* balance =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:evm_proxy_server_endpoint
  in
  Check.((balance = Wei.of_eth_int 9999) Wei.typ)
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
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  (* For our needs, we just test these two relevant fields for now: *)
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  let expected_transactions =
    Array.(
      map
        (fun accounts -> accounts.Eth_account.genesis_mint_tx)
        Eth_account.bootstrap_accounts
      |> to_list)
  in
  let transactions =
    match block.transactions with
    | Empty -> Test.fail "Genesis block shouldn't be empty"
    | Hash l -> l
    | Full _ -> Test.fail "Expected only transaction hashes"
  in
  Check.(transactions = expected_transactions)
    (Check.list Check.string)
    ~error_msg:"Unexpected list of transactions, should be %%R, but got %%L" ;
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
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
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
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
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
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
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
  let* {node; client; sc_rollup_node; _} = setup_evm_kernel protocol in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let check_block_progression ~expected_block_level =
    let* _level = next_evm_level ~sc_rollup_node ~node ~client in
    let* block_number =
      Eth_cli.block_number ~endpoint:evm_proxy_server_endpoint
    in
    return
    @@ Check.((block_number = expected_block_level) int)
         ~error_msg:"Unexpected block number, should be %%R, but got %%L"
  in
  let* () = check_block_progression ~expected_block_level:2 in
  let* () = check_block_progression ~expected_block_level:3 in
  unit

let test_l2_deploy =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"]
    ~title:"Check L2 contract deployment"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; _} = setup_evm_kernel protocol in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* () =
    Eth_cli.add_abi
      ~label:"simpleStorage"
      ~abi:(kernel_inputs_path ^ "/storage_abi.json")
      ()
  in
  let contract_compiled_file = kernel_inputs_path ^ "/storage.bin" in
  let send_deploy () =
    Eth_cli.deploy
      ~source_private_key:sender.private_key
      ~endpoint:evm_proxy_server_endpoint
      ~abi:"simpleStorage"
      ~bin:contract_compiled_file
  in
  let* contract_address, tx_hash =
    wait_for_application ~sc_rollup_node ~node ~client send_deploy ()
  in
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
  let* tx_object =
    Eth_cli.transaction_get ~endpoint:evm_proxy_server_endpoint ~tx_hash
  in
  (match tx_object with
  | Some tx_object ->
      Check.((tx_object.to_ = None) (option string))
        ~error_msg:
          "The transaction object of a contract creation should not have the \
           [to] field present"
  | None -> Test.fail "The transaction object of %s should be available" tx_hash) ;
  unit

let transfer ?data protocol =
  let* {node; client; sc_rollup_node; evm_proxy_server; _} =
    setup_past_genesis protocol
  in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let balance account =
    Eth_cli.balance ~account ~endpoint:evm_proxy_server_endpoint
  in
  let sender, receiver =
    (Eth_account.bootstrap_accounts.(0), Eth_account.bootstrap_accounts.(1))
  in
  let* sender_balance = balance sender.address in
  let* receiver_balance = balance receiver.address in
  let* sender_nonce = get_transaction_count evm_proxy_server sender.address in
  (* We always send less than the balance, to ensure it always works. *)
  let value = Wei.(sender_balance - one) in
  let* tx_hash =
    send_and_wait_until_tx_mined
      ~sc_rollup_node
      ~node
      ~client
      ~source_private_key:sender.private_key
      ~to_public_key:receiver.address
      ~value
      ?data
      ~evm_proxy_server_endpoint
      ()
  in
  let* new_sender_balance = balance sender.address in
  let* new_receiver_balance = balance receiver.address in
  let* new_sender_nonce =
    get_transaction_count evm_proxy_server sender.address
  in
  Check.(Wei.(new_sender_balance = sender_balance - value) Wei.typ)
    ~error_msg:
      "Unexpected sender balance after transfer, should be %R, but got %L" ;
  Check.(Wei.(new_receiver_balance = receiver_balance + value) Wei.typ)
    ~error_msg:
      "Unexpected receiver balance after transfer, should be %R, but got %L" ;
  Check.((new_sender_nonce = Int64.succ sender_nonce) int64)
    ~error_msg:
      "Unexpected sender nonce after transfer, should be %R, but got %L" ;
  (* Perform some sanity checks on the transaction object produced by the
     kernel. *)
  let* tx_object =
    Eth_cli.transaction_get ~endpoint:evm_proxy_server_endpoint ~tx_hash
  in
  let tx_object =
    match tx_object with
    | Some tx_object -> tx_object
    | None ->
        Test.fail "The transaction object of %s should be available" tx_hash
  in
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

let test_simulate =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "simulate"]
    ~title:"A block can be simulated in the rollup node"
    (fun protocol ->
      let* {evm_proxy_server; sc_rollup_client; _} =
        setup_past_genesis protocol
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

let test_full_blocks =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "full_blocks"]
    ~title:
      "Check `evm_getBlockByNumber` with full blocks returns the correct \
       informations"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
  let* genesis_block =
    Evm_proxy_server.(
      call_evm_rpc
        evm_proxy_server
        {
          method_ = "eth_getBlockByNumber";
          parameters = `A [`String "0x0"; `Bool true];
        })
  in
  let block =
    genesis_block |> Evm_proxy_server.extract_result |> Block.of_json
  in
  let transactions =
    match block.Block.transactions with
    | Empty -> Test.fail "Genesis block contains at least 3 transactions."
    | Hash _ -> Test.fail "Expected full transactions, got hashes"
    | Full txs -> txs
  in
  List.iter
    (fun tx ->
      let index = tx.Transaction.transactionIndex |> Int32.to_int in
      Check.(
        (tx.Transaction.hash
       = Eth_account.bootstrap_accounts.(index).genesis_mint_tx)
          string)
        ~error_msg:"Expected transaction hash %R, but got %L")
    transactions ;
  unit

let test_latest_block =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "blocks"; "latest"]
    ~title:
      "Check `evm_getBlockByNumber` works correctly when asking for the \
       `latest`"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
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
  Check.((latest_block.Block.number = 2l) int32)
    ~error_msg:"Expected latest being block number %R, but got %L" ;
  unit

let test_eth_call_nullable_recipient =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "eth_call"; "null"]
    ~title:"Check `eth_call.to` input can be null"
  @@ fun protocol ->
  let* {evm_proxy_server; _} = setup_past_genesis protocol in
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

let register_evm_proxy_server ~protocols =
  test_originate_evm_kernel protocols ;
  test_evm_proxy_server_connection protocols ;
  test_rpc_getBalance protocols ;
  test_rpc_getBlockByNumber protocols ;
  test_rpc_getTransactionCount protocols ;
  test_rpc_getTransactionCountBatch protocols ;
  test_rpc_batch protocols ;
  test_l2_blocks_progression protocols ;
  test_l2_transfer protocols ;
  test_chunked_transaction protocols ;
  test_l2_deploy protocols ;
  test_rpc_txpool_content protocols ;
  test_rpc_web3_clientVersion protocols ;
  test_simulate protocols ;
  test_full_blocks protocols ;
  test_latest_block protocols ;
  test_eth_call_nullable_recipient protocols

let register ~protocols = register_evm_proxy_server ~protocols
