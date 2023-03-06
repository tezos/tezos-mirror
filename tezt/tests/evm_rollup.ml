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

let get_transaction_count proxy_server address =
  let parameters : JSON.u = `A [`String address; `String "latest"] in
  let* transaction_count =
    Evm_proxy_server.call_evm_rpc
      proxy_server
      ~method_:"eth_getTransactionCount"
      ~parameters
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

module Account = struct
  type t = {address : string; private_key : string}

  let accounts =
    [|
      {
        address = "0x6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c";
        private_key =
          "0x9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0";
      };
      {
        address = "0xB53dc01974176E5dFf2298C5a94343c2585E3c54";
        private_key =
          "0x3a6a6ca30c1ef1ce605a63a7a1a4ff4c689f8414ca0838bca29423f0ec280ff5";
      };
      {
        address = "0x9b49c988b5817Be31DfB00F7a5a4671772dCce2B";
        private_key =
          "0x0eb9bfa77d6cd145cdc0e3d6f902ee1464aeb5f62b02e38f111c9b60cd3adab5";
      };
    |]

  (** Prefunded account public key in the kernel, has a balance of 9999.

      TODO: https://gitlab.com/tezos/tezos/-/issues/5071
  *)
  let prefunded_account_address = accounts.(0).address
end

(** [next_evm_level ~sc_rollup_node ~node ~client] moves [sc_rollup_node] to
    the [node]'s next level. *)
let next_evm_level ~sc_rollup_node ~node ~client =
  let* () = Client.bake_for_and_wait client in
  Sc_rollup_node.wait_for_level
    ~timeout:30.
    sc_rollup_node
    (Node.get_level node)

(** [next_evm_until ?max_next_level ~stop_condition ~sc_rollup_node ~node client]
    calls {!next_evm_level} until [stop_condition] returns true.

    [max_next_level] limits the number of calls to {!next_evm_level},
    defaults to [10].
*)
let rec next_evm_until ?(max_next_level = 10) ~stop_condition ~sc_rollup_node
    ~node client =
  let* stop_condition_ok = stop_condition () in
  if stop_condition_ok then unit
  else if max_next_level = 0 then
    Test.fail "[next_evm_until] is not allowed to move to next level again"
  else
    let* _level = next_evm_level ~sc_rollup_node ~node ~client in
    next_evm_until
      ~max_next_level:(max_next_level - 1)
      ~stop_condition
      ~sc_rollup_node
      ~node
      client

let wait_until_tx_included ~evm_proxy_server_endpoint ~sc_rollup_node ~node
    ~tx_hash client =
  let stop_condition () =
    let endpoint = evm_proxy_server_endpoint in
    let* current = Eth_cli.block_number ~endpoint in
    let* {transactions; _} =
      Eth_cli.get_block ~block_id:(string_of_int current) ~endpoint
    in
    return (List.exists (String.equal tx_hash) transactions)
  in
  next_evm_until ~stop_condition ~sc_rollup_node ~node client

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
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup_address
  in
  let* () = Sc_rollup_node.run sc_rollup_node [] in
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
  let* _filename = Sc_rollup_node.config_init sc_rollup_node sc_rollup in
  let* _ = Sc_rollup_node.run sc_rollup_node [] in
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
  let eth_accounts_key = "eth_accounts" in
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
      eth_accounts_key
      storage_root_keys
      ~error_msg:"Expected %L to be initialized by the EVM kernel.") ;
  unit

let test_rpc_getBalance =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_balance"]
    ~title:"RPC method eth_getBalance"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; evm_proxy_server; _} =
    setup_evm_kernel protocol
  in
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* balance =
    Eth_cli.balance
      ~account:Account.prefunded_account_address
      ~endpoint:evm_proxy_server_endpoint
  in
  Check.((balance = 9999) int)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Account.prefunded_account_address) ;
  unit

let test_rpc_sendRawTransaction =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "send_raw_transaction"]
    ~title:"RPC method eth_sendRawTransaction"
  @@ fun protocol ->
  let* {node; client; evm_proxy_server; sc_rollup_node; sc_rollup_client; _} =
    setup_evm_kernel protocol
  in
  (* [Eth_cli.transaction_send] implicitly calls `eth_blockNumber` at some point.
     We thus need to at least go the first evm run level for the kernel to be able
     to read at current block's number path, otherwise the test will fail. *)
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* tx_hash =
    Eth_cli.transaction_send
      ~source_private_key:Account.accounts.(0).private_key
      ~to_public_key:Account.accounts.(1).address
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5024
            Introduce a eth/wei module. *)
      ~value:Z.(of_int 42 * (of_int 10 ** 18))
      ~endpoint:evm_proxy_server_endpoint
  in
  Log.info "Sent %s to the proxy server." tx_hash ;
  let*! batcher_queue = Sc_rollup_client.batcher_queue sc_rollup_client in
  let () =
    match batcher_queue with
    | [(_hash, _binary_msg)] -> ()
    | _ ->
        Test.fail
          ~__LOC__
          "Expected exactly one element to the batcher queue, got %d"
          (List.length batcher_queue)
  in
  let* () =
    wait_until_tx_included
      ~evm_proxy_server_endpoint
      ~sc_rollup_node
      ~tx_hash
      ~node
      client
  in
  unit

let test_rpc_getBlockByNumber =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_block_by_number"]
    ~title:"RPC method eth_getBlockByNumber"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; _} = setup_evm_kernel protocol in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* () = Client.bake_for_and_wait client in
  let first_evm_run_level = Node.get_level node in
  let* _level =
    Sc_rollup_node.wait_for_level
      ~timeout:30.
      sc_rollup_node
      first_evm_run_level
  in
  let* block =
    Eth_cli.get_block ~block_id:"0" ~endpoint:evm_proxy_server_endpoint
  in
  (* For our needs, we just test these two relevant fields for now: *)
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  Check.(block.transactions = [])
    (Check.list Check.string)
    ~error_msg:"Unexpected list of transactions, should be %%R, but got %%L" ;
  unit

let test_rpc_getTransactionCount =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "get_transaction_count"]
    ~title:"RPC method eth_getTransactionCount"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; _} = setup_evm_kernel protocol in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  (* Force a level to got past the genesis block *)
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let* transaction_count =
    get_transaction_count evm_proxy_server Account.prefunded_account_address
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
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
  let* () = check_block_progression ~expected_block_level:0 in
  let* () = check_block_progression ~expected_block_level:1 in
  unit

let test_l2_transfer =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_transfer"]
    ~title:"Check L2 transfers are applied"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; _} = setup_evm_kernel protocol in
  let* evm_proxy_server = Evm_proxy_server.init sc_rollup_node in
  let evm_proxy_server_endpoint = Evm_proxy_server.endpoint evm_proxy_server in
  let* _level = next_evm_level ~sc_rollup_node ~node ~client in
  let balance account =
    Eth_cli.balance ~account ~endpoint:evm_proxy_server_endpoint
  in
  let sender, receiver = (Account.accounts.(0), Account.accounts.(1)) in
  let* sender_balance = balance sender.address in
  let* receiver_balance = balance receiver.address in
  let* sender_nonce = get_transaction_count evm_proxy_server sender.address in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5024
      Introduce a eth/wei module. *)
  let eth_amount = sender_balance / 2 in
  (* We always send less than the balance, to ensure it always works. *)
  let amount = Z.(of_int eth_amount * (of_int 10 ** 18)) in
  let* tx_hash =
    Eth_cli.transaction_send
      ~source_private_key:sender.private_key
      ~to_public_key:receiver.address
      ~value:amount
      ~endpoint:evm_proxy_server_endpoint
  in
  (* Wait for the transaction to be included in a block. *)
  let* () =
    wait_until_tx_included
      ~evm_proxy_server_endpoint
      ~sc_rollup_node
      ~tx_hash
      ~node
      client
  in
  let* new_sender_balance = balance sender.address in
  let* new_receiver_balance = balance receiver.address in
  let* new_sender_nonce =
    get_transaction_count evm_proxy_server sender.address
  in
  Check.((new_sender_balance = sender_balance - eth_amount) int)
    ~error_msg:
      "Unexpected sender balance after transfer, should be %R, but got %L" ;
  Check.((new_receiver_balance = receiver_balance + eth_amount) int)
    ~error_msg:
      "Unexpected receiver balance after transfer, should be %R, but got %L" ;
  Check.((new_sender_nonce = Int64.succ sender_nonce) int64)
    ~error_msg:
      "Unexpected sender nonce after transfer, should be %R, but got %L" ;
  unit

let register_evm_proxy_server ~protocols =
  test_originate_evm_kernel protocols ;
  test_evm_proxy_server_connection protocols ;
  test_rpc_getBalance protocols ;
  test_rpc_sendRawTransaction protocols ;
  test_rpc_getBlockByNumber protocols ;
  test_rpc_getTransactionCount protocols ;
  test_l2_blocks_progression protocols ;
  test_l2_transfer protocols

let register ~protocols = register_evm_proxy_server ~protocols
