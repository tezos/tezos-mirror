(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: EVM Kernel
   Requirement:  make -f kernels.mk build
                 npm install eth-cli
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_rollup.ml
*)
open Sc_rollup_helpers
open Helpers
open Rpc.Syntax
open Contract_path

let pvm_kind = "wasm_2_0_0"

let kernel_inputs_path = "etherlink/tezt/tests/evm_kernel_inputs"

type l1_contracts = {exchanger : string; bridge : string; admin : string}

type full_evm_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_address : string;
  originator_key : string;
  rollup_operator_key : string;
  evm_node : Evm_node.t;
  endpoint : string;
  l1_contracts : l1_contracts option;
  config :
    [ `Config of Installer_kernel_config.t
    | `Path of string
    | `Both of Installer_kernel_config.instr list * string ]
    option;
}

let hex_256_of n = Printf.sprintf "%064x" n

let hex_256_of_address acc =
  let s = acc.Eth_account.address in
  (* strip 0x and convert to lowercase *)
  let n = String.length s in
  let s = String.lowercase_ascii @@ String.sub s 2 (n - 2) in
  (* prepend 24 leading zeros *)
  String.("0x" ^ make 24 '0' ^ s)

let expected_gas_fees ~gas_price ~gas_used =
  let open Wei in
  let gas_price = gas_price |> Z.of_int32 |> Wei.to_wei_z in
  let gas_used = gas_used |> Z.of_int32 in
  gas_price * gas_used

let evm_node_version evm_node =
  let endpoint = Evm_node.endpoint evm_node in
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

(* Check simple transfer base fee is correct

   We apply a flat base fee to every tx - which is paid through an
   increase in estimate gas used at the given price.
*)
let check_tx_gas_for_flat_fee ~flat_fee ~gas_price ~gas_used =
  let expected_gas_used = 21000l in
  let gas_for_flat_fee = Z.of_int32 (Int32.sub gas_used expected_gas_used) in
  let gas_price = gas_price |> Z.of_int32 in
  (* integer division truncates - so we make sure to take ceil(a/b) not floor(a/b).
     otherwise we'd end up with very slightly less gas than needed to cover the
     flat fee. *)
  let actual_gas_for_flat_fee = Wei.cdiv flat_fee gas_price in
  Check.((actual_gas_for_flat_fee = Wei.to_wei_z gas_for_flat_fee) Wei.typ)
    ~error_msg:"Unexpected flat fee"

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
let get_value_in_storage sc_rollup_node address nth =
  Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
  @@ Sc_rollup_rpc.get_global_block_durable_state_value
       ~pvm_kind
       ~operation:Sc_rollup_rpc.Value
       ~key:(Durable_storage_path.storage address ~key:(hex_256_of nth) ())
       ()

let check_str_in_storage ~evm_setup ~address ~nth ~expected =
  let* value = get_value_in_storage evm_setup.sc_rollup_node address nth in
  Check.((value = Some expected) (option string))
    ~error_msg:"Unexpected value in storage, should be %R, but got %L" ;
  unit

let check_nb_in_storage ~evm_setup ~address ~nth ~expected =
  check_str_in_storage ~evm_setup ~address ~nth ~expected:(hex_256_of expected)

let get_storage_size sc_rollup_node ~address =
  let* storage =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:(Durable_storage_path.storage address ())
         ()
  in
  return (List.length storage)

let check_storage_size sc_rollup_node ~address size =
  (* check storage size *)
  let* storage_size = get_storage_size sc_rollup_node ~address in
  Check.((storage_size = size) int)
    ~error_msg:"Unexpected storage size, should be %R, but is %L" ;
  unit

(** [wait_for_transaction_receipt ~evm_node ~transaction_hash] takes an
    transaction_hash and returns only when the receipt is non null, or [count]
    blocks have passed and the receipt is still not available. *)
let wait_for_transaction_receipt ?(count = 3) ~evm_node ~transaction_hash () =
  let rec loop count =
    let* () = Lwt_unix.sleep 5. in
    let* receipt =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_getTransactionReceipt";
            parameters = `A [`String transaction_hash];
          })
    in
    if receipt |> Evm_node.extract_result |> JSON.is_null then
      if count > 0 then loop (count - 1)
      else Test.fail "Transaction still hasn't been included"
    else
      receipt |> Evm_node.extract_result
      |> Transaction.transaction_receipt_of_json |> return
  in
  loop count

let wait_for_application ~evm_node ~sc_rollup_node ~node ~client apply () =
  let* start_level = Client.level client in
  let max_iteration = 10 in
  let application_result = apply () in
  let rec loop () =
    let* () = Lwt_unix.sleep 5. in
    let* new_level =
      Helpers.next_evm_level ~evm_node ~sc_rollup_node ~node ~client
    in
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

(* sending more than ~300 tx could fail, because or curl *)
let send_n_transactions ~sc_rollup_node ~node ~client ~evm_node ?wait_for_blocks
    txs =
  let requests =
    List.map
      (fun tx ->
        Evm_node.
          {method_ = "eth_sendRawTransaction"; parameters = `A [`String tx]})
      txs
  in
  let* hashes = Evm_node.batch_evm_rpc evm_node requests in
  let hashes =
    hashes |> JSON.as_list
    |> List.map (fun json -> Evm_node.extract_result json |> JSON.as_string)
  in
  let first_hash = List.hd hashes in
  (* Let's wait until one of the transactions is injected into a block, and
      test this block contains the `n` transactions as expected. *)
  let* receipt =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt
         ?count:wait_for_blocks
         ~evm_node
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
  let* () = Client.bake_for_and_wait ~keys:[] client in

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
  let* () = Client.bake_for_and_wait ~keys:[] client in

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
  let* () = Client.bake_for_and_wait ~keys:[] client in

  return {exchanger; bridge; admin}

type setup_mode =
  | Setup_sequencer of {
      time_between_blocks : Evm_node.time_between_blocks option;
      sequencer : Account.key;
    }
  | Setup_proxy of {devmode : bool}

let setup_evm_kernel ?config ?(kernel_installee = Constant.WASM.evm_kernel)
    ?(originator_key = Constant.bootstrap1.public_key_hash)
    ?(rollup_operator_key = Constant.bootstrap1.public_key_hash)
    ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    ?(with_administrator = true) ?flat_fee ~admin ?commitment_period
    ?challenge_window ?timestamp ?(setup_mode = Setup_proxy {devmode = true})
    protocol =
  let* node, client =
    setup_l1 ?commitment_period ?challenge_window ?timestamp protocol
  in
  let* l1_contracts =
    match admin with
    | Some admin ->
        let* res = setup_l1_contracts ~admin client in
        return (Some res)
    | None -> return None
  in
  (* If a L1 bridge was set up, we make the kernel aware of the address. *)
  let base_config =
    let ticketer = Option.map (fun {exchanger; _} -> exchanger) l1_contracts in
    let administrator =
      if with_administrator then
        Option.map (fun {admin; _} -> admin) l1_contracts
      else None
    in
    let sequencer =
      match setup_mode with
      | Setup_proxy _ -> None
      | Setup_sequencer {sequencer; _} -> Some sequencer.public_key
    in
    Configuration.make_config
      ~bootstrap_accounts
      ?flat_fee
      ?ticketer
      ?administrator
      ?sequencer
      ()
  in
  let config =
    match (config, base_config) with
    | Some (`Config config), Some (`Config base) ->
        Some (`Config (base @ config))
    | Some (`Path path), Some (`Config base) -> Some (`Both (base, path))
    | None, _ -> base_config
    | Some (`Config config), None -> Some (`Config config)
    | Some (`Path path), None -> Some (`Path path)
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~default_operator:rollup_operator_key
  in
  (* Start a rollup node *)
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* {output; _} =
    prepare_installer_kernel ~preimages_dir ?config kernel_installee
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:pvm_kind
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:evm_type
      ~src:originator_key
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  (* EVM Kernel installation level. *)
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* level = Node.get_level node in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
  let* mode =
    match setup_mode with
    | Setup_proxy {devmode} -> return (Evm_node.Proxy {devmode})
    | Setup_sequencer {time_between_blocks; sequencer} ->
        let private_rpc_port = Port.fresh () in
        let sequencer =
          match sequencer.secret_key with
          | Unencrypted sk -> sk
          | Encrypted _ ->
              Test.fail "Provide an unencrypted key for the sequencer"
        in
        return
          (Evm_node.Sequencer
             {
               initial_kernel = output;
               preimage_dir = preimages_dir;
               private_rpc_port;
               time_between_blocks;
               sequencer;
               genesis_timestamp = None;
             })
  in
  let* evm_node =
    Evm_node.init ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let endpoint = Evm_node.endpoint evm_node in
  return
    {
      node;
      client;
      sc_rollup_node;
      sc_rollup_address;
      originator_key;
      rollup_operator_key;
      evm_node;
      endpoint;
      l1_contracts;
      config;
    }

let register_test ?config ~title ~tags ?(admin = None) ?uses ?commitment_period
    ?challenge_window ?bootstrap_accounts ?flat_fee ~setup_mode f =
  let extra_tag =
    match setup_mode with
    | Setup_proxy _ -> "proxy"
    | Setup_sequencer _ -> "sequencer"
  in
  let uses =
    Option.value
      ~default:(fun _protocol ->
        [
          Constant.octez_smart_rollup_node;
          Constant.octez_evm_node;
          Constant.smart_rollup_installer;
          Constant.WASM.evm_kernel;
        ])
      uses
  in
  Protocol.register_test
    ~__FILE__
    ~tags:(extra_tag :: tags)
    ~uses
    ~title:(sf "%s (%s)" title extra_tag)
    (fun protocol ->
      let* evm_setup =
        setup_evm_kernel
          ?config
          ?commitment_period
          ?challenge_window
          ?bootstrap_accounts
          ?flat_fee
          ~admin
          ~setup_mode
          protocol
      in
      f ~protocol ~evm_setup)

let register_proxy ?config ~title ~tags ?uses ?admin ?commitment_period
    ?challenge_window ?bootstrap_accounts f protocols =
  register_test
    ?config
    ~title
    ~tags
    ?uses
    ?admin
    ?commitment_period
    ?challenge_window
    ?bootstrap_accounts
    f
    protocols
    ~setup_mode:(Setup_proxy {devmode = true})

let register_both ~title ~tags ?uses ?admin ?commitment_period ?challenge_window
    ?bootstrap_accounts ?flat_fee ?config ?time_between_blocks f protocols =
  let register =
    register_test
      ?config
      ~title
      ~tags
      ?uses
      ?admin
      ?commitment_period
      ?challenge_window
      ?bootstrap_accounts
      ?flat_fee
      f
      protocols
  in
  register ~setup_mode:(Setup_proxy {devmode = true}) ;
  register
    ~setup_mode:
      (Setup_sequencer {time_between_blocks; sequencer = Constant.bootstrap1})

type contract = {label : string; abi : string; bin : string}

let deploy ~contract ~sender full_evm_setup =
  let {node; client; sc_rollup_node; evm_node; _} = full_evm_setup in
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* () = Eth_cli.add_abi ~label:contract.label ~abi:contract.abi () in
  let send_deploy () =
    Eth_cli.deploy
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint:evm_node_endpoint
      ~abi:contract.label
      ~bin:contract.bin
  in
  wait_for_application ~evm_node ~sc_rollup_node ~node ~client send_deploy ()

type deploy_checks = {
  contract : contract;
  expected_address : string;
  expected_code : string;
}

let deploy_with_base_checks {contract; expected_address; expected_code}
    full_evm_setup =
  let {sc_rollup_node; evm_node; _} = full_evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address, tx = deploy ~contract ~sender full_evm_setup in
  let address = String.lowercase_ascii contract_address in
  Check.(
    (address = expected_address)
      string
      ~error_msg:"Expected address to be %R but was %L.") ;
  let* code_in_kernel =
    Evm_node.fetch_contract_code evm_node contract_address
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
  let* accounts =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:Durable_storage_path.eth_accounts
         ()
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

let send ~sender ~receiver ~value ?data full_evm_setup =
  let {node; client; sc_rollup_node; evm_node; _} = full_evm_setup in
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let send =
    Eth_cli.transaction_send
      ~source_private_key:sender.Eth_account.private_key
      ~to_public_key:receiver.Eth_account.address
      ~value
      ~endpoint:evm_node_endpoint
      ?data
  in
  wait_for_application ~evm_node ~sc_rollup_node ~node ~client send ()

let check_block_progression ~evm_node ~sc_rollup_node ~node ~client ~endpoint
    ~expected_block_level =
  let* _level = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let* block_number = Eth_cli.block_number ~endpoint in
  return
  @@ Check.((block_number = expected_block_level) int)
       ~error_msg:"Unexpected block number, should be %%R, but got %%L"

let test_evm_node_connection =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"]
    ~uses:(fun _protocol -> Constant.[octez_smart_rollup_node; octez_evm_node])
    ~title:"EVM node server connection"
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
  let evm_node = Evm_node.create (Sc_rollup_node.endpoint sc_rollup_node) in
  (* Tries to start the EVM node server without a listening rollup node. *)
  let process = Evm_node.spawn_run evm_node in
  let* () = Process.check ~expect_failure:true process in
  (* Starts the rollup node. *)
  let* _ = Sc_rollup_node.run sc_rollup_node sc_rollup [] in
  (* Starts the EVM node server and asks its version. *)
  let* () = Evm_node.run evm_node in
  let*? process = evm_node_version evm_node in
  let* () = Process.check process in
  unit

let test_originate_evm_kernel =
  register_both ~tags:["evm"] ~title:"Originate EVM kernel with installer"
  @@ fun ~protocol:_ ~evm_setup:{client; node; sc_rollup_node; _} ->
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* () = Client.bake_for_and_wait ~keys:[] client in
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
  let* storage_root_keys =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:""
         ()
  in
  Check.(
    list_mem
      string
      evm_key
      storage_root_keys
      ~error_msg:"Expected %L to be initialized by the EVM kernel.") ;
  unit

let test_rpc_getBalance =
  register_both ~tags:["evm"; "get_balance"] ~title:"RPC method eth_getBalance"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* balance =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:evm_node_endpoint
  in
  Check.((balance = Configuration.default_bootstrap_account_balance) Wei.typ)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Eth_account.bootstrap_accounts.(0).address) ;
  unit

let test_rpc_getBlockByNumber =
  register_both
    ~tags:["evm"; "get_block_by_number"]
    ~title:"RPC method eth_getBlockByNumber"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  unit

let get_block_by_hash ?(full_tx_objects = false) evm_setup block_hash =
  let* block =
    Evm_node.(
      call_evm_rpc
        evm_setup.evm_node
        {
          method_ = "eth_getBlockByHash";
          parameters = `A [`String block_hash; `Bool full_tx_objects];
        })
  in
  return @@ (block |> Evm_node.extract_result |> Block.of_json)

let test_rpc_getBlockByHash =
  register_both
    ~tags:["evm"; "get_block_by_hash"]
    ~title:"RPC method eth_getBlockByHash"
  @@ fun ~protocol:_ ~evm_setup ->
  let evm_node_endpoint = Evm_node.endpoint evm_setup.evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  let* block' = get_block_by_hash evm_setup (Option.get block.hash) in
  assert (block = block') ;
  unit

let test_l2_block_size_non_zero =
  register_both
    ~tags:["evm"; "block"; "size"]
    ~title:"Block size is greater than zero"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  Check.((block.size > 0l) int32)
    ~error_msg:"Unexpected block size, should be > 0, but got %%L" ;
  unit

let transaction_count_request address =
  Evm_node.
    {
      method_ = "eth_getTransactionCount";
      parameters = `A [`String address; `String "latest"];
    }

let get_transaction_count evm_node address =
  let* transaction_count =
    Evm_node.call_evm_rpc evm_node (transaction_count_request address)
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

let test_rpc_getTransactionCount =
  register_both
    ~tags:["evm"; "get_transaction_count"]
    ~title:"RPC method eth_getTransactionCount"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* transaction_count =
    get_transaction_count evm_node Eth_account.bootstrap_accounts.(0).address
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  unit

let test_rpc_getTransactionCountBatch =
  register_both
    ~tags:["evm"; "get_transaction_count_as_batch"]
    ~title:"RPC method eth_getTransactionCount in batch"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* transaction_count =
    get_transaction_count evm_node Eth_account.bootstrap_accounts.(0).address
  in
  let* transaction_count_batch =
    let* transaction_count =
      Evm_node.batch_evm_rpc
        evm_node
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
  register_both ~tags:["evm"; "rpc"; "batch"] ~title:"RPC batch requests"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* transaction_count, chain_id =
    let transaction_count =
      transaction_count_request Eth_account.bootstrap_accounts.(0).address
    in
    let chain_id = Evm_node.{method_ = "eth_chainId"; parameters = `Null} in
    let* results =
      Evm_node.batch_evm_rpc evm_node [transaction_count; chain_id]
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
  register_proxy
    ~tags:["evm"; "l2_blocks_progression"]
    ~title:"Check L2 blocks progression"
  @@ fun ~protocol:_
             ~evm_setup:{node; client; sc_rollup_node; endpoint; evm_node; _} ->
  let* () =
    check_block_progression
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      ~endpoint
      ~expected_block_level:1
  in
  let* () =
    check_block_progression
      ~evm_node
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
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check L2 blocks consistency of hashes"
  @@ fun protocol ->
  let* {node; client; sc_rollup_node; endpoint; evm_node; _} =
    setup_evm_kernel ~admin:None protocol
  in
  let new_block () =
    let* _level = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
    let* number = Eth_cli.block_number ~endpoint in
    Eth_cli.get_block ~block_id:(string_of_int number) ~endpoint
  in

  let* block0 = Eth_cli.get_block ~block_id:(string_of_int 0) ~endpoint in
  let* block1 = new_block () in
  let* block2 = new_block () in
  let* block3 = new_block () in
  let* block4 = new_block () in

  let check_parent_hash parent block =
    let parent_hash = Option.value ~default:"" parent.Block.hash in
    Check.((block.Block.parent = parent_hash) string)
      ~error_msg:"Unexpected parent hash, should be %%R, but got %%L"
  in

  (* Check consistency accross blocks. *)
  check_parent_hash block0 block1 ;
  check_parent_hash block1 block2 ;
  check_parent_hash block2 block3 ;
  check_parent_hash block3 block4 ;

  let block_hashes, parent_hashes =
    List.map
      (fun Block.{hash; parent; _} -> (hash, parent))
      [block0; block1; block2; block3; block4]
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
    See [etherlink/tezt/tests/evm_kernel_inputs/storage.*] *)
let simple_storage =
  {
    label = "simpleStorage";
    abi = kernel_inputs_path ^ "/storage.abi";
    bin = kernel_inputs_path ^ "/storage.bin";
  }

(** The info for the "erc20tok.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*] *)
let erc20 =
  {
    label = "erc20tok";
    abi = kernel_inputs_path ^ "/erc20tok.abi";
    bin = kernel_inputs_path ^ "/erc20tok.bin";
  }

(** The info for the "loop.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/loop.*] *)
let loop =
  {
    label = "loop";
    abi = kernel_inputs_path ^ "/loop.abi";
    bin = kernel_inputs_path ^ "/loop.bin";
  }

(** The info for the "mapping_storage.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/mapping_storage*] *)
let mapping_storage =
  {
    label = "mappingStorage";
    abi = kernel_inputs_path ^ "/mapping_storage_abi.json";
    bin = kernel_inputs_path ^ "/mapping_storage.bin";
  }

(** The info for the "storage.sol" contract, compiled for Shanghai.
    See [etherlink/tezt/tests/evm_kernel_inputs/shanghai_storage.*] *)
let shanghai_storage =
  {
    label = "shanghai";
    abi = kernel_inputs_path ^ "/shanghai_storage.abi";
    bin = kernel_inputs_path ^ "/shanghai_storage.bin";
  }

(** The info for the Callee contract.
    See [src\kernel_evm\solidity_examples\caller_callee.sol] *)
let callee =
  {
    label = "callee";
    abi = kernel_inputs_path ^ "/callee.abi";
    bin = kernel_inputs_path ^ "/callee.bin";
  }

(** The info for the Caller contract.
    See [src\kernel_evm\solidity_examples\caller_callee.sol] *)
let caller =
  {
    label = "caller";
    abi = kernel_inputs_path ^ "/caller.abi";
    bin = kernel_inputs_path ^ "/caller.bin";
  }

(** The info for the "events.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/events.sol] *)
let events =
  {
    label = "events";
    abi = kernel_inputs_path ^ "/events.abi";
    bin = kernel_inputs_path ^ "/events.bin";
  }

(** The info for the "nested_create.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/nested_create.sol] *)
let nested_create =
  {
    label = "nested_create";
    abi = kernel_inputs_path ^ "/nested_create.abi";
    bin = kernel_inputs_path ^ "/nested_create.bin";
  }

(** The info for the "revert.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/revert.sol] *)
let revert =
  {
    label = "revert";
    abi = kernel_inputs_path ^ "/revert.abi";
    bin = kernel_inputs_path ^ "/revert.bin";
  }

(** The info for the "create2.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/create2.sol] *)
let create2 =
  {
    label = "create2";
    abi = kernel_inputs_path ^ "/create2.abi";
    bin = kernel_inputs_path ^ "/create2.bin";
  }

(** The info for the "oog_call.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/oog_call.sol] *)
let oog_call =
  {
    label = "oog_call";
    abi = kernel_inputs_path ^ "/oog_call.abi";
    bin = kernel_inputs_path ^ "/oog_call.bin";
  }

(** The info for the "ether_wallet.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/ether_wallet.sol] *)
let ether_wallet =
  {
    label = "ether_wallet";
    abi = kernel_inputs_path ^ "/ether_wallet.abi";
    bin = kernel_inputs_path ^ "/ether_wallet.bin";
  }

(** The info for the "block_hash_gen.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/block_hash_gen.sol] *)
let block_hash_gen =
  {
    label = "block_hash_gen";
    abi = kernel_inputs_path ^ "/block_hash_gen.abi";
    bin = kernel_inputs_path ^ "/block_hash_gen.bin";
  }

(** Test that the contract creation works.  *)
let test_l2_deploy_simple_storage =
  register_proxy
    ~tags:["evm"; "l2_deploy"]
    ~title:"Check L2 contract deployment"
  @@ fun ~protocol:_ ~evm_setup ->
  deploy_with_base_checks
    {
      contract = simple_storage;
      expected_address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344";
      (* The same deployment has been reproduced on the Sepolia testnet, resulting
         on this specific code. *)
      expected_code =
        "0x608060405234801561001057600080fd5b50600436106100415760003560e01c80634e70b1dc1461004657806360fe47b1146100645780636d4ce63c14610080575b600080fd5b61004e61009e565b60405161005b91906100d0565b60405180910390f35b61007e6004803603810190610079919061011c565b6100a4565b005b6100886100ae565b60405161009591906100d0565b60405180910390f35b60005481565b8060008190555050565b60008054905090565b6000819050919050565b6100ca816100b7565b82525050565b60006020820190506100e560008301846100c1565b92915050565b600080fd5b6100f9816100b7565b811461010457600080fd5b50565b600081359050610116816100f0565b92915050565b600060208284031215610132576101316100eb565b5b600061014084828501610107565b9150509291505056fea2646970667358221220ec57e49a647342208a1f5c9b1f2049bf1a27f02e19940819f38929bf67670a5964736f6c63430008120033";
    }
    evm_setup

let send_call_set_storage_simple contract_address sender n
    {sc_rollup_node; node; client; endpoint; evm_node; _} =
  let call_set (sender : Eth_account.t) n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:simple_storage.label
      ~address:contract_address
      ~method_call:(Printf.sprintf "set(%d)" n)
  in
  wait_for_application
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    (call_set sender n)
    ()

(** Test that a contract can be called,
    and that the call can modify the storage.  *)
let test_l2_call_simple_storage =
  register_proxy
    ~tags:["evm"; "l2_deploy"; "l2_call"]
    ~title:"Check L2 contract call"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in

  (* deploy contract *)
  let* address, _tx = deploy ~contract:simple_storage ~sender evm_setup in

  (* set 42 *)
  let* tx = send_call_set_storage_simple address sender 42 evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address 1 in
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
  let* () = check_storage_size sc_rollup_node ~address 1 in
  (* value stored has changed *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:24 in

  (* set -1 *)
  (* some environments prevent sending a negative value, as the value is
     unsigned (eg remix) but it is actually the expected result *)
  let* tx = send_call_set_storage_simple address sender (-1) evm_setup in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address 1 in
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
  register_proxy
    ~tags:["evm"; "l2_deploy"; "erc20"; "l2_call"]
    ~title:"Check L2 erc20 contract deployment"
  @@ fun ~protocol:_ ~evm_setup ->
  (* setup *)
  let {evm_node; node; client; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
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
  let* accounts =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:Durable_storage_path.eth_accounts
         ()
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
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_mint sender 42)
      ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs sender 42) ~tx
  in

  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:42 in

  (* player mints 100 *)
  let* tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_mint player 100)
      ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(mint_logs player 100) ~tx
  in
  (* totalSupply is the first value in storage *)
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 100, should fail *)
  let* _tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_burn ~expect_failure:true sender 100)
      ()
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:142 in

  (* sender tries to burn 42, should succeed *)
  let* tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_burn sender 42)
      ()
  in
  let* () =
    check_status_n_logs ~endpoint ~status:true ~logs:(burn_logs sender 42) ~tx
  in
  let* () = check_nb_in_storage ~evm_setup ~address ~nth:0 ~expected:100 in
  unit

let test_deploy_contract_for_shanghai =
  register_proxy
    ~tags:["evm"; "deploy"; "shanghai"]
    ~title:
      "Check that a contract containing PUSH0 can successfully be deployed."
  @@ fun ~protocol:_ ~evm_setup ->
  deploy_with_base_checks
    {
      contract = shanghai_storage;
      expected_address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344";
      expected_code =
        "0x608060405234801561000f575f80fd5b5060043610610034575f3560e01c80632e64cec1146100385780636057361d14610056575b5f80fd5b610040610072565b60405161004d919061009b565b60405180910390f35b610070600480360381019061006b91906100e2565b61007a565b005b5f8054905090565b805f8190555050565b5f819050919050565b61009581610083565b82525050565b5f6020820190506100ae5f83018461008c565b92915050565b5f80fd5b6100c181610083565b81146100cb575f80fd5b50565b5f813590506100dc816100b8565b92915050565b5f602082840312156100f7576100f66100b4565b5b5f610104848285016100ce565b9150509291505056fea2646970667358221220c1aa96a14de9ab1c36fb97f3051eac7ba11ec6ac604ddeab90e5b6ac8bd4efc064736f6c63430008140033";
    }
    evm_setup

let check_log_indices ~endpoint ~status ~tx indices =
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
      let received_indices =
        List.map (fun tx -> tx.Transaction.logIndex) r.logs
      in
      Check.(
        (received_indices = indices)
          (list int32)
          ~__LOC__
          ~error_msg:
            "Unexpected transaction logs indices, expected:\n%R but got:\n%L") ;
      unit

let test_log_index =
  register_both
    ~tags:["evm"; "log_index"]
    ~title:"Check that log index is correctly computed"
  @@ fun ~protocol:_ ~evm_setup ->
  (* setup *)
  let {evm_node; node; client; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let _player = Eth_account.bootstrap_accounts.(1) in
  (* deploy the events contract *)
  let* _address, _tx = deploy ~contract:events ~sender evm_setup in
  (* Emits two events: EventA and EventB *)
  let raw_emitBoth =
    "0xf88901843b9aca00826bf694d77420f73b4612a7a99dba8c2afd30a1886b034480a4cc79cf9d0000000000000000000000000000000000000000000000000000000000000064820a96a01350f66edc1a5bfa7dc8651d5735dbb343c491939a9e49b3f1a041b6a234df72a0028c5523a2bcc1077e090360a0e96ffaff7a2f26fd161b87107252e4bb83c47b"
  in
  (* Emits one event: EventA *)
  let raw_emitA =
    "0xf88980843b9aca0082644094d77420f73b4612a7a99dba8c2afd30a1886b034480a413c49adf000000000000000000000000000000000000000000000000000000000000000a820a95a0a46df17e7392d9777a94248e2dd6d9a0a097143cf915152d531c07fa604d2219a053c59f5e070adef0e2c443e5f7afb6435dfed20e04ddcfcfccb150972535cd2d"
  in
  let* _requests, _receipt, hashes =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_node
      [raw_emitBoth; raw_emitA]
  in
  let* () =
    check_log_indices ~endpoint ~status:true ~tx:(List.hd hashes) [0l; 1l]
  in
  check_log_indices ~endpoint ~status:true ~tx:(List.nth hashes 1) [2l]

(* TODO: add internal parameters here (e.g the kernel version) *)
type config_result = {chain_id : int64}

let config_setup evm_setup =
  let web3_clientVersion =
    Evm_node.{method_ = "web3_clientVersion"; parameters = `A []}
  in
  let chain_id = Evm_node.{method_ = "eth_chainId"; parameters = `Null} in
  let* results =
    Evm_node.batch_evm_rpc evm_setup.evm_node [web3_clientVersion; chain_id]
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

let latest_block ?(full_tx_objects = false) evm_node =
  Rpc.get_block_by_number ~full_tx_objects ~block:"latest" evm_node

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
    Evm_node.call_evm_rpc
      full_evm_setup.evm_node
      {method_ = "eth_getTransactionReceipt"; parameters = `A [`String tx_hash]}
  in
  return JSON.(json |-> "result" |> Transaction.transaction_receipt_of_json)

let ensure_transfer_result_integrity ~transfer_result ~sender ~receiver
    full_evm_setup =
  let endpoint = Evm_node.endpoint full_evm_setup.evm_node in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance = balance sender.Eth_account.address in
  assert (sender_balance = transfer_result.sender_balance_after) ;
  let* receiver_balance = balance receiver.Eth_account.address in
  assert (receiver_balance = transfer_result.receiver_balance_after) ;
  let* sender_nonce =
    get_transaction_count full_evm_setup.evm_node sender.address
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
  let endpoint = Evm_node.endpoint full_evm_setup.evm_node in
  let balance account = Eth_cli.balance ~account ~endpoint in
  let* sender_balance_before = balance sender.Eth_account.address in
  let* receiver_balance_before = balance receiver.Eth_account.address in
  let* sender_nonce_before =
    get_transaction_count full_evm_setup.evm_node sender.address
  in
  let* tx_hash = send ~sender ~receiver ~value ?data full_evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx:tx_hash in
  let* sender_balance_after = balance sender.address in
  let* receiver_balance_after = balance receiver.address in
  let* sender_nonce_after =
    get_transaction_count full_evm_setup.evm_node sender.address
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

let transfer ?data ~flat_fee ~evm_setup () =
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
      ~value:Wei.(Configuration.default_bootstrap_account_balance - one_eth)
      ~sender
      ~receiver
      evm_setup
  in
  let* receipt =
    Eth_cli.get_receipt ~endpoint:evm_setup.endpoint ~tx:tx_object.hash
  in
  let gas_used, gas_price =
    match receipt with
    | Some Transaction.{status = true; gasUsed; effectiveGasPrice; _} ->
        (gasUsed, effectiveGasPrice)
    | _ -> Test.fail "Transaction didn't succeed"
  in
  let fees = expected_gas_fees ~gas_price ~gas_used in
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
  if Option.is_none data then
    check_tx_gas_for_flat_fee ~flat_fee ~gas_used ~gas_price ;
  unit

let test_l2_transfer =
  let flat_fee = Wei.zero in
  let test_f ~protocol:_ ~evm_setup = transfer ~evm_setup ~flat_fee () in
  let title = "Check L2 transfers are applied" in
  let tags = ["evm"; "l2_transfer"] in
  register_both ~title ~tags test_f

let test_chunked_transaction =
  let flat_fee = Wei.zero in
  let test_f ~protocol:_ ~evm_setup =
    transfer ~data:("0x" ^ String.make 12_000 'a') ~flat_fee ~evm_setup ()
  in
  let title = "Check L2 chunked transfers are applied" in
  let tags = ["evm"; "l2_transfer"; "chunked"] in
  register_both ~title ~tags test_f

let test_rpc_txpool_content =
  register_both
    ~tags:["evm"; "txpool_content"]
    ~title:"Check RPC txpool_content is available"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  (* The content of the txpool is not relevant for now, this test only checks
     the the RPC is correct, i.e. an object containing both the `pending` and
     `queued` fields, containing the correct objects: addresses pointing to a
     mapping of nonces to transactions. *)
  let* _result = Evm_node.txpool_content evm_node in
  unit

let test_rpc_web3_clientVersion =
  register_both
    ~tags:["evm"; "client_version"]
    ~title:"Check RPC web3_clientVersion"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* web3_clientVersion =
    Evm_node.(
      call_evm_rpc evm_node {method_ = "web3_clientVersion"; parameters = `A []})
  in
  let* server_version = evm_node_version evm_node |> Runnable.run in
  Check.(
    (JSON.(web3_clientVersion |-> "result" |> as_string)
    = JSON.as_string server_version)
      string)
    ~error_msg:"Expected version %%R, got %%L." ;
  unit

let test_rpc_web3_sha3 =
  register_both ~tags:["evm"; "sha3"] ~title:"Check RPC web3_sha3"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  (* From the example provided in
     https://ethereum.org/en/developers/docs/apis/json-rpc/#web3_sha3 *)
  let input_data = "0x68656c6c6f20776f726c64" in
  let expected_reply =
    "0x47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
  in
  let* web3_sha3 =
    Evm_node.(
      call_evm_rpc
        evm_node
        {method_ = "web3_sha3"; parameters = `A [`String input_data]})
  in
  Check.((JSON.(web3_sha3 |-> "result" |> as_string) = expected_reply) string)
    ~error_msg:"Expected hash %%R, got %%L." ;
  unit

let test_simulate =
  register_proxy
    ~tags:["evm"; "simulate"]
    ~title:"A block can be simulated in the rollup node"
    (fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; _} ->
      let*@ block_number = Rpc.block_number evm_node in
      let* simulation_result =
        Sc_rollup_node.RPC.call sc_rollup_node
        @@ Sc_rollup_rpc.post_global_block_simulate
             ~insight_requests:
               [`Durable_storage_key ["evm"; "blocks"; "current"; "number"]]
             []
      in
      let simulated_block_number =
        match simulation_result.insights with
        | [insight] -> Option.map Helpers.hex_string_to_int insight
        | _ -> None
      in
      Check.(
        (simulated_block_number = Some (Int32.to_int block_number + 1))
          (option int))
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
  register_proxy
    ~config:(`Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml"))
    ~tags:["evm"; "full_blocks"]
    ~title:
      "Check `evm_getBlockByNumber` with full blocks returns the correct \
       informations"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  let txs =
    read_tx_from_file ()
    |> List.filteri (fun i _ -> i < 5)
    |> List.map (fun (tx, _hash) -> tx)
  in
  let* _requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_node txs
  in
  let* block =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool true];
        })
  in
  let block = block |> Evm_node.extract_result |> Block.of_json in
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
  register_proxy
    ~tags:["evm"; "blocks"; "latest"]
    ~title:
      "Check `evm_getBlockByNumber` works correctly when asking for the \
       `latest`"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  (* The first execution of the kernel actually builds two blocks: the genesis
     block and the block for the current inbox. As such, the latest block is
     always of level 1. *)
  let* latest_block =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters = `A [`String "latest"; `Bool false];
        })
  in
  let latest_block = latest_block |> Evm_node.extract_result |> Block.of_json in
  Check.((latest_block.Block.number = 1l) int32)
    ~error_msg:"Expected latest being block number %R, but got %L" ;
  unit

let test_eth_call_nullable_recipient =
  register_both
    ~tags:["evm"; "eth_call"; "null"]
    ~title:"Check `eth_call.to` input can be null"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_call";
          parameters = `A [`O [("to", `Null)]; `String "latest"];
        })
  in
  (* Check the RPC returns a `result`. *)
  let _result = call_result |> Evm_node.extract_result in
  unit

let test_inject_100_transactions =
  register_proxy
    ~tags:["evm"; "bigger_blocks"]
    ~title:"Check blocks can contain more than 64 transactions"
    ~config:(`Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml"))
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_node txs
  in
  let* block_with_100tx =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_getBlockByNumber";
          parameters =
            `A
              [`String (Format.sprintf "%#lx" receipt.blockNumber); `Bool false];
        })
  in
  let block_with_100tx =
    block_with_100tx |> Evm_node.extract_result |> Block.of_json
  in
  (match block_with_100tx.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the latest block, got %L") ;

  let* _level = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let*@ latest_evm_level = Rpc.block_number evm_node in
  (* At each loop, the kernel reads the previous block. Until the patch, the
     kernel failed to read the previous block if there was more than 64 hash,
     this test ensures it works by assessing new blocks are produced. *)
  Check.((latest_evm_level >= Int32.succ block_with_100tx.Block.number) int32)
    ~error_msg:
      "Expected a new block after the one with 100 transactions, but level \
       hasn't changed" ;
  unit

let check_estimate_gas {evm_node; _} eth_call expected_gas =
  (* Make the call to the EVM node. *)
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {
          method_ = "eth_estimateGas";
          parameters = `A [`O eth_call; `String "latest"];
        })
  in
  (* Check the RPC result. *)
  let r = call_result |> Evm_node.extract_result |> JSON.as_int in
  Check.((r >= expected_gas) int)
    ~error_msg:"Expected result greater than %R, but got %L" ;
  unit

let check_eth_call {evm_node; _} eth_call expected_result =
  (* Make the call to the EVM node. *)
  let* call_result =
    Evm_node.(
      call_evm_rpc
        evm_node
        {method_ = "eth_call"; parameters = `A [`O eth_call; `String "latest"]})
  in
  (* Check the RPC result. *)
  let r = call_result |> Evm_node.extract_result in
  Check.((JSON.as_string r = expected_result) string)
    ~error_msg:"Expected result %R, but got %L" ;
  unit

let test_eth_call_large =
  let test_f ~protocol:_ ~evm_setup =
    let sender = Eth_account.bootstrap_accounts.(0) in
    (* large request *)
    let eth_call =
      [
        ("to", Ezjsonm.encode_string sender.address);
        ("data", Ezjsonm.encode_string ("0x" ^ String.make 12_000 'a'));
      ]
    in

    check_eth_call evm_setup eth_call "0x"
  in
  let title = "eth_call with a large amount of data" in
  let tags = ["evm"; "eth_call"; "simulate"; "large"] in
  register_both ~title ~tags test_f

let test_estimate_gas =
  let test_f ~protocol:_ ~evm_setup =
    (* large request *)
    let data = read_file simple_storage.bin in
    let eth_call = [("data", Ezjsonm.encode_string @@ "0x" ^ data)] in

    check_estimate_gas evm_setup eth_call 23423
  in

  let title = "eth_estimateGas for contract creation" in
  let tags = ["evm"; "eth_estimategas"; "simulate"] in
  register_both ~title ~tags test_f

let test_estimate_gas_additionnal_field =
  let test_f ~protocol:_ ~evm_setup =
    (* large request *)
    let data = read_file simple_storage.bin in
    let eth_call =
      [
        ( "from",
          Ezjsonm.encode_string @@ "0x6ce4d79d4e77402e1ef3417fdda433aa744c6e1c"
        );
        ("data", Ezjsonm.encode_string @@ "0x" ^ data);
        ("value", Ezjsonm.encode_string @@ "0x0");
        (* for some reason remix adds the "type" field *)
        ("type", Ezjsonm.encode_string @@ "0x1");
      ]
    in

    check_estimate_gas evm_setup eth_call 23423
  in
  let title = "eth_estimateGas allows additional fields" in
  let tags = ["evm"; "eth_estimategas"; "simulate"; "remix"] in
  register_both ~title ~tags test_f

let test_eth_call_storage_contract =
  let test_f ~protocol:_ ~evm_setup:({evm_node; endpoint; _} as evm_setup) =
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
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O eth_call; `String "latest"];
          })
    in

    let r = call_result |> Evm_node.extract_result in
    Check.(
      (JSON.as_string r
     = "0x0000000000000000000000000000000000000000000000000000000000000000")
        string)
      ~error_msg:"Expected result %R, but got %L" ;

    let* tx = send_call_set_storage_simple address sender 42 evm_setup in
    let* () = check_tx_succeeded ~endpoint ~tx in

    (* make call to proxy *)
    let* call_result =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O eth_call; `String "latest"];
          })
    in
    let r = call_result |> Evm_node.extract_result in
    Check.(
      (JSON.as_string r
     = "0x000000000000000000000000000000000000000000000000000000000000002a")
        string)
      ~error_msg:"Expected result %R, but got %L" ;
    unit
  in
  let title = "Call a view" in
  let tags = ["evm"; "eth_call"; "simulate"] in
  register_both ~title ~tags test_f

let test_eth_call_storage_contract_eth_cli =
  let test_f ~protocol:_
      ~evm_setup:
        ({evm_node; endpoint; sc_rollup_node; node; client; _} as evm_setup) =
    (* sanity *)
    let* call_result =
      Evm_node.(
        call_evm_rpc
          evm_node
          {
            method_ = "eth_call";
            parameters = `A [`O [("to", `Null)]; `String "latest"];
          })
    in
    (* Check the RPC returns a `result`. *)
    let _result = call_result |> Evm_node.extract_result in

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
      wait_for_application ~evm_node ~sc_rollup_node ~node ~client call_num ()
    in

    Check.((String.trim res = "42") string)
      ~error_msg:"Expected result %R, but got %L" ;
    unit
  in
  let title = "Call a view through an ethereum client" in
  let tags = ["evm"; "eth_call"; "simulate"] in

  register_both ~title ~tags test_f

let test_preinitialized_evm_kernel =
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
  register_proxy
    ~tags:["evm"; "administrator"; "config"]
    ~title:"Creates a kernel with an initialized administrator key"
    ~config
  @@ fun ~protocol:_ ~evm_setup:{sc_rollup_node; _} ->
  let* found_administrator_key_hex =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:administrator_key_path
         ()
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

let deposit ~amount_mutez ~bridge ~depositor ~receiver ~evm_node ~sc_rollup_node
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
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  unit

let withdraw protocol ~commitment_period ~challenge_window ~amount_wei ~sender
    ~receiver ~evm_node ~sc_rollup_node ~sc_rollup_address ~node ~client
    ~endpoint =
  let* withdrawal_level = Client.level client in

  (* Call the withdrawal precompiled contract. *)
  let* () =
    Eth_cli.add_abi ~label:"withdraw" ~abi:(withdrawal_abi_path ()) ()
  in
  let call_withdraw =
    Eth_cli.contract_send
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:"withdraw"
      ~address:"0xff00000000000000000000000000000000000001"
      ~method_call:(sf {|withdraw_base58("%s")|} receiver)
      ~value:amount_wei
  in
  let* _tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      call_withdraw
      ()
  in

  (* Bake enough levels to have a commitment. *)
  let* _ =
    repeat (commitment_period * 2) (fun () ->
        let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
        unit)
  in

  (* Bake enough levels to cement the commitment. *)
  let* _ =
    repeat (challenge_window + 1) (fun () ->
        let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
        unit)
  in

  (* Construct and execute the outbox proof. *)
  let find_outbox level =
    let rec aux level' =
      if level' > level + 10 then
        Test.fail "Looked for an outbox for 10 levels, stopping the loop"
      else
        let* outbox =
          Sc_rollup_node.RPC.call sc_rollup_node
          @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level:level' ()
        in
        if
          JSON.is_null outbox
          || (JSON.is_list outbox && JSON.as_list outbox = [])
        then aux (level' + 1)
        else return (outbox, level')
    in
    aux level
  in
  let* outbox, withdrawal_level = find_outbox withdrawal_level in
  let outbox_message =
    JSON.(outbox |=> 0 |-> "message" |-> "transactions" |=> 0)
  in
  let parameters_json = JSON.(outbox_message |-> "parameters") in
  let batch =
    {
      destination = JSON.(outbox_message |-> "destination" |> as_string);
      entrypoint = Some JSON.(outbox_message |-> "entrypoint" |> as_string);
      parameters = JSON.unannotate parameters_json;
      parameters_ty = None;
    }
  in
  let message_json = Sc_rollup_helpers.json_of_output_tx_batch [batch] in
  let* message =
    Codec.encode
      ~name:(Protocol.encoding_prefix protocol ^ ".smart_rollup.outbox.message")
      message_json
  in
  let* outbox_proof =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.outbox_proof_single
         ~message_index:0
         ~outbox_level:withdrawal_level
         ~message
         ()
  in
  let Sc_rollup_rpc.{proof; commitment_hash} =
    match outbox_proof with
    | Some r -> r
    | None -> Test.fail "No outbox proof found for the withdrawal"
  in
  let*! () =
    Client.Sc_rollup.execute_outbox_message
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~rollup:sc_rollup_address
      ~src:Constant.bootstrap1.alias
      ~commitment_hash
      ~proof
      client
  in
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  unit

let check_balance ~receiver ~endpoint expected_balance =
  let* balance = Eth_cli.balance ~account:receiver ~endpoint in
  let balance = Wei.truncate_to_mutez balance in
  Check.((balance = Tez.to_mutez expected_balance) int)
    ~error_msg:(sf "Expected balance of %s should be %%R, but got %%L" receiver) ;
  unit

let test_deposit_and_withdraw =
  let admin = Constant.bootstrap5 in
  let commitment_period = 5 and challenge_window = 5 in
  register_proxy
    ~tags:["evm"; "deposit"; "withdraw"]
    ~title:"Deposit and withdraw tez"
    ~admin:(Some admin)
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.octez_codec;
        Constant.WASM.evm_kernel;
      ])
    ~commitment_period
    ~challenge_window
  @@ fun ~protocol
             ~evm_setup:
               {
                 client;
                 sc_rollup_address;
                 l1_contracts;
                 sc_rollup_node;
                 node;
                 endpoint;
                 evm_node;
                 _;
               } ->
  let {bridge; admin = _; exchanger = _} =
    match l1_contracts with
    | Some x -> x
    | None -> Test.fail ~__LOC__ "The test needs the L1 bridge"
  in

  let amount_mutez = Tez.of_mutez_int 100_000_000 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
        public_key =
          "0466ed90f9a86c0908746475fbe0a40c72237de22d89076302e22c2a8da259b4aba5c7ee1f3dc3fd0b240645462620ae62b6fe8fe5b3464c3b1b4ae6c06c97b7b6";
      }
  in

  let* () =
    deposit
      ~amount_mutez
      ~sc_rollup_address
      ~bridge
      ~depositor:admin
      ~receiver:receiver.address
      ~evm_node
      ~sc_rollup_node
      ~node
      client
  in
  let* () = check_balance ~receiver:receiver.address ~endpoint amount_mutez in

  let amount_wei = Wei.of_tez amount_mutez in
  (* Keep a small amount to pay for the gas. *)
  let amount_wei = Wei.(amount_wei - one_eth) in

  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in
  let* _tx =
    withdraw
      protocol
      ~evm_node
      ~sc_rollup_address
      ~commitment_period
      ~challenge_window
      ~amount_wei
      ~sender:receiver
      ~receiver:withdraw_receiver
      ~node
      ~sc_rollup_node
      ~client
      ~endpoint
  in

  let* balance = Client.get_balance_for ~account:withdraw_receiver client in
  let expected_balance = Tez.(amount_mutez - one) in
  Check.((balance = expected_balance) Tez.typ)
    ~error_msg:(sf "Expected %%R amount instead of %%L after withdrawal") ;
  return ()

let get_kernel_boot_wasm ~sc_rollup_node =
  let rpc_hooks : RPC_core.rpc_hooks =
    let on_request _verb ~uri:_ _data = Regression.capture "<boot.wasm>" in
    let on_response _status ~body:_ = Regression.capture "<boot.wasm>" in
    {on_request; on_response}
  in
  let* kernel_boot_opt =
    Sc_rollup_node.RPC.call sc_rollup_node ~log_response_body:false ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:Durable_storage_path.kernel_boot_wasm
         ()
  in
  match kernel_boot_opt with
  | Some boot_wasm -> return boot_wasm
  | None -> failwith "Kernel `boot.wasm` should be accessible/readable."

let gen_test_kernel_upgrade ?timestamp ?(activation_timestamp = "0")
    ?(from_ghostnet = false) ?evm_setup ?rollup_address ?(should_fail = false)
    ~installee ?with_administrator ?expect_l1_failure
    ?(admin = Constant.bootstrap1) ?(upgrador = admin) protocol =
  let* {
         node;
         client;
         sc_rollup_node;
         sc_rollup_address;
         evm_node;
         l1_contracts;
         _;
       } =
    match evm_setup with
    | Some evm_setup -> return evm_setup
    | None ->
        setup_evm_kernel
          ?timestamp
          ?with_administrator
          ~admin:(Some admin)
          protocol
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
  let* payload =
    let* {root_hash; _} =
      Sc_rollup_helpers.prepare_installer_kernel ~preimages_dir installee
    in
    if from_ghostnet then return root_hash
    else Evm_node.upgrade_payload ~root_hash ~activation_timestamp
  in
  let* kernel_boot_wasm_before_upgrade = get_kernel_boot_wasm ~sc_rollup_node in
  let* expected_kernel_boot_wasm =
    if should_fail then return kernel_boot_wasm_before_upgrade
    else return @@ Hex.show @@ Hex.of_string @@ read_file (Uses.path installee)
  in
  let* () =
    let* () =
      Client.transfer
        ?expect_failure:expect_l1_failure
        ~amount:Tez.zero
        ~giver:upgrador.public_key_hash
        ~receiver:l1_contracts.admin
        ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
        ~burn_cap:Tez.one
        client
    in
    let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
    unit
  in
  let* kernel_boot_wasm_after_upgrade = get_kernel_boot_wasm ~sc_rollup_node in
  Check.((expected_kernel_boot_wasm = kernel_boot_wasm_after_upgrade) string)
    ~error_msg:(sf "Unexpected `boot.wasm`.") ;
  return
    (sc_rollup_node, node, client, evm_node, kernel_boot_wasm_before_upgrade)

let test_kernel_upgrade_evm_to_evm =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade integrity to itself"
  @@ fun protocol ->
  let* sc_rollup_node, node, client, evm_node, _ =
    gen_test_kernel_upgrade ~installee:Constant.WASM.evm_kernel protocol
  in
  (* We ensure the upgrade went well by checking if the kernel still produces
     blocks. *)
  let endpoint = Evm_node.endpoint evm_node in
  check_block_progression
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    ~endpoint
    ~expected_block_level:2

let test_kernel_upgrade_wrong_key =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails with a wrong administrator key"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      ~admin:Constant.bootstrap1
      ~upgrador:Constant.bootstrap2
      protocol
  in
  unit

let test_kernel_upgrade_wrong_rollup_address =
  Protocol.register_test
    ~__FILE__
    ~tags:["address"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails with a wrong rollup address"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~expect_l1_failure:true
      ~rollup_address:"sr1T13qeVewVm3tudQb8dwn8qRjptNo7KVkj"
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      protocol
  in
  unit

let test_kernel_upgrade_no_administrator =
  Protocol.register_test
    ~__FILE__
    ~tags:["administrator"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade fails if there is no administrator"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~should_fail:true
      ~installee:Constant.WASM.debug_kernel
      ~with_administrator:false
      protocol
  in
  unit

let test_kernel_upgrade_failing_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["migration"; "upgrade"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.failed_migration;
      ])
    ~title:"Ensures EVM kernel's upgrade rollback when migration fails"
  @@ fun protocol ->
  let* sc_rollup_node, node, client, evm_node, _original_kernel_boot_wasm =
    gen_test_kernel_upgrade
      ~installee:Constant.WASM.failed_migration
      ~should_fail:true
      protocol
  in
  (* We make sure that we can't read under the tmp file, after migration failed,
     everything is reverted. *)
  let* tmp_dummy =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:"/tmp/__dummy"
         ()
  in
  (match tmp_dummy with
  | Some _ -> failwith "Nothing should be readable under the temporary dir."
  | None -> ()) ;
  (* We ensure that the fallback mechanism went well by checking if the
     kernel still produces blocks since it has booted back to the previous,
     original kernel. *)
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let endpoint = Evm_node.endpoint evm_node in
  check_block_progression
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    ~endpoint
    ~expected_block_level:3

let send_raw_transaction_request raw_tx =
  Evm_node.
    {method_ = "eth_sendRawTransaction"; parameters = `A [`String raw_tx]}

let send_raw_transaction evm_node raw_tx =
  let* response =
    Evm_node.call_evm_rpc evm_node (send_raw_transaction_request raw_tx)
  in
  let hash = response |> Evm_node.extract_result |> JSON.as_string_opt in
  let error_message =
    response |> Evm_node.extract_error_message |> JSON.as_string_opt
  in
  match (hash, error_message) with
  | Some hash, _ -> return (Ok hash)
  | _, Some error_code -> return (Error error_code)
  | _ -> failwith "invalid response from eth_sendRawTransaction"

let test_rpc_sendRawTransaction =
  register_both
    ~tags:["evm"; "tx_hash"]
    ~title:
      "Ensure EVM node returns appropriate hash for any given transactions."
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 5) in
  let* hashes =
    Lwt_list.map_p
      (fun (tx_raw, _) ->
        let* hash = send_raw_transaction evm_node tx_raw in
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
  Evm_node.
    {
      method_ = "eth_getTransactionByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_transaction_by_block_arg_and_index ~by evm_node block_hash index =
  let* transaction_object =
    Evm_node.call_evm_rpc
      evm_node
      (get_transaction_by_block_arg_and_index_request ~by block_hash index)
  in
  return
    JSON.(
      transaction_object |-> "result" |> Transaction.transaction_object_of_json)

let test_rpc_getTransactionByBlockArgAndIndex ~by ~evm_setup =
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let* _, _, hashes =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_node
      (List.map fst txs)
  in
  Lwt_list.iter_s
    (fun transaction_hash ->
      let* receipt =
        wait_for_application
          ~evm_node
          ~sc_rollup_node
          ~node
          ~client
          (wait_for_transaction_receipt ~evm_node ~transaction_hash)
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
          evm_node
          block_arg
          (Int32.to_string index)
      in
      Check.(
        ((transaction_object.hash = transaction_hash) string)
          ~error_msg:"Incorrect transaction hash, should be %R, but got %L.") ;
      unit)
    hashes

let test_rpc_getTransactionByBlockHashAndIndex =
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  register_both
    ~tags:["evm"; "get_transaction_by"; "block_hash_and_index"]
    ~title:"RPC method eth_getTransactionByBlockHashAndIndex"
    ~config
  @@ fun ~protocol:_ -> test_rpc_getTransactionByBlockArgAndIndex ~by:`Hash

let test_rpc_getTransactionByBlockNumberAndIndex =
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  register_both
    ~tags:["evm"; "get_transaction_by"; "block_number_and_index"]
    ~title:"RPC method eth_getTransactionByBlockNumberAndIndex"
    ~config
  @@ fun ~protocol:_ -> test_rpc_getTransactionByBlockArgAndIndex ~by:`Number

let test_validation_result =
  register_both
    ~tags:["evm"; "simulate"]
    ~title:
      "Ensure validation returns appropriate address for a given transaction."
  @@ fun ~protocol:_ ~evm_setup:{sc_rollup_node; _} ->
  (* tx is a signed legacy transaction obtained with the following data, using
     the following private key:
        data = {
            "nonce": "0x00",
            "gasPrice": "0x5208",
            "gasLimit": "0x00",
            "to": "0x0000000000000000000000000000000000000000",
            "value": "0x00",
            "data": "0x",
            "chainId": 1337
        }
        private key=0x84e147b8bc36d99cc6b1676318a0635d8febc9f02897b0563ad27358589ee502
  *)
  let tx =
    "f86180825208809400000000000000000000000000000000000000008080820a95a0f47140763cf73d6d9b342727e5a0809f7997bb62375060932af9bbc2e74b6212a03a018079a2fd7fefb625451ce2fafcdf873b892ff9d4e3e1f2ada5650012f072"
  in
  let simulation_msg = "ff0101" ^ tx in
  let* simulation_result =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.post_global_block_simulate
         ~insight_requests:
           [
             `Durable_storage_key ["evm"; "simulation_status"];
             `Durable_storage_key ["evm"; "simulation_result"];
           ]
         [Hex.to_string @@ `Hex "ff"; Hex.to_string @@ `Hex simulation_msg]
  in
  let expected_insights =
    [Some "01"; Some "f0affc80a5f69f4a9a3ee01a640873b6ba53e539"]
  in
  Check.(
    (simulation_result.insights = expected_insights) (list @@ option string))
    ~error_msg:"Expected result %R, but got %L" ;
  unit

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
let gen_kernel_migration_test ?config ?(admin = Constant.bootstrap5)
    ~scenario_prior ~scenario_after protocol =
  let* evm_setup =
    setup_evm_kernel
      ?config
      ~kernel_installee:Constant.WASM.ghostnet_evm_kernel
      ~admin:(Some admin)
      protocol
  in
  (* Load the EVM rollup's storage and sanity check results. *)
  let* evm_node =
    Evm_node.init (Sc_rollup_node.endpoint evm_setup.sc_rollup_node)
  in
  let endpoint = Evm_node.endpoint evm_node in
  let* sanity_check =
    scenario_prior ~evm_setup:{evm_setup with evm_node; endpoint}
  in
  (* Upgrade the kernel. *)
  let* _ =
    gen_test_kernel_upgrade
      ~from_ghostnet:true
      ~evm_setup
      ~installee:Constant.WASM.evm_kernel
      ~admin
      protocol
  in
  let* _ =
    (* wait for the migration to be processed *)
    next_evm_level
      ~evm_node
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
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Ensures EVM kernel's upgrade succeed with potential migration(s)."
  @@ fun protocol ->
  let sender, receiver =
    (Eth_account.bootstrap_accounts.(0), Eth_account.bootstrap_accounts.(1))
  in
  let scenario_prior ~evm_setup =
    let* transfer_result =
      make_transfer
        ~value:Wei.(Configuration.default_bootstrap_account_balance - one_eth)
        ~sender
        ~receiver
        evm_setup
    in
    let*@ block_result = latest_block evm_setup.evm_node in
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
    ~uses:(fun _protocol ->
      Constant.
        [
          octez_smart_rollup_node;
          smart_rollup_installer;
          octez_evm_node;
          Constant.WASM.evm_kernel;
        ])
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
      ~parameters_ty:evm_type
      ~installee:Constant.WASM.evm_kernel
      ~config:(`Path Base.(project_root // "etherlink/config/dailynet.yaml"))
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
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      rollup_address
      smart_rollup_node_extra_args
  in

  let* evm_node = Evm_node.init (Sc_rollup_node.endpoint sc_rollup_node) in
  let endpoint = Evm_node.endpoint evm_node in

  (* Deposit tokens to the EVM rollup. *)
  let amount_mutez = Tez.of_mutez_int 100_000_000 in
  let receiver = "0x119811f34EF4491014Fbc3C969C426d37067D6A4" in

  let* () =
    deposit
      ~amount_mutez
      ~bridge:bridge_address
      ~depositor:Constant.bootstrap2
      ~receiver
      ~evm_node
      ~sc_rollup_node
      ~sc_rollup_address:rollup_address
      ~node
      client
  in

  (* Check the balance in the EVM rollup. *)
  check_balance ~receiver ~endpoint amount_mutez

let test_rpc_sendRawTransaction_nonce_too_low =
  register_both
    ~tags:["evm"; "nonce"]
    ~title:"Returns an error if the nonce is too low"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  (* Nonce: 0 *)
  let raw_tx =
    "0xf86c80825208831e8480940000000000000000000000000000000000000000888ac7230489e8000080820a96a038294f867266c767aee6c3b54a0c444368fb8d5e90353219bce1da78de16aea4a018a7d3c58ddb1f6b33bad5dde106843acfbd6467e5df181d22270229dcfdf601"
  in
  let* result = send_raw_transaction evm_node raw_tx in
  let transaction_hash = Result.get_ok result in
  let* _ =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_node ~transaction_hash)
      ()
  in
  let* result = send_raw_transaction evm_node raw_tx in
  let error_message = Result.get_error result in
  Check.(
    ((error_message = "Nonce too low.") string)
      ~error_msg:"The transaction should fail") ;
  unit

let test_rpc_sendRawTransaction_nonce_too_high =
  register_both
    ~tags:["evm"; "nonce"]
    ~title:"Accepts transactions with nonce too high."
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  (* Nonce: 1 *)
  let raw_tx =
    "0xf86c01825208831e8480940000000000000000000000000000000000000000888ac7230489e8000080820a95a0a349864bedc9b84aea88cda197e96538c62c242286ead58eb7180a611f850237a01206525ff16ae5b708ee02b362f9b4d7565e0d7e9b4c536d7ef7dec81cda3ac7"
  in
  let* result = send_raw_transaction evm_node raw_tx in
  Check.(
    ((Result.is_ok result = true) bool) ~error_msg:"The transaction should fail") ;
  unit

let test_deposit_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "deposit"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
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
          evm_node;
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
        ~evm_node
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
          evm_node;
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
        ~evm_node
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
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Block storage before and after migration"
  @@ fun protocol ->
  let block_id = "1" in
  let scenario_prior
      ~evm_setup:{endpoint; evm_node; sc_rollup_node; node; client; _} =
    let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
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
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Returns an error if the chainId is not correct."
  @@ fun protocol ->
  let* {evm_node; _} = setup_evm_kernel ~admin:None protocol in
  (* Nonce: 0, chainId: 4242*)
  let raw_tx =
    "0xf86a8080831e8480940000000000000000000000000000000000000000888ac7230489e8000080822148a0e09f1fb4920f2e64a274b83d925890dd0b109fdf31f2811a781e918118daf34aa00f425e9a93bd92d710d3d323998b093a8c7d497d2af688c062a8099b076813e8"
  in
  let* result = send_raw_transaction evm_node raw_tx in
  let error_message = Result.get_error result in
  Check.(
    ((error_message = "Invalid chain id.") string)
      ~error_msg:"The transaction should fail") ;
  unit

let tez_kernelVersion evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      Evm_node.{method_ = "tez_kernelVersion"; parameters = `Null}
  in
  return JSON.(json |-> "result" |> as_string)

let test_kernel_upgrade_version_change =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "version"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Kernel version changes after an upgrade"
  @@ fun protocol ->
  let scenario_prior ~evm_setup = tez_kernelVersion evm_setup.evm_node in
  let scenario_after ~evm_setup ~sanity_check:old =
    let* new_ = tez_kernelVersion evm_setup.evm_node in
    Check.((old <> new_) string)
      ~error_msg:"The kernel version must change after an upgrade" ;
    unit
  in
  gen_kernel_migration_test ~scenario_prior ~scenario_after protocol

(** This tests that giving epoch (or any timestamps from the past) as
    the activation timestamp results in a immediate upgrade. *)
let test_kernel_upgrade_epoch =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "timestamp"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Upgrade immediatly when activation timestamp is epoch"
  @@ fun protocol ->
  let* _ =
    gen_test_kernel_upgrade
      ~activation_timestamp:"0"
      ~installee:Constant.WASM.debug_kernel
      protocol
  in
  unit

(** This tests that the kernel waits the activation timestamp to apply
    the upgrade.  *)
let test_kernel_upgrade_delay =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "upgrade"; "timestamp"; "delay"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.debug_kernel;
      ])
    ~title:"Upgrade after a delay when activation timestamp is in the future"
  @@ fun protocol ->
  let timestamp = Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z")) in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  (* It shoulnd't be upgrade in a single block, which {!gen_test_kernel_upgrade}
     expect. *)
  let* sc_rollup_node, node, client, evm_node, _ =
    gen_test_kernel_upgrade
      ~timestamp
      ~activation_timestamp
      ~installee:Constant.WASM.debug_kernel
      ~should_fail:true
      protocol
  in
  let* _ =
    repeat 5 (fun _ ->
        let* _ = next_evm_level ~sc_rollup_node ~node ~client ~evm_node in
        unit)
  in
  let kernel_debug_content = read_file (Uses.path Constant.WASM.debug_kernel) in
  let* kernel = get_kernel_boot_wasm ~sc_rollup_node in
  Check.((kernel <> kernel_debug_content) string)
    ~error_msg:(sf "The kernel hasn't upgraded") ;
  unit

let test_transaction_storage_before_and_after_migration =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "migration"; "transaction"; "storage"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.WASM.ghostnet_evm_kernel;
      ])
    ~title:"Transaction storage before and after migration"
  @@ fun protocol ->
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let raw_txs, tx_hashes = List.split txs in
  let scenario_prior ~evm_setup:{sc_rollup_node; node; client; evm_node; _} =
    let* _requests, _receipt, _hashes =
      send_n_transactions ~sc_rollup_node ~node ~client ~evm_node raw_txs
    in
    return ()
  in
  let scenario_after ~evm_setup ~sanity_check:() =
    let check_one tx_hash =
      let* _receipt =
        get_transaction_receipt ~full_evm_setup:evm_setup ~tx_hash
      in
      let* _tx_object = get_tx_object ~endpoint:evm_setup.endpoint ~tx_hash in
      unit
    in
    Lwt_list.iter_p check_one tx_hashes
  in
  gen_kernel_migration_test ~config ~scenario_prior ~scenario_after protocol

let register_evm_migration ~protocols =
  test_kernel_migration protocols ;
  test_deposit_before_and_after_migration protocols ;
  test_block_storage_before_and_after_migration protocols ;
  test_transaction_storage_before_and_after_migration protocols

let block_transaction_count_by ~by arg =
  let method_ = "eth_getBlockTransactionCountBy" ^ by_block_arg_string by in
  Evm_node.{method_; parameters = `A [`String arg]}

let get_block_transaction_count_by evm_node ~by arg =
  let* transaction_count =
    Evm_node.call_evm_rpc evm_node (block_transaction_count_by ~by arg)
  in
  return JSON.(transaction_count |-> "result" |> as_int64)

let test_rpc_getBlockTransactionCountBy =
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  register_both
    ~tags:["evm"; "get_block_transaction_count_by"]
    ~title:
      "RPC methods eth_getBlockTransactionCountByHash and \
       eth_getBlockTransactionCountByNumber"
    ~config
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 5) in
  let* _, receipt, _ =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_node
      (List.map fst txs)
  in
  let* block = get_block_by_hash evm_setup receipt.blockHash in
  let expected_count =
    match block.transactions with
    | Empty -> 0L
    | Hash l -> Int64.of_int @@ List.length l
    | Full l -> Int64.of_int @@ List.length l
  in
  let* transaction_count =
    get_block_transaction_count_by evm_node ~by:`Hash receipt.blockHash
  in
  Check.((transaction_count = expected_count) int64)
    ~error_msg:
      "Expected %R transactions with eth_getBlockTransactionCountByHash, but \
       got %L" ;
  let* transaction_count =
    get_block_transaction_count_by
      evm_node
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
  Evm_node.{method_; parameters = `A [`String arg]}

let get_uncle_count_by_block_arg evm_node ~by arg =
  let* uncle_count =
    Evm_node.call_evm_rpc evm_node (uncle_count_by_block_arg_request ~by arg)
  in
  return JSON.(uncle_count |-> "result" |> as_int64)

let test_rpc_getUncleCountByBlock =
  register_both
    ~tags:["evm"; "get_uncle_count_by_block"]
    ~title:
      "RPC methods eth_getUncleCountByBlockHash and \
       eth_getUncleCountByBlockNumber"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let* block = Eth_cli.get_block ~block_id:"0" ~endpoint:evm_node_endpoint in
  let* uncle_count =
    get_uncle_count_by_block_arg evm_node ~by:`Hash (Option.get block.hash)
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockHash, but got %L" ;
  let* uncle_count =
    get_uncle_count_by_block_arg
      evm_node
      ~by:`Number
      (Int32.to_string block.number)
  in
  Check.((uncle_count = Int64.zero) int64)
    ~error_msg:
      "Expected %R uncles with eth_getUncleCountByBlockNumber, but got %L" ;
  unit

let uncle_by_block_arg_and_index_request ~by arg index =
  let by = by_block_arg_string by in
  Evm_node.
    {
      method_ = "eth_getUncleByBlock" ^ by ^ "AndIndex";
      parameters = `A [`String arg; `String index];
    }

let get_uncle_by_block_arg_and_index ~by evm_node arg index =
  let* block =
    Evm_node.call_evm_rpc
      evm_node
      (uncle_by_block_arg_and_index_request ~by arg index)
  in
  let result = JSON.(block |-> "result") in
  if JSON.is_null result then return None
  else return @@ Some (result |> Block.of_json)

let test_rpc_getUncleByBlockArgAndIndex =
  register_both
    ~tags:["evm"; "get_uncle_by_block_arg_and_index"]
    ~title:
      "RPC methods eth_getUncleByBlockHashAndIndex and \
       eth_getUncleByBlockNumberAndIndex"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let evm_node_endpoint = Evm_node.endpoint evm_node in
  let block_id = "0" in
  let* block = Eth_cli.get_block ~block_id ~endpoint:evm_node_endpoint in
  let* uncle =
    get_uncle_by_block_arg_and_index
      ~by:`Hash
      evm_node
      (Option.get block.hash)
      block_id
  in
  assert (Option.is_none uncle) ;
  let* uncle =
    get_uncle_by_block_arg_and_index
      ~by:`Number
      evm_node
      (Int32.to_string block.number)
      block_id
  in
  assert (Option.is_none uncle) ;
  unit

let test_simulation_eip2200 =
  register_both
    ~tags:["evm"; "loop"; "simulation"; "eip2200"]
    ~title:"Simulation is EIP2200 resilient"
  @@ fun ~protocol:_ ~evm_setup ->
  let {sc_rollup_node; node; client; endpoint; evm_node; _} = evm_setup in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* loop_address, _tx = deploy ~contract:loop ~sender evm_setup in
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
  let* _tx =
    wait_for_application ~evm_node ~sc_rollup_node ~node ~client call ()
  in
  unit

let test_cover_fees =
  register_both
    ~tags:["evm"; "validity"]
    ~title:"Transaction is invalid if sender cannot cover the fees"
    ~bootstrap_accounts:[||]
  (* No bootstrap accounts, so no one has funds. *)
  @@ fun ~protocol:_
             ~evm_setup:{evm_node; endpoint; sc_rollup_node; node; client; _} ->
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1).  We do not use eth-cli in
     this test because we want the results of the simulation. *)
  let raw_transfer =
    "0xf86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  (* This should fail when the tx-pool check this. *)
  let* hash_result = send_raw_transaction evm_node raw_transfer in
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
          let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
          check_for_receipt (blocks_left - 1)
  in
  check_for_receipt 6

let test_rpc_sendRawTransaction_with_consecutive_nonce =
  register_both ~tags:["evm"; "tx_nonce"] ~title:"Can submit many transactions."
  @@ fun ~protocol:_ ~evm_setup:{evm_node; node; client; sc_rollup_node; _} ->
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6520 *)
  (* Nonce: 0*)
  let tx_1 =
    "0xf86480825208831e84809400000000000000000000000000000000000000008080820a96a0718d24970c6d2fc794e972f4319caf24a939ff3d822959c7e6b022813d16c8c4a04535ad83a67307759569b1e2087b0b79f80d4502027b6d1d52e3c072634b3f8b"
  in
  let* result = send_raw_transaction evm_node tx_1 in
  let hash_1 = Result.get_ok result in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6520 *)
  (* Nonce: 1*)
  let tx_2 =
    "0xf86401825208831e84809400000000000000000000000000000000000000008080820a95a01f47f2ec950d998bd99f7ff656a7f13a385603373f0e96130290ba2869f56515a018bd20697ab1f3cd82891663c62f514de7b2deeee2ed569e85b3aa351e1b1c3b"
  in
  let* result = send_raw_transaction evm_node tx_2 in
  let hash_2 = Result.get_ok result in
  let* _ =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_node ~transaction_hash:hash_1)
      ()
  in
  let* _ =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_node ~transaction_hash:hash_2)
      ()
  in
  unit

let test_rpc_sendRawTransaction_not_included =
  register_both
    ~tags:["evm"; "tx_nonce"]
    ~title:
      "Tx with nonce too high are not included without previous transactions."
  @@ fun ~protocol:_
             ~evm_setup:{evm_node; node; client; sc_rollup_node; endpoint; _} ->
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6520 *)
  (* Nonce: 1 *)
  let tx =
    "0xf86401825208831e84809400000000000000000000000000000000000000008080820a96a05709a6fbce9cf391d0530f4b4d4c9fd57fa160dd20fead5bd5c49c3ec78efcc9a06e4fcb1d5596e00bc34fa5d97ccafce8fa1f44534b36920d7db0a3ad29ca03f8"
  in
  let* result =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (fun () -> send_raw_transaction evm_node tx)
      ()
  in
  let tx_hash = Result.get_ok result in
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  (* Check if txs is not included *)
  let* receipt = Eth_cli.get_receipt ~endpoint ~tx:tx_hash in
  Check.((Option.is_none receipt = true) bool)
    ~error_msg:"Receipt should not be present" ;

  unit

let test_rpc_gasPrice =
  register_both ~tags:["evm"; "gas_price"] ~title:"RPC methods eth_gasPrice"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; _} ->
  let expected_gas_price = 21_000l in
  let* gas_price =
    Evm_node.(
      let* price =
        call_evm_rpc evm_node {method_ = "eth_gasPrice"; parameters = `A []}
      in
      return JSON.(price |-> "result" |> as_int64))
  in
  Check.((gas_price = Int64.of_int32 expected_gas_price) int64)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let send_foo_mapping_storage contract_address sender
    {sc_rollup_node; node; client; endpoint; evm_node; _} =
  let call_foo (sender : Eth_account.t) =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:mapping_storage.label
      ~address:contract_address
      ~method_call:"foo()"
  in
  wait_for_application
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    (call_foo sender)
    ()

let test_rpc_getStorageAt =
  register_both
    ~tags:["evm"; "get_storage_at"]
    ~title:"RPC methods eth_getStorageAt"
  @@ fun ~protocol:_ ~evm_setup ->
  let {endpoint; evm_node; _} = evm_setup in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* deploy contract *)
  let* address, _tx = deploy ~contract:mapping_storage ~sender evm_setup in
  (* Example from
      https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getstorageat
  *)
  let expected_value0 = 1234 in
  let expected_value1 = 5678 in

  (* set values *)
  let* tx = send_foo_mapping_storage address sender evm_setup in
  let* () = check_tx_succeeded ~endpoint ~tx in
  let* hex_value =
    Evm_node.(
      let* value =
        call_evm_rpc
          evm_node
          {
            method_ = "eth_getStorageAt";
            parameters = `A [`String address; `String "0x0"; `String "latest"];
          }
      in
      return JSON.(value |-> "result" |> as_string))
  in
  Check.((Helpers.no_0x hex_value = hex_256_of expected_value0) string)
    ~error_msg:"Expected %R, but got %L" ;
  let pos = Helpers.mapping_position sender.address 1 in
  let* hex_value =
    Evm_node.(
      let* value =
        call_evm_rpc
          evm_node
          {
            method_ = "eth_getStorageAt";
            parameters = `A [`String address; `String pos; `String "latest"];
          }
      in
      return JSON.(value |-> "result" |> as_string))
  in
  Check.((Helpers.no_0x hex_value = hex_256_of expected_value1) string)
    ~error_msg:"Expected %R, but got %L" ;
  unit

let test_accounts_double_indexing =
  register_proxy
    ~tags:["evm"; "accounts"; "index"]
    ~title:"Accounts have a unique index"
  @@ fun ~protocol:_ ~evm_setup:full_evm_setup ->
  let check_accounts_length expected_length =
    let* length =
      Sc_rollup_node.RPC.call full_evm_setup.sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:"/evm/indexes/accounts/length"
           ()
    in
    let length = Option.map Helpers.hex_string_to_int length in
    Check.((length = Some expected_length) (option int))
      ~error_msg:"Expected %R accounts, got %L" ;
    unit
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  (* Send a first transaction, there must be 2 indexes. *)
  let* _tx_hash = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
  let* () = check_accounts_length 2 in
  (* After a second transaction with the same accounts, there still must
     be 2 indexes. *)
  let* _tx_hash = send ~sender ~receiver ~value:Wei.one_eth full_evm_setup in
  let* () = check_accounts_length 2 in
  unit

let test_originate_evm_kernel_and_dump_pvm_state =
  register_proxy
    ~tags:["evm"]
    ~title:"Originate EVM kernel with installer and dump PVM state"
  @@ fun ~protocol:_ ~evm_setup:{node; client; sc_rollup_node; evm_node; _} ->
  (* First run of the installed EVM kernel, it will initialize the directory
     "eth_accounts". *)
  let* _level = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let dump = Temp.file "dump.json" in
  let* () = Sc_rollup_node.dump_durable_storage ~sc_rollup_node ~dump () in
  let installer = Installer_kernel_config.of_json dump in

  (* Check the consistency of the PVM state as queried by RPCs and the dumped PVM state. *)
  Lwt_list.iter_s
    (function
      (* We consider only the Set instruction because the dump durable storage
         command of the node produce only this instruction. *)
      | Installer_kernel_config.Set {value; to_} ->
          let* expected_value =
            Sc_rollup_node.RPC.call sc_rollup_node
            @@ Sc_rollup_rpc.get_global_block_durable_state_value
                 ~pvm_kind:"wasm_2_0_0"
                 ~operation:Sc_rollup_rpc.Value
                 ~key:to_
                 ()
          in
          let expected_value =
            match expected_value with
            | Some expected_value -> expected_value
            | None ->
                Test.fail "The key %S doesn't exist in the durable storage" to_
          in
          Check.((expected_value = value) string)
            ~error_msg:
              (sf "Value found in installer is %%R but expected %%L at %S" to_) ;
          unit
      | _ -> assert false)
    installer

(** Test that a contract can be called,
    and that the call can modify the storage.  *)
let test_l2_call_inter_contract =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_deploy"; "l2_call"; "inter_contract"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Check L2 inter contract call"
  @@ fun protocol ->
  (* setup *)
  let* ({evm_node; sc_rollup_node; node; client; _} as evm_setup) =
    setup_evm_kernel ~admin:None protocol
  in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in

  (* deploy Callee contract *)
  let* callee_address, _tx = deploy ~contract:callee ~sender evm_setup in

  (* set 20 directly in the Callee *)
  let* tx =
    let call_set_directly (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:callee.label
        ~address:callee_address
        ~method_call:(Printf.sprintf "setX(%d)" n)
    in
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_set_directly sender 20)
      ()
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address:callee_address 1 in
  let* () =
    check_nb_in_storage ~evm_setup ~address:callee_address ~nth:0 ~expected:20
  in

  (* deploy caller contract *)
  let* caller_address, _tx = deploy ~contract:caller ~sender evm_setup in

  (* set 10 through the caller *)
  let* tx =
    let call_set_from_caller (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:caller.label
        ~address:caller_address
        ~method_call:(Printf.sprintf "setX(\"%s\", %d)" callee_address n)
    in
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_set_from_caller sender 10)
      ()
  in

  let* () = check_tx_succeeded ~endpoint ~tx in
  let* () = check_storage_size sc_rollup_node ~address:callee_address 1 in
  let* () =
    check_nb_in_storage ~evm_setup ~address:callee_address ~nth:0 ~expected:10
  in
  unit

let get_logs_request ?from_block ?to_block ?address ?topics () =
  let parse_topic = function
    | [] -> `Null
    | [t] -> `String t
    | l -> `A (List.map (fun s -> `String s) l)
  in
  let parameters : JSON.u =
    `A
      [
        `O
          (Option.fold
             ~none:[]
             ~some:(fun f -> [("fromBlock", `String f)])
             from_block
          @ Option.fold
              ~none:[]
              ~some:(fun t -> [("toBlock", `String t)])
              to_block
          @ Option.fold
              ~none:[]
              ~some:(fun a -> [("address", `String a)])
              address
          @ Option.fold
              ~none:[]
              ~some:(fun t -> [("topics", `A (List.map parse_topic t))])
              topics);
      ]
  in
  Evm_node.{method_ = "eth_getLogs"; parameters}

let get_logs ?from_block ?to_block ?address ?topics proxy_server =
  let* response =
    Evm_node.call_evm_rpc
      proxy_server
      (get_logs_request ?from_block ?to_block ?address ?topics ())
  in
  return
    JSON.(response |-> "result" |> as_list |> List.map Transaction.logs_of_json)

let test_rpc_getLogs =
  register_both ~tags:["evm"; "rpc"; "get_logs"] ~title:"Check getLogs RPC"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; node; client; sc_rollup_node; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let player = Eth_account.bootstrap_accounts.(1) in
  (* deploy the contract *)
  let* address, _tx = deploy ~contract:erc20 ~sender evm_setup in
  let address = String.lowercase_ascii address in
  Check.(
    (address = "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")
      string
      ~error_msg:"Expected address to be %R but was %L.") ;
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
  let burn_logs sender amount =
    [
      ( address,
        [transfer_event_topic; hex_256_of_address sender; zero_address],
        "0x" ^ hex_256_of amount );
    ]
  in
  (* sender mints 42 *)
  let* tx1 =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_mint sender 42)
      ()
  in
  (* player mints 100 *)
  let* _tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_mint player 100)
      ()
  in
  (* sender burns 42 *)
  let* _tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_burn sender 42)
      ()
  in
  (* Check that there have been 3 logs in total *)
  let* all_logs = get_logs ~from_block:"0" evm_node in
  Check.((List.length all_logs = 3) int) ~error_msg:"Expected %R logs, got %L" ;
  (* Check that the [address] contract has produced 3 logs in total *)
  let* contract_logs = get_logs ~from_block:"0" ~address evm_node in
  Check.((List.length contract_logs = 3) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check that there have been 3 logs with the transfer event topic *)
  let* transfer_logs =
    get_logs ~from_block:"0" ~topics:[[transfer_event_topic]] evm_node
  in
  Check.((List.length transfer_logs = 3) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check that [sender] appears in 2 logs.
     Note: this would also match on a transfer from zero to zero. *)
  let* sender_logs =
    get_logs
      ~from_block:"0"
      ~topics:
        [
          [];
          [hex_256_of_address sender; zero_address];
          [hex_256_of_address sender; zero_address];
        ]
      evm_node
  in
  Check.((List.length sender_logs = 2) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Look for a specific log, for the sender burn. *)
  let* sender_burn_logs =
    get_logs
      ~from_block:"0"
      ~topics:
        [[transfer_event_topic]; [hex_256_of_address sender]; [zero_address]]
      evm_node
  in
  Check.(
    (List.map Transaction.extract_log_body sender_burn_logs
    = burn_logs sender 42)
      (list (tuple3 string (list string) string)))
    ~error_msg:"Expected logs %R, got %L" ;
  (* Check that a specific block has a log *)
  let* tx1_receipt =
    get_transaction_receipt ~full_evm_setup:evm_setup ~tx_hash:tx1
  in
  let* tx1_block_logs =
    get_logs
      ~from_block:(Int32.to_string tx1_receipt.blockNumber)
      ~to_block:(Int32.to_string tx1_receipt.blockNumber)
      evm_node
  in
  Check.((List.length tx1_block_logs = 1) int)
    ~error_msg:"Expected %R logs, got %L" ;
  (* Check no logs after transactions *)
  let* no_logs_start = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let* new_logs = get_logs ~from_block:(string_of_int no_logs_start) evm_node in
  Check.((List.length new_logs = 0) int) ~error_msg:"Expected %R logs, got %L" ;
  unit

let test_tx_pool_replacing_transactions =
  register_both ~tags:["evm"; "tx_pool"] ~title:"Transactions can be replaced"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  let bob = Eth_account.bootstrap_accounts.(0) in
  let* bob_nonce = get_transaction_count evm_node bob.address in
  (* nonce: 0, private_key: bootstrappe_account(0), amount: 10; max_fees: 21000*)
  let tx_a =
    "0xf86b80825208825208940000000000000000000000000000000000000000888ac7230489e8000080820a95a05fc733145b2066166e074bc42239a7312b2358f5cbf9ce17bab404abd1dfaff0a0493e763aa933d3eb724d75f9ad6fb4bbffdf3d54568d44d6f70cfcf0a07dc4f8"
  in
  (* nonce: 0, private_key: bootstrappe_account(0), amount: 5; max_fees: 30000*)
  let tx_b =
    "0xf86b80827530825208940000000000000000000000000000000000000000884563918244f4000080820a96a008410806e7a3c6b403bbfa99d82886e5460921a664410eaea5fe99050c4dc63da031c3eb45ac8a42600b27029d1c910b4c0006f1f435a29f91626964a8cf25da3f"
  in
  (* Send the transactions to the proxy*)
  let* result = send_raw_transaction evm_node tx_a in
  let _tx_a_hash = Result.get_ok result in
  let* result = send_raw_transaction evm_node tx_b in
  let tx_b_hash = Result.get_ok result in
  let* receipt =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_node ~transaction_hash:tx_b_hash)
      ()
  in
  let* new_bob_nonce = get_transaction_count evm_node bob.address in
  Check.((receipt.status = true) bool) ~error_msg:"Transaction has failed" ;
  Check.((new_bob_nonce = Int64.(add bob_nonce one)) int64)
    ~error_msg:"Bob has sent more than one transaction" ;
  unit

let test_l2_nested_create =
  register_both
    ~tags:["evm"; "l2_deploy"; "l2_create"; "inter_contract"]
    ~title:"Check L2 nested create"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* nested_create_address, _tx =
    deploy ~contract:nested_create ~sender evm_setup
  in
  let* tx1 =
    let call_create (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:nested_create.label
        ~address:nested_create_address
        ~method_call:(Printf.sprintf "create(%d)" n)
    in
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_create sender 1)
      ()
  in
  let* tx2 =
    let call_create (sender : Eth_account.t) n salt =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:nested_create.label
        ~address:nested_create_address
        ~method_call:(Printf.sprintf "create2(%d, \"%s\")" n salt)
    in
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_create sender 1 "0x")
      ()
  in
  let* () = check_tx_succeeded ~endpoint ~tx:tx1 in
  let* () = check_tx_succeeded ~endpoint ~tx:tx2 in
  unit

let test_block_hash_regression =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "block"; "hash"; "regression"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Regression test for L2 block hash"
  @@ fun protocol ->
  let config =
    `Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml")
  in
  (* We use a timestamp equal to the next day after genesis.
     The genesis timestamp can be found in tezt/lib_tezos/client.ml *)
  let* {evm_node; sc_rollup_node; node; client; _} =
    setup_evm_kernel
      ~config
      ~admin:None
      ~timestamp:(At (Option.get @@ Ptime.of_date (2018, 7, 1)))
      protocol
  in
  let txs = read_tx_from_file () |> List.filteri (fun i _ -> i < 3) in
  let raw_txs, _tx_hashes = List.split txs in
  let* _requests, receipt, _hashes =
    send_n_transactions ~sc_rollup_node ~node ~client ~evm_node raw_txs
  in
  Regression.capture @@ sf "Block hash: %s" receipt.blockHash ;
  unit

let test_l2_revert_returns_unused_gas =
  register_both ~tags:["evm"] ~title:"Check L2 revert returns unused gas"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* _revert_address, _tx = deploy ~contract:revert ~sender evm_setup in
  (* Tx data is constructed by:
     cd src/kernel_evm/benchmarks/scripts
     node sign_tx.js ../../../../etherlink/tezt/tests/evm_kernel_inputs/call_revert.json "9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0"
  *)
  let tx =
    "0xf8690183010000830186a094d77420f73b4612a7a99dba8c2afd30a1886b03448084c0406226820a96a0869b3a97d2c87d41c22eaeafba2644c276e74267998dff3504d1d2b35fae0e2ba058f0661adcff7d2abd3c6eb4d663e4731c838f6ef15ebb797b88db87c4fee39b"
  in
  let* balance_before = Eth_cli.balance ~account:sender.address ~endpoint in
  let* result = send_raw_transaction evm_node tx in
  let transaction_hash = Result.get_ok result in
  let* transaction_receipt =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (wait_for_transaction_receipt ~evm_node ~transaction_hash)
      ()
  in
  let gas_used = transaction_receipt.gasUsed in
  let* () = check_tx_failed ~endpoint ~tx:transaction_hash in
  Check.((gas_used < 100000l) int32)
    ~error_msg:"Expected gas usage less than %R logs, got %L" ;
  let* balance_after = Eth_cli.balance ~account:sender.address ~endpoint in
  let gas_fee_paid = Wei.(balance_before - balance_after) in
  let gas_price = transaction_receipt.effectiveGasPrice in
  let expected_gas_fee_paid = expected_gas_fees ~gas_price ~gas_used in
  Check.((expected_gas_fee_paid = gas_fee_paid) Wei.typ)
    ~error_msg:"Expected gas fee paid to be %L, got %R" ;
  unit

let test_l2_create_collision =
  register_both
    ~tags:["evm"; "l2_create"; "collision"]
    ~title:"Check L2 create collision"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* create2_address, _tx = deploy ~contract:create2 ~sender evm_setup in

  let call_create2 (sender : Eth_account.t) ~expect_failure =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:create2.label
      ~address:create2_address
      ~method_call:(Printf.sprintf "create2()")
  in

  let* tx1 =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_create2 sender ~expect_failure:false)
      ()
  in

  let* tx2 =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_create2 sender ~expect_failure:true)
      ()
  in

  let* () = check_tx_succeeded ~tx:tx1 ~endpoint in

  (* Eth-cli would wrap receipt of failed transaction in its own error message.
      Since we currently don't parse error message from eth-cli,
      we cannot fetch the tx_hash in this case and would get an empty string as tx_hash for failed transaction.
      After handling of failed transaction is correctly implemented, the following check would need to be replaced with
      let* () = check_tx_failed ~tx:tx2 ~endpoint in *)
  Check.(
    (tx2 = "")
      string
      ~error_msg:"Expected returned transaction hash to be %R but was %L.") ;
  unit

let test_l2_intermediate_OOG_call =
  register_both
    ~tags:["evm"; "out_of_gas"; "call"]
    ~title:
      "Check that an L2 call to a smart contract with an intermediate call \
       that runs out of gas still succeeds."
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* random_contract_address, _tx =
    deploy ~contract:simple_storage ~sender evm_setup
  in
  let* oog_call_address, _tx = deploy ~contract:oog_call ~sender evm_setup in
  let call_oog (sender : Eth_account.t) ~expect_failure =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:oog_call.label
      ~address:oog_call_address
      ~method_call:
        (Printf.sprintf "sendViaCall(\"%s\")" random_contract_address)
  in
  let* tx =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_oog sender ~expect_failure:false)
      ()
  in
  check_tx_succeeded ~tx ~endpoint

let test_l2_ether_wallet =
  register_both
    ~tags:["evm"; "l2_call"; "wallet"]
    ~title:"Check ether wallet functions correctly"
  @@ fun ~protocol:_ ~evm_setup ->
  let {evm_node; sc_rollup_node; node; client; _} = evm_setup in
  let endpoint = Evm_node.endpoint evm_node in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* ether_wallet_address, _tx =
    deploy ~contract:ether_wallet ~sender evm_setup
  in
  let* tx1 =
    let transaction =
      Eth_cli.transaction_send
        ~source_private_key:sender.private_key
        ~to_public_key:ether_wallet_address
        ~value:(Wei.of_eth_int 100)
        ~endpoint
    in
    wait_for_application ~evm_node ~sc_rollup_node ~node ~client transaction ()
  in
  let* tx2 =
    let call_withdraw (sender : Eth_account.t) n =
      Eth_cli.contract_send
        ~source_private_key:sender.private_key
        ~endpoint
        ~abi_label:ether_wallet.label
        ~address:ether_wallet_address
        ~method_call:(Printf.sprintf "withdraw(%d)" n)
    in
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
      (call_withdraw sender 100)
      ()
  in
  let* () = check_tx_succeeded ~endpoint ~tx:tx1 in
  let* () = check_tx_succeeded ~endpoint ~tx:tx2 in
  unit

let test_keep_alive =
  Protocol.register_test
    ~__FILE__
    ~tags:["keep_alive"; "proxy"]
    ~title:"Proxy mode keep alive argument"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    (fun protocol ->
      let* {sc_rollup_node; sc_rollup_address; evm_node; endpoint = _; _} =
        setup_evm_kernel ~admin:None protocol
      in
      (* Stop the EVM and rollup nodes. *)
      let* () = Evm_node.terminate evm_node in
      let* () = Sc_rollup_node.terminate sc_rollup_node in
      (* Restart the evm node without keep alive, expected to fail. *)
      let process = Evm_node.spawn_run evm_node in
      let* () =
        Process.check_error ~msg:(rex "the communication was lost") process
      in
      (* Restart with keep alive. The EVM node is waiting for the connection. *)
      let* () =
        Evm_node.run ~wait:false ~extra_arguments:["--keep-alive"] evm_node
      in
      let* () =
        Evm_node.wait_for evm_node "evm_node_retrying_connect.v0"
        @@ fun _json -> Some ()
      in
      (* Restart the rollup node to restore the connection. *)
      let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
      let* () = Evm_node.wait_for_ready evm_node in
      (* The EVM node should respond to RPCs. *)
      let*@ _block_number = Rpc.block_number evm_node in
      (* Stop the rollup node, the EVM node no longer properly respond to RPCs. *)
      let* () = Sc_rollup_node.terminate sc_rollup_node in
      let*@? error = Rpc.block_number evm_node in
      Check.(error.message =~ rex "the communication was lost")
        ~error_msg:
          "The RPC was supposed to failed because of lost communication" ;
      (* Restart the EVM node, do the same RPC. *)
      let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
      let*@ _block_number = Rpc.block_number evm_node in
      unit)

let test_regression_block_hash_gen =
  (* This test is created because of bug in blockConstant in simulation,
     which caused the simulation to return a wrong estimate of gas limit,
     leading to failed contract deployment for block_hash_gen.
     This test checks regression for the fix *)
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "l2_call"; "block_hash"; "timestamp"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Random generation based on block hash and timestamp"
  @@ fun protocol ->
  let* ({evm_node; sc_rollup_node; node; client; _} as evm_setup) =
    setup_evm_kernel ~admin:None protocol
  in
  let* _ = next_evm_level ~evm_node ~sc_rollup_node ~node ~client in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* _address, _tx = deploy ~contract:block_hash_gen ~sender evm_setup in
  unit

let test_reboot_out_of_ticks =
  register_proxy
    ~tags:["evm"; "reboot"; "loop"; "out_of_ticks"]
    ~title:
      "Check that the kernel can handle transactions that take too many ticks \
       for a single run"
  @@ fun ~protocol:_ ~evm_setup:{evm_node; sc_rollup_node; node; client; _} ->
  (* Retrieves all the messages and prepare them for the current rollup. *)
  let txs =
    read_file (kernel_inputs_path ^ "/loops-out-of-ticks")
    |> String.trim |> String.split_on_char '\n'
  in
  (* The first three transactions are sent in a separate block, to handle any nonce issue. *)
  let first_block, second_block =
    match txs with
    | faucet1 :: faucet2 :: create :: rem -> ([faucet1; faucet2; create], rem)
    | _ ->
        failwith
          "The prepared transactions should contain at least 3 transactions"
  in
  let* _requests, _receipt, _hashes =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_node
      ~wait_for_blocks:5
      first_block
  in
  let* total_tick_number_before_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* l1_level_before_out_of_ticks = Node.get_level node in
  let* requests, receipt, _hashes =
    send_n_transactions
      ~sc_rollup_node
      ~node
      ~client
      ~evm_node
      ~wait_for_blocks:5
        (* By default, it waits for 3 blocks. We need to take into account the
           blocks before the inclusion which is generally 2. The loops can be a
           bit long to execute, as such the inclusion test might fail before the
           execution is over, making it flaky. *)
      second_block
  in
  let* total_tick_number_with_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let*@ block_with_out_of_ticks =
    Rpc.get_block_by_number
      ~block:(Format.sprintf "%#lx" receipt.blockNumber)
      evm_node
  in
  (* Check that all the transactions are actually included in the same block,
     otherwise it wouldn't make sense to continue. *)
  (match block_with_out_of_ticks.Block.transactions with
  | Block.Empty -> Test.fail "Expected a non empty block"
  | Block.Full _ ->
      Test.fail "Block is supposed to contain only transaction hashes"
  | Block.Hash hashes ->
      Check.((List.length hashes = List.length requests) int)
        ~error_msg:"Expected %R transactions in the resulting block, got %L") ;

  (* Check the number of ticks spent during the period when there should have
     been a reboot due to out of ticks. There have been a reboot if the number
     of ticks is not `number of blocks` * `ticks per l1 level`. *)
  let* l1_level_after_out_of_ticks = Node.get_level node in
  let number_of_blocks =
    l1_level_after_out_of_ticks - l1_level_before_out_of_ticks
  in
  let ticks_after_expected_reboot =
    total_tick_number_with_expected_reboots
    - total_tick_number_before_expected_reboots
  in

  (* The PVM takes 11G ticks for collecting inputs, 11G for a kernel_run. As such,
     an L1 level is at least 22G ticks. *)
  let ticks_per_snapshot =
    Tezos_protocol_alpha.Protocol.Sc_rollup_wasm.V2_0_0.ticks_per_snapshot
    |> Z.to_int
  in
  let min_ticks_per_l1_level = ticks_per_snapshot * 2 in
  Check.(
    (ticks_after_expected_reboot
    = (min_ticks_per_l1_level * number_of_blocks) + ticks_per_snapshot)
      int)
    ~error_msg:
      "The number of ticks spent during the period should be %R, but got %L, \
       which implies there have been no reboot, contrary to what was expected." ;
  unit

let register_evm_node ~protocols =
  test_originate_evm_kernel protocols ;
  test_evm_node_connection protocols ;
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
  test_deploy_contract_for_shanghai protocols ;
  test_inject_100_transactions protocols ;
  test_eth_call_storage_contract protocols ;
  test_eth_call_storage_contract_eth_cli protocols ;
  test_eth_call_large protocols ;
  test_preinitialized_evm_kernel protocols ;
  test_deposit_and_withdraw protocols ;
  test_estimate_gas protocols ;
  test_estimate_gas_additionnal_field protocols ;
  test_kernel_upgrade_epoch protocols ;
  test_kernel_upgrade_delay protocols ;
  test_kernel_upgrade_evm_to_evm protocols ;
  test_kernel_upgrade_wrong_key protocols ;
  test_kernel_upgrade_wrong_rollup_address protocols ;
  test_kernel_upgrade_no_administrator protocols ;
  test_kernel_upgrade_failing_migration protocols ;
  test_kernel_upgrade_version_change protocols ;
  test_rpc_sendRawTransaction protocols ;
  test_deposit_dailynet protocols ;
  test_rpc_sendRawTransaction_nonce_too_low protocols ;
  test_rpc_sendRawTransaction_nonce_too_high protocols ;
  test_rpc_sendRawTransaction_invalid_chain_id protocols ;
  test_rpc_getTransactionByBlockHashAndIndex protocols ;
  test_rpc_getTransactionByBlockNumberAndIndex protocols ;
  test_rpc_getBlockTransactionCountBy protocols ;
  test_rpc_getUncleCountByBlock protocols ;
  test_rpc_getUncleByBlockArgAndIndex protocols ;
  test_simulation_eip2200 protocols ;
  test_cover_fees protocols ;
  test_rpc_gasPrice protocols ;
  test_rpc_getStorageAt protocols ;
  test_accounts_double_indexing protocols ;
  test_validation_result protocols ;
  test_rpc_sendRawTransaction_with_consecutive_nonce protocols ;
  test_rpc_sendRawTransaction_not_included protocols ;
  test_originate_evm_kernel_and_dump_pvm_state protocols ;
  test_l2_call_inter_contract protocols ;
  test_rpc_getLogs protocols ;
  test_log_index protocols ;
  test_tx_pool_replacing_transactions protocols ;
  test_l2_nested_create protocols ;
  test_block_hash_regression protocols ;
  test_l2_revert_returns_unused_gas protocols ;
  test_l2_create_collision protocols ;
  test_l2_intermediate_OOG_call protocols ;
  test_l2_ether_wallet protocols ;
  test_keep_alive protocols ;
  test_regression_block_hash_gen protocols ;
  test_reboot_out_of_ticks protocols

let () =
  register_evm_node ~protocols:[Alpha] ;
  register_evm_migration ~protocols:[Alpha]
