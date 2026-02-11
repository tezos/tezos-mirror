(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tezosx.ml
 *)

open Rpc.Syntax

let runtime_tags = List.map Tezosx_runtime.tag

(* /!\ Read this before adding a new test

   By default, to write a test, you should use [register_sandbox_test]. Only
   for tests actually requiring to consider L1/L2 communication (typically,
   related to the bridge, or when we need to distinguish between the latest and
   finalized block) is it useful to use [register_fullstack_test]. *)

module Setup = struct
  let register_sandbox_test ?uses_client ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Test_helpers.register_sandbox
      ~__FILE__
      ?uses_client
      ?tez_bootstrap_accounts
      ~kernel:Latest
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~with_runtimes

  let register_fullstack_test ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts =
    Setup.register_test
      ~__FILE__
      ~rpc_server:Evm_node.Resto
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~kernel:Latest
      ~with_runtimes
      ~enable_dal:false
      ~enable_multichain:false
      ?tez_bootstrap_accounts
end

let test_runtime_feature_flag ~runtime () =
  Setup.register_sandbox_test
    ~title:"Set Tezos runtime feature flag"
    ~tags:["feature_flag"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let* rpc_result =
    Rpc.state_value sandbox (Tezosx_runtime.feature_flag runtime)
  in
  match rpc_result with
  | Ok (Some _) -> unit
  | Ok None ->
      Test.fail
        "Feature flag for the %s runtime was not set"
        (String.capitalize_ascii (Tezosx_runtime.to_string runtime))
  | Error err ->
      Test.fail
        "Could not read feature flag %s: %s"
        (Tezosx_runtime.feature_flag runtime)
        err.message

let tezos_client node =
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.{(Evm_node.rpc_endpoint_record node) with path = "/tezlink"})
  in
  Client.init ~endpoint ()

let check_account sandbox (account : Account.key) =
  let* client = tezos_client sandbox in
  let* _balance =
    Client.get_balance_for ~account:account.public_key_hash client
  in
  unit

let account_str_rpc sequencer account key =
  let path =
    sf "/tezlink/chains/main/blocks/head/context/contracts/%s/%s" account key
  in

  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      ~args:["-v"]
      (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  return @@ JSON.parse ~origin:"curl_protocols" res

let account_rpc sequencer account key =
  account_str_rpc sequencer account.Account.public_key_hash key

(** Durable storage path where Michelson originated contracts are indexed in
    Tezos X. Each subkey is the hex-encoded [Contract_repr.t] of the
    Michelson contract. *)
let tezosx_michelson_contracts_index = "/evm/world_state/contracts/index"

(** [read_michelson_contract_storage sc_rollup_node contract_hex] reads the
    storage of an originated Michelson contract from the durable storage,
    given its hex-encoded [Contract_repr.t] key. *)
let read_michelson_contract_storage sc_rollup_node contract_hex =
  let path =
    sf "%s/%s/data/storage" tezosx_michelson_contracts_index contract_hex
  in
  Sc_rollup_node.RPC.call sc_rollup_node
  @@ Sc_rollup_rpc.get_global_block_durable_state_value
       ~pvm_kind:"wasm_2_0_0"
       ~operation:Sc_rollup_rpc.Value
       ~key:path
       ()

(** [decode_michelson_contract_address hex] decodes a hex-encoded
    [Contract_repr.t] into a b58check KT1 address string. *)
let decode_michelson_contract_address hex =
  let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
  Hex.to_bytes (`Hex hex)
  |> Data_encoding.Binary.of_bytes_exn C.encoding
  |> C.to_b58check

(** [decode_micheline_storage hex_str] decodes a hex string from the durable
    storage into a Micheline [JSON.u] value. Returns [None] if decoding
    fails. *)
let decode_micheline_storage hex_str =
  let module S = Tezos_protocol_alpha.Protocol.Script_repr in
  Data_encoding.Binary.of_bytes_opt
    S.expr_encoding
    (Hex.to_bytes (`Hex hex_str))
  |> Option.map (fun e ->
         Data_encoding.Json.(
           construct S.expr_encoding e
           |> to_string
           |> JSON.parse ~origin:"decode_micheline_storage"))

(** [send_tezos_op_to_delayed_inbox_and_wait] sends a Tezos operation via the
    delayed inbox and waits until it is included and the delayed inbox is
    empty. *)
let send_tezos_op_to_delayed_inbox_and_wait ~sc_rollup_address ~sc_rollup_node
    ~client ~l1_contracts ~sequencer operation =
  let* _hash =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      operation
  and* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node)

(** [originate_michelson_contract_via_delayed_inbox] originates a Michelson
    contract via the delayed inbox. Loads the script from
    [michelson_test_scripts], converts code and initial storage to JSON,
    forges and sends the origination operation, then returns the hex key and
    KT1 address of the new contract. *)
let originate_michelson_contract_via_delayed_inbox ~sc_rollup_address
    ~sc_rollup_node ~client ~l1_contracts ~sequencer ~source ~counter
    ~script_name ~init_storage_data ?(init_balance = 0) protocol =
  let* contracts_before =
    Delayed_inbox.subkeys
      tezosx_michelson_contracts_index
      (Sc_rollup_node sc_rollup_node)
  in
  let script_path = Michelson_script.(find script_name protocol |> path) in
  let* code = Client.convert_script_to_json ~script:script_path client in
  let* init_storage =
    Client.convert_data_to_json ~data:init_storage_data client
  in
  let* origination_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter
            ~gas_limit:10000
            ~storage_limit:1000
            ~source
            (origination ~code ~init_storage ~init_balance ());
        ])
      client
  in
  let* () =
    send_tezos_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      origination_op
  in
  let* contracts_after =
    Delayed_inbox.subkeys
      tezosx_michelson_contracts_index
      (Sc_rollup_node sc_rollup_node)
  in
  let new_contracts =
    List.filter (fun c -> not (List.mem c contracts_before)) contracts_after
  in
  Check.(
    (List.length new_contracts = 1)
      int
      ~error_msg:"Expected %R new contract but got %L") ;
  let contract_hex = List.hd new_contracts in
  let kt1_address = decode_michelson_contract_address contract_hex in
  Log.info "Originated contract: %s" kt1_address ;
  return (contract_hex, kt1_address)

(** [call_michelson_contract_via_delayed_inbox] calls a Michelson contract
    via the delayed inbox. Converts the argument from Michelson notation to
    JSON, forges and sends the call operation. *)
let call_michelson_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node
    ~client ~l1_contracts ~sequencer ~source ~counter ~dest ~arg_data
    ?(entrypoint = "default") ?(amount = 0) () =
  let* arg = Client.convert_data_to_json ~data:arg_data client in
  let* call_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter
            ~gas_limit:10000
            ~storage_limit:1000
            ~source
            (call ~dest ~arg ~entrypoint ~amount ());
        ])
      client
  in
  send_tezos_op_to_delayed_inbox_and_wait
    ~sc_rollup_address
    ~sc_rollup_node
    ~client
    ~l1_contracts
    ~sequencer
    call_op

(** [check_michelson_storage_value] reads the storage of a Michelson contract
    from the durable storage and checks that it matches the expected JSON
    value. *)
let check_michelson_storage_value ~sc_rollup_node ~contract_hex ~expected () =
  let* storage_raw =
    read_michelson_contract_storage sc_rollup_node contract_hex
  in
  match storage_raw with
  | None ->
      let kt1 = decode_michelson_contract_address contract_hex in
      Test.fail "Storage not found for contract %s" kt1
  | Some hex_str ->
      let expected =
        JSON.annotate ~origin:"check_michelson_storage_value" expected
        |> Option.some
      in
      Check.(
        (decode_micheline_storage hex_str = expected)
          (option json)
          ~error_msg:"Expected storage %R but got %L") ;
      unit

(** [with_check_source_delta_balance ~source ~tez_client ~expected_consumed action]
    reads the balance of [source] before and after running [action ()], then
    checks that exactly [expected_consumed] mutez were consumed. The balance
    check always runs, even if [action] fails. Returns the result of
    [action]. *)
let with_check_source_delta_balance ~source ~tez_client ~expected_consumed
    action =
  let* balance_before =
    Client.get_balance_for ~account:source.Account.public_key_hash tez_client
  in
  Lwt.finalize action (fun () ->
      let* balance_after =
        Client.get_balance_for
          ~account:source.Account.public_key_hash
          tez_client
      in
      let consumed = Tez.to_mutez balance_before - Tez.to_mutez balance_after in
      Log.info
        "Balance consumed: %d mutez (expected %d)"
        consumed
        expected_consumed ;
      Check.(
        (consumed = expected_consumed)
          int
          ~error_msg:"Expected %R mutez consumed but got %L") ;
      Lwt.return_unit)

let test_bootstrap_kernel_config () =
  let tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts in
  Setup.register_sandbox_test
    ~tez_bootstrap_accounts
    ~title:"Set tezos bootstrap accounts"
    ~uses_client:true
    ~tags:["bootstrap"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  Lwt_list.iter_p (check_account sandbox) tez_bootstrap_accounts

let deposit ~(l1_contracts : Tezt_etherlink.Setup.l1_contracts)
    ~sc_rollup_address ~sc_rollup_node ~sequencer ~client ~depositor
    ~(receiver_account : Account.key) ~amount =
  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         receiver_account.public_key_hash
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Tezos_protocol_alpha.Protocol.Contract_repr.encoding
      receiver
  in
  let (`Hex receiver) = Hex.of_bytes bytes in
  let deposit_info =
    Delayed_inbox.{receiver = TezosAddr receiver; chain_id = None}
  in
  let* () =
    Delayed_inbox.send_deposit_to_delayed_inbox
      ~rlp:true
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  unit

let test_reveal =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Reveal tezos native account"
    ~tags:["reveal"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@
  fun {client; sc_rollup_node; sc_rollup_address; sequencer; l1_contracts; _}
      _protocol
    ->
  (* Make a deposit to have an unrevealed account *)
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let receiver_account = Constant.bootstrap2 in

  let* () =
    deposit
      ~l1_contracts
      ~sc_rollup_address
      ~sc_rollup_node
      ~sequencer
      ~client
      ~depositor
      ~receiver_account
      ~amount
  in
  let* reveal =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source:receiver_account
            (reveal receiver_account ());
        ])
      client
  in
  let* () =
    send_tezos_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      reveal
  in
  let* manager_key = account_rpc sequencer receiver_account "manager_key" in
  Check.(
    JSON.(manager_key |> as_string_opt = Some Constant.bootstrap2.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_transfer =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Transfer on tezos native account"
    ~tags:["transfer"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let* transfer =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source:depositor
            (transfer ~dest:receiver_account ~amount:(Tez.to_mutez amount) ());
        ])
      client
  in
  let* () =
    send_tezos_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      transfer
  in
  let* client = tezos_client sequencer in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash client
  in
  Check.(
    (Tez.to_mutez balance = 1_000_000_000)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

let test_deposit =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Deposit on tezos native account"
    ~tags:["deposit"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let* () =
    deposit
      ~l1_contracts
      ~sc_rollup_address
      ~sc_rollup_node
      ~sequencer
      ~client
      ~depositor
      ~receiver_account
      ~amount
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* client = tezos_client sequencer in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash client
  in
  Check.(
    (Tez.to_mutez balance = 1_000_000_000)
      int
      ~error_msg:"Expected %R mutez but got %L") ;

  (* We expect the deposit to be in the latest blueprint, but the latest
       block should be empty from the POV of Etherlink *)
  let* head = Rpc.get_block_by_number ~block:"latest" sequencer in
  let block_infos =
    match head with
    | Ok block -> block
    | Error _ -> failwith "Should have find latest block"
  in
  match block_infos.transactions with
  | Empty -> unit
  | _ -> failwith "Latest block should be empty"

(** Originate a Michelson contract via the delayed inbox, call it, and verify
    that its storage is updated.

    Uses [store_input.tz] from [michelson_test_scripts/opcodes]: a minimal
    contract that stores its string parameter as the new storage.

    Steps:
    1. Originate the store_input contract with an initial balance,
       check that exactly fee + init_balance + origination burn was consumed.
    2. Call the contract with a string parameter and some XTZ,
       check that exactly fee + call_amount was consumed.
    3. Read the contract storage and verify it matches the parameter *)
let test_michelson_origination_and_call =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson origination and call on tezos X"
    ~tags:["origination"; "call"; "michelson"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in

  (* Step 1: Originate a Michelson contract (store_input.tz) *)
  let init_balance = 1_234_567 in
  let* contract_hex, kt1_address =
    with_check_source_delta_balance
      ~source
      ~tez_client
        (* 72500 = origination burn: 257 * 250 (base) + 33 * 250 (script) *)
      ~expected_consumed:(1000 + init_balance + 72500)
      (fun () ->
        originate_michelson_contract_via_delayed_inbox
          ~sc_rollup_address
          ~sc_rollup_node
          ~client
          ~l1_contracts
          ~sequencer
          ~source
          ~counter:1
          ~script_name:["opcodes"; "store_input"]
          ~init_storage_data:{|""|}
          ~init_balance
          protocol)
  in

  (* Step 2: Call the contract with a string parameter and send XTZ *)
  let expected_storage = "Hello Tezos X" in
  let call_amount = 1_234_567 in
  let* () =
    with_check_source_delta_balance
      ~source
      ~tez_client
      ~expected_consumed:(1000 + call_amount)
      (fun () ->
        call_michelson_contract_via_delayed_inbox
          ~sc_rollup_address
          ~sc_rollup_node
          ~client
          ~l1_contracts
          ~sequencer
          ~source
          ~counter:2
          ~dest:kt1_address
          ~arg_data:(sf {|"%s"|} expected_storage)
          ~amount:call_amount
          ())
  in

  (* Step 3: Read the contract storage and verify *)
  check_michelson_storage_value
    ~sc_rollup_node
    ~contract_hex
    ~expected:(`O [("string", `String expected_storage)])
    ()

(** Call a non-existing KT1 address via the delayed inbox. The operation
    should be included but fail: only fees are consumed, the transferred
    amount is not deducted from the sender's balance. *)
let test_michelson_call_nonexistent_contract =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson call to non-existing contract on tezos X"
    ~tags:["call"; "michelson"; "nonexistent"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  let fake_kt1 = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" in
  let transfer_amount = 1_234_567 in
  (* Only the fee (1000 mutez) should be consumed, not the transfer amount *)
  with_check_source_delta_balance
    ~source
    ~tez_client
    ~expected_consumed:1000
    (fun () ->
      call_michelson_contract_via_delayed_inbox
        ~sc_rollup_address
        ~sc_rollup_node
        ~client
        ~l1_contracts
        ~sequencer
        ~source
        ~counter:1
        ~dest:fake_kt1
        ~arg_data:{|Unit|}
        ~amount:transfer_amount
        ())

let test_get_tezos_ethereum_address_rpc ~runtime () =
  Setup.register_sandbox_test
    ~title:"Test the tez_getTezosEthereumAddress RPC"
    ~tags:["rpc"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let tezos_address = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  let expected = "0x341af4de1e67241d8d2536b2ea47c7e9debf7cb2" in
  let* rpc_result =
    Rpc.Tezosx.tez_getTezosEthereumAddress tezos_address sandbox
  in
  match rpc_result with
  | Ok evm_address ->
      Check.(
        (evm_address = expected) string ~error_msg:"Expected %R but got %L") ;
      unit
  | Error err ->
      Test.fail
        "Could not get the EVM address corresponding to the Tezos address %s: \
         %s"
        tezos_address
        err.Rpc.message

let test_get_ethereum_tezos_address_rpc ~runtime () =
  Setup.register_sandbox_test
    ~title:"Test the tez_getEthereumTezosAddress RPC"
    ~tags:["rpc"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let ethereum_address = "0xccef676171871a48bbd6e2be75bbcc09d38830c5" in
  let expected = "KT1C5w6TSyk4syouR5ncGPVyshR2hvYi8sKE" in
  let* rpc_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress ethereum_address sandbox
  in
  match rpc_result with
  | Ok tezos_address ->
      Check.(
        (tezos_address = expected) string ~error_msg:"Expected %R but got %L") ;
      unit
  | Error err ->
      Test.fail
        "Could not get the Tezos address corresponding to the Ethereum address \
         %s: %s"
        ethereum_address
        err.Rpc.message

let test_eth_rpc_with_alias ~runtime =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Test ETH RPC on Ethereum address alias from Tezos account"
    ~tags:["rpc"; "alias"]
    ~with_runtimes:[runtime]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {
        client;
        observer;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        _;
      }
      _protocol
    ->
  (* Deposit to instantiate the alias on ethereum side *)
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in

  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         receiver_account.public_key_hash
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Tezos_protocol_alpha.Protocol.Contract_repr.encoding
      receiver
  in
  let (`Hex receiver) = Hex.of_bytes bytes in
  let deposit_info =
    Delayed_inbox.{receiver = TezosAddr receiver; chain_id = None}
  in
  let* () =
    Delayed_inbox.send_deposit_to_delayed_inbox
      ~rlp:true
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let*@ evm_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress
      receiver_account.public_key_hash
      observer
  in
  (* Balance should be 0 *)
  let*@ balance = Rpc.get_balance ~address:evm_address observer in
  Check.((balance = Wei.zero) Wei.typ ~error_msg:"Expected %R wei but got %L") ;
  (* Transaction count should be 0 *)
  let*@ transaction_count =
    Rpc.get_transaction_count ~address:evm_address observer
  in
  Check.((transaction_count = 0L) int64 ~error_msg:"Expected %R but got %L") ;
  (* Get storage at 0x0 *)
  let*@ storage = Rpc.get_storage_at ~address:evm_address ~pos:"0x0" observer in
  Check.(
    (storage
   = "0x0000000000000000000000000000000000000000000000000000000000000000")
      string
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezos_block_stored_after_deposit =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"TezosX Tezos block is stored and accessible via Tezlink RPC"
    ~tags:["tezos_block"; "tezosx"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* receiver_account = Client.gen_and_show_keys client in
  let* () =
    deposit
      ~l1_contracts
      ~sc_rollup_address
      ~sc_rollup_node
      ~sequencer
      ~client
      ~depositor
      ~receiver_account
      ~amount
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  (* Query the Tezlink block header RPC to verify the Tezos block was stored *)
  let path = "/tezlink/chains/main/blocks/head/header" in
  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      ~args:["-v"]
      (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  let header = JSON.parse ~origin:"tezlink_block_header" res in
  let level = JSON.(header |-> "level" |> as_int) in
  Check.((level = 2) int ~error_msg:"Expected Tezos block level = 2 but got %L") ;
  unit

let () =
  test_bootstrap_kernel_config () ;
  test_deposit [Alpha] ;
  test_reveal [Alpha] ;
  test_transfer [Alpha] ;
  test_tezos_block_stored_after_deposit [Alpha] ;
  test_michelson_origination_and_call [Alpha] ;
  test_michelson_call_nonexistent_contract [Alpha] ;
  test_eth_rpc_with_alias ~runtime:Tezos [Alpha] ;
  test_runtime_feature_flag ~runtime:Tezos () ;
  test_get_tezos_ethereum_address_rpc ~runtime:Tezos () ;
  test_get_ethereum_tezos_address_rpc ~runtime:Tezos ()
