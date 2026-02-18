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

(** EVM precompile address for cross-runtime gateway calls.
    See [revm/src/precompiles/constants.rs:RUNTIME_GATEWAY_PRECOMPILE_ADDRESS]. *)
let evm_gateway_address = "0xff00000000000000000000000000000000000007"

(** [craft_and_send_evm_transaction ~sequencer ~sender ~nonce ~value ~address
    ~abi_signature ~arguments ()] crafts an EVM transaction, sends it, produces
    a block, asserts the receipt status matches [expected_status] (default
    [true]), and returns the receipt. *)
let craft_and_send_evm_transaction ~sequencer ~sender ~nonce ~value ~address
    ~abi_signature ~arguments ?(expected_status = true) () =
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value
      ~address
      ~signature:abi_signature
      ~arguments
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match receipt with
  | Some r ->
      Check.(
        (r.status = expected_status)
          bool
          ~error_msg:"Expected receipt status %R but got %L") ;
      return r
  | None -> Test.fail "No receipt for EVM transaction to %s" address

(** [call_evm_gateway ~sequencer ~sender ~nonce ~value ~destination ()]
    calls the cross-runtime gateway precompile to reach [destination] (a Tezos
    address).  Asserts the receipt status matches [expected_status] (default
    [true]).  Returns the transaction receipt. *)
let call_evm_gateway ~sequencer ~sender ~nonce ~value ~destination
    ?expected_status () =
  craft_and_send_evm_transaction
    ~sequencer
    ~sender
    ~nonce
    ~value
    ~address:evm_gateway_address
    ~abi_signature:"transfer(string)"
    ~arguments:[destination]
    ?expected_status
    ()

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

(** Originate a Michelson contract and check that [Client.get_balance_for]
    works on the KT1 address via the tezlink RPC. *)
let test_michelson_get_balance =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Get balance of Michelson contract on Tezos X"
    ~tags:["balance"; "michelson"; "rpc"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let init_balance = 1_234_567 in
  let* _contract_hex, kt1_address =
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
      protocol
  in
  let* tez_client = tezos_client sequencer in
  let* balance = Client.get_balance_for ~account:kt1_address tez_client in
  Check.(
    (Tez.to_mutez balance = init_balance)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

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

(** Call an existing Michelson contract with a non-existing entrypoint via
    the delayed inbox. The operation should fail: only fees are consumed. *)
let test_michelson_call_wrong_entrypoint =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson call to wrong entrypoint on tezos X"
    ~tags:["call"; "michelson"; "entrypoint"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  (* Originate store_input.tz (only has "default" entrypoint) *)
  let* _contract_hex, kt1_address =
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
      protocol
  in
  (* Call with a non-existing entrypoint: only fees should be consumed *)
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
        ~counter:2
        ~dest:kt1_address
        ~arg_data:{|"hello"|}
        ~entrypoint:"nonexistent"
        ~amount:1_234_567
        ())

(** Originate a Michelson contract with wrongly-typed initial storage via
    the delayed inbox. The origination should fail: only fees are consumed,
    not the initial balance. *)
let test_michelson_origination_wrong_storage_type =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson origination with wrong storage type on tezos X"
    ~tags:["origination"; "michelson"; "type_error"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  (* We don't use [originate_michelson_contract_via_delayed_inbox] here
     because it expects the origination to succeed. *)
  let* contracts_before =
    Delayed_inbox.subkeys
      tezosx_michelson_contracts_index
      (Sc_rollup_node sc_rollup_node)
  in
  let* () =
    with_check_source_delta_balance
      ~source
      ~tez_client
      ~expected_consumed:1000
      (fun () ->
        let script_path =
          Michelson_script.(find ["opcodes"; "store_input"] protocol |> path)
        in
        let* code = Client.convert_script_to_json ~script:script_path client in
        (* store_input.tz expects storage string, but we pass an int *)
        let* init_storage = Client.convert_data_to_json ~data:"42" client in
        let* origination_op =
          Operation.Manager.(
            operation
              [
                make
                  ~fee:1000
                  ~counter:1
                  ~gas_limit:10000
                  ~storage_limit:1000
                  ~source
                  (origination ~code ~init_storage ~init_balance:1_234_567 ());
              ])
            client
        in
        send_tezos_op_to_delayed_inbox_and_wait
          ~sc_rollup_address
          ~sc_rollup_node
          ~client
          ~l1_contracts
          ~sequencer
          origination_op)
  in
  (* No new contract should have been created *)
  let* contracts_after =
    Delayed_inbox.subkeys
      tezosx_michelson_contracts_index
      (Sc_rollup_node sc_rollup_node)
  in
  Check.(
    (List.length contracts_after = List.length contracts_before)
      int
      ~error_msg:"Expected no new contracts but got %L (was %R)") ;
  unit

(** Call a Michelson contract that always fails (FAILWITH) via the delayed
    inbox. The operation should fail: only fees are consumed. *)
let test_michelson_call_failwith =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson call that reverts with FAILWITH on tezos X"
    ~tags:["call"; "michelson"; "failwith"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  (* Originate always_fails.tz *)
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "always_fails"]
      ~init_storage_data:"Unit"
      protocol
  in
  (* Call always_fails: only fees should be consumed *)
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
        ~counter:2
        ~dest:kt1_address
        ~arg_data:{|"trigger fail"|}
        ~amount:1_234_567
        ())

(** Test inter-contract calls via TRANSFER_TOKENS.

    1. Originate [execution_order_storer.tz]: takes a string parameter and
       appends it to its string storage.
    2. Originate [execution_order_appender.tz]: calls the storer via
       TRANSFER_TOKENS with a string stored in its own storage.
    3. Call the appender and verify that the storer's storage was updated,
       proving that the target KT1's code was actually executed. *)
let test_michelson_inter_contract_call =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson inter-contract call via TRANSFER_TOKENS on tezos X"
    ~tags:["call"; "michelson"; "internal"; "inter_contract"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate storer (string storage, initially empty) *)
  let* storer_hex, storer_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "execution_order_storer"]
      ~init_storage_data:{|""|}
      protocol
  in
  (* Step 2: Originate appender with storage = Pair <storer_address> "Hello" *)
  let* _appender_hex, appender_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~script_name:["mini_scenarios"; "execution_order_appender"]
      ~init_storage_data:(sf {|Pair "%s" "Hello"|} storer_kt1)
      protocol
  in
  (* Sanity check: storer's storage is still empty *)
  let* () =
    check_michelson_storage_value
      ~sc_rollup_node
      ~contract_hex:storer_hex
      ~expected:(`O [("string", `String "")])
      ()
  in
  (* Step 3: Call the appender — it will TRANSFER_TOKENS to the storer *)
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:3
      ~dest:appender_kt1
      ~arg_data:"Unit"
      ()
  in
  (* Step 4: Verify the storer's storage was updated to "Hello" *)
  check_michelson_storage_value
    ~sc_rollup_node
    ~contract_hex:storer_hex
    ~expected:(`O [("string", `String "Hello")])
    ()

(** Test that a failing internal call reverts all sibling internal operations.

    1. Originate [balance.tz]: stores its own balance (parameter unit).
    2. Originate [always_fails_unit.tz]: always FAILWITHs (parameter unit).
    3. Originate [reentrancy.tz]: emits two TRANSFER_TOKENS (5 XTZ each),
       first to [balance.tz], second to [always_fails_unit.tz].
    4. Call [reentrancy.tz]: the second internal call fails, so everything
       should be reverted — [balance.tz] should not receive any funds. *)
let test_michelson_internal_call_revert =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson internal call revert on tezos X"
    ~tags:["call"; "michelson"; "internal"; "revert"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  (* Step 1: Originate balance.tz — stores its own BALANCE, accepts unit *)
  let* _balance_hex, balance_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "balance"]
      ~init_storage_data:"0"
      protocol
  in
  (* Step 2: Originate always_fails_unit.tz — FAILWITHs on any call *)
  let* _fails_hex, fails_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~script_name:["mini_scenarios"; "always_fails_unit"]
      ~init_storage_data:"Unit"
      protocol
  in
  (* Step 3: Originate reentrancy.tz — sends 5 XTZ to each address *)
  let* _reentry_hex, reentry_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:3
      ~script_name:["attic"; "reentrancy"]
      ~init_storage_data:(sf {|Pair "%s" "%s"|} balance_kt1 fails_kt1)
      ~init_balance:20_000_000
      protocol
  in
  (* Step 4: Call reentrancy — second internal call fails, all should revert *)
  let* () =
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
          ~counter:4
          ~dest:reentry_kt1
          ~arg_data:"Unit"
          ())
  in
  (* balance.tz should NOT have received any funds (revert) *)
  let* balance = Client.get_balance_for ~account:balance_kt1 tez_client in
  Check.(
    (Tez.to_mutez balance = 0)
      int
      ~error_msg:"Expected %R mutez but got %L (revert did not work)") ;
  unit

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

(** [check_evm_to_michelson_transfer ~sequencer ~sender ~nonce ~tezos_destination
    ~transfer_amount ()] sends [transfer_amount] from the EVM [sender] to the
    Tezos [tezos_destination] via the gateway precompile, then checks that:
    - the Michelson destination balance increased by [transfer_amount] (in mutez)
    - the EVM sender balance decreased by exactly [value + gas_fees] *)
let check_evm_to_michelson_transfer ~sequencer ~sender ~nonce ~tezos_destination
    ~transfer_amount () =
  let* tez_client = tezos_client sequencer in
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  let value = Wei.of_tez transfer_amount in
  let* mic_balance_before =
    Client.get_balance_for ~account:tezos_destination tez_client
  in
  let*@ evm_balance_before =
    Rpc.get_balance ~address:sender.Eth_account.address sequencer
  in
  let* receipt =
    call_evm_gateway
      ~sequencer
      ~sender
      ~nonce
      ~value
      ~destination:tezos_destination
      ()
  in
  let* mic_balance_after =
    Client.get_balance_for ~account:tezos_destination tez_client
  in
  Check.(
    (Tez.to_mutez mic_balance_after
    = Tez.to_mutez mic_balance_before + transfer_amount_mutez)
      int
      ~error_msg:"Expected Michelson balance %R but got %L") ;
  (* Verify EVM sender balance decreased by exactly value + gas fees *)
  let gas_fees =
    let gas_price = receipt.effectiveGasPrice |> Z.of_int64 |> Wei.to_wei_z in
    let gas_used = receipt.gasUsed |> Z.of_int64 in
    Wei.(gas_price * gas_used)
  in
  let*@ evm_balance_after =
    Rpc.get_balance ~address:sender.Eth_account.address sequencer
  in
  Check.(
    (evm_balance_after = Wei.(evm_balance_before - value - gas_fees))
      Wei.typ
      ~error_msg:"Expected EVM sender balance %R but got %L") ;
  unit

let test_cross_runtime_transfer_from_evm_to_tz =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:
      "Cross-runtime transfer to an implicit Michelson address via EVM gateway"
    ~tags:["cross_runtime"; "transfer"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun {sequencer; _} _protocol ->
  let tezos_destination = Constant.bootstrap1.public_key_hash in
  let sender = Eth_account.bootstrap_accounts.(0) in
  check_evm_to_michelson_transfer
    ~sequencer
    ~sender
    ~nonce:0
    ~tezos_destination
    ~transfer_amount:(Tez.of_int 100)
    ()

let gateway_address = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw"

let test_cross_runtime_transfer_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway"
    ~tags:["cross_runtime"; "transfer"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap5 in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let transfer_amount = Tez.of_int 100 in
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  (* Build a Tezos operation that calls the gateway KT1 to transfer
     funds to an EVM address. The gateway expects a Micheline string
     parameter containing the EVM destination address. *)
  let* call_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source
            (call
               ~dest:gateway_address
               ~amount:transfer_amount_mutez
               ~arg:(`O [("string", `String evm_destination)])
               ());
        ])
      client
  in
  (* Send the operation via the delayed inbox *)
  let* _ =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      call_op
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
  (* Check the EVM balance of the destination.
     The gateway passes the raw mutez amount to the EVM bridge,
     which credits it directly as the EVM balance. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sequencer in
  let expected_balance = Wei.of_string (string_of_int transfer_amount_mutez) in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  unit

let test_cross_runtime_call_executes_evm_bytecode =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Tezos to EVM executes contract bytecode"
    ~tags:["cross_runtime"; "bytecode"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Step 1: Deploy a simple EVM contract that writes 0x42 to storage slot 1.
     Runtime bytecode: PUSH1 0x42, PUSH1 0x01, SSTORE, PUSH1 0x01, SLOAD
     Init code copies runtime code to memory and returns it. *)
  let init_code = "600880600b6000396000f36042600155600154" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* raw_tx =
    Cast.craft_deploy_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas:2_000_000
      ~gas_price:1_000_000_000
      ~data:init_code
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  let contract_address =
    match receipt with
    | Some {contractAddress = Some addr; status = true; _} -> addr
    | Some {status = false; _} ->
        Test.fail "Contract deployment transaction failed"
    | _ -> Test.fail "No receipt or no contract address for deployment tx"
  in
  (* Step 2: Call the Michelson gateway with the deployed EVM contract
     as destination. The gateway sends a cross-runtime call that
     triggers the EVM contract's bytecode execution. *)
  let source = Constant.bootstrap5 in
  let* call_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:1
            ~source
            (call
               ~dest:gateway_address
               ~amount:0
               ~arg:(`O [("string", `String contract_address)])
               ());
        ])
      client
  in
  let* _ =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      call_op
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
  (* Step 3: Verify the contract's storage slot 1 contains 0x42,
     proving the EVM bytecode was executed via the cross-runtime call. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x1" sequencer
  in
  let expected_storage =
    "0x0000000000000000000000000000000000000000000000000000000000000042"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 1 = %R but got %L") ;
  unit

let test_cross_runtime_transfer_from_evm_to_kt1 =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer to a Michelson KT1 via EVM gateway"
    ~tags:["cross_runtime"; "transfer"; "kt1"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate transfer_amount.tz — stores AMOUNT in storage *)
  let* contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "transfer_amount"]
      ~init_storage_data:"0"
      protocol
  in
  (* Step 2: Verify initial storage is 0 *)
  let* () =
    check_michelson_storage_value
      ~sc_rollup_node
      ~contract_hex
      ~expected:(`O [("int", `String "0")])
      ()
  in
  (* Step 3: Transfer from EVM to the KT1 via the gateway precompile *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let transfer_amount = Tez.of_int 100 in
  let* () =
    check_evm_to_michelson_transfer
      ~sequencer
      ~sender
      ~nonce:0
      ~tezos_destination:kt1_address
      ~transfer_amount
      ()
  in
  (* Step 4: Sync the rollup node with the sequencer so it sees the
     EVM gateway state changes. *)
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Step 5: Verify the contract storage contains the transferred amount *)
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  check_michelson_storage_value
    ~sc_rollup_node
    ~contract_hex
    ~expected:(`O [("int", `String (string_of_int transfer_amount_mutez))])
    ()

let test_cross_runtime_call_failwith =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:
      "Cross-runtime transfer to a FAILWITH Michelson KT1 reverts via EVM \
       gateway"
    ~tags:["cross_runtime"; "transfer"; "kt1"; "failwith"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate always_fails.tz *)
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "always_fails"]
      ~init_storage_data:"Unit"
      protocol
  in
  (* Step 2: Call the KT1 from EVM via the gateway precompile *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let transfer_amount = Tez.of_int 100 in
  let value = Wei.of_tez transfer_amount in
  let* tez_client = tezos_client sequencer in
  let*@ evm_balance_before =
    Rpc.get_balance ~address:sender.Eth_account.address sequencer
  in
  let* kt1_balance_before =
    Client.get_balance_for ~account:kt1_address tez_client
  in
  let* receipt =
    call_evm_gateway
      ~sequencer
      ~sender
      ~nonce:0
      ~value
      ~destination:kt1_address
      ~expected_status:false
      ()
  in
  (* Step 3: The EVM sender should only have lost gas fees, not the
     transfer amount — the value is refunded on revert. *)
  let gas_fees =
    let gas_price = receipt.effectiveGasPrice |> Z.of_int64 |> Wei.to_wei_z in
    let gas_used = receipt.gasUsed |> Z.of_int64 in
    Wei.(gas_price * gas_used)
  in
  let*@ evm_balance_after =
    Rpc.get_balance ~address:sender.Eth_account.address sequencer
  in
  Check.(
    (evm_balance_after = Wei.(evm_balance_before - gas_fees))
      Wei.typ
      ~error_msg:"Expected EVM sender balance %R but got %L") ;
  (* Step 4: The Michelson contract balance should be unchanged *)
  let* kt1_balance_after =
    Client.get_balance_for ~account:kt1_address tez_client
  in
  Check.(
    (Tez.to_mutez kt1_balance_after = Tez.to_mutez kt1_balance_before)
      int
      ~error_msg:"Expected KT1 balance %R but got %L") ;
  unit

let () =
  test_bootstrap_kernel_config () ;
  test_deposit [Alpha] ;
  test_reveal [Alpha] ;
  test_transfer [Alpha] ;
  test_cross_runtime_transfer_from_evm_to_tz [Alpha] ;
  test_cross_runtime_transfer_to_evm [Alpha] ;
  test_cross_runtime_call_executes_evm_bytecode [Alpha] ;
  test_cross_runtime_transfer_from_evm_to_kt1 [Alpha] ;
  test_cross_runtime_call_failwith [Alpha] ;
  test_tezos_block_stored_after_deposit [Alpha] ;
  test_michelson_origination_and_call [Alpha] ;
  test_michelson_get_balance [Alpha] ;
  test_michelson_call_nonexistent_contract [Alpha] ;
  test_michelson_call_wrong_entrypoint [Alpha] ;
  test_michelson_origination_wrong_storage_type [Alpha] ;
  test_michelson_call_failwith [Alpha] ;
  test_michelson_inter_contract_call [Alpha] ;
  test_michelson_internal_call_revert [Alpha] ;
  test_eth_rpc_with_alias ~runtime:Tezos [Alpha] ;
  test_runtime_feature_flag ~runtime:Tezos () ;
  test_get_tezos_ethereum_address_rpc ~runtime:Tezos () ;
  test_get_ethereum_tezos_address_rpc ~runtime:Tezos ()
