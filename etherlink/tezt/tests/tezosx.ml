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
      ?tez_bootstrap_accounts ?chain_id ?enable_michelson_gas_refund =
    Test_helpers.register_sandbox
      ~__FILE__
      ?uses_client
      ?tez_bootstrap_accounts
      ?chain_id
      ~kernel:Latest
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~with_runtimes
      ?enable_michelson_gas_refund

  let register_sandbox_regression_test ?uses_client ~title ~tags ~with_runtimes
      ?tez_bootstrap_accounts body =
    let uses_client = Option.value ~default:false uses_client in
    Regression.register
      ~__FILE__
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~uses_admin_client:uses_client
      ~uses_client
      ~uses_node:false
      ~uses:
        [
          Constant.octez_evm_node;
          Constant.WASM.evm_kernel;
          Constant.smart_rollup_installer;
        ]
    @@ fun () ->
    let* sequencer =
      Test_helpers.init_sequencer_sandbox
        ~kernel:Latest
        ?tez_bootstrap_accounts
        ?with_runtimes:(Some with_runtimes)
        ()
    in
    body sequencer

  let register_sandbox_with_oberver_test ?uses_client ~title ~tags
      ~with_runtimes ?eth_bootstrap_accounts ?tez_bootstrap_accounts =
    Test_helpers.register_sandbox_with_observer
      ~__FILE__
      ?uses_client
      ?eth_bootstrap_accounts
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
      ?tez_bootstrap_accounts
end

let test_runtime_feature_flag ~runtime () =
  Setup.register_sandbox_test
    ~title:"Set Tezos runtime feature flag"
    ~tags:["feature_flag"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let path = Tezosx_runtime.feature_flag Kernel.Latest runtime in
  let* rpc_result = Rpc.state_value sandbox path in
  match rpc_result with
  | Ok (Some _) -> unit
  | Ok None ->
      Test.fail
        "Feature flag for the %s runtime was not set"
        (String.capitalize_ascii (Tezosx_runtime.to_string runtime))
  | Error err -> Test.fail "Could not read feature flag %s: %s" path err.message

let test_gas_refund_feature_flag () =
  Setup.register_sandbox_test
    ~title:"Set gas refund feature flag"
    ~tags:["feature_flag"; "gas_refund"]
    ~with_runtimes:[Tezos]
    ~enable_michelson_gas_refund:true
  @@ fun sandbox ->
  let* rpc_result =
    Rpc.state_value sandbox "/base/feature_flags/enable_michelson_gas_refund"
  in
  match rpc_result with
  | Ok (Some _) -> unit
  | Ok None -> Test.fail "Feature flag for gas refund was not set"
  | Error err ->
      Test.fail "Could not read gas refund feature flag: %s" err.message

let tezos_client node =
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.{(Evm_node.rpc_endpoint_record node) with path = "/tezlink"})
  in
  Client.init ~endpoint ()

let get_tezlink_block_header node =
  let path = "/tezlink/chains/main/blocks/head/header" in
  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name node)
      (Evm_node.endpoint node ^ path)
    |> Runnable.run
  in
  return (JSON.parse ~origin:"tezlink_block_header" res)

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
let tezosx_michelson_contracts_index = "/tez/tez_accounts/contracts/index"

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
    ?(entrypoint = "default") ?(amount = 0) ?(gas_limit = 100_000) () =
  let* arg = Client.convert_data_to_json ~data:arg_data client in
  let* call_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter
            ~gas_limit
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

(** EVM predeployed address for the FA1.2 wrapper contract.
    See [revm/src/precompiles/constants.rs:FA12_WRAPPER_SOL_ADDR]. *)
let fa12_wrapper_address = "0xff00000000000000000000000000000000ffff09"

let assert_evm_balance_zero ~address sequencer =
  let*@ balance = Rpc.get_balance ~address sequencer in
  Check.(
    (balance = Wei.zero) Wei.typ ~error_msg:"Expected balance 0 but got %L") ;
  unit

(** [craft_and_send_evm_transaction ~sequencer ~sender ~nonce ~value ~address
    ~abi_signature ~arguments ()] crafts an EVM transaction, sends it, produces
    a block, asserts the receipt status matches [expected_status] (default
    [true]), and returns the receipt. *)
let craft_and_send_evm_transaction ~sequencer ~sender ~nonce ~value ~address
    ~abi_signature ~arguments ?(expected_status = true) ?(gas = 3_000_000) () =
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce
      ~gas
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

(** [deploy_evm_contract ~sequencer ~sender ~nonce ~init_code ()] deploys an
    EVM contract from [init_code], produces a block, and returns the deployed
    contract address.  Fails if the deployment transaction reverts or yields no
    contract address. *)
let deploy_evm_contract ~sequencer ~sender ~nonce ~init_code () =
  let* raw_tx =
    Cast.craft_deploy_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce
      ~gas:2_000_000
      ~gas_price:1_000_000_000
      ~data:init_code
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match receipt with
  | Some {contractAddress = Some addr; status = true; _} -> return addr
  | Some {status = false; _} ->
      Test.fail "Contract deployment transaction failed"
  | _ -> Test.fail "No receipt or no contract address for deployment tx"

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

(** Structured view of a Michelson internal [event] receipt, as decoded from
    the Tezlink RPC. There is no canonical helper for this in
    [tezt/lib_tezos] — [tezt/tests/events.ml] navigates the JSON by hand. *)
type event_receipt = {
  kind : string;
  tag : string;
  source : string;
  result_status : string;
  type_prim : string;
  (* For a [pair] type: each immediate type arg, as [(prim, annots)]. *)
  type_args : (string * string list) list;
  payload_prim : string;
  (* The flat list of integer leaves of the payload, parsed as OCaml ints. *)
  payload_int_args : int list;
}

(** Structured view of a tez deposit operation receipt, as decoded from the
    Tezlink RPC. *)
type deposit_receipt = {
  source : string;
  op_status : string;
  balance_updates : Operation_receipt.Balance_updates.t list;
  internal_ops : event_receipt list;
}

(** [parse_head_deposit_receipt ~sequencer ()] fetches the head Tezos block's
    first manager operation through the Tezlink RPC and decomposes it into a
    [deposit_receipt]. Reuses [Operation_receipt.Balance_updates] for the
    balance_updates list; structural validation is left to the caller. *)
let parse_head_deposit_receipt ~sequencer () =
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"}
  in
  let* operation =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:3
         ~operation_offset:0
         ()
  in
  let open JSON in
  let content = operation |-> "contents" |=> 0 in
  let metadata = content |-> "metadata" in
  let op_result = metadata |-> "operation_result" in
  let* balance_updates =
    Operation_receipt.Balance_updates.from_result [op_result]
  in
  let parse_type_arg ty_arg =
    ( ty_arg |-> "prim" |> as_string,
      ty_arg |-> "annots" |> as_list |> List.map as_string )
  in
  let parse_event event =
    let ty = event |-> "type" in
    let payload = event |-> "payload" in
    {
      kind = event |-> "kind" |> as_string;
      tag = event |-> "tag" |> as_string;
      source = event |-> "source" |> as_string;
      result_status = event |-> "result" |-> "status" |> as_string;
      type_prim = ty |-> "prim" |> as_string;
      type_args = ty |-> "args" |> as_list |> List.map parse_type_arg;
      payload_prim = payload |-> "prim" |> as_string;
      payload_int_args =
        payload |-> "args" |> as_list
        |> List.map (fun a -> a |-> "int" |> as_string |> int_of_string);
    }
  in
  return
    {
      source = content |-> "source" |> as_string;
      op_status = op_result |-> "status" |> as_string;
      balance_updates;
      internal_ops =
        metadata |-> "internal_operation_results" |> as_list
        |> List.map parse_event;
    }

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

let test_delayed_inbox_transfer =
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
  let amount_mutez = Tez.to_mutez amount in
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
  let* tez_client = tezos_client sequencer in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash tez_client
  in
  Check.(
    (Tez.to_mutez balance = amount_mutez)
      int
      ~error_msg:"Expected %R mutez but got %L") ;

  (* Inspect the deposit operation's receipt:
     - [TransferSuccess.balance_updates] should debit the synthetic
       [TEZLINK_DEPOSITOR] and credit the receiver.
     - [internal_operation_results] should carry a single Michelson
       [deposit] event of type [pair (nat %inbox_level) (nat %inbox_msg_id)]
       whose payload exposes the originating shared-inbox coordinates,
       so indexers can correlate a deposit credit with its inbox slot. *)
  let* receipt = parse_head_deposit_receipt ~sequencer () in
  Check.(
    (receipt.op_status = "applied")
      string
      ~error_msg:"Expected deposit operation_result.status to be %R, got %L") ;
  let bu_summary (bu : Operation_receipt.Balance_updates.t) =
    (bu.contract, bu.change)
  in
  Check.(
    (List.map bu_summary receipt.balance_updates
    = [
        (Some receipt.source, -amount_mutez);
        (Some receiver_account.public_key_hash, amount_mutez);
      ])
      (list (tuple2 (option string) int))
      ~error_msg:
        "Expected TransferSuccess.balance_updates (contract, change) to be %R, \
         got %L") ;
  (match receipt.internal_ops with
  | [event] -> (
      Check.(
        (event.kind = "event")
          string
          ~error_msg:"Expected internal op kind to be %R, got %L") ;
      Check.(
        (event.tag = "deposit")
          string
          ~error_msg:"Expected event tag to be %R, got %L") ;
      Check.(
        (event.source = receipt.source)
          string
          ~error_msg:"Expected event source to be %R, got %L") ;
      Check.(
        (event.result_status = "applied")
          string
          ~error_msg:"Expected event result.status to be %R, got %L") ;
      Check.(
        (event.type_prim = "pair")
          string
          ~error_msg:"Expected event type prim to be %R, got %L") ;
      Check.(
        (event.type_args
        = [("nat", ["%inbox_level"]); ("nat", ["%inbox_msg_id"])])
          (list (tuple2 string (list string)))
          ~error_msg:"Expected event type args to be %R, got %L") ;
      Check.(
        (event.payload_prim = "Pair")
          string
          ~error_msg:"Expected event payload prim to be %R, got %L") ;
      match event.payload_int_args with
      | [inbox_level; inbox_msg_id] ->
          Check.(
            (inbox_level > 0)
              int
              ~error_msg:"Expected inbox_level to be positive, got %L") ;
          Check.(
            (inbox_msg_id >= 0)
              int
              ~error_msg:"Expected inbox_msg_id to be non-negative, got %L")
      | args ->
          Test.fail
            "Expected event payload to carry 2 int leaves, got %d"
            (List.length args))
  | ops ->
      Test.fail
        "Expected exactly 1 internal operation (the deposit event), got %d"
        (List.length ops)) ;

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
  let check_rpc_result tezos_address expected_evm_address_opt =
    let* rpc_result =
      Rpc.Tezosx.tez_getTezosEthereumAddress tezos_address sandbox
    in
    match (rpc_result, expected_evm_address_opt) with
    | Ok evm_address, Some expected ->
        Check.(
          (evm_address = expected) string ~error_msg:"Expected %R but got %L") ;
        unit
    | Error _, None -> unit
    | Error err, Some _ ->
        Test.fail
          "Could not get the EVM address corresponding to the Tezos address \
           %s: %s"
          tezos_address
          err.Rpc.message
    | Ok evm_address, None ->
        Test.fail
          "Expected an error for address %s but got the result %s"
          tezos_address
          evm_address
  in
  let* () =
    check_rpc_result
      "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
      (Some "0xccef676171871a48bbd6e2be75bbcc09d38830c5")
  in
  check_rpc_result
    "KT1Uik8kf8QBs4JoFCbeJBVaEwozebEjoEXQ"
    (Some "0x9ae7fe293cbb7c039fd4139295f60fd945d99b5c")

let test_get_ethereum_tezos_address_rpc ~runtime () =
  Setup.register_sandbox_test
    ~title:"Test the tez_getEthereumTezosAddress RPC"
    ~tags:["rpc"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  let ethereum_address = "0xccef676171871a48bbd6e2be75bbcc09d38830c5" in
  let expected = "KT1TLraR9PboPAvxLKYQs9eU4n75rGFJTbWk" in
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
  Check.(
    (level >= 2) int ~error_msg:"Expected Tezos block level >= 2 but got %L") ;
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
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
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

(** Init code for a minimal EVM contract that stores calldataload(4) in slot 0.
    Runtime bytecode: PUSH1 04 | CALLDATALOAD | PUSH1 00 | SSTORE | STOP
    = 60 04 35 60 00 55 00 (7 bytes) *)
let evm_storer_init_code = "600780600b6000396000f360043560005500"

(** Init code for a minimal EVM contract that stores msg.sender (CALLER) in
    slot 0 and stops.
    Runtime bytecode: CALLER | PUSH1 00 | SSTORE | STOP
    = 33 60 00 55 00 (5 bytes) *)
let evm_caller_storer_init_code = "600580600b6000396000f33360005500"

(** Init code for a minimal EVM contract that always REVERTs.
    Runtime bytecode: PUSH1 00 | PUSH1 00 | REVERT
    = 60 00 60 00 FD (5 bytes) *)
let evm_reverter_init_code = "600580600b6000396000f360006000fd"

(** Init code for a minimal EVM contract that forwards its incoming
    calldata to the runtime gateway precompile via STATICCALL, then
    either RETURNs the returndata on success or REVERTs with the
    returndata on failure.

    This is the idiomatic Solidity pattern for calling a read-only
    entry (a `view` modifier under the hood emits STATICCALL). Using
    it against `callMichelsonView` verifies the precompile entry does
    not perform any state mutation (log emission, alias-cache write,
    value burn) that REVM would reject in a static context.

    Runtime bytecode (61 bytes):
    - CALLDATACOPY memory[0..calldatasize] ← calldata
    - STATICCALL(gas, 0xff..0007, 0, calldatasize, 0, 0)
    - ISZERO + JUMPI → jump to the revert path on failure
    - On success: RETURNDATACOPY then RETURN returndata
    - On failure: RETURNDATACOPY then REVERT with returndata *)
let evm_gateway_staticcall_forwarder_init_code =
  "603d80600b6000396000f33660006000376000600036600073ff000000000000000000000000000000000000075afa156032573d600060003e3d6000f35b3d600060003e3d6000fd"

(** Same shape as [evm_gateway_staticcall_forwarder_init_code], but
    uses DELEGATECALL (opcode 0xf4) instead of STATICCALL (0xfa) to
    reach the precompile. Used to verify that the gateway rejects
    delegated frames — all four entries revert with the
    "DELEGATECALL and CALLCODE are not allowed" message, surfacing
    here as a failing receipt because the forwarder REVERTs on
    non-success. *)
let evm_gateway_delegatecall_forwarder_init_code =
  "603d80600b6000396000f33660006000376000600036600073ff000000000000000000000000000000000000075af4156032573d600060003e3d6000f35b3d600060003e3d6000fd"

(** ABI encoding of uint256(42). *)
let abi_encoded_uint256_42 =
  "000000000000000000000000000000000000000000000000000000000000002a"

(** KT1 address of the ERC-20 wrapper enshrined contract.
    See [tezos_execution/src/enshrined_contracts.rs:ERC20_WRAPPER_ADDRESS]. *)
let erc20_wrapper_address = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVKvCChb"

(** [michelson_to_evm_transfer ~source ~destination ~transfer_amount setup]
    transfers [transfer_amount] from the Michelson [source] to the EVM
    [destination] address via the gateway contract. The operation is built
    with optional [~counter] (default 1) and [~fee] (default 1000), then
    sent through the delayed inbox and baked until the rollup is synced. *)
let michelson_to_evm_transfer ~source ~evm_destination ~transfer_amount
    ?(counter = 1) ?(fee = 1000) ?(gas_limit = 100_000) ?call
    Tezt_etherlink.Setup.
      {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _} =
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  (* Build a Tezos operation that calls the gateway KT1. When [call] is
     provided as (fn_sig, calldata), uses the "call_evm" entrypoint; otherwise
     uses the default entrypoint for a simple transfer. *)
  let* entrypoint, arg =
    match call with
    | None -> Lwt.return ("default", `O [("string", `String evm_destination)])
    | Some (fn_sig, calldata) ->
        let* arg =
          Client.convert_data_to_json
            ~data:
              (sf
                 {|Pair "%s" (Pair "%s" (Pair 0x%s None))|}
                 evm_destination
                 fn_sig
                 calldata)
            client
        in
        Lwt.return ("call_evm", arg)
  in
  let* call_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee
            ~gas_limit
            ~counter
            ~source
            (call
               ~dest:gateway_address
               ~amount:transfer_amount_mutez
               ~entrypoint
               ~arg
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
  unit

let test_cross_runtime_transfer_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway"
    ~tags:["cross_runtime"; "transfer"]
    ~with_runtimes:[Tezos]
  @@ fun setup _protocol ->
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let amount = Tez.of_int 100 in
  let* () =
    michelson_to_evm_transfer
      ~source:Constant.bootstrap1
      ~evm_destination
      ~transfer_amount:amount
      setup
  in
  (* Check the EVM balance of the destination.
     The gateway passes the raw mutez amount to the EVM bridge,
     which credits it directly as the EVM balance. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination setup.sequencer in
  let expected_balance = Wei.of_tez amount in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client setup.sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Same as [test_cross_runtime_transfer_to_evm] but using the generic
    %call entrypoint instead of %default. Verifies that value transfer
    works through the %call path (HTTP POST to the EVM runtime) and
    that the gateway balance is zeroed after forwarding. *)
let test_cross_runtime_transfer_to_evm_via_call =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway %call"
    ~tags:["cross_runtime"; "transfer"; "call"]
    ~with_runtimes:[Tezos]
  @@ fun setup _protocol ->
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let amount = Tez.of_int 100 in
  let amount_mutez = Tez.to_mutez amount in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address:setup.sc_rollup_address
      ~sc_rollup_node:setup.sc_rollup_node
      ~client:setup.client
      ~l1_contracts:setup.l1_contracts
      ~sequencer:setup.sequencer
      ~source:Constant.bootstrap1
      ~counter:1
      ~dest:gateway_address
      ~entrypoint:"call"
      ~amount:amount_mutez
      ~arg_data:
        (sf
           {|Pair "http://ethereum/%s" (Pair {} (Pair 0x (Pair 1 None)))|}
           evm_destination)
      ()
  in
  (* Check the EVM balance of the destination. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination setup.sequencer in
  let expected_balance = Wei.of_tez amount in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client setup.sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Same as [test_cross_runtime_transfer_to_evm] but using the
    %call_evm entrypoint with an empty calldata instead of %default.
    Verifies that value transfer works through %call_evm and that the
    gateway balance is zeroed after forwarding. *)
let test_cross_runtime_transfer_to_evm_via_call_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway %call_evm"
    ~tags:["cross_runtime"; "transfer"; "call_evm"]
    ~with_runtimes:[Tezos]
  @@ fun setup _protocol ->
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let amount = Tez.of_int 100 in
  let amount_mutez = Tez.to_mutez amount in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address:setup.sc_rollup_address
      ~sc_rollup_node:setup.sc_rollup_node
      ~client:setup.client
      ~l1_contracts:setup.l1_contracts
      ~sequencer:setup.sequencer
      ~source:Constant.bootstrap1
      ~counter:1
      ~dest:gateway_address
      ~entrypoint:"call_evm"
      ~amount:amount_mutez
      ~arg_data:(sf {|Pair "%s" (Pair "" (Pair 0x None))|} evm_destination)
      ()
  in
  (* Check the EVM balance of the destination. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination setup.sequencer in
  let expected_balance = Wei.of_tez amount in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client setup.sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(* This test exercises nested cross-runtime calls:
     Tezos Runtime (tz1 transfer) -> EVM Runtime (CRAC contract) -> Tezos Runtime (KT1 destination).
     The goal is to see a modification in the storage of the KT1 destination in the Tezos runtime. *)
let test_nested_crac =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Tezos to EVM back to Tezos"
    ~tags:["cross_runtime"; "bytecode"; "nested"]
    ~with_runtimes:[Tezos]
  @@ fun setup protocol ->
  (* Step 1: Deploy the Michelson contract (final destination of the nested
     calls, whose storage we check to verify the full chain worked) and the
     EVM CRAC contract (intermediary that forwards back to Michelson runtime).  *)
  let contract_storage =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    let contract =
      Result.get_ok @@ C.of_b58check Constant.bootstrap1.public_key_hash
    in
    let (`Hex hex) =
      Hex.of_bytes (Data_encoding.Binary.to_bytes_exn C.encoding contract)
    in
    hex
  in
  let* contract_hex, kt1_address =
    let source = Constant.bootstrap5 in
    let Tezt_etherlink.Setup.
          {
            client;
            l1_contracts;
            sc_rollup_address;
            sc_rollup_node;
            sequencer;
            _;
          } =
      setup
    in
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "source"]
      ~init_storage_data:(Format.sprintf "0x%s" contract_storage)
      ~init_balance:0
      protocol
  in
  (* Verify that the contract storage is correctly set *)
  let* () =
    check_michelson_storage_value
      ~sc_rollup_node:setup.sc_rollup_node
      ~contract_hex
      ~expected:(`O [("bytes", `String contract_storage)])
      ()
  in
  let* crac_contract = Solidity_contracts.transfer_crac setup.evm_version in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let bytecode = Tezt.Base.read_file crac_contract.bin in
  let* evm_crac_address =
    deploy_evm_contract
      ~sequencer:setup.sequencer
      ~sender
      ~nonce:0
      ~init_code:("0x" ^ bytecode)
      ()
  in
  (* Step 2: Call the Michelson gateway with the deployed EVM contract
     as destination. The gateway sends a cross-runtime call that
     triggers the EVM contract's bytecode execution. *)
  let transfer_amount = Tez.of_int 55 in
  let selector = "crac(string)" in
  let* args = Cast.calldata ~args:[kt1_address] selector in
  let args = String.sub args 10 (String.length args - 10) in
  let* () =
    michelson_to_evm_transfer
      ~source:Constant.bootstrap1
      ~evm_destination:evm_crac_address
      ~transfer_amount
      ~call:(selector, args)
      setup
  in
  (* Step 3: Verify the CRAC contract balance and the Michelson
     contract storage after the nested cross-runtime call. *)
  let*@ crac_contract_balance =
    Rpc.get_balance ~address:evm_crac_address setup.sequencer
  in
  Check.(
    (crac_contract_balance = Wei.zero)
      Wei.typ
      ~error_msg:"Expected EVM CRAC balance %R but got %L") ;
  let* tez_client = tezos_client setup.sequencer in
  let* kt1_balance = Client.get_balance_for ~account:kt1_address tez_client in
  Check.(
    (Tez.to_mutez kt1_balance = Tez.to_mutez transfer_amount)
      int
      ~error_msg:"Expected KT1 balance %R but got %L") ;
  (* Verify that the contract storage has been changed because of the nested CRAC *)
  check_michelson_storage_value
    ~sc_rollup_node:setup.sc_rollup_node
    ~contract_hex
    ~expected:
      (* The source of a CRAC is address zero for now *)
      (`O [("bytes", `String "00000000000000000000000000000000000000000000")])
    ()

let test_cross_runtime_call_executes_evm_bytecode =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Tezos to EVM executes contract bytecode"
    ~tags:["cross_runtime"; "bytecode"]
    ~with_runtimes:[Tezos]
  @@ fun setup _protocol ->
  (* Step 1: Deploy a simple EVM contract that writes 0x42 to storage slot 1.
     Runtime bytecode: PUSH1 0x42, PUSH1 0x01, SSTORE, PUSH1 0x01, SLOAD
     Init code copies runtime code to memory and returns it. *)
  let init_code = "600880600b6000396000f36042600155600154" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:setup.sequencer
      ~sender
      ~nonce:0
      ~init_code
      ()
  in
  (* Step 2: Call the Michelson gateway with the deployed EVM contract
     as destination. The gateway sends a cross-runtime call that
     triggers the EVM contract's bytecode execution. *)
  let* () =
    michelson_to_evm_transfer
      ~source:Constant.bootstrap5
      ~evm_destination:contract_address
      ~transfer_amount:(Tez.of_int 0)
      setup
  in
  (* Step 3: Verify the contract's storage slot 1 contains 0x42,
     proving the EVM bytecode was executed via the cross-runtime call. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x1" setup.sequencer
  in
  let expected_storage =
    "0x0000000000000000000000000000000000000000000000000000000000000042"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 1 = %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client setup.sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
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
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
  unit

(** Test cross-runtime call from EVM to Michelson with calldata.

    1. Originate [store_input.tz] via delayed inbox.
    2. Call [call(string,string,bytes)] on the EVM gateway precompile,
       passing the KT1 address, "default" entrypoint, and Micheline-encoded
       string "Hello from EVM" as parameters.
    3. Verify the Michelson contract storage is updated to "Hello from EVM". *)
let test_cross_runtime_call_from_evm_to_michelson =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from EVM to Michelson with calldata"
    ~tags:["cross_runtime"; "call"; "calldata"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate store_input_ep.tz — like store_input but with a named
     entrypoint %save instead of %default. *)
  let* contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "store_input_ep"]
      ~init_storage_data:{|""|}
      protocol
  in
  (* Step 2: Call the gateway precompile with call(string,string,bytes).
     The bytes parameter is the Micheline binary encoding of the string
     "Hello from EVM":
       0x01 = string tag
       0x0000000e = length (14)
       48656c6c6f2066726f6d2045564d = "Hello from EVM" *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let micheline_hello = "0x010000000e48656c6c6f2066726f6d2045564d" in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:(Wei.of_tez (Tez.of_int 1))
      ~address:evm_gateway_address
      ~abi_signature:"callMichelson(string,string,bytes)"
      ~arguments:[kt1_address; "save"; micheline_hello]
      ()
  in
  (* Step 3: Sync and verify storage *)
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () =
    check_michelson_storage_value
      ~sc_rollup_node
      ~contract_hex
      ~expected:(`O [("string", `String "Hello from EVM")])
      ()
  in
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
  unit

(** Test cross-runtime view call from EVM to Michelson.

    1. Originate [view_toplevel_lib.tz] (which defines a toplevel view
       [add : nat -> nat] computing [input + storage]) with storage = 5.
    2. Call [callMichelsonView(string,string,bytes)] on the EVM gateway
       precompile via [eth_call] (simulation, so we can capture the
       return value) with destination = KT1, viewName = "add", input =
       Micheline-encoded nat 7.
    3. Verify the return bytes decode as Micheline nat 12 (= 5 + 7).
    4. Submit the same call as a real transaction and verify the
       receipt is successful.
    5. Deploy the STATICCALL forwarder contract and call it with the
       same calldata; verify the precompile is STATICCALL-compatible
       (no log emission, no alias-cache write) by checking the outer
       tx succeeds and the forwarder returns the expected Micheline
       bytes. *)
let test_cross_runtime_view_call_from_evm_to_michelson =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime view call from EVM to Michelson"
    ~tags:["cross_runtime"; "view"; "michelson"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate view_toplevel_lib.tz with storage = nat 5. The
     "add" view body is `UNPAIR ; ADD` — with the initial stack
     `pair(input, storage)` this computes `input + storage`. *)
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      protocol
  in
  (* Step 2: Micheline binary encoding of nat 7 is `0x00 07` — int tag
     then zarith 7 (fits in the 6 data bits of the first byte, no
     continuation, sign 0). The view returns `input + storage = 12`,
     which encodes as 2 bytes of Micheline (`0x00 0c`). We decode the
     ABI `(bytes,)` return, extract the Micheline body, and verify it
     decodes to the Michelson value `12 : int` via the protocol's
     Micheline JSON representation. *)
  let micheline_nat_7 = "0x0007" in
  let expected_nat_12_json = `O [("int", `String "12")] in
  (* Decode the precompile's `(bytes,)` ABI return.
     Layout: [0..64]   = offset (0x20) padded to 32 bytes
             [64..128] = length padded to 32 bytes
             [128..]   = body right-padded to 32 bytes *)
  let assert_view_returns_nat_12 raw_return =
    let hex = String.sub raw_return 2 (String.length raw_return - 2) in
    (* Expect the Micheline payload to be exactly 2 bytes. The ABI
       length word is a 256-bit big-endian integer; for a 2-byte
       payload the hex form is 62 zero nibbles followed by `02`. *)
    let length_word = String.sub hex 64 64 in
    Check.(
      (length_word
     = "0000000000000000000000000000000000000000000000000000000000000002")
        string
        ~error_msg:"Expected ABI length word for a 2-byte payload: %R, got %L") ;
    let body_hex = String.sub hex 128 4 in
    let decoded = decode_micheline_storage body_hex in
    let expected =
      JSON.annotate ~origin:"assert_view_returns_nat_12" expected_nat_12_json
      |> Option.some
    in
    Check.(
      (decoded = expected)
        (option json)
        ~error_msg:"Expected view to return Michelson value %R but got %L")
  in
  (* Build the eth_call calldata and simulate the view call. The
     default `eth_call` gas (30M) would convert to 3B Tezos milligas,
     above the hard per-operation limit on the Michelson side; pass
     an explicit 1M gas instead (-> 100M Tezos milligas, safely under
     the ~1.04B protocol limit) which is ample for the view. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* calldata =
    Cast.calldata
      ~args:[kt1_address; "add"; micheline_nat_7]
      "callMichelsonView(string,string,bytes)"
  in
  let eth_call_with_gas ~to_ ~data =
    let* response =
      Evm_node.(
        call_evm_rpc
          sequencer
          {
            method_ = "eth_call";
            parameters =
              `A
                [
                  `O
                    [
                      ("to", `String to_);
                      ("data", `String data);
                      ("gas", `String "0xf4240");
                    ];
                  `String "latest";
                ];
          })
    in
    return (response |> Evm_node.extract_result |> JSON.as_string)
  in
  let* call_return =
    eth_call_with_gas ~to_:evm_gateway_address ~data:calldata
  in
  assert_view_returns_nat_12 call_return ;
  (* Step 3: Submit the same call as a real transaction, assert
     success (the precompile completes without reverting). *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "add"; micheline_nat_7]
      ()
  in
  (* Step 4: Deploy the STATICCALL forwarder contract and call it
     with the same `callMichelsonView(...)` calldata. The forwarder's
     runtime uses the `STATICCALL` opcode to invoke the precompile,
     so this end-to-end asserts:
     - no log emission (REVM would revert on `LOG*` in a static context),
     - no alias-cache write on cache miss (REVM would revert on SSTORE),
     - no value burn.
     If any of those fail, the forwarder reverts and the outer tx's
     receipt status is `false`. *)
  let* forwarder_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:1
      ~init_code:evm_gateway_staticcall_forwarder_init_code
      ()
  in
  let* forwarder_return =
    eth_call_with_gas ~to_:forwarder_address ~data:calldata
  in
  assert_view_returns_nat_12 forwarder_return ;
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:2
      ~value:Wei.zero
      ~address:forwarder_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "add"; micheline_nat_7]
      ()
  in
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
  unit

(** Same cross-runtime view semantics as
    [test_cross_runtime_view_call_from_evm_to_michelson], but exercised
    through the *low-level* [call(string,(string,string)[],bytes,uint8)]
    entry of the gateway precompile (with the [method] argument set to
    [0] = GET) instead of the typed [callMichelsonView] entry.

    The low-level [call] entry is the generic cross-runtime HTTP bridge
    used by contracts that don't want the typed wrappers. Asserting that
    it drives a view call correctly exercises the same code path as
    [callMichelsonView] but through a different ABI, and proves the GET
    branch of [call] is STATICCALL-compatible end-to-end (no log, no
    alias-cache write, no value burn). *)
let test_cross_runtime_view_call_via_low_level_call =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime view call via low-level call entry (method=GET)"
    ~tags:["cross_runtime"; "view"; "michelson"; "low_level_call"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      protocol
  in
  let url = sf "http://tezos/%s/add" kt1_address in
  let micheline_nat_7 = "0x0007" in
  let expected_nat_12_json = `O [("int", `String "12")] in
  let assert_view_returns_nat_12 raw_return =
    let hex = String.sub raw_return 2 (String.length raw_return - 2) in
    let length_word = String.sub hex 64 64 in
    Check.(
      (length_word
     = "0000000000000000000000000000000000000000000000000000000000000002")
        string
        ~error_msg:"Expected ABI length word for a 2-byte payload: %R, got %L") ;
    let body_hex = String.sub hex 128 4 in
    let decoded = decode_micheline_storage body_hex in
    let expected =
      JSON.annotate
        ~origin:"assert_view_returns_nat_12_low_level"
        expected_nat_12_json
      |> Option.some
    in
    Check.(
      (decoded = expected)
        (option json)
        ~error_msg:"Expected view to return Michelson value %R but got %L")
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* method = 0 → GET → read-only view dispatch on the Michelson runtime. *)
  let* calldata =
    Cast.calldata
      ~args:[url; "[]"; micheline_nat_7; "0"]
      "call(string,(string,string)[],bytes,uint8)"
  in
  let eth_call_with_gas ~to_ ~data =
    let* response =
      Evm_node.(
        call_evm_rpc
          sequencer
          {
            method_ = "eth_call";
            parameters =
              `A
                [
                  `O
                    [
                      ("to", `String to_);
                      ("data", `String data);
                      ("gas", `String "0xf4240");
                    ];
                  `String "latest";
                ];
          })
    in
    return (response |> Evm_node.extract_result |> JSON.as_string)
  in
  let* call_return =
    eth_call_with_gas ~to_:evm_gateway_address ~data:calldata
  in
  assert_view_returns_nat_12 call_return ;
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_nat_7; "0"]
      ()
  in
  (* STATICCALL forwarder routes the same calldata through the STATICCALL
     opcode. If the GET path of `call` had any residual state mutation
     (log emission, alias-cache write, balance burn), REVM would revert
     and the outer tx receipt would be `false`. *)
  let* forwarder_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:1
      ~init_code:evm_gateway_staticcall_forwarder_init_code
      ()
  in
  let* forwarder_return =
    eth_call_with_gas ~to_:forwarder_address ~data:calldata
  in
  assert_view_returns_nat_12 forwarder_return ;
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:2
      ~value:Wei.zero
      ~address:forwarder_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_nat_7; "0"]
      ()
  in
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
  unit

(** Negative cases for the cross-runtime Michelson view call.

    Each case submits a real transaction whose receipt is expected to
    fail (revert, not block-abort):
    - unknown view name on an otherwise valid contract,
    - non-zero `msg.value` (the precompile must revert before issuing
      the cross-runtime call — views cannot carry value),
    - implicit-address (tz1) destination (views live only on KT1s; the
      Michelson server's view URL parser rejects the URL shape),
    - DELEGATECALL to the gateway (rejected gateway-wide, all four
      precompile entries refuse delegated frames). *)
let test_cross_runtime_view_call_negative =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime view call — negative cases"
    ~tags:["cross_runtime"; "view"; "michelson"; "negative"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      protocol
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let micheline_nat_7 = "0x0007" in
  (* Unknown view name → precompile reverts (catchable 400 on the
     Michelson side, surfaced as a failing receipt). *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "this_view_does_not_exist"; micheline_nat_7]
      ~expected_status:false
      ()
  in
  (* Non-zero `msg.value` must be rejected by the precompile before
     it even builds the GET — views cannot carry value. *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:1
      ~value:(Wei.of_tez (Tez.of_int 1))
      ~address:evm_gateway_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "add"; micheline_nat_7]
      ~expected_status:false
      ()
  in
  (* Implicit (tz1) destination: `parse_tezos_view_url` on the
     Michelson side rejects the URL as an originated-only destination,
     surfacing as a 400 → catchable revert in the precompile. *)
  let tz1_destination = Constant.bootstrap1.public_key_hash in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:2
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[tz1_destination; "add"; micheline_nat_7]
      ~expected_status:false
      ()
  in
  (* DELEGATECALL to the gateway: rejected by the gateway-wide guard
     (`target_address != bytecode_address`). Deploying a
     delegatecall-forwarder that REVERTs on non-success and pointing
     it at any typed entry — we use `callMichelsonView` here because
     we already have its calldata ready — surfaces the rejection as a
     failing receipt. *)
  let* delegatecall_forwarder =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:3
      ~init_code:evm_gateway_delegatecall_forwarder_init_code
      ()
  in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:4
      ~value:Wei.zero
      ~address:delegatecall_forwarder
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "add"; micheline_nat_7]
      ~expected_status:false
      ()
  in
  unit

(** Test cross-runtime call from Michelson to EVM with calldata.

    1. Deploy a simple EVM contract that stores [calldataload(4)] in
       storage slot 0 (i.e., the first ABI-encoded uint256 argument).
    2. From Michelson, call the gateway KT1 with entrypoint "call" and
       a Pair containing the EVM contract address, method signature
       "store(uint256)", and ABI-encoded value 42.
    3. Verify that EVM storage slot 0 = 42, proving calldata was
       forwarded through the cross-runtime gateway. *)
let test_cross_runtime_call_from_michelson_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Michelson to EVM with calldata"
    ~tags:["cross_runtime"; "call"; "calldata"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Step 1: Deploy EVM contract that stores calldataload(4) in slot 0. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:0
      ~init_code:evm_storer_init_code
      ()
  in
  (* Step 2: Call the gateway KT1 from Michelson with entrypoint "call".
     The parameter is Pair <evm_addr> (Pair "store(uint256)" <abi_bytes>)
     where abi_bytes is the ABI encoding of uint256(42). *)
  let source = Constant.bootstrap5 in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~dest:gateway_address
      ~arg_data:
        (sf
           {|Pair "%s" (Pair "store(uint256)" (Pair 0x%s None))|}
           contract_address
           abi_encoded_uint256_42)
      ~entrypoint:"call_evm"
      ~amount:1_000_000
      ()
  in
  (* Step 3: Verify EVM storage slot 0 = 42 *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sequencer
  in
  let expected_storage =
    "0x000000000000000000000000000000000000000000000000000000000000002a"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 0 = %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Test that a Michelson FAILWITH in a cross-runtime subcall does not
    force-revert the entire EVM transaction. Deploys a Solidity contract
    that makes two low-level gateway calls: one to a KT1 that succeeds
    (transfer_amount) and one to a KT1 that always FAILWITHs
    (always_fails_unit). The EVM caller catches the revert and
    continues execution. *)
let test_evm_gateway_catch_revert =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Michelson revert does not force EVM revert"
    ~tags:["cross_runtime"; "gateway"; "revert"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate transfer_amount.tz — stores AMOUNT in storage *)
  let* success_hex, success_kt1 =
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
  (* Step 3: Compile and deploy the Solidity catch-revert contract *)
  let* contract = Solidity_contracts.gateway_catch_revert evm_version in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let bytecode = Tezt.Base.read_file contract.bin in
  let* contract_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:0
      ~init_code:("0x" ^ bytecode)
      ()
  in
  (* Step 4: Call testCatchRevert(successKT1, failsKT1) with value *)
  let value = Wei.of_tez (Tez.of_int 200) in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:1
      ~value
      ~address:contract_address
      ~abi_signature:"testCatchRevert(string,string)"
      ~arguments:[success_kt1; fails_kt1]
      ()
  in
  (* Step 5: Sync rollup node so Michelson state changes are visible *)
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Step 6: Verify results *)
  (* Storage layout: all three bools are packed in slot 0.
     byte 0 = firstCallSuccess, byte 1 = secondCallSuccess,
     byte 2 = executionCompleted.
     Expected: 0x...00010001 (first=true, second=false, completed=true). *)
  let*@ slot0 =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sequencer
  in
  let byte_at hex pos =
    let len = String.length hex in
    if len >= 2 * (pos + 1) then String.sub hex (len - (2 * (pos + 1))) 2
    else "00"
  in
  let first_call = byte_at slot0 0 in
  let second_call = byte_at slot0 1 in
  let completed = byte_at slot0 2 in
  Check.(
    (first_call = "01")
      string
      ~error_msg:"Expected firstCallSuccess=true but got %L") ;
  Check.(
    (second_call = "00")
      string
      ~error_msg:"Expected secondCallSuccess=false but got %L") ;
  Check.(
    (completed = "01")
      string
      ~error_msg:"Expected executionCompleted=true but got %L") ;
  (* Verify the success call actually transferred funds: transfer_amount.tz
     stores AMOUNT (in mutez) so its storage should contain half the value. *)
  let half_mutez = Tez.to_mutez (Tez.of_int 200) / 2 in
  let* () =
    check_michelson_storage_value
      ~sc_rollup_node
      ~contract_hex:success_hex
      ~expected:(`O [("int", `String (string_of_int half_mutez))])
      ()
  in
  (* Verify the EVM contract got the 100 XTZ back from the failed subcall. *)
  let*@ contract_balance =
    Rpc.get_balance ~address:contract_address sequencer
  in
  let expected_contract_balance = Wei.of_tez (Tez.of_int 100) in
  Check.(
    (contract_balance = expected_contract_balance)
      Wei.typ
      ~error_msg:"Expected EVM contract balance %R but got %L") ;
  (* Verify the failing contract received no funds — the revert was atomic. *)
  let* tez_client = tezos_client sequencer in
  let* fails_balance = Client.get_balance_for ~account:fails_kt1 tez_client in
  Check.(
    (Tez.to_mutez fails_balance = 0)
      int
      ~error_msg:"Expected failing contract balance 0 but got %L") ;
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sequencer in
  unit

(** Test that EVM REVERT in cross-runtime calls from Michelson properly
    fails the Michelson operation and refunds the transfer amount.

    Unlike EVM which can catch reverts via low-level calls, Michelson has no
    try/catch — a failed cross-runtime call fails the entire Tezos operation.

    Both gateway entrypoints are tested:
    1. Deploy an EVM "reverter" contract that always REVERTs.
    2. Transfer to reverter via gateway KT1 default entrypoint with
       100 tez → should fail, only fees consumed.
    3. Call reverter via gateway KT1 "call" entrypoint with a function
       signature, calldata and 50 tez → should also fail, only fees
       consumed.
    4. Verify: reverter balance = 0 after both calls. *)
let test_michelson_gateway_evm_revert =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"EVM revert is properly handled via Michelson gateway"
    ~tags:["cross_runtime"; "gateway"; "revert"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Deploy EVM "reverter" — always REVERTs on any call. *)
  let* reverter_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:0
      ~init_code:evm_reverter_init_code
      ()
  in
  Log.info "Deployed reverter at %s" reverter_address ;
  (* Step 2: Transfer to reverter via gateway default entrypoint
     with 100 tez. The EVM REVERT should cause the Michelson operation
     to fail, so only the fee (1000 mutez) should be consumed. *)
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
          ~counter:1
          ~dest:gateway_address
          ~arg_data:(sf {|"%s"|} reverter_address)
          ~amount:100_000_000
          ())
  in
  (* Step 3: Call reverter via gateway "call_evm" entrypoint with a function
     signature and calldata. This exercises a different code path in the
     enshrined contract (tezosx_call_evm vs tezosx_transfer_tez).
     The calldata content is irrelevant — the reverter always REVERTs
     regardless of input. *)
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
          ~counter:2
          ~dest:gateway_address
          ~arg_data:
            (sf
               {|Pair "%s" (Pair "store(uint256)" (Pair 0x%s None))|}
               reverter_address
               abi_encoded_uint256_42)
          ~entrypoint:"call_evm"
          ~amount:50_000_000
          ())
  in
  (* Step 4: Verify the reverter received no funds from either call. *)
  let*@ reverter_balance =
    Rpc.get_balance ~address:reverter_address sequencer
  in
  Check.(
    (reverter_balance = Wei.zero)
      Wei.typ
      ~error_msg:"Expected reverter balance 0 but got %L") ;
  unit

let test_tx_queue_mixed_transaction_types ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:
      "Tx queue accepts Ethereum transactions and Tezos operations together"
    ~tags:["observer"; "tx_queue"; "mixed_types"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
  @@ fun {sandbox; observer} ->
  let tez_amount = Tez.one in
  let eth_amount = Wei.one in
  let fee = Tez.one in
  let wait_for_eth_add = Evm_node.wait_for_tx_queue_add_transaction sandbox in
  let* raw_eth_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~value:eth_amount
      ()
  in
  let* eth_hash =
    let*@ hash = Rpc.send_raw_transaction ~raw_tx:raw_eth_tx observer in
    return hash
  and* eth_hash_added = wait_for_eth_add in
  Check.((eth_hash = eth_hash_added) string)
    ~error_msg:"Expected Ethereum transaction hash %R to be added, but got %L" ;
  let wait_for_tezos_add = Evm_node.wait_for_tx_queue_add_transaction sandbox in
  let* tezos_client = tezos_client sandbox in
  let* () =
    Client.transfer
      ~amount:tez_amount
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      tezos_client
  in
  let* tezos_hash_added = wait_for_tezos_add in
  Check.((tezos_hash_added <> eth_hash) string)
    ~error_msg:
      "Expected distinct hashes for Ethereum and Tezos queue additions, but \
       both were %L" ;
  let wait_for_eth_confirmed =
    Evm_node.wait_for_tx_queue_transaction_confirmed
      ~hash:eth_hash
      ~timeout:1.
      sandbox
  in
  let wait_for_tezos_confirmed =
    Evm_node.wait_for_tx_queue_transaction_confirmed
      ~hash:tezos_hash_added
      ~timeout:1.
      sandbox
  in
  let* nb_txs =
    let*@ n = Rpc.produce_block sandbox in
    return n
  and* _ = wait_for_eth_confirmed
  and* _ = wait_for_tezos_confirmed in
  Check.(
    (nb_txs = 2)
      int
      ~error_msg:"Expected %R transactions in the block but got %L") ;

  let* tezos_receiver_balance =
    Client.get_balance_for
      ~account:Constant.bootstrap2.public_key_hash
      tezos_client
  in
  Check.(
    (Tez.to_mutez tezos_receiver_balance = Tez.to_mutez tez_amount)
      int
      ~error_msg:"Expected %R mutez but got %L") ;

  let* eth_receiver_balance =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sandbox)
      ()
  in
  Check.(
    (eth_receiver_balance = eth_amount)
      Wei.typ
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

let test_instant_confirmations ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:"Instant confirmations are executed and outputs do not diverge"
    ~tags:["observer"; "instant_confirmations"; "mixed_types"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
    ~genesis_timestamp:Test_helpers.genesis_timestamp
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
  @@ fun {sandbox; observer} ->
  let timestamp = Test_helpers.get_timestamp 0 in
  (* Create a second sandbox without IC *)
  let* sandbox_no_ic =
    Test_helpers.init_sequencer_sandbox
      ~genesis_timestamp:Test_helpers.genesis_timestamp
      ~with_runtimes:[runtime]
      ~tez_bootstrap_accounts:[Constant.bootstrap1]
      ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
      ~sequencer_keys:[Constant.bootstrap1]
      ()
  in
  let tez_amount = Tez.one in
  let eth_amount = Wei.one in
  let fee = Tez.one in
  (* Craft ETH transaction *)
  let* raw_eth_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~value:eth_amount
      ()
  in
  (* IC path: send via observer with preconfirmation *)
  let wait_for_eth_ic = Evm_node.wait_for_single_tx_execution_done observer in
  let*@ () =
    (* We must propose a timestamp else the sequencer will use its internal clock *)
    Rpc.propose_next_block_timestamp ~timestamp sandbox
  in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx:raw_eth_tx observer in
  let* _ = wait_for_eth_ic in
  let wait_for_tez_ic = Evm_node.wait_for_single_tx_execution_done observer in
  let* tezos_client_ic = tezos_client sandbox in
  let* () =
    Client.transfer
      ~amount:tez_amount
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      tezos_client_ic
  in
  let* _ = wait_for_tez_ic in
  let*@ nb_txs_ic = Rpc.produce_block ~timestamp sandbox in
  Check.(
    (nb_txs_ic = 2)
      int
      ~error_msg:"Expected %R transactions in the IC block but got %L") ;
  (* Non-IC path: send directly to sequencer *)
  let wait_for_eth_no_ic =
    Evm_node.wait_for_tx_queue_add_transaction sandbox_no_ic
  in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx:raw_eth_tx sandbox_no_ic in
  let* _ = wait_for_eth_no_ic in
  let wait_for_tez_no_ic =
    Evm_node.wait_for_tx_queue_add_transaction sandbox_no_ic
  in
  let* tezos_client_no_ic = tezos_client sandbox_no_ic in
  let* () =
    Client.transfer
      ~amount:tez_amount
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      tezos_client_no_ic
  in
  let* _ = wait_for_tez_no_ic in
  let*@ nb_txs_no_ic = Rpc.produce_block ~timestamp sandbox_no_ic in
  Check.(
    (nb_txs_no_ic = 2)
      int
      ~error_msg:"Expected %R transactions in the non-IC block but got %L") ;
  (* Compare ETH blocks *)
  let*@ ic_eth_block = Rpc.get_block_by_number ~block:"latest" sandbox in
  let*@ no_ic_eth_block =
    Rpc.get_block_by_number ~block:"latest" sandbox_no_ic
  in
  Check.(
    (ic_eth_block.hash = no_ic_eth_block.hash)
      string
      ~error_msg:"ETH block hashes should be identical: IC=%L, non-IC=%R") ;
  Check.(
    (ic_eth_block.number = no_ic_eth_block.number)
      int32
      ~error_msg:"ETH block numbers should be identical: IC=%L, non-IC=%R") ;
  Check.(
    (ic_eth_block.gasUsed = no_ic_eth_block.gasUsed)
      int64
      ~error_msg:"ETH block gasUsed should be identical: IC=%L, non-IC=%R") ;
  Check.(
    (ic_eth_block.stateRoot = no_ic_eth_block.stateRoot)
      string
      ~error_msg:"ETH block stateRoot should be identical: IC=%L, non-IC=%R") ;
  Check.(
    (ic_eth_block.transactionRoot = no_ic_eth_block.transactionRoot)
      string
      ~error_msg:"ETH transactionRoot should be identical: IC=%L, non-IC=%R") ;
  (* Compare TEZ block headers *)
  let* ic_tez_header = get_tezlink_block_header sandbox in
  let* no_ic_tez_header = get_tezlink_block_header sandbox_no_ic in
  let ic_tez_hash = JSON.(ic_tez_header |-> "hash" |> as_string) in
  let no_ic_tez_hash = JSON.(no_ic_tez_header |-> "hash" |> as_string) in
  Check.(
    (ic_tez_hash = no_ic_tez_hash)
      string
      ~error_msg:"TEZ block hashes should be identical: IC=%L, non-IC=%R") ;
  let ic_tez_level = JSON.(ic_tez_header |-> "level" |> as_int) in
  let no_ic_tez_level = JSON.(no_ic_tez_header |-> "level" |> as_int) in
  Check.(
    (ic_tez_level = no_ic_tez_level)
      int
      ~error_msg:"TEZ block levels should be identical: IC=%L, non-IC=%R") ;
  (* Compare balances *)
  let* tezos_receiver_balance_ic =
    Client.get_balance_for
      ~account:Constant.bootstrap2.public_key_hash
      tezos_client_ic
  in
  Check.(
    (Tez.to_mutez tezos_receiver_balance_ic = Tez.to_mutez tez_amount)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  let* tezos_receiver_balance_no_ic =
    Client.get_balance_for
      ~account:Constant.bootstrap2.public_key_hash
      tezos_client_no_ic
  in
  Check.(
    (Tez.to_mutez tezos_receiver_balance_ic
    = Tez.to_mutez tezos_receiver_balance_no_ic)
      int
      ~error_msg:"TEZ balances should match: IC=%L, non-IC=%R") ;
  let* eth_receiver_balance_ic =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sandbox)
      ()
  in
  let* eth_receiver_balance_no_ic =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sandbox_no_ic)
      ()
  in
  Check.(
    (eth_receiver_balance_ic = eth_receiver_balance_no_ic)
      Wei.typ
      ~error_msg:"ETH balances should match: IC=%L, non-IC=%R") ;
  unit

(** Blueprint-uniqueness invariant for EVM [state_root].

    Two different blueprints at the same level must produce different
    [state_root] values. Since [blueprint_hash] factors the timestamp,
    two blocks at the same level produced with distinct timestamps must
    diverge on [state_root]. *)
let test_state_root_blueprint_uniqueness ~runtime () =
  Test.register
    ~__FILE__
    ~title:"State root differs across distinct blueprints at same level"
    ~tags:
      (["tezosx"; "state_root"; "invariant"]
      @ runtime_tags [runtime]
      @ [Kernel.to_uses_and_tags Kernel.Latest |> fst])
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let make_sandbox () =
    Test_helpers.init_sequencer_sandbox
      ~kernel:Kernel.Latest
      ~genesis_timestamp:Test_helpers.genesis_timestamp
      ~with_runtimes:[runtime]
      ~tez_bootstrap_accounts:[Constant.bootstrap1]
      ~sequencer_keys:[Constant.bootstrap1]
      ()
  in
  let* sandbox_a = make_sandbox () in
  let* sandbox_b = make_sandbox () in
  (* Same level, different timestamps => different blueprints. *)
  let ts_a = Test_helpers.get_timestamp 0 in
  let ts_b = Test_helpers.get_timestamp 1 in
  let*@ _ = Rpc.produce_block ~timestamp:ts_a sandbox_a in
  let*@ _ = Rpc.produce_block ~timestamp:ts_b sandbox_b in
  let*@ block_a = Rpc.get_block_by_number ~block:"latest" sandbox_a in
  let*@ block_b = Rpc.get_block_by_number ~block:"latest" sandbox_b in
  Check.(
    (block_a.number = block_b.number)
      int32
      ~error_msg:"Sanity: blocks should be at the same level (A=%L, B=%R)") ;
  Check.(
    (block_a.stateRoot <> block_b.stateRoot)
      string
      ~error_msg:
        "Blueprint-uniqueness invariant broken: state_root identical across \
         distinct blueprints at the same level (A=%L, B=%R)") ;
  unit

(** Pure-Michelson divergence invariant for EVM [state_root].

    Two blueprints at the same level with identical EVM contents, identical
    timestamps, but distinct pure-Michelson ops must produce different
    EVM [state_root] values. Exercises the [michelson_ops_commitment] term
    of [blueprint_hash] — without it, post-Phase-5 the EVM side would not
    observe pure-Michelson divergence once Michelson data moves out of
    [/evm/world_state/eth_accounts].

    Before Phase 5 this test still passes via the [h(eth_accounts)]
    factor (Michelson writes under [eth_accounts/tezos]); it serves as a
    durable guardrail that remains meaningful once the keyspace split
    lands. *)
let test_state_root_pure_michelson_divergence () =
  Test.register
    ~__FILE__
    ~title:"State root differs for pure-Michelson divergence"
    ~tags:
      (["tezosx"; "state_root"; "invariant"; "michelson"]
      @ runtime_tags [Tezos]
      @ [Kernel.to_uses_and_tags Kernel.Latest |> fst])
    ~uses_admin_client:true
    ~uses_client:true
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let make_sandbox () =
    Test_helpers.init_sequencer_sandbox
      ~kernel:Kernel.Latest
      ~genesis_timestamp:Test_helpers.genesis_timestamp
      ~with_runtimes:[Tezos]
      ~tez_bootstrap_accounts:[Constant.bootstrap1]
      ()
  in
  let* sandbox_a = make_sandbox () in
  let* sandbox_b = make_sandbox () in
  let* client_a = tezos_client sandbox_a in
  let* client_b = tezos_client sandbox_b in
  let giver = Constant.bootstrap1.alias in
  let receiver = Constant.bootstrap2.alias in
  (* Same timestamp, same level, same (empty) EVM txs, but distinct
     Michelson transfers (different amounts). *)
  let ts = Test_helpers.get_timestamp 0 in
  let wait_a = Evm_node.wait_for_tx_queue_add_transaction sandbox_a in
  let* () =
    Client.transfer ~amount:Tez.one ~giver ~receiver ~burn_cap:Tez.one client_a
  in
  let* _ = wait_a in
  let wait_b = Evm_node.wait_for_tx_queue_add_transaction sandbox_b in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 2)
      ~giver
      ~receiver
      ~burn_cap:Tez.one
      client_b
  in
  let* _ = wait_b in
  let*@ _ = Rpc.produce_block ~timestamp:ts sandbox_a in
  let*@ _ = Rpc.produce_block ~timestamp:ts sandbox_b in
  let*@ block_a = Rpc.get_block_by_number ~block:"latest" sandbox_a in
  let*@ block_b = Rpc.get_block_by_number ~block:"latest" sandbox_b in
  Check.(
    (block_a.number = block_b.number)
      int32
      ~error_msg:"Sanity: blocks should be at the same level (A=%L, B=%R)") ;
  Check.(
    (block_a.stateRoot <> block_b.stateRoot)
      string
      ~error_msg:
        "Pure-Michelson-divergence invariant broken: EVM state_root identical \
         across distinct Michelson ops at same level and timestamp (A=%L, \
         B=%R)") ;
  unit

(** Test cross-runtime call from a Michelson contract to EVM via the gateway.

    Unlike [test_cross_runtime_call_from_michelson_to_evm] which calls the
    gateway KT1 directly via the delayed inbox, this test exercises the
    internal-call path: a Michelson contract makes a TRANSFER_TOKENS to the
    enshrined gateway, which then bridges into EVM.

    1. Deploy a simple EVM contract that stores [calldataload(4)] in
       storage slot 0.
    2. Originate [gateway_caller.tz] with storage pointing to the gateway
       KT1, the EVM contract address, method signature, and ABI-encoded 42.
    3. Call the Michelson contract via the delayed inbox.
    4. Verify EVM storage slot 0 = 42, proving the call went:
       delayed inbox -> Michelson contract -> TRANSFER_TOKENS ->
       enshrined gateway -> cross-runtime bridge -> EVM contract. *)
let test_cross_runtime_call_from_michelson_contract_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Michelson contract to EVM"
    ~tags:["cross_runtime"; "call"; "calldata"; "internal"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  (* Step 1: Deploy EVM contract that stores calldataload(4) in slot 0. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer
      ~sender
      ~nonce:0
      ~init_code:evm_storer_init_code
      ()
  in
  (* Step 2: Originate gateway_caller.tz with storage =
     Pair "<gateway_kt1>" (Pair "<evm_addr>" (Pair "store(uint256)" <abi_42>)) *)
  let source = Constant.bootstrap5 in
  let init_storage_data =
    sf
      {|Pair "%s" (Pair "%s" (Pair "store(uint256)" 0x%s))|}
      gateway_address
      contract_address
      abi_encoded_uint256_42
  in
  let* _contract_hex, caller_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "gateway_caller"]
      ~init_storage_data
      protocol
  in
  (* Read balances before the call. *)
  let* tez_client = tezos_client sequencer in
  let* source_balance_before =
    Client.get_balance_for ~account:source.public_key_hash tez_client
  in
  (* Step 3: Call the Michelson contract with Unit and some amount. *)
  let amount = 1_000_000 in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount
      ()
  in
  (* Step 4a: Check source balance decreased by amount + fee. *)
  let* source_balance_after =
    Client.get_balance_for ~account:source.public_key_hash tez_client
  in
  let consumed =
    Tez.to_mutez source_balance_before - Tez.to_mutez source_balance_after
  in
  Check.(
    (consumed = amount + 1000)
      int
      ~error_msg:"Expected source to consume %R mutez but consumed %L") ;
  (* Step 4b: Check that the gateway did not retain any funds. *)
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  (* Step 4c: Verify EVM storage slot 0 = 42. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sequencer
  in
  let expected_storage = "0x" ^ abi_encoded_uint256_42 in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 0 = %R but got %L") ;
  unit

(** Test cross-runtime call from a Michelson contract to a reverting EVM
    contract via the gateway.

    Same setup as [test_cross_runtime_call_from_michelson_contract_to_evm]
    but the EVM contract always reverts. We verify the Michelson transaction
    is fully rolled back: the source loses only the fee (1 000 mutez) and
    the gateway balance is unchanged. *)
let test_cross_runtime_call_from_michelson_contract_to_evm_revert =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime call from Michelson contract to reverting EVM"
    ~tags:["cross_runtime"; "call"; "revert"; "internal"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  (* Step 1: Deploy an EVM contract that always reverts.
     Runtime: 60006000fd = PUSH1 0, PUSH1 0, REVERT *)
  let init_code = "600580600b6000396000f360006000fd" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract ~sequencer ~sender ~nonce:0 ~init_code ()
  in
  (* Step 2: Originate gateway_caller.tz with storage pointing to the
     reverting contract. The method signature and arg are arbitrary since
     the contract reverts unconditionally. *)
  let source = Constant.bootstrap5 in
  let abi_encoded_arg =
    "0000000000000000000000000000000000000000000000000000000000000000"
  in
  let init_storage_data =
    sf
      {|Pair "%s" (Pair "%s" (Pair "foo()" 0x%s))|}
      gateway_address
      contract_address
      abi_encoded_arg
  in
  let* _contract_hex, caller_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "gateway_caller"]
      ~init_storage_data
      protocol
  in
  (* Read balances before the call. *)
  let* tez_client = tezos_client sequencer in
  let* source_balance_before =
    Client.get_balance_for ~account:source.public_key_hash tez_client
  in
  (* Step 3: Call the Michelson contract with Unit and some amount. *)
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount:1_000_000
      ()
  in
  (* Step 4a: Source loses only the fee (1 000 mutez), amount is refunded. *)
  let* source_balance_after =
    Client.get_balance_for ~account:source.public_key_hash tez_client
  in
  let consumed =
    Tez.to_mutez source_balance_before - Tez.to_mutez source_balance_after
  in
  Check.(
    (consumed = 1000)
      int
      ~error_msg:
        "Expected source to consume %R mutez (fee only) but consumed %L") ;
  (* Step 4b: Gateway balance is 0. *)
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Test cross-runtime transfer from a Michelson contract to an EVM EOA
    via the gateway's default entrypoint.

    1. Originate [gateway_transfer.tz] with storage pointing to the gateway
       KT1 and an EVM destination address.
    2. Call the Michelson contract via the delayed inbox with some amount.
    3. Verify the EVM destination received the funds, proving the call went:
       delayed inbox -> Michelson contract -> TRANSFER_TOKENS ->
       enshrined gateway (default) -> cross-runtime bridge -> EVM EOA. *)
let test_cross_runtime_transfer_from_michelson_contract_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime transfer from Michelson contract to EVM"
    ~tags:["cross_runtime"; "transfer"; "internal"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let transfer_amount_mutez = 1_000_000 in
  (* Step 1: Originate gateway_transfer.tz *)
  let init_storage_data =
    sf {|Pair "%s" "%s"|} gateway_address evm_destination
  in
  let* _contract_hex, caller_kt1 =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "gateway_transfer"]
      ~init_storage_data
      protocol
  in
  (* Step 2: Call the Michelson contract with Unit and some amount. *)
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount:transfer_amount_mutez
      ()
  in
  (* Step 3: Verify the EVM destination received the funds. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sequencer in
  let expected_balance = Wei.of_tez (Tez.of_mutez_int transfer_amount_mutez) in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Step 4: Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sequencer in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Test cross-runtime FA1.2 approve from EVM to Michelson.

    1. Originate [fa12_reference.tz] via delayed inbox with empty ledger,
       admin = bootstrap5, paused = false, totalSupply = 0.
    2. From EVM, call the FA1.2 wrapper predeployed contract with
       [approve(string,bytes22,uint256)] where the spender is the null
       tz1 address and the value is 42.
    3. Verify the EVM transaction succeeds (status=true), proving the
       Micheline encoding was correctly forwarded to the FA1.2 contract.

    The cross-runtime sender is always [tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU]
    (null address), which becomes SENDER in Michelson. The approve creates
    a new ledger entry for the null address with balance=0 and an allowance
    of 42 for the specified spender. *)
let test_cross_runtime_fa12_approve_from_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime FA1.2 approve from EVM to Michelson"
    ~tags:["cross_runtime"; "fa12"; "approve"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate the FA1.2 reference contract with empty ledger. *)
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "fa12_reference"]
      ~init_storage_data:
        (sf {|Pair {} (Pair "%s" (Pair False 0))|} source.public_key_hash)
      protocol
  in
  (* Step 2: Call "approve" from EVM via the predeployed FA1.2 wrapper.
     - tokenAddress: the KT1 of the FA1.2 contract (as a string)
     - spender: 22-byte binary Tezos address of the null tz1
       (tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU = 0x0000 + 20 zero bytes)
     - value: 42 *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let null_tz1_bytes22 = "0x00000000000000000000000000000000000000000000" in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:fa12_wrapper_address
      ~abi_signature:"approve(string,bytes22,uint256)"
      ~arguments:[kt1_address; null_tz1_bytes22; "42"]
      ()
  in
  (* craft_and_send_evm_transaction asserts status=true. If the Micheline
     encoding in the wrapper were wrong, the FA1.2 contract would FAILWITH
     and the cross-runtime call would revert.
     FIXME: Check the storage and perform a transfer when we have all RPC *)
  unit

(** Test cross-runtime ERC-20 transfer from Michelson to EVM via the
    ERC-20 wrapper enshrined contract.

    1. Deploy a simple EVM contract that stores [calldataload(36)] in
       storage slot 0 (i.e., the second ABI word = the uint256 amount).
    2. From Michelson, call the ERC-20 wrapper enshrined contract
       ({!erc20_wrapper_address}) with "transfer" entrypoint and
       parameter [Pair "<evm_addr>" (Pair 0x<to> 42)].
    3. Verify EVM storage slot 0 = 42, proving the ERC-20 wrapper
       correctly ABI-encoded the parameters and forwarded them. *)
let test_cross_runtime_erc20_transfer_from_michelson =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime ERC-20 transfer from Michelson to EVM"
    ~tags:["cross_runtime"; "erc20"; "transfer"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Step 1: Deploy EVM contract that stores calldataload(36) in slot 0.
     Runtime bytecode: PUSH1 0x24 | CALLDATALOAD | PUSH1 0x00 | SSTORE | STOP
     = 60 24 35 60 00 55 00 (7 bytes) *)
  let init_code = "600780600b6000396000f360243560005500" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract ~sequencer ~sender ~nonce:0 ~init_code ()
  in
  (* Step 2: Call the ERC-20 wrapper KT1 from Michelson with "transfer".
     The parameter is Pair "<evm_contract>" (Pair 0x<to_addr> 42).
     The wrapper computes transfer(address,uint256) selector and
     ABI-encodes the arguments before calling the EVM contract. *)
  let source = Constant.bootstrap5 in
  let to_address = "1111111111111111111111111111111111111111" in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~dest:erc20_wrapper_address
      ~arg_data:(sf {|Pair "%s" (Pair 0x%s 42)|} contract_address to_address)
      ~entrypoint:"transfer"
      ()
  in
  (* Step 3: Verify EVM storage slot 0 = 42, confirming the
     ERC-20 wrapper correctly encoded the uint256 amount. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sequencer
  in
  let expected_storage =
    "0x000000000000000000000000000000000000000000000000000000000000002a"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 0 = %R but got %L") ;
  unit

(** Query manager_key on a specific block hash (not head) via the Tezlink RPC.

    1. Capture the current Tezlink block header hash (the reveal block).
    2. Produce one more block via a transfer (head moves forward).
    3. Query manager_key using the captured block hash in the URL path.
    4. Verify it returns the correct public key. *)
let test_manager_key_on_block_hash =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Query manager_key on a specific block hash via Tezlink RPC"
    ~tags:["manager_key"; "block_hash"; "rpc"]
    ~with_runtimes:[Tezos]
  @@ fun {sequencer; _} _protocol ->
  let receiver_account = Constant.bootstrap2 in
  let*@ _block_number = Rpc.produce_block sequencer in

  (* Step 1: Capture the current block header hash (block with the reveal) *)
  let* header = get_tezlink_block_header sequencer in
  let block_hash = JSON.(header |-> "hash" |> as_string) in
  Log.info "Captured block hash: %s" block_hash ;
  (* Step 2: Produce one more block via a transfer to move head forward *)
  let*@ _block_number = Rpc.produce_block sequencer in

  (* Sanity check: head has moved past the reveal block *)
  let* new_header = get_tezlink_block_header sequencer in
  let new_hash = JSON.(new_header |-> "hash" |> as_string) in
  Check.(
    (block_hash <> new_hash)
      string
      ~error_msg:"Head should have moved but hash is still %L") ;
  (* Step 3: Query manager_key using the saved block hash *)
  let path =
    sf
      "/tezlink/chains/main/blocks/%s/context/contracts/%s/manager_key"
      block_hash
      receiver_account.public_key_hash
  in
  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  let manager_key = JSON.parse ~origin:"manager_key_on_block_hash" res in
  (* Step 4: Verify manager_key matches the revealed public key *)
  Check.(
    JSON.(manager_key |> as_string_opt = Some Constant.bootstrap2.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

(** Test call from EVM to Michelson via the transaction pool.

    1. Originate [store_input.tz] via tezlink (transaction pool).
    2. From EVM, call the [call] precompile with URL
       [http://tezos/<kt1>/default], POST method, and a Micheline-encoded
       string as body.
    3. Verify the Michelson contract storage was updated to the sent string. *)
let test_call_from_evm_to_michelson ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:"call from EVM to Michelson via tx pool"
    ~tags:["call"; "cross_runtime"; "tx_pool"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
  @@ fun {sandbox; observer = _} ->
  (* Step 1: Originate store_input.tz via tezlink *)
  let* tez_client = tezos_client sandbox in
  let script_path =
    Michelson_script.(
      find ["opcodes"; "store_input"] Michelson_contracts.tezlink_protocol
      |> path)
  in
  let* kt1_address =
    Client.originate_contract
      ~alias:"store_input_http"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:{|""|}
      ~prg:script_path
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 2: Call the call precompile from EVM.
     Micheline binary encoding of string "Hello from EVM":
       0x01 = string tag
       0x0000000e = length (14)
       48656c6c6f2066726f6d2045564d = "Hello from EVM" *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let url = sf "http://tezos/%s/default" kt1_address in
  let micheline_hello = "0x010000000e48656c6c6f2066726f6d2045564d" in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_hello; "1"]
      ()
  in
  (* Step 3: Verify Michelson storage *)
  let* storage_json = account_str_rpc sandbox kt1_address "storage" in
  Check.(
    (JSON.(storage_json |-> "string" |> as_string) = "Hello from EVM")
      string
      ~error_msg:"Expected Michelson storage %R but got %L") ;
  unit

(** Test call from Michelson to EVM via the transaction pool.

    1. Deploy a simple EVM contract that stores [calldataload(4)] in
       storage slot 0.
    2. From Michelson, call the gateway KT1's [call] entrypoint via
       tezlink with URL [http://ethereum/<evm_addr>], POST method, and
       calldata containing ABI-encoded uint256(42).
    3. Verify EVM storage slot 0 = 42. *)
let test_call_from_michelson_to_evm ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:"call from Michelson to EVM via tx pool"
    ~tags:["call"; "cross_runtime"; "tx_pool"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
  @@ fun {sandbox; observer = _} ->
  (* Step 1: Deploy EVM storer contract *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~init_code:evm_storer_init_code
      ()
  in
  (* Step 2: Call the gateway http_call entrypoint via tezlink.
     Parameter type: pair string (pair (list (pair string string))
                                      (pair bytes nat))
     = (url, (headers, (body, method)))
     Body = 4-byte function selector (zeros) + ABI-encoded uint256(42)
     Method 1 = POST *)
  let* tez_client = tezos_client sandbox in
  let body_hex = "00000000" ^ abi_encoded_uint256_42 in
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:gateway_address
      ~burn_cap:Tez.one
      ~entrypoint:"call"
      ~arg:
        (sf
           {|Pair "http://ethereum/%s" (Pair {} (Pair 0x%s (Pair 1 None)))|}
           contract_address
           body_hex)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 3: Verify EVM storage slot 0 = 42 *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
  in
  let expected_storage =
    "0x000000000000000000000000000000000000000000000000000000000000002a"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected EVM storage slot 0 = %R but got %L") ;
  unit

(** Test the Tezos X simulation on a Tezos operation. *)
let test_tezosx_simulation () =
  Setup.register_sandbox_test
    ~regression:true
    ~uses_client:true
    ~title:"Tezos operation simulation in Tezos X"
    ~tags:["simulation"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let* client = tezos_client sandbox in
  let amount = Tez.of_int 1 in
  let giver = Constant.bootstrap1.public_key_hash in
  let receiver = Constant.bootstrap2.public_key_hash in
  let hooks =
    let replacements =
      ("edsig\\w{94}", "[SIGNATURE]")
      :: ("\\w{250,}", "[SERIALIZED_OPERATION]")
      :: Tezos_regression.replacements
    in
    Tezos_regression.hooks_custom
      ~replace_variables:(fun s ->
        Tezos_regression.replace_variables ~replacements s)
      ()
  in
  Client.transfer ~hooks ~log_requests:true ~amount ~giver ~receiver client

(** Query the /entrypoints RPC for [address] via the Tezlink endpoint. *)
let get_entrypoints sequencer address =
  let path =
    sf
      "/tezlink/chains/main/blocks/head/context/contracts/%s/entrypoints"
      address
  in
  let* res =
    Curl.get_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  return @@ JSON.parse ~origin:"entrypoints" res

let get_script sequencer address =
  let foreign_endpoint =
    {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"}
  in
  let* response =
    RPC_core.call_raw foreign_endpoint
    @@ RPC.get_chain_block_context_contract_script ~id:address ()
  in
  return @@ JSON.parse ~origin:"script" response.RPC_core.body

(** Recursively collect entrypoint names (field annotations, i.e. [%name])
    from a Micheline type tree. Recurses into [or] branches; on any other
    node collects [%name] annotations from the node's [annots] array. *)
let rec entrypoint_names_of_type json =
  match JSON.(json |-> "prim" |> as_string_opt) with
  | Some "or" ->
      JSON.(json |-> "args" |> as_list)
      |> List.concat_map entrypoint_names_of_type
  | _ ->
      let annots =
        Option.value ~default:[] JSON.(json |-> "annots" |> as_list_opt)
      in
      List.filter_map
        (fun a ->
          let s = JSON.as_string a in
          if String.length s > 1 && s.[0] = '%' then
            Some (String.sub s 1 (String.length s - 1))
          else None)
        annots

(** Test that the /entrypoints RPC works for the enshrined TezosX Gateway
    contract.  The gateway has no Micheline code in durable storage; its
    entrypoints are returned by the kernel via the [tezosx_michelson_entrypoints]
    entrypoint.

    Note: the kernel always returns right-combs flattened into multi-arg pairs
    (e.g. [pair(a, b, c)] instead of [pair(a, pair(b, c))]) because MIR's type
    encoding ([into_micheline_optimized_legacy]) linearizes pairs.  This is
    equivalent to the L1 [normalize_types=true] behavior for comb flattening. *)
let test_entrypoints_enshrined () =
  Setup.register_sandbox_regression_test
    ~title:"Entrypoints RPC for enshrined TezosX Gateway contract"
    ~tags:["rpc"; "entrypoints"; "tezlink"; "tezosx"; "gateway"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let* ep_json = get_entrypoints sandbox gateway_address in
  Regression.capture (JSON.encode ep_json) ;

  (* The gateway exposes three entrypoints: %default (string),
     %call_evm (pair string (pair string (pair bytes (option (contract bytes))))), and
     %call (pair string (pair (list (pair string string)) (pair bytes (pair nat (option (contract bytes)))))). *)
  let entrypoints = JSON.(ep_json |-> "entrypoints") in
  let assert_ep name expected_prim =
    let prim = JSON.(entrypoints |-> name |-> "prim" |> as_string) in
    Check.(
      (prim = expected_prim)
        string
        ~error_msg:(sf "Expected prim %%R for entrypoint %s but got %%L" name))
  in
  assert_ep "default" "string" ;
  assert_ep "call" "pair" ;
  assert_ep "call_evm" "pair" ;

  unit

(** Test that /script and /entrypoints are coherent for the enshrined TezosX
    Gateway contract. The parameter type in /script must carry exactly the same
    entrypoint names as /entrypoints. *)
let test_script_coherency_enshrined () =
  Setup.register_sandbox_test
    ~title:"Script RPC coherency with entrypoints for enshrined TezosX Gateway"
    ~tags:["rpc"; "script"; "entrypoints"; "tezlink"; "tezosx"; "gateway"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let* ep_json = get_entrypoints sandbox gateway_address in
  let* script_json = get_script sandbox gateway_address in
  (* Names from /entrypoints: keys of the "entrypoints" object. *)
  let ep_names =
    JSON.(ep_json |-> "entrypoints" |> as_object)
    |> List.map fst |> List.sort String.compare
  in
  (* Names from /script: field annotations in the parameter type. *)
  let code_list = JSON.(script_json |-> "code" |> as_list) in
  let parameter_item =
    List.find
      (fun item -> JSON.(item |-> "prim" |> as_string_opt) = Some "parameter")
      code_list
  in
  let param_type = JSON.(parameter_item |-> "args" |> as_list |> List.hd) in
  let script_names =
    entrypoint_names_of_type param_type |> List.sort String.compare
  in
  Check.(
    (ep_names = script_names)
      (list string)
      ~error_msg:
        "Entrypoint names from /entrypoints (%R) do not match those in the \
         /script parameter type (%L)") ;
  unit

(** Test the http_traceCall RPC with an EVM call that triggers a
    cross-runtime call.

    1. Originate a Michelson store_input.tz contract.
    2. Call http_traceCall with an EVM call targeting the gateway
       precompile, sending data to the Michelson contract.
    3. Verify the response contains HTTP traces for the cross-runtime call. *)
let test_trace_call_evm ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:"http_traceCall traces EVM-to-Michelson cross-runtime call"
    ~tags:["rpc"; "cross_runtime"; "trace_call"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
  @@ fun {sandbox; observer = _} ->
  (* Step 1: Originate store_input.tz *)
  let* tez_client = tezos_client sandbox in
  let script_path =
    Michelson_script.(
      find ["opcodes"; "store_input"] Michelson_contracts.tezlink_protocol
      |> path)
  in
  let* kt1_address =
    Client.originate_contract
      ~alias:"store_input_trace"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:{|""|}
      ~prg:script_path
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 2: Encode calldata for the gateway precompile's call() function.
     call(string,(string,string)[],bytes,uint8) *)
  let url = sf "http://tezos/%s/default" kt1_address in
  let micheline_hello = "0x010000000e48656c6c6f2066726f6d2045564d" in
  let* calldata =
    Cast.calldata
      ~args:[url; "[]"; micheline_hello; "1"]
      "call(string,(string,string)[],bytes,uint8)"
  in
  (* Step 3: Call http_traceCall *)
  let*@ result =
    Rpc.Tezosx.http_traceCall_evm
      ~to_:evm_gateway_address
      ~data:calldata
      ~gas:"0x493E0"
      sandbox
  in
  (* Step 4: Verify response structure *)
  let traces = JSON.(result |-> "traces" |> as_list) in
  Check.(
    (List.length traces > 0)
      int
      ~error_msg:"Expected at least one HTTP trace but got none") ;
  let first_trace = List.hd traces in
  let trace_url = JSON.(first_trace |-> "url" |> as_string) in
  Check.(
    (String.length trace_url > 0)
      int
      ~error_msg:"Expected non-empty URL in trace") ;
  Log.info "http_traceCall returned %d traces" (List.length traces) ;
  Log.info "First trace URL: %s" trace_url ;
  unit

(** End-to-end test for the replay-based HTTP trace RPCs
    [http_traceTransaction], [http_traceBlockByNumber] and
    [http_traceBlockByHash].

    1. Originate a Michelson [store_input.tz] contract.
    2. Send an EVM transaction to the gateway precompile that triggers a
       cross-runtime call to the Michelson contract. This produces a block
       containing the CRAC transaction.
    3. Call [http_traceTransaction] with the tx hash — expect a non-empty
       [traces] list and a matching [txHash].
    4. Call [http_traceBlockByNumber] with the containing block's number —
       expect one entry matching the CRAC tx with non-empty traces.
    5. Call [http_traceBlockByHash] with the containing block's hash — expect
       the same entries as [http_traceBlockByNumber].
    6. Call [http_traceTransaction] on a bogus hash — expect an error.
    7. Call [http_traceBlockByNumber] on an empty (no-tx) block — expect the
       empty list, not an error. *)
let test_http_trace_replay ~runtime () =
  Setup.register_sandbox_with_oberver_test
    ~title:
      "http_traceTransaction and http_traceBlock* replay EVM-to-Michelson CRAC"
    ~tags:["rpc"; "cross_runtime"; "http_trace"; "replay"]
    ~with_runtimes:[runtime]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~eth_bootstrap_accounts:[Eth_account.bootstrap_accounts.(0).address]
  @@ fun {sandbox; observer = _} ->
  (* Step 1: originate a Michelson contract that will be the CRAC target. *)
  let* tez_client = tezos_client sandbox in
  let script_path =
    Michelson_script.(
      find ["opcodes"; "store_input"] Michelson_contracts.tezlink_protocol
      |> path)
  in
  let* kt1_address =
    Client.originate_contract
      ~alias:"store_input_http_trace"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:{|""|}
      ~prg:script_path
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Produce an extra, empty block up-front so we have a known no-CRAC block
     to exercise the empty-traces path in step 7. *)
  let*@ empty_block_number = Rpc.produce_block sandbox in
  (* Step 2: send an EVM tx that triggers a CRAC. This bakes a new block
     which contains the traced transaction. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let url = sf "http://tezos/%s/default" kt1_address in
  let micheline_hello = "0x010000000e48656c6c6f2066726f6d2045564d" in
  let* receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_hello; "1"]
      ()
  in
  let tx_hash = receipt.transactionHash in
  let block_hash = receipt.blockHash in
  let block_number = Printf.sprintf "0x%lx" receipt.blockNumber in
  (* Step 3: http_traceTransaction. *)
  let*@ result = Rpc.Tezosx.http_traceTransaction ~tx_hash sandbox in
  let returned_tx_hash = JSON.(result |-> "txHash" |> as_string) in
  Check.(
    (returned_tx_hash = tx_hash)
      string
      ~error_msg:"http_traceTransaction returned txHash %L, expected %R") ;
  let traces = JSON.(result |-> "traces" |> as_list) in
  Check.(
    (traces <> [])
      (list json)
      ~error_msg:"Expected at least one HTTP trace on the CRAC tx but got %L") ;
  (* Content assertions on the first trace: the CRAC is an EVM→Michelson
     call going through the gateway precompile, so we expect the URL to
     end with the [store_input_http_trace] KT1 we originated followed by
     the [default] entrypoint, and the response status to be a 2xx. *)
  let first_trace = List.hd traces in
  let first_url = JSON.(first_trace |-> "url" |> as_string) in
  let first_status = JSON.(first_trace |-> "responseStatus" |> as_int) in
  let expected_url_suffix = sf "/%s/default" kt1_address in
  let has_expected_suffix =
    let n = String.length first_url in
    let m = String.length expected_url_suffix in
    n >= m && String.sub first_url (n - m) m = expected_url_suffix
  in
  if not has_expected_suffix then
    Test.fail
      "Expected the CRAC trace URL to end with %S, got %S"
      expected_url_suffix
      first_url ;
  Check.(
    (first_status >= 200)
      int
      ~error_msg:"Expected the CRAC trace response status to be >= 200, got %L") ;
  Check.(
    (first_status < 300)
      int
      ~error_msg:"Expected the CRAC trace response status to be < 300, got %L") ;
  (* Step 4: http_traceBlockByNumber. *)
  let*@ by_number_result =
    Rpc.Tezosx.http_traceBlockByNumber ~block:block_number sandbox
  in
  let entries = JSON.(by_number_result |> as_list) in
  let matching_entry =
    List.find_opt
      (fun e -> JSON.(e |-> "txHash" |> as_string) = tx_hash)
      entries
  in
  let entry =
    match matching_entry with
    | Some e -> e
    | None -> Test.fail "http_traceBlockByNumber is missing the CRAC tx entry"
  in
  let entry_traces = JSON.(entry |-> "traces" |> as_list) in
  Check.(
    (entry_traces <> [])
      (list json)
      ~error_msg:
        "Expected non-empty traces on the CRAC tx in http_traceBlockByNumber \
         response, got %L") ;
  (* Step 5: http_traceBlockByHash returns the same entries as
     http_traceBlockByNumber — assert on the full JSON content so a
     regression that returns the right count with wrong txHash/traces would
     still fail the test. *)
  let*@ by_hash_result = Rpc.Tezosx.http_traceBlockByHash ~block_hash sandbox in
  let by_hash_encoded = JSON.encode by_hash_result in
  let by_number_encoded = JSON.encode by_number_result in
  Check.(
    (by_hash_encoded = by_number_encoded)
      string
      ~error_msg:
        "http_traceBlockByHash and http_traceBlockByNumber must return \
         identical bodies. by_hash=%L by_number=%R") ;
  (* Step 6: http_traceTransaction on an unknown hash returns a JSON-RPC
     error with the "transaction not found" message the RPC is supposed to
     emit, rather than a generic internal error or a silent empty result. *)
  let bogus_hash =
    "0x0000000000000000000000000000000000000000000000000000000000000042"
  in
  let* bogus_result =
    Rpc.Tezosx.http_traceTransaction ~tx_hash:bogus_hash sandbox
  in
  (match bogus_result with
  | Ok _ ->
      Test.fail
        "http_traceTransaction on an unknown tx hash should fail, got a \
         successful response"
  | Error err ->
      (* Match the specific [Transaction ... not found] wording emitted by
         [Rpc_errors.trace_transaction_not_found] rather than a bare "not
         found" substring, so an unrelated "config file not found" style
         error would fail the assertion. *)
      Check.(err.message =~ rex "Transaction .* not found")
        ~error_msg:
          "http_traceTransaction on an unknown tx hash should report the \
           [Transaction ... not found] error, got message: %L") ;
  (* Step 7: a block with no transactions returns the empty list. Verify the
     block we target is actually empty through eth_getBlockByNumber before
     asserting on its trace result — otherwise the assertion could pass for
     the wrong reason (e.g. the produced block happened to contain an
     implicit transaction). *)
  let empty_block_number_hex = Printf.sprintf "0x%x" empty_block_number in
  let*@ empty_block =
    Rpc.get_block_by_number ~block:empty_block_number_hex sandbox
  in
  let empty_block_tx_count =
    match empty_block.transactions with
    | Empty -> 0
    | Hash hashes -> List.length hashes
    | Full objs -> List.length objs
  in
  Check.(
    (empty_block_tx_count = 0)
      int
      ~error_msg:
        "Test setup: expected block %L to contain 0 transactions to exercise \
         the empty-traces path, but it contains %R") ;
  let*@ empty_result =
    Rpc.Tezosx.http_traceBlockByNumber ~block:empty_block_number_hex sandbox
  in
  let empty_entries = JSON.(empty_result |> as_list) in
  Check.(
    (empty_entries = [])
      (list json)
      ~error_msg:
        "Expected empty list for a block with no transactions, got %L entries") ;
  Log.info
    "http_traceTransaction ok (%d traces); http_traceBlockBy* ok (%d entries)"
    (List.length traces)
    (List.length entries) ;
  unit

(** Test RPC/kernel consistency over Michelson aliases of EVM address.
    Uses a Michelson contract that stores the sender and perform a cross-runtime
    call. Compare the storage contents with the result of the RPC. *)
let test_cross_runtime_evm_sender_is_alias =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Cross-runtime EVM to Michelson: sender is alias of EVM source"
    ~tags:["cross_runtime"; "sender"; "alias"]
    ~with_runtimes:[Tezos]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      protocol
    ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Originate sender.tz with a dummy initial address. *)
  let* _, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source:Constant.bootstrap1
      ~counter:1
      ~script_name:["opcodes"; "sender"]
      ~init_storage_data:(sf {|"%s"|} Constant.bootstrap1.public_key_hash)
      protocol
  in
  (* Step 2: Call the contract from EVM via the gateway precompile. *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"callMichelson(string,string,bytes)"
      ~arguments:[kt1_address; ""; "0x"]
      ()
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Step 3: Resolve the expected KT1 alias for the EVM sender. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let expected = "\"" ^ Result.get_ok alias_result ^ "\"\n" in
  let* tezos_node = tezos_client observer in
  let* storage = Client.contract_storage kt1_address tezos_node in
  Check.((storage = expected) string ~error_msg:"Expected %R but got %L") ;
  unit

(** Test cross-runtime Michelson to EVM: msg.sender is the Ethereum alias of
    the Michelson source. *)
let test_cross_runtime_michelson_sender_is_alias =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:
      "Cross-runtime Michelson to EVM: msg.sender is alias of Michelson source"
    ~tags:["cross_runtime"; "sender"; "alias"]
    ~with_runtimes:[Tezos]
  @@ fun ({sequencer; _} as setup) _protocol ->
  let source = Constant.bootstrap1 in
  (* Step 1: Deploy an EVM contract that stores msg.sender (CALLER) in slot 0. *)
  let deployer = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer
      ~sender:deployer
      ~nonce:0
      ~init_code:evm_caller_storer_init_code
      ()
  in
  (* Step 2: Call the EVM contract from the Michelson source via the gateway. *)
  let* () =
    michelson_to_evm_transfer
      ~source
      ~evm_destination:contract_address
      ~transfer_amount:Tez.zero
      ~call:("store()", "")
      setup
  in
  (* Step 3: Read msg.sender recorded by the EVM contract. *)
  let*@ stored_sender =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sequencer
  in
  (* Step 4: Resolve the alias via the RPC. *)
  let* alias_result =
    Rpc.Tezosx.tez_getTezosEthereumAddress source.public_key_hash sequencer
  in
  let rpc_address = Result.get_ok alias_result in
  let expected_stored_sender =
    (* 32 bytes with 0x moved at the beginning. *)
    "0x000000000000000000000000" ^ String.sub rpc_address 2 40
  in
  Check.(
    (stored_sender = expected_stored_sender)
      string
      ~error_msg:"Expected stored msg.sender %R but got %L") ;
  unit

(** Test that the alias forwarder contract correctly forwards funds
    from a Tezos KT1 to an EVM address via the TezosXGateway.
    This originates the forwarder contract, sends tez to it,
    and verifies the destination EVM address receives the funds. *)
let test_alias_forwarder_forwards_to_evm =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Alias forwarder contract forwards tez to EVM address"
    ~tags:["alias"; "forwarder"; "cross_runtime"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      protocol
    ->
  let source = Constant.bootstrap5 in
  let evm_receiver = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Originate the alias forwarder contract. *)
  let evm_destination = evm_receiver.address in
  let* _contract_hex, kt1_address =
    originate_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:1
      ~script_name:["mini_scenarios"; "alias_forwarder"]
      ~init_storage_data:(sf {|"%s"|} evm_destination)
      protocol
  in
  (* Step 2: Record the EVM receiver balance before the transfer. *)
  let*@ evm_balance_before =
    Rpc.get_balance ~address:evm_destination sequencer
  in
  (* Step 3: Send tez to the alias forwarder KT1.
     The forwarder contract should forward its BALANCE to the
     TezosXGateway enshrined contract, which routes funds to the EVM address. *)
  let transfer_amount = Tez.of_int 100 in
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter:2
      ~dest:kt1_address
      ~arg_data:"Unit"
      ~amount:transfer_amount_mutez
      ()
  in
  (* Step 4: Sync the rollup node. *)
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Step 5: Verify the EVM receiver balance increased. *)
  let*@ evm_balance_after =
    Rpc.get_balance ~address:evm_destination sequencer
  in
  let expected_increase = Wei.of_tez transfer_amount in
  Check.(
    (evm_balance_after = Wei.(evm_balance_before + expected_increase))
      Wei.typ
      ~error_msg:"Expected EVM balance %R but got %L") ;
  unit

(** Test that an EVM-to-Tezos cross-runtime call creates a Tezos alias
    for the EVM sender (via generate_alias with raw address bytes), and
    that sending tez to this alias KT1 forwards funds back to the EVM
    address.

    This exercises the full alias lifecycle:
    1. EVM account calls the RuntimeGateway to reach a Tezos address
       — journal.rs passes raw 20-byte address to generate_alias
    2. The alias KT1 is created with the forwarding Michelson contract
    3. Sending tez to the alias forwards it back to the EVM account *)
let test_alias_forwarder_created_by_evm_cross_runtime_call =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:
      "EVM cross-runtime call creates alias forwarder that forwards tez back"
    ~tags:["alias"; "forwarder"; "cross_runtime"; "evm_to_tezos"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let evm_sender = Eth_account.bootstrap_accounts.(0) in
  let tezos_receiver = Constant.bootstrap1.public_key_hash in
  (* Step 1: Make a cross-runtime transfer from EVM to Tezos.
     This triggers alias creation for the EVM sender on the Tezos side
     via journal.rs which passes the address as a string. *)
  let* _receipt =
    check_evm_to_michelson_transfer
      ~sequencer
      ~sender:evm_sender
      ~nonce:0
      ~tezos_destination:tezos_receiver
      ~transfer_amount:(Tez.of_int 1)
      ()
  in
  (* Step 2: Look up the Tezos alias (KT1) for the EVM sender. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_sender.address sequencer
  in
  let alias_kt1 = Result.get_ok alias_result in
  Log.info "EVM sender %s has Tezos alias %s" evm_sender.address alias_kt1 ;
  (* Step 3: Record the EVM sender balance before forwarding. *)
  let*@ evm_balance_before =
    Rpc.get_balance ~address:evm_sender.address sequencer
  in
  (* Step 4: Send tez to the alias KT1. The forwarder contract should
     forward BALANCE to the EVM address via the TezosXGateway. *)
  let forward_amount = Tez.of_int 100 in
  let forward_amount_mutez = Tez.to_mutez forward_amount in
  let* () =
    call_michelson_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source:Constant.bootstrap5
      ~counter:1
      ~dest:alias_kt1
      ~arg_data:"Unit"
      ~amount:forward_amount_mutez
      ()
  in
  (* Step 5: Sync and verify the EVM sender balance increased. *)
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let*@ evm_balance_after =
    Rpc.get_balance ~address:evm_sender.address sequencer
  in
  let expected_increase = Wei.of_tez forward_amount in
  Check.(
    (evm_balance_after = Wei.(evm_balance_before + expected_increase))
      Wei.typ
      ~error_msg:
        "Expected EVM balance to increase by forwarded amount: expected %R but \
         got %L") ;
  unit

(** Transfer tez between two accounts on the Michelson runtime and verify
    that the receiver balance is updated. *)
let test_tez_transfer =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Transfer for Michelson runtime"
    ~tags:["transfer"; "michelson"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let* client = tezos_client sequencer in

  let amount = Tez.one in

  let giver = Constant.bootstrap1.alias in
  let receiver = Constant.bootstrap2.alias in

  let wait_for_tx_in_tx_queue =
    Evm_node.wait_for_tx_queue_add_transaction sequencer
  in
  let* () = Client.transfer ~amount ~giver ~receiver ~burn_cap:Tez.one client in
  let* _tx_hash = wait_for_tx_in_tx_queue in

  let*@ nb_txs = Rpc.produce_block sequencer in
  Check.(
    (nb_txs = 1)
      int
      ~error_msg:"Expected %R transactions in the block but got %L") ;

  let* receiver_balance = Client.get_balance_for ~account:receiver client in
  Check.(
    (receiver_balance = amount)
      Tez.typ
      ~error_msg:"Expected receiver balance %R mutez but got %L") ;

  unit

(** The Michelson runtime chain ID is derived from the EVM chain ID via
    Blake2B-256 (first 4 bytes).  This test checks that the Michelson
    runtime exposes the expected derived chain ID, acting as a
    non-regression test for the derivation function.

    Derivation: Blake2B-256(chain_id as U256 little-endian), take first
    4 bytes, encode as Tezos Chain_id (base58check with Net prefix).

    Reference values:
    - EVM 1337  (test default) -> NetXUSADs17gHCN
    - EVM 42793 (Etherlink mainnet) -> NetXohUVN5QWR4f *)
let test_michelson_runtime_chain_id_derivation ~evm_chain_id ~expected_chain_id
    () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~chain_id:evm_chain_id
    ~title:
      (sf
         "Michelson runtime chain ID derivation (EVM chain ID %d)"
         evm_chain_id)
    ~tags:["chain_id"; "michelson"; "derivation"]
    ~with_runtimes:[Tezos]
  @@ fun sequencer ->
  let* client = tezos_client sequencer in
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* chain_id =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_chain_id ()
  in
  Check.(
    (chain_id = expected_chain_id)
      string
      ~error_msg:
        (sf
           "Expected Michelson runtime chain_id %%R (derived from EVM chain_id \
            %d) but got %%L"
           evm_chain_id)) ;
  unit

let test_michelson_chain_id_in_crac ~runtime () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"CHAIN_ID instruction returns correct value in CRAC"
    ~tags:["chain_id"; "michelson"; "crac"]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
    ~with_runtimes:[runtime]
  @@ fun sequencer ->
  (* Step 1: Get the expected chain_id from the tezlink RPC *)
  let* tez_client = tezos_client sequencer in
  let* expected_b58_chain_id =
    let endpoint =
      Client.(
        Foreign_endpoint
          Endpoint.
            {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
    in
    Client.RPC.call ~endpoint tez_client @@ RPC.get_chain_chain_id ()
  in
  (* Step 2: Originate chain_id_store.tz via tezlink *)
  let script_path =
    Michelson_script.(
      find ["opcodes"; "chain_id_store"] Michelson_contracts.tezlink_protocol
      |> path)
  in
  let* kt1_address =
    Client.originate_contract
      ~alias:"chain_id_store"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"None"
      ~prg:script_path
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  (* Step 3: Call via the EVM gateway precompile (CRAC).
     Micheline binary encoding of Unit: 0x03 (Prim, no args) 0x0b (Unit). *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let url = sf "http://tezos/%s/default" kt1_address in
  let micheline_unit = "0x030b" in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_unit; "1"]
      ()
  in
  (* Step 4: Read storage and verify chain_id matches the RPC value.
     The RPC returns Micheline JSON: {"prim":"Some","args":[{"bytes":"..."}]}
     Convert the hex bytes to a b58check chain_id for comparison. *)
  let* storage_json = account_str_rpc sequencer kt1_address "storage" in
  let stored_hex =
    JSON.(storage_json |-> "args" |=> 0 |-> "bytes" |> as_string)
  in
  let stored_chain_id =
    Hex.to_bytes (`Hex stored_hex)
    |> Tezos_crypto.Hashed.Chain_id.of_bytes_exn
    |> Tezos_crypto.Hashed.Chain_id.to_b58check
  in
  Check.(
    (stored_chain_id = expected_b58_chain_id)
      string
      ~error_msg:
        "Expected CHAIN_ID instruction to return %R but contract storage \
         contains %L") ;
  unit

(** tz2 (secp256k1) test account generated with octez-client. *)
let tz2_bootstrap : Account.key =
  {
    alias = "tz2_bootstrap";
    public_key_hash = "tz2GpnbHg8Hg64pWp42hhJmSaUUWWuHw9xjX";
    public_key = "sppk7axNHeFvBJbCSAySkyABv8VwuyxwVvFwWK1p49myMkSyRWFXTjX";
    secret_key =
      Account.Unencrypted
        "spsk1k4A9FYh378QPYDa6RCMiGoo797h7JStrNcvFh1ouWbphHMZyG";
  }

(** tz3 (P256) test account generated with octez-client. *)
let tz3_bootstrap : Account.key =
  {
    alias = "tz3_bootstrap";
    public_key_hash = "tz3QjAwKfmeUi74PwGjy77ZbwCb3xXtA2woa";
    public_key = "p2pk67vm51Cd59hXf8hmXPW9cwsHR1bLE5WC1WaSVVgyDH1oPkqigT8";
    secret_key =
      Account.Unencrypted
        "p2sk2rmHAdX9dDPrVSS4JJQK4i3cA3gRAERXEpWHZuSb7idjeMFD2U";
  }

(** Test EIP-1271 isValidSignature for a single account: create alias,
    sign a hash, assert valid signature returns the magic value,
    and assert wrong hash returns failure. *)
let check_eip1271_for_account ~setup ~counter (account : Account.key) =
  let label = account.alias in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let eip1271_magic =
    "0x1626ba7e00000000000000000000000000000000000000000000000000000000"
  in
  let eip1271_failure =
    "0xffffffff00000000000000000000000000000000000000000000000000000000"
  in
  (* Create alias via cross-runtime transfer *)
  let* () =
    michelson_to_evm_transfer
      ~source:account
      ~evm_destination
      ~transfer_amount:(Tez.of_int 1)
      ~counter
      setup
  in
  let*@ alias_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress
      account.public_key_hash
      setup.sequencer
  in
  Log.info "[%s] alias EVM address: %s" label alias_address ;
  (* Sign and verify *)
  let hash_hex =
    "0x64b58ce5683684531652feb45eb41f30ef7ebae57a440d942ca0579b7309ff83"
  in
  let hash_bytes =
    Hex.to_bytes
      (`Hex "64b58ce5683684531652feb45eb41f30ef7ebae57a440d942ca0579b7309ff83")
  in
  let signature = Account.sign_bytes ~signer:account hash_bytes in
  let (`Hex sig_hex) = Tezos_crypto.Signature.to_hex signature in
  let sig_with_prefix = "0x" ^ sig_hex in
  let* calldata =
    Cast.calldata
      ~args:[hash_hex; sig_with_prefix]
      "isValidSignature(bytes32,bytes)"
  in
  let*@ call_result =
    Rpc.call ~to_:alias_address ~data:calldata setup.sequencer
  in
  Check.(
    (call_result = eip1271_magic)
      string
      ~error_msg:(sf "[%s] Expected isValidSignature %%R but got %%L" label)) ;
  Log.info "[%s] valid signature returns magic" label ;
  (* Wrong hash should always fail *)
  let wrong_hash =
    "0x0000000000000000000000000000000000000000000000000000000000000001"
  in
  let* bad_calldata =
    Cast.calldata
      ~args:[wrong_hash; sig_with_prefix]
      "isValidSignature(bytes32,bytes)"
  in
  let*@ bad_result =
    Rpc.call ~to_:alias_address ~data:bad_calldata setup.sequencer
  in
  Check.(
    (bad_result = eip1271_failure)
      string
      ~error_msg:(sf "[%s] wrong hash: expected %%R but got %%L" label)) ;
  Log.info "[%s] wrong hash → failure" label ;
  unit

(** EIP-1271 signature verification across tz1, tz2 and tz3 key types *)
let test_eip1271_signature_verification =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"EIP-1271 isValidSignature on tz1, tz2 and tz3 aliases"
    ~tags:["eip1271"; "signature"; "alias"; "cross_runtime"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun setup _protocol ->
  let Tezt_etherlink.Setup.
        {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      =
    setup
  in
  (* Fund and reveal tz2/tz3 accounts via delayed inbox since the
     config tool only supports Ed25519 bootstrap accounts. *)
  let fund_and_reveal ~source_counter account =
    (* Transfer from bootstrap5 to fund the account *)
    let* transfer =
      Operation.Manager.(
        operation
          [
            make
              ~fee:1000
              ~counter:source_counter
              ~source:Constant.bootstrap5
              (transfer ~dest:account ~amount:100_000_000 ());
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
    (* Reveal the account's public key *)
    let* reveal =
      Operation.Manager.(
        operation
          [make ~fee:1000 ~counter:1 ~source:account (reveal account ())])
        client
    in
    send_tezos_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      reveal
  in
  let* () = fund_and_reveal ~source_counter:1 tz2_bootstrap in
  let* () = fund_and_reveal ~source_counter:2 tz3_bootstrap in
  (* tz1: Ed25519 *)
  let* () = check_eip1271_for_account ~setup ~counter:1 Constant.bootstrap1 in
  (* tz2: secp256k1 *)
  let* () = check_eip1271_for_account ~setup ~counter:2 tz2_bootstrap in
  (* tz3: P256 *)
  check_eip1271_for_account ~setup ~counter:2 tz3_bootstrap

(** Regression test: a well-formed signature signed over a different hash
    than the one submitted must be rejected by isValidSignature. *)
let test_eip1271_wrong_signature_rejected =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"EIP-1271 isValidSignature rejects a wrong signature"
    ~tags:["eip1271"; "signature"; "alias"; "wrong_signature"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun setup _protocol ->
  let account = Constant.bootstrap1 in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let eip1271_failure =
    "0xffffffff00000000000000000000000000000000000000000000000000000000"
  in
  let* () =
    michelson_to_evm_transfer
      ~source:account
      ~evm_destination
      ~transfer_amount:(Tez.of_int 1)
      ~counter:1
      setup
  in
  let*@ alias_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress
      account.public_key_hash
      setup.sequencer
  in
  (* The signature is produced over [signed_hash] but we submit
     [claimed_hash] to [isValidSignature]. The two are distinct so
     verification must fail. *)
  let claimed_hash =
    "0x0000000000000000000000000000000000000000000000000000000000000001"
  in
  let signed_hash_bytes =
    Hex.to_bytes
      (`Hex "0000000000000000000000000000000000000000000000000000000000000002")
  in
  let wrong_sig = Account.sign_bytes ~signer:account signed_hash_bytes in
  let (`Hex wrong_sig_hex) = Tezos_crypto.Signature.to_hex wrong_sig in
  let* calldata =
    Cast.calldata
      ~args:[claimed_hash; "0x" ^ wrong_sig_hex]
      "isValidSignature(bytes32,bytes)"
  in
  let*@ call_result =
    Rpc.call ~to_:alias_address ~data:calldata setup.sequencer
  in
  Check.(
    (call_result = eip1271_failure)
      string
      ~error_msg:
        "isValidSignature on a wrong signature returned %L; expected the \
         EIP-1271 failure magic %R") ;
  unit

let test_meta_block_rpcs ~runtime () =
  Setup.register_sandbox_test
    ~title:"tez_getMetaBlockByNumber and tez_getMetaBlockByHash"
    ~tags:["meta_block"; "rpc"]
    ~with_runtimes:[runtime]
  @@ fun sandbox ->
  (* Produce a block so there is a level with both EVM and Michelson hashes *)
  let*@ _nb_txs = Rpc.produce_block sandbox in
  (* Get the EVM block at "latest" to retrieve its hash and number *)
  let*@ evm_block = Rpc.get_block_by_number ~block:"latest" sandbox in
  let evm_hash = evm_block.hash in
  let hex_level = Format.sprintf "0x%lx" evm_block.number in
  (* 1. tez_getMetaBlockByNumber: verify that symbolic block parameters are
     supported, and that the level, EVM hash and Michelson hash match. *)
  let*@ meta = Rpc.Tezosx.tez_getMetaBlockByNumber ~block:"latest" sandbox in
  Check.(
    (JSON.(meta |-> "level" |> as_string) = hex_level)
      string
      ~error_msg:"tez_getMetaBlockByNumber: expected level %R, got %L") ;
  Check.(
    (JSON.(meta |-> "block_hashes" |-> "evm_block_hash" |> as_string) = evm_hash)
      string
      ~error_msg:"tez_getMetaBlockByNumber: expected EVM hash %R, got %L") ;
  let*@ meta_by_number =
    Rpc.Tezosx.tez_getMetaBlockByNumber ~block:hex_level sandbox
  in
  Check.(
    (meta_by_number = meta)
      json
      ~error_msg:"tez_getMetaBlockByNumber (number): expected %R, got %L") ;
  let michelson_hash =
    JSON.(meta |-> "block_hashes" |-> "michelson_block_hash" |> as_string)
  in
  (* 2. tez_getMetaBlockByHash with the EVM hash returns the same meta-block. *)
  let*@ meta_by_evm =
    Rpc.Tezosx.tez_getMetaBlockByHash ~hash:evm_hash sandbox
  in
  Check.(
    (meta_by_evm = meta)
      json
      ~error_msg:"tez_getMetaBlockByHash (EVM): expected %R, got %L") ;
  (* 3. tez_getMetaBlockByHash with the Michelson hash returns the same
     meta-block. *)
  let*@ meta_by_mic =
    Rpc.Tezosx.tez_getMetaBlockByHash ~hash:michelson_hash sandbox
  in
  Check.(
    (meta_by_mic = meta)
      json
      ~error_msg:"tez_getMetaBlockByHash (Michelson): expected %R, got %L") ;
  unit

let test_meta_block_rpcs_without_michelson_runtime () =
  Setup.register_sandbox_test
    ~title:
      "tez_getMetaBlockByNumber returns null Michelson hash without Tezos \
       runtime"
    ~tags:["meta_block"; "rpc"]
    ~with_runtimes:[]
  @@ fun sandbox ->
  let*@ _nb_txs = Rpc.produce_block sandbox in
  let*@ evm_block = Rpc.get_block_by_number ~block:"latest" sandbox in
  let*@ meta = Rpc.Tezosx.tez_getMetaBlockByNumber ~block:"latest" sandbox in
  Check.(
    (JSON.(meta |-> "block_hashes" |-> "evm_block_hash" |> as_string)
    = evm_block.hash)
      string
      ~error_msg:"tez_getMetaBlockByNumber: expected EVM hash %R, got %L") ;
  Check.is_true
    ~__LOC__
    JSON.(meta |-> "block_hashes" |-> "michelson_block_hash" |> is_null)
    ~error_msg:
      "tez_getMetaBlockByNumber: Michelson hash should be null when the Tezos \
       runtime is not activated" ;
  unit

let () =
  test_bootstrap_kernel_config () ;
  test_deposit [Alpha] ;
  test_reveal [Alpha] ;
  test_delayed_inbox_transfer [Alpha] ;
  test_cross_runtime_transfer_from_evm_to_tz [Alpha] ;
  test_cross_runtime_transfer_to_evm [Alpha] ;
  test_cross_runtime_transfer_to_evm_via_call [Alpha] ;
  test_cross_runtime_transfer_to_evm_via_call_evm [Alpha] ;
  test_cross_runtime_call_executes_evm_bytecode [Alpha] ;
  test_cross_runtime_transfer_from_evm_to_kt1 [Alpha] ;
  test_cross_runtime_call_failwith [Alpha] ;
  test_cross_runtime_call_from_evm_to_michelson [Alpha] ;
  test_cross_runtime_view_call_from_evm_to_michelson [Alpha] ;
  test_cross_runtime_view_call_via_low_level_call [Alpha] ;
  test_cross_runtime_view_call_negative [Alpha] ;
  test_cross_runtime_call_from_michelson_to_evm [Alpha] ;
  test_michelson_gateway_evm_revert [Alpha] ;
  test_cross_runtime_fa12_approve_from_evm [Alpha] ;
  test_cross_runtime_erc20_transfer_from_michelson [Alpha] ;
  test_cross_runtime_call_from_michelson_contract_to_evm [Alpha] ;
  test_cross_runtime_call_from_michelson_contract_to_evm_revert [Alpha] ;
  test_cross_runtime_transfer_from_michelson_contract_to_evm [Alpha] ;
  test_tezos_block_stored_after_deposit [Alpha] ;
  test_michelson_origination_and_call [Alpha] ;
  test_michelson_get_balance [Alpha] ;
  test_michelson_call_nonexistent_contract [Alpha] ;
  test_michelson_call_wrong_entrypoint [Alpha] ;
  test_michelson_origination_wrong_storage_type [Alpha] ;
  test_michelson_call_failwith [Alpha] ;
  test_michelson_inter_contract_call [Alpha] ;
  test_michelson_internal_call_revert [Alpha] ;
  test_evm_gateway_catch_revert [Alpha] ;
  test_eth_rpc_with_alias ~runtime:Tezos [Alpha] ;
  test_runtime_feature_flag ~runtime:Tezos () ;
  test_gas_refund_feature_flag () ;
  test_get_tezos_ethereum_address_rpc ~runtime:Tezos () ;
  test_get_ethereum_tezos_address_rpc ~runtime:Tezos () ;
  test_tx_queue_mixed_transaction_types ~runtime:Tezos () ;
  test_manager_key_on_block_hash [Alpha] ;
  test_instant_confirmations ~runtime:Tezos () ;
  test_state_root_blueprint_uniqueness ~runtime:Tezos () ;
  test_state_root_pure_michelson_divergence () ;
  test_nested_crac [Alpha] ;
  test_call_from_evm_to_michelson ~runtime:Tezos () ;
  test_call_from_michelson_to_evm ~runtime:Tezos () ;
  test_tezosx_simulation () ;
  test_entrypoints_enshrined () ;
  test_script_coherency_enshrined () ;
  test_trace_call_evm ~runtime:Tezos () ;
  test_http_trace_replay ~runtime:Tezos () ;
  test_cross_runtime_evm_sender_is_alias [Alpha] ;
  test_cross_runtime_michelson_sender_is_alias [Alpha] ;
  test_alias_forwarder_forwards_to_evm [Alpha] ;
  test_alias_forwarder_created_by_evm_cross_runtime_call [Alpha] ;
  test_tez_transfer [Alpha] ;
  test_michelson_runtime_chain_id_derivation
    ~evm_chain_id:1337
    ~expected_chain_id:"NetXUSADs17gHCN"
    () ;
  test_michelson_runtime_chain_id_derivation
    ~evm_chain_id:42793
    ~expected_chain_id:"NetXohUVN5QWR4f"
    () ;
  test_michelson_chain_id_in_crac ~runtime:Tezos () ;
  test_eip1271_signature_verification [Alpha] ;
  test_eip1271_wrong_signature_rejected [Alpha] ;
  test_meta_block_rpcs ~runtime:Tezos () ;
  test_meta_block_rpcs_without_michelson_runtime ()
