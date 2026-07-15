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
   related to the bridge / deposits, or when we need to distinguish between the
   latest and finalized block) is it useful to use [register_fullstack_test].

   Michelson runtime: a Michelson operation can be injected directly into the
   node tx pool and bake a block, using the sandbox helpers:
     - [sandbox_michelson_to_evm_transfer]   (gateway %call / %call_evm)
     - [sandbox_originate_michelson_contract] (Client.originate_contract)
     - [sandbox_call_michelson_contract]      (Client.transfer to a KT1)
   then assert via block / RPC / storage inspection
   ([Rpc.get_storage_at], [Client.contract_storage], [Client.get_balance_for],
   the operation receipt status) -- never via fee deltas.

   Fees: let the node estimate fee/gas/storage. Only hardcode them when you
   also pass [~force:true], which is required for operations the client
   predicts will fail (FAILWITH, removed/unknown entrypoint, a CRAC that errors
   such as a state-mutating EVM call on the GET/static path). On the success
   path, hardcoding fees just triggers [insufficient_fees].

   [Client.contract_storage] may pretty-print storage across several lines;
   compare with [remove_whitespace] rather than an exact-layout string. *)

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
      ?(kernel = Kernel.Latest) ?additional_uses ?tez_bootstrap_accounts
      ?time_between_blocks ?michelson_runtime_chain_id ?da_fee =
    Setup.register_test
      ~__FILE__
      ~rpc_server:Evm_node.Resto
      ~title
      ~tags:(["tezosx"] @ runtime_tags with_runtimes @ tags)
      ~kernel
      ~with_runtimes
      ~enable_dal:false
      ?additional_uses
      ?tez_bootstrap_accounts
      ?time_between_blocks
      ?michelson_runtime_chain_id
      ?da_fee
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

(* A recent Tezos X block hash to use as [branch]; sync the rollup first so it is committed to the kernel's live_blocks. *)
let tezlink_branch ~sc_rollup_node ~client ~sequencer =
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"}
  in
  RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_hash ()

(** [sandbox_test_script_path names] resolves a Michelson test script,
    identified by its [michelson_test_scripts] path components, to its on-disk
    path for the [Alpha] protocol. Shared by the sandbox-based Michelson
    tests. *)
let sandbox_test_script_path names = Michelson_script.(find names Alpha |> path)

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

(** [check_block_op_status ~index ~expected_status client] fetches the
    operations of the head block through [client], asserts the block holds
    exactly one manager operation, and checks that its top-level
    [operation_result] status equals [expected_status] (e.g. ["failed"]).
    Fails the test otherwise.

    Handy after force-injecting an operation the client predicted would fail:
    it turns the "the op is indeed [failed]" assertion into a one-liner. *)
let check_block_op_status ?block ~index ~expected_status client =
  let* ops =
    Client.RPC.call client (Node.RPC.get_chain_block_operations ?block ())
  in
  match
    List.nth_opt (JSON.as_list ops |> List.concat_map JSON.as_list) index
  with
  | Some op ->
      let status =
        JSON.(
          op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
          |-> "status" |> as_string)
      in
      Check.(
        (status = expected_status)
          string
          ~error_msg:"Expected operation status %R but got %L") ;
      unit
  | _ -> Test.fail "Expected one operation in the block, got something else"

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

(** [michelson_contract_hex_of_kt1 kt1] is the hex-encoded [Contract_repr.t]
    durable-storage key for [kt1] (inverse of
    [decode_michelson_contract_address]). *)
let michelson_contract_hex_of_kt1 kt1 =
  let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
  let contract = Result.get_ok (C.of_b58check kt1) in
  let (`Hex h) =
    Hex.of_bytes (Data_encoding.Binary.to_bytes_exn C.encoding contract)
  in
  h

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

(** [call_michelson_contract_via_delayed_inbox] calls a Michelson contract
    via the delayed inbox. Converts the argument from Michelson notation to
    JSON, forges and sends the call operation. *)
let call_michelson_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node
    ~client ~l1_contracts ~sequencer ~source ~counter ~dest ~arg_data
    ?(entrypoint = "default") ?(amount = 0) ?(gas_limit = 100_000) () =
  let* arg = Client.convert_data_to_json ~data:arg_data client in
  let* branch = tezlink_branch ~sc_rollup_node ~client ~sequencer in
  let* call_op =
    Operation.Manager.(
      operation
        ~branch
        [
          make
            ~fee:1000000
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

(** EVM precompile address for cross-runtime gateway calls.
    See [revm/src/precompiles/constants.rs:RUNTIME_GATEWAY_PRECOMPILE_ADDRESS]. *)
let evm_gateway_address = "0xff00000000000000000000000000000000000007"

(** [evm_alias_of_kt1 kt1_address] is the deterministic Ethereum alias of
    a Michelson contract — [keccak256(kt1_base58_ascii)] truncated to 20
    bytes — as lowercase hex (40 chars, no 0x prefix). *)
let evm_alias_of_kt1 kt1_address =
  let digest =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string kt1_address)
  in
  Hex.show (Hex.of_bytes (Bytes.sub digest 0 20))

(** EVM predeployed address for the FA1.2 wrapper contract.
    See [revm/src/precompiles/constants.rs:FA12_WRAPPER_SOL_ADDR]. *)
let fa12_wrapper_address = "0xff00000000000000000000000000000000ffff09"

(** Null implicit address — the synthetic source the kernel uses for
    all CRAC-shape operations (top-level transactions, alias-forwarder
    originations, CRAC events). Same constant as [handler_address] in
    [cross_runtime.ml]; kept local here to avoid a cross-file dep
    between two tezt test modules. *)
let null_pkh = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

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
    address), through the generic [call] entrypoint as an HTTP POST with an
    empty body.  Asserts the receipt status matches [expected_status] (default
    [true]).  Returns the transaction receipt. *)
let call_evm_gateway ~sequencer ~sender ~nonce ~value ~destination
    ?expected_status () =
  craft_and_send_evm_transaction
    ~sequencer
    ~sender
    ~nonce
    ~value
    ~address:evm_gateway_address
    ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
    ~arguments:[sf "http://tezos/%s" destination; "[]"; "0x"; "1"]
    ?expected_status
    ()

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

let test_reveal () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Reveal tezos native account"
    ~tags:["reveal"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Fund a freshly generated account from a bootstrap account: it now exists
     and has a balance, but its manager key is still unrevealed. *)
  let* receiver_account = Client.gen_and_show_keys tez_client in
  let* () =
    Client.transfer
      ~giver:source.alias
      ~receiver:receiver_account.public_key_hash
      ~amount:(Tez.of_int 1000)
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Reveal the account's manager key. *)
  let* () =
    Client.reveal ~src:receiver_account.alias tez_client |> Runnable.run
  in
  let*@ _ = Rpc.produce_block sandbox in
  let* manager_key = account_rpc sandbox receiver_account "manager_key" in
  Check.(
    JSON.(manager_key |> as_string_opt = Some receiver_account.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_delayed_inbox_transfer () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Transfer on tezos native account"
    ~tags:["transfer"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  let* receiver_account = Client.gen_and_show_keys tez_client in
  let* () =
    Client.transfer
      ~giver:depositor.alias
      ~receiver:receiver_account.public_key_hash
      ~amount
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  let* balance =
    Client.get_balance_for ~account:receiver_account.public_key_hash tez_client
  in
  Check.(
    (Tez.to_mutez balance = 1_000_000_000)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

(** A Tezos manager operation whose fee does not cover the configured DA fee is
    refused by the sequencer's tezlink prevalidator ([insufficient_fees]) when
    injected directly, but the delayed inbox force-includes it without charging
    the DA fee (the kernel's delayed-inbox path skips the DA fee; see also
    [arb_da_fee_for_delayed_inbox] in evm_sequencer.ml). This demonstrates the
    delayed inbox as a censorship-resistant ingress: an operation the sequencer
    rejects on fees still gets in. *)
let test_low_fee_op_refused_by_sequencer_accepted_via_delayed_inbox =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
      (* Any DA fee > 0 makes a zero-fee operation insufficient. *)
    ~da_fee:(Wei.of_eth_int 4)
    ~title:
      "Low-fee Tezos op is refused by the sequencer but accepted via the \
       delayed inbox"
    ~tags:["delayed_inbox"; "fee"; "prevalidation"; "censorship"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap1 in
  let receiver = Constant.bootstrap2 in
  let amount = Tez.of_int 10 in
  (* Enough to pay execution fees, but not enough to pay inclusion fees *)
  let fee = Tez.of_mutez_int 500 in
  let* tez_client = tezos_client sequencer in
  (* Part 1: injecting directly into the sequencer's tezlink tx pool with a
     zero fee is refused by prevalidation. *)
  let process =
    Client.spawn_transfer
      ~fee
      ~gas_limit:10000
      ~storage_limit:0
      ~burn_cap:Tez.one
      ~amount
      ~giver:source.alias
      ~receiver:receiver.public_key_hash
      tez_client
  in
  let* err = Process.check_and_read_stderr ~expect_failure:true process in
  Check.(err =~ rex "evm_node.dev.insufficient_fees")
    ~error_msg:
      "Direct injection should be refused by prevalidation with %R but got %L" ;
  (* Part 2: the same zero-fee transfer, sent through the delayed inbox, is
     force-included without a DA fee and applied — the receiver is credited. *)
  let* receiver_balance_before =
    Client.get_balance_for ~account:receiver.public_key_hash tez_client
  in
  let* branch = tezlink_branch ~sc_rollup_node ~client ~sequencer in
  let* transfer_op =
    Operation.Manager.(
      operation
        ~branch
        [
          make
            ~fee:(Tez.to_mutez fee)
            ~counter:1
            ~gas_limit:10000
            ~storage_limit:0
            ~source
            (transfer ~dest:receiver ~amount:(Tez.to_mutez amount) ());
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
      transfer_op
  in
  let* () =
    check_block_op_status ~index:0 ~expected_status:"applied" tez_client
  in
  let* receiver_balance_after =
    Client.get_balance_for ~account:receiver.public_key_hash tez_client
  in
  Check.(
    (Tez.to_mutez receiver_balance_after
    = Tez.to_mutez receiver_balance_before + Tez.to_mutez amount)
      int
      ~error_msg:
        "Delayed-inbox transfer should have credited the receiver by the full \
         amount (DA fee waived): expected %R but got %L") ;
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

(** Originate a Michelson contract, call it, and verify that its storage is
    updated.

    Uses [store_input.tz] from [michelson_test_scripts/opcodes]: a minimal
    contract that stores its string parameter as the new storage.

    Steps:
    1. Originate the store_input contract with an initial balance.
    2. Call the contract with a string parameter and some XTZ.
    3. Read the contract storage and verify it matches the parameter. *)
let test_michelson_origination_and_call () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson origination and call on tezos X"
    ~tags:["origination"; "call"; "michelson"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate a Michelson contract (store_input.tz). *)
  let init_balance = 1_234_567 in
  let* kt1_address =
    Client.originate_contract
      ~src:source.alias
      ~burn_cap:Tez.one
      ~prg:(sandbox_test_script_path ["opcodes"; "store_input"])
      ~init:{|""|}
      ~alias:"store_input"
      ~amount:(Tez.of_mutez_int init_balance)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 2: Call the contract with a string parameter and send XTZ. *)
  let expected_storage = "Hello Tezos X" in
  let* () =
    Client.transfer
      ~giver:source.alias
      ~receiver:kt1_address
      ~arg:(sf {|"%s"|} expected_storage)
      ~amount:(Tez.of_mutez_int 1_234_567)
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 3: Read the contract storage and verify it matches the parameter. *)
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim storage = sf {|"%s"|} expected_storage)
      string
      ~error_msg:"Expected contract storage %R but got %L") ;
  unit

(** Originate a Michelson contract and check that [Client.get_balance_for]
    works on the KT1 address via the tezlink RPC. *)
let test_michelson_get_balance () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Get balance of Michelson contract on Tezos X"
    ~tags:["balance"; "michelson"; "rpc"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let init_balance = 1_234_567 in
  let* tez_client = tezos_client sandbox in
  let* kt1_address =
    Client.originate_contract
      ~src:source.alias
      ~burn_cap:Tez.one
      ~prg:(sandbox_test_script_path ["opcodes"; "store_input"])
      ~init:{|""|}
      ~alias:"store_input"
      ~amount:(Tez.of_mutez_int init_balance)
      tez_client
  in
  let* _ = Rpc.produce_block sandbox in
  let* balance = Client.get_balance_for ~account:kt1_address tez_client in
  Check.(
    (Tez.to_mutez balance = init_balance)
      int
      ~error_msg:"Expected %R mutez but got %L") ;
  unit

(** Call a non-existing KT1 address. The operation should be included but fail:
    only fees are consumed, the transferred amount is not deducted from the
    sender's balance. *)
let test_michelson_call_nonexistent_contract () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson call to non-existing contract on tezos X"
    ~tags:["call"; "michelson"; "nonexistent"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  let fake_kt1 = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" in
  let transfer_amount = 1_234_567 in
  (* Call a non-existing KT1: the operation is force-injected and must fail,
     consuming only fees. [~force:true] is required to inject an operation the
     client predicts will fail. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:fake_kt1
      ~arg:{|Unit|}
      ~amount:(Tez.of_mutez_int transfer_amount)
      ~gas_limit:500000
      ~storage_limit:1
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  check_block_op_status ~index:0 ~expected_status:"failed" tez_client

(** Call an existing Michelson contract with a non-existing entrypoint. The
    operation should fail: only fees are consumed. *)
let test_michelson_call_wrong_entrypoint () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson call to wrong entrypoint on tezos X"
    ~tags:["call"; "michelson"; "entrypoint"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Originate store_input.tz (only has the "default" entrypoint). *)
  let* kt1_address =
    Client.originate_contract
      ~prg:(sandbox_test_script_path ["opcodes"; "store_input"])
      ~alias:"store_input"
      ~src:source.alias
      ~init:{|""|}
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Call a non-existing entrypoint: the operation is force-injected and must
     fail, consuming only fees. [~force:true] is required to inject an operation
     the client predicts will fail. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:kt1_address
      ~entrypoint:"nonexistent"
      ~arg:{|"hello"|}
      ~amount:(Tez.of_mutez_int 1_234_567)
      ~gas_limit:500000
      ~storage_limit:1
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  check_block_op_status ~index:0 ~expected_status:"failed" tez_client

(** Originate a Michelson contract with wrongly-typed initial storage. The
    origination should fail: only fees are consumed, not the initial balance. *)
let test_michelson_origination_wrong_storage_type () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson origination with wrong storage type on tezos X"
    ~tags:["origination"; "michelson"; "type_error"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* store_input.tz expects a string storage, but we initialise it with an int.
     The origination is forged with [Operation.Manager] and injected with
     [Operation.inject]: tezlink's prevalidator only *validates* (parse,
     signature, counter, fees, limits) and never *applies*, so the operation is
     accepted into the pool and only fails at block production on the storage
     type mismatch — a failed op-status then proves nothing was originated.
     [~dont_wait:true] is required because [tez_client] targets a foreign
     (tezlink) endpoint rather than a [Node.t]. *)
  let script_path = sandbox_test_script_path ["opcodes"; "store_input"] in
  (* mockup client since the proper RPCs are not implemented by the EVM node yet *)
  let* mockup_client = Client.init_mockup ~protocol:Alpha () in
  let* code = Client.convert_script_to_json ~script:script_path mockup_client in
  let* init_storage = Client.convert_data_to_json ~data:"42" mockup_client in
  let* origination_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~gas_limit:10000
            ~storage_limit:1000
            ~source
            (origination ~code ~init_storage ~init_balance:1_234_567 ());
        ]
        tez_client)
  in
  let* (`OpHash _) =
    Operation.inject ~dont_wait:true origination_op tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  check_block_op_status ~index:0 ~expected_status:"failed" tez_client

(** End-to-end reproduction of the forged-`big_map`-id vulnerability. A
    contract must not be able to read or overwrite another contract's
    `big_map` by passing its bare id (a forged lazy-storage reference) as an
    operation parameter. Mirrors L1's `allow_forged_lazy_storage_id:false`
    gate on user-supplied parameters.

    Scenario: a victim owns `big_map { 0 -> "genesis-42" }`; a reader stores
    whatever it reads at key 0; a writer overwrites key 0 with "Hacked-by-mi".
    Both reader and writer are called with the victim's forged big_map id.
    With the fix, both calls are rejected — the reader's storage stays [None]
    and the victim's value remains "genesis-42". Without the fix, the reader
    leaks "genesis-42" and the writer overwrites the victim with
    "Hacked-by-mi". *)
let test_michelson_forged_big_map_id_rejected () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Forged big_map id parameter is rejected on tezos X"
    ~tags:["michelson"; "big_map"; "forged"; "security"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Victim: owns a `big_map nat string`. On a unit call it materialises
     big_map[0] into its (option string) field, making the content observable
     from durable storage without itself using a forged id. *)
  let victim_code =
    "parameter unit ; storage (pair (option string) (big_map nat string)) ; \
     code { CDR ; CDR ; DUP ; PUSH nat 0 ; GET ; PAIR ; NIL operation ; PAIR }"
  in
  (* Reader: stores big_map[0] read from its parameter. *)
  let reader_code =
    "parameter (big_map nat string) ; storage (option string) ; code { CAR ; \
     PUSH nat 0 ; GET ; NIL operation ; PAIR }"
  in
  (* Writer: overwrites key 0 with the string Hacked-by-mi and stores it. *)
  let writer_code =
    "parameter (big_map nat string) ; storage (big_map nat string) ; code { \
     CAR ; PUSH string \"Hacked-by-mi\" ; SOME ; PUSH nat 0 ; UPDATE ; NIL \
     operation ; PAIR }"
  in
  let originate ~code ~init_storage_data ~alias =
    let* kt1 =
      Client.originate_contract
        ~src:source.alias
        ~prg:code
        ~init:init_storage_data
        ~alias
        ~amount:Tez.zero
        ~burn_cap:Tez.one
        tez_client
    in
    let*@ _ = Rpc.produce_block sandbox in
    return kt1
  in
  (* 1. Originate the victim with big_map { 0 -> "genesis-42" }. *)
  let* victim_kt1 =
    originate
      ~code:victim_code
      ~init_storage_data:{|Pair None { Elt 0 "genesis-42" }|}
      ~alias:"victim"
  in
  (* Read the victim's big_map id from its storage `Pair None <id>`. *)
  let* victim_storage = Client.contract_storage victim_kt1 tez_client in
  let victim_bigmap_id =
    match
      String.trim victim_storage |> String.split_on_char ' ' |> List.rev
    with
    | id :: _ -> id
    | [] ->
        Test.fail
          "Could not parse victim big_map id from storage %s"
          victim_storage
  in
  Log.info "Victim big_map id = %s" victim_bigmap_id ;
  (* 2. Originate the reader and the writer. *)
  let* reader_kt1 =
    originate ~code:reader_code ~init_storage_data:"None" ~alias:"reader"
  in
  let* writer_kt1 =
    originate ~code:writer_code ~init_storage_data:"{}" ~alias:"writer"
  in
  (* 3. READ attempt: call the reader with the forged victim id. The forged
     lazy-storage reference is rejected, so the operation is force-injected
     (the client predicts failure) and fails without leaking the big_map. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:reader_kt1
      ~arg:victim_bigmap_id
      ~amount:Tez.zero
      ~gas_limit:500000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* The READ op must be included but failed: the forged id is rejected. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  (* 4. WRITE attempt: call the writer with the forged victim id. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:writer_kt1
      ~arg:victim_bigmap_id
      ~amount:Tez.zero
      ~gas_limit:500000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* The WRITE op must be included but failed: the forged id is rejected. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  (* 5. Materialise the victim's big_map[0] into its readable storage field. *)
  let* () =
    Client.transfer
      ~giver:source.alias
      ~receiver:victim_kt1
      ~arg:"Unit"
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* The reader must NOT have read the victim's big_map: storage stays None. *)
  let* reader_storage = Client.contract_storage reader_kt1 tez_client in
  Check.(
    (String.trim reader_storage = "None")
      string
      ~error_msg:
        "reader read the victim's big_map through a forged id (storage = %L, \
         expected None)") ;
  (* The writer must NOT have overwritten the victim: big_map[0] is unchanged. *)
  let* victim_storage = Client.contract_storage victim_kt1 tez_client in
  Check.(
    (String.trim victim_storage
    = sf {|Pair (Some "genesis-42") %s|} victim_bigmap_id)
      string
      ~error_msg:
        "victim big_map was overwritten/leaked through a forged id: got %L") ;
  unit

(** Call a Michelson contract that always fails (FAILWITH) via the delayed
    inbox. The operation should fail: only fees are consumed. *)
let test_michelson_call_failwith () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson call that reverts with FAILWITH on tezos X"
    ~tags:["call"; "michelson"; "failwith"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Originate always_fails.tz *)
  let* kt1_address =
    Client.originate_contract
      ~src:source.alias
      ~burn_cap:Tez.one
      ~prg:(sandbox_test_script_path ["mini_scenarios"; "always_fails"])
      ~init:"Unit"
      ~alias:"always_fails"
      ~amount:Tez.zero
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Call always_fails: the contract FAILWITHs, so the operation must fail.
     [~force:true] is required to inject an operation the client predicts will
     fail. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:kt1_address
      ~arg:{|"trigger fail"|}
      ~amount:(Tez.of_mutez_int 1_234_567)
      ~gas_limit:500000
      ~storage_limit:1
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  check_block_op_status ~index:0 ~expected_status:"failed" tez_client

(** Test inter-contract calls via TRANSFER_TOKENS.

    1. Originate [execution_order_storer.tz]: takes a string parameter and
       appends it to its string storage.
    2. Originate [execution_order_appender.tz]: calls the storer via
       TRANSFER_TOKENS with a string stored in its own storage.
    3. Call the appender and verify that the storer's storage was updated,
       proving that the target KT1's code was actually executed. *)
let test_michelson_inter_contract_call () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson inter-contract call via TRANSFER_TOKENS on tezos X"
    ~tags:["call"; "michelson"; "internal"; "inter_contract"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate storer (string storage, initially empty) *)
  let* storer_kt1 =
    Client.originate_contract
      ~src:source.alias
      ~burn_cap:Tez.one
      ~prg:
        (sandbox_test_script_path ["mini_scenarios"; "execution_order_storer"])
      ~init:{|""|}
      ~alias:"order_storer"
      ~amount:Tez.zero
      tez_client
  in
  let* _ = Rpc.produce_block sandbox in
  (* Step 2: Originate appender with storage = Pair <storer_address> "Hello" *)
  let* appender_kt1 =
    Client.originate_contract
      ~src:source.alias
      ~burn_cap:Tez.one
      ~prg:
        (sandbox_test_script_path
           ["mini_scenarios"; "execution_order_appender"])
      ~init:(sf {|Pair "%s" "Hello"|} storer_kt1)
      ~alias:"order_appender"
      ~amount:Tez.zero
      tez_client
  in
  let* _ = Rpc.produce_block sandbox in
  (* Sanity check: storer's storage is still empty *)
  let* () =
    let* storage = Client.contract_storage storer_kt1 tez_client in
    Check.(
      (String.trim storage = {|""|})
        string
        ~error_msg:"Expected storer storage %R but got %L") ;
    unit
  in
  let* _ = Rpc.produce_block sandbox in
  (* Step 3: Call the appender — it will TRANSFER_TOKENS to the storer *)
  let* () =
    Client.transfer
      ~giver:source.alias
      ~receiver:appender_kt1
      ~arg:"Unit"
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      tez_client
  in
  let* _ = Rpc.produce_block sandbox in
  (* Step 4: Verify the storer's storage was updated to "Hello" *)
  let* storage = Client.contract_storage storer_kt1 tez_client in
  Check.(
    (String.trim storage = {|"Hello"|})
      string
      ~error_msg:"Expected storer storage %R but got %L") ;
  unit

(** Test that a failing internal call reverts all sibling internal operations.

    1. Originate [balance.tz]: stores its own balance (parameter unit).
    2. Originate [always_fails_unit.tz]: always FAILWITHs (parameter unit).
    3. Originate [reentrancy.tz]: emits two TRANSFER_TOKENS (5 XTZ each),
       first to [balance.tz], second to [always_fails_unit.tz].
    4. Call [reentrancy.tz]: the second internal call fails, so everything
       should be reverted — [balance.tz] should not receive any funds. *)
let test_michelson_internal_call_revert () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson internal call revert on tezos X"
    ~tags:["call"; "michelson"; "internal"; "revert"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate balance.tz — stores its own BALANCE, accepts unit *)
  let* balance_kt1 =
    Client.originate_contract
      ~src:source.alias
      ~init:"0"
      ~amount:Tez.zero
      ~prg:(sandbox_test_script_path ["opcodes"; "balance"])
      ~alias:"opcodes_balance"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 2: Originate always_fails_unit.tz — FAILWITHs on any call *)
  let* fails_kt1 =
    Client.originate_contract
      ~src:source.alias
      ~init:"Unit"
      ~amount:Tez.zero
      ~prg:(sandbox_test_script_path ["mini_scenarios"; "always_fails_unit"])
      ~alias:"scenarios_fails"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 3: Originate reentrancy.tz — sends 5 XTZ to each address *)
  let* reentry_kt1 =
    Client.originate_contract
      ~src:source.alias
      ~init:(sf {|Pair "%s" "%s"|} balance_kt1 fails_kt1)
      ~amount:Tez.(of_int 20)
      ~prg:(sandbox_test_script_path ["attic"; "reentrancy"])
      ~alias:"reentrancy"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* Step 4: Call reentrancy — second internal call fails, all should revert *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:source.alias
      ~receiver:reentry_kt1
      ~arg:"Unit"
      ~amount:Tez.zero
      ~gas_limit:500000
      ~storage_limit:1
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  let* ops =
    Client.RPC.call tez_client (Node.RPC.get_chain_block_operations ())
  in
  (* [/operations] returns one list per validation pass; manager operations
     live in the last pass. Flatten them: the block holds a single operation,
     the call to [reentry_kt1]. *)
  match JSON.as_list ops |> List.concat_map JSON.as_list with
  | [op] ->
      (* The call to [reentry_kt1] emits two internal transfers: the first to
         [balance.tz], the second to [always_fails_unit.tz] which FAILWITHs.
         The failure must revert the batch, so the first internal op is
         backtracked and the second is failed. *)
      let open JSON in
      let internal_statuses =
        op |-> "contents" |=> 0 |-> "metadata" |-> "internal_operation_results"
        |> as_list
        |> List.map (fun internal_op ->
               internal_op |-> "result" |-> "status" |> as_string)
      in
      Check.(
        (internal_statuses = ["backtracked"; "failed"])
          (list string)
          ~error_msg:
            "Expected internal operation statuses %R but got %L (the failing \
             second call should revert the first)") ;
      unit
  | _ -> Test.fail "Expected one operation in the block, got something else"

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

let test_cross_runtime_transfer_from_evm_to_tz () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Cross-runtime transfer to an implicit Michelson address via EVM gateway"
    ~tags:["cross_runtime"; "transfer"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let tezos_destination = Constant.bootstrap1.public_key_hash in
  let sender = Eth_account.bootstrap_accounts.(0) in
  check_evm_to_michelson_transfer
    ~sequencer:sandbox
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
     uses the generic "call" entrypoint (HTTP POST with an empty body) for a
     simple transfer. *)
  let* entrypoint, arg =
    match call with
    | None ->
        let* arg =
          Client.convert_data_to_json
            ~data:
              (sf
                 {|Pair "http://ethereum/%s" (Pair {} (Pair 0x (Pair 1 None)))|}
                 evm_destination)
            client
        in
        Lwt.return ("call", arg)
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
  let* branch = tezlink_branch ~sc_rollup_node ~client ~sequencer in
  let* call_op =
    Operation.Manager.(
      operation
        ~branch
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

(** Sandbox analogue of [michelson_to_evm_transfer]. Instead of shipping the
    gateway call through the delayed inbox, inject it directly via the tezlink
    tx pool with [Client.transfer] and bake one block.

    The gateway call is client-simulatable, so we let the node estimate
    fee/gas/storage rather than hardcoding them (only [burn_cap] is capped).

    [source] must be a funded, revealed tez bootstrap account. When [call] is
    [Some (fn_sig, calldata)] the "call_evm" entrypoint is used, otherwise the
    generic "call" entrypoint (HTTP POST to the EVM runtime with an empty
    body). [?tez_client] lets callers reuse a client that already has [source]'s
    key imported (default: a fresh client, which knows the standard bootstrap
    accounts). *)
let sandbox_michelson_to_evm_transfer ~source ~evm_destination ~transfer_amount
    ?call ?tez_client sandbox =
  let* tez_client =
    match tez_client with Some c -> return c | None -> tezos_client sandbox
  in
  let entrypoint, arg =
    match call with
    | None ->
        ( "call",
          sf
            {|Pair "http://ethereum/%s" (Pair {} (Pair 0x (Pair 1 None)))|}
            evm_destination )
    | Some (fn_sig, calldata) ->
        ( "call_evm",
          sf
            {|Pair "%s" (Pair "%s" (Pair 0x%s None))|}
            evm_destination
            fn_sig
            calldata )
  in
  let* () =
    Client.transfer
      ~giver:source.Account.alias
      ~receiver:gateway_address
      ~entrypoint
      ~arg
      ~amount:transfer_amount
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  unit

(** Sandbox analogue of [originate_michelson_contract_via_delayed_inbox]:
    originate a script from [michelson_test_scripts] via the tezlink tx pool
    with [Client.originate_contract] and bake one block. Returns the KT1
    address. [source] must be a funded, revealed tez bootstrap account. *)
let sandbox_originate_michelson_contract ~source ~script_name ~init_storage_data
    ?(init_balance = 0) ?(burn_cap = Tez.of_int 10) ?alias sandbox =
  let* tez_client = tezos_client sandbox in
  let alias =
    match alias with Some a -> a | None -> String.concat "_" script_name
  in
  let* kt1 =
    Client.originate_contract
      ~src:source.Account.alias
      ~prg:(sandbox_test_script_path script_name)
      ~init:init_storage_data
      ~alias
      ~amount:(Tez.of_mutez_int init_balance)
      ~burn_cap
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  return kt1

(** Sandbox analogue of [call_michelson_contract_via_delayed_inbox]: call a
    Michelson contract via the tezlink tx pool with [Client.transfer] and bake
    one block. [amount] is in mutez. By default the node estimates fees; for
    calls the client predicts will fail (e.g. a CRAC that errors), pass
    [~force:true] together with an explicit [gas_limit]/[storage_limit]/[fee]. *)
let sandbox_call_michelson_contract ~source ~dest ~arg_data
    ?(entrypoint = "default") ?(amount = 0) ?force ?fee ?gas_limit
    ?storage_limit sandbox =
  let* tez_client = tezos_client sandbox in
  let* () =
    Client.transfer
      ?force
      ?fee
      ?gas_limit
      ?storage_limit
      ~giver:source.Account.alias
      ~receiver:dest
      ~entrypoint
      ~arg:arg_data
      ~amount:(Tez.of_mutez_int amount)
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  unit

(** Strip all whitespace from a string. Used to compare Michelson storage
    values (from [Client.contract_storage]), which may be pretty-printed
    across multiple lines, against a single-line expected representation. *)
let remove_whitespace s =
  String.to_seq s
  |> Seq.filter (fun c -> not (c = ' ' || c = '\n' || c = '\t' || c = '\r'))
  |> String.of_seq

(** Cross-runtime transfer from Tezos to EVM through the gateway's
    generic %call entrypoint. Verifies that value transfer works
    through the %call path (HTTP POST to the EVM runtime) and that
    the gateway balance is zeroed after forwarding. *)
let test_cross_runtime_transfer_to_evm_via_call () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway %call"
    ~tags:["cross_runtime"; "transfer"; "call"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let amount = Tez.of_int 100 in
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:Constant.bootstrap1
      ~evm_destination
      ~transfer_amount:amount
      sandbox
  in
  (* Check the EVM balance of the destination. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sandbox in
  let expected_balance = Wei.of_tez amount in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Same as [test_cross_runtime_transfer_to_evm_via_call] but using
    the %call_evm entrypoint with an empty calldata. Verifies that
    value transfer works through %call_evm and that the gateway
    balance is zeroed after forwarding. *)
let test_cross_runtime_transfer_to_evm_via_call_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime transfer from Tezos to EVM via gateway %call_evm"
    ~tags:["cross_runtime"; "transfer"; "call_evm"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let amount = Tez.of_int 100 in
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:Constant.bootstrap1
      ~evm_destination
      ~transfer_amount:amount
      ~call:("", "")
      sandbox
  in
  (* Check the EVM balance of the destination. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sandbox in
  let expected_balance = Wei.of_tez amount in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** The legacy [transfer(string)] selector was removed from the EVM
    gateway precompile: a transaction carrying it must revert (invalid
    input encoding) and leave the destination uncredited. *)
let test_evm_gateway_rejects_removed_transfer_selector () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"EVM gateway rejects the removed transfer(string) selector"
    ~tags:["cross_runtime"; "transfer"; "gateway"; "removed_entrypoint"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let sequencer = sandbox in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let tezos_destination = Constant.bootstrap2.public_key_hash in
  let* tez_client = tezos_client sequencer in
  let* destination_balance_before =
    Client.get_balance_for ~account:tezos_destination tez_client
  in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer
      ~sender
      ~nonce:0
      ~value:(Wei.of_tez (Tez.of_int 10))
      ~address:evm_gateway_address
      ~abi_signature:"transfer(string)"
      ~arguments:[tezos_destination]
      ~expected_status:false
      ()
  in
  (* The reverted call must not have bridged any funds. *)
  let* destination_balance_after =
    Client.get_balance_for ~account:tezos_destination tez_client
  in
  Check.(
    (Tez.to_mutez destination_balance_after
    = Tez.to_mutez destination_balance_before)
      int
      ~error_msg:"Expected destination balance %R but got %L") ;
  assert_evm_balance_zero ~address:evm_gateway_address sequencer

(** The legacy %default (simple transfer) entrypoint was removed from
    the Michelson gateway: calling it must fail with an unknown
    entrypoint and leave the EVM destination uncredited. *)
let test_michelson_gateway_rejects_removed_default_entrypoint () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson gateway rejects the removed %default entrypoint"
    ~tags:["cross_runtime"; "transfer"; "gateway"; "removed_entrypoint"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let evm_destination = "0x2222222222222222222222222222222222222222" in
  let amount = Tez.of_int 100 in
  let* tez_client = tezos_client sandbox in
  (* The %default entrypoint was removed, so the call must fail. [~force:true]
     is required to inject an operation the client predicts will fail. *)
  let* () =
    Client.transfer
      ~force:true
      ~giver:Constant.bootstrap1.alias
      ~receiver:gateway_address
      ~entrypoint:"default"
      ~arg:(sf {|"%s"|} evm_destination)
      ~amount
      ~gas_limit:500000
      ~storage_limit:1
      ~fee:(Tez.of_int 1)
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  (* The operation must be included in the block with a failed status. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  (* The failed operation must not have bridged any funds. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sandbox in
  Check.((balance = Wei.zero) Wei.typ ~error_msg:"Expected %R but got %L") ;
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
let test_nested_crac () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from Tezos to EVM back to Tezos"
    ~tags:["cross_runtime"; "bytecode"; "nested"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
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
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "source"]
      ~init_storage_data:(Format.sprintf "0x%s" contract_storage)
      ~init_balance:0
      sandbox
  in
  (* Verify that the contract storage is correctly set. [Client.contract_storage]
     decompiles the [address] storage to its b58 form. *)
  let* () =
    let* storage = Client.contract_storage kt1_address tez_client in
    Check.(
      (String.trim storage = sf {|"%s"|} Constant.bootstrap1.public_key_hash)
        string
        ~error_msg:"Expected initial storage %R but got %L") ;
    unit
  in
  let* crac_contract =
    Solidity_contracts.transfer_crac (Kernel.select_evm_version Kernel.Latest)
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let bytecode = Tezt.Base.read_file crac_contract.bin in
  let* evm_crac_address =
    deploy_evm_contract
      ~sequencer:sandbox
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
    sandbox_michelson_to_evm_transfer
      ~source:Constant.bootstrap1
      ~evm_destination:evm_crac_address
      ~transfer_amount
      ~call:(selector, args)
      sandbox
  in
  (* Step 3: Verify the CRAC contract balance and the Michelson
     contract storage after the nested cross-runtime call. *)
  let*@ crac_contract_balance =
    Rpc.get_balance ~address:evm_crac_address sandbox
  in
  Check.(
    (crac_contract_balance = Wei.zero)
      Wei.typ
      ~error_msg:"Expected EVM CRAC balance %R but got %L") ;
  let* kt1_balance = Client.get_balance_for ~account:kt1_address tez_client in
  Check.(
    (Tez.to_mutez kt1_balance = Tez.to_mutez transfer_amount)
      int
      ~error_msg:"Expected KT1 balance %R but got %L") ;
  (* Verify that the contract storage has been changed because of the nested
     CRAC. The source of a CRAC is the zero implicit account, which
     [Client.contract_storage] decompiles to its b58 form. *)
  let crac_source = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim storage = sf {|"%s"|} crac_source)
      string
      ~error_msg:"Expected storage %R but got %L") ;
  unit

let test_cross_runtime_call_executes_evm_bytecode () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from Tezos to EVM executes contract bytecode"
    ~tags:["cross_runtime"; "bytecode"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy a simple EVM contract that writes 0x42 to storage slot 1.
     Runtime bytecode: PUSH1 0x42, PUSH1 0x01, SSTORE, PUSH1 0x01, SLOAD
     Init code copies runtime code to memory and returns it. *)
  let init_code = "600880600b6000396000f36042600155600154" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract ~sequencer:sandbox ~sender ~nonce:0 ~init_code ()
  in
  (* Step 2: Call the Michelson gateway with the deployed EVM contract
     as destination. The gateway sends a cross-runtime call that
     triggers the EVM contract's bytecode execution. *)
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:Constant.bootstrap5
      ~evm_destination:contract_address
      ~transfer_amount:(Tez.of_int 0)
      sandbox
  in
  (* Step 3: Verify the contract's storage slot 1 contains 0x42,
     proving the EVM bytecode was executed via the cross-runtime call. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x1" sandbox
  in
  let expected_storage =
    "0x0000000000000000000000000000000000000000000000000000000000000042"
  in
  Check.(
    (storage = expected_storage)
      string
      ~error_msg:"Expected storage slot 1 = %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** L2-1259: a Michelson caller hits the gateway's generic [%call]
    entrypoint with [method = 0] (GET). Asserts the call lands on
    the EVM static-execution path on the other side, by targeting
    an EVM contract whose runtime would [SSTORE] under a normal
    POST — REVM's [is_static = true] enforcement on the top frame
    must reject the SSTORE with [StateChangeDuringStaticCall], the
    EVM runtime returns 4xx, the gateway surfaces it as a CRAC
    operation error, and the post-condition is that EVM storage is
    unchanged. Complements the synthetic [VIEW "staticcall_evm"]
    path by covering the entrypoint-driven GET route. *)
let test_cross_runtime_call_get_from_michelson_routes_to_static () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime %call GET from Michelson routes to EVM static path"
    ~tags:["cross_runtime"; "call"; "get"; "static"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy the EVM storer (`SSTORE calldataload(4) at slot 0`). *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~init_code:evm_storer_init_code
      ()
  in
  (* Sanity: the storer is genuinely state-mutating — confirm slot 0
     is 0 before the GET call so an unchanged 0 after is meaningful. *)
  let*@ storage_before =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
  in
  let zero =
    "0x0000000000000000000000000000000000000000000000000000000000000000"
  in
  Check.(
    (storage_before = zero)
      string
      ~error_msg:"Sanity: expected fresh EVM storage slot 0 = %R, got %L") ;
  (* Step 2: From Michelson, hit gateway's `%call` entrypoint with
     `method = 0` (GET). Body = 4-byte zero selector + ABI uint256(42)
     — the calldata the storer would `SSTORE` under POST. *)
  let source = Constant.bootstrap5 in
  let body_hex = "00000000" ^ abi_encoded_uint256_42 in
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:gateway_address
      ~arg_data:
        (sf
           {|Pair "http://ethereum/%s" (Pair {} (Pair 0x%s (Pair 0 None)))|}
           contract_address
           body_hex)
      ~entrypoint:"call"
      ~force:true
      ~gas_limit:100_000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      sandbox
  in
  (* Step 3: Storage slot 0 must still be 0 — the static-call
     enforcement on the EVM side rejected the SSTORE. *)
  let*@ storage_after =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
  in
  Check.(
    (storage_after = zero)
      string
      ~error_msg:
        "Expected EVM storage slot 0 still %R after a `%%call` GET (the static \
         path must reject SSTORE), got %L") ;
  (* Sanity: the failed CRAC didn't leave funds on the gateway. *)
  let* tez_client = tezos_client sandbox in
  (* The %call GET must fail at the op level: static-call rejects the SSTORE. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** L2-1259: a Michelson contract reached by a regular delayed-inbox
    call uses [VIEW "staticcall_evm"] on the TezosXGateway from
    within its entrypoint body — not from a Michelson view body.
    Asserts the synthetic view is reachable from any code path that
    can issue [VIEW], not just from [callMichelsonView]-bridged view
    bodies. *)
let test_cross_runtime_staticcall_evm_from_on_chain_entrypoint () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"On-chain Michelson entrypoint uses VIEW staticcall_evm"
    ~tags:["cross_runtime"; "staticcall_evm"; "view"; "on_chain"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* EVM runtime always returning the 32-byte constant `0x..0042`. *)
  let init_code = "600a80600b6000396000f3604260005260206000f3" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* evm_contract_address =
    deploy_evm_contract ~sequencer:sandbox ~sender ~nonce:0 ~init_code ()
  in
  (* Originate the Michelson contract whose `default` entrypoint
     issues `VIEW "staticcall_evm"` against the gateway. Storage
     holds (None, evm_addr). *)
  let init_storage_data = sf {|Pair None "%s"|} evm_contract_address in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "staticcall_evm_on_chain"]
      ~init_storage_data
      sandbox
  in
  (* Trigger the entrypoint. The contract's code fires
     `VIEW "staticcall_evm"`, receives the EVM bytes back, and stashes
     them under `%last`. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:kt1_address
      ~arg_data:"Unit"
      sandbox
  in
  (* Verify the response bytes ended up in storage — the synthetic view
     dispatched a real cross-runtime GET to EVM and returned its 32-byte
     constant payload. *)
  let expected_bytes =
    "0000000000000000000000000000000000000000000000000000000000000042"
  in
  let* tez_client = tezos_client sandbox in
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (remove_whitespace storage
    = remove_whitespace
        (sf {|Pair (Some 0x%s) "%s"|} expected_bytes evm_contract_address))
      string
      ~error_msg:"Expected storage %R but got %L") ;
  (* And the gateway didn't accumulate any balance along the way. *)
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** L2-1259: nested GET CRAC exercising both directions in a single
    EVM transaction. The EVM caller hits the gateway precompile's
    [callMichelsonView] (outer EVM → Michelson GET, from L2-1244)
    targeting a Michelson view whose body issues
    [VIEW "staticcall_evm"] on the gateway address (inner Michelson
    → EVM GET, added in this MR). The inner GET routes through the
    new MIR [try_dispatch_enshrined_view] hook, fires a real
    [Registry::serve] back into the EVM runtime, and the response
    bytes propagate through the Michelson view to the EVM caller as
    the precompile's ABI-encoded return value. *)
let test_cross_runtime_staticcall_evm_nested_view_view () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Nested GET: callMichelsonView → Michelson VIEW staticcall_evm → EVM"
    ~tags:["cross_runtime"; "staticcall_evm"; "get"; "nested"; "view"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* EVM contract returning `msg.sender` left-padded to 32 bytes.
     Used to verify that the inner GET reaches the EVM target with
     `msg.sender` set to the deterministic alias of the calling
     Michelson KT1 (i.e. that
     `tezosx_resolve_source_alias_readonly` is wired into the
     view-side bridge, vs. a hardcoded null sender).

     Init (11B): PUSH1 9 / DUP1 / PUSH1 11 / PUSH1 0 / CODECOPY /
       PUSH1 0 / RETURN — copies the 9-byte runtime from offset 11.
     Runtime (9B): CALLER / PUSH1 0 / MSTORE / PUSH1 32 / PUSH1 0 /
       RETURN — stores msg.sender at mem[0..32] and returns it. *)
  let init_code = "600980600b6000396000f33360005260206000f3" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* evm_contract_address =
    deploy_evm_contract ~sequencer:sandbox ~sender ~nonce:0 ~init_code ()
  in
  (* Originate the Michelson contract whose `read_evm` view bridges
     to `gateway.staticcall_evm`. Storage holds just the EVM target
     address. *)
  let init_storage_data = sf {|"%s"|} evm_contract_address in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "staticcall_evm_view"]
      ~init_storage_data
      sandbox
  in
  (* Drive the outer call via `eth_call` so the precompile's return
     value is surfaced directly (vs. having to dig through a tx
     receipt). `0x030b` is the Micheline binary encoding of `Unit`:
     `0x03` = primitive-with-no-arg tag, `0x0b` = the `Unit`
     primitive number. *)
  let micheline_unit = "0x030b" in
  let* calldata =
    Cast.calldata
      ~args:[kt1_address; "read_evm"; micheline_unit]
      "callMichelsonView(string,string,bytes)"
  in
  let eth_call_with_gas ~to_ ~data =
    let* response =
      Evm_node.(
        call_evm_rpc
          sandbox
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
  (* `callMichelsonView` ABI-encodes its return as `(bytes,)`:
     32-byte offset word (`0x20`), then a 32-byte length word, then
     the payload right-padded to a 32-byte boundary. The Michelson
     view returned the raw EVM response (32 bytes carrying
     `msg.sender` left-padded) wrapped in a Micheline `Bytes`
     literal — tag `0x0a`, 4-byte big-endian length `0x00000020`,
     then the 32 payload bytes — for a total Micheline length of 37
     bytes (`0x25`). *)
  let hex = String.sub call_return 2 (String.length call_return - 2) in
  let length_word = String.sub hex 64 64 in
  Check.(
    (length_word
   = "0000000000000000000000000000000000000000000000000000000000000025")
      string
      ~error_msg:
        "Expected ABI length word for the 37-byte Micheline bytes: %R, got %L") ;
  let body_hex = String.sub hex 128 (37 * 2) in
  (* The expected `msg.sender` on the EVM side is the deterministic
     alias the Michelson KT1 gets when first crossed read-only into
     the EVM runtime: `keccak256(kt1_base58_ascii)[..20]`. The
     gateway view's bridge resolves this via
     `tezosx_resolve_source_alias_readonly`. EVM `MSTORE` of the
     20-byte address left-pads it with 12 zero bytes inside the
     32-byte word. *)
  let expected_alias_hex =
    Test_helpers.(remove_0x (evm_alias_of_tezos_address kt1_address))
  in
  let expected_body_hex =
    "0a00000020" ^ String.make 24 '0' ^ expected_alias_hex
  in
  Check.(
    (body_hex = expected_body_hex)
      string
      ~error_msg:
        "Expected Micheline-encoded `Bytes <KT1 alias>` payload: %R, got %L") ;
  let* tez_client = tezos_client sandbox in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** The nested [staticcall_evm] view must preserve the *outer EVM
    originator* as the inner callee's [tx.origin] (ORIGIN) while keeping
    the immediate Michelson caller as [msg.sender] (CALLER).

    Path: EVM [eth_call] -> [callMichelsonView] -> Michelson VIEW
    -> [VIEW "staticcall_evm"] -> EVM echo.

    Expected: CALLER == alias(KT1) and ORIGIN == the EVM EOA that issued
    the outer call. The originator's native identity is carried on the
    shared cross-runtime journal and re-injected as [X-Tezos-Source], so
    ORIGIN is preserved without depending on any durable alias record. *)
let test_cross_runtime_staticcall_evm_nested_view_preserves_origin () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Nested staticcall_evm view preserves outer EVM tx.origin"
    ~tags:["cross_runtime"; "staticcall_evm"; "get"; "nested"; "view"; "origin"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* EVM echo returning CALLER (msg.sender) followed by ORIGIN
     (tx.origin), each left-padded to a 32-byte word (64 bytes total).

     Init (11B): PUSH1 0d / DUP1 / PUSH1 0b / PUSH1 0 / CODECOPY /
       PUSH1 0 / RETURN — copies the 13-byte runtime from offset 11.
     Runtime (13B): CALLER / PUSH1 0 / MSTORE / ORIGIN / PUSH1 32 /
       MSTORE / PUSH1 64 / PUSH1 0 / RETURN — writes msg.sender at
       mem[0..32], tx.origin at mem[32..64], and returns mem[0..64]. *)
  let init_code = "600d80600b6000396000f3336000523260205260406000f3" in
  let outer_originator = Eth_account.bootstrap_accounts.(0) in
  let* evm_contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender:outer_originator
      ~nonce:0
      ~init_code
      ()
  in
  (* Originate the Michelson contract whose `read_evm` view bridges to
     `gateway.staticcall_evm`, targeting the EVM echo above. *)
  let init_storage_data = sf {|"%s"|} evm_contract_address in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "staticcall_evm_view"]
      ~init_storage_data
      sandbox
  in
  (* No alias warm-up: the outer EVM originator's Tezos alias is never
     materialized before the read-only call. Origin preservation must not
     depend on a durable origin record — the entering `callMichelsonView`
     gateway captures the originator's native identity on the shared
     journal, and the nested `staticcall_evm` view reads it back from
     there. A read-only round-trip never writes such a record, so a
     warm-up here would mask exactly the record-dependency this test
     guards against; do not re-add one. *)
  (* Drive the outer call via `eth_call`, pinning `from` to the EVM EOA
     so the asserted originator is unambiguous. `0x030b` is the Micheline
     binary encoding of `Unit`. *)
  let micheline_unit = "0x030b" in
  let* calldata =
    Cast.calldata
      ~args:[kt1_address; "read_evm"; micheline_unit]
      "callMichelsonView(string,string,bytes)"
  in
  let* response =
    Evm_node.(
      call_evm_rpc
        sandbox
        {
          method_ = "eth_call";
          parameters =
            `A
              [
                `O
                  [
                    ("from", `String outer_originator.Eth_account.address);
                    ("to", `String evm_gateway_address);
                    ("data", `String calldata);
                    ("gas", `String "0xf4240");
                  ];
                `String "latest";
              ];
        })
  in
  let call_return = response |> Evm_node.extract_result |> JSON.as_string in
  let hex = String.sub call_return 2 (String.length call_return - 2) in
  (* `callMichelsonView` ABI-encodes its return as `(bytes,)`: a 32-byte
     offset word, a 32-byte length word, then the payload right-padded to
     a 32-byte boundary. The Michelson view returned the raw 64-byte EVM
     response wrapped in a Micheline `Bytes` literal — tag `0x0a`, 4-byte
     big-endian length `0x00000040`, then the 64 payload bytes — a
     69-byte (`0x45`) Micheline value. *)
  let length_word = String.sub hex 64 64 in
  Check.(
    (length_word
   = "0000000000000000000000000000000000000000000000000000000000000045")
      string
      ~error_msg:
        "Expected ABI length word for the 69-byte Micheline bytes: %R, got %L") ;
  let body_hex = String.sub hex 128 (69 * 2) in
  let micheline_prefix = String.sub body_hex 0 10 in
  Check.(
    (micheline_prefix = "0a00000040")
      string
      ~error_msg:"Expected Micheline `Bytes` 64-byte header: %R, got %L") ;
  let caller_word = String.sub body_hex 10 64 in
  let origin_word = String.sub body_hex 74 64 in
  (* CALLER (msg.sender) is the deterministic Ethereum alias of the
     calling Michelson KT1: `keccak256(kt1_base58_ascii)[..20]`,
     left-padded inside the 32-byte word. *)
  let expected_alias_hex = evm_alias_of_kt1 kt1_address in
  let expected_caller = String.make 24 '0' ^ expected_alias_hex in
  (* ORIGIN (tx.origin) must be preserved as the outer EVM originator,
     not collapse onto the Michelson caller alias. *)
  let expected_origin =
    String.make 24 '0'
    ^ String.lowercase_ascii
        (String.sub outer_originator.Eth_account.address 2 40)
  in
  (* Guard against a degenerate setup where the two identities happen to
     coincide — that would make the ORIGIN assertion pass even under the
     identity-collapse bug. *)
  if expected_caller = expected_origin then
    Test.fail
      "Test setup error: alias(KT1) coincides with the outer EVM originator" ;
  Check.(
    (caller_word = expected_caller)
      string
      ~error_msg:"Expected inner CALLER = alias(KT1) %R, got %L") ;
  Check.(
    (origin_word = expected_origin)
      string
      ~error_msg:
        "Expected inner ORIGIN to preserve the outer EVM originator %R, got %L") ;
  unit

let test_cross_runtime_transfer_from_evm_to_kt1 () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime transfer to a Michelson KT1 via EVM gateway"
    ~tags:["cross_runtime"; "transfer"; "kt1"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate transfer_amount.tz — stores AMOUNT in storage *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "transfer_amount"]
      ~init_storage_data:"0"
      sandbox
  in
  (* Step 2: Verify initial storage is 0 *)
  let* () =
    let* storage = Client.contract_storage kt1_address tez_client in
    Check.(
      (String.trim storage = "0")
        string
        ~error_msg:"Expected initial storage %R but got %L") ;
    unit
  in
  (* Step 3: Transfer from EVM to the KT1 via the gateway precompile *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let transfer_amount = Tez.of_int 100 in
  let* () =
    check_evm_to_michelson_transfer
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~tezos_destination:kt1_address
      ~transfer_amount
      ()
  in
  (* Step 4: Verify the contract storage contains the transferred amount *)
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim storage = string_of_int transfer_amount_mutez)
      string
      ~error_msg:"Expected storage %R but got %L") ;
  unit

let test_cross_runtime_call_failwith () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Cross-runtime transfer to a FAILWITH Michelson KT1 reverts via EVM \
       gateway"
    ~tags:["cross_runtime"; "transfer"; "kt1"; "failwith"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate always_fails.tz *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "always_fails"]
      ~init_storage_data:"Unit"
      sandbox
  in
  (* Step 2: Call the KT1 from EVM via the gateway precompile *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let transfer_amount = Tez.of_int 100 in
  let value = Wei.of_tez transfer_amount in
  let* tez_client = tezos_client sandbox in
  let*@ evm_balance_before =
    Rpc.get_balance ~address:sender.Eth_account.address sandbox
  in
  let* kt1_balance_before =
    Client.get_balance_for ~account:kt1_address tez_client
  in
  let* receipt =
    call_evm_gateway
      ~sequencer:sandbox
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
    Rpc.get_balance ~address:sender.Eth_account.address sandbox
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
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sandbox in
  unit

(** Test cross-runtime call from EVM to Michelson with calldata.

    1. Originate [store_input.tz] via delayed inbox.
    2. Call [call(string,string,bytes)] on the EVM gateway precompile,
       passing the KT1 address, "default" entrypoint, and Micheline-encoded
       string "Hello from EVM" as parameters.
    3. Verify the Michelson contract storage is updated to "Hello from EVM". *)
let test_cross_runtime_call_from_evm_to_michelson () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from EVM to Michelson with calldata"
    ~tags:["cross_runtime"; "call"; "calldata"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate store_input_ep.tz — like store_input but with a named
     entrypoint %save instead of %default. *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "store_input_ep"]
      ~init_storage_data:{|""|}
      sandbox
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
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~value:(Wei.of_tez (Tez.of_int 1))
      ~address:evm_gateway_address
      ~abi_signature:"callMichelson(string,string,bytes)"
      ~arguments:[kt1_address; "save"; micheline_hello]
      ()
  in
  (* Step 3: Verify storage was updated to "Hello from EVM". *)
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim storage = {|"Hello from EVM"|})
      string
      ~error_msg:"Expected storage %R but got %L") ;
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sandbox in
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
let test_cross_runtime_view_call_from_evm_to_michelson () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime view call from EVM to Michelson"
    ~tags:["cross_runtime"; "view"; "michelson"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate view_toplevel_lib.tz with storage = nat 5. The
     "add" view body is `UNPAIR ; ADD` — with the initial stack
     `pair(input, storage)` this computes `input + storage`. *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      sandbox
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
          sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
      ~sender
      ~nonce:2
      ~value:Wei.zero
      ~address:forwarder_address
      ~abi_signature:"callMichelsonView(string,string,bytes)"
      ~arguments:[kt1_address; "add"; micheline_nat_7]
      ()
  in
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sandbox in
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
let test_cross_runtime_view_call_via_low_level_call () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime view call via low-level call entry (method=GET)"
    ~tags:["cross_runtime"; "view"; "michelson"; "low_level_call"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      sandbox
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
          sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
      ~sender
      ~nonce:2
      ~value:Wei.zero
      ~address:forwarder_address
      ~abi_signature:"call(string,(string,string)[],bytes,uint8)"
      ~arguments:[url; "[]"; micheline_nat_7; "0"]
      ()
  in
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sandbox in
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
let test_cross_runtime_view_call_negative () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime view call — negative cases"
    ~tags:["cross_runtime"; "view"; "michelson"; "negative"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "view_toplevel_lib"]
      ~init_storage_data:"5"
      sandbox
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let micheline_nat_7 = "0x0007" in
  (* Unknown view name → precompile reverts (catchable 400 on the
     Michelson side, surfaced as a failing receipt). *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
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
      ~sequencer:sandbox
      ~sender
      ~nonce:3
      ~init_code:evm_gateway_delegatecall_forwarder_init_code
      ()
  in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
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
let test_cross_runtime_call_from_michelson_to_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from Michelson to EVM with calldata"
    ~tags:["cross_runtime"; "call"; "calldata"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy EVM contract that stores calldataload(4) in slot 0. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~init_code:evm_storer_init_code
      ()
  in
  (* Step 2: Call the gateway KT1 from Michelson with entrypoint "call_evm".
     The parameter is Pair <evm_addr> (Pair "store(uint256)" <abi_bytes>)
     where abi_bytes is the ABI encoding of uint256(42). *)
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:Constant.bootstrap5
      ~evm_destination:contract_address
      ~transfer_amount:Tez.one
      ~call:("store(uint256)", abi_encoded_uint256_42)
      sandbox
  in
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
      ~error_msg:"Expected storage slot 0 = %R but got %L") ;
  (* Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
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
let test_evm_gateway_catch_revert () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson revert does not force EVM revert"
    ~tags:["cross_runtime"; "gateway"; "revert"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  (* Step 1: Originate transfer_amount.tz — stores AMOUNT in storage *)
  let* success_kt1 =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["opcodes"; "transfer_amount"]
      ~init_storage_data:"0"
      sandbox
  in
  (* Step 2: Originate always_fails_unit.tz — FAILWITHs on any call *)
  let* fails_kt1 =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "always_fails_unit"]
      ~init_storage_data:"Unit"
      sandbox
  in
  (* Step 3: Compile and deploy the Solidity catch-revert contract *)
  let* contract =
    Solidity_contracts.gateway_catch_revert
      (Kernel.select_evm_version Kernel.Latest)
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let bytecode = Tezt.Base.read_file contract.bin in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~init_code:("0x" ^ bytecode)
      ()
  in
  (* Step 4: Call testCatchRevert(successKT1, failsKT1) with value *)
  let value = Wei.of_tez (Tez.of_int 200) in
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender
      ~nonce:1
      ~value
      ~address:contract_address
      ~abi_signature:"testCatchRevert(string,string)"
      ~arguments:[success_kt1; fails_kt1]
      ()
  in
  (* Step 5: Verify results *)
  (* Storage layout: all three bools are packed in slot 0.
     byte 0 = firstCallSuccess, byte 1 = secondCallSuccess,
     byte 2 = executionCompleted.
     Expected: 0x...00010001 (first=true, second=false, completed=true). *)
  let*@ slot0 =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
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
    let* storage = Client.contract_storage success_kt1 tez_client in
    Check.(
      (String.trim storage = string_of_int half_mutez)
        string
        ~error_msg:"Expected success KT1 storage %R but got %L") ;
    unit
  in
  (* Verify the EVM contract got the 100 XTZ back from the failed subcall. *)
  let*@ contract_balance = Rpc.get_balance ~address:contract_address sandbox in
  let expected_contract_balance = Wei.of_tez (Tez.of_int 100) in
  Check.(
    (contract_balance = expected_contract_balance)
      Wei.typ
      ~error_msg:"Expected EVM contract balance %R but got %L") ;
  (* Verify the failing contract received no funds — the revert was atomic. *)
  let* fails_balance = Client.get_balance_for ~account:fails_kt1 tez_client in
  Check.(
    (Tez.to_mutez fails_balance = 0)
      int
      ~error_msg:"Expected failing contract balance 0 but got %L") ;
  let* () = assert_evm_balance_zero ~address:evm_gateway_address sandbox in
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
let test_michelson_gateway_evm_revert () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"EVM revert is properly handled via Michelson gateway"
    ~tags:["cross_runtime"; "gateway"; "revert"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Deploy EVM "reverter" — always REVERTs on any call. *)
  let* reverter_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~init_code:evm_reverter_init_code
      ()
  in
  Log.info "Deployed reverter at %s" reverter_address ;
  (* Step 2: Transfer to reverter via gateway default entrypoint with 100 tez.
     The EVM REVERT causes the Michelson operation to fail, so the call is
     force-injected (the client predicts failure) and no funds reach the
     reverter. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:gateway_address
      ~arg_data:(sf {|"%s"|} reverter_address)
      ~amount:100_000_000
      ~force:true
      ~gas_limit:500000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      sandbox
  in
  let* tez_client = tezos_client sandbox in
  (* The transfer op must be included but failed: the EVM reverts. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  (* Step 3: Call reverter via gateway "call_evm" entrypoint with a function
     signature and calldata. This exercises a different code path in the
     enshrined contract (tezosx_call_evm vs tezosx_transfer_tez).
     The calldata content is irrelevant — the reverter always REVERTs
     regardless of input. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:gateway_address
      ~entrypoint:"call_evm"
      ~arg_data:
        (sf
           {|Pair "%s" (Pair "store(uint256)" (Pair 0x%s None))|}
           reverter_address
           abi_encoded_uint256_42)
      ~amount:50_000_000
      ~force:true
      ~gas_limit:500000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      sandbox
  in
  (* The call_evm op must be included but failed: the EVM reverts. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"failed" tez_client
  in
  (* Step 4: Verify the reverter received no funds from either call. *)
  let*@ reverter_balance = Rpc.get_balance ~address:reverter_address sandbox in
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
      ~timeout:5.
      sandbox
  in
  let wait_for_tezos_confirmed =
    Evm_node.wait_for_tx_queue_transaction_confirmed
      ~hash:tezos_hash_added
      ~timeout:5.
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
let test_cross_runtime_call_from_michelson_contract_to_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from Michelson contract to EVM"
    ~tags:["cross_runtime"; "call"; "calldata"; "internal"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy EVM contract that stores calldataload(4) in slot 0. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
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
  let* caller_kt1 =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "gateway_caller"]
      ~init_storage_data
      sandbox
  in
  (* Step 3: Call the Michelson contract with Unit and some amount. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount:1_000_000
      sandbox
  in
  (* Step 4a: Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  (* Step 4b: Verify EVM storage slot 0 = 42. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
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
let test_cross_runtime_call_from_michelson_contract_to_evm_revert () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime call from Michelson contract to reverting EVM"
    ~tags:["cross_runtime"; "call"; "revert"; "internal"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy an EVM contract that always reverts.
     Runtime: 60006000fd = PUSH1 0, PUSH1 0, REVERT *)
  let init_code = "600580600b6000396000f360006000fd" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract ~sequencer:sandbox ~sender ~nonce:0 ~init_code ()
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
  let* caller_kt1 =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "gateway_caller"]
      ~init_storage_data
      sandbox
  in
  (* Step 3: Call the Michelson contract with Unit and some amount. The EVM
     contract reverts, so the whole Michelson operation fails: the call is
     force-injected (the client predicts failure) and the amount is refunded. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount:1_000_000
      ~force:true
      ~gas_limit:500000
      ~storage_limit:1000
      ~fee:(Tez.of_int 1)
      sandbox
  in
  (* Step 4: Gateway balance is 0. *)
  let* tez_client = tezos_client sandbox in
  (* The call op must be included but failed: the EVM contract reverts. *)
  let* () =
    check_block_op_status ~index:0 ~expected_status:"backtracked" tez_client
  in
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address tez_client
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance 0 but got %L") ;
  unit

(** Test cross-runtime transfer from a Michelson contract to an EVM EOA
    via the gateway's generic %call entrypoint.

    1. Originate [gateway_transfer.tz] with storage pointing to the gateway
       KT1 and an EVM destination address.
    2. Call the Michelson contract via the delayed inbox with some amount.
    3. Verify the EVM destination received the funds, proving the call went:
       delayed inbox -> Michelson contract -> TRANSFER_TOKENS ->
       enshrined gateway (%call) -> cross-runtime bridge -> EVM EOA. *)
let test_cross_runtime_transfer_from_michelson_contract_to_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime transfer from Michelson contract to EVM"
    ~tags:["cross_runtime"; "transfer"; "internal"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let transfer_amount_mutez = 1_000_000 in
  (* Step 1: Originate gateway_transfer.tz *)
  let init_storage_data =
    sf {|Pair "%s" "%s"|} gateway_address evm_destination
  in
  let* caller_kt1 =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "gateway_transfer"]
      ~init_storage_data
      sandbox
  in
  (* Step 2: Call the Michelson contract with Unit and some amount. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:caller_kt1
      ~arg_data:"Unit"
      ~amount:transfer_amount_mutez
      sandbox
  in
  (* Step 3: Verify the EVM destination received the funds. *)
  let*@ balance = Rpc.get_balance ~address:evm_destination sandbox in
  let expected_balance = Wei.of_tez (Tez.of_mutez_int transfer_amount_mutez) in
  Check.(
    (balance = expected_balance) Wei.typ ~error_msg:"Expected %R but got %L") ;
  (* Step 4: Check that the gateway did not retain any funds. *)
  let* tez_client = tezos_client sandbox in
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
let test_cross_runtime_fa12_approve_from_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime FA1.2 approve from EVM to Michelson"
    ~tags:["cross_runtime"; "fa12"; "approve"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  (* Step 1: Originate the FA1.2 reference contract with empty ledger. *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "fa12_reference"]
      ~init_storage_data:
        (sf {|Pair {} (Pair "%s" (Pair False 0))|} source.public_key_hash)
      sandbox
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
      ~sequencer:sandbox
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
let test_cross_runtime_erc20_transfer_from_michelson () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime ERC-20 transfer from Michelson to EVM"
    ~tags:["cross_runtime"; "erc20"; "transfer"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  (* Step 1: Deploy EVM contract that stores calldataload(36) in slot 0.
     Runtime bytecode: PUSH1 0x24 | CALLDATALOAD | PUSH1 0x00 | SSTORE | STOP
     = 60 24 35 60 00 55 00 (7 bytes) *)
  let init_code = "600780600b6000396000f360243560005500" in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract ~sequencer:sandbox ~sender ~nonce:0 ~init_code ()
  in
  (* Step 2: Call the ERC-20 wrapper KT1 from Michelson with "transfer".
     The parameter is Pair "<evm_contract>" (Pair 0x<to_addr> 42).
     The wrapper computes transfer(address,uint256) selector and
     ABI-encodes the arguments before calling the EVM contract. *)
  let source = Constant.bootstrap5 in
  let to_address = "1111111111111111111111111111111111111111" in
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:erc20_wrapper_address
      ~arg_data:(sf {|Pair "%s" (Pair 0x%s 42)|} contract_address to_address)
      ~entrypoint:"transfer"
      sandbox
  in
  (* Step 3: Verify EVM storage slot 0 = 42, confirming the
     ERC-20 wrapper correctly encoded the uint256 amount. *)
  let*@ storage =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
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
let test_manager_key_on_block_hash () =
  Setup.register_sandbox_test
    ~title:"Query manager_key on a specific block hash via Tezlink RPC"
    ~tags:["manager_key"; "block_hash"; "rpc"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let receiver_account = Constant.bootstrap2 in
  let*@ _block_number = Rpc.produce_block sandbox in

  (* Step 1: Capture the current block header hash. Bootstrap accounts are
     revealed from genesis, so manager_key is already set at this block. *)
  let* header = get_tezlink_block_header sandbox in
  let block_hash = JSON.(header |-> "hash" |> as_string) in
  Log.info "Captured block hash: %s" block_hash ;
  (* Step 2: Produce one more block via a transfer to move head forward *)
  let*@ _block_number = Rpc.produce_block sandbox in

  (* Sanity check: head has moved past the captured block *)
  let* new_header = get_tezlink_block_header sandbox in
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
      ~name:("curl#" ^ Evm_node.name sandbox)
      (Evm_node.endpoint sandbox ^ path)
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

  (* The gateway exposes these entrypoints:
     %call_evm (pair string (pair string (pair bytes (option (contract bytes))))),
     %call (pair string (pair (list (pair string string)) (pair bytes (pair nat (option (contract bytes)))))),
     and %collect_result (bytes). *)
  let entrypoints = JSON.(ep_json |-> "entrypoints") in
  let assert_ep name expected_prim =
    let prim = JSON.(entrypoints |-> name |-> "prim" |> as_string) in
    Check.(
      (prim = expected_prim)
        string
        ~error_msg:(sf "Expected prim %%R for entrypoint %s but got %%L" name))
  in
  assert_ep "call" "pair" ;
  assert_ep "call_evm" "pair" ;
  assert_ep "collect_result" "bytes" ;
  (* %default was removed: it must no longer be exposed. *)
  Check.(
    (JSON.(entrypoints |-> "default" |> is_null) = true)
      bool
      ~error_msg:"Expected gateway to no longer expose the %default entrypoint") ;

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

(** Extract the `view` toplevel items from a /script Micheline code list,
    returning the name carried by each as a string. View nodes have shape
    `{prim:view, args:[<name>, <param_ty>, <return_ty>, <code>]}` where
    [<name>] is a Micheline `String` node. *)
let script_view_names code_list =
  List.filter_map
    (fun item ->
      match JSON.(item |-> "prim" |> as_string_opt) with
      | Some "view" ->
          let name_node = JSON.(item |-> "args" |=> 0) in
          JSON.(name_node |-> "string" |> as_string_opt)
      | _ -> None)
    code_list

(** Regression test that captures the full /script output for the
    enshrined TezosX Gateway. Any change to the synthesized script
    schema — added/removed/renamed entrypoint, added/removed/renamed
    synthetic view, type change, code body change — surfaces as a
    diff against the captured [.out] file and forces an explicit
    review acknowledgement before landing. Complements
    [test_entrypoints_enshrined] which does the same for
    /entrypoints. *)
let test_script_synthetic_views_enshrined () =
  Setup.register_sandbox_regression_test
    ~title:"Script RPC exposes synthetic views for enshrined TezosX Gateway"
    ~tags:["rpc"; "script"; "views"; "tezlink"; "tezosx"; "gateway"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let* script_json = get_script sandbox gateway_address in
  Regression.capture (JSON.encode script_json) ;
  unit

(** Regression: a regular originated KT1 with no views in its source
    code must not have any `view` declaration appear in its /script.
    Without this guard, an accidental change to the script synthesizer
    (which only fires for enshrined contracts) could regress and start
    leaking declarations into the responses of regular contracts too. *)
let test_script_no_synthetic_views_for_originated_contract () =
  Setup.register_sandbox_test
    ~title:"Script RPC has no views for a regular originated KT1 (no views)"
    ~tags:["rpc"; "script"; "views"; "tezlink"; "tezosx"; "originated"]
    ~with_runtimes:[Tezos]
    ~uses_client:true
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun sandbox ->
  let* tez_client = tezos_client sandbox in
  let script_path =
    Michelson_script.(
      find ["opcodes"; "store_input"] Michelson_contracts.tezlink_protocol
      |> path)
  in
  let* kt1_address =
    Client.originate_contract
      ~alias:"no_views_kt1"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:{|""|}
      ~prg:script_path
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  let* script_json = get_script sandbox kt1_address in
  let code_list = JSON.(script_json |-> "code" |> as_list) in
  let view_names = script_view_names code_list in
  Check.(
    (view_names = [])
      (list string)
      ~error_msg:
        "Expected no view declarations in /script for a regular originated \
         KT1, got %L") ;
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
      ~gas:"0xF4240"
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
let test_cross_runtime_evm_sender_is_alias () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Cross-runtime EVM to Michelson: sender is alias of EVM source"
    ~tags:["cross_runtime"; "sender"; "alias"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Originate sender.tz with a dummy initial address. *)
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source:Constant.bootstrap1
      ~script_name:["opcodes"; "sender"]
      ~init_storage_data:(sf {|"%s"|} Constant.bootstrap1.public_key_hash)
      sandbox
  in
  (* Step 2: Call the contract from EVM via the gateway precompile. *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender
      ~nonce:0
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~abi_signature:"callMichelson(string,string,bytes)"
      ~arguments:[kt1_address; ""; "0x"]
      ()
  in
  (* Step 3: Resolve the expected KT1 alias for the EVM sender. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sandbox
  in
  let expected = sf {|"%s"|} (Result.get_ok alias_result) in
  let* tez_client = tezos_client sandbox in
  let* storage = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim storage = expected) string ~error_msg:"Expected %R but got %L") ;
  unit

(** Test cross-runtime Michelson to EVM: msg.sender is the Ethereum alias of
    the Michelson source. *)
let test_cross_runtime_michelson_sender_is_alias () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "Cross-runtime Michelson to EVM: msg.sender is alias of Michelson source"
    ~tags:["cross_runtime"; "sender"; "alias"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap1 in
  (* Step 1: Deploy an EVM contract that stores msg.sender (CALLER) in slot 0. *)
  let deployer = Eth_account.bootstrap_accounts.(0) in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender:deployer
      ~nonce:0
      ~init_code:evm_caller_storer_init_code
      ()
  in
  (* Step 2: Call the EVM contract from the Michelson source via the gateway. *)
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source
      ~evm_destination:contract_address
      ~transfer_amount:Tez.zero
      ~call:("store()", "")
      sandbox
  in
  (* Step 3: Read msg.sender recorded by the EVM contract. *)
  let*@ stored_sender =
    Rpc.get_storage_at ~address:contract_address ~pos:"0x0" sandbox
  in
  (* Step 4: Resolve the alias via the RPC. *)
  let* alias_result =
    Rpc.Tezosx.tez_getTezosEthereumAddress source.public_key_hash sandbox
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
let test_alias_forwarder_forwards_to_evm () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Alias forwarder contract forwards tez to EVM address"
    ~tags:["alias"; "forwarder"; "cross_runtime"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let evm_receiver = Eth_account.bootstrap_accounts.(0) in
  (* Step 1: Originate the alias forwarder contract. *)
  let evm_destination = evm_receiver.address in
  let* kt1_address =
    sandbox_originate_michelson_contract
      ~source
      ~script_name:["mini_scenarios"; "alias_forwarder"]
      ~init_storage_data:(sf {|"%s"|} evm_destination)
      sandbox
  in
  (* Step 2: Record the EVM receiver balance before the transfer. *)
  let*@ evm_balance_before = Rpc.get_balance ~address:evm_destination sandbox in
  (* Step 3: Send tez to the alias forwarder KT1.
     The forwarder contract should forward its BALANCE to the
     TezosXGateway enshrined contract, which routes funds to the EVM address. *)
  let transfer_amount = Tez.of_int 100 in
  let transfer_amount_mutez = Tez.to_mutez transfer_amount in
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:kt1_address
      ~arg_data:"Unit"
      ~amount:transfer_amount_mutez
      sandbox
  in
  (* Step 4: Verify the EVM receiver balance increased. *)
  let*@ evm_balance_after = Rpc.get_balance ~address:evm_destination sandbox in
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
let test_alias_forwarder_created_by_evm_cross_runtime_call () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:
      "EVM cross-runtime call creates alias forwarder that forwards tez back"
    ~tags:["alias"; "forwarder"; "cross_runtime"; "evm_to_tezos"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let evm_sender = Eth_account.bootstrap_accounts.(0) in
  let tezos_receiver = Constant.bootstrap1.public_key_hash in
  (* Step 1: Make a cross-runtime transfer from EVM to Tezos.
     This triggers alias creation for the EVM sender on the Tezos side
     via journal.rs which passes the address as a string. *)
  let* _receipt =
    check_evm_to_michelson_transfer
      ~sequencer:sandbox
      ~sender:evm_sender
      ~nonce:0
      ~tezos_destination:tezos_receiver
      ~transfer_amount:(Tez.of_int 1)
      ()
  in
  (* Step 2: Look up the Tezos alias (KT1) for the EVM sender. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_sender.address sandbox
  in
  let alias_kt1 = Result.get_ok alias_result in
  Log.info "EVM sender %s has Tezos alias %s" evm_sender.address alias_kt1 ;
  (* Step 2b: The alias materialization must surface as an internal
     `Origination` (sender = `NULL_PKH`, `originated_contracts` =
     [alias_kt1]) inside the receipt of the synthetic top-level CRAC
     manager-op — there is exactly ONE top-level operation in the
     block: the pre-existing CRAC's Transaction with destination =
     [alias_kt1] (alias of E_0). The alias internal op is plumbed
     from the EVM precompile through the
     `X-Tezos-Alias-Origination` HTTP header and prepended to the
     CRAC's internal_operation_results. *)
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sandbox) with path = "/tezlink"}
  in
  let* block_operations =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_operations ()
  in
  let open JSON in
  let managers = List.nth (as_list block_operations) 3 |> as_list in
  let crac_op =
    match managers with
    | [op] -> op
    | _ ->
        Test.fail
          "Expected exactly one top-level manager operation in the Michelson \
           head block, found %d"
          (List.length managers)
  in
  let content = crac_op |-> "contents" |=> 0 in
  Check.(
    (content |-> "kind" |> as_string = "transaction")
      string
      ~error_msg:"Expected top-level kind %R but got %L") ;
  Check.(
    (content |-> "source" |> as_string = null_pkh)
      string
      ~error_msg:"Expected top-level source %R but got %L") ;
  Check.(
    (content |-> "destination" |> as_string = alias_kt1)
      string
      ~error_msg:"Expected top-level destination %R but got %L") ;
  let internals =
    content |-> "metadata" |-> "internal_operation_results" |> as_list
  in
  let internal_originations_for_alias =
    List.filter
      (fun iop ->
        iop |-> "kind" |> as_string = "origination"
        &&
        let originated =
          iop |-> "result" |-> "originated_contracts" |> as_list
          |> List.map as_string
        in
        originated = [alias_kt1])
      internals
  in
  let origination_internal =
    match internal_originations_for_alias with
    | [op] -> op
    | [] ->
        Test.fail
          "Expected one internal Origination for alias %s among the CRAC's \
           internals, found none"
          alias_kt1
    | ops ->
        Test.fail
          "Expected one internal Origination for alias %s, found %d"
          alias_kt1
          (List.length ops)
  in
  Check.(
    (origination_internal |-> "source" |> as_string = null_pkh)
      string
      ~error_msg:"Expected internal Origination source %R but got %L") ;
  Check.(
    (origination_internal |-> "result" |-> "status" |> as_string = "applied")
      string
      ~error_msg:"Expected alias origination status %R but got %L") ;
  (* The alias materialization must precede the synthetic transfer
     in the CRAC receipt's internal ops: the cross-runtime call
     enters Michelson AFTER its aliases exist. The synthetic
     transfer is sourced by the resolved sender alias (alias(E_1)),
     which equals [alias_kt1] in this test where sender == source. *)
  let position predicate = List.find_index predicate internals |> Option.get in
  let origination_idx =
    position (fun iop ->
        iop |-> "kind" |> as_string = "origination"
        &&
        let originated =
          iop |-> "result" |-> "originated_contracts" |> as_list
          |> List.map as_string
        in
        originated = [alias_kt1])
  in
  let transfer_idx =
    position (fun iop ->
        iop |-> "kind" |> as_string = "transaction"
        && iop |-> "source" |> as_string = alias_kt1)
  in
  Check.(
    (origination_idx < transfer_idx)
      int
      ~error_msg:
        "Expected alias origination at position %L to precede the synthetic \
         transfer at position %R, but it did not") ;
  (* Step 3: Record the EVM sender balance before forwarding. *)
  let*@ evm_balance_before =
    Rpc.get_balance ~address:evm_sender.address sandbox
  in
  (* Step 4: Send tez to the alias KT1. The forwarder contract should
     forward BALANCE to the EVM address via the TezosXGateway. *)
  let forward_amount = Tez.of_int 100 in
  let forward_amount_mutez = Tez.to_mutez forward_amount in
  let* () =
    sandbox_call_michelson_contract
      ~source:Constant.bootstrap5
      ~dest:alias_kt1
      ~arg_data:"Unit"
      ~amount:forward_amount_mutez
      sandbox
  in
  (* Step 5: Verify the EVM sender balance increased. *)
  let*@ evm_balance_after =
    Rpc.get_balance ~address:evm_sender.address sandbox
  in
  let expected_increase = Wei.of_tez forward_amount in
  Check.(
    (evm_balance_after = Wei.(evm_balance_before + expected_increase))
      Wei.typ
      ~error_msg:
        "Expected EVM balance to increase by forwarded amount: expected %R but \
         got %L") ;
  (* L2-1529: the alias was materialized *code-less* — it stores no
     /data/code, so its script is the single shared implementation. The
     successful forwarding above already proves the kernel resolves a
     code-less alias to that shared code end-to-end; here we pin the durable
     shape: no /data/code, but a per-contract /data/storage (the native
     address) is present. *)
  let alias_contract_hex = michelson_contract_hex_of_kt1 alias_kt1 in
  let*@ alias_code =
    Rpc.state_value
      sandbox
      (sf "%s/%s/data/code" tezosx_michelson_contracts_index alias_contract_hex)
  in
  Check.(
    (alias_code = None)
      (option string)
      ~error_msg:"Expected a code-less alias (no /data/code), got %L") ;
  let*@ alias_storage =
    Rpc.state_value
      sandbox
      (sf
         "%s/%s/data/storage"
         tezosx_michelson_contracts_index
         alias_contract_hex)
  in
  Check.(
    (Option.is_some alias_storage = true)
      bool
      ~error_msg:"Expected the alias to keep its own /data/storage (%L)") ;
  (* The EVM node's `/script` RPC resolves the code-less alias to the shared
     forwarder (not an enshrined stub): the forwarder declares `storage
     string`, whereas the stub would be `storage unit`. This exercises the
     node-side `get_code` alias resolution end-to-end. *)
  let* alias_script = get_script sandbox alias_kt1 in
  let storage_prim =
    JSON.(alias_script |-> "code" |> as_list)
    |> List.find (fun item -> JSON.(item |-> "prim" |> as_string) = "storage")
    |> fun item -> JSON.(item |-> "args" |=> 0 |-> "prim" |> as_string)
  in
  Check.(
    (storage_prim = "string")
      string
      ~error_msg:
        "Expected the alias /script to resolve to the forwarder (storage %R), \
         got %L") ;
  unit

(** Test that when an EVM transaction triggers alias materialization
    via the gateway and then reverts, the alias-forwarder origination
    surfaces in the Michelson block with a [Backtracked] status (the
    enclosing synthetic CRAC top-level and any other internal ops are
    backtracked too), while the underlying storage write is rolled
    back so the alias account no longer exists post-revert. *)
let test_alias_forwarder_backtracked_when_evm_reverts () =
  Setup.register_sandbox_test
    ~title:
      "Alias origination is Backtracked when the enclosing EVM frame reverts"
    ~tags:["alias"; "forwarder"; "cross_runtime"; "revert"; "backtracked"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let evm_sender = Eth_account.bootstrap_accounts.(0) in
  let tezos_destination = Constant.bootstrap1.public_key_hash in
  (* Step 1: Deploy a contract that calls the gateway with a valid
     destination (succeeding inside the gateway) and then reverts
     unconditionally. *)
  let* contract =
    Solidity_contracts.gateway_call_then_revert
      (Kernel.select_evm_version Kernel.Latest)
  in
  let bytecode = Tezt.Base.read_file contract.bin in
  let* contract_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender:evm_sender
      ~nonce:0
      ~init_code:("0x" ^ bytecode)
      ()
  in
  (* Step 2: Invoke callAndRevert. The EVM transaction must revert
     (status=false) because the contract's terminal opcode is REVERT. *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender:evm_sender
      ~nonce:1
      ~value:(Wei.of_tez (Tez.of_int 1))
      ~address:contract_address
      ~abi_signature:"callAndRevert(string)"
      ~arguments:[tezos_destination]
      ~expected_status:false
      ()
  in
  (* Step 3: The Tezos alias for the EVM sender (the EOA, which is
     the SOURCE of the EVM transaction) is what the kernel would
     have materialized; compute it via the read-only RPC. The
     transaction reverted before materialization could persist, so
     reading the alias does NOT fault, and the deterministic KT1 is
     what we look for in the (backtracked) receipt below. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_sender.address sandbox
  in
  let alias_kt1 = Result.get_ok alias_result in
  (* Step 4: Inspect the Michelson head block. Expect exactly one
     top-level Transaction, whose status is Backtracked (the EVM
     frame reverted), whose internals contain the alias-forwarder
     origination ALSO Backtracked. *)
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sandbox) with path = "/tezlink"}
  in
  let* block_operations =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_operations ()
  in
  let open JSON in
  let managers = List.nth (as_list block_operations) 3 |> as_list in
  let crac_op =
    match managers with
    | [op] -> op
    | _ ->
        Test.fail
          "Expected exactly one top-level manager operation, found %d"
          (List.length managers)
  in
  let content = crac_op |-> "contents" |=> 0 in
  Check.(
    (content |-> "source" |> as_string = null_pkh)
      string
      ~error_msg:"Expected top-level source %R but got %L") ;
  Check.(
    (content |-> "metadata" |-> "operation_result" |-> "status" |> as_string
   = "backtracked")
      string
      ~error_msg:"Expected top-level status %R but got %L") ;
  let internals =
    content |-> "metadata" |-> "internal_operation_results" |> as_list
  in
  let origination_internal =
    match
      List.filter
        (fun iop ->
          iop |-> "kind" |> as_string = "origination"
          &&
          let originated =
            iop |-> "result" |-> "originated_contracts" |> as_list
            |> List.map as_string
          in
          originated = [alias_kt1])
        internals
    with
    | [op] -> op
    | _ ->
        Test.fail
          "Expected one internal Origination for alias %s among internals"
          alias_kt1
  in
  Check.(
    (origination_internal |-> "result" |-> "status" |> as_string = "backtracked")
      string
      ~error_msg:
        "Expected alias origination status %R but got %L (the storage write \
         was rolled back so the receipt must reflect Backtracked)") ;
  Check.(
    (origination_internal |-> "source" |> as_string = null_pkh)
      string
      ~error_msg:"Expected internal Origination source %R but got %L") ;
  unit

(** Test that two distinct alias materializations triggered by a
    single EVM transaction surface as two separate Origination
    internals inside the merged Michelson CRAC receipt, in
    execution order. The Solidity driver deploys two proxy
    contracts (different addresses → different EVM senders → two
    distinct Tezos aliases) and calls them in sequence inside one
    EVM transaction. The two gateway calls produce two CRAC
    AppliedOperations on the Michelson side, which the kernel's
    [drain_pending_crac_receipts] / [merge_crac_internals] merge
    into one final top-level operation with both alias originations
    interleaved with their respective synthetic transfers. *)
let test_alias_forwarders_multi_crac_in_one_evm_tx () =
  Setup.register_sandbox_test
    ~title:
      "Two alias originations in one EVM transaction surface in execution order"
    ~tags:["alias"; "forwarder"; "cross_runtime"; "nested"; "multi"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let evm_sender = Eth_account.bootstrap_accounts.(0) in
  let destination_a = Constant.bootstrap1.public_key_hash in
  let destination_b = Constant.bootstrap2.public_key_hash in
  (* Step 1: Deploy the driver, which spawns two proxy contracts in
     its constructor. *)
  let* contract =
    Solidity_contracts.gateway_chain_two_aliases
      (Kernel.select_evm_version Kernel.Latest)
  in
  let bytecode = Tezt.Base.read_file contract.bin in
  let* driver_address =
    deploy_evm_contract
      ~sequencer:sandbox
      ~sender:evm_sender
      ~nonce:0
      ~init_code:("0x" ^ bytecode)
      ()
  in
  (* Step 2: Read the two proxy addresses from the driver's public
     getters [proxyA()] / [proxyB()]. *)
  let eth_call ~to_ ~data =
    let* response =
      Evm_node.(
        call_evm_rpc
          sandbox
          {
            method_ = "eth_call";
            parameters =
              `A
                [
                  `O [("to", `String to_); ("data", `String data)];
                  `String "latest";
                ];
          })
    in
    return (response |> Evm_node.extract_result |> JSON.as_string)
  in
  let* proxy_a_data = Cast.calldata "proxyA()" in
  let* proxy_a_hex = eth_call ~to_:driver_address ~data:proxy_a_data in
  let* proxy_b_data = Cast.calldata "proxyB()" in
  let* proxy_b_hex = eth_call ~to_:driver_address ~data:proxy_b_data in
  (* The view return is a left-padded 20-byte address inside a
     32-byte word: take the last 40 hex chars. *)
  let trim_address hex =
    let h =
      if String.length hex >= 2 && String.sub hex 0 2 = "0x" then
        String.sub hex 2 (String.length hex - 2)
      else hex
    in
    "0x" ^ String.sub h (String.length h - 40) 40
  in
  let proxy_a_address = trim_address proxy_a_hex in
  let proxy_b_address = trim_address proxy_b_hex in
  Log.info "proxyA=%s proxyB=%s" proxy_a_address proxy_b_address ;
  (* Step 3: Call runTwoCalls(destinationA, destinationB) with value
     so each proxy forwards half through the gateway. *)
  let* _receipt =
    craft_and_send_evm_transaction
      ~sequencer:sandbox
      ~sender:evm_sender
      ~nonce:1
      ~value:(Wei.of_tez (Tez.of_int 2))
      ~address:driver_address
      ~abi_signature:"runTwoCalls(string,string)"
      ~arguments:[destination_a; destination_b]
      ()
  in
  (* Step 4: Resolve the expected Tezos-side aliases for proxyA and
     proxyB. tez_getEthereumTezosAddress is the deterministic
     read-only lookup, so calling it post-tx returns the canonical
     KT1 either runtime would compute. *)
  let* alias_a_res =
    Rpc.Tezosx.tez_getEthereumTezosAddress proxy_a_address sandbox
  in
  let* alias_b_res =
    Rpc.Tezosx.tez_getEthereumTezosAddress proxy_b_address sandbox
  in
  let alias_a = Result.get_ok alias_a_res in
  let alias_b = Result.get_ok alias_b_res in
  Log.info "alias(proxyA)=%s alias(proxyB)=%s" alias_a alias_b ;
  (* Step 5: Inspect the Michelson head block. Expect ONE top-level
     Transaction (the merged CRAC receipt) whose internals contain
     both proxy aliases as Origination ops, in proxyA-before-proxyB
     order. *)
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sandbox) with path = "/tezlink"}
  in
  let* block_operations =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_operations ()
  in
  let open JSON in
  let managers = List.nth (as_list block_operations) 3 |> as_list in
  let crac_op =
    match managers with
    | [op] -> op
    | _ ->
        Test.fail
          "Expected exactly one top-level manager operation in the merged \
           block, found %d"
          (List.length managers)
  in
  let content = crac_op |-> "contents" |=> 0 in
  Check.(
    (content |-> "source" |> as_string = null_pkh)
      string
      ~error_msg:"Expected top-level source %R but got %L") ;
  let internals =
    content |-> "metadata" |-> "internal_operation_results" |> as_list
  in
  let find_origination_idx alias_kt1 =
    match
      List.find_index
        (fun iop ->
          iop |-> "kind" |> as_string = "origination"
          &&
          let originated =
            iop |-> "result" |-> "originated_contracts" |> as_list
            |> List.map as_string
          in
          originated = [alias_kt1])
        internals
    with
    | Some i -> i
    | None ->
        Test.fail
          "Expected an internal Origination for alias %s among the merged \
           CRAC's internals, found none"
          alias_kt1
  in
  let idx_a = find_origination_idx alias_a in
  let idx_b = find_origination_idx alias_b in
  Check.(
    (idx_a < idx_b)
      int
      ~error_msg:
        "Expected alias(proxyA) origination at position %L to precede \
         alias(proxyB) origination at position %R, but order was reversed") ;
  (* And both must be Applied (the EVM tx did not revert). *)
  let status_at idx =
    List.nth internals idx |-> "result" |-> "status" |> as_string
  in
  Check.(
    (status_at idx_a = "applied")
      string
      ~error_msg:"Expected proxyA alias origination status %R but got %L") ;
  Check.(
    (status_at idx_b = "applied")
      string
      ~error_msg:"Expected proxyB alias origination status %R but got %L") ;
  unit

(** Transfer tez between two accounts on the Michelson runtime and verify
    that the receiver balance is updated. *)
let test_tez_transfer () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Transfer for Michelson runtime"
    ~tags:["transfer"; "michelson"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:[Constant.bootstrap1]
  @@ fun sandbox ->
  let* client = tezos_client sandbox in

  let amount = Tez.one in

  let giver = Constant.bootstrap1.alias in
  let receiver = Constant.bootstrap2.alias in

  let* () = Client.transfer ~amount ~giver ~receiver ~burn_cap:Tez.one client in

  let*@ nb_txs = Rpc.produce_block sandbox in
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

(** The configured Michelson runtime chain id used by the delayed-inbox
    CHAIN_ID regression test.  Deliberately != the default derived value for
    EVM chain id 1337 so that the buggy path (raw LE bytes of 1337) and the
    correct path (configured chain id) can be told apart. *)
let delayed_chain_id_test_value = "NetXohUVN5QWR4f"

(** Verify that a Michelson contract executed via the delayed inbox observes
    the configured Michelson runtime chain id — not the EVM chain id's low
    4 bytes.  All three ingress lanes (sequenced, gateway, delayed) must
    present the same CHAIN_ID value so that Michelson contract behavior is
    independent of which lane delivered the operation. *)
let test_delayed_michelson_chain_id =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~michelson_runtime_chain_id:delayed_chain_id_test_value
    ~title:
      "Delayed-inbox CHAIN_ID matches the configured Michelson runtime chain id"
    ~tags:["michelson"; "chain_id"; "delayed_inbox"; "security"]
    ~with_runtimes:[Tezos]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let* tez_client = tezos_client sequencer in
  (* Inline the chain_id_store script: parameter unit, stores the result of
     CHAIN_ID as (option chain_id). *)
  let chain_id_store_code =
    "parameter unit ; storage (option chain_id) ; code { DROP ; CHAIN_ID ; \
     SOME ; NIL operation ; PAIR }"
  in
  let source = Constant.bootstrap5 in
  (* Step 1: Originate the contract via the delayed inbox. *)
  let* kt1_address =
    Client.originate_contract
      ~alias:"chain_id_record"
      ~amount:Tez.zero
      ~src:source.alias
      ~prg:chain_id_store_code
      ~init:"None"
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  (* Step 2: Call the contract via the delayed inbox to trigger CHAIN_ID. *)
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
      ()
  in
  (* Step 3: Read the stored chain_id and assert it equals the configured
     value.  On the buggy path the contract would see the EVM chain id's low
     4 bytes instead of the configured Michelson runtime chain id. *)
  let* stored_chain_id = Client.contract_storage kt1_address tez_client in
  Check.(
    (String.trim stored_chain_id = sf {|Some "%s"|} delayed_chain_id_test_value)
      string
      ~error_msg:
        "Expected delayed-inbox CHAIN_ID to equal the configured Michelson \
         runtime chain id %R but got %L (the EVM chain id's low bytes were \
         incorrectly used on the delayed ingress lane)") ;
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

(** The P-256 (secp256r1) group order [n], big-endian, as a 64-char hex
    string. The malleable twin of a signature [(r, s)] is [(r, n - s)]. *)
let p256_group_order =
  "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"

(** [be_sub a b] returns [a - b] for two equal-length big-endian byte strings,
    assuming [a >= b]. *)
let be_sub a b =
  let len = String.length a in
  let out = Bytes.create len in
  let borrow = ref 0 in
  for i = len - 1 downto 0 do
    let d = Char.code a.[i] - Char.code b.[i] - !borrow in
    if d < 0 then (
      Bytes.set out i (Char.chr (d + 256)) ;
      borrow := 1)
    else (
      Bytes.set out i (Char.chr d) ;
      borrow := 0)
  done ;
  Bytes.to_string out

(** [low_and_high_s sig_hex] takes a 64-byte P-256 signature [r ‖ s] (128 hex
    chars, no [0x] prefix) and returns its two malleable forms
    [(low_s_hex, high_s_hex)], both 128-char hex strings sharing the same [r].
    Exactly one of [s] and [n - s] lies in [\[1, n/2\]] (the canonical "low-S"
    form); the [verify_tezos_signature] precompile accepts only that one, while
    a verifier missing the low-S guard accepts both. *)
let low_and_high_s sig_hex =
  let bytes_of_hex h = Hex.to_bytes (`Hex h) |> Bytes.to_string in
  let raw = bytes_of_hex sig_hex in
  Check.((String.length raw = 64) int)
    ~error_msg:"expected a 64-byte P-256 signature, got %L bytes" ;
  let r = String.sub raw 0 32 in
  let s = String.sub raw 32 32 in
  let n = bytes_of_hex p256_group_order in
  let n_minus_s = be_sub n s in
  (* Equal-length big-endian byte strings compare as their integer values. *)
  let low, high =
    if String.compare s n_minus_s <= 0 then (s, n_minus_s) else (n_minus_s, s)
  in
  let hex_of x =
    let (`Hex h) = Hex.of_string x in
    h
  in
  (hex_of (r ^ low), hex_of (r ^ high))

(** [sign_bytes_low_s ~signer bytes] signs [bytes] with [signer] and returns the
    signature hex (no [0x] prefix), normalized to canonical low-S for tz3
    (P-256) signers. Octez does not normalize P-256 signatures, so a raw tz3
    signature is often high-S; the [verify_tezos_signature] precompile rejects
    the malleable high-S form, so callers exercising the accepted path must use
    the low-S twin. Non-P256 signatures are returned unchanged. *)
let sign_bytes_low_s ~(signer : Account.key) bytes =
  let signature = Account.sign_bytes ~signer bytes in
  let (`Hex sig_hex) = Tezos_crypto.Signature.to_hex signature in
  if
    String.length signer.public_key_hash >= 3
    && String.sub signer.public_key_hash 0 3 = "tz3"
  then fst (low_and_high_s sig_hex)
  else sig_hex

(** Test EIP-1271 isValidSignature for a single account: create alias,
    sign a hash, assert valid signature returns the magic value,
    and assert wrong hash returns failure. *)
let check_eip1271_for_account ~evm_node ~transfer (account : Account.key) =
  let label = account.alias in
  let eip1271_magic =
    "0x1626ba7e00000000000000000000000000000000000000000000000000000000"
  in
  let eip1271_failure =
    "0xffffffff00000000000000000000000000000000000000000000000000000000"
  in
  (* Create the alias via a cross-runtime transfer from [account]. *)
  let* () = transfer () in
  let*@ alias_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress account.public_key_hash evm_node
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
  let sig_hex = sign_bytes_low_s ~signer:account hash_bytes in
  let sig_with_prefix = "0x" ^ sig_hex in
  let* calldata =
    Cast.calldata
      ~args:[hash_hex; sig_with_prefix]
      "isValidSignature(bytes32,bytes)"
  in
  let*@ call_result = Rpc.call ~to_:alias_address ~data:calldata evm_node in
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
  let*@ bad_result = Rpc.call ~to_:alias_address ~data:bad_calldata evm_node in
  Check.(
    (bad_result = eip1271_failure)
      string
      ~error_msg:(sf "[%s] wrong hash: expected %%R but got %%L" label)) ;
  Log.info "[%s] wrong hash → failure" label ;
  unit

(** Worst-consequence showcase of tz3 (P-256) ECDSA signature malleability.

    A [MalleableReplayVault] EVM contract pays a fixed reward to whoever
    presents a signature — by an authorized tz3 account, validated through the
    alias forwarder's EIP-1271 [isValidSignature] (which reaches the
    [verify_tezos_signature] precompile) — over a fixed voucher hash. The vault
    enforces "one payout per signature" by remembering [keccak256(signature)].

    The attacker holds ONE legitimate voucher signature. They:
      1. redeem with the canonical (low-S) signature → paid once;
      2. rebuild the malleable twin [(r, n - s)] and redeem AGAIN.

    The twin has a different [keccak256], so the vault's anti-replay does not
    fire. Without the precompile's low-S guard the twin's [isValidSignature]
    returns the magic value, the second [redeem] succeeds, and the vault is
    drained twice (worst consequence: theft of the full balance). With the
    guard the twin is rejected, the second [redeem] reverts, and the vault
    keeps the remaining funds.

    The test asserts the guarded behaviour, so it fails (the twin redeem would
    succeed and drain the vault) if the precompile's low-S guard is reverted. *)
let test_p256_malleable_signature_drains_vault () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"tz3 P-256 signature malleability drains a replay-guarded vault"
    ~tags:["eip1271"; "signature"; "p256"; "malleability"; "replay"; "vault"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let sequencer = sandbox in
  let one_ether = Wei.of_string "1000000000000000000" in
  let two_ether = Wei.of_string "2000000000000000000" in
  (* Fund and reveal the tz3 voucher signer (the config tool only seeds
     Ed25519 bootstrap accounts): import its key, fund it from bootstrap5, and
     reveal its manager key. *)
  let* tez_client = tezos_client sandbox in
  let* () =
    Client.import_secret_key
      ~force:true
      tez_client
      tz3_bootstrap.secret_key
      ~alias:tz3_bootstrap.alias
  in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap5.alias
      ~receiver:tz3_bootstrap.public_key_hash
      ~amount:(Tez.of_int 100)
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sandbox in
  let* () = Client.reveal ~src:tz3_bootstrap.alias tez_client |> Runnable.run in
  let*@ _ = Rpc.produce_block sandbox in
  (* Materialize the tz3 alias (an EIP-1271 signer) via a cross-runtime
     transfer, then resolve its EVM address. *)
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:tz3_bootstrap
      ~evm_destination:"0x1111111111111111111111111111111111111111"
      ~transfer_amount:(Tez.of_int 1)
      ~tez_client
      sandbox
  in
  let*@ alias_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress
      tz3_bootstrap.public_key_hash
      sequencer
  in
  Log.info "tz3 alias (EIP-1271 signer) EVM address: %s" alias_address ;
  (* The attacker is a pre-funded EVM bootstrap account. *)
  let attacker = Eth_account.bootstrap_accounts.(0) in
  (* Deploy the vault, funding it with 2 ether (= 2 × PAYOUT) so that a
     successful double-spend is actually payable. *)
  let* vault_contract =
    Solidity_contracts.malleable_replay_vault Evm_version.Shanghai
  in
  let init_code =
    "0x" ^ Tezt.Base.read_file vault_contract.Solidity_contracts.bin
  in
  let* deploy_tx =
    Cast.craft_deploy_tx
      ~source_private_key:attacker.private_key
      ~chain_id:1337
      ~nonce:0
      ~value:two_ether
      ~gas:2_000_000
      ~gas_price:1_000_000_000
      ~data:init_code
      ()
  in
  let*@ deploy_hash = Rpc.send_raw_transaction ~raw_tx:deploy_tx sequencer in
  let*@ _ = Rpc.produce_block sequencer in
  let* deploy_receipt =
    Test_helpers.wait_for_transaction_receipt
      ~evm_node:sequencer
      ~transaction_hash:deploy_hash
      ()
  in
  Check.((deploy_receipt.status = true) bool)
    ~error_msg:"vault deployment transaction failed" ;
  let vault =
    match deploy_receipt.contractAddress with
    | Some addr -> addr
    | None -> Test.fail "vault deployment produced no contract address"
  in
  let*@ vault_balance0 =
    Rpc.get_balance ~address:vault ~block:Latest sequencer
  in
  Check.((vault_balance0 = two_ether) Wei.typ)
    ~error_msg:"vault should hold 2 ether after funding, got %L" ;
  (* The attacker holds ONE valid voucher signature. Sign the voucher hash with
     the tz3 key, then derive its canonical (low-S) form and its malleable twin
     (high-S). [Account.sign_bytes] blake2b-prehashes exactly like the kernel
     verifier, matching the EIP-1271 path used elsewhere in this file. *)
  let voucher_hash_raw =
    "abababababababababababababababababababababababababababababababab"
  in
  let voucher_hash_hex = "0x" ^ voucher_hash_raw in
  let voucher_hash_bytes = Hex.to_bytes (`Hex voucher_hash_raw) in
  let signature = Account.sign_bytes ~signer:tz3_bootstrap voucher_hash_bytes in
  let (`Hex sig_hex) = Tezos_crypto.Signature.to_hex signature in
  let low_s_sig, high_s_sig = low_and_high_s sig_hex in
  (* Helper: call [redeem(address,bytes32,bytes)] and check the receipt status. *)
  let redeem ~nonce ~sig_hex ~expected_status =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:attacker.private_key
        ~chain_id:1337
        ~nonce
        ~value:Wei.zero
        ~gas:2_000_000
        ~gas_price:1_000_000_000
        ~address:vault
        ~signature:"redeem(address,bytes32,bytes)"
        ~arguments:[alias_address; voucher_hash_hex; "0x" ^ sig_hex]
        ()
    in
    let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
    let*@ _ = Rpc.produce_block sequencer in
    let* receipt =
      Test_helpers.wait_for_transaction_receipt
        ~evm_node:sequencer
        ~transaction_hash:tx_hash
        ()
    in
    Check.((receipt.status = expected_status) bool)
      ~error_msg:
        (sf "redeem: expected receipt status %%R but got %%L (nonce %d)" nonce) ;
    unit
  in
  (* First redeem with the canonical (low-S) signature: accepted, paid once. *)
  let* () = redeem ~nonce:1 ~sig_hex:low_s_sig ~expected_status:true in
  let*@ vault_balance1 =
    Rpc.get_balance ~address:vault ~block:Latest sequencer
  in
  Check.((vault_balance1 = one_ether) Wei.typ)
    ~error_msg:"vault should hold 1 ether after the first redeem, got %L" ;
  Log.info "First redeem (low-S) paid out; vault holds 1 ether" ;
  (* Second redeem with the malleable twin (high-S). On the FIXED kernel the
     twin is rejected by the P-256 verifier, so [isValidSignature] fails, the
     [redeem] reverts (status 0), and the vault keeps its remaining ether.
     On a vulnerable kernel this redeem SUCCEEDS and drains the vault to 0. *)
  let* () = redeem ~nonce:2 ~sig_hex:high_s_sig ~expected_status:false in
  let*@ vault_balance2 =
    Rpc.get_balance ~address:vault ~block:Latest sequencer
  in
  Check.((vault_balance2 = one_ether) Wei.typ)
    ~error_msg:
      "REPLAY: the malleable twin drained the vault — expected 1 ether to \
       remain but vault holds %L (0 ether means the high-S guard is missing)" ;
  Log.info "Malleable twin (high-S) rejected; vault not drained" ;
  unit

(** Boots on Kernel.Previewnet with the Tezos runtime enabled at genesis,
    upgrades to Kernel.Latest, then checks the AliasForwarder predeploy still
    forwards a cross-runtime transfer and validates a signature end to end. *)
let test_kernel_upgrade_replaces_alias_forwarder =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Kernel upgrade preserves the AliasForwarder"
    ~tags:["upgrade"; "kernel_governance"; "alias"; "forwarder"]
    ~with_runtimes:[Tezos]
    ~kernel:Kernel.Previewnet
    ~additional_uses:[Constant.WASM.evm_kernel]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun setup _protocol ->
  let Tezt_etherlink.Setup.
        {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      =
    setup
  in
  (* The admin contract's dictator defaults to bootstrap2, so that is the
     key that must sign the upgrade governance transfer. *)
  let admin = Constant.bootstrap2 in
  let alias_forwarder_address = "0xff00000000000000000000000000000000ffff08" in
  let*@ _ = Rpc.produce_block sequencer in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~client ~sequencer ()
  in
  (* The AliasForwarder is installed from genesis because the setup
     enabled the Tezos runtime. *)
  let*@ pre_upgrade_code =
    Rpc.get_code ~address:alias_forwarder_address sequencer
  in
  Check.((pre_upgrade_code <> "0x") string)
    ~error_msg:
      "Pre-upgrade AliasForwarder should have non-empty bytecode, got %L" ;
  (* Upgrade from Previewnet to Latest as a plain kernel swap; the
     governance flag is already set via the genesis runtime. *)
  let* _root_hash =
    Test_helpers.upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:admin.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses Kernel.Latest)
      ~activation_timestamp:"0"
  in
  let* () =
    repeat 3 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let*@ _ = Rpc.produce_block sequencer in
  let* () =
    Test_helpers.bake_until_sync
      ~timeout:120.
      ~sc_rollup_node
      ~client
      ~sequencer
      ()
  in
  let*@ post_upgrade_code =
    Rpc.get_code ~address:alias_forwarder_address sequencer
  in
  Check.((post_upgrade_code <> "0x") string)
    ~error_msg:
      "Post-upgrade AliasForwarder should have non-empty bytecode, got %L" ;
  (* Previewnet genesis does not fund EVM bootstrap accounts, so credit the
     EVM sender from the Tezos side before driving the forwarding path. *)
  let evm_sender = Eth_account.bootstrap_accounts.(0) in
  let tezos_receiver = Constant.bootstrap1.public_key_hash in
  let* () =
    michelson_to_evm_transfer
      ~source:Constant.bootstrap3
      ~evm_destination:evm_sender.address
      ~transfer_amount:(Tez.of_int 10)
      ~counter:1
      setup
  in
  (* The forwarder must still forward a cross-runtime transfer after the
     upgrade: sending tez from the EVM sender to a Tezos address forces the
     kernel to materialize the sender alias and forward the value. *)
  let* _receipt =
    check_evm_to_michelson_transfer
      ~sequencer
      ~sender:evm_sender
      ~nonce:0
      ~tezos_destination:tezos_receiver
      ~transfer_amount:(Tez.of_int 1)
      ()
  in
  (* Check the upgraded forwarder's signature validation endpoint: a valid
     signature must be accepted and a wrong one rejected on a fresh alias. *)
  check_eip1271_for_account
    ~evm_node:sequencer
    ~transfer:(fun () ->
      michelson_to_evm_transfer
        ~source:Constant.bootstrap1
        ~evm_destination:"0x1111111111111111111111111111111111111111"
        ~transfer_amount:(Tez.of_int 1)
        ~counter:1
        setup)
    Constant.bootstrap1

(** EIP-1271 signature verification across tz1, tz2 and tz3 key types *)
let test_eip1271_signature_verification () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"EIP-1271 isValidSignature on tz1, tz2 and tz3 aliases"
    ~tags:["eip1271"; "signature"; "alias"; "cross_runtime"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let* tez_client = tezos_client sandbox in
  (* Fund and reveal tz2/tz3 accounts since the config tool only supports
     Ed25519 bootstrap accounts. Import their keys into the client, fund from
     bootstrap5, then reveal the manager key. *)
  let fund_and_reveal (account : Account.key) =
    let* () =
      Client.import_secret_key
        ~force:true
        tez_client
        account.secret_key
        ~alias:account.alias
    in
    let* () =
      Client.transfer
        ~giver:Constant.bootstrap5.alias
        ~receiver:account.public_key_hash
        ~amount:(Tez.of_int 100)
        ~burn_cap:Tez.one
        tez_client
    in
    let*@ _ = Rpc.produce_block sandbox in
    let* () = Client.reveal ~src:account.alias tez_client |> Runnable.run in
    let*@ _ = Rpc.produce_block sandbox in
    unit
  in
  let* () = fund_and_reveal tz2_bootstrap in
  let* () = fund_and_reveal tz3_bootstrap in
  let check (account : Account.key) =
    check_eip1271_for_account
      ~evm_node:sandbox
      ~transfer:(fun () ->
        sandbox_michelson_to_evm_transfer
          ~source:account
          ~evm_destination:"0x1111111111111111111111111111111111111111"
          ~transfer_amount:(Tez.of_int 1)
          ~tez_client
          sandbox)
      account
  in
  (* tz1: Ed25519 *)
  let* () = check Constant.bootstrap1 in
  (* tz2: secp256k1 *)
  let* () = check tz2_bootstrap in
  (* tz3: P256 *)
  check tz3_bootstrap

(** Regression test: a well-formed signature signed over a different hash
    than the one submitted must be rejected by isValidSignature. *)
let test_eip1271_wrong_signature_rejected () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"EIP-1271 isValidSignature rejects a wrong signature"
    ~tags:["eip1271"; "signature"; "alias"; "wrong_signature"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let account = Constant.bootstrap1 in
  let evm_destination = "0x1111111111111111111111111111111111111111" in
  let eip1271_failure =
    "0xffffffff00000000000000000000000000000000000000000000000000000000"
  in
  let* () =
    sandbox_michelson_to_evm_transfer
      ~source:account
      ~evm_destination
      ~transfer_amount:(Tez.of_int 1)
      sandbox
  in
  let*@ alias_address =
    Rpc.Tezosx.tez_getTezosEthereumAddress account.public_key_hash sandbox
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
  let*@ call_result = Rpc.call ~to_:alias_address ~data:calldata sandbox in
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

(* Branch validation (L2-1748): valid/foreign branch across the tx-pool and delayed-inbox paths. *)

(* A well-formed block hash that is not a block of this instance. *)
let foreign_branch = "BL5HqLzfzPd4b72LwoGtwgXYth26dvit8S8RW4tMkQgbzTWfzww"

let branch_transfer_amount = 1_000_000

let branch_transfer_fee = 1_000_000

let branch_transfer_gas_limit = 10_000

(* Test 1: tx-pool transfer with a recent branch is applied. *)
let test_native_branch_valid_applied () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Native transfer with a recent branch is applied (tx pool)"
    ~tags:["branch"; "liveness"; "replay"; "transfer"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap1 in
  let dest = Constant.bootstrap2 in
  let* tez_client = tezos_client sandbox in
  let* balance_before =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  let* branch = Client.RPC.call tez_client @@ RPC.get_chain_block_hash () in
  let* counter =
    Client.RPC.call tez_client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:source.public_key_hash
         ()
  in
  let counter = JSON.as_int counter + 1 in
  let* op =
    Operation.Manager.mk_single_transfer
      ~source
      ~fee:branch_transfer_fee
      ~gas_limit:branch_transfer_gas_limit
      ~amount:branch_transfer_amount
      ~counter
      ~branch
      ~dest
      tez_client
  in
  let* (`OpHash _) = Operation.inject ~dont_wait:true op tez_client in
  let*@ _ = Rpc.produce_block sandbox in
  let* balance_after =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  Check.(
    (Tez.to_mutez balance_after
    = Tez.to_mutez balance_before + branch_transfer_amount)
      int)
    ~error_msg:"Recipient balance: expected %R, got %L" ;
  unit

(* Test 2: delayed-inbox transfer with a recent branch is applied. *)
let test_native_branch_valid_delayed_applied =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Delayed native transfer with a recent branch is applied"
    ~tags:["branch"; "liveness"; "replay"; "delayed_inbox"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap1 in
  let dest = Constant.bootstrap2 in
  let* tez_client = tezos_client sequencer in
  let* balance_before =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  let* branch = tezlink_branch ~sc_rollup_node ~client ~sequencer in
  let* counter =
    Client.RPC.call tez_client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:source.public_key_hash
         ()
  in
  let counter = JSON.as_int counter + 1 in
  let* op =
    Operation.Manager.mk_single_transfer
      ~source
      ~fee:branch_transfer_fee
      ~gas_limit:branch_transfer_gas_limit
      ~amount:branch_transfer_amount
      ~counter
      ~branch
      ~dest
      client
  in
  let* () =
    send_tezos_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      op
  in
  let* balance_after =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  Check.(
    (Tez.to_mutez balance_after
    = Tez.to_mutez balance_before + branch_transfer_amount)
      int)
    ~error_msg:"Recipient balance: expected %R, got %L" ;
  unit

(* Test 3: a transfer with a foreign branch is rejected by the prevalidator at
   injection (so it never gets included and pays no inclusion fees). *)
let test_native_branch_foreign_rejected () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Native transfer with a foreign branch is rejected (tx pool)"
    ~tags:["branch"; "liveness"; "replay"; "transfer"; "security"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sandbox ->
  let source = Constant.bootstrap1 in
  let dest = Constant.bootstrap2 in
  let* tez_client = tezos_client sandbox in
  let* counter =
    Client.RPC.call tez_client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:source.public_key_hash
         ()
  in
  let counter = JSON.as_int counter + 1 in
  let* op =
    Operation.Manager.mk_single_transfer
      ~source
      ~fee:branch_transfer_fee
      ~gas_limit:branch_transfer_gas_limit
      ~amount:branch_transfer_amount
      ~counter
      ~branch:foreign_branch
      ~dest
      tez_client
  in
  let* _ =
    Operation.inject
      ~error:(rex "evm_node.dev.tezlink.outdated_operation")
      ~dont_wait:true
      op
      tez_client
  in
  unit

(* Test 4: a foreign-branch transfer routed through the delayed inbox is dropped
   at entry; it never enters the inbox and no funds move. *)
let test_native_branch_foreign_rejected_delayed =
  Setup.register_fullstack_test
    ~time_between_blocks:Nothing
    ~title:"Delayed native transfer with a foreign branch is dropped"
    ~tags:["branch"; "liveness"; "replay"; "delayed_inbox"; "security"]
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let source = Constant.bootstrap1 in
  let dest = Constant.bootstrap2 in
  let* tez_client = tezos_client sequencer in
  let* balance_before =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  let* counter =
    Client.RPC.call tez_client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:source.public_key_hash
         ()
  in
  let counter = JSON.as_int counter + 1 in
  let* op =
    Operation.Manager.mk_single_transfer
      ~source
      ~fee:branch_transfer_fee
      ~gas_limit:branch_transfer_gas_limit
      ~amount:branch_transfer_amount
      ~counter
      ~branch:foreign_branch
      ~dest
      client
  in
  (* Send to the delayed bridge without waiting for inbox insertion: the op is
     dropped at entry (foreign branch), so it never enters the delayed inbox. *)
  let* _hash =
    Delayed_inbox.send_tezos_operation_to_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~tezosx_format:true
      op
  in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* balance_after =
    Client.get_balance_for ~account:dest.public_key_hash tez_client
  in
  Check.((Tez.to_mutez balance_after = Tez.to_mutez balance_before) int)
    ~error_msg:
      "Foreign-branch delayed transfer must be dropped: recipient balance \
       changed from %R to %L" ;
  unit

(** Fetch the head block's first manager operation through the Tezlink RPC and
    check its operation_result's address_registry_diff as [(address, index)]
    pairs. The field is a dft in the protocol encoding, so it may be absent
    when empty. *)
let check_head_registry_diff ~__LOC__ sandbox expected =
  let tezlink_endpoint =
    Endpoint.{(Evm_node.rpc_endpoint_record sandbox) with path = "/tezlink"}
  in
  let* operation =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:3
         ~operation_offset:0
         ()
  in
  let open JSON in
  let op_result =
    operation |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
  in
  Check.(
    (op_result |-> "status" |> as_string = "applied")
      string
      ~__LOC__
      ~error_msg:"Expected operation status %R but got %L") ;
  let diff =
    op_result |-> "address_registry_diff" |> as_list_opt
    |> Option.value ~default:[]
    |> List.map (fun entry ->
           (entry |-> "address" |> as_string, entry |-> "index" |> as_string))
  in
  Check.(
    (diff = expected)
      (list (tuple2 string string))
      ~__LOC__
      ~error_msg:"Expected address_registry_diff %R but got %L") ;
  unit

let test_michelson_address_registry () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson INDEX_ADDRESS and GET_ADDRESS_INDEX on Tezos X"
    ~tags:["michelson"; "address_registry"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  let originate ~script_name =
    sandbox_originate_michelson_contract
      ~source
      ~script_name
      ~init_storage_data:"None"
      sandbox
  in
  let call ~dest pkh =
    sandbox_call_michelson_contract
      ~source
      ~dest
      ~arg_data:(sf {|"%s"|} pkh)
      sandbox
  in
  let check_storage ~__LOC__ kt1 expected =
    let* storage = Client.contract_storage kt1 tez_client in
    Check.(
      (remove_whitespace storage = remove_whitespace expected)
        string
        ~__LOC__
        ~error_msg:"Expected contract storage %R but got %L") ;
    unit
  in
  let check_head_registry_diff ~__LOC__ expected =
    check_head_registry_diff ~__LOC__ sandbox expected
  in
  (* Originate a contract that stores the result of INDEX_ADDRESS on its
     parameter. *)
  let* idx_kt1 = originate ~script_name:["opcodes"; "index_address"] in
  Log.info "Originated index_address contract: %s" idx_kt1 ;
  (* Index 0 is reserved for the pre-registered null address (as on L1), so the
     first user-registered address gets index 1. *)
  let bootstrap1_pkh = Constant.bootstrap1.public_key_hash in
  let* () = call ~dest:idx_kt1 bootstrap1_pkh in
  let* () = check_storage ~__LOC__ idx_kt1 "Some 1" in
  (* The fresh registration is reported on the operation receipt. *)
  let* () = check_head_registry_diff ~__LOC__ [(bootstrap1_pkh, "1")] in
  (* Registering the same address again is idempotent. *)
  let* () = call ~dest:idx_kt1 bootstrap1_pkh in
  let* () = check_storage ~__LOC__ idx_kt1 "Some 1" in
  (* ... and an idempotent re-registration emits no receipt diff. *)
  let* () = check_head_registry_diff ~__LOC__ [] in
  (* A distinct address gets the next index. *)
  let* () = call ~dest:idx_kt1 Constant.bootstrap2.public_key_hash in
  let* () = check_storage ~__LOC__ idx_kt1 "Some 2" in
  let* () =
    check_head_registry_diff
      ~__LOC__
      [(Constant.bootstrap2.public_key_hash, "2")]
  in
  (* Originate a contract that stores the result of GET_ADDRESS_INDEX on its
     parameter. *)
  let* get_kt1 = originate ~script_name:["opcodes"; "get_address_index"] in
  Log.info "Originated get_address_index contract: %s" get_kt1 ;
  (* The registry is shared: an address registered through the index_address
     contract is visible from the get_address_index contract. *)
  let* () = call ~dest:get_kt1 bootstrap1_pkh in
  let* () = check_storage ~__LOC__ get_kt1 "Some 1" in
  (* GET_ADDRESS_INDEX is read-only: no receipt diff. *)
  let* () = check_head_registry_diff ~__LOC__ [] in
  (* An address that was never registered yields None. *)
  let* () = call ~dest:get_kt1 Constant.bootstrap3.public_key_hash in
  let* () = check_storage ~__LOC__ get_kt1 "None" in
  unit

(** L2-1452 scenario: INDEX_ADDRESS is rejected by the typechecker directly in
    a view, but the restriction does not cross lambda boundaries, so a lambda
    EXEC'd by a view still reaches the instruction at runtime. Unlike Michelson
    storage writes, the registry write goes straight to durable storage
    (bypassing the journal), so this pins E2E that it nevertheless shares the
    fate of the enclosing operation — the same semantics as L1's, tested by
    "Contract onchain opcodes: INDEX_ADDRESS in a view lambda" in
    tezt/tests/contract_onchain_opcodes.ml:
    - if the operation fails after the view returned, the write is reverted
      with the rest of the transaction (no registry entry, and the registry
      counter does not advance) — the E2E counterpart of the
      [index_address_write_is_reverted_on_transaction_revert] unit test in
      tezos_execution/src/mir_ctx.rs;
    - if the operation succeeds, the registration persists and is reported on
      the caller's receipt diff — counterpart of
      [index_address_write_persists_on_transaction_commit]. *)
let test_michelson_address_registry_view_lambda () =
  Setup.register_sandbox_test
    ~uses_client:true
    ~title:"Michelson INDEX_ADDRESS in a view lambda on Tezos X"
    ~tags:["michelson"; "address_registry"; "view"]
    ~with_runtimes:[Tezos]
  @@ fun sandbox ->
  let source = Constant.bootstrap5 in
  let* tez_client = tezos_client sandbox in
  let originate ~script_name ~init_storage_data =
    sandbox_originate_michelson_contract
      ~source
      ~script_name
      ~init_storage_data
      sandbox
  in
  (* The contract whose view EXECs a lambda containing INDEX_ADDRESS, and the
     caller that invokes the view on-chain through VIEW and then either fails
     with the returned index or stores it. *)
  let* target_kt1 =
    originate
      ~script_name:["opcodes"; "index_address_view_lambda"]
      ~init_storage_data:"Unit"
  in
  let* caller_kt1 =
    originate
      ~script_name:["opcodes"; "index_address_view_lambda_caller"]
      ~init_storage_data:"None"
  in
  (* Probes for the durable registry state: get_address_index reads an entry;
     index_address registers a fresh address, so the index it reports reveals
     the registry counter. *)
  let* get_kt1 =
    originate
      ~script_name:["opcodes"; "get_address_index"]
      ~init_storage_data:"None"
  in
  let* idx_kt1 =
    originate
      ~script_name:["opcodes"; "index_address"]
      ~init_storage_data:"None"
  in
  let check_storage ~__LOC__ kt1 expected =
    let* storage = Client.contract_storage kt1 tez_client in
    Check.(
      (remove_whitespace storage = remove_whitespace expected)
        string
        ~__LOC__
        ~error_msg:"Expected contract storage %R but got %L") ;
    unit
  in
  let registrand = Constant.bootstrap1.public_key_hash in
  (* Failure leg: the caller FAILWITHs (with the index the view just returned)
     after the view call. [~force:true] injects the operation the client
     predicts will fail, so the failed operation is actually included in a
     block and its revert is exercised. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:caller_kt1
      ~arg_data:(sf {|Pair "%s" "%s" True|} target_kt1 registrand)
      ~force:true
      ~gas_limit:500_000
      ~storage_limit:100
      ~fee:(Tez.of_int 1)
      sandbox
  in
  let* ops =
    Client.RPC.call tez_client (Node.RPC.get_chain_block_operations ())
  in
  let operation_result =
    match JSON.as_list ops |> List.concat_map JSON.as_list with
    | [op] ->
        JSON.(op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
    | _ -> Test.fail "Expected one operation in the block, got something else"
  in
  Check.(
    (JSON.(operation_result |-> "status" |> as_string) = "failed")
      string
      ~__LOC__
      ~error_msg:"Expected operation status %R but got %L") ;
  (* The caller FAILWITH'd with the index INDEX_ADDRESS assigned inside the
     view before the operation was reverted: index 0 is the pre-registered
     zero address, so the first fresh registration takes 1. This pins that
     the instruction did run and its durable write was then undone — rather
     than never having happened. Tezlink receipts flatten errors into a
     single {kind; id; error_message} object (L2-363), so unlike on L1 there
     is no structured "with" field to read the rejection value from: look for
     the interpreter's "failed with: Nat(1)" rendering instead. *)
  let error_messages =
    JSON.(operation_result |-> "errors" |> as_list)
    |> List.map (fun err ->
           JSON.(err |-> "error_message" |> as_string_opt)
           |> Option.value ~default:(JSON.encode err))
    |> String.concat "; "
  in
  Check.(error_messages =~ rex "failed with: Nat\\(1\\)")
    ~error_msg:"Expected the failure receipt (%L) to mention %R" ;
  (* The reverted registration left no durable trace: no entry for the
     address... *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:get_kt1
      ~arg_data:(sf {|"%s"|} registrand)
      sandbox
  in
  let* () = check_storage ~__LOC__ get_kt1 "None" in
  (* ...and the counter did not advance: the next fresh registration still
     takes index 1. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:idx_kt1
      ~arg_data:(sf {|"%s"|} Constant.bootstrap2.public_key_hash)
      sandbox
  in
  let* () = check_storage ~__LOC__ idx_kt1 "Some 1" in
  (* Success leg: same call without the deliberate failure. The registration
     made inside the view persists: it lands in the caller's storage (index 2,
     the counter-probe above consumed 1)... *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:caller_kt1
      ~arg_data:(sf {|Pair "%s" "%s" False|} target_kt1 registrand)
      sandbox
  in
  let* () = check_storage ~__LOC__ caller_kt1 "Some 2" in
  (* ...is reported on the *caller*'s receipt diff... *)
  let* () = check_head_registry_diff ~__LOC__ sandbox [(registrand, "2")] in
  (* ...and is durably visible from another contract. *)
  let* () =
    sandbox_call_michelson_contract
      ~source
      ~dest:get_kt1
      ~arg_data:(sf {|"%s"|} registrand)
      sandbox
  in
  check_storage ~__LOC__ get_kt1 "Some 2"

let () =
  test_native_branch_valid_applied () ;
  test_native_branch_valid_delayed_applied [Alpha] ;
  test_native_branch_foreign_rejected_delayed [Alpha] ;
  test_native_branch_foreign_rejected () ;
  test_bootstrap_kernel_config () ;
  test_low_fee_op_refused_by_sequencer_accepted_via_delayed_inbox [Alpha] ;
  test_deposit [Alpha] ;
  test_reveal () ;
  test_delayed_inbox_transfer () ;
  test_cross_runtime_transfer_from_evm_to_tz () ;
  test_cross_runtime_transfer_to_evm_via_call () ;
  test_evm_gateway_rejects_removed_transfer_selector () ;
  test_michelson_gateway_rejects_removed_default_entrypoint () ;
  test_cross_runtime_transfer_to_evm_via_call_evm () ;
  test_cross_runtime_call_executes_evm_bytecode () ;
  test_cross_runtime_call_get_from_michelson_routes_to_static () ;
  test_cross_runtime_staticcall_evm_from_on_chain_entrypoint () ;
  test_cross_runtime_staticcall_evm_nested_view_view () ;
  test_cross_runtime_staticcall_evm_nested_view_preserves_origin () ;
  test_cross_runtime_transfer_from_evm_to_kt1 () ;
  test_cross_runtime_call_failwith () ;
  test_cross_runtime_call_from_evm_to_michelson () ;
  test_cross_runtime_view_call_from_evm_to_michelson () ;
  test_cross_runtime_view_call_via_low_level_call () ;
  test_cross_runtime_view_call_negative () ;
  test_cross_runtime_call_from_michelson_to_evm () ;
  test_michelson_gateway_evm_revert () ;
  test_cross_runtime_fa12_approve_from_evm () ;
  test_cross_runtime_erc20_transfer_from_michelson () ;
  test_cross_runtime_call_from_michelson_contract_to_evm () ;
  test_cross_runtime_call_from_michelson_contract_to_evm_revert () ;
  test_cross_runtime_transfer_from_michelson_contract_to_evm () ;
  test_tezos_block_stored_after_deposit [Alpha] ;
  test_michelson_origination_and_call () ;
  test_michelson_get_balance () ;
  test_michelson_call_nonexistent_contract () ;
  test_michelson_call_wrong_entrypoint () ;
  test_michelson_origination_wrong_storage_type () ;
  test_michelson_forged_big_map_id_rejected () ;
  test_michelson_call_failwith () ;
  test_michelson_inter_contract_call () ;
  test_michelson_internal_call_revert () ;
  test_evm_gateway_catch_revert () ;
  test_eth_rpc_with_alias ~runtime:Tezos [Alpha] ;
  test_runtime_feature_flag ~runtime:Tezos () ;
  test_gas_refund_feature_flag () ;
  test_get_tezos_ethereum_address_rpc ~runtime:Tezos () ;
  test_get_ethereum_tezos_address_rpc ~runtime:Tezos () ;
  test_tx_queue_mixed_transaction_types ~runtime:Tezos () ;
  test_manager_key_on_block_hash () ;
  test_instant_confirmations ~runtime:Tezos () ;
  test_state_root_blueprint_uniqueness ~runtime:Tezos () ;
  test_state_root_pure_michelson_divergence () ;
  test_nested_crac () ;
  test_call_from_evm_to_michelson ~runtime:Tezos () ;
  test_call_from_michelson_to_evm ~runtime:Tezos () ;
  test_tezosx_simulation () ;
  test_entrypoints_enshrined () ;
  test_script_coherency_enshrined () ;
  test_script_synthetic_views_enshrined () ;
  test_script_no_synthetic_views_for_originated_contract () ;
  test_trace_call_evm ~runtime:Tezos () ;
  test_http_trace_replay ~runtime:Tezos () ;
  test_cross_runtime_evm_sender_is_alias () ;
  test_cross_runtime_michelson_sender_is_alias () ;
  test_alias_forwarder_forwards_to_evm () ;
  test_alias_forwarder_created_by_evm_cross_runtime_call () ;
  test_alias_forwarder_backtracked_when_evm_reverts () ;
  test_alias_forwarders_multi_crac_in_one_evm_tx () ;
  test_kernel_upgrade_replaces_alias_forwarder [Alpha] ;
  test_tez_transfer () ;
  test_michelson_runtime_chain_id_derivation
    ~evm_chain_id:1337
    ~expected_chain_id:"NetXUSADs17gHCN"
    () ;
  test_michelson_runtime_chain_id_derivation
    ~evm_chain_id:42793
    ~expected_chain_id:"NetXohUVN5QWR4f"
    () ;
  test_michelson_chain_id_in_crac ~runtime:Tezos () ;
  test_delayed_michelson_chain_id [Alpha] ;
  test_eip1271_signature_verification () ;
  test_eip1271_wrong_signature_rejected () ;
  test_p256_malleable_signature_drains_vault () ;
  test_meta_block_rpcs ~runtime:Tezos () ;
  test_meta_block_rpcs_without_michelson_runtime () ;
  test_michelson_address_registry () ;
  test_michelson_address_registry_view_lambda ()
