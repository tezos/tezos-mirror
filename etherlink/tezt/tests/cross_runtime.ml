(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file cross_runtime.ml
 *)

open Rpc.Syntax

module EvmContract = struct
  let tezosx_evm_chain_id = 1337

  (** [deploy_contract ~sequencer ~sender ~nonce ~init_code ()] deploys an
   *  EVM contract from [init_code], produces a block, and returns the deployed
   *  contract address.  Fails if the deployment transaction reverts or yields no
   *  contract address. *)
  let deploy_contract ~sequencer ~sender ~nonce ~init_code () =
    let* raw_tx =
      Cast.craft_deploy_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:tezosx_evm_chain_id
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

  (** [deploy_solidity_contract ~sequencer ~sender ~nonce ~contract ()]
   *  compiles and deploys a Solidity contract. *)
  let deploy_solidity_contract ?(evm_version = Evm_version.Shanghai) ~sequencer
      ~sender ~nonce ~contract () =
    let* contract = contract evm_version in
    let init_code =
      "0x" ^ Tezt.Base.read_file contract.Solidity_contracts.bin
    in
    deploy_contract ~sequencer ~sender ~nonce ~init_code ()

  (** [craft_and_send_transaction ~sequencer ~sender ~nonce ~value ~address
   *  ~abi_signature ~arguments ()] crafts an EVM transaction, sends it, produces
   *  a block, asserts the receipt status matches [expected_status] (default
    [true]), and returns the receipt. *)
  let craft_and_send_transaction ~sequencer ~sender ~nonce ~value ~address
      ~abi_signature ~arguments ?(expected_status = true) ?access_list ?legacy
      ?(gas = 3_000_000) () =
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
        ?access_list
        ?legacy
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
end

module TezContract = struct
  (** Durable storage path where Michelson originated contracts are indexed in
   *  Tezos X. Each subkey is the hex-encoded [Contract_repr.t] of the
   *  Michelson contract. *)
  let tezosx_michelson_contracts_index = "/evm/world_state/contracts/index"

  (** [decode_contract_address hex] decodes a hex-encoded
   *  [Contract_repr.t] into a b58check KT1 address string. *)
  let decode_contract_address hex =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    Hex.to_bytes (`Hex hex)
    |> Data_encoding.Binary.of_bytes_exn C.encoding
    |> C.to_b58check

  (** [send_op_to_delayed_inbox_and_wait] sends a Tezos operation via the
   *  delayed inbox and waits until it is included and the delayed inbox is
   *  empty. *)
  let send_op_to_delayed_inbox_and_wait ~sc_rollup_address ~sc_rollup_node
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

  (** [originate_contract_via_delayed_inbox] originates a Michelson
   *  contract via the delayed inbox. Loads the script from
   *  [michelson_test_scripts], converts code and initial storage to JSON,
   *  forges and sends the origination operation, then returns the hex key and
   *  KT1 address of the new contract. *)
  let originate_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node
      ~client ~l1_contracts ~sequencer ~source ~counter ~script_name
      ~init_storage_data ?(init_balance = 0) protocol =
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
      send_op_to_delayed_inbox_and_wait
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
    let kt1_address = decode_contract_address contract_hex in
    Log.info "Originated contract: %s" kt1_address ;
    return (contract_hex, kt1_address)

  (** [call_contract_via_delayed_inbox] calls a Michelson contract
   *  via the delayed inbox. Converts the argument from Michelson notation to
   *  JSON, forges and sends the call operation. *)
  let call_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node ~client
      ~l1_contracts ~sequencer ~source ~counter ~dest ~arg_data
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
    send_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      call_op

  (** [get_consumed_milligas ~block sequencer] queries the tezlink RPC
   *  for [block]'s first manager operation and returns the top-level
   *  [consumed_milligas] from the operation result.
   *
   *  @param block Block identifier (default ["head"]). *)
  let get_consumed_milligas ?(block = "head") sequencer =
    let path = sf "/tezlink/chains/main/blocks/%s/operations/3/0" block in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"tezlink_operation_receipt" res in
    return
      JSON.(
        json |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
        |-> "consumed_milligas" |> as_int)

  (** [get_gateway_consumed_milligas ~block ~entrypoint sequencer] queries
   *  the tezlink RPC for [block]'s first manager operation and returns
   *  the [consumed_milligas] of the internal gateway operation matching
   *  [entrypoint].
   *
   *  @param block Block identifier (default ["head"]).
   *  @param entrypoint Gateway entrypoint to match (default ["call_evm"]). *)
  let get_gateway_consumed_milligas ?(block = "head") ?(entrypoint = "call_evm")
      sequencer =
    let path = sf "/tezlink/chains/main/blocks/%s/operations/3/0" block in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"tezlink_operation_receipt" res in
    let internal_ops =
      JSON.(
        json |-> "contents" |=> 0 |-> "metadata"
        |-> "internal_operation_results" |> as_list)
    in
    match
      List.find_opt
        (fun op ->
          JSON.(op |-> "parameters" |-> "entrypoint" |> as_string_opt)
          = Some entrypoint)
        internal_ops
    with
    | None ->
        Test.fail
          "get_gateway_consumed_milligas: no internal operation with \
           entrypoint %S found in block %s (found %d internal ops)"
          entrypoint
          block
          (List.length internal_ops)
    | Some gateway_op ->
        let consumed =
          JSON.(gateway_op |-> "result" |-> "consumed_milligas" |> as_int)
        in
        return consumed

  (** [read_michelson_contract_storage sc_rollup_node contract_hex] reads the
   *  storage of an originated Michelson contract from the durable storage,
   *  given its hex-encoded [Contract_repr.t] key. *)
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
   *  [Contract_repr.t] into a b58check KT1 address string. *)
  let decode_michelson_contract_address hex =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    Hex.to_bytes (`Hex hex)
    |> Data_encoding.Binary.of_bytes_exn C.encoding
    |> C.to_b58check

  (** [decode_micheline_storage hex_str] decodes a hex string from the durable
   *  storage into a Micheline [JSON.u] value. Returns [None] if decoding
   *  fails. *)
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

  (** Reads and decodes the storage of the Michelson contract identified
   *  by [contract_hex].  Fails if the storage is missing or cannot be
   *  decoded. *)
  let get_storage ~sc_rollup_node contract_hex =
    let* storage_raw =
      read_michelson_contract_storage sc_rollup_node contract_hex
    in
    match storage_raw with
    | None ->
        let kt1 = decode_michelson_contract_address contract_hex in
        Test.fail "Storage not found for contract %s" kt1
    | Some hex_str -> (
        match decode_micheline_storage hex_str with
        | None ->
            let kt1 = decode_michelson_contract_address contract_hex in
            Test.fail "Storage failed to be decoded for contract %s" kt1
        | Some storage -> return storage)
end

(** Contracts *)

(** Common [run()] caller for EVM contracts. *)
module EvmRunner = struct
  open EvmContract

  (** Calls [run()] on the given EVM contract and returns [gasUsed]. *)
  let call_run ?expected_status ~sequencer ~sender ~nonce ~value ?access_list
      ?gas runner =
    let legacy = if Option.is_some access_list then Some false else None in
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:runner
        ~abi_signature:"run()"
        ~arguments:[]
        ?expected_status
        ?access_list
        ?legacy
        ?gas
        ()
    in
    return receipt.gasUsed
end

(** EVM contract that forwards [run()] to a Tezos target via the
 *  CRAC gateway.  Increments [counter] before and after the
 *  cross-runtime call. *)
module EvmCrossRuntimeRunnerTez = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.cross_runtime_run_tez

  (** Initialises the contract.  [tez_contract_target_address] is the
   *  KT1 address that will be called on [run()]. *)
  let init ~sequencer ~sender ~nonce ~value ~tez_contract_target_address
      cross_runtime_run_tez =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:cross_runtime_run_tez
        ~abi_signature:"initialize(string)"
        ~arguments:[tez_contract_target_address]
        ()
    in
    unit

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sequencer ~expected_counter cross_runtime_run_tez =
    let counter_storage_pos = "0x00" in
    let*@ evm_count =
      Rpc.get_storage_at
        ~address:cross_runtime_run_tez
        ~pos:counter_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmCrossRuntimeRunnerTez `count` %R but got %L") ;
    unit
end

(** EVM contract that exercises the generic [call(url, headers, body, method)]
 *  precompile to reach a Tezos target.  Increments [count] before and after
 *  the cross-runtime call.  [runCatch()] wraps the call in try/catch. *)
module EvmCracHttpCall = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.crac_http_call

  let init ~sequencer ~sender ~nonce ~value ~tez_contract_target_address
      contract =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"initialize(string)"
        ~arguments:[tez_contract_target_address]
        ()
    in
    unit

  let call_run_catch ?expected_status ~sequencer ~sender ~nonce ~value contract
      =
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"runCatch()"
        ~arguments:[]
        ?expected_status
        ()
    in
    return receipt.gasUsed

  let call_run_catch_with_gas_limit ?expected_status ~sequencer ~sender ~nonce
      ~value ~gas_limit contract =
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"runCatchWithGasLimit(uint256)"
        ~arguments:[string_of_int gas_limit]
        ?expected_status
        ()
    in
    return receipt.gasUsed

  let check_storage ~sequencer ?(expected_catches = 0) ~expected_counter
      contract =
    let count_storage_pos = "0x00" in
    let*@ evm_count =
      Rpc.get_storage_at ~address:contract ~pos:count_storage_pos sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmCracHttpCall `count` %R but got %L") ;
    let catches_storage_pos = "0x01" in
    let*@ evm_catches =
      Rpc.get_storage_at ~address:contract ~pos:catches_storage_pos sequencer
    in
    Check.(
      (int_of_string evm_catches = expected_catches)
        int
        ~error_msg:"Expected EvmCracHttpCall `catches` %R but got %L") ;
    unit
end

(** EVM contract that calls a Michelson contract via the generic [call]
 *  precompile and stores the returned HTTP response body.  Used to
 *  verify that [%collect_result] bytes deposited on the Michelson side
 *  surface as the EVM precompile's return value. *)
module EvmCollectResult = struct
  open EvmContract

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.crac_collect_result

  (** Initialises the contract.  [tez_contract_target_address] is the
   *  KT1 address that will be called on [run()]. *)
  let init ~sequencer ~sender ~nonce ~value ~tez_contract_target_address
      contract =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"initialize(string)"
        ~arguments:[tez_contract_target_address]
        ()
    in
    unit

  let call_run ?expected_status ~sequencer ~sender ~nonce ~value contract =
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"run()"
        ~arguments:[]
        ?expected_status
        ()
    in
    return receipt.gasUsed

  (** Call [runCatch()] which wraps the precompile call in try/catch so
   *  the outer tx commits even when the CRAC call reverts. *)
  let call_run_catch ?expected_status ~sequencer ~sender ~nonce ~value contract
      =
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"runCatch()"
        ~arguments:[]
        ?expected_status
        ()
    in
    return receipt.gasUsed

  (** Assert whether the precompile call reverted, as recorded by
   *  [runCatch()]. *)
  let check_caught ~sequencer ~expected contract =
    let caught_storage_pos = "0x02" in
    let*@ caught =
      Rpc.get_storage_at ~address:contract ~pos:caught_storage_pos sequencer
    in
    let bit = int_of_string caught in
    Check.(
      (bit = if expected then 1 else 0)
        int
        ~error_msg:"Expected EvmCollectResult `caught` %R but got %L") ;
    unit

  (** Assert that the contract's [result] getter returns bytes whose hex
   *  representation equals [expected_hex] (lowercase, no [0x] prefix). *)
  let check_result ~sequencer ~expected_hex contract =
    let endpoint = Evm_node.endpoint sequencer in
    (* [cast call <addr> "result()(bytes)"] returns the decoded bytes
       as [0x<hex>] — no manual ABI offset/length parsing needed. *)
    let* raw = Cast.call "result()(bytes)" ~endpoint ~address:contract in
    let strip_0x s = Test_helpers.remove_0x s in
    Check.(
      (String.lowercase_ascii (strip_0x raw)
      = String.lowercase_ascii expected_hex)
        string
        ~error_msg:"Expected result bytes %R but got %L") ;
    unit
end

(** EVM contract that iterates [run()] over its [callees].  Before
 *  each call, increments [counter].  If a callee's revert is caught,
 *  increments [catches] instead of propagating.  After all calls,
 *  reverts if initialised to do so, otherwise increments [counter]. *)
module EvmMultiRunCaller = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.multi_run_caller

  (** Initialises the contract.  [callees] is the list of contracts
   *  that will be called on [run()], together with whether their
   *  revert should be caught.  [revert] controls whether the contract
   *  reverts after all calls. *)
  let init ~sequencer ~sender ~nonce ~value ~revert ~callees multi_run_caller =
    let callees_arg =
      let pp_comma fmt () = Format.fprintf fmt "," in
      let pp_callee fmt (addr, do_catch) =
        Format.fprintf fmt "(%s,%b)" addr do_catch
      in
      Format.asprintf
        "[%a]"
        (Format.pp_print_list ~pp_sep:pp_comma pp_callee)
        callees
    in
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:multi_run_caller
        ~abi_signature:"initialize(bool,(address,bool)[])"
        ~arguments:[string_of_bool revert; callees_arg]
        ()
    in
    unit

  (** Asserts that the contract's [catches] and [counter] equal the
   *  expected values. *)
  let check_storage ~sequencer ?(expected_catches = 0) ~expected_counter
      multi_run_caller =
    let catches_storage_pos = "0x00" in
    let*@ evm_catches =
      Rpc.get_storage_at
        ~address:multi_run_caller
        ~pos:catches_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_catches = expected_catches)
        int
        ~error_msg:"Expected EvmMultiRunCaller `catches` %R but got %L") ;
    let counter_storage_pos = "0x01" in
    let*@ evm_count =
      Rpc.get_storage_at
        ~address:multi_run_caller
        ~pos:counter_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmMultiRunCaller `count` %R but got %L") ;
    unit
end

(** EVM contract that burns a large, predictable amount of gas when
 *  [run()] is called.  Used as a leaf node in gas-model tests to
 *  make callee gas consumption clearly observable. *)
module EvmGasBurner = struct
  open EvmContract
  include EvmRunner

  let deploy = deploy_solidity_contract ~contract:Solidity_contracts.gas_burner
end

(** Common [%run] caller for Tezos contracts. *)
module TezRunner = struct
  open TezContract

  (** Calls [%run] with [Unit] via the delayed inbox. *)
  let call_run ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ?amount ?gas_limit runner =
    call_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ?amount
      ?gas_limit
      ~dest:runner
      ~arg_data:"Unit"
      ~entrypoint:"run"
      ()
end

(** Tezos contract that forwards [%run] to an EVM target via the
 *  CRAC gateway.  Increments [counter] before and after the
 *  cross-runtime call. *)
module TezCrossRuntimeRunnerEvm = struct
  open TezContract
  include TezRunner

  (** Originates the contract.  [evm_contract_target_address] is the
   *  EVM address that will be called on [%run]. *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance
      ~evm_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_run_evm"] in
    let init_storage_data = sf {|Pair 0 "%s"|} evm_contract_target_address in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sc_rollup_node ~expected_counter
      (cross_runtime_run_tez_hex, _cross_runtime_run_tez_address) =
    let* storage = get_storage ~sc_rollup_node cross_runtime_run_tez_hex in
    (* Pair(nat %count, string %destination) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCrossRuntimeRunnerEvm `count` %R but got %L") ;
    unit
end

let multi_run_caller_init_storage ~revert ~callees =
  let pp_bool fmt b =
    if b then Format.fprintf fmt "True" else Format.fprintf fmt "False"
  in
  let pp_semicolon fmt () = Format.fprintf fmt ";" in
  let pp_callee fmt = Format.fprintf fmt "%S" in
  Format.asprintf
    {|Pair 0 (Pair %a {%a})|}
    pp_bool
    revert
    (Format.pp_print_list ~pp_sep:pp_semicolon pp_callee)
    callees

(** Tezos contract that forwards [%run] to an EVM target via the
 *  CRAC gateway's [%call] (HTTP) entrypoint.  Functionally identical
 *  to {!TezCrossRuntimeRunnerEvm} but exercises the [%call] code path
 *  which has a separate gas accounting path (extract_gas_consumed +
 *  cast_and_consume_milligas on master). *)
module TezCrossRuntimeHttpCallEvm = struct
  open TezContract
  include TezRunner

  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance
      ~evm_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_http_call_evm"] in
    let init_storage_data = sf {|Pair 0 "%s"|} evm_contract_target_address in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  let check_storage ~sc_rollup_node ~expected_counter
      (contract_hex, _contract_address) =
    let* storage = get_storage ~sc_rollup_node contract_hex in
    (* Pair(nat %count, string %destination) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCrossRuntimeHttpCallEvm `count` %R but got %L") ;
    unit
end

(** Tezos contract that iterates [%run] over its [callees].  Before
 *  each call, increments [counter].  After all calls, reverts if
 *  initialised to do so, otherwise increments [counter]. *)
module TezMultiRunCaller = struct
  open TezContract
  include TezRunner

  (** Originates the contract.  [callees] is the list of contracts
   *  that will be called on [%run].  [revert] controls whether the
   *  contract reverts after all calls. *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance ~revert ~callees () =
    let script_name = ["mini_scenarios"; "multi_run_caller"] in
    let init_storage_data = multi_run_caller_init_storage ~revert ~callees in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sc_rollup_node ~expected_counter
      (multi_run_caller_hex, _multi_run_caller_address) =
    let* storage = get_storage ~sc_rollup_node multi_run_caller_hex in
    (* Pair(int %counter, Pair(bool %willRevert, list address %callees)) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezMultiRunCaller `count` %R but got %L") ;
    unit
end

(** Tezos contract that burns all available gas by looping forever.
 *  Exposes [%run] for compatibility with the CRAC runner framework. *)
module TezGasBurner = struct
  open TezContract

  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance () =
    let script_name = ["mini_scenarios"; "gas_burner"] in
    let init_storage_data = "Unit" in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** L1 counterpart of {!TezMultiRunCaller}: originates
    [multi_run_caller] contracts directly on the L1 node via
    [Client.originate_contract_at]. *)
module L1TezMultiRunCaller = struct
  let originate ~client ~node ~protocol ~alias_counter ~revert ~callees () =
    let alias =
      let n = !alias_counter in
      incr alias_counter ;
      sf "mrc_%d" n
    in
    let init = multi_run_caller_init_storage ~revert ~callees in
    let* _alias, addr =
      Client.originate_contract_at
        ~alias
        ~amount:Tez.zero
        ~src:Constant.bootstrap2.alias
        ~init
        ~burn_cap:Tez.one
        client
        ["mini_scenarios"; "multi_run_caller"]
        protocol
    in
    let* () = Client.bake_for_and_wait ~node client in
    return addr
end

(** L1 counterpart of {!TezRunner}: calls [%run] directly on the L1
    node via [Client.transfer]. *)
module L1TezRunner = struct
  let call_run ~client ~node addr =
    let* () =
      Client.transfer
        ~burn_cap:Tez.one
        ~fee:Tez.one
        ~amount:Tez.zero
        ~gas_limit:1_000_000
        ~storage_limit:10_000
        ~giver:Constant.bootstrap2.alias
        ~receiver:addr
        ~entrypoint:"run"
        ~arg:"Unit"
        ~force:true
        client
    in
    Client.bake_for_and_wait ~node client
end

let fetch_l1_manager_ops client =
  let* ops =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations ~force_metadata:true ()
  in
  let manager_ops = JSON.(ops |=> 3 |> as_list) in
  match List.rev manager_ops with
  | [] -> Test.fail "fetch_l1_manager_ops: no manager operations in block"
  | last_op :: _ -> (
      match List.rev JSON.(last_op |-> "contents" |> as_list) with
      | [] ->
          Test.fail "fetch_l1_manager_ops: manager operation has no contents"
      | top :: _ -> return top)

(** EVM contract that stores a uint256 and returns it.  Used as a
 *  target for gateway calls in callback tests. *)
module EvmStoreAndReturn = struct
  open EvmContract

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.store_and_return

  (** Asserts that the contract's stored value equals [expected_value]. *)
  let check_storage ~sequencer ~expected_value store_and_return =
    let*@ storage =
      Rpc.get_storage_at ~address:store_and_return ~pos:"0x0" sequencer
    in
    Check.(
      (int_of_string storage = expected_value)
        int
        ~error_msg:"Expected StoreAndReturn value %R but got %L") ;
    unit
end

(** Michelson-to-EVM bridge with callback for CRAC test scenarios.
 *  On [%run], calls the gateway's [%call_evm] with
 *  [Some(SELF %on_result)] as callback.  [%on_result] stores the
 *  received bytes and increments the counter.
 *  When [failing] is true, originates [failing_callback_run_evm.tz]
 *  whose [%on_result] always FAILWITHs. *)
module TezCallbackRunnerEvm = struct
  open TezContract
  include TezRunner

  (** Originates the contract.  [evm_contract_target_address] is the
   *  EVM address that will be called on [%run]. *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance ~failing
      ~evm_contract_target_address ~method_sig ~abi_params () =
    let script_name =
      if failing then ["mini_scenarios"; "failing_callback_run_evm"]
      else ["mini_scenarios"; "callback_run_evm"]
    in
    let init_storage_data =
      sf
        {|Pair 0 (Pair "%s" (Pair "%s" (Pair 0x%s None)))|}
        evm_contract_target_address
        method_sig
        abi_params
    in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_counter ~sc_rollup_node ~expected_counter (hex, _addr) =
    let* storage = get_storage ~sc_rollup_node hex in
    (* Pair(nat %count, Pair(string %dest, ...)) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCallbackRunnerEvm `count` %R but got %L") ;
    unit

  (** Asserts that the callback result field matches [expected_bytes].
   *  [None] means the result should be [None] (callback did not fire
   *  or was reverted).  [Some hex] means the result should be
   *  [Some <hex>]. *)
  let check_result ~sc_rollup_node ~expected_bytes (hex, _addr) =
    let* storage = get_storage ~sc_rollup_node hex in
    (* Storage: Pair(count, Pair(dest, Pair(sig, Pair(params, result)))) *)
    let result_node =
      JSON.(
        storage |-> "args" |=> 1 |-> "args" |=> 1 |-> "args" |=> 1 |-> "args"
        |=> 1)
    in
    (match expected_bytes with
    | None ->
        Check.(
          (JSON.(result_node |-> "prim" |> as_string) = "None")
            string
            ~error_msg:"Expected result None but got %L")
    | Some bytes ->
        Check.(
          (JSON.(result_node |-> "prim" |> as_string) = "Some")
            string
            ~error_msg:"Expected result Some but got %L") ;
        let actual =
          JSON.(result_node |-> "args" |=> 0 |-> "bytes" |> as_string)
        in
        Check.(
          (actual = bytes)
            string
            ~error_msg:"Expected result bytes %R but got %L")) ;
    unit
end

(** Tezos contract that burns significant gas (SHA256 loop) and emits
 *  an internal operation (self-call to [%_noop]).  The internal
 *  operation triggers the [get_and_reset_milligas_consumed()] baseline
 *  reset that caused the [X-Tezos-Gas-Consumed] header bug.
 *
 *  Unlike {!TezGasBurner} (infinite loop, always OOGs), this contract
 *  terminates normally after burning a predictable amount of gas. *)
module TezMichelsonGasBurner = struct
  open TezContract

  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance () =
    let script_name = ["mini_scenarios"; "gas_burner_with_internal_op"] in
    let init_storage_data = "Unit" in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Enshrined Michelson gateway contract — called by Michelson contracts
    to initiate outgoing CRACs to EVM. *)
let gateway_address = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw"

(** Direct gateway calls (fire-and-forget with None callback). *)
module Gateway = struct
  open TezContract

  (** Calls the gateway's [%call_evm] with [None] callback. *)
  let call_evm ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~evm_target ~method_sig ~abi_params
      ?(amount = 0) () =
    let arg_data =
      sf
        {|Pair "%s" (Pair "%s" (Pair 0x%s None))|}
        evm_target
        method_sig
        abi_params
    in
    call_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~dest:gateway_address
      ~arg_data
      ~entrypoint:"call_evm"
      ~amount
      ~gas_limit:200_000
      ()
end

(** Tezos contract whose [%default] entrypoint deposits a fixed bytes
 *  payload into the current CRAC frame by calling the gateway's
 *  [%collect_result] entrypoint.  Used to verify that the payload is
 *  surfaced as the Michelson server's HTTP response body. *)
module TezCollectResult = struct
  open TezContract

  (** Originates [gateway_collect_result.tz] with storage
   *  [Pair gateway_address 0x<payload_hex>].  [payload_hex] is the
   *  bytes payload deposited via [%collect_result] (no [0x] prefix). *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance ~payload_hex () =
    let script_name = ["mini_scenarios"; "gateway_collect_result"] in
    let init_storage_data = sf {|Pair "%s" 0x%s|} gateway_address payload_hex in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Tezos contract that FAILWITHs on any call (unit parameter). *)
module TezAlwaysFailsUnit = struct
  open TezContract

  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance () =
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "always_fails_unit"]
      ~init_storage_data:"Unit"
      ?init_balance
      protocol
end

(** Tezos contract that emits two internal operations:  a call to the
 *  gateway's [%collect_result] with a fixed payload, and a call to a
 *  failing contract.  Used to verify that bytes deposited by the first
 *  internal op do not leak as the HTTP response body when a later
 *  internal op triggers a 4xx. *)
module TezCollectResultThenFail = struct
  open TezContract

  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance ~failing_kt1
      ~payload_hex () =
    let script_name = ["mini_scenarios"; "gateway_collect_result_then_fail"] in
    let init_storage_data =
      sf {|Pair "%s" (Pair "%s" 0x%s)|} gateway_address failing_kt1 payload_hex
    in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Wraps the CRAC runner contract modules into a first-class module
 *  that manages the shared test state (nonces, counters, sequencer
 *  setup) so that tests only provide scenario-specific parameters.
 *  Contracts are represented as {!evm_runner} and {!tez_runner}
 *  values to distinguish EVM and Tezos contracts at the type level. *)
module CracRunnerWrapper = struct
  type evm_runner = [`Evm_runner of string]

  type tez_runner = [`Tez_runner of string * string]

  (** Simplified interface over the CRAC runner contracts.  Each
   *  sub-module mirrors its namesake but with nonces, counters and
   *  sequencer setup already applied. *)
  module type S = sig
    val sequencer : Evm_node.t

    val sc_rollup_node : Sc_rollup_node.t

    val client : Client.t

    val sender : Eth_account.t

    val source : Account.key

    val evm_nonce : unit -> int

    val tez_counter : unit -> int

    module EvmRunner : sig
      val call_run :
        ?expected_status:bool ->
        ?value:Wei.t ->
        ?access_list:(string * string list) list ->
        ?gas:int ->
        evm_runner ->
        int64 Lwt.t
    end

    module EvmCrossRuntimeRunnerTez : sig
      val deploy_and_init : ?value:Wei.t -> tez_runner -> evm_runner Lwt.t

      val check_storage : expected_counter:int -> evm_runner -> unit Lwt.t
    end

    module EvmCracHttpCall : sig
      val deploy_and_init : ?value:Wei.t -> tez_runner -> evm_runner Lwt.t

      val call_run_catch :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val call_run_catch_with_gas_limit :
        ?expected_status:bool ->
        ?value:Wei.t ->
        gas_limit:int ->
        evm_runner ->
        int64 Lwt.t

      val check_storage :
        ?expected_catches:int ->
        expected_counter:int ->
        evm_runner ->
        unit Lwt.t
    end

    module EvmCollectResult : sig
      val deploy_and_init : ?value:Wei.t -> tez_runner -> evm_runner Lwt.t

      val call_run :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val call_run_catch :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val check_result : expected_hex:string -> evm_runner -> unit Lwt.t

      val check_caught : expected:bool -> evm_runner -> unit Lwt.t
    end

    module EvmMultiRunCaller : sig
      val deploy_and_init :
        ?value:Wei.t ->
        ?revert:bool ->
        ?callees:(evm_runner * bool) list ->
        unit ->
        evm_runner Lwt.t

      val check_storage :
        ?expected_catches:int ->
        expected_counter:int ->
        evm_runner ->
        unit Lwt.t
    end

    module EvmGasBurner : sig
      val deploy : unit -> evm_runner Lwt.t
    end

    module TezRunner : sig
      val call_run : ?amount:int -> ?gas_limit:int -> tez_runner -> unit Lwt.t

      val get_consumed_milligas : ?block:string -> unit -> int Lwt.t

      val get_gateway_consumed_milligas :
        ?block:string -> ?entrypoint:string -> unit -> int Lwt.t
    end

    module TezCrossRuntimeRunnerEvm : sig
      val originate : ?init_balance:int -> evm_runner -> tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end

    module TezCrossRuntimeHttpCallEvm : sig
      val originate : ?init_balance:int -> evm_runner -> tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end

    module TezMultiRunCaller : sig
      val originate :
        ?init_balance:int ->
        ?revert:bool ->
        ?callees:tez_runner list ->
        unit ->
        tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end

    module TezCollectResult : sig
      val originate :
        ?init_balance:int -> payload_hex:string -> unit -> tez_runner Lwt.t
    end

    module TezAlwaysFailsUnit : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    module TezCollectResultThenFail : sig
      val originate :
        ?init_balance:int ->
        failing:tez_runner ->
        payload_hex:string ->
        unit ->
        tez_runner Lwt.t
    end

    module TezGasBurner : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    (** Deploy EVM runner + TEZ bridge + TEZ runner in one call. *)
    val setup_crac_pipeline : unit -> (evm_runner * tez_runner) Lwt.t

    (** Inject a TEZ→EVM CRAC via the Tezlink RPC without producing a
        block.  The CRAC calls the [run] entrypoint on [tez_runner]. *)
    val inject_crac_no_block : tez_runner -> unit Lwt.t

    (** Send a simple EVM transfer via [send_raw_transaction] without
        producing a block.  Returns the tx hash. *)
    val send_evm_transfer_no_block :
      ?value:Wei.t -> address:string -> unit -> string Lwt.t

    module EvmStoreAndReturn : sig
      val deploy : unit -> evm_runner Lwt.t

      val check_storage : expected_value:int -> evm_runner -> unit Lwt.t
    end

    module TezCallbackRunnerEvm : sig
      val originate :
        ?failing:bool ->
        method_sig:string ->
        abi_params:string ->
        evm_runner ->
        tez_runner Lwt.t

      val check_counter : expected_counter:int -> tez_runner -> unit Lwt.t

      val check_result :
        expected_bytes:string option -> tez_runner -> unit Lwt.t
    end

    module Gateway : sig
      val call_evm :
        evm_target:evm_runner ->
        method_sig:string ->
        abi_params:string ->
        ?amount:int ->
        unit ->
        unit Lwt.t
    end

    module TezMichelsonGasBurner : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end
  end

  (** Builds a {!S} module from the given test setup.  [nonce] and
   *  [counter] are the initial EVM nonce and Tezos operation counter;
   *  both are auto-incremented on each use. *)
  let build ?(nonce = 0) ?(counter = 0)
      ~sequencer_setup:
        ({
           sc_rollup_address;
           sc_rollup_node;
           client;
           l1_contracts;
           sequencer;
           evm_version;
           _;
         } :
          Tezt_etherlink.Setup.sequencer_setup) ~sender ~source protocol :
      (module S) =
    let ref_nonce = ref nonce in
    let ref_counter = ref counter in

    let evm_nonce () =
      let nonce = !ref_nonce in
      incr ref_nonce ;
      nonce
    in
    let tez_counter () =
      let counter = !ref_counter in
      incr ref_counter ;
      counter
    in
    let module Helper = struct
      let sequencer = sequencer

      let sc_rollup_node = sc_rollup_node

      let client = client

      let sender = sender

      let source = source

      let sc_rollup_address = sc_rollup_address

      let l1_contracts = l1_contracts

      let evm_nonce = evm_nonce

      let tez_counter = tez_counter

      module EvmRunner = struct
        let call_run ?expected_status ?(value = Wei.zero) ?access_list ?gas
            (`Evm_runner runner) =
          let* gas_used =
            EvmRunner.call_run
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ?access_list
              ?gas
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used
      end

      module EvmCrossRuntimeRunnerTez = struct
        let deploy_and_init ?(value = Wei.zero) (`Tez_runner (_, address)) =
          let* addr =
            EvmCrossRuntimeRunnerTez.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCrossRuntimeRunnerTez.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~tez_contract_target_address:address
              addr
          in
          return (`Evm_runner addr)

        let check_storage ~expected_counter (`Evm_runner runner) =
          EvmCrossRuntimeRunnerTez.check_storage
            ~sequencer
            ~expected_counter
            runner
      end

      module EvmCracHttpCall = struct
        let deploy_and_init ?(value = Wei.zero) (`Tez_runner (_, address)) =
          let* addr =
            EvmCracHttpCall.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCracHttpCall.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~tez_contract_target_address:address
              addr
          in
          return (`Evm_runner addr)

        let call_run_catch ?expected_status ?(value = Wei.zero)
            (`Evm_runner runner) =
          let* gas_used =
            EvmCracHttpCall.call_run_catch
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used

        let call_run_catch_with_gas_limit ?expected_status ?(value = Wei.zero)
            ~gas_limit (`Evm_runner runner) =
          let* gas_used =
            EvmCracHttpCall.call_run_catch_with_gas_limit
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~gas_limit
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used

        let check_storage ?expected_catches ~expected_counter
            (`Evm_runner runner) =
          EvmCracHttpCall.check_storage
            ~sequencer
            ?expected_catches
            ~expected_counter
            runner
      end

      module EvmCollectResult = struct
        let deploy_and_init ?(value = Wei.zero) (`Tez_runner (_, address)) =
          let* addr =
            EvmCollectResult.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCollectResult.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~tez_contract_target_address:address
              addr
          in
          return (`Evm_runner addr)

        let call_run ?expected_status ?(value = Wei.zero) (`Evm_runner runner) =
          let* gas_used =
            EvmCollectResult.call_run
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used

        let call_run_catch ?expected_status ?(value = Wei.zero)
            (`Evm_runner runner) =
          let* gas_used =
            EvmCollectResult.call_run_catch
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used

        let check_result ~expected_hex (`Evm_runner runner) =
          EvmCollectResult.check_result ~sequencer ~expected_hex runner

        let check_caught ~expected (`Evm_runner runner) =
          EvmCollectResult.check_caught ~sequencer ~expected runner
      end

      module EvmMultiRunCaller = struct
        let deploy_and_init ?(value = Wei.zero) ?(revert = false)
            ?(callees = []) () =
          let callees =
            List.map
              (fun (`Evm_runner addr, do_catch) -> (addr, do_catch))
              callees
          in
          let* addr =
            EvmMultiRunCaller.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmMultiRunCaller.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~revert
              ~callees
              addr
          in
          return (`Evm_runner addr)

        let check_storage ?expected_catches ~expected_counter
            (`Evm_runner runner) =
          EvmMultiRunCaller.check_storage
            ~sequencer
            ?expected_catches
            ~expected_counter
            runner
      end

      module EvmGasBurner = struct
        let deploy () =
          let* addr =
            EvmGasBurner.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          return (`Evm_runner addr)
      end

      module TezRunner = struct
        let call_run ?amount ?gas_limit (`Tez_runner (_, runner)) =
          TezRunner.call_run
            ~sc_rollup_address
            ~sc_rollup_node
            ~client
            ~l1_contracts
            ~sequencer
            ~source
            ~counter:(tez_counter ())
            ?amount
            ?gas_limit
            runner

        let get_consumed_milligas ?block () =
          TezContract.get_consumed_milligas ?block sequencer

        let get_gateway_consumed_milligas ?block ?entrypoint () =
          TezContract.get_gateway_consumed_milligas ?block ?entrypoint sequencer
      end

      module TezCrossRuntimeRunnerEvm = struct
        let originate ?init_balance (`Evm_runner address) =
          let* contract_hex, address =
            TezCrossRuntimeRunnerEvm.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~evm_contract_target_address:address
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeRunnerEvm.check_storage
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCrossRuntimeHttpCallEvm = struct
        let originate ?init_balance (`Evm_runner address) =
          let* contract_hex, address =
            TezCrossRuntimeHttpCallEvm.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~evm_contract_target_address:address
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeHttpCallEvm.check_storage
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezMultiRunCaller = struct
        let originate ?init_balance ?(revert = false) ?(callees = []) () =
          let callees =
            List.map (fun (`Tez_runner (_runner_hex, runner)) -> runner) callees
          in
          let* runner_hex, runner =
            TezMultiRunCaller.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~revert
              ~callees
              ()
          in
          return (`Tez_runner (runner_hex, runner))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezMultiRunCaller.check_storage
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCollectResult = struct
        let originate ?init_balance ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResult.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~payload_hex
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezAlwaysFailsUnit = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezAlwaysFailsUnit.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezCollectResultThenFail = struct
        let originate ?init_balance ~failing:(`Tez_runner (_, failing_kt1))
            ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResultThenFail.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~failing_kt1
              ~payload_hex
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezGasBurner = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezGasBurner.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      let setup_crac_pipeline () =
        let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
        let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
        let* tez_runner =
          TezMultiRunCaller.originate ~callees:[tez_bridge] ()
        in
        return (evm_runner, tez_runner)

      let inject_crac_no_block (`Tez_runner (_, dest)) =
        let tezlink_endpoint =
          Client.(
            Foreign_endpoint
              Endpoint.
                {
                  (Evm_node.rpc_endpoint_record sequencer) with
                  path = "/tezlink";
                })
        in
        let* client_tezlink = Client.init ~endpoint:tezlink_endpoint () in
        let* arg = Client.convert_data_to_json ~data:"Unit" client in
        let* crac_op =
          Operation.Manager.(
            operation
              [
                make
                  ~fee:1000
                  ~counter:(tez_counter ())
                  ~gas_limit:100_000
                  ~storage_limit:1000
                  ~source
                  (call ~dest ~arg ~entrypoint:"run" ~amount:0 ());
              ])
            client
        in
        let* _crac_hash =
          Operation.inject ~dont_wait:true crac_op client_tezlink
        in
        unit

      let send_evm_transfer_no_block ?(value = Wei.zero) ~address () =
        let* raw_tx =
          Cast.craft_tx
            ~source_private_key:sender.Eth_account.private_key
            ~chain_id:1337
            ~nonce:(evm_nonce ())
            ~gas:21_000
            ~gas_price:1_000_000_000
            ~value
            ~address
            ()
        in
        let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
        return tx_hash

      module EvmStoreAndReturn = struct
        let deploy () =
          let* addr =
            EvmStoreAndReturn.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          return (`Evm_runner addr)

        let check_storage ~expected_value (`Evm_runner runner) =
          EvmStoreAndReturn.check_storage ~sequencer ~expected_value runner
      end

      module TezCallbackRunnerEvm = struct
        let originate ?(failing = false) ~method_sig ~abi_params
            (`Evm_runner address) =
          let* contract_hex, address =
            TezCallbackRunnerEvm.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ~failing
              ~evm_contract_target_address:address
              ~method_sig
              ~abi_params
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_counter ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCallbackRunnerEvm.check_counter
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)

        let check_result ~expected_bytes
            (`Tez_runner (runner_hex, runner_address)) =
          TezCallbackRunnerEvm.check_result
            ~sc_rollup_node
            ~expected_bytes
            (runner_hex, runner_address)
      end

      module Gateway = struct
        let call_evm ~evm_target:(`Evm_runner address) ~method_sig ~abi_params
            ?amount () =
          Gateway.call_evm
            ~sc_rollup_address
            ~sc_rollup_node
            ~client
            ~l1_contracts
            ~sequencer
            ~source
            ~counter:(tez_counter ())
            ~evm_target:address
            ~method_sig
            ~abi_params
            ?amount
            ()
      end

      module TezMichelsonGasBurner = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezMichelsonGasBurner.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end
    end in
    (module Helper)
end

(** Registers a fullstack CRAC runner test.  Sets up the sequencer,
 *  builds a {!CracRunnerWrapper.S} and passes it to [body]. *)
let register_crac_runner_test ~title ?(tags = []) body =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"]
    @ List.map Tezosx_runtime.tag with_runtimes
    @ ["crac"; "runner"] @ tags
  in
  Setup.register_test
    ~__FILE__
    ~rpc_server:Evm_node.Resto
    ~title
    ~time_between_blocks:Nothing
    ~tags
    ~kernel:Latest
    ~with_runtimes
    ~enable_dal:false
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer_setup protocol ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let source = Constant.bootstrap5 in
  let (module Wrapper) =
    CracRunnerWrapper.build ~counter:1 ~sequencer_setup ~sender ~source protocol
  in
  body (module Wrapper : CracRunnerWrapper.S)

(** Simple EVM-to-TEZ cross-runtime call.
 *
 *    EVM[evm_runner]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *
 *)
let test_crac_evm_to_tez =
  register_crac_runner_test ~title:"CRAC: EVM runner calls TEZ runner"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner calling the bridge" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM runner" ;
  let* _ = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  unit

(** Multiple independent CRAC crossings.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *
 *)
let test_crac_evm_multiple_independent_crossings =
  register_crac_runner_test ~title:"CRAC: multiple independent crossings"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_2
  in
  unit

(** Double CRAC crossing: EVM calls EVM inner both directly and through
 *  an EVM->TEZ->EVM round-trip bridge chain.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_inner]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *     |-> EVM[evm_inner]
 *
 *)
let test_crac_evm_double_crossing =
  register_crac_runner_test
    ~title:"CRAC: double crossing EVM via TEZ back to EVM"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM inner runner" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM inner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  Log.debug ~prefix "Deploy EVM bridge to TEZ bridge" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Deploy EVM main calling inner directly and via bridges" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_inner, false); (evm_bridge, false); (evm_inner, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_inner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:4 evm_main in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Shared TEZ leaf called three times: twice directly via the same EVM
 *  bridge, and once through a 3-CRAC chain that also ends at the same leaf.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_direct] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_chain] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_direct] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_evm_shared_leaf_via_direct_and_chain =
  register_crac_runner_test
    ~title:"CRAC: EVM shared TEZ leaf via direct bridge and 3-CRAC chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge_direct to TEZ leaf" ;
  let* evm_bridge_direct = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM bridge_inner to TEZ leaf (for chain)" ;
  let* evm_bridge_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Originate TEZ bridge to EVM bridge_inner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge_inner in
  Log.debug ~prefix "Deploy EVM bridge_chain to TEZ bridge" ;
  let* evm_bridge_chain = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [
          (evm_bridge_direct, false);
          (evm_bridge_chain, false);
          (evm_bridge_direct, false);
        ]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:4 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge_direct
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_chain
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_inner
  in
  unit

(** EVM 5-crossing chain.
 *
 *    EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_evm_5_crossing_chain =
  register_crac_runner_test ~title:"CRAC: EVM 5-crossing chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Call EVM a" ;
  let* _ = EvmRunner.call_run evm_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_e in
  unit

(** Simple TEZ-to-EVM cross-runtime call.
 *
 *     TEZ[tez_runner]
 *      |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *
 *)
let test_crac_tez_to_evm =
  register_crac_runner_test ~title:"CRAC: TEZ runner calls EVM runner"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ runner" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Multiple independent TEZ-to-EVM crossings from a single TEZ caller.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *
 *)
let test_crac_tez_multiple_independent_crossings =
  register_crac_runner_test
    ~title:"CRAC: TEZ multiple independent crossings to EVM"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_2
  in
  unit

(** TEZ-to-EVM-to-TEZ double crossing.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_inner]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *     |-> TEZ[tez_inner]
 *
 *)
let test_crac_tez_double_crossing =
  register_crac_runner_test ~title:"CRAC: TEZ-to-EVM-to-TEZ double crossing"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ inner runner" ;
  let* tez_inner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ inner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  Log.debug ~prefix "Originate TEZ bridge from EVM bridge" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_inner; tez_bridge; tez_inner] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_inner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Shared EVM leaf called three times: twice directly via the same TEZ
 *  bridge, and once through a 3-CRAC chain that also ends at the same leaf.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_direct] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_chain] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_direct] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_shared_leaf_via_direct_and_chain =
  register_crac_runner_test
    ~title:"CRAC: TEZ shared EVM leaf via direct bridge and 3-CRAC chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge_direct to EVM leaf" ;
  let* tez_bridge_direct = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ bridge_inner to EVM leaf (for chain)" ;
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM bridge to TEZ bridge_inner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner in
  Log.debug ~prefix "Originate TEZ bridge_chain to EVM bridge" ;
  let* tez_bridge_chain = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_direct; tez_bridge_chain; tez_bridge_direct]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_direct
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_chain
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_inner
  in
  unit

(** TEZ 5-crossing chain.
 *
 *    TEZ[tez_a] ~CRAC~> EVM[evm_b] ~CRAC~> TEZ[tez_c] ~CRAC~> EVM[evm_d] ~CRAC~> TEZ[tez_e] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_5_crossing_chain =
  register_crac_runner_test ~title:"CRAC: TEZ 5-crossing chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Call TEZ a" ;
  (* The default gas_limit (10_000) is too low for 5 crossings: each
     round-trip costs ~50K EVM gas (~5_000 gas units), exhausting the
     budget after ~2 crossings. 20_000 units (= 200K EVM gas) provides
     enough headroom even with correct cross-runtime gas charge-back. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_leaf in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_e in
  unit

(** Access list preserved across EVM->TEZ->EVM CRAC.
 *
 *  Deploys one double-crossing chain:
 *
 *    EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *
 *  Calls [run()] twice on the same chain: first without an access list,
 *  then with one pre-warming [evm_inner]'s counter storage slot.
 *  The second call should use strictly less gas.
 *)
let test_crac_access_list_preserved =
  register_crac_runner_test
    ~title:"CRAC: access list preserved across EVM->TEZ->EVM"
    ~tags:["access_list"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-AL" in
  Log.debug ~prefix "Deploy chain" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  (* Call 1: without access list *)
  Log.debug ~prefix "Call run() without access list" ;
  let* gas_without = EvmRunner.call_run evm_bridge in
  (* Call 2: with access list pre-warming evm_inner's counter slot *)
  let (`Evm_runner evm_inner_addr) = evm_inner in
  let access_list =
    [
      ( evm_inner_addr,
        ["0x0000000000000000000000000000000000000000000000000000000000000001"]
      );
    ]
  in
  Log.debug ~prefix "Call run() with access list" ;
  let* gas_with = EvmRunner.call_run ~access_list evm_bridge in
  (* The access list warms evm_inner's storage slot before the
     EVM->TEZ->EVM round-trip, so the 3rd EVM execution (evm_inner
     after the round-trip) pays warm-access cost instead of cold. *)
  Log.info "Gas with access list: %Ld, gas without: %Ld" gas_with gas_without ;
  Check.(
    (gas_with < gas_without)
      int64
      ~error_msg:
        "Expected gas with access list (%L) to be strictly less than without \
         (%R)") ;
  (* Verify correctness: counters incremented twice (once per call) *)
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_inner in
  unit

(** TEZ CRAC target reverts.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_evm_to_tez_reverts =
  register_crac_runner_test ~title:"CRAC: EVM to TEZ reverts" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ reverter" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** EVM CRAC target reverts.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_tez_to_evm_reverts =
  register_crac_runner_test ~title:"CRAC: TEZ to EVM reverts" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM reverter" ;
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridge to EVM reverter" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  Log.debug ~prefix "Originate TEZ main caller" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main (BackTracked due to EVM revert)" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters are all zero (state reverted)" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** CRAC: when a Michelson transaction makes a single CRAC into EVM,
 *  the EVM block must contain a fake transaction carrying the logs
 *  emitted during the cross-runtime EVM execution.
 *
 *     TEZ[tez_runner]
 *      |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *
 *  After the Michelson transaction, we fetch the latest EVM block and
 *  assert it contains at least one transaction (the CRAC envelope).
 *)
let test_crac_tez_to_evm_fake_tx_in_block =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM produces fake EVM transaction in block"
    ~tags:["crac_tx"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-TX" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ runner (triggers CRAC into EVM)" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Fetch latest EVM block" ;
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_count =
    match block.transactions with
    | Block.Hash txs -> List.length txs
    | Block.Full txs -> List.length txs
    | Block.Empty -> 0
  in
  Log.info "EVM block %ld contains %d transaction(s)" block.number tx_count ;
  Check.(
    (tx_count >= 1)
      int
      ~error_msg:
        "Expected at least 1 transaction in the EVM block (the CRAC envelope), \
         but found %L") ;
  unit

(** Two separate TEZ->EVM CRAC calls in separate blocks must produce fake
 *  transactions with distinct hashes.  Regression test for a bug where the
 *  CRAC transaction hash was computed from the block-local transaction index
 *  only (without the block number), causing a UNIQUE constraint violation in
 *  the EVM node's transactions table on the second insertion.
 *)
let test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM fake transactions have unique hashes across blocks"
    ~tags:["crac_tx"; "regression"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HASH" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "First CRAC call (TEZ->EVM)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Second CRAC call (TEZ->EVM) — must not crash the node" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  Log.debug ~prefix "Both CRAC fake transactions stored successfully" ;
  unit

(** debug_traceTransaction on a CRAC fake transaction (TEZ→EVM).
 *
 *  Sets up a TEZ→EVM CRAC, fetches the fake tx hash from the EVM block,
 *  and calls debug_traceTransaction with both callTracer and structLogger.
 *  Verifies that the kernel produces a valid trace during Blueprint replay.
 *)
let test_crac_debug_trace_transaction =
  register_crac_runner_test
    ~title:"CRAC: debug_traceTransaction on TEZ->EVM fake tx"
    ~tags:["crac_tx"; "trace"; "crac_trace"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "TRACE-CRAC" in
  Log.debug ~prefix "Setup CRAC pipeline" ;
  let* evm_runner, tez_runner = setup_crac_pipeline () in
  Log.debug ~prefix "Call TEZ runner (triggers CRAC into EVM)" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Verify CRAC completed" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Fetch latest EVM block to get the CRAC fake tx hash" ;
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let crac_tx_hash =
    match block.transactions with
    | Block.Hash (h :: _) -> h
    | _ -> Test.fail "Expected at least one tx hash in EVM block"
  in
  Log.info "CRAC fake tx hash: %s" crac_tx_hash ;
  (* Test 1: callTracer *)
  Log.debug ~prefix "debug_traceTransaction with callTracer" ;
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash:crac_tx_hash
      ~tracer:"callTracer"
      sequencer
  in
  (match trace_result with
  | Ok t ->
      (* The trace should have a "type" field (top-level call type) *)
      let type_ = JSON.(t |-> "type" |> as_string) in
      Check.(
        (type_ = "CALL")
          string
          ~error_msg:"Expected CALL type for CRAC trace, got %L")
  | Error err ->
      Test.fail
        "debug_traceTransaction (callTracer) failed on CRAC fake tx: %s"
        err.Rpc.message) ;
  (* Test 2: structLogger (default tracer) *)
  Log.debug ~prefix "debug_traceTransaction with structLogger" ;
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash:crac_tx_hash
      ~tracer:"structLogger"
      sequencer
  in
  (match trace_result with
  | Ok t ->
      (* structLogger output should have a structLogs array *)
      let struct_logs = JSON.(t |-> "structLogs" |> as_list) in
      Log.info "structLogger produced %d opcode logs" (List.length struct_logs) ;
      (* Verify structLogger fields for CRAC fake tx *)
      let failed = JSON.(t |-> "failed" |> as_bool) in
      Check.(
        (failed = false)
          bool
          ~error_msg:"Expected failed=false for successful CRAC tx, got %L") ;
      let gas = JSON.(t |-> "gas" |> as_int) in
      Check.((gas > 0) int ~error_msg:"Expected positive gas value, got %L") ;
      let return_value = JSON.(t |-> "returnValue" |> as_string) in
      Check.(
        (return_value = "0x")
          string
          ~error_msg:"Expected empty returnValue (0x) for CRAC fake tx, got %L")
  | Error err ->
      Test.fail
        "debug_traceTransaction (structLogger) failed on CRAC fake tx: %s"
        err.Rpc.message) ;
  unit

(** debug_traceBlockByNumber on a mixed block containing BOTH a normal
 *  EVM transaction AND a CRAC fake tx.
 *
 *  Sends a dummy EVM self-transfer and a TEZ→EVM CRAC into the same
 *  block via the Tezlink RPC, then calls debug_traceBlockByNumber and
 *  verifies that traces are returned for both transactions.
 *)
let test_crac_debug_trace_block =
  register_crac_runner_test
    ~title:"CRAC: debug_traceBlockByNumber on mixed block"
    ~tags:["crac_tx"; "trace"; "crac_trace"; "block"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "TRACE-BLK" in
  Log.debug ~prefix "Setup CRAC pipeline" ;
  let* evm_runner, tez_runner = setup_crac_pipeline () in
  (* Step 1: Send a dummy EVM self-transfer to the mempool (no block). *)
  Log.debug ~prefix "Send dummy EVM tx to mempool" ;
  let* _dummy_hash =
    send_evm_transfer_no_block ~address:sender.Eth_account.address ()
  in
  (* Step 2: Inject TEZ→EVM CRAC via Tezlink RPC (no block). *)
  Log.debug ~prefix "Inject CRAC via Tezlink RPC" ;
  let* () = inject_crac_no_block tez_runner in
  (* Step 3: Produce one block containing both transactions. *)
  Log.debug ~prefix "Produce mixed block" ;
  let*@ _block_number = Rpc.produce_block sequencer in
  Log.debug ~prefix "Verify CRAC completed" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Fetch latest EVM block" ;
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_hashes =
    match block.transactions with
    | Block.Hash hashes -> hashes
    | _ -> Test.fail "Expected tx hashes in EVM block"
  in
  let tx_count = List.length tx_hashes in
  Log.info "EVM block contains %d transaction(s)" tx_count ;
  Check.(
    (tx_count = 2)
      int
      ~error_msg:"Expected 2 transactions (1 normal + 1 CRAC) but got %L") ;
  Log.debug ~prefix "debug_traceBlockByNumber with callTracer" ;
  let*@ trace_results =
    Rpc.trace_block ~block:(Number (Int32.to_int block.number)) sequencer
  in
  Check.(
    (List.length trace_results = tx_count)
      int
      ~error_msg:"Expected %R traces but got %L") ;
  (* Verify each trace has a txHash and a CALL result *)
  List.iter
    (fun t ->
      let tx_hash = JSON.(t |-> "txHash" |> as_string) in
      Check.(
        (String.length tx_hash = 66)
          int
          ~error_msg:"Expected a 66-char tx hash but got length %L") ;
      let type_ = JSON.(t |-> "result" |-> "type" |> as_string) in
      Check.(
        (type_ = "CALL")
          string
          ~error_msg:"Expected CALL type in trace result, got %L"))
    trace_results ;
  Log.info
    "Block trace returned %d traces matching %d transactions"
    (List.length trace_results)
    tx_count ;
  unit

(** debug_traceTransaction on a normal EVM tx in a block that also
 *  contains a CRAC fake tx.  Ensures that CRAC presence does not break
 *  tracing of regular transactions in the same block.
 *)
let test_crac_debug_trace_normal_tx_in_crac_block =
  register_crac_runner_test
    ~title:"CRAC: debug_traceTransaction on normal tx in CRAC block"
    ~tags:["crac_tx"; "trace"; "crac_trace"; "regression"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "TRACE-REG" in
  Log.debug ~prefix "Setup CRAC pipeline" ;
  let* evm_runner, tez_runner = setup_crac_pipeline () in
  (* Step 1: Send a normal EVM transfer to the mempool (no block). *)
  Log.debug ~prefix "Send normal EVM transfer to mempool" ;
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* normal_tx_hash = send_evm_transfer_no_block ~value:Wei.one ~address () in
  (* Step 2: Inject TEZ→EVM CRAC via Tezlink RPC (no block). *)
  Log.debug ~prefix "Inject CRAC via Tezlink RPC" ;
  let* () = inject_crac_no_block tez_runner in
  (* Step 3: Produce one block containing both transactions. *)
  Log.debug ~prefix "Produce mixed block" ;
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "debug_traceTransaction on the normal tx" ;
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash:normal_tx_hash
      ~tracer:"callTracer"
      sequencer
  in
  (match trace_result with
  | Ok t ->
      let type_ = JSON.(t |-> "type" |> as_string) in
      Check.(
        (type_ = "CALL")
          string
          ~error_msg:"Expected CALL type for normal tx trace, got %L") ;
      Log.info "Normal tx trace OK in mixed CRAC block"
  | Error err ->
      Test.fail
        "debug_traceTransaction failed on normal tx in CRAC block: %s"
        err.Rpc.message) ;
  unit

(** Helper for the [http_trace*] tests: CRAC exchanges surface as POST
    requests to either [http://tezos/<kt1>/<entrypoint>] (EVM→TEZ) or
    [http://ethereum/<address>] (TEZ→EVM). Assert the trace matches one
    of those shapes and a 2xx response. *)
let check_http_trace_shape trace =
  let url = JSON.(trace |-> "url" |> as_string) in
  let meth = JSON.(trace |-> "method" |> as_string) in
  let status = JSON.(trace |-> "responseStatus" |> as_int) in
  let starts_with prefix =
    String.length url >= String.length prefix
    && String.sub url 0 (String.length prefix) = prefix
  in
  Check.(
    (meth = "POST")
      string
      ~error_msg:"Expected CRAC HTTP trace method %R, got %L") ;
  if not (starts_with "http://tezos/" || starts_with "http://ethereum/") then
    Test.fail
      "Expected CRAC HTTP trace URL to start with http://tezos/ or \
       http://ethereum/, got %s"
      url ;
  Check.(
    (status >= 200)
      int
      ~error_msg:"Expected CRAC HTTP trace status >= 200, got %L") ;
  Check.(
    (status < 300)
      int
      ~error_msg:"Expected CRAC HTTP trace status < 300, got %L")

(** [http_traceTransaction] on a nested CRAC (EVM → TEZ → EVM → TEZ): the
    outer HTTP trace must itself carry the inner CRAC in its [innerTraces]
    field, so a single tx exposes the full call tree. This exercises the
    nested-trace path of the kernel journal as well as the RLP round-trip
    for `inner_traces`. *)
let test_http_trace_nested_crac =
  register_crac_runner_test
    ~title:"CRAC: http_traceTransaction on nested EVM->TEZ->EVM->TEZ"
    ~tags:["http_trace"; "crac_trace"; "nested"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "HTTP-TRACE-NESTED" in
  Log.debug ~prefix "Deploy EVM inner leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy inner EVM bridge to TEZ leaf" ;
  let* evm_bridge_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Originate TEZ bridge to inner EVM bridge" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge_inner in
  Log.debug ~prefix "Deploy outer EVM bridge to TEZ bridge" ;
  let* evm_outer = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Call outer EVM (triggers nested EVM->TEZ->EVM->TEZ CRAC)" ;
  let* _ = EvmRunner.call_run evm_outer in
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_hash =
    match block.transactions with
    | Block.Hash (h :: _) -> h
    | _ -> Test.fail "Expected a tx hash in the latest EVM block"
  in
  Log.debug ~prefix "http_traceTransaction %s" tx_hash ;
  let*@ trace_json = Rpc.Tezosx.http_traceTransaction ~tx_hash sequencer in
  let returned_hash = JSON.(trace_json |-> "txHash" |> as_string) in
  Check.(
    (returned_hash = tx_hash)
      string
      ~error_msg:"http_traceTransaction returned wrong txHash %L, expected %R") ;
  let traces = JSON.(trace_json |-> "traces" |> as_list) in
  Check.(
    (traces <> [])
      (list json)
      ~error_msg:"Expected at least one top-level HTTP trace, got %L") ;
  (* Outer trace: EVM outer bridge -> TEZ bridge. *)
  let outer = List.hd traces in
  check_http_trace_shape outer ;
  (* Nested CRAC: inside the outer TEZ execution there is a second
     EVM -> TEZ hop, which must appear as an entry in [innerTraces]. *)
  let inner_traces = JSON.(outer |-> "innerTraces" |> as_list) in
  Check.(
    (inner_traces <> [])
      (list json)
      ~error_msg:
        "Expected at least one nested HTTP trace under outer.innerTraces, got \
         %L") ;
  List.iter check_http_trace_shape inner_traces ;
  Log.info
    "Nested http_traceTransaction: %d top-level traces, %d inner at depth 1"
    (List.length traces)
    (List.length inner_traces) ;
  unit

(** [http_traceTransaction] on a single EVM tx that triggers several
    independent CRACs — the kernel journal records them as sibling
    top-level traces and [maybe_store_http_traces_for_tx] must preserve
    order through RLP. *)
let test_http_trace_multiple_independent_cracs =
  register_crac_runner_test
    ~title:"CRAC: http_traceTransaction with multiple independent CRACs"
    ~tags:["http_trace"; "crac_trace"; "multi"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "HTTP-TRACE-MULTI" in
  Log.debug ~prefix "Originate TEZ leaves" ;
  let* tez_leaf_1 = TezMultiRunCaller.originate () in
  let* tez_leaf_2 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridges to the two leaves" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_2 in
  Log.debug ~prefix "Deploy EVM main invoking both bridges" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_hash =
    match block.transactions with
    | Block.Hash (h :: _) -> h
    | _ -> Test.fail "Expected a tx hash in the latest EVM block"
  in
  Log.debug ~prefix "http_traceTransaction %s" tx_hash ;
  let*@ result = Rpc.Tezosx.http_traceTransaction ~tx_hash sequencer in
  let traces = JSON.(result |-> "traces" |> as_list) in
  Check.(
    (List.length traces >= 2)
      int
      ~error_msg:
        "Expected at least 2 top-level HTTP traces (one per independent CRAC), \
         got %L") ;
  List.iter check_http_trace_shape traces ;
  (* Both top-level traces should target the gateway precompile (same URL
     prefix up to the KT1), so the two URLs should differ only in the
     target contract. *)
  let urls = List.map (fun t -> JSON.(t |-> "url" |> as_string)) traces in
  let distinct_urls = List.sort_uniq String.compare urls |> List.length in
  Check.(
    (distinct_urls >= 2)
      int
      ~error_msg:
        "Expected at least 2 distinct CRAC target URLs across traces, got %L \
         (urls collapsed)") ;
  Log.info
    "Multi-CRAC http_traceTransaction: %d top-level traces, %d distinct URLs"
    (List.length traces)
    distinct_urls ;
  unit

(** [http_traceBlockByNumber] on a block containing one non-CRAC EVM
    transfer and one EVM->TEZ CRAC: the RPC must return one entry per
    transaction, with non-empty traces on the CRAC tx and an empty
    [traces] list on the plain transfer. Exercises the "multiple
    transactions with mixed CRAC content in the same block" axis that
    single-tx tests don't cover. *)
let test_http_trace_mixed_block =
  register_crac_runner_test
    ~title:"CRAC: http_traceBlockByNumber on mixed CRAC + non-CRAC block"
    ~tags:["http_trace"; "crac_trace"; "block"; "mixed"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "HTTP-TRACE-MIXED" in
  Log.debug ~prefix "Originate TEZ runner and deploy EVM bridge" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  let* (`Evm_runner bridge_addr) =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner
  in
  (* Queue a plain self-transfer in the mempool. *)
  Log.debug ~prefix "Queue a plain EVM self-transfer" ;
  let* transfer_tx_hash =
    send_evm_transfer_no_block ~address:sender.Eth_account.address ()
  in
  (* Queue an EVM->TEZ CRAC invocation in the same mempool (no block
     produced yet). *)
  Log.debug ~prefix "Queue an EVM->TEZ CRAC invocation" ;
  let* crac_raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:3_000_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:bridge_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ crac_tx_hash = Rpc.send_raw_transaction ~raw_tx:crac_raw_tx sequencer in
  Log.debug ~prefix "Produce the block containing both txs" ;
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_hashes =
    match block.transactions with
    | Block.Hash hashes -> hashes
    | _ -> Test.fail "Expected tx hashes in the latest EVM block"
  in
  Check.(
    (List.length tx_hashes = 2)
      int
      ~error_msg:
        "Expected 2 transactions in the mixed block (1 transfer + 1 CRAC), got \
         %L") ;
  let block_number_hex = Printf.sprintf "0x%lx" block.number in
  Log.debug ~prefix "http_traceBlockByNumber %s" block_number_hex ;
  let*@ entries =
    Rpc.Tezosx.http_traceBlockByNumber ~block:block_number_hex sequencer
  in
  let entries_list = JSON.as_list entries in
  Check.(
    (List.length entries_list = 2)
      int
      ~error_msg:"Expected one http_trace entry per tx, got %L") ;
  let find_entry hash =
    List.find_opt
      (fun e -> JSON.(e |-> "txHash" |> as_string) = hash)
      entries_list
    |> function
    | Some e -> e
    | None -> Test.fail "http_traceBlockByNumber missing expected entry %s" hash
  in
  let transfer_entry = find_entry transfer_tx_hash in
  let crac_entry = find_entry crac_tx_hash in
  Check.(
    (JSON.(transfer_entry |-> "traces" |> as_list) = [])
      (list json)
      ~error_msg:"Expected empty traces on the plain transfer tx, got %L") ;
  let crac_traces = JSON.(crac_entry |-> "traces" |> as_list) in
  Check.(
    (crac_traces <> [])
      (list json)
      ~error_msg:"Expected non-empty traces on the CRAC tx, got %L") ;
  List.iter check_http_trace_shape crac_traces ;
  Log.info
    "Mixed-block http_traceBlockByNumber: %d entries, transfer empty, CRAC \
     with %d traces"
    (List.length entries_list)
    (List.length crac_traces) ;
  unit

(** EVM journal is preserved across CrossRuntime boundaries: TEZ revert must
 *  roll back inner EVM storage changes made via a TEZ->EVM CRAC.
 *
 *    EVM[evm_outer] ~CRAC~> TEZ[tez_runner]
 *                             |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *                             |-> REVERT
 *
 *  evm_inner gets called from within the TEZ execution (TEZ->EVM inner CRAC).
 *  When tez_runner reverts, both the TEZ state and evm_inner's EVM storage
 *  must be rolled back to zero.
 *)
let test_crac_tez_revert_rolls_back_inner_evm_storage =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert rolls back inner EVM storage"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy inner EVM contract" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to inner EVM" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  Log.debug ~prefix "Originate TEZ runner (calls bridge, then reverts)" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM outer bridge to TEZ runner" ;
  let* evm_outer = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Call EVM outer (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_outer in
  Log.debug ~prefix "Verify inner EVM storage was rolled back" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_inner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_outer
  in
  unit

(** TEZ caller reverts after a successful cross-runtime call to EVM.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_propagates_to_evm =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert propagates to EVM"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge] ~revert:true ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** EVM journal state preserved across CRAC sub-calls.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_ok] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_reverter] (pure EVM revert)
 *
 *  The first double-CRAC modifies journal state (logs, checkpoints),
 *  making the outer checkpoint log_i > 0. The caught EVM revert
 *  triggers REVM's logs[checkpoint.log_i..] slice operation which
 *  panics if the prior CRAC drained or reset journal state.
 *)
let test_crac_evm_journal_state_preserved =
  register_crac_runner_test
    ~title:"CRAC: EVM journal state preserved"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_ok = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_ok, false); (evm_reverter, true)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:3
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_ok
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Simplest catch scenario: a TEZ revert behind one CRAC is caught by the
 *  EVM caller. Verifies that Solidity try/catch works across a single
 *  cross-runtime boundary: the caught callee is rolled back while the
 *  outer caller continues execution.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                         |-> REVERT
 *)
let test_crac_catch_tez_revert =
  register_crac_runner_test ~title:"CRAC: catch TEZ revert" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** EVM caller reverts after a successful cross-runtime call to TEZ.
 *  Tests whether the TEZ-side effects are rolled back.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_propagates_to_tez =
  register_crac_runner_test
    ~title:"CRAC: EVM revert propagates to TEZ"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf runner" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM main calling bridge then reverting" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge, false)]
      ~revert:true
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Second CRAC target TEZ reverts, rolling back the first.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_second_crac_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: second CRAC target TEZ reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** EVM revert rolls back two successful CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_rolls_back_two_cracs =
  register_crac_runner_test
    ~title:"CRAC: EVM revert rolls back two CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main with revert" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** Deep first branch with TEZ revert in second branch.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1] ~CRAC~> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_deep_branch_with_second_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: deep first branch with TEZ revert in second"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build first branch (deep chain)" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_bridge_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_runner_1 = TezCrossRuntimeRunnerEvm.originate evm_bridge_inner in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  Log.debug ~prefix "Build second branch (TEZ revert)" ;
  let* tez_runner_2 = TezMultiRunCaller.originate ~revert:true () in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_1
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** Nested CRAC revert cascade without catch.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *                                 |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *                                 |-> REVERT
 *
 *)
let test_crac_nested_revert_cascade_without_catch =
  register_crac_runner_test
    ~title:"CRAC: nested revert cascade without catch"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner with revert" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** Double-nested CRAC with intermediate EVM revert.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_outer] ~CRAC~> TEZ[tez_runner_outer] ~CRAC~> EVM[evm_runner]
 *                                                                     |-> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_runner_inner] ~CRAC~> EVM[evm_leaf]
 *                                                                     |-> REVERT
 *
 *)
let test_crac_double_nested_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: double-nested CRAC with intermediate EVM revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build inner chain" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_runner_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_inner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_inner
  in
  Log.debug ~prefix "Deploy EVM runner (calls inner chain then reverts)" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_inner, false)]
      ()
  in
  Log.debug ~prefix "Build outer chain" ;
  let* tez_runner_outer = TezCrossRuntimeRunnerEvm.originate evm_runner in
  let* evm_bridge_outer =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_outer
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_outer, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_outer
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_inner
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_outer
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  unit

(** Deep nesting across 6 CRAC levels.
 *
 *    EVM[a] ~CRAC~> TEZ[b] ~CRAC~> EVM[c] ~CRAC~> TEZ[d] ~CRAC~> EVM[e] ~CRAC~> TEZ[f]
 *                                                                               |-> REVERT
 *
 *)
let test_crac_deep_nesting_6_levels =
  register_crac_runner_test
    ~title:"CRAC: deep nesting across 6 CRAC levels"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 6-level CRAC chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Call EVM a (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  unit

(** EVM revert after nested CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[a] ~CRAC~> TEZ[b] ~CRAC~> EVM[c] ~CRAC~> TEZ[d] ~CRAC~> EVM[e] ~CRAC~> TEZ[f]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_after_nested_cracs =
  register_crac_runner_test
    ~title:"CRAC: EVM revert after nested CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with revert" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~revert:true ~callees:[(evm_a, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  unit

(** EVM CRAC target reverts.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_evm_target_reverts =
  register_crac_runner_test
    ~title:"CRAC: EVM CRAC target reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM reverter" ;
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridge to EVM reverter" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** Second CRAC target EVM reverts, rolling back the first.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_second_crac_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: second CRAC target EVM reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** TEZ revert rolls back two successful CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_rolls_back_two_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert rolls back two CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~revert:true
      ~callees:[tez_bridge_1; tez_bridge_2]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** Deep first branch with EVM revert in second branch.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_deep_branch_with_second_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: deep first branch with EVM revert in second"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build first branch (deep chain)" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_runner_1 =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner
  in
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  Log.debug ~prefix "Build second branch (EVM revert)" ;
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** TEZ nested revert cascade without catch.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *                                 |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *                                 |-> REVERT
 *
 *)
let test_crac_tez_nested_revert_cascade_without_catch =
  register_crac_runner_test
    ~title:"CRAC: TEZ nested revert cascade without catch"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner with revert" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Double-nested CRAC with intermediate TEZ revert.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_outer] ~CRAC~> EVM[evm_runner_outer] ~CRAC~> TEZ[tez_runner]
 *                                                                     |-> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_runner_inner] ~CRAC~> TEZ[tez_leaf]
 *                                                                     |-> REVERT
 *
 *)
let test_crac_double_nested_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: double-nested CRAC with intermediate TEZ revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build inner chain" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_runner_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_runner_inner in
  Log.debug ~prefix "Originate TEZ runner (calls inner chain then reverts)" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge_inner] ()
  in
  Log.debug ~prefix "Build outer chain" ;
  let* evm_runner_outer = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let* tez_bridge_outer = TezCrossRuntimeRunnerEvm.originate evm_runner_outer in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge_outer] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_outer
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_inner
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_outer
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  unit

(** TEZ deep nesting across 6 CRAC levels.
 *
 *    TEZ[a] ~CRAC~> EVM[b] ~CRAC~> TEZ[c] ~CRAC~> EVM[d] ~CRAC~> TEZ[e] ~CRAC~> EVM[f]
 *                                                                               |-> REVERT
 *
 *)
let test_crac_tez_deep_nesting_6_levels =
  register_crac_runner_test
    ~title:"CRAC: TEZ deep nesting across 6 CRAC levels"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 6-level CRAC chain (inside-out)" ;
  let* evm_f = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_f in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Call TEZ a" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_f in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_e in
  unit

(** TEZ revert after nested CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[a] ~CRAC~> EVM[b] ~CRAC~> TEZ[c] ~CRAC~> EVM[d] ~CRAC~> TEZ[e] ~CRAC~> EVM[f]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_after_nested_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert after nested CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* evm_f = EvmMultiRunCaller.deploy_and_init () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_f in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_a] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_f in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_e in
  unit

(** Caught EVM revert between successful cross-runtime calls.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> (Catch) EVM[evm_reverter]
 *     |           |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |           |-> REVERT
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_catch_evm_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch EVM revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling bridge" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge, false); (evm_reverter, true); (evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Caught cross-runtime TEZ revert between successful CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> (Catch) EVM[evm_bridge_rev] ~CRAC~> TEZ[tez_reverter]
 *     |                                       |-> TEZ[tez_leaf]
 *     |                                       |-> REVERT
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_catch_tez_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch cross-runtime TEZ revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Originate TEZ reverter calling leaf" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_leaf] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge_rev = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [(evm_bridge, false); (evm_bridge_rev, true); (evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_rev
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** Caught TEZ revert with nested CRAC to EVM inside caught block.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_reverter]
 *     |                                     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |                                     |-> REVERT
 *     |-> EVM[evm_leaf]
 *
 *)
let test_crac_catch_tez_revert_with_nested_crac =
  register_crac_runner_test
    ~title:"CRAC: catch TEZ revert with nested CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM bridge_1 to TEZ bridge (double CRAC)" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Originate TEZ reverter calling bridge" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM bridge_2 to TEZ reverter" ;
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, true); (evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** Caught deep EVM revert propagated through double CRAC.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_reverter]
 *     |                                                                |-> EVM[evm_leaf]
 *     |                                                                |-> REVERT
 *     |-> EVM[evm_leaf]
 *
 *)
let test_crac_catch_deep_evm_revert_through_double_crac =
  register_crac_runner_test
    ~title:"CRAC: catch deep EVM revert through double CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Build first double CRAC chain to evm_leaf" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_1 in
  Log.debug ~prefix "Deploy EVM reverter calling evm_leaf" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Build second double CRAC chain to evm_reverter" ;
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, true); (evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** TEZ-initiated: caught EVM revert between CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_leaf]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_catcher]
 *     |                           |-> (Catch) EVM[evm_reverter]
 *     |                           |           |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |                           |           |-> REVERT
 *     |-> TEZ[tez_leaf]
 *
 *)
let test_crac_tez_catch_evm_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch EVM revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling bridge" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_reverter, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_leaf; tez_bridge; tez_leaf] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  (* The default gas_limit (10_000) is too low: the inner try-catch path
     crosses two runtimes (TEZ->EVM->TEZ) and adds extra EVM contract calls,
     exhausting the budget. 20_000 provides enough headroom. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** TEZ-initiated: caught cross-runtime TEZ revert between CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_catcher] ~CRAC~> EVM[evm_catcher]
 *     |                                   |-> (Catch) EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_reverter]
 *     |                                                                             |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |                                                                             |-> REVERT
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_catch_tez_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch TEZ revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge_leaf = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ reverter calling bridge to EVM leaf" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge_leaf] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge_inner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter
  in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_inner, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge_catcher = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_leaf; tez_bridge_catcher; tez_bridge_leaf]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_leaf
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage
      ~expected_counter:2
      tez_bridge_catcher
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** TEZ-initiated: caught deep EVM revert through double CRAC.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_catcher] ~CRAC~> EVM[evm_catcher]
 *     |                                   |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_reverter]
 *     |                                                                                                     |-> EVM[evm_leaf]
 *     |                                                                                                     |-> REVERT
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_catch_deep_revert_through_double_crac =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch deep revert through double CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge_leaf = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling evm_leaf" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Build double CRAC chain to EVM reverter" ;
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge_catcher = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_leaf; tez_bridge_catcher; tez_bridge_leaf]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_leaf
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage
      ~expected_counter:2
      tez_bridge_catcher
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Caught EVM revert after 3 sequential CRACs.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_reverter]
 *                 |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_leaf_1]
 *                 |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_leaf_2]
 *                 |-> EVM[evm_bridge_3] ~CRAC~> TEZ[tez_leaf_3]
 *                 |-> REVERT
 *
 *)
let test_crac_catch_revert_after_multiple_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch revert after multiple sequential CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate 3 TEZ leaves" ;
  let* tez_leaf_1 = TezMultiRunCaller.originate () in
  let* tez_leaf_2 = TezMultiRunCaller.originate () in
  let* tez_leaf_3 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy 3 EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_2 in
  let* evm_bridge_3 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_3 in
  Log.debug ~prefix "Deploy EVM reverter calling 3 bridges" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:
        [(evm_bridge_1, false); (evm_bridge_2, false); (evm_bridge_3, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_reverter, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_3
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_2 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_3 in
  unit

(** Caught TEZ revert after 3 return CRACs to EVM.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                         |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_leaf_1]
 *                                         |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_leaf_2]
 *                                         |-> TEZ[tez_bridge_3] ~CRAC~> EVM[evm_leaf_3]
 *                                         |-> REVERT
 *
 *)
let test_crac_catch_tez_revert_after_multiple_return_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch TEZ revert after multiple return CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy 3 EVM leaves" ;
  let* evm_leaf_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_leaf_2 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_leaf_3 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate 3 TEZ bridges to EVM leaves" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_leaf_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_leaf_2 in
  let* tez_bridge_3 = TezCrossRuntimeRunnerEvm.originate evm_leaf_3 in
  Log.debug ~prefix "Originate TEZ reverter calling 3 bridges" ;
  let* tez_reverter =
    TezMultiRunCaller.originate
      ~revert:true
      ~callees:[tez_bridge_1; tez_bridge_2; tez_bridge_3]
      ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_3
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_2 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_3 in
  unit

(** Caught EVM revert at end of 4-crossing chain.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e]
 *                                                                                               |-> REVERT
 *
 *)
let test_crac_catch_4_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: catch 4-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 4-crossing chain (inside-out)" ;
  let* evm_e = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_e in
  unit

(** TEZ-initiated: caught 4-crossing chain revert.
 *
 *    TEZ[tez_x] ~CRAC~> EVM[evm_catcher]
 *                       |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e]
 *                                                                                                                 |-> REVERT
 *
 *)
let test_crac_tez_catch_4_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch 4-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 4-crossing chain (inside-out)" ;
  let* evm_e = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_x = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Call TEZ x" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_x in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_x in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_e in
  unit

(** Caught TEZ revert at end of 5-crossing chain.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_f]
 *                                                                                                                   |-> REVERT
 *
 *)
let test_crac_catch_5_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: catch 5-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  unit

(** TEZ-initiated: caught 5-crossing chain revert.
 *
 *    TEZ[tez_x] ~CRAC~> EVM[evm_catcher]
 *                       |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_f]
 *                                                                                                                                     |-> REVERT
 *
 *)
let test_crac_tez_catch_5_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch 5-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_x = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Call TEZ x" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_x in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_x in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  unit

(** Caught chained TEZ revert behind CRAC.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_runner_1]
 *                                         |-> TEZ[tez_runner_2]
 *                                             |-> TEZ[tez_reverter]
 *                                                 |-> REVERT
 *
 *)
let test_crac_chained_tez_calls_behind_crac =
  register_crac_runner_test
    ~title:"CRAC: chained TEZ calls behind CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ reverter and chain" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let* tez_runner_2 = TezMultiRunCaller.originate ~callees:[tez_reverter] () in
  let* tez_runner_1 = TezMultiRunCaller.originate ~callees:[tez_runner_2] () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ chain" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Nested catches with multiple reverts at two depths.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge_1] ~CRAC~> TEZ[tez_reverter_1]
 *     |                                     |-> REVERT
 *     |-> (Catch) EVM[evm_reverter]
 *     |           |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_reverter_2]
 *     |           |                                     |-> REVERT
 *     |           |-> EVM[evm_bridge_3] ~CRAC~> TEZ[tez_leaf]
 *     |           |-> REVERT
 *     |-> EVM[evm_bridge_4] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_nested_catches_with_multiple_reverts =
  register_crac_runner_test
    ~title:"CRAC: nested catches with multiple reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf and reverters" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* tez_reverter_1 = TezMultiRunCaller.originate ~revert:true () in
  let* tez_reverter_2 = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_2 in
  let* evm_bridge_3 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* evm_bridge_4 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter with inner catch" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_2, true); (evm_bridge_3, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [(evm_bridge_1, true); (evm_reverter, true); (evm_bridge_4, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_2
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_3
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_4
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  unit

let test_crac_gas_model_alias_caching =
  register_crac_runner_test
    ~title:"CRAC: gas model charges less on alias cache hit"
    ~tags:["gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner calling the bridge" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "First call (aliases generated)" ;
  let* gas_first = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "Second call (aliases cached)" ;
  let* gas_second = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "gas_first=%Ld gas_second=%Ld" gas_first gas_second ;
  Check.(
    (gas_first > gas_second)
      int64
      ~error_msg:"First CRAC (%L gas) should cost more than second (%R gas)") ;
  let diff = Int64.sub gas_first gas_second in
  (* On cache miss, each alias incurs the Tezos origination gas
     (~105,137 milligas = ~1,051 EVM gas) on top of the lookup cost.
     On cache hit, only ALIAS_CACHE_HIT_COST (2,100) is paid.
     Two aliases are resolved per CRAC, so the minimum difference
     attributable to alias generation alone is 2 * ~1,051. We use a
     conservative lower bound of 1,000 per alias. *)
  let tezos_alias_generation_evm_gas = 1_000 in
  let expected_min_diff = Int64.of_int (2 * tezos_alias_generation_evm_gas) in
  Check.(
    (diff >= expected_min_diff)
      int64
      ~error_msg:
        "Gas difference (%L) should be >= %R (2x alias generation surcharge)") ;
  unit

(* ── Receipt test helpers ──────────────────────────────────────── *)

(** Fetch Tezlink block manager operations (pass 3) as a JSON list.
    Uses [/operations] (all 4 passes) and extracts index 3 (manager ops).
    [block] is a block identifier (level number or ["head"]). *)
let fetch_michelson_manager_ops ~block sequencer =
  let michelson_base = Evm_node.endpoint sequencer ^ "/tezlink" in
  let path = sf "/chains/main/blocks/%s/operations" block in
  let* res =
    Curl.get_raw ~name:"curl#michelson-ops" (michelson_base ^ path)
    |> Runnable.run
  in
  let all_passes = JSON.parse ~origin:"michelson_operations" res in
  return JSON.(all_passes |=> 3)

(** Return the current Michelson runtime head level as a string. *)
let michelson_head_level sequencer =
  let michelson_base = Evm_node.endpoint sequencer ^ "/tezlink" in
  let* res =
    Curl.get_raw
      ~name:"curl#michelson-head"
      (michelson_base ^ "/chains/main/blocks/head/header")
    |> Runnable.run
  in
  let head = JSON.parse ~origin:"michelson_header" res in
  return JSON.(head |-> "level" |> as_int)

(** Fetch manager operations from the most recent non-empty Tezlink block.
    bake_until_sync may advance the head past the block containing the
    CRAC receipt, so we scan backwards from [head]. *)
let fetch_recent_michelson_manager_ops sequencer =
  let* head = michelson_head_level sequencer in
  let rec find_ops level =
    if level < 1 then return (JSON.parse ~origin:"empty" "[]")
    else
      let* ops =
        fetch_michelson_manager_ops ~block:(string_of_int level) sequencer
      in
      let op_list = JSON.(ops |> as_list) in
      if op_list <> [] then return ops else find_ops (level - 1)
  in
  find_ops head

(** Null implicit address — source of all synthetic Michelson operations
    (both CRAC and non-CRAC). *)
let handler_address = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

(** Fetch the latest EVM block, verify it has [expected_tx_count] transactions,
    and return the block.  Uses [prefix] for log messages. *)
let check_evm_block_tx_count ~prefix ~expected_tx_count sequencer =
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_count =
    match block.transactions with
    | Block.Hash txs -> List.length txs
    | Block.Full txs -> List.length txs
    | Block.Empty -> 0
  in
  Log.info "%s: EVM block %ld has %d tx(s)" prefix block.number tx_count ;
  Check.(
    (tx_count = expected_tx_count)
      int
      ~error_msg:"Expected %R EVM transaction(s), got %L") ;
  return block

(** Compute the expected fake CRAC transaction hash for a given [crac_id]
    and [block_number], then verify it is present in the block's tx hashes.
    The kernel computes: hash = keccak256("CRAC-TX" || block_number_be256 || crac_id). *)
let check_fake_crac_tx_hash ~prefix ~expected_crac_id (block : Block.t) =
  let block_number_be256 =
    let buf = Bytes.make 32 '\000' in
    Bytes.set_int64_be buf 24 (Int64.of_int32 block.number) ;
    buf
  in
  let expected_hash_bytes =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest
      (Bytes.cat
         (Bytes.cat (Bytes.of_string "CRAC-TX") block_number_be256)
         (Bytes.of_string expected_crac_id))
  in
  let expected_hash = "0x" ^ (Hex.of_bytes expected_hash_bytes |> Hex.show) in
  Log.info
    "%s: expected fake tx hash for CRAC-ID %s = %s"
    prefix
    expected_crac_id
    expected_hash ;
  let tx_hashes =
    match block.transactions with
    | Block.Hash hs -> List.map String.lowercase_ascii hs
    | Block.Full txs ->
        List.map
          (fun (tx : Transaction.transaction_object) ->
            String.lowercase_ascii tx.hash)
          txs
    | Block.Empty -> []
  in
  Check.is_true
    (List.mem expected_hash tx_hashes)
    ~error_msg:
      (sf
         "%s: Expected fake CRAC tx hash (CRAC-ID %s) in EVM block tx hashes"
         prefix
         expected_crac_id)

(** Verify that [top] is a valid CRAC top-level content item per the RFC.
    Checks source = handler, destination = [expected_destination],
    status = [expected_status], and all synthetic fields = 0.
    Returns the list of internal_operation_results from metadata. *)
let check_crac_top_level ~prefix ~expected_destination ~expected_status top =
  let kind = JSON.(top |-> "kind" |> as_string) in
  Log.info ~prefix "top-level kind = %s" kind ;
  Check.(
    (kind = "transaction")
      string
      ~error_msg:"Expected top-level kind %R, got %L") ;
  let source = JSON.(top |-> "source" |> as_string) in
  Log.info "%s: top-level source = %s" prefix source ;
  Check.(
    (source = handler_address)
      string
      ~error_msg:"Expected top-level source = Handler_M (%R), got %L") ;
  let destination = JSON.(top |-> "destination" |> as_string) in
  Log.info "%s: top-level destination (alias E_0) = %s" prefix destination ;
  Check.(
    (destination = expected_destination)
      string
      ~error_msg:"Expected top-level destination %R, got %L") ;
  Check.(
    (JSON.(top |-> "amount" |> as_string) = "0")
      string
      ~error_msg:"Expected amount %R, got %L") ;
  Check.(
    (JSON.(top |-> "fee" |> as_string) = "0")
      string
      ~error_msg:"Expected fee %R, got %L") ;
  Check.(
    (JSON.(top |-> "counter" |> as_string) = "0")
      string
      ~error_msg:"Expected counter %R, got %L") ;
  Check.(
    (JSON.(top |-> "gas_limit" |> as_string) = "0")
      string
      ~error_msg:"Expected gas_limit %R, got %L") ;
  Check.(
    (JSON.(top |-> "storage_limit" |> as_string) = "0")
      string
      ~error_msg:"Expected storage_limit %R, got %L") ;
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Log.info "%s: top-level status = %s" prefix top_status ;
  Check.(
    (top_status = expected_status)
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  JSON.(metadata |-> "internal_operation_results" |> as_list)

(** Verify that [iop] is a valid CRAC event with the given [expected_crac_id].
    Checks kind = "event", source = handler, tag = "crac",
    payload = expected_crac_id.  When [expected_status] is provided,
    also checks the event result status (omit for failed CRACs where
    the event may be backtracked). *)
let check_crac_event ~prefix ~expected_crac_id ?expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "event")
      string
      ~error_msg:(sf "%s: Expected event kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = handler_address)
      string
      ~error_msg:
        (sf "%s: Expected event source = handler (%%R), got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "tag" |> as_string) = "crac")
      string
      ~error_msg:(sf "%s: Expected event tag %%R, got %%L" prefix)) ;
  let payload = JSON.(iop |-> "payload" |-> "string" |> as_string) in
  Log.info "%s: CRAC-ID = %s" prefix payload ;
  Check.(
    (payload = expected_crac_id)
      string
      ~error_msg:(sf "%s: Expected CRAC-ID %%R, got %%L" prefix)) ;
  match expected_status with
  | Some s ->
      Check.(
        (JSON.(iop |-> "result" |-> "status" |> as_string) = s)
          string
          ~error_msg:(sf "%s: Expected event status %%R, got %%L" prefix))
  | None -> ()

(** Verify that [iop] is a valid internal transaction with the given fields. *)
let check_crac_internal_transaction ~prefix ~expected_nonce ~expected_source
    ~expected_destination ~expected_entrypoint ~expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "transaction")
      string
      ~error_msg:(sf "%s: Expected kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = expected_source)
      string
      ~error_msg:(sf "%s: Expected source %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "nonce" |> as_int) = expected_nonce)
      int
      ~error_msg:(sf "%s: Expected nonce %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "destination" |> as_string) = expected_destination)
      string
      ~error_msg:(sf "%s: Expected destination %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "parameters" |-> "entrypoint" |> as_string)
    = expected_entrypoint)
      string
      ~error_msg:(sf "%s: Expected entrypoint %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "result" |-> "status" |> as_string) = expected_status)
      string
      ~error_msg:(sf "%s: Expected status %%R, got %%L" prefix))

(* ── Receipt tests ────────────────────────────────────────────── *)

(* CRAC receipt structure test — single EVM→TEZ crossing.
 *
 *  Per the RFC the Michelson runtime block contains a manager operation with:
 *   - Top-level: handler → alias(E_0), synthetic fields = 0, applied
 *   - Internal #0 (event): gateway emits "crac" tag with CRAC-ID payload
 *   - Internal #1 (transaction): alias(E_1) → tez_runner, applied
 *   - Further internal ops from Michelson contract execution
 *
 *     EVM[evm_runner] |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *)
let test_crac_evm_to_tez_receipt =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ receipt matches RFC structure"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-E2T" in
  let* tez_runner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_runner_kt1)) = tez_runner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run evm_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 2: 3 internal ops — event, alias→tez_runner, self-call *)
  Check.(
    (List.length internals = 3)
      int
      ~error_msg:"Expected 3 internal operations, got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(E_1) → tez_runner (%run) ───────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_runner → tez_runner (%_incrementWitness) ─ *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_runner_kt1
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  unit

(* Two EVM→TEZ CRACs from the SAME EVM transaction (RFC Example 5).
 *
 *  Per the RFC, the Michelson runtime block should have ONE manager operation
 *  with at least TWO internal transactions (one per gateway call, plus
 *  additional ops from Michelson execution) and ONE CRAC event (first).
 *
 *    EVM[evm_main]
 *     |-> EVM[bridge_1] ~CRAC~> TEZ[tez_1]
 *     |-> EVM[bridge_2] ~CRAC~> TEZ[tez_2]
 *)
let test_crac_receipt_two_independent =
  register_crac_runner_test
    ~title:"CRAC: two EVM->TEZ from same tx produce one Michelson op"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-2IND" in
  let* tez_1 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_1_kt1)) = tez_1 in
  let* tez_2 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_2_kt1)) = tez_2 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_2 in
  let (`Evm_runner bridge_2_addr) = bridge_2 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(bridge_1, false); (bridge_2, false)]
      ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_1_addr sequencer
  in
  let*@ bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_2_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_2 in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 5: 5 internal ops — event, then two pairs of
     alias(bridge) → tez_runner (%run) + tez_runner → tez_runner
     (%_incrementWitness), in execution order (tez_1 before tez_2). *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal operations (RFC Example 5), got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(bridge_1) → tez_1 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_1 → tez_1 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(bridge_2) → tez_2 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_2 → tez_2 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_2_kt1
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  unit

(* Two truly independent EVM→TEZ CRACs from SEPARATE EVM transactions
 * in the same block (RFC Example 6).
 *
 *  Each EVM transaction gets a different tx_index → different CRAC-ID.
 *  The Michelson runtime block should have TWO manager operations, each with its
 *  own CRAC event and a different CRAC-ID.
 *
 *   EVM tx 0: EVM[runner_1] → EVM[bridge_1] ~CRAC~> TEZ[tez_1]  (CRAC-ID "1-0")
 *   EVM tx 1: EVM[runner_2] → EVM[bridge_2] ~CRAC~> TEZ[tez_2]  (CRAC-ID "1-1")
 *)
let test_crac_receipt_separate_tx_two_cracs =
  register_crac_runner_test
    ~title:
      "CRAC: two separate EVM txs produce two Michelson ops with different \
       CRAC-IDs"
    ~tags:["crac_receipt"; "crac_id"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-SEP" in
  (* Deploy infrastructure via the wrapper (each deploy produces a block). *)
  let* tez_1 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_1_kt1)) = tez_1 in
  let* tez_2 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_2_kt1)) = tez_2 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_2 in
  let (`Evm_runner bridge_2_addr) = bridge_2 in
  let* runner_1 =
    EvmMultiRunCaller.deploy_and_init ~callees:[(bridge_1, false)] ()
  in
  let (`Evm_runner runner_1_addr) = runner_1 in
  let* runner_2 =
    EvmMultiRunCaller.deploy_and_init ~callees:[(bridge_2, false)] ()
  in
  let (`Evm_runner runner_2_addr) = runner_2 in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_1_addr sequencer
  in
  let*@ bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_2_addr sequencer
  in
  (* Send both run() transactions to the mempool without producing a
     block, then produce one block so both land with different tx_index. *)
  let* raw_tx_1 =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:runner_1_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ _tx_hash_1 = Rpc.send_raw_transaction ~raw_tx:raw_tx_1 sequencer in
  let* raw_tx_2 =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:runner_2_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ _tx_hash_2 = Rpc.send_raw_transaction ~raw_tx:raw_tx_2 sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Verify both Tez contracts were called *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_2 in
  (* Fetch Michelson runtime manager operations *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 2) int ~error_msg:"Expected 2 operations, got %L") ;
  (* Verify each operation has correct CRAC structure:
     event + bridge_alias → tez (%run) + tez → tez (%_incrementWitness) *)
  let check_op ~prefix ~expected_destination ~expected_bridge_alias
      ~expected_dest ~expected_crac_id ~nonce_offset op =
    let top = JSON.(op |-> "contents" |=> 0) in
    let internals =
      check_crac_top_level
        ~prefix
        ~expected_destination
        ~expected_status:"applied"
        top
    in
    Check.(
      (List.length internals = 3)
        int
        ~error_msg:(sf "%s: expected 3 internal operations, got %%L" prefix)) ;
    check_crac_event
      ~prefix
      ~expected_crac_id
      ~expected_status:"applied"
      (List.nth internals 0) ;
    (* Internal operation nonces are block-global (L1 semantics):
       event gets nonce_offset, first tx gets nonce_offset+1, etc. *)
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 1)
      ~expected_source:expected_bridge_alias
      ~expected_destination:expected_dest
      ~expected_entrypoint:"run"
      ~expected_status:"applied"
      (List.nth internals 1) ;
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 2)
      ~expected_source:expected_dest
      ~expected_destination:expected_dest
      ~expected_entrypoint:"_incrementWitness"
      ~expected_status:"applied"
      (List.nth internals 2)
  in
  (* First operation: nonces 0,1,2 (event=0, run=1, _incrementWitness=2) *)
  check_op
    ~prefix:(prefix ^ "#0")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_1_alias
    ~expected_dest:tez_1_kt1
    ~expected_crac_id:"1-0"
    ~nonce_offset:0
    JSON.(ops |=> 0) ;
  (* Second operation: nonces continue at 3,4,5 (block-global counter) *)
  check_op
    ~prefix:(prefix ^ "#1")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_2_alias
    ~expected_dest:tez_2_kt1
    ~expected_crac_id:"1-1"
    ~nonce_offset:3
    JSON.(ops |=> 1) ;
  unit

(* EVM→TEZ→EVM double crossing — verify receipts on BOTH sides.
 *
 *  The EVM transaction triggers a CRAC into Michelson, which then CRACs
 *  back into EVM.  We verify:
 *   - Tezlink block: one manager op with handler source, event, internals
 *   - EVM block: at least one additional fake transaction (from TEZ→EVM)
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *)
let test_crac_receipt_evm_tez_evm =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ->EVM double crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-ETE" in
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_inner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  (* ── Michelson runtime side: EVM→TEZ leg ─────────────────────────────── *)
  Log.debug ~prefix "Verify Michelson runtime receipt" ;
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected = 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 3: 5 internal ops — event, alias(evm_bridge) → tez_bridge (%run),
     tez_bridge → tez_bridge (%_incrementWitness) (pre),
     tez_bridge → GW_M (%call_evm), tez_bridge → tez_bridge (%_incrementWitness) (post). *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal operations (RFC Example 3), got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(evm_bridge) → tez_bridge (%run) ────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_bridge → tez_bridge (%_incrementWitness) (pre) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: tez_bridge → GW_M (%call_evm) ─────────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_bridge → tez_bridge (%_incrementWitness) (post) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── EVM side: re-entrant TEZ→EVM leg ─────────────────────── *)
  (* Per RFC principle 6, the re-entrant EVM execution appears as
     internal transactions under the ORIGINAL EVM transaction, not as
     a separate top-level fake CRAC tx. *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* TEZ→EVM→TEZ double crossing — verify receipts on BOTH sides.
 * This covers RFC Example 7 (Michelson → EVM → Michelson multi-hop).
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_outer_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *
 *  We verify:
 *   - EVM block: fake CRAC transaction whose hash matches CRAC-ID "0-0"
 *   - Tezlink block: manager operation from the second EVM→TEZ leg
 *)
let test_crac_receipt_tez_evm_tez =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM->TEZ double crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TET" in
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* tez_outer_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  let (`Tez_runner (_, tez_outer_bridge_kt1)) = tez_outer_bridge in
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_outer_bridge] () in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* () = TezRunner.call_run tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  (* ── EVM side: TEZ→EVM leg ─────────────────────────────────── *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  (* ── Michelson runtime side: EVM→TEZ return leg ────────────────────── *)
  (* Origin is TEZ (runtime_id=0), so re-entering TEZ should NOT
     produce a CRAC event — the event was already emitted on the
     first crossing out of the origin runtime. *)
  (* ── Michelson runtime side: TEZ→EVM→TEZ ────────────────────────────── *)
  (* Per RFC principle 6 (one top-level per runtime per CRAC-ID),
     the re-entrant TEZ execution (EVM→TEZ return leg) appears as
     internal operations within the ORIGINAL TEZ transaction, not
     as a separate manager operation.  Verify the Michelson runtime block has
     1 manager operation with the return leg visible as internal ops. *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  (* The original TEZ transaction is an outgoing CRAC (TEZ→EVM), so the
     top-level source is the injected sender, not the handler.  Re-entrant
     EVM→TEZ effects are merged as internal ops per RFC principle 6.
     No CRAC event is emitted (origin runtime = TEZ). *)
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.(
    (top_status = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* Expected internal ops (all transactions, no CRAC event):
     #0: tez_main → tez_main (%_incrementWitness)
     #1: tez_main → tez_outer_bridge (%run)
     #2: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (pre)
     #3: tez_outer_bridge → GW_M (%call_evm)
     #4: alias(evm_bridge) → tez_inner (%run)          (re-entrant)
     #5: tez_inner → tez_inner (%_incrementWitness)     (re-entrant)
     #6: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (post)
     #7: tez_main → tez_main (%_incrementWitness) (final) *)
  Check.(
    (List.length internals = 8)
      int
      ~error_msg:"Expected 8 internal operations, got %L") ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#0")
    ~expected_nonce:0
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  unit

(* EVM→TEZ→EVM→TEZ triple crossing — nested CRAC that re-enters TEZ.
 * Mirrors test_crac_receipt_tez_evm_tez but starts from EVM, exercising
 * a nested CRAC within the same runtime (TEZ appears twice).
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge] ~CRAC~>
 *         EVM[evm_bridge_2] ~CRAC~> TEZ[tez_inner]
 *
 *  We verify:
 *   - Michelson runtime block: 1 manager op with 1 content item,
 *     handler source, KT1 alias destination, synthetic fields = 0,
 *     internal transaction(s) all "applied", CRAC event with
 *     tag "crac" and CRAC-ID "1-0", event status "applied"
 *   - EVM block: 1 transaction (original tx; re-entrant TEZ→EVM leg
 *     is internal per RFC principle 6)
 *)
let test_crac_receipt_evm_tez_evm_tez =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ->EVM->TEZ triple crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-ETET" in
  (* Build chain: evm_main -> evm_bridge_1 ~> tez_bridge ~> evm_bridge_2 ~> tez_inner *)
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_2_addr) = evm_bridge_2 in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge_2 in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_1_addr) = evm_bridge_1 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_1, false)] ()
  in
  let*@ evm_bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_1_addr sequencer
  in
  let*@ evm_bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_2_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  (* Verify execution reached both ends *)
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  (* ── Michelson runtime side ──────────────────────────────────── *)
  Log.debug ~prefix "Verify Michelson runtime receipt" ;
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* EVM→TEZ→EVM→TEZ: 7 internal ops — event + 6 transactions.
     #0: event (CRAC-ID "1-0")
     #1: alias(evm_bridge_1) → tez_bridge (%run)
     #2: tez_bridge → tez_bridge (%_incrementWitness) (pre)
     #3: tez_bridge → GW_M (%call_evm)
     #4: alias(evm_bridge_2) → tez_inner (%run)          (re-entrant)
     #5: tez_inner → tez_inner (%_incrementWitness)       (re-entrant)
     #6: tez_bridge → tez_bridge (%_incrementWitness) (post) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 7)
      int
      ~error_msg:"Expected 7 internal operations, got %L") ;
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:evm_bridge_1_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_bridge_2_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  (* ── EVM side ──────────────────────────────────────────────── *)
  (* 1 original EVM tx; re-entrant TEZ→EVM leg is internal per RFC principle 6 *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* CRAC revert receipt — RFC Example 4.
 *
 *  When the TEZ callee reverts, the Michelson block should still contain
 *  a manager operation with status "failed" and internal operations
 *  (including the CRAC event as first internal op).
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter] → REVERT
 *)
let test_crac_receipt_evm_to_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ revert produces failed receipt"
    ~tags:["crac_receipt"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-REV" in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_kt1)) = tez_reverter in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  (* Counters stay at 0 — execution was reverted *)
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  (* ── Michelson runtime side: failed receipt ──────────────────── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  (* RFC §"Incoming CRAC with backtracking": failed CRAC should carry
     internal operations with backtracked / failed / skipped statuses
     so indexers can see what was attempted before the failure. *)
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"failed"
      top
  in
  (* 3 internal ops: CRAC event + alias→reverter call + reverter's
     internal _revert call (RFC Example 4: all sub-calls are shown) *)
  Check.(
    (List.length internals = 3)
      int
      ~error_msg:"Expected 3 internal operations, got %L") ;
  (* ── Internal #0: CRAC event (always emitted, even on failure;
     status is "backtracked" because the downstream transfer
     failed — matching real Tezos protocol backtracking semantics) ── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(E_bridge) → tez_reverter (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_reverter → tez_reverter (%_revert) — failed
     The reverter contract's internal call that triggers the failure. *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_reverter_kt1
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 2) ;
  unit

(* Two failing EVM->TEZ CRACs from the SAME EVM transaction, each caught
 * by the EVM caller's try/catch.  Per RFC principle 6 (one top-level per
 * runtime per CRAC-ID), the Michelson runtime block must merge both
 * failures into a SINGLE manager operation.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[bridge_1] ~CRAC~> TEZ[tez_reverter_1] -> REVERT
 *     |-> (Catch) EVM[bridge_2] ~CRAC~> TEZ[tez_reverter_2] -> REVERT
 *)
let test_crac_receipt_two_failed_independent =
  register_crac_runner_test
    ~title:"CRAC: two EVM->TEZ failures from same tx produce one Michelson op"
    ~tags:["crac_receipt"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-2FAIL" in
  let* tez_reverter_1 = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_1_kt1)) = tez_reverter_1 in
  let* tez_reverter_2 = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_2_kt1)) = tez_reverter_2 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_2 in
  let (`Evm_runner bridge_2_addr) = bridge_2 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(bridge_1, true); (bridge_2, true)]
      ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_1_addr sequencer
  in
  let*@ bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_2_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  (* EVM main caught both failures — counter incremented per iteration
     plus once at the end (3), and catches incremented twice (2). *)
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:3
      evm_main
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_1
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_2
  in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  (* RFC principle 6: a single CRAC-ID per top-level EVM tx → a single
     top-level Michelson operation, even when sub-calls fail. *)
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:
        "Expected 1 Michelson operation (RFC merging of failed CRACs), got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"failed"
      top
  in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* 5 internal ops: event + (run failed, _revert failed) for each bridge. *)
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal operations, got %L") ;
  (* ── Internal #0: CRAC event — single event for the merged top-level. *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(bridge_1) → tez_reverter_1 (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_reverter_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_reverter_1 → tez_reverter_1 (%_revert) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_reverter_1_kt1
    ~expected_destination:tez_reverter_1_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(bridge_2) → tez_reverter_2 (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_reverter_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_reverter_2 → tez_reverter_2 (%_revert) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_reverter_2_kt1
    ~expected_destination:tez_reverter_2_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 4) ;
  unit

(* Interleaved successful + failed CRACs from the SAME EVM tx, with the
 * pattern S, F, S, F, S, F, S, F (4 successes and 4 failures alternating).
 *
 * Without the execution-order fix, `drain_pending_crac_receipts`
 * concatenated `failed ++ pending`, so merged internals came out in
 * bucket order (all failures first, then successes) rather than the
 * order they actually ran in — and `renumber_nonces` then assigned
 * block-global nonces to those internals in that wrong order.  Also,
 * when the first-executed receipt was a failed CRAC the top-level
 * `ContentResult` inherited `Failed` even though later CRACs succeeded,
 * producing the L1-invalid "Applied internals under Failed parent"
 * combination.
 *
 *    EVM[evm_main]
 *     |->         EVM[bridge_ok]   ~CRAC~> TEZ[tez_ok]        -> OK
 *     |-> (Catch) EVM[bridge_fail] ~CRAC~> TEZ[tez_reverter]  -> REVERT
 *     |->         EVM[bridge_ok]   ~CRAC~> TEZ[tez_ok]        -> OK
 *     |-> (Catch) EVM[bridge_fail] ~CRAC~> TEZ[tez_reverter]  -> REVERT
 *     ... (4 repetitions of the OK / FAIL pair)
 *
 * Expected merged receipt:
 *  - Single top-level Michelson op (RFC principle 6), destination =
 *    alias(sender).
 *  - Top-level status = `applied` (forced because at least one CRAC
 *    succeeded; without the fix it would be `failed` whenever the first
 *    CRAC was the failing one).
 *  - 17 internals: 1 CRAC event + 4×(ok run, _incrementWitness)
 *    + 4×(fail run, _revert), in execution order.  The event is
 *    `applied` because the first-executed receipt in the sort is a
 *    success.  Applied/failed statuses alternate in pairs of two,
 *    matching the per-CRAC outcome.
 *)
let test_crac_receipt_interleaved_failed_pending =
  register_crac_runner_test
    ~title:
      "CRAC: SFSFSFSF interleaved CRACs merge in execution order with applied \
       top-level"
    ~tags:["crac_receipt"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-SFSF" in
  let* tez_ok = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_ok_kt1)) = tez_ok in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_kt1)) = tez_reverter in
  let* bridge_ok = EvmCrossRuntimeRunnerTez.deploy_and_init tez_ok in
  let (`Evm_runner bridge_ok_addr) = bridge_ok in
  let* bridge_fail = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let (`Evm_runner bridge_fail_addr) = bridge_fail in
  (* 4 S / F pairs, alternating. *)
  let n_pairs = 4 in
  let callees =
    List.concat
      (List.init n_pairs (fun _ -> [(bridge_ok, false); (bridge_fail, true)]))
  in
  let* evm_main = EvmMultiRunCaller.deploy_and_init ~callees () in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_ok_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_ok_addr sequencer
  in
  let*@ bridge_fail_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_fail_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  (* 4 catches absorbed the failing CRACs; the 4 successful CRACs went
     through.  The EVM counter increments once per callee + once after
     the loop = 2*n_pairs + 1 = 9. *)
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:n_pairs
      ~expected_counter:((2 * n_pairs) + 1)
      evm_main
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:n_pairs tez_ok in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  (* RFC principle 6: a single CRAC-ID per top-level EVM tx → a single
     top-level Michelson operation, regardless of per-CRAC outcomes. *)
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 merged Michelson operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  (* Post-fix: any successful CRAC in the merge forces the top-level to
     `applied` (otherwise we'd have `applied` internals under a `failed`
     parent, which L1 forbids). *)
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* 1 event + 4*(ok pair) + 4*(fail pair) = 1 + 16 = 17 internals. *)
  let expected_internals = 1 + (4 * n_pairs) in
  Check.(
    (List.length internals = expected_internals)
      int
      ~error_msg:
        (sf "Expected %d internal operations, got %%L" expected_internals)) ;
  (* ── Internal #0: CRAC event.  First-executed receipt in the sort is
        a success, so the event comes from `build_crac_receipt` with
        `Applied` status (in contrast to the all-failed case, where it
        would be `Backtracked` from `build_failed_crac_receipt`). *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Each S/F pair contributes 4 internals, in execution order.
        Block-global nonce starts at 1 (the event takes nonce 0). *)
  List.iter
    (fun pair_ix ->
      let base_nonce = 1 + (pair_ix * 4) in
      (* S: alias(bridge_ok) → tez_ok (%run, applied)
         followed by tez_ok self-call (%_incrementWitness, applied). *)
      check_crac_internal_transaction
        ~prefix
        ~expected_nonce:base_nonce
        ~expected_source:bridge_ok_alias
        ~expected_destination:tez_ok_kt1
        ~expected_entrypoint:"run"
        ~expected_status:"applied"
        (List.nth internals base_nonce) ;
      check_crac_internal_transaction
        ~prefix
        ~expected_nonce:(base_nonce + 1)
        ~expected_source:tez_ok_kt1
        ~expected_destination:tez_ok_kt1
        ~expected_entrypoint:"_incrementWitness"
        ~expected_status:"applied"
        (List.nth internals (base_nonce + 1)) ;
      (* F: alias(bridge_fail) → tez_reverter (%run, failed)
         followed by tez_reverter self-call (%_revert, failed). *)
      check_crac_internal_transaction
        ~prefix
        ~expected_nonce:(base_nonce + 2)
        ~expected_source:bridge_fail_alias
        ~expected_destination:tez_reverter_kt1
        ~expected_entrypoint:"run"
        ~expected_status:"failed"
        (List.nth internals (base_nonce + 2)) ;
      check_crac_internal_transaction
        ~prefix
        ~expected_nonce:(base_nonce + 3)
        ~expected_source:tez_reverter_kt1
        ~expected_destination:tez_reverter_kt1
        ~expected_entrypoint:"_revert"
        ~expected_status:"failed"
        (List.nth internals (base_nonce + 3)))
    (List.init n_pairs Fun.id) ;
  unit

(* CRAC at tx_index > 0: inject a dummy ETH transfer into the mempool
 * before the CRAC tx, so both land in the same block and the CRAC tx
 * gets tx_index = 1 → CRAC-ID = "1-1".
 *
 *   [dummy ETH transfer]  (tx_index 0)
 *   EVM[evm_runner] |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]  (tx_index 1)
 *)
let test_crac_receipt_evm_not_first_tx =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ CRAC-ID reflects tx_index > 0"
    ~tags:["crac_receipt"; "crac_id"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-IDX" in
  let* tez_runner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_runner_kt1)) = tez_runner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  (* Inject a dummy ETH transfer into the mempool (no block produced).
     We use bootstrap_accounts.(1) as sender to avoid nonce conflicts
     with the main sender (bootstrap_accounts.(0)). *)
  let dummy_sender = Eth_account.bootstrap_accounts.(1) in
  let* raw_dummy =
    Cast.craft_tx
      ~source_private_key:dummy_sender.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas:21_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:dummy_sender.address
      ()
  in
  let*@ _dummy_hash = Rpc.send_raw_transaction ~raw_tx:raw_dummy sequencer in
  (* Now call_run which sends the CRAC tx and produces a block.
     Both the dummy and CRAC tx land in the same block. *)
  let* _ = EvmRunner.call_run evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  (* Verify the CRAC-ID has tx_index > 0 *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* Same structure as simple EVM→TEZ: 3 internal ops *)
  Check.(
    (List.length internals = 3)
      int
      ~error_msg:"Expected 3 internal operations, got %L") ;
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-1"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_runner_kt1
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  unit

(* TEZ→EVM CRAC at tx_index > 0: inject a dummy Michelson transfer and
 * the CRAC call into the same block via the Tezlink RPC, so the
 * dummy occupies Michelson tx_index 0 and the CRAC gets tx_index 1 →
 * CRAC-ID = "0-1".
 *
 * The kernel computes the fake EVM transaction hash as:
 *   keccak256("CRAC-TX" || block_number_be256 || crac_id_string)
 * Since there is no "crac" event on the Michelson runtime side for TEZ-originated
 * CRACs (origin_runtime = 0), we verify the CRAC-ID by matching the
 * fake transaction hash in the EVM block.
 *
 *   TEZ[dummy transfer]   (Michelson tx_index 0)
 *   TEZ[tez_runner] |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]  (Michelson tx_index 1)
 *)
let test_crac_receipt_tez_not_first_tx =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM CRAC-ID reflects tx_index > 0"
    ~tags:["crac_receipt"; "crac_id"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TIDX" in
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  (* Create a Tezlink client for direct L2 Michelson injection. *)
  let tezlink_endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint:tezlink_endpoint () in
  (* Inject a dummy Michelson transfer into the mempool (no block produced).
     Per the RFC, CRAC-ID tx_index is per-runtime, so this dummy
     Michelson tx occupies tx_index 0 in the Michelson block. *)
  let* dummy_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:(tez_counter ())
            ~gas_limit:100_000
            ~storage_limit:1000
            ~source
            (transfer ~amount:0 ());
        ])
      client
  in
  let* _dummy_hash = Operation.inject ~dont_wait:true dummy_op client_tezlink in
  (* Inject the CRAC call into the mempool (no block produced yet). *)
  let (`Tez_runner (_, dest)) = tez_runner in
  let* arg = Client.convert_data_to_json ~data:"Unit" client in
  let* crac_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:(tez_counter ())
            ~gas_limit:100_000
            ~storage_limit:1000
            ~source
            (call ~dest ~arg ~entrypoint:"run" ~amount:0 ());
        ])
      client
  in
  let* _crac_hash = Operation.inject ~dont_wait:true crac_op client_tezlink in
  (* Produce one block containing both Michelson transactions. *)
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  (* Verify the fake CRAC tx hash matches CRAC-ID "0-1". *)
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-1" block ;
  unit

(* Mixed block: one EVM tx (tx_index 0) followed by one TezosDelayed
 * that CRACs into EVM (tx_index 1 globally, but michelson_index 0).
 *
 * Verifies that the CRAC-ID for the Tezos-originated CRAC uses the
 * per-runtime Michelson index (0), not the global EVM index (1).
 *
 *   EVM[dummy self-transfer]   (EVM tx_index 0)
 *   TEZ[tez_runner] |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *       (Michelson tx_index 0 → CRAC-ID "0-0")
 *)
let test_crac_receipt_evm_then_tez_same_block =
  register_crac_runner_test
    ~title:
      "CRAC: EVM tx before TEZ->EVM in same block — CRAC-ID uses \
       michelson_index"
    ~tags:["crac_receipt"; "crac_id"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-MIX" in
  (* Deploy TEZ→EVM CRAC chain *)
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  (* Step 1: Craft a dummy EVM self-transfer and send to mempool
     (no block produced yet).  Use bootstrap_accounts.(1) to avoid
     nonce conflicts with the main sender. *)
  let dummy_sender = Eth_account.bootstrap_accounts.(1) in
  let* raw_dummy =
    Cast.craft_tx
      ~source_private_key:dummy_sender.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas:21_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:dummy_sender.address
      ()
  in
  let*@ _dummy_hash = Rpc.send_raw_transaction ~raw_tx:raw_dummy sequencer in
  (* Step 2: Inject the TEZ→EVM CRAC call via the Tezlink RPC
     (no block produced yet). *)
  let tezlink_endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint:tezlink_endpoint () in
  let (`Tez_runner (_, tez_runner_dest)) = tez_runner in
  let* arg = Client.convert_data_to_json ~data:"Unit" client in
  let* crac_op =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~counter:(tez_counter ())
            ~gas_limit:100_000
            ~storage_limit:1000
            ~source
            (call ~dest:tez_runner_dest ~arg ~entrypoint:"run" ~amount:0 ());
        ])
      client
  in
  let* _crac_hash = Operation.inject ~dont_wait:true crac_op client_tezlink in
  (* Step 3: Produce one block containing both transactions. *)
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_inner in
  (* Step 4: Verify the fake CRAC tx hash matches CRAC-ID "0-0".
     The Tezos operation is the FIRST (and only) Michelson operation
     in the block, so michelson_index = 0 → CRAC-ID "0-0".
     The global EVM index of this transaction is 1 (after the dummy
     EVM tx at index 0), but the CRAC-ID must use the per-runtime index.
     The block contains 2 transactions: the dummy EVM tx and the fake CRAC tx. *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:2 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  unit

(* RFC Example 1: Michelson → EVM (simple success).
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *
 *  Michelson block: standard transaction from M_0 to the gateway. No CRAC event
 *  (Michelson is the originating runtime).
 *  EVM block: fake CRAC transaction from Handler_E with CRAC event log "0-0".
 *)
let test_crac_receipt_tez_to_evm =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM receipt matches RFC structure"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-T2E" in
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let* () = TezRunner.call_run tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_inner in
  (* ── Michelson runtime side: no CRAC event (originating) ──────── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.(
    (top_status = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  (* Outgoing CRAC from TEZ: no CRAC event in the originating runtime.
     Expected internal ops (all transactions):
     #0: tez_main → tez_main (%_incrementWitness)
     #1: tez_main → tez_bridge (%run)
     #2: tez_bridge → tez_bridge (%_incrementWitness) (pre)
     #3: tez_bridge → GW_M (%call_evm)
     #4: tez_bridge → tez_bridge (%_incrementWitness) (post)
     #5: tez_main → tez_main (%_incrementWitness) (final) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 6)
      int
      ~error_msg:"Expected 6 internal operations, got %L") ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#0")
    ~expected_nonce:0
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* ── EVM side: fake CRAC transaction with event ─────────────── *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* RFC Example 9: M→E→M→E triple crossing.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_outer_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~>
 *         TEZ[tez_inner_bridge] ~CRAC~> EVM[evm_inner]
 *
 *  Michelson block: no CRAC event (originating runtime). Internal ops include
 *  re-entrant alias(E_1) → tez_inner_bridge and tez_inner_bridge → GW_M.
 *  EVM block: 1 fake CRAC tx with CRAC-ID "0-0".
 *)
let test_crac_receipt_tez_evm_tez_evm =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM->TEZ->EVM triple crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TETE" in
  (* Build chain: tez_main → tez_outer_bridge ~> evm_bridge ~> tez_inner_bridge ~> evm_inner *)
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_inner_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let (`Tez_runner (_, tez_inner_bridge_kt1)) = tez_inner_bridge in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner_bridge in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* tez_outer_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  let (`Tez_runner (_, tez_outer_bridge_kt1)) = tez_outer_bridge in
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_outer_bridge] () in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  (* The 4-hop chain (TEZ→EVM→TEZ→EVM) requires significantly more gas
     than a simple crossing because each runtime crossing partitions the
     gas budget: Tezos milligas ÷100 → EVM gas, then ×100 → milligas,
     then ÷100 again. Each hop also consumes gas for interpretation,
     alias generation, manager operations and EVM base transaction costs. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  (* Verify execution reached both ends *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_inner in
  (* ── Michelson runtime side: no CRAC event (originating) ──── *)
  Log.debug ~prefix "Verify Michelson runtime receipt" ;
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.(
    (top_status = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  (* No CRAC event in the originating runtime. 10 internal transactions:
     #0: tez_main → tez_main (%_incrementWitness)
     #1: tez_main → tez_outer_bridge (%run)
     #2: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (pre)
     #3: tez_outer_bridge → GW_M (%call_evm)
     #4: alias(evm_bridge) → tez_inner_bridge (%run)       (re-entrant)
     #5: tez_inner_bridge → tez_inner_bridge (%_incrementWitness) (pre, re-ent)
     #6: tez_inner_bridge → GW_M (%call_evm)               (re-entrant)
     #7: tez_inner_bridge → tez_inner_bridge (%_incrementWitness) (post, re-ent)
     #8: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (post)
     #9: tez_main → tez_main (%_incrementWitness) (final) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 10)
      int
      ~error_msg:"Expected 10 internal operations, got %L") ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#0")
    ~expected_nonce:0
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  (* ── EVM side: fake CRAC transaction ───────────────────────── *)
  (* 1 fake CRAC tx; re-entrant legs are internal per RFC principle 6 *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  unit

(* 5-crossing chain receipt — EVM-originated CRAC chain with 5 hops.
 *
 *    EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~>
 *    TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_leaf]
 *
 *  Per the RFC, Michelson block: 1 manager op with handler source,
 *  destination = alias(sender), CRAC event with CRAC-ID "1-0", and
 *  all TEZ execution flattened as internal ops (re-entrant legs included).
 *  EVM block: 1 transaction (all re-entrant EVM legs are internal
 *  per RFC principle 6).
 *)
let test_crac_receipt_evm_5_crossing_chain =
  register_crac_runner_test
    ~title:"CRAC: EVM 5-crossing chain receipt"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-5CHAIN" in
  let* tez_leaf = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_leaf_kt1)) = tez_leaf in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let (`Evm_runner evm_e_addr) = evm_e in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let (`Tez_runner (_, tez_d_kt1)) = tez_d in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let (`Evm_runner evm_c_addr) = evm_c in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let (`Tez_runner (_, tez_b_kt1)) = tez_b in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  let (`Evm_runner evm_a_addr) = evm_a in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ evm_a_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_a_addr sequencer
  in
  let*@ evm_c_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_c_addr sequencer
  in
  let*@ evm_e_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_e_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_a in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_e in
  (* ── Michelson runtime side ──────────────────────────────────── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* 11 internal ops: event + 10 transactions.
     Per RFC Example 8, internal operations follow execution order:
     outermost CRAC first, re-entrant ops interleaved at gateway call
     sites, post-gateway ops resume after the inner chain returns.
     #0: event (CRAC-ID "1-0")
     #1: alias(evm_a) → tez_b (%run) [outermost CRAC]
     #2: tez_b → tez_b (%_incrementWitness) [pre]
     #3: tez_b → GW_M (%call_evm) [outgoing to evm_c]
     #4: alias(evm_c) → tez_d (%run) [middle CRAC, interleaved]
     #5: tez_d → tez_d (%_incrementWitness) [pre]
     #6: tez_d → GW_M (%call_evm) [outgoing to evm_e]
     #7: alias(evm_e) → tez_leaf (%run) [deepest CRAC, interleaved]
     #8: tez_leaf → tez_leaf (%_incrementWitness)
     #9: tez_d → tez_d (%_incrementWitness) [post, resumes after GW]
     #10: tez_b → tez_b (%_incrementWitness) [post, resumes after GW] *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 11)
      int
      ~error_msg:"Expected 11 internal operations, got %L") ;
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Outermost CRAC: evm_a → tez_b ────────────────────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:evm_a_alias
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_b_kt1
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_b_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Middle CRAC: evm_c → tez_d (interleaved at GW call site) *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_c_alias
    ~expected_destination:tez_d_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_d_kt1
    ~expected_destination:tez_d_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_d_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  (* ── Deepest CRAC: evm_e → tez_leaf (interleaved at GW call site) *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:evm_e_alias
    ~expected_destination:tez_leaf_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_source:tez_leaf_kt1
    ~expected_destination:tez_leaf_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  (* ── Post-gateway continuations (stack unwinding order) ─────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_d_kt1
    ~expected_destination:tez_d_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#10")
    ~expected_nonce:10
    ~expected_source:tez_b_kt1
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  (* ── EVM side ──────────────────────────────────────────────── *)
  (* 1 original EVM tx; re-entrant TEZ→EVM legs are internal per RFC principle 6 *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* TEZ-originated mixed calls with interleaved CRAC and direct calls
 * to the same contract.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_inner]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *     |-> TEZ[tez_inner]
 *
 *  Michelson block: 1 manager op (TEZ-originated, no CRAC event).
 *  All execution, including the re-entrant EVM→TEZ CRAC back into
 *  tez_inner, should appear as internal operations within the
 *  original TEZ transaction.
 *  EVM block: 1 fake CRAC tx with CRAC-ID "0-0".
 *)
let test_crac_receipt_tez_mixed_calls_with_crac =
  register_crac_runner_test
    ~title:"CRAC: TEZ mixed calls with interleaved CRAC receipt"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TMIX" in
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_inner; tez_bridge; tez_inner] ()
  in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* () = TezRunner.call_run tez_main in
  (* Counter checks:
     tez_main: 4 increments (one before each of 3 callees + final)
     tez_inner: 3 increments (called twice directly + once via re-entrant CRAC)
     tez_bridge: 2 increments (pre + post gateway call) *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_inner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  (* ── Michelson runtime side: no CRAC event (originating) ──── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.(
    (top_status = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  (* No CRAC event in the originating runtime.  14 internal transactions.
     Operations appear in call order (parent call first, then sub-ops),
     matching the RFC examples.  Re-entrant CRAC ops are correctly
     interleaved after the gateway call.
     #0: tez_main → tez_main (%_incrementWitness) [before 1st callee]
     #1: tez_main → tez_inner (%run) [1st callee call]
     #2: tez_inner → tez_inner (%_incrementWitness) [sub-op of 1st callee]
     #3: tez_main → tez_main (%_incrementWitness) [before 2nd callee]
     #4: tez_main → tez_bridge (%run) [2nd callee call]
     #5: tez_bridge → tez_bridge (%_incrementWitness) [sub-op: pre]
     #6: tez_bridge → GW_M (%call_evm) [sub-op: outgoing CRAC]
     #7: alias(evm_bridge) → tez_inner (%run) [re-entrant CRAC]
     #8: tez_inner → tez_inner (%_incrementWitness) [re-entrant sub-op]
     #9: tez_bridge → tez_bridge (%_incrementWitness) [sub-op: post]
     #10: tez_main → tez_main (%_incrementWitness) [before 3rd callee]
     #11: tez_main → tez_inner (%run) [3rd callee call]
     #12: tez_inner → tez_inner (%_incrementWitness) [sub-op of 3rd callee]
     #13: tez_main → tez_main (%_incrementWitness) [final] *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 14)
      int
      ~error_msg:"Expected 14 internal operations, got %L") ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#0")
    ~expected_nonce:0
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#10")
    ~expected_nonce:10
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#11")
    ~expected_nonce:11
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#12")
    ~expected_nonce:12
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 12) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#13")
    ~expected_nonce:13
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 13) ;
  (* ── EVM side: fake CRAC transaction ───────────────────────── *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  unit

(** Generic call() precompile: EVM calls TEZ via HTTP, succeeds.
 *
 *    EVM[evm_caller] --call()--> TEZ[tez_runner]
 *
 *)
let test_crac_http_call_success =
  register_crac_runner_test
    ~title:"CRAC: generic call() EVM to TEZ success"
    ~tags:["http_call"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM caller using generic call()" ;
  let* evm_caller = EvmCracHttpCall.deploy_and_init tez_runner in
  Log.debug ~prefix "Call run() on EVM caller" ;
  let* _ = EvmRunner.call_run evm_caller in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmCracHttpCall.check_storage ~expected_counter:2 evm_caller in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  unit

(** Generic call() precompile: TEZ target reverts, 4xx is caught.
 *
 *    EVM[evm_caller] --call()--> TEZ[tez_reverter]
 *                                |-> REVERT
 *    runCatch() catches the revert, execution continues.
 *
 *)
let test_crac_http_call_catch_revert =
  register_crac_runner_test
    ~title:"CRAC: generic call() 4xx is catchable"
    ~tags:["http_call"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP" in
  Log.debug ~prefix "Originate TEZ reverter" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM caller using generic call()" ;
  let* evm_caller = EvmCracHttpCall.deploy_and_init tez_reverter in
  Log.debug ~prefix "Call runCatch() on EVM caller" ;
  let* _ = EvmCracHttpCall.call_run_catch evm_caller in
  Log.debug ~prefix "Verify counters: catches=1, count=2 (pre + post catch)" ;
  let* () =
    EvmCracHttpCall.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_caller
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** Generic call() precompile: TEZ target OOGs, parent catches and continues.
 *
 *    EVM[evm_caller] --call(gasLimit)--> TEZ[gas_burner]
 *                                        |-> OOG (loops forever)
 *    runCatchWithGasLimit() forwards limited gas to the subcall.
 *    The subcall OOGs (429), the catch triggers, and the parent
 *    continues with its remaining gas.
 *
 *)
let test_crac_http_call_catch_oog =
  register_crac_runner_test
    ~title:"CRAC: generic call() subcall OOG is catchable"
    ~tags:["http_call"; "oog"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP" in
  Log.debug ~prefix "Originate TEZ gas burner" ;
  let* tez_burner = TezGasBurner.originate () in
  Log.debug ~prefix "Deploy EVM caller using generic call()" ;
  let* evm_caller = EvmCracHttpCall.deploy_and_init tez_burner in
  Log.debug ~prefix "Call runCatchWithGasLimit() on EVM caller" ;
  let* _ =
    EvmCracHttpCall.call_run_catch_with_gas_limit ~gas_limit:50000 evm_caller
  in
  Log.debug ~prefix "Verify counters: catches=1, count=2 (pre + post catch)" ;
  let* () =
    EvmCracHttpCall.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_caller
  in
  unit

(* L1 vs TezosX receipt comparison helpers *)

let extract_entrypoint_status internals =
  List.map
    (fun iop ->
      let kind = JSON.(iop |-> "kind" |> as_string) in
      let ep =
        match kind with
        | "transaction" ->
            JSON.(
              iop |-> "parameters" |-> "entrypoint" |> as_string_opt
              |> Option.value ~default:"default")
        | _ -> kind
      in
      let status = JSON.(iop |-> "result" |-> "status" |> as_string) in
      (ep, status))
    internals

let fetch_tezosx_top_content sequencer =
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  match List.rev JSON.(ops |> as_list) with
  | [] -> Test.fail "fetch_tezosx_top_content: no manager operations in block"
  | last_op :: _ -> (
      match List.rev JSON.(last_op |-> "contents" |> as_list) with
      | [] ->
          Test.fail
            "fetch_tezosx_top_content: manager operation has no contents"
      | top :: _ -> return top)

(** Deploy the 6-contract nested-FAILWITH tree, call A.run, and return
    the top-level status and [(entrypoint, status)] list.

    [originate] and [call_run] abstract over L1 vs TezosX.
    [fetch_top_content] returns the top-level content JSON from the
    most recent block. *)
let run_nested_failwith_scenario ~name ~originate ~call_run ~fetch_top_content =
  Log.info "=== Running nested-FAILWITH scenario on %s ===" name ;
  let* addr_f = originate ~revert:false ~callees:[] () in
  let* addr_e = originate ~revert:false ~callees:[] () in
  let* addr_d = originate ~revert:false ~callees:[] () in
  let* addr_c = originate ~revert:true ~callees:[addr_d] () in
  let* addr_b = originate ~revert:false ~callees:[addr_c; addr_e] () in
  let* addr_a = originate ~revert:false ~callees:[addr_b; addr_f] () in
  Log.info
    "%s: A=%s B=%s C=%s D=%s E=%s F=%s"
    name
    addr_a
    addr_b
    addr_c
    addr_d
    addr_e
    addr_f ;
  let* () = call_run addr_a in
  let* top = fetch_top_content () in
  let status =
    JSON.(top |-> "metadata" |-> "operation_result" |-> "status" |> as_string)
  in
  let internals =
    JSON.(top |-> "metadata" |-> "internal_operation_results" |> as_list)
  in
  let pairs = extract_entrypoint_status internals in
  Log.info "%s: top=%s  %d internal ops" name status (List.length pairs) ;
  List.iteri
    (fun i (ep, st) -> Log.info "%s: #%d  ep=%-20s status=%s" name i ep st)
    pairs ;
  return (status, pairs)

(* L1 vs TezosX: deploy a nested call tree ending with FAILWITH and
 * compare the receipts.  L1 is the source of truth.
 *
 *  Contracts (all multi_run_caller):
 *    A (callees=[B,F], revert=false)
 *     |-> B (callees=[C,E], revert=false)
 *     |    |-> C (callees=[D], revert=true)
 *     |    |    |-> D (callees=[], revert=false)
 *     |    |    |-> FAILWITH
 *     |    |-> E (callees=[], revert=false)    (never reached)
 *     |-> F (callees=[], revert=false)         (never reached)
 *
 *  On L1, all executed internal ops get backtracked, the failing op
 *  is failed, and the rest are skipped.  TezosX should match.
 *)
let test_l1_vs_tezosx_nested_failwith_receipt =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"]
    @ List.map Tezosx_runtime.tag with_runtimes
    @ ["crac"; "receipt"; "l1_comparison"]
  in
  Setup.register_test
    ~__FILE__
    ~rpc_server:Evm_node.Resto
    ~title:"L1 vs TezosX: nested FAILWITH receipt comparison"
    ~time_between_blocks:Nothing
    ~tags
    ~kernel:Latest
    ~with_runtimes
    ~enable_dal:false
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer_setup protocol ->
  let client = sequencer_setup.client in
  let node = sequencer_setup.node in
  let source = Constant.bootstrap5 in
  let {Setup.sc_rollup_address; sc_rollup_node; l1_contracts; sequencer; _} =
    sequencer_setup
  in
  let tez_counter = ref 1 in
  let next_tez_counter () =
    let c = !tez_counter in
    incr tez_counter ;
    c
  in
  let l1_alias_counter = ref 0 in
  let* l1_status, l1_pairs =
    run_nested_failwith_scenario
      ~name:"L1"
      ~originate:
        (L1TezMultiRunCaller.originate
           ~client
           ~node
           ~protocol
           ~alias_counter:l1_alias_counter)
      ~call_run:(L1TezRunner.call_run ~client ~node)
      ~fetch_top_content:(fun () -> fetch_l1_manager_ops client)
  in
  let* tx_status, tx_pairs =
    run_nested_failwith_scenario
      ~name:"TezosX"
      ~originate:(fun ~revert ~callees () ->
        let* _hex, kt1 =
          TezMultiRunCaller.originate
            ~sc_rollup_address
            ~sc_rollup_node
            ~client
            ~l1_contracts
            ~sequencer
            ~source
            ~counter:(next_tez_counter ())
            ~protocol
            ~revert
            ~callees
            ()
        in
        return kt1)
      ~call_run:(fun addr ->
        TezRunner.call_run
          ~sc_rollup_address
          ~sc_rollup_node
          ~client
          ~l1_contracts
          ~sequencer
          ~source
          ~counter:(next_tez_counter ())
          addr)
      ~fetch_top_content:(fun () -> fetch_tezosx_top_content sequencer)
  in
  Log.info "Comparing L1 vs TezosX receipts" ;
  Check.(
    (l1_status = tx_status)
      string
      ~error_msg:"Top-level status: L1=%L, TezosX=%R") ;
  Check.(
    (List.length l1_pairs = List.length tx_pairs)
      int
      ~error_msg:"Internal op count: L1=%L, TezosX=%R") ;
  List.iteri
    (fun i ((l1_ep, l1_st), (tx_ep, tx_st)) ->
      Check.(
        (l1_ep = tx_ep)
          string
          ~error_msg:(sf "Internal #%d entrypoint: L1=%%L, TezosX=%%R" i)) ;
      Check.(
        (l1_st = tx_st)
          string
          ~error_msg:(sf "Internal #%d status: L1=%%L, TezosX=%%R" i)))
    (List.combine l1_pairs tx_pairs) ;
  unit

(** ABI encoding of uint256(42). *)
let abi_encoded_uint256_42 =
  "000000000000000000000000000000000000000000000000000000000000002a"

(** ABI encoding of uint256(99). *)
let abi_encoded_uint256_99 =
  "0000000000000000000000000000000000000000000000000000000000000063"

(** Callback fire-and-forget: call the gateway directly with None callback.
 *
 *    Gateway.call_evm ~> EVM[store_and_return]
 *
 *)
let test_crac_callback_fire_and_forget =
  register_crac_runner_test
    ~title:"CRAC: callback fire-and-forget (None)"
    ~tags:["callback"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Call gateway with None callback" ;
  let* () =
    Gateway.call_evm
      ~evm_target
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      ()
  in
  Log.debug ~prefix "Verify EVM storage" ;
  EvmStoreAndReturn.check_storage ~expected_value:42 evm_target

(** Callback receives EVM result bytes.
 *
 *    TEZ[callback_runner] --%run-->
 *        Gateway.call_evm(Some callback) ~> EVM[store_and_return]
 *        Gateway --> TEZ[callback_runner %on_result]
 *
 *)
let test_crac_callback_receives_result_bytes =
  register_crac_runner_test
    ~title:"CRAC: callback receives EVM result bytes"
    ~tags:["callback"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Originate callback runner" ;
  let* caller =
    TezCallbackRunnerEvm.originate
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Call callback runner" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 caller in
  Log.debug ~prefix "Verify EVM storage" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:42 evm_target in
  Log.debug ~prefix "Verify callback counter and result" ;
  let* () = TezCallbackRunnerEvm.check_counter ~expected_counter:3 caller in
  TezCallbackRunnerEvm.check_result
    ~expected_bytes:(Some abi_encoded_uint256_42)
    caller

(** Failing callback reverts entire operation group.
 *
 *    TEZ[failing_callback_runner] --%run-->
 *        Gateway.call_evm(Some callback) ~> EVM[store_and_return]
 *        Gateway --> TEZ[failing_callback_runner %on_result] --> FAILWITH
 *
 *)
let test_crac_callback_failure_reverts_all =
  register_crac_runner_test
    ~title:"CRAC: failing callback reverts entire operation group"
    ~tags:["callback"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Pre-store value 99 via fire-and-forget" ;
  let* () =
    Gateway.call_evm
      ~evm_target
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_99
      ()
  in
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Originate failing callback runner" ;
  let* failing_caller =
    TezCallbackRunnerEvm.originate
      ~failing:true
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Call failing callback runner" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 failing_caller in
  Log.debug ~prefix "Verify EVM storage unchanged (still 99)" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Verify callback counter and result reverted" ;
  let* () =
    TezCallbackRunnerEvm.check_counter ~expected_counter:0 failing_caller
  in
  TezCallbackRunnerEvm.check_result ~expected_bytes:None failing_caller

(** Callback behind a CRAC crossing: EVM bridge invokes the callback
 *  runner's [%run] via CRAC.  The callback runner then does its own
 *  gateway call with [Some callback] to the EVM target.
 *
 *    EVM[evm_bridge] ~CRAC~> TEZ[callback_runner] --%run-->
 *        Gateway.call_evm(Some callback) ~> EVM[store_and_return]
 *        Gateway --> TEZ[callback_runner %on_result]
 *
 *)
let test_crac_callback_behind_crac =
  register_crac_runner_test
    ~title:"CRAC: callback behind a CRAC crossing"
    ~tags:["callback"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Originate callback runner" ;
  let* callback_runner =
    TezCallbackRunnerEvm.originate
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Deploy EVM bridge to callback runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init callback_runner in
  Log.debug ~prefix "Call EVM bridge" ;
  let* _ = EvmRunner.call_run evm_bridge in
  Log.debug ~prefix "Verify EVM target storage" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:42 evm_target in
  Log.debug ~prefix "Verify callback runner counter and result" ;
  let* () =
    TezCallbackRunnerEvm.check_counter ~expected_counter:3 callback_runner
  in
  let* () =
    TezCallbackRunnerEvm.check_result
      ~expected_bytes:(Some abi_encoded_uint256_42)
      callback_runner
  in
  Log.debug ~prefix "Verify EVM bridge counter" ;
  EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge

(** TEZ caller invokes callback runner then reverts.  Both the callback
 *  result and the EVM side effects must be rolled back.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[callback_runner] --%run--> Gateway ~> EVM[store_and_return]
 *     |-> REVERT
 *
 *)
let test_crac_callback_tez_revert_rolls_back_callback =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert rolls back callback result"
    ~tags:["callback"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Pre-store value 99" ;
  let* () =
    Gateway.call_evm
      ~evm_target
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_99
      ()
  in
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Originate callback runner" ;
  let* callback_runner =
    TezCallbackRunnerEvm.originate
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Originate TEZ main (calls callback runner, then reverts)" ;
  let* tez_main =
    TezMultiRunCaller.originate ~revert:true ~callees:[callback_runner] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify EVM storage unchanged (still 99)" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Verify callback runner reverted" ;
  let* () =
    TezCallbackRunnerEvm.check_counter ~expected_counter:0 callback_runner
  in
  let* () =
    TezCallbackRunnerEvm.check_result ~expected_bytes:None callback_runner
  in
  Log.debug ~prefix "Verify TEZ main reverted" ;
  TezMultiRunCaller.check_storage ~expected_counter:0 tez_main

(** EVM caller catches a failing callback runner behind a CRAC crossing.
 *  The failing callback reverts the CRAC, but the EVM caller catches it
 *  and continues execution.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[failing_callback_runner]
 *                                              |-> Gateway ~> EVM[store_and_return]
 *                                              |-> %on_result --> FAILWITH
 *
 *)
let test_crac_callback_evm_catches_failing_callback_behind_crac =
  register_crac_runner_test
    ~title:"CRAC: EVM catches failing callback behind CRAC"
    ~tags:["callback"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Pre-store value 99" ;
  let* () =
    Gateway.call_evm
      ~evm_target
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_99
      ()
  in
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Originate failing callback runner" ;
  let* failing_runner =
    TezCallbackRunnerEvm.originate
      ~failing:true
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Deploy EVM bridge to failing callback runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init failing_runner in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify EVM target unchanged (still 99)" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:99 evm_target in
  Log.debug ~prefix "Verify EVM main caught the revert" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  Log.debug ~prefix "Verify callback runner and bridge reverted" ;
  let* () =
    TezCallbackRunnerEvm.check_counter ~expected_counter:0 failing_runner
  in
  let* () =
    TezCallbackRunnerEvm.check_result ~expected_bytes:None failing_runner
  in
  EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge

(** Callback runner alongside a normal CRAC runner in a mixed topology.
 *  An EVM multi-caller invokes both a normal EVM-to-TEZ bridge and an
 *  EVM bridge to a callback runner.  Both should succeed.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_normal] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_cb]     ~CRAC~> TEZ[callback_runner]
 *                                             |-> Gateway ~> EVM[store_and_return]
 *                                             |-> %on_result stores bytes
 *
 *)
let test_crac_callback_mixed_with_normal_runners =
  register_crac_runner_test
    ~title:"CRAC: callback runner alongside normal CRAC runner"
    ~tags:["callback"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-CB" in
  Log.debug ~prefix "Deploy EVM store-and-return" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Originate TEZ leaf (normal runner)" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Originate callback runner" ;
  let* callback_runner =
    TezCallbackRunnerEvm.originate
      ~method_sig:"store(uint256)"
      ~abi_params:abi_encoded_uint256_42
      evm_target
  in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_normal = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* evm_bridge_cb =
    EvmCrossRuntimeRunnerTez.deploy_and_init callback_runner
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_normal, false); (evm_bridge_cb, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify EVM target storage" ;
  let* () = EvmStoreAndReturn.check_storage ~expected_value:42 evm_target in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  let* () =
    TezCallbackRunnerEvm.check_counter ~expected_counter:3 callback_runner
  in
  let* () =
    TezCallbackRunnerEvm.check_result
      ~expected_bytes:(Some abi_encoded_uint256_42)
      callback_runner
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_normal
  in
  EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_cb

(** Verify that the [X-Tezos-Gas-Consumed] header reports cumulative
 *  Michelson gas even after internal operations reset the per-segment
 *  baseline.
 *
 *  == Scenario ==
 *
 *  Baseline:  EVM[wrapper] -> CRAC -> TEZ[simple counter]
 *  Burner:    EVM[wrapper] -> CRAC -> TEZ[michelson gas burner]
 *
 *  The Michelson gas burner runs 20000 KECCAK iterations (~32M milligas)
 *  then emits a self-call to [%%_noop].  The self-call is an internal
 *  operation that resets the per-segment gas baseline — the header must
 *  still report the full cumulative consumption.
 *
 *  == Assertions ==
 *
 *  Sanity: [G_burner > 5 * G_baseline] ensures the KECCAK gas
 *  dominates CRAC overhead, making the regression check robust.
 *
 *  Regression: [G_burner > threshold] proves the header carries the
 *  Michelson gas.  If the header reported only the delta since the
 *  last internal operation, G_burner would be ~ G_baseline (~60K).
 *  Instead the ~32M milligas flow through the header to the EVM
 *  receipt, pushing G_burner well above the threshold.
 *)
let test_crac_gas_header_michelson_burner =
  register_crac_runner_test
    ~title:"CRAC: X-Tezos-Gas-Consumed reports Michelson gas"
    ~tags:["gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-header" in

  (* Baseline: EVM->TEZ with simple counter (minimal Michelson gas) *)
  Log.debug ~prefix "[baseline] Originate simple TEZ counter" ;
  let* tez_simple = TezMultiRunCaller.originate () in
  Log.debug ~prefix "[baseline] Deploy EVM bridge to TEZ counter" ;
  let* evm_bridge_baseline =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_simple
  in
  Log.debug ~prefix "[baseline] Deploy EVM wrapper" ;
  let* runner_baseline =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_baseline, false)] ()
  in

  (* Burner: EVM->TEZ with Michelson gas burner + internal op *)
  Log.debug ~prefix "[burner] Originate Michelson gas burner" ;
  let* tez_burner = TezMichelsonGasBurner.originate () in
  Log.debug ~prefix "[burner] Deploy EVM bridge to gas burner" ;
  let* evm_bridge_burner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_burner
  in
  Log.debug ~prefix "[burner] Deploy EVM wrapper" ;
  let* runner_burner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_burner, false)] ()
  in

  (* Baseline: direct TEZ call to the gas burner to measure its
     true Michelson gas cost independently of CRAC. *)
  Log.debug ~prefix "Direct TEZ call to gas burner (measure milligas)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_burner in
  let* mg_direct = TezRunner.get_consumed_milligas () in

  (* Warmup calls (alias generation) *)
  Log.debug ~prefix "Warmup calls" ;
  let* _ = EvmRunner.call_run runner_baseline in
  let* _ = EvmRunner.call_run runner_burner in

  (* Measurement calls (aliases cached) *)
  Log.debug ~prefix "Measurement calls" ;
  let* g_baseline = EvmRunner.call_run runner_baseline in
  let* g_burner = EvmRunner.call_run runner_burner in

  let g_expected = Int64.of_int (mg_direct / 100) in
  let delta = Int64.sub g_burner g_baseline in
  Log.info
    ~prefix
    "mg_direct=%d  g_expected=%Ld  G_baseline=%Ld  G_burner=%Ld  delta=%Ld"
    mg_direct
    g_expected
    g_baseline
    g_burner
    delta ;

  (* delta = G_burner - G_baseline isolates the Michelson gas
     contribution by subtracting the CRAC overhead.  g_expected
     = mg_direct / 100 is the true Michelson gas cost measured
     independently via a direct TEZ call.

     The 4/5 lower bound allows for the small discrepancy between
     the direct TEZ call's consumed_milligas (which includes base
     operation overhead) and the CRAC header's gas report. *)

  (* Lower bound: the delta must include the Michelson gas. *)
  Check.(
    (delta > Int64.div (Int64.mul g_expected 4L) 5L)
      int64
      ~error_msg:
        "Delta (%L) should exceed 4/5 * g_expected (%R), proving \
         X-Tezos-Gas-Consumed header reports Michelson gas") ;

  (* Upper bound: 3/2 of g_expected.  If double-counted, the delta
     would be ~2x g_expected.  The 3/2 bound sits between the
     expected ratio and the double-counting signal. *)
  Check.(
    (delta < Int64.div (Int64.mul g_expected 3L) 2L)
      int64
      ~error_msg:
        "Delta (%L) should be below 3/2 * g_expected (%R): possible \
         double-counting in X-Tezos-Gas-Consumed") ;

  unit

(** Verify that the outer EVM receipt's gasUsed in an EVM->TEZ->EVM
 *  chain includes the inner callee gas.
 *
 *    EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~(%%call_evm)~> EVM[gas_burner]
 *
 *  The GasBurner contract writes 120 storage slots, consuming
 *  significant EVM gas.  The total gasUsed must exceed a baseline
 *  that only makes sense if the inner callee gas is accounted for.
 *)
let test_crac_gas_model_callee_gas_in_evm_receipt =
  register_crac_runner_test
    ~title:"CRAC: EVM receipt includes callee gas for %%call_evm"
    ~tags:["gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-evm" in
  Log.debug ~prefix "Deploy GasBurner (heavy EVM leaf)" ;
  let* gas_burner = EvmGasBurner.deploy () in

  (* Overhead: EVM->TEZ with no EVM callback (CRAC overhead only). *)
  Log.debug ~prefix "[overhead] Originate simple TEZ counter" ;
  let* tez_simple = TezMultiRunCaller.originate () in
  Log.debug ~prefix "[overhead] Deploy EVM bridge to TEZ counter" ;
  let* evm_bridge_overhead =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_simple
  in
  Log.debug ~prefix "[overhead] Deploy EVM runner" ;
  let* runner_overhead =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_overhead, false)] ()
  in

  (* Full chain: EVM->TEZ->EVM via %call_evm to GasBurner. *)
  Log.debug ~prefix "[chain] Originate TEZ bridge to GasBurner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate gas_burner in
  Log.debug ~prefix "[chain] Deploy EVM bridge to TEZ bridge" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "[chain] Deploy EVM runner calling the bridge" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in

  (* Pre-warm GasBurner storage. *)
  Log.debug ~prefix "Warm up GasBurner storage slots" ;
  let* _ = EvmRunner.call_run gas_burner in

  (* Reference: direct warm GasBurner call = true inner EVM cost. *)
  Log.debug ~prefix "Measure G_direct (warm GasBurner)" ;
  let* g_direct = EvmRunner.call_run gas_burner in

  (* Warmup calls (alias generation). *)
  Log.debug ~prefix "Warmup calls" ;
  let* _ = EvmRunner.call_run runner_overhead in
  let* _ = EvmRunner.call_run evm_runner in

  (* Measurement calls (aliases cached). *)
  Log.debug ~prefix "Measurement calls" ;
  let* g_overhead = EvmRunner.call_run runner_overhead in
  let* gas_with_callee = EvmRunner.call_run evm_runner in

  let delta = Int64.sub gas_with_callee g_overhead in
  Log.info
    ~prefix
    "G_direct=%Ld  G_overhead=%Ld  gas_with_callee=%Ld  delta=%Ld"
    g_direct
    g_overhead
    gas_with_callee
    delta ;

  (* delta = gas_with_callee - G_overhead isolates the inner EVM gas
     by subtracting the CRAC overhead. *)

  (* Lower bound: the delta must include the inner EVM gas. *)
  Check.(
    (delta > g_direct)
      int64
      ~error_msg:
        "Delta (%L) should exceed G_direct (%R), proving callee gas is \
         included in the EVM receipt") ;
  (* Upper bound: 3/2 of G_direct.  If the inner EVM gas were
     double-counted, the delta would be ~2x G_direct.  The 3/2
     bound sits between the two. *)
  Check.(
    (delta < Int64.div (Int64.mul g_direct 3L) 2L)
      int64
      ~error_msg:
        "Delta (%L) should be below 3/2 * G_direct (%R): possible \
         double-counting of callee gas in the EVM receipt") ;
  unit

(** Regression test: the Michelson gateway's %%call_evm must charge
 *  back the EVM callee gas in the Michelson operation receipt.
 *
 *    TEZ[tez_bridge] ~(%%call_evm)~> EVM[gas_burner]
 *
 *  Reads the [call_evm] internal operation's [consumed_milligas] from
 *  the tezlink RPC.  The GasBurner contract writes 120 storage slots,
 *  consuming significant EVM gas (~12M milligas when converted).
 *
 *  The [consumed_milligas] must include the inner EVM callee gas
 *  (~13M milligas), not just the alias resolution cost (~520K).
 *  Threshold: 2M milligas.
 *)
let test_crac_gas_model_callee_gas_in_receipt =
  register_crac_runner_test
    ~title:"CRAC: Michelson receipt includes callee gas for %%call_evm"
    ~tags:["gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-receipt" in
  Log.debug ~prefix "Deploy GasBurner (heavy EVM leaf)" ;
  let* gas_burner = EvmGasBurner.deploy () in

  (* Overhead: TEZ->EVM via %%call_evm to lightweight counter. *)
  Log.debug ~prefix "[overhead] Deploy simple EVM counter" ;
  let* evm_counter = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "[overhead] Originate TEZ bridge to counter" ;
  let* tez_bridge_overhead = TezCrossRuntimeRunnerEvm.originate evm_counter in

  (* Full chain: TEZ->EVM via %%call_evm to GasBurner. *)
  Log.debug ~prefix "[chain] Originate TEZ bridge to GasBurner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate gas_burner in

  (* Pre-warm GasBurner storage. *)
  Log.debug ~prefix "Warm up GasBurner storage slots" ;
  let* _ = EvmRunner.call_run gas_burner in

  (* Reference: direct warm GasBurner call = true inner EVM cost. *)
  Log.debug ~prefix "Measure G_direct (warm GasBurner)" ;
  let* g_direct = EvmRunner.call_run gas_burner in

  (* Warmup calls (alias generation). *)
  Log.debug ~prefix "Warmup calls (alias generation)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge_overhead in
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge in

  (* Measurement calls (aliases cached). *)
  Log.debug ~prefix "[overhead] Measure gateway consumed_milligas" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge_overhead in
  let* mg_overhead = TezRunner.get_gateway_consumed_milligas () in

  Log.debug ~prefix "[chain] Measure gateway consumed_milligas" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge in
  let* gateway_milligas = TezRunner.get_gateway_consumed_milligas () in

  let mg_expected = Int64.to_int g_direct * 100 in
  let mg_delta = gateway_milligas - mg_overhead in
  Log.info
    ~prefix
    "G_direct=%Ld  mg_expected=%d  mg_overhead=%d  gateway_milligas=%d  \
     mg_delta=%d"
    g_direct
    mg_expected
    mg_overhead
    gateway_milligas
    mg_delta ;

  (* mg_delta = gateway_milligas - mg_overhead isolates the inner EVM
     gas contribution (in milligas) by subtracting the gateway
     overhead.  mg_expected = G_direct * 100 is the expected inner
     EVM cost converted to milligas. *)

  (* Lower bound: the delta must include the inner EVM gas. *)
  Check.(
    (mg_delta > mg_expected * 4 / 5)
      int
      ~error_msg:
        "Milligas delta (%L) should exceed 4/5 * mg_expected (%R), proving \
         callee gas is charged in the Michelson receipt") ;
  (* Upper bound: 3/2 of mg_expected.  If the inner EVM gas were
     double-counted, the delta would be ~2x mg_expected. *)
  Check.(
    (mg_delta < mg_expected * 3 / 2)
      int
      ~error_msg:
        "Milligas delta (%L) should be below 3/2 * mg_expected (%R): possible \
         double-counting of callee gas in the Michelson receipt") ;
  unit

(** Gas accounting regression test for CRAC cross-runtime calls.
 *
 *  Verifies that inner EVM gas is correctly reported in the outer EVM
 *  receipt and in the TEZ milligas, without double-counting.
 *
 *  == Background ==
 *
 *  In an EVM->TEZ->EVM chain, the inner EVM execution (GasBurner)
 *  consumes gas that must flow back to the outer EVM receipt via the
 *  TEZ runtime's [X-Tezos-Gas-Consumed] response header.  The EVM
 *  precompile reads this header and charges it to the caller's gas.
 *
 *  == Scenarios ==
 *
 *  A. Direct EVM call to GasBurner (no CRAC)
 *     => G_direct: the true cost of the inner EVM work.
 *
 *  B. EVM->TEZ with no EVM callback
 *     EVM ~CRAC~> TEZ[counter]
 *     => G_overhead: pure CRAC overhead (precompile + aliases + TEZ).
 *
 *  C. EVM->TEZ->EVM via %%call_evm
 *     EVM ~CRAC~> TEZ ~(%%call_evm)~> EVM[gas_burner]
 *     => G_call_evm: overhead + inner EVM gas (if correctly charged).
 *
 *  D. EVM->TEZ->EVM via %%call (HTTP entrypoint)
 *     EVM ~CRAC~> TEZ ~(%%call)~> EVM[gas_burner]
 *     => G_call: same chain through the %%call entrypoint.
 *
 *  E/F. TEZ->EVM via %%call_evm / %%call
 *     TEZ ~(%%call_evm|%%call)~> EVM[gas_burner]
 *     => milligas from the tezlink operation receipt.
 *
 *  == Assertion strategy ==
 *
 *  The key invariant is: [C - B ≈ G_direct].  The delta between
 *  scenario C (with inner EVM) and B (without) must reflect the inner
 *  EVM cost, no more, no less.
 *
 *  To make upper bounds robust, we need the inner EVM cost to dominate
 *  the CRAC overhead.  Otherwise, overhead fluctuations could push the
 *  ratio [delta / G_direct] too close to the bound.  The sanity check
 *  [G_direct > 10 * G_overhead] guarantees that overhead is at most
 *  ~10%% of G_direct, leaving a wide gap between the normal ratio
 *  (~1.05x) and the double-counting signal (~2.05x).  The upper bound
 *  at 3/2 sits in the middle with comfortable margin on both sides.
 *
 *  If the sanity check fails, increase the number of storage slots in
 *  [GasBurner.sol] to make [G_direct] larger.  Do NOT loosen the
 *  upper bound — the whole point is to catch double-counting.
 *
 *)
let test_crac_gas_accounting_investigation =
  register_crac_runner_test
    ~title:"CRAC: gas accounting investigation (A/B/C/D scenarios)"
    ~tags:["gas"; "investigation"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-inv" in

  (* --- Deploy contracts ------------------------------------------------- *)
  Log.debug ~prefix "Deploy GasBurner" ;
  let* gas_burner = EvmGasBurner.deploy () in

  (* Scenario A: direct EVM call to GasBurner (no CRAC) *)

  (* Scenario B: EVM->TEZ, no callback
     Note: EvmMultiRunCaller wraps each bridge because calling the
     bridge directly (as the top-level contract) produces flat gasUsed
     that does not reflect the CRAC precompile's gas charges.  The
     wrapper's nested CALL ensures correct gas propagation. *)
  Log.debug ~prefix "[B] Originate simple TEZ counter (no EVM callback)" ;
  let* tez_simple = TezMultiRunCaller.originate () in
  Log.debug ~prefix "[B] Deploy EVM bridge to TEZ counter" ;
  let* evm_bridge_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_simple in
  Log.debug ~prefix "[B] Deploy EVM runner calling the bridge" ;
  let* runner_b =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_b, false)] ()
  in

  (* Scenario C: EVM->TEZ->EVM via %call_evm *)
  Log.debug ~prefix "[C] Originate TEZ bridge to GasBurner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate gas_burner in
  Log.debug ~prefix "[C] Deploy EVM bridge to TEZ bridge" ;
  let* evm_bridge_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "[C] Deploy EVM runner calling the bridge" ;
  let* runner_c =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_c, false)] ()
  in

  (* Scenario D: EVM->TEZ->EVM via %call (HTTP entrypoint) *)
  Log.debug ~prefix "[D] Originate TEZ HTTP bridge to GasBurner" ;
  let* tez_http_bridge = TezCrossRuntimeHttpCallEvm.originate gas_burner in
  Log.debug ~prefix "[D] Deploy EVM bridge to TEZ HTTP bridge" ;
  let* evm_bridge_d =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_http_bridge
  in
  Log.debug ~prefix "[D] Deploy EVM runner calling the bridge" ;
  let* runner_d =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_d, false)] ()
  in

  (* --- Warmup calls (alias generation) ---------------------------------- *)
  Log.debug ~prefix "Warmup calls (generate aliases)" ;
  let* _warmup_a = EvmRunner.call_run gas_burner in
  let* _warmup_b = EvmRunner.call_run runner_b in
  let* _warmup_c = EvmRunner.call_run runner_c in
  let* _warmup_d = EvmRunner.call_run runner_d in

  (* --- Measurement calls ------------------------------------------------ *)
  Log.debug ~prefix "Measurement calls (aliases cached)" ;
  let* g_direct = EvmRunner.call_run gas_burner in
  let* g_overhead = EvmRunner.call_run runner_b in
  let* g_call_evm = EvmRunner.call_run runner_c in
  let* g_call = EvmRunner.call_run runner_d in

  (* --- Analysis & Assertions -------------------------------------------- *)
  let delta_c_b = Int64.sub g_call_evm g_overhead in
  let delta_d_c = Int64.sub g_call g_call_evm in
  Log.info
    ~prefix
    "RESULTS: G_direct=%Ld  G_overhead=%Ld  G_call_evm=%Ld  G_call=%Ld"
    g_direct
    g_overhead
    g_call_evm
    g_call ;
  Log.info ~prefix "DELTAS: C-B=%Ld  D-C=%Ld" delta_c_b delta_d_c ;

  (* -- Sanity check: G_direct >> G_overhead --
     The upper-bound assertions below use a 3/2 multiplier to catch
     double-counting.  This only works if the CRAC overhead (aliases,
     precompile, TEZ execution) is small relative to the inner EVM
     cost.  We require a 10x ratio so that even if overhead doubles in
     a future refactor, it stays well below the 3/2 bound.

     If this check fails, the GasBurner contract needs more storage
     slots to increase G_direct.  Do NOT weaken the upper bounds —
     they exist to catch double-counting regressions. *)
  Check.(
    (g_direct > Int64.mul g_overhead 10L)
      int64
      ~error_msg:
        "Sanity: G_direct (%L) should be > 10 * G_overhead (%R). Increase \
         GasBurner slots if this fails.") ;

  (* -- Lower bound: inner EVM gas appears in the outer receipt --
     delta(C-B) isolates the gas contribution of the inner EVM call
     by subtracting the CRAC overhead (B) from the full chain (C).
     It must be at least G_direct: the outer receipt must account for
     the inner EVM work. *)
  Check.(
    (delta_c_b > g_direct)
      int64
      ~error_msg:
        "Inner EVM gas (%L) should exceed G_direct (%R) in outer receipt") ;

  (* -- Upper bound: no double-counting --
     If the inner EVM gas were charged twice (once through the
     X-Tezos-Gas-Consumed header round-trip and once through some
     other mechanism), delta(C-B) would be ~2x G_direct.  The 3/2
     bound sits between 1x (correct) and 2x (double-counted), with
     the sanity check guaranteeing that overhead noise stays < 10%. *)
  Check.(
    (delta_c_b < Int64.div (Int64.mul g_direct 3L) 2L)
      int64
      ~error_msg:
        "Inner EVM gas (%L) should be < 3/2 * G_direct (%R): possible \
         double-counting") ;

  (* -- Path consistency: %%call_evm and %%call --
     Both gateway entrypoints route to the same inner EVM execution.
     The outer gasUsed must be virtually identical.  A large gap
     would indicate one path charges gas differently. *)
  Check.(
    (Int64.abs delta_d_c < 2_000L)
      int64
      ~error_msg:
        "|G_call - G_call_evm| (%L) should be < %R: %%call and %%call_evm must \
         be consistent") ;

  (* --- TEZ->EVM scenarios (measure TEZ milligas) ------------------------ *)
  (* E: TEZ->EVM via %call_evm *)
  Log.debug ~prefix "[E] TEZ->EVM via %%call_evm (warmup)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge in
  Log.debug ~prefix "[E] TEZ->EVM via %%call_evm (measure)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_bridge in
  let* mg_call_evm = TezRunner.get_gateway_consumed_milligas () in

  (* F: TEZ->EVM via %call *)
  Log.debug ~prefix "[F] TEZ->EVM via %%call (warmup)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_http_bridge in
  Log.debug ~prefix "[F] TEZ->EVM via %%call (measure)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_http_bridge in
  let* mg_call =
    TezRunner.get_gateway_consumed_milligas ~entrypoint:"call" ()
  in

  Log.info
    ~prefix
    "TEZ MILLIGAS: E(%%call_evm)=%d  F(%%call)=%d"
    mg_call_evm
    mg_call ;

  (* -- TEZ milligas: lower bound --
     The gateway's consumed_milligas must include the inner EVM gas
     converted to milligas (1 EVM gas = 100 milligas).  We use 80x
     instead of 100x as the lower bound to allow for differences
     between the direct EVM call (G_direct) and the CRAC inner call
     (which may have slightly different intrinsic gas treatment). *)
  let mg_ref = Int64.to_int g_direct in
  Check.(
    (mg_call_evm > mg_ref * 80)
      int
      ~error_msg:
        "%%call_evm milligas (%L) should exceed G_direct*80 (%R): inner EVM \
         gas must be charged") ;

  (* -- TEZ milligas: upper bound (double-counting) --
     Same logic as the EVM-side upper bound.  If the inner EVM gas
     were counted twice in milligas, the value would be ~200x G_direct.
     The 150x bound catches this while leaving room for TEZ overhead. *)
  Check.(
    (mg_call_evm < mg_ref * 150)
      int
      ~error_msg:
        "%%call_evm milligas (%L) should be < G_direct*150 (%R): possible \
         double-counting") ;

  (* -- TEZ path consistency --
     %%call and %%call_evm must report similar milligas.  They route
     to the same inner EVM execution through different gateway
     entrypoints.  A large gap would indicate an entrypoint-specific
     gas accounting bug. *)
  Check.(
    (abs (mg_call - mg_call_evm) < 50_000)
      int
      ~error_msg:
        "|F - E| (%L) should be < %R: %%call and %%call_evm milligas must be \
         consistent") ;
  unit

(** Regression test for the error path of [execute_request].
 *
 *  When the TEZ runtime's [execute_request] enters the error path
 *  (e.g. an internal operation FAILWITHs), the [X-Tezos-Gas-Consumed]
 *  header must still report the accurate cumulative gas consumption.
 *
 *  The bug: the error path used [get_and_reset_milligas_consumed()]
 *  which returns only the delta since the last per-operation baseline
 *  reset.  After earlier internal operations (like a gateway call to
 *  GasBurner) have already executed and reset the baseline, the delta
 *  is ~0 — so the header reports ~0 and the EVM side charges nothing.
 *
 *  The fix: use [total_milligas_consumed()] on the error path, same
 *  as the success path.
 *
 *  == Scenario ==
 *
 *    EVM[evm_main] -(catch)-> EVM[evm_bridge] ~CRAC~> TEZ[tez_caller]
 *      TEZ[tez_caller]:
 *        |-> TEZ[tez_bridge] ~CRAC~> EVM[gas_burner]  (succeeds)
 *        |-> TEZ[tez_reverter]                         (FAILWITHs)
 *
 *  The TEZ caller invokes [bridge] first (which calls GasBurner via
 *  [%%call_evm], consuming ~600K EVM gas charged to TEZ milligas),
 *  then the reverter FAILWITHs.  The overall TEZ execution fails,
 *  entering the error path.
 *
 *  If the error path reports accurate gas, the outer gasUsed must
 *  include the GasBurner gas (> G_direct).  If buggy (~0), it cannot.
 *)
let test_crac_gas_error_path_reporting =
  register_crac_runner_test
    ~title:"CRAC: error path reports accurate gas in X-Tezos-Gas-Consumed"
    ~tags:["gas"; "error"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-err" in

  (* --- Deploy contracts ------------------------------------------------- *)
  Log.debug ~prefix "Deploy GasBurner (heavy EVM leaf)" ;
  let* gas_burner = EvmGasBurner.deploy () in

  (* Pre-warm GasBurner storage: first call fills 120 slots with
     non-zero values, avoiding cold 22,100-gas SSTOREs later. *)
  Log.debug ~prefix "Warm up GasBurner storage slots" ;
  let* _ = EvmRunner.call_run gas_burner in

  (* Reference: direct warm GasBurner call = true inner EVM cost. *)
  Log.debug ~prefix "Measure G_direct (warm GasBurner)" ;
  let* g_direct = EvmRunner.call_run gas_burner in
  Log.info ~prefix "G_direct = %Ld" g_direct ;

  (* Shared reverter for both chains. *)
  Log.debug ~prefix "Originate TEZ reverter" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in

  (* Overhead chain: [simple counter; reverter] — same structure as
     the error chain but without the inner CRAC to GasBurner. *)
  Log.debug ~prefix "[overhead] Originate simple TEZ counter" ;
  let* tez_simple = TezMultiRunCaller.originate () in
  Log.debug ~prefix "[overhead] Originate TEZ caller with [simple; reverter]" ;
  let* tez_caller_overhead =
    TezMultiRunCaller.originate ~callees:[tez_simple; tez_reverter] ()
  in
  Log.debug ~prefix "[overhead] Deploy EVM bridge to TEZ caller" ;
  let* evm_bridge_overhead =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_caller_overhead
  in
  Log.debug ~prefix "[overhead] Deploy EVM main with catch" ;
  let* evm_main_overhead =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_overhead, true)] ()
  in

  (* Error chain: [bridge -> GasBurner; reverter] *)
  Log.debug ~prefix "[error] Originate TEZ bridge to GasBurner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate gas_burner in
  Log.debug ~prefix "[error] Originate TEZ caller with [bridge; reverter]" ;
  let* tez_caller =
    TezMultiRunCaller.originate ~callees:[tez_bridge; tez_reverter] ()
  in
  Log.debug ~prefix "[error] Deploy EVM bridge to TEZ caller" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_caller in
  Log.debug ~prefix "[error] Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in

  (* --- Warmup calls (alias generation, catches revert) ------------------ *)
  Log.debug ~prefix "Warmup calls" ;
  let* _warmup = EvmRunner.call_run evm_main_overhead in
  let* _warmup = EvmRunner.call_run evm_main in

  (* --- Measurement calls ------------------------------------------------ *)
  Log.debug ~prefix "Measurement calls" ;
  let* g_overhead = EvmRunner.call_run evm_main_overhead in
  let* g_error = EvmRunner.call_run evm_main in

  let delta = Int64.sub g_error g_overhead in
  Log.info
    ~prefix
    "G_direct=%Ld  G_overhead=%Ld  G_error=%Ld  delta=%Ld"
    g_direct
    g_overhead
    g_error
    delta ;

  (* Verify the catches worked. *)
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:4
      evm_main_overhead
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:4
      evm_main
  in

  (* --- Assertions ------------------------------------------------------- *)
  (* delta = G_error - G_overhead isolates the GasBurner contribution
     by subtracting the CRAC + caller + reverter overhead.  Same
     approach as {!test_crac_gas_accounting_investigation}. *)

  (* Lower bound: the delta must include the inner EVM gas. *)
  Check.(
    (delta > g_direct)
      int64
      ~error_msg:
        "Error path delta (%L) should exceed G_direct (%R), proving the error \
         path reports GasBurner gas in X-Tezos-Gas-Consumed") ;
  (* Upper bound: 3/2 of G_direct.  If the inner EVM gas were
     double-counted, the delta would be ~2x G_direct.  The 3/2
     bound sits between the expected ratio and the double-counting
     signal. *)
  Check.(
    (delta < Int64.div (Int64.mul g_direct 3L) 2L)
      int64
      ~error_msg:
        "Error path delta (%L) should be below 3/2 * G_direct (%R): possible \
         double-counting on the error path") ;
  unit

(** Verify that the OOG error path of [execute_request] reports the
 *  full gas budget as consumed via [X-Tezos-Gas-Consumed].
 *
 *  Complements {!test_crac_gas_error_path_reporting} (which tests
 *  FAILWITH) by exercising the OOG-specific branch where
 *  [total_milligas_consumed()] returns [initial_limit] (gas
 *  exhausted, [remaining] is [None]).
 *
 *  == Scenario ==
 *
 *  Baseline:  EVM[wrapper] -> CRAC -> TEZ[simple counter]     (succeeds)
 *  OOG:       EVM[wrapper(catch)] -> CRAC -> TEZ[gas burner]  (OOGs, caught)
 *
 *  == Assertion ==
 *
 *  G_oog must significantly exceed G_baseline.  The gas burner
 *  consumes its entire TEZ budget; the header carries this back
 *  to the EVM receipt.  If the header reported ~0 on OOG, G_oog
 *  would be close to G_baseline.
 *)
let test_crac_gas_oog_path_reporting =
  register_crac_runner_test
    ~title:"CRAC: OOG error path reports consumed gas in X-Tezos-Gas-Consumed"
    ~tags:["gas"; "oog"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-oog" in

  (* Baseline: EVM->TEZ with simple counter (succeeds, low gas) *)
  Log.debug ~prefix "[baseline] Originate simple TEZ counter" ;
  let* tez_simple = TezMultiRunCaller.originate () in
  Log.debug ~prefix "[baseline] Deploy EVM bridge to TEZ counter" ;
  let* evm_bridge_baseline =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_simple
  in
  Log.debug ~prefix "[baseline] Deploy EVM wrapper" ;
  let* runner_baseline =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_baseline, false)] ()
  in

  (* OOG: EVM->TEZ with infinite gas burner (always OOGs, caught) *)
  Log.debug ~prefix "[oog] Originate TEZ gas burner (infinite loop)" ;
  let* tez_burner = TezGasBurner.originate () in
  Log.debug ~prefix "[oog] Deploy EVM bridge to gas burner" ;
  let* evm_bridge_oog = EvmCrossRuntimeRunnerTez.deploy_and_init tez_burner in
  Log.debug ~prefix "[oog] Deploy EVM wrapper with catch" ;
  let* runner_oog =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_oog, true)] ()
  in

  let gas_limit = 3_000_000 in

  (* Warmup calls (alias generation) *)
  Log.debug ~prefix "Warmup calls" ;
  let* _ = EvmRunner.call_run runner_baseline in
  let* _ = EvmRunner.call_run ~gas:gas_limit runner_oog in

  (* Measurement calls *)
  Log.debug ~prefix "Measurement calls" ;
  let* g_baseline = EvmRunner.call_run runner_baseline in
  let* g_oog = EvmRunner.call_run ~gas:gas_limit runner_oog in

  Log.info ~prefix "G_baseline=%Ld  G_oog=%Ld" g_baseline g_oog ;

  (* Verify the catch worked *)
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:4
      runner_oog
  in

  (* The gas burner consumes its entire TEZ budget, which is derived
     from the EVM gas_limit.  If the error path reports gas correctly,
     g_oog should be close to gas_limit.  If the header reported ~0,
     g_oog would only reflect EVM overhead (~60K).

     No upper bound: gasUsed cannot exceed gas_limit by EVM
     semantics, so a 3/2 bound would be meaningless here. *)
  let gas_limit = Int64.of_int gas_limit in
  Check.(
    (g_oog > Int64.div (Int64.mul gas_limit 2L) 3L)
      int64
      ~error_msg:
        "OOG gasUsed (%L) should exceed 2/3 * gas_limit (%R), proving the OOG \
         error path reports consumed gas via X-Tezos-Gas-Consumed") ;
  unit

(** End-to-end happy-path check: the Michelson contract deposits bytes
    via [%collect_result]; the server surfaces them as the HTTP response
    body; the EVM caller recovers them via the [call] precompile. *)
let test_crac_collect_result_surfaces_in_response_body =
  register_crac_runner_test
    ~title:"CRAC: %collect_result bytes surface in HTTP response body"
    ~tags:["collect_result"; "http_call"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let payload_hex = "cafebabe" in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate Michelson contract that calls %%collect_result" ;
  let* tez_collector = TezCollectResult.originate ~payload_hex () in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_collector in
  Log.debug ~prefix "Run: EVM → Michelson → gateway %%collect_result" ;
  let* _ = EvmCollectResult.call_run evm_reader in
  Log.debug ~prefix "Verify the returned bytes" ;
  EvmCollectResult.check_result ~expected_hex:payload_hex evm_reader

(** End-to-end revert-path check: the Michelson contract deposits bytes
    via [%collect_result] then triggers a failure via a second internal
    op.  The server must return 4xx with no bytes leaking, so the EVM
    precompile call reverts and [result] stays empty even after the
    outer tx completes via [runCatch()]. *)
let test_crac_collect_result_revert_discards_bytes =
  register_crac_runner_test
    ~title:"CRAC: revert discards %collect_result bytes"
    ~tags:["collect_result"; "http_call"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let payload_hex = "cafebabe" in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate always_fails_unit.tz" ;
  let* failing = TezAlwaysFailsUnit.originate () in
  Log.debug
    ~prefix
    "Originate Michelson contract that calls %%collect_result then fails" ;
  let* tez_contract =
    TezCollectResultThenFail.originate ~failing ~payload_hex ()
  in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_contract in
  Log.debug ~prefix "Run (catching): CRAC should revert, no bytes leaked" ;
  let* _ = EvmCollectResult.call_run_catch evm_reader in
  Log.debug ~prefix "Verify the precompile call was caught as reverted" ;
  let* () = EvmCollectResult.check_caught ~expected:true evm_reader in
  Log.debug ~prefix "Verify no bytes leaked into the result slot" ;
  EvmCollectResult.check_result ~expected_hex:"" evm_reader

(** Size sweep for [%collect_result] gas carbonation: the
    handler pre-charges [460 + 1.5 * size] milligas for the store +
    result surfacing + response-body forward triptych. Run three
    adapters with 256, 1024, and 4096-byte payloads through the CRAC
    path and check that the measured handler cost matches the model
    exactly, via the synthetic tezlink receipt's [collect_result]
    internal op.

    Per-internal-op the apply loop charges [Cost::manager_operation]
    = 100_000 mgas; MIR then charges [VALUE_STEP] = 100 mgas for the
    bytes typecheck; then the handler adds the size-dependent term.
    Total: [100_560 + 1.5 * size] mgas. *)
let test_crac_collect_result_size_sweep_matches_model =
  register_crac_runner_test
    ~title:"CRAC: %collect_result size sweep matches model"
    ~tags:["collect_result"; "gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-size" in
  let hex_of_bytes n = String.make (2 * n) '0' in
  let sizes = [256; 1024; 4096] in
  Log.debug ~prefix "Originate adapters for sizes 256/1024/4096" ;
  let* adapters =
    Lwt_list.map_s
      (fun n ->
        let* t = TezCollectResult.originate ~payload_hex:(hex_of_bytes n) () in
        let* e = EvmCollectResult.deploy_and_init t in
        return (n, e))
      sizes
  in
  let expected_mgas n = 100_000 + 100 + 460 + (3 * n / 2) in
  Log.debug ~prefix "Warmup each reader (alias + storage)" ;
  let* () =
    Lwt_list.iter_s
      (fun (_, e) ->
        let* _ = EvmCollectResult.call_run e in
        unit)
      adapters
  in
  Log.debug ~prefix "Measurement runs" ;
  let* () =
    Lwt_list.iter_s
      (fun (n, e) ->
        let* _ = EvmCollectResult.call_run e in
        let* mgas =
          TezRunner.get_gateway_consumed_milligas
            ~entrypoint:"collect_result"
            ()
        in
        let expected = expected_mgas n in
        Log.info ~prefix "size=%d mgas=%d (expected %d)" n mgas expected ;
        Check.(
          (mgas = expected)
            int
            ~error_msg:
              (Printf.sprintf
                 "size=%d: consumed_milligas %%L does not match model %%R"
                 n)) ;
        unit)
      adapters
  in
  unit

let () =
  test_crac_evm_to_tez [Alpha] ;
  test_crac_evm_multiple_independent_crossings [Alpha] ;
  test_crac_evm_double_crossing [Alpha] ;
  test_crac_evm_shared_leaf_via_direct_and_chain [Alpha] ;
  test_crac_evm_5_crossing_chain [Alpha] ;
  test_crac_tez_to_evm [Alpha] ;
  test_crac_tez_multiple_independent_crossings [Alpha] ;
  test_crac_tez_double_crossing [Alpha] ;
  test_crac_tez_shared_leaf_via_direct_and_chain [Alpha] ;
  test_crac_tez_5_crossing_chain [Alpha] ;
  test_crac_access_list_preserved [Alpha] ;
  test_crac_evm_to_tez_reverts [Alpha] ;
  test_crac_tez_to_evm_reverts [Alpha] ;
  test_crac_tez_to_evm_fake_tx_in_block [Alpha] ;
  test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks [Alpha] ;
  test_crac_tez_revert_rolls_back_inner_evm_storage [Alpha] ;
  test_crac_tez_revert_propagates_to_evm [Alpha] ;
  test_crac_evm_journal_state_preserved [Alpha] ;
  test_crac_catch_tez_revert [Alpha] ;
  test_crac_evm_revert_propagates_to_tez [Alpha] ;
  test_crac_second_crac_tez_revert [Alpha] ;
  test_crac_evm_revert_rolls_back_two_cracs [Alpha] ;
  test_crac_deep_branch_with_second_tez_revert [Alpha] ;
  test_crac_nested_revert_cascade_without_catch [Alpha] ;
  test_crac_double_nested_evm_revert [Alpha] ;
  test_crac_deep_nesting_6_levels [Alpha] ;
  test_crac_evm_revert_after_nested_cracs [Alpha] ;
  test_crac_evm_target_reverts [Alpha] ;
  test_crac_second_crac_evm_revert [Alpha] ;
  test_crac_tez_revert_rolls_back_two_cracs [Alpha] ;
  test_crac_deep_branch_with_second_evm_revert [Alpha] ;
  test_crac_tez_nested_revert_cascade_without_catch [Alpha] ;
  test_crac_double_nested_tez_revert [Alpha] ;
  test_crac_tez_deep_nesting_6_levels [Alpha] ;
  test_crac_tez_revert_after_nested_cracs [Alpha] ;
  test_crac_catch_evm_revert_between_cracs [Alpha] ;
  test_crac_catch_tez_revert_between_cracs [Alpha] ;
  test_crac_catch_tez_revert_with_nested_crac [Alpha] ;
  test_crac_catch_deep_evm_revert_through_double_crac [Alpha] ;
  test_crac_tez_catch_evm_revert_between_cracs [Alpha] ;
  test_crac_tez_catch_tez_revert_between_cracs [Alpha] ;
  test_crac_tez_catch_deep_revert_through_double_crac [Alpha] ;
  test_crac_catch_revert_after_multiple_cracs [Alpha] ;
  test_crac_catch_tez_revert_after_multiple_return_cracs [Alpha] ;
  test_crac_catch_4_crossing_chain_revert [Alpha] ;
  test_crac_tez_catch_4_crossing_chain_revert [Alpha] ;
  test_crac_catch_5_crossing_chain_revert [Alpha] ;
  test_crac_tez_catch_5_crossing_chain_revert [Alpha] ;
  test_crac_chained_tez_calls_behind_crac [Alpha] ;
  test_crac_nested_catches_with_multiple_reverts [Alpha] ;
  test_crac_gas_model_alias_caching [Alpha] ;
  test_crac_evm_to_tez_receipt [Alpha] ;
  test_crac_receipt_two_independent [Alpha] ;
  test_crac_receipt_separate_tx_two_cracs [Alpha] ;
  test_crac_receipt_evm_tez_evm [Alpha] ;
  test_crac_receipt_tez_evm_tez [Alpha] ;
  test_crac_receipt_evm_tez_evm_tez [Alpha] ;
  test_crac_receipt_evm_to_tez_revert [Alpha] ;
  test_crac_receipt_two_failed_independent [Alpha] ;
  test_crac_receipt_interleaved_failed_pending [Alpha] ;
  test_crac_receipt_evm_not_first_tx [Alpha] ;
  test_crac_receipt_tez_not_first_tx [Alpha] ;
  test_crac_receipt_evm_then_tez_same_block [Alpha] ;
  test_crac_receipt_tez_to_evm [Alpha] ;
  test_crac_receipt_tez_evm_tez_evm [Alpha] ;
  test_crac_receipt_evm_5_crossing_chain [Alpha] ;
  test_crac_receipt_tez_mixed_calls_with_crac [Alpha] ;
  test_crac_http_call_success [Alpha] ;
  test_crac_http_call_catch_revert [Alpha] ;
  test_crac_http_call_catch_oog [Alpha] ;
  test_crac_debug_trace_transaction [Alpha] ;
  test_crac_debug_trace_block [Alpha] ;
  test_crac_debug_trace_normal_tx_in_crac_block [Alpha] ;
  test_http_trace_nested_crac [Alpha] ;
  test_http_trace_multiple_independent_cracs [Alpha] ;
  test_http_trace_mixed_block [Alpha] ;
  test_l1_vs_tezosx_nested_failwith_receipt [Alpha] ;
  test_crac_callback_fire_and_forget [Alpha] ;
  test_crac_callback_receives_result_bytes [Alpha] ;
  test_crac_callback_failure_reverts_all [Alpha] ;
  test_crac_callback_behind_crac [Alpha] ;
  test_crac_callback_tez_revert_rolls_back_callback [Alpha] ;
  test_crac_callback_evm_catches_failing_callback_behind_crac [Alpha] ;
  test_crac_callback_mixed_with_normal_runners [Alpha] ;
  test_crac_gas_header_michelson_burner [Alpha] ;
  test_crac_gas_model_callee_gas_in_evm_receipt [Alpha] ;
  test_crac_gas_model_callee_gas_in_receipt [Alpha] ;
  test_crac_gas_accounting_investigation [Alpha] ;
  test_crac_gas_error_path_reporting [Alpha] ;
  test_crac_gas_oog_path_reporting [Alpha] ;
  test_crac_collect_result_surfaces_in_response_body [Alpha] ;
  test_crac_collect_result_revert_discards_bytes [Alpha] ;
  test_crac_collect_result_size_sweep_matches_model [Alpha]
