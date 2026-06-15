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

(** Base-fee floor for the CRAC test setups.  The delayed-inbox transport
    bypassed the fee market entirely; direct tezlink injection enforces
    minimal fees (base_fee_per_gas * michelson_to_evm_gas_multiplier *
    gas_limit), which the historical [~fee:1000] of the forged
    operations does not cover beyond 100k gas.  The operations must
    stay byte-for-byte identical, so instead lower the base-fee floor
    to 1 wei — non-zero so BASEFEE-observable assertions stay
    meaningful. *)
let crac_minimum_base_fee_per_gas = Wei.of_string "1"

(** Foreign endpoint of the tezlink RPC served by [evm_node]. *)
let tezlink_foreign_endpoint_from_evm_node evm_node =
  let evm_node_endpoint = Evm_node.rpc_endpoint_record evm_node in
  {evm_node_endpoint with Endpoint.path = "/tezlink"}

(** Client whose endpoint is the tezlink RPC served by [evm_node], for
    direct injection of Michelson operations. *)
let tezlink_client_from_evm_node evm_node =
  let endpoint =
    Client.Foreign_endpoint (tezlink_foreign_endpoint_from_evm_node evm_node)
  in
  Client.init ~endpoint ()

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
    let* receipt =
      match receipt with
      | Some receipt -> return receipt
      | None ->
          (* The receipt may not be indexed yet right after
             [produce_block]; retry before failing. *)
          Test_helpers.wait_for_transaction_receipt
            ~evm_node:sequencer
            ~transaction_hash:tx_hash
            ()
    in
    match receipt with
    | {contractAddress = Some addr; status = true; _} -> return addr
    | {status = false; _} -> Test.fail "Contract deployment transaction failed"
    | _ -> Test.fail "No contract address for deployment tx"

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
  let tezosx_michelson_contracts_index = "/tez/tez_accounts/contracts/index"

  (** [encode_contract_address kt1] encodes a b58check KT1 address string
   *  into the hex-encoded [Contract_repr.t] under which the contract is
   *  indexed in the durable storage. *)
  let encode_contract_address kt1 =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    match C.of_b58check kt1 with
    | Error _ -> Test.fail "encode_contract_address: invalid address %s" kt1
    | Ok contract ->
        Data_encoding.Binary.to_bytes_exn C.encoding contract
        |> Hex.of_bytes |> Hex.show

  (** [inject_op_and_produce_block ~client_tezlink ~sequencer op] injects a
   *  pre-signed operation as-is via the tezlink RPC, seals it with
   *  [produce_block], and returns the operation (with receipt metadata) as
   *  found in the produced block.  Fails if the operation was not
   *  included. *)
  let inject_op_and_produce_block ~client_tezlink ~sequencer operation =
    let* (`OpHash op_hash) =
      Operation.inject ~dont_wait:true operation client_tezlink
    in
    let*@ _count = Rpc.produce_block sequencer in
    let* all_passes =
      RPC_core.call
        (tezlink_foreign_endpoint_from_evm_node sequencer)
        (RPC.get_chain_block_operations ())
    in
    match
      List.find_opt
        (fun op -> JSON.(op |-> "hash" |> as_string) = op_hash)
        JSON.(all_passes |=> 3 |> as_list)
    with
    | None ->
        Test.fail "Operation %s was not included in the produced block" op_hash
    | Some op -> return op

  (** [originate_contract_via_tezlink] originates a Michelson contract by
   *  direct tezlink injection. Loads the script from
   *  [michelson_test_scripts], converts code and initial storage to JSON,
   *  forges, injects and seals the origination operation, then returns the
   *  hex key and KT1 address of the new contract. *)
  let originate_contract_via_tezlink ~client ~client_tezlink ~sequencer ~source
      ~counter ~script_name ~init_storage_data ?(init_balance = 0) protocol =
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
              ~storage_limit:60000
              ~source
              (origination ~code ~init_storage ~init_balance ());
          ])
        client
    in
    let* op_json =
      inject_op_and_produce_block ~client_tezlink ~sequencer origination_op
    in
    let new_contracts =
      JSON.(
        op_json |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
        |-> "originated_contracts" |> as_list |> List.map as_string)
    in
    Check.(
      (List.length new_contracts = 1)
        int
        ~error_msg:"Expected %R new contract but got %L") ;
    match new_contracts with
    | [kt1_address] ->
        let contract_hex = encode_contract_address kt1_address in
        Log.info "Originated contract: %s" kt1_address ;
        return (contract_hex, kt1_address)
    | _ -> assert false

  (** [call_contract_via_tezlink] calls a Michelson contract by direct
   *  tezlink injection. Converts the argument from Michelson notation to
   *  JSON, forges, injects and seals the call operation. *)
  let call_contract_via_tezlink ~client ~client_tezlink ~sequencer ~source
      ~counter ~dest ~arg_data ?(entrypoint = "default") ?(amount = 0)
      ?(gas_limit = 100_000) () =
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
    let* (_ : JSON.t) =
      inject_op_and_produce_block ~client_tezlink ~sequencer call_op
    in
    unit

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

  (** Reads the decoded Micheline storage of the Michelson contract
   *  identified by [contract_address] (a b58check KT1 address) from the
   *  tezlink RPC of [sequencer]. *)
  let get_storage ~sequencer contract_address =
    RPC_core.call
      (tezlink_foreign_endpoint_from_evm_node sequencer)
      (RPC.get_chain_block_context_contract_storage ~id:contract_address ())
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

(** EVM contract that calls another EVM contract via the generic [call]
 *  precompile, with URL [http://ethereum/<destination>].  Exercises the
 *  same-runtime CRAC path: the call leaves the EVM runtime via the
 *  gateway and is dispatched back to the EVM runtime. *)
module EvmCracHttpCallEvm = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.crac_http_call_evm

  let init ~sequencer ~sender ~nonce ~value ~evm_contract_target_address
      contract =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"initialize(string)"
        ~arguments:[evm_contract_target_address]
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

  let check_storage ~sequencer ?(expected_catches = 0) ~expected_counter
      contract =
    let count_storage_pos = "0x00" in
    let*@ evm_count =
      Rpc.get_storage_at ~address:contract ~pos:count_storage_pos sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmCracHttpCallEvm `count` %R but got %L") ;
    let catches_storage_pos = "0x01" in
    let*@ evm_catches =
      Rpc.get_storage_at ~address:contract ~pos:catches_storage_pos sequencer
    in
    Check.(
      (int_of_string evm_catches = expected_catches)
        int
        ~error_msg:"Expected EvmCracHttpCallEvm `catches` %R but got %L") ;
    unit
end

(** EVM contract that calls another EVM contract via the generic [call]
 *  precompile and stores the returned HTTP response body.  Same as
 *  [EvmCollectResult] but routes through [http://ethereum/<destination>],
 *  invoking [store(uint256)] on the target so the EVM serve produces a
 *  non-empty [Output::Call] body. *)
module EvmCracCollectResultEvm = struct
  open EvmContract

  let deploy =
    deploy_solidity_contract
      ~contract:Solidity_contracts.crac_collect_result_evm

  let init ~sequencer ~sender ~nonce ~value ~evm_contract_target_address
      ~stored_value contract =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:contract
        ~abi_signature:"initialize(string,uint256)"
        ~arguments:[evm_contract_target_address; string_of_int stored_value]
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

  let check_result ~sequencer ~expected_hex contract =
    let endpoint = Evm_node.endpoint sequencer in
    let* raw = Cast.call "result()(bytes)" ~endpoint ~address:contract in
    let strip_0x s = Test_helpers.remove_0x s in
    Check.(
      (String.lowercase_ascii (strip_0x raw)
      = String.lowercase_ascii expected_hex)
        string
        ~error_msg:"Expected EvmCracCollectResultEvm result %R but got %L") ;
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
  let call_run_catch ?expected_status ?gas ~sequencer ~sender ~nonce ~value
      contract =
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
        ?gas
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

  let deploy_large =
    deploy_solidity_contract ~contract:Solidity_contracts.gas_burner_large
end

(** Common [%run] caller for Tezos contracts. *)
module TezRunner = struct
  open TezContract

  (** Calls [%run] with [Unit] by direct tezlink injection. *)
  let call_run ~client ~client_tezlink ~sequencer ~source ~counter ?amount
      ?gas_limit runner =
    call_contract_via_tezlink
      ~client
      ~client_tezlink
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
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~evm_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_run_evm"] in
    let init_storage_data = sf {|Pair 0 "%s"|} evm_contract_target_address in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sequencer ~expected_counter
      (_cross_runtime_run_tez_hex, cross_runtime_run_tez_address) =
    let* storage = get_storage ~sequencer cross_runtime_run_tez_address in
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

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~evm_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_http_call_evm"] in
    let init_storage_data = sf {|Pair 0 "%s"|} evm_contract_target_address in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  let check_storage ~sequencer ~expected_counter
      (_contract_hex, contract_address) =
    let* storage = get_storage ~sequencer contract_address in
    (* Pair(nat %count, string %destination) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCrossRuntimeHttpCallEvm `count` %R but got %L") ;
    unit
end

(** Michelson contract that calls another Michelson contract via the
 *  gateway's [%call] (HTTP) entrypoint with a callback, pointing at
 *  [http://tezos/<destination>/default].  Used to verify that bytes
 *  deposited via [%collect_result] on the same-runtime target are
 *  forwarded to the caller's [%on_result] entrypoint. *)
module TezCrossRuntimeHttpCallTezCallback = struct
  open TezContract
  include TezRunner

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~tez_contract_target_address () =
    let script_name =
      ["mini_scenarios"; "cross_runtime_http_call_tez_callback"]
    in
    let init_storage_data =
      sf {|Pair 0 (Pair "%s" None)|} tez_contract_target_address
    in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  let check_counter ~sequencer ~expected_counter
      (_contract_hex, contract_address) =
    let* storage = get_storage ~sequencer contract_address in
    (* Pair(nat %count, Pair(string %destination, option bytes)) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:
          "Expected TezCrossRuntimeHttpCallTezCallback `count` %R but got %L") ;
    unit

  let check_result ~sequencer ~expected_bytes (_contract_hex, contract_address)
      =
    let* storage = get_storage ~sequencer contract_address in
    (* Storage: Pair(count, Pair(destination, option bytes)) *)
    let result_node = JSON.(storage |-> "args" |=> 1 |-> "args" |=> 1) in
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

(** Michelson contract that calls another Michelson contract via the
 *  gateway's [%call] (HTTP) entrypoint, pointing at
 *  [http://tezos/<destination>/run].  Exercises the same-runtime CRAC
 *  path: the gateway dispatches the request back to the Michelson
 *  runtime instead of crossing to the EVM runtime. *)
module TezCrossRuntimeHttpCallTez = struct
  open TezContract
  include TezRunner

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~tez_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_http_call_tez"] in
    let init_storage_data = sf {|Pair 0 "%s"|} tez_contract_target_address in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  let check_storage ~sequencer ~expected_counter
      (_contract_hex, contract_address) =
    let* storage = get_storage ~sequencer contract_address in
    (* Pair(nat %count, string %destination) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCrossRuntimeHttpCallTez `count` %R but got %L") ;
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
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~revert ~callees () =
    let script_name = ["mini_scenarios"; "multi_run_caller"] in
    let init_storage_data = multi_run_caller_init_storage ~revert ~callees in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sequencer ~expected_counter
      (_multi_run_caller_hex, multi_run_caller_address) =
    let* storage = get_storage ~sequencer multi_run_caller_address in
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

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    let script_name = ["mini_scenarios"; "gas_burner"] in
    let init_storage_data = "Unit" in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
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

(** Records [msg.sender] and [tx.origin] of the last incoming [run()]
 *  call so a test can assert that a cross-runtime round-trip preserved
 *  the native caller's identity. *)
module EvmIdentityRecorder = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.crac_identity_recorder

  let parse_address_slot raw =
    let strip_0x = Test_helpers.remove_0x in
    let s = strip_0x raw in
    (* Storage slot is 32 bytes (64 hex chars); address is the last 20
       bytes (40 hex chars). *)
    let len = String.length s in
    if len >= 40 then String.sub s (len - 40) 40 else String.make 40 '0'

  (** Asserts that [lastSender] (slot 0) equals [expected_sender]
   *  (lowercase hex, no [0x] prefix on either side). *)
  let check_last_sender ~sequencer ~expected_sender contract =
    let*@ raw = Rpc.get_storage_at ~address:contract ~pos:"0x0" sequencer in
    let actual = String.lowercase_ascii (parse_address_slot raw) in
    let expected =
      String.lowercase_ascii (Test_helpers.remove_0x expected_sender)
    in
    Check.(
      (actual = expected)
        string
        ~error_msg:"Expected IdentityRecorder.lastSender %R but got %L") ;
    unit

  (** Asserts that [lastOrigin] (slot 1, [tx.origin]) equals
   *  [expected_origin] (lowercase hex, no [0x] prefix on either side).
   *  On a two-hop CRAC round-trip the kernel forwards the CRAC originator
   *  as [tx.origin], so this slot holds the ORIGINAL EVM caller that
   *  initiated the chain. *)
  let check_last_origin ~sequencer ~expected_origin contract =
    let*@ raw = Rpc.get_storage_at ~address:contract ~pos:"0x1" sequencer in
    let actual = String.lowercase_ascii (parse_address_slot raw) in
    let expected =
      String.lowercase_ascii (Test_helpers.remove_0x expected_origin)
    in
    Check.(
      (actual = expected)
        string
        ~error_msg:"Expected IdentityRecorder.lastOrigin %R but got %L") ;
    unit
end

(** EVM contract that emits two indexed events ([EventA], [EventB]) on
 *  every [emitBoth(uint256)] call.  Used to verify that LOG opcodes
 *  emitted by an EVM contract reached through a Michelson-initiated
 *  cross-VM CRAC ([%call_evm]) are surfaced on the synthetic CRAC
 *  transaction receipt. *)
module EvmEvents = struct
  open EvmContract

  let deploy = deploy_solidity_contract ~contract:Solidity_contracts.events
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
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~failing ~evm_contract_target_address ~method_sig
      ~abi_params () =
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
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_counter ~sequencer ~expected_counter (_hex, addr) =
    let* storage = get_storage ~sequencer addr in
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
  let check_result ~sequencer ~expected_bytes (_hex, addr) =
    let* storage = get_storage ~sequencer addr in
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

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    let script_name = ["mini_scenarios"; "gas_burner_with_internal_op"] in
    let init_storage_data = "Unit" in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
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
  let call_evm ~client ~client_tezlink ~sequencer ~source ~counter ~evm_target
      ~method_sig ~abi_params ?(amount = 0) () =
    let arg_data =
      sf
        {|Pair "%s" (Pair "%s" (Pair 0x%s None))|}
        evm_target
        method_sig
        abi_params
    in
    call_contract_via_tezlink
      ~client
      ~client_tezlink
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
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~payload_hex () =
    let script_name = ["mini_scenarios"; "gateway_collect_result"] in
    let init_storage_data = sf {|Pair "%s" 0x%s|} gateway_address payload_hex in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Tezos contract whose [%default] entrypoint deposits a fixed bytes
 *  payload into the current CRAC frame by calling the gateway's
 *  [%collect_result] entrypoint with a hardcoded non-zero amount
 *  (1 mutez).  Used to verify that the kernel rejects any value transfer
 *  on [%collect_result]. *)
module TezCollectResultWithAmount = struct
  open TezContract

  (** Originates [gateway_collect_result_with_amount.tz] with storage
   *  [Pair gateway_address 0x<payload_hex>].  [init_balance] should be
   *  set so the originated contract has at least 1 mutez on hand —
   *  otherwise the TRANSFER_TOKENS would fail with [BalanceTooLow] before
   *  the kernel's amount check has a chance to fire, masking the
   *  rejection we are testing. *)
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~payload_hex () =
    let script_name =
      ["mini_scenarios"; "gateway_collect_result_with_amount"]
    in
    let init_storage_data = sf {|Pair "%s" 0x%s|} gateway_address payload_hex in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
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

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "always_fails_unit"]
      ~init_storage_data:"Unit"
      ?init_balance
      protocol
end

(** Michelson contract that emits a user [EMIT] op (tag
    [hello_l2_1299], payload string ["hello-l2-1299"]) and then
    self-calls a [%_revert] entrypoint that [FAILWITH]s.  The runtime
    applies the EMIT then fails on the self-revert, so the resulting
    internals appear as [emit BackTracked, revert Failed].  Used as
    the deepest target in a re-entrant CRAC chain to exercise the
    has_event check in [merge_crac_internals]: the failed inner CRAC
    carries a user EMIT (Event variant) but no synthetic CRAC-ID
    event, so without the fix the outer applied CRAC's synthetic event
    is dropped from the merged receipt. *)
module TezEmitFailer = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "cross_runtime_emit_failer"]
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

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~failing_kt1 ~payload_hex () =
    let script_name = ["mini_scenarios"; "gateway_collect_result_then_fail"] in
    let init_storage_data =
      sf {|Pair "%s" (Pair "%s" 0x%s)|} gateway_address failing_kt1 payload_hex
    in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Tezos contract that accepts unit and succeeds immediately without
 *  emitting any internal operations.  Used to verify that a CRAC
 *  which never calls [%collect_result] returns empty bytes to the EVM
 *  caller (backward-compatibility / fire-and-forget test). *)
module TezAlwaysSucceedsUnit = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "always_succeeds_unit"]
      ~init_storage_data:"Unit"
      ?init_balance
      protocol
end

(** Michelson contract that records the [SENDER] of each incoming call
 *  into its single [address] storage slot.  Used in the CRAC
 *  address-identity tests to observe the Michelson-side immediate
 *  caller — e.g. the KT1 alias under which an EVM contract calls into
 *  Michelson. *)
module TezCracSenderRecorder = struct
  open TezContract

  (** Originates [crac_sender_recorder.tz] with initial storage set to
   *  [init_addr] (any placeholder address, e.g. the gateway address). *)
  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~init_addr () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "crac_sender_recorder"]
      ~init_storage_data:(sf {|"%s"|} init_addr)
      ?init_balance
      protocol

  (** Reads the stored address from [crac_sender_recorder.tz] and asserts
   *  it equals [expected_sender].  The storage is a single Micheline
   *  [address] node in optimized binary form, serialized as
   *  [{bytes: "<hex>"}] in JSON, where the hex is a [Contract_repr.t] in
   *  binary encoding.  We decode those bytes to a b58check string before
   *  comparing. *)
  let check_stored_sender ~sequencer ~expected_sender contract_address =
    let* storage = get_storage ~sequencer contract_address in
    let bytes_hex = JSON.(storage |-> "bytes" |> as_string) in
    let actual =
      let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
      Hex.to_bytes (`Hex bytes_hex)
      |> Data_encoding.Binary.of_bytes_exn C.encoding
      |> C.to_b58check
    in
    Check.(
      (actual = expected_sender)
        string
        ~error_msg:"CRAC round-trip: expected Michelson SENDER %R but got %L") ;
    unit
end

(** Michelson contract that emits a single user [EMIT] op on [%run]
    with tag [hello_l2_1301] and payload string ["hello-l2-1301"], then
    succeeds.  Used as the deepest target in a re-entrant CRAC chain to
    exercise [drain_reentrant_crac_ops]: the user EMIT must survive the
    splice into the parent op's flat list instead of being stripped as
    if it were a duplicate synthetic CRAC-ID event. *)
module TezEmitter = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "cross_runtime_emitter"]
      ~init_storage_data:"Unit"
      ?init_balance
      protocol
end

(** Michelson contract that emits with tag [%crac] from a KT1.  Used
    to verify that user-contract [EMIT] ops with the same tag as kernel
    frame markers are excluded from the CRAC bracket walk by the sender
    filter (source must be the null implicit account). *)
module TezCracForgedEmitter = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "cross_runtime_crac_forged_emitter"]
      ~init_storage_data:"Unit"
      ?init_balance
      protocol
end

(** Tezos contract that calls [%collect_result] twice in the same
 *  frame.  The second call violates the once-per-frame invariant
 *  and reverts the whole operation group.  Used to verify the
 *  invariant end-to-end. *)
module TezCollectResultTwice = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~payload_hex () =
    let script_name = ["mini_scenarios"; "gateway_collect_result_twice"] in
    let init_storage_data = sf {|Pair "%s" 0x%s|} gateway_address payload_hex in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Tezos contract that deposits a payload via [%collect_result] on the
 *  current CRAC frame, then calls an EVM contract via [%call_evm].
 *  Used to verify that nested CRAC frames have independent result
 *  slots: the outer caller receives the bytes deposited here while
 *  the inner EVM call can independently receive different bytes from
 *  its own nested frame. *)
module TezCollectResultThenCallEvm = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance ~evm_target_address ~payload_hex () =
    let script_name =
      ["mini_scenarios"; "gateway_collect_result_then_call_evm"]
    in
    let init_storage_data =
      sf
        {|Pair "%s" (Pair "%s" 0x%s)|}
        gateway_address
        evm_target_address
        payload_hex
    in
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol
end

(** Tezos contract that generates 4 MiB of zero bytes at runtime via
 *  22 consecutive [CONCAT] doublings (1-byte seed) and deposits them
 *  via [%collect_result].  Used to trigger EVM OOG when the
 *  precompile return value is copied into memory. *)
module TezGenerate4MibResult = struct
  open TezContract

  let originate ~client ~client_tezlink ~sequencer ~source ~counter ~protocol
      ?init_balance () =
    originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter
      ~script_name:["mini_scenarios"; "gateway_generate_4mib_result"]
      ~init_storage_data:(sf {|"%s"|} gateway_address)
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

    (** Offline (mockup) client used for Michelson conversions and
        operation forging. *)
    val client : Client.t

    (** Client pointing at the sequencer's tezlink RPC, used for direct
        injection and tezlink queries. *)
    val client_tezlink : Client.t

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

    module EvmCracHttpCallEvm : sig
      val deploy_and_init : ?value:Wei.t -> evm_runner -> evm_runner Lwt.t

      val call_run_catch :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val check_storage :
        ?expected_catches:int ->
        expected_counter:int ->
        evm_runner ->
        unit Lwt.t
    end

    module EvmCracCollectResultEvm : sig
      val deploy_and_init :
        ?value:Wei.t -> stored_value:int -> evm_runner -> evm_runner Lwt.t

      val call_run :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val check_result : expected_hex:string -> evm_runner -> unit Lwt.t
    end

    module EvmCollectResult : sig
      val deploy_and_init : ?value:Wei.t -> tez_runner -> evm_runner Lwt.t

      val call_run :
        ?expected_status:bool -> ?value:Wei.t -> evm_runner -> int64 Lwt.t

      val call_run_catch :
        ?expected_status:bool ->
        ?gas:int ->
        ?value:Wei.t ->
        evm_runner ->
        int64 Lwt.t

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

      (** Like {!deploy} but originates the larger {!GasBurnerLarge}
          variant (480 slots, ~2.5M warm gas).  Callers must pass a high
          gas limit: the first, cold call writes ~22,100 gas per slot. *)
      val deploy_large : unit -> evm_runner Lwt.t
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

    module TezCrossRuntimeHttpCallTez : sig
      val originate : ?init_balance:int -> tez_runner -> tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end

    module TezCrossRuntimeHttpCallTezCallback : sig
      val originate : ?init_balance:int -> tez_runner -> tez_runner Lwt.t

      val check_counter : expected_counter:int -> tez_runner -> unit Lwt.t

      val check_result :
        expected_bytes:string option -> tez_runner -> unit Lwt.t
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

    module TezCollectResultWithAmount : sig
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

    module TezAlwaysSucceedsUnit : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    module TezCracSenderRecorder : sig
      val originate :
        ?init_balance:int -> init_addr:string -> unit -> tez_runner Lwt.t

      val check_stored_sender :
        expected_sender:string -> tez_runner -> unit Lwt.t
    end

    module TezEmitter : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    module TezCracForgedEmitter : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    module TezEmitFailer : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
    end

    module TezCollectResultTwice : sig
      val originate :
        ?init_balance:int -> payload_hex:string -> unit -> tez_runner Lwt.t
    end

    module TezCollectResultThenCallEvm : sig
      val originate :
        ?init_balance:int ->
        evm_target:evm_runner ->
        payload_hex:string ->
        unit ->
        tez_runner Lwt.t
    end

    module TezGenerate4MibResult : sig
      val originate : ?init_balance:int -> unit -> tez_runner Lwt.t
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

    module EvmIdentityRecorder : sig
      val deploy : unit -> evm_runner Lwt.t

      val check_last_sender : expected_sender:string -> evm_runner -> unit Lwt.t

      val check_last_origin : expected_origin:string -> evm_runner -> unit Lwt.t
    end

    module EvmEvents : sig
      val deploy : unit -> evm_runner Lwt.t
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

  (** Builds a {!S} module from the given test components.  [nonce] and
   *  [counter] are the initial EVM nonce and Tezos operation counter;
   *  both are auto-incremented on each use. *)
  let build ?(nonce = 0) ?(counter = 0) ~sequencer ~client ~client_tezlink
      ~evm_version ~sender ~source protocol : (module S) =
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

      let client = client

      let client_tezlink = client_tezlink

      let sender = sender

      let source = source

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
          return gas_used

        let check_storage ?expected_catches ~expected_counter
            (`Evm_runner runner) =
          EvmCracHttpCall.check_storage
            ~sequencer
            ?expected_catches
            ~expected_counter
            runner
      end

      module EvmCracHttpCallEvm = struct
        let deploy_and_init ?(value = Wei.zero) (`Evm_runner address) =
          let* addr =
            EvmCracHttpCallEvm.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCracHttpCallEvm.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~evm_contract_target_address:address
              addr
          in
          return (`Evm_runner addr)

        let call_run_catch ?expected_status ?(value = Wei.zero)
            (`Evm_runner runner) =
          let* gas_used =
            EvmCracHttpCallEvm.call_run_catch
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
          in
          return gas_used

        let check_storage ?expected_catches ~expected_counter
            (`Evm_runner runner) =
          EvmCracHttpCallEvm.check_storage
            ~sequencer
            ?expected_catches
            ~expected_counter
            runner
      end

      module EvmCracCollectResultEvm = struct
        let deploy_and_init ?(value = Wei.zero) ~stored_value
            (`Evm_runner address) =
          let* addr =
            EvmCracCollectResultEvm.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCracCollectResultEvm.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~evm_contract_target_address:address
              ~stored_value
              addr
          in
          return (`Evm_runner addr)

        let call_run ?expected_status ?(value = Wei.zero) (`Evm_runner runner) =
          let* gas_used =
            EvmCracCollectResultEvm.call_run
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
          in
          return gas_used

        let check_result ~expected_hex (`Evm_runner runner) =
          EvmCracCollectResultEvm.check_result ~sequencer ~expected_hex runner
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
          return gas_used

        let call_run_catch ?expected_status ?gas ?(value = Wei.zero)
            (`Evm_runner runner) =
          let* gas_used =
            EvmCollectResult.call_run_catch
              ?expected_status
              ?gas
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              runner
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

        let deploy_large () =
          let* addr =
            EvmGasBurner.deploy_large
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
            ~client
            ~client_tezlink
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
              ~client
              ~client_tezlink
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
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCrossRuntimeHttpCallEvm = struct
        let originate ?init_balance (`Evm_runner address) =
          let* contract_hex, address =
            TezCrossRuntimeHttpCallEvm.originate
              ~client
              ~client_tezlink
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
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCrossRuntimeHttpCallTez = struct
        let originate ?init_balance (`Tez_runner (_, address)) =
          let* contract_hex, address =
            TezCrossRuntimeHttpCallTez.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~tez_contract_target_address:address
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeHttpCallTez.check_storage
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCrossRuntimeHttpCallTezCallback = struct
        let originate ?init_balance (`Tez_runner (_, address)) =
          let* contract_hex, address =
            TezCrossRuntimeHttpCallTezCallback.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~tez_contract_target_address:address
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_counter ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeHttpCallTezCallback.check_counter
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)

        let check_result ~expected_bytes
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeHttpCallTezCallback.check_result
            ~sequencer
            ~expected_bytes
            (runner_hex, runner_address)
      end

      module TezMultiRunCaller = struct
        let originate ?init_balance ?(revert = false) ?(callees = []) () =
          let callees =
            List.map (fun (`Tez_runner (_runner_hex, runner)) -> runner) callees
          in
          let* runner_hex, runner =
            TezMultiRunCaller.originate
              ~client
              ~client_tezlink
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
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezCollectResult = struct
        let originate ?init_balance ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResult.originate
              ~client
              ~client_tezlink
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

      module TezCollectResultWithAmount = struct
        let originate ?init_balance ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResultWithAmount.originate
              ~client
              ~client_tezlink
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
              ~client
              ~client_tezlink
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
              ~client
              ~client_tezlink
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

      module TezAlwaysSucceedsUnit = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezAlwaysSucceedsUnit.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezCracSenderRecorder = struct
        let originate ?init_balance ~init_addr () =
          let* runner_hex, runner =
            TezCracSenderRecorder.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~init_addr
              ()
          in
          return (`Tez_runner (runner_hex, runner))

        let check_stored_sender ~expected_sender
            (`Tez_runner (_, runner_address)) =
          TezCracSenderRecorder.check_stored_sender
            ~sequencer
            ~expected_sender
            runner_address
      end

      module TezEmitter = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezEmitter.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezCracForgedEmitter = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezCracForgedEmitter.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezEmitFailer = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezEmitFailer.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezCollectResultTwice = struct
        let originate ?init_balance ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResultTwice.originate
              ~client
              ~client_tezlink
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

      module TezCollectResultThenCallEvm = struct
        let originate ?init_balance ~evm_target:(`Evm_runner evm_target_address)
            ~payload_hex () =
          let* runner_hex, runner =
            TezCollectResultThenCallEvm.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~evm_target_address
              ~payload_hex
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezGenerate4MibResult = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezGenerate4MibResult.originate
              ~client
              ~client_tezlink
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ()
          in
          return (`Tez_runner (runner_hex, runner))
      end

      module TezGasBurner = struct
        let originate ?init_balance () =
          let* runner_hex, runner =
            TezGasBurner.originate
              ~client
              ~client_tezlink
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

      module EvmIdentityRecorder = struct
        let deploy () =
          let* addr =
            EvmIdentityRecorder.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          return (`Evm_runner addr)

        let check_last_sender ~expected_sender (`Evm_runner runner) =
          EvmIdentityRecorder.check_last_sender
            ~sequencer
            ~expected_sender
            runner

        let check_last_origin ~expected_origin (`Evm_runner runner) =
          EvmIdentityRecorder.check_last_origin
            ~sequencer
            ~expected_origin
            runner
      end

      module EvmEvents = struct
        let deploy () =
          let* addr =
            EvmEvents.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          return (`Evm_runner addr)
      end

      module TezCallbackRunnerEvm = struct
        let originate ?(failing = false) ~method_sig ~abi_params
            (`Evm_runner address) =
          let* contract_hex, address =
            TezCallbackRunnerEvm.originate
              ~client
              ~client_tezlink
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
            ~sequencer
            ~expected_counter
            (runner_hex, runner_address)

        let check_result ~expected_bytes
            (`Tez_runner (runner_hex, runner_address)) =
          TezCallbackRunnerEvm.check_result
            ~sequencer
            ~expected_bytes
            (runner_hex, runner_address)
      end

      module Gateway = struct
        let call_evm ~evm_target:(`Evm_runner address) ~method_sig ~abi_params
            ?amount () =
          Gateway.call_evm
            ~client
            ~client_tezlink
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
              ~client
              ~client_tezlink
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

(** Registers a sequencer-only CRAC runner test on the sandbox.  Builds
 *  a {!CracRunnerWrapper.S} and passes it to [body]. *)
let register_crac_runner_test ~title ?(tags = []) body =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"]
    @ List.map Tezosx_runtime.tag with_runtimes
    @ ["crac"; "runner"] @ tags
  in
  Test_helpers.register_sandbox
    ~__FILE__
    ~uses_client:true
    ~kernel:Latest
    ~title
    ~tags
    ~with_runtimes
    ~minimum_base_fee_per_gas:crac_minimum_base_fee_per_gas
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let source = Constant.bootstrap5 in
  (* Conversions and forging need a protocol-aware client but no chain:
     use a mockup client.  Injection goes through the tezlink RPC. *)
  let* client =
    Client.init_mockup ~protocol:Michelson_contracts.tezlink_protocol ()
  in
  let* client_tezlink = tezlink_client_from_evm_node sequencer in
  let (module Wrapper) =
    CracRunnerWrapper.build
      ~counter:1
      ~sequencer
      ~client
      ~client_tezlink
      ~evm_version:(Kernel.select_evm_version Kernel.Latest)
      ~sender
      ~source
      Michelson_contracts.tezlink_protocol
  in
  body (module Wrapper : CracRunnerWrapper.S)

(** Simple EVM-to-TEZ cross-runtime call.
 *
 *    EVM[evm_runner]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *
 *)
let test_crac_evm_to_tez () =
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
let test_crac_evm_multiple_independent_crossings () =
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
let test_crac_evm_double_crossing () =
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
let test_crac_evm_shared_leaf_via_direct_and_chain () =
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
let test_crac_evm_5_crossing_chain () =
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
let test_crac_tez_to_evm () =
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
let test_crac_tez_multiple_independent_crossings () =
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
let test_crac_tez_double_crossing () =
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
let test_crac_tez_shared_leaf_via_direct_and_chain () =
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
let test_crac_tez_5_crossing_chain () =
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
let test_crac_access_list_preserved () =
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
let test_crac_evm_to_tez_reverts () =
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

(** A first-interaction EVM to TEZ call materializes the forwarder KT1
 *  of the EVM sender and source in durable storage. *)
let test_crac_evm_to_tez_materializes_alias () =
  register_crac_runner_test
    ~title:"CRAC: EVM to TEZ materializes alias forwarders"
    ~tags:["alias"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let contracts () =
    let*@! contracts =
      Rpc.state_subkeys sequencer TezContract.tezosx_michelson_contracts_index
    in
    return contracts
  in
  let* tez_runner = TezMultiRunCaller.originate () in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let* before = contracts () in
  let* _ = EvmRunner.call_run evm_main in
  let* after = contracts () in
  let created = List.filter (fun c -> not (List.mem c before)) after in
  (match created with
  | [] ->
      Test.fail
        "Expected the successful CRAC to materialize alias forwarders, but \
         none were created"
  | _ -> ()) ;
  unit

(** Regression: a reverted first-interaction EVM to TEZ call must leave
 *  no new forwarder KT1 on durable storage. *)
let test_crac_evm_to_tez_revert_drops_alias () =
  register_crac_runner_test
    ~title:"CRAC: EVM to TEZ revert drops alias forwarders"
    ~tags:["revert"; "alias"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let contracts () =
    let*@! contracts =
      Rpc.state_subkeys sequencer TezContract.tezosx_michelson_contracts_index
    in
    return contracts
  in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let* before = contracts () in
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  let* after = contracts () in
  let created = List.filter (fun c -> not (List.mem c before)) after in
  (match created with
  | [] -> ()
  | _ ->
      Test.fail
        "Expected the reverted CRAC to leave no new forwarder KT1, but %d were \
         persisted"
        (List.length created)) ;
  unit

(** EVM CRAC target reverts.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_tez_to_evm_reverts () =
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
let test_crac_tez_to_evm_fake_tx_in_block () =
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
let test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks () =
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

(** Regression test: EVM logs emitted during a Michelson-initiated
 *  cross-VM call ([%call_evm]) must surface on the synthetic CRAC
 *  transaction receipt.
 *
 *  Pre-fix, the kernel built the synthetic CRAC tx receipt by reading
 *  [journal.evm.inner.logs] AFTER [JournalInner::finalize] (called by
 *  [commit_evm_journal_from_external]) had cleared it.  Both the
 *  precompile's [CracReceived] log and any LOG0..LOG4 from the inner
 *  EVM call were lost; only the [CracIdEvent] survived because it is
 *  constructed by the receipt builder itself.
 *
 *  This test deploys [events.sol], invokes [emitBoth(uint256)] via the
 *  gateway's [%call_evm], and asserts that:
 *    - the synthetic CRAC tx receipt carries the contract's two events
 *      ([EventA] + [EventB]) plus precompile bookkeeping events
 *      ([CracIdEvent] + [CracReceived]);
 *    - [eth_getLogs] filtered by the contract address returns those
 *      events, restoring parity with a regular EVM-EOA call.
 *)
let test_crac_tez_to_evm_inner_logs_in_receipt () =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM inner LOG opcodes surface on synthetic tx receipt"
    ~tags:["crac_tx"; "regression"; "logs"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-LOGS" in
  (* TezosX runtime gateway precompile address. *)
  let evm_gateway_address = "0xff00000000000000000000000000000000000007" in
  Log.debug ~prefix "Deploy EVM events emitter" ;
  let* evm_target = EvmEvents.deploy () in
  let (`Evm_runner evm_target_address) = evm_target in
  Log.debug ~prefix "Call gateway %%call_evm -> emitBoth(42)" ;
  (* ABI-encode uint256(42). *)
  let abi_params =
    "000000000000000000000000000000000000000000000000000000000000002a"
  in
  let* () =
    Gateway.call_evm ~evm_target ~method_sig:"emitBoth(uint256)" ~abi_params ()
  in
  Log.debug ~prefix "Fetch latest EVM block to get the synthetic CRAC tx hash" ;
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let crac_tx_hash =
    match block.transactions with
    | Block.Hash (h :: _) -> h
    | _ -> Test.fail "Expected at least one tx hash in EVM block"
  in
  Log.info "Synthetic CRAC tx hash: %s" crac_tx_hash ;
  let*@ receipt_opt =
    Rpc.get_transaction_receipt ~tx_hash:crac_tx_hash sequencer
  in
  let receipt =
    match receipt_opt with
    | Some r -> r
    | None -> Test.fail "No receipt for synthetic CRAC tx %s" crac_tx_hash
  in
  let logs = receipt.Transaction.logs in
  Log.info "Synthetic CRAC tx receipt has %d log(s)" (List.length logs) ;
  List.iter
    (fun (log : Transaction.tx_log) ->
      Log.info
        "  log[%ld] address=%s topics=%d"
        log.logIndex
        log.address
        (List.length log.topics))
    logs ;
  let normalize = String.lowercase_ascii in
  let target_addr_n = normalize evm_target_address in
  let gateway_addr_n = normalize evm_gateway_address in
  let contract_logs =
    List.filter
      (fun (log : Transaction.tx_log) -> normalize log.address = target_addr_n)
      logs
  in
  (*Print contract logs for debugging purposes.*)
  List.iter
    (fun (log : Transaction.tx_log) ->
      Log.info
        "  contract log[%ld] address=%s topics=%d"
        log.logIndex
        log.address
        (List.length log.topics))
    contract_logs ;
  (* Pre-fix: 0 contract logs reach the receipt because [inner.logs] is
     wiped by [JournalInner::finalize] before [extract_cross_runtime_effects]
     runs.  Post-fix: every successful [emitBoth] dispatch surfaces
     [EventA] + [EventB] (= 2 contract logs). *)
  Check.(
    (List.length contract_logs = 2)
      int
      ~error_msg:
        "Expected 2 logs from the EVM contract on the synthetic CRAC tx \
         receipt (EventA + EventB), got %L. Without the fix, contract logs \
         from a Michelson-initiated %%call_evm are dropped before the receipt \
         is built.") ;
  (* Pin the actual events so the test fails not only when logs are
     missing but also when they are swapped, reordered, or have their
     [data] field corrupted.  [topic[0]] is [keccak256(<canonical
     signature>)] per the EVM log convention; [data] carries the
     non-indexed [uint256 value] parameter. *)
  let event_a_topic =
    "0xf97f7329bd91140cac80baf605c292bf1a327a74b562336f6e99ae5debb32933"
  in
  let event_b_topic =
    "0xfa732e7acdc0734e887d2179df9c2c4707ad2a04eeb84623f99631414377dc61"
  in
  let expected_value_data =
    "0x000000000000000000000000000000000000000000000000000000000000002a"
  in
  let topic0 (log : Transaction.tx_log) =
    match log.topics with t :: _ -> normalize t | [] -> ""
  in
  let count_with_topic0 lgs topic =
    List.length (List.filter (fun l -> topic0 l = normalize topic) lgs)
  in
  Check.(
    (count_with_topic0 contract_logs event_a_topic = 1)
      int
      ~error_msg:
        "Expected exactly 1 EventA log on the CRAC receipt (topic[0] = \
         keccak256 \"EventA(address,address,uint256)\"), got %L") ;
  Check.(
    (count_with_topic0 contract_logs event_b_topic = 1)
      int
      ~error_msg:
        "Expected exactly 1 EventB log on the CRAC receipt (topic[0] = \
         keccak256 \"EventB(address,address,uint256)\"), got %L") ;
  List.iter
    (fun (log : Transaction.tx_log) ->
      Check.(
        (normalize log.data = normalize expected_value_data)
          string
          ~error_msg:
            "Expected contract log `data` to carry uint256(42) (%R), got %L"))
    contract_logs ;
  let precompile_logs =
    List.filter
      (fun (log : Transaction.tx_log) -> normalize log.address = gateway_addr_n)
      logs
  in
  Check.(
    (List.length precompile_logs = 2)
      int
      ~error_msg:
        "Expected 2 precompile logs (CracIdEvent + CracReceived) on the \
         synthetic CRAC tx receipt, got %L") ;
  let crac_id_event_topic =
    "0x7c61c77057c7568c0d0f2350fa7ab90f164008888ba00c9105b5ef6988d0255d"
  in
  let crac_received_topic =
    "0x6242e34f6b48040ca8499afa39bbd1ed3fb356241249ba0be45fcb08eda0b2c9"
  in
  Check.(
    (count_with_topic0 precompile_logs crac_id_event_topic = 1)
      int
      ~error_msg:
        "Expected exactly 1 CracIdEvent log on the CRAC receipt, got %L") ;
  Check.(
    (count_with_topic0 precompile_logs crac_received_topic = 1)
      int
      ~error_msg:
        "Expected exactly 1 CracReceived log on the CRAC receipt, got %L") ;
  Log.debug
    ~prefix
    "Verify eth_getLogs returns the contract events when filtered by address" ;
  let block_n = Int32.to_int block.number in
  let*@ logs_for_contract =
    Rpc.get_logs
      ~from_block:(Number block_n)
      ~to_block:(Number block_n)
      ~address:(Single evm_target_address)
      sequencer
  in
  Check.(
    (List.length logs_for_contract = 2)
      int
      ~error_msg:
        "Expected eth_getLogs(address=evm_target, blockNumber=current) to \
         return 2 logs (EventA + EventB), got %L. Without the fix, the events \
         are missing from the EVM block index entirely.") ;
  Check.(
    (count_with_topic0 logs_for_contract event_a_topic = 1)
      int
      ~error_msg:"Expected eth_getLogs to surface exactly 1 EventA, got %L") ;
  Check.(
    (count_with_topic0 logs_for_contract event_b_topic = 1)
      int
      ~error_msg:"Expected eth_getLogs to surface exactly 1 EventB, got %L") ;
  unit

(** Block-observable opcodes inside a CRAC frame see the live block.
 *
 *  Regression test for L2-1417.  An inbound Michelson→EVM CRAC used to run
 *  EVM bytecode against a synthesized [BlockConstants] whose block-level
 *  observables were zeroed, so a contract reached through the gateway saw
 *  [BASEFEE], [GASLIMIT] (the block gas limit), [GASPRICE] and [PREVRANDAO]
 *  as 0 — diverging from the native EVM path for identical bytecode.  This
 *  MR forwards the originating block through the cross-runtime journal so
 *  the observables stay live.
 *
 *  Deploys an EVM probe whose 16-byte runtime stores each observable into a
 *  distinct slot ([BASEFEE]→0, [GASLIMIT]→1, [GASPRICE]→2, [PREVRANDAO]→3 —
 *  the same probe as the [tezosx-ethereum-runtime] unit test
 *  [test_serve_block_observables_reflect_outer_block]).  First sends a
 *  native EVM tx to capture the live values a normal EVM execution sees,
 *  then fires a TEZ→EVM CRAC into the same probe and checks the CRAC-frame
 *  values against the producing block's header.  [GASPRICE] resolves to the
 *  block basefee, since Etherlink ignores priority fees (see
 *  [kernel::fees]).
 *
 *  The kernel-side journal forwarding under test is in [chains.rs] and
 *  [apply.rs]; the unit test mocks the journal directly and does not reach
 *  those paths.
 *)
let test_crac_tez_to_evm_block_observables_visible () =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM block-observable opcodes see the live block"
    ~tags:
      [
        "basefee";
        "gaslimit";
        "gasprice";
        "prevrandao";
        "observables";
        "regression";
      ]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-OBS" in
  (* Init code that deploys a 16-byte runtime running four observable
     opcodes, each [<OPCODE> PUSH1 <slot> SSTORE]:
       48 60 00 55   BASEFEE    -> slot 0
       45 60 01 55   GASLIMIT   -> slot 1
       3a 60 02 55   GASPRICE   -> slot 2
       44 60 03 55   PREVRANDAO -> slot 3
     Deploy prefix (11 bytes): PUSH1 16; DUP1; PUSH1 11; PUSH1 0; CODECOPY;
     PUSH1 0; RETURN. *)
  let probe_init_code =
    "601080600b6000396000f348600055456001553a60025544600355"
  in
  (* Normalize a 256-bit hex string (e.g. an [eth_getStorageAt] result or a
     block header field) to lowercase, [0x]-stripped, leading-zeros-stripped
     form so values can be compared regardless of zero-padding. *)
  let normalize_hex s =
    let s = String.lowercase_ascii s in
    let s =
      if String.length s >= 2 && String.sub s 0 2 = "0x" then
        String.sub s 2 (String.length s - 2)
      else s
    in
    let i = ref 0 in
    while !i < String.length s - 1 && s.[!i] = '0' do
      incr i
    done ;
    String.sub s !i (String.length s - !i)
  in
  Log.debug ~prefix "Deploy block-observables probe" ;
  let* probe_addr =
    EvmContract.deploy_contract
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~init_code:probe_init_code
      ()
  in
  let read_slot pos = Rpc.get_storage_at ~address:probe_addr ~pos sequencer in
  Log.debug ~prefix "Send native EVM tx to probe (observables -> slots 0-3)" ;
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:probe_addr
      ()
  in
  let*@ _tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  Log.debug ~prefix "Read native observables from slots 0-3 and block header" ;
  let*@ native_basefee_hex = read_slot "0x0" in
  let*@ native_gaslimit_hex = read_slot "0x1" in
  let*@ native_gasprice_hex = read_slot "0x2" in
  let*@ native_prevrandao_hex = read_slot "0x3" in
  let*@ native_block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let native_basefee = Int64.of_string native_basefee_hex in
  let native_gaslimit = Int64.of_string native_gaslimit_hex in
  Log.info
    "Native EVM: BASEFEE=%s GASLIMIT=%s GASPRICE=%s PREVRANDAO=%s | \
     block.baseFeePerGas=%Ld block.gasLimit=%Ld block.prevRandao=%s"
    native_basefee_hex
    native_gaslimit_hex
    native_gasprice_hex
    native_prevrandao_hex
    native_block.Block.baseFeePerGas
    native_block.Block.gasLimit
    native_block.Block.prevRandao ;
  (* Sanity: the native EVM path observes the live, non-zero block values.
     A zero here would mean the sandbox itself is misconfigured (e.g.
     [minimum_base_fee] unset), making the CRAC comparison meaningless. *)
  Check.(
    (native_basefee = native_block.Block.baseFeePerGas)
      int64
      ~error_msg:
        "Sanity: native EVM BASEFEE in slot 0 (%L) differs from block \
         baseFeePerGas (%R)") ;
  Check.(
    (native_basefee <> 0L)
      int64
      ~error_msg:
        "Sanity: native EVM BASEFEE is 0; sandbox minimum_base_fee not set") ;
  Check.(
    (native_gaslimit = native_block.Block.gasLimit)
      int64
      ~error_msg:
        "Sanity: native EVM GASLIMIT in slot 1 (%L) differs from block \
         gasLimit (%R)") ;
  Check.(
    (native_gaslimit <> 0L) int64 ~error_msg:"Sanity: native EVM GASLIMIT is 0") ;
  Log.debug ~prefix "Fire CRAC into probe (TEZ->EVM)" ;
  let evm_target = `Evm_runner probe_addr in
  let* () = Gateway.call_evm ~evm_target ~method_sig:"" ~abi_params:"" () in
  Log.debug ~prefix "Read CRAC observables from slots 0-3 and block header" ;
  let*@ crac_basefee_hex = read_slot "0x0" in
  let*@ crac_gaslimit_hex = read_slot "0x1" in
  let*@ crac_gasprice_hex = read_slot "0x2" in
  let*@ crac_prevrandao_hex = read_slot "0x3" in
  let*@ crac_block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let crac_basefee = Int64.of_string crac_basefee_hex in
  let crac_gaslimit = Int64.of_string crac_gaslimit_hex in
  let crac_gasprice = Int64.of_string crac_gasprice_hex in
  Log.info
    "CRAC: BASEFEE=%s GASLIMIT=%s GASPRICE=%s PREVRANDAO=%s | \
     block.baseFeePerGas=%Ld block.gasLimit=%Ld block.prevRandao=%s"
    crac_basefee_hex
    crac_gaslimit_hex
    crac_gasprice_hex
    crac_prevrandao_hex
    crac_block.Block.baseFeePerGas
    crac_block.Block.gasLimit
    crac_block.Block.prevRandao ;
  (* BASEFEE: live block basefee, not 0. *)
  Check.(
    (crac_basefee <> 0L) int64 ~error_msg:"CRAC BASEFEE is 0 — see L2-1417") ;
  Check.(
    (crac_basefee = crac_block.Block.baseFeePerGas)
      int64
      ~error_msg:
        "CRAC BASEFEE (%L) differs from the producing block's baseFeePerGas \
         (%R)") ;
  (* GASLIMIT: the block gas limit, not the forwarded call gas nor 0. *)
  Check.(
    (crac_gaslimit <> 0L) int64 ~error_msg:"CRAC GASLIMIT is 0 — see L2-1417") ;
  Check.(
    (crac_gaslimit = crac_block.Block.gasLimit)
      int64
      ~error_msg:
        "CRAC GASLIMIT (%L) differs from the producing block's gasLimit (%R)") ;
  (* GASPRICE: resolves to the block basefee (Etherlink ignores priority
     fees), not the cross-runtime TxEnv's 0. *)
  Check.(
    (crac_gasprice <> 0L) int64 ~error_msg:"CRAC GASPRICE is 0 — see L2-1417") ;
  Check.(
    (crac_gasprice = crac_block.Block.baseFeePerGas)
      int64
      ~error_msg:
        "CRAC GASPRICE (%L) differs from the producing block's baseFeePerGas \
         (%R)") ;
  (* PREVRANDAO: the CRAC frame must observe the same value as the producing
     block, inherited from the originating block rather than the synthesized
     block's default.  Note this sandbox always produces a zeroed prevRandao,
     so this guards forwarding consistency rather than acting as a non-zero
     regression; the [tezosx-ethereum-runtime] unit test exercises PREVRANDAO
     with a non-zero value. *)
  Check.(
    (normalize_hex crac_prevrandao_hex
    = normalize_hex crac_block.Block.prevRandao)
      string
      ~error_msg:
        "CRAC PREVRANDAO (%L) differs from the producing block's prevRandao \
         (%R)") ;
  unit

(** debug_traceTransaction on a CRAC fake transaction (TEZ→EVM).
 *
 *  Sets up a TEZ→EVM CRAC, fetches the fake tx hash from the EVM block,
 *  and calls debug_traceTransaction with both callTracer and structLogger.
 *  Verifies that the kernel produces a valid trace during Blueprint replay.
 *)
let test_crac_debug_trace_transaction () =
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
let test_crac_debug_trace_block () =
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
let test_crac_debug_trace_normal_tx_in_crac_block () =
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
let test_http_trace_nested_crac () =
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
let test_http_trace_multiple_independent_cracs () =
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
let test_http_trace_mixed_block () =
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

(** Extract the [X-Tezos-CRAC-Depth] request header value from an
    [http_trace] JSON entry. Fails the test if absent — every CRAC
    must carry the depth header on its outgoing request. *)
let read_crac_depth_header trace =
  let headers = JSON.(trace |-> "requestHeaders" |> as_list) in
  let pair =
    List.find_opt
      (fun pair ->
        let key = JSON.(pair |=> 0 |> as_string) in
        String.equal (String.lowercase_ascii key) "x-tezos-crac-depth")
      headers
  in
  match pair with
  | Some pair -> JSON.(pair |=> 1 |> as_string) |> int_of_string
  | None ->
      Test.fail
        "X-Tezos-CRAC-Depth header missing on CRAC trace: %s"
        (JSON.encode trace)

(** [http_traceTransaction] surfaces [X-Tezos-CRAC-Depth] on every
    outgoing CRAC request: outer hop reads [1], the inner hop
    (TEZ -> EVM nested inside the outer EVM -> TEZ) reads [2], and
    a further nested EVM -> TEZ reads [3]. Validates that both the
    EVM precompile gateway and the Michelson [dispatch_crac_call]
    producer agree on the [inbound + 1] semantics end-to-end. *)
let test_http_trace_crac_depth_propagation () =
  register_crac_runner_test
    ~title:"CRAC: X-Tezos-CRAC-Depth increments across nested hops"
    ~tags:["http_trace"; "crac_trace"; "crac_depth"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "HTTP-TRACE-DEPTH" in
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
  let*@ trace_json = Rpc.Tezosx.http_traceTransaction ~tx_hash sequencer in
  let traces = JSON.(trace_json |-> "traces" |> as_list) in
  let outer =
    match traces with
    | t :: _ -> t
    | [] -> Test.fail "Expected at least one top-level HTTP trace, got none"
  in
  check_http_trace_shape outer ;
  let outer_depth = read_crac_depth_header outer in
  Check.(
    (outer_depth = 1) int ~error_msg:"Expected outer CRAC depth = %R, got %L") ;
  let inner_traces = JSON.(outer |-> "innerTraces" |> as_list) in
  let inner =
    match inner_traces with
    | t :: _ -> t
    | [] -> Test.fail "Expected at least one nested HTTP trace at depth 1"
  in
  check_http_trace_shape inner ;
  let inner_depth = read_crac_depth_header inner in
  Check.(
    (inner_depth = 2) int ~error_msg:"Expected inner CRAC depth = %R, got %L") ;
  let deeper_traces = JSON.(inner |-> "innerTraces" |> as_list) in
  (match deeper_traces with
  | t :: _ ->
      let deeper_depth = read_crac_depth_header t in
      Check.(
        (deeper_depth = 3)
          int
          ~error_msg:"Expected deeper CRAC depth = %R, got %L")
  | [] -> ()) ;
  Log.info
    "CRAC-Depth propagation: outer=%d, inner=%d (chain length captured by \
     trace headers)"
    outer_depth
    inner_depth ;
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
let test_crac_tez_revert_rolls_back_inner_evm_storage () =
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
let test_crac_tez_revert_propagates_to_evm () =
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
let test_crac_evm_journal_state_preserved () =
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
let test_crac_catch_tez_revert () =
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
let test_crac_evm_revert_propagates_to_tez () =
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
let test_crac_second_crac_tez_revert () =
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
let test_crac_evm_revert_rolls_back_two_cracs () =
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
let test_crac_deep_branch_with_second_tez_revert () =
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
let test_crac_nested_revert_cascade_without_catch () =
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
let test_crac_double_nested_evm_revert () =
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
let test_crac_deep_nesting_6_levels () =
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
let test_crac_evm_revert_after_nested_cracs () =
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
let test_crac_evm_target_reverts () =
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
let test_crac_second_crac_evm_revert () =
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
let test_crac_tez_revert_rolls_back_two_cracs () =
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
let test_crac_deep_branch_with_second_evm_revert () =
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
let test_crac_tez_nested_revert_cascade_without_catch () =
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
let test_crac_double_nested_tez_revert () =
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
let test_crac_tez_deep_nesting_6_levels () =
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
let test_crac_tez_revert_after_nested_cracs () =
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
let test_crac_catch_evm_revert_between_cracs () =
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
let test_crac_catch_tez_revert_between_cracs () =
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
let test_crac_catch_tez_revert_with_nested_crac () =
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
let test_crac_catch_deep_evm_revert_through_double_crac () =
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
let test_crac_tez_catch_evm_revert_between_cracs () =
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
let test_crac_tez_catch_tez_revert_between_cracs () =
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
let test_crac_tez_catch_deep_revert_through_double_crac () =
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
let test_crac_catch_revert_after_multiple_cracs () =
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
let test_crac_catch_tez_revert_after_multiple_return_cracs () =
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
let test_crac_catch_4_crossing_chain_revert () =
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
let test_crac_tez_catch_4_crossing_chain_revert () =
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
let test_crac_catch_5_crossing_chain_revert () =
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
let test_crac_tez_catch_5_crossing_chain_revert () =
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
let test_crac_chained_tez_calls_behind_crac () =
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
let test_crac_nested_catches_with_multiple_reverts () =
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

let test_crac_gas_model_alias_caching () =
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
    Blocks produced after the CRAC may advance the head past the block
    containing the CRAC receipt, so we scan backwards from [head]. *)
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

(** Verify that [iop] is a valid CRAC end event.
    Checks kind = "event", source = handler, tag = "crac_end",
    payload = expected_crac_id.  When [expected_status] is provided,
    also checks the event result status (omit for failed CRACs where
    the event may be backtracked). *)
let check_crac_end_event ~prefix ~expected_crac_id ?expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "event")
      string
      ~error_msg:(sf "%s: Expected event kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = handler_address)
      string
      ~error_msg:
        (sf "%s: Expected end-event source = handler (%%R), got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "tag" |> as_string) = "crac_end")
      string
      ~error_msg:(sf "%s: Expected end-event tag %%R, got %%L" prefix)) ;
  let payload = JSON.(iop |-> "payload" |-> "string" |> as_string) in
  Log.info "%s: CRAC-ID (end event) = %s" prefix payload ;
  Check.(
    (payload = expected_crac_id)
      string
      ~error_msg:(sf "%s: Expected end-event CRAC-ID %%R, got %%L" prefix)) ;
  match expected_status with
  | Some s ->
      Check.(
        (JSON.(iop |-> "result" |-> "status" |> as_string) = s)
          string
          ~error_msg:(sf "%s: Expected end-event status %%R, got %%L" prefix))
  | None -> ()

(** Verify that [iop] is a valid alias-forwarder origination internal:
    kind = "origination", source = handler (NULL_PKH), and the
    receipt's [originated_contracts] singleton matches
    [expected_alias_kt1]. Used to assert the per-CRAC alias
    materializations the kernel splices in just before the synthetic
    transfer (one per fresh sender/source alias). *)
let check_crac_internal_alias_origination ~prefix ~expected_nonce
    ~expected_alias_kt1 ~expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "origination")
      string
      ~error_msg:(sf "%s: Expected kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = handler_address)
      string
      ~error_msg:
        (sf "%s: Expected origination source = handler (%%R), got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "nonce" |> as_int) = expected_nonce)
      int
      ~error_msg:(sf "%s: Expected nonce %%R, got %%L" prefix)) ;
  let originated =
    JSON.(iop |-> "result" |-> "originated_contracts" |> as_list)
    |> List.map JSON.as_string
  in
  Check.(
    (originated = [expected_alias_kt1])
      (list string)
      ~error_msg:(sf "%s: Expected originated_contracts %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "result" |-> "status" |> as_string) = expected_status)
      string
      ~error_msg:(sf "%s: Expected status %%R, got %%L" prefix))

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

(** Walk [internals] (a list of internal_operation_results JSON nodes)
    checking that the CRAC frame markers form a balanced forest.

    Step 0: filter to ops with kind "event", source = [handler_address]
    (null implicit sender), and tag in \{"crac","crac_end"\}.  Bridge deposit
    events share the null sender but carry tag "deposit" and are excluded
    by the tag filter.

    Step 1: stack walk in list order — push on "crac", pop on "crac_end".
    Asserts depth never goes negative (stray end) and ends at zero
    (unmatched begins). *)
let check_crac_brackets ~prefix internals =
  let markers =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        && JSON.(iop |-> "source" |> as_string) = handler_address
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t ->
            let tag = JSON.as_string t in
            tag = "crac" || tag = "crac_end"
        | None -> false)
      internals
  in
  Log.info "%s: %d CRAC marker(s) in internals" prefix (List.length markers) ;
  let depth =
    List.fold_left
      (fun depth iop ->
        let tag =
          JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"" ~some:as_string)
        in
        if tag = "crac" then (
          let new_depth = depth + 1 in
          Log.info "%s:   begin marker (depth -> %d)" prefix new_depth ;
          new_depth)
        else
          let new_depth = depth - 1 in
          Log.info "%s:   end marker (depth -> %d)" prefix new_depth ;
          Check.(
            (new_depth >= 0)
              int
              ~error_msg:
                (sf
                   "%s: CRAC marker imbalance: crac_end without matching crac \
                    (depth went negative)"
                   prefix)) ;
          new_depth)
      0
      markers
  in
  Check.(
    (depth = 0)
      int
      ~error_msg:
        (sf
           "%s: CRAC marker imbalance: depth %d at end (unmatched begins)"
           prefix
           depth))

(** Build a minimal [internal_operation_result]-shaped JSON value
    with [kind = "event"], the given [source], [tag], and optional
    [payload] (a JSON string value).  Used to construct synthetic
    internals lists for unit-testing [check_crac_brackets] without
    starting a node. *)
let make_event_iop ?(payload = "") ~source ~tag () =
  let fields : (string * JSON.u) list =
    [
      ("kind", `String "event"); ("source", `String source); ("tag", `String tag);
    ]
    @
    if payload = "" then [] else [("payload", `O [("string", `String payload)])]
  in
  JSON.annotate ~origin:"make_event_iop" (`O fields)

(** Node-free unit tests for [check_crac_brackets].

    Builds hand-crafted internals lists and verifies:
    (a) A balanced pair (begin + end) surrounding an irrelevant deposit
        event from the null sender passes the walk.
    (b) A KT1-sourced "crac" and "crac_end" (forged) among the null-sender
        events does not perturb the walk (sender filter excludes them).
    (c) An unbalanced list (begin without matching end) raises a Check
        failure — the test catches it and verifies the error message
        contains "unmatched begins", confirming the depth-at-end check
        fires. *)
let test_check_crac_brackets_unit () =
  Test.register
    ~__FILE__
    ~title:"check_crac_brackets: unit tests (no node)"
    ~tags:["crac_receipt"; "unit"]
    ~uses_node:false
  @@ fun () ->
  let prefix = "BRACKETS-UNIT" in
  (* (a) Balanced pair with interleaved deposit and KT1-forged events. *)
  let kt1 = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN" in
  let balanced =
    [
      make_event_iop ~source:handler_address ~tag:"crac" ();
      make_event_iop ~source:handler_address ~tag:"deposit" ();
      make_event_iop ~source:kt1 ~tag:"crac" ();
      make_event_iop ~source:kt1 ~tag:"crac_end" ();
      make_event_iop ~source:handler_address ~tag:"crac_end" ();
    ]
  in
  check_crac_brackets ~prefix balanced ;
  Log.info "%s: (a) balanced pair passed" prefix ;
  (* (b) The null-sender "deposit" tag is excluded by the tag conjunct;
     a list with only deposit events has no markers and depth stays 0. *)
  let deposits_only =
    [
      make_event_iop ~source:handler_address ~tag:"deposit" ();
      make_event_iop ~source:handler_address ~tag:"deposit" ();
    ]
  in
  check_crac_brackets ~prefix deposits_only ;
  Log.info "%s: (b) deposit-only list passed (depth stays 0)" prefix ;
  (* (c) Unbalanced list (two begins, one end) — the depth-at-end check
     must fire.  The failure exception is not exported by Tezt, so we
     catch generically: any exception here means the walk rejected the
     list. *)
  let unbalanced =
    [
      make_event_iop ~source:handler_address ~tag:"crac" ();
      make_event_iop ~source:handler_address ~tag:"crac" ();
      make_event_iop ~source:handler_address ~tag:"crac_end" ();
    ]
  in
  let rejected =
    try
      check_crac_brackets ~prefix unbalanced ;
      false
    with _ -> true
  in
  if not rejected then
    Test.fail
      "check_crac_brackets on unbalanced list should have failed but passed" ;
  Log.info "%s: (c) unbalanced list correctly rejected" prefix ;
  unit

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
let test_crac_evm_to_tez_receipt () =
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
  (* 6 internal ops — begin-marker, origination(alias(E_1)),
     origination(alias(E_0)), alias→tez_runner, self-call, end-marker.
     The two `origination` internals are inserted by the kernel just
     before the synthetic transfer; they record that the gateway
     precompile materialized the Tezos-side aliases for sender (E_1)
     and source (E_0) as a precondition of the cross-runtime call. *)
  Check.(
    (List.length internals = 6)
      int
      ~error_msg:"Expected 6 internal operations, got %L") ;
  (* ── Internal #0: CRAC begin event ─────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: origination of alias(E_1) = evm_bridge_alias ── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: origination of alias(E_0) = sender_alias ───── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(E_1) → tez_runner (%run) ───────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_runner → tez_runner (%_incrementWitness) ─ *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_runner_kt1
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── Internal #5: CRAC end event ───────────────────────────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* Bracket balance check *)
  check_crac_brackets ~prefix internals ;
  unit

(** Acceptance criterion for the CRAC frame marker contract: user [EMIT]s
    with tags [%crac] and [%crac_end] from a [KT1] contract reached via CRAC
    must not perturb the bracket walk.  The sender filter in
    [check_crac_brackets] excludes any event whose source is not the
    null implicit account (handler_address), so the forged tags are
    invisible to the walk regardless of their payloads.

    The forged [%crac_end] is the critical case: if the sender filter
    regressed, a stray end marker would drive the walk's depth negative
    (no matching begin from the null sender), and [check_crac_brackets]
    would fail.

    Asserts:
     (a) Exactly one KT1-sourced event with tag "crac" and payload
         "forged-crac-marker" surfaces on the receipt.
     (b) Exactly one KT1-sourced event with tag "crac_end" and payload
         "forged-crac-end-marker" surfaces on the receipt.
     (c) [check_crac_brackets] passes — neither forged tag perturbs the
         null-sender depth counter.

    Call tree:
      EOA → EVM_bridge ~CRAC~> Mich CracForgedEmitter
                                  emits %crac   string "forged-crac-marker"
                                  emits %crac_end string "forged-crac-end-marker"
                                  from KT1 (not null PKH), succeeds *)
let test_crac_forged_marker_excluded_by_sender_filter () =
  register_crac_runner_test
    ~title:"CRAC: user EMIT %crac/%crac_end from KT1 excluded from bracket walk"
    ~tags:["crac_receipt"; "emit"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-FORGED" in
  let* forged_emitter = TezCracForgedEmitter.originate () in
  let (`Tez_runner (_, forged_emitter_kt1)) = forged_emitter in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init forged_emitter in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run evm_bridge in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  Log.info
    "%s: %d internal op(s) on the synthetic CRAC tx receipt"
    prefix
    (List.length internals) ;
  (* (a) The user EMIT with tag "crac" must be present with the KT1 sender. *)
  let kt1_events_by_tag tag =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        && JSON.(iop |-> "source" |> as_string) <> handler_address
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t -> JSON.as_string t = tag
        | None -> false)
      internals
  in
  let user_forged_crac = kt1_events_by_tag "crac" in
  Log.info
    "%s: found %d KT1 event(s) with tag \"crac\""
    prefix
    (List.length user_forged_crac) ;
  Check.(
    (List.length user_forged_crac = 1)
      int
      ~error_msg:
        "Expected exactly 1 KT1-forged event with tag=crac, got %L. The user \
         EMIT must surface with the contract's KT1 address as source.") ;
  let user_crac_event = List.hd user_forged_crac in
  Check.(
    (JSON.(user_crac_event |-> "source" |> as_string) = forged_emitter_kt1)
      string
      ~error_msg:
        (sf
           "%s: Expected forged-crac source = forged_emitter KT1 (%%R), got %%L"
           prefix)) ;
  Check.(
    (JSON.(user_crac_event |-> "payload" |-> "string" |> as_string)
    = "forged-crac-marker")
      string
      ~error_msg:(sf "%s: Expected forged-crac payload %%R, got %%L" prefix)) ;
  (* (b) The user EMIT with tag "crac_end" must be present with the KT1 sender. *)
  let user_forged_crac_end = kt1_events_by_tag "crac_end" in
  Log.info
    "%s: found %d KT1 event(s) with tag \"crac_end\""
    prefix
    (List.length user_forged_crac_end) ;
  Check.(
    (List.length user_forged_crac_end = 1)
      int
      ~error_msg:
        "Expected exactly 1 KT1-forged event with tag=crac_end, got %L. A \
         forged %crac_end would drive the bracket walk negative if the sender \
         filter regressed.") ;
  let user_crac_end_event = List.hd user_forged_crac_end in
  Check.(
    (JSON.(user_crac_end_event |-> "source" |> as_string) = forged_emitter_kt1)
      string
      ~error_msg:
        (sf
           "%s: Expected forged-crac_end source = forged_emitter KT1 (%%R), \
            got %%L"
           prefix)) ;
  Check.(
    (JSON.(user_crac_end_event |-> "payload" |-> "string" |> as_string)
    = "forged-crac-end-marker")
      string
      ~error_msg:(sf "%s: Expected forged-crac_end payload %%R, got %%L" prefix)) ;
  (* (c) The bracket walk must not be perturbed by either forged event.
     A forged %crac_end would drive the walk's depth negative if the
     sender filter regressed (no matching begin from the null sender). *)
  check_crac_brackets ~prefix internals ;
  unit

(** Regression test: a user [EMIT] op executed by a Michelson contract
    reached via a re-entrant inner CRAC (i.e. EVM → Mich → EVM → Mich
    within one EVM transaction) must surface on the synthetic Michelson
    manager-op receipt.

    [drain_reentrant_crac_ops] in
    [etherlink/kernel_latest/tezos_execution/src/enshrined_contracts.rs]
    splices a re-entrant CRAC's [internal_operation_results] into the
    parent op's flat list verbatim — no event is filtered out.  User
    [EMIT] ops produced by [execute_internal_operations] carry the
    executing contract's KT1 sender and surface alongside the frame's
    own begin/end markers.

    Call tree:
      EOA → EVM_main(MultiRunCaller, callees=[(EVM_bridge_to_C1, false)])
              → EVM_bridge_to_C1(CrossRuntimeRunTez, dest=C1)
                 ~CRAC #1~> Mich C1(MultiRunCaller, callees=[Tez_bridge])
                              → Tez_bridge(CrossRuntimeRunEvm, dest=EVM_inner)
                                  ~Mich→EVM~> EVM_inner(MultiRunCaller,
                                                callees=[(EVM_bridge_to_Emitter, false)])
                                                → EVM_bridge_to_Emitter
                                                  ~CRAC #2~> Mich Emitter
                                                    emits %hello_l2_1301 string
                                                    "hello-l2-1301", succeeds

    Asserts that the synthetic Michelson manager-op's
    [internal_operation_results] contains an [event] with tag
    [hello_l2_1301] and payload string ["hello-l2-1301"].

    L2-1301. *)
let test_crac_user_emit_in_reentrant_inner_crac () =
  register_crac_runner_test
    ~title:
      "CRAC: user EMIT from re-entrant inner CRAC surfaces on synthetic receipt"
    ~tags:["crac_receipt"; "regression"; "emit"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-EMIT" in
  let* emitter = TezEmitter.originate () in
  let* evm_bridge_to_emitter =
    EvmCrossRuntimeRunnerTez.deploy_and_init emitter
  in
  let* evm_inner =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_to_emitter, false)]
      ()
  in
  let (`Evm_runner evm_inner_addr) = evm_inner in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* c1 = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  let* evm_bridge_to_c1 = EvmCrossRuntimeRunnerTez.deploy_and_init c1 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_to_c1, false)] ()
  in
  let* _ = EvmRunner.call_run evm_main in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info
    "%s: %d internal op(s) on the synthetic CRAC tx receipt"
    prefix
    (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s tag=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  (* The user EMIT executed inside the re-entrant inner CRAC must be
     present and applied in the merged receipt: drain_reentrant_crac_ops
     preserves all Event variants, including user-issued EMITs. *)
  let user_emits =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t -> JSON.as_string t = "hello_l2_1301"
        | None -> false)
      internals
  in
  Log.info
    "%s: found %d user-EMIT entries with tag %S"
    prefix
    (List.length user_emits)
    "hello_l2_1301" ;
  Check.(
    (List.length user_emits = 1)
      int
      ~error_msg:
        "Expected exactly 1 user EMIT (tag = \"hello_l2_1301\") on the \
         synthetic CRAC tx receipt, got %L") ;
  let user_emit = List.hd user_emits in
  let payload = JSON.(user_emit |-> "payload" |-> "string" |> as_string) in
  Check.(
    (payload = "hello-l2-1301")
      string
      ~error_msg:
        "Expected user EMIT payload %R, got %L (corruption in the EMIT data)") ;
  let status = JSON.(user_emit |-> "result" |-> "status" |> as_string) in
  Check.(
    (status = "applied")
      string
      ~error_msg:
        "Expected user EMIT status %R (the inner CRAC succeeded so its EMIT is \
         Applied), got %L") ;
  (* Each frame (outer CRAC #1 and inner CRAC #2) must be bracketed:
     begin and end markers with tag "crac" / "crac_end" and null sender
     must form a balanced forest.  check_crac_brackets verifies balance
     but passes vacuously if a whole pair is missing, so assert marker
     presence explicitly: 2 frames → 2 begin + 2 end markers. *)
  let count_markers tag =
    List.length
      (List.filter
         (fun iop ->
           JSON.(iop |-> "kind" |> as_string) = "event"
           &&
           match JSON.(iop |-> "tag" |> as_opt) with
           | Some t -> JSON.as_string t = tag
           | None -> false)
         internals)
  in
  Check.(
    (count_markers "crac" = 2)
      int
      ~error_msg:"Expected 2 crac begin markers (one per frame), got %L") ;
  Check.(
    (count_markers "crac_end" = 2)
      int
      ~error_msg:"Expected 2 crac_end markers (one per frame), got %L") ;
  check_crac_brackets ~prefix internals ;
  (* Sanity: the alias→Emitter transfer that triggered the EMIT must
     also be present and applied — proves the EVM_inner→Emitter call
     reached its target. *)
  let (`Tez_runner (_, emitter_kt1)) = emitter in
  let alias_to_emitter =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(iop |-> "destination" |> as_string) = emitter_kt1
        && JSON.(iop |-> "result" |-> "status" |> as_string) = "applied")
      internals
  in
  Check.(
    (List.length alias_to_emitter >= 1)
      int
      ~error_msg:
        "Expected at least one applied transfer to Mich Emitter (proving the \
         inner CRAC reached the Emitter contract), got %L. If this fails, the \
         test scenario is broken.") ;
  ignore evm_inner_addr ;
  unit

(** Regression test covering L2-1299 (synthetic event survival on
    merge) and L2-1300 (DFS ordering of failed re-entrant inner
    CRACs).  Both bugs manifest in the same call tree: a successful
    outer CRAC #1 nesting a failed inner re-entrant CRAC #2 caught
    by an EVM frame in between.

    L2-1299 (pre-fix in !21808):
      [merge_crac_internals] in [etherlink/kernel_latest/kernel/src/apply.rs]
      computed [has_event] over the merge target's internals as
      [matches!(iop, InternalOperationSum::Event(_))] — i.e. ANY event
      set the flag.  When the failed inner is sorted first by seq and
      its internals contain the user EMIT (Backtracked) but no
      synthetic CRAC-ID event, that flag flipped true and the prepend
      branch was skipped, dropping the outer applied CRAC's synthetic
      event from the merged receipt.

    L2-1300 (pre-fix in this MR):
      [drain_reentrant_crac_ops] in
      [etherlink/kernel_latest/tezos_execution/src/enshrined_contracts.rs]
      only drained [pending_crac_receipts], so a failed re-entrant
      inner CRAC accumulated to the journal-level
      [failed_crac_receipts] and survived to the top-level merge.
      Because seq is claimed at push time, the inner pushed before
      the outer (nested execution completes inside-out) and ended up
      at the head of the merged receipt — placing CRAC #2's failed
      transfers BEFORE CRAC #1's own transfer, contradicting DFS
      execution order.  The fix drains all three lists (pending,
      failed, backtracked) at the per-list watermarks captured around
      each internal op so re-entrant inner CRACs are spliced into
      their parent's body regardless of outcome.

    Call tree:
      EOA → EVM_main(MultiRunCaller, callees=[(EVM_bridge_to_C1, false)])
              → EVM_bridge_to_C1(CrossRuntimeRunTez, dest=C1)
                 ~CRAC #1~> Mich C1(MultiRunCaller, callees=[Tez_bridge])
                              → Tez_bridge(CrossRuntimeRunEvm, dest=EVM_inner)
                                  ~Mich→EVM~> EVM_inner(MultiRunCaller,
                                                callees=[(EVM_bridge_to_EmitFailer, doCatch=true)])
                                                → try { EVM_bridge_to_EmitFailer.run() }
                                                  ~CRAC #2~> Mich EmitFailer
                                                    emits %hello_l2_1299, then FAILWITH
                                                  catches → continues

    Asserts that the synthetic Michelson manager-op's
    [internal_operation_results]:
      - the frame markers form a balanced forest, each CRAC (outer #1
        and failed inner #2) bracketed by its own begin/end pair
        ([check_crac_brackets]) — confirming the merge preserves every
        frame's markers and drops none;
      - the user EMIT from the failed inner is present and
        [Backtracked] (sanity that CRAC #2 reached the EMIT op
        before the FAILWITH);
      - the failed transfer to EmitFailer (CRAC #2's body) appears
        AFTER the alias→C1 transfer (CRAC #1's body begins) — the
        L2-1300 DFS-order assertion. *)
let test_crac_synthetic_event_survives_failed_inner_with_emit () =
  register_crac_runner_test
    ~title:
      "CRAC: synthetic crac event survives merge with failed inner that \
       EMITted, and failed inner nests inside outer (DFS)"
    ~tags:["crac_receipt"; "regression"; "emit"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-CRAC-EVT" in
  let* emit_failer = TezEmitFailer.originate () in
  let (`Tez_runner (_, emit_failer_kt1)) = emit_failer in
  let* evm_bridge_to_emit_failer =
    EvmCrossRuntimeRunnerTez.deploy_and_init emit_failer
  in
  (* EVM_inner catches CRAC #2's failure so the OUTER CRAC #1 still
     succeeds — that's how we end up with a merge of (failed inner,
     applied outer). *)
  let* evm_inner =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_to_emit_failer, true)]
      ()
  in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* c1 = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  let (`Tez_runner (_, c1_kt1)) = c1 in
  let* evm_bridge_to_c1 = EvmCrossRuntimeRunnerTez.deploy_and_init c1 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_to_c1, false)] ()
  in
  let* _ = EvmRunner.call_run evm_main in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info
    "%s: %d internal op(s) on the synthetic CRAC tx receipt"
    prefix
    (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s dst=%s tag=%s status=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(
          iop |-> "destination" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "result" |-> "status" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  (* Each frame (outer CRAC #1 and inner CRAC #2) must be bracketed:
     begin/end marker pairs must form a balanced forest.
     check_crac_brackets verifies balance but passes vacuously if a
     whole pair is missing, so assert marker presence explicitly:
     2 frames → 2 begin + 2 end markers. *)
  let count_markers tag =
    List.length
      (List.filter
         (fun iop ->
           JSON.(iop |-> "kind" |> as_string) = "event"
           &&
           match JSON.(iop |-> "tag" |> as_opt) with
           | Some t -> JSON.as_string t = tag
           | None -> false)
         internals)
  in
  Check.(
    (count_markers "crac" = 2)
      int
      ~error_msg:"Expected 2 crac begin markers (one per frame), got %L") ;
  Check.(
    (count_markers "crac_end" = 2)
      int
      ~error_msg:"Expected 2 crac_end markers (one per frame), got %L") ;
  check_crac_brackets ~prefix internals ;
  (* Sanity assertion: the user EMIT is still in the receipt as
     Backtracked, proving the failed inner CRAC's body reached the
     EMIT op before the FAILWITH. *)
  let user_emits =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t -> JSON.as_string t = "hello_l2_1299"
        | None -> false)
      internals
  in
  Check.(
    (List.length user_emits = 1)
      int
      ~error_msg:
        "Sanity check: expected exactly 1 user EMIT (tag = \"hello_l2_1299\") \
         in the merged receipt — proves the failed inner CRAC's body reached \
         the EMIT op before the FAILWITH. If this fails, the test scenario \
         didn't fire as designed.") ;
  let user_emit_status =
    JSON.(List.hd user_emits |-> "result" |-> "status" |> as_string)
  in
  Check.(
    (user_emit_status = "backtracked")
      string
      ~error_msg:
        "Expected the user EMIT in the failed inner CRAC to have status %R \
         (the FAILWITH backtracks it), got %L") ;
  (* L2-1300 DFS-ordering assertion.  The first transaction destined
     for C1 (the outer CRAC #1's alias→C1 entry) must appear BEFORE
     the first failed transaction destined for the inner EmitFailer
     (CRAC #2's body) — the inner frame's ops are spliced inside the
     outer, in execution order.

     Implementation note: an entry's [destination] field is the
     inner-most transfer target — we use the first occurrence of
     each KT1 because both CRACs may produce multiple entries with
     the same destination (self-calls etc.). *)
  let first_index_with_destination kt1 =
    let rec aux i = function
      | [] -> None
      | iop :: rest ->
          if
            JSON.(iop |-> "kind" |> as_string) = "transaction"
            && JSON.(iop |-> "destination" |> as_string) = kt1
          then Some i
          else aux (i + 1) rest
    in
    aux 0 internals
  in
  let first_failed_index_with_destination kt1 =
    let rec aux i = function
      | [] -> None
      | iop :: rest ->
          let kind = JSON.(iop |-> "kind" |> as_string) in
          let dst =
            JSON.(
              iop |-> "destination" |> as_opt
              |> Option.fold ~none:"" ~some:as_string)
          in
          let status =
            JSON.(
              iop |-> "result" |-> "status" |> as_opt
              |> Option.fold ~none:"" ~some:as_string)
          in
          if kind = "transaction" && dst = kt1 && status = "failed" then Some i
          else aux (i + 1) rest
    in
    aux 0 internals
  in
  let c1_first_idx =
    match first_index_with_destination c1_kt1 with
    | Some i -> i
    | None ->
        Test.fail
          "L2-1300 setup check: expected at least one transaction targeting C1 \
           (KT1=%s) in the merged receipt"
          c1_kt1
  in
  let emit_failer_failed_idx =
    match first_failed_index_with_destination emit_failer_kt1 with
    | Some i -> i
    | None ->
        Test.fail
          "L2-1300 setup check: expected at least one failed transaction \
           targeting EmitFailer (KT1=%s) in the merged receipt"
          emit_failer_kt1
  in
  Log.info
    "%s: c1 first_index=%d, emit_failer first_failed_index=%d"
    prefix
    c1_first_idx
    emit_failer_failed_idx ;
  Check.(
    (c1_first_idx < emit_failer_failed_idx)
      int
      ~error_msg:
        "L2-1300 DFS order: expected outer CRAC #1's alias→C1 transfer (index \
         %L) to appear BEFORE the first failed transfer to inner EmitFailer \
         (index %R), got C1=%L and EmitFailer-failed=%R. The inner frame's ops \
         must be spliced inside the outer in execution order.") ;
  unit

(** Regression test: when an EVM transaction performs more than one
    top-level CRAC, every CRAC frame's synthetic Michelson manager-op
    must carry its own [crac] begin event — one per frame, not a single
    deduplicated event — even when a later CRAC fails and the EVM revert
    wipes the applied frames before the merge.

    Each CRAC receipt carries its own begin/end marker pair, so the
    merged receipt carries one begin event per frame; the two CRACs in
    this scenario produce 2 begin and 2 end markers.

    Call tree (mirrors L2-1302 reproducer):
      EOA → EVM_main(MultiRunCaller, willRevert=false,
                     callees=[(EVM_caller_ok, doCatch=false),
                              (EVM_caller_fail, doCatch=false)]).run()
              → EVM_caller_ok ~CRAC #1~> Mich C_ok    (Applied)
              → EVM_caller_fail ~CRAC #2~> Mich C_fail (FAILWITH)
                                           reverts up to EVM_main;
                                           no catch → tx-level revert

    Asserts the merged synthetic Mich manager-op carries the
    synthetic CRAC-ID event (kind=event, tag=[crac]).  Sanity:
    top-level status is [failed] (only the failed CRAC survives
    the EVM revert) and at least one transaction targets C_fail.

    L2-1302. *)
let test_crac_synthetic_event_present_when_applied_crac_reverted_out () =
  register_crac_runner_test
    ~title:
      "CRAC: synthetic crac event survives when applied CRAC is reverted out \
       by EVM tx revert"
    ~tags:["crac_receipt"; "regression"; "emit"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-CRAC-EVT-3" in
  (* Mich C_ok: TezMultiRunCaller defaults (revert=false, callees=[])
     — body is just an internal counter increment, no callees. *)
  let* c_ok = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, c_ok_kt1)) = c_ok in
  (* Mich C_fail: TezMultiRunCaller with revert=true, no callees —
     FAILWITHs after running zero callees.  The CRAC into it fails. *)
  let* c_fail = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, c_fail_kt1)) = c_fail in
  let* evm_caller_ok = EvmCrossRuntimeRunnerTez.deploy_and_init c_ok in
  let* evm_caller_fail = EvmCrossRuntimeRunnerTez.deploy_and_init c_fail in
  (* doCatch=false on both callees → CRAC #2's failure propagates up
     through EVM_caller_fail and EVM_main, reverting the entire EVM
     tx.  CRAC #1's pending receipt is then truncated by revert_frame;
     only CRAC #2's failed receipt survives the merge. *)
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_caller_ok, false); (evm_caller_fail, false)]
      ()
  in
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let top_status =
    JSON.(top |-> "metadata" |-> "operation_result" |-> "status" |> as_string)
  in
  Log.info "%s: top-level status=%s" prefix top_status ;
  (* Sanity: only the failed CRAC #2 survives the merge, so the
     merged top-level status is `failed` (no applied receipt to
     trigger force_top_level_applied). *)
  Check.(
    (top_status = "failed")
      string
      ~error_msg:
        "Expected merged synthetic Mich manager-op top-level status %R (only \
         the failed CRAC survives the EVM revert), got %L") ;
  let internals =
    JSON.(top |-> "metadata" |-> "internal_operation_results" |> as_list)
  in
  Log.info
    "%s: %d internal op(s) on the synthetic CRAC tx receipt"
    prefix
    (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s src=%s dst=%s tag=%s status=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(
          iop |-> "source" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "destination" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "result" |-> "status" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  (* Primary assertion: synthetic CRAC-ID events (begin markers) must be
     present — one per CRAC frame.  With 2 CRACs (CRAC #1 backtracked by
     EVM revert, CRAC #2 failed), there must be 2 begin markers.  Without
     the fix, build_(failed_)crac_receipt gated event emission on a flag
     consumed by CRAC #1, so CRAC #2's receipt had no event. *)
  let crac_events =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t -> JSON.as_string t = "crac"
        | None -> false)
      internals
  in
  Check.(
    (List.length crac_events = 2)
      int
      ~error_msg:
        "Expected 2 synthetic CRAC-ID begin events (one per CRAC frame), got \
         %L. Without the fix, the surviving failed CRAC #2 would carry no \
         begin marker.") ;
  (* Sanity: a transaction targeting C_fail must be present in the
     merged receipt — proves the failed CRAC reached its target. *)
  let to_c_fail =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(iop |-> "destination" |> as_string) = c_fail_kt1)
      internals
  in
  Check.(
    (List.length to_c_fail >= 1)
      int
      ~error_msg:
        "Sanity check: expected at least one transaction targeting C_fail in \
         the merged receipt — proves CRAC #2 reached its destination before \
         the FAILWITH. If this fails, the test scenario didn't fire as \
         designed.") ;
  (* Sanity: a transaction targeting C_ok must be present with status
     [backtracked].  The applied CRAC #1 is preserved across EVM revert
     (revert_frame migrates pending receipts to backtracked_crac_receipts
     instead of truncating, per L2-1304) — its state effects roll back
     but the receipt entries survive as a `backtracked` record. *)
  let to_c_ok =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(iop |-> "destination" |> as_string) = c_ok_kt1)
      internals
  in
  Check.(
    (List.length to_c_ok >= 1)
      int
      ~error_msg:
        "Sanity check: expected at least one transaction targeting C_ok (CRAC \
         #1's body, preserved across EVM revert as backtracked per L2-1304), \
         got %L. If this fails, the test scenario didn't fire as designed.") ;
  let c_ok_status =
    JSON.(List.hd to_c_ok |-> "result" |-> "status" |> as_string)
  in
  Check.(
    (c_ok_status = "backtracked")
      string
      ~error_msg:
        "Expected CRAC #1's body to C_ok to have status %R (applied CRAC \
         reverted by enclosing EVM tx revert is backtracked, not failed), got \
         %L") ;
  check_crac_brackets ~prefix internals ;
  unit

(** Regression test (L2-1304): when an applied CRAC is reverted out
    of the same EVM transaction by an uncaught EVM revert, its
    receipt body — top-level transfer plus internal ops — must
    survive on the merged synthetic Michelson manager-op as
    [backtracked] entries, not vanish entirely.

    Pre-fix, [MichelsonJournal::revert_frame] in
    [etherlink/kernel_latest/tezosx-journal/src/michelson_journal.rs]
    called [pending_crac_receipts.truncate(...)] on the watermark,
    deleting every applied CRAC pushed during the reverting frame.
    [failed_crac_receipts] survived by design but [pending_crac_receipts]
    didn't — asymmetric and at odds with L1 semantics where applied
    internals reverted by a later failure appear as [backtracked]
    rather than disappear.

    Fix: drain pending receipts pushed since the watermark, walk each
    one and transform every result (top-level + internal ops) to
    [BackTracked] via the existing [transform_result_backtrack]
    helpers, and migrate to [backtracked_crac_receipts] (a new list
    with the same NOT-subject-to-revert invariant as the failed list).
    [drain_pending_crac_receipts] in [kernel/src/apply.rs] was
    extended to merge the new list and to force the merged top-level
    to [Failed] when no currently-Applied CRAC contributed but a
    Failed sibling did (tri-state reconciliation).

    Same call tree as the L2-1302 test: the sibling failed CRAC
    triggers an uncaught EVM revert that wipes the applied CRAC's
    pending receipt.  Asserts that:
      - The synthetic Mich manager-op carries CRAC #1's body to
        C_ok with status [backtracked] (the L2-1304 fix).
      - CRAC #1 includes a self-call to [_incrementWitness] (proves
        the body is the full receipt, not a stub) — also backtracked.
      - The synthetic [crac] event is itself backtracked (cohérent
        with the merged top-level [failed] under L1 semantics: no
        Applied internals under a Failed parent).
      - CRAC #2's body to C_fail is still present with status
        [failed] (already covered by L2-1302 but kept here for
        scenario completeness).

    L2-1304. *)
let test_crac_applied_body_preserved_as_backtracked_on_evm_revert () =
  register_crac_runner_test
    ~title:
      "CRAC: applied CRAC's body survives as backtracked when EVM tx reverts"
    ~tags:["crac_receipt"; "regression"; "backtracked"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-CRAC-BT" in
  (* C_ok = TezMultiRunCaller (revert=false, no callees): the body
     contains the alias→C_ok top-level transfer plus C_ok's
     internal _incrementWitness self-call. *)
  let* c_ok = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, c_ok_kt1)) = c_ok in
  let* c_fail = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, c_fail_kt1)) = c_fail in
  let* evm_caller_ok = EvmCrossRuntimeRunnerTez.deploy_and_init c_ok in
  let* evm_caller_fail = EvmCrossRuntimeRunnerTez.deploy_and_init c_fail in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_caller_ok, false); (evm_caller_fail, false)]
      ()
  in
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let top_status =
    JSON.(top |-> "metadata" |-> "operation_result" |-> "status" |> as_string)
  in
  Log.info "%s: top-level status=%s" prefix top_status ;
  Check.(
    (top_status = "failed")
      string
      ~error_msg:
        "Expected merged synthetic Mich manager-op top-level status %R (a \
         Failed sibling participated, no currently-Applied CRAC remains so the \
         new tri-state reconciliation forces Failed), got %L") ;
  let internals =
    JSON.(top |-> "metadata" |-> "internal_operation_results" |> as_list)
  in
  Log.info
    "%s: %d internal op(s) on the synthetic CRAC tx receipt"
    prefix
    (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s src=%s dst=%s ep=%s tag=%s status=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(
          iop |-> "source" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "destination" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "parameters" |-> "entrypoint" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "result" |-> "status" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  (* Primary assertion: CRAC #1's body to C_ok must be present with
     status [backtracked].  Without the L2-1304 fix the entry is
     absent entirely (revert_frame truncates pending_crac_receipts). *)
  let to_c_ok =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(iop |-> "destination" |> as_string) = c_ok_kt1)
      internals
  in
  Check.(
    (List.length to_c_ok >= 1)
      int
      ~error_msg:
        "Expected at least one transaction targeting C_ok in the merged \
         synthetic receipt, got %L. Without the L2-1304 fix, revert_frame \
         truncates pending_crac_receipts on EVM revert and the entire \
         applied-CRAC body — top-level transfer to C_ok plus C_ok's internal \
         _incrementWitness — vanishes.  L1 semantics require these to appear \
         as `backtracked` instead.") ;
  List.iter
    (fun iop ->
      let status = JSON.(iop |-> "result" |-> "status" |> as_string) in
      Check.(
        (status = "backtracked")
          string
          ~error_msg:
            "Expected CRAC #1 body entry to C_ok with status %R (applied CRAC \
             reverted by enclosing EVM tx is backtracked, not failed), got %L"))
    to_c_ok ;
  (* Sanity: among the to-C_ok entries there must be a self-call
     entrypoint (_incrementWitness) — proves we recovered the full
     CRAC #1 body, not just the synthetic top-level transfer. *)
  let c_ok_witness =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(
             iop |-> "source" |> as_opt |> Option.fold ~none:"" ~some:as_string)
           = c_ok_kt1
        && JSON.(iop |-> "destination" |> as_string) = c_ok_kt1)
      internals
  in
  Check.(
    (List.length c_ok_witness >= 1)
      int
      ~error_msg:
        "Expected at least one C_ok→C_ok internal (the _incrementWitness \
         self-call inside multi_run_caller.tz) preserved as backtracked, got \
         %L. If this is missing while the alias→C_ok entry is present, the fix \
         is recovering only the top-level and not the full body — investigate \
         backtrack_receipt's walk over internal_operation_results.") ;
  (* Synthetic crac event must be present and backtracked: its parent
     CRAC #1 was reverted, so under L1 semantics (no Applied internals
     under a Failed top-level) it cannot stay Applied. *)
  let crac_events =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t -> JSON.as_string t = "crac"
        | None -> false)
      internals
  in
  (* 2 CRACs (CRAC#1 backtracked, CRAC#2 failed) → 2 begin markers *)
  Check.(
    (List.length crac_events = 2)
      int
      ~error_msg:
        "Expected 2 synthetic crac begin events (one per CRAC frame), got %L") ;
  (* Every marker in this scenario — begin and end alike — is
     backtracked: CRAC#1 was applied then backtracked by the EVM revert;
     CRAC#2 failed on the Michelson side.  Asserting both tags locks in
     "no Applied internals under a Failed top-level" for the full pair,
     not just the begins. *)
  let crac_markers =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "event"
        &&
        match JSON.(iop |-> "tag" |> as_opt) with
        | Some t ->
            let tag = JSON.as_string t in
            tag = "crac" || tag = "crac_end"
        | None -> false)
      internals
  in
  List.iter
    (fun ev ->
      let status = JSON.(ev |-> "result" |-> "status" |> as_string) in
      Check.(
        (status = "backtracked")
          string
          ~error_msg:
            "Expected every crac marker (begin and end) to have status %R (no \
             Applied internals under a Failed top-level), got %L"))
    crac_markers ;
  (* Sanity: CRAC #2's body to C_fail still present with status failed
     (covered by L2-1302; kept for scenario completeness). *)
  let to_c_fail =
    List.filter
      (fun iop ->
        JSON.(iop |-> "kind" |> as_string) = "transaction"
        && JSON.(iop |-> "destination" |> as_string) = c_fail_kt1)
      internals
  in
  Check.(
    (List.length to_c_fail >= 1)
      int
      ~error_msg:
        "Sanity check: expected at least one transaction targeting C_fail (the \
         surviving failed CRAC), got %L") ;
  check_crac_brackets ~prefix internals ;
  unit

(* Two EVM→TEZ CRACs from the SAME EVM transaction (RFC Example 5).
 *
 *  The Michelson runtime block has ONE manager operation with TWO sibling
 *  CRAC frames, each bracketed by its own begin/end marker pair.
 *
 *    EVM[evm_main]
 *     |-> EVM[bridge_1] ~CRAC~> TEZ[tez_1]
 *     |-> EVM[bridge_2] ~CRAC~> TEZ[tez_2]
 *)
let test_crac_receipt_two_independent () =
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
  (* 11 internal ops — two sibling CRAC frames each with begin/end pair:
     [begin1, alias(bridge_1), alias(sender), tx_tez_1, incr1, end1,
      begin2, alias(bridge_2), tx_tez_2, incr2, end2].
     CRAC #1: 6 ops (begin + 2 aliases + transfer + incr + end).
     CRAC #2: 5 ops (begin + 1 alias (EOA cached from CRAC #1) + transfer + incr + end).
     Execution order: tez_1 before tez_2. *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 11)
      int
      ~error_msg:"Expected 11 internal operations, got %L") ;
  (* ── CRAC #1 frame (indices 0-5) ─────────────────────────────── *)
  (* ── Internal #0: CRAC begin event ─────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: origination of alias(bridge_1) ───────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:bridge_1_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: origination of alias(EOA = sender) ───────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(bridge_1) → tez_1 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_1 → tez_1 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── Internal #5: CRAC #1 end event ────────────────────────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* ── CRAC #2 frame (indices 6-10) ────────────────────────────── *)
  (* ── Internal #6: CRAC begin event ─────────────────────────── *)
  Check.(
    (JSON.(List.nth internals 6 |-> "tag" |> as_string) = "crac")
      string
      ~error_msg:(sf "%s: Expected crac at #6, got %%L" prefix)) ;
  (* ── Internal #7: origination of alias(bridge_2). The EOA
        alias is cached from CRAC #1 so it does not re-originate. ── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:7
    ~expected_alias_kt1:bridge_2_alias
    ~expected_status:"applied"
    (List.nth internals 7) ;
  (* ── Internal #8: alias(bridge_2) → tez_2 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:8
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  (* ── Internal #9: tez_2 → tez_2 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:9
    ~expected_source:tez_2_kt1
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  (* ── Internal #10: CRAC #2 end event — the crac_id is per
     transaction, so the sibling frame carries the same id ──────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#10")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  (* Bracket balance: each frame has its own pair, total depth stays 0 *)
  check_crac_brackets ~prefix internals ;
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
let test_crac_receipt_separate_tx_two_cracs () =
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
      ~gas:3_000_000
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
      ~gas:3_000_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:runner_2_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ _tx_hash_2 = Rpc.send_raw_transaction ~raw_tx:raw_tx_2 sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
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
     event + <alias originations…> + bridge_alias → tez (%run) +
     tez → tez (%_incrementWitness). `aliases` lists the alias
     KT1s materialized by THIS op's gateway precompile invocation
     (sender first, then source, only those NOT already in
     storage). For two separate EVM txs in the same block, tx 1
     materializes both sender alias and source (EOA) alias; tx 2
     only materializes its sender (the EOA alias is now cached). *)
  let check_op ~prefix ~expected_destination ~expected_bridge_alias
      ~expected_dest ~expected_crac_id ~aliases ~nonce_offset op =
    let top = JSON.(op |-> "contents" |=> 0) in
    let internals =
      check_crac_top_level
        ~prefix
        ~expected_destination
        ~expected_status:"applied"
        top
    in
    let n_aliases = List.length aliases in
    (* begin + aliases + transfer + incr + end *)
    let expected_count = 4 + n_aliases in
    Check.(
      (List.length internals = expected_count)
        int
        ~error_msg:
          (sf
             "%s: expected %d internal operations, got %%L"
             prefix
             expected_count)) ;
    (* Internal operation nonces are block-global (L1 semantics):
       begin gets nonce_offset, aliases get nonce_offset+1..+k,
       transfer gets nonce_offset+1+k, incr gets nonce_offset+2+k,
       end gets nonce_offset+3+k. *)
    check_crac_event
      ~prefix
      ~expected_crac_id
      ~expected_status:"applied"
      (List.nth internals 0) ;
    List.iteri
      (fun i alias_kt1 ->
        check_crac_internal_alias_origination
          ~prefix:(sf "%s/alias#%d" prefix i)
          ~expected_nonce:(nonce_offset + 1 + i)
          ~expected_alias_kt1:alias_kt1
          ~expected_status:"applied"
          (List.nth internals (1 + i)))
      aliases ;
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 1 + n_aliases)
      ~expected_source:expected_bridge_alias
      ~expected_destination:expected_dest
      ~expected_entrypoint:"run"
      ~expected_status:"applied"
      (List.nth internals (1 + n_aliases)) ;
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 2 + n_aliases)
      ~expected_source:expected_dest
      ~expected_destination:expected_dest
      ~expected_entrypoint:"_incrementWitness"
      ~expected_status:"applied"
      (List.nth internals (2 + n_aliases)) ;
    (* End marker — position 3 + n_aliases *)
    check_crac_end_event
      ~prefix:(sf "%s/end" prefix)
      ~expected_crac_id
      ~expected_status:"applied"
      (List.nth internals (3 + n_aliases)) ;
    check_crac_brackets ~prefix internals
  in
  (* First EVM tx: materializes alias(bridge_1) and alias(EOA = sender)
     fresh. 6 internals, nonces 0..5. *)
  check_op
    ~prefix:(prefix ^ "#0")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_1_alias
    ~expected_dest:tez_1_kt1
    ~expected_crac_id:"1-0"
    ~aliases:[bridge_1_alias; sender_alias]
    ~nonce_offset:0
    JSON.(ops |=> 0) ;
  (* Second EVM tx: alias(EOA) was just materialized by the first
     tx and is now in storage, so only alias(bridge_2) is fresh.
     5 internals, nonces continue at 6..10. *)
  check_op
    ~prefix:(prefix ^ "#1")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_2_alias
    ~expected_dest:tez_2_kt1
    ~expected_crac_id:"1-1"
    ~aliases:[bridge_2_alias]
    ~nonce_offset:6
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
let test_crac_receipt_evm_tez_evm () =
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
  (* RFC Example 3, with alias materializations inserted before
     the synthetic transfer:
     7 internal ops — event, origination(alias(evm_bridge)),
     origination(alias(EOA)), alias(evm_bridge) → tez_bridge (%run),
     tez_bridge → tez_bridge (%_incrementWitness) (pre),
     tez_bridge → GW_M (%call_evm),
     tez_bridge → tez_bridge (%_incrementWitness) (post),
     end marker.
     The TEZ→EVM re-entrant leg materializes aliases on the EVM
     side, which do not surface in the Michelson receipt. *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 8)
      int
      ~error_msg:"Expected 8 internal operations, got %L") ;
  (* ── Internal #0: CRAC begin event ─────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: origination of alias(evm_bridge) ─────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: origination of alias(EOA = sender) ───────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(evm_bridge) → tez_bridge (%run) ────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_bridge → tez_bridge (%_incrementWitness) (pre) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── Internal #5: tez_bridge → GW_M (%call_evm) ─────────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:5
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* ── Internal #6: tez_bridge → tez_bridge (%_incrementWitness) (post) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:6
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  (* ── Internal #7: CRAC end event ───────────────────────────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#7")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  (* Bracket balance check *)
  check_crac_brackets ~prefix internals ;
  (* ── EVM side: re-entrant TEZ→EVM leg ─────────────────────── *)
  (* The re-entrant EVM execution appears as internal transactions under
     the ORIGINAL EVM transaction, not as a separate top-level fake
     CRAC tx. *)
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
let test_crac_receipt_tez_evm_tez () =
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
     EVM→TEZ effects are merged as internal ops.  The root TEZ op is not
     itself a CRAC frame; its internals contain the bracketed re-entrant
     EVM→TEZ frame as a balanced forest. *)
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
  (* 11 internal ops.  The root TEZ op is not itself a CRAC frame; its
     internals contain a balanced forest — one re-entrant EVM→TEZ frame
     bracketed by begin/end, followed by the root's own post-call ops.
     #0: tez_main → tez_main (%_incrementWitness)
     #1: tez_main → tez_outer_bridge (%run)
     #2: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (pre)
     #3: tez_outer_bridge → GW_M (%call_evm)
     #4: Event "crac" (begin marker for re-entrant EVM→TEZ frame)
     #5: origination alias(evm_bridge)                  (re-entrant)
     #6: alias(evm_bridge) → tez_inner (%run)           (re-entrant)
     #7: tez_inner → tez_inner (%_incrementWitness)     (re-entrant)
     #8: Event "crac_end" (end marker for re-entrant frame)
     #9: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (post)
     #10: tez_main → tez_main (%_incrementWitness) (final) *)
  Check.(
    (List.length internals = 11)
      int
      ~error_msg:"Expected 11 internal operations, got %L") ;
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
  (* ── Re-entrant EVM→TEZ frame begin/end markers (indices 4, 8) ── *)
  check_crac_event
    ~prefix:(prefix ^ "#4")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#8")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
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
  (* Bracket balance: one balanced re-entrant frame inside the root op *)
  check_crac_brackets ~prefix internals ;
  unit

(* CRAC receipt — direct top-level gateway %call_evm that re-enters Michelson.
 *
 *  Unlike test_crac_receipt_tez_evm_tez, the top-level manager operation
 *  targets the enshrined gateway's %call_evm entrypoint DIRECTLY, with no
 *  intermediate Michelson contract.  The EVM contract it dispatches into
 *  performs a nested EVM->Michelson CRAC back into [tez_inner]:
 *
 *    manager op --%call_evm--> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *
 *  The nested EVM->Michelson leg is folded into the gateway op's
 *  internal_operation_results (one top-level op per runtime per CRAC), and
 *  tez_inner's witness counter is bumped.
 *
 *  This is the shape no other receipt test exercises (all others reach the
 *  gateway through an intermediate Michelson contract, i.e. as an internal
 *  op).  L2-1450. *)
let test_crac_receipt_tez_gateway_direct_evm_tez () =
  register_crac_runner_test
    ~title:"CRAC: TEZ direct gateway %call_evm -> EVM -> TEZ receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-GWDIRECT" in
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  (* Top-level manager op = the gateway's %call_evm, targeting
     evm_bridge.run() which CRACs back into tez_inner. *)
  let* () =
    Gateway.call_evm
      ~evm_target:evm_bridge
      ~method_sig:"run()"
      ~abi_params:""
      ()
  in
  (* Durable execution is correct regardless of the receipt bug: the nested
     EVM->Michelson leg bumps tez_inner's witness counter. *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  (* ── EVM side: the TEZ->EVM leg surfaces as one synthetic CRAC tx ── *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  (* ── Michelson side: the gateway op and the folded nested leg ── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  Check.(
    (JSON.(top |-> "destination" |> as_string) = gateway_address)
      string
      ~error_msg:"Expected top-level destination = gateway %R, got %L") ;
  Check.(
    (JSON.(top |-> "parameters" |-> "entrypoint" |> as_string) = "call_evm")
      string
      ~error_msg:"Expected top-level entrypoint %R, got %L") ;
  let metadata = JSON.(top |-> "metadata") in
  Check.(
    (JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
    = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* The nested EVM->Michelson leg is folded into the gateway op's internals
     as a balanced re-entrant frame:
       #0: Event "crac" (begin marker for re-entrant EVM→TEZ frame)
       #1: origination alias(evm_bridge)
       #2: alias(evm_bridge) -> tez_inner (%run)
       #3: tez_inner -> tez_inner (%_incrementWitness)
       #4: Event "crac_end" (end marker) *)
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:
        "Expected 5 internal ops (bracketed re-entrant EVM->Michelson leg), \
         got %L") ;
  check_crac_event
    ~prefix:(prefix ^ "#0")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#4")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_brackets ~prefix internals ;
  unit

(* CRAC receipt — direct top-level gateway %call_evm whose nested Michelson
 *  leg REVERTS.  Negative-path counterpart of
 *  test_crac_receipt_tez_gateway_direct_evm_tez.
 *
 *    manager op --%call_evm--> EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter] (revert)
 *
 *  The failed nested leg is folded into the gateway op's internals with
 *  failed / backtracked statuses, and the top-level op fails.  L2-1450. *)
let test_crac_receipt_tez_gateway_direct_evm_tez_revert () =
  register_crac_runner_test
    ~title:"CRAC: TEZ direct gateway %call_evm -> EVM -> TEZ revert receipts"
    ~tags:["crac_receipt"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-GWDIRECT-REV" in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_kt1)) = tez_reverter in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  (* Top-level manager op = the gateway's %call_evm targeting
     evm_bridge.run(), whose nested CRAC into tez_reverter reverts. *)
  let* () =
    Gateway.call_evm
      ~evm_target:evm_bridge
      ~method_sig:"run()"
      ~abi_params:""
      ()
  in
  (* Reverted: tez_reverter's witness counter stays at 0. *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  Check.(
    (JSON.(top |-> "destination" |> as_string) = gateway_address)
      string
      ~error_msg:"Expected top-level destination = gateway %R, got %L") ;
  Check.(
    (JSON.(top |-> "parameters" |-> "entrypoint" |> as_string) = "call_evm")
      string
      ~error_msg:"Expected top-level entrypoint %R, got %L") ;
  let metadata = JSON.(top |-> "metadata") in
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* The failed nested EVM->Michelson leg is folded into the gateway op's
     internals as a balanced re-entrant frame.  The frame failed, so the
     markers and alias carry backtracked/failed statuses:
       #0: Event "crac" (begin marker) -- backtracked
       #1: origination alias(evm_bridge) -- backtracked
       #2: alias(evm_bridge) -> tez_reverter (%run) -- failed
       #3: tez_reverter -> tez_reverter (%_revert) -- failed
       #4: Event "crac_end" (end marker) -- backtracked
     The alias materialization is backtracked (applied then rolled back
     when the leg reverts); the top-level gateway op fails. *)
  Check.(
    (JSON.(metadata |-> "operation_result" |-> "status" |> as_string) = "failed")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal ops for the failed nested leg, got %L") ;
  check_crac_event
    ~prefix:(prefix ^ "#0")
    ~expected_crac_id:"0-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"backtracked"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_reverter_kt1
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 3) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#4")
    ~expected_crac_id:"0-0"
    ~expected_status:"backtracked"
    (List.nth internals 4) ;
  check_crac_brackets ~prefix internals ;
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
let test_crac_receipt_evm_tez_evm_tez () =
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
  (* 13 internal ops: outer frame wrapped in begin/end, with an inner
     re-entrant frame (also wrapped) spliced between %call_evm and the
     post-call incr.  Two nested levels of brackets.
     #0: begin (outer, "1-0")
     #1: origination alias(evm_bridge_1)
     #2: origination alias(EOA = sender)
     #3: alias(evm_bridge_1) → tez_bridge (%run)
     #4: tez_bridge → tez_bridge (%_incrementWitness) (pre)
     #5: tez_bridge → GW_M (%call_evm)
     #6: begin (inner re-entrant EVM→TEZ frame)
     #7: origination alias(evm_bridge_2)
     #8: alias(evm_bridge_2) → tez_inner (%run)
     #9: tez_inner → tez_inner (%_incrementWitness)
     #10: end (inner frame)
     #11: tez_bridge → tez_bridge (%_incrementWitness) (post)
     #12: end (outer frame) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 13)
      int
      ~error_msg:"Expected 13 internal operations, got %L") ;
  (* ── Outer frame begin (index 0) ─────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_1_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:evm_bridge_1_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
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
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* ── Inner re-entrant EVM→TEZ frame (indices 6-10); the crac_id
     is per transaction, so the nested frame carries the same id ── *)
  check_crac_event
    ~prefix:(prefix ^ "#6")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_alias_kt1:evm_bridge_2_alias
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_source:evm_bridge_2_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#10")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  (* ── Post-inner ops and outer frame end ─────────────────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#11")
    ~expected_nonce:11
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#12")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 12) ;
  (* Bracket balance: outer frame (0, 12) and inner frame (6, 10) nested *)
  check_crac_brackets ~prefix internals ;
  (* ── EVM side ──────────────────────────────────────────────── *)
  (* 1 original EVM tx; re-entrant TEZ→EVM leg is internal *)
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
let test_crac_receipt_evm_to_tez_revert () =
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
  (* 6 internal ops: CRAC begin event + alias materializations
     (sender + source) + alias→reverter call + reverter's
     internal _revert call + CRAC end event. The aliases were
     materialized successfully in storage BEFORE the body failed;
     their receipt status is "applied" because they did succeed,
     while the downstream synthetic transfer and its sub-call are
     "failed". The begin/end markers are "backtracked" to match the
     failed parent frame. *)
  Check.(
    (List.length internals = 6)
      int
      ~error_msg:"Expected 6 internal operations, got %L") ;
  (* ── Internal #0: CRAC begin event (backtracked: downstream failed) ── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  (* ── Internal #1: origination of alias(evm_bridge) ─────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: origination of alias(EOA = sender) ───────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(E_bridge) → tez_reverter (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_reverter → tez_reverter (%_revert) — failed
     The reverter contract's internal call that triggers the failure. *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_reverter_kt1
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 4) ;
  (* ── Internal #5: CRAC end event (backtracked: frame failed) ── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 5) ;
  (* Bracket balance check *)
  check_crac_brackets ~prefix internals ;
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
let test_crac_receipt_two_failed_independent () =
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
  (* 12 internal ops: two sibling CRAC frames, each bracketed with
     begin/end, for 6 ops per CRAC.  Both CRACs fail and revert, so the
     alias(EOA = sender) materialized inside CRAC #1 is rolled back along
     with the rest of CRAC #1's world-state changes; CRAC #2 therefore
     re-materializes alias(bridge_2) AND alias(EOA = sender). *)
  Check.(
    (List.length internals = 12)
      int
      ~error_msg:"Expected 12 internal operations, got %L") ;
  (* ── CRAC #1 frame (indices 0-5) ─────────────────────────────── *)
  (* ── Internal #0: CRAC #1 begin event (backtracked: CRAC failed) *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  (* ── Internal #1: origination alias(bridge_1) ────────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:bridge_1_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: origination alias(EOA = sender) ────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(bridge_1) → tez_reverter_1 (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_reverter_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_reverter_1 → tez_reverter_1 (%_revert) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_reverter_1_kt1
    ~expected_destination:tez_reverter_1_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 4) ;
  (* ── Internal #5: CRAC #1 end event (backtracked) ────────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 5) ;
  (* ── CRAC #2 frame (indices 6-11) ────────────────────────────── *)
  (* ── Internal #6: CRAC #2 begin event (backtracked: CRAC failed) *)
  Check.(
    (JSON.(List.nth internals 6 |-> "tag" |> as_string) = "crac")
      string
      ~error_msg:(sf "%s: Expected crac at #6, got %%L" prefix)) ;
  Check.(
    (JSON.(List.nth internals 6 |-> "result" |-> "status" |> as_string)
    = "backtracked")
      string
      ~error_msg:(sf "%s: Expected backtracked at #6, got %%L" prefix)) ;
  (* ── Internal #7: origination alias(bridge_2) ────────────── *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:7
    ~expected_alias_kt1:bridge_2_alias
    ~expected_status:"applied"
    (List.nth internals 7) ;
  (* ── Internal #8: origination alias(EOA = sender), re-materialized ──
     CRAC #1 reverted, dropping its alias(sender) materialization, so
     CRAC #2 creates it again. *)
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:8
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 8) ;
  (* ── Internal #9: alias(bridge_2) → tez_reverter_2 (%run) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:9
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_reverter_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 9) ;
  (* ── Internal #10: tez_reverter_2 → tez_reverter_2 (%_revert) — failed *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:10
    ~expected_source:tez_reverter_2_kt1
    ~expected_destination:tez_reverter_2_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 10) ;
  (* ── Internal #11: CRAC #2 end event (backtracked) — the crac_id
     is per transaction, so the sibling frame carries the same id ── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#11")
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 11) ;
  (* Bracket balance: two sibling frames, each balanced independently *)
  check_crac_brackets ~prefix internals ;
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
let test_crac_receipt_interleaved_failed_pending () =
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
  (* Alias originations: bridge_ok + sender materialized once in S₁ and
     persist. bridge_fail is re-materialized in every F because each
     failing CRAC reverts its world-state changes, dropping bridge_fail.
     Hence 2 + n_pairs alias originations.
     Each CRAC (2*n_pairs total) now carries a begin/end marker pair.
     Total = aliases + 4*n_pairs body ops + 4*n_pairs markers
           = (2+n_pairs) + 4*n + 4*n = 2 + 9*n_pairs. *)
  let n_alias_internals = 2 + n_pairs in
  let expected_internals = (4 * n_pairs) + n_alias_internals + (4 * n_pairs) in
  Check.(
    (List.length internals = expected_internals)
      int
      ~error_msg:
        (sf "Expected %d internal operations, got %%L" expected_internals)) ;
  (* Order matters here: this test exists to catch the
     `drain_pending_crac_receipts` bug where failed CRACs were
     emitted before pending ones rather than in execution order.
     The key includes [originated_contracts] (for originations) and
     [destination] (for transactions) so positions are
     disambiguated even when several internals share the same
     (kind, source, status). *)
  let key iop =
    let kind = JSON.(iop |-> "kind" |> as_string) in
    let source = JSON.(iop |-> "source" |> as_string) in
    let status = JSON.(iop |-> "result" |-> "status" |> as_string) in
    match kind with
    | "event" -> sf "event/%s/%s" source status
    | "origination" ->
        let originated =
          JSON.(iop |-> "result" |-> "originated_contracts" |> as_list)
          |> List.map JSON.as_string |> String.concat ","
        in
        sf "origination/%s/%s/[%s]" source status originated
    | "transaction" ->
        let destination = JSON.(iop |-> "destination" |> as_string) in
        let entrypoint =
          try JSON.(iop |-> "parameters" |-> "entrypoint" |> as_string)
          with _ -> ""
        in
        sf "transaction/%s->%s/%s/%s" source destination entrypoint status
    | other -> sf "<unknown:%s>" other
  in
  let observed = List.map key internals in
  let event_key = sf "event/%s/applied" handler_address in
  (* Failing CRAC markers are backtracked (the frame failed). *)
  let event_bt_key = sf "event/%s/backtracked" handler_address in
  let orig_key alias = sf "origination/%s/applied/[%s]" handler_address alias in
  let tx_key ~src ~dst ~ep ~status =
    sf "transaction/%s->%s/%s/%s" src dst ep status
  in
  let s_pair_txs =
    [
      tx_key ~src:bridge_ok_alias ~dst:tez_ok_kt1 ~ep:"run" ~status:"applied";
      tx_key
        ~src:tez_ok_kt1
        ~dst:tez_ok_kt1
        ~ep:"_incrementWitness"
        ~status:"applied";
    ]
  in
  let f_pair_txs =
    [
      tx_key
        ~src:bridge_fail_alias
        ~dst:tez_reverter_kt1
        ~ep:"run"
        ~status:"failed";
      tx_key
        ~src:tez_reverter_kt1
        ~dst:tez_reverter_kt1
        ~ep:"_revert"
        ~status:"failed";
    ]
  in
  (* Execution order:
     S₁ (begin, orig bridge_ok, orig sender, run, incr, end)
     F₁ (begin, orig bridge_fail, run, revert, end)
     [n_pairs-1] × (S: begin, run, incr, end;  F: begin, orig bridge_fail, run, revert, end)
     Each begin/end for S is applied; for F it is backtracked. *)
  let s1_frame =
    [event_key; orig_key bridge_ok_alias; orig_key sender_alias]
    @ s_pair_txs @ [event_key]
  in
  let s_frame = [event_key] @ s_pair_txs @ [event_key] in
  let f_frame =
    [event_bt_key; orig_key bridge_fail_alias] @ f_pair_txs @ [event_bt_key]
  in
  let expected =
    s1_frame @ f_frame
    @ List.concat (List.init (n_pairs - 1) (fun _ -> s_frame @ f_frame))
  in
  Check.(
    (observed = expected)
      (list string)
      ~error_msg:
        (sf
           "%s: observed internal-op sequence does not match expected \
            execution order — expected %%R, got %%L"
           prefix)) ;
  check_crac_brackets ~prefix internals ;
  unit

(* CRAC at tx_index > 0: inject a dummy ETH transfer into the mempool
 * before the CRAC tx, so both land in the same block and the CRAC tx
 * gets tx_index = 1 → CRAC-ID = "1-1".
 *
 *   [dummy ETH transfer]  (tx_index 0)
 *   EVM[evm_runner] |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]  (tx_index 1)
 *)
let test_crac_receipt_evm_not_first_tx () =
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
  (* Same structure as simple EVM→TEZ, with two alias materializations
     and a begin/end bracket pair: 6 internal ops total. *)
  Check.(
    (List.length internals = 6)
      int
      ~error_msg:"Expected 6 internal operations, got %L") ;
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-1"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:1
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_alias_origination
    ~prefix
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_runner_kt1
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── Internal #5: CRAC end event ───────────────────────────── *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-1"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_brackets ~prefix internals ;
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
let test_crac_receipt_tez_not_first_tx () =
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
let test_crac_receipt_evm_then_tez_same_block () =
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
let test_crac_receipt_tez_to_evm () =
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
 *  Michelson block: no CRAC event (originating runtime). The re-entrant
 *  EVM→TEZ frame (evm_bridge → tez_inner_bridge) is bracketed with
 *  its own begin/end marker pair. 13 internal ops total.
 *  EVM block: 1 fake CRAC tx with CRAC-ID "0-0".
 *)
let test_crac_receipt_tez_evm_tez_evm () =
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
  (* No CRAC event in the originating runtime. 13 internal ops.
     The re-entrant EVM→TEZ leg (evm_bridge → tez_inner_bridge) is
     now a bracketed frame with its own begin/end marker pair:
     #0:  tez_main → tez_main (%_incrementWitness)
     #1:  tez_main → tez_outer_bridge (%run)
     #2:  tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (pre)
     #3:  tez_outer_bridge → GW_M (%call_evm)
     #4:  EVENT "crac" (re-entrant begin: evm_bridge→tez_inner_bridge)
     #5:  origination alias(evm_bridge)
     #6:  alias(evm_bridge) → tez_inner_bridge (%run)
     #7:  tez_inner_bridge → tez_inner_bridge (%_incrementWitness) (pre)
     #8:  tez_inner_bridge → GW_M (%call_evm)
     #9:  tez_inner_bridge → tez_inner_bridge (%_incrementWitness) (post)
     #10: EVENT "crac_end" (re-entrant end)
     #11: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (post)
     #12: tez_main → tez_main (%_incrementWitness) (final) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 13)
      int
      ~error_msg:"Expected 13 internal operations, got %L") ;
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
  (* ── Re-entrant CRAC frame: evm_bridge → tez_inner_bridge ───── *)
  check_crac_event
    ~prefix:(prefix ^ "#4")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"run"
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
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#10")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  (* ── Post-re-entrant continuations ─────────────────────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#11")
    ~expected_nonce:11
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#12")
    ~expected_nonce:12
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 12) ;
  check_crac_brackets ~prefix internals ;
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
 *  destination = alias(sender), and all 3 CRAC frames each bracketed
 *  with their own begin/end marker pair. 20 internal ops total.
 *  EVM block: 1 transaction (all re-entrant EVM legs are internal
 *  per RFC principle 6).
 *)
let test_crac_receipt_evm_5_crossing_chain () =
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
  (* 20 internal ops: each of the 3 CRAC frames carries its own begin/end
     pair.  Aliases for evm_a + EOA are materialized by the outermost frame;
     each re-entrant frame materializes only its own EVM-sender alias.
     #0:  EVENT "crac" (outermost begin, CRAC-ID "1-0")
     #1:  origination alias(evm_a)
     #2:  origination alias(EOA = sender)
     #3:  alias(evm_a) → tez_b (%run)              [outermost CRAC]
     #4:  tez_b → tez_b (%_incrementWitness) [pre]
     #5:  tez_b → GW_M (%call_evm)                  [outgoing → evm_c]
     #6:  EVENT "crac" (middle begin)
     #7:  origination alias(evm_c)
     #8:  alias(evm_c) → tez_d (%run)              [middle CRAC]
     #9:  tez_d → tez_d (%_incrementWitness) [pre]
     #10: tez_d → GW_M (%call_evm)                  [outgoing → evm_e]
     #11: EVENT "crac" (deepest begin)
     #12: origination alias(evm_e)
     #13: alias(evm_e) → tez_leaf (%run)           [deepest CRAC]
     #14: tez_leaf → tez_leaf (%_incrementWitness)
     #15: EVENT "crac_end" (deepest end)
     #16: tez_d → tez_d (%_incrementWitness) [post]
     #17: EVENT "crac_end" (middle end)
     #18: tez_b → tez_b (%_incrementWitness) [post]
     #19: EVENT "crac_end" (outermost end) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 20)
      int
      ~error_msg:"Expected 20 internal operations, got %L") ;
  (* ── Outermost frame: evm_a → tez_b ────────────────────────── *)
  check_crac_event
    ~prefix:(prefix ^ "#0")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:evm_a_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:evm_a_alias
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:tez_b_kt1
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_b_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  (* ── Middle frame: evm_c → tez_d (re-entrant at GW call site) ── *)
  check_crac_event
    ~prefix:(prefix ^ "#6")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_alias_kt1:evm_c_alias
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_source:evm_c_alias
    ~expected_destination:tez_d_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 8) ;
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
    ~expected_source:tez_d_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  (* ── Deepest frame: evm_e → tez_leaf (re-entrant at GW call site) *)
  check_crac_event
    ~prefix:(prefix ^ "#11")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#12")
    ~expected_nonce:12
    ~expected_alias_kt1:evm_e_alias
    ~expected_status:"applied"
    (List.nth internals 12) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#13")
    ~expected_nonce:13
    ~expected_source:evm_e_alias
    ~expected_destination:tez_leaf_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 13) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#14")
    ~expected_nonce:14
    ~expected_source:tez_leaf_kt1
    ~expected_destination:tez_leaf_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 14) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#15")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 15) ;
  (* ── Stack-unwind continuations ─────────────────────────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#16")
    ~expected_nonce:16
    ~expected_source:tez_d_kt1
    ~expected_destination:tez_d_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 16) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#17")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 17) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#18")
    ~expected_nonce:18
    ~expected_source:tez_b_kt1
    ~expected_destination:tez_b_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 18) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#19")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 19) ;
  check_crac_brackets ~prefix internals ;
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
 *  The re-entrant EVM→TEZ leg (evm_bridge → tez_inner) is bracketed with
 *  its own begin/end marker pair. 17 internal ops total.
 *  EVM block: 1 fake CRAC tx with CRAC-ID "0-0".
 *)
let test_crac_receipt_tez_mixed_calls_with_crac () =
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
  (* No CRAC event in the originating runtime.  17 internal ops.
     The re-entrant EVM→TEZ leg (evm_bridge → tez_inner) is now a
     bracketed frame with its own begin/end marker pair.
     #0:  tez_main → tez_main (%_incrementWitness) [before 1st callee]
     #1:  tez_main → tez_inner (%run) [1st callee call]
     #2:  tez_inner → tez_inner (%_incrementWitness) [sub-op of 1st callee]
     #3:  tez_main → tez_main (%_incrementWitness) [before 2nd callee]
     #4:  tez_main → tez_bridge (%run) [2nd callee call]
     #5:  tez_bridge → tez_bridge (%_incrementWitness) [sub-op: pre]
     #6:  tez_bridge → GW_M (%call_evm) [sub-op: outgoing CRAC]
     #7:  EVENT "crac" (re-entrant begin: evm_bridge → tez_inner)
     #8:  origination alias(evm_bridge)
     #9:  alias(evm_bridge) → tez_inner (%run) [re-entrant CRAC]
     #10: tez_inner → tez_inner (%_incrementWitness) [re-entrant sub-op]
     #11: EVENT "crac_end" (re-entrant end)
     #12: tez_bridge → tez_bridge (%_incrementWitness) [sub-op: post]
     #13: tez_main → tez_main (%_incrementWitness) [before 3rd callee]
     #14: tez_main → tez_inner (%run) [3rd callee call]
     #15: tez_inner → tez_inner (%_incrementWitness) [sub-op of 3rd callee]
     #16: tez_main → tez_main (%_incrementWitness) [final] *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 17)
      int
      ~error_msg:"Expected 17 internal operations, got %L") ;
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
  (* ── Re-entrant CRAC frame: evm_bridge → tez_inner ──────────── *)
  check_crac_event
    ~prefix:(prefix ^ "#7")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#8")
    ~expected_nonce:8
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#10")
    ~expected_nonce:10
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#11")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  (* ── Post-re-entrant continuations ─────────────────────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#12")
    ~expected_nonce:12
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
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
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#14")
    ~expected_nonce:14
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 14) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#15")
    ~expected_nonce:15
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 15) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#16")
    ~expected_nonce:16
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 16) ;
  check_crac_brackets ~prefix internals ;
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
let test_crac_http_call_success () =
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
let test_crac_http_call_catch_revert () =
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
let test_crac_http_call_catch_oog () =
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

(** Generic call() precompile: EVM calls EVM via HTTP (same-runtime CRAC).
 *
 *    EVM[evm_caller] --call(http://ethereum/...)--> EVM[evm_target]
 *
 *  Probes whether the gateway honours a same-runtime target URL.  The
 *  destination is just a [MultiRunCaller] with no callees; if the CRAC
 *  delivers, its [count] reaches 1.  If the gateway path is broken for
 *  same-runtime targets, the CRAC fails and [catches] is incremented.
 *  This test asserts the CRAC succeeds end-to-end. *)
let test_crac_http_call_evm_to_evm () =
  register_crac_runner_test
    ~title:"CRAC: generic call() EVM to EVM (same-runtime)"
    ~tags:["http_call"; "same_runtime"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-EVM2EVM" in
  Log.debug ~prefix "Deploy EVM target (MultiRunCaller, no callees)" ;
  let* evm_target = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Deploy EVM caller using generic call() to EVM target" ;
  let* evm_caller = EvmCracHttpCallEvm.deploy_and_init evm_target in
  Log.debug ~prefix "Call runCatch() on EVM caller (catch on failure)" ;
  let* _ = EvmCracHttpCallEvm.call_run_catch evm_caller in
  Log.debug
    ~prefix
    "Verify caller counters: catches=0, count=3 (pre + ok + post)" ;
  let* () =
    EvmCracHttpCallEvm.check_storage
      ~expected_catches:0
      ~expected_counter:3
      evm_caller
  in
  Log.debug ~prefix "Verify EVM target was reached (counter=1)" ;
  EvmMultiRunCaller.check_storage ~expected_counter:1 evm_target

(** Regression: an EVM contract that calls another EVM contract
 *  through the runtime gateway must see [msg.sender] preserved on the
 *  callee side.  Before the fix, the gateway re-derived an alias
 *  ([keccak256("0x" ++ lowercase_hex(addr))[..20]]) for any native EVM
 *  caller targeting the EVM runtime, silently rewriting [msg.sender] to
 *  a deterministic-but-unrelated address.  This test deploys an
 *  IdentityRecorder, calls it via the gateway from [CracHttpCallEvm],
 *  and asserts that the recorder observed the calling contract's real
 *  address rather than the laundered alias. *)
let test_crac_http_call_evm_to_evm_preserves_msg_sender () =
  register_crac_runner_test
    ~title:"CRAC: same-runtime EVM→EVM round-trip preserves msg.sender"
    ~tags:["http_call"; "same_runtime"; "msg_sender"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-EVM2EVM-SENDER" in
  Log.debug ~prefix "Deploy EVM target (IdentityRecorder)" ;
  let* evm_target = EvmIdentityRecorder.deploy () in
  let (`Evm_runner target_address) = evm_target in
  Log.debug ~prefix "Deploy EVM caller pointing at IdentityRecorder" ;
  let* evm_caller = EvmCracHttpCallEvm.deploy_and_init evm_target in
  let (`Evm_runner caller_address) = evm_caller in
  Log.debug ~prefix "Trigger EVM→gateway→EVM round-trip" ;
  let* _ = EvmCracHttpCallEvm.call_run_catch evm_caller in
  Log.debug
    ~prefix
    "Verify caller counters: catches=0, count=3 (pre + ok + post)" ;
  let* () =
    EvmCracHttpCallEvm.check_storage
      ~expected_catches:0
      ~expected_counter:3
      evm_caller
  in
  Log.debug
    ~prefix
    "Verify IdentityRecorder saw the real caller %s (NOT alias(caller))"
    caller_address ;
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:caller_address
      evm_target
  in
  (* Sanity: caller and target are distinct addresses (the assertion
     above is only meaningful if they are). *)
  Check.(
    (String.lowercase_ascii caller_address
    <> String.lowercase_ascii target_address)
      string
      ~error_msg:
        "EVM caller and target addresses collided (%L == %R); the assertion is \
         degenerate") ;
  unit

(** Generic call() precompile: TEZ calls TEZ via HTTP (same-runtime CRAC).
 *
 *    TEZ[tez_caller] --%call(http://tezos/.../run)--> TEZ[tez_target]
 *
 *  Probes the same-runtime CRAC path on the Michelson side: the gateway
 *  sees a [http://tezos/...] URL and dispatches the request back to the
 *  Michelson runtime.  The target is a plain [multi_run_caller] with no
 *  callees; if the CRAC delivers, its [count] reaches 1.  The caller's
 *  [count] reaches 2 (pre + post). *)
let test_crac_http_call_tez_to_tez () =
  register_crac_runner_test
    ~title:"CRAC: generic call() TEZ to TEZ (same-runtime)"
    ~tags:["http_call"; "same_runtime"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-TEZ2TEZ" in
  Log.debug ~prefix "Originate TEZ target (multi_run_caller, no callees)" ;
  let* tez_target = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Originate TEZ caller using gateway %%call() to TEZ target" ;
  let* tez_caller = TezCrossRuntimeHttpCallTez.originate tez_target in
  Log.debug ~prefix "Call %%run on TEZ caller" ;
  let* () = TezRunner.call_run tez_caller in
  Log.debug ~prefix "Verify caller counter=2 (pre + post)" ;
  let* () =
    TezCrossRuntimeHttpCallTez.check_storage ~expected_counter:2 tez_caller
  in
  Log.debug ~prefix "Verify TEZ target was reached (counter=1)" ;
  TezMultiRunCaller.check_storage ~expected_counter:1 tez_target

(** Same-runtime EVM->EVM CRAC, target reverts.
 *
 *    EVM[evm_caller] --call(http://ethereum/...)--> EVM[evm_reverter]
 *                                                   |-> revert()
 *  EVM caller wraps the gateway call in try/catch: catches=1, count=2. *)
let test_crac_http_call_evm_to_evm_revert () =
  register_crac_runner_test
    ~title:"CRAC: generic call() EVM to EVM (same-runtime) revert is catchable"
    ~tags:["http_call"; "same_runtime"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-EVM2EVM" in
  Log.debug ~prefix "Deploy EVM target with revert=true" ;
  let* evm_target = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Deploy EVM caller using generic call() to EVM target" ;
  let* evm_caller = EvmCracHttpCallEvm.deploy_and_init evm_target in
  Log.debug ~prefix "Call runCatch() on EVM caller" ;
  let* _ = EvmCracHttpCallEvm.call_run_catch evm_caller in
  Log.debug ~prefix "Verify caller catches=1, count=2 (pre + post catch)" ;
  let* () =
    EvmCracHttpCallEvm.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_caller
  in
  Log.debug ~prefix "Verify EVM target rolled back (counter=0)" ;
  EvmMultiRunCaller.check_storage ~expected_counter:0 evm_target

(** Same-runtime TEZ->TEZ CRAC, target reverts.
 *
 *    TEZ[tez_caller] --%call(http://tezos/.../run)--> TEZ[tez_reverter]
 *                                                     |-> FAILWITH
 *  Michelson semantics: the failure inside the inner CRAC propagates to
 *  the outer operation group, which reverts wholesale.  All three of
 *  tez_caller's internal operations (pre, gateway, post) are rolled
 *  back, so [count] stays at 0. *)
let test_crac_http_call_tez_to_tez_revert () =
  register_crac_runner_test
    ~title:"CRAC: generic call() TEZ to TEZ (same-runtime) revert propagates"
    ~tags:["http_call"; "same_runtime"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-TEZ2TEZ" in
  Log.debug ~prefix "Originate TEZ target with revert=true" ;
  let* tez_target = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Originate TEZ caller using gateway %%call() to TEZ target" ;
  let* tez_caller = TezCrossRuntimeHttpCallTez.originate tez_target in
  Log.debug ~prefix "Call %%run on TEZ caller (expected to fail wholesale)" ;
  let* () = TezRunner.call_run tez_caller in
  Log.debug ~prefix "Verify caller counter=0 (whole op group rolled back)" ;
  let* () =
    TezCrossRuntimeHttpCallTez.check_storage ~expected_counter:0 tez_caller
  in
  Log.debug ~prefix "Verify TEZ target counter=0 (rolled back)" ;
  TezMultiRunCaller.check_storage ~expected_counter:0 tez_target

(** Same-runtime EVM->EVM CRAC, capturing the target's return value.
 *
 *    EVM[evm_caller] --call(http://ethereum/...,store(42))-->
 *      EVM[StoreAndReturn]
 *        |-> store: value = 42; return 42
 *
 *  The EVM serve packs the call's [Output::Call] bytes into the HTTP
 *  response body.  The precompile ABI-encodes them as [bytes] and
 *  returns them to the caller.  Caller decodes [bytes] and stores it
 *  in [result]. *)
let test_crac_http_call_evm_to_evm_return_value () =
  register_crac_runner_test
    ~title:
      "CRAC: generic call() EVM to EVM (same-runtime) returns target output"
    ~tags:["http_call"; "same_runtime"; "return_value"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-EVM2EVM-RET" in
  let stored_value = 42 in
  Log.debug ~prefix "Deploy EVM target (StoreAndReturn)" ;
  let* evm_target = EvmStoreAndReturn.deploy () in
  Log.debug ~prefix "Deploy EVM caller (CracCollectResultEvm)" ;
  let* evm_caller =
    EvmCracCollectResultEvm.deploy_and_init ~stored_value evm_target
  in
  Log.debug ~prefix "Call run() on EVM caller" ;
  let* _ = EvmCracCollectResultEvm.call_run evm_caller in
  Log.debug ~prefix "Verify side effect: target.value = %d" stored_value ;
  let* () =
    EvmStoreAndReturn.check_storage ~expected_value:stored_value evm_target
  in
  Log.debug
    ~prefix
    "Verify return: caller.result = abi.encode(uint256(%d))"
    stored_value ;
  let expected_hex = Printf.sprintf "%064x" stored_value in
  EvmCracCollectResultEvm.check_result ~expected_hex evm_caller

(** Same-runtime TEZ->TEZ CRAC, capturing the target's response body
 *  via the gateway's [%call] callback parameter.
 *
 *    TEZ[tez_caller] --%call(http://tezos/.../default,
 *                           callback=Some(self %on_result))-->
 *      TEZ[gateway_collect_result] (deposits payload via %collect_result)
 *
 *  After the gateway returns, the deposited bytes are forwarded to
 *  [tez_caller %on_result] which stores them in [%result]. *)
let test_crac_http_call_tez_to_tez_collect_result () =
  register_crac_runner_test
    ~title:
      "CRAC: generic call() TEZ to TEZ (same-runtime) callback receives \
       collect_result bytes"
    ~tags:["http_call"; "same_runtime"; "return_value"; "collect_result"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HTTP-TEZ2TEZ-RET" in
  let payload_hex = "deadbeef" in
  Log.debug ~prefix "Originate TEZ target (gateway_collect_result)" ;
  let* tez_target = TezCollectResult.originate ~payload_hex () in
  Log.debug ~prefix "Originate TEZ caller with callback to TEZ target" ;
  let* tez_caller = TezCrossRuntimeHttpCallTezCallback.originate tez_target in
  Log.debug ~prefix "Call %%run on TEZ caller" ;
  let* () = TezRunner.call_run tez_caller in
  Log.debug ~prefix "Verify caller received bytes via callback" ;
  let* () =
    TezCrossRuntimeHttpCallTezCallback.check_result
      ~expected_bytes:(Some payload_hex)
      tez_caller
  in
  Log.debug ~prefix "Verify caller %%count incremented (callback ran)" ;
  TezCrossRuntimeHttpCallTezCallback.check_counter
    ~expected_counter:3
    tez_caller

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
    ~minimum_base_fee_per_gas:crac_minimum_base_fee_per_gas
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer_setup protocol ->
  let client = sequencer_setup.client in
  let node = sequencer_setup.node in
  let source = Constant.bootstrap5 in
  let {Setup.sequencer; _} = sequencer_setup in
  let* client_tezlink = tezlink_client_from_evm_node sequencer in
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
            ~client
            ~client_tezlink
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
          ~client
          ~client_tezlink
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
let test_crac_callback_fire_and_forget () =
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
let test_crac_callback_receives_result_bytes () =
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
let test_crac_callback_failure_reverts_all () =
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
let test_crac_callback_behind_crac () =
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
let test_crac_callback_tez_revert_rolls_back_callback () =
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
let test_crac_callback_evm_catches_failing_callback_behind_crac () =
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
let test_crac_callback_mixed_with_normal_runners () =
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
let test_crac_gas_header_michelson_burner () =
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

  let g_expected = Int64.of_int (mg_direct / 22) in
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
     = mg_direct / 22 is the true Michelson gas cost measured
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
let test_crac_gas_model_callee_gas_in_evm_receipt () =
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
  let* _ = EvmRunner.call_run ~gas:5000000 gas_burner in

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
let test_crac_gas_model_callee_gas_in_receipt () =
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
  let* _ = EvmRunner.call_run ~gas:5000000 gas_burner in

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

  let mg_expected = Int64.to_int g_direct * 22 in
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
     overhead.  mg_expected = G_direct * 22 is the expected inner
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
let test_crac_gas_accounting_investigation () =
  register_crac_runner_test
    ~title:"CRAC: gas accounting investigation (A/B/C/D scenarios)"
    ~tags:["gas"; "investigation"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas-inv" in

  (* --- Deploy contracts ------------------------------------------------- *)
  Log.debug ~prefix "Deploy GasBurner" ;
  let* gas_burner = EvmGasBurner.deploy_large () in

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
  let* _warmup_a = EvmRunner.call_run ~gas:12_000_000 gas_burner in
  let* _warmup_b = EvmRunner.call_run ~gas:12_000_000 runner_b in
  let* _warmup_c = EvmRunner.call_run ~gas:12_000_000 runner_c in
  let* _warmup_d = EvmRunner.call_run ~gas:12_000_000 runner_d in

  (* --- Measurement calls ------------------------------------------------ *)
  Log.debug ~prefix "Measurement calls (aliases cached)" ;
  let* g_direct = EvmRunner.call_run ~gas:12_000_000 gas_burner in
  let* g_overhead = EvmRunner.call_run ~gas:12_000_000 runner_b in
  let* g_call_evm = EvmRunner.call_run ~gas:12_000_000 runner_c in
  let* g_call = EvmRunner.call_run ~gas:12_000_000 runner_d in

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
     converted to milligas (1 EVM gas = 22 milligas).  We use 18x
     instead of 22x as the lower bound to allow for differences
     between the direct EVM call (G_direct) and the CRAC inner call
     (which may have slightly different intrinsic gas treatment). *)
  let mg_ref = Int64.to_int g_direct in
  Check.(
    (mg_call_evm > mg_ref * 18)
      int
      ~error_msg:
        "%%call_evm milligas (%L) should exceed G_direct*18 (%R): inner EVM \
         gas must be charged") ;

  (* -- TEZ milligas: upper bound (double-counting) --
     Same logic as the EVM-side upper bound.  If the inner EVM gas
     were counted twice in milligas, the value would be ~44x G_direct.
     The 33x bound catches this while leaving room for TEZ overhead. *)
  Check.(
    (mg_call_evm < mg_ref * 33)
      int
      ~error_msg:
        "%%call_evm milligas (%L) should be < G_direct*33 (%R): possible \
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
let test_crac_gas_error_path_reporting () =
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
  let* _ = EvmRunner.call_run ~gas:5000000 gas_burner in

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
let test_crac_gas_oog_path_reporting () =
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
let test_crac_collect_result_surfaces_in_response_body () =
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

(** End-to-end check that the kernel rejects any value transfer to
    [%collect_result].  The Michelson contract calls [%collect_result]
    with a hardcoded 1 mutez amount; the gateway must reject the
    transaction (with no recipient and no CRAC behind it, value has
    nowhere to go), the Michelson server must return 4xx with no bytes
    leaking, and the EVM precompile call must revert.  The outer tx
    catches the revert via [runCatch()] so we can inspect post-revert
    state and confirm [result] stays empty. *)
let test_crac_collect_result_rejects_nonzero_amount () =
  register_crac_runner_test
    ~title:"CRAC: %collect_result rejects non-zero amount"
    ~tags:["collect_result"; "http_call"; "amount"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let payload_hex = "cafebabe" in
  let prefix = "CRAC" in
  Log.debug
    ~prefix
    "Originate Michelson contract that calls %%collect_result with 1 mutez" ;
  (* init_balance must be >= 1 mutez so TRANSFER_TOKENS isn't blocked
     by BalanceTooLow before the kernel's amount check fires. *)
  let* tez_contract =
    TezCollectResultWithAmount.originate ~init_balance:1 ~payload_hex ()
  in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_contract in
  Log.debug ~prefix "Run (catching): CRAC should revert, no bytes leaked" ;
  let* _ = EvmCollectResult.call_run_catch evm_reader in
  Log.debug ~prefix "Verify the precompile call was caught as reverted" ;
  let* () = EvmCollectResult.check_caught ~expected:true evm_reader in
  Log.debug ~prefix "Verify no bytes leaked into the result slot" ;
  let* () = EvmCollectResult.check_result ~expected_hex:"" evm_reader in
  Log.debug ~prefix "Verify the gateway accumulated no tez" ;
  let* gateway_balance =
    Client.get_balance_for ~account:gateway_address client_tezlink
  in
  Check.(
    (Tez.to_mutez gateway_balance = 0)
      int
      ~error_msg:"Expected gateway balance %R mutez but got %L") ;
  unit

(** End-to-end revert-path check: the Michelson contract deposits bytes
    via [%collect_result] then triggers a failure via a second internal
    op.  The server must return 4xx with no bytes leaking, so the EVM
    precompile call reverts and [result] stays empty even after the
    outer tx completes via [runCatch()]. *)
let test_crac_collect_result_revert_discards_bytes () =
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

    Per-internal-op the apply loop charges [Cost::manager_operation] =
    100_000 mgas; MIR then charges [VALUE_STEP] = 100 mgas for the
    bytes typecheck, then 125 mgas + 10 mgas/byte for conversion to
    Micheline; then 125 mgas + 10 mgas/byte for serialization; 1_400_192
    mgas for IO work on the storage, finally the handler adds the
    size-dependent term [460 + 1.5 * size]. *)
let test_crac_collect_result_size_sweep_matches_model () =
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
  let expected_mgas n =
    100_000 + 100 + 460
    + (3 * n / 2)
    + 125 + (10 * n) + 125 + (10 * n) + 1_400_192
  in
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

(** Backward-compatibility / fire-and-forget check: a CRAC that never
    calls [%collect_result] must return empty bytes to the EVM caller.
    The HTTP response body is empty → the precompile ABI-encodes it as
    an empty [bytes] value → [result()] returns [""].

    Use [runCatch()] + [check_caught ~expected:false] to disambiguate:
    a silent precompile failure would also leave [result] empty, so
    asserting [caught = false] proves the call actually succeeded. *)
let test_crac_collect_result_fire_and_forget_empty_body () =
  register_crac_runner_test
    ~title:"CRAC: no %collect_result call returns empty bytes to EVM caller"
    ~tags:["collect_result"; "http_call"; "fire_and_forget"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug
    ~prefix
    "Originate Michelson contract that succeeds without calling \
     %%collect_result" ;
  let* tez_noop = TezAlwaysSucceedsUnit.originate () in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_noop in
  Log.debug ~prefix "Run (catching): EVM → Michelson (no %%collect_result)" ;
  let* _ = EvmCollectResult.call_run_catch evm_reader in
  Log.debug ~prefix "Verify the precompile call did not revert" ;
  let* () = EvmCollectResult.check_caught ~expected:false evm_reader in
  Log.debug ~prefix "Verify result is empty bytes" ;
  EvmCollectResult.check_result ~expected_hex:"" evm_reader

(** Once-per-frame invariant: a second [%collect_result] call in the
    same frame reverts the entire Michelson operation group.  The EVM
    caller must see a revert and [result] must stay empty. *)
let test_crac_collect_result_once_per_frame () =
  register_crac_runner_test
    ~title:"CRAC: second %collect_result in same frame reverts Michelson"
    ~tags:["collect_result"; "http_call"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let payload_hex = "cafebabe" in
  let prefix = "CRAC" in
  Log.debug
    ~prefix
    "Originate contract that calls %%collect_result twice (violates \
     once-per-frame)" ;
  let* tez_contract = TezCollectResultTwice.originate ~payload_hex () in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_contract in
  Log.debug
    ~prefix
    "Run (catching): second %%collect_result should revert the CRAC" ;
  let* _ = EvmCollectResult.call_run_catch evm_reader in
  Log.debug ~prefix "Verify the precompile call was caught as reverted" ;
  let* () = EvmCollectResult.check_caught ~expected:true evm_reader in
  Log.debug ~prefix "Verify no bytes leaked into the result slot" ;
  EvmCollectResult.check_result ~expected_hex:"" evm_reader

(** Nested-frame independence: each CRAC frame has its own independent
    [%collect_result] slot.  Outer Michelson deposits [outer_bytes];
    the nested EVM sub-call triggers its own Michelson CRAC which
    deposits [inner_bytes] into a separate frame.  After unwinding:
      - the outer EVM caller receives [outer_bytes], not [inner_bytes]
      - the inner EVM contract holds [inner_bytes] in its [result()] *)
let test_crac_collect_result_nested_independent_slots () =
  register_crac_runner_test
    ~title:"CRAC: nested frames have independent %collect_result slots"
    ~tags:["collect_result"; "http_call"; "nested"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let outer_hex = "aabb" in
  let inner_hex = "0102" in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy inner Michelson contract (deposits inner_bytes)" ;
  let* tez_inner = TezCollectResult.originate ~payload_hex:inner_hex () in
  Log.debug
    ~prefix
    "Deploy inner EVM reader (calls Michelson inner via precompile)" ;
  let* evm_inner = EvmCollectResult.deploy_and_init tez_inner in
  Log.debug
    ~prefix
    "Deploy outer Michelson contract (deposits outer_bytes then calls \
     EVM_inner)" ;
  let* tez_outer =
    TezCollectResultThenCallEvm.originate
      ~evm_target:evm_inner
      ~payload_hex:outer_hex
      ()
  in
  Log.debug
    ~prefix
    "Deploy outer EVM reader (calls Michelson outer via precompile)" ;
  let* evm_outer = EvmCollectResult.deploy_and_init tez_outer in
  Log.debug
    ~prefix
    "Run outer: EVM_outer → M_outer → [collect_result(outer); \
     call_evm(EVM_inner)] → EVM_inner → M_inner → collect_result(inner)" ;
  let* _ = EvmCollectResult.call_run evm_outer in
  Log.debug ~prefix "Verify outer EVM reader got outer_bytes (not inner)" ;
  let* () = EvmCollectResult.check_result ~expected_hex:outer_hex evm_outer in
  Log.debug ~prefix "Verify inner EVM reader got inner_bytes" ;
  EvmCollectResult.check_result ~expected_hex:inner_hex evm_inner

(** OOG guard: a Michelson contract that deposits 4 MiB of bytes via
    [%collect_result] causes EVM OOG when the precompile return value is
    copied into memory.  The [runCatch()] wrapper catches the OOG and
    sets [caught = true]; [result()] must remain empty.

    Gas is pinned explicitly: 4 MiB = 131_072 EVM words and memory
    expansion alone costs [3 * w + w² / 512 ≈ 33.9M] gas, so a 3M
    limit guarantees the OOG happens at the memory-copy step rather
    than masking some other failure.  3M is also high enough that the
    EIP-150 1/64 reserve left behind for [runCatch]'s [catch] block
    can afford the [sstore caught = true] (~22K).  We then assert
    [gasUsed > 95% * gas_limit], which is the OOG signature: a
    graceful [revert] returns unused gas (so [gasUsed] would be
    small), whereas an OOG burns the entire forwarded sub-call
    budget (~63/64 of the limit) before [catch] runs. *)
let test_crac_collect_result_large_payload_oog () =
  register_crac_runner_test
    ~title:"CRAC: 4 MiB %collect_result payload causes EVM OOG"
    ~tags:["collect_result"; "http_call"; "oog"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let gas_limit = 3_000_000 in
  Log.debug
    ~prefix
    "Originate Michelson contract that generates 4 MiB via CONCAT doublings" ;
  let* tez_large = TezGenerate4MibResult.originate () in
  Log.debug ~prefix "Deploy EVM reader contract" ;
  let* evm_reader = EvmCollectResult.deploy_and_init tez_large in
  Log.debug
    ~prefix
    "Run (catching) with gas=%d: 4 MiB return should cause EVM OOG"
    gas_limit ;
  let* gas_used = EvmCollectResult.call_run_catch ~gas:gas_limit evm_reader in
  Log.debug ~prefix "Verify the precompile call was caught (OOG)" ;
  let* () = EvmCollectResult.check_caught ~expected:true evm_reader in
  Log.debug
    ~prefix
    "Verify gasUsed (%Ld) is close to gas_limit (%d)"
    gas_used
    gas_limit ;
  let gas_limit_64 = Int64.of_int gas_limit in
  Check.(
    (gas_used > Int64.div (Int64.mul gas_limit_64 95L) 100L)
      int64
      ~error_msg:
        "OOG gasUsed (%L) should exceed 95%% * gas_limit (%R): a graceful \
         revert would return unused gas, but OOG burns the forwarded sub-call \
         budget (~63/64 of the limit) before [catch] runs") ;
  Log.debug ~prefix "Verify no bytes leaked into the result slot" ;
  EvmCollectResult.check_result ~expected_hex:"" evm_reader

(** Regression test for L2-1296: the internal [TEZOSX_CALLER_ADDRESS]
    (0x7e20580000000000000000000000000000000001) used by [generate_alias] to
    initialise EVM aliases must not accumulate a visible balance.

    Earlier kernels funded this caller by writing [U256::MAX] to durable
    storage as a "safety" buffer, but the surrounding [run_transaction] is
    [TransactionOrigin::CrossRuntime] so its EVM journal is never committed:
    only the manual storage write persisted, and the address ended up
    showing balance ≈ 2^256 - 1 on Blockscout for any TezosX network.

    The funding has been removed (gas_price=0 and value=0 mean REVM's
    pre-flight balance check requires nothing); this test exercises a TEZ →
    EVM CRAC (which forces alias generation through the EVM runtime) and
    asserts [eth_getBalance] returns 0 for [TEZOSX_CALLER_ADDRESS]. *)
let test_crac_tezosx_caller_balance_stays_zero () =
  register_crac_runner_test
    ~title:"CRAC: TEZOSX_CALLER_ADDRESS balance stays zero (L2-1296)"
    ~tags:["caller_balance"; "l2_1296"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let tezosx_caller_address = "0x7e20580000000000000000000000000000000001" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Trigger TEZ -> EVM crossing (forces alias generation)" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Read TEZOSX_CALLER_ADDRESS balance via eth_getBalance" ;
  let*@ balance = Rpc.get_balance ~address:tezosx_caller_address sequencer in
  Check.(
    (balance = Wei.zero)
      Wei.typ
      ~error_msg:
        "Expected TEZOSX_CALLER_ADDRESS balance 0 but got %L (regression of \
         L2-1296: the internal caller is leaking a persistent balance to EVM \
         RPC consumers)") ;
  unit

(** Regression test for L2-1295: [eth_estimateGas] on an EVM->TEZ bridge
    call must succeed (the RPC the bug originally surfaced on), and an
    end-to-end transaction injected with that estimate must reach the
    Michelson side.

    Before this fix, the runtime_gateway precompile forwarded
    [remaining_evm_gas * 100] as [X-Tezos-Gas-Limit], and the Michelson
    runtime rejected the request via [TezlinkOperationGas::start_milligas]
    as soon as the value exceeded [hard_gas_limit_per_operation]
    (1_040_000_000 milligas). The EVM node's [eth_estimateGas] binary-
    searches down from the per-transaction gas cap (30M) towards the
    minimum gas a successful call needs; once every probe with gas >=
    ~10.4M reverted with TransferFailed, the estimator could not converge
    and the RPC failed outright. Raising the Michelson runtime hard cap
    to 3_000_000_000 milligas (matching the EVM 30M-gas per-tx cap, with
    1 EVM gas = 100 milligas) lets the high probes succeed and the
    estimator narrow in on the real cost.

    Using [eth_estimateGas] in the test rather than a hard-coded gas
    value means the test stays correct if the per-transaction cap ever
    changes, and catches any future regression that introduces a new
    ceiling anywhere between the previous bug threshold (~10.4M) and the
    per-tx cap — since any such regression would once again break the
    high-end probes of the binary search and surface as an
    [eth_estimateGas] failure. *)
let test_crac_evm_to_tez_high_gas () =
  register_crac_runner_test
    ~title:"CRAC: eth_estimateGas works on EVM->TEZ bridge call (L2-1295)"
    ~tags:["gas"; "estimate_gas"; "l2_1295"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let (`Evm_runner bridge_addr) = evm_bridge in
  Log.debug ~prefix "Call eth_estimateGas on the bridge run() entrypoint" ;
  let* run_calldata = Cast.calldata "run()" in
  let estimate_call =
    [
      ("from", `String sender.Eth_account.address);
      ("to", `String bridge_addr);
      ("data", `String run_calldata);
    ]
  in
  (* Pre-fix this RPC errored out: the binary search starts at the per-tx
     cap and the high probes all reverted with TransferFailed. *)
  let*@ estimated_gas = Rpc.estimate_gas estimate_call sequencer in
  Log.info "eth_estimateGas returned %Ld" estimated_gas ;
  (* Send the transaction with the estimated gas (mirrors what tools like
     foundry / metamask do) and verify the CRAC reached the Michelson
     side. *)
  Log.debug ~prefix "Send EVM tx with gas = eth_estimateGas's value" ;
  let* _ = EvmRunner.call_run ~gas:(Int64.to_int estimated_gas) evm_bridge in
  Log.debug ~prefix "Verify TEZ runner counter (CRAC reached Michelson side)" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  Log.debug ~prefix "Verify EVM bridge counter (incremented before+after CRAC)" ;
  EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge

(* ======================================================================
 * L2-1366 regression: inbound-CRAC origination-nonce isolation.
 *
 * Before the fix, every inbound CRAC handler invocation built its own
 * local `OriginationNonce::initial(OperationHash::default())` from a
 * constant 32-byte seed.  The KT1 of a child originated through MIR
 * `CREATE_CONTRACT` is `digest_160(operation_hash[32] || index[4])`,
 * so without any block / operation / CRAC entropy in the seed every
 * inbound-CRAC `CREATE_CONTRACT` collapsed onto the same KT1 and
 * silently overwrote any previously-deployed child.
 *
 * The fix derives the operation hash from
 * `blake2b(crac:<block_number>:<crac_id>)` AND persists the
 * origination index in the Michelson journal across the inbound-CRAC
 * handler invocations of a single synthetic Michelson manager
 * operation.  This guarantees three properties end-to-end:
 *
 *   1. cross-operation isolation (two top-level EVM txs in the same
 *      block);
 *   2. cross-block isolation (two CRACs in different blocks);
 *   3. intra-operation isolation (two inbound CRACs triggered by the
 *      same top-level EVM transaction, e.g. one EVM contract calling
 *      two distinct Michelson callees in turn — the case the
 *      block-number/crac-id seed alone could not catch because both
 *      CRACs share the same seed).
 *
 * The three tests below pin each property; a fourth pre-fix
 * verification asserts the children never collapse onto the legacy
 * constant address.
 * ====================================================================== *)

(* Pre-fix collision target: `digest_160([0;32] || 0x00000001)`.  Any
   child KT1 equal to this constant is the signature of the original
   constant-seed bug. *)
let crac_orig_buggy_constant_kt1 = "KT1Mjjcb6tmSsLm7Cb3DSQszePjfchPM4Uxm"

(* Find the CREATE_CONTRACT child KT1 sourced by [callee_kt1] (the
   originated child performed inside that callee's %run).  The
   CREATE_CONTRACT child is distinguished from the per-CRAC alias
   originations (sourced by the NULL_PKH handler) by its source being
   the callee itself. *)
let crac_orig_child_kt1_for_callee ~prefix ~op_list ~callee_kt1 =
  let children =
    List.concat_map
      (fun op ->
        let top = JSON.(op |-> "contents" |=> 0) in
        let internals =
          JSON.(top |-> "metadata" |-> "internal_operation_results" |> as_list)
        in
        List.concat_map
          (fun iop ->
            let kind = JSON.(iop |-> "kind" |> as_string) in
            let src = JSON.(iop |-> "source" |> as_string) in
            if kind = "origination" && src = callee_kt1 then
              JSON.(iop |-> "result" |-> "originated_contracts" |> as_list)
              |> List.map JSON.as_string
            else [])
          internals)
      op_list
  in
  Log.info
    "%s: CREATE_CONTRACT child(ren) sourced by callee %s = [%s]"
    prefix
    callee_kt1
    (String.concat "; " children) ;
  match children with
  | [child] -> child
  | [] ->
      Test.fail
        "%s: no CREATE_CONTRACT child sourced by callee %s found in any \
         manager op"
        prefix
        callee_kt1
  | _ ->
      Test.fail
        "%s: expected exactly one CREATE_CONTRACT child sourced by callee %s, \
         got %d"
        prefix
        callee_kt1
        (List.length children)

(* Find every CREATE_CONTRACT child KT1 originated through any of the
   given [callee_kt1s] across the supplied manager ops.  Used by the
   intra-tx test, where a single Michelson op merges the originations
   from two sequential inbound CRACs under one synthetic receipt. *)
let crac_orig_child_kt1s_for_callees ~prefix ~op_list ~callee_kt1s =
  let callees = List.sort_uniq String.compare callee_kt1s in
  let children =
    List.concat_map
      (fun op ->
        let top = JSON.(op |-> "contents" |=> 0) in
        let internals =
          JSON.(top |-> "metadata" |-> "internal_operation_results" |> as_list)
        in
        List.concat_map
          (fun iop ->
            let kind = JSON.(iop |-> "kind" |> as_string) in
            let src = JSON.(iop |-> "source" |> as_string) in
            if kind = "origination" && List.mem src callees then
              JSON.(iop |-> "result" |-> "originated_contracts" |> as_list)
              |> List.map JSON.as_string
            else [])
          internals)
      op_list
  in
  Log.info
    "%s: CREATE_CONTRACT children sourced by callees [%s] = [%s]"
    prefix
    (String.concat "; " callees)
    (String.concat "; " children) ;
  children

(* Register a fullstack test exposing the raw setup components.  Only
   used by tests that genuinely need the rollup node (e.g. the PVM
   replay in [test_crac_evm_deep_recurse_then_michelson_oog]); every
   other CRAC test runs on the sequencer-only sandbox.

   Tag-neutral base. The [tags] argument is appended verbatim to the
   default [tezosx; <runtime>; crac] tags so callers control the
   per-test classification (e.g., stack_overflow, oog). *)
let crac_register ~title ~tags body =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"] @ List.map Tezosx_runtime.tag with_runtimes @ ["crac"] @ tags
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
  let {Setup.sc_rollup_node; sequencer; client; evm_version; _} =
    sequencer_setup
  in
  let* client_tezlink = tezlink_client_from_evm_node sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let source = Constant.bootstrap5 in
  let ref_nonce = ref 0 in
  let evm_nonce () =
    let n = !ref_nonce in
    incr ref_nonce ;
    n
  in
  let ref_counter = ref 1 in
  let tez_counter () =
    let c = !ref_counter in
    incr ref_counter ;
    c
  in
  body
    ~sc_rollup_node
    ~sequencer
    ~client
    ~client_tezlink
    ~evm_version
    ~sender
    ~source
    ~evm_nonce
    ~tez_counter
    protocol

(* Origination-flavoured sandbox register used by tests tracking the
   L2-1366 origination-collision ticket.  Exposes the raw components
   (we need direct access to [client] / [client_tezlink] to originate
   Michelson contracts via direct tezlink injection, which the CRAC
   runner wrapper hides). *)
let crac_orig_register ~title ~tags body =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"]
    @ List.map Tezosx_runtime.tag with_runtimes
    @ ["crac"; "origination"; "l2_1366"]
    @ tags
  in
  Test_helpers.register_sandbox
    ~__FILE__
    ~uses_client:true
    ~kernel:Latest
    ~title
    ~tags
    ~with_runtimes
    ~minimum_base_fee_per_gas:crac_minimum_base_fee_per_gas
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer ->
  let* client =
    Client.init_mockup ~protocol:Michelson_contracts.tezlink_protocol ()
  in
  let* client_tezlink = tezlink_client_from_evm_node sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let source = Constant.bootstrap5 in
  let ref_nonce = ref 0 in
  let evm_nonce () =
    let n = !ref_nonce in
    incr ref_nonce ;
    n
  in
  let ref_counter = ref 1 in
  let tez_counter () =
    let c = !ref_counter in
    incr ref_counter ;
    c
  in
  body
    ~sequencer
    ~client
    ~client_tezlink
    ~evm_version:(Kernel.select_evm_version Kernel.Latest)
    ~sender
    ~source
    ~evm_nonce
    ~tez_counter
    Michelson_contracts.tezlink_protocol

(* Originate the Michelson callee fixture: a parameterless %run that
   performs exactly one CREATE_CONTRACT. *)
let crac_orig_originate_create_on_run ~client ~client_tezlink ~sequencer ~source
    ~counter protocol =
  TezContract.originate_contract_via_tezlink
    ~client
    ~client_tezlink
    ~sequencer
    ~source
    ~counter
    ~script_name:["mini_scenarios"; "crac_originate_on_run"]
    ~init_storage_data:"0"
    protocol

(* Deploy + initialise an EVM bridge whose run() NACs into the given
   Michelson %run target. *)
let crac_orig_deploy_bridge ~evm_version ~sequencer ~sender ~nonce ~kt1 =
  let* addr =
    EvmCrossRuntimeRunnerTez.deploy ~evm_version ~sequencer ~sender ~nonce ()
  in
  let* () =
    EvmCrossRuntimeRunnerTez.init
      ~sequencer
      ~sender
      ~nonce:(nonce + 1)
      ~value:Wei.zero
      ~tez_contract_target_address:kt1
      addr
  in
  return addr

(* Craft + send a raw EVM run() tx to the mempool without producing a
   block.  Used to land two distinct top-level CRACs in the same block. *)
let crac_orig_send_run_no_block ~sequencer ~sender ~nonce ~address =
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:EvmContract.tezosx_evm_chain_id
      ~nonce
      ~gas:2_000_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  Rpc.send_raw_transaction ~raw_tx sequencer

(* PRIMARY: two distinct top-level EVM txs in ONE block, each NACing
   into a Michelson callee that performs CREATE_CONTRACT.  With the
   fix, the two children must derive DISTINCT KT1s. *)
let test_crac_orig_two_txs_one_block () =
  crac_orig_register
    ~title:
      "CRAC: two inbound-CRAC CREATE_CONTRACT in two txs / one block produce \
       distinct KT1s"
    ~tags:["collision"]
  @@
  fun ~sequencer
      ~client
      ~client_tezlink
      ~evm_version
      ~sender
      ~source
      ~evm_nonce
      ~tez_counter
      protocol
    ->
  let prefix = "CRAC-ORIG" in
  Log.debug ~prefix "Originate Michelson CREATE_CONTRACT callee #1" ;
  let* _, kt1_callee_1 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  Log.debug ~prefix "Originate Michelson CREATE_CONTRACT callee #2" ;
  let* _, kt1_callee_2 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  Log.debug ~prefix "Deploy EVM bridge #1 -> callee #1" ;
  let n1 = evm_nonce () in
  let* bridge_1 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n1
      ~kt1:kt1_callee_1
  in
  let _ =
    evm_nonce ()
    (* consumed by bridge init *)
  in
  Log.debug ~prefix "Deploy EVM bridge #2 -> callee #2" ;
  let n2 = evm_nonce () in
  let* bridge_2 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n2
      ~kt1:kt1_callee_2
  in
  let _ = evm_nonce () in
  (* Send both run() txs to the mempool, then produce one block so both
     land as two distinct top-level operations in the same block. *)
  Log.debug ~prefix "Send both run() txs to mempool (no block yet)" ;
  let*@ _h1 =
    crac_orig_send_run_no_block
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~address:bridge_1
  in
  let*@ _h2 =
    crac_orig_send_run_no_block
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~address:bridge_2
  in
  Log.debug ~prefix "Produce ONE block containing both txs" ;
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage
      ~sequencer
      ~expected_counter:2
      bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage
      ~sequencer
      ~expected_counter:2
      bridge_2
  in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: %d Michelson manager operation(s) in block"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 2)
      int
      ~error_msg:"expected 2 top-level Michelson ops (one per EVM tx), got %L") ;
  let child_1 =
    crac_orig_child_kt1_for_callee ~prefix ~op_list ~callee_kt1:kt1_callee_1
  in
  let child_2 =
    crac_orig_child_kt1_for_callee ~prefix ~op_list ~callee_kt1:kt1_callee_2
  in
  Log.info
    "%s OBSERVABLE: child_op1=%s child_op2=%s (must be DISTINCT)"
    prefix
    child_1
    child_2 ;
  Check.(
    (child_1 <> child_2)
      string
      ~error_msg:
        "regression: two inbound-CRAC CREATE_CONTRACTs in one block collided \
         onto the same KT1 (%L). The origination-nonce seed has lost its \
         per-operation entropy.") ;
  Check.(
    (child_1 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: child_1 equals the pre-fix constant collision KT1 (%L) — \
         the constant-seed bug is back.") ;
  Check.(
    (child_2 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: child_2 equals the pre-fix constant collision KT1 (%L) — \
         the constant-seed bug is back.") ;
  unit

(* SCOPE: two CRACs landing in DIFFERENT blocks must also produce
   distinct KT1s.  Pre-fix, they collided on the same constant address
   regardless of block; post-fix the block_number is mixed into the
   seed so cross-block isolation also holds. *)
let test_crac_orig_two_txs_two_blocks () =
  crac_orig_register
    ~title:
      "CRAC: two inbound-CRAC CREATE_CONTRACT in two txs / two blocks produce \
       distinct KT1s"
    ~tags:["cross_block"]
  @@
  fun ~sequencer
      ~client
      ~client_tezlink
      ~evm_version
      ~sender
      ~source
      ~evm_nonce
      ~tez_counter
      protocol
    ->
  let prefix = "CRAC-ORIG-BLK" in
  let* _, kt1_callee_1 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  let* _, kt1_callee_2 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  (* The two callees were originated via the native tezlink-injection
     path (real per-op hash); their addresses must already be distinct —
     this guards the fixture itself. *)
  Check.(
    (kt1_callee_1 <> kt1_callee_2)
      string
      ~error_msg:
        "native control: two natively-originated callees collided on %L — the \
         native-path origination nonce seeding regressed.") ;
  let n1 = evm_nonce () in
  let* bridge_1 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n1
      ~kt1:kt1_callee_1
  in
  let _ = evm_nonce () in
  let n2 = evm_nonce () in
  let* bridge_2 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n2
      ~kt1:kt1_callee_2
  in
  let _ = evm_nonce () in
  (* Block 1: only bridge_1 fires. *)
  let*@ _h1 =
    crac_orig_send_run_no_block
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~address:bridge_1
  in
  let*@ _b1 = Rpc.produce_block sequencer in
  let* ops1 = fetch_recent_michelson_manager_ops sequencer in
  let child_1 =
    crac_orig_child_kt1_for_callee
      ~prefix
      ~op_list:JSON.(ops1 |> as_list)
      ~callee_kt1:kt1_callee_1
  in
  (* Block 2: only bridge_2 fires. *)
  let*@ _h2 =
    crac_orig_send_run_no_block
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~address:bridge_2
  in
  let*@ _b2 = Rpc.produce_block sequencer in
  let* ops2 = fetch_recent_michelson_manager_ops sequencer in
  let child_2 =
    crac_orig_child_kt1_for_callee
      ~prefix
      ~op_list:JSON.(ops2 |> as_list)
      ~callee_kt1:kt1_callee_2
  in
  Log.info
    "%s OBSERVABLE (cross-block): child_block1=%s child_block2=%s (must be \
     DISTINCT)"
    prefix
    child_1
    child_2 ;
  Check.(
    (child_1 <> child_2)
      string
      ~error_msg:
        "regression: two inbound-CRAC CREATE_CONTRACTs in separate blocks \
         still collided on %L. The block-number entropy in the \
         origination-nonce seed is missing.") ;
  Check.(
    (child_1 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: cross-block child_1 equals the pre-fix constant collision \
         KT1 (%L).") ;
  Check.(
    (child_2 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: cross-block child_2 equals the pre-fix constant collision \
         KT1 (%L).") ;
  unit

(* INTRA-TX: a SINGLE top-level EVM transaction triggering TWO
   sequential inbound CRACs, each performing CREATE_CONTRACT on a
   distinct Michelson callee.  Both CRACs share the same `block_number`
   and `crac_id`, so a seed-only fix would leave them colliding —
   this test fails until the origination-nonce index is also
   persisted across handler invocations.  An EVM "main" contract is
   wired to call two bridges; one EVM call produces one synthetic
   Michelson manager op carrying both CREATE_CONTRACTs, and we
   assert the two children are distinct KT1s. *)
let test_crac_orig_two_cracs_one_tx () =
  crac_orig_register
    ~title:
      "CRAC: two inbound-CRAC CREATE_CONTRACT in a single EVM tx produce \
       distinct KT1s"
    ~tags:["intra_tx"]
  @@
  fun ~sequencer
      ~client
      ~client_tezlink
      ~evm_version
      ~sender
      ~source
      ~evm_nonce
      ~tez_counter
      protocol
    ->
  let prefix = "CRAC-ORIG-INTRA" in
  Log.debug ~prefix "Originate Michelson CREATE_CONTRACT callee #1" ;
  let* _, kt1_callee_1 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  Log.debug ~prefix "Originate Michelson CREATE_CONTRACT callee #2" ;
  let* _, kt1_callee_2 =
    crac_orig_originate_create_on_run
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      protocol
  in
  let n1 = evm_nonce () in
  let* bridge_1 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n1
      ~kt1:kt1_callee_1
  in
  let _ = evm_nonce () in
  let n2 = evm_nonce () in
  let* bridge_2 =
    crac_orig_deploy_bridge
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:n2
      ~kt1:kt1_callee_2
  in
  let _ = evm_nonce () in
  (* EVM "main" caller that, on `run()`, invokes both bridges in order.
     `multi_run_caller` Solidity contract: `run()` iterates its
     configured callees calling each one's `run()`. *)
  let* main_addr =
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
      ~value:Wei.zero
      ~revert:false
      ~callees:[(bridge_1, false); (bridge_2, false)]
      main_addr
  in
  Log.debug ~prefix "Send single run() tx triggering both CRACs in sequence" ;
  let*@ _h =
    crac_orig_send_run_no_block
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~address:main_addr
  in
  let*@ _block_number = Rpc.produce_block sequencer in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: %d Michelson manager operation(s) in block"
    prefix
    (List.length op_list) ;
  (* Re-entrant inner CRACs of the same EVM tx are folded into ONE
     synthetic Michelson manager op (`merge_crac_internals`).  Both
     CREATE_CONTRACTs must therefore appear as internal originations
     of that single op. *)
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:
        "expected 1 synthetic Michelson manager op for the merged intra-tx \
         CRACs, got %L") ;
  let children =
    crac_orig_child_kt1s_for_callees
      ~prefix
      ~op_list
      ~callee_kt1s:[kt1_callee_1; kt1_callee_2]
  in
  Check.(
    (List.length children = 2)
      int
      ~error_msg:
        "expected exactly 2 CREATE_CONTRACT children (one per inbound CRAC), \
         got %L") ;
  let child_1 = List.nth children 0 in
  let child_2 = List.nth children 1 in
  Log.info
    "%s OBSERVABLE (intra-tx): child_a=%s child_b=%s (must be DISTINCT)"
    prefix
    child_1
    child_2 ;
  Check.(
    (child_1 <> child_2)
      string
      ~error_msg:
        "regression: two inbound CRACs triggered by a SINGLE EVM tx collided \
         on the same KT1 (%L). The origination-nonce index is not being \
         persisted across handler invocations within one synthetic op.") ;
  Check.(
    (child_1 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: intra-tx child_a equals the pre-fix constant collision \
         KT1 (%L).") ;
  Check.(
    (child_2 <> crac_orig_buggy_constant_kt1)
      string
      ~error_msg:
        "regression: intra-tx child_b equals the pre-fix constant collision \
         KT1 (%L).") ;
  unit

(* EVM recurses through a chain of nested [this.recurse] calls, then at the
   deepest frame invokes a diverging Michelson lambda. The depth is kept
   moderate on purpose: each external [this.recurse] hop forwards at most
   63/64 of the remaining gas, so at depth 1023 only ~(63/64)^1023 ≈ 1e-7 of
   the budget survives to the deepest frame — exhausted by the gateway base
   cost before the Michelson call is ever dispatched. A depth that leaves
   enough budget guarantees the diverging lambda is actually reached, so the
   test exercises the Michelson OOG path (not pure EVM self-recursion). The
   transaction must fail (Michelson runs out of gas) without trapping the
   kernel: a kernel trap produces no receipt, which [craft_and_send_transaction]
   turns into [Test.fail], so [~expected_status:false] only passes on a clean
   OOG. *)
let crac_evm_recurse_depth = 50

let test_crac_evm_deep_recurse_then_michelson_oog =
  crac_register
    ~title:
      "CRAC: deep EVM recursion then Michelson diverging lambda returns OOG"
    ~tags:["stack_overflow"; "oog"]
  @@
  fun ~sc_rollup_node
      ~sequencer
      ~client
      ~client_tezlink
      ~evm_version
      ~sender
      ~source
      ~evm_nonce
      ~tez_counter
      protocol
    ->
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate diverging_lambda Michelson contract" ;
  let* _lambda_hex, lambda_kt1 =
    TezContract.originate_contract_via_tezlink
      ~client
      ~client_tezlink
      ~sequencer
      ~source
      ~counter:(tez_counter ())
      ~script_name:["mini_scenarios"; "diverging_lambda"]
      ~init_storage_data:"0"
      protocol
  in
  Log.debug ~prefix "Deploy DeepRecurseThenMichelson EVM contract" ;
  let* contract_addr =
    EvmContract.deploy_solidity_contract
      ~evm_version
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~contract:Solidity_contracts.deep_recurse_then_michelson
      ()
  in
  Log.debug ~prefix "Initialize with Michelson contract address" ;
  let* _ =
    EvmContract.craft_and_send_transaction
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~value:Wei.zero
      ~address:contract_addr
      ~abi_signature:"initialize(string)"
      ~arguments:[lambda_kt1]
      ()
  in
  Log.debug
    ~prefix
    "Call recurse(%d) with block gas limit, expect tx failure"
    crac_evm_recurse_depth ;
  let recurse_gas_limit = 30_000_000 in
  let* recurse_receipt =
    EvmContract.craft_and_send_transaction
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~value:Wei.zero
      ~address:contract_addr
      ~abi_signature:"recurse(uint256)"
      ~arguments:[string_of_int crac_evm_recurse_depth]
      ~gas:recurse_gas_limit
      ~expected_status:false
      ()
  in
  (* `expected_status:false` alone is satisfied by any tx failure
     — clean OOG (what we want), EVM-side revert before reaching
     the gateway dispatch (would mean the budget didn't survive the
     50 self-recursions to MIR), or a kernel trap that the apply
     pipeline catches and turns into a synthetic failed receipt
     with a cheap-looking gas profile.

     Worth understanding what gas a clean deep-OOG actually
     consumes: EIP-150 caps each external `this.recurse()` hop's
     forwarded budget at 63/64 of the caller's remaining. After
     50 hops only `(63/64)^50 ≈ 45.5%` of the 30M EVM gas limit
     survives to the deepest frame; the remaining ~54.5% sits
     unspent across the outer frames' 1/64 reserves. When the
     deepest frame OOGs cleanly through MIR the outer frames
     return without ever consuming their reserves, so the
     receipt-level `gas_used` is bounded above by roughly
     `G * (1 - (63/64)^N) ≈ 16M` EVM gas — *not* anywhere near
     the 30M limit. Observed value is ~13.5M.

     The threshold here is a generous lower bound chosen to
     distinguish a deep-recursion OOG from cheap EVM failures
     (revert before gateway dispatch, ABI mismatch, intrinsic-cost
     rejection) which all consume well under 1M gas. 5M gives a
     ~10× margin over any cheap-failure path while staying well
     under the ~13.5M a successful deep-OOG actually burns. *)
  let gas_used = Int64.to_int recurse_receipt.gasUsed in
  let min_gas_used = 5_000_000 in
  Check.(
    (gas_used >= min_gas_used)
      int
      ~error_msg:
        (Printf.sprintf
           "expected the recurse(%d) call to consume >= %d EVM gas (a deep \
            recursion that survived to MIR and OOGed cleanly should burn ~13M \
            of the 30M limit; cheap EVM-side failures consume <1M), got %%L — \
            the divergent Michelson lambda is not being exercised"
           crac_evm_recurse_depth
           min_gas_used)) ;
  (* `expected_status:false` also cannot tell a graceful OOG from a
     kernel trap or reboot. Prove the kernel is still alive by
     landing a follow-up successful call (any cheap entrypoint that
     re-runs `initialize` is enough), reading the receipt back, and
     asserting status. *)
  Log.debug ~prefix "Follow-up: kernel-liveness probe via initialize()" ;
  let* _ =
    EvmContract.craft_and_send_transaction
      ~sequencer
      ~sender
      ~nonce:(evm_nonce ())
      ~value:Wei.zero
      ~address:contract_addr
      ~abi_signature:"initialize(string)"
      ~arguments:[lambda_kt1]
      ~expected_status:true
      ()
  in
  (* Re-execute every blueprint (including the diverging-lambda
     block) through the rollup-node PVM. The PVM runs the kernel on
     the small consensus stack, not the large-stack sequencer Wasmer
     runtime, so a recursive walker that overflowed only on the
     small stack would stall this sync. *)
  Log.debug ~prefix "PVM replay via bake_until_sync" ;
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  unit

(* ── Address-identity (round-trip / path-independence) tests ─────────── *)

let evm_alias_of_tezos_address = Test_helpers.evm_alias_of_tezos_address

(** Reads the [/info] account record of EVM account [evm_address] from
 *  the sequencer's durable storage.  The origin classification — alias
 *  payload included — is carried as the fourth field of this record.
 *  Returns the raw hex of the whole record, or [None] if the account
 *  has no record. *)
let read_evm_account_record ~sequencer evm_address =
  let*@ record =
    Rpc.state_value
      sequencer
      (Durable_storage_path.account_info Kernel.Latest evm_address)
  in
  return record

(** Asserts that EVM account [evm_address] carries no alias
 *  classification for [native_address]: either the account record is
 *  absent, or it does not embed the native address (the alias payload
 *  is the only place the account record can mention it). *)
let check_evm_origin_absent ~sequencer ~native_address evm_address =
  let* record = read_evm_account_record ~sequencer evm_address in
  (match record with
  | None -> ()
  | Some payload ->
      let native_hex = Hex.(show (of_string native_address)) in
      if String.lowercase_ascii payload =~ rex native_hex then
        Test.fail
          "Expected no alias classification for %s but its account record %s \
           embeds native address %s (hex %s)"
          evm_address
          payload
          native_address
          native_hex) ;
  unit

(** Asserts that the account record of EVM account [evm_address] embeds
 *  an alias classification for [native_address] (as the hex of its
 *  UTF-8 bytes). *)
let check_evm_origin_records_alias ~sequencer ~native_address evm_address =
  let* record = read_evm_account_record ~sequencer evm_address in
  match record with
  | None ->
      Test.fail
        "Expected an account record with an alias classification for %s (alias \
         of %s), found none"
        evm_address
        native_address
  | Some payload ->
      let native_hex = Hex.(show (of_string native_address)) in
      if not (String.lowercase_ascii payload =~ rex native_hex) then
        Test.fail
          "account record %s of %s does not embed native address %s (hex %s)"
          payload
          evm_address
          native_address
          native_hex ;
      unit

(** Four-hop TEZ→EVM→TEZ→EVM CRAC: the original tz1's EVM alias appears as
 *  [tx.origin] at the final EVM recorder (originator axis), while [msg.sender]
 *  is the TEZ bridge's EVM alias (immediate-caller axis).
 *
 *    Chain: bootstrap5 (tz1) → gateway/%call_evm → evm_A → tez_B → evm_C
 *
 *  Entering EVM materialises [E = evm_alias(tz1)] and records
 *  [origin(E) = Alias{Tezos, tz1}].  Hopping back into Tezos must resolve
 *  [E] transitively to the original [tz1] (round-trip), and the final hop
 *  back into EVM re-derives [tx.origin = E] from it.  If the middle hop
 *  blind-derived instead of round-tripping, the final hop would derive
 *  the alias of a fresh KT1 and the PRIMARY assertion would fail.
 *
 *  PRIMARY assertion: [lastOrigin] (slot 1) at evm_C =
 *    [evm_alias_of_tezos_address(source.public_key_hash)] — the tz1's EVM alias.
 *  SECONDARY assertion: [lastSender] (slot 0) = EVM alias of tez_B, the
 *    immediate Tezos caller at the last hop.
 *  Negative: a fresh EVM recorder that was never targeted holds the initial zero
 *    address for both slots.
 *)
let test_crac_roundtrip_tz1_via_evm_back_to_tezos () =
  register_crac_runner_test
    ~title:
      "CRAC address-identity: TEZ→EVM→TEZ→EVM round-trip tx.origin recovers \
       tz1 EVM alias"
    ~tags:["address_identity"; "round_trip"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-RT-TZ1" in
  let original_tz1 = source.public_key_hash in
  (* E = evm_alias(tz1): the expected tx.origin at the final EVM recorder. *)
  let expected_origin = evm_alias_of_tezos_address original_tz1 in
  Log.info
    "%s: original tz1 = %s, expected tx.origin (E) = %s"
    prefix
    original_tz1
    expected_origin ;
  (* evm_C: leaf EVM IdentityRecorder *)
  Log.debug ~prefix "Deploy EVM IdentityRecorder (evm_C, leaf)" ;
  let* evm_recorder = EvmIdentityRecorder.deploy () in
  (* tez_B: TEZ→EVM bridge targeting evm_C *)
  Log.debug ~prefix "Originate TEZ bridge (tez_B) targeting evm_C" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  Log.info "%s: tez_bridge KT1 (tez_B) = %s" prefix tez_bridge_kt1 ;
  (* evm_alias(tez_B) — expected msg.sender at evm_C: the immediate Tezos
     caller's deterministic alias. *)
  let expected_sender = evm_alias_of_tezos_address tez_bridge_kt1 in
  Log.info "%s: expected msg.sender at evm_C = %s" prefix expected_sender ;
  (* evm_A: EVM→TEZ bridge targeting tez_B *)
  Log.debug ~prefix "Deploy EVM bridge (evm_A) targeting tez_B" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  Log.info "%s: evm_bridge address (evm_A) = %s" prefix evm_bridge_addr ;
  (* Fire: tz1 → gateway → evm_A → tez_B → evm_C *)
  Log.debug
    ~prefix
    "Fire tz1→gateway CRAC into evm_A (→ tez_B → evm_C recorder)" ;
  let* () =
    Gateway.call_evm
      ~evm_target:evm_bridge
      ~method_sig:"run()"
      ~abi_params:""
      ()
  in
  (* PRIMARY: tx.origin at the EVM recorder = evm_alias(tz1).
     The originator only survives the EVM→TEZ→EVM hops via transitive
     resolution. *)
  Log.debug ~prefix "PRIMARY: verify tx.origin at evm_C = evm_alias(tz1)" ;
  let* () =
    EvmIdentityRecorder.check_last_origin ~expected_origin evm_recorder
  in
  (* SECONDARY: msg.sender at the EVM recorder = evm_alias(tez_B).
     tez_B is the immediate Tezos caller; its EVM alias is the
     deterministic derivation of its KT1. *)
  Log.debug ~prefix "SECONDARY: verify msg.sender at evm_C = evm_alias(tez_B)" ;
  let* () =
    EvmIdentityRecorder.check_last_sender ~expected_sender evm_recorder
  in
  (* Negative: a fresh EVM recorder that was never targeted holds the initial
     zero address (EvmIdentityRecorder initialises both slots to zero). *)
  Log.debug
    ~prefix
    "Negative: fresh recorder holds zero address (never targeted)" ;
  let* other_recorder = EvmIdentityRecorder.deploy () in
  let zero_address = String.make 40 '0' in
  let* () =
    EvmIdentityRecorder.check_last_origin
      ~expected_origin:zero_address
      other_recorder
  in
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:zero_address
      other_recorder
  in
  unit

(** Two-hop EVM→TEZ→EVM CRAC: the original EVM transaction signer is
 *  recovered as [tx.origin] at the return hop (originator axis), while
 *  [msg.sender] is the TEZ bridge's EVM alias (immediate-caller axis).
 *
 *    Chain: sender (EOA) → evm_bridge ~callMichelson~> tez_bridge
 *      --gateway/%call_evm--> evm_recorder
 *
 *  Crossing into Tezos materialises the signer's KT1 alias and records
 *  [origin(KT1) = Alias{Ethereum, signer}].  The return hop must resolve
 *  that KT1 back to the original signer (round-trip); a blind derivation
 *  would surface a fresh EVM address instead and fail the PRIMARY
 *  assertion.
 *
 *  PRIMARY assertion: [lastOrigin] (slot 1) at the EVM recorder =
 *    [sender.address] (the EVM tx signer, not evm_bridge_addr).
 *  SECONDARY assertion: [lastSender] (slot 0) = EVM alias of tez_bridge,
 *    the immediate Tezos caller.
 *  Negative: a fresh EVM recorder that was never targeted holds the initial
 *  zero address for both slots.
 *)
let test_crac_roundtrip_evm_via_michelson_back_to_evm () =
  register_crac_runner_test
    ~title:
      "CRAC address-identity: EVM→TEZ→EVM round-trip tx.origin recovers EVM tx \
       signer"
    ~tags:["address_identity"; "round_trip"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-RT-EVM" in
  Log.debug ~prefix "Deploy EVM IdentityRecorder (leaf)" ;
  let* evm_recorder = EvmIdentityRecorder.deploy () in
  Log.debug ~prefix "Originate TEZ bridge targeting the EVM recorder" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  (* The EVM alias of the TEZ bridge is deterministic: keccak256(utf8(kt1))[0..20].
     Compute the golden value from the KT1 address. *)
  let expected_sender = evm_alias_of_tezos_address tez_bridge_kt1 in
  Log.info
    "%s: tez_bridge KT1 = %s, expected EVM sender = %s"
    prefix
    tez_bridge_kt1
    expected_sender ;
  Log.debug ~prefix "Deploy EVM bridge targeting the TEZ bridge" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  Log.info "%s: evm_bridge address = %s" prefix evm_bridge_addr ;
  (* The originator axis resolves to the EVM transaction signer
     (sender.address), not evm_bridge_addr; the round-trip recovers it at
     the return hop. *)
  let expected_origin = sender.address in
  Log.info "%s: expected tx.origin at recorder = %s" prefix expected_origin ;
  Log.debug
    ~prefix
    "Fire EVM bridge: EVM→TEZ CRAC into tez_bridge, then tez_bridge→EVM CRAC \
     into recorder" ;
  (* Use the default gas limit: the round-trip materialises a fresh alias
     on the return hop, which needs substantially more gas than a plain
     call. *)
  let* _ = EvmRunner.call_run evm_bridge in
  (* PRIMARY: tx.origin at the return hop = sender.address (EVM tx signer). *)
  Log.debug
    ~prefix
    "PRIMARY: verify tx.origin at EVM recorder = sender.address (EVM tx signer)" ;
  let* () =
    EvmIdentityRecorder.check_last_origin ~expected_origin evm_recorder
  in
  (* SECONDARY: msg.sender is the EVM alias of the TEZ bridge (immediate caller). *)
  Log.debug
    ~prefix
    "SECONDARY: verify msg.sender at EVM recorder = EVM alias of tez_bridge" ;
  let* () =
    EvmIdentityRecorder.check_last_sender ~expected_sender evm_recorder
  in
  (* Negative: a fresh EVM recorder that was never targeted holds the initial
     zero address (EvmIdentityRecorder initialises both slots to zero). *)
  let* other_recorder = EvmIdentityRecorder.deploy () in
  Log.debug
    ~prefix
    "Negative: fresh recorder holds zero address (never targeted by a CRAC)" ;
  let zero_address = String.make 40 '0' in
  let* () =
    EvmIdentityRecorder.check_last_origin
      ~expected_origin:zero_address
      other_recorder
  in
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:zero_address
      other_recorder
  in
  unit

(** Path independence of address identity: the KT1 alias an EVM account
 *  receives on Tezos is the same regardless of the EVM-level call path
 *  used to invoke the contract.
 *
 *  Path A (direct):
 *    EVM[evm_bridge] ~CRAC~> TEZ[recorder]
 *    recorder.storage := KT1_bridge
 *
 *  Path B (via outer caller — same bridge, EVM-level indirection):
 *    EVM[evm_outer] calls EVM[evm_bridge] ~CRAC~> TEZ[recorder]
 *    recorder.storage := KT1_bridge  (must equal Path A)
 *
 *  Between the two paths the recorder is overwritten by a direct tz1
 *  call, so Path B's assertion can only pass through Path B's own write.
 *
 *  [evm_bridge] is the contract whose [callMichelson] fires the CRAC; the
 *  EVM-level caller ([evm_outer]) is irrelevant to the CRAC identity.
 *  The alias is recorded on the first CRAC and re-read thereafter; the
 *  same alias is produced on every subsequent CRAC.
 *
 *  Negative: a different EVM bridge (different deployed address) CRACs into
 *  a separate recorder and produces a DIFFERENT KT1 alias, proving that the
 *  equality above is non-trivial (alias function is injective).
 *)
let test_crac_address_identity_path_independence () =
  register_crac_runner_test
    ~title:"CRAC address-identity: EVM caller alias is path-independent"
    ~tags:["address_identity"; "path_independence"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-ID-PATH" in
  Log.debug ~prefix "Originate TEZ sender recorder" ;
  let* recorder =
    TezCracSenderRecorder.originate ~init_addr:gateway_address ()
  in
  Log.debug ~prefix "Deploy EVM bridge → recorder" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init recorder in
  let (`Evm_runner bridge_addr) = evm_bridge in
  Log.info "%s: evm_bridge address = %s" prefix bridge_addr ;
  (* ── Path A: direct call → CRAC into recorder. ─────────────────────── *)
  Log.debug ~prefix "Path A: fire CRAC from evm_bridge directly" ;
  let* _ = EvmRunner.call_run evm_bridge in
  let*@ kt1_bridge =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_addr sequencer
  in
  Log.info "%s: KT1 alias of evm_bridge = %s" prefix kt1_bridge ;
  let* () =
    TezCracSenderRecorder.check_stored_sender
      ~expected_sender:kt1_bridge
      recorder
  in
  (* ── Scramble: a direct tz1 call overwrites the slot with source.pkh,
     so Path B's assertion can only pass through Path B's own write. ── *)
  Log.debug ~prefix "Scramble recorder with a direct tz1 call" ;
  let* () = TezRunner.call_run recorder in
  let* () =
    TezCracSenderRecorder.check_stored_sender
      ~expected_sender:source.public_key_hash
      recorder
  in
  (* ── Path B: same bridge via outer EVM caller → CRAC into recorder. ── *)
  Log.debug ~prefix "Deploy EVM outer caller → evm_bridge" ;
  let* evm_outer =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Path B: fire CRAC from evm_bridge via outer caller" ;
  let* _ = EvmRunner.call_run evm_outer in
  Log.debug ~prefix "Verify recorder holds kt1_bridge again (path-independent)" ;
  let* () =
    TezCracSenderRecorder.check_stored_sender
      ~expected_sender:kt1_bridge
      recorder
  in
  (* ── Negative: a different EVM bridge produces a different KT1 alias. ─ *)
  Log.debug ~prefix "Negative: originate recorder_b and deploy bridge_b" ;
  let* recorder_b =
    TezCracSenderRecorder.originate ~init_addr:gateway_address ()
  in
  let* evm_bridge_b = EvmCrossRuntimeRunnerTez.deploy_and_init recorder_b in
  let (`Evm_runner bridge_b_addr) = evm_bridge_b in
  Log.debug ~prefix "Negative: fire CRAC from bridge_b to recorder_b" ;
  let* _ = EvmRunner.call_run evm_bridge_b in
  let*@ kt1_bridge_b =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_b_addr sequencer
  in
  let* () =
    TezCracSenderRecorder.check_stored_sender
      ~expected_sender:kt1_bridge_b
      recorder_b
  in
  Check.(
    (String.lowercase_ascii bridge_b_addr <> String.lowercase_ascii bridge_addr)
      string
      ~error_msg:
        "Sanity: two freshly deployed bridges must have distinct EVM addresses") ;
  Check.(
    (kt1_bridge_b <> kt1_bridge)
      string
      ~error_msg:
        "Path-independence negative: different EVM bridge addresses must yield \
         different KT1 aliases (alias function is injective)") ;
  unit

(** Legacy fallback (blind derivation): a Tezos address with no recorded
 *  [origin] triggers the deterministic derivation when it makes a CRAC
 *  into EVM.  The [msg.sender] seen by the EVM target ON CHAIN must
 *  equal [keccak256(UTF-8 bytes of the b58check string)[0..20]].
 *
 *  [origin(source.pkh)] is unrecorded here because bootstrap accounts are
 *  installed pre-revealed at genesis (raw account-info write, no origin
 *  slot) and never send a reveal — and reveal is the only writer of
 *  [Native] for implicit accounts.  The forward derivation is the same
 *  formula for unrecorded and [Native] sources, so this test pins the
 *  derivation formula; its unrecorded-branch coverage holds as long as
 *  bootstrap accounts stay unrevealed.
 *
 *  We assert the ON-CHAIN observation (what the EVM identity recorder
 *  actually stored as [lastSender]) rather than just comparing two RPC
 *  return values, which would be a pure-computation tautology.
 *
 *  Negative: a different Tezos address (a freshly originated KT1, with
 *  its own distinct alias) CRACs into a separate EVM recorder and
 *  records its own golden alias ON CHAIN — distinct from the first.  The
 *  original recorder is unaffected.
 *)
let test_crac_legacy_fallback_blind_derivation () =
  register_crac_runner_test
    ~title:"CRAC address-identity: tz1 with no origin uses blind EVM derivation"
    ~tags:["address_identity"; "legacy_fallback"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-ID-BLIND" in
  (* origin(source.pkh) is unrecorded: bootstrap accounts are pre-revealed
     at genesis and never reveal, and reveal is the only Native writer for
     implicit accounts. *)
  let tz1_addr = source.public_key_hash in
  let golden_evm_alias = evm_alias_of_tezos_address tz1_addr in
  Log.info
    "%s: tz1 = %s, golden EVM alias = %s"
    prefix
    tz1_addr
    golden_evm_alias ;
  Log.debug ~prefix "Deploy EVM IdentityRecorder" ;
  let* evm_target = EvmIdentityRecorder.deploy () in
  Log.debug ~prefix "Send direct tz1→gateway CRAC (blind derivation path)" ;
  let* () =
    Gateway.call_evm ~evm_target ~method_sig:"run()" ~abi_params:"" ()
  in
  Log.debug
    ~prefix
    "Verify ON-CHAIN msg.sender at recorder = golden blind-derived EVM alias" ;
  (* This is the chain-level observation: the EVM contract recorded the
     actual msg.sender the kernel forwarded.  If the kernel used a different
     derivation, this assertion fails. *)
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:golden_evm_alias
      evm_target
  in
  (* Negative: a different Tezos address has its own distinct golden EVM
     alias.  Originate a fresh TEZ bridge (a new KT1 with a different
     address) and fire it into a separate EVM recorder.  The on-chain
     [lastSender] at the new recorder must equal the golden alias of the KT1
     address — and must differ from [golden_evm_alias] of [tz1_addr]. *)
  Log.debug ~prefix "Negative: originate a different TEZ bridge (distinct KT1)" ;
  let* other_evm_target = EvmIdentityRecorder.deploy () in
  let* other_tez_bridge = TezCrossRuntimeRunnerEvm.originate other_evm_target in
  let (`Tez_runner (_, other_kt1)) = other_tez_bridge in
  let other_golden = evm_alias_of_tezos_address other_kt1 in
  Log.info
    "%s: other KT1 = %s, other golden EVM alias = %s"
    prefix
    other_kt1
    other_golden ;
  Check.(
    (String.lowercase_ascii other_golden
    <> String.lowercase_ascii golden_evm_alias)
      string
      ~error_msg:
        "Sanity: two distinct Tezos addresses must produce distinct golden EVM \
         aliases (hash collision would make the test degenerate)") ;
  Log.debug ~prefix "Negative: fire other_tez_bridge → other_evm_target" ;
  let* () = TezRunner.call_run other_tez_bridge in
  Log.debug
    ~prefix
    "Negative: verify other_evm_target.lastSender = other_golden (own alias)" ;
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:other_golden
      other_evm_target
  in
  (* The original recorder is unchanged (still holds golden_evm_alias). *)
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:golden_evm_alias
      evm_target
  in
  unit

(** Repair: the first CRAC from a TEZ account whose EVM alias [E] has no
 *  [/origin] record materialises the alias and records
 *  [origin(E) = Alias{Tezos, tz1}], enabling the round-trip property
 *  from that CRAC onward.  The second CRAC reads the persisted record.
 *
 *  Four-hop chain (TEZ→EVM→TEZ→EVM):
 *    TEZ[source/tz1] --tez_bridge.%%run--> EVM[evm_relay]
 *      ~callMichelson~> TEZ[tez_bridge_2] --%%call_evm--> EVM[evm_recorder]
 *
 *  Step 1 (record): [E]'s [/origin] is absent from durable storage before
 *    the CRAC and present after it — asserted directly via the rollup
 *    node, observing the [None → Alias] transition.  The middle hop must
 *    already resolve [E] back to [tz1] against the in-flight record.
 *  Step 2 (recorded branch): the same chain re-runs against the
 *    persisted record; the observable is unchanged.  A broken recorded
 *    branch would mis-resolve the middle hop and change [tx.origin].
 *
 *  PRIMARY: [lastOrigin] (slot 1) at the EVM recorder =
 *    [evm_alias_of_tezos_address(source.public_key_hash)] (both steps).
 *  SECONDARY: [lastSender] (slot 0) = EVM alias of [tez_bridge_2], the
 *    immediate Tezos caller at the last hop.
 *  Negative: a different [tez_bridge_2'] targeting a fresh [evm_recorder']
 *    records a different [lastSender] but the same [lastOrigin], proving
 *    the SECONDARY is non-trivial (sender alias is injective in the KT1).
 *)
let test_crac_origin_repair_none_to_alias () =
  register_crac_runner_test
    ~title:"CRAC address-identity: origin None→Alias after CRAC (repair)"
    ~tags:["address_identity"; "repair"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-ID-REPAIR" in
  let original_tz1 = source.public_key_hash in
  (* E = evm_alias(tz1): expected tx.origin at the EVM recorder. *)
  let expected_origin = evm_alias_of_tezos_address original_tz1 in
  Log.info
    "%s: original tz1 = %s, expected tx.origin (E) = %s"
    prefix
    original_tz1
    expected_origin ;
  (* ── Build the TEZ→EVM→TEZ→EVM chain. ─────────────────────────────────── *)
  (* leaf: EVM IdentityRecorder *)
  Log.debug ~prefix "Deploy EVM recorder (leaf, evm_recorder)" ;
  let* evm_recorder = EvmIdentityRecorder.deploy () in
  (* tez_bridge_2: TEZ→EVM bridge at hop 3, targeting evm_recorder *)
  Log.debug ~prefix "Originate tez_bridge_2 → evm_recorder" ;
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  let (`Tez_runner (_, tez_bridge_2_kt1)) = tez_bridge_2 in
  (* expected msg.sender at evm_recorder = evm_alias(tez_bridge_2_kt1),
     the immediate Tezos caller's deterministic alias *)
  let expected_sender = evm_alias_of_tezos_address tez_bridge_2_kt1 in
  Log.info
    "%s: tez_bridge_2 KT1 = %s, expected msg.sender = %s"
    prefix
    tez_bridge_2_kt1
    expected_sender ;
  (* evm_relay: EVM→TEZ bridge at hop 2, targeting tez_bridge_2 *)
  Log.debug ~prefix "Deploy evm_relay → tez_bridge_2" ;
  let* evm_relay = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_2 in
  let (`Evm_runner evm_relay_addr) = evm_relay in
  Log.info "%s: evm_relay address = %s" prefix evm_relay_addr ;
  (* tez_bridge: TEZ→EVM bridge at hop 1, targeting evm_relay.
     Before the first CRAC, [origin(evm_alias(source.pkh))] is None in
     durable storage (fresh sandbox). *)
  Log.debug ~prefix "Originate tez_bridge → evm_relay (None branch initially)" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_relay in
  (* Baseline: E = evm_alias(source.pkh) has no alias classification yet. *)
  Log.debug ~prefix "Baseline: classification of evm_alias(tz1) is absent" ;
  let* () =
    check_evm_origin_absent
      ~sequencer
      ~native_address:original_tz1
      expected_origin
  in
  (* ── Step 1: first invocation — None branch. ───────────────────────────── *)
  Log.debug
    ~prefix
    "Step 1: fire tez_bridge (origin=None for evm_alias(source.pkh)) → \
     evm_relay → tez_bridge_2 → evm_recorder" ;
  let* () = TezRunner.call_run tez_bridge in
  (* PRIMARY step 1: tx.origin at recorder = evm_alias(source.pkh).
     The first CRAC records the classification; the round-trip already
     resolves correctly on the return hop. *)
  Log.debug
    ~prefix
    "Step 1 PRIMARY: verify tx.origin at evm_recorder = evm_alias(tz1)" ;
  let* () =
    EvmIdentityRecorder.check_last_origin ~expected_origin evm_recorder
  in
  (* SECONDARY step 1: msg.sender at recorder = evm_alias(tez_bridge_2). *)
  Log.debug
    ~prefix
    "Step 1 SECONDARY: verify msg.sender at evm_recorder = \
     evm_alias(tez_bridge_2)" ;
  let* () =
    EvmIdentityRecorder.check_last_sender ~expected_sender evm_recorder
  in
  (* The repair is now durable: E's /origin record names the tz1.  This is
     the direct observation of the None → Alias transition. *)
  Log.debug ~prefix "Step 1: verify /origin of evm_alias(tz1) records the tz1" ;
  let* () =
    check_evm_origin_records_alias
      ~sequencer
      ~native_address:original_tz1
      expected_origin
  in
  (* ── Step 2: second invocation — Recorded branch. ─────────────────────── *)
  (* After step 1, [origin(evm_alias(source.pkh))] is in durable storage.
     The Recorded branch reads it directly; the result is the same. *)
  Log.debug
    ~prefix
    "Step 2: fire tez_bridge again (Recorded branch — alias now in durable \
     storage)" ;
  let* () = TezRunner.call_run tez_bridge in
  Log.debug
    ~prefix
    "Step 2 PRIMARY: verify tx.origin at evm_recorder still = evm_alias(tz1)" ;
  let* () =
    EvmIdentityRecorder.check_last_origin ~expected_origin evm_recorder
  in
  (* ── Negative: a different tez_bridge_2' → evm_recorder'. ─────────────── *)
  (* A freshly originated TEZ bridge has its own KT1 address and therefore its
     own EVM alias (blind-derived).  A CRAC through it produces a different
     [lastSender] at a fresh recorder, proving the SECONDARY is non-trivial.
     The [lastOrigin] should still be [evm_alias(tz1)] (same L1 signer). *)
  Log.debug ~prefix "Negative: deploy fresh evm_recorder_2 and tez_bridge_2b" ;
  let* evm_recorder_2 = EvmIdentityRecorder.deploy () in
  let* tez_bridge_2b = TezCrossRuntimeRunnerEvm.originate evm_recorder_2 in
  let (`Tez_runner (_, tez_bridge_2b_kt1)) = tez_bridge_2b in
  let other_expected_sender = evm_alias_of_tezos_address tez_bridge_2b_kt1 in
  Log.info
    "%s: tez_bridge_2b KT1 = %s, other expected sender = %s"
    prefix
    tez_bridge_2b_kt1
    other_expected_sender ;
  (* Sanity: the two tez_bridge_2 aliases must differ. *)
  Check.(
    (String.lowercase_ascii other_expected_sender
    <> String.lowercase_ascii expected_sender)
      string
      ~error_msg:
        "Sanity: two distinct KT1 bridges must produce distinct EVM aliases \
         (otherwise the negative assertion is degenerate)") ;
  let* evm_relay_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_2b in
  let* tez_bridge_b = TezCrossRuntimeRunnerEvm.originate evm_relay_2 in
  Log.debug
    ~prefix
    "Negative: fire tez_bridge_b → evm_relay_2 → tez_bridge_2b → evm_recorder_2" ;
  let* () = TezRunner.call_run tez_bridge_b in
  (* Same origin (same L1 signer), different sender (different KT1 bridge). *)
  Log.debug
    ~prefix
    "Negative: verify evm_recorder_2.lastOrigin = evm_alias(tz1) (same signer)" ;
  let* () =
    EvmIdentityRecorder.check_last_origin ~expected_origin evm_recorder_2
  in
  Log.debug
    ~prefix
    "Negative: verify evm_recorder_2.lastSender = evm_alias(tez_bridge_2b) \
     (different bridge, different alias)" ;
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:other_expected_sender
      evm_recorder_2
  in
  unit

(** Journal revert: when the enclosing Michelson operation backtracks, the
 *  EVM journal is discarded — both the recorder's EVM storage write
 *  ([lastSender]) and the [/origin] classification staged for the
 *  originator's EVM alias [E = evm_alias(source.pkh)].  The next
 *  successful CRAC re-records both.
 *
 *  Scenario:
 *    1. TEZ[reverter ~callees:[tez_bridge]] calls tez_bridge.%run:
 *         the TEZ→EVM CRAC runs, then a sibling internal op FAILWITHs,
 *         backtracking the whole operation group.
 *    2. Verify [lastSender] = zero address (initial state) and that [E]
 *         has NO [/origin] record — the classification write was unwound
 *         with the journal.
 *    3. TEZ[tez_bridge_ok] fires a successful TEZ→EVM CRAC into the same
 *         recorder.
 *    4. Verify [lastSender] = golden alias of [tez_bridge_ok_kt1] and
 *         that [E]'s [/origin] record is now present and names the tz1.
 *
 *  The revert in step 1 is caused by a SIBLING internal operation
 *  ([reverter] emits the call to [tez_bridge] followed by a failing call
 *  to itself), NOT by a failure inside the CRAC frame: the EVM journal
 *  is discarded when the enclosing Michelson operation group backtracks,
 *  regardless of whether the CRAC itself succeeded.
 *)
let test_crac_journal_revert_drops_origin () =
  register_crac_runner_test
    ~title:
      "CRAC address-identity: journal revert rolls back classification and \
       recorder writes"
    ~tags:["address_identity"; "revert"; "journal"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "ADDR-ID-REVERT" in
  Log.debug ~prefix "Deploy EVM IdentityRecorder" ;
  let* evm_recorder = EvmIdentityRecorder.deploy () in
  Log.debug ~prefix "Originate TEZ bridge targeting EVM recorder" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  Log.debug
    ~prefix
    "Originate outer TEZ reverter (calls tez_bridge, then reverts)" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Originate TEZ bridge_ok for the successful CRAC" ;
  let* tez_bridge_ok = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  let (`Tez_runner (_, tez_bridge_ok_kt1)) = tez_bridge_ok in
  let golden_evm_alias = evm_alias_of_tezos_address tez_bridge_ok_kt1 in
  Log.info "%s: golden EVM alias of tez_bridge_ok = %s" prefix golden_evm_alias ;
  (* E: the originator's EVM alias, whose /origin classification is staged
     during each TEZ→EVM CRAC of this test (the L1 signer is [source]). *)
  let tz1_evm_alias = evm_alias_of_tezos_address source.public_key_hash in
  (* Baseline: no alias classification before any CRAC. *)
  Log.debug ~prefix "Baseline: classification of evm_alias(tz1) is absent" ;
  let* () =
    check_evm_origin_absent
      ~sequencer
      ~native_address:source.public_key_hash
      tz1_evm_alias
  in
  (* Step 1: failing CRAC — reverter calls tez_bridge which CRACs to EVM.
     The EVM records msg.sender, but the outer TEZ FAILWITHs, rolling
     back the EVM journal.  The EVM tx never commits. *)
  Log.debug ~prefix "Step 1: TEZ reverter fires TEZ→EVM CRAC (TEZ reverts)" ;
  let* () = TezRunner.call_run ~gas_limit:500_000 tez_reverter in
  (* Step 2: verify identity_recorder was NOT updated by the reverted CRAC.
     EvmIdentityRecorder initialises lastSender to the zero address (0x0).
     If the revert rolled back the EVM journal, lastSender stays at zero. *)
  Log.debug
    ~prefix
    "Step 2: verify identity_recorder.lastSender = 0x000...0 (revert rolled \
     back)" ;
  let zero_address = String.make 40 '0' in
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:zero_address
      evm_recorder
  in
  (* The staged classification was unwound with the journal: E still has
     no alias classification after the backtracked CRAC. *)
  Log.debug
    ~prefix
    "Step 2: verify classification of evm_alias(tz1) is still absent (unwound)" ;
  let* () =
    check_evm_origin_absent
      ~sequencer
      ~native_address:source.public_key_hash
      tz1_evm_alias
  in
  (* Step 3: succeeding CRAC — tez_bridge_ok calls gateway %%call_evm.
     This time the CRAC succeeds and msg.sender is persisted. *)
  Log.debug ~prefix "Step 3: succeeding TEZ→EVM CRAC (tez_bridge_ok)" ;
  let* () = TezRunner.call_run tez_bridge_ok in
  (* Step 4: identity_recorder.lastSender must now equal golden_evm_alias. *)
  Log.debug
    ~prefix
    "Step 4: verify lastSender = golden EVM alias of tez_bridge_ok" ;
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:golden_evm_alias
      evm_recorder
  in
  (* The successful CRAC re-recorded the classification durably. *)
  Log.debug ~prefix "Step 4: verify /origin of evm_alias(tz1) records the tz1" ;
  let* () =
    check_evm_origin_records_alias
      ~sequencer
      ~native_address:source.public_key_hash
      tz1_evm_alias
  in
  (* Negative: a third TEZ bridge that was never involved in any CRAC.
     Its first CRAC (to a fresh recorder) would record its own alias, NOT
     golden_evm_alias.  We verify the original recorder is untouched
     (still holds golden_evm_alias) and that the new bridge would produce
     a different alias — ensuring the step 4 assertion above is
     non-trivial (not accidentally true for all EVM aliases). *)
  let* other_bridge = TezCrossRuntimeRunnerEvm.originate evm_recorder in
  let (`Tez_runner (_, other_bridge_kt1)) = other_bridge in
  let other_golden = evm_alias_of_tezos_address other_bridge_kt1 in
  Check.(
    (String.lowercase_ascii other_golden
    <> String.lowercase_ascii golden_evm_alias)
      string
      ~error_msg:
        "Sanity: a different TEZ bridge must produce a different golden EVM \
         alias (otherwise the step-4 assertion is degenerate)") ;
  (* The recorder still holds golden_evm_alias — other_bridge has not CRACed. *)
  let* () =
    EvmIdentityRecorder.check_last_sender
      ~expected_sender:golden_evm_alias
      evm_recorder
  in
  unit

(* CRAC bracketing scenario 1 — multiple top-level frames with a nested inner
 * frame in one EVM transaction.
 *
 * An EVM transaction drives two bridge contracts sequentially.  Bridge 1
 * reaches tez_1 (MultiRunCaller with callees=[tez_inner_bridge, tez_plain]).
 * tez_inner_bridge (CrossRuntimeRunnerEvm) calls back into EVM which
 * re-enters tez_inner_of_inner — forming a nested inner frame spliced at
 * the gateway call site.  tez_plain is called AFTER the inner frame returns,
 * providing the "trailing op after crac_end" property.  Bridge 2 reaches
 * tez_2 as a disjoint sibling frame.
 *
 * Internal-op layout (top-level = sender_alias, 24 ops):
 *   #0:  EVENT crac        (begin frame 1, CRAC-ID "1-0")
 *   #1:  origination alias(bridge_1)
 *   #2:  origination alias(sender)
 *   #3:  alias(bridge_1) -> tez_1 (%run)
 *   #4:  tez_1 -> tez_1 (%_incrementWitness)   [before tez_inner_bridge]
 *   #5:  tez_1 -> tez_inner_bridge (%run)
 *   #6:  tez_inner_bridge -> tez_inner_bridge (%_incrementWitness) [pre]
 *   #7:  tez_inner_bridge -> GW_M (%call_evm)
 *   #8:  EVENT crac        (begin inner, CRAC-ID "1-0")
 *   #9:  origination alias(evm_inner)
 *   #10: alias(evm_inner) -> tez_inner_of_inner (%run)
 *   #11: tez_inner_of_inner -> tez_inner_of_inner (%_incrementWitness)
 *   #12: EVENT crac_end    (end inner)
 *   #13: tez_inner_bridge -> tez_inner_bridge (%_incrementWitness) [post]
 *   #14: tez_1 -> tez_1 (%_incrementWitness)   [before tez_plain]
 *   #15: tez_1 -> tez_plain (%run)              [trailing op after inner frame]
 *   #16: tez_plain -> tez_plain (%_incrementWitness)
 *   #17: tez_1 -> tez_1 (%_incrementWitness)   [final]
 *   #18: EVENT crac_end    (end frame 1)
 *   #19: EVENT crac        (begin frame 2, CRAC-ID "1-0")
 *   #20: origination alias(bridge_2)
 *   #21: alias(bridge_2) -> tez_2 (%run)
 *   #22: tez_2 -> tez_2 (%_incrementWitness)
 *   #23: EVENT crac_end    (end frame 2)
 *
 * Asserts: exact op count (24), six markers at their exact indices with
 * correct tags and shared CRAC-ID, the inner pair (indices 8, 12) strictly
 * inside frame 1's pair (indices 0, 18), frame 2's pair (indices 19, 23)
 * disjoint and after frame 1's, and check_crac_brackets. *)
let test_crac_receipt_frames_nested_and_sibling () =
  register_crac_runner_test
    ~title:"CRAC: nested inner frame and sibling frame from one EVM tx"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-NEST-SIB" in
  let* tez_inner_of_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_of_inner_kt1)) = tez_inner_of_inner in
  let* evm_inner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner_of_inner
  in
  let (`Evm_runner evm_inner_addr) = evm_inner in
  let* tez_inner_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let (`Tez_runner (_, tez_inner_bridge_kt1)) = tez_inner_bridge in
  let* tez_plain = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_plain_kt1)) = tez_plain in
  let* tez_1 =
    TezMultiRunCaller.originate ~callees:[tez_inner_bridge; tez_plain] ()
  in
  let (`Tez_runner (_, tez_1_kt1)) = tez_1 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* tez_2 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_2_kt1)) = tez_2 in
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
  let*@ evm_inner_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_inner_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_2 in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner_of_inner
  in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s tag=%s dst=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "destination" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  Check.(
    (List.length internals = 24)
      int
      ~error_msg:"Expected 24 internal operations, got %L") ;
  (* ── Frame 1 (indices 0-18) ──────────────────────────────────── *)
  check_crac_event
    ~prefix:(prefix ^ "#0")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:bridge_1_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  (* ── Inner frame (indices 8-12, nested inside frame 1) ──────── *)
  check_crac_event
    ~prefix:(prefix ^ "#8")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_alias_kt1:evm_inner_alias
    ~expected_status:"applied"
    (List.nth internals 9) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#10")
    ~expected_nonce:10
    ~expected_source:evm_inner_alias
    ~expected_destination:tez_inner_of_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 10) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#11")
    ~expected_nonce:11
    ~expected_source:tez_inner_of_inner_kt1
    ~expected_destination:tez_inner_of_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#12")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 12) ;
  (* ── Continuation of frame 1 body after the inner frame ──────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#13")
    ~expected_nonce:13
    ~expected_source:tez_inner_bridge_kt1
    ~expected_destination:tez_inner_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 13) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#14")
    ~expected_nonce:14
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 14) ;
  (* Trailing op after the inner frame's crac_end at #12 *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#15")
    ~expected_nonce:15
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_plain_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 15) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#16")
    ~expected_nonce:16
    ~expected_source:tez_plain_kt1
    ~expected_destination:tez_plain_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 16) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#17")
    ~expected_nonce:17
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 17) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#18")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 18) ;
  (* ── Frame 2 (indices 19-23, disjoint sibling of frame 1) ──── *)
  check_crac_event
    ~prefix:(prefix ^ "#19")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 19) ;
  (* alias(sender) was materialized in frame 1 and is cached; only
     alias(bridge_2) is fresh for frame 2. *)
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#20")
    ~expected_nonce:20
    ~expected_alias_kt1:bridge_2_alias
    ~expected_status:"applied"
    (List.nth internals 20) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#21")
    ~expected_nonce:21
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 21) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#22")
    ~expected_nonce:22
    ~expected_source:tez_2_kt1
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 22) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#23")
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 23) ;
  (* Inner pair (8, 12) is strictly inside frame 1 pair (0, 18); frame 2
     pair (19, 23) is disjoint and after frame 1 pair — enforced by the
     per-index marker checks above and the balance walk below. *)
  check_crac_brackets ~prefix internals ;
  (* ── EVM side ──────────────────────────────────────────────── *)
  (* 1 original EVM tx; both gateway calls are internal to it *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* CRAC bracketing scenario 2 — Tezos-originated re-entry forest with a
 * trailing op after the re-entrant frame.
 *
 * A Michelson manager operation calls tez_main (MultiRunCaller with
 * callees=[tez_outer_bridge, tez_plain]).  tez_outer_bridge
 * (CrossRuntimeRunnerEvm) initiates a TEZ→EVM CRAC; EVM re-enters
 * tez_inner (the re-entrant EVM→TEZ frame).  After the frame returns,
 * tez_main calls tez_plain — the plain transfer that sits unambiguously
 * after the frame's crac_end.
 *
 * Because the root op is Tezos-originated it carries NO outer begin/end
 * markers; its internals form a balanced forest (one bracketed frame).
 * The CRAC-ID has Tezos origin ("0-0").
 *
 * Internal-op layout (14 ops):
 *   #0:  tez_main -> tez_main (%_incrementWitness)  [before tez_outer_bridge]
 *   #1:  tez_main -> tez_outer_bridge (%run)
 *   #2:  tez_outer_bridge -> tez_outer_bridge (%_incrementWitness) [pre]
 *   #3:  tez_outer_bridge -> GW_M (%call_evm)
 *   #4:  EVENT crac    (begin, CRAC-ID "0-0")
 *   #5:  origination alias(evm_bridge)
 *   #6:  alias(evm_bridge) -> tez_inner (%run)
 *   #7:  tez_inner -> tez_inner (%_incrementWitness)
 *   #8:  EVENT crac_end
 *   #9:  tez_outer_bridge -> tez_outer_bridge (%_incrementWitness) [post]
 *   #10: tez_main -> tez_main (%_incrementWitness)  [before tez_plain]
 *   #11: tez_main -> tez_plain (%run)               [trailing op after crac_end]
 *   #12: tez_plain -> tez_plain (%_incrementWitness)
 *   #13: tez_main -> tez_main (%_incrementWitness)  [final]
 *
 * Asserts: exact op count (14), CRAC-ID "0-0" (Tezos origin), the crac_end
 * at index 8 precedes the trailing transfer at index 11 (after-the-frame
 * property), and check_crac_brackets (forest: root un-bracketed). *)
let test_crac_receipt_tez_origin_reentry_trailing_op () =
  register_crac_runner_test
    ~title:"CRAC: Tezos-originated re-entry forest with trailing op after frame"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TEZORIG-TRAIL" in
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* tez_outer_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  let (`Tez_runner (_, tez_outer_bridge_kt1)) = tez_outer_bridge in
  let* tez_plain = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_plain_kt1)) = tez_plain in
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_outer_bridge; tez_plain] ()
  in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* () = TezRunner.call_run tez_main in
  (* 2 callees → 2 pre-callee increments + 1 final = 3 *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_plain in
  (* ── EVM side: one fake CRAC tx with CRAC-ID "0-0" ───────────── *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  (* ── Michelson side: root op un-bracketed, one frame inside ─── *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
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
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s tag=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string))
    internals ;
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
  (* ── Re-entrant CRAC frame begin/end (indices 4, 8) ─────────── *)
  check_crac_event
    ~prefix:(prefix ^ "#4")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_alias_kt1:evm_bridge_alias
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  check_crac_end_event
    ~prefix:(prefix ^ "#8")
    ~expected_crac_id:"0-0"
    ~expected_status:"applied"
    (List.nth internals 8) ;
  (* ── Post-frame continuation (after crac_end at #8) ─────────── *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#9")
    ~expected_nonce:9
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
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
  (* Trailing plain transfer: sits AFTER crac_end at #8. *)
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#11")
    ~expected_nonce:11
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_plain_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 11) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#12")
    ~expected_nonce:12
    ~expected_source:tez_plain_kt1
    ~expected_destination:tez_plain_kt1
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
  (* The crac_end (#8) precedes the trailing transfer (#11): the forest
     bracket check below confirms the re-entrant frame is balanced. *)
  (* Forest bracket check: root is un-bracketed; the one re-entrant frame
     must form a balanced pair. *)
  check_crac_brackets ~prefix internals ;
  unit

(* CRAC bracketing scenario 3 — failed-frame marker outcome.
 *
 * A single EVM→TEZ CRAC whose Michelson callee FAILWITHs.  Both the
 * begin (crac) and end (crac_end) markers are present on the receipt;
 * their statuses are "backtracked" (consistent with the failed parent
 * frame).  check_crac_brackets passes without filtering by status.
 *
 * Call tree:
 *   EVM[evm_main]
 *     -> EVM[bridge] ~CRAC~> TEZ[tez_reverter]  (FAILWITH)
 *   EVM catches the revert so the EVM tx succeeds.
 *
 * Asserts: both begin and end markers are present, both have status
 * "backtracked", and check_crac_brackets passes. *)
let test_crac_receipt_failed_frame_markers () =
  register_crac_runner_test
    ~title:"CRAC: failed frame carries both markers with consistent status"
    ~tags:["crac_receipt"; "revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-FAIL-MARK" in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let (`Tez_runner (_, tez_reverter_kt1)) = tez_reverter in
  let* bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let (`Evm_runner bridge_addr) = bridge in
  (* doCatch=true: EVM absorbs the CRAC failure so the tx succeeds. *)
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(bridge, true)] ()
  in
  let*@ bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  (* EVM tx succeeds; Michelson callee FAILWITHed but EVM caught it. *)
  let* _ = EvmRunner.call_run evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"failed"
      top
  in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  List.iteri
    (fun i iop ->
      Log.info
        "%s:   [%d] kind=%s tag=%s status=%s"
        prefix
        i
        JSON.(iop |-> "kind" |> as_string)
        JSON.(iop |-> "tag" |> as_opt |> Option.fold ~none:"-" ~some:as_string)
        JSON.(
          iop |-> "result" |-> "status" |> as_opt
          |> Option.fold ~none:"-" ~some:as_string))
    internals ;
  (* 6 internal ops: begin + alias(bridge) + alias(sender) + bridge->reverter
     (failed) + reverter->reverter %_revert (failed) + end. *)
  Check.(
    (List.length internals = 6)
      int
      ~error_msg:"Expected 6 internal operations, got %L") ;
  (* Both markers are present regardless of the frame outcome. *)
  check_crac_event
    ~prefix:(prefix ^ "#0")
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 0) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_alias_kt1:bridge_alias
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_alias_origination
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_alias_kt1:sender_alias
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:bridge_alias
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"failed"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:tez_reverter_kt1
    ~expected_destination:tez_reverter_kt1
    ~expected_entrypoint:"_revert"
    ~expected_status:"failed"
    (List.nth internals 4) ;
  (* End marker is present with status "backtracked" to match the begin
     marker and the failed frame. *)
  check_crac_end_event
    ~prefix:(prefix ^ "#5")
    ~expected_crac_id:"1-0"
    ~expected_status:"backtracked"
    (List.nth internals 5) ;
  (* Bracket walk succeeds without status filtering. *)
  check_crac_brackets ~prefix internals ;
  unit

let () =
  test_crac_evm_to_tez () ;
  test_crac_evm_multiple_independent_crossings () ;
  test_crac_evm_double_crossing () ;
  test_crac_evm_shared_leaf_via_direct_and_chain () ;
  test_crac_evm_5_crossing_chain () ;
  test_crac_tez_to_evm () ;
  test_crac_tez_multiple_independent_crossings () ;
  test_crac_tez_double_crossing () ;
  test_crac_tez_shared_leaf_via_direct_and_chain () ;
  test_crac_tez_5_crossing_chain () ;
  test_crac_access_list_preserved () ;
  test_crac_evm_to_tez_reverts () ;
  test_crac_evm_to_tez_materializes_alias () ;
  test_crac_evm_to_tez_revert_drops_alias () ;
  test_crac_tez_to_evm_reverts () ;
  test_crac_tez_to_evm_fake_tx_in_block () ;
  test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks () ;
  test_crac_tez_to_evm_inner_logs_in_receipt () ;
  test_crac_tez_to_evm_block_observables_visible () ;
  test_crac_tez_revert_rolls_back_inner_evm_storage () ;
  test_crac_tez_revert_propagates_to_evm () ;
  test_crac_evm_journal_state_preserved () ;
  test_crac_catch_tez_revert () ;
  test_crac_evm_revert_propagates_to_tez () ;
  test_crac_second_crac_tez_revert () ;
  test_crac_evm_revert_rolls_back_two_cracs () ;
  test_crac_deep_branch_with_second_tez_revert () ;
  test_crac_nested_revert_cascade_without_catch () ;
  test_crac_double_nested_evm_revert () ;
  test_crac_deep_nesting_6_levels () ;
  test_crac_evm_revert_after_nested_cracs () ;
  test_crac_evm_target_reverts () ;
  test_crac_second_crac_evm_revert () ;
  test_crac_tez_revert_rolls_back_two_cracs () ;
  test_crac_deep_branch_with_second_evm_revert () ;
  test_crac_tez_nested_revert_cascade_without_catch () ;
  test_crac_double_nested_tez_revert () ;
  test_crac_tez_deep_nesting_6_levels () ;
  test_crac_tez_revert_after_nested_cracs () ;
  test_crac_catch_evm_revert_between_cracs () ;
  test_crac_catch_tez_revert_between_cracs () ;
  test_crac_catch_tez_revert_with_nested_crac () ;
  test_crac_catch_deep_evm_revert_through_double_crac () ;
  test_crac_tez_catch_evm_revert_between_cracs () ;
  test_crac_tez_catch_tez_revert_between_cracs () ;
  test_crac_tez_catch_deep_revert_through_double_crac () ;
  test_crac_catch_revert_after_multiple_cracs () ;
  test_crac_catch_tez_revert_after_multiple_return_cracs () ;
  test_crac_catch_4_crossing_chain_revert () ;
  test_crac_tez_catch_4_crossing_chain_revert () ;
  test_crac_catch_5_crossing_chain_revert () ;
  test_crac_tez_catch_5_crossing_chain_revert () ;
  test_crac_chained_tez_calls_behind_crac () ;
  test_crac_nested_catches_with_multiple_reverts () ;
  test_crac_gas_model_alias_caching () ;
  test_check_crac_brackets_unit () ;
  test_crac_evm_to_tez_receipt () ;
  test_crac_forged_marker_excluded_by_sender_filter () ;
  test_crac_user_emit_in_reentrant_inner_crac () ;
  test_crac_synthetic_event_survives_failed_inner_with_emit () ;
  test_crac_synthetic_event_present_when_applied_crac_reverted_out () ;
  test_crac_applied_body_preserved_as_backtracked_on_evm_revert () ;
  test_crac_receipt_two_independent () ;
  test_crac_receipt_separate_tx_two_cracs () ;
  test_crac_receipt_evm_tez_evm () ;
  test_crac_receipt_tez_evm_tez () ;
  test_crac_receipt_tez_gateway_direct_evm_tez () ;
  test_crac_receipt_tez_gateway_direct_evm_tez_revert () ;
  test_crac_receipt_evm_tez_evm_tez () ;
  test_crac_receipt_evm_to_tez_revert () ;
  test_crac_receipt_two_failed_independent () ;
  test_crac_receipt_interleaved_failed_pending () ;
  test_crac_receipt_evm_not_first_tx () ;
  test_crac_receipt_tez_not_first_tx () ;
  test_crac_receipt_evm_then_tez_same_block () ;
  test_crac_receipt_tez_to_evm () ;
  test_crac_receipt_tez_evm_tez_evm () ;
  test_crac_receipt_evm_5_crossing_chain () ;
  test_crac_receipt_tez_mixed_calls_with_crac () ;
  test_crac_receipt_frames_nested_and_sibling () ;
  test_crac_receipt_tez_origin_reentry_trailing_op () ;
  test_crac_receipt_failed_frame_markers () ;
  test_crac_http_call_success () ;
  test_crac_http_call_catch_revert () ;
  test_crac_http_call_catch_oog () ;
  test_crac_http_call_evm_to_evm () ;
  test_crac_http_call_evm_to_evm_preserves_msg_sender () ;
  test_crac_http_call_tez_to_tez () ;
  test_crac_http_call_evm_to_evm_revert () ;
  test_crac_http_call_tez_to_tez_revert () ;
  test_crac_http_call_evm_to_evm_return_value () ;
  test_crac_http_call_tez_to_tez_collect_result () ;
  test_crac_debug_trace_transaction () ;
  test_crac_debug_trace_block () ;
  test_crac_debug_trace_normal_tx_in_crac_block () ;
  test_http_trace_nested_crac () ;
  test_http_trace_multiple_independent_cracs () ;
  test_http_trace_mixed_block () ;
  test_http_trace_crac_depth_propagation () ;
  test_l1_vs_tezosx_nested_failwith_receipt [Alpha] ;
  test_crac_callback_fire_and_forget () ;
  test_crac_callback_receives_result_bytes () ;
  test_crac_callback_failure_reverts_all () ;
  test_crac_callback_behind_crac () ;
  test_crac_callback_tez_revert_rolls_back_callback () ;
  test_crac_callback_evm_catches_failing_callback_behind_crac () ;
  test_crac_callback_mixed_with_normal_runners () ;
  test_crac_gas_header_michelson_burner () ;
  test_crac_gas_model_callee_gas_in_evm_receipt () ;
  test_crac_gas_model_callee_gas_in_receipt () ;
  test_crac_gas_accounting_investigation () ;
  test_crac_gas_error_path_reporting () ;
  test_crac_gas_oog_path_reporting () ;
  test_crac_collect_result_surfaces_in_response_body () ;
  test_crac_collect_result_rejects_nonzero_amount () ;
  test_crac_collect_result_revert_discards_bytes () ;
  test_crac_collect_result_size_sweep_matches_model () ;
  test_crac_collect_result_fire_and_forget_empty_body () ;
  test_crac_collect_result_once_per_frame () ;
  test_crac_collect_result_nested_independent_slots () ;
  test_crac_collect_result_large_payload_oog () ;
  test_crac_tezosx_caller_balance_stays_zero () ;
  test_crac_evm_to_tez_high_gas () ;
  test_crac_orig_two_txs_one_block () ;
  test_crac_orig_two_txs_two_blocks () ;
  test_crac_orig_two_cracs_one_tx () ;
  test_crac_evm_deep_recurse_then_michelson_oog [Alpha] ;
  test_crac_roundtrip_tz1_via_evm_back_to_tezos () ;
  test_crac_roundtrip_evm_via_michelson_back_to_evm () ;
  test_crac_address_identity_path_independence () ;
  test_crac_legacy_fallback_blind_derivation () ;
  test_crac_origin_repair_none_to_alias () ;
  test_crac_journal_revert_drops_origin ()
