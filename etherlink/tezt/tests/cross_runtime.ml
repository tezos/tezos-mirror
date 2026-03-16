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
    send_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      call_op
end

(** Contracts *)

(** Common [run()] caller for EVM contracts. *)
module EvmRunner = struct
  open EvmContract

  (** Calls [run()] on the given EVM contract. *)
  let call_run ?expected_status ~sequencer ~sender ~nonce ~value runner =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:runner
        ~abi_signature:"run()"
        ~arguments:[]
        ?expected_status
        ()
    in
    unit
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
end

(** Common [%run] caller for Tezos contracts. *)
module TezRunner = struct
  open TezContract

  (** Calls [%run] with [Unit] via the delayed inbox. *)
  let call_run ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ?amount runner =
    call_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ?amount
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
    let init_storage_data =
      let pp_bool fmt b =
        if b then Format.fprintf fmt "True" else Format.fprintf fmt "False"
      in
      let pp_semicolon fmt () = Format.fprintf fmt ";" in
      let pp_callee fmt = Format.fprintf fmt {|"%s"|} in
      Format.asprintf
        {|Pair 0 (Pair %a {%a})|}
        pp_bool
        revert
        (Format.pp_print_list ~pp_sep:pp_semicolon pp_callee)
        callees
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

let () = ()
