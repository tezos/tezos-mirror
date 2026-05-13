(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Tezlink
   Requirement:  make -f etherlink.mk build
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tezlink.ml
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Test_helpers
open Setup
open Delayed_inbox

let check_kernel_version ~evm_node ~equal expected =
  let*@ kernel_version = Rpc.tez_kernelVersion evm_node in
  if equal then
    Check.((kernel_version = expected) string)
      ~error_msg:"Expected kernelVersion to be %R, got %L"
  else
    Check.((kernel_version <> expected) string)
      ~error_msg:"Expected kernelVersion to be different than %R" ;
  return kernel_version

let register_tezosx_test ~title ~tags ?(kernels = [Kernel.Latest])
    ?bootstrap_accounts ?bootstrap_contracts ?genesis_timestamp
    ?(time_between_blocks = Evm_node.Nothing) ?additional_uses
    ?(wait_for_valid_block = true) ?max_blueprints_lag ?max_blueprints_catchup
    ?catchup_cooldown ?da_fee ?sequencer_pool_address
    ?michelson_to_evm_gas_multiplier ?enable_michelson_gas_refund scenario
    protocols =
  let scenario setup protocol =
    let* () =
      if wait_for_valid_block then
        match time_between_blocks with
        | Evm_node.Nothing ->
            (* Blocks are not produced automatically *)
            let* () = produce_block_and_wait_for ~sequencer:setup.sequencer 1 in
            produce_block_and_wait_for ~sequencer:setup.sequencer 2
        | Time_between_blocks _ ->
            Evm_node.wait_for_blueprint_applied setup.sequencer 2
      else return ()
    in
    scenario setup protocol
  in
  List.iter
    (fun kernel ->
      Setup.register_test
        ~__FILE__
        ~rpc_server:Evm_node.Resto
        ?tez_bootstrap_accounts:bootstrap_accounts
        ?tez_bootstrap_contracts:bootstrap_contracts
        ~michelson_runtime_chain_id:"NetXH12DFfBVHi4"
        ?genesis_timestamp
        ?max_blueprints_lag
        ?max_blueprints_catchup
        ?catchup_cooldown
        ~time_between_blocks
        ?additional_uses
        ?da_fee
        ?sequencer_pool_address
        ?michelson_to_evm_gas_multiplier
        ~kernel
        ~title
        ~tags:("tezlink" :: "tezosx" :: tags)
        ~with_runtimes:[Tezos]
        ~enable_dal:false
        ~instant_confirmations:false
        ?enable_michelson_gas_refund
        scenario
        protocols)
    kernels

let register_tezosx_upgrade_test ~title ~tags ~genesis_timestamp
    ?(kernels = Kernel.tezlink_all) ?(upgrade_to = Kernel.upgrade_to)
    ?(additional_uses = []) scenario protocols =
  List.iter
    (fun from ->
      let from_tag, _ = Kernel.to_uses_and_tags from in
      let to_ = upgrade_to from in
      let to_tag, to_use = Kernel.to_uses_and_tags to_ in
      register_tezosx_test
        ~kernels:[from]
        ~genesis_timestamp
        ~time_between_blocks:Nothing
        ~tags:("upgrade_scenario" :: to_tag :: tags)
        ~title:Format.(sprintf "%s (%s -> %s)" title from_tag to_tag)
        ~additional_uses:(to_use :: additional_uses)
          (* No need to wait in an upgrade scenario. *)
          (* If this boolean is set to true, upgrade test
             will fail because blueprint are produced at
             an unexpected timestamp. *)
        ~wait_for_valid_block:false
        (scenario from to_)
        protocols)
    kernels

let register_tezosx_regression_test ~title ~tags ?bootstrap_accounts
    ?bootstrap_contracts ?(time_between_blocks = Evm_node.Nothing) scenario =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:("tezlink" :: "tezosx" :: tags)
    ~title
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_client;
        Constant.WASM.evm_kernel;
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
      ])
  @@ fun protocol ->
  let l2_chain =
    {
      (Evm_node.default_l2_setup ~l2_chain_id:12) with
      tez_bootstrap_accounts = bootstrap_accounts;
      tez_bootstrap_contracts = bootstrap_contracts;
    }
  in
  let* setup =
    Setup.setup_sequencer
      ~enable_dal:false
      ~with_runtimes:[Tezos]
      ~l2_chain
      ~rpc_server:Evm_node.Resto
      ~time_between_blocks
      protocol
  in
  (* Produce block to skip block 1 *)
  let* () = produce_block_and_wait_for ~sequencer:setup.sequencer 1 in
  scenario setup protocol

let tezlink_foreign_endpoint_from_evm_node evm_node =
  let evm_node_endpoint = Evm_node.rpc_endpoint_record evm_node in
  {evm_node_endpoint with path = "/tezlink"}

let tezlink_endpoint_from_evm_node evm_node =
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node evm_node in
  Client.Foreign_endpoint tezlink_endpoint

let tezlink_client_from_evm_node evm_node =
  let endpoint = tezlink_endpoint_from_evm_node evm_node in
  Client.init ~endpoint

(** Helper to parse RPC response. Returns [Some json] if 200, [None] if 404,
    fails otherwise. *)
let parse_rpc_response ~origin response =
  match response.RPC_core.code with
  | 200 -> Lwt.return_some (JSON.parse ~origin response.body)
  | 404 -> Lwt.return_none
  | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code

let test_describe_endpoint =
  register_tezosx_regression_test
    ~tags:["evm"; "rpc"; "describe"]
    ~title:"Test the /describe endpoint"
  @@ fun {sequencer; client; _} _ ->
  let hooks = Tezos_regression.hooks in
  let sequencer_endpoint = Evm_node.rpc_endpoint_record sequencer in
  (* List all the endpoint of the sequencer *)
  let root_endpoint = Client.(Foreign_endpoint sequencer_endpoint) in
  let* (_ : string) = Client.rpc_list ~hooks ~endpoint:root_endpoint client in
  (* List the endpoints of the /tezlink directory *)
  let tezlink_endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* (_ : string) =
    Client.rpc_list ~hooks ~endpoint:tezlink_endpoint client
  in
  let* (_ : string) =
    Client.rpc_list
      ~hooks
      ~endpoint:tezlink_endpoint
      ~url:"chains/main/blocks/head"
      client
  in
  unit

let test_observer_starts =
  register_tezosx_test
    ~title:"Test michelson runtime observer does not crash at startup"
    ~tags:["observer"]
  @@ fun {sequencer; observer; _} _protocol ->
  let observer_promise = Evm_node.wait_for_blueprint_applied observer 3 in
  let* () =
    let*@ _ = Rpc.produce_block sequencer in
    unit
  and* () = observer_promise in
  unit

let test_current_level =
  register_tezosx_test
    ~title:"Test of the current_level rpc"
    ~tags:["rpc"; "current_level"]
  @@ fun {sequencer; _} _protocol ->
  (* call the current_level rpc and parse the result *)
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in

  let rpc_current_level ?offset block =
    let* response =
      RPC_core.call_json tezlink_endpoint
      @@ RPC.get_chain_block_helper_current_level ~block ?offset ()
    in
    return response.body
  in

  (* verify an answer by the current_level rpc *)
  let check_current_level res expected_level =
    (* cycle length is hardcoded for now it won't change for some time *)
    let cycle_length = 14400 in
    Check.(
      JSON.(res |-> "level" |> as_int = expected_level)
        int
        ~__LOC__
        ~error_msg:"Level: expected %R but got %L") ;
    Check.(
      JSON.(res |-> "level_position" |> as_int = expected_level - 1)
        int
        ~__LOC__
        ~error_msg:"Expected %R but got %L") ;
    Check.(
      JSON.(res |-> "cycle" |> as_int = expected_level / cycle_length)
        int
        ~__LOC__
        ~error_msg:"Cycle: expected %R but got %L") ;
    Check.(
      JSON.(
        res |-> "cycle_position" |> as_int
        = (expected_level - 1) mod cycle_length)
        int
        ~__LOC__
        ~error_msg:"Cycle_position: expected %R but got %L") ;
    Check.(
      JSON.(res |-> "expected_commitment" |> as_bool = false)
        bool
        ~__LOC__
        ~error_msg:"Expected_commitment: expected %R but got %L") ;
    unit
  in

  (* checks *)
  let* res = rpc_current_level "head" in
  let* () = check_current_level res 2 in
  (* test with offset *)
  let* res = rpc_current_level "head" ~offset:1 in
  let* () = check_current_level res 3 in
  (* test with offset larger than a cycle *)
  let* res = rpc_current_level "head" ~offset:40000 in
  let* () = check_current_level res 40002 in
  (* Bake 5 blocks and test that "head" is now at level 7. *)
  let* () =
    repeat 5 (fun () ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let* res = rpc_current_level "head" in
  let* () = check_current_level res 7 in
  (* Check with block parameter *)
  let* res = rpc_current_level "head~2" in
  let* () = check_current_level res 5 in
  let* res = rpc_current_level "head~2" ~offset:1 in
  let* () = check_current_level res 6 in
  (* test negative offset *)
  let* res = rpc_current_level "head" ~offset:(-1) in
  Check.(
    JSON.(
      res |> as_list |> List.hd |-> "msg" |> as_string
      = "The specified level offset should be positive.")
      string
      ~error_msg:"Should have failed: expected %R but got %L") ;
  (* test numeric block parameter *)
  let* res = rpc_current_level "5" ~offset:1 in
  let* () = check_current_level res 6 in
  let* res = rpc_current_level "genesis" in
  let* () = check_current_level res 0 in
  unit

let test_protocols =
  register_tezosx_test
    ~title:"Test of the protocols rpc"
    ~tags:["rpc"; "protocols"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let protocol_hash = Protocol.hash Michelson_contracts.tezlink_protocol in

  let* res =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_protocols ()
  in
  Check.(
    JSON.(res |-> "protocol" |> as_string = protocol_hash)
      string
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_genesis_block_arg =
  register_tezosx_test
    ~title:"Test of the genesis block argument"
    ~tags:["rpc"; "genesis"; "protocols"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let rpc_protocols block =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_protocols ~block ()
  in

  let* res_genesis = rpc_protocols "genesis" in
  let* res_0 = rpc_protocols "0" in
  Check.(
    JSON.(
      res_genesis |-> "protocol" |> as_string
      = (res_0 |-> "protocol" |> as_string))
      string
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_expected_issuance =
  register_tezosx_test
    ~title:"Test the mocked expected issuance rpc"
    ~tags:["rpc"; "issuance"; "mock"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let* res =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_issuance_expected_issuance ()
  in
  let rewards = JSON.(res |> as_list) in
  (* The mock assumes we stay in cycle 0 for now *)
  Check.(
    JSON.(List.hd rewards |-> "cycle" |> as_int = 0)
      int
      ~error_msg:"Expected %R but got %L") ;

  Check.(
    (List.length rewards = 3)
      int
      ~error_msg:"Expected %R (consensus_rights_delay + 1) but got %L") ;
  unit

let test_balance =
  register_tezosx_test
    ~title:"Test of the balance rpc"
    ~tags:["rpc"; "balance"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  (* call the balance rpc and parse the result *)
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* valid_res =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.(
    (Tez.to_mutez valid_res = 3800000000000)
      int
      ~error_msg:"Expected %R but got %L") ;

  let* invalid_res =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.((Tez.to_mutez invalid_res = 0) int ~error_msg:"Expected %R but got %L") ;
  unit

let test_storage_via_client =
  let contract = Michelson_contracts.concat_hello () in
  register_tezosx_test
    ~title:"Test of the storage rpc via client"
    ~tags:["rpc"; "storage"]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* storage = Client.contract_storage ~endpoint contract.address client in
  Check.(
    (String.trim storage = String.trim contract.initial_storage)
      string
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;
  unit

let test_contract_info =
  register_tezosx_test
    ~title:"Test of the contract info rpc"
    ~tags:["rpc"; "contract"; "info"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  (* call the contract info rpc and check the result *)
  let* valid_info =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract
         ~id:Constant.bootstrap1.Account.public_key_hash
         ()
  in
  let balance = JSON.(valid_info |-> "balance" |> as_int) in
  let counter = JSON.(valid_info |-> "counter" |> as_int) in

  Check.((balance = 3800000000000) int ~error_msg:"Expected %R but got %L") ;
  Check.((counter = 0) int ~error_msg:"Expected %R but got %L") ;
  unit

let test_list_entrypoints =
  let contract = Michelson_contracts.faucet_contract () in
  register_tezosx_test
    ~title:"Test of the contract entrypoint list"
    ~tags:["rpc"; "contract"; "info"; "list_entrypoints"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  (* call the list_entrypoint rpc and check the result *)
  let* entrypoints =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_entrypoints ~id:contract.address ()
  in
  let expected =
    JSON.parse
      ~origin:"expected_entrypoints"
      "{\"entrypoints\":{\"fund\":{\"prim\":\"mutez\"}}}"
  in
  Check.((entrypoints = expected) json ~error_msg:"Expected %R but got %L") ;
  unit

let test_contract_info_script =
  let contract = Michelson_contracts.concat_hello () in
  register_tezosx_test
    ~title:"Test of the contract info rpc on smart contracts"
    ~tags:["rpc"; "contract"; "info"; "script"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  (* call the contract info rpc and check the result *)
  let* valid_info =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract ~id:contract.address ()
  in
  let balance = JSON.(valid_info |-> "balance" |> as_int) in
  let counter = JSON.(valid_info |-> "counter" |> as_int_opt) in
  let script = JSON.(valid_info |-> "script") in
  let storage = JSON.(script |-> "storage" |> as_list) in
  let code = JSON.(script |-> "code" |> as_list) in

  Check.((balance = 3800000000000) int ~error_msg:"Expected %R but got %L") ;
  Check.((counter = None) (option int) ~error_msg:"Expected %R but got %L") ;
  Check.(
    (List.length storage = 1)
      int
      ~error_msg:"Expected storage with %R element but got %L") ;
  Check.(
    (List.length code = 3)
      int
      ~error_msg:"Expected code with %R elements but got %L") ;
  unit

let test_manager_key =
  register_tezosx_test
    ~title:"Test of the manager_key rpc"
    ~tags:["rpc"; "manager_key"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let get_contract_manager_key account =
    let* res =
      RPC_core.call_json tezlink_endpoint
      @@ RPC.get_chain_block_context_contract_manager_key
           ~id:account.Account.public_key_hash
           ()
    in
    return res.body
  in
  let* valid_res = get_contract_manager_key Constant.bootstrap1 in
  Check.(
    JSON.(valid_res |> as_string_opt = Some Constant.bootstrap1.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  let* invalid_res = get_contract_manager_key Constant.bootstrap2 in
  Check.(
    JSON.(invalid_res |> as_string_opt = None)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_counter =
  register_tezosx_test
    ~title:"Test of the counter rpc"
    ~tags:["evm"; "rpc"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let get_contract_counter account =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_counter
         ~id:account.Account.public_key_hash
         ()
  in
  let get_contract_info account =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract
         ~id:account.Account.public_key_hash
         ()
  in
  let* res = get_contract_counter Constant.bootstrap1 in
  Check.(JSON.(res |> as_int = 0) int ~error_msg:"Expected %R but got %L") ;
  let* res = get_contract_info Constant.bootstrap1 in
  Check.(
    JSON.(res |-> "counter" |> as_int = 0)
      int
      ~error_msg:"Expected %R but got %L") ;
  let* res = get_contract_counter Constant.bootstrap2 in
  Check.(JSON.(res |> as_int = 0) int ~error_msg:"Expected %R but got %L") ;
  let* res = get_contract_info Constant.bootstrap2 in
  Check.(
    JSON.(res |-> "counter" |> as_int = 0)
      int
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_version =
  register_tezosx_test ~title:"Test of the version rpc" ~tags:["rpc"; "version"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let* res = RPC_core.call tezlink_endpoint RPC.get_version in
  Check.(
    JSON.(res |-> "version" |-> "major" |> as_int = 0)
      int
      ~error_msg:"Expected version %R but got %L") ;
  unit

let test_constants =
  Protocol.register_regression_test
    ~title:"Test of the constants rpc"
    ~tags:["tezlink"; "rpc"; "constants"]
    ~__FILE__
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_client;
        Constant.WASM.evm_kernel;
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
      ])
  @@ fun protocol ->
  let* {sequencer; client; _} =
    Setup.setup_sequencer
      ~enable_dal:false
      ~with_runtimes:[Tezos]
      ~rpc_server:Evm_node.Resto
      protocol
  in
  let hooks = Tezos_regression.hooks in
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* _ =
    Client.RPC.call ~hooks ~endpoint client
    @@ RPC.get_chain_block_context_constants ()
  in
  unit

let test_storage_rpc =
  register_tezosx_test
    ~title:"Test of the /storage rpc"
    ~tags:["rpc"; "storage"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let foreign_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.(Foreign_endpoint foreign_endpoint) in
  (* Helper to get storage of a contract. Returns None if 404. *)
  let storage_rpc contract =
    let* response =
      RPC_core.call_raw foreign_endpoint
      @@ RPC.get_chain_block_context_contract_storage ~id:contract ()
    in
    parse_rpc_response ~origin:"storage_rpc" response
  in
  (* 1. Storage of a non-existent KT1 should return 404 *)
  let fake_kt1 = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" in
  let* storage = storage_rpc fake_kt1 in
  Check.(
    (storage = None)
      (option json)
      ~error_msg:"Expected None for non-existent KT1, got %L") ;

  (* 2. Storage of an implicit account (tz1) should return 404 *)
  let* storage = storage_rpc Constant.bootstrap1.public_key_hash in
  Check.(
    (storage = None)
      (option json)
      ~error_msg:"Expected None for implicit account, got %L") ;

  (* 3. Storage of a non-existent tz1 should return 404 *)
  let fake_tz1 = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let* storage = storage_rpc fake_tz1 in
  Check.(
    (storage = None)
      (option json)
      ~error_msg:"Expected None for non-existent tz1, got %L") ;

  (* 4. Storage of an originated contract should return its storage *)
  let concat_hello = Tezt_etherlink.Michelson_contracts.concat_hello () in
  let* contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias:"concat_hello"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:concat_hello.initial_storage
      ~prg:concat_hello.path
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in

  let* storage = storage_rpc contract in
  Check.(
    (storage = Some (JSON.parse ~origin:"/storage" "[{\"string\":\"initial\"}]"))
      (option json)
      ~error_msg:"Expected Some %R for originated contract storage, got %L.") ;

  unit

let test_chain_id =
  register_tezosx_test
    ~title:"Test of the chain_id rpc"
    ~tags:["rpc"; "chain_id"]
  @@ fun {sequencer; client; l2_chain; _} _protocol ->
  let expected_chain_id = l2_chain.l2_chain_id in
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* chain_id =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_chain_id ()
  in
  check_chain_id ~expected_chain_id ~chain_id ;
  unit

let test_contracts_rpc =
  register_tezosx_test
    ~title:"Test of the contracts rpc"
    ~tags:["rpc"; "contracts"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Helper to get contracts as a list of strings. *)
  let contracts_rpc () =
    let* contracts =
      Client.RPC.call ~hooks ~endpoint client
      @@ RPC.get_chain_block_context_contracts ()
    in
    Lwt.return JSON.(as_list contracts |> List.map as_string)
  in
  (* Helper to deploy a faucet contract. *)
  let deploy_contract ~id ~initial_balance =
    let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
    let* contract =
      Client.originate_contract
        ~endpoint
        ~amount:(Tez.of_int initial_balance)
        ~alias:(Format.sprintf "faucet_%d" id)
        ~src:Constant.bootstrap1.public_key_hash
        ~init:faucet.initial_storage
        ~prg:faucet.path
        ~burn_cap:Tez.one
        client
    in
    let*@ _ = produce_block sequencer in
    return contract
  in
  (* Helper to check that two list of contracts are equal. *)
  let check_contracts_eq expected actual =
    let sort = List.fast_sort String.compare in
    Check.(
      (sort expected = sort actual)
        (list string)
        ~error_msg:"Expected contracts %L, got %R")
  in
  (* Helper to check that the balance of a given account is as expected. *)
  let check_balance account expected =
    let* balance =
      Client.get_balance_for
        ~endpoint
        ~account:account.Account.public_key_hash
        client
    in
    Tez.(
      Check.(
        (balance = of_int expected)
          typ
          ~error_msg:"Wrong balance: expected %R, got %L")) ;
    unit
  in
  (* 1. Contracts & funded implicit accounts appear in /contracts RPC result's *)
  let* contracts_before = contracts_rpc () in
  let* fresh_account = Client.gen_and_show_keys client in
  let*@ _ = produce_block sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.alias
      ~receiver:fresh_account.alias
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* faucet1 = deploy_contract ~id:1 ~initial_balance:0 in
  let* faucet2 = deploy_contract ~id:2 ~initial_balance:1 in

  let expected_contracts =
    fresh_account.public_key_hash :: faucet1 :: faucet2 :: contracts_before
  in
  let* contracts_after = contracts_rpc () in
  check_contracts_eq expected_contracts contracts_after ;

  (* 2. Empty implicit accounts remain in /contracts RPC result.
     Unlike L1, Tezlink does not deallocate implicit accounts when their
     balance reaches zero. *)
  let*! () =
    Client.reveal ~endpoint ~fee:(Tez.of_int 1) ~src:fresh_account.alias client
  in
  let*@ _ = produce_block sequencer in
  let* () = check_balance fresh_account 9 in

  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 8)
      ~fee:(Tez.of_int 1)
      ~giver:fresh_account.alias
      ~receiver:Constant.bootstrap1.alias
      client
  in
  let*@ _ = produce_block sequencer in
  let* () = check_balance fresh_account 0 in

  let* contracts_final = contracts_rpc () in
  check_contracts_eq expected_contracts contracts_final ;
  unit

let test_header =
  register_tezosx_test
    ~title:"Test of the header rpc"
    ~tags:["rpc"; "header"; "offset"]
  @@ fun {sequencer; client; l2_chain; _} _protocol ->
  let chain_id = Some l2_chain.l2_chain_id in

  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* () = produce_block_and_wait_for ~sequencer 3 in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let* () =
    produce_block_and_wait_for ~timestamp:current_timestamp ~sequencer 4
  in
  let* block_1 =
    Client.RPC.call ~hooks ~endpoint client
    @@ RPC.get_chain_block_header ~block:"head~1" ()
  in
  let* block_2 =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block_header ()
  in

  return
  @@ check_header
       ~previous_header:block_1
       ~current_header:block_2
       ~chain_id
       ~current_timestamp:(Some current_timestamp)

let test_block_metadata =
  register_tezosx_test
    ~title:"Test of the metadata rpc"
    ~tags:["rpc"; "metadata"; "offset"]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let check_current_block_metadata () =
    let* block_metadata_raw =
      Client.RPC.call ~hooks ~endpoint client
      @@ RPC.get_chain_block_metadata_raw ()
    in
    let* block_data =
      Client.RPC.call ~hooks ~endpoint client
      @@ RPC.get_chain_block ~force_metadata:true ~metadata:`Always ()
    in
    Check.(
      JSON.(block_metadata_raw = (block_data |-> "metadata"))
        json
        ~error_msg:"Check block metadata failed, Expected %R but got %L") ;
    unit
  in
  let* () = check_current_block_metadata () in
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let* () = check_current_block_metadata () in
  let* () =
    produce_block_and_wait_for ~timestamp:current_timestamp ~sequencer 4
  in
  check_current_block_metadata ()

let test_block_info =
  register_tezosx_test
    ~title:"Test of the block_info rpc"
    ~tags:["rpc"; "block_info"]
  @@ fun {sequencer; client; l2_chain; _} _protocol ->
  let chain_id = Some l2_chain.l2_chain_id in

  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* () = produce_block_and_wait_for ~sequencer 3 in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let* () =
    produce_block_and_wait_for ~timestamp:current_timestamp ~sequencer 4
  in
  let* block_1 =
    Client.RPC.call ~hooks ~endpoint client
    @@ RPC.get_chain_block ~block:"head~1" ()
  in
  let* block_2 =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block ()
  in

  return
  @@ check_block_info
       ~previous_block_info:block_1
       ~current_block_info:block_2
       ~chain_id
       ~current_timestamp:(Some current_timestamp)
       ~expected_operations:[[]; []; []; []]

let test_bootstrapped =
  register_tezosx_test
    ~title:"Test of the bootstrapped rpc"
    ~tags:["rpc"; "bootstrapped"]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let* () =
    produce_block_and_wait_for ~timestamp:current_timestamp ~sequencer 3
  in
  let* block =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block_header ()
  in
  let* rpc_bootstrapped =
    RPC_core.call tezlink_endpoint RPC.get_monitor_bootstrapped
  in
  Check.(
    JSON.(
      rpc_bootstrapped |-> "block" |> as_string = (block |-> "hash" |> as_string))
      string
      ~error_msg:"Check block_hash is latest, Expected %R but got %L") ;
  Check.(
    JSON.(
      rpc_bootstrapped |-> "timestamp" |> as_string
      = (block |-> "timestamp" |> as_string))
      string
      ~error_msg:"Check timestamp is latest, Expected %R but got %L") ;
  unit

let test_monitor_heads =
  register_tezosx_test
    ~title:"Test of the monitor/heads RPC"
    ~tags:["evm"; "rpc"; "monitor_heads"]
  @@ fun {sequencer; client; _} _protocol ->
  let open Lwt.Syntax in
  (* Prepare the RPC endpoint *)
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let total_headers = 4 in

  (* Promise to resolve when we’ve received all expected headers *)
  let wait_for_all, notify_all = Lwt.wait () in
  let received_count = ref 0 in

  (* Fetch the initial head for monotonicity checks *)
  let* initial =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block_header ()
  in
  let previous_header = ref initial in

  (* Notifier that is going to be used to indicate the block has been received by the monitor *)
  let notify_block_received : (unit -> unit) ref =
    ref (fun () -> ())
    (* dummy, will be replaced below *)
  in

  (* Process each JSON header line from curl *)
  let process_line line process =
    !notify_block_received () ;
    incr received_count ;
    Log.info "Received block header #%d" !received_count ;
    let current = JSON.parse ~origin:"curl_monitor_heads" line in
    check_header
      ~previous_header:!previous_header
      ~current_header:current
      ~current_timestamp:None
      ~chain_id:None ;
    previous_header := current ;
    if !received_count = total_headers then (
      Log.info "Received all %d headers" total_headers ;
      Tezt.Process.terminate process ;
      Lwt.wakeup_later notify_all ()) ;
    unit
  in

  (* Start streaming headers via curl and process lines *)
  let start_monitor () =
    let url = Evm_node.endpoint sequencer ^ "/tezlink/monitor/heads/main" in
    let process = Tezt.Process.spawn "curl" ["--no-buffer"; "--silent"; url] in
    let ic = Tezt.Process.stdout process in
    let rec loop () =
      let* line_opt = Lwt_io.read_line_opt ic in
      match line_opt with
      | None -> Lwt.return_unit
      | Some line ->
          let* () = process_line line process in
          loop ()
    in
    Lwt.async loop ;
    unit
  in

  (* Produce blocks *)
  let rec produce i =
    if i = 0 then Lwt.return_unit
    else
      let wait_block_received, block_notifier = Lwt.wait () in
      (notify_block_received := fun () -> Lwt.wakeup_later block_notifier ()) ;
      let* _ = Rpc.produce_block sequencer in
      let* () = wait_block_received in
      produce (i - 1)
  in

  (* Timeout in case headers don’t arrive in time *)
  let timeout =
    let* () = Evm_node.wait_for_blueprint_applied sequencer total_headers in
    let* () = Lwt_unix.sleep 10.0 in
    Test.fail ~__LOC__ "Timed out waiting for streamed headers"
  in

  let* () = start_monitor () and* () = produce total_headers in
  let* () = Lwt.pick [wait_for_all; timeout] in

  unit

let test_produceBlock =
  register_tezosx_test
    ~title:"Test Michelson runtime production block"
    ~tags:["kernel"; "produce_block"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let rpc_current_level_head () =
    let* json =
      RPC_core.call_json tezlink_endpoint
      @@ RPC.get_chain_block_helper_current_level ()
    in
    return @@ JSON.(json.body |-> "level" |> as_int)
  in
  let* start_level = rpc_current_level_head () in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let* end_level = rpc_current_level_head () in
  Check.((start_level + 3 = end_level) int)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let test_hash_rpc =
  register_tezosx_test
    ~title:"Test Michelson runtime hash rpc"
    ~tags:["rpc"; "hash"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let rpc_hash block =
    RPC_core.call tezlink_endpoint @@ RPC.get_chain_block_hash ~block ()
  in
  let* hash_old_head = rpc_hash "head" in
  let*@ _ = produce_block sequencer in
  let* hash_current_head = rpc_hash "head" in
  Check.(
    (hash_current_head <> hash_old_head)
      string
      ~error_msg:"Block hash should be different") ;
  let* hash_old_hash = rpc_hash hash_old_head in
  Check.(
    (hash_old_hash = hash_old_head)
      string
      ~error_msg:"Block hash should be equal") ;
  let* hash_current_hash = rpc_hash hash_current_head in
  Check.(
    (hash_current_hash = hash_current_head)
      string
      ~error_msg:"Block hash should be equal") ;
  let* hash_previous_hash = rpc_hash (hash_current_head ^ "~1") in
  Check.(
    (hash_previous_hash = hash_old_head)
      string
      ~error_msg:"Block hash should be equal") ;
  unit

let test_script_rpc =
  register_tezosx_test
    ~title:"Test of the script rpc"
    ~tags:["rpc"; "script"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let foreign_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.(Foreign_endpoint foreign_endpoint) in
  (* Helper to get the script of a contract. Returns None if 404. *)
  let script_rpc contract =
    let* response =
      RPC_core.call_raw foreign_endpoint
      @@ RPC.get_chain_block_context_contract_script ~id:contract ()
    in
    parse_rpc_response ~origin:"script_rpc" response
  in
  (* 1. Script of a non-existent KT1 should return 404. *)
  let fake_kt1 = "KT1J8Hr3BP8bpbfmgGpRPoC9nAMSYtStZG43" in
  let* script = script_rpc fake_kt1 in
  Check.(
    (script = None)
      (option json)
      ~error_msg:"Expected None for non-existent KT1, got %L") ;

  (* 2. Script of an implicit account (tz1) should return 404 *)
  let* script = script_rpc Constant.bootstrap1.public_key_hash in
  Check.(
    (script = None)
      (option json)
      ~error_msg:"Expected None for implicit account, got %L") ;

  (* 3. Script of a non-existent tz1 should return 404 *)
  let fake_tz1 = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let* script = script_rpc fake_tz1 in
  Check.(
    (script = None)
      (option json)
      ~error_msg:"Expected None for non-existent tz1, got %L") ;

  (* 4. Script of an originated contract should return its script *)
  let concat_hello = Tezt_etherlink.Michelson_contracts.concat_hello () in
  let* contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias:"concat_hello_script_test"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:concat_hello.initial_storage
      ~prg:concat_hello.path
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in

  let* script = script_rpc contract in
  Check.(
    (script <> None)
      (option json)
      ~error_msg:"Expected Some script for originated contract, got None") ;

  (* 5. Verify script contains both code and storage fields *)
  let script_json = Option.get script in
  let code = JSON.(script_json |-> "code") in
  let storage = JSON.(script_json |-> "storage") in
  Check.(
    (JSON.is_null code = false)
      bool
      ~error_msg:"Expected script to contain 'code' field") ;
  Check.(
    (JSON.is_null storage = false)
      bool
      ~error_msg:"Expected script to contain 'storage' field") ;

  unit

let test_blocks_list =
  register_tezosx_test
    ~title:"Test of the blocks list rpc"
    ~tags:["rpc"; "blocks"; "list"]
  @@ fun {sequencer; _} _protocol ->
  let* client = tezlink_client_from_evm_node sequencer () in

  let rpc_hash block =
    Client.RPC.call client @@ RPC.get_chain_block_hash ~block ()
  in
  let rpc_blocks ?heads ?length ?min_date () =
    Client.RPC.call client @@ RPC.get_chain_blocks ?heads ?length ?min_date ()
  in
  let rpc_header block =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block ()
  in

  let parse_chains res =
    JSON.(
      res |> as_list
      |> List.map (fun chain -> chain |> as_list |> List.map as_string))
  in

  let check_chains ~__LOC__ ~label ~expected got =
    Check.(
      (got = expected)
        (list (list string))
        ~__LOC__
        ~error_msg:
          (sf "Unexpected block list (%s): expected %%R but got %%L" label))
  in

  let head_block i = if i = 0 then "head" else sf "head~%d" i in

  let produce_blocks n =
    (* Give each produced block a distinct timestamp so min_date filtering is
       deterministic (otherwise multiple blocks can share a timestamp).

       We must also ensure those timestamps are not in the past w.r.t. the
       current head timestamp, otherwise the EVM node rejects the blueprint
       (TimestampFromPast). *)
    let* head_header = rpc_header "head" in
    let head_timestamp =
      JSON.(head_header |-> "timestamp" |> as_string)
      |> Tezos_base.Time.Protocol.of_notation_exn
    in
    let rec loop i remaining =
      if remaining <= 0 then return ()
      else
        let timestamp =
          Tezos_base.Time.Protocol.(add head_timestamp (Int64.of_int (i + 1)))
          |> Tezos_base.Time.Protocol.to_notation
        in
        let*@ _ = produce_block ~timestamp sequencer in
        loop (i + 1) (remaining - 1)
    in
    loop 0 n
  in

  let collect_head_hashes n =
    let rec loop i acc =
      if i >= n then return (List.rev acc)
      else
        let* h = rpc_hash (head_block i) in
        loop (i + 1) (h :: acc)
    in
    loop 0 []
  in

  let fetch_chains ?heads ?length ?min_date () =
    let* res = rpc_blocks ?heads ?length ?min_date () in
    return (parse_chains res)
  in

  let run_and_check ?heads ?length ?min_date ~__LOC__ ~label ~expected () =
    let* got = fetch_chains ?heads ?length ?min_date () in
    check_chains ~__LOC__ ~label ~expected got ;
    return ()
  in

  let nb_blocks_to_produce = 6 in
  let* () = produce_blocks nb_blocks_to_produce in
  let* head_hashes = collect_head_hashes nb_blocks_to_produce in

  let head, head_1, head_2, head_3, head_4, head_5 =
    match head_hashes with
    | h0 :: h1 :: h2 :: h3 :: h4 :: h5 :: _ -> (h0, h1, h2, h3, h4, h5)
    | _ ->
        Test.fail
          ~__LOC__
          "Expected at least %d head hashes, got %d"
          nb_blocks_to_produce
          (List.length head_hashes)
  in
  let* head_6 = rpc_hash "head~6" in
  let* head_7 = rpc_hash "head~7" in

  let* () = run_and_check ~__LOC__ ~label:"default" ~expected:[[head]] () in
  let* () =
    run_and_check ~__LOC__ ~label:"length=1" ~length:1 ~expected:[[head]] ()
  in
  let* () =
    run_and_check
      ~__LOC__
      ~label:"length=nb_blocks_to_produce"
      ~length:nb_blocks_to_produce
      ~expected:[head_hashes]
      ()
  in

  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head~1],length=2"
      ~heads:[head_1]
      ~length:2
      ~expected:[[head_1; head_2]]
      ()
  in
  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head~3;head~1],length=3"
      ~heads:[head_3; head_1]
      ~length:3
      ~expected:[[head_1; head_2; head_3]; [head_3]]
      ()
  in
  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head~2;head~1],length=3"
      ~heads:[head_2; head_1]
      ~length:3
      ~expected:[[head_1; head_2; head_3]; [head_2]]
      ()
  in
  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head~5;head~1],length=3"
      ~heads:[head_5; head_1]
      ~length:3
      ~expected:[[head_1; head_2; head_3]; [head_5; head_6; head_7]]
      ()
  in

  let* head_3_header = rpc_header "head~3" in
  let min_date =
    JSON.(head_3_header |-> "timestamp" |> as_string)
    |> Tezos_base.Time.Protocol.of_notation_exn
    |> Tezos_base.Time.Protocol.to_seconds |> Int64.to_int
  in

  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head;head~4],length=3,min_date=head~3"
      ~heads:[head; head_4]
      ~length:3
      ~min_date
      ~expected:[[head; head_1; head_2]]
      ()
  in

  let* () =
    run_and_check
      ~__LOC__
      ~label:"heads=[head;head~4],length=nb_blocks_to_produce,min_date=head~3"
      ~heads:[head; head_4]
      ~length:nb_blocks_to_produce
      ~min_date
      ~expected:[head_hashes]
      ()
  in
  unit

(* This test was copied and adapted from tezt/tests/contract_storage_normalization.ml *)

let test_contract_storage_normalization =
  let contract = Michelson_contracts.storage_normalization () in
  register_tezosx_test
    ~title:"Test of the storage rpc with normalization"
    ~tags:["rpc"; "storage"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; client; _} _protocol ->
  let foreign_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.(Foreign_endpoint foreign_endpoint) in
  let address_readable = sf "%S" Constant.bootstrap1.public_key_hash in
  let* address_optimized =
    Client.normalize_data
      ~mode:Optimized
      ~data:address_readable
      ~typ:"address"
      client
  in
  let address_optimized = String.trim address_optimized in
  Lwt_list.iter_s
    (fun (unparsing_mode, expected) ->
      let* storage =
        Client.contract_storage
          ~endpoint
          ~unparsing_mode
          contract.address
          client
      in
      Log.info
        "Checking contract storage using unparsing mode %s"
        (Client.normalize_mode_to_string unparsing_mode) ;
      Check.(
        (String.trim storage = expected)
          string
          ~__LOC__
          ~error_msg:"expected storage %R, got %L") ;
      unit)
    [
      (Readable, sf "Pair %s Unit Unit Unit" address_readable);
      (Optimized, sf "{ %s ; Unit ; Unit ; Unit }" address_optimized);
      ( Optimized_legacy,
        sf "Pair %s (Pair Unit (Pair Unit Unit))" address_optimized );
    ]

let test_contract_counter =
  let contract = Michelson_contracts.concat_hello () in
  register_tezosx_test
    ~title:"Test that smart contracts don't have any counter"
    ~tags:["rpc"; "counter"]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let* valid_res =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract ~id:contract.address ()
  in
  Check.(
    JSON.(valid_res |-> "counter" |> as_int_opt = None)
      (option int)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_raw_json_cycle =
  register_tezosx_test
    ~title:"Test Michelson runtime raw json cycle rpc"
    ~tags:["rpc"; "cycle"; "raw"]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let*@ _ = produce_block sequencer in
  let* cycle_0 =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_raw_json ~path:["cycle"; "0"] ()
  in
  (* 0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8 is the mock value
     used for the ranom seed and never changes *)
  Check.(
    JSON.(
      cycle_0 |-> "random_seed" |> as_string
      = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")
      string
      ~error_msg:"Unexpected random_seed field") ;
  unit

let test_transfer =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  (* Slot burn for crediting an unallocated implicit account. *)
  let allocation_burn = 64250 in
  register_tezosx_test
    ~title:"Test michelson runtime transfer"
    ~tags:["kernel"; "transfer"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let amount = Tez.one in
  let fee = Tez.one in
  let* () =
    Client.transfer
      ~endpoint
      ~amount
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance
      - Tez.to_mutez amount - Tez.to_mutez fee - allocation_burn)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_observer_transfer =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  (* Slot burn for crediting an unallocated implicit account. *)
  let allocation_burn = 64250 in
  register_tezosx_test
    ~title:"Test michelson runtime transfer via an observer"
    ~tags:["observer"; "transfer"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; observer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node observer in
  let amount = Tez.one in
  let fee = Tez.one in
  (* Send a transaction to the observer and wait for it to be relayed to the sequencer *)
  let tx_hash_p = Evm_node.wait_for_tx_queue_add_transaction sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~amount
      ~fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let* _tx_hash = tx_hash_p in
  let*@ nb_txs = produce_block sequencer in
  Check.((nb_txs = 1) int)
    ~error_msg:"Expected %R transaction in the block, got %L" ;
  let* () = Evm_node.wait_for_blueprint_applied observer 3 in
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance
      - Tez.to_mutez amount - Tez.to_mutez fee - allocation_burn)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_transfer_and_wait =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  (* Slot burn for crediting an unallocated implicit account. *)
  let allocation_burn = 64250 in
  register_tezosx_test
    ~title:"Test michelson runtime transfer and wait for inclusion"
    ~tags:["kernel"; "transfer"; "wait"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~time_between_blocks:(Time_between_blocks 0.1)
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let amount = Tez.one in
  let fee = Tez.one in
  (* The branch of the operation injected by the client is the
     grand-parent of the current block. Looking for operation hashes
     relies on the operation_hashes RPC which is not implemented for
     the genesis block. For this reason, we wait for block level 5
     before building the operation to ensure that the operation's
     branch is not genesis. *)
  let* () = Evm_node.wait_for_blueprint_applied sequencer 5 in
  let* () =
    Client.transfer
      ~wait:"2"
      ~endpoint
      ~fee
      ~amount
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance
      - Tez.to_mutez amount - Tez.to_mutez fee - allocation_burn)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_reveal =
  register_tezosx_test
    ~title:"Test michelson runtime reveal"
    ~tags:["kernel"; "reveal"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in
  let get_contract_manager_key account =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_manager_key
         ~id:account.Account.public_key_hash
         ()
  in
  let amount = Tez.one in
  let* () =
    Client.transfer
      ~endpoint
      ~amount
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* manager_key = get_contract_manager_key Constant.bootstrap2 in
  Check.(
    JSON.(manager_key |> as_string_opt = None)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  let*! () = Client.reveal ~endpoint ~src:Constant.bootstrap2.alias client in
  let*@ _ = produce_block sequencer in
  let* manager_key = get_contract_manager_key Constant.bootstrap2 in
  Check.(
    JSON.(manager_key |> as_string_opt = Some Constant.bootstrap2.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_bootstrap_block_info =
  (* This test just makes sure the block info call on block 4 does not fail
     catastrophically. We check block 4 in particular because at this level
     we mock information to index bootstrap accounts. *)
  let contract = Michelson_contracts.concat_hello () in
  register_tezosx_test
    ~title:"Test of tezlink block info on block 4"
    ~tags:["bootstrap"; "block_info"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let* info =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block ~block:"4" ()
  in
  Check.(
    JSON.(info |-> "header" |-> "level" |> as_int = 4)
      int
      ~error_msg:
        "Expect %R but got %L: we should have been able to get block info for \
         level 4") ;
  unit

let test_execution =
  let contract = Michelson_contracts.concat_hello () in
  register_tezosx_test
    ~title:"Test of tezlink execution"
    ~tags:["execution"; "hello"; "storage"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let expected_result = "{ \"Hello world\" }" in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:contract.address
      ~arg:"{\"world\"}"
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* actual_result =
    Client.contract_storage ~endpoint contract.address client
  in
  Check.(
    (String.trim actual_result = String.trim expected_result)
      string
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;
  unit

let test_bigmap_option =
  let option_contract = Michelson_contracts.big_map_option () in
  register_tezosx_test
    ~title:"Test which syntax is used for big maps in contract storages"
    ~tags:["syntax"; "big_map"; "option"; "storage"]
    ~bootstrap_contracts:[option_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let expected_result = "Some 0" in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:option_contract.address
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* actual_result =
    Client.contract_storage ~endpoint option_contract.address client
  in
  Check.(
    (String.trim actual_result = String.trim expected_result)
      string
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;
  unit

let test_bigmap_counter =
  let counter_contract = Michelson_contracts.big_map_counter () in
  register_tezosx_test
    ~title:"Test of tezlink big_map persistency"
    ~tags:["persistency"; "big_map"; "counter"; "storage"]
    ~bootstrap_contracts:[counter_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  (* This test verifies big_map persistence in storage.
     It relies on the counter.tz invariant that a counter with a matching key
     exists in the stored big_map. The contract is called twice to ensure
     that the big_map is committed to durable storage between calls. *)
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:counter_contract.address
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* result_1 =
    Client.contract_storage ~endpoint counter_contract.address client
  in

  Check.(
    (String.trim result_1 =~ rex "Pair 1 .*")
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;

  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:counter_contract.address
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* result_2 =
    Client.contract_storage ~endpoint counter_contract.address client
  in

  Check.(
    (String.trim result_2 =~ rex "Pair 2 .*")
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;
  unit

(** Regression guard for the installer / kernel / node-RPC path alignment on
    bootstrap KT1s. Before this guard, the three sides disagreed on where
    Tezlink contract state lives: the installer wrote to
    [/evm/world_state/contracts/index/<hex>/...], the kernel read from
    [/tez/tez_accounts/contracts/index/<hex>/...] (post-Phase-5),
    and the node RPC read back from the installer's ghost path. RPC-only
    tests (reading [data/storage] or [balance]) happened to be internally
    consistent and did not catch the mismatch.

    The balance assertion here is the tight check: it only reads the
    expected value if the kernel was able to credit the bootstrap KT1 at
    the same path the RPC queries. *)
let test_bootstrap_kt1_is_executable =
  let counter_contract = Michelson_contracts.big_map_counter () in
  register_tezosx_test
    ~title:"Bootstrap KT1 is executable (installer/kernel/RPC path alignment)"
    ~tags:["bootstrap"; "contract"; "kt1"; "installer"]
    ~bootstrap_contracts:[counter_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Installer-set bootstrap balance for tez accounts, in mutez. *)
  let bootstrap_balance_mutez = 3_800_000_000_000 in
  let transfer_mutez = 10_000_000 in
  (* 1. Installer-written balance is visible through the RPC. Fails if the
        installer and the RPC disagree on the contract subtree root. *)
  let* initial_balance =
    Client.get_balance_for ~endpoint ~account:counter_contract.address client
  in
  Check.(
    (Tez.to_mutez initial_balance = bootstrap_balance_mutez)
      int
      ~error_msg:
        "Bootstrap KT1 balance not visible to RPC: got %L, expected %R. \
         Installer/RPC disagreement on the Tezlink contract subtree root.") ;
  (* 2. Call the KT1. [big_map_counter]'s default entrypoint rotates a key
        in the big_map and increments the counter, so the kernel must be
        able to read [data/code] + [data/storage] from the same path the
        installer populated. *)
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:counter_contract.address
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  (* 3. Storage mutated: counter went from 0 to 1. Fails if the kernel could
        not find the script to execute. *)
  let* storage =
    Client.contract_storage ~endpoint counter_contract.address client
  in
  Check.(
    (String.trim storage =~ rex "Pair 1 .*")
      ~error_msg:
        "Bootstrap KT1 storage did not update after call: got \"%L\". Kernel \
         failed to execute the contract (data/code or data/storage unreadable \
         at the expected path).") ;
  (* 4. Balance was credited by the kernel at the path the RPC reads from.
        Fails if the kernel writes the updated balance to a different subtree
        than the RPC queries. *)
  let* post_call_balance =
    Client.get_balance_for ~endpoint ~account:counter_contract.address client
  in
  Check.(
    (Tez.to_mutez post_call_balance = bootstrap_balance_mutez + transfer_mutez)
      int
      ~error_msg:
        "Bootstrap KT1 balance after transfer: got %L, expected %R. Kernel \
         balance write and RPC balance read disagree on the contract's balance \
         path.") ;
  unit

let test_bigmap_rpcs =
  let counter_contract = Michelson_contracts.big_map_counter () in
  register_tezosx_test
    ~title:"Test of the big_map RPCs"
    ~tags:["rpc"; "big_map"]
    ~bootstrap_contracts:[counter_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Call the contract to populate the big_map with key 1 *)
  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:counter_contract.address
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let big_map_id = "0" in
  (* Test big_map get RPC *)
  let* hash_result = Client.hash_data ~data:"1" ~typ:"nat" client in
  let key_hash = hash_result.script_expr_hash in
  let* json =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_context_big_map ~id:big_map_id ~key_hash ()
  in
  let prim = JSON.(json |-> "prim" |> as_string) in
  Check.((prim = "Unit") string ~error_msg:"Expected %R but got %L") ;
  (* Test big_map raw info RPC *)
  let* json =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_context_raw_json_big_maps_index ~id:big_map_id ()
  in
  let key_type_prim = JSON.(json |-> "key_type" |-> "prim" |> as_string) in
  Check.(
    (key_type_prim = "nat") string ~error_msg:"Expected key_type %R but got %L") ;
  let value_type_prim = JSON.(json |-> "value_type" |-> "prim" |> as_string) in
  Check.(
    (value_type_prim = "unit")
      string
      ~error_msg:"Expected value_type %R but got %L") ;
  unit

let test_pack_data =
  register_tezosx_test
    ~title:"Test of the pack_data RPC"
    ~tags:["rpc"; "pack_data"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Test cases: (data, type) as Micheline JSON *)
  let test_cases =
    [
      (`O [("int", `String "1")], `O [("prim", `String "nat")]);
      (`O [("prim", `String "True")], `O [("prim", `String "bool")]);
      (`O [("string", `String "hello")], `O [("prim", `String "string")]);
    ]
  in
  let check_pack_data (data, ty) =
    (* Call pack_data on Tezlink *)
    let* tezlink_result =
      Client.RPC.call ~endpoint:tezlink_endpoint client
      @@ RPC.post_chain_block_helpers_scripts_pack_data ~data ~ty ()
    in
    (* Call pack_data on L1 *)
    let* l1_result =
      Client.RPC.call client
      @@ RPC.post_chain_block_helpers_scripts_pack_data ~data ~ty ()
    in
    Check.(
      (tezlink_result = l1_result)
        json
        ~error_msg:"pack_data mismatch: Tezlink=%L, L1=%R") ;
    unit
  in
  Lwt_list.iter_s check_pack_data test_cases

let test_run_operation =
  register_tezosx_test
    ~title:"Test of the run_operation RPC"
    ~tags:["rpc"; "run_operation"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Get Tezlink chain_id *)
  let* tezlink_chain_id =
    Client.RPC.call ~endpoint:tezlink_endpoint client
    @@ RPC.get_chain_chain_id ()
  in
  (* Get Tezlink head hash for branch *)
  let* tezlink_head =
    Client.RPC.call ~endpoint:tezlink_endpoint client
    @@ RPC.get_chain_block_hash ()
  in
  (* Create operation JSON manually with counter=1 (fresh account on Tezlink).
     This has been validated manually in sandbox mode. *)
  let op_json =
    `O
      [
        ( "operation",
          `O
            [
              ("branch", `String tezlink_head);
              ( "contents",
                `A
                  [
                    `O
                      [
                        ("kind", `String "transaction");
                        ("source", `String Constant.bootstrap1.public_key_hash);
                        ("fee", `String "1000");
                        ("counter", `String "1");
                        ("gas_limit", `String "10000");
                        ("storage_limit", `String "0");
                        ("amount", `String "1000000");
                        ( "destination",
                          `String Constant.bootstrap2.public_key_hash );
                      ];
                  ] );
              ( "signature",
                `String
                  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
              );
            ] );
        ("chain_id", `String tezlink_chain_id);
      ]
  in
  (* Call run_operation on Tezlink *)
  let* tezlink_result =
    Client.RPC.call ~endpoint:tezlink_endpoint client
    @@ RPC.post_chain_block_helpers_scripts_run_operation (Data op_json)
  in
  (* Check the status is "applied" *)
  let tezlink_status =
    JSON.(
      tezlink_result |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
      |-> "status" |> as_string)
  in
  Check.(
    (tezlink_status = "applied")
      string
      ~error_msg:
        "Michelson runtime run_operation status: expected %R but got %L") ;
  unit

let test_reveal_transfer_batch =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  (* Slot burn for crediting an unallocated implicit account. *)
  let allocation_burn = 64250 in
  register_tezosx_test
    ~title:"Test michelson runtime reveal+transfer batch"
    ~tags:["kernel"; "reveal"; "transfer"; "batch"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  (* Unfortunately, the client does not allow us to stipulate fees when revealing
     an account with a batch, so we need to capture this information from the log *)
  let collected_fees : int list ref = ref [] in

  (* Regex: find the number after the ꜩ symbol in the line containing "Fee to the baker" *)
  let fee_regex = Str.regexp "Fee to the baker: ꜩ\\([0-9]+\\.?[0-9]*\\)" in

  let on_log line =
    match
      try Some (Str.search_forward fee_regex line 0) with Not_found -> None
    with
    | Some _ -> (
        let fee_str = Str.matched_group 1 line in
        (* Remove the dot and calculate the new integer string *)
        let fee_mutez_str =
          match String.split_on_char '.' fee_str with
          | [int_part; frac_part] ->
              let frac_part_padded =
                frac_part
                ^ String.make (max 0 (6 - String.length frac_part)) '0'
              in
              int_part ^ String.sub frac_part_padded 0 6
          | [int_part] -> int_part ^ "000000"
          | _ -> "" (* fallback for malformed input *)
        in
        match int_of_string_opt fee_mutez_str with
        | Some fee_mutez ->
            collected_fees := fee_mutez :: !collected_fees ;
            Log.info "Captured fee: %s (mutez: %d)" fee_str fee_mutez
        | None -> Log.warn "Could not parse fee: %s" fee_str)
    | None -> ()
  in

  let on_spawn _ _ = () in

  let* () =
    Client.transfer
      ~endpoint
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      ~hooks:{on_log; on_spawn}
      client
  in
  let fee_bootstrap1 = List.fold_left ( + ) 0 !collected_fees in
  collected_fees := [] ;
  let*@ _ = produce_block sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:Tez.one
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      ~hooks:{on_log; on_spawn}
      client
  in
  let fee_bootstrap2 = List.fold_left ( + ) 0 !collected_fees in
  let*@ _ = produce_block sequencer in
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Log.info "bootstrap_1 fees: %d" fee_bootstrap1 ;
  Log.info "bootstrap_2 fees: %d" fee_bootstrap2 ;
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance
      - Tez.(to_mutez one)
      - fee_bootstrap1 - allocation_burn)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.(to_mutez one) - fee_bootstrap2) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_batch =
  (* Slot burn for crediting an unallocated implicit account. *)
  let allocation_burn = 64250 in
  register_tezosx_test
    ~title:"Test of tezlink batches"
    ~tags:["batch"; "multiple_transfers"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let fee_2 = Tez.of_mutez_int 100_000 in
  let fee_3 = Tez.of_mutez_int 200_000 in
  let amount2 = Tez.of_mutez_int 1_000_000 in
  let amount3 = Tez.of_mutez_int 2_000_000 in
  let json_batch =
    `A
      [
        `O
          [
            ("destination", `String Constant.bootstrap2.alias);
            ("amount", `String (Tez.to_string amount2));
            ("fee", `String (Tez.to_string fee_2));
          ];
        `O
          [
            ("destination", `String Constant.bootstrap3.alias);
            ("amount", `String (Tez.to_string amount3));
            ("fee", `String (Tez.to_string fee_3));
          ];
      ]
    |> JSON.encode_u
  in
  let* init_balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* init_balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  let* init_balance3 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap3.alias client
  in
  let*! () =
    Client.multiple_transfers
      ~endpoint
      ~giver:Constant.bootstrap1.alias
      ~json_batch
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* end_balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* end_balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  let* end_balance3 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap3.alias client
  in
  Check.(
    (Tez.to_mutez end_balance1
    = Tez.to_mutez init_balance1 - Tez.to_mutez amount2 - Tez.to_mutez amount3
      - Tez.to_mutez fee_2 - Tez.to_mutez fee_3 - (2 * allocation_burn))
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.(
    (Tez.to_mutez end_balance2
    = Tez.to_mutez init_balance2 + Tez.to_mutez amount2)
      int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  Check.(
    (Tez.to_mutez end_balance3
    = Tez.to_mutez init_balance3 + Tez.to_mutez amount3)
      int)
    ~error_msg:"Wrong balance for bootstrap3: expected %R, actual %L" ;
  unit

(* The goal of this test is twofold:
   - Test that long transfer batches are supported by Tezlink (see also
     [test_batch] for a test checking the balance changes on a batch of
     size 2),
   - Test that there is no regression for the counter RPC which used to return
     128 instead of 64 (see MR !19237).
*)
let test_long_batch =
  register_tezosx_test
    ~title:"Long transaction batch"
    ~tags:["transaction"; "long"; "batch"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in
  let batch_size = 64 in
  let json_transfer =
    `O
      [
        ("destination", `String Constant.bootstrap1.alias);
        ("amount", `String Tez.(to_string one));
      ]
  in
  let json_batch =
    `A (List.init batch_size (fun _op_index -> json_transfer)) |> JSON.encode_u
  in
  let*! () =
    Client.multiple_transfers
      ~endpoint
      ~giver:Constant.bootstrap1.alias
      ~json_batch
        (* The default fee cap is not enough because of overestimated gas costs by simulation mock *)
      ~fee_cap:Tez.(of_int 10)
      client
  in
  let*@ _ = produce_block sequencer in
  let* counter =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.Account.public_key_hash
         ()
  in
  Check.(
    (batch_size = (counter |> JSON.as_int))
      int
      ~error_msg:
        "Wrong counter after batch application: expected: %L, actual: %R.") ;
  unit

let test_internal_operation =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  (* code + storage size = 81 bytes. *)
  let bootstrap_contract_first_use_burn = 81 * 250 in
  register_tezosx_test
    ~title:"Internal operation"
    ~tags:["internal"; "operation"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[faucet]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~fee:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:faucet.address
      ~burn_cap:Tez.one
      ~entrypoint:"fund"
      ~arg:"2000000"
      client
  in
  let*@ _ = produce_block sequencer in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.(
    (Tez.to_mutez balance
    = Tez.to_mutez bootstrap_balance
      + Tez.(to_mutez one)
      - bootstrap_contract_first_use_burn)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  unit

let test_internal_receipts =
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  register_tezosx_test
    ~title:"Internal receipts"
    ~tags:["internal"; "operation"; "receipts"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[faucet]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let amount = 1000000 in
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~fee:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:faucet.address
      ~burn_cap:Tez.one
      ~entrypoint:"fund"
      ~arg:(string_of_int amount)
      client
  in
  let*@ _ = produce_block sequencer in

  let* operation =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:3
         ~operation_offset:0
         ()
  in

  let operation_metadata =
    JSON.(operation |-> "contents" |> as_list |> List.hd |-> "metadata")
  in
  Check.(
    JSON.(
      operation_metadata |-> "operation_result" |-> "status" |> as_string
      = "applied")
      string
      ~error_msg:"Expected status to be %R but got %L") ;

  let internal_ops_list =
    JSON.(operation_metadata |-> "internal_operation_results" |> as_list)
  in
  Check.(
    (List.length internal_ops_list = 1)
      int
      ~error_msg:"Expected 1 internal operation but got %L") ;

  (* Check the internal operation *)
  let internal_op_content = List.hd internal_ops_list in
  Check.(
    JSON.(internal_op_content |-> "kind" |> as_string = "transaction")
      string
      ~error_msg:"Expected internal operation kind to be %R but got %L") ;
  Check.(
    JSON.(
      internal_op_content |-> "destination" |> as_string
      = Constant.bootstrap1.public_key_hash)
      string
      ~error_msg:"Expected internal operation destination to be %R but got %L") ;
  Check.(
    JSON.(internal_op_content |-> "amount" |> as_int = amount)
      int
      ~error_msg:"Expected internal operation amount to be %R but got %L") ;
  Check.(
    JSON.(internal_op_content |-> "source" |> as_string = faucet.address)
      string
      ~error_msg:"Expected internal operation source to be %R but got %L") ;
  let internal_op_result = JSON.(internal_op_content |-> "result") in
  Check.(
    JSON.(internal_op_result |-> "status" |> as_string = "applied")
      string
      ~error_msg:"Expected internal operation status to be %R but got %L") ;

  (* Check the internal operation result's balance updates *)
  let balance_updates =
    JSON.(internal_op_result |-> "balance_updates" |> as_list)
  in
  Check.(
    (List.length balance_updates = 2)
      int
      ~error_msg:"Expected 2 INTERNAL balance updates but got %L") ;

  let check_balance_update json_data ~kind ~contract ~change ~origin =
    Check.(
      JSON.(json_data |-> "kind" |> as_string = kind)
        string
        ~error_msg:"Expected kind to be %R but got %L") ;
    Check.(
      JSON.(json_data |-> "contract" |> as_string = contract)
        string
        ~error_msg:"Expected contract to be %R but got %L") ;
    Check.(
      JSON.(json_data |-> "change" |> as_int = change)
        int
        ~error_msg:"Expected change to be %R but got %L") ;
    Check.(
      JSON.(json_data |-> "origin" |> as_string = origin)
        string
        ~error_msg:"Expected origin to be %R but got %L")
  in

  let first_update = List.hd balance_updates in
  check_balance_update
    first_update
    ~kind:"contract"
    ~contract:faucet.address
    ~change:(-amount)
    ~origin:"block" ;

  let second_update = List.nth balance_updates 1 in
  check_balance_update
    second_update
    ~kind:"contract"
    ~contract:Constant.bootstrap1.public_key_hash
    ~change:amount
    ~origin:"block" ;
  unit

let test_origination =
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  register_tezosx_test
    ~title:"Contract origination"
    ~tags:["origination"; "operation"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* deployed_faucet =
    Client.originate_contract
      ~endpoint
      ~amount:(Tez.of_int 2)
      ~alias:"faucet"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:faucet.initial_storage
      ~prg:faucet.path
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = produce_block sequencer in
  let* faucet_balance =
    Client.get_balance_for ~endpoint ~account:deployed_faucet client
  in
  Check.((Tez.to_mutez faucet_balance = Tez.(to_mutez (of_int 2))) int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  let* bootstrap_balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* () =
    Client.transfer
      ~endpoint
      ~fee:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:deployed_faucet
      ~burn_cap:Tez.one
      ~entrypoint:"fund"
      ~arg:"2000000"
      client
  in
  let*@ _ = produce_block sequencer in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.(
    (Tez.to_mutez balance = Tez.to_mutez bootstrap_balance + Tez.(to_mutez one))
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  unit

let test_operation_hashes_in_pass =
  register_tezosx_test
    ~title:"Test /operation_hashes/<pass> rpc"
    ~tags:["rpc"; "operation_hashes"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* transfer_hash =
    let process =
      Client.spawn_transfer
        ~endpoint
        ~amount:Tez.one
        ~fee:Tez.one
        ~giver:Constant.bootstrap1.alias
        ~receiver:Constant.bootstrap2.alias
        ~burn_cap:Tez.one
        client
    in
    let* client_output = Process.check_and_read_stdout process in
    match client_output =~* rex "Operation hash is '?(o\\w{50})'" with
    | None ->
        Test.fail
          "Cannot extract operation hash from client_output: %s"
          client_output
    | Some hash -> return hash
  in
  let*@ _ = produce_block sequencer in
  let block = "head" in

  let* hashes_in_pass0 =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass ~block 0
  in
  Check.(
    (hashes_in_pass0 = []) (list string) ~error_msg:"Expected %R but got %L") ;
  let* hashes_in_pass1 =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass ~block 1
  in
  Check.(
    (hashes_in_pass1 = []) (list string) ~error_msg:"Expected %R but got %L") ;
  let* hashes_in_pass2 =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass ~block 2
  in
  Check.(
    (hashes_in_pass2 = []) (list string) ~error_msg:"Expected %R but got %L") ;
  let* hashes_in_pass3 =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass ~block 3
  in
  Check.(
    (hashes_in_pass3 = [transfer_hash])
      (list string)
      ~error_msg:"Expected %R but got %L") ;

  let* operation_hashes =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_operation_hashes ~block ()
  in
  Check.(
    (operation_hashes
    = [hashes_in_pass0; hashes_in_pass1; hashes_in_pass2; hashes_in_pass3])
      (list (list string))
      ~error_msg:"Expected %R but got %L") ;

  unit

let test_event =
  let emit_events_contract =
    Tezt_etherlink.Michelson_contracts.emit_events_contract ()
  in
  register_tezosx_test
    ~title:"Contract emits an event"
    ~tags:["operation"; "event"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* deployed_emit_events_contract =
    Client.originate_contract
      ~endpoint
      ~amount:(Tez.of_int 2)
      ~alias:"emit_events_contract"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:emit_events_contract.initial_storage
      ~prg:emit_events_contract.path
      ~burn_cap:Tez.one
      client
  in
  let*@ _l2_level = produce_block sequencer in
  let* () =
    Client.transfer
      ~endpoint
      ~fee:Tez.one
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:deployed_emit_events_contract
      ~burn_cap:Tez.one
      client
  in
  let*@ _l2_level = produce_block sequencer in

  let* first_manager_operation =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:3
         ~operation_offset:0
         ()
  in

  let open JSON in
  let metadata = first_manager_operation |-> "contents" |=> 0 |-> "metadata" in
  assert (metadata |-> "operation_result" |-> "status" |> as_string = "applied") ;
  let events = metadata |-> "internal_operation_results" in
  let event = events |=> 0 in
  let assert_prim ~prim ~annots json =
    assert (json |-> "prim" |> as_string = prim) ;
    assert (json |-> "annots" |> as_list |> List.map as_string = annots)
  in
  let assert_type ~annots event =
    let ty = event |-> "type" in
    assert_prim ty ~prim:"or" ~annots:[] ;
    let args = ty |-> "args" in
    assert_prim
      (args |=> 0)
      ~prim:"nat"
      ~annots:(if annots then ["%int"] else []) ;
    assert_prim
      (args |=> 1)
      ~prim:"string"
      ~annots:(if annots then ["%str"] else [])
  in

  let expected_event_gas_cost =
    (* See Michelson_v1_gas.manager_operation_int *) 100_000
  in
  assert_type ~annots:false event ;
  let data = event |-> "payload" in
  assert (data |-> "prim" |> as_string = "Right") ;
  assert (data |-> "args" |=> 0 |-> "string" |> as_string = "right") ;
  let tag = event |-> "tag" |> as_string in
  assert (tag = "tag1") ;
  assert (event |-> "result" |-> "status" |> as_string = "applied") ;
  assert (
    event |-> "result" |-> "consumed_milligas" |> as_int
    = expected_event_gas_cost) ;

  let event = events |=> 1 in
  assert_type ~annots:true event ;
  let data = event |-> "payload" in
  assert (data |-> "prim" |> as_string = "Left") ;
  assert (data |-> "args" |=> 0 |-> "int" |> as_string = "2") ;
  let tag = event |-> "tag" |> as_string in
  assert (tag = "tag2") ;
  assert (event |-> "result" |-> "status" |> as_string = "applied") ;
  assert (
    event |-> "result" |-> "consumed_milligas" |> as_int
    = expected_event_gas_cost) ;
  unit

let test_forge_operations =
  register_tezosx_test
    ~title:"Test of the forge/operations rpc"
    ~tags:["rpc"; "forge"; "operations"]
    ~additional_uses:[Constant.octez_codec]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  (* call the forge/operations rpc and parse the result *)
  let json_data =
    Ezjsonm.value_from_string
      {|
{
  "branch": "BL5HqLzfzPd4b72LwoGtwgXYth26dvit8S8RW4tMkQgbzTWfzww",
  "contents": [
    {
      "kind": "reveal",
      "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
      "fee": "1",
      "counter": "1",
      "gas_limit": "100000",
      "storage_limit": "100",
      "public_key": "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
    },
    {
      "kind": "transaction",
      "source": "tz1boqBV6XhVjXA2i5ysZ4megxZxa2EEgyxg",
      "fee": "644",
      "counter": "21893998",
      "gas_limit": "2169",
      "storage_limit": "0",
      "amount": "100000",
      "destination": "tz1boqBV6XhVjXA2i5ysZ4megxZxa2EEgyxg"
    }
  ]
}
|}
  in
  let* binary =
    let name =
      Protocol.encoding_prefix Michelson_contracts.tezlink_protocol
      ^ ".operation.unsigned"
    in
    Codec.encode ~name json_data
  in
  let expected_res = sf "\"%s\"\n" binary in
  let* res =
    RPC_core.call_raw tezlink_endpoint
    @@ RPC.post_chain_block_helpers_forge_operations
         ~data:(RPC_core.Data json_data)
         ()
  in
  Check.((res.body = expected_res) string ~error_msg:"Expected %R but got %L") ;
  unit

let test_prevalidation =
  register_tezosx_test
    ~title:"Test michelson runtime prevalidation"
    ~tags:["kernel"; "prevalidation"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  (* - Setup - *)
  (* An implicit address completely unknown from network*)
  let* unknown = Client.gen_and_show_keys ~sig_alg:"p256" client_tezlink in
  (* An implicit address with funds but unrevealed *)
  let* unrevealed = Client.gen_and_show_keys ~sig_alg:"p256" client_tezlink in
  let* _ =
    Operation.inject_transfer
      ~source:Constant.bootstrap1
      ~dest:unrevealed
      ~fee:2000
      ~gas_limit:5000
      client_tezlink
  in

  let*@ _ = produce_block sequencer in

  (* - Tests - *)
  (* case unknown source*)
  let* op_unknown_source =
    Operation.Manager.(
      operation [make ~fee:1000 ~source:unknown (transfer ())] client)
  in
  (* Unknown contracts are treated as unrevealed *)
  let unknown_rex =
    rex (sf "Unrevealed manager key for contract %s." unknown.public_key_hash)
  in
  let* _ =
    Operation.inject
      ~error:unknown_rex
      ~dont_wait:true
      op_unknown_source
      client_tezlink
  in
  (* case unrevealed source*)
  let* op_unrevealed =
    Operation.Manager.(
      operation [make ~fee:1000 ~source:unrevealed (transfer ())] client)
  in
  let unrevealed_rex =
    rex
      (sf "Unrevealed manager key for contract %s." unrevealed.public_key_hash)
  in
  let* _ =
    Operation.inject
      ~error:unrevealed_rex
      ~dont_wait:true
      op_unrevealed
      client_tezlink
  in

  let block_payload_hash =
    "vh2jSMcbbWrJ39CqQ87CfK8BPrPsnCgmxWLSmgrDFFgaiCVnKDcL"
  in
  let* op_not_manager =
    Operation.Consensus.(
      operation
        ~signer:Constant.bootstrap1
        (consensus
           ~kind:(Attestation {with_dal = false; companion_key = None})
           ~slot:0
           ~level:0
           ~round:0
           ~block_payload_hash)
        client)
  in
  (* This error comes from the Operation.decode function. The test will need to be adapted when decoding is simplified to not include any validation. *)
  let not_manager_rex = rex "not_a_manager_operation" in
  let* _ =
    Operation.inject
      ~error:not_manager_rex
      ~dont_wait:true
      op_not_manager
      client_tezlink
  in

  (* case wrong key *)
  let inconsistent_hash_rex =
    rex
      (sf
         "The hash of the manager public key %s is not %s as announced but %s"
         Constant.bootstrap2.public_key
         Constant.bootstrap2.public_key_hash
         unrevealed.public_key_hash)
    (* According to the protocol error, the first hash is the one of the public
       key, the second one is the unrevealed manager key. *)
  in
  let* op_inconsistent =
    Operation.Manager.(
      operation
        [make ~fee:1000 ~source:unrevealed (reveal Constant.bootstrap2 ())]
        client)
  in
  let* _ =
    Operation.inject
      ~error:inconsistent_hash_rex
      ~dont_wait:true
      op_inconsistent
      client_tezlink
  in

  (* case already revealed *)
  let previously_revealed_rex =
    rex
      (sf
         "Previously revealed manager key for contract %s."
         Constant.bootstrap1.public_key_hash)
  in
  let* op_previously_revealed =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~source:Constant.bootstrap1
            (reveal Constant.bootstrap1 ());
        ])
      client
  in
  let* _ =
    Operation.inject
      ~error:previously_revealed_rex
      ~dont_wait:true
      op_previously_revealed
      client_tezlink
  in

  (* case unsupported manager *)
  let* op_not_supported =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~source:Constant.bootstrap1
            (update_consensus_key ~public_key:unknown.public_key ());
        ]
        client)
  in
  let unsupported_rex =
    rex "evm_node.dev.tezlink.unsupported_manager_operation"
  in
  let* _ =
    Operation.inject
      ~error:unsupported_rex
      ~dont_wait:true
      op_not_supported
      client_tezlink
  in

  (* case batch with two sources *)
  let counter = 2 in
  let* op_two_sources =
    Operation.Manager.(
      operation
        [
          make ~fee:1000 ~counter ~source:Constant.bootstrap1 (transfer ());
          make
            ~fee:1000
            ~counter:(counter + 1)
            ~source:Constant.bootstrap2
            (transfer ());
        ]
        client)
  in
  let two_sources_rex =
    rex
      "Inconsistent sources in operation batch. All operations in a batch must \
       have the same source."
  in
  let* _ =
    Operation.inject
      ~error:two_sources_rex
      ~dont_wait:true
      op_two_sources
      client_tezlink
  in

  (* case balance too low *)
  let counter = 2 in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let balance = Tez.to_mutez balance in
  let fee = 10000 in
  (* the second transfer should put us over the balance *)
  let fee2 = balance - fee + 1 in
  let* op_balance_too_low =
    Operation.Manager.(
      operation
        [
          make ~fee ~counter ~source:Constant.bootstrap1 (transfer ~amount:1 ());
          make
            ~fee:fee2
            ~counter:(counter + 1)
            ~source:Constant.bootstrap1
            (transfer ~amount:1 ());
        ]
        client)
  in
  let balance_too_low_rex =
    rex
      (sf "Balance of contract %s too low" Constant.bootstrap1.public_key_hash)
  in
  let* _ =
    Operation.inject
      ~error:balance_too_low_rex
      ~dont_wait:true
      op_balance_too_low
      client_tezlink
  in

  (* case counter in the past *)
  let* op_wrong_counter =
    Operation.Manager.(
      operation
        [make ~fee:1000 ~counter:0 ~source:Constant.bootstrap1 (transfer ())]
        client)
  in
  let wrong_counter_rex =
    rex
      (sf
         "Counter %d already used for contract %s"
         0
         Constant.bootstrap1.public_key_hash)
  in
  let* _ =
    Operation.inject
      ~error:wrong_counter_rex
      ~dont_wait:true
      op_wrong_counter
      client_tezlink
  in

  (* case batch with non consecutive counters *)
  let counter = 2 in
  let* op_non_consecutive_counter =
    Operation.Manager.(
      operation
        [
          make ~fee:1000 ~counter ~source:Constant.bootstrap1 (transfer ());
          make
            ~fee:1000
            ~counter:(counter + 4)
            ~source:Constant.bootstrap1
            (transfer ());
        ]
        client)
  in
  let non_consecutive_counter_rex =
    rex
      (sf
         "Non-consecutive counters for source %s: jumped from %d to %d"
         Constant.bootstrap1.public_key_hash
         2
         6)
  in
  let* _ =
    Operation.inject
      ~error:non_consecutive_counter_rex
      ~dont_wait:true
      op_non_consecutive_counter
      client_tezlink
  in

  (* case wrong signer *)
  let* op_wrong_signer =
    Operation.Manager.(
      operation
        ~signer:Constant.bootstrap2
        [make ~fee:1000 ~counter:2 ~source:Constant.bootstrap1 (transfer ())]
        client)
  in
  let wrong_signer_rex = rex "The operation signature is invalid" in
  let* _ =
    Operation.inject
      ~error:wrong_signer_rex
      ~dont_wait:true
      op_wrong_signer
      client_tezlink
  in

  let no_signer_rex = rex "The operation requires a signature" in
  let* _ =
    Operation.inject
      ~error:no_signer_rex
      ~dont_wait:true
      ~signature:Tezos_crypto.Signature.zero
      op_wrong_signer
      client_tezlink
  in

  let hard_gas_limit_per_operation = 3_000_000 in
  let hard_gas_limit_per_block = 3_000_000 in

  (* case wrong gas limit *)
  let gas_limit_too_high_rex =
    rex "A transaction tried to exceed the hard limit on gas"
  in
  let* op_wrong_gas_limit =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~gas_limit:(hard_gas_limit_per_operation + 1)
            ~counter:2
            ~source:Constant.bootstrap1
            (transfer ());
        ]
        client)
  in
  let* _ =
    Operation.inject
      ~error:gas_limit_too_high_rex
      ~dont_wait:true
      op_wrong_gas_limit
      client_tezlink
  in
  (* gas limit upper bound is higher for batches, but still needs to fit in a
     block *)
  let* batch_wrong_gas_limit =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~gas_limit:(hard_gas_limit_per_operation - 1)
            ~counter:2
            ~source:Constant.bootstrap1
            (transfer ());
          make
            ~fee:1000
            ~gas_limit:(hard_gas_limit_per_operation - 1)
            ~counter:3
            ~source:Constant.bootstrap1
            (transfer ());
        ]
        client)
  in
  let* _ =
    Operation.inject
      ~error:gas_limit_too_high_rex
      ~dont_wait:true
      batch_wrong_gas_limit
      client_tezlink
  in

  (* case tz4 *)
  let* tz4 = Client.gen_and_show_keys ~sig_alg:"bls" client_tezlink in
  let* op_not_supported =
    Operation.Manager.(
      operation
        [make ~fee:1000 ~source:Constant.bootstrap1 (transfer ~dest:tz4 ())]
        client)
  in
  let unsupported_rex = rex "evm_node.dev.tezlink.bls_is_not_allowed" in
  let* _ =
    Operation.inject
      ~error:unsupported_rex
      ~dont_wait:true
      op_not_supported
      client_tezlink
  in

  (* case gas limit of batch too high*)
  let not_quite_too_high = hard_gas_limit_per_block - 1000 in
  let* op_wrong_gas_limit2 =
    Operation.Manager.(
      operation
        [
          make
            ~fee:1000
            ~gas_limit:not_quite_too_high
            ~counter:2
            ~source:Constant.bootstrap1
            (transfer ());
          make
            ~fee:1000
            ~gas_limit:not_quite_too_high
            ~counter:3
            ~source:Constant.bootstrap1
            (transfer ());
        ]
        client)
  in
  let* _ =
    Operation.inject
      ~error:gas_limit_too_high_rex
      ~dont_wait:true
      op_wrong_gas_limit2
      client_tezlink
  in
  unit

let test_prevalidation_gas_limit_lower_bound =
  register_tezosx_test
    ~title:
      "Test michelson runtime prevalidation of operation gas limit lower bound"
    ~tags:["kernel"; "prevalidation"; "gas_limit"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let new_test () =
    (* Print a banner to delimitate a test. Usefull here because the error
       messages can't be customized to easily find which test fails.*)
    Tezt.Log.(
      info
        ~color:Color.(bold ++ FG.green)
        "******************************************************************") ;
    Tezt.Log.(info ~color:Color.(bold ++ FG.green) ~prefix:"NEW TEST")
  in

  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  let build_and_inject ?error operations =
    let* op = Operation.Manager.operation operations client in
    Operation.inject ~dont_wait:true ?error op client_tezlink
  in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () = produce_block_and_wait_for ~sequencer 4 in

  (* **** hardcoded cost values, from protocol **** *)
  (* base cost for manager operations*)
  let manager_cost = 100 in
  (* A transfer adds the cost of decoding empty micheline. That cost 10 milligas, but we can't be that precise so we round up.  *)
  let minimum_transfer_cost = manager_cost + 1 in

  new_test () "Test 1: 0 gas is not high enough for anything" ;
  let* _ =
    build_and_inject
      ~error:(rex "gas_exhausted.operation")
      Operation.Manager.
        [make ~gas_limit:0 ~source:Constant.bootstrap1 (transfer ())]
  in

  new_test () "Test 2: manager cost is not enough for a single operation" ;
  (* minimum cost, without any gas for signature verification should be rejected. *)
  let* _ =
    build_and_inject
      ~error:(rex "gas_exhausted.operation")
      Operation.Manager.
        [
          make
            ~gas_limit:minimum_transfer_cost
            ~source:Constant.bootstrap1
            (transfer ());
        ]
  in

  (* We want to avoid hardcoding signature cost, which depends on the size of
     the encoded transaction and protocol constant. We take advantage of the
     batch strategy: the cost of signature verification is the responsability
     of the first operation in the batch. So as long as we send a batch with a
     valid first transaction, we can ignore the signature cost for the other
     operations.

     Note that it doesn't work for testing reveal costs, because the reveal
     _must_ be the first operation. We don't have any specific logic for reveal
     though: because we don't have tz4 the only cost of a reveal is manager
     cost. So we don't write a specific test.
  *)
  new_test
    ()
    "Sanity check: minimum cost is enough if signature cost is already \
     accounted for" ;
  (* Sanity check for transfer cost: we check that the hardcoded minimum is
     correct. *)
  let* (`OpHash op_just_enough_hash) =
    build_and_inject
      Operation.Manager.
        [
          make
            ~counter:1
            ~gas_limit:2000
            ~source:Constant.bootstrap1
            (transfer ());
          make
            ~counter:2
            ~gas_limit:minimum_transfer_cost
            ~source:Constant.bootstrap1
            (transfer ());
        ]
  in
  let* () = produce_block_and_wait_for ~sequencer 5 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"5"
      ~expected:[op_just_enough_hash]
      ()
  in
  let* _ =
    Client.get_receipt_for ~operation:op_just_enough_hash client_tezlink
  in

  let not_enough_gas_for_deserializing =
    rex
      "Command failed: Gas limit was not high enough to deserialize the \
       transaction parameters or origination script code or initial storage \
       etc., making the operation impossible to parse within the provided gas \
       bounds."
  in
  new_test () "Test 3: transfers need to set up gas for decoding parameters" ;
  (* Check that a transfer with just manager cost is rejected when checking the
     cost of the parameter. *)
  let* _ =
    build_and_inject
      ~error:not_enough_gas_for_deserializing
      Operation.Manager.
        [
          make
            ~counter:3
            ~gas_limit:2000
            ~source:Constant.bootstrap1
            (transfer ());
          make
            ~counter:4
            ~gas_limit:manager_cost
            ~source:Constant.bootstrap1
            (transfer ());
        ]
  in

  new_test
    ()
    "Test 4: originations need to set up gas for decoding code and storage" ;
  (* Check that an origination with just manager cost is rejected when checking
     cost of code and storage. *)
  let* _ =
    build_and_inject
      ~error:not_enough_gas_for_deserializing
      Operation.Manager.
        [
          make
            ~counter:3
            ~gas_limit:2000
            ~source:Constant.bootstrap1
            (transfer ());
          make
            ~counter:4
            ~gas_limit:manager_cost
            ~source:Constant.bootstrap1
            (origination ~code:(`A []) ~init_storage:(`A []) ());
        ]
  in

  unit

let test_validation_gas_limit =
  register_tezosx_test
    ~title:"Test michelson runtime validation of block gas limit"
    ~tags:["kernel"; "validation"; "gas_limit"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  let hard_gas_limit_per_block = 3_000_000 in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () = produce_block_and_wait_for ~sequencer 4 in

  let almost_half_block = (hard_gas_limit_per_block / 2) - 1 in

  (* Sanity check: can fit two op in a blueprint *)
  (* fee = 20_000 mutez covers execution gas fee (gas_limit * 10 * base_fee / 10^12
     ≤ 1_499_999 * 10 * 10^9 / 10^12 ≈ 15_000 mutez) so operations are not
     rejected for insufficient fees before the gas limit check. *)
  let fee = 20_000 in
  let* (`OpHash op1) =
    Operation.inject_transfer
      ~counter:1
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~fee
      ~gas_limit:almost_half_block
      client_tezlink
  in
  let* (`OpHash op2) =
    Operation.inject_transfer
      ~counter:2
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~fee
      ~gas_limit:almost_half_block
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 5 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"5"
      ~expected:[op1; op2]
      ()
  in

  (* check: with just a bit more gas_limit two op don't fit in a blueprint *)
  let* (`OpHash op3) =
    Operation.inject_transfer
      ~counter:3
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~fee
      ~gas_limit:(almost_half_block + 100)
      client_tezlink
  in
  let* (`OpHash op4) =
    Operation.inject_transfer
      ~counter:4
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~fee
      ~gas_limit:(almost_half_block + 100)
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 6 in
  let* () = produce_block_and_wait_for ~sequencer 7 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"6"
      ~expected:[op3]
      ()
  in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"7"
      ~expected:[op4]
      ()
  in
  unit

let test_validation_counter =
  register_tezosx_test
    ~title:"Test michelson runtime validation of counters"
    ~tags:["kernel"; "validation"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 1 in
  let* () = produce_block_and_wait_for ~sequencer 2 in
  let* (`OpHash op1) =
    Operation.inject_transfer
      ~counter:1
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      client_tezlink
  in
  let* (`OpHash op2) =
    Operation.inject_transfer
      ~counter:2
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      client_tezlink
  in
  let* (`OpHash _) =
    Operation.inject_transfer
      ~counter:2
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      client_tezlink
  in
  let* (`OpHash op3) =
    Operation.inject_transfer
      ~counter:1
      ~source:Constant.bootstrap2
      ~dest:Constant.bootstrap1
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 5 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"5"
      ~expected:[op1; op2; op3]
      ()
  in
  unit

let test_validation_balance =
  register_tezosx_test
    ~title:"Test michelson runtime validation of balance"
    ~tags:["kernel"; "validation"; "balance"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  let new_account = Constant.bootstrap3 in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 2 in
  let* () = produce_block_and_wait_for ~sequencer 3 in

  (* setup *)
  let amount = 100_000_000 in
  let* _ =
    Operation.inject_transfer
      ~counter:1
      ~source:Constant.bootstrap1
      ~dest:new_account
      ~amount
      ~gas_limit:5000
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 5 in
  let* reveal =
    Operation.Manager.(
      operation [make ~fee:1000 ~source:new_account (reveal new_account ())])
      client_tezlink
  in
  let* (`OpHash op_reveal) =
    Operation.inject ~dont_wait:true reveal client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 6 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"6"
      ~expected:[op_reveal]
      ()
  in

  (* sanity check *)
  let* balance =
    Client.get_balance_for ~account:new_account.alias client_tezlink
  in
  Check.(
    (balance > Tez.zero)
      Tez.typ
      ~error_msg:
        (sf
           "Expected %s to have some funds but got %%L"
           new_account.public_key_hash)) ;

  (* test that amount isn't taken into account for insertion *)
  let* (`OpHash big_transfer) =
    Operation.inject_transfer
      ~counter:2
      ~source:new_account
      ~dest:Constant.bootstrap2
      ~amount:(amount + 10)
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 7 in
  (* big transfer should go in but fail *)
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"7"
      ~expected:[big_transfer]
      ()
  in
  let* receipt =
    Client.get_receipt_for ~operation:big_transfer client_tezlink
  in
  Check.(
    (receipt =~ rex ".*This operation FAILED.*")
      ~error_msg:"Operation should have failed. Receipt was %L") ;

  (* test that balance is checked against fees:
     - first should be included, not second *)
  let half_amount = amount / 2 in

  let* (`OpHash big_but_in) =
    Operation.inject_transfer
      ~counter:3
      ~source:new_account
      ~dest:Constant.bootstrap2
      ~fee:half_amount
      client_tezlink
  in

  (* Following transfer won't be in the block because the previous transactions
     will decrease the balance too much, but should still be injected as on it's
     own it's valid. It's be rejected before the blueprint is produced. *)
  let* (`OpHash _out) =
    Operation.inject_transfer
      ~counter:4
      ~source:new_account
      ~dest:Constant.bootstrap2
      ~fee:half_amount
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 8 in
  let* () =
    check_operations
      ~__LOC__
      ~client:client_tezlink
      ~block:"8"
      ~expected:[big_but_in]
      ()
  in
  let* () = produce_block_and_wait_for ~sequencer 9 in
  let* () =
    check_operations ~__LOC__ ~client:client_tezlink ~block:"9" ~expected:[] ()
  in
  unit

(** Sends a Michelson transfer with zero fees and verifies it is rejected.
    [Client.spawn_transfer] triggers a simulation that rejects the operation
    with [Insufficient_fees]. *)
let test_insufficient_da_fee =
  register_tezosx_test
    ~title:"Michelson transfer with insufficient DA fee is rejected"
    ~tags:["kernel"; "validation"; "da_fee"; "insufficient"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~da_fee:(Wei.of_eth_int 4)
  (* For da fees, anything superior to zero works for testing. *)
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let process =
    Client.spawn_transfer
      ~endpoint
      ~fee:Tez.zero
      ~force:true
      ~gas_limit:10000
      ~storage_limit:10000
      ~burn_cap:Tez.one
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  (* Rejected by prevalidation. *)
  let* err = Process.check_and_read_stderr ~expect_failure:true process in
  Check.(err =~ rex "evm_node.dev.insufficient_fees")
    ~error_msg:"Prevalidation should have failed with %R but got %L" ;
  unit

let test_da_fee_credited_to_pool =
  let sequencer_pool_address = "0xb7a97043983f24991398e5a82f63f4c58a417185" in
  let da_fee_eth_int = 4 in
  register_tezosx_test
    ~title:"DA fees are credited to sequencer pool address"
    ~tags:["kernel"; "da_fee"; "sequencer_pool_address"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~da_fee:(Wei.of_eth_int da_fee_eth_int)
    ~sequencer_pool_address
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Check initial balance of pool address is zero. *)
  let*@ initial_balance =
    Rpc.get_balance ~address:sequencer_pool_address sequencer
  in
  Check.((Wei.zero = initial_balance) Wei.typ)
    ~error_msg:"Initial balance of the sequencer pool address should be zero" ;
  (* Perform a successful transfer with fee > 0. *)
  let fee = Tez.of_int 650 in
  let* () =
    Client.transfer
      ~endpoint
      ~amount:Tez.one
      ~fee
      ~gas_limit:10000
      ~storage_limit:10000
      ~fee_cap:fee
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      client
  in
  let*@ _ = produce_block sequencer in
  (* Check balance of pool address increased.
     In this scenario, DA fees per byte is [da_fee_eth_int=4],
     and the size of the operation is [raw_op_size=154].
     [raw_op_size] × [da_fee_eth_int] = 616 *)
  let raw_op_size = 154 in
  let expected_da_fees = Tez.of_int (raw_op_size * da_fee_eth_int) in
  let*@ balance = Rpc.get_balance ~address:sequencer_pool_address sequencer in
  Check.((Wei.of_tez expected_da_fees = balance) Wei.typ)
    ~error_msg:"Balance of the sequencer pool address should be %L but go %R" ;
  unit

(** Tests the interaction between DA fees and fee estimation.
    The client internally simulates with fee=0 to estimate gas, so the
    kernel and node must skip the DA fee check during simulation. The fee
    is then patched using the DA fee before injection.
    First, verifies that an explicit [--minimal-nanotez-per-byte] below
    the DA fee (1000 nanotez/byte vs 4 mutez/byte) is rejected with
    [insufficient_fees]. Then, verifies that without the flag, the client
    fetches the correct DA fee from the mempool filter RPC and the
    transfer succeeds. *)
let test_simulation_with_da_fee =
  register_tezosx_test
    ~title:"Michelson transfer simulation succeeds with DA fees enabled"
    ~tags:["kernel"; "validation"; "da_fee"; "simulation"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~da_fee:(Wei.of_string "1000000000000000")
  (* 1000 mutez/byte = 1000 * 10^12 wei *)
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* 1000 nanotez/byte = 1 mutez/byte (L1 default), below the DA fee of
     1000 mutez/byte: the total fee doesn't cover the DA cost. *)
  let process =
    Client.spawn_transfer
      ~endpoint
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      ~minimal_nanotez_per_byte:1000
      client
  in
  let* err = Process.check_and_read_stderr ~expect_failure:true process in
  Check.(err =~ rex "evm_node.dev.insufficient_fees")
    ~error_msg:"Expected insufficient_fees error with %R but got %L" ;

  (* Without [--minimal-nanotez-per-byte], the client fetches the DA fee
     from the node's [GET .../mempool/filter] RPC (1000000 nanotez/byte here)
     and uses it to compute a sufficient fee. *)
  let* () =
    Client.transfer
      ~endpoint
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = Rpc.produce_block sequencer in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.((Tez.to_mutez balance = Tez.(to_mutez one)) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

(** Tests that Tezos operations feed the gas backlog.
    A single Michelson transfer consumes ~1,039 gas (1,039,000 milligas).
    With DEFAULT_MICHELSON_TO_EVM_GAS_MULTIPLIER = 1,000,000, each adds ~1,039,000,000
    to cumulative_execution_gas, which exceeds the EIP-1559 TOLERANCE (135,000,000)
    and pushes baseFeePerGas upward without overflowing the u64 backlog. *)
let test_michelson_gas_backlog =
  (* A single Michelson transfer costs ~1,039 gas (1,039,000 milligas / 1000).
     With a multiplier of 1,000,000 the EVM-equivalent gas is ~1,039,000,000
     which exceeds the EIP-1559 TOLERANCE (135,000,000), pushing baseFeePerGas
     upward without overflowing the u64 backlog. *)
  register_tezosx_test
    ~title:"Tezos operations update EVM gas backlog"
    ~tags:["kernel"; "gas"; "backlog"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~michelson_to_evm_gas_multiplier:1_000_000L
  @@ fun {sequencer; _} _protocol ->
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  let* initial_result = Rpc.get_block_by_number ~block:"latest" sequencer in
  let initial_base_fee =
    match initial_result with
    | Ok block -> block.Block.baseFeePerGas
    | Error _ -> Test.fail "Couldn't retrieve latest block"
  in
  (* Fee and gas_limit are auto-estimated by the client.
     fee_cap must be high enough to cover the large execution gas fees
     induced by multiplier = 1,000,000. *)
  let fee_cap = Tez.of_mutez_int 1_100_000_000 in
  let* () =
    Client.transfer
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~fee_cap
      ~burn_cap:Tez.one
      client_tezlink
  in
  (* Produce block containing the transfer, then another whose
     baseFeePerGas reflects the updated backlog. *)
  let* () =
    repeat 2 (fun () ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ new_block = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((new_block.Block.baseFeePerGas > initial_base_fee) int64)
    ~error_msg:"baseFeePerGas should have increased: expected > %R, got %L" ;
  unit

(** Tests that the gas backlog accounts for the actual gas consumed by failed
    operations — not zero and not the full gas_limit.

    Deploys two contracts and calls both with the same [gas_limit]:
    - [fail_on_false.tz] with [False]: FAILWITHs almost immediately, consuming
      very little gas.
    - [loop.tz] with 1_500_000 iterations: runs until gas exhaustion,
      consuming the entire gas budget.

    After each call, we measure how much [baseFeePerGas] increased. Since both
    operations use the same [gas_limit], charging the full limit (or zero) would
    produce equal increases. Instead, the gas-exhausting operation should produce
    a significantly larger increase than the early FAILWITH, confirming that
    actual consumed gas is what feeds the backlog. *)
let test_michelson_gas_backlog_on_failed_op =
  register_tezosx_test
    ~title:"Failed Tezos operations update EVM gas backlog"
    ~tags:["kernel"; "gas"; "backlog"; "failed"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~michelson_to_evm_gas_multiplier:1_000_000L
  @@ fun {sequencer; _} protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in
  (* Use fixed timestamps so the backlog decay is deterministic
     (independent of wall-clock / CI speed). *)
  let next_timestamp =
    let t = ref 1 in
    fun () ->
      let ts = sf "2050-01-01T00:00:%02dZ" !t in
      incr t ;
      ts
  in
  (* Deploy fail_on_false.tz *)
  let* fail_contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias:"fail_on_false"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:
        Michelson_script.(
          find ["mini_scenarios"; "fail_on_false"] protocol |> path)
      ~burn_cap:Tez.one
      client_tezlink
  in
  let*@ _ = Rpc.produce_block ~timestamp:(next_timestamp ()) sequencer in
  (* Deploy loop.tz *)
  let* loop_contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias:"loop"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:Michelson_script.(find ["mini_scenarios"; "loop"] protocol |> path)
      ~burn_cap:Tez.one
      client_tezlink
  in
  let*@ _ = Rpc.produce_block ~timestamp:(next_timestamp ()) sequencer in
  let gas_limit = 10_000 in
  let get_base_fee () =
    let* result = Rpc.get_block_by_number ~block:"latest" sequencer in
    match result with
    | Ok block -> return block.Block.baseFeePerGas
    | Error _ -> Test.fail "Couldn't retrieve latest block"
  in
  let produce_blocks () =
    repeat 2 (fun () ->
        let*@ _ = Rpc.produce_block ~timestamp:(next_timestamp ()) sequencer in
        unit)
  in
  (* Fee must cover execution gas costs. 15 tez is enough for gas_limit = 10,000. *)
  let fee = Tez.of_mutez_int 15_000_000 in
  let fee_cap = Tez.of_mutez_int 20_000_000 in
  (* Both operations below are guaranteed to fail:
     - fail_on_false.tz + False = immediate FAILWITH
     - loop.tz + 1,500,000 iterations with gas_limit=10,000 = gas exhaustion
     ~force:true is needed because the client simulation already rejects them. *)
  (* 1. Fail early: FAILWITH almost immediately, very little gas consumed *)
  let* base_fee_0 = get_base_fee () in
  let* () =
    Client.transfer
      ~endpoint
      ~force:true
      ~amount:Tez.zero
      ~fee
      ~fee_cap
      ~gas_limit
      ~storage_limit:0
      ~giver:Constant.bootstrap1.alias
      ~receiver:fail_contract
      ~arg:"False"
      ~burn_cap:Tez.one
      client_tezlink
  in
  let* () = produce_blocks () in
  let* base_fee_1 = get_base_fee () in
  let delta_early = Int64.sub base_fee_1 base_fee_0 in
  (* 2. Fail late: loop until gas exhaustion, consuming the entire gas budget *)
  let* () =
    Client.transfer
      ~endpoint
      ~force:true
      ~amount:Tez.zero
      ~fee
      ~fee_cap
      ~gas_limit
      ~storage_limit:0
      ~giver:Constant.bootstrap1.alias
      ~receiver:loop_contract
      ~arg:"1500000"
      ~burn_cap:Tez.one
      client_tezlink
  in
  let* () = produce_blocks () in
  let* base_fee_2 = get_base_fee () in
  let delta_late = Int64.sub base_fee_2 base_fee_1 in
  (* Not (A): early failure still consumes some gas *)
  Check.((delta_early > 0L) int64)
    ~error_msg:
      "baseFeePerGas should increase after early-failing op: delta = %L" ;
  (* Not (B): gas-exhausting op consumes ~10,000 gas while early FAILWITH
     consumes ~100 gas, so the expected ratio is ~100x. We use 10x as a
     conservative threshold that still clearly rules out equal deltas
     (which would indicate gas_limit is charged instead of actual gas). *)
  Check.((delta_late > Int64.mul 10L delta_early) int64)
    ~error_msg:
      "Gas-exhausting op should bump baseFee much more than early FAILWITH \
       (delta_late = %L, 10 * delta_early = %R). Equal deltas would mean \
       gas_limit is charged instead of actual consumed gas." ;
  unit

(** Tests that Michelson operations in TezosX validate execution gas fees.
    With base_fee_per_gas = 10^9 (1 Gwei) and MICHELSON_TO_EVM_GAS_MULTIPLIER = 10,
    gas_limit = 500_000 requires execution gas fee =
    500_000 * 10 * 10^9 / 10^12 = 5_000 mutez.

    A fee of 1_000 mutez does not cover execution gas.
    A fee of 10_000 mutez covers execution gas: the operation is included. *)
let test_michelson_execution_gas_fee =
  register_tezosx_test
    ~title:"Michelson transfer validates execution gas fee"
    ~tags:["kernel"; "validation"; "execution"; "gas"; "fee"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  (* Fee = 1_000 mutez: below execution gas cost → rejected at
     simulation. *)
  let process =
    Client.spawn_transfer
      ~amount:Tez.one
      ~fee:(Tez.of_mutez_int 1000)
      ~gas_limit:500_000
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~endpoint
      client
  in
  let* err = Process.check_and_read_stderr ~expect_failure:true process in
  Check.(err =~ rex "evm_node.dev.insufficient_fees")
    ~error_msg:"Expected insufficient_fees error with %R but got %L" ;
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () =
    check_operations ~__LOC__ ~client ~endpoint ~block:"3" ~expected:[] ()
  in
  (* Fee = 10_000 mutez: covers execution gas → included. *)
  let* () =
    Client.transfer
      ~amount:Tez.one
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap1.alias
      ~fee:(Tez.of_mutez_int 10_000)
      ~gas_limit:500_000
      ~endpoint
      client
  in
  let* () = produce_block_and_wait_for ~sequencer 4 in
  unit

(** Test that gas exhaustion is handled correctly.

    Originates [loop.tz] which loops [n] times, then calls it with
    [n = 150_000] and [gas_limit = 10_000] gas units.  Each iteration
    costs roughly 0.1 gas unit, so 150k iterations need ~15k gas but
    only 10k are available.  The operation is included in the block
    but fails with gas exhaustion, and the fee is fully consumed. *)
let test_michelson_gas_exhaustion =
  register_tezosx_test
    ~title:"Michelson gas exhaustion"
    ~tags:["gas"; "exhaustion"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let script_path =
    Michelson_script.(find ["mini_scenarios"; "loop"] protocol |> path)
  in
  let* contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias:"loop"
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:script_path
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = Rpc.produce_block sequencer in
  let* balance_before =
    Client.get_balance_for
      ~endpoint
      ~account:Constant.bootstrap1.public_key_hash
      client
  in
  let fee = 10_000 in
  (* [~force:true] bypasses client-side simulation which would reject the
     operation before injection.  The node prevalidator accepts it (gas_limit
     and fee are valid) and the failure only happens during block execution. *)
  let* () =
    Client.transfer
      ~endpoint
      ~force:true
      ~amount:Tez.zero
      ~fee:(Tez.of_mutez_int fee)
      ~gas_limit:10_000
      ~storage_limit:0
      ~giver:Constant.bootstrap1.alias
      ~receiver:contract
      ~arg:"150000"
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = Rpc.produce_block sequencer in
  (* Verify the operation was included but failed. *)
  let* operations =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_block_operations ()
  in
  let op_result =
    JSON.(
      operations |=> 3 |=> 0 |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  let status = JSON.(op_result |-> "status" |> as_string) in
  Check.(
    (status = "failed")
      string
      ~error_msg:"Expected operation to fail (gas exhaustion), got status %L") ;
  (* Verify the error is specifically gas exhaustion. *)
  let error_message =
    JSON.(op_result |-> "errors" |=> 0 |-> "error_message" |> as_string)
  in
  Check.(
    (error_message =~ rex "Gas_exhaustion")
      ~error_msg:"Expected Gas_exhaustion error, got %L") ;
  let* balance_after =
    Client.get_balance_for
      ~endpoint
      ~account:Constant.bootstrap1.public_key_hash
      client
  in
  let consumed = Tez.to_mutez balance_before - Tez.to_mutez balance_after in
  Check.(
    (consumed = fee)
      int
      ~error_msg:"Expected %R mutez consumed (fee only), got %L") ;
  unit

(* Tests that the [/mempool/filter] RPC returns the expected
    [minimal_nanotez_per_gas_unit] and [minimal_nanotez_per_byte] computed
    from the kernel's [base_fee_per_gas] and [michelson_to_evm_gas_multiplier]
    instead of the protocol defaults.

    With [base_fee_per_gas = 10^9 Wei] (default) and
    [michelson_to_evm_gas_multiplier = 25]:
      [nanotez_per_gas = wei_to_nanotez(10^9) * 25 = 1 * 25 = 25]

    With [da_fee = 5 * 10^15 Wei] (5000 mutez/byte), we expect
      [nanotez_per_byte = wei_to_nanotez(5 * 10^15) = 5_000_000] *)
let test_mempool_filter_fields =
  register_tezosx_test
    ~title:"mempool/filter RPC returns Michelson base_fee and DA fee"
    ~tags:["rpc"; "mempool"; "filter"; "fee"]
    ~da_fee:(Wei.of_string "5000000000000000")
    ~michelson_to_evm_gas_multiplier:25L
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* json =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_mempool_filter ~include_default:true ()
  in
  (* Michelson nanotez_per_gas = wei_to_nanotez(base_fee) * multiplier
     = (10^9 / 10^9) * 25 = 25, encoded as ["25", "1"]. *)
  let michelson_nanotez_per_gas =
    JSON.(json |-> "minimal_nanotez_per_gas_unit" |> as_list)
  in
  Check.(
    (List.map JSON.as_string michelson_nanotez_per_gas = ["25"; "1"])
      (list string)
      ~error_msg:"Expected minimal_nanotez_per_gas_unit = %R but got %L") ;
  (* nanotez_per_byte = wei_to_nanotez(da_fee)
     = 5 * 10^15 / 10^9 = 5_000_000, encoded as ["5000000", "1"]. *)
  let nanotez_per_byte =
    JSON.(json |-> "minimal_nanotez_per_byte" |> as_list)
  in
  Check.(
    (List.map JSON.as_string nanotez_per_byte = ["5000000"; "1"])
      (list string)
      ~error_msg:"Expected minimal_nanotez_per_byte = %R but got %L") ;
  unit

(* ---- Gas refund test helpers ----

   All gas refund tests use:
     fee = 1 tez,  gas_limit = 10_000
   With defaults (base_fee_per_gas = 10^9, multiplier = 10):
     execution_gas_fees = gas_to_mutez(10^9, 10, 10_000) = 100 mutez. *)

(** Execute [f], produce a block, return mutez consumed by bootstrap1. *)
let gas_refund_measure_consumed ~endpoint ~sequencer client f =
  let balance client =
    Client.get_balance_for
      ~endpoint
      ~account:Constant.bootstrap1.public_key_hash
      client
  in
  let* before = balance client in
  let* () = f () in
  let*@ _ = Rpc.produce_block sequencer in
  let* after = balance client in
  Check.((Tez.to_mutez before >= Tez.to_mutez after) int)
    ~error_msg:"balance increased unexpectedly (before=%L, after=%R)" ;
  return (Tez.to_mutez before - Tez.to_mutez after)

let gas_refund_originate ~endpoint ~sequencer ~client ~protocol scripts ~alias
    ~init =
  let script_path = Michelson_script.(find scripts protocol |> path) in
  let* contract =
    Client.originate_contract
      ~endpoint
      ~amount:Tez.zero
      ~alias
      ~src:Constant.bootstrap1.public_key_hash
      ~init
      ~prg:script_path
      ~burn_cap:Tez.one
      client
  in
  let*@ _ = Rpc.produce_block sequencer in
  return contract

let gas_refund_last_op_status ~endpoint client =
  let* operations =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_block_operations ()
  in
  return
    JSON.(
      operations |=> 3 |=> 0 |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result" |-> "status" |> as_string)

(* ---- Gas refund test scenarios ---- *)

(** Gas refund on a successful implicit transfer.
    enabled:  consumed < amount + fee (refund).
    disabled: consumed = amount + fee (no refund). *)
let test_gas_refund_on_transfer ~enable_refund =
  let label = if enable_refund then "enabled" else "disabled" in
  register_tezosx_test
    ~title:(sf "Gas refund on transfer (%s)" label)
    ~tags:["gas_refund"; "transfer"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
    ~enable_michelson_gas_refund:enable_refund
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let amount = Tez.one in
  let fee = Tez.one in
  let gas_limit = 10_000 in
  let* consumed =
    gas_refund_measure_consumed ~endpoint ~sequencer client (fun () ->
        Client.transfer
          ~endpoint
          ~amount
          ~fee
          ~gas_limit
          ~giver:Constant.bootstrap1.alias
          ~receiver:Constant.bootstrap2.alias
          ~burn_cap:Tez.one
          client)
  in
  let baseline = Tez.to_mutez amount + Tez.to_mutez fee in
  Log.info "consumed=%d baseline=%d refund=%b" consumed baseline enable_refund ;
  (if enable_refund then
     Check.(
       (consumed < baseline)
         int
         ~error_msg:
           "Expected consumed (%L) < amount + fee (%R), gas refund should occur")
   else
     Check.(
       (consumed = baseline)
         int
         ~error_msg:
           "Expected consumed (%L) = amount + fee (%R), no refund expected")) ;
  unit

(** Gas refund on a FAILWITH operation.
    enabled:  consumed < fee (refund even on failure).
    disabled: consumed = fee (no refund). *)
let test_gas_refund_on_failwith ~enable_refund =
  let label = if enable_refund then "enabled" else "disabled" in
  register_tezosx_test
    ~title:(sf "Gas refund on FAILWITH (%s)" label)
    ~tags:["gas_refund"; "failwith"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~enable_michelson_gas_refund:enable_refund
  @@ fun {sequencer; client; _} protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* contract =
    gas_refund_originate
      ~endpoint
      ~sequencer
      ~client
      ~protocol
      ["mini_scenarios"; "fail_on_false"]
      ~alias:"fail_on_false"
      ~init:"Unit"
  in
  let fee = Tez.one in
  let gas_limit = 10_000 in
  let* consumed =
    gas_refund_measure_consumed ~endpoint ~sequencer client (fun () ->
        Client.transfer
          ~endpoint
          ~force:true
          ~amount:Tez.zero
          ~fee
          ~gas_limit
          ~storage_limit:0
          ~giver:Constant.bootstrap1.alias
          ~receiver:contract
          ~arg:"False"
          ~burn_cap:Tez.one
          client)
  in
  let* status = gas_refund_last_op_status ~endpoint client in
  Check.((status = "failed") string ~error_msg:"Expected failed, got %L") ;
  let fee_mutez = Tez.to_mutez fee in
  Log.info "consumed=%d fee=%d refund=%b" consumed fee_mutez enable_refund ;
  (if enable_refund then
     Check.(
       (consumed < fee_mutez)
         int
         ~error_msg:
           "Expected consumed (%L) < fee (%R), gas refund should occur on \
            failure")
   else
     Check.(
       (consumed = fee_mutez)
         int
         ~error_msg:"Expected consumed (%L) = fee (%R), no refund expected")) ;
  unit

(** Gas refund on out-of-gas: all gas consumed, but fee surplus is still refunded.
    consumed < fee because declared fee exceeds actual costs (DA + gas). *)
let test_gas_refund_on_out_of_gas =
  register_tezosx_test
    ~title:"Gas refund on out-of-gas (fee surplus refunded)"
    ~tags:["gas_refund"; "out_of_gas"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~enable_michelson_gas_refund:true
  @@ fun {sequencer; client; _} protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* contract =
    gas_refund_originate
      ~endpoint
      ~sequencer
      ~client
      ~protocol
      ["mini_scenarios"; "loop"]
      ~alias:"loop"
      ~init:"Unit"
  in
  let fee = Tez.one in
  let gas_limit = 10_000 in
  let* consumed =
    gas_refund_measure_consumed ~endpoint ~sequencer client (fun () ->
        Client.transfer
          ~endpoint
          ~force:true
          ~amount:Tez.zero
          ~fee
          ~gas_limit
          ~storage_limit:0
          ~giver:Constant.bootstrap1.alias
          ~receiver:contract
          ~arg:"150000"
          ~burn_cap:Tez.one
          client)
  in
  let* status = gas_refund_last_op_status ~endpoint client in
  Check.((status = "failed") string ~error_msg:"Expected failed, got %L") ;
  let fee_mutez = Tez.to_mutez fee in
  Log.info "consumed=%d fee=%d" consumed fee_mutez ;
  Check.(
    (consumed < fee_mutez)
      int
      ~error_msg:
        "Expected consumed (%L) < fee (%R), fee surplus should be refunded") ;
  unit

(** Verify gas refund balance_updates in an operation metadata.
    [expected_refund]: if [Some n], assert that a refund of exactly [n] mutez
    is present. If [None], assert that no refund entries exist.
    [source]: the source public_key_hash.
    [metadata]: the JSON metadata from contents[0].metadata. *)
let check_gas_refund_balance_updates ?expected_refund ~source ~metadata () =
  (* Parse balance_updates from metadata (fee-level, where refund appears).
     - Without refund: 2 entries (fee debit from source, credit to block fees).
     - With refund: 4 entries (fee debit/credit + refund credit/debit). *)
  let expected_bu_count = if Option.is_some expected_refund then 4 else 2 in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  Check.(
    (List.length balance_updates = expected_bu_count)
      int
      ~error_msg:"Expected %R balance_updates entries, got %L") ;
  (* Verify token conservation: sum of all changes must be zero. *)
  let total_change =
    List.fold_left
      (fun acc bu -> acc + JSON.(bu |-> "change" |> as_int))
      0
      balance_updates
  in
  Check.(
    (total_change = 0) int ~error_msg:"Balance updates do not sum to zero: %L") ;
  (* Helper: find a balance_update matching kind/contract and sign. *)
  let find_bu ~kind ?contract ~positive bus =
    List.find_opt
      (fun bu ->
        let k = JSON.(bu |-> "kind" |> as_string) in
        if k <> kind then false
        else
          let c = JSON.(bu |-> "change" |> as_int) in
          let sign_ok = if positive then c > 0 else c < 0 in
          let contract_ok =
            match contract with
            | None -> true
            | Some addr -> JSON.(bu |-> "contract" |> as_string) = addr
          in
          sign_ok && contract_ok)
      bus
  in
  (* Fee entries: debit from source (negative) and credit to block fees (positive). *)
  let fee_debit =
    find_bu ~kind:"contract" ~contract:source ~positive:false balance_updates
  in
  Check.(
    (fee_debit <> None)
      (option json)
      ~error_msg:"Missing fee debit (kind=contract, change<0) for source") ;
  let fee_credit = find_bu ~kind:"accumulator" ~positive:true balance_updates in
  Check.(
    (fee_credit <> None)
      (option json)
      ~error_msg:"Missing fee credit (kind=accumulator, change>0)") ;
  match expected_refund with
  | Some expected_refund ->
      Check.((expected_refund > 0) int ~error_msg:"Expected refund (%L) > 0") ;
      (* Refund credit: source gets +refund (kind=contract, positive). *)
      let refund_credit =
        find_bu ~kind:"contract" ~contract:source ~positive:true balance_updates
      in
      Check.(
        (refund_credit <> None)
          (option json)
          ~error_msg:
            "Missing refund credit (kind=contract, change>0) for source") ;
      let actual_refund =
        JSON.(Option.get refund_credit |-> "change" |> as_int)
      in
      Check.(
        (actual_refund = expected_refund)
          int
          ~error_msg:"Refund credit mismatch: got %L, expected %R") ;
      (* Refund debit: block fees gets -refund (kind=accumulator, negative). *)
      let refund_debit =
        find_bu ~kind:"accumulator" ~positive:false balance_updates
      in
      Check.(
        (refund_debit <> None)
          (option json)
          ~error_msg:
            "Missing refund debit (kind=accumulator, change<0) in block fees") ;
      let actual_debit =
        JSON.(Option.get refund_debit |-> "change" |> as_int)
      in
      Check.(
        (actual_debit = -expected_refund)
          int
          ~error_msg:"Refund debit mismatch: got %L, expected %R") ;
      ()
  | None ->
      (* No refund entries should exist: no positive credit to source,
         no negative debit in block fees. *)
      let spurious_credit =
        find_bu ~kind:"contract" ~contract:source ~positive:true balance_updates
      in
      Check.(
        (spurious_credit = None)
          (option json)
          ~error_msg:
            "Expected no refund credit in balance_updates (refund is disabled)") ;
      let spurious_debit =
        find_bu ~kind:"accumulator" ~positive:false balance_updates
      in
      Check.(
        (spurious_debit = None)
          (option json)
          ~error_msg:
            "Expected no refund debit in balance_updates (refund is disabled)") ;
      ()

(** Gas refund in run_operation (simulation) receipts.
    With refund enabled, the metadata-level balance_updates should contain
    4 entries: fee debit/credit + refund credit/debit, all summing to zero.
    The refund amount should match: fee - da_fees - consumed_gas_fees
    (da_fees = 0 in simulation).
    Without refund, only the 2 fee debit/credit entries should appear. *)
let test_gas_refund_in_run_operation ~enable_refund =
  let label = if enable_refund then "enabled" else "disabled" in
  register_tezosx_test
    ~title:(sf "Gas refund in run_operation (%s)" label)
    ~tags:["gas_refund"; "run_operation"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
    ~enable_michelson_gas_refund:enable_refund
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let* branch =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_block_hash ()
  in
  let* chain_id =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_chain_id ()
  in
  (* 1 tez = 1_000_000 mutez, deliberately high to produce a large refund. *)
  let fee = 1_000_000 in
  let gas_limit = 10_000 in
  let amount = 10 in
  let source = Constant.bootstrap1.public_key_hash in
  let op_json =
    `O
      [
        ( "operation",
          `O
            [
              ("branch", `String branch);
              ( "contents",
                `A
                  [
                    `O
                      [
                        ("kind", `String "transaction");
                        ("source", `String source);
                        ("fee", `String (string_of_int fee));
                        (* Counter and signature are not checked in simulation mode. *)
                        ("counter", `String "1");
                        ("gas_limit", `String (string_of_int gas_limit));
                        ("storage_limit", `String "0");
                        ("amount", `String (string_of_int amount));
                        ( "destination",
                          `String Constant.bootstrap2.public_key_hash );
                      ];
                  ] );
              (* Dummy signature: ignored in simulation mode. *)
              ( "signature",
                `String
                  "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
              );
            ] );
        ("chain_id", `String chain_id);
      ]
  in
  let* result =
    Client.RPC.call ~endpoint client
    @@ RPC.post_chain_block_helpers_scripts_run_operation (Data op_json)
  in
  let metadata = JSON.(result |-> "contents" |=> 0 |-> "metadata") in
  (* Check status is applied *)
  let status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.((status = "applied") string ~error_msg:"Expected %R but got %L") ;
  let expected_refund =
    if enable_refund then
      (* refund = fee - da_fees - consumed_gas_fees.
         da_fees = 0 in simulation, consumed_gas_fees = ceil(consumed_milligas/1000) / 100. *)
      let consumed_milligas =
        JSON.(metadata |-> "operation_result" |-> "consumed_milligas" |> as_int)
      in
      let consumed_gas = (consumed_milligas + 999) / 1000 in
      let consumed_gas_fees = consumed_gas / 100 in
      Some (fee - consumed_gas_fees)
    else None
  in
  check_gas_refund_balance_updates ?expected_refund ~source ~metadata () ;
  unit

(** Gas refund in preapply/operations (preapplication) receipts.
    Same checks as run_operation but through the preapply RPC which
    validates signatures and fees (skip_signature_check = false). *)
let test_gas_refund_in_preapply ~enable_refund =
  let label = if enable_refund then "enabled" else "disabled" in
  register_tezosx_test
    ~title:(sf "Gas refund in preapply (%s)" label)
    ~tags:["gas_refund"; "preapply"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
    ~enable_michelson_gas_refund:enable_refund
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in
  let fee = 1_000_000 in
  let gas_limit = 10_000 in
  let amount = 10 in
  let source = Constant.bootstrap1 in
  (* Fetch branch and counter from the tezlink endpoint (the kernel's view).
     mk_single_transfer would use the client's default endpoint which may
     differ from the kernel's internal state. *)
  let* branch =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_block_hash ()
  in
  let* counter =
    Client.RPC.call ~endpoint client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:source.public_key_hash
         ()
  in
  let counter = JSON.as_int counter + 1 in
  let* op =
    Operation.Manager.mk_single_transfer
      ~source
      ~fee
      ~gas_limit
      ~amount
      ~counter
      ~branch
      ~dest:Constant.bootstrap2
      client
  in
  let* signature = Operation.sign op client in
  (* Get the Tezlink protocol hash (different from Alpha). *)
  let* protocols =
    Client.RPC.call ~endpoint client @@ RPC.get_chain_block_protocols ()
  in
  let protocol_hash = JSON.(protocols |-> "protocol" |> as_string) in
  (* Build the preapply JSON: take the operation's fields, add protocol
     and signature. Operation.json returns a JSON.u (`O fields). *)
  let preapply_input =
    match Operation.json op with
    | `O fields ->
        `O
          (("protocol", `String protocol_hash)
          :: ( "signature",
               `String (Tezos_crypto.Signature.to_b58check signature) )
          :: fields)
    | _ -> Test.fail "Unexpected operation JSON format"
  in
  let* result =
    Client.RPC.call ~endpoint client
    @@ RPC.post_chain_block_helpers_preapply_operations
         ~version:"1"
         ~data:(Data (`A [preapply_input]))
         ()
  in
  (* preapply returns an array of operations, each with "contents". *)
  let metadata = JSON.(result |=> 0 |-> "contents" |=> 0 |-> "metadata") in
  let status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.((status = "applied") string ~error_msg:"Expected %R but got %L") ;
  let expected_refund =
    if enable_refund then
      let consumed_milligas =
        JSON.(metadata |-> "operation_result" |-> "consumed_milligas" |> as_int)
      in
      let consumed_gas = (consumed_milligas + 999) / 1000 in
      let consumed_gas_fees = consumed_gas / 100 in
      Some (fee - consumed_gas_fees)
    else None
  in
  check_gas_refund_balance_updates
    ?expected_refund
    ~source:source.public_key_hash
    ~metadata
    () ;
  unit

let test_gas_vs_l1 =
  register_tezosx_regression_test
    ~title:"Test michelson runtime gas vs L1 operations"
    ~tags:["kernel"; "gas"; "l1"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _ ->
  let* client_tezlink = tezlink_client_from_evm_node sequencer () in

  let get_consumed_gas operations =
    JSON.(
      operations |> geti 3 |> geti 0 |> get "contents" |> geti 0
      |> get "metadata" |> get "operation_result" |> get "consumed_milligas"
      |> as_int)
  in
  let transfer client =
    Client.transfer
      ~fee:Tez.one
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in

  (* Inject transfer via L1 *)
  let* () = transfer client in
  let* _ = Client.bake_for_and_wait client in

  let* gas_used_l1 =
    let* operations =
      Client.RPC.call client @@ RPC.get_chain_block_operations ()
    in
    Lwt.return @@ get_consumed_gas operations
  in

  (* Inject transfer via Tezlink *)
  let* () = transfer client_tezlink in
  let* _ = produce_block sequencer in
  let* gas_used_tezlink =
    let* operations =
      Client.RPC.call client_tezlink @@ RPC.get_chain_block_operations ()
    in
    Lwt.return @@ get_consumed_gas operations
  in

  Regression.capture @@ sf "Gas used L1: %d\n" gas_used_l1 ;
  Regression.capture @@ sf "Gas used Michelson runtime: %d\n" gas_used_tezlink ;

  unit

let test_node_catchup =
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = 10 in
  let catchup_cooldown = 4 in
  register_tezosx_test
    ~max_blueprints_lag
    ~max_blueprints_catchup
    ~catchup_cooldown
    ~tags:["blueprint"; "catchup"]
    ~title:"EVM node catchup with the michelson runtime"
    ~time_between_blocks:Nothing
  @@
  fun {sequencer; sc_rollup_node; client; sc_rollup_address; node; _}
      _protocol
    ->
  let*@ _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* l1_level = Node.get_level node in
  let wait_for_level_processed =
    Lwt.join
      [
        (* l1_level + 2 is seen, so l1_level is finalized *)
        (let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (l1_level + 2) in
         unit);
        (* l1_level is finalized and processed. *)
        (let* _ =
           Evm_node.wait_for_processed_l1_level sequencer ~level:l1_level
         in
         unit);
      ]
  in
  let* () =
    (* So the EVM node see the level as finalized *)
    repeat 2 (fun () ->
        let* _ = Client.bake_for_and_wait client in
        unit)
  and* () = wait_for_level_processed in

  let*@ before_level = Rpc.generic_block_number sequencer in
  let before_level = Int32.to_int before_level in
  Log.info
    "Stop the rollup node then produce a first block than won't be in the \
     rollup inbox." ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () =
    Evm_node.wait_for_blueprint_injection_failure
      ~level:(before_level + 1)
      sequencer
  in
  Log.info "Produce more blocks to create a blueprints lag." ;
  let* () =
    (* Produces L2 blocks to grow the blueprint lag but not enough to
       trigger a catchup. *)
    repeat (max_blueprints_lag - 2) (fun () ->
        let*@ _txn = produce_block sequencer in
        unit)
  in
  Log.info "Restart the rollup node." ;
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup_address []
  and* () =
    Evm_node.wait_for_rollup_node_follower_connection_acquired sequencer
  in
  Log.info "Continue to produce blocks to fills the batcher queue." ;
  let* () =
    (* 2 * max_blueprints_lag: just a random number to have a lot of
       blueprints injected. *)
    repeat (2 * max_blueprints_lag) (fun () ->
        let*@ _txn = produce_block sequencer in
        let*@ level = Rpc.generic_block_number sequencer in
        Evm_node.wait_for_blueprint_injected sequencer (Int32.to_int level))
  in
  Log.info "Produce an L1 block that triggers the catchup." ;
  let wait_for_catchup = Evm_node.wait_for_blueprint_catchup sequencer in
  let wait_for_last_blueprint_catchup_injected =
    Evm_node.wait_for_blueprint_injected
      sequencer
      (before_level + max_blueprints_catchup)
  in
  (* Bake L1 blocks to trigger catchup. *)
  let* _ = Client.bake_for_and_wait client
  and* min, max = wait_for_catchup
  and* () =
    (* last blueprint of catchup injected *)
    wait_for_last_blueprint_catchup_injected
  in
  Check.(
    (min = before_level + 1)
      ~__LOC__
      int
      ~error_msg:
        "Blueprint catchup check \"from\" failed, found %L, expected %R") ;
  Check.(
    (max = before_level + max_blueprints_catchup)
      ~__LOC__
      int
      ~error_msg:"Blueprint catchup check \"to\" failed, found %L, expected %R") ;
  Log.info
    "Catchup is correct, baking 2 l1 blocks to let the rollup node process the \
     inbox." ;
  (* Bake first block to trigger blueprints injection. *)
  let* l1_level = Node.get_level node in
  let l1_level_blueprints_in_inbox = l1_level + 1 in
  let* _ = Client.bake_for_and_wait client
  and* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node l1_level_blueprints_in_inbox
  in

  let l1_level_blueprints_processed = l1_level + 2 in
  (* Bake second block so the rollup node eval blueprints. *)
  let* _ = Client.bake_for_and_wait client
  and* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node l1_level_blueprints_processed
  in

  let*@ rollup_level = rollup_level sc_rollup_node in
  (* Check that the rollup is at least about the expected catchup. *)
  Check.(
    (Int32.to_int rollup_level >= before_level + max_blueprints_catchup)
      ~__LOC__
      int
      ~error_msg:"Check rollup head failed. Found %L, expected >= %R") ;

  let l1_level_blueprints_processed_finalized =
    l1_level_blueprints_processed + 2
  in
  let finalized_blueprint_ref = ref None in
  let wait_for_catchup_l1_level_finalized =
    Lwt.join
      [
        (* l1_level + 2 is seen, so l1_level is finalized. *)
        (let* _ =
           Sc_rollup_node.wait_for_level
             sc_rollup_node
             l1_level_blueprints_processed_finalized
         in
         unit);
        (* l1_level is finalized and processed. *)
        (let* {l1_level = _; finalized_blueprint} =
           Evm_node.wait_for_processed_l1_level
             sequencer
             ~level:l1_level_blueprints_processed
         in
         finalized_blueprint_ref := Some finalized_blueprint ;
         unit);
      ]
  in
  (* 2 block so catchup blueprints processed by the rollup node are
     marked as finalized *)
  let* () =
    repeat 2 (fun () ->
        let* _ = Client.bake_for_and_wait client in
        unit)
  and* _ = wait_for_catchup_l1_level_finalized in

  Check.(
    (!finalized_blueprint_ref >= Some (before_level + max_blueprints_catchup))
      ~__LOC__
      (option int)
      ~error_msg:"Finalized blueprint check failed. Found %L, expected >= %R") ;
  unit

let test_delayed_deposit_is_included =
  register_tezosx_test
    ~time_between_blocks:Nothing
    ~bootstrap_accounts:[]
    ~tags:["sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Michelson runtime delayed deposit is included"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.((balance = Tez.zero) Tez.typ)
    ~error_msg:"Expected balance at 0 for bootstrap1" ;
  let amount = Tez.of_int 1000 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         Constant.bootstrap1.public_key_hash
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
    send_deposit_to_delayed_inbox
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
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.((balance = Tez.of_int 1000) Tez.typ)
    ~error_msg:"Expected a 1000 tez on bootstrap1" ;
  unit

let test_bridged_tez_transfer =
  register_tezosx_test
    ~time_between_blocks:Nothing
    ~bootstrap_accounts:[]
    ~tags:["deposit"; "transfer"]
    ~title:"A Tezos account that has only bridged tez can make a transfer"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in

  (* Check that account is empty and not revealed. *)
  let* balance_1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.((balance_1 = Tez.zero) Tez.typ)
    ~error_msg:"Expected balance at 0 for bootstrap1" ;
  let* manager_key =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_manager_key
         ~id:Constant.bootstrap1.Account.public_key_hash
         ()
  in
  Check.(
    JSON.(manager_key |> as_string_opt = None)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  let depositor = Constant.bootstrap5 in
  let receiver =
    Result.get_ok
    @@ Tezos_protocol_alpha.Protocol.Contract_repr.of_b58check
         Constant.bootstrap1.public_key_hash
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
    send_deposit_to_delayed_inbox
      ~rlp:true
      ~amount:(Tez.of_int 1000)
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  let* () =
    Client.transfer
      ~endpoint
      ~amount:Tez.one
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      client
  in
  let* _ = produce_block sequencer in
  let* balance_2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.((balance_2 = Tez.one) Tez.typ)
    ~error_msg:"Expected balance at 1 tez for bootstrap2" ;
  unit

let originate_contract ~sequencer ~client ~endpoint ~protocol ~filename ~storage
    =
  let* _alias, address =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:storage
      ~burn_cap:Tez.one
      ~force:true
      ~endpoint
      client
      ["big_maps"; filename]
      protocol
  in
  let*@ _ = Rpc.produce_block sequencer in
  return address

let call_contract ~sequencer ~client ~endpoint ~address ~arg =
  let transfer () =
    Client.transfer
      ~burn_cap:Tez.one
      ~amount:Tez.zero
        (* We need to wait for inclusion so that
           octez-client receipt is complete with
           lazy_storage_updates related to big_maps *)
      ~wait:"0"
      ~giver:"bootstrap1"
      ~receiver:address
      ~arg
      ~hooks:Tezos_regression.hooks
      ~endpoint
      client
  in
  wait_for_application
    ~time_between_blocks:1.
    ~produce_block:(fun () -> produce_block sequencer)
    transfer

let test_big_map_transfer =
  register_tezosx_regression_test
    ~title:"Test of the big_map transfers"
    ~tags:["big_map"; "compatibility"; "operations"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} protocol ->
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in

  let endpoint = tezlink_endpoint_from_evm_node sequencer in

  (* This code comes from tezt/tests/contract_big_map_transfer.ml *)
  let* () =
    Lwt_list.iter_s
      (fun (sender_filename, sender_storage) ->
        Lwt_list.iter_s
          (fun (receiver_filename, receiver_storage) ->
            let () =
              Regression.capture
                (sf
                   "Test transferring big map from %S to %S"
                   sender_filename
                   receiver_filename)
            in
            let* receiver =
              originate_contract
                ~sequencer
                ~client
                ~endpoint
                ~protocol
                ~filename:receiver_filename
                ~storage:receiver_storage
            in
            let* sender =
              originate_contract
                ~sequencer
                ~client
                ~endpoint
                ~protocol
                ~filename:sender_filename
                ~storage:sender_storage
            in
            let* () =
              call_contract
                ~sequencer
                ~client
                ~endpoint
                ~address:sender
                ~arg:(sf "%S" receiver)
            in
            unit)
          [
            ("receiver_drop", "Unit");
            ("receiver_store", "{}");
            ("receiver_store_updated", "{}");
          ])
      [
        ("sender_fresh", "Unit");
        ("sender_stored", "{Elt \"d\" 0x; }");
        ("sender_stored_updated", "{Elt \"b\" 0x; Elt \"d\" 0x; }");
      ]
  in
  unit

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_upgrade_kernel_auto_sync =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let timestamp = "2020-01-01T00:00:05Z" in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_tezosx_upgrade_test
    ~genesis_timestamp
    ~tags:["sequencer"; "upgrade"; "auto"; "sync"; Tag.flaky]
    ~title:
      "Tezos X rollup-node kernel upgrade is applied to the sequencer state."
  @@
  fun from
      to_
      {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () =
    match Kernel.commit_of from with
    | Some from_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:true from_commit
        in
        unit
    | None -> unit
  in

  (* Kill the observer to demonstrate the sequencer propagates the upgrade on
     replay. *)
  let* () = Evm_node.terminate observer in

  (* Sends the upgrade to L1, but not to the sequencer. *)
  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:to_use
      ~activation_timestamp
  in

  (* Per the activation timestamp, the state will remain synchronised until
     the kernel is upgraded. *)
  let* _ =
    repeat 2 (fun () ->
        let*@ _ = produce_block ~timestamp sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Produce a block after activation timestamp, both the rollup node
     and the sequencer will upgrade to latest kernel. *)
  let* _ =
    let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer in
    unit
  and* _upgrade = Evm_node.wait_for_successful_upgrade sequencer in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    match Kernel.commit_of to_ with
    | Some to_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:true to_commit
        in
        unit
    | None -> unit
  in

  (* Start the observer again and wait for a successful upgrade *)
  let* () = Evm_node.run observer in
  let* _upgrade = Evm_node.wait_for_successful_upgrade observer in

  let* () = Evm_node.wait_for_blueprint_applied observer 3 in

  unit

(** Query the /entrypoints RPC for [address] via the Tezlink endpoint. *)
let get_entrypoints ?normalize_types tezlink_endpoint address =
  RPC_core.call tezlink_endpoint
  @@ RPC.get_chain_block_context_contract_entrypoints
       ?normalize_types
       ~id:address
       ()

(** Test that the /entrypoints RPC works for an originated Michelson contract.
    The faucet contract has a single [%fund : mutez] entrypoint. *)
let test_entrypoints_originated =
  let faucet = Michelson_contracts.faucet_contract () in
  register_tezosx_test
    ~title:"Entrypoints RPC for originated Michelson contract"
    ~tags:["rpc"; "entrypoints"; "originated"]
    ~bootstrap_contracts:[faucet]
  @@ fun {sequencer; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let* ep_json =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_entrypoints ~id:faucet.address ()
  in
  let expected =
    JSON.parse
      ~origin:"expected_entrypoints"
      {|{"entrypoints":{"fund":{"prim":"mutez"}}}|}
  in
  Check.((ep_json = expected) json ~error_msg:"Expected %R but got %L") ;
  unit

(** Test that the /entrypoints RPC respects the [normalize_types] query
    parameter for an originated Michelson contract.  Uses [fa12_reference.tz]
    whose [%transfer] entrypoint carries [:from], [:to] and [:value] variable
    annotations that are stripped when [normalize_types=true]. *)
let test_entrypoints_normalize_types =
  register_tezosx_test
    ~title:"Entrypoints RPC normalize_types for originated Michelson contract"
    ~tags:["rpc"; "entrypoints"; "originated"; "normalize_types"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_endpoint = tezlink_foreign_endpoint_from_evm_node sequencer in
  let endpoint = Client.Foreign_endpoint tezlink_endpoint in
  let* _alias, kt1_address =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "Pair {} %S False 0" Constant.bootstrap1.public_key_hash)
      ~burn_cap:(Tez.of_mutez_int 2_000_000)
      ~force:true
      ~endpoint
      client
      ["mini_scenarios"; "fa12_reference"]
      Alpha
  in
  let*@ _ = Rpc.produce_block sequencer in
  (* Without normalize_types, %transfer retains its :from/:to/:value annotations. *)
  let* entrypoints =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_entrypoints ~id:kt1_address ()
  in
  let transfer_type = JSON.(entrypoints |-> "entrypoints" |-> "transfer") in
  let expected_transfer_without_normalize =
    JSON.parse
      ~origin:"expected_transfer_type"
      {|{"prim":"pair","args":[{"prim":"address","annots":[":from"]},{"prim":"pair","args":[{"prim":"address","annots":[":to"]},{"prim":"nat","annots":[":value"]}]}]}|}
  in
  Check.(
    (transfer_type = expected_transfer_without_normalize)
      json
      ~error_msg:"Expected %R but got %L") ;
  (* With normalize_types=true, all annotations are stripped and the type is in
     canonical comb form. *)
  let* entrypoints_normalized =
    RPC_core.call tezlink_endpoint
    @@ RPC.get_chain_block_context_contract_entrypoints
         ~normalize_types:true
         ~id:kt1_address
         ()
  in
  let transfer_type_normalized =
    JSON.(entrypoints_normalized |-> "entrypoints" |-> "transfer")
  in
  let expected_transfer_normalized =
    JSON.parse
      ~origin:"expected_transfer_type_normalized"
      {|{"prim":"pair","args":[{"prim":"address"},{"prim":"address"},{"prim":"nat"}]}|}
  in
  Check.(
    (transfer_type_normalized = expected_transfer_normalized)
      json
      ~error_msg:"Expected %R but got %L") ;
  unit

let () =
  test_observer_starts [Alpha] ;
  test_describe_endpoint [Alpha] ;
  test_current_level [Alpha] ;
  test_contract_info [Alpha] ;
  test_list_entrypoints [Alpha] ;
  test_contract_info_script [Alpha] ;
  test_balance [Alpha] ;
  test_manager_key [Alpha] ;
  test_counter [Alpha] ;
  test_protocols [Alpha] ;
  test_genesis_block_arg [Alpha] ;
  test_expected_issuance [Alpha] ;
  test_monitor_heads [Alpha] ;
  test_version [Alpha] ;
  test_contracts_rpc [Alpha] ;
  test_header [Alpha] ;
  test_block_metadata [Alpha] ;
  test_constants [Alpha] ;
  test_storage_rpc [Alpha] ;
  test_produceBlock [Alpha] ;
  test_hash_rpc [Alpha] ;
  test_script_rpc [Alpha] ;
  test_blocks_list [Alpha] ;
  test_contract_storage_normalization [Alpha] ;
  test_contract_counter [Alpha] ;
  test_raw_json_cycle [Alpha] ;
  test_chain_id [Alpha] ;
  test_bootstrapped [Alpha] ;
  test_transfer [Alpha] ;
  test_observer_transfer [Alpha] ;
  test_transfer_and_wait [Alpha] ;
  test_reveal [Alpha] ;
  test_block_info [Alpha] ;
  test_storage_via_client [Alpha] ;
  test_execution [Alpha] ;
  test_bigmap_option [Alpha] ;
  test_bigmap_counter [Alpha] ;
  test_bootstrap_kt1_is_executable [Alpha] ;
  test_bigmap_rpcs [Alpha] ;
  test_pack_data [Alpha] ;
  test_run_operation [Alpha] ;
  test_reveal_transfer_batch [Alpha] ;
  test_batch [Alpha] ;
  test_long_batch [Alpha] ;
  test_bootstrap_block_info [Alpha] ;
  test_internal_operation [Alpha] ;
  test_internal_receipts [Alpha] ;
  test_operation_hashes_in_pass [Alpha] ;
  test_event [Alpha] ;
  test_prevalidation [Alpha] ;
  test_prevalidation_gas_limit_lower_bound [Alpha] ;
  test_validation_gas_limit [Alpha] ;
  test_validation_counter [Alpha] ;
  test_validation_balance [Alpha] ;
  test_insufficient_da_fee [Alpha] ;
  test_da_fee_credited_to_pool [Alpha] ;
  test_simulation_with_da_fee [Alpha] ;
  test_origination [Alpha] ;
  test_forge_operations [Alpha] ;
  test_michelson_gas_backlog [Alpha] ;
  test_michelson_gas_backlog_on_failed_op [Alpha] ;
  test_michelson_execution_gas_fee [Alpha] ;
  test_michelson_gas_exhaustion [Alpha] ;
  test_mempool_filter_fields [Alpha] ;
  test_gas_refund_on_transfer ~enable_refund:true [Alpha] ;
  test_gas_refund_on_transfer ~enable_refund:false [Alpha] ;
  test_gas_refund_on_failwith ~enable_refund:true [Alpha] ;
  test_gas_refund_on_failwith ~enable_refund:false [Alpha] ;
  test_gas_refund_on_out_of_gas [Alpha] ;
  test_gas_refund_in_run_operation ~enable_refund:true [Alpha] ;
  test_gas_refund_in_run_operation ~enable_refund:false [Alpha] ;
  test_gas_refund_in_preapply ~enable_refund:true [Alpha] ;
  test_gas_refund_in_preapply ~enable_refund:false [Alpha] ;
  test_gas_vs_l1 [Alpha] ;
  test_node_catchup [Alpha] ;
  test_delayed_deposit_is_included [Alpha] ;
  test_bridged_tez_transfer [Alpha] ;
  test_big_map_transfer [Alpha] ;
  test_upgrade_kernel_auto_sync [Alpha] ;
  test_entrypoints_originated [Alpha] ;
  test_entrypoints_normalize_types [Alpha]
