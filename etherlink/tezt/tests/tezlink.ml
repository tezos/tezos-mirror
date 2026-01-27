(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
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

let register_tezlink_test ~title ~tags ?bootstrap_accounts ?bootstrap_contracts
    ?genesis_timestamp ?(time_between_blocks = Evm_node.Nothing)
    ?additional_uses ?max_blueprints_catchup ?max_blueprints_lag
    ?catchup_cooldown ?(kernels = [Kernel.Latest]) scenario protocols =
  register_all
    ~__FILE__
    ~kernels
    ~title
    ~tags:("tezlink" :: tags)
    ~l2_setups:
      [
        {
          (Evm_node.default_l2_setup ~l2_chain_id:2937611481) with
          l2_chain_family = "Michelson";
          tez_bootstrap_accounts = bootstrap_accounts;
          tez_bootstrap_contracts = bootstrap_contracts;
        };
      ]
    ~use_multichain:Register_with_feature
    ~rpc_server:Evm_node.Resto
    ?genesis_timestamp
    ~time_between_blocks
    ?max_blueprints_catchup
    ?max_blueprints_lag
    ?catchup_cooldown
    ?additional_uses
    scenario
    protocols

let register_tezlink_upgrade_test ~title ~tags ~genesis_timestamp
    ?(time_between_blocks = Evm_node.Nothing) ?(kernels = Kernel.tezlink_all)
    ?(upgrade_to = Kernel.upgrade_to) ?(additional_uses = []) scenario protocols
    =
  List.iter
    (fun from ->
      let from_tag, _ = Kernel.to_uses_and_tags from in
      let to_ = upgrade_to from in
      let to_tag, to_use = Kernel.to_uses_and_tags to_ in
      register_tezlink_test
        ~kernels:[from]
        ~genesis_timestamp
        ~time_between_blocks
        ~tags:("upgrade_scenario" :: to_tag :: tags)
        ~title:Format.(sprintf "%s (%s -> %s)" title from_tag to_tag)
        ~additional_uses:(to_use :: additional_uses)
        (scenario from to_)
        protocols)
    kernels

let register_tezlink_regression_test ~title ~tags ?bootstrap_accounts
    ?bootstrap_contracts ?(time_between_blocks = Evm_node.Nothing) scenario =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:("tezlink" :: tags)
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
  let l2_chains =
    [
      {
        (Evm_node.default_l2_setup ~l2_chain_id:12) with
        l2_chain_family = "Michelson";
        tez_bootstrap_accounts = bootstrap_accounts;
        tez_bootstrap_contracts = bootstrap_contracts;
      };
    ]
  in
  let* setup =
    Setup.setup_sequencer
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:true
      ~l2_chains
      ~rpc_server:Evm_node.Resto
      ~spawn_rpc:(Port.fresh ())
      ~time_between_blocks
      protocol
  in
  scenario setup protocol

(** Helper to parse RPC response. Returns [Some json] if 200, [None] if 404,
    fails otherwise. *)
let parse_rpc_response ~origin response =
  match response.RPC_core.code with
  | 200 -> Lwt.return_some (JSON.parse ~origin response.body)
  | 404 -> Lwt.return_none
  | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code

let test_describe_endpoint =
  register_tezlink_regression_test
    ~tags:["evm"; "rpc"; "describe"]
    ~title:"Test the /describe endpoint"
  @@ fun {sequencer; client; _} _ ->
  let hooks = Tezos_regression.hooks in
  let sequencer_endpoint = Evm_node.rpc_endpoint_record sequencer in
  (* List all the endpoint of the sequencer *)
  let root_endpoint = Client.(Foreign_endpoint sequencer_endpoint) in
  let* (_ : string) = Client.rpc_list ~hooks ~endpoint:root_endpoint client in
  (* List the endpoints of the /tezlink directory *)
  let tezlink_endpoint =
    Client.(Foreign_endpoint {sequencer_endpoint with path = "/tezlink"})
  in
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
  register_tezlink_test
    ~title:"Test Tezlink observer does not crash at startup"
    ~tags:["observer"]
  @@ fun {sequencer; observer; _} _protocol ->
  (* Wait 60s that the drift monitor process starts *)
  let* () = Lwt_unix.sleep 60. in

  let observer_promise = Evm_node.wait_for_blueprint_applied observer 1 in
  let* () =
    let*@ _ = Rpc.produce_block sequencer in
    unit
  and* () = observer_promise in
  unit

let test_tezlink_current_level =
  register_tezlink_test
    ~title:"Test of the current_level rpc"
    ~tags:["rpc"; "current_level"]
  @@ fun {sequencer; _} _protocol ->
  (* call the current_level rpc and parse the result *)
  let rpc_current_level ?offset block =
    let offset_str =
      Option.map (fun offset -> "?offset=" ^ string_of_int offset) offset
      |> Option.value ~default:""
    in
    let path =
      "/tezlink/chains/main/blocks/" ^ block ^ "/helpers/current_level"
      ^ offset_str
    in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_current_level" res
  in

  (* verify an answer by the current_level rpc *)
  let check_current_level res expected_level =
    (* cycle length is hardcoded for now it won't change for some time *)
    let cycle_length = 10800 in
    Check.(
      JSON.(res |-> "level" |> as_int = expected_level)
        int
        ~error_msg:"Level: expected %R but got %L") ;
    Check.(
      JSON.(res |-> "level_position" |> as_int = expected_level - 1)
        int
        ~error_msg:"Expected %R but got %L") ;
    Check.(
      JSON.(res |-> "cycle" |> as_int = expected_level / cycle_length)
        int
        ~error_msg:"Cycle: expected %R but got %L") ;
    Check.(
      JSON.(
        res |-> "cycle_position" |> as_int
        = (expected_level - 1) mod cycle_length)
        int
        ~error_msg:"Cycle_position: expected %R but got %L") ;
    Check.(
      JSON.(res |-> "expected_commitment" |> as_bool = false)
        bool
        ~error_msg:"Expected_commitment: expected %R but got %L") ;
    unit
  in

  (* checks *)
  let* res = rpc_current_level "head" in
  let* () = check_current_level res 0 in
  (* test with offset *)
  let* res = rpc_current_level "head" ~offset:1 in
  let* () = check_current_level res 1 in
  (* test with offset larger than a cycle *)
  let* res = rpc_current_level "head" ~offset:40000 in
  let* () = check_current_level res 40000 in
  (* Bake 5 blocks and test that "head" is now at level 5. *)
  let* () =
    repeat 5 (fun () ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let* res = rpc_current_level "head" in
  let* () = check_current_level res 5 in
  (* Check with block parameter *)
  let* res = rpc_current_level "head~2" in
  let* () = check_current_level res 3 in
  let* res = rpc_current_level "head~2" ~offset:1 in
  let* () = check_current_level res 4 in
  (* test negative offset *)
  let* res = rpc_current_level "head" ~offset:(-1) in
  Check.(
    JSON.(
      res |> as_list |> List.hd |-> "msg" |> as_string
      = "The specified level offset should be positive.")
      string
      ~error_msg:"Should have failed: expected %R but got %L") ;
  (* test numeric block parameter *)
  let* res = rpc_current_level "3" ~offset:1 in
  let* () = check_current_level res 4 in
  let* res = rpc_current_level "genesis" in
  let* () = check_current_level res 0 in
  unit

let test_tezlink_protocols =
  register_tezlink_test
    ~title:"Test of the protocols rpc"
    ~tags:["rpc"; "protocols"]
  @@ fun {sequencer; _} _protocol ->
  (* call the protocols rpc and parse the result *)
  let rpc_protocols () =
    let path = "/tezlink/chains/main/blocks/head/protocols" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_protocols" res
  in
  let protocol_hash = Protocol.hash Michelson_contracts.tezlink_protocol in

  let* res = rpc_protocols () in
  Check.(
    JSON.(res |-> "protocol" |> as_string = protocol_hash)
      string
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_genesis_block_arg =
  register_tezlink_test
    ~title:"Test of the genesis block argument"
    ~tags:["rpc"; "genesis"; "protocols"]
  @@ fun {sequencer; _} _protocol ->
  (* call the protocols rpc and parse the result *)
  let rpc_protocols block_arg =
    let path =
      Format.sprintf "/tezlink/chains/main/blocks/%s/protocols" block_arg
    in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_protocols" res
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

let test_tezlink_expected_issuance =
  register_tezlink_test
    ~title:"Test the mocked expected issuance rpc"
    ~tags:["rpc"; "issuance"; "mock"]
  @@ fun {sequencer; _} _protocol ->
  (* call the rpc and parse the result *)
  let rpc () =
    let path =
      "/tezlink/chains/main/blocks/head/context/issuance/expected_issuance"
    in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl" res
  in

  let* res = rpc () in
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

let test_tezlink_balance =
  register_tezlink_test
    ~title:"Test of the balance rpc"
    ~tags:["rpc"; "balance"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  (* call the balance rpc and parse the result *)
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

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

let test_tezlink_storage_via_client =
  let contract = Michelson_contracts.concat_hello () in
  register_tezlink_test
    ~title:"Test of the storage rpc via client"
    ~tags:["rpc"; "storage"]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* storage = Client.contract_storage ~endpoint contract.address client in
  Check.(
    (String.trim storage = String.trim contract.initial_storage)
      string
      ~error_msg:"Expected \"%R\" but got \"%L\"") ;
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

let test_tezlink_contract_info =
  register_tezlink_test
    ~title:"Test of the contract info rpc"
    ~tags:["rpc"; "contract"; "info"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  (* call the balance rpc and check the result *)
  let* valid_info = account_rpc sequencer Constant.bootstrap1 "" in
  let balance = JSON.(valid_info |-> "balance" |> as_int) in
  let counter = JSON.(valid_info |-> "counter" |> as_int) in

  Check.((balance = 3800000000000) int ~error_msg:"Expected %R but got %L") ;
  Check.((counter = 0) int ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_list_entrypoints =
  let contract = Michelson_contracts.faucet_contract () in
  register_tezlink_test
    ~title:"Test of the contract entrypoint list"
    ~tags:["rpc"; "contract"; "info"; "list_entrypoints"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[contract]
  @@ fun {sequencer; _} _protocol ->
  (* call the list_entrypoint rpc and check the result *)
  let* entrypoints = account_str_rpc sequencer contract.address "entrypoints" in
  let expected =
    JSON.parse
      ~origin:"expected_entrypoints"
      "{\"entrypoints\":{\"fund\":{\"prim\":\"mutez\"}}}"
  in
  Check.((entrypoints = expected) json ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_contract_info_script =
  let contract = Michelson_contracts.concat_hello () in
  register_tezlink_test
    ~title:"Test of the contract info rpc on smart contracts"
    ~tags:["rpc"; "contract"; "info"; "script"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  (* call the contract info rpc and check the result *)
  let* valid_info = account_str_rpc sequencer contract.address "" in
  let balance = JSON.(valid_info |-> "balance" |> as_int) in
  let counter = JSON.(valid_info |-> "counter" |> as_int) in
  let script = JSON.(valid_info |-> "script") in
  let storage = JSON.(script |-> "storage" |> as_list) in
  let code = JSON.(script |-> "code" |> as_list) in

  Check.((balance = 3800000000000) int ~error_msg:"Expected %R but got %L") ;
  Check.((counter = 1) int ~error_msg:"Expected %R but got %L") ;
  Check.(
    (List.length storage = 1)
      int
      ~error_msg:"Expected storage with %R element but got %L") ;
  Check.(
    (List.length code = 3)
      int
      ~error_msg:"Expected code with %R elements but got %L") ;
  unit

let test_tezlink_manager_key =
  register_tezlink_test
    ~title:"Test of the manager_key rpc"
    ~tags:["rpc"; "manager_key"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let* valid_res = account_rpc sequencer Constant.bootstrap1 "manager_key" in
  Check.(
    JSON.(valid_res |> as_string_opt = Some Constant.bootstrap1.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  let* invalid_res = account_rpc sequencer Constant.bootstrap2 "manager_key" in
  Check.(
    JSON.(invalid_res |> as_string_opt = None)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_counter =
  register_tezlink_test
    ~title:"Test of the counter rpc"
    ~tags:["evm"; "rpc"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; _} _protocol ->
  let* valid_res = account_rpc sequencer Constant.bootstrap1 "counter" in
  Check.(JSON.(valid_res |> as_int = 0) int ~error_msg:"Expected %R but got %L") ;
  let* invalid_res = account_rpc sequencer Constant.bootstrap2 "counter" in
  Check.(
    JSON.(invalid_res |> as_int = 1) int ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_contract_info_on_liquidity_baking =
  register_tezlink_test
    ~title:"Test of the contract info rpc on liquidity baking addresses"
    ~tags:["rpc"; "contract"; "info"; "liquidity_baking"]
  @@ fun {sequencer; _} _protocol ->
  (* call the balance rpc and check the result *)
  let verify_account_info_rpc address =
    let path =
      Format.sprintf
        "/tezlink/chains/main/blocks/head/context/contracts/%s"
        address
    in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"curl_protocols" res in
    (* Verify that the script field exists*)
    let script = JSON.(json |-> "script") in
    return
      Check.(
        (is_false @@ JSON.is_null script)
          ~error_msg:"Expected script but found nothing")
  in
  let cpmm_address = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" in
  let lqt_address = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo" in
  let lq_fallback_token = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN" in

  let* () = verify_account_info_rpc cpmm_address in
  let* () = verify_account_info_rpc lqt_address in
  let* () = verify_account_info_rpc lq_fallback_token in
  unit

let test_tezlink_version =
  register_tezlink_test
    ~title:"Test of the version rpc"
    ~tags:["rpc"; "version"]
  @@ fun {sequencer; _} _protocol ->
  (* call the version rpc and parse the result *)
  let rpc_version () =
    let path = "/tezlink/version" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_version" res
  in

  let* res = rpc_version () in
  Check.(
    JSON.(res |-> "version" |-> "major" |> as_int = 0)
      int
      ~error_msg:"Expected version %R but got %L") ;
  unit

let test_tezlink_constants =
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
  let l2_chains =
    [
      {
        (Evm_node.default_l2_setup ~l2_chain_id:12) with
        l2_chain_family = "Michelson";
      };
    ]
  in
  let* {sequencer; client; _} =
    Setup.setup_sequencer
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:true
      ~l2_chains
      ~rpc_server:Evm_node.Resto
      ~spawn_rpc:(Port.fresh ())
      protocol
  in
  let hooks = Tezos_regression.hooks in
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* _ =
    Client.RPC.call ~hooks ~endpoint client
    @@ RPC.get_chain_block_context_constants ()
  in
  unit

let test_tezlink_storage_rpc =
  register_tezlink_test
    ~title:"Test of the /storage rpc"
    ~tags:["rpc"; "storage"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let foreign_endpoint =
    {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"}
  in
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

let test_tezlink_chain_id =
  register_tezlink_test
    ~title:"Test of the chain_id rpc"
    ~tags:["rpc"; "chain_id"]
  @@ fun {sequencer; client; l2_chains; _} _protocol ->
  let expected_chain_id =
    match l2_chains with
    | [l2_chain] -> l2_chain.l2_chain_id
    | _ -> Test.fail ~__LOC__ "Expected one l2 chain"
  in
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* chain_id =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_chain_id ()
  in
  check_chain_id ~expected_chain_id ~chain_id ;
  unit

let test_tezlink_header =
  register_tezlink_test
    ~title:"Test of the header rpc"
    ~tags:["rpc"; "header"; "offset"]
  @@ fun {sequencer; client; l2_chains; _} _protocol ->
  let chain_id =
    match l2_chains with
    | [l2_chain] -> Some l2_chain.l2_chain_id
    | _ -> Test.fail ~__LOC__ "Expected one l2 chain"
  in

  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

  let*@ n = Rpc.produce_block sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer n in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let*@ n = Rpc.produce_block ~timestamp:current_timestamp sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer n in
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

let test_tezlink_block_info =
  register_tezlink_test
    ~title:"Test of the block_info rpc"
    ~tags:["rpc"; "block_info"]
  @@ fun {sequencer; client; l2_chains; _} _protocol ->
  let chain_id =
    match l2_chains with
    | [l2_chain] -> Some l2_chain.l2_chain_id
    | _ -> Test.fail ~__LOC__ "Expected one l2 chain"
  in

  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

  let*@ n = Rpc.produce_block sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer n in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let*@ n = Rpc.produce_block ~timestamp:current_timestamp sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer n in
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

let test_tezlink_bootstrapped =
  register_tezlink_test
    ~title:"Test of the bootstrapped rpc"
    ~tags:["rpc"; "bootstrapped"]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let current_timestamp =
    Tezos_base.Time.(
      System.now () |> System.to_protocol |> Protocol.to_notation)
  in
  let*@ n = Rpc.produce_block ~timestamp:current_timestamp sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer n in
  let* block =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block_header ()
  in
  let* rpc_bootstrapped =
    let path = "/tezlink/monitor/bootstrapped" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_bootstrapped" res
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

let test_tezlink_monitor_heads =
  register_tezlink_test
    ~title:"Test of the monitor/heads RPC"
    ~tags:["evm"; "rpc"; "monitor_heads"]
  @@ fun {sequencer; client; _} _protocol ->
  let open Lwt.Syntax in
  (* Prepare the RPC endpoint *)
  let rpc_info = Evm_node.rpc_endpoint_record sequencer in
  let endpoint = Client.Foreign_endpoint {rpc_info with path = "/tezlink"} in

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

let test_tezlink_produceBlock =
  register_tezlink_test
    ~title:"Test Tezlink production block"
    ~tags:["kernel"; "produce_block"]
  @@ fun {sequencer; _} _protocol ->
  let rpc_current_level_head sequencer =
    let path = "/tezlink/chains/main/blocks/head/helpers/current_level" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"curl_current_level" res in
    return @@ JSON.(json |-> "level" |> as_int)
  in
  let* start_level = rpc_current_level_head sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let* end_level = rpc_current_level_head sequencer in
  Check.((start_level + 3 = end_level) int)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let test_tezlink_hash_rpc =
  register_tezlink_test ~title:"Test Tezlink hash rpc" ~tags:["rpc"; "hash"]
  @@ fun {sequencer; _} _protocol ->
  let path block = sf "/tezlink/chains/main/blocks/%s/hash" block in
  let rpc_hash block =
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path block)
      |> Runnable.run
    in
    return @@ JSON.(parse ~origin:"curl_hash" res |> as_string)
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

let test_tezlink_raw_json_cycle =
  register_tezlink_test
    ~title:"Test Tezlink raw json cycle rpc"
    ~tags:["rpc"; "cycle"; "raw"]
  @@ fun {sequencer; _} _protocol ->
  let path_head = "/tezlink/chains/main/blocks/head/" in
  let rpc_raw_json_cycle () =
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path_head ^ "context/raw/json/cycle/0")
      |> Runnable.run
    in
    return @@ JSON.parse ~origin:"curl_hash" res
  in
  let*@ _ = produce_block sequencer in
  let* cycle_0 = rpc_raw_json_cycle () in
  (* 0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8 is the mock value
     used for the ranom seed and never changes *)
  Check.(
    JSON.(
      cycle_0 |-> "random_seed" |> as_string
      = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")
      string
      ~error_msg:"Unexpected random_seed field") ;
  unit

let test_tezlink_transfer =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  register_tezlink_test
    ~title:"Test Tezlink transfer"
    ~tags:["kernel"; "transfer"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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
    = Tez.to_mutez bootstrap_balance - Tez.to_mutez amount - Tez.to_mutez fee)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_tezlink_observer_transfer =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  register_tezlink_test
    ~title:"Test Tezlink transfer via an observer"
    ~tags:["observer"; "transfer"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; observer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record observer) with path = "/tezlink"})
  in
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
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance - Tez.to_mutez amount - Tez.to_mutez fee)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_tezlink_transfer_and_wait =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  register_tezlink_test
    ~title:"Test Tezlink transfer and wait for inclusion"
    ~tags:["kernel"; "transfer"; "wait"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~time_between_blocks:(Time_between_blocks 0.1)
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let amount = Tez.one in
  let fee = Tez.one in
  (* The branch of the operation injected by the client is the
     grand-parent of the current block. Looking for operation hashes
     relies on the operation_hashes RPC which is not implemented for
     the genesis block. For this reason, we wait for block level 3
     before building the operation to ensure that the operation's
     branch is not genesis. *)
  let* () = Evm_node.wait_for_blueprint_applied sequencer 3 in
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
    = Tez.to_mutez bootstrap_balance - Tez.to_mutez amount - Tez.to_mutez fee)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.to_mutez amount) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_tezlink_reveal =
  register_tezlink_test
    ~title:"Test Tezlink reveal"
    ~tags:["kernel"; "reveal"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
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
  let* manager_key = account_rpc sequencer Constant.bootstrap2 "manager_key" in
  Check.(
    JSON.(manager_key |> as_string_opt = None)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  let*! () = Client.reveal ~endpoint ~src:Constant.bootstrap2.alias client in
  let*@ _ = produce_block sequencer in
  let* manager_key = account_rpc sequencer Constant.bootstrap2 "manager_key" in
  Check.(
    JSON.(manager_key |> as_string_opt = Some Constant.bootstrap2.public_key)
      (option string)
      ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_bootstrap_block_info =
  (* This test just makes sure the block info call on block 2 does not fail
     catastrophically. We check block 2 in particular because at this level
     we mock information to index bootstrap accounts. *)
  let contract = Michelson_contracts.concat_hello () in
  register_tezlink_test
    ~title:"Test of tezlink block info on block 2"
    ~tags:["bootstrap"; "block_info"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let* info =
    Client.RPC.call ~hooks ~endpoint client @@ RPC.get_chain_block ~block:"2" ()
  in
  Check.(
    JSON.(info |-> "header" |-> "level" |> as_int = 2)
      int
      ~error_msg:
        "Expect %R but got %L: we should have been able to get block info for \
         level 2") ;
  unit

let test_tezlink_execution =
  let contract = Michelson_contracts.concat_hello () in
  register_tezlink_test
    ~title:"Test of tezlink execution"
    ~tags:["execution"; "hello"]
    ~bootstrap_contracts:[contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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

let test_tezlink_bigmap_option =
  let option_contract = Michelson_contracts.big_map_option () in
  register_tezlink_test
    ~title:"Test which syntax is used for big maps in contract storages"
    ~tags:["syntax"; "big_map"; "option"]
    ~bootstrap_contracts:[option_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let expected_result = "Some 4" in
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

let test_tezlink_bigmap_counter =
  let counter_contract = Michelson_contracts.big_map_counter () in
  register_tezlink_test
    ~title:"Test of tezlink big_map persistency"
    ~tags:["persistency"; "big_map"; "counter"]
    ~bootstrap_contracts:[counter_contract]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  (* This test verifies big_map persistence in storage.
     It relies on the counter.tz invariant that a counter with a matching key
     exists in the stored big_map. The contract is called twice to ensure
     that the big_map is committed to durable storage between calls. *)
  let endpoint =
    Client.(
      Foreign_endpoint
        {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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

let test_tezlink_reveal_transfer_batch =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  register_tezlink_test
    ~title:"Test Tezlink reveal+transfer batch"
    ~tags:["kernel"; "reveal"; "transfer"; "batch"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

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
    = Tez.to_mutez bootstrap_balance - Tez.(to_mutez one) - fee_bootstrap1)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.((Tez.to_mutez balance2 = Tez.(to_mutez one) - fee_bootstrap2) int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_tezlink_batch =
  register_tezlink_test
    ~title:"Test of tezlink batches"
    ~tags:["batch"; "multiple_transfers"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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
      - Tez.to_mutez fee_2 - Tez.to_mutez fee_3)
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
     [test_tezlink_batch] for a test checking the balance changes on a batch of
     size 2),
   - Test that there is no regression for the counter RPC which used to return
     128 instead of 64 (see MR !19237).
*)
let test_tezlink_long_batch =
  register_tezlink_test
    ~title:"Long transaction batch"
    ~tags:["transaction"; "long"; "batch"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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
  let* counter = account_rpc sequencer Constant.bootstrap1 "counter" in
  Check.(
    (batch_size = (counter |> JSON.as_int))
      int
      ~error_msg:
        "Wrong counter after batch application: expected: %L, actual: %R.") ;
  unit

let test_tezlink_sandbox () =
  Test.register
    ~__FILE__
    ~title:"Tezlink sandbox transfer test"
    ~tags:["sequencer"; "sandbox"; "tezlink"]
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let wallet_dir = Temp.dir "wallet" in
  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  let () = Account.write Constant.all_secret_keys ~base_dir:wallet_dir in
  let* {output; _} =
    prepare_installer_kernel_with_arbitrary_file
      ~preimages_dir
      (Uses.path Constant.WASM.evm_kernel)
  in
  let sequencer_config : Evm_node.sequencer_config =
    {
      time_between_blocks = Some Nothing;
      genesis_timestamp = None;
      max_number_of_chunks = None;
      wallet_dir = Some wallet_dir;
    }
  in
  let sequencer_mode =
    Evm_node.Tezlink_sandbox
      {
        sequencer_config;
        funded_addresses =
          [Constant.bootstrap1.public_key; Constant.bootstrap2.public_key];
        verbose = false;
      }
  in

  let* sequencer =
    Evm_node.init
      ~mode:sequencer_mode
      ~spawn_rpc:(Port.fresh ())
      ~initial_kernel:output
      ~preimages_dir
      ~private_rpc_port:(Port.fresh ())
      ()
  in

  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

  let client = Client.create ~endpoint ~base_dir:wallet_dir () in

  let amount = Tez.one in
  let fee = Tez.one in
  let* () =
    Client.transfer
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
    = Tez.to_mutez bootstrap_balance - Tez.to_mutez amount - Tez.to_mutez fee)
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  Check.(
    (Tez.to_mutez balance2
    = Tez.to_mutez bootstrap_balance + Tez.to_mutez amount)
      int)
    ~error_msg:"Wrong balance for bootstrap2: expected %R, actual %L" ;
  unit

let test_tezlink_internal_operation =
  let bootstrap_balance = Tez.of_mutez_int 3_800_000_000_000 in
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  register_tezlink_test
    ~title:"Internal operation"
    ~tags:["internal"; "operation"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[faucet]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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
    (Tez.to_mutez balance = Tez.to_mutez bootstrap_balance + Tez.(to_mutez one))
      int)
    ~error_msg:"Wrong balance for bootstrap1: expected %R, actual %L" ;
  unit

let test_tezlink_internal_receipts =
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  register_tezlink_test
    ~title:"Internal receipts"
    ~tags:["internal"; "operation"; "receipts"]
    ~bootstrap_accounts:[Constant.bootstrap1]
    ~bootstrap_contracts:[faucet]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_path = "/tezlink" in
  let amount = 1000000 in
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = tezlink_path})
  in
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
    let path = tezlink_path ^ "/chains/main/blocks/head/operations/3/0" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"curl_operation_hashes" res in
    return json
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

let test_tezlink_origination =
  let faucet = Tezt_etherlink.Michelson_contracts.faucet_contract () in
  register_tezlink_test
    ~title:"Contract origination"
    ~tags:["origination"; "operation"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
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

let test_event =
  let emit_events_contract =
    Tezt_etherlink.Michelson_contracts.emit_events_contract ()
  in
  register_tezlink_test
    ~title:"Contract emits an event"
    ~tags:["operation"; "event"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} _protocol ->
  let tezlink_path = "/tezlink" in
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = tezlink_path})
  in
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
    let path = tezlink_path ^ "/chains/main/blocks/head/operations/3/0" in
    let* res =
      Curl.get_raw
        ~name:("curl#" ^ Evm_node.name sequencer)
        ~args:["-v"]
        (Evm_node.endpoint sequencer ^ path)
      |> Runnable.run
    in
    let json = JSON.parse ~origin:"curl_operation_hashes" res in
    return json
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

let test_tezlink_forge_operations =
  register_tezlink_test
    ~title:"Test of the forge/operations rpc"
    ~tags:["rpc"; "forge"; "operations"]
    ~additional_uses:[Constant.octez_codec]
  @@ fun {sequencer; _} _protocol ->
  (* call the forge/operations rpc and parse the result *)
  let json_data =
    JSON.parse
      ~origin:"curl_forge_operations"
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
  let rpc_forge_operations () =
    let path = "/tezlink/chains/main/blocks/head/helpers/forge/operations" in
    Curl.post_raw
      ~name:("curl#" ^ Evm_node.name sequencer)
      (Evm_node.endpoint sequencer ^ path)
      json_data
    |> Runnable.run
  in
  let* binary =
    let name =
      Protocol.encoding_prefix Michelson_contracts.tezlink_protocol
      ^ ".operation.unsigned"
    in
    Codec.encode ~name (JSON.unannotate json_data)
  in
  let expected_res = sf "\"%s\"\n" binary in
  let* res = rpc_forge_operations () in
  Check.((res = expected_res) string ~error_msg:"Expected %R but got %L") ;
  unit

let test_tezlink_prevalidation =
  register_tezlink_test
    ~title:"Test Tezlink prevalidation"
    ~tags:["kernel"; "prevalidation"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint () in
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
  let unknown_rex = rex "Empty implicit contract" in
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

  let hard_gas_limit_per_operation = 1_040_000 in
  let hard_gas_limit_per_block = 1_386_666 in

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

let test_tezlink_prevalidation_gas_limit_lower_bound =
  register_tezlink_test
    ~title:"Test Tezlink prevalidation of operation gas limit lower bound"
    ~tags:["kernel"; "prevalidation"; "gas_limit"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let new_test () =
    (* Print a banner to delimitate a test. Usefull here because the error
       messages can't be customized to easily find which test fails.*)
    Tezt.Log.(
      info
        ~color:Color.(bold ++ FG.green)
        "******************************************************************") ;
    Tezt.Log.(info ~color:Color.(bold ++ FG.green) ~prefix:"NEW TEST")
  in

  let* client_tezlink = Client.init ~endpoint () in
  let build_and_inject ?error operations =
    let* op = Operation.Manager.operation operations client in
    Operation.inject ~dont_wait:true ?error op client_tezlink
  in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 1 in
  let* () = produce_block_and_wait_for ~sequencer 2 in

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
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () =
    check_operations
      ~client:client_tezlink
      ~block:"3"
      ~expected:[op_just_enough_hash]
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

let test_tezlink_validation_gas_limit =
  register_tezlink_test
    ~title:"Test Tezlink validation of block gas limit"
    ~tags:["kernel"; "validation"; "gas_limit"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint () in
  let hard_gas_limit_per_block = 1_386_666 in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 1 in
  let* () = produce_block_and_wait_for ~sequencer 2 in

  let almost_half_block = (hard_gas_limit_per_block / 2) - 1 in

  (* Sanity check: can fit two op in a blueprint *)
  let* (`OpHash op1) =
    Operation.inject_transfer
      ~counter:1
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~gas_limit:almost_half_block
      client_tezlink
  in
  let* (`OpHash op2) =
    Operation.inject_transfer
      ~counter:2
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~gas_limit:almost_half_block
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () =
    check_operations ~client:client_tezlink ~block:"3" ~expected:[op1; op2]
  in

  (* check: with just a bit more gas_limit two op don't fit in a blueprint *)
  let* (`OpHash op3) =
    Operation.inject_transfer
      ~counter:3
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~gas_limit:(almost_half_block + 100)
      client_tezlink
  in
  let* (`OpHash op4) =
    Operation.inject_transfer
      ~counter:4
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~gas_limit:(almost_half_block + 100)
      client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 4 in
  let* () = produce_block_and_wait_for ~sequencer 5 in
  let* () =
    check_operations ~client:client_tezlink ~block:"4" ~expected:[op3]
  in
  let* () =
    check_operations ~client:client_tezlink ~block:"5" ~expected:[op4]
  in
  unit

let test_tezlink_validation_counter =
  register_tezlink_test
    ~title:"Test Tezlink validation of counters"
    ~tags:["kernel"; "validation"; "counter"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint () in

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
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* () =
    check_operations ~client:client_tezlink ~block:"3" ~expected:[op1; op2; op3]
  in
  unit

let test_tezlink_validation_balance =
  register_tezlink_test
    ~title:"Test Tezlink validation of balance"
    ~tags:["kernel"; "validation"; "balance"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; _} _protocol ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in
  let* client_tezlink = Client.init ~endpoint () in
  let new_account = Constant.bootstrap3 in

  (* make sure there are no transactions in the queue *)
  let* () = produce_block_and_wait_for ~sequencer 1 in
  let* () = produce_block_and_wait_for ~sequencer 2 in

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
  let* () = produce_block_and_wait_for ~sequencer 3 in
  let* reveal =
    Operation.Manager.(
      operation [make ~fee:1000 ~source:new_account (reveal new_account ())])
      client_tezlink
  in
  let* (`OpHash op_reveal) =
    Operation.inject ~dont_wait:true reveal client_tezlink
  in
  let* () = produce_block_and_wait_for ~sequencer 4 in
  let* () =
    check_operations ~client:client_tezlink ~block:"4" ~expected:[op_reveal]
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
  let* () = produce_block_and_wait_for ~sequencer 5 in
  (* big transfer should go in but fail *)
  let* () =
    check_operations ~client:client_tezlink ~block:"5" ~expected:[big_transfer]
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
  let* () = produce_block_and_wait_for ~sequencer 6 in
  let* () =
    check_operations ~client:client_tezlink ~block:"6" ~expected:[big_but_in]
  in
  let* () = produce_block_and_wait_for ~sequencer 7 in
  let* () = check_operations ~client:client_tezlink ~block:"7" ~expected:[] in
  unit

let test_tezlink_gas_vs_l1 =
  register_tezlink_regression_test
    ~title:"Test Tezlink gas vs L1 operations"
    ~tags:["kernel"; "gas"; "l1"]
    ~bootstrap_accounts:[Constant.bootstrap1; Constant.bootstrap2]
  @@ fun {sequencer; client; _} _ ->
  let tezlink_endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

  let* client_tezlink = Client.init ~endpoint:tezlink_endpoint () in

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
  Regression.capture @@ sf "Gas used Tezlink: %d\n" gas_used_tezlink ;

  unit

let test_node_catchup_on_multichain =
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = 10 in
  let catchup_cooldown = 4 in
  register_tezlink_test
    ~max_blueprints_lag
    ~max_blueprints_catchup
    ~catchup_cooldown
    ~tags:["blueprint"; "catchup"]
    ~title:"EVM node catchup on multichain"
    ~time_between_blocks:Nothing
  @@
  fun {sequencer; sc_rollup_node; client; sc_rollup_address; node; _}
      _protocol
    ->
  let*@ _ = produce_block sequencer in
  let* () =
    bake_until_sync ~network:Tezlink ~sc_rollup_node ~sequencer ~client ()
  in
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

  let*@ rollup_level = rollup_level ~network:Tezlink sc_rollup_node in
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
  register_tezlink_test
    ~time_between_blocks:Nothing
    ~tags:["sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Tezlink Delayed deposit is included"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

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
  let* () =
    bake_until_sync ~network:Tezlink ~sc_rollup_node ~sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* balance =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  Check.((balance = Tez.of_int 1000) Tez.typ)
    ~error_msg:"Expected a 1000 tez on bootstrap1" ;
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
  register_tezlink_regression_test
    ~title:"Test of the big_map transfers"
    ~tags:["big_map"; "compatibility"; "operations"]
    ~bootstrap_accounts:[Constant.bootstrap1]
  @@ fun {sequencer; client; _} protocol ->
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in

  let endpoint =
    Client.(
      Foreign_endpoint
        Endpoint.
          {(Evm_node.rpc_endpoint_record sequencer) with path = "/tezlink"})
  in

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
let test_tezlink_upgrade_kernel_auto_sync =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_tezlink_upgrade_test
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["sequencer"; "upgrade"; "auto"; "sync"]
    ~title:
      "Tezlink rollup-node kernel upgrade is applied to the sequencer state."
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
        let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
        unit)
  in
  let* () =
    bake_until_sync ~network:Tezlink ~sc_rollup_node ~client ~sequencer ()
  in

  (* Produce a block after activation timestamp, both the rollup node
     and the sequencer will upgrade to latest kernel. *)
  let* _ =
    let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer in
    unit
  and* _upgrade = Evm_node.wait_for_successful_upgrade sequencer in

  let* () =
    bake_until_sync ~network:Tezlink ~sc_rollup_node ~client ~sequencer ()
  in

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

let () =
  test_observer_starts [Alpha] ;
  test_describe_endpoint [Alpha] ;
  test_tezlink_current_level [Alpha] ;
  test_tezlink_contract_info [Alpha] ;
  test_tezlink_list_entrypoints [Alpha] ;
  test_tezlink_contract_info_script [Alpha] ;
  test_tezlink_balance [Alpha] ;
  test_tezlink_manager_key [Alpha] ;
  test_tezlink_contract_info_on_liquidity_baking [Alpha] ;
  test_tezlink_counter [Alpha] ;
  test_tezlink_protocols [Alpha] ;
  test_tezlink_genesis_block_arg [Alpha] ;
  test_tezlink_expected_issuance [Alpha] ;
  test_tezlink_monitor_heads [Alpha] ;
  test_tezlink_version [Alpha] ;
  test_tezlink_header [Alpha] ;
  test_tezlink_constants [Alpha] ;
  test_tezlink_storage_rpc [Alpha] ;
  test_tezlink_produceBlock [Alpha] ;
  test_tezlink_hash_rpc [Alpha] ;
  test_tezlink_raw_json_cycle [Alpha] ;
  test_tezlink_chain_id [Alpha] ;
  test_tezlink_bootstrapped [Alpha] ;
  test_tezlink_transfer [Alpha] ;
  test_tezlink_observer_transfer [Alpha] ;
  test_tezlink_transfer_and_wait [Alpha] ;
  test_tezlink_reveal [Alpha] ;
  test_tezlink_block_info [Alpha] ;
  test_tezlink_storage_via_client [Alpha] ;
  test_tezlink_execution [Alpha] ;
  test_tezlink_bigmap_option [Alpha] ;
  test_tezlink_bigmap_counter [Alpha] ;
  test_tezlink_reveal_transfer_batch [Alpha] ;
  test_tezlink_batch [Alpha] ;
  test_tezlink_long_batch [Alpha] ;
  test_tezlink_bootstrap_block_info [Alpha] ;
  test_tezlink_sandbox () ;
  test_tezlink_internal_operation [Alpha] ;
  test_tezlink_internal_receipts [Alpha] ;
  test_event [Alpha] ;
  test_tezlink_prevalidation [Alpha] ;
  test_tezlink_prevalidation_gas_limit_lower_bound [Alpha] ;
  test_tezlink_validation_gas_limit [Alpha] ;
  test_tezlink_validation_counter [Alpha] ;
  test_tezlink_validation_balance [Alpha] ;
  test_tezlink_origination [Alpha] ;
  test_tezlink_forge_operations [Alpha] ;
  test_tezlink_gas_vs_l1 [Alpha] ;
  test_node_catchup_on_multichain [Alpha] ;
  test_delayed_deposit_is_included [Alpha] ;
  test_big_map_transfer [Alpha] ;
  test_tezlink_upgrade_kernel_auto_sync [Alpha]
