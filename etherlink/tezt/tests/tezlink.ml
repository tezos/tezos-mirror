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

let test_describe_endpoint =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "rpc"; "describe"]
    ~title:"Test the /describe endpoint"
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
      ~enable_revm:false
      ~l2_chains
      ~rpc_server:Evm_node.Resto
      ~spawn_rpc:(Port.fresh ())
      ~time_between_blocks:Nothing
      protocol
  in
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

let register_tezlink_test ~title ~tags ?bootstrap_accounts ?bootstrap_contracts
    scenario protocols =
  register_all
    ~kernels:[Kernel.Latest]
    ~title
    ~tags:("tezlink" :: tags)
    ~l2_setups:
      [
        {
          (Evm_node.default_l2_setup ~l2_chain_id:12) with
          l2_chain_family = "Michelson";
          tez_bootstrap_accounts = bootstrap_accounts;
          tez_bootstrap_contracts = bootstrap_contracts;
        };
      ]
    ~use_multichain:Register_with_feature
    ~rpc_server:Evm_node.Resto
    ~time_between_blocks:Nothing
    scenario
    protocols

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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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

let test_tezlink_storage =
  let contract = Michelson_contracts.concat_hello () in
  register_tezlink_test
    ~title:"Test of the storage rpc"
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

let account_rpc sequencer account key =
  let path =
    sf
      "/tezlink/chains/main/blocks/head/context/contracts/%s/%s"
      account.Account.public_key_hash
      key
  in

  let* res =
    Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
    |> Runnable.run
  in
  return @@ JSON.parse ~origin:"curl_protocols" res

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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      ~enable_revm:false
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path)
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
      Curl.get_raw ~args:["-v"] (Evm_node.endpoint sequencer ^ path block)
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
  let* balance1 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap1.alias client
  in
  let* balance2 =
    Client.get_balance_for ~endpoint ~account:Constant.bootstrap2.alias client
  in
  Check.(
    (Tez.to_mutez balance1
    = Tez.to_mutez bootstrap_balance - Tez.to_mutez amount)
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

let () =
  test_describe_endpoint [Alpha] ;
  test_tezlink_current_level [Alpha] ;
  test_tezlink_contract_info [Alpha] ;
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
  test_tezlink_produceBlock [Alpha] ;
  test_tezlink_hash_rpc [Alpha] ;
  test_tezlink_raw_json_cycle [Alpha] ;
  test_tezlink_chain_id [Alpha] ;
  test_tezlink_bootstrapped [Alpha] ;
  test_tezlink_transfer [Alpha] ;
  test_tezlink_reveal [Alpha] ;
  test_tezlink_block_info [Alpha] ;
  test_tezlink_storage [Alpha] ;
  test_tezlink_execution [Alpha]
