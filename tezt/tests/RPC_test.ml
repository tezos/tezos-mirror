(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component: RPC regression tests
   Invocation: dune exec tezt/tests/main.exe rpc regression

               Note that to reset the regression outputs, one can use
               the [--reset-regressions] option. When doing so, it is
               recommended to clear the output directory [tezt/_regressions/rpc]
               first to remove unused files in case some paths change.
   Subject: RPC regression tests capture the output of RPC calls and compare it
            with the output from the previous run. The test passes only if the
            outputs match exactly.
  *)

(* These hooks must be attached to every process that should be captured for
   regression testing *)
let hooks = Tezos_regression.hooks

type client_mode_tag = Client | Light | Proxy

let client_mode_tag_to_suffix = function
  | Client ->
      "client"
  | Light ->
      "light"
  | Proxy ->
      "proxy"

let create_client client_mode_tag : Client.t Lwt.t =
  let node_args = Node.[Synchronisation_threshold 0; Connections 0] in
  match client_mode_tag with
  | Client ->
      let* node = Node.init node_args in
      let* client = Client.init ~node () in
      Client.set_mode (Client (Some node)) client ;
      return client
  | Light ->
      let* (client, _) = Client.init_light () in
      return client
  | Proxy ->
      let* node = Node.init node_args in
      let* client = Client.init ~node () in
      Client.set_mode (Proxy node) client ;
      return client

(* A helper to register a RPC test environment with a node and a client for the
   given protocol version. *)
let check_rpc ~group_name ~protocols ~client_mode_tag
    ~(rpcs :
       (string * (Client.t -> unit Lwt.t) * Protocol.parameter_overrides option)
       list) () =
  let client_mode_suffix = client_mode_tag_to_suffix client_mode_tag in
  List.iter
    (fun (sub_group, rpc, parameter_overrides) ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:
          (sf
             "%s (mode %s) RPC regression tests: %s"
             group_name
             client_mode_suffix
             sub_group)
        ~tags:["rpc"; group_name; sub_group]
        ~output_file:
          ("rpc" // sf "%s.%s.%s" group_name client_mode_suffix sub_group)
        ~protocols
      @@ fun protocol ->
      (* Initialize a node with alpha protocol and data to be used for RPC calls.
         The log of the node is not captured in the regression output. *)
      let* client = create_client client_mode_tag in
      let* parameter_file =
        match parameter_overrides with
        | None ->
            Lwt.return_none
        | Some overrides ->
            let* file = Protocol.write_parameter_file ~protocol overrides in
            Lwt.return_some file
      in
      let* () = Client.activate_protocol ~protocol ?parameter_file client in
      Log.info "Activated protocol." ;
      let* _ = rpc client in
      unit)
    rpcs

(* Test the contracts RPC. *)
let test_contracts client =
  let test_implicit_contract contract_id =
    let* _ = RPC.Contracts.get ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_balance ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_counter ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_manager_key ~hooks ~contract_id client in
    unit
  in
  let* _contracts = RPC.Contracts.get_all ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* () = test_implicit_contract bootstrap in
  let* _ = RPC.Contracts.get_delegate ~hooks ~contract_id:bootstrap client in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let* _ = RPC.Contracts.get ~hooks ~contract_id:unallocated_implicit client in
  Log.info "Test non-delegated implicit contract" ;
  let simple_implicit = "simple" in
  let* simple_implicit_key =
    Client.gen_and_show_keys ~alias:simple_implicit client
  in
  let bootstrap1 = "bootstrap1" in
  let* () =
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~giver:bootstrap1
      ~receiver:simple_implicit_key.public_key_hash
      ~burn_cap:Constant.implicit_account_burn
      client
  in
  let* () = Client.bake_for client in
  let* () = test_implicit_contract simple_implicit_key.public_key_hash in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        rpc
          ?node:None
          ?hooks:(Some hooks)
          ?chain:None
          ?block:None
          ~contract_id:simple_implicit_key.public_key_hash
          client
        |> Process.check ~expect_failure:true)
      [ RPC.Contracts.spawn_get_delegate;
        RPC.Contracts.spawn_get_entrypoints;
        RPC.Contracts.spawn_get_script;
        RPC.Contracts.spawn_get_storage ]
  in
  Log.info "Test delegated implicit contract" ;
  let delegated_implicit = "delegated" in
  let* delegated_implicit_key =
    Client.gen_and_show_keys ~alias:delegated_implicit client
  in
  let* () =
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~giver:bootstrap1
      ~receiver:delegated_implicit
      ~burn_cap:Constant.implicit_account_burn
      client
  in
  let* () = Client.bake_for client in
  let* () =
    Client.set_delegate ~src:delegated_implicit ~delegate:bootstrap1 client
  in
  let* () = Client.bake_for client in
  let* () = test_implicit_contract delegated_implicit_key.public_key_hash in
  let* _ =
    RPC.Contracts.get_delegate
      ~hooks
      ~contract_id:delegated_implicit_key.public_key_hash
      client
  in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        rpc
          ?node:None
          ?hooks:(Some hooks)
          ?chain:None
          ?block:None
          ~contract_id:delegated_implicit_key.public_key_hash
          client
        |> Process.check ~expect_failure:true)
      [ RPC.Contracts.spawn_get_entrypoints;
        RPC.Contracts.spawn_get_script;
        RPC.Contracts.spawn_get_storage ]
  in
  let test_originated_contract contract_id =
    let* _ = RPC.Contracts.get ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_balance ~hooks ~contract_id client in
    let* () =
      RPC.Contracts.spawn_get_counter ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let* () =
      RPC.Contracts.spawn_get_manager_key ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let* _ =
      RPC.Contracts.big_map_get ~hooks ~contract_id ~data:big_map_key client
    in
    let* _ = RPC.Contracts.get_entrypoints ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_script ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_storage ~hooks ~contract_id client in
    unit
  in
  (* A smart contract without any big map or entrypoints *)
  Log.info "Test simple originated contract" ;
  let* originated_contract_simple =
    Client.originate_contract
      ~alias:"originated_contract_simple"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/str_id.tz"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
  in
  let* () = Client.bake_for client in
  let* () = test_originated_contract originated_contract_simple in
  (* A smart contract with a big map and entrypoints *)
  Log.info "Test advanced originated contract" ;
  let* originated_contract_advanced =
    Client.originate_contract
      ~alias:"originated_contract_advanced"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/big_map_entrypoints.tz"
      ~init:"Pair { Elt \"dup\" 0 } { Elt \"dup\" 1 ; Elt \"test\" 5 }"
      ~burn_cap:Tez.(of_int 3)
      client
  in
  let* () = Client.bake_for client in
  let* () = test_originated_contract originated_contract_advanced in
  let unique_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"test\" }, \"type\": { \"prim\": \"string\" \
       } }"
  in
  let* _ =
    RPC.Contracts.big_map_get
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:unique_big_map_key
      client
  in
  let duplicate_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"dup\" }, \"type\": { \"prim\": \"string\" } }"
  in
  let* _ =
    RPC.Contracts.big_map_get
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:duplicate_big_map_key
      client
  in
  unit

(* Test the delegates RPC for protocol Alpha. *)
let test_delegates_alpha client =
  let* _contracts = RPC.Contracts.get_all ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_balance ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_deactivated ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_delegated_balance ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_delegated_contracts ~hooks ~pkh:bootstrap client
  in
  let* _ = RPC.Delegates.get_frozen_balance ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle ~hooks ~pkh:bootstrap client
  in
  let* _ = RPC.Delegates.get_grace_period ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_staking_balance ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_voting_power ~hooks ~pkh:bootstrap client in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Delegates.spawn_get ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_balance ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_deactivated ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.spawn_get_delegated_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.spawn_get_grace_period ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_staking_balance ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_voting_power ~hooks ~pkh:unregistered_baker client
  in
  unit

(* Test the delegates RPC for the current Mainnet protocol. *)
let test_delegates_current_mainnet client =
  let* _contracts = RPC.Contracts.get_all ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_balance ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_deactivated ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_delegated_balance ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_delegated_contracts ~hooks ~pkh:bootstrap client
  in
  let* _ = RPC.Delegates.get_frozen_balance ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle ~hooks ~pkh:bootstrap client
  in
  let* _ = RPC.Delegates.get_grace_period ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_staking_balance ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_voting_power ~hooks ~pkh:bootstrap client in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Delegates.spawn_get ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_balance ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_deactivated ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.spawn_get_delegated_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.spawn_get_grace_period ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_staking_balance ~hooks ~pkh:unregistered_baker client
  in
  let* _ =
    RPC.Delegates.get_voting_power ~hooks ~pkh:unregistered_baker client
  in
  unit

(* Test the votes RPC for protocol Alpha. *)
let test_votes_alpha client =
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = Client.bake_for client in
  (* RPC calls *)
  let* _ = RPC.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Votes.get_ballots ~hooks client in
  let* _ = RPC.Votes.get_current_period ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Votes.get_listings ~hooks client in
  let* _ = RPC.Votes.get_proposals ~hooks client in
  let* _ = RPC.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = Client.bake_for client in
  (* RPC calls again *)
  let* _ = RPC.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Votes.get_ballots ~hooks client in
  let* _ = RPC.Votes.get_current_period ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Votes.get_listings ~hooks client in
  let* _ = RPC.Votes.get_proposals ~hooks client in
  let* _ = RPC.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ~hooks client in
  (* RPC calls again *)
  unit

(* Test the votes RPC for the current Mainnet protocol. *)
let test_votes_current_mainnet client =
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = Client.bake_for client in
  (* RPC calls *)
  let* _ = RPC.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Votes.get_ballots ~hooks client in
  let* _ = RPC.Votes.get_current_period ~hooks client in
  let* _ = RPC.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Votes.get_listings ~hooks client in
  let* _ = RPC.Votes.get_proposals ~hooks client in
  let* _ = RPC.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = Client.bake_for client in
  (* RPC calls again *)
  let* _ = RPC.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Votes.get_ballots ~hooks client in
  let* _ = RPC.Votes.get_current_period ~hooks client in
  let* _ = RPC.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Votes.get_listings ~hooks client in
  let* _ = RPC.Votes.get_proposals ~hooks client in
  let* _ = RPC.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ~hooks client in
  unit

(* Test the various other RPCs. *)
let test_others client =
  let* _ = RPC.get_constants ~hooks client in
  let* _ = RPC.get_baking_rights ~hooks client in
  let* _ = RPC.get_current_level ~hooks client in
  let* _ = RPC.get_endorsing_rights ~hooks client in
  let* _ = RPC.get_levels_in_current_cycle ~hooks client in
  unit

let start_with_acl address acl =
  let node = Node.create ~rpc_host:address [] in
  let* () = Node.config_init node [] in
  Node.Config_file.update node (JSON.update "rpc" (JSON.put ("acl", acl))) ;
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  Client.init ~node ()

(* Test access to RPC regulated with an ACL. *)
let test_whitelist address () =
  let whitelist =
    JSON.annotate ~origin:"whitelist"
    @@ `A
         [ `O
             [ ("address", `String address);
               ( "whitelist",
                 `A [`String "GET /describe/**"; `String "GET /chains/**"] ) ]
         ]
  in
  let* client = start_with_acl address whitelist in
  let* success_resp = Client.rpc GET ["chains"; "main"; "blocks"] client in
  let block_hash = JSON.geti 0 success_resp |> JSON.geti 0 |> JSON.as_string in
  let* () =
    if block_hash = "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2" then
      unit
    else
      Test.fail "Received an unexpected block hash from node: '%s'." block_hash
  in
  let* () =
    Client.spawn_rpc GET ["network"; "connections"] client
    |> Process.check_error ~exit_code:1
  in
  unit

let test_blacklist address () =
  let blacklist =
    JSON.annotate ~origin:"blacklist"
    @@ `A
         [ `O
             [ ("address", `String address);
               ("blacklist", `A [`String "GET /chains/**"]) ] ]
  in
  let* client = start_with_acl address blacklist in
  let* () =
    Client.spawn_rpc GET ["chains"; "main"; "blocks"] client
    |> Process.check_error ~exit_code:1
  in
  let* _success_resp = Client.rpc GET ["network"; "connections"] client in
  unit

let register () =
  let register_alpha client_mode_tag =
    check_rpc
      ~group_name:"alpha"
      ~protocols:[Protocol.Alpha]
      ~client_mode_tag
      ~rpcs:
        [ ("contracts", test_contracts, None);
          ("delegates", test_delegates_alpha, None);
          ( "votes",
            test_votes_alpha,
            Some
              (* reduced periods duration to get to testing vote period faster *)
              [ (["blocks_per_cycle"], Some "4");
                (["blocks_per_voting_period"], Some "4") ] );
          ("others", test_others, None) ]
      ()
  in
  let register_current_mainnet client_mode_tag =
    check_rpc
      ~group_name:"current"
      ~protocols:[Protocol.current_mainnet]
      ~client_mode_tag
      ~rpcs:
        [ ("contracts", test_contracts, None);
          ("delegates", test_delegates_current_mainnet, None);
          ( "votes",
            test_votes_current_mainnet,
            Some
              (* reduced periods duration to get to testing vote period faster *)
              [ (["blocks_per_cycle"], Some "4");
                (["blocks_per_voting_period"], Some "4") ] );
          ("others", test_others, None) ]
      ()
  in
  let modes = [Client; Proxy; Light] in
  List.iter
    (fun mode ->
      register_alpha mode ;
      register_current_mainnet mode)
    modes ;
  let addresses = ["localhost"; "127.0.0.1"] in
  let mk_title list_type address =
    Format.sprintf "Test RPC %s @@ %s" list_type address
  in
  List.iter
    (fun addr ->
      Test.register
        ~__FILE__
        ~title:(mk_title "whitelist" addr)
        ~tags:["rpc"; "acl"]
        (test_whitelist addr) ;
      Test.register
        ~__FILE__
        ~title:(mk_title "blacklist" addr)
        ~tags:["rpc"; "acl"]
        (test_blacklist addr))
    addresses
