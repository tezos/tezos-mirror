(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
   Invocation: dune exec tezt/tests/main.exe -- -f RPC_test.ml

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

(* From a test mode, return:
   - the client mode to use;
   - what to add in the test title;
   - what ~uses to add to the test. *)
let metadata_of_test_mode = function
  | `Client -> (`Client, "client", [])
  | `Client_data_dir_proxy_server ->
      (`Client, "proxy_server_data_dir", [Constant.octez_proxy_server])
  | `Client_rpc_proxy_server ->
      (`Client, "proxy_server_rpc", [Constant.octez_proxy_server])
  | `Light -> (`Light, "light", [])
  | `Proxy -> (`Proxy, "proxy", [])

let patch_protocol_parameters protocol = function
  | None -> Lwt.return_none
  | Some overrides ->
      let* file =
        Protocol.write_parameter_file
          ~base:(Either.right (protocol, None))
          (overrides protocol)
      in
      Lwt.return_some file

let initialize_chain_for_test client = function
  | `Client_data_dir_proxy_server | `Client_rpc_proxy_server ->
      (* Because the proxy server doesn't support genesis. *)
      Client.bake_for_and_wait client
  | `Client | `Light | `Proxy -> unit

let endpoint_of_test_mode_tag node = function
  | `Client | `Light | `Proxy -> Lwt.return_some Client.(Node node)
  | (`Client_rpc_proxy_server | `Client_data_dir_proxy_server) as
    proxy_server_mode ->
      let args =
        Some
          (match proxy_server_mode with
          | `Client_rpc_proxy_server -> [Proxy_server.Data_dir]
          | `Client_data_dir_proxy_server -> [])
      in
      let* proxy_server = Proxy_server.init ?args node in
      Lwt.return_some Client.(Proxy_server proxy_server)

(* A helper to register a RPC test environment with a node and a client for the
   given protocol version.
   - [test_mode_tag] specifies the client mode ([`Client], [`Light] or [`Proxy]).
   - [test_function] is the function to run to perform RPCs after the test
     environment is set up.
   - [parameter_overrides] specifies protocol parameters to change from the default
     sandbox parameters.
   - [nodes_args] specifies additional parameters to pass to the node.
   - [event_sections_levels] is passed to the node initialization.
   - [sub_group] is a short identifier for your test, used in the test title and as a tag.
   Additionally, since this uses [Protocol.register_regression_test], this has an
   implicit argument to specify the list of protocols to test. *)
let check_rpc_regression ~test_mode_tag ~test_function ?supports
    ?parameter_overrides ?nodes_args ?event_sections_levels sub_group =
  let client_mode_tag, title_tag, uses = metadata_of_test_mode test_mode_tag in
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "(mode %s) RPC regression tests: %s" title_tag sub_group)
    ~tags:["rpc"; title_tag; sub_group]
    ~uses:(fun _protocol -> uses)
    ?supports
  @@ fun protocol ->
  let* parameter_file =
    patch_protocol_parameters protocol parameter_overrides
  in
  (* Initialize a node with alpha protocol and data to be used for RPC calls.
     The log of the node is not captured in the regression output. *)
  let* node, client =
    Client.init_with_protocol
      ?parameter_file
      ?nodes_args
      ?event_sections_levels
      ~protocol
      client_mode_tag
      ()
  in
  let* () = initialize_chain_for_test client test_mode_tag in
  let* endpoint = endpoint_of_test_mode_tag node test_mode_tag in
  let* _ = test_function test_mode_tag protocol ?endpoint client in
  unit

(* Like [check_rpc_regression], but does not register a regression tests *)
let check_rpc ~test_mode_tag ~test_function ?parameter_overrides ?nodes_args
    sub_group =
  let client_mode_tag, title_tag, uses = metadata_of_test_mode test_mode_tag in
  Protocol.register_test
    ~__FILE__
    ~title:(sf "(mode %s) RPC tests: %s" title_tag sub_group)
    ~tags:["rpc"; title_tag; sub_group]
    ~uses:(fun _protocol -> uses)
  @@ fun protocol ->
  let* parameter_file =
    patch_protocol_parameters protocol parameter_overrides
  in
  (* Initialize a node with alpha protocol and data to be used for RPC calls. *)
  let* node, client =
    Client.init_with_protocol
      ?parameter_file
      ?nodes_args
      ~protocol
      client_mode_tag
      ()
  in
  let* () = initialize_chain_for_test client test_mode_tag in
  let* endpoint = endpoint_of_test_mode_tag node test_mode_tag in
  let* _ = test_function test_mode_tag protocol ?endpoint client in
  unit

(* Test the contracts RPC. *)
let test_contracts _test_mode_tag protocol ?endpoint client =
  let test_implicit_contract contract_id =
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract ~id:contract_id ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_balance ~id:contract_id ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_counter ~id:contract_id ()
    in
    let*! _ =
      Client.RPC.spawn ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_manager_key ~id:contract_id ()
    in
    unit
  in
  let* _contracts =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contracts ()
  in
  let* contracts =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegates ()
  in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* () = test_implicit_contract bootstrap in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract_delegate ~id:bootstrap ()
  in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract ~id:unallocated_implicit ()
  in
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
  let* () = Client.bake_for_and_wait client in
  let* () = test_implicit_contract simple_implicit_key.public_key_hash in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        let*? process =
          Client.RPC.spawn ?endpoint ~hooks client
          @@ rpc
               ?chain:None
               ?block:None
               ~id:simple_implicit_key.public_key_hash
               ()
        in
        Process.check ~expect_failure:true process)
      [
        RPC.get_chain_block_context_contract_delegate;
        RPC.get_chain_block_context_contract_entrypoints;
        RPC.get_chain_block_context_contract_script;
        RPC.get_chain_block_context_contract_storage;
      ]
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
  let* () = Client.bake_for_and_wait client in
  let*! () =
    Client.set_delegate ~src:delegated_implicit ~delegate:bootstrap1 client
  in
  let* () = Client.bake_for_and_wait client in
  let* () = test_implicit_contract delegated_implicit_key.public_key_hash in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract_delegate
         ~id:delegated_implicit_key.public_key_hash
         ()
  in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        let*? process =
          Client.RPC.spawn ?endpoint ~hooks client
          @@ rpc
               ?chain:None
               ?block:None
               ~id:delegated_implicit_key.public_key_hash
               ()
        in
        Process.check ~expect_failure:true process)
      [
        RPC.get_chain_block_context_contract_entrypoints;
        RPC.get_chain_block_context_contract_script;
        RPC.get_chain_block_context_contract_storage;
      ]
  in
  let test_originated_contract contract_id =
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract ~id:contract_id ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_balance ~id:contract_id ()
    in
    let*? process =
      Client.RPC.spawn ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_counter ~id:contract_id ()
    in
    let* () = Process.check ~expect_failure:true process in
    let*? process =
      Client.RPC.spawn ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_manager_key ~id:contract_id ()
    in
    let* () = Process.check ~expect_failure:true process in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.post_chain_block_context_contract_big_map_get
           ~id:contract_id
           ~data:(Data big_map_key)
           ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_entrypoints ~id:contract_id ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_script ~id:contract_id ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_context_contract_storage ~id:contract_id ()
    in
    unit
  in
  (* A smart contract without any big map or entrypoints *)
  Log.info "Test simple originated contract" ;
  let* _alias, originated_contract_simple =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
      ["mini_scenarios"; "str_id"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () = test_originated_contract originated_contract_simple in
  (* A smart contract with a big map and entrypoints *)
  Log.info "Test advanced originated contract" ;
  let* _alias, originated_contract_advanced =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Pair { Elt \"dup\" 0 } { Elt \"dup\" 1 ; Elt \"test\" 5 }"
      ~burn_cap:Tez.(of_int 3)
      client
      ["mini_scenarios"; "big_map_entrypoints"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () = test_originated_contract originated_contract_advanced in
  let unique_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"test\" }, \"type\": { \"prim\": \"string\" } \
       }"
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.post_chain_block_context_contract_big_map_get
         ~id:originated_contract_advanced
         ~data:(Data unique_big_map_key)
         ()
  in
  let duplicate_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"dup\" }, \"type\": { \"prim\": \"string\" } }"
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.post_chain_block_context_contract_big_map_get
         ~id:originated_contract_advanced
         ~data:(Data duplicate_big_map_key)
         ()
  in
  unit

let test_delegates_on_registered_alpha ~contracts ?endpoint client =
  Log.info "Test implicit baker contract" ;

  let bootstrap = List.hd contracts in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_delegate bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_full_balance bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_frozen_deposits bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_deactivated bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_delegated_balance bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_delegated_contracts bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_grace_period bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_staking_balance bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_voting_power bootstrap
  in
  unit

let test_adaptive_issuance_on_oxford ~contracts ?endpoint client =
  Log.info "Test adaptive issuance parameters retrieval" ;

  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_total_supply ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_total_frozen_stake ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_issuance_current_yearly_rate ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_issuance_current_yearly_rate_exact ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_issuance_issuance_per_minute ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_issuance_expected_issuance ()
  in
  let bootstrap = List.hd contracts in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_stakers bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint client ~hooks
    @@ RPC.get_chain_block_context_issuance_current_yearly_rate_details ()
  in
  let bootstrap = List.hd contracts in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_total_delegated_stake bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_staking_denominator bootstrap
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract_staking_numerator bootstrap
  in
  unit

let test_delegates_on_registered_hangzhou ~contracts ?endpoint client =
  Log.info "Test implicit baker contract" ;

  let bootstrap = List.hd contracts in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_balance bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_frozen_balance bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_frozen_balance_by_cycle bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_staking_balance bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_delegated_contracts bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_delegated_balance bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_deactivated bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_grace_period bootstrap
  in

  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_voting_power bootstrap
  in

  unit

(* Test the delegates RPC with unregistered baker for Tenderbake protocols. *)
let test_delegates_on_unregistered_alpha ~contracts ?endpoint client =
  Log.info "Test with a PKH that is not a registered baker contract" ;

  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let check_failure rpc =
    let*? process = Client.RPC.spawn ?endpoint ~hooks client @@ rpc in
    Process.check ~expect_failure:true process
  in
  let* () =
    check_failure @@ RPC.get_chain_block_context_delegate unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_full_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_frozen_deposits unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_deactivated unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_delegated_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_delegated_contracts
         unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_grace_period unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_staking_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_voting_power unregistered_baker
  in
  unit

(* Test the delegates RPC with unregistered baker for Emmy protocols. *)
let test_delegates_on_unregistered_hangzhou ~contracts ?endpoint client =
  Log.info "Test with a PKH that is not a registered baker contract" ;

  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;

  let check_failure rpc =
    let*? process = Client.RPC.spawn ?endpoint ~hooks client @@ rpc in
    Process.check ~expect_failure:true process
  in

  let* () =
    check_failure @@ RPC.get_chain_block_context_delegate unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_deactivated unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_delegated_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_delegated_contracts
         unregistered_baker
  in

  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_frozen_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_frozen_balance_by_cycle
         unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_grace_period unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_staking_balance unregistered_baker
  in
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_voting_power unregistered_baker
  in
  unit

let get_contracts ?endpoint client =
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contracts ()
  in
  let* contracts =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegates ()
  in

  Lwt.return contracts

(* Test the delegates RPC for the specified protocol. *)
let test_delegates _test_mode_tag _protocol ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  let* () = test_delegates_on_registered_alpha ~contracts ?endpoint client in
  test_delegates_on_unregistered_alpha ~contracts ?endpoint client

(* Test the adaptive issuance RPC. *)
let test_adaptive_issuance _test_mode_tag (_ : Protocol.t) ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  test_adaptive_issuance_on_oxford ~contracts ?endpoint client

(* Test the votes RPC. *)
let test_votes _test_mode_tag _protocol ?endpoint client =
  (* initialize data *)
  let proto_hash = Protocol.demo_noops_hash in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = Client.bake_for_and_wait client in
  (* RPC calls *)
  let call rpc = Client.RPC.call ?endpoint ~hooks client rpc in
  let* _ = call @@ RPC.get_chain_block_votes_ballot_list () in
  let* _ = call @@ RPC.get_chain_block_votes_ballots () in
  let* _ = call @@ RPC.get_chain_block_votes_current_period () in
  let* _ = call @@ RPC.get_chain_block_votes_current_proposal () in
  let* _ = call @@ RPC.get_chain_block_votes_current_quorum () in
  let* _ = call @@ RPC.get_chain_block_votes_listings () in
  let* _ = call @@ RPC.get_chain_block_votes_proposals () in
  let* _ = call @@ RPC.get_chain_block_votes_successor_period () in
  let* _ = call @@ RPC.get_chain_block_votes_total_voting_power () in
  (* bake to testing vote period and submit some ballots *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = Client.bake_for_and_wait client in
  (* RPC calls again *)
  let* _ = call @@ RPC.get_chain_block_votes_ballot_list () in
  let* _ = call @@ RPC.get_chain_block_votes_ballots () in
  let* _ = call @@ RPC.get_chain_block_votes_current_period () in
  let* _ = call @@ RPC.get_chain_block_votes_current_proposal () in
  let* _ = call @@ RPC.get_chain_block_votes_current_quorum () in
  let* _ = call @@ RPC.get_chain_block_votes_listings () in
  let* _ = call @@ RPC.get_chain_block_votes_proposals () in
  let* _ = call @@ RPC.get_chain_block_votes_successor_period () in
  let* _ = call @@ RPC.get_chain_block_votes_total_voting_power () in
  unit

(* Test the various other RPCs. *)
let test_misc_protocol _test_mode_tag protocol ?endpoint client =
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_constants ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_helper_baking_rights ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_helper_baking_rights
         ~delegate:Constant.bootstrap5.public_key_hash
         ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_helper_current_level ()
  in
  let* () =
    if Protocol.(number protocol >= number Oxford) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_context_denunciations ()
      in
      unit
    else unit
  in
  let* () =
    if Protocol.(number protocol <= number Oxford) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_helper_endorsing_rights ()
      in
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_helper_endorsing_rights
             ~delegate:Constant.bootstrap4.public_key_hash
             ()
      in
      unit
    else unit
  in
  let* () =
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_helper_attestation_rights ()
    in
    let* _ =
      Client.RPC.call ?endpoint ~hooks client
      @@ RPC.get_chain_block_helper_attestation_rights
           ~delegate:Constant.bootstrap4.public_key_hash
           ()
    in
    unit
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_helper_levels_in_current_cycle ()
  in
  unit

let mempool_hooks =
  let replace_variable string =
    let replacements =
      [
        ("edsig\\w{94}", "[SIGNATURE]");
        ("sig\\w{93}", "[SIGNATURE]");
        ("o\\w{50}", "[OPERATION_HASH]");
        ("B\\w{50}", "[BRANCH_HASH]");
        ("vh\\w{50}", "[BLOCK_PAYLOAD_HASH]");
      ]
    in
    List.fold_left
      (fun string (replace, by) ->
        Base.replace_string ~all:true (rex replace) ~by string)
      string
      replacements
  in
  {
    Tezos_regression.hooks with
    on_log = (fun output -> replace_variable output |> hooks.on_log);
  }

let get_client_port client =
  match
    Client.get_mode client |> Client.mode_to_endpoint
    |> Option.map Client.rpc_port
  with
  | Some port -> port
  | None ->
      Test.fail
        "Client for mempool rpc tests should be initialized with a node or a \
         proxy server only. Both have an endpoint and hence a RPC port so this \
         should not happen."

let mempool_node_flags =
  Node.
    [
      Synchronisation_threshold 0;
      (* Node does not need to be synchronized with peers before being
         bootstrapped *)
      Connections 1
      (* Number of connection allowed for each of our 2 nodes used in the
         mempool tests *);
    ]

let bake_empty_block ?endpoint client =
  let mempool = Client.empty_mempool_file () in
  (* We defer to using a low-level command here since we enforce using protocol
     in order to distinguish which form of the option we actually need, since
     the name has changed. *)
  Client.spawn_command
    ?endpoint
    client
    (["bake"; "for"; Constant.bootstrap1.alias; "--minimal-timestamp"]
    @
    let option_name = "--operations-pool" in
    [option_name; mempool])
  |> Process.check

(* Test the mempool RPCs: /chains/<chain>/mempool/...

   Tested RPCs:
   - GET pending_operations
   - GET monitor_operations
   - GET filter
   - POST filter

   Testing RPCs monitor_operations and pending_operations:

   We create a validated operation and three operations that fail
   to be validated. Each one of these failed operations has a different
   classification to test every possibility of the monitor_operations and
   pending_operations RPCs. The error classifications are the following :
   - Branch_refused (Branch error classification in protocol)
   - Branch_delayed (Temporary)
   - Refused (Permanent)
   - Outdated (Outdated)

   The main goal is to have a record of the encoding of the different
   operations returned by the RPC calls. This allows
   us to detect undesired changes. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/1900
   Test the following RPCs:
   - POST request_operations
   - POST ban_operation
   - POST unban_operation
   - POST unban_all_operations
*)
(* [mode] is only useful insofar as it helps adapting the test to specific
 * `Proxy mode constraint*)
let test_mempool _test_mode_tag protocol ?endpoint client =
  let* node = Node.init mempool_node_flags in
  let* () = Client.Admin.trust_address ?endpoint client ~peer:node in
  let* () = Client.Admin.connect_address ?endpoint client ~peer:node in
  let level = 1 in
  let* _ = Node.wait_for_level node level in
  let* node1_identity = Node.wait_for_identity node in
  let* () = Client.Admin.kick_peer ~peer:node1_identity client in
  (* Inject a transfer to increment the counter of bootstrap1. Bake with this
     transfer to trigger the counter_in_the_past error for the following
     transfer. *)
  let* _ =
    Operation.Manager.(inject ~force:true [make @@ transfer ()] client)
  in
  let* _ = Client.bake_for_and_wait ?endpoint client in

  let* _ = bake_empty_block ?endpoint client in

  (* Outdated operation after the second empty baking. *)
  let* () = Client.attest_for ~protocol ~force:true client in
  let* _ = bake_empty_block ?endpoint client in

  let monitor_path =
    (* To test the monitor_operations rpc we use curl since the client does
       not support streaming RPCs yet. *)
    sf
      "http://%s:%d/chains/main/mempool/monitor_operations?validated=true&outdated=true&branch_delayed=true&refused=true&branch_refused=true"
      Constant.default_host
      (get_client_port client)
  in
  let proc_monitor =
    (* monitor_operation rpc must be lanched before adding operations in order
       to record them. *)
    Process.spawn ~hooks:mempool_hooks "curl" ["-s"; monitor_path]
  in
  let* counter_json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.Account.public_key_hash
         ()
  in
  let counter = JSON.as_int counter_json in
  (* Branch_refused op: counter_in_the_past *)
  let* _ =
    Operation.Manager.(inject ~force:true [make ~counter @@ transfer ()] client)
  in
  let* counter_json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap2.Account.public_key_hash
         ()
  in
  let counter = JSON.as_int counter_json in
  (* Branch_delayed op: counter_in_the_future *)
  let* _ =
    Operation.Manager.(
      inject
        ~force:true
        [make ~source:Constant.bootstrap2 ~counter:(counter + 5) @@ transfer ()]
        client)
  in
  (* Refused op: fees_too_low *)
  let* _ =
    Operation.Manager.(
      inject
        [
          make ~source:Constant.bootstrap3 ~fee:0
          @@ transfer ~dest:Constant.bootstrap2 ();
        ]
        client)
  in
  (* Validated op *)
  let* _ =
    Operation.Manager.(
      inject
        [
          make ~source:Constant.bootstrap4
          @@ transfer ~dest:Constant.bootstrap2 ();
        ]
        client)
  in
  let* () = Client.Admin.connect_address ?endpoint ~peer:node client in
  let flush_waiter = Node_event_level.wait_for_flush node in
  let* _ =
    Prevalidator.bake_empty_block ~protocol ~endpoint:(Client.Node node) client
  in
  let* _ = flush_waiter in
  let* _output_monitor = Process.check_and_read_stdout proc_monitor in
  let* complete_mempool =
    Mempool.get_mempool ?endpoint ~hooks:mempool_hooks client
  in

  let* consensus_mempool =
    Mempool.get_mempool
      ?endpoint
      ~hooks:mempool_hooks
      ~validation_passes:[0]
      client
  in
  let expected_consensus_mempool =
    {Mempool.empty with outdated = complete_mempool.outdated}
  in
  Check.(
    (expected_consensus_mempool = consensus_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool %L, got %R") ;

  let* manager_mempool =
    Mempool.get_mempool
      ?endpoint
      ~hooks:mempool_hooks
      ~validation_passes:[3]
      client
  in
  let expected_manager_mempool =
    {
      Mempool.empty with
      validated = complete_mempool.validated;
      refused = complete_mempool.refused;
      branch_refused = complete_mempool.branch_refused;
      branch_delayed = complete_mempool.branch_delayed;
    }
  in
  Check.(
    (expected_manager_mempool = manager_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool %L, got %R") ;

  let* consensus_manager_mempool =
    Mempool.get_mempool
      ?endpoint
      ~hooks:mempool_hooks
      ~validation_passes:[0; 3]
      client
  in
  Check.(
    (consensus_manager_mempool = complete_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool %L, got %R") ;

  let* voting_anonymous_mempool =
    Mempool.get_mempool
      ?endpoint
      ~hooks:mempool_hooks
      ~validation_passes:[1; 2]
      client
  in
  Check.(
    (voting_anonymous_mempool = Mempool.empty)
      Mempool.classified_typ
      ~error_msg:"Expected mempool %L, got %R") ;

  let* _ =
    Lwt_list.iter_s
      (fun i ->
        let* mempool =
          Mempool.get_mempool
            ?endpoint
            ~hooks:mempool_hooks
            ~validated:(i = `Validated)
            ~refused:(i = `Refused)
            ~branch_delayed:(i = `Branch_delayed)
            ~branch_refused:(i = `Branch_refused)
            ~outdated:(i = `Outdated)
            client
        in
        let may_get_field field ophs = if i = field then ophs else [] in
        let expected_mempool =
          Mempool.
            {
              validated = may_get_field `Validated complete_mempool.validated;
              refused = may_get_field `Refused complete_mempool.refused;
              outdated = may_get_field `Outdated complete_mempool.outdated;
              branch_refused =
                may_get_field `Branch_refused complete_mempool.branch_refused;
              branch_delayed =
                may_get_field `Branch_delayed complete_mempool.branch_delayed;
              unprocessed = [];
            }
        in
        Check.(
          (expected_mempool = mempool)
            Mempool.classified_typ
            ~error_msg:"Expected mempool %L, got %R") ;
        unit)
      [`Validated; `Refused; `Branch_delayed; `Branch_refused; `Outdated]
  in
  (* We spawn a second monitor_operation RPC that monitor operations
     reclassified with errors. *)
  let proc_monitor =
    Process.spawn ~hooks:mempool_hooks "curl" ["-s"; monitor_path]
  in
  let* _ =
    Prevalidator.bake_empty_block ~protocol ~endpoint:(Client.Node node) client
  in
  let* _output_monitor = Process.check_and_read_stdout proc_monitor in

  Log.info
    ~color:Log.Color.BG.yellow
    "## Test RPCs [GET|POST /chains/main/mempool/filter]" ;
  let open Mempool.Config in
  Log.info
    "Check that GET filter returns the default configuration (the full default \
     configuration when the query parameter [include_default] is either absent \
     or set to [true], or an empty configuration when [include_default] is \
     [false])." ;
  let* () =
    check_get_filter_all_variations
      ~log:true
      ?endpoint
      ~hooks:mempool_hooks
      default
      client
  in
  Log.info
    "For various configurations, call POST filter then check that GET filter \
     returns the expected result (for all variations of [include_default)." ;
  let set_config_and_check msg config =
    Log.info "%s" msg ;
    let* output =
      post_filter ~log:true ?endpoint ~hooks:mempool_hooks config client
    in
    check_equal (fill_with_default config) (of_json output) ;
    check_get_filter_all_variations
      ~log:true
      ?endpoint
      ~hooks:mempool_hooks
      config
      client
  in
  let* () =
    set_config_and_check
      "Config 1: all fields provided and distinct from default."
      {
        minimal_fees = Some 1;
        minimal_nanotez_per_gas_unit = Some (2, 3);
        minimal_nanotez_per_byte = Some (4, 5);
        replace_by_fee_factor = Some (6, 7);
        max_operations = Some 8;
        max_total_bytes = Some 9;
      }
  in
  let* () =
    set_config_and_check
      "Config 2: omitted fields (which should be set to default)."
      {
        minimal_fees = Some 25;
        minimal_nanotez_per_gas_unit = None;
        minimal_nanotez_per_byte = Some (1050, 1);
        replace_by_fee_factor = None;
        max_operations = Some 2000;
        max_total_bytes = None;
      }
  in
  let* () =
    set_config_and_check
      "Config 3: {} (ie. all fields should be set to default)."
      {
        minimal_fees = None;
        minimal_nanotez_per_gas_unit = None;
        minimal_nanotez_per_byte = None;
        replace_by_fee_factor = None;
        max_operations = None;
        max_total_bytes = None;
      }
  in
  let* () =
    set_config_and_check
      "Config 4: divide by zero. (Should this config be invalid?)"
      {
        minimal_fees = None;
        minimal_nanotez_per_gas_unit = Some (100, 0);
        minimal_nanotez_per_byte = None;
        replace_by_fee_factor = None;
        max_operations = None;
        max_total_bytes = None;
      }
  in
  let config5 =
    {
      minimal_fees = None;
      minimal_nanotez_per_gas_unit = Some default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = Some (max_int, 1);
      replace_by_fee_factor = None;
      max_operations = Some default_max_operations;
      max_total_bytes = Some 0;
    }
  in
  let* () =
    set_config_and_check
      "Config 5: some fields omitted, some provided and equal to default, some \
       with extreme values."
      config5
  in
  Log.info
    "Post invalid configurations; check that the config is unchanged and that \
     event [invalid_mempool_filter_configuration] is witnessed." ;
  let config5_full = fill_with_default config5 in
  let endpoint =
    match endpoint with
    | Some endpoint -> endpoint
    | None -> Test.fail "This test requires an actual endpoint at %s" __LOC__
  in
  let test_invalid_config msg config_str =
    Log.info "%s" msg ;
    let waiter =
      Client.endpoint_wait_for
        endpoint
        "invalid_mempool_filter_configuration.v0"
        (Fun.const (Some ()))
    in
    let* output_post =
      post_filter_str ~log:true ~endpoint ~hooks:mempool_hooks config_str client
    in
    check_equal config5_full (of_json output_post) ;
    let* () = waiter in
    let* output_get = call_get_filter ~endpoint ~hooks:mempool_hooks client in
    check_equal config5_full (of_json output_get) ;
    unit
  in
  let* () =
    test_invalid_config
      "Invalid config 1: invalid field name"
      {|{ "minimal_fees": "100", "minimal_nanotez_per_gas_unit": [ "1050", "1" ], "minimal_nanotez_per_byte": [ "7", "5" ], "replace_by_fee_factor": ["21", "20"], "max_operations": 10, "max_total_bytes": 10_000_000, "invalid_field_name": 100 }|}
  in
  let* () =
    test_invalid_config
      "Invalid config 2: wrong type"
      {|{ "minimal_fees": "true" }|}
  in
  let* () =
    test_invalid_config
      "Invalid config 3: wrong type bis"
      {|{ "max_operations": "1000" }|}
  in
  let* () =
    test_invalid_config
      "Invalid config 4: not enough elements in fraction"
      {|{ "minimal_nanotez_per_gas_unit": [ "100" ] }|}
  in
  let* () =
    test_invalid_config
      "Invalid config 5: too many elements in fraction"
      {|{ "minimal_nanotez_per_gas_unit": [ "100", "1", "10" ] }|}
  in
  let* () =
    test_invalid_config
      "Invalid config 6: negative"
      {|{ "minimal_fees": "-1" }|}
  in
  test_invalid_config
    "Invalid config 7: negative bis"
    {|{ "max_operations": -1 }|}

let start_with_acl address acl =
  let* node =
    Node.init
      ~rpc_host:address
      ~patch_config:(JSON.update "rpc" (JSON.put ("acl", acl)))
      []
  in
  let endpoint = Client.Node node in
  Client.init ~endpoint ()

let test_network test_mode_tag _protocol ?endpoint client =
  let test peer_id =
    let call rpc = Client.RPC.call ?endpoint client rpc in
    let* _ = call @@ RPC.get_network_connection peer_id in
    let* _ = call RPC.get_network_greylist_clear in
    (* Peers *)
    let* _ = call RPC.get_network_peers in
    let* _ = call @@ RPC.get_network_peer peer_id in
    let* _ = call @@ RPC.get_network_peer_ban peer_id in
    let* _ = call @@ RPC.get_network_peer_banned peer_id in
    let* _ = call @@ RPC.get_network_peer_unban peer_id in
    let* _ = call @@ RPC.get_network_peer_untrust peer_id in
    let* _ = call @@ RPC.get_network_peer_trust peer_id in
    (* Connections *)
    let* points = call @@ RPC.get_network_points in
    let point_id =
      match points with
      | (p, _) :: _ -> p
      | _ -> Test.fail "Expected at least one point."
    in
    let* _ = call @@ RPC.get_network_point point_id in
    let* _ = call @@ RPC.get_network_point_ban point_id in
    let* _ = call @@ RPC.get_network_point_banned point_id in
    let* _ = call @@ RPC.get_network_point_unban point_id in
    let* _ = call @@ RPC.get_network_point_untrust point_id in
    let* _ = call @@ RPC.get_network_point_trust point_id in
    let* _ = call RPC.get_network_stat in
    let* _ = call RPC.get_network_version in
    let* _ = call RPC.get_network_versions in
    unit
  in
  match test_mode_tag with
  | `Client_data_dir_proxy_server | `Client_rpc_proxy_server ->
      Log.info "Skipping network RPCs" ;
      unit
  | `Light ->
      (* In light mode, the node is already connected to another node: use that as peer *)
      let* peers = Client.RPC.call ?endpoint client @@ RPC.get_network_peers in
      let peer_id =
        match peers with
        | (p, _) :: _ -> p
        | _ -> Test.fail "Expected at least one peer in light mode."
      in
      test peer_id
  | `Proxy | `Client ->
      (* Create a second node to get a peer for testing RPCs that are
         parameterized by a point or peer id *)
      let* node = Node.init ~name:"node" [Connections 1] in
      let* () = Client.Admin.trust_address ?endpoint client ~peer:node in
      let* () = Client.Admin.connect_address ?endpoint client ~peer:node in
      let* _ = Node.wait_for_level node 1 in
      let* client = Client.init ~endpoint:(Node node) () in
      let* peer_id =
        Client.RPC.call ~endpoint:(Node node) client RPC.get_network_self
      in
      test peer_id

let test_workers _test_mode_tag _protocol ?endpoint client =
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_worker_block_validator in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_workers_chain_validators
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_worker_chain_validator ()
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_worker_chain_validator_ddb ()
  in
  let* _ =
    Client.RPC.call ?endpoint client
    @@ RPC.get_worker_chain_validator_peers_validators ()
  in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_workers_prevalidators in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_worker_prevalidator () in
  unit

let test_misc_shell _test_mode_tag protocol ?endpoint client =
  let protocol_hash = Protocol.hash protocol in
  (* Turn off logging to avoid polluting the logs with the error schema *)
  let* _ =
    Client.RPC.call ?endpoint ~log_output:false client @@ RPC.get_errors
  in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_protocols in
  (* Turn off logging to avoid polluting the logs with full protocol
     source code *)
  let* _ =
    Client.RPC.call ?endpoint ~log_output:false client
    @@ RPC.get_protocol protocol_hash
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_fetch_protocol protocol_hash
  in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_stats_gc in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_stats_memory in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_config in
  unit

let test_chain _test_mode_tag _protocol ?endpoint client =
  let block_level = 3 in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_chain_blocks () in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_chain_chain_id () in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_invalid_blocks ()
  in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_chain_block () in
  let* _ =
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_context_constants_errors ()
  in
  let* _ =
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_context_nonce block_level
  in
  let* _ =
    (* Calls [/chains/main/blocks/head/context/raw/bytes] *)
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_context_raw ~ctxt_type:Bytes ~value_path:[] ()
  in
  let* delegates =
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_context_raw
         ~depth:2
         ~ctxt_type:Bytes
         ~value_path:["delegates"]
         ()
  in
  Check.(
    (JSON.unannotate delegates
    = `O
        [
          ( "ed25519",
            `O
              [
                ("02298c03ed7d454a101eb7022bc95f7e5f41ac78", `Null);
                ("a9ceae0f8909125492a7c4700acc59274cc6c846", `Null);
                ("c55cf02dbeecc978d9c84625dcae72bb77ea4fbd", `Null);
                ("dac9f52543da1aed0bc1d6b46bf7c10db7014cd6", `Null);
                ("e7670f32038107a59a2b9cfefae36ea21f5aa63c", `Null);
              ] );
        ])
      json_u
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  let* () =
    let spawn_get_chain_block_context_raw ?depth ~error_msg value_path =
      let*? process =
        Client.RPC.spawn ?endpoint client
        @@ RPC.get_chain_block_context_raw
             ?depth
             ~ctxt_type:Bytes
             ~value_path
             ()
      in
      Process.check_error ~msg:(rex error_msg) process
    in
    let* () =
      spawn_get_chain_block_context_raw
        ["non-existent"]
        ~error_msg:"No service found at this URL"
    in
    let* () =
      spawn_get_chain_block_context_raw
        ~depth:(-1)
        []
        ~error_msg:"Failed to parse argument 'depth' \\(\"-1\"\\)"
    in
    spawn_get_chain_block_context_raw
      ~depth:0
      ["non-existent"]
      ~error_msg:"No service found at this URL"
  in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_chain_block_hash () in
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_chain_block_header () in
  let* _ =
    (* Calls [/chains/main/blocks/head/header/protocol_data] *)
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_header_protocol_data ()
  in
  let* _ =
    (* Calls [/chains/main/blocks/head/header/protocol_data/raw] *)
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_header_protocol_data_raw ()
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_block_header_raw ()
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_block_header_raw ()
  in
  let* _ =
    let pkh = Constant.bootstrap1.public_key in
    let prefix = String.sub pkh 0 10 in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3234

       The result of this RPC endpoint is empty, but I guess it
       should be a list containing [pkh]. The original python
       test has the same result. *)
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_helper_complete prefix
  in
  let* _ =
    let* () = Client.bake_for_and_wait client in
    let* head_hash =
      Client.RPC.call ?endpoint client @@ RPC.get_chain_block_hash ()
    in
    let prefix = String.sub head_hash 0 10 in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3234

       The result of this RPC endpoint is empty, but I guess it
       should be a list containing [head_hash]. The original python
       test has the same result. *)
    Client.RPC.call ?endpoint client
    @@ RPC.get_chain_block_helper_complete prefix
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_block_live_blocks ()
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_block_metadata ()
  in
  let* _ =
    Client.RPC.call ?endpoint client @@ RPC.get_chain_block_operation_hashes ()
  in
  let* () =
    let validation_pass = 3 in
    let operation_offset = 0 in
    let* () =
      Client.transfer
        ~amount:Tez.one
        ~giver:"bootstrap1"
        ~receiver:"bootstrap2"
        client
    in
    let* () = Client.bake_for_and_wait client in
    let* _ =
      Client.RPC.call ?endpoint client
      @@ RPC.get_chain_block_operation_hashes ()
    in
    let* _ =
      Client.RPC.call ?endpoint client
      @@ RPC.get_chain_block_operation_hashes_of_validation_pass validation_pass
    in
    let* _ =
      Client.RPC.call ?endpoint client
      @@ RPC.get_chain_block_operation_hash
           ~validation_pass
           ~operation_offset
           ()
    in
    let* _ =
      Client.RPC.call ?endpoint client @@ RPC.get_chain_block_operations ()
    in
    let* _ =
      Client.RPC.call ?endpoint client
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass ()
    in
    let* _ =
      Client.RPC.call ?endpoint client
      @@ RPC.get_chain_block_operations_validation_pass
           ~validation_pass
           ~operation_offset
           ()
    in
    unit
  in
  unit

let test_deprecated _test_mode_tag protocol ?endpoint client =
  let check_rpc_not_found rpc =
    let*? process = Client.RPC.spawn ?endpoint ~hooks client @@ rpc in
    Process.check_error
      ~msg:
        (rex
           "(No service found at this URL|Did not find service|Rpc request \
            failed)")
      process
  in

  let implicit_account = Account.Bootstrap.keys.(0).public_key_hash in
  let make path = RPC_core.make GET path Fun.id in
  let* () =
    check_rpc_not_found
    @@ make
         [
           "chains";
           "main";
           "blocks";
           "head";
           "context";
           "contracts";
           implicit_account;
           "delegatable";
         ]
  in

  let* _alias, originated_account =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
      ["mini_scenarios"; "str_id"]
      protocol
  in
  let* () =
    Lwt_list.iter_s
      (fun contract_id ->
        check_rpc_not_found
        @@ make
             [
               "chains";
               "main";
               "blocks";
               "head";
               "contracts";
               contract_id;
               "spendable";
             ])
      [originated_account; implicit_account]
  in

  unit

(* Test access to RPC regulated with an ACL. *)
let test_whitelist address () =
  let whitelist =
    JSON.annotate ~origin:"whitelist"
    @@ `A
         [
           `O
             [
               ("address", `String address);
               ( "whitelist",
                 `A [`String "GET /describe/**"; `String "GET /chains/**"] );
             ];
         ]
  in
  let* client = start_with_acl address whitelist in
  let* success_resp = Client.RPC.call client @@ RPC.get_chain_blocks () in
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
         [
           `O
             [
               ("address", `String address);
               ("blacklist", `A [`String "GET /chains/**"]);
             ];
         ]
  in
  let* client = start_with_acl address blacklist in
  let* () =
    Client.spawn_rpc GET ["chains"; "main"; "blocks"] client
    |> Process.check_error ~exit_code:1
  in
  let* _success_resp = Client.RPC.call client @@ RPC.get_network_connections in
  unit

let binary_regression_test () =
  let node = Node.create [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* json_client = Client.init ~endpoint ~media_type:Json () in
  let* binary_client = Client.init ~endpoint ~media_type:Binary () in
  let call_rpc client =
    Client.spawn_rpc
      ~hooks
      GET
      ["chains"; "main"; "blocks"; "head"; "header"; "shell"]
      client
    |> Process.check_and_read_stdout
  in
  let* json_result = call_rpc json_client in
  let* binary_result = call_rpc binary_client in
  let* decoded_binary_result =
    Codec.decode ~name:"block_header.shell" binary_result
  in
  if JSON.unannotate decoded_binary_result = Ezjsonm.from_string json_result
  then Lwt.return_unit
  else Test.fail "Unexpected binary answer"

let test_node_binary_mode address () =
  let node = Node.create ~rpc_host:address [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [Media_type Binary] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint ~media_type:Json () in
  Client.spawn_rpc GET ["chains"; "main"; "blocks"] client
  |> Process.check_error ~exit_code:1

let test_no_service_at_valid_prefix address () =
  let node = Node.create ~rpc_host:address [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint () in
  let* () =
    Client.spawn_rpc ~better_errors:true GET ["chains"; "main"] client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex
              "Fatal error:\n\
              \  No service found at this URL (but this is a valid prefix)*\n\
               *")
  in
  unit

let register protocols =
  Regression.register
    ~__FILE__
    ~title:"Binary RPC regression tests"
    ~tags:["rpc"; "regression"; "binary"]
    ~uses:[Constant.octez_codec]
    binary_regression_test ;
  let register protocols test_mode_tag =
    let check_rpc_regression ?parameter_overrides ?supports ?nodes_args
        ?event_sections_levels ~test_function sub_group =
      check_rpc_regression
        ~test_mode_tag
        ?parameter_overrides
        ?nodes_args
        ?event_sections_levels
        ~test_function
        sub_group
        ?supports
        protocols
    in
    let check_rpc ?parameter_overrides ?nodes_args ~test_function sub_group =
      check_rpc
        ~test_mode_tag
        ?parameter_overrides
        ?nodes_args
        ~test_function
        sub_group
        protocols
    in
    let consensus_threshold _protocol = [(["consensus_threshold"], `Int 0)] in
    check_rpc_regression
      "contracts"
      ~test_function:test_contracts
      ~parameter_overrides:consensus_threshold ;
    check_rpc_regression
      "delegates"
      ~test_function:test_delegates
      ~parameter_overrides:consensus_threshold ;
    check_rpc_regression
      "adaptive_issuance"
      ~supports:Protocol.(From_protocol (number Nairobi + 1))
      ~test_function:test_adaptive_issuance ;
    check_rpc_regression
      "votes"
      ~test_function:test_votes
      ~parameter_overrides:(fun protocol ->
        (* reduced periods duration to get to testing vote period faster *)
        (* We need nonce_revelation_threshold < blocks_per_cycle for sanity
           checks *)
        [
          (["blocks_per_cycle"], `Int 4);
          (["cycles_per_voting_period"], `Int 1);
          (["nonce_revelation_threshold"], `Int 3);
        ]
        @ consensus_threshold protocol) ;
    check_rpc_regression
      "misc_protocol"
      ~test_function:test_misc_protocol
      ~parameter_overrides:consensus_threshold ;
    (match test_mode_tag with
    | `Client_data_dir_proxy_server | `Client_rpc_proxy_server | `Light -> ()
    | _ ->
        check_rpc_regression
          "mempool"
          ~test_function:test_mempool
          ~nodes_args:mempool_node_flags
          ~event_sections_levels:[("prevalidator", `Debug)]
        (* Level [Debug] is needed to witness prevalidator event
           [invalid_mempool_filter_configuration]. *)) ;
    check_rpc
      "network"
      ~test_function:test_network
      ~parameter_overrides:consensus_threshold
      ~nodes_args:[Connections 1] ;
    (match test_mode_tag with
    (* No worker RPCs in these modes *)
    | `Client_data_dir_proxy_server | `Client_rpc_proxy_server -> ()
    | _ ->
        check_rpc
          "workers"
          ~test_function:test_workers
          ~parameter_overrides:consensus_threshold) ;
    (match test_mode_tag with
    (* No misc shell RPCs in these modes *)
    | `Client_data_dir_proxy_server | `Client_rpc_proxy_server -> ()
    | _ ->
        check_rpc
          "misc_shell"
          ~test_function:test_misc_shell
          ~parameter_overrides:consensus_threshold) ;
    (match test_mode_tag with
    (* No chain RPCs in these modes *)
    | `Client_data_dir_proxy_server | `Client_rpc_proxy_server -> ()
    | _ ->
        check_rpc
          "chain"
          ~test_function:test_chain
          ~parameter_overrides:consensus_threshold) ;
    check_rpc "deprecated" ~test_function:test_deprecated
  in
  List.iter
    (register protocols)
    [
      `Client;
      `Light;
      `Proxy;
      `Client_data_dir_proxy_server;
      `Client_rpc_proxy_server;
    ] ;

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
        (test_blacklist addr) ;
      Test.register
        ~__FILE__
        ~title:(mk_title "no_service_at_valid_prefix" addr)
        ~tags:["rpc"]
        (test_no_service_at_valid_prefix addr) ;
      Test.register
        ~__FILE__
        ~title:(mk_title "node_binary_mode" addr)
        ~tags:["rpc"; "binary"]
        (test_node_binary_mode addr))
    addresses
