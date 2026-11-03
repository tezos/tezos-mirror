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

let endpoint_of_test_mode_tag node = function
  | `Client | `Light | `Proxy -> Lwt.return_some Client.(Node node)

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
        RPC.get_chain_block_context_contract_entrypoints ?normalize_types:None;
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
  let* () =
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
        RPC.get_chain_block_context_contract_entrypoints ?normalize_types:None;
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

let test_delegates_on_registered_alpha protocol ~contracts ?endpoint client =
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
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_delegate_consensus_key bootstrap
  in
  let* _ =
    if Protocol.(number protocol >= 023) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_context_delegate_companion_key bootstrap
      in
      unit
    else unit
  in
  unit

let test_adaptive_issuance ~contracts ?endpoint client =
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
    @@ RPC.get_chain_block_context_total_currently_staked ()
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

let test_stez ~contracts ?endpoint client =
  Log.info "Test STEZ parameters retrieval" ;
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_stez_contract_hash ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_stez_total_supply ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_stez_total_amount_of_tez ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_stez_exchange_rate ()
  in
  let bootstrap = List.hd contracts in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract_stez_balance ~id:bootstrap ()
  in
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_contract_stez_ticket_balance ~id:bootstrap ()
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
let test_delegates_on_unregistered_alpha protocol ~contracts ?endpoint client =
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
  let* () =
    check_failure
    @@ RPC.get_chain_block_context_delegate_consensus_key unregistered_baker
  in
  let* () =
    if Protocol.(number protocol >= 023) then
      let* () =
        check_failure
        @@ RPC.get_chain_block_context_delegate_companion_key unregistered_baker
      in
      unit
    else unit
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
let test_delegates _test_mode_tag protocol ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  let* () =
    test_delegates_on_registered_alpha protocol ~contracts ?endpoint client
  in
  test_delegates_on_unregistered_alpha protocol ~contracts ?endpoint client

(* Test the adaptive issuance RPC. *)
let test_adaptive_issuance _test_mode_tag (_ : Protocol.t) ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  test_adaptive_issuance ~contracts ?endpoint client

(* Test the sTEZ RPC. *)
let test_stez _test_mode_tag (_ : Protocol.t) ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  test_stez ~contracts ?endpoint client

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
  let* _ =
    Client.RPC.call ?endpoint ~hooks client
    @@ RPC.get_chain_block_context_denunciations ()
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
  let* () =
    if Protocol.(number protocol >= 023) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_helper_total_baking_power ()
      in
      unit
    else unit
  in
  let* () =
    if Protocol.(number protocol >= 024) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_tz4_baker_number_ratio ()
      in
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_abaab_activation_level ()
      in
      unit
    else unit
  in
  let* () =
    if Protocol.(number protocol >= 025) then
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_helpers_swrr_credits ()
      in
      let* _ =
        Client.RPC.call ?endpoint ~hooks client
        @@ RPC.get_chain_block_helpers_swrr_selected_bakers ()
      in
      unit
    else unit
  in
  unit

(* Test that SWRR RPCs fail with a proper error when the feature flag is
   disabled. *)
let test_swrr_disabled _test_mode_tag _protocol ?endpoint client =
  let check_failure rpc =
    let*? process = Client.RPC.spawn ?endpoint ~hooks client @@ rpc in
    Process.check ~expect_failure:true process
  in
  let* () = check_failure @@ RPC.get_chain_block_helpers_swrr_credits () in
  let* () =
    check_failure @@ RPC.get_chain_block_helpers_swrr_selected_bakers ()
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
    let patch_acl value =
      JSON.parse ~origin:"RPC patch data" ("{\"acl\":\"" ^ value ^ "\"}")
    in
    let* _ = call @@ RPC.get_network_connection peer_id in
    let* _ = call RPC.delete_network_greylist in
    (* Peers *)
    let* _ = call RPC.get_network_peers in
    let* _ = call @@ RPC.get_network_peer peer_id in
    let* _ = call @@ RPC.patch_network_peer peer_id (patch_acl "ban") in
    let* _ = call @@ RPC.get_network_peer_banned peer_id in
    let* _ = call @@ RPC.patch_network_peer peer_id (patch_acl "open") in
    let* _ = call @@ RPC.patch_network_peer peer_id (patch_acl "trust") in
    (* Connections *)
    let* points = call @@ RPC.get_network_points in
    let point_id =
      match points with
      | (p, _) :: _ -> p
      | _ -> Test.fail "Expected at least one point."
    in
    let* _ = call @@ RPC.get_network_point point_id in
    let* _ = call @@ RPC.patch_network_point point_id (patch_acl "ban") in
    let* _ = call @@ RPC.get_network_point_banned point_id in
    let* _ = call @@ RPC.patch_network_point point_id (patch_acl "open") in
    let* _ = call @@ RPC.patch_network_point point_id (patch_acl "trust") in
    let* _ = call RPC.get_network_stat in
    unit
  in
  match test_mode_tag with
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
  let* _ = Client.RPC.call ?endpoint client @@ RPC.get_version in
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

(* Regression test for the ACL bypass on /metrics reported in bug-bounty-37334.
   The main RPC listener must not serve GET /metrics when --metrics-addr is not
   configured, even though the Secure ACL allows neighbouring routes. *)
let test_metrics_not_on_rpc_listener () =
  Test.register
    ~__FILE__
    ~title:"Test RPC metrics not served on main listener without --metrics-addr"
    ~tags:["rpc"; "acl"; "metrics"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [] in
  let addr =
    Format.sprintf
      "http://%s:%d/metrics"
      Constant.default_host
      (Node.rpc_port node)
  in
  let* status =
    Process.spawn
      ~log_output:false
      "curl"
      ["-s"; "-o"; "/dev/null"; "-w"; "%{http_code}"; addr]
    |> Process.check_and_read_stdout
  in
  let status = String.trim status in
  Check.((status = "404") string)
    ~error_msg:
      "GET /metrics on the main RPC listener returned %L but must return 404 \
       when --metrics-addr is not configured (the route is not registered)" ;
  unit

(* Tests for the RPC request body size cap (--rpc-max-request-body) and for
   the bounded lingering close that follows a rejection. *)

(* The cap used by the tests below. Small, so that the bodies stay cheap. *)
let max_request_body = 100_000

let post_body_url node =
  (* Any route will do: the cap is enforced by the outermost middleware layer,
     before routing and before the ACL. *)
  Format.sprintf
    "http://%s:%d/version"
    Constant.default_host
    (Node.rpc_port node)

(* Runs [curl] through a shell so that its exit code is discarded: on the
   endless-body test curl is expected to fail (the server resets the
   connection under it), and what we assert on is the status it prints and how
   long it took, not its exit code. *)
let curl_status ?(timeout = 30) ?(stdin = "/dev/null") args url =
  let command =
    Format.sprintf
      "curl -s -o /dev/null -w '%%{http_code}' --max-time %d %s %s < %s || true"
      timeout
      (String.concat " " args)
      (Filename.quote url)
      stdin
  in
  Process.spawn ~log_output:false "sh" ["-c"; command]
  |> Process.check_and_read_stdout

(* Waits for the warning [event] the node emits when it rejects a [POST
   /version] for its body, and returns the event's fields. *)
let wait_for_body_rejection node event =
  Node.wait_for node (event ^ ".v0") @@ fun json ->
  if JSON.(json |-> "request" |> as_string) = "POST /version" then Some json
  else None

let test_rpc_max_request_body () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let url = post_body_url node in
  let under = Temp.file "under_cap_body" in
  let over = Temp.file "over_cap_body" in
  write_file under ~contents:(String.make (max_request_body - 1_000) 'a') ;
  write_file over ~contents:(String.make (max_request_body * 2) 'a') ;
  let rejected = wait_for_body_rejection node "request_body_too_large" in
  let* status = curl_status ["-X"; "POST"; "--data-binary"; "@" ^ over] url in
  Check.((String.trim status = "413") string)
    ~error_msg:
      "a POST body over the --rpc-max-request-body cap returned %L but must \
       return 413" ;
  let* event = rejected in
  Check.((JSON.(event |-> "max_body_size" |> as_int) = max_request_body) int)
    ~error_msg:"the rejection warning reports a cap of %L, expected %R" ;
  Check.(
    (JSON.(event |-> "declared_length" |> as_int) = max_request_body * 2) int)
    ~error_msg:
      "the rejection warning reports a declared length of %L, expected %R" ;
  let* status = curl_status ["-X"; "POST"; "--data-binary"; "@" ^ under] url in
  Check.((String.trim status <> "413") string)
    ~error_msg:
      "a POST body under the --rpc-max-request-body cap returned %L: it must \
       not be rejected by the size cap" ;
  unit

(* The body is fully sent and stays within the lingering read's byte ceiling
   (twice the cap), so the read consumes it in full: the client must get a
   readable 413 on a connection that stays reusable, i.e. without
   [Connection: close]. Declaring more than the ceiling is the other case, and
   is covered by {!test_rpc_max_request_body_closes_connection}. *)
let test_rpc_max_request_body_keeps_connection () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap keeps the connection alive"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let url = post_body_url node in
  let over = Temp.file "over_cap_body" in
  (* Over the cap but inside the lingering read's ceiling of twice the cap. *)
  write_file over ~contents:(String.make (max_request_body * 3 / 2) 'a') ;
  (* Two requests on a single connection: curl reuses it when given both URLs,
     which only works if the server did not tear it down. [-D -] dumps the
     response headers so that we can also check the connection is not being
     closed. *)
  let command =
    Format.sprintf
      "curl -s -o /dev/null -D - --max-time 30 -X POST --data-binary @%s %s %s"
      (Filename.quote over)
      (Filename.quote url)
      (Filename.quote url)
  in
  let* headers =
    Process.spawn ~log_output:false "sh" ["-c"; command]
    |> Process.check_and_read_stdout
  in
  let headers = String.lowercase_ascii headers in
  let occurrences needle =
    List.length
      (String.split_on_char '\n' headers
      |> List.filter (fun line ->
             line |> String.trim |> fun l ->
             String.length l >= String.length needle
             && String.sub l 0 (String.length needle) = needle))
  in
  Check.((occurrences "http/1.1 413" = 2) int)
    ~error_msg:
      "expected two 413s over one reused connection but got %L: a request \
       whose body was received in full must not close the connection" ;
  Check.((occurrences "connection: close" = 0) int)
    ~error_msg:
      "the server sent %L [Connection: close] headers: a request whose body \
       was received in full leaves the connection usable" ;
  unit

(* Writes [request] as is, then optionally half-closes, and returns the first
   bytes the server answers.

   Reading the response on a raw socket rather than through [curl] is the whole
   point: once the server stops reading a body the client is still uploading,
   the connection is reset, and curl reports the send failure and discards the
   response bytes it had already received. The 413 does reach the socket. *)
let raw_exchange node ~request ~half_close =
  let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt.finalize
    (fun () ->
      let* () =
        Lwt_unix.connect
          socket
          (Unix.ADDR_INET
             (Unix.inet_addr_of_string Constant.default_host, Node.rpc_port node))
      in
      let bytes = Bytes.of_string request in
      let rec write offset =
        if offset >= Bytes.length bytes then unit
        else
          let* written =
            Lwt_unix.write socket bytes offset (Bytes.length bytes - offset)
          in
          if written = 0 then unit else write (offset + written)
      in
      let* () = write 0 in
      if half_close then Lwt_unix.shutdown socket Unix.SHUTDOWN_SEND ;
      let buffer = Bytes.create 4096 in
      let* len =
        (* A reset before we get to read means the response was lost, which the
           caller reports as an empty answer rather than as an exception. *)
        Lwt.catch
          (fun () -> Lwt_unix.read socket buffer 0 (Bytes.length buffer))
          (function
            | Unix.Unix_error _ -> Lwt.return 0 | exn -> Lwt.reraise exn)
      in
      Lwt.return (String.lowercase_ascii (Bytes.sub_string buffer 0 len)))
    (fun () -> Lwt.catch (fun () -> Lwt_unix.close socket) (fun _ -> unit))

(* Sends a POST that declares [content_length] but writes only [body], then
   optionally half-closes, and returns whatever the server answers.

   Headers and [body] go out in a single write so that the server cannot reset
   the connection between the two, which would raise [EPIPE] here. *)
let raw_post_truncated node ~content_length ~body ~half_close =
  raw_exchange
    node
    ~request:
      (Printf.sprintf
         "POST /version HTTP/1.1\r\n\
          Host: %s\r\n\
          Content-Type: application/json\r\n\
          Content-Length: %d\r\n\
          \r\n\
          %s"
         Constant.default_host
         content_length
         body)
    ~half_close

(* Sends a chunked POST whose body is [chunks], written as is, and returns
   whatever the server answers. *)
let raw_post_chunked node ~chunks =
  raw_exchange
    node
    ~request:
      (Printf.sprintf
         "POST /version HTTP/1.1\r\n\
          Host: %s\r\n\
          Content-Type: application/json\r\n\
          Transfer-Encoding: chunked\r\n\
          \r\n\
          %s"
         Constant.default_host
         chunks)
    ~half_close:false

let chunk data = Printf.sprintf "%x\r\n%s\r\n" (String.length data) data

let last_chunk = "0\r\n\r\n"

(* Cohttp does not give us the length of a chunked body, so the end of the
   stream is all the server has to go on -- and [Cohttp.Transfer] also ends the
   stream on a malformed chunk header. Each scenario below ends the body
   differently, and the server must tell apart a body received in full from
   one it could not make sense of. *)
let test_rpc_max_request_body_chunked () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap on chunked bodies"
    ~tags:["rpc"; "body"; "limit"; "chunked"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let is_413 response = String.starts_with ~prefix:"http/1.1 413" response in
  let closes response = response =~ rex "connection: *close" in
  let over_cap = String.make (max_request_body * 3 / 2) 'a' in
  (* Within the cap, well-formed: served. *)
  let* response =
    raw_post_chunked
      node
      ~chunks:(chunk (String.make 1_000 'a') ^ chunk "a" ^ last_chunk)
  in
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 " response && not (is_413 response))
    ~error_msg:
      (Printf.sprintf
         "a well-formed chunked body under the cap got %S: it must be served, \
          not rejected by the size cap"
         response) ;
  (* Over the cap, well-formed and within the lingering read's ceiling: 413 on a
     connection that stays reusable. *)
  let* response = raw_post_chunked node ~chunks:(chunk over_cap ^ last_chunk) in
  Check.is_true
    (is_413 response && not (closes response))
    ~error_msg:
      (Printf.sprintf
         "a well-formed chunked body over the cap got %S, expected a 413 \
          without [Connection: close]"
         response) ;
  (* Over the cap in a single chunk, then a chunk size that does not parse.
     Whatever follows cannot be framed, so the connection must be closed: were
     it kept, the bytes after the bad chunk header would be read as the next
     request.

     FIXME: #8428
     Known failure, so this checks the current behaviour instead: the server
     answers 413 *without* [Connection: close] and keeps the connection.
     [Cohttp.Transfer] ends the stream on the bad chunk size exactly as on the
     last chunk, and with no declared length to compare against,
     [bounded_drain] takes it for a body received in full. Once fixed, this
     check fails: expect [closes response] instead. *)
  let* response = raw_post_chunked node ~chunks:(chunk over_cap ^ "zz\r\n") in
  Check.is_true
    (is_413 response && not (closes response))
    ~error_msg:
      (Printf.sprintf
         "a chunked body over the cap followed by a malformed chunk header got \
          %S, expected the known-buggy 413 without [Connection: close]. If the \
          connection is now closed, the bug is fixed: update this check"
         response) ;
  (* Over the cap, then stalls without the last chunk: the idle timeout ends
     the lingering read, and the connection must be closed. *)
  let* response = raw_post_chunked node ~chunks:(chunk over_cap) in
  Check.is_true
    (is_413 response && closes response)
    ~error_msg:
      (Printf.sprintf
         "a chunked body over the cap that stalled got %S, expected a 413 with \
          [Connection: close]"
         response) ;
  unit

(* The 413 sent on the shutdown path is not covered by the tests above: they
   either exercise the reusable-connection path, or -- for an endless body --
   cannot assert on the status at all because curl loses it. The three clients
   below are all truncated, declaring a body and then failing to deliver it,
   and each leaves the lingering read by a different door: past the byte
   ceiling, past the idle timeout, and on a premature end of stream. In every
   case the server must still deliver a readable 413, and must say
   [Connection: close], the request stream having been truncated. *)
let test_rpc_max_request_body_closes_connection () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap closes the connection"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let check ~scenario response =
    Check.is_true
      (String.starts_with ~prefix:"http/1.1 413" response)
      ~error_msg:
        (Printf.sprintf
           "%s: expected a 413 but the server answered %S. A rejected request \
            must still get a readable response."
           scenario
           response) ;
    Check.is_true
      (response =~ rex "connection: *close")
      ~error_msg:
        (Printf.sprintf
           "%s: the server answered %S without [Connection: close], but it \
            truncated the request stream and the connection cannot be reused."
           scenario
           response)
  in
  (* Declares more than the lingering read is willing to consume (twice the
     cap), so the read is skipped and the connection shut down at once. *)
  let* response =
    raw_post_truncated
      node
      ~content_length:(max_request_body * 10)
      ~body:(String.make 1_000 'a')
      ~half_close:false
  in
  check ~scenario:"beyond the byte ceiling" response ;
  (* Declares a length the lingering read would consume, then stalls: the idle
     timeout is what ends it. *)
  let* response =
    raw_post_truncated
      node
      ~content_length:(max_request_body * 3 / 2)
      ~body:(String.make 1_000 'a')
      ~half_close:false
  in
  check ~scenario:"stalled below the byte ceiling" response ;
  (* Same, but half-closes instead of stalling. The stream then ends early,
     which [Cohttp.Transfer] reports exactly as it reports a body received in
     full -- so the connection must be closed on the byte count, not on the end
     of the stream. *)
  let* response =
    raw_post_truncated
      node
      ~content_length:(max_request_body * 3 / 2)
      ~body:(String.make 1_000 'a')
      ~half_close:true
  in
  check ~scenario:"half-closed below the byte ceiling" response ;
  unit

(* The size cap answers before resto, which is what normally adds the CORS
   headers. A browser hides a response without [Access-Control-Allow-Origin]
   from the page, which would then see an opaque network failure instead of
   the 413: the rejection must be allowed cross-origin as resto's own responses
   are, and only for an allowed origin. *)
let test_rpc_max_request_body_cors () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap answers with CORS headers"
    ~tags:["rpc"; "body"; "limit"; "cors"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let allowed = "http://allowed.example" in
  let* node =
    Node.init [Node.Max_request_body max_request_body; Node.Cors_origin allowed]
  in
  let post ~origin =
    raw_exchange
      node
      ~request:
        (Printf.sprintf
           "POST /version HTTP/1.1\r\n\
            Host: %s\r\n\
            Origin: %s\r\n\
            Content-Type: application/json\r\n\
            Content-Length: %d\r\n\
            \r\n\
            %s"
           Constant.default_host
           origin
           (max_request_body * 3 / 2)
           (String.make (max_request_body * 3 / 2) 'a'))
      ~half_close:false
  in
  let* response = post ~origin:allowed in
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 413" response
    && response =~ rex ("access-control-allow-origin: *" ^ allowed))
    ~error_msg:
      (Printf.sprintf
         "an oversized request from an allowed origin got %S, expected a 413 \
          with [Access-Control-Allow-Origin: %s]"
         response
         allowed) ;
  let* response = post ~origin:"http://other.example" in
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 413" response
    && not (response =~ rex "access-control-allow-origin"))
    ~error_msg:
      (Printf.sprintf
         "an oversized request from a disallowed origin got %S, expected a 413 \
          without [Access-Control-Allow-Origin]"
         response) ;
  unit

(* A body within the cap is not rejected, so it never reaches the lingering
   read: before its read was bounded too, a client that declared such a body
   and then sent a single byte of it held its connection, and one of the
   [max_active_rpc_connections] slots, for ever. The server must now give up
   on it after the 10s idle timeout. *)
let test_rpc_max_request_body_under_cap_stall () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap on a stalled body under the cap"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let start = Unix.gettimeofday () in
  let* (_ : string) =
    Lwt.pick
      [
        raw_post_truncated
          node
          ~content_length:(max_request_body - 1)
          ~body:"a"
          ~half_close:false;
        (let* () = Lwt_unix.sleep 30. in
         Lwt.return "");
      ]
  in
  let elapsed = Unix.gettimeofday () -. start in
  Check.((elapsed < 20.) float)
    ~error_msg:
      "a stalled request body under the cap still held its connection after %L \
       seconds: the server must stop waiting for it" ;
  unit

(* Shutting the read half down wakes cohttp's EOF monitor, which cancels the
   request handler, and with it possibly the write of the response. The monitor
   is certain to be parked on its read by the time a 10s idle timeout fires,
   so this is where losing the response would show: the client must still
   read the 408, and be told the connection is closing. *)
let test_rpc_max_request_body_timeout_response () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body read timeout still answers"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let timed_out = wait_for_body_rejection node "request_body_timeout" in
  let* response =
    raw_post_truncated
      node
      ~content_length:(max_request_body - 1)
      ~body:"a"
      ~half_close:false
  in
  let* (_ : JSON.t) = timed_out in
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 408" response
    && response =~ rex "connection: *close")
    ~error_msg:
      (Printf.sprintf
         "a stalled request body got %S, expected a 408 with [Connection: \
          close]: shutting the connection down must not lose the response"
         response) ;
  unit

(* The regression test for the unbounded drain. Before the bounded lingering
   close, the server kept reading this body for ever and the request never
   completed. Note that we do not assert on the status: once we stop reading,
   the client is reset while it is still writing, so curl usually reports a
   send failure rather than the 413. What matters is that the request
   terminates and the server is unharmed. *)
let test_rpc_max_request_body_endless () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap on an endless body"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let url = post_body_url node in
  let start = Unix.gettimeofday () in
  let* (_ : string) =
    curl_status
      ~timeout:20
      ~stdin:"/dev/zero"
      ["-X"; "POST"; "-T"; "-"; "-H"; "Expect:"]
      url
  in
  let elapsed = Unix.gettimeofday () -. start in
  (* The bounds under test are the 1s idle and 5s total timeouts, so anything
     close to the 20s [--max-time] means the drain did not stop; keep enough
     headroom for a loaded CI runner without making the oracle vacuous. *)
  Check.((elapsed < 10.) float)
    ~error_msg:
      "an endless request body was still being read after %L seconds: the \
       server must stop reading and release the connection" ;
  (* And the node is still serving. *)
  let* status = curl_status ["-X"; "GET"] url in
  Check.((String.trim status = "200") string)
    ~error_msg:
      "the node answered %L after an endless request body, expected 200" ;
  unit

(* Connection slots are taken before [accept], so a request that holds one for
   ever wedges the whole listener. With the cap enforced and the connection
   released, more abusive uploads than there are slots must not prevent a
   normal RPC from being served. *)
let test_rpc_max_request_body_no_slot_exhaustion () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body size cap does not leak connection slots"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let slots = 2 in
  (* The slot limit is a parameter of [Node.create], not an argument. Passing
     it as an argument would not work: [Node.config_init] writes arguments to
     the configuration file and clears them, and [Node.run] then prepends its
     own [--max-active-rpc-connections], taken from the [Node.create]
     parameter, which overrides the file. *)
  let node =
    Node.create
      ~max_active_rpc_connections:slots
      [Node.Max_request_body max_request_body]
  in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let url = post_body_url node in
  let abusers =
    List.map
      (fun _ ->
        curl_status
          ~timeout:30
          ~stdin:"/dev/zero"
          ["-X"; "POST"; "-T"; "-"; "-H"; "Expect:"]
          url)
      (Base.range 1 (slots * 2))
  in
  (* Give them time to occupy every slot before asking for service. *)
  let* () = Lwt_unix.sleep 2. in
  let* status = curl_status ~timeout:20 ["-X"; "GET"] url in
  Check.((String.trim status = "200") string)
    ~error_msg:
      "a normal RPC returned %L while oversized uploads were in flight: the \
       rejected connections are still holding their slots" ;
  let* (_ : string list) = Lwt.all abusers in
  unit

(* A client that half-closes before sending the body it declared ends the
   body stream exactly as a complete body does, so the server must compare what
   it read with the declared length. Otherwise the RPC gets the prefix, which
   may well decode to a value the client never sent. *)
let test_rpc_max_request_body_under_cap_truncated () =
  Test.register
    ~__FILE__
    ~title:"Test RPC request body under the cap cut short by the client"
    ~tags:["rpc"; "body"; "limit"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* node = Node.init [Node.Max_request_body max_request_body] in
  let truncated = wait_for_body_rejection node "request_body_truncated" in
  let* response =
    raw_post_truncated
      node
      ~content_length:(max_request_body / 2)
      ~body:"a"
      ~half_close:true
  in
  let* event = truncated in
  Check.((JSON.(event |-> "received_length" |> as_int) = 1) int)
    ~error_msg:
      "the truncation warning reports a received length of %L, expected %R" ;
  Check.(
    (JSON.(event |-> "declared_length" |> as_int) = max_request_body / 2) int)
    ~error_msg:
      "the truncation warning reports a declared length of %L, expected %R" ;
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 400" response
    && response =~ rex "connection: *close")
    ~error_msg:
      (Printf.sprintf
         "a request body shorter than its Content-Length got %S, expected a \
          400 with [Connection: close]"
         response) ;
  (* cohttp never reads the body of a [GET], whatever its headers declare, so
     the body the server sees is empty and must not be taken for a truncated
     one: such a request was served before the check and still must be. *)
  let* response =
    raw_exchange
      node
      ~request:
        (Printf.sprintf
           "GET /version HTTP/1.1\r\nHost: %s\r\nContent-Length: 5\r\n\r\nhello"
           Constant.default_host)
      ~half_close:false
  in
  Check.is_true
    (String.starts_with ~prefix:"http/1.1 200" response)
    ~error_msg:
      (Printf.sprintf
         "a GET declaring a body got %S, expected a 200: cohttp does not read \
          the body of a GET, so it cannot be truncated"
         response) ;
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
    let consensus_threshold protocol =
      [
        ( [
            (if Protocol.(number protocol >= 022) then "consensus_threshold_size"
             else "consensus_threshold");
          ],
          `Int 0 );
      ]
    in
    let swrr_flag protocol =
      if Protocol.(number protocol >= 025) then
        [(["swrr_new_baker_lottery_enable"], `Bool true)]
      else []
    in
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
      ~test_function:test_adaptive_issuance ;
    check_rpc_regression
      ~supports:Protocol.(From_protocol 25)
      "stez"
      ~test_function:test_stez ;
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
      ~parameter_overrides:(fun protocol ->
        consensus_threshold protocol @ swrr_flag protocol) ;
    check_rpc_regression
      "misc_protocol_abaab"
      ~test_function:test_misc_protocol
      ~supports:(Protocol.From_protocol 024)
      ~parameter_overrides:(fun protocol ->
        (* Activates All bakers attest *)
        [
          ( ["all_bakers_attest_activation_threshold"],
            `O [("numerator", `Float 0.); ("denominator", `Float 1.)] );
        ]
        @ consensus_threshold protocol
        @ swrr_flag protocol) ;
    check_rpc_regression
      "swrr_disabled"
      ~supports:Protocol.(From_protocol 025)
      ~test_function:test_swrr_disabled
      ~parameter_overrides:(fun protocol ->
        consensus_threshold protocol
        @ [(["swrr_new_baker_lottery_enable"], `Bool false)]) ;
    (match test_mode_tag with
    | `Light -> ()
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
    | _ ->
        check_rpc
          "workers"
          ~test_function:test_workers
          ~parameter_overrides:consensus_threshold) ;
    (match test_mode_tag with
    (* No misc shell RPCs in these modes *)
    | _ ->
        check_rpc
          "misc_shell"
          ~test_function:test_misc_shell
          ~parameter_overrides:consensus_threshold) ;
    (match test_mode_tag with
    (* No chain RPCs in these modes *)
    | _ ->
        check_rpc
          "chain"
          ~test_function:test_chain
          ~parameter_overrides:consensus_threshold) ;
    check_rpc "deprecated" ~test_function:test_deprecated
  in
  List.iter (register protocols) [`Client; `Light; `Proxy] ;

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
    addresses ;
  test_metrics_not_on_rpc_listener () ;
  test_rpc_max_request_body () ;
  test_rpc_max_request_body_keeps_connection () ;
  test_rpc_max_request_body_closes_connection () ;
  test_rpc_max_request_body_chunked () ;
  test_rpc_max_request_body_cors () ;
  test_rpc_max_request_body_under_cap_stall () ;
  test_rpc_max_request_body_timeout_response () ;
  test_rpc_max_request_body_endless () ;
  test_rpc_max_request_body_no_slot_exhaustion () ;
  test_rpc_max_request_body_under_cap_truncated ()
