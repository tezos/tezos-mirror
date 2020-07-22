(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
let hooks = Regression.scrubbing_hooks

type client_mode_tag = Client | Mockup | Proxy

let client_mode_tag_to_suffix = function
  | Client ->
      "client"
  | Mockup ->
      "mockup"
  | Proxy ->
      "proxy"

let client_mode_tag_to_mode client_mode_tag node : Client.mode =
  match client_mode_tag with
  | Client ->
      Client (Some node)
  | Mockup ->
      Mockup
  | Proxy ->
      Proxy node

(* A helper to register a RPC test environment with a node and a client for the
   given protocol version. *)
let check_rpc ~group_name ~protocol ~client_mode_tag
    ~(rpcs :
       (string * (Client.t -> unit Lwt.t) * Protocol.parameter_overrides option)
       list) () =
  let client_mode_suffix = client_mode_tag_to_suffix client_mode_tag in
  List.iter
    (fun (sub_group, rpc, parameter_overrides) ->
      Regression.register
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
      @@ fun () ->
      (* Initialize a node with alpha protocol and data to be used for RPC calls.
         The log of the node is not captured in the regression output. *)
      let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
      let* client = Client.init ~node () in
      Client.set_mode (client_mode_tag_to_mode client_mode_tag node) client ;
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
      (* remember baker contracts to be able to use their alias *)
      let* () =
        match protocol with
        | Protocol.Alpha ->
            Client.remember_baker_contracts client
        | Carthage | Delphi | Edo ->
            unit
      in
      let* _ = rpc client in
      unit)
    rpcs

(* Test the contracts RPC for protocol Alpha. *)
let test_contracts_alpha client =
  let module Protocol_rpc = RPC.Proto_alpha in
  let test_implicit_contract contract_id =
    let* _ = Protocol_rpc.Contract.get ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_balance ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_counter ~hooks ~contract_id client in
    let* _ =
      Protocol_rpc.Contract.get_manager_key ~hooks ~contract_id client
    in
    unit
  in
  let* contracts = Protocol_rpc.Contract.get_all ~hooks client in
  let* bakers = RPC.Proto_alpha.Baker.get_all client in
  Log.info "Test baker contract" ;
  let baker = List.hd bakers in
  let* _ = RPC.Proto_alpha.Contract.get ~hooks ~contract_id:baker client in
  let* _ =
    Protocol_rpc.Contract.get_balance ~hooks ~contract_id:baker client
  in
  let* _ =
    Protocol_rpc.Contract.get_counter ~hooks ~contract_id:baker client
  in
  let* _ =
    Protocol_rpc.Contract.spawn_get_manager_key
      ~hooks
      ~contract_id:baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    Protocol_rpc.Contract.get_delegate ~hooks ~contract_id:baker client
  in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let* _ =
    Protocol_rpc.Contract.get ~hooks ~contract_id:unallocated_implicit client
  in
  Log.info "Test non-delegated implicit contract" ;
  let simple_implicit = "simple" in
  let* simple_implicit_key =
    Client.gen_and_show_keys ~alias:simple_implicit client
  in
  let bootstrap1 = "bootstrap1" in
  let baker1 = "baker1" in
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
      [ Protocol_rpc.Contract.spawn_get_delegate;
        Protocol_rpc.Contract.spawn_get_entrypoints;
        Protocol_rpc.Contract.spawn_get_script;
        Protocol_rpc.Contract.spawn_get_storage ]
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
    Client.set_delegate ~src:delegated_implicit ~delegate:baker1 client
  in
  let* () = Client.bake_for client in
  let* () = test_implicit_contract delegated_implicit_key.public_key_hash in
  let* _ =
    Protocol_rpc.Contract.get_delegate
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
      [ Protocol_rpc.Contract.spawn_get_entrypoints;
        Protocol_rpc.Contract.spawn_get_script;
        Protocol_rpc.Contract.spawn_get_storage ]
  in
  let test_originated_contract contract_id =
    let* _ = Protocol_rpc.Contract.get ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_balance ~hooks ~contract_id client in
    let* () =
      Protocol_rpc.Contract.spawn_get_counter ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let* () =
      Protocol_rpc.Contract.spawn_get_manager_key ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let* _ =
      Protocol_rpc.Contract.big_map_get
        ~hooks
        ~contract_id
        ~data:big_map_key
        client
    in
    let* _ =
      Protocol_rpc.Contract.get_entrypoints ~hooks ~contract_id client
    in
    let* _ = Protocol_rpc.Contract.get_script ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_storage ~hooks ~contract_id client in
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
    Protocol_rpc.Contract.big_map_get
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
    Protocol_rpc.Contract.big_map_get
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:duplicate_big_map_key
      client
  in
  unit

(* Test the contracts RPC for protocol 007. *)
let test_contracts_007 client =
  let module Protocol_rpc = RPC.Proto_007 in
  let test_implicit_contract contract_id =
    let* _ = Protocol_rpc.Contract.get ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_balance ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_counter ~hooks ~contract_id client in
    let* _ =
      Protocol_rpc.Contract.get_manager_key ~hooks ~contract_id client
    in
    unit
  in
  let* contracts = Protocol_rpc.Contract.get_all ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* () = test_implicit_contract bootstrap in
  let* _ =
    Protocol_rpc.Contract.get_delegate ~hooks ~contract_id:bootstrap client
  in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let* _ =
    Protocol_rpc.Contract.get ~hooks ~contract_id:unallocated_implicit client
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
  let* () = Client.bake_for ~key:bootstrap1 client in
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
      [ Protocol_rpc.Contract.spawn_get_delegate;
        Protocol_rpc.Contract.spawn_get_entrypoints;
        Protocol_rpc.Contract.spawn_get_script;
        Protocol_rpc.Contract.spawn_get_storage ]
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
  let* () = Client.bake_for ~key:bootstrap1 client in
  let* () =
    Client.set_delegate ~src:delegated_implicit ~delegate:bootstrap1 client
  in
  let* () = Client.bake_for ~key:bootstrap1 client in
  let* () = test_implicit_contract delegated_implicit_key.public_key_hash in
  let* _ =
    Protocol_rpc.Contract.get_delegate
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
      [ Protocol_rpc.Contract.spawn_get_entrypoints;
        Protocol_rpc.Contract.spawn_get_script;
        Protocol_rpc.Contract.spawn_get_storage ]
  in
  let test_originated_contract contract_id =
    let* _ = Protocol_rpc.Contract.get ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_balance ~hooks ~contract_id client in
    let* () =
      Protocol_rpc.Contract.spawn_get_counter ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let* () =
      Protocol_rpc.Contract.spawn_get_manager_key ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let* _ =
      Protocol_rpc.Contract.big_map_get
        ~hooks
        ~contract_id
        ~data:big_map_key
        client
    in
    let* _ =
      Protocol_rpc.Contract.get_entrypoints ~hooks ~contract_id client
    in
    let* _ = Protocol_rpc.Contract.get_script ~hooks ~contract_id client in
    let* _ = Protocol_rpc.Contract.get_storage ~hooks ~contract_id client in
    unit
  in
  (* A smart contract without any big map or entrypoints *)
  Log.info "Test simple originated contract" ;
  let* originated_contract_simple =
    Client.originate_contract
      ~alias:"originated_contract_simple"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_007/str_id.tz"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
  in
  let* () = Client.bake_for ~key:bootstrap1 client in
  let* () = test_originated_contract originated_contract_simple in
  (* A smart contract with a big map and entrypoints *)
  Log.info "Test advanced originated contract" ;
  let* originated_contract_advanced =
    Client.originate_contract
      ~alias:"originated_contract_advanced"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_007/big_map_entrypoints.tz"
      ~init:"Pair { Elt \"dup\" 0 } { Elt \"dup\" 1 ; Elt \"test\" 5 }"
      ~burn_cap:Tez.(of_int 3)
      client
  in
  let* () = Client.bake_for ~key:bootstrap1 client in
  let* () = test_originated_contract originated_contract_advanced in
  let unique_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"test\" }, \"type\": { \"prim\": \"string\" \
       } }"
  in
  let* _ =
    Protocol_rpc.Contract.big_map_get
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
    Protocol_rpc.Contract.big_map_get
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:duplicate_big_map_key
      client
  in
  unit

(* Test the delegates RPC for protocol Alpha. *)
let test_delegates_alpha client =
  (* The delegates RPC has been replaced by bakers RPC, but delegates RPC should
     be preserved for compatibility. In the compatibility RPC, hashes of
     baker accounts' consensus keys are being used in place of the previous
     implicit accounts public key hashes. *)
  let* bakers = RPC.Proto_alpha.Baker.get_all client in
  Log.info "Test implicit baker contract" ;
  let baker_hash = List.hd bakers in
  let* baker_pk = RPC.Proto_alpha.Baker.get_consensus_key ~baker_hash client in
  let baker_pkh =
    Tezos_crypto.Signature.(
      Public_key.hash baker_pk |> Public_key_hash.to_b58check)
  in
  let* _ = RPC.Proto_alpha.Delegate.get ~hooks ~pkh:baker_pkh client in
  let* _ = RPC.Proto_alpha.Delegate.get_balance ~hooks ~pkh:baker_pkh client in
  let* _ =
    RPC.Proto_alpha.Delegate.get_deactivated ~hooks ~pkh:baker_pkh client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_delegated_balance ~hooks ~pkh:baker_pkh client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_delegated_contracts
      ~hooks
      ~pkh:baker_pkh
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_frozen_balance ~hooks ~pkh:baker_pkh client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:baker_pkh
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_grace_period ~hooks ~pkh:baker_pkh client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_staking_balance ~hooks ~pkh:baker_pkh client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_voting_power ~hooks ~pkh:baker_pkh client
  in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker bakers) ;
  let* _ =
    RPC.Proto_alpha.Delegate.spawn_get ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_alpha.Delegate.spawn_get_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_deactivated
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.spawn_get_delegated_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_delegated_contracts
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_frozen_balance
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.spawn_get_grace_period
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_staking_balance
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_alpha.Delegate.get_voting_power
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  unit

(* Test the delegates RPC for protocol 007. *)
let test_delegates_007 client =
  let* contracts = RPC.Proto_007.Contract.get_all client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* _ = RPC.Proto_007.Delegate.get ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Proto_007.Delegate.get_balance ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Proto_007.Delegate.get_deactivated ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_delegated_balance ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_delegated_contracts ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_frozen_balance ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:bootstrap
      client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_grace_period ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_staking_balance ~hooks ~pkh:bootstrap client
  in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Proto_007.Delegate.spawn_get ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_007.Delegate.spawn_get_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_007.Delegate.get_deactivated
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_007.Delegate.spawn_get_delegated_balance
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_007.Delegate.get_delegated_contracts
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_frozen_balance
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_007.Delegate.get_frozen_balance_by_cycle
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Proto_007.Delegate.spawn_get_grace_period
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Proto_007.Delegate.get_staking_balance
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  unit

(* Test the votes RPC for protocol Alpha. *)
let test_votes_alpha client =
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~key:"baker1" ~proto_hash client in
  let* () = Client.bake_for client in
  (* RPC calls *)
  let* _ = RPC.Proto_alpha.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_ballots ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_period ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_listings ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_proposals ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_total_voting_power ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = Client.bake_for client in
  let* () = Client.bake_for client in
  let* () = Client.submit_ballot ~key:"baker1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"baker2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"baker3" ~proto_hash Pass client in
  let* () = Client.bake_for client in
  (* RPC calls again *)
  let* _ = RPC.Proto_alpha.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_ballots ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_period ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_listings ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_proposals ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_successor_period ~hooks client in
  let* _ = RPC.Proto_alpha.Votes.get_total_voting_power ~hooks client in
  (* RPC calls again *)
  unit

(* Test the votes RPC for protocol 007. *)
let test_votes_007 client =
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = Client.bake_for client ~key:"bootstrap1" in
  (* RPC calls *)
  let* _ = RPC.Proto_007.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_ballots ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_listings ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_proposals ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = Client.bake_for client ~key:"bootstrap1" in
  let* () = Client.bake_for client ~key:"bootstrap1" in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = Client.bake_for client ~key:"bootstrap1" in
  (* RPC calls again *)
  let* _ = RPC.Proto_007.Votes.get_ballot_list ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_ballots ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_period_kind ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_proposal ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_current_quorum ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_listings ~hooks client in
  let* _ = RPC.Proto_007.Votes.get_proposals ~hooks client in
  unit

(* Test the various other RPCs. *)
let test_others (module Protocol_rpc : RPC_intf.COMMON_PROTOCOL_ALL) client =
  let* _ = Protocol_rpc.get_constants ~hooks client in
  let* _ = Protocol_rpc.get_baking_rights ~hooks client in
  let* _ = Protocol_rpc.get_current_level ~hooks client in
  let* _ = Protocol_rpc.get_endorsing_rights ~hooks client in
  let* _ = Protocol_rpc.get_levels_in_current_cycle ~hooks client in
  unit

let register () =
  let register_alpha client_mode_tag =
    check_rpc
      ~group_name:"alpha"
      ~protocol:Protocol.Alpha
      ~client_mode_tag
      ~rpcs:
        [ ("contracts", test_contracts_alpha, None);
          ("delegates", test_delegates_alpha, None);
          ( "votes",
            test_votes_alpha,
            Some
              (* reduced periods duration to get to testing vote period faster *)
              [ (["blocks_per_cycle"], Some "4");
                (["blocks_per_voting_period"], Some "4") ] );
          ("others", test_others (module RPC.Proto_alpha), None) ]
      ()
  in
  let register_007 client_mode_tag =
    check_rpc
      ~group_name:"007"
      ~protocol:Protocol.Delphi
      ~client_mode_tag
      ~rpcs:
        [ ("contracts", test_contracts_007, None);
          ("delegates", test_delegates_007, None);
          ( "votes",
            test_votes_007,
            Some
              (* reduced periods duration to get to testing vote period faster *)
              [ (["blocks_per_cycle"], Some "4");
                (["blocks_per_voting_period"], Some "4") ] );
          ("others", test_others (module RPC.Proto_007), None) ]
      ()
  in
  let modes = [Client; Proxy] in
  List.iter (fun mode -> register_alpha mode ; register_007 mode) modes
