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

(** This test file is factored out on the Client mode. In Light mode we need some special care regarding waiting for all the nodes to be in sync, otherwise the commands fail as it looks like some nodes are rogue. *)
let node_wait_for_level level client =
  match Client.get_mode client with
  | Light (_min_agreement, nodes) ->
      Lwt_list.iter_p
        (function
          | Client.Node node ->
              Lwt.map (Fun.const ()) @@ Node.wait_for_level node level
          | _ -> Lwt.return_unit)
        nodes
  | Mockup -> assert false (* This file does not use Mockup mode *)
  | Client (Some (Client.Node node)) | Proxy (Client.Node node) ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit
  | Client _ | Proxy _ -> Lwt.return_unit

(** [make_client_bake_for ()] returns a function similar to {!Client.bake_for} which additionally keeps track of the current level, and in Light mode, waits for all nodes to be in sync after baking. *)
let make_client_bake_for () =
  let current_level = ref 1 in
  fun client ->
    let* () = Client.bake_for client in
    current_level := !current_level + 1 ;
    node_wait_for_level !current_level client

(* A helper to register a RPC test environment with a node and a client for the
   given protocol version. *)
let check_rpc ~group_name ~protocols ~test_mode_tag
    ~(rpcs :
       (string
       * (?endpoint:Client.endpoint -> Client.t -> unit Lwt.t)
       * Protocol.parameter_overrides option
       * Node.argument list option)
       list) () =
  let (client_mode_tag, title_tag) =
    match test_mode_tag with
    | `Client -> (`Client, "client")
    | `Client_with_proxy_server -> (`Client, "proxy_server")
    | `Light -> (`Light, "light")
    | `Proxy -> (`Proxy, "proxy")
  in
  List.iter
    (fun (sub_group, rpc, parameter_overrides, node_parameters) ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:
          (sf
             "%s (mode %s) RPC regression tests: %s"
             group_name
             title_tag
             sub_group)
        ~tags:["rpc"; group_name; sub_group]
        ~output_file:("rpc" // sf "%s.%s.%s" group_name title_tag sub_group)
        ~protocols
      @@ fun protocol ->
      (* Initialize a node with alpha protocol and data to be used for RPC calls.
         The log of the node is not captured in the regression output. *)
      let* parameter_file =
        match parameter_overrides with
        | None -> Lwt.return_none
        | Some overrides ->
            let* file = Protocol.write_parameter_file ~protocol overrides in
            Lwt.return_some file
      in
      let bake =
        match test_mode_tag with
        | `Client_with_proxy_server ->
            (* Because the proxy server doesn't support genesis. *)
            true
        | `Client | `Light | `Proxy -> false
      in
      let* (node, client) =
        Client.init_activate_bake
          ?parameter_file
          ?nodes_args:node_parameters
          ~bake
          ~protocol
          client_mode_tag
          ()
      in
      let* endpoint =
        match test_mode_tag with
        | `Client | `Light | `Proxy -> return Client.(Node node)
        | `Client_with_proxy_server ->
            let* proxy_server = Proxy_server.init node in
            return Client.(Proxy_server proxy_server)
      in
      let* _ = rpc ?endpoint:(Some endpoint) client in
      unit)
    rpcs

(* Test the contracts RPC. *)
let test_contracts ?endpoint client =
  let client_bake_for = make_client_bake_for () in
  let test_implicit_contract contract_id =
    let* _ = RPC.Contracts.get ?endpoint ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_balance ?endpoint ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_counter ?endpoint ~hooks ~contract_id client in
    let* _ =
      RPC.Contracts.get_manager_key ?endpoint ~hooks ~contract_id client
    in
    unit
  in
  let* _contracts = RPC.Contracts.get_all ?endpoint ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ?endpoint ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* () = test_implicit_contract bootstrap in
  let* _ =
    RPC.Contracts.get_delegate ?endpoint ~hooks ~contract_id:bootstrap client
  in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let* _ =
    RPC.Contracts.get ?endpoint ~hooks ~contract_id:unallocated_implicit client
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
  let* () = client_bake_for client in
  let* () = test_implicit_contract simple_implicit_key.public_key_hash in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        rpc
          ?endpoint
          ?hooks:(Some hooks)
          ?chain:None
          ?block:None
          ~contract_id:simple_implicit_key.public_key_hash
          client
        |> Process.check ~expect_failure:true)
      [
        RPC.Contracts.spawn_get_delegate;
        RPC.Contracts.spawn_get_entrypoints;
        RPC.Contracts.spawn_get_script;
        RPC.Contracts.spawn_get_storage;
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
  let* () = client_bake_for client in
  let* () =
    Client.set_delegate ~src:delegated_implicit ~delegate:bootstrap1 client
  in
  let* () = client_bake_for client in
  let* () = test_implicit_contract delegated_implicit_key.public_key_hash in
  let* _ =
    RPC.Contracts.get_delegate
      ?endpoint
      ~hooks
      ~contract_id:delegated_implicit_key.public_key_hash
      client
  in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        rpc
          ?endpoint
          ?hooks:(Some hooks)
          ?chain:None
          ?block:None
          ~contract_id:delegated_implicit_key.public_key_hash
          client
        |> Process.check ~expect_failure:true)
      [
        RPC.Contracts.spawn_get_entrypoints;
        RPC.Contracts.spawn_get_script;
        RPC.Contracts.spawn_get_storage;
      ]
  in
  let test_originated_contract contract_id =
    let* _ = RPC.Contracts.get ?endpoint ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_balance ?endpoint ~hooks ~contract_id client in
    let* () =
      RPC.Contracts.spawn_get_counter ?endpoint ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let* () =
      RPC.Contracts.spawn_get_manager_key ?endpoint ~hooks ~contract_id client
      |> Process.check ~expect_failure:true
    in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let* _ =
      RPC.Contracts.big_map_get
        ?endpoint
        ~hooks
        ~contract_id
        ~data:big_map_key
        client
    in
    let* _ =
      RPC.Contracts.get_entrypoints ?endpoint ~hooks ~contract_id client
    in
    let* _ = RPC.Contracts.get_script ?endpoint ~hooks ~contract_id client in
    let* _ = RPC.Contracts.get_storage ?endpoint ~hooks ~contract_id client in
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
  let* () = client_bake_for client in
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
  let* () = client_bake_for client in
  let* () = test_originated_contract originated_contract_advanced in
  let unique_big_map_key =
    Ezjsonm.value_from_string
      "{ \"key\": { \"string\": \"test\" }, \"type\": { \"prim\": \"string\" } \
       }"
  in
  let* _ =
    RPC.Contracts.big_map_get
      ?endpoint
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
      ?endpoint
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:duplicate_big_map_key
      client
  in
  unit

(* Test the delegates RPC for protocol Alpha. *)
let test_delegates_alpha ?endpoint client =
  let* _contracts = RPC.Contracts.get_all ?endpoint ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ?endpoint ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_balance ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_deactivated ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle
      ?endpoint
      ~hooks
      ~pkh:bootstrap
      client
  in
  let* _ =
    RPC.Delegates.get_grace_period ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_staking_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_voting_power ?endpoint ~hooks ~pkh:bootstrap client
  in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Delegates.spawn_get ?endpoint ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_deactivated
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_delegated_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_delegated_contracts
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_frozen_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_frozen_balance_by_cycle
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_grace_period
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_staking_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_voting_power
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  unit

(* Test the delegates RPC for the current Mainnet protocol. *)
let test_delegates_current_mainnet ?endpoint client =
  let* _contracts = RPC.Contracts.get_all ?endpoint ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ?endpoint ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_balance ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_deactivated ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle
      ?endpoint
      ~hooks
      ~pkh:bootstrap
      client
  in
  let* _ =
    RPC.Delegates.get_grace_period ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_staking_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_voting_power ?endpoint ~hooks ~pkh:bootstrap client
  in
  Log.info "Test with a PKH that is not a registered baker contract" ;
  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Delegates.spawn_get ?endpoint ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_deactivated
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.spawn_get_delegated_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.get_frozen_balance_by_cycle
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.spawn_get_grace_period
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.get_staking_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  let* _ =
    RPC.Delegates.get_voting_power
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
  in
  unit

(* Test the votes RPC for protocol Alpha. *)
let test_votes_alpha ?endpoint client =
  let client_bake_for = make_client_bake_for () in
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = client_bake_for client in
  (* RPC calls *)
  let* _ = RPC.Votes.get_ballot_list ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_ballots ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_listings ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_proposals ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_successor_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ?endpoint ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = client_bake_for client in
  let* () = client_bake_for client in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = client_bake_for client in
  (* RPC calls again *)
  let* _ = RPC.Votes.get_ballot_list ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_ballots ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_listings ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_proposals ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_successor_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ?endpoint ~hooks client in
  (* RPC calls again *)
  unit

(* Test the votes RPC for the current Mainnet protocol. *)
let test_votes_current_mainnet ?endpoint client =
  let client_bake_for = make_client_bake_for () in
  (* initialize data *)
  let proto_hash = "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp" in
  let* () = Client.submit_proposals ~proto_hash client in
  let* () = client_bake_for client in
  (* RPC calls *)
  let* _ = RPC.Votes.get_ballot_list ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_ballots ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_listings ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_proposals ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_successor_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ?endpoint ~hooks client in
  (* bake to testing vote period and submit some ballots *)
  let* () = client_bake_for client in
  let* () = client_bake_for client in
  let* () = Client.submit_ballot ~key:"bootstrap1" ~proto_hash Yay client in
  let* () = Client.submit_ballot ~key:"bootstrap2" ~proto_hash Nay client in
  let* () = Client.submit_ballot ~key:"bootstrap3" ~proto_hash Pass client in
  let* () = client_bake_for client in
  (* RPC calls again *)
  let* _ = RPC.Votes.get_ballot_list ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_ballots ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_proposal ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_current_quorum ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_listings ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_proposals ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_successor_period ?endpoint ~hooks client in
  let* _ = RPC.Votes.get_total_voting_power ?endpoint ~hooks client in
  unit

(* Test the various other RPCs. *)
let test_others ?endpoint client =
  let* _ = RPC.get_constants ?endpoint ~hooks client in
  let* _ = RPC.get_baking_rights ?endpoint ~hooks client in
  let* _ = RPC.get_current_level ?endpoint ~hooks client in
  let* _ = RPC.get_endorsing_rights ?endpoint ~hooks client in
  let* _ = RPC.get_levels_in_current_cycle ?endpoint ~hooks client in
  unit

let mempool_hooks =
  let replace_variable string =
    let replacements =
      [
        ("edsig\\w{94}", "[SIGNATURE]");
        ("sig\\w{93}", "[SIGNATURE]");
        ("o\\w{50}", "[OPERATION_HASH]");
        ("B\\w{50}", "[BRANCH_HASH]");
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

(* Test the mempool RPCs *)
(* In this test, we create an applied operation and three operations that fails
   to be applied. Each one of this failed operation has a different
   classification to test every possibilities of the monitor_operations and
   pending_operations RPCs. The errors classification are the following :
   - Branch_refused (Branch error classification in protocol)
   - Branch_delayed (Temporary)
   - Refused (Permanent)

   The main goal is to have a record of the encoding of the different
   operations returned by the RPC calls. This allows
   us to detect undesired changes. *)
let test_mempool ?endpoint client =
  let open Lwt in
  let* node = Node.init [Synchronisation_threshold 0; Connections 1] in
  let* () = Client.Admin.trust_address ?endpoint client ~peer:node in
  let* () = Client.Admin.connect_address ?endpoint client ~peer:node in
  let level = 1 in
  let* _ = Node.wait_for_level node level in
  let* node1_identity = Node.wait_for_identity node in
  let* () = Client.Admin.kick_peer ~peer:node1_identity client in
  let* _ = Mempool.bake_empty_mempool client in
  let monitor_path =
    (* To test the monitor_operations rpc we use curl since the client does
       not support streaming RPCs yet. *)
    sf
      "http://localhost:%d/chains/main/mempool/monitor_operations?applied=true&branch_delayed=true&refused=true&branch_refused=true"
      (get_client_port client)
  in
  let proc_monitor =
    (* monitor_operation rpc must be lanched before adding operations in order
       to record them. *)
    Process.spawn ~hooks:mempool_hooks "curl" ["-s"; monitor_path]
  in
  (* Refused operation after the reclassification following the flush. *)
  let* branch = RPC.get_branch client >|= JSON.as_string in
  let* _ =
    Mempool.forge_and_inject_operation
      ~branch
      ~fee:10
      ~gas_limit:1040
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter:1
      ~signer:Constant.bootstrap1
      ~client
  in
  (* Applied operation. *)
  let* _ =
    Client.transfer
      ?endpoint
      ~amount:Tez.(of_int 2)
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap3.alias
      client
  in
  (* This operation as the same giver as the previous one and so the same
     counter. It is applied on a different branch since we bake an empty block
     on the other endpoint. Once the nodes will be synced this operation will
     be classified as Branch_refused because its counter is in the past on the
     other endpoint. *)
  let* _ =
    Client.transfer
      ~endpoint:(Client.Node node)
      ~amount:Tez.(of_int 2)
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap3.alias
      client
  in
  (* Branch_delayed operation after the empty baking. *)
  let* _ = Client.endorse_for ?endpoint client in
  (* Reconnect and sync nodes to force branch_refused operation and delay of
     endorsement. *)
  let* () = Client.Admin.connect_address ?endpoint ~peer:node client in
  let flush_waiter = Node_event_level.wait_for_flush node in
  let* _ = Mempool.bake_empty_mempool ~endpoint:(Client.Node node) client in
  let* _ = flush_waiter in
  let* _output_monitor = Process.check_and_read_stdout proc_monitor in
  let* _ =
    RPC.get_mempool_pending_operations ?endpoint ~hooks:mempool_hooks client
  in
  (* We spawn a second monitor_operation RPC that monitor operations
     reclassified with errors. *)
  let proc_monitor =
    Process.spawn ~hooks:mempool_hooks "curl" ["-s"; monitor_path]
  in
  let* _ = Mempool.bake_empty_mempool ~endpoint:(Client.Node node) client in
  let* _output_monitor = Process.check_and_read_stdout proc_monitor in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1765

     Once openapi-diff is integrated, this test could be removed. *)
  (* Call describe on mempool RPCs and record the output. *)
  let describe_path =
    sf
      "http://localhost:%d/describe/chains/main/mempool?recurse=yes"
      (get_client_port client)
  in
  let message = Log.quote_shell_command "curl" ["-s"; describe_path] in
  Regression.capture ("\n" ^ message) ;
  let proc_describe = Process.spawn "curl" ["-s"; describe_path] in
  let* output = Process.check_and_read_stdout proc_describe in
  (* To ease the reading of the output (which is expected to be a JSON
     value), we prettify the output. It makes easier to interpret
     regressions. *)
  let prettified_output =
    output
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1766

       End of line characters are trimmed because the /describe/ RPC may
       introduce them in string litteral which seems forbidden by the JSON
       standard. *)
    |> replace_string ~all:true (rex "\n") ~by:""
    |> JSON.parse ~origin:message |> JSON.encode
  in
  Regression.capture prettified_output ;
  unit

let start_with_acl address acl =
  let node = Node.create ~rpc_host:address [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  Node.Config_file.update node (JSON.update "rpc" (JSON.put ("acl", acl))) ;
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  Client.init ~endpoint ()

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
  let* _success_resp = Client.rpc GET ["network"; "connections"] client in
  unit

let test_no_service_at_valid_prefix address () =
  let node = Node.create ~rpc_host:address [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  let* client = Client.init ~endpoint () in
  let* () =
    Client.spawn_rpc GET ["chains"; "main"] client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex
              "Fatal error:\n\
              \  No service found at this URL (but this is a valid prefix)*\n\
               *")
  in
  unit

let register () =
  let register_alpha test_mode_tag =
    check_rpc
      ~group_name:"alpha"
      ~protocols:[Protocol.Alpha]
      ~test_mode_tag
      ~rpcs:
        ([
           ("contracts", test_contracts, None, None);
           ("delegates", test_delegates_alpha, None, None);
           ( "votes",
             test_votes_alpha,
             Some
               (* reduced periods duration to get to testing vote period faster *)
               [
                 (["blocks_per_cycle"], Some "4");
                 (["blocks_per_voting_period"], Some "4");
               ],
             None );
           ("others", test_others, None, None);
         ]
        @
        match test_mode_tag with
        | `Client_with_proxy_server | `Light -> []
        | _ ->
            [
              ( "mempool",
                test_mempool,
                None,
                Some [Node.Synchronisation_threshold 0; Node.Connections 1] );
            ])
      ()
  in
  let register_current_mainnet test_mode_tag =
    check_rpc
      ~group_name:"current"
      ~protocols:[Protocol.Granada]
      ~test_mode_tag
      ~rpcs:
        ([
           ("contracts", test_contracts, None, None);
           ("delegates", test_delegates_current_mainnet, None, None);
           ( "votes",
             test_votes_current_mainnet,
             Some
               (* reduced periods duration to get to testing vote period faster *)
               [
                 (["blocks_per_cycle"], Some "4");
                 (["blocks_per_voting_period"], Some "4");
               ],
             None );
           ("others", test_others, None, None);
         ]
        @
        match test_mode_tag with
        | `Client_with_proxy_server | `Light -> []
        | _ ->
            [
              ( "mempool",
                test_mempool,
                None,
                Some [Node.Synchronisation_threshold 0; Node.Connections 1] );
            ])
      ()
  in
  let modes = [`Client; `Light; `Proxy; `Client_with_proxy_server] in
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
        (test_blacklist addr) ;
      Test.register
        ~__FILE__
        ~title:(mk_title "no_service_at_valid_prefix" addr)
        ~tags:["rpc"]
        (test_no_service_at_valid_prefix addr))
    addresses
