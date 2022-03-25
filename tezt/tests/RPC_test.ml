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
  | Client (Some (Client.Node node), _) | Proxy (Client.Node node) ->
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
   given protocol version.
   - [test_mode_tag] specifies the client mode ([`Client], [`Light] or [`Proxy]).
   - [test_function] is the function to run to perform RPCs after the test
     environment is set up.
   - [parameter_overrides] specifies protocol parameters to change from the default
     sandbox parameters.
   - [node_parameters] specifies additional parameters to pass to the node.
   - [sub_group] is a short identifier for your test, used in the test title and as a tag.
   Additionally, since this uses [Protocol.register_regression_test], this has an
   implicit argument to specify the list of protocols to test. *)
let check_rpc ~test_mode_tag ~test_function ?parameter_overrides
    ?node_parameters sub_group =
  let (client_mode_tag, title_tag) =
    match test_mode_tag with
    | `Client -> (`Client, "client")
    | `Client_with_proxy_server -> (`Client, "proxy_server")
    | `Light -> (`Light, "light")
    | `Proxy -> (`Proxy, "proxy")
  in
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "(mode %s) RPC regression tests: %s" title_tag sub_group)
    ~tags:["rpc"; sub_group]
    ~output_file:(fun p ->
      "rpc" // sf "%s.%s.%s" (Protocol.tag p) title_tag sub_group)
  @@ fun protocol ->
  (* Initialize a node with alpha protocol and data to be used for RPC calls.
     The log of the node is not captured in the regression output. *)
  let* parameter_file =
    match parameter_overrides with
    | None -> Lwt.return_none
    | Some overrides ->
        let* file =
          Protocol.write_parameter_file
            ~base:(Either.right (protocol, None))
            (overrides protocol)
        in
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
    Client.init_with_protocol
      ?parameter_file
      ?nodes_args:node_parameters
      ~protocol
      client_mode_tag
      ()
  in
  let* () = if bake then Client.bake_for client else Lwt.return_unit in
  let* endpoint =
    match test_mode_tag with
    | `Client | `Light | `Proxy -> return Client.(Node node)
    | `Client_with_proxy_server ->
        let* proxy_server = Proxy_server.init node in
        return Client.(Proxy_server proxy_server)
  in
  let* _ = test_function protocol ?endpoint:(Some endpoint) client in
  unit

(* Test the contracts RPC. *)
let test_contracts _protocol ?endpoint client =
  let client_bake_for = make_client_bake_for () in
  let test_implicit_contract contract_id =
    let*! _ = RPC.Contracts.get ?endpoint ~hooks ~contract_id client in
    let*! _ = RPC.Contracts.get_balance ?endpoint ~hooks ~contract_id client in
    let*! _ = RPC.Contracts.get_counter ?endpoint ~hooks ~contract_id client in
    let*! _ =
      RPC.Contracts.get_manager_key ?endpoint ~hooks ~contract_id client
    in
    unit
  in
  let*! _contracts = RPC.Contracts.get_all ?endpoint ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ?endpoint ~hooks client in
  Log.info "Test implicit baker contract" ;
  let bootstrap = List.hd contracts in
  let* () = test_implicit_contract bootstrap in
  let*! _ =
    RPC.Contracts.get_delegate ?endpoint ~hooks ~contract_id:bootstrap client
  in
  Log.info "Test un-allocated implicit contract" ;
  let unallocated_implicit = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unallocated_implicit contracts) ;
  let*! _ =
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
        let*? process =
          rpc
            ?endpoint
            ?hooks:(Some hooks)
            ?chain:None
            ?block:None
            ~contract_id:simple_implicit_key.public_key_hash
            client
        in
        Process.check ~expect_failure:true process)
      [
        RPC.Contracts.get_delegate;
        RPC.Contracts.get_entrypoints;
        RPC.Contracts.get_script;
        RPC.Contracts.get_storage;
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
  let*! _ =
    RPC.Contracts.get_delegate
      ?endpoint
      ~hooks
      ~contract_id:delegated_implicit_key.public_key_hash
      client
  in
  let* () =
    Lwt_list.iter_s
      (fun rpc ->
        let*? process =
          rpc
            ?endpoint
            ?hooks:(Some hooks)
            ?chain:None
            ?block:None
            ~contract_id:delegated_implicit_key.public_key_hash
            client
        in
        Process.check ~expect_failure:true process)
      [
        RPC.Contracts.get_entrypoints;
        RPC.Contracts.get_script;
        RPC.Contracts.get_storage;
      ]
  in
  let test_originated_contract contract_id =
    let*! _ = RPC.Contracts.get ?endpoint ~hooks ~contract_id client in
    let*! _ = RPC.Contracts.get_balance ?endpoint ~hooks ~contract_id client in
    let*? process =
      RPC.Contracts.get_counter ?endpoint ~hooks ~contract_id client
    in
    let* () = Process.check ~expect_failure:true process in
    let*? process =
      RPC.Contracts.get_manager_key ?endpoint ~hooks ~contract_id client
    in
    let* () = Process.check ~expect_failure:true process in
    let big_map_key =
      Ezjsonm.value_from_string
        "{ \"key\": { \"int\": \"0\" }, \"type\": { \"prim\": \"int\" } }"
    in
    let*! _ =
      RPC.Contracts.big_map_get
        ?endpoint
        ~hooks
        ~contract_id
        ~data:big_map_key
        client
    in
    let*! _ =
      RPC.Contracts.get_entrypoints ?endpoint ~hooks ~contract_id client
    in
    let*! _ = RPC.Contracts.get_script ?endpoint ~hooks ~contract_id client in
    let*! _ = RPC.Contracts.get_storage ?endpoint ~hooks ~contract_id client in
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
  let*! _ =
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
  let*! _ =
    RPC.Contracts.big_map_get
      ?endpoint
      ~hooks
      ~contract_id:originated_contract_advanced
      ~data:duplicate_big_map_key
      client
  in
  unit

let test_delegates_on_registered_alpha ~contracts ?endpoint client =
  Log.info "Test implicit baker contract" ;

  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ =
    RPC.Delegates.get_full_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_frozen_deposits ?endpoint ~hooks ~pkh:bootstrap client
  in
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
    RPC.Delegates.get_grace_period ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_staking_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_voting_power ?endpoint ~hooks ~pkh:bootstrap client
  in

  unit

let test_delegates_on_registered_hangzhou ~contracts ?endpoint client =
  Log.info "Test implicit baker contract" ;

  let bootstrap = List.hd contracts in
  let* _ = RPC.Delegates.get ?endpoint ~hooks ~pkh:bootstrap client in
  let* _ = RPC.Delegates.get_balance ?endpoint ~hooks ~pkh:bootstrap client in
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
    RPC.Delegates.get_staking_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_contracts ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_delegated_balance ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_deactivated ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_grace_period ?endpoint ~hooks ~pkh:bootstrap client
  in
  let* _ =
    RPC.Delegates.get_voting_power ?endpoint ~hooks ~pkh:bootstrap client
  in

  unit

(* Test the delegates RPC with unregistered baker for Tenderbake protocols. *)
let test_delegates_on_unregistered_alpha ~contracts ?endpoint client =
  Log.info "Test with a PKH that is not a registered baker contract" ;

  let unregistered_baker = "tz1c5BVkpwCiaPHJBzyjg7UHpJEMPTYA1bHG" in
  assert (not @@ List.mem unregistered_baker contracts) ;
  let* _ =
    RPC.Delegates.spawn_get ?endpoint ~hooks ~pkh:unregistered_baker client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_full_balance
      ?endpoint
      ~hooks
      ~pkh:unregistered_baker
      client
    |> Process.check ~expect_failure:true
  in
  let* _ =
    RPC.Delegates.spawn_get_frozen_deposits
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

(* Test the delegates RPC with unregistered baker for Emmy protocols. *)
let test_delegates_on_unregistered_hangzhou ~contracts ?endpoint client =
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

let get_contracts ?endpoint client =
  let*! _ = RPC.Contracts.get_all ?endpoint ~hooks client in
  let* contracts = RPC.Contracts.get_all_delegates ?endpoint ~hooks client in

  Lwt.return contracts

(* Test the delegates RPC for the specified protocol. *)
let test_delegates protocol ?endpoint client =
  let* contracts = get_contracts ?endpoint client in
  let* () =
    match protocol with
    | Protocol.Ithaca | Protocol.Alpha ->
        let* () =
          test_delegates_on_registered_alpha ~contracts ?endpoint client
        in
        test_delegates_on_unregistered_alpha ~contracts ?endpoint client
    | Protocol.Hangzhou ->
        let* () =
          test_delegates_on_registered_hangzhou ~contracts ?endpoint client
        in
        test_delegates_on_unregistered_hangzhou ~contracts ?endpoint client
  in

  unit

(* Test the votes RPC. *)
let test_votes _protocol ?endpoint client =
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
let test_others _protocol ?endpoint client =
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
      Connections 1;
      (* Number of connection allowed for each of our 2 nodes used in the
         mempool tests *)
    ]

let bake_empty_block ?endpoint client =
  let* mempool = Client.empty_mempool_file () in
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

   We create an applied operation and three operations that fail
   to be applied. Each one of these failed operations has a different
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
let test_mempool protocol ?endpoint client =
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
    Operation.inject_transfer
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      client
  in
  let* _ = Client.bake_for ?endpoint client in

  let* _ = bake_empty_block ?endpoint client in

  (* Outdated operation after the second empty baking. *)
  let* () = Client.endorse_for ~protocol ~force:true client in
  let* _ = bake_empty_block ?endpoint client in

  let monitor_path =
    (* To test the monitor_operations rpc we use curl since the client does
       not support streaming RPCs yet. *)
    sf
      "http://localhost:%d/chains/main/mempool/monitor_operations?applied=true&outdated=true&branch_delayed=true&refused=true&branch_refused=true"
      (get_client_port client)
  in
  let proc_monitor =
    (* monitor_operation rpc must be lanched before adding operations in order
       to record them. *)
    Process.spawn ~hooks:mempool_hooks "curl" ["-s"; monitor_path]
  in
  let*! counter =
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.Account.public_key_hash
      client
  in
  let counter = JSON.as_int counter in
  (* Branch_refused op: counter_in_the_past *)
  let* _ =
    Operation.inject_transfer
      ~force:true
      ~source:Constant.bootstrap1
      ~dest:Constant.bootstrap2
      ~counter
      client
  in
  let*! counter =
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap2.Account.public_key_hash
      client
  in
  let counter = JSON.as_int counter in
  (* Branch_delayed op: counter_in_the_future *)
  let* _ =
    Operation.inject_transfer
      ~force:true
      ~source:Constant.bootstrap2
      ~dest:Constant.bootstrap2
      ~counter:(counter + 5)
      client
  in
  (* Refused op: fees_too_low *)
  let* _ =
    Operation.inject_transfer
      ~source:Constant.bootstrap3
      ~dest:Constant.bootstrap2
      ~fee:0
      client
  in
  (* Applied op *)
  let* _ =
    Operation.inject_transfer
      ~source:Constant.bootstrap4
      ~dest:Constant.bootstrap2
      client
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
  let* _ =
    Lwt_list.iter_s
      (fun i ->
        let* mempool =
          Mempool.get_mempool
            ?endpoint
            ~hooks:mempool_hooks
            ~applied:(i = `Applied)
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
              applied = may_get_field `Applied complete_mempool.applied;
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
      [`Applied; `Refused; `Branch_delayed; `Branch_refused; `Outdated]
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
  (* Test RPCs [GET|POST /chains/main/mempool/filter] *)
  let get_filter_variations () =
    let get_filter = RPC.get_mempool_filter ?endpoint ~hooks:mempool_hooks in
    let* _ = get_filter client in
    let* _ = get_filter ~include_default:true client in
    let* _ = get_filter ~include_default:false client in
    unit
  in
  let post_and_get_filter config_str =
    let* _ =
      RPC.post_mempool_filter
        ?endpoint
        ~hooks:mempool_hooks
        ~data:(Ezjsonm.from_string config_str)
        client
    in
    get_filter_variations ()
  in
  let* _ = get_filter_variations () in
  let* _ =
    post_and_get_filter
      {|{ "minimal_fees": "50", "minimal_nanotez_per_gas_unit": [ "201", "5" ], "minimal_nanotez_per_byte": [ "56", "3" ], "allow_script_failure": false }|}
  in
  let* _ =
    post_and_get_filter
      {|{ "minimal_fees": "200", "allow_script_failure": true }|}
  in
  let* _ = post_and_get_filter "{}" in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1765

     Once openapi-diff is integrated, this test could be removed. *)
  (* Call describe on mempool RPCs and record the output. *)
  let describe_path =
    sf
      "http://localhost:%d/describe/chains/main/mempool?recurse=yes"
      (get_client_port client)
  in
  let describe_path_filtered =
    "http://localhost:[PORT]/describe/chains/main/mempool?recurse=yes"
  in
  let message = Log.quote_shell_command "curl" ["-s"; describe_path_filtered] in
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

let binary_regression_test () =
  let node = Node.create ~rpc_host:"127.0.0.1" [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
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
  let* client = Client.init ~endpoint ~media_type:Json () in
  Client.spawn_rpc GET ["chains"; "main"; "blocks"] client
  |> Process.check_error ~exit_code:1

let test_no_service_at_valid_prefix address () =
  let node = Node.create ~rpc_host:address [] in
  let endpoint = Client.(Node node) in
  let* () = Node.config_init node [] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
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
    ~output_file:"rpc/binary_rpc"
    binary_regression_test ;
  let register protocols test_mode_tag =
    let check_rpc ?parameter_overrides ?node_parameters ~test_function sub_group
        =
      check_rpc
        ~test_mode_tag
        ?parameter_overrides
        ?node_parameters
        ~test_function
        sub_group
        protocols
    in
    let consensus_threshold protocol =
      if Protocol.number protocol >= 012 then
        [(["consensus_threshold"], Some "0")]
      else []
    in
    check_rpc
      "contracts"
      ~test_function:test_contracts
      ~parameter_overrides:consensus_threshold ;
    check_rpc
      "delegates"
      ~test_function:test_delegates
      ~parameter_overrides:consensus_threshold ;
    check_rpc
      "votes"
      ~test_function:test_votes
      ~parameter_overrides:(fun protocol ->
        (* reduced periods duration to get to testing vote period faster *)
        let cycles_per_voting_period =
          if Protocol.number protocol >= 013 then
            (["cycles_per_voting_period"], Some "1")
          else (["blocks_per_voting_period"], Some "4")
        in
        [(["blocks_per_cycle"], Some "4"); cycles_per_voting_period]
        @ consensus_threshold protocol) ;
    check_rpc
      "others"
      ~test_function:test_others
      ~parameter_overrides:consensus_threshold ;
    match test_mode_tag with
    | `Client_with_proxy_server | `Light -> ()
    | _ ->
        check_rpc
          "mempool"
          ~test_function:test_mempool
          ~node_parameters:mempool_node_flags
  in
  List.iter
    (register protocols)
    [`Client; `Light; `Proxy; `Client_with_proxy_server] ;

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
