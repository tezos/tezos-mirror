(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_test.ml
   Subject:      Run the baker while performing a lot of transfers
*)

let team = Tag.layer1

let hooks = Tezos_regression.hooks

let log_step counter msg =
  let color = Log.Color.(bold ++ FG.blue) in
  let prefix = "step" ^ string_of_int counter in
  Log.info ~color ~prefix msg

(* [fetch_baking_rights client lvl] calls the baking_rights RPC and returns the
   result as an association list. The list associates each round number with the
   public key hash of the delegate holding baking rights for that round. *)
let fetch_baking_rights client level =
  let* baking_rights_json =
    Client.RPC.call client @@ RPC.get_chain_block_helper_baking_rights ~level ()
  in
  return
  @@ List.map
       JSON.(
         fun json ->
           let round = json |-> "round" |> as_int in
           let delegate_pkh = json |-> "delegate" |> as_string in
           (round, delegate_pkh))
       JSON.(as_list baking_rights_json)

let check_node_version_check_bypass_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker node version check bypass test"
    ~tags:[team; "node"; "baker"]
    ~supports:Protocol.(From_protocol 021)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let baker =
    Agnostic_baker.create ~node_version_check_bypass:true node client
  in
  let check_bypassed_event_promise =
    Agnostic_baker.wait_for baker "node_version_check_bypass.v0" (fun _ ->
        Some ())
  in
  let* () = Agnostic_baker.run baker in
  let* () = check_bypassed_event_promise in
  unit

let check_node_version_allowed_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker node version allowed test"
    ~tags:[team; "node"; "baker"]
    ~supports:Protocol.(From_protocol 022)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let* _baker =
    Agnostic_baker.init
      ~node_version_allowed:"octez-v7894.789:1a991a03"
      node
      client
  in
  unit

let check_node_version_no_commit_allowed_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker node version no commit allowed test"
    ~tags:[team; "node"; "baker"]
    ~supports:Protocol.(From_protocol 022)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let* _baker =
    Agnostic_baker.init ~node_version_allowed:"octez-v7894.789" node client
  in
  unit

let baker_reward_test =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Baker rewards"
    ~tags:[team; "baker"; "rewards"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    (fun protocol ->
      let* parameter_file =
        Protocol.write_parameter_file
          ~base:(Either.Right (protocol, Some Constants_mainnet))
          []
      in
      let* node, client =
        Client.init_with_protocol
          `Client
          ~protocol
          ~timestamp:Now
          ~parameter_file
          ()
      in
      let level_2_promise = Node.wait_for_level node 2 in
      let* baker = Agnostic_baker.init node client in
      Log.info "Wait for new head." ;
      Agnostic_baker.log_events baker ;
      let* _ = level_2_promise in
      let* _ =
        Client.RPC.call ~hooks client @@ RPC.get_chain_block_metadata ()
      in
      unit)

let baker_test protocol ~keys =
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      []
  in
  let* node, client =
    Client.init_with_protocol
      ~keys:(Constant.activator :: keys)
      `Client
      ~protocol
      ~timestamp:Now
      ~parameter_file
      ()
  in
  let level_2_promise = Node.wait_for_level node 2 in
  let level_3_promise = Node.wait_for_level node 3 in
  let* baker = Agnostic_baker.init node client in
  Log.info "Wait for new head." ;
  Agnostic_baker.log_events baker ;
  let* _ = level_2_promise in
  Log.info "New head arrive level 2" ;
  let* _ = level_3_promise in
  Log.info "New head arrive level 3" ;
  Lwt.return client

let baker_simple_test =
  Protocol.register_test
    ~__FILE__
    ~title:"baker test"
    ~tags:[team; "node"; "baker"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* _ =
    baker_test protocol ~keys:(Account.Bootstrap.keys |> Array.to_list)
  in
  unit

let baker_stresstest =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest"
    ~tags:[team; "node"; "baker"; "stresstest"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Agnostic_baker.init node client in
  let* _ = Node.wait_for_level node 3 in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~transfers:100 client in
  Lwt.return_unit

(* Force the baker to apply operations after validating them *)
let baker_stresstest_apply =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest with forced application"
    ~tags:[team; "node"; "baker"; "stresstest"; "apply"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Agnostic_baker.init ~force_apply_from_round:0 node client in
  let* _ = Node.wait_for_level node 3 in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~transfers:100 client in
  unit

let no_bls_baker_test =
  Protocol.register_test
    ~__FILE__
    ~title:"No BLS baker test"
    ~tags:[team; "node"; "baker"; "bls"]
    ~supports:Protocol.(Until_protocol (number Quebec))
  @@ fun protocol ->
  let* client0 = Client.init_mockup ~protocol () in
  Log.info "Generate BLS keys for client" ;
  let* keys =
    Lwt_list.map_s
      (fun i ->
        Client.gen_and_show_keys
          ~alias:(sf "bootstrap_bls_%d" i)
          ~sig_alg:"bls"
          client0)
      (Base.range 1 5)
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      []
  in
  let* _node, client =
    Client.init_with_node ~keys:(Constant.activator :: keys) `Client ()
  in
  let activate_process =
    Client.spawn_activate_protocol
      ~protocol
      ~timestamp:Now
      ~parameter_file
      client
  in
  let msg =
    rex "The delegate tz4.*\\w is forbidden as it is a BLS public key hash"
  in
  Process.check_error activate_process ~exit_code:1 ~msg

let bls_baker_test =
  Protocol.register_test
    ~__FILE__
    ~title:"BLS baker test"
    ~tags:[team; "node"; "baker"; "bls"]
    ~supports:Protocol.(From_protocol (number Quebec + 1))
  @@ fun protocol ->
  let* client0 = Client.init_mockup ~protocol () in
  Log.info "Generate BLS keys for client" ;
  let* keys =
    Lwt_list.map_s
      (fun i ->
        Client.gen_and_show_keys
          ~alias:(sf "bootstrap_bls_%d" i)
          ~sig_alg:"bls"
          client0)
      (Base.range 1 5)
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      [(["allow_tz4_delegate_enable"], `Bool true)]
  in
  let* _node, client =
    Client.init_with_node ~keys:(Constant.activator :: keys) `Client ()
  in
  let activate_process =
    Client.spawn_activate_protocol
      ~protocol
      ~timestamp:Now
      ~parameter_file
      client
  in
  Process.check activate_process

let baker_remote_test =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker in RPC-only mode"
    ~tags:[team; "baker"; "remote"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Agnostic_baker.init ~remote_mode:true node client in
  let* _ = Node.wait_for_level node 3 in
  unit

let baker_check_consensus_branch =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker check branch in consensus operations"
    ~tags:[team; "baker"; "grandparent"; "parent"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Init client and node with protocol %s" (Protocol.name protocol) ;
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in

  let target_level = 5 in
  Log.info "Start a baker and bake until level %d" target_level ;
  let* baker = Agnostic_baker.init node client in
  let* _ = Node.wait_for_level node target_level in
  let* () = Agnostic_baker.kill baker in

  Log.info "Retrieve mempool" ;
  let* mempool =
    Client.RPC.call client
    @@ RPC.get_chain_mempool_pending_operations ~outdated:false ()
  in

  Log.info "Check that consensus operations are branched on grandparent block" ;
  let ops = JSON.(mempool |-> "validated" |> as_list) in
  assert (not ([] = ops)) ;
  Tezos_base__TzPervasives.List.iter_s
    (fun op ->
      let branch = JSON.(op |-> "branch" |> as_string) in
      let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
      let level = JSON.(content |-> "level" |> as_int) in

      let* target_branch =
        Client.RPC.call client
        @@ RPC.get_chain_block_header ~block:(string_of_int (level - 2)) ()
      in
      let target_branch = JSON.(target_branch |-> "hash" |> as_string) in

      Tezt.Check.(
        (target_branch = branch)
          string
          ~error_msg:"Consensus operation should be branch on block %L, got %R") ;
      unit)
    ops

(** Test that blocks are applied only if round >= [force_apply_from_round]. *)
let force_apply_from_round =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker check force apply from round"
    ~tags:[team; "baker"; "force_apply_from_round"]
    ~supports:Protocol.(From_protocol 021)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  log_step 1 "initialize a node and a client with protocol" ;
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in

  log_step 2 "wait for level 1" ;
  let* _ = Node.wait_for_level node 1 in

  log_step 3 "find suitable delegate and rounds" ;
  let force_apply_from_round = 3 in
  let* baking_rights_at_level_3 = fetch_baking_rights client 2 in
  (* pick the delegate that has baking rights at level 3 round 1 *)
  let delegate = List.assoc 1 baking_rights_at_level_3 in
  (* find the smallest level (>= 4) where [delegate] first baking round is
     greater or equal than [force_apply_from_round]. *)
  let rec loop level =
    let* baking_rights = fetch_baking_rights client level in
    match
      List.find_opt (fun (_, delegate') -> delegate = delegate') baking_rights
    with
    | Some (round, _) when round >= force_apply_from_round ->
        return (level, round)
    | _ -> loop (succ level)
  in
  let* level, round = loop 4 in
  Log.info
    "delegate %s has baking rights for level 3 round 1 and level %d round %d"
    delegate
    level
    round ;

  log_step
    5
    "spawn a baker with delegate %s and force_apply_from_round %d"
    delegate
    force_apply_from_round ;
  let* baker =
    Agnostic_baker.init
      ~force_apply_from_round
      ~delegates:[delegate]
      node
      client
  in

  (* fail if a block is applied at a round < [force_apply_from_round] *)
  Agnostic_baker.on_event baker (fun Agnostic_baker.{name; value; _} ->
      if name = "forging_block.v0" then
        let round = JSON.(value |-> "round" |> as_int) in
        let level = JSON.(value |-> "level" |> as_int) in
        let force_apply = JSON.(value |-> "force_apply" |> as_bool) in
        if force_apply && round < force_apply_from_round then
          Test.fail
            "block at level %d round %d shouldn't have been applied"
            level
            round) ;

  (* a promise that resolves on event forging_block when
     (force_apply = true && round = [round] && level = [level]) *)
  let forced_application_promise =
    Agnostic_baker.wait_for baker "forging_block.v0" (fun json ->
        let round' = JSON.(json |-> "round" |> as_int) in
        let level' = JSON.(json |-> "level" |> as_int) in
        let force_apply = JSON.(json |-> "force_apply" |> as_bool) in
        if force_apply && level = level' && round = round' then Some ()
        else None)
  in

  let* _ = Node.wait_for_level node level in
  log_step
    6
    "level %d round %d has been baked, check that block has been forged with \
     force_apply = true"
    level
    round ;

  (* promise should be fulfilled *)
  if Lwt.state forced_application_promise <> Lwt.Return () then
    Tezt.Test.fail
      "level %d has been baked at round >= %d and block wasn't forged with \
       force_apply = true"
      level
      round ;
  unit

(* [check_aggregate ~expected_committee aggregate_json] fails if the set of
   committee members in [aggregate_json] differs from [expected_committee]. *)
let check_aggregate ~expected_committee aggregate_json =
  let expected_committee = List.sort String.compare expected_committee in
  let contents = JSON.(aggregate_json |-> "contents" |> as_list |> List.hd) in
  let committee =
    JSON.(contents |-> "metadata" |-> "committee" |> as_list)
    |> List.map JSON.(fun json -> json |-> "delegate" |> as_string)
    |> List.sort String.compare
  in
  if not (List.equal String.equal committee expected_committee) then
    let pp = Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) in
    Test.fail
      "@[<v 0>Wrong commitee@,@[<v 2>expected:@,%a@]@,@[<v 2>found:@,%a@]@]"
      pp
      expected_committee
      pp
      committee

(* [find_aggregate_receipt operations] returns the sole attestations aggregate
   found in [operations]. Fails the test if no such operation exists or if
   more than one if found. *)
let find_aggregate_receipt consensus_operations =
  let aggregates =
    List.filter
      (fun json ->
        let kind =
          JSON.(
            json |-> "contents" |> as_list |> List.hd |-> "kind" |> as_string)
        in
        kind = "attestations_aggregate")
      consensus_operations
  in
  match aggregates with
  | [] -> Test.fail "The block doesn't contain any attestations aggregate"
  | _ :: _ :: _ ->
      Test.fail
        "Multiple attestation aggregates found, but only one is expected"
  | [json] -> json

(* [check_for_non_aggregated_eligible_attestations operations] fails the test if
   [operations] contains a non-aggregated attestation that is eligible for
   aggregation. *)
let check_for_non_aggregated_eligible_attestations consensus_operations =
  let is_tz4 = String.starts_with ~prefix:"tz4" in
  let has_non_aggregated_eligible_attestations =
    List.exists
      (fun json ->
        let kind =
          JSON.(
            json |-> "contents" |> as_list |> List.hd |-> "kind" |> as_string)
        in
        if kind = "attestation" then
          let consensus_key =
            JSON.(
              json |-> "contents" |> as_list |> List.hd |-> "metadata"
              |-> "consensus_key" |> as_string)
          in
          is_tz4 consensus_key
        else false)
      consensus_operations
  in
  if has_non_aggregated_eligible_attestations then
    Test.fail "The block contains a non-aggregated eligible attestation"

(* Test that the baker aggregates eligible attestations.*)
let simple_attestations_aggregation =
  Protocol.register_test
    ~__FILE__
    ~title:"Simple attestations aggregation"
    ~tags:[team; "baker"; "attestation"; "aggregation"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  log_step 1 "Initialize a node and a client with protocol" ;
  let consensus_rights_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [
        (["allow_tz4_delegate_enable"], `Bool true);
        (["aggregate_attestation"], `Bool true);
        (* Diminish some constants to activate consensus keys faster *)
        (["blocks_per_cycle"], `Int 2);
        (["nonce_revelation_threshold"], `Int 1);
        (["consensus_rights_delay"], `Int consensus_rights_delay);
        (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
        (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
      ]
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ()
      ~timestamp:Now
  in
  log_step 2 "Wait for level 1" ;
  let* _ = Node.wait_for_level node 1 in
  log_step 3 "Generate bls keys" ;
  let* b1 = Client.gen_keys ~sig_alg:"bls" client in
  let* b2 = Client.gen_keys ~sig_alg:"bls" client in
  let* b3 = Client.gen_keys ~sig_alg:"bls" client in
  log_step 4 "Use them as consensus keys for some delegates" ;
  let bootstrap1, bootstrap2, bootstrap3 =
    Constant.(bootstrap1, bootstrap2, bootstrap3)
  in
  let* () = Client.update_consensus_key ~src:bootstrap1.alias ~pk:b1 client in
  let* () = Client.update_consensus_key ~src:bootstrap2.alias ~pk:b2 client in
  let* () = Client.update_consensus_key ~src:bootstrap3.alias ~pk:b3 client in
  let delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun (account : Account.key) -> account.Account.alias)
    |> List.append [b1; b2; b3]
  in
  (* Expected committee that should be found in attestations aggregate *)
  let expected_committee =
    [
      bootstrap1.public_key_hash;
      bootstrap2.public_key_hash;
      bootstrap3.public_key_hash;
    ]
  in
  (* Testing the "bake for" command *)
  log_step 4 "Bake for until level 8" ;
  (* Waiting for level 8 ensures that the bls consensus keys are activated *)
  let* () = Client.bake_for_and_wait ~count:7 ~keys:delegates client in
  log_step 5 "Fetch latest block consensus operations" ;
  let* consensus_operations =
    let* json =
      Client.RPC.call client
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
    in
    return JSON.(as_list json)
  in
  log_step
    6
    "Check that it doesn't contain any non-aggregated eligible attestation" ;
  let () =
    check_for_non_aggregated_eligible_attestations consensus_operations
  in
  log_step 7 "Check that it contains an aggregate" ;
  let aggregate_json = find_aggregate_receipt consensus_operations in
  log_step 8 "Check that the aggregate contains the expected attestations" ;
  let () = check_aggregate ~expected_committee aggregate_json in
  (* Testing the "bake for" command with minimal timestamp *)
  log_step 9 "Bake for with minimal timestamp until level 10" ;
  let* () =
    Client.bake_for_and_wait
      ~minimal_timestamp:true
      ~count:2
      ~keys:delegates
      client
  in
  log_step 10 "Fetch latest block consensus operations" ;
  let* consensus_operations =
    let* json =
      Client.RPC.call client
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
    in
    return JSON.(as_list json)
  in
  log_step
    11
    "Check that it doesn't contain any non-aggregated eligible attestation" ;
  let () =
    check_for_non_aggregated_eligible_attestations consensus_operations
  in
  log_step 12 "Check that it contains an aggregate" ;
  let aggregate_json = find_aggregate_receipt consensus_operations in
  log_step 13 "Check that the aggregate contains the expected attestations" ;
  let () = check_aggregate ~expected_committee aggregate_json in
  log_step 14 "Start a baker and bake until level 12" ;
  (* Testing the baker automaton *)
  let* _baker = Agnostic_baker.init ~delegates node client in
  let* _ = Node.wait_for_level node 12 in
  log_step 15 "Fetch latest block consensus operations" ;
  let* consensus_operations =
    let* json =
      Client.RPC.call client
      @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
    in
    return JSON.(as_list json)
  in
  log_step
    16
    "Check that it doesn't contain any non-aggregated eligible attestation" ;
  let () =
    check_for_non_aggregated_eligible_attestations consensus_operations
  in
  log_step 17 "Check that it contains an aggregate" ;
  let _ = find_aggregate_receipt consensus_operations in
  log_step 18 "Check that the aggregate contains the expected attestations" ;
  let () = check_aggregate ~expected_committee aggregate_json in
  unit

let register ~protocols =
  check_node_version_check_bypass_test protocols ;
  check_node_version_allowed_test protocols ;
  check_node_version_no_commit_allowed_test protocols ;
  baker_simple_test protocols ;
  baker_reward_test protocols ;
  baker_stresstest protocols ;
  baker_stresstest_apply protocols ;
  bls_baker_test protocols ;
  no_bls_baker_test protocols ;
  baker_remote_test protocols ;
  baker_check_consensus_branch protocols ;
  force_apply_from_round protocols ;
  simple_attestations_aggregation protocols
