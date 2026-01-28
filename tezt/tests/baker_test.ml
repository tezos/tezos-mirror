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

let title_abaab ~abaab title =
  Format.asprintf "%s (abaab: %s)" title (if abaab then "on" else "off")

let abaab_threshold ~abaab ~protocol =
  let pair_to_ratio (num, den) =
    let str = "all_bakers_attest_activation_threshold" in
    [([str; "numerator"], `Int num); ([str; "denominator"], `Int den)]
  in
  if Protocol.number protocol < 024 then []
  else if abaab then pair_to_ratio (0, 1)
  else pair_to_ratio (2, 1)

let bootstrap1, bootstrap2, bootstrap3, bootstrap4, bootstrap5 =
  Constant.(bootstrap1, bootstrap2, bootstrap3, bootstrap4, bootstrap5)

let public_key_hashes =
  List.map (fun (account : Account.key) -> account.public_key_hash)

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

let fetch_round ?block client =
  Client.RPC.call client @@ RPC.get_chain_block_helper_round ?block ()

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
      ~overwrite_bootstrap_accounts:(Some (List.map (fun k -> (k, None)) keys))
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

let bls_baker_test =
  Protocol.register_test
    ~__FILE__
    ~title:"BLS baker test"
    ~tags:[team; "node"; "baker"; "bls"]
    ~supports:Protocol.(From_protocol 22)
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
      ~overwrite_bootstrap_accounts:(Some (List.map (fun k -> (k, None)) keys))
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

let baker_check_consensus_branch ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:(title_abaab ~abaab "Baker check branch in consensus operations")
    ~tags:[team; "baker"; "grandparent"; "parent"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Init client and node with protocol %s" (Protocol.name protocol) ;
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      (abaab_threshold ~abaab ~protocol)
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ()
      ~timestamp:Now
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
let force_apply_from_round ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:(title_abaab ~abaab "Baker check force apply from round")
    ~tags:[team; "baker"; "force_apply_from_round"]
    ~supports:Protocol.(From_protocol 021)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  log_step 1 "initialize a node and a client with protocol" ;
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      (abaab_threshold ~abaab ~protocol)
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ()
      ~timestamp:Now
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
  let expected_committee =
    public_key_hashes expected_committee |> List.sort String.compare
  in
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

let fetch_consensus_operations ?block client =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ?block
         ~validation_pass:0
         ()
  in
  return JSON.(as_list json)

type kind = Attestation | Preattestation

let pp_kind fmt = function
  | Attestation -> Format.fprintf fmt "attestation"
  | Preattestation -> Format.fprintf fmt "preattestation"

(* [check_non_aggregated_consensus_operations kind ~expected found] fails if the
   set of delegates in the consensus operations list [found] differs from the
   set [expected]. See [check_consensus_operations]. *)
let check_non_aggregated_consensus_operations kind ~expected found =
  match (expected, found) with
  | None, _ -> ()
  | Some expected, _ ->
      let sorted_expected =
        public_key_hashes expected |> List.sort String.compare
      in
      let sorted_found =
        found
        |> List.map
             JSON.(
               fun operation ->
                 operation |-> "contents" |> as_list |> List.hd |-> "metadata"
                 |-> "delegate" |> as_string)
        |> List.sort String.compare
      in
      if not (List.equal String.equal sorted_expected sorted_found) then
        let pp = Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) in
        Test.fail
          "@[<v 0>Wrong %a set@,@[<v 2>expected:@,%a@]@,@[<v 2>found:@,%a@]@]"
          pp_kind
          kind
          pp
          sorted_expected
          pp
          sorted_found

let check_aggregated_consensus_operation kind ~expected found =
  match (expected, found) with
  | _, _ :: _ :: _ -> Test.fail "Multiple %as_aggregate found" pp_kind kind
  | None, _ -> ()
  | Some _, [] -> Test.fail "No %as_aggregate found" pp_kind kind
  | Some expected_committee, [aggregate_json] ->
      let expected_committee =
        public_key_hashes expected_committee |> List.sort String.compare
      in
      let contents =
        JSON.(aggregate_json |-> "contents" |> as_list |> List.hd)
      in
      let committee =
        JSON.(contents |-> "metadata" |-> "committee" |> as_list)
        |> List.map JSON.(fun json -> json |-> "delegate" |> as_string)
        |> List.sort String.compare
      in
      if not (List.equal String.equal committee expected_committee) then
        let pp = Format.(pp_print_list ~pp_sep:pp_print_cut pp_print_string) in
        Test.fail
          "@[<v 0>Wrong %a commitee@,\
           @[<v 2>expected:@,\
           %a@]@,\
           @[<v 2>found:@,\
           %a@]@]"
          pp_kind
          kind
          pp
          expected_committee
          pp
          committee

let check_dal ~expected ~attestations_aggregates ~attestations =
  match expected with
  | None -> ()
  | Some expected ->
      let open JSON in
      let aggregated_delegates_with_dal =
        match attestations_aggregates with
        | _ :: _ :: _ -> Test.fail "Multiple attestations_aggregate found"
        | [] -> []
        | [json] ->
            let contents = json |-> "contents" |> as_list |> List.hd in
            (* Filter delegates with DAL *)
            List.fold_left2
              (fun acc committee_json metadata_json ->
                match committee_json |-> "dal_attestation" |> as_string_opt with
                | Some dal_attestation_as_string ->
                    let delegate = metadata_json |-> "delegate" |> as_string in
                    (delegate, dal_attestation_as_string) :: acc
                | None -> acc)
              []
              (contents |-> "committee" |> as_list)
              (contents |-> "metadata" |-> "committee" |> as_list)
      in
      let unaggregated_delegates_with_dal =
        (* Filter attestations with dal *)
        List.filter_map
          (fun json ->
            let contents = json |-> "contents" |> as_list |> List.hd in
            match contents |-> "dal_attestation" |> as_string_opt with
            | Some dal_attestation_as_string ->
                let delegate =
                  contents |-> "metadata" |-> "delegate" |> as_string
                in
                Some (delegate, dal_attestation_as_string)
            | None -> None)
          attestations
      in
      let delegates_with_dal =
        aggregated_delegates_with_dal @ unaggregated_delegates_with_dal
      in
      let cmp (d, _) (d', _) = String.compare d d' in
      let sorted_found = List.sort cmp delegates_with_dal in
      let sorted_expected =
        List.map
          (fun (account, dal) -> (account.Account.public_key_hash, dal))
          expected
        |> List.sort cmp
      in
      let eq (d, dal) (d', dal') = String.equal d d' && String.equal dal dal' in
      if not (List.equal eq sorted_found sorted_expected) then
        let pp_tuple fmt (delegate, dal_attestation) =
          Format.fprintf fmt "(%s, %s)" delegate dal_attestation
        in
        let pp = Format.(pp_print_list ~pp_sep:pp_print_cut pp_tuple) in
        Test.fail
          "@[<v 0>Wrong dal attestation set@,\
           @[<v 2>expected:@,\
           %a@]@,\
           @[<v 2>found:@,\
           %a@]@]"
          pp
          sorted_expected
          pp
          sorted_found

(** Fetch consensus operations and check that they match the expected contents.
    Defaults to "head" if no [block] is provided. *)
let check_consensus_operations ?expected_attestations_committee
    ?expected_preattestations_committee ?expected_preattestations
    ?expected_attestations ?expected_dal_attestations ?block client =
  let* consensus_operations = fetch_consensus_operations ?block client in
  (* Partition the consensus operations list by kind *)
  let ( attestations_aggregates,
        preattestations_aggregates,
        attestations,
        preattestations ) =
    List.fold_left
      (fun ( attestations_aggregates,
             preattestations_aggregates,
             attestations,
             preattestations )
           operation
         ->
        let kind =
          JSON.(
            operation |-> "contents" |> as_list |> List.hd |-> "kind"
            |> as_string)
        in
        match kind with
        | "attestations_aggregate" ->
            ( operation :: attestations_aggregates,
              preattestations_aggregates,
              attestations,
              preattestations )
        | "preattestations_aggregate" ->
            ( attestations_aggregates,
              operation :: preattestations_aggregates,
              attestations,
              preattestations )
        | "attestation" | "attestation_with_dal" ->
            ( attestations_aggregates,
              preattestations_aggregates,
              operation :: attestations,
              preattestations )
        | "preattestation" ->
            ( attestations_aggregates,
              preattestations_aggregates,
              attestations,
              operation :: preattestations )
        | _ -> Test.fail "check_consensus_operations: unexpected operation")
      ([], [], [], [])
      consensus_operations
  in
  (* Checking attestations_aggregate *)
  let () =
    check_aggregated_consensus_operation
      Attestation
      ~expected:expected_attestations_committee
      attestations_aggregates
  in
  (* Checking preattestations_aggregate *)
  let () =
    check_aggregated_consensus_operation
      Preattestation
      ~expected:expected_preattestations_committee
      preattestations_aggregates
  in
  (* Checking attestations *)
  let () =
    check_non_aggregated_consensus_operations
      Attestation
      ~expected:expected_attestations
      attestations
  in
  (* Checking preattestations *)
  let () =
    check_non_aggregated_consensus_operations
      Preattestation
      ~expected:expected_preattestations
      preattestations
  in
  let () =
    check_dal
      ~expected:expected_dal_attestations
      ~attestations_aggregates
      ~attestations
  in
  unit

let simple_attestation_aggregation ~abaab ~remote_mode protocol =
  log_step 1 "Initialize a node and a client with protocol" ;
  let consensus_rights_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      ([
         (["allow_tz4_delegate_enable"], `Bool true);
         (["aggregate_attestation"], `Bool true);
         (* Diminish some constants to activate consensus keys faster *)
         (["blocks_per_cycle"], `Int 2);
         (["nonce_revelation_threshold"], `Int 1);
         (["consensus_rights_delay"], `Int consensus_rights_delay);
         (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
         (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
       ]
      @ abaab_threshold ~abaab ~protocol)
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
  log_step 3 "Generate fresh BLS consensus keys for bootstrap1 to bootstrap3" ;
  let* b1 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap1 client in
  let* b2 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap2 client in
  let* b3 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap3 client in
  let delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.append [b1; b2; b3]
    |> List.map (fun (account : Account.key) -> account.Account.alias)
  in
  (* Expected committee that should be found in attestations aggregate *)
  let expected_attestations_committee = [bootstrap1; bootstrap2; bootstrap3] in
  (* Expected attestations that should be found non-aggregated *)
  let expected_attestations = [bootstrap4; bootstrap5] in
  (* Testing the "bake for" command *)
  log_step 4 "Bake for until level 8" ;
  (* Baking until level 8 ensures that the BLS consensus keys are activated *)
  let* () = Client.bake_for_and_wait ~count:7 ~keys:delegates client in
  log_step 5 "Check consensus operations" ;
  let* () =
    check_consensus_operations
      ~expected_attestations_committee
      ~expected_attestations
      client
  in
  (* Testing the "bake for" command with minimal timestamp *)
  log_step 6 "Bake for with minimal timestamp until level 10" ;
  let* () =
    Client.bake_for_and_wait
      ~minimal_timestamp:true
      ~count:2
      ~keys:delegates
      client
  in
  log_step 7 "Check consensus operations" ;
  let* () =
    check_consensus_operations
      ~expected_attestations_committee
      ~expected_attestations
      client
  in
  (* Testing the baker automaton *)
  log_step 8 "Start a baker and wait until level 12" ;
  let* _baker = Agnostic_baker.init ~remote_mode ~delegates node client in
  let* _ = Node.wait_for_level node 12 in
  log_step 9 "Check consensus operations" ;
  let* () =
    check_consensus_operations
      ~expected_attestations_committee
      ~expected_attestations
      client
  in
  unit

(* Test that the baker aggregates eligible attestations.*)
let simple_attestations_aggregation_local_context ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:(title_abaab ~abaab "Simple attestations aggregation local context")
    ~tags:[team; "baker"; "attestation"; "aggregation"; "local"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  simple_attestation_aggregation ~abaab ~remote_mode:false protocol

(* Test that the baker aggregates eligible attestations.*)
let simple_attestations_aggregation_remote_node ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:(title_abaab ~abaab "Simple attestations aggregation remote node")
    ~tags:[team; "baker"; "attestation"; "aggregation"; "remote"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  simple_attestation_aggregation ~abaab ~remote_mode:true protocol

let prequorum_check_levels ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:(title_abaab ~abaab "prequorum monitoring check operations level")
    ~tags:[team; "prequorum"; "monitoring"; "check"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let parameter_file =
    Protocol.parameter_file ~constants:Constants_mainnet protocol
  in
  (* Using custom constants to make the test run faster *)
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Left parameter_file)
      ([(["minimal_block_delay"], `String "4")]
      @ abaab_threshold ~abaab ~protocol)
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in
  let* _ = Node.wait_for_level node 1 in
  let delegate = Constant.bootstrap1 in
  let* baker =
    Agnostic_baker.init
      ~event_level:`Debug
      ~delegates:[Constant.activator.public_key_hash]
      node
      client
  in
  Agnostic_baker.log_events ~max_length:1000 baker ;
  let* current_level =
    Client.bake_for_and_wait_level
      ~node
      ~keys:(public_key_hashes Constant.all_secret_keys)
      ~count:3
      client
  in
  let previous_level = current_level - 1 in
  let next_level = current_level + 1 in
  let* block_payload_hash =
    let* json = Client.RPC.call client @@ RPC.get_chain_block_header () in
    return @@ JSON.(json |-> "payload_hash" |> as_string)
  in
  let preattest_for ~delegate level =
    let* slot =
      Operation.Consensus.get_attestation_slot ~level ~protocol ~delegate client
    in
    let* _ =
      Operation.Consensus.preattest_for
        ~slot
        ~level
        ~block_payload_hash
        ~round:0
        ~protocol
        delegate
        client
    in
    unit
  in
  let wait_for_preattestations_received () =
    Agnostic_baker.wait_for
      baker
      "preattestations_received.v0"
      JSON.(
        fun json ->
          let count = json |-> "count" |> as_int in
          let delta = json |-> "delta_power" |> as_int in
          let voting_power = json |-> "voting_power" |> as_int in
          let preattestations = json |-> "preattestations" |> as_int in
          Some (count, delta, voting_power, preattestations))
  in
  let wait_for_non_relevant_operation_received () =
    Agnostic_baker.wait_for baker "non_relevant_operation_received.v0" (fun _ ->
        Some ())
  in
  Log.info "Injecting a preattestation for level %d round 0" current_level ;
  (* Listen for the next preattestations_received event and inject a
     preattestation for the current level. The preattestation is expected to be
     accounted in the prequorum monitoring. *)
  let waiter = wait_for_preattestations_received () in
  let* () = preattest_for ~delegate current_level in
  let* count, delta, voting_power, preattestations = waiter in
  if count <> 1 || delta = 0 || voting_power = 0 || preattestations <> 1 then
    Test.fail "Prequorum is expected to progress" ;
  Log.info "Injecting a preattestation for level %d round 0" previous_level ;
  (* Same process but we inject a preattestation for the previous level. This
     time, the preattestation is not expected to be accounted in the prequorum
     monitoring. *)
  let waiter = wait_for_non_relevant_operation_received () in
  let* () = preattest_for ~delegate previous_level in
  let* () = waiter in
  Log.info "Injecting a preattestation for level %d round 0" next_level ;
  (* Same for next level. Again, the preattestation is not expected to be
     accounted in the prequorum monitoring. *)
  let waiter = wait_for_non_relevant_operation_received () in
  let* () = preattest_for ~delegate next_level in
  let* () = waiter in
  let delegate2 = Constant.bootstrap2 in
  Log.info "Injecting a preattestation for level %d round 0" current_level ;
  (* Listen for the next preattestations_received event and inject a
     preattestation for delegate2 for the current level. The preattestation is
     expected to be accounted in the prequorum monitoring. *)
  let waiter = wait_for_preattestations_received () in
  let* () = preattest_for ~delegate:delegate2 current_level in
  let* count', delta', voting_power', preattestations' = waiter in
  if
    count' <> 1 || delta' = 0
    || voting_power' <= voting_power
    || preattestations' <> 2
  then Test.fail "Prequorum is expected to progress" ;
  unit

let z_of_bool_vector dal_attestation =
  let aux (acc, n) b =
    let bit = if b then 1 else 0 in
    (acc lor (bit lsl n), n + 1)
  in
  Array.fold_left aux (0, 0) dal_attestation |> fst |> Z.of_int

let attestations_aggregation_on_reproposal ~abaab ~remote_mode protocol =
  let consensus_rights_delay = 1 in
  let consensus_committee_size = 256 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      ([
         (["allow_tz4_delegate_enable"], `Bool true);
         (["aggregate_attestation"], `Bool true);
         (* Using custom consensus constants to be able to trigger reproposals *)
         (["consensus_committee_size"], `Int consensus_committee_size);
         (["consensus_threshold_size"], `Int 70);
         (* Diminish some constants to activate consensus keys faster,
           and make round durations as small as possible *)
         (["minimal_block_delay"], `String "4");
         (["delay_increment_per_round"], `String "0");
         (["blocks_per_cycle"], `Int 2);
         (["nonce_revelation_threshold"], `Int 1);
         (["consensus_rights_delay"], `Int consensus_rights_delay);
         (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
         (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
       ]
      @ abaab_threshold ~abaab ~protocol)
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~additional_revealed_bootstrap_account_count:1
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in
  let* _ = Node.wait_for_level node 1 in
  (* Setup bootstrap6 as an additional delegate *)
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  Log.info
    "Generate BLS keys and assign them as consensus keys for bootstrap 1 to 3" ;
  let* consensus_key1 =
    Client.update_fresh_consensus_key ~algo:"bls" bootstrap1 client
  in
  let* consensus_key2 =
    Client.update_fresh_consensus_key ~algo:"bls" bootstrap2 client
  in
  let* consensus_key3 =
    Client.update_fresh_consensus_key ~algo:"bls" bootstrap3 client
  in
  let keys =
    public_key_hashes
      [
        consensus_key1;
        consensus_key2;
        consensus_key3;
        bootstrap1;
        bootstrap2;
        bootstrap3;
        bootstrap4;
      ]
  in
  let* () = Client.bake_for_and_wait ~keys client in
  let* companion_key1 =
    Client.update_fresh_companion_key ~algo:"bls" bootstrap1 client
  in
  let* companion_key2 =
    Client.update_fresh_companion_key ~algo:"bls" bootstrap2 client
  in
  let* companion_key3 =
    Client.update_fresh_companion_key ~algo:"bls" bootstrap3 client
  in
  Log.info "Bake until BLS consensus keys are activated" ;
  let* _ = Client.bake_for_and_wait ~keys ~count:6 client in
  (* Bootstrap5 does not have enough voting power to progress independently. We
     manually inject consensus operations to control the progression of
     consensus. *)
  Log.info "Launch a baker with bootstrap5" ;
  let* _baker =
    Agnostic_baker.init
      ~remote_mode
      ~delegates:[Constant.bootstrap5.public_key_hash]
      node
      client
  in
  let* _ = Client.bake_for_and_wait ~keys client in
  let base_level = 9 in
  (* BLS consensus keys are now activated. We feed the node with just enough
     consensus operations for the baker to bake a block at [base_level + 1]. *)
  let* round = fetch_round client in
  let* branch =
    Operation.Consensus.get_branch ~attested_level:base_level client
  in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int base_level)
      client
  in
  Log.info
    "Injecting consensus for bootstrap1 at level %d round %d@."
    base_level
    round ;
  let* dal_parameters = Dal_common.Parameters.from_client protocol client in
  let dal_attestation =
    Dal_common.Attestations.encode_for_one_lag protocol dal_parameters
    @@ Array.init 16 (fun _ -> true)
  in
  let* () =
    Operation.Consensus.(
      let* slot =
        get_attestation_slot
          ~level:base_level
          ~protocol
          ~consensus_key:consensus_key1
          client
      in
      let* _ =
        preattest_for
          ~protocol
          ~branch
          ~slot
          ~level:base_level
          ~round
          ~block_payload_hash
          consensus_key1
          client
      in
      let* _ =
        attest_for
          ~protocol
          ~branch
          ~slot
          ~level:base_level
          ~round
          ~block_payload_hash
          ~dal_attestation
          ~companion_key:companion_key1
          consensus_key1
          client
      in
      unit)
  in
  let* _ = Node.wait_for_level node (base_level + 1) in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1]
      ~expected_attestations:[bootstrap5]
      client
  in
  (* The baker running bootstrap5 doesn't have enough voting power to progress
     alone. Since we won't attest any block at [base_level + 1], it will keep
     baking blocks for [base_level + 1] as round increases. *)
  Log.info "Attesting level %d round %d with bootstrap2 & 4" base_level round ;
  (* Inject additional attestations for [base_level]. These attestations are
     expected to be included in the coming [base_level + 1] proposal. In
     particular, bootstrap2 attestation is expected to be incorporated into the
     aggregation. Since the baker didn't witnessed a prequorum, it is expected
     to bake a fresh proposal. *)
  let* () =
    Lwt_list.iter_s
      Operation.Consensus.(
        fun ((consensus_key, companion_key) : Account.key * Account.key option)
          ->
          let* slot =
            get_attestation_slot
              ~level:base_level
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            attest_for
              ~protocol
              ~branch
              ~slot
              ~level:base_level
              ~round
              ~block_payload_hash
              ~dal_attestation
              ?companion_key
              consensus_key
              client
          in
          unit)
      [(consensus_key2, Some companion_key2); (bootstrap4, None)]
  in
  let* _ = Node.wait_for_branch_switch ~level:(base_level + 1) node in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1; bootstrap2]
      ~expected_attestations:[bootstrap4; bootstrap5]
      client
  in
  Log.info
    "Preattesting the latest block at level %d with bootstrap1 & 2"
    (base_level + 1) ;
  (* We preattest the latest block at level [base_level + 1] with enough voting
     power to trigger a prequorum. Consequently, the baker is expected to lock
     on the preattested payload and only bake reproposals. *)
  let* () =
    let* round = fetch_round client in
    let* branch =
      Operation.Consensus.get_branch ~attested_level:(base_level + 1) client
    in
    let* block_payload_hash =
      Operation.Consensus.get_block_payload_hash client
    in
    Log.info
      "Preattesting level %d round %d with branch %s"
      (base_level + 1)
      round
      branch ;
    Lwt_list.iter_s
      Operation.Consensus.(
        fun (consensus_key : Account.key) ->
          let* slot =
            get_attestation_slot
              ~level:(base_level + 1)
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            preattest_for
              ~protocol
              ~branch
              ~slot
              ~level:(base_level + 1)
              ~round
              ~block_payload_hash
              consensus_key
              client
          in
          unit)
      [consensus_key1; consensus_key2; bootstrap4]
  in
  (* Inject additional attestations for [base_level]. These attestations are
     expected to be included in the coming [base_level + 1] reproposals. In
     particular, bootstrap3 attestation is expected to be incorporated into the
     aggregation. *)
  Log.info "Attesting level %d round %d with bootstrap3 & 6" base_level round ;
  let* () =
    Lwt_list.iter_s
      Operation.Consensus.(
        fun ((consensus_key, companion_key) : Account.key * Account.key option)
          ->
          let* slot =
            get_attestation_slot
              ~level:base_level
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            attest_for
              ~protocol
              ~branch
              ~slot
              ~level:base_level
              ~round
              ~block_payload_hash
              ~dal_attestation
              ?companion_key
              consensus_key
              client
          in
          unit)
      [(consensus_key3, Some companion_key3); (bootstrap6, None)]
  in
  let* _ = Node.wait_for_branch_switch ~level:(base_level + 1) node in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1; bootstrap2; bootstrap3]
      ~expected_preattestations_committee:[bootstrap1; bootstrap2]
      ~expected_attestations:[bootstrap4; bootstrap5; bootstrap6]
      ~expected_preattestations:[bootstrap4; bootstrap5]
      ~expected_dal_attestations:
        [
          (bootstrap1, dal_attestation);
          (bootstrap2, dal_attestation);
          (bootstrap3, dal_attestation);
          (bootstrap4, dal_attestation);
          (bootstrap6, dal_attestation);
        ]
      client
  in
  unit

(* Test that the baker correctly aggregates eligible attestations on reproposals.*)
let attestations_aggregation_on_reproposal_local_context ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:
      (title_abaab
         ~abaab
         "Attestations aggregation on reproposal local context")
    ~tags:[team; "baker"; "attestation"; "aggregation"; "reproposal"; "local"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  attestations_aggregation_on_reproposal ~abaab ~remote_mode:false protocol

let attestations_aggregation_on_reproposal_remote_node ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:
      (title_abaab ~abaab "Attestations aggregation on reproposal remote node")
    ~tags:[team; "baker"; "attestation"; "aggregation"; "reproposal"; "remote"]
    ~supports:Protocol.(From_protocol 023)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  attestations_aggregation_on_reproposal ~abaab ~remote_mode:true protocol

let aggregated_operations_retrival_from_block_content ~abaab =
  Protocol.register_test
    ~__FILE__
    ~title:
      (title_abaab
         ~abaab
         "Aggregated operations are retrieved from the block content")
    ~tags:[team; "baker"; "aggregation"; "reproposal"; "retrival"]
    ~supports:Protocol.(From_protocol 023)
  @@ fun protocol ->
  log_step 1 "Initialize a node and a client with protocol" ;
  let consensus_rights_delay = 1 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      ([
         (["allow_tz4_delegate_enable"], `Bool true);
         (["aggregate_attestation"], `Bool true);
         (* Diminish some constants to activate consensus keys faster *)
         (["blocks_per_cycle"], `Int 2);
         (["nonce_revelation_threshold"], `Int 1);
         (["consensus_rights_delay"], `Int consensus_rights_delay);
         (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
         (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
       ]
      @ abaab_threshold ~abaab ~protocol)
  in
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~parameter_file ()
  in
  log_step 2 "Wait for level 1" ;
  let* _ = Node.wait_for_level node 1 in
  log_step 3 "Generate fresh BLS consensus keys for bootstrap1 to bootstrap3" ;
  let* b1 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap1 client in
  let* b2 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap2 client in
  let* b3 = Client.update_fresh_consensus_key ~algo:"bls" bootstrap3 client in
  let delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.append [b1; b2; b3]
    |> List.map (fun (account : Account.key) -> account.Account.alias)
  in
  (* Expected committee that should be found in aggregations *)
  let expected_aggregated_committee = [bootstrap1; bootstrap2; bootstrap3] in
  (* Expected delegates that should be found non-aggregated *)
  let expected_non_aggregated = [bootstrap4; bootstrap5] in
  log_step 4 "Bake for until level 8" ;
  (* Baking until level 8 ensures that the BLS consensus keys are activated *)
  let* () = Client.bake_for_and_wait ~count:7 ~keys:delegates client in
  log_step 5 "Check consensus operations" ;
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:expected_aggregated_committee
      ~expected_attestations:expected_non_aggregated
      client
  in
  (* The bake for command bakes a block with handmade (aggregated) attestations
     that are not injected in the node. To repropose with these operations, the
     baker must retrieve them from the block content. *)
  let* () =
    Client.repropose_for_and_wait ~key:delegates ~minimal_timestamp:true client
  in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:expected_aggregated_committee
      ~expected_attestations:expected_non_aggregated
      client
  in
  let* () =
    Client.repropose_for_and_wait
      ~key:delegates
      ~force_reproposal:true
      ~minimal_timestamp:true
      client
  in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:expected_aggregated_committee
      ~expected_attestations:expected_non_aggregated
      ~expected_preattestations_committee:expected_aggregated_committee
      ~expected_preattestations:expected_non_aggregated
      client
  in
  (* Same as the bake_for command, the repropose_for ~force_reproposal:true
      bakes a block with handmade (aggregated) preattestations. To include them
     in a new reproposal, the baker must retrieve them from the block content. *)
  let* () =
    Client.repropose_for_and_wait ~key:delegates ~minimal_timestamp:true client
  in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:expected_aggregated_committee
      ~expected_attestations:expected_non_aggregated
      ~expected_preattestations_committee:expected_aggregated_committee
      ~expected_preattestations:expected_non_aggregated
      client
  in
  unit

let unable_to_reach_node_mempool =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker shuts down when unable to reach the node mempool"
    ~tags:[team; "operation_worker"; "shutdown"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~supports:Protocol.(From_protocol 023)
  @@ fun protocol ->
  log_step 1 "Initialize a node with the mempool disabled" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Connections 0; Synchronisation_threshold 0; Disable_mempool]
      `Client
      ~timestamp:Now
      ~protocol
      ()
  in
  log_step 2 "Run a baker" ;
  let* baker = Agnostic_baker.init node client in
  log_step 3 "Wait for baker termination" ;
  let* outcome =
    Lwt.choose
      [
        (let* () = Agnostic_baker.wait_for_termination baker in
         return `Terminated);
        (* Saves some CI time by failing after 2 minutes if the baker fails to
           shut down as expected.*)
        (let* () = Lwt_unix.sleep 120. in
         return `Timeout);
      ]
  in
  match outcome with
  | `Terminated -> unit
  | `Timeout ->
      Test.fail
        "The baker failed to shut down after being unable to reach the node \
         mempool for 2 minutes"

let stream_is_refreshed_on_timeout =
  Protocol.register_test
    ~__FILE__
    ~title:"Operation stream is refreshed on timeout"
    ~tags:[team; "operation_worker"; "timeout"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~supports:Protocol.(From_protocol 023)
  @@ fun protocol ->
  log_step 1 "Initialize a node with mainnet constants" ;
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      []
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in
  log_step 2 "Run a baker" ;
  let* baker =
    Agnostic_baker.init
      ~delegates:Constant.[activator.public_key_hash]
      node
      client
  in
  log_step 3 "Bake 2 levels" ;
  let* () =
    Client.bake_for_and_wait ~keys:[] ~minimal_timestamp:true ~count:2 client
  in
  (* Since the node will neither switch heads nor forward new operations, the
     operation worker is expected to refresh the monitor_operations stream after
     the equivalent of two rounds of inactivity. *)
  log_step 4 "Wait for stream_timeout event" ;
  let wait_for_stream_timeout () =
    Agnostic_baker.wait_for
      baker
      "monitor_operations_stream_timeout.v0"
      (fun _ -> Some ())
  in
  let* waiter =
    Lwt.choose
      [
        (let* () = wait_for_stream_timeout () in
         return `Stream_timeout);
        (* Saves some CI time by failing after 2 minutes if the baker fails to
           refresh the stream as expected.*)
        (let* () = Lwt_unix.sleep 120. in
         return `Timeout);
      ]
  in
  match waiter with
  | `Stream_timeout -> unit
  | `Timeout ->
      Test.fail
        "The baker did not refresh the streamed RPC after prolonged inactivity \
         from the mempool"

let test_reproposal_at_abaab_activation_level =
  Protocol.register_test
    ~__FILE__
    ~title:"Attestations aggregation on reproposal at abaab activation level"
    ~tags:
      [
        team;
        "baker";
        "attestation";
        "aggregation";
        "reproposal";
        "remote";
        "abaab";
      ]
    ~supports:Protocol.(From_protocol 024)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let consensus_rights_delay = 1 in
  let consensus_committee_size = 256 in
  let algo = Tezos_crypto.Signature.Bls in
  let consensus_key1 = Account.generate_new_key ~algo ~alias:"consensus_key1" in
  let consensus_key2 = Account.generate_new_key ~algo ~alias:"consensus_key2" in
  let consensus_key3 = Account.generate_new_key ~algo ~alias:"consensus_key3" in
  let overwrite_bootstrap_accounts =
    Some
      [
        ( bootstrap1,
          Some
            {
              Protocol.default_bootstrap_parameters with
              consensus_key = Some consensus_key1;
            } );
        ( bootstrap2,
          Some
            {
              Protocol.default_bootstrap_parameters with
              consensus_key = Some consensus_key2;
            } );
        ( bootstrap3,
          Some
            {
              Protocol.default_bootstrap_parameters with
              consensus_key = Some consensus_key3;
            } );
        (bootstrap4, None);
        (bootstrap5, None);
      ]
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~overwrite_bootstrap_accounts
      ~base:(Right (protocol, None))
      [
        (["allow_tz4_delegate_enable"], `Bool true);
        (["aggregate_attestation"], `Bool true);
        (* Using custom consensus constants to be able to trigger reproposals *)
        (["consensus_committee_size"], `Int consensus_committee_size);
        (["consensus_threshold_size"], `Int 70);
        (* Diminish some constants to activate consensus keys faster,
           and make round durations as small as possible *)
        (["minimal_block_delay"], `String "4");
        (["delay_increment_per_round"], `String "0");
        (["blocks_per_cycle"], `Int 2);
        (* [blocks_per_cycle] is too short in this testing scenario,
           so we increase [tolerated_inactivity_period] *)
        (["tolerated_inactivity_period"], `Int 99);
        (["nonce_revelation_threshold"], `Int 1);
        (["consensus_rights_delay"], `Int consensus_rights_delay);
        (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
        (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
        (* We start with 6 bootstrap accounts, 3 with a tz4 consensus key,
           a 4th one will activate his later, triggering abaab. *)
        (["all_bakers_attest_activation_threshold"; "numerator"], `Int 4);
        (["all_bakers_attest_activation_threshold"; "denominator"], `Int 6);
      ]
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~additional_revealed_bootstrap_account_count:1
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in
  let* _ = Node.wait_for_level node 1 in
  (* Register the consensus keys in the client *)
  let* () =
    Client.import_secret_key
      client
      consensus_key1.secret_key
      ~alias:consensus_key1.alias
  in
  let* () =
    Client.import_secret_key
      client
      consensus_key2.secret_key
      ~alias:consensus_key2.alias
  in
  let* () =
    Client.import_secret_key
      client
      consensus_key3.secret_key
      ~alias:consensus_key3.alias
  in
  (* Check tz4 ratio *)
  let* ratio = Client.RPC.call client @@ RPC.get_tz4_baker_number_ratio () in
  assert (String.equal (JSON.as_string ratio) "50.00%") ;
  (* Setup bootstrap6 as an additional delegate with a tz4 ck *)
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* _consensus_key6 =
    Client.update_fresh_consensus_key ~algo:"bls" bootstrap6 client
  in
  let keys =
    public_key_hashes
      [
        consensus_key1;
        consensus_key2;
        consensus_key3;
        bootstrap1;
        bootstrap2;
        bootstrap3;
        bootstrap4;
      ]
  in
  let* () = Client.bake_for_and_wait ~keys client in
  (* Check abaab activation_level
     Activation of last tz4 at beginning of cycle 2: level 5  *)
  let* ctxt =
    Client.RPC.call client @@ RPC.get_chain_block_context_raw ~value_path:[] ()
  in
  let activation_level =
    JSON.(ctxt |-> "all_bakers_attest_first_level" |-> "level" |> as_int)
  in
  assert (activation_level = 5) ;
  (* Bootstrap5 does not have enough voting power to progress independently. We
     manually inject consensus operations to control the progression of
     consensus. *)
  Log.info "Launch a baker with bootstrap5" ;
  let* _baker =
    Agnostic_baker.init
      ~delegates:[Constant.bootstrap5.public_key_hash]
      node
      client
  in
  let* _ = Client.bake_for_and_wait ~keys ~count:2 client in
  let base_level = activation_level - 1 in
  let* current_level =
    Client.RPC.call client @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (base_level = current_level.level) ;
  (* BLS consensus keys are now activated. We feed the node with just enough
     consensus operations for the baker to bake a block at [base_level + 1]. *)
  let* round = fetch_round client in
  let* branch =
    Operation.Consensus.get_branch ~attested_level:base_level client
  in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int base_level)
      client
  in
  Log.info
    "Injecting consensus for bootstrap1 at level %d round %d@."
    base_level
    round ;
  let* () =
    Operation.Consensus.(
      let* slot =
        get_attestation_slot
          ~level:base_level
          ~protocol
          ~consensus_key:consensus_key1
          client
      in
      let* _ =
        preattest_for
          ~protocol
          ~branch
          ~slot
          ~level:base_level
          ~round
          ~block_payload_hash
          consensus_key1
          client
      in
      let* _ =
        attest_for
          ~protocol
          ~branch
          ~slot
          ~level:base_level
          ~round
          ~block_payload_hash
          consensus_key1
          client
      in
      unit)
  in
  let* _ = Node.wait_for_level node (base_level + 1) in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1]
      ~expected_attestations:[bootstrap5]
      client
  in
  (* The baker running bootstrap5 doesn't have enough voting power to progress
     alone. Since we won't attest any block at [base_level + 1], it will keep
     baking blocks for [base_level + 1] as round increases. *)
  Log.info "Attesting level %d round %d with bootstrap2 & 4" base_level round ;
  (* Inject additional attestations for [base_level]. These attestations are
     expected to be included in the coming [base_level + 1] proposal. In
     particular, bootstrap2 attestation is expected to be incorporated into the
     aggregation. Since the baker didn't witnessed a prequorum, it is expected
     to bake a fresh proposal. *)
  let* () =
    Lwt_list.iter_s
      Operation.Consensus.(
        fun ((consensus_key, companion_key) : Account.key * Account.key option)
          ->
          let* slot =
            get_attestation_slot
              ~level:base_level
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            attest_for
              ~protocol
              ~branch
              ~slot
              ~level:base_level
              ~round
              ~block_payload_hash
              ?companion_key
              consensus_key
              client
          in
          unit)
      [(consensus_key2, None); (bootstrap4, None)]
  in
  let* _ = Node.wait_for_branch_switch ~level:(base_level + 1) node in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1; bootstrap2]
      ~expected_attestations:[bootstrap4; bootstrap5]
      client
  in
  Log.info
    "Preattesting the latest block at level %d with bootstrap1 & 2"
    (base_level + 1) ;
  (* We preattest the latest block at level [base_level + 1] with enough voting
     power to trigger a prequorum. Consequently, the baker is expected to lock
     on the preattested payload and only bake reproposals. *)
  let* () =
    let* round = fetch_round client in
    let* branch =
      Operation.Consensus.get_branch ~attested_level:(base_level + 1) client
    in
    let* block_payload_hash =
      Operation.Consensus.get_block_payload_hash client
    in
    Log.info
      "Preattesting level %d round %d with branch %s"
      (base_level + 1)
      round
      branch ;
    Lwt_list.iter_s
      Operation.Consensus.(
        fun (consensus_key : Account.key) ->
          let* slot =
            get_attestation_slot
              ~level:(base_level + 1)
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            preattest_for
              ~protocol
              ~branch
              ~slot
              ~level:(base_level + 1)
              ~round
              ~block_payload_hash
              consensus_key
              client
          in
          unit)
      [consensus_key1; consensus_key2; bootstrap4]
  in
  (* Inject additional attestations for [base_level]. These attestations are
     expected to be included in the coming [base_level + 1] reproposals. In
     particular, bootstrap3 attestation is expected to be incorporated into the
     aggregation. *)
  Log.info "Attesting level %d round %d with bootstrap3 & 6" base_level round ;
  let* () =
    Lwt_list.iter_s
      Operation.Consensus.(
        fun ((consensus_key, companion_key) : Account.key * Account.key option)
          ->
          let* slot =
            get_attestation_slot
              ~level:base_level
              ~protocol
              ~consensus_key
              client
          in
          let* _ =
            attest_for
              ~protocol
              ~branch
              ~slot
              ~level:base_level
              ~round
              ~block_payload_hash
              ?companion_key
              consensus_key
              client
          in
          unit)
      [(consensus_key3, None); (bootstrap6, None)]
  in
  let* _ = Node.wait_for_branch_switch ~level:(base_level + 1) node in
  let* () =
    check_consensus_operations
      ~expected_attestations_committee:[bootstrap1; bootstrap2; bootstrap3]
      ~expected_preattestations_committee:[bootstrap1; bootstrap2]
      ~expected_attestations:[bootstrap4; bootstrap5; bootstrap6]
      ~expected_preattestations:[bootstrap4; bootstrap5]
      client
  in
  (* Check tz4 ratio *)
  let* ratio = Client.RPC.call client @@ RPC.get_tz4_baker_number_ratio () in
  assert (String.equal (JSON.as_string ratio) "66.66%") ;
  unit

let register_with_abaab ~abaab ~protocols =
  baker_check_consensus_branch ~abaab protocols ;
  force_apply_from_round ~abaab protocols ;
  simple_attestations_aggregation_local_context ~abaab protocols ;
  simple_attestations_aggregation_remote_node ~abaab protocols ;
  prequorum_check_levels ~abaab protocols ;
  attestations_aggregation_on_reproposal_local_context ~abaab protocols ;
  attestations_aggregation_on_reproposal_remote_node ~abaab protocols ;
  aggregated_operations_retrival_from_block_content ~abaab protocols

let register ~protocols =
  check_node_version_check_bypass_test protocols ;
  check_node_version_allowed_test protocols ;
  check_node_version_no_commit_allowed_test protocols ;
  baker_simple_test protocols ;
  baker_reward_test protocols ;
  baker_stresstest protocols ;
  baker_stresstest_apply protocols ;
  bls_baker_test protocols ;
  baker_remote_test protocols ;
  unable_to_reach_node_mempool protocols ;
  stream_is_refreshed_on_timeout protocols ;
  test_reproposal_at_abaab_activation_level protocols ;
  register_with_abaab ~abaab:false ~protocols ;
  register_with_abaab ~abaab:true ~protocols
