(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Tezindex (octez-tezindex)
   Invocation:   dune exec tezt/tests/main.exe -- --file tezindex_test.ml
   Subject:      Verify that tezindex correctly indexes balance updates
                 and reports rewards/losses via its RPC server.
*)

let team = Tag.layer1

let rewards_split_test =
  Protocol.register_test
    ~__FILE__
    ~title:"tezindex rewards split"
    ~tags:[team; "tezindex"]
    ~supports:Protocol.(From_protocol 024)
    ~uses:(fun _protocol ->
      [Constant.octez_agnostic_baker; Constant.octez_tezindex])
  @@ fun protocol ->
  (* Step 1: Start node and activate protocol (sandbox: blocks_per_cycle=4) *)
  Log.info "Starting node and activating protocol" ;
  let parameter_file =
    Protocol.parameter_file ~constants:Constants_sandbox protocol
  in
  (* Create a delegator account that delegates to bootstrap1 *)
  let delegator1_balance = 500_000_000_000 in
  let delegator1 =
    Account.generate_new_key
      ~algo:Tezos_crypto.Signature.Ed25519
      ~alias:"delegator1"
  in
  let additional_bootstrap_accounts =
    [
      ( delegator1,
        Some
          {
            Protocol.balance = Some delegator1_balance;
            consensus_key = None;
            delegate = Some Constant.bootstrap1;
          },
        true );
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base:(Left parameter_file)
      [
        (["minimal_block_delay"], `String "4");
        (["blocks_per_cycle"], `Int 4);
        (["nonce_revelation_threshold"], `Int 1);
      ]
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~parameter_file
      ~protocol
      ~timestamp:Now
      ()
  in

  let delegates =
    [
      Constant.bootstrap1.alias;
      Constant.bootstrap2.alias;
      Constant.bootstrap3.alias;
      Constant.bootstrap4.alias;
    ]
  in
  (* Bake one block to ensure the head is on the target protocol *)
  let* () = Client.bake_for ~keys:delegates client in

  (* Step 2: Start tezindex connected to the node, watching bootstrap1 *)
  Log.info "Starting tezindex" ;
  let bootstrap1_pkh = Constant.bootstrap1.public_key_hash in
  let bootstrap5_pkh = Constant.bootstrap5.public_key_hash in
  let tezindex =
    Tezindex.run ~node ~watched_addresses:[bootstrap1_pkh; bootstrap5_pkh] ()
  in

  (* Step 3: Wait for tezindex to be ready *)
  Log.info "Waiting for tezindex to be ready" ;
  let* () = Tezindex.wait_for_ready tezindex in

  (* Step 4: Start baker with bootstrap1-4, excluding bootstrap5 *)
  Log.info "Starting baker with bootstrap1-4 (excluding bootstrap5)" ;
  let* _baker = Agnostic_baker.init ~delegates node client in

  (* Step 5: Wait for enough blocks (cycle 1 completes at level 4,
     cycle 2 starts at level 5 with blocks_per_cycle=4) *)
  let target_level = 9 in
  Log.info "Waiting for level %d" target_level ;
  let* _level = Node.wait_for_level node target_level in
  (* Step 6: Query v1/rewards/split for cycle 1 data *)
  let query_cycle = 1 in

  let check_field ~who ~field ?expected json =
    let v = JSON.(json |-> field |> as_int64) in
    Log.info "%s %s: %Ld mutez" who field v ;
    match expected with
    | Some exp ->
        Check.((v = exp) int64)
          ~error_msg:(sf "%s %s (%%L) should equal %%R" who field)
    | None ->
        Check.((v > 0L) int64)
          ~error_msg:(sf "%s %s should be positive, got %%L" who field)
  in

  (* Step 7: Assert bootstrap1 rewards *)
  Log.info "Checking bootstrap1 rewards" ;
  let* baker1_json =
    Tezindex.get_v1_rewards_split
      tezindex
      ~baker:bootstrap1_pkh
      ~cycle:query_cycle
  in
  check_field ~who:"bootstrap1" ~field:"blockRewardsStakedOwn" baker1_json ;
  check_field ~who:"bootstrap1" ~field:"attestationRewardsStakedOwn" baker1_json ;
  check_field
    ~who:"bootstrap1"
    ~field:"dalAttestationRewardsStakedOwn"
    baker1_json ;

  (* Step 8: Assert bootstrap1 has delegators (itself + delegator1) *)
  Log.info "Checking bootstrap1 delegators" ;
  let delegators = JSON.(baker1_json |-> "delegators" |> as_list) in
  check_field
    ~who:"bootstrap1"
    ~field:"delegatorsCount"
    ~expected:(Int64.of_int (List.length delegators))
    baker1_json ;
  Check.((List.length delegators >= 2) int)
    ~error_msg:
      "delegators count should be at least 2 (baker + delegator1), got %L" ;
  List.iter
    (fun d ->
      let addr = JSON.(d |-> "address" |> as_string) in
      Check.((String.length addr > 0) int)
        ~error_msg:"delegator address should not be empty" ;
      check_field ~who:addr ~field:"delegatedBalance" d)
    delegators ;
  (* Verify delegator1 appears in the list *)
  let delegator1_found =
    List.exists
      (fun d ->
        let addr = JSON.(d |-> "address" |> as_string) in
        String.equal addr delegator1.public_key_hash)
      delegators
  in
  Check.(delegator1_found = true)
    Check.bool
    ~error_msg:"delegator1 should appear in bootstrap1's delegators list" ;

  (* Step 8b: Assert ownDelegatedBalance matches baker's entry in delegators
     and externalDelegatedBalance matches delegator1's known balance *)
  Log.info "Checking bootstrap1 delegated balances" ;
  let baker_delegator_balance =
    List.find_map
      (fun d ->
        let addr = JSON.(d |-> "address" |> as_string) in
        if String.equal addr bootstrap1_pkh then
          Some JSON.(d |-> "delegatedBalance" |> as_int64)
        else None)
      delegators
  in
  let baker_delegator_balance =
    match baker_delegator_balance with
    | Some bal -> bal
    | None -> Test.fail "baker should appear in its own delegators list"
  in
  check_field
    ~who:"bootstrap1"
    ~field:"ownDelegatedBalance"
    ~expected:baker_delegator_balance
    baker1_json ;
  check_field
    ~who:"bootstrap1"
    ~field:"externalDelegatedBalance"
    ~expected:(Int64.of_int delegator1_balance)
    baker1_json ;

  (* Step 9: Assert bootstrap5 has lost attestation rewards *)
  Log.info "Checking bootstrap5 rewards" ;
  let* bootstrap5_json =
    Tezindex.get_v1_rewards_split
      tezindex
      ~baker:bootstrap5_pkh
      ~cycle:query_cycle
  in
  check_field
    ~who:"bootstrap5"
    ~field:"missedAttestationRewards"
    bootstrap5_json ;
  unit

let register ~protocols = rewards_split_test protocols
