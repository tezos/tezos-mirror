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
  let baker1 =
    Constant.bootstrap3
    (* we choose bootstrap3 because it has the higher number of expected block
       avoiding CI flakyness in case it misses some *)
  in
  let baker2 = Constant.bootstrap2 in

  (* Step 1: Start node and activate protocol *)
  Log.info "Starting node and activating protocol" ;
  let parameter_file =
    Protocol.parameter_file ~constants:Constants_sandbox protocol
  in
  (* Create a delegator account that delegates to baker1 *)
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
            delegate = Some baker1;
          },
        true );
    ]
  in
  let cycle_length = 16 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base:(Left parameter_file)
      [
        (["minimal_block_delay"], `String "4");
        (["blocks_per_cycle"], `Int cycle_length);
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

  (* Exclude baker2 from the list of baking delegate to ensure that it will miss
     block and attestation rewards. *)
  let delegates =
    [
      Constant.bootstrap1.alias;
      Constant.bootstrap3.alias;
      Constant.bootstrap4.alias;
      Constant.bootstrap5.alias;
    ]
  in
  (* Bake one block to ensure the head is on the target protocol *)
  let* () = Client.bake_for ~keys:delegates client in

  (* Step 2: Start tezindex connected to the node, watching baker1 and baker2 *)
  Log.info "Starting tezindex watching %s and %s" baker1.alias baker2.alias ;
  let tezindex =
    Tezindex.run
      ~node
      ~watched_addresses:[baker1.public_key_hash; baker2.public_key_hash]
      ()
  in

  (* Step 3: Wait for tezindex to be ready *)
  Log.info "Waiting for tezindex to be ready" ;
  let* () = Tezindex.wait_for_ready tezindex in

  (* Step 4: Start baker with delegates *)
  Log.info "Starting baker (excluding %s)" baker2.alias ;
  let* _baker = Agnostic_baker.init ~delegates node client in

  (* Step 5: Wait for enough blocks to complete a full cycle *)
  let target_level = cycle_length + 1 in
  Log.info "Waiting for level %d" target_level ;
  let* _level = Node.wait_for_level node target_level in

  (* Step 6: Query v1/rewards/split for cycle 0 data *)
  let query_cycle = 0 in

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

  (* Step 7: Assert baker1 rewards *)
  Log.info "Checking %s rewards" baker1.alias ;
  let* baker1_json =
    Tezindex.get_v1_rewards_split
      tezindex
      ~baker:baker1.public_key_hash
      ~cycle:query_cycle
  in
  check_field ~who:baker1.alias ~field:"blockRewardsStakedOwn" baker1_json ;
  check_field ~who:baker1.alias ~field:"attestationRewardsStakedOwn" baker1_json ;
  check_field
    ~who:baker1.alias
    ~field:"dalAttestationRewardsStakedOwn"
    baker1_json ;

  (* Step 8: Assert baker1 has delegators (itself + delegator1) *)
  Log.info "Checking %s delegators" baker1.alias ;
  let delegators = JSON.(baker1_json |-> "delegators" |> as_list) in
  check_field
    ~who:baker1.alias
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
    ~error_msg:
      (sf "delegator1 should appear in %s's delegators list" baker1.alias) ;

  (* Step 8b: Assert ownDelegatedBalance matches baker's entry in delegators
     and externalDelegatedBalance matches delegator1's known balance *)
  Log.info "Checking %s delegated balances" baker1.alias ;
  let baker_delegator_balance =
    List.find_map
      (fun d ->
        let addr = JSON.(d |-> "address" |> as_string) in
        if String.equal addr baker1.public_key_hash then
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
    ~who:baker1.alias
    ~field:"ownDelegatedBalance"
    ~expected:baker_delegator_balance
    baker1_json ;
  check_field
    ~who:baker1.alias
    ~field:"externalDelegatedBalance"
    ~expected:(Int64.of_int delegator1_balance)
    baker1_json ;

  (* Step 8c: Check blocks and expectedBlocks for baker1 *)
  Log.info "Checking blocks and expectedBlocks for %s" baker1.alias ;
  check_field ~who:baker1.alias ~field:"blocks" baker1_json ;
  check_field ~who:baker1.alias ~field:"expectedBlocks" baker1_json ;

  (* Step 9: Verify baker2 (not baking) has expectedBlocks > 0 but blocks = 0,
     and has lost attestation/DAL rewards *)
  Log.info "Checking %s has expected blocks but 0 baked blocks" baker2.alias ;
  let* baker2_json =
    Tezindex.get_v1_rewards_split
      tezindex
      ~baker:baker2.public_key_hash
      ~cycle:query_cycle
  in
  check_field
    ~who:baker2.alias
    ~field:"blocks"
    ~expected:(Int64.of_int 0)
    baker2_json ;
  check_field ~who:baker2.alias ~field:"expectedBlocks" baker2_json ;
  check_field ~who:baker2.alias ~field:"missedAttestationRewards" baker2_json ;
  check_field ~who:baker2.alias ~field:"missedDalAttestationRewards" baker2_json ;
  unit

let register ~protocols = rewards_split_test protocols
