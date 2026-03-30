(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL protocol migration tests. *)

open Dal_helpers
module Dal = Dal_common

let test_migration_plugin ~__FILE__ ~migrate_from ~migrate_to =
  let tags = ["plugin"]
  and description = "test plugin update"
  and scenario ~migration_level _dal_parameters client node dal_node =
    let* current_level =
      let* json = Node.RPC.(call node @@ get_chain_block_header_shell ()) in
      JSON.(json |-> "level" |> as_int |> return)
    in

    if migration_level <= current_level then
      Test.fail ~__LOC__ "The migration level (%d) is too low" migration_level ;

    let blocks_till_migration = migration_level - current_level in

    let wait_for_plugin =
      Dal_node.wait_for dal_node "dal_plugin_resolved.v0" (fun json ->
          let proto_hash = JSON.(json |-> "proto_hash" |> as_string) in
          if String.equal proto_hash (Protocol.hash migrate_to) then Some ()
          else None)
    in

    Log.info "Bake %d blocks" blocks_till_migration ;
    let* () = repeat blocks_till_migration (fun () -> bake_for client) in

    Log.info "Migrated to %s" (Protocol.name migrate_to) ;
    (* Even though the migration block is not yet finalized, the new plugin
       should have been registered, as the DAL node listens to the last L1 head. *)
    wait_for_plugin
  in
  test_l1_migration_scenario
    ~__FILE__
    ~migrate_from
    ~migrate_to
    ~scenario
    ~tags
    ~description
    ()

(* This test checks the following scenario (that occurred on `nextnet--20250203`):
   - start a DAL node attester in protocol [migrate_from]
   - then start a DAL node accuser in protocol [migrate_to]
   - check that no baker is denounced

   This issue was that bakers were denounced, because the DAL node attester was
   started when the DAL incentives were not enabled, so they did not check for
   traps, and the DAL node did not used the new protocol parameters after
   migrating to the new protocol, so they also did not check for traps when the
   DAL incentives were enabled.
*)
let test_migration_accuser_issue ~__FILE__ ~migrate_from ~migrate_to =
  let slot_index = 0 in
  let tags = ["accuser"; "migration"] in
  let description = "test accuser" in
  let scenario ~migration_level dal_parameters client node dal_node =
    let* proto_params =
      Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
    in
    let blocks_per_cycle =
      JSON.(proto_params |-> "blocks_per_cycle" |> as_int)
    in
    assert (migration_level < blocks_per_cycle) ;
    let* level = Client.level client in
    let first_level = level + 1 in
    let last_level = blocks_per_cycle in
    Log.info
      "Publish slots from first_level = %d to last_level = %d"
      first_level
      last_level ;
    let _promise =
      simple_slot_producer
        ~slot_size:dal_parameters.Dal.Parameters.cryptobox.slot_size
        ~slot_index
        ~from:(migration_level - 1)
        ~into:last_level
        dal_node
        node
        client
    in
    Log.info "Start bakers for the current and the next protocols" ;

    let baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.create ~dal_node_rpc_endpoint node client
    in
    let* () = Agnostic_baker.run baker in

    let* _level = Node.wait_for_level node migration_level in
    Log.info "migrated to next protocol, starting a second DAL node accuser" ;
    let dal_node_p2p_endpoint = Dal_node.listen_addr dal_node in
    let accuser = Dal_node.create ~node () in
    let* () =
      Dal_node.init_config
        ~peers:[dal_node_p2p_endpoint]
        ~operator_profiles:[slot_index]
        accuser
    in
    let* () = Dal_node.run accuser ~wait_ready:true in

    let* _level = Node.wait_for_level node last_level in
    let* () = Agnostic_baker.terminate baker in

    Lwt_list.iter_s
      (fun delegate ->
        let* dal_participation =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_delegate_dal_participation
               ~block:(string_of_int (last_level - 1))
               delegate.Account.public_key_hash
        in
        Check.is_false
          dal_participation.denounced
          ~__LOC__
          ~error_msg:"Expected the delegate to not be denounced" ;
        unit)
      (Array.to_list Account.Bootstrap.keys)
  in
  test_l1_migration_scenario
    ~__FILE__
    ~scenario
    ~tags
    ~description
    ~uses:[Constant.octez_agnostic_baker]
    ~activation_timestamp:Now
    ~operator_profiles:[slot_index]
    ~minimal_block_delay:
      (* to be sure there is enough time to publish slots *)
      "2"
    ~blocks_per_cycle:32
    ~consensus_committee_size:512
      (* Use more blocks per cycle and more shards so that we're sure that we
         encounter traps: there are 32 (blocks per cycle) - 4 (migration level)
         - 8 (attestation lag) = 20 attestable levels; so 20 * 512 = 10240
         shards. Since [traps_fraction = 1 / 2000], the probability to not
         encounter at least one trap in these many shards is (1 - 1/2000)^10240
         = 0.6%. *)
    ~number_of_shards:512
    ~migrate_from
    ~migrate_to
    ()

(* This test checks that the migration goes well when the attestation lag is
   reduced. It uses the baker and features publication at the last level of
   "previous" protocol. It verifies that this publication is not attested in
   the "new" protocol.

   Here is an explanation of the potential issue:
   We denote by M the migration level. We assume a migration from attestation
   lag from 8 to 5 in this explanation.

   If we use publication time protocol, we expect

   M - 9 -> attested at M - 1
   M - 8 -> attested at M
   M - 7 -> attested at M + 1
   M - 6 -> attested at M + 2
   M - 5 -> attested at M + 3
   M - 4 -> attested at M + 4
   M - 3 -> attested at M + 5 <<< Conflict
   M - 2 -> attested at M + 6 <<<
   M - 1 -> attested at M + 7 <<<
   M     -> attested at M + 5 <<< Conflict
   M + 1 -> attested at M + 6 <<<
   M + 2 -> attested at M + 7 <<<
   M + 3 -> attested at M + 8

   We see that attestations at levels M + 5, M + 6 and M + 7 are ambiguous
   since there are 2 publication levels which are expected to be attested at
   the same level.
   The publication levels targetting attestation at an ambiguous level are
   marked with <<<.

   If we use attestation time protocol, we expect

   M - 10 -> attested at M - 2
   M - 9  -> attested at M - 1
   M - 8  -> attested at M
   M - 7  -> undefined
   M - 6  -> undefined
   M - 5  -> undefined
   M - 4  -> attested at M + 1
   M - 3  -> attested at M + 2

   There is an issue in both cases, so one has to make a choice.
   We assume in this test that the publications between
   M - previous_attestations_lag + 1 and M are not attested.
*)
let test_migration_with_attestation_lag_change ~__FILE__ ~migrate_from
    ~migrate_to =
  let tags = ["migration"; "dal"; "attestation_lag"] in
  let description = "test migration with reduction of the attestation_lag" in
  let {log_step} = init_logger () in
  (* We select a slot index to publish on. *)
  let slot_index = 0 in
  let scenario ~migration_level (dal_parameters : Dal.Parameters.t) client node
      dal_node =
    let publish_and_store level dal_params cryptobox =
      let publish source ~index message =
        let* _op_hash =
          publish_dummy_slot ~source ~index ~message cryptobox client
        in
        unit
      in
      let source = Constant.bootstrap1 in
      let slot_content = Format.asprintf "content at level %d" level in
      let* () = publish source ~index:slot_index slot_content in
      let* _commitment, _proof =
        let slot_size = dal_params.Dal.Parameters.cryptobox.slot_size in
        Helpers.(
          store_slot dal_node ~slot_index @@ make_slot ~slot_size slot_content)
      in
      Log.info "Slot (normally) published at level %d" level ;
      unit
    in
    (* [dal_parameters] should contain the parameters of the "previous"
       protocol. *)
    log_step "Getting the parameters of the next protocol" ;
    let base = Either.right (migrate_to, None) in
    let* proto_parameters = generate_protocol_parameters base migrate_to [] in
    let new_dal_parameters =
      Dal.Parameters.from_protocol_parameters proto_parameters
    in
    let old_lag = dal_parameters.attestation_lag in
    let new_lag = new_dal_parameters.attestation_lag in
    let* () =
      let* res =
        Cryptobox.init_prover_dal
          ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
          ~fetch_trusted_setup:false
          ()
      in
      match res with
      | Ok () -> unit
      | Error _ -> Test.fail "Cannot init the cryptobox prover mode"
    in
    let old_cryptobox =
      match Cryptobox.make dal_parameters.cryptobox with
      | Ok cryptobox -> cryptobox
      | Error (`Fail s) -> Test.fail "Old cryptobox cannot be computed: %s" s
    in
    let new_cryptobox =
      match Cryptobox.make new_dal_parameters.cryptobox with
      | Ok cryptobox -> cryptobox
      | Error (`Fail s) -> Test.fail "New cryptobox cannot be computed: %s" s
    in
    let* level = Client.level client in
    (* We will publish random data for a whole cycle, which includes a migration
       in the middle. *)
    let first_level = level + 1 in
    let last_publication_level = migration_level + new_lag + 2 in

    (* We want to have enough levels to attest in the "previous" protocol and to
       attest commitments published in the "next" protocol. *)
    log_step
      "Checking that the migration level is at good distance of both ends" ;
    assert (first_level + old_lag < migration_level) ;
    assert (migration_level + new_lag < last_publication_level) ;
    log_step
      "Producing DAL slots and baking blocks from first_level = %d to \
       last_level = %d"
      first_level
      last_publication_level ;
    let rec loop loop_lvl =
      let current_cryptobox =
        if loop_lvl <= migration_level then old_cryptobox else new_cryptobox
      in
      let current_dal_params =
        if loop_lvl <= migration_level then dal_parameters
        else new_dal_parameters
      in
      if loop_lvl > last_publication_level then unit
      else (
        Log.info "Currently at level %d, publishing and baking" loop_lvl ;
        if loop_lvl = migration_level then
          Log.info "Migration is happening in this level" ;
        let* () =
          publish_and_store loop_lvl current_dal_params current_cryptobox
        in
        let* () =
          bake_for ~dal_node_endpoint:(Dal_node.rpc_endpoint dal_node) client
        and* _ = Node.wait_for_level node loop_lvl
        and* () =
          if loop_lvl > 3 then
            wait_for_layer1_final_block dal_node (loop_lvl - 2)
          else unit
        in
        loop (loop_lvl + 1))
    in
    let* () = loop first_level in
    log_step "Baking %d last blocks before doing the checks" new_lag ;
    let* () =
      bake_for
        ~count:new_lag
        ~dal_node_endpoint:(Dal_node.rpc_endpoint dal_node)
        client
    in
    let attested_levels =
      first_level + old_lag + 2 --> (last_publication_level + new_lag)
    in
    let* all_slot_availabilities =
      Dal.collect_slot_availabilities node ~attested_levels
    in
    let proto_and_dal_params attested_level =
      if attested_level > migration_level then (migrate_to, new_dal_parameters)
      else (migrate_from, dal_parameters)
    in
    let to_attested_levels ~published_level =
      if published_level < migration_level then
        (* For pre-migration publications, each lag in the old protocol's
           attestation_lags produces an attested level. The attested level may
           fall before or after migration, so we select the right protocol and
           parameters for each, and compute the lag_index in that protocol's
           attestation_lags list. *)
        let old_lags = dal_parameters.Dal.Parameters.attestation_lags in
        List.filter_map
          (fun lag ->
            let attested_level = published_level + lag in
            let proto, dal_params = proto_and_dal_params attested_level in
            let lags = dal_params.Dal.Parameters.attestation_lags in
            match List.find_index (fun l -> l = lag) lags with
            | Some lag_index ->
                Some (attested_level, lag_index, dal_params, proto)
            | None -> None)
          old_lags
      else
        Dal.to_attested_levels
          ~protocol:migrate_to
          ~dal_parameters:new_dal_parameters
          ~published_level
    in

    let check_if_metadata_contain_expected_dal published_level =
      let* is_attested =
        Dal.is_slot_attested
          ~endpoint:(Node.as_rpc_endpoint node)
          ~published_level
          ~slot_index
          ~to_attested_levels
          all_slot_availabilities
      in
      if published_level <= migration_level - old_lag then (
        log_step
          "Checking that published level %d which is at least %d before \
           migration is considered as attested"
          old_lag
          published_level ;
        Check.is_true
          is_attested
          ~__LOC__
          ~error_msg:
            "Slot before migration - old_lag is expected to be attested")
      else if published_level <= migration_level then (
        (* Slots published in the window [migration_level - old_lag,
           migration_level] have their attestation window crossing the migration
           boundary. In protocol U, the protocol zeroes out DAL attestation
           payloads for the first [attestation_lag] blocks of the new protocol
           (see [record_dal_content] in [apply.ml]).  So a cross-migration slot
           is attested only if at least one lag produces an attested level
           strictly before [migration_level], where it is processed entirely by
           the old protocol. *)
        let expected_attested =
          Protocol.number migrate_to <> 025
          || List.exists
               (fun lag -> published_level + lag < migration_level)
               dal_parameters.attestation_lags
        in
        log_step
          "Checking that a slot published at level %d, just before migration \
           is attested: %b (old_lag=%d, new_lag=%d)."
          published_level
          expected_attested
          old_lag
          new_lag ;
        Check.(
          (is_attested = expected_attested)
            bool
            ~__LOC__
            ~error_msg:
              "Slot published before migration attested? Expected %R, got %L"))
      else (
        log_step
          "Checking that published level %d which is after migration is \
           attested"
          published_level ;
        Check.is_true
          is_attested
          ~__LOC__
          ~error_msg:"Slot published after migration is expected to be attested") ;
      unit
    in
    let rec loop published_level =
      if published_level <= last_publication_level then
        let* () = check_if_metadata_contain_expected_dal published_level in
        loop (published_level + 1)
      else unit
    in
    let* () = loop (first_level + old_lag + 2) in
    unit
  in
  test_l1_migration_scenario
    ~__FILE__
    ~scenario
    ~tags
    ~description
    ~activation_timestamp:Now
    ~operator_profiles:[slot_index]
    ~migration_level:13
    ~migrate_from
    ~migrate_to
    ()

(* A migration test for smart rollups importing a DAL page.
   There are 5 levels involved when a DAL page is imported by a rollup:
   1. The publication level (P),
   2. The attestation level (A),
   3. The import level (I),
   4. The commitment level (C),
   5. The refutation level (R).
   We have the guarantee that P < A ≤ I ≤ C < R.

   In this test, we enforce that the migration level (M) is such that A < M < I. *)

let test_migration_with_rollup ~__FILE__ ~migrate_from ~migrate_to =
  let tags = ["migration"; "dal"; "rollup"; "page_import"; "page_size"] in
  let description =
    "rollup DAL page import across migration uses DAL parameters at \
     publication level"
  in
  let {log_step} = init_logger () in

  (* Must match the toy kernel hardcoded constants. *)
  let published_level = 15 in
  let slot_index = 1 in
  (* Durable key written by the toy kernel. *)
  let durable_key = "/dal/page" in

  (* Pick a migration level strictly after the attestation level. *)
  let migration_level = 25 in

  (* Publishes a slot at [published_level] on [slot_index] and bakes enough so it
     becomes attestable+attested (at least published_level + dal_lag). *)
  let publish_and_attest_slot ~dal_lag ~slot_size dal_node node client =
    log_step
      "Publishing slot at level %d (slot_index=%d), then baking until attested"
      published_level
      slot_index ;
    (* Publish around the expected published_level. *)
    let _producer_promise =
      simple_slot_producer
        ~slot_size
        ~slot_index
        ~from:(published_level - 3)
        ~into:published_level
        dal_node
        node
        client
    in
    (* Ensure we reach (at least) the attestation level. *)
    let* _ = Node.wait_for_level node (published_level + dal_lag + 1) in
    unit
  in

  let scenario ~migration_level (dal_parameters : Dal.Parameters.t) client node
      dal_node =
    let dal_lag = dal_parameters.attestation_lag in

    (* Start baker. *)
    log_step "Start baker" ;
    let baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.create
        ~dal_node_rpc_endpoint
        ~delegates:
          (List.map
             (fun x -> x.Account.public_key_hash)
             Constant.all_secret_keys)
        node
        client
    in
    let* () = Agnostic_baker.run baker in

    (* Originate rollup BEFORE migration but do NOT start rollup node yet.
       This guarantees the import happens after migration when we start it. *)
    log_step "Originating rollup (kernel will import page after migration)" ;
    let* rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:(Tez.of_int 999)
        ~alias:"rollup"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:"wasm_2_0_0"
        ~boot_sector:
          (boot_sector
             Constant.WASM.echo_dal_reveal_pages_with_external_message)
        ~parameters_ty:"string"
        client
    in
    (* Ensure we are before publication level then publish slot at P and bake until attested. *)
    let* current_level = Client.level client in
    Check.((current_level < published_level) int)
      ~error_msg:
        "The level is already too high, we did not had the opportunity to \
         publish" ;

    (* Publish + attest slot at P under old proto. *)
    let old_slot_size = dal_parameters.cryptobox.slot_size in
    let* () =
      publish_and_attest_slot
        ~dal_lag
        ~slot_size:old_slot_size
        dal_node
        node
        client
    in

    (* Wait for migration. *)
    log_step
      "Waiting for migration level M=%d (switch to next protocol)"
      migration_level ;
    let* _ = Node.wait_for_level node migration_level in

    (* Start rollup node AFTER migration => import happens post-migration (I > M). *)
    (* Well, this is not really true, it enforces that the import happens after
       the L1 node has seen the migration level, but this is not really the property
       we are interested in, which is that the import level as seen by the rollup node
       is after the migration level. *)
    log_step "Starting rollup node AFTER migration (enforces A < M < I)" ;
    let sc_rollup_node =
      Sc_rollup_node.create
        ~name:"rollup-node"
        ~base_dir:(Client.base_dir client)
        ~default_operator:Constant.bootstrap1.alias
        ~kind:"wasm_2_0_0"
        ~dal_node
        Sc_rollup_node.Operator
        node
    in
    let* () = bake_for client in
    let* () = Sc_rollup_node.run sc_rollup_node rollup_address [] in
    (* Let the rollup node process a bit. *)
    log_step "Baking a few blocks before the kernel performs the DAL reveal" ;
    let import_level = migration_level + 2 in
    let* _ = Node.wait_for_level node import_level in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node import_level
    in
    log_step "Send the inbox message triggering the import" ;
    (* The message as to start with an I to trigger the import. *)
    let* () =
      Client.Sc_rollup.send_message
        ~src:Constant.bootstrap2.alias
        ~msg:"text:[\"I\"]"
        client
    in
    log_step "Baking 3 blocks after the messages" ;
    let* _ = Node.wait_for_level node (import_level + 3) in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node (import_level + 3)
    in
    (* Read durable storage key written by the kernel. *)
    log_step "Reading durable storage value at key %S" durable_key ;
    let* value_opt =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:durable_key
           ()
    in
    (* Value should exist and its length must match page_size at P (old proto). *)
    match value_opt with
    | None ->
        Test.fail
          "Expected durable key %S to exist (kernel should have written the \
           imported page)"
          durable_key
    | Some value ->
        let got_len = String.length value in
        log_step "Durable value length at %S is %d" durable_key got_len ;
        Check.(
          (* The pages are stored in hexa in the durable storage, so a factor 2 is expected. *)
          (got_len = dal_parameters.cryptobox.page_size * 2)
            int
            ~__LOC__
            ~error_msg:
              ("Imported page length should equal page_size at publication \
                level " ^ "(old proto): expected %R, got %L")) ;

        log_step
          "OK: imported page length matches old page_size; migration-safe \
           import works" ;
        (* Cleanup *)
        let* () = Sc_rollup_node.terminate sc_rollup_node in
        let* () = Agnostic_baker.terminate baker in
        unit
  in
  test_l1_migration_scenario
    ~__FILE__
    ~scenario
    ~tags
    ~description
    ~uses:
      [
        Constant.octez_agnostic_baker;
        Constant.octez_dal_node;
        Constant.octez_smart_rollup_node;
        Constant.WASM.echo_dal_reveal_pages_with_external_message;
      ]
    ~activation_timestamp:Now
    ~operator_profiles:[slot_index]
    ~migration_level
    ~migrate_from
    ~migrate_to
    ()

(* Test that a refutation game over a DAL page import is correctly resolved even
   when a protocol migration occurs between the DAL slot publication and the
   refutation game.

   Timeline:
   - [old protocol] origination level: smart rollup is originated
   - [old protocol] published_level: DAL slot is published
   - [old protocol] attestation_level: DAL slot is attested
   - migration at migration_level
   - [new protocol] ~migration_level + 6: both rollup nodes process the DAL
     page import; the loser flips the page content
   - [new protocol] refutation game resolves: honest player keeps bond,
     loser player is slashed

   The key property verified: the refutation game proof validation uses the DAL
   cryptobox parameters at the slot's publication level (old protocol), not the
   import level (new protocol).  This ensures the dal_snapshot stored in the
   game is correctly populated from the old protocol's history. *)
let test_refutation_with_dal_page_import_across_migration ~__FILE__
    ~migrate_from ~migrate_to =
  let tags = ["rollup"; "refutation"; "page_import"] in
  let description =
    "refutation game over DAL page import across protocol migration"
  in
  let {log_step} = init_logger () in
  (* Must match the toy kernel hardcoded constants. *)
  let published_level = 15 in
  let slot_index = 1 in
  (* Pick a migration level strictly after the attestation level
     matching the approach of test_migration_with_rollup.
     Since the legacy lag is 8, we take a level after 15+8=23. *)
  let migration_level = 25 in
  let commitment_period = 10 in
  let challenge_window = 10 in
  (* Timeout period per move in the refutation game.  We reduce it from the
     mainnet default (one week) to something reasonable for a test. *)
  let timeout_period = 50 in

  let scenario ~migration_level (dal_parameters : Dal.Parameters.t) client node
      dal_node =
    (* For migration from T024, one must NOT override the attestation_lag:
       the migration code in dal_slot_storage.ml asserts
         min alpha_attestation_lags < prev_attestation_lag
       as min alpha_attestation_lags is 1, we need prev_attestation_lag >= 2.
       With T024's default lag = 8 we get 5 < 8 which is satisfied.

       DAL availability condition (sc_rollup_proof_repr.ml):
       attested_level = published_level + attestation_lag_at_published_level
       not_too_recent = attested_level <= import_inbox_level
       With T024's lag = 8: attested_level = 15 + 8 = 23.
       This gives a theoretical lower bound (23) for import availability.
       In practice, when rollup nodes start post-migration and replay history,
       the first observed DAL-page decision can occur later than 23.

       The migration must happen AFTER the attestation (level 23) so that:
       - the skip list cell for level 15 is fully finalized before migration
         (preventing "DAL attestation status not found" errors in the rollup node),
       - the migration loop [current_level - prev_lag .. current_level] =
         [26-8..26] = [18..26] does not include level 15 and therefore does not
         alter its attestation status.
       The rollup nodes start after migration (level 26).  The refutation game is
       entirely in Alpha, but the proof must fetch T024's cryptobox parameters via
       find_dal_parameters(published_level=15) to validate the import. *)
    let dal_lag = dal_parameters.attestation_lag in
    let expected_import_level_from_old_lag = published_level + dal_lag in
    (* Run long enough for commitments, the challenge window, and the full
     refutation game dissection to complete.

     The "+70" budget accounts for:

     1. Dissection rounds. The WASM PVM refutation game has two phases:
        - Snapshot-level: isolate the faulty kernel_run among
          [commitment_period] snapshots. With [number_of_sections_in_dissection]
          (currently 32) > [commitment_period] (10), this takes 1 round.
        - Tick-level: binary-search within [ticks_per_snapshot] (5 * 10^13)
          ticks, narrowing by a factor of ~([number_of_sections] - 1) = 31
          per round. This takes ceil(log_31(5 * 10^13)) ≈ 10 rounds.
        Including the final proof step, the game needs ~12 moves total.

     2. Rollup-node reaction time. Each move is not instant: the node must
        detect its turn, compute the dissection, inject the operation, and
        wait for inclusion. Empirically this takes ~3-4 blocks per move,
        giving ~48 blocks for the dissection.

     3. Overhead (~20 blocks) for conflict detection, game initiation,
        and node catch-up after migration.

     If [number_of_sections_in_dissection] or [ticks_per_snapshot] change,
     the number of dissection moves changes as
       ceil(log_{sections - 1}(commitment_period))
       + ceil(log_{sections - 1}(ticks_per_snapshot))
       + 1 (for the final proof move)
     and this budget should be adjusted accordingly. *)
    let final_level =
      expected_import_level_from_old_lag + commitment_period + challenge_window
      + 70
    in
    (* If no loser_mode flip happened by this level, the scenario is almost
     certainly misconfigured; fail early instead of baking to [final_level]. *)
    let flip_deadline_level =
      max
        (migration_level + 3)
        (expected_import_level_from_old_lag + challenge_window)
    in
    let diagnostic_from = max 2 (expected_import_level_from_old_lag - 2) in
    let diagnostic_to =
      max (migration_level + 3) (expected_import_level_from_old_lag + 5)
    in

    log_step
      "Scenario parameters: published_level=%d slot_index=%d lag(runtime)=%d \
       expected_import_level_from_old_lag=%d migration_level=%d \
       flip_deadline_level=%d final_level=%d diagnostic_window=[%d..%d]"
      published_level
      slot_index
      dal_lag
      expected_import_level_from_old_lag
      migration_level
      flip_deadline_level
      final_level
      diagnostic_from
      diagnostic_to ;

    let dal_node_endpoint = Dal_node.rpc_endpoint dal_node in

    (* Originate rollup BEFORE migration. We do NOT start the rollup node
       yet; it will be started after migration so the import inbox_level is
       strictly greater than the migration level. *)
    log_step "Originating rollup (kernel: echo_dal_reveal_pages)" ;
    let* rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:(Tez.of_int 999)
        ~alias:"rollup"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:"wasm_2_0_0"
        ~boot_sector:(boot_sector Constant.WASM.echo_dal_reveal_pages)
        ~parameters_ty:"string"
        client
    in
    let* () = bake_for ~dal_node_endpoint client in
    let* current_level = Client.level client in
    Check.((current_level < published_level) int)
      ~error_msg:
        "The level is already too high, we did not have the opportunity to \
         publish" ;

    (* Publish and attest the DAL slot BEFORE migration so the dal_snapshot
       carried in the game (created post-migration) still contains this slot. *)
    log_step
      "Publishing slot at level %d (slot_index=%d), baking until attested"
      published_level
      slot_index ;
    let old_slot_size = dal_parameters.cryptobox.slot_size in
    let slot =
      Cryptobox.Internal_for_tests.generate_slot ~slot_size:old_slot_size
      |> Bytes.to_string
      |> Helpers.make_slot ~slot_size:old_slot_size
    in
    let* commitment, proof = Helpers.store_slot ~slot_index dal_node slot in
    (* Bake to published_level - 1 so the next block is published_level. *)
    let levels_to_pre_publish = published_level - 1 - current_level in
    let* () =
      if levels_to_pre_publish > 0 then
        bake_for ~count:levels_to_pre_publish ~dal_node_endpoint client
      else unit
    in
    log_step "Injecting DAL publish_commitment for slot_index=%d" slot_index ;
    let* _op_hash =
      publish_commitment
        ~force:true
        ~fee:5000
        ~source:Constant.bootstrap2
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    (* Bake one block: publication is included at exactly published_level. *)
    let* () = bake_for ~dal_node_endpoint client in
    (* Bake until attestation_level + 1 = published_level + dal_lag + 1 so the
       slot attestation is processed.  The ~dal_node_endpoint flag ensures DAL
       attestations are included automatically by the bake command. *)
    let* () = bake_for ~count:dal_lag ~dal_node_endpoint client in

    (* Bake to one block past the migration level.  The migration block is
       special: its binary-encoded constants may not be decodable by the new
       protocol's rollup-node plugin (the new field
       dal.minimal_participation_ratio is not yet present in the old binary
       format).  Starting rollup nodes only after level M+1 ensures the HEAD
       they observe has fully-migrated constants. *)
    log_step
      "Baking to migration level M=%d + 1 (switch to next protocol)"
      migration_level ;
    let* current_level = Client.level client in
    let to_migration = migration_level + 1 - current_level in
    let* () =
      repeat to_migration (fun () -> bake_for ~dal_node_endpoint client)
    in

    (* Start the HONEST rollup node after migration so the DAL page import
       inbox_level is greater than the migration level. *)
    log_step "Starting honest rollup node (bootstrap1) after migration" ;
    let honest_node =
      Sc_rollup_node.create
        ~name:"honest-rollup-node"
        ~base_dir:(Client.base_dir client)
        ~default_operator:Constant.bootstrap1.alias
        ~kind:"wasm_2_0_0"
        ~dal_node
        Sc_rollup_node.Operator
        node
    in
    let* () =
      Sc_rollup_node.run ~event_level:`Debug honest_node rollup_address []
    in

    (* Start the LOSER rollup node after migration.  It uses a loser mode that
       flips the content of the imported DAL page for this target
       (published_level, slot_index, page_index), creating a divergence with
       the honest rollup node. *)
    (* We pin the inbox_level to [migration_level + 1] instead of using the
       wildcard [*]. When the loser node catches up with the honest node
       after migration, the U025+ plugin is used to import the DAL page. The
       wildcard would make the loser node flip pages at every level, including
       levels that were already processed honestly before migration; pinning
       avoids that false divergence. *)
    let loser_inbox_level = migration_level + 1 in
    log_step
      "Starting loser rollup node (bootstrap2) with loser_mode reveal_dal_page \
       ... strategy:flip inbox_level:%d"
      loser_inbox_level ;
    let loser_mode =
      Format.sprintf
        "reveal_dal_page published_level:%d slot_index:%d page_index:2 \
         strategy:flip inbox_level:%d"
        published_level
        slot_index
        loser_inbox_level
    in
    let loser_node =
      Sc_rollup_node.create
        ~name:"loser-rollup-node"
        ~base_dir:(Client.base_dir client)
        ~default_operator:Constant.bootstrap2.alias
        ~kind:"wasm_2_0_0"
        ~dal_node
        ~loser_mode
        ~allow_degraded:true
        Sc_rollup_node.Operator
        node
    in
    let* () =
      Sc_rollup_node.run ~event_level:`Debug loser_node rollup_address []
    in

    (* Set up a conflict-detection monitor on the honest node.  We flip this
       flag from a background Lwt promise so we can assert later that a
       refutation game actually started. *)
    let conflict_detected_honest = ref false in
    let conflict_detected_loser = ref false in
    let _conflict_monitor_honest =
      let* json =
        Sc_rollup_node.wait_for
          honest_node
          "smart_rollup_node_conflict_detected.v0"
        @@ fun json -> Some json
      in
      conflict_detected_honest := true ;
      log_step "honest conflict event detected: %s" (JSON.encode json) ;
      Lwt.return_unit
    in
    let _conflict_monitor_loser =
      let* json =
        Sc_rollup_node.wait_for
          loser_node
          "smart_rollup_node_conflict_detected.v0"
        @@ fun json -> Some json
      in
      conflict_detected_loser := true ;
      log_step "loser conflict event detected: %s" (JSON.encode json) ;
      Lwt.return_unit
    in

    (* Monitor loser-mode DAL decisions to pinpoint exactly which inbox level
       first matches (or fails to match) the configured reveal selector. *)
    let loser_mode_events = ref 0 in
    let loser_mode_misses = ref 0 in
    let loser_mode_flips = ref 0 in
    let first_mismatch_level = ref None in
    let first_flip_level = ref None in
    let rec watch_loser_mode_dal_decisions () =
      let* json =
        Sc_rollup_node.wait_for loser_node "loser_mode_dal_decision.v0"
        @@ fun json -> Some json
      in
      incr loser_mode_events ;
      let decision =
        match JSON.(json |-> "decision" |> as_string_opt) with
        | Some d -> d
        | None -> "<unknown>"
      in
      let inbox_level = JSON.(json |-> "inbox_level" |> as_int_opt) in
      (match (decision, inbox_level) with
      | "didn't match", Some l ->
          incr loser_mode_misses ;
          if !first_mismatch_level = None then first_mismatch_level := Some l
      | d, Some l when String.starts_with ~prefix:"flipped" d ->
          incr loser_mode_flips ;
          if !first_flip_level = None then first_flip_level := Some l
      | _ -> ()) ;
      let inbox_level_s =
        match inbox_level with None -> "<none>" | Some l -> string_of_int l
      in
      if String.starts_with ~prefix:"flipped" decision then
        log_step
          "loser_mode flip event #%d: decision=%s inbox_level=%s raw=%s"
          !loser_mode_events
          decision
          inbox_level_s
          (JSON.encode json)
      else if decision = "didn't match" && !first_mismatch_level = inbox_level
      then
        log_step
          "loser_mode first mismatch at inbox_level=%s (event #%d)"
          inbox_level_s
          !loser_mode_events ;
      watch_loser_mode_dal_decisions ()
    in
    let _loser_mode_monitor = watch_loser_mode_dal_decisions () in

    (* Bake one block at a time until the refutation game resolves or
       final_level is reached.  Each bake_for call deterministically produces
       one block, giving rollup nodes time to react and inject their moves
       into the mempool before the next block. *)
    log_step
      "Baking to level %d (max) to allow refutation game to complete"
      final_level ;
    let* current_level = Client.level client in
    let game_seen = ref false in
    let first_game_level = ref None in
    let rec bake_and_check level =
      if level >= final_level then unit
      else
        let next_level = level + 1 in
        let* () = bake_for ~dal_node_endpoint client in
        (* Poll game state.  Once the game has been seen, poll every level
           to detect resolution promptly and terminate early. *)
        let should_poll_games =
          !game_seen || next_level <= diagnostic_to || next_level mod 5 = 0
        in
        let* no_active_games =
          if should_poll_games then (
            let* honest_games =
              Client.RPC.call client
              @@ RPC
                 .get_chain_block_context_smart_rollups_smart_rollup_staker_games
                   ~staker:Constant.bootstrap1.public_key_hash
                   rollup_address
                   ()
            in
            let* loser_games =
              Client.RPC.call client
              @@ RPC
                 .get_chain_block_context_smart_rollups_smart_rollup_staker_games
                   ~staker:Constant.bootstrap2.public_key_hash
                   rollup_address
                   ()
            in
            log_step
              "level=%d: staker_games sizes (honest=%d loser=%d)"
              next_level
              (List.length (JSON.as_list honest_games))
              (List.length (JSON.as_list loser_games)) ;
            if JSON.as_list honest_games <> [] || JSON.as_list loser_games <> []
            then (
              game_seen := true ;
              if !first_game_level = None then (
                first_game_level := Some next_level ;
                log_step "First refutation game observed at level=%d" next_level)) ;
            let no_active =
              JSON.as_list honest_games = [] && JSON.as_list loser_games = []
            in
            return no_active)
          else return false
        in
        let* () =
          if next_level >= diagnostic_from && next_level <= diagnostic_to then (
            let* honest_state_hash =
              Sc_rollup_node.RPC.call ~rpc_hooks honest_node
              @@ Sc_rollup_rpc.get_global_block_state_hash ()
            in
            let* loser_state_hash =
              Sc_rollup_node.RPC.call ~rpc_hooks loser_node
              @@ Sc_rollup_rpc.get_global_block_state_hash ()
            in
            log_step
              "level=%d: state_hashes honest=%s loser=%s equal=%b"
              next_level
              honest_state_hash
              loser_state_hash
              (String.equal honest_state_hash loser_state_hash) ;
            unit)
          else unit
        in
        let* () =
          if next_level >= flip_deadline_level && !loser_mode_flips = 0 then
            Test.fail
              "No loser_mode flip observed by level %d (migration_level=%d, \
               expected_import_level_from_old_lag=%d)"
              flip_deadline_level
              migration_level
              expected_import_level_from_old_lag
          else unit
        in
        if !game_seen && no_active_games then (
          log_step
            "Refutation game resolved at level=%d (early termination)"
            next_level ;
          unit)
        else bake_and_check next_level
    in
    let* () = bake_and_check current_level in

    log_step
      "Summary: conflict_detected(honest=%b loser=%b) loser_mode(events=%d \
       misses=%d flips=%d first_mismatch=%s first_flip=%s)"
      !conflict_detected_honest
      !conflict_detected_loser
      !loser_mode_events
      !loser_mode_misses
      !loser_mode_flips
      (match !first_mismatch_level with
      | None -> "<none>"
      | Some l -> string_of_int l)
      (match !first_flip_level with
      | None -> "<none>"
      | Some l -> string_of_int l) ;

    if not !game_seen then
      Test.fail
        "No refutation game was observed during the scenario (staker_games \
         stayed empty)" ;

    if !loser_mode_flips = 0 then
      Test.fail
        "No loser_mode flipped decision observed (events=%d, misses=%d, \
         first_mismatch=%s). Divergence was not injected."
        !loser_mode_events
        !loser_mode_misses
        (match !first_mismatch_level with
        | None -> "<none>"
        | Some l -> string_of_int l) ;

    (match !first_flip_level with
    | None -> ()
    | Some first_flip ->
        if first_flip < migration_level + 1 then
          Test.fail
            "First loser_mode flip must be post-migration (expected >= %d, got \
             %d)"
            (migration_level + 1)
            first_flip) ;

    (* Assert that the honest node detected the conflict. *)
    if not !conflict_detected_honest then
      Test.fail
        "Honest node did not detect conflict (loser_mode: events=%d, \
         misses=%d, flips=%d, first_mismatch=%s, first_flip=%s)"
        !loser_mode_events
        !loser_mode_misses
        !loser_mode_flips
        (match !first_mismatch_level with
        | None -> "<none>"
        | Some l -> string_of_int l)
        (match !first_flip_level with
        | None -> "<none>"
        | Some l -> string_of_int l) ;

    (* Assert no active games remain (game resolved). *)
    log_step "Checking no active refutation games remain" ;
    let* games =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_staker_games
           ~staker:Constant.bootstrap1.public_key_hash
           rollup_address
           ()
    in
    if JSON.as_list games <> [] then
      Test.fail
        "Expected no active refutation games after final_level=%d, but found \
         some"
        final_level ;

    (* Assert the loser's frozen bond is zero (slashed by the protocol). *)
    log_step
      "Checking deposits: loser should be slashed, honest should keep bond" ;
    let* loser_deposit =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_frozen_bonds
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    Check.(
      (loser_deposit = Tez.zero)
        Tez.typ
        ~__LOC__
        ~error_msg:
          "Loser should have been slashed (expected deposit = 0), got %L") ;

    (* Assert the honest player's frozen bond is non-zero (intact). *)
    let* honest_deposit =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_frozen_bonds
           ~id:Constant.bootstrap1.public_key_hash
           ()
    in
    if honest_deposit = Tez.zero then
      Test.fail
        "Honest player's bond should be non-zero after winning the refutation \
         game" ;

    log_step
      "OK: honest player won the refutation game over a DAL page import that \
       crosses a protocol migration" ;

    let* () = Sc_rollup_node.terminate honest_node in
    let* () = Sc_rollup_node.terminate loser_node in
    unit
  in
  if Protocol.number migrate_from >= 024 then
    test_l1_migration_scenario
      ~__FILE__
      ~scenario
      ~tags
      ~description
      ~uses:
        [
          Constant.octez_dal_node;
          Constant.octez_smart_rollup_node;
          Constant.WASM.echo_dal_reveal_pages;
        ]
      ~activation_timestamp:Now
      ~operator_profiles:[slot_index]
      ~parameter_overrides:
        (make_int_parameter
           ["smart_rollup_commitment_period_in_blocks"]
           (Some commitment_period)
        @ make_int_parameter
            ["smart_rollup_challenge_window_in_blocks"]
            (Some challenge_window)
        @ make_int_parameter
            ["smart_rollup_timeout_period_in_blocks"]
            (Some timeout_period))
      ~migration_level
      ~migrate_from
      ~migrate_to
      ()

(* Test that the DAL node's skip-list store contains the expected published
   levels after a protocol migration. This verifies that the data store is
   correctly maintained across the migration boundary. *)
let test_skip_list_store_with_migration ~__FILE__ ~migrate_from ~migrate_to
    ~migration_level =
  let slot_index = 3 in
  let non_gc_period, parameter_overrides =
    (* We choose some arbitrary, small values *)
    let a = 1 in
    let b = 2 in
    let c = 2 in
    (* The period in blocks for which cells are kept. *)
    let non_gc_period = 2 * (a + b + c) in
    ( non_gc_period,
      make_int_parameter ["smart_rollup_commitment_period_in_blocks"] (Some a)
      @ make_int_parameter ["smart_rollup_challenge_window_in_blocks"] (Some b)
      @ make_int_parameter
          [
            "smart_rollup_reveal_activation_level";
            "dal_attested_slots_validity_lag";
          ]
          (Some c) )
  in
  let scenario ~migration_level dal_parameters client node dal_node =
    Log.info "The \"non-GC period\" is %d" non_gc_period ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let rec publish_and_store ~max_level level =
      (* Try to publish a slot at each level *)
      if level > max_level then unit
      else
        let wait_mempool_injection =
          Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
        in
        let wait_for_dal_node =
          wait_for_layer1_final_block dal_node (level - 1)
        in
        let* _commitment =
          Helpers.publish_and_store_slot
            client
            dal_node
            Constant.bootstrap1
            ~index:slot_index
            ~force:true
          @@ Helpers.make_slot ~slot_size ("slot " ^ string_of_int level)
        in
        let* () = wait_mempool_injection in
        let* () = bake_for client in
        let* _level = Node.wait_for_level node (level + 1) in
        let* () = if level > 2 then wait_for_dal_node else unit in
        publish_and_store ~max_level (level + 1)
    in
    let lag = dal_parameters.Dal.Parameters.attestation_lag in
    Check.(
      (migration_level > lag)
        int
        ~error_msg:
          "The migration level (%L) should be greater than the attestation lag \
           (%R)") ;

    (* We bake enough blocks past migration so that the cells for the levels
       before migration should have been GC-ed. *)
    let last_final_level = migration_level + lag + 1 + non_gc_period in
    let target_head = last_final_level + 2 in

    let* starting_level = Client.level client in
    Log.info
      "Publishing in the previous protocol, from published level %d to %d"
      (starting_level + 1)
      migration_level ;
    let* () =
      publish_and_store ~max_level:(migration_level - 1) starting_level
    in

    Log.info "Migrated to the next protocol." ;

    let* new_proto_params =
      Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
    in
    let new_dal_params =
      Dal.Parameters.from_protocol_parameters new_proto_params
    in
    let new_lag = new_dal_params.attestation_lag in

    let last_confirmed_published_level = migration_level + 3 in
    let last_attested_level = last_confirmed_published_level + new_lag in
    (* The maximum level that needs to be reached (we use +2 to make last
       attested level final). *)
    let max_level = last_attested_level + 2 in
    Log.info
      "last published_level = %d, last attested_level = %d, last level = %d"
      last_confirmed_published_level
      last_attested_level
      max_level ;

    let* second_level_new_proto = Client.level client in
    assert (second_level_new_proto = migration_level) ;
    Log.info
      "Publish commitments in the new protocol, from published level %d to %d"
      (second_level_new_proto + 1)
      last_confirmed_published_level ;
    let* () =
      publish_and_store
        ~max_level:(last_confirmed_published_level - 1)
        second_level_new_proto
    in

    let wait_for_dal_node =
      wait_for_layer1_final_block dal_node last_final_level
    in
    let* current_level = Client.level client in
    let* () = bake_for client ~count:(target_head - current_level) in
    let* () = wait_for_dal_node in
    Log.info "Baked until level %d " target_head ;

    let first_level_with_cells =
      last_final_level - new_lag - non_gc_period + 1
    in
    assert (first_level_with_cells > migration_level) ;
    Log.info
      "Checking skip-list store for expected published levels %d to %d"
      first_level_with_cells
      last_final_level ;
    let expected_levels =
      let number_of_levels =
        if Protocol.number migrate_to >= 025 then non_gc_period + new_lag
        else non_gc_period
      in
      List.init number_of_levels (fun i ->
          string_of_int (first_level_with_cells + i))
    in
    check_skip_list_store
      dal_node
      ~number_of_slots:new_dal_params.number_of_slots
      ~expected_levels
  in
  test_l1_migration_scenario
    ~__FILE__
    ~migrate_from
    ~migrate_to
    ~migration_level
    ~scenario
    ~tags:["skip_list"; "store"]
    ~description:"test skip-list store across migration"
    ~operator_profiles:[slot_index]
    ~parameter_overrides
    ()

(* A commitment is published in the "previous protocol" and attested in the
   same protocol.

   More precisely, two attestations are crafted, one after "new attestation lag"
   levels, and one after "previous attestation lag" levels.

   Then, two corresponding denunciations are sent in the "new protocol".  We
   expect the one using the "new attestation lag" attestation to be invalid and
   the other one to be included.
*)
let test_accusation_migration_with_attestation_lag_decrease ~__FILE__
    ~migrate_from ~migrate_to =
  let slot_index = 0 in
  let tags = ["migration"; "dal"; "accusation"; "attestation_lag"] in
  let description =
    "test accusation during migration with reduction of the attestation_lag"
  in
  let scenario ~migration_level (dal_parameters : Dal.Parameters.t) client node
      dal_node =
    (* We do not use the DAL node in this test *)
    let* () = Dal_node.terminate dal_node in
    Log.info "Compute the attestation lag before and after migration" ;
    (* [dal_parameters] should contain the parameters of the "previous"
       protocol. *)
    let old_lag = dal_parameters.attestation_lag in
    let base = Either.right (migrate_to, None) in
    let* new_proto_parameters =
      generate_protocol_parameters base migrate_to []
    in
    let new_dal_parameters =
      Dal.Parameters.from_protocol_parameters new_proto_parameters
    in
    let new_lag = new_dal_parameters.attestation_lag in
    let new_lags = new_dal_parameters.attestation_lags in
    Log.info "Initializing the cryptobox" ;
    let* () = Helpers.init_prover ~__LOC__ () in
    let* cryptobox = Helpers.make_cryptobox dal_parameters.cryptobox in
    let slot_size = dal_parameters.cryptobox.slot_size in
    (* A slot which will be published should be targeted by the denunciation. *)
    let slot_with_trap = Helpers.(bytes_of_slot (make_slot ~slot_size "A")) in
    let commitment_with_trap, proof_with_trap, shards_with_proofs_with_trap =
      Helpers.get_commitment_and_shards_with_proofs
        cryptobox
        ~slot:slot_with_trap
    in
    let shard_index = 0 in
    let shard_denounced, proof_denounced =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs_with_trap
      |> Option.get
    in

    let* level = Client.level client in
    let publi_with_trap_level = level + 1 in
    Log.info
      "Computing the delegates which send the two attestations related to the \
       shard we want to denounce." ;
    (* Given that there are 2 potential attestation lags, we compute the one
       associated to both. *)
    (* This computation relies on the fact the shard index is the same as the
       index in the Tenderbake committee. *)
    let* delegate_old =
      get_delegate
        client
        ~protocol:migrate_from
        ~level:(publi_with_trap_level + old_lag - 1)
        ~tb_index:shard_index
    in
    let* delegate_new =
      get_delegate
        client
        ~protocol:migrate_from
        ~level:(publi_with_trap_level + new_lag - 1)
        ~tb_index:shard_index
    in
    (* We want to have enough levels to attest in "previous" what has been
       published at [publi_with_trap_level]. *)
    assert (publi_with_trap_level + old_lag <= migration_level) ;
    Log.info
      "Publishing the to-be-denounced slot at level %d."
      publi_with_trap_level ;
    let* _op_hash =
      Helpers.publish_commitment
        ~source:Constant.bootstrap1
        ~index:slot_index
        ~commitment:commitment_with_trap
        ~proof:proof_with_trap
        client
    in
    let* () = bake_for client in
    let availability = Slots [slot_index] in
    let craft_attestation delegate =
      let* attestation, _op_hash =
        inject_dal_attestation_exn
          ~protocol:migrate_from
          ~signer:delegate
          availability
          client
          dal_parameters
          (Node.as_rpc_endpoint node)
      in
      let* signature = Operation.sign attestation client in
      return (attestation, signature)
    in
    Log.info "Bake %d blocks" (new_lag - 1) ;
    let* () = bake_for ~count:(new_lag - 1) client in
    Log.info "Crafting the attestation using the \"new\" attestation lag" ;
    let* attestation_new = craft_attestation delegate_new in
    let* () =
      if old_lag <> new_lag then (
        Log.info "Bake %d blocks" (old_lag - new_lag) ;
        bake_for ~count:(old_lag - new_lag) client)
      else unit
    in
    Log.info "Crafting the attestation using the \"old\" attestation lag" ;
    let* attestation_old = craft_attestation delegate_old in
    Log.info "We bake until 2 levels after the migration." ;
    let* () =
      repeat
        (migration_level - publi_with_trap_level - old_lag + 3)
        (fun () -> bake_for client)
    in
    Log.info
      "Craft an entrapment evidence which uses the \"new\" attestation lag" ;
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol:migrate_to
        ~attestation:attestation_new
        ~slot_index
        ~lag_index:(List.length new_lags - 1)
        shard_denounced
        proof_denounced
    in
    Log.info "Inject this accusation" ;
    (* This accusation should be accepted only if the protocol migration does
       not imply a variation of the attestation lag *)
    if new_lag = old_lag then (
      let () =
        Log.info
          "Since attestation lag did not change, this accusation should be \
           injected and included without issue."
      in
      let* _op_hash = Operation.Anonymous.inject accusation client in
      let* () = bake_for client in
      let* ops =
        Node.RPC.call node
        @@ RPC.get_chain_block_operations_validation_pass
             ~block:"head"
             ~validation_pass:2
             ()
      in
      Check.(List.length (JSON.as_list ops) = 1)
        ~__LOC__
        Check.(int)
        ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
      unit)
    else
      let () =
        Log.info
          "Since attestation lag changed, this accusation refers to the wrong \
           commitment, hence injection should fail."
      in
      let* _op_hash =
        Operation.Anonymous.inject
          ~error:
            (if Protocol.number migrate_to >= 025 then
               let number_of_lags = List.length new_lags in
               Operation.dal_entrapment_invalid_lag_index
                 ~lag_index:(number_of_lags - 1)
                 ~max_bound:0
             else
               Operation.dal_entrapment_of_not_published_commitment migrate_to)
          accusation
          client
      in
      let* () = bake_for client in
      Log.info
        "Craft an entrapment evidence which uses the \"old\" attestation lag" ;
      let accusation =
        Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
          ~protocol:migrate_to
          ~attestation:attestation_old
          ~slot_index
          shard_denounced
          proof_denounced
      in
      Log.info
        "Injection of this new accusation should work fine and be included in \
         the next block" ;
      let* _op_hash = Operation.Anonymous.inject accusation client in
      let* () = bake_for client in
      let* ops =
        Node.RPC.call node
        @@ RPC.get_chain_block_operations_validation_pass
             ~block:"head"
             ~validation_pass:2
             ()
      in
      Check.(List.length (JSON.as_list ops) = 1)
        ~__LOC__
        Check.(int)
        ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
      unit
  in
  test_l1_migration_scenario
    ~__FILE__
    ~scenario
    ~tags
    ~description
    ~activation_timestamp:Now
    ~operator_profiles:[slot_index]
    ~traps_fraction:Q.one
    ~migration_level:10
    ~migrate_from
    ~migrate_to
    ()

(* This test sets up a migration and starts the DAL around the migration
   block. It mainly tests that the DAL node uses the right protocol plugins at
   migration.

   The [offset] says when to start the DAL node wrt to the migration level. It
   can be negative.

   If [check_rpc] is set, a call is made to [get_attestable_slots] on the
   migration level. This will fail if the encoding of the DAL committee has
   changed between protocols and the node does not use the right plugin. *)
let test_start_dal_node_around_migration ~__FILE__ ~migrate_from ~migrate_to
    ~offset ~check_rpc =
  Test.register
    ~__FILE__
    ~tags:
      [
        Tag.tezos2;
        "dal";
        Protocol.tag migrate_from;
        Protocol.tag migrate_to;
        "migration";
        "plugin";
      ]
    ~uses:[Constant.octez_dal_node]
    ~title:
      (sf
         "%s->%s: start the DAL node with offset of %d levels wrt to the \
          migration level"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         offset)
  @@ fun () ->
  (* be sure to use the same parameters in both protocols *)
  let* node, client, dal_parameters =
    setup_node ~parameter_overrides:[] ~protocol:migrate_from ()
  in

  (* The [+2] here is a bit arbitrary. We just want to avoid possible corner
     cases due to a too small migration level, which will not occur in
     practice. *)
  let migration_level = dal_parameters.attestation_lag + 2 in

  Log.info "Set user-activated-upgrade at level %d" migration_level ;
  let* () = Node.terminate node in
  let patch_config =
    Node.Config_file.update_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let nodes_args = Node.[Synchronisation_threshold 0; No_bootstrap_peers] in
  let* () = Node.run ~patch_config node nodes_args in
  let* () = Node.wait_for_ready node in

  let* dal_node =
    if offset >= 0 then (
      (* [bake_for ~count] does not work across the migration block *)
      let* () = bake_for ~count:(migration_level - 1) client in
      let* () = if offset > 0 then bake_for ~count:offset client else unit in

      Log.info "Start the DAL node" ;
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config dal_node in
      let* () = Dal_node.run dal_node ~wait_ready:true in
      return dal_node)
    else
      let* () = bake_for ~count:(migration_level - 1 + offset) client in

      Log.info "Start the DAL node" ;
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config dal_node in
      let* () = Dal_node.run dal_node ~wait_ready:true in

      (* that is, till the migration level *)
      let* () = bake_for ~count:(-offset) client in
      return dal_node
  in
  let* () =
    if check_rpc then
      let* _ =
        Dal_RPC.(
          call dal_node
          @@ get_attestable_slots
               ~attester:Constant.bootstrap1
               ~attested_level:migration_level)
      in
      unit
    else unit
  in
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node (migration_level + 2)
  in
  let* () = bake_for ~count:4 client in
  let* () = wait_for_dal_node in
  unit

let tests_start_dal_node_around_migration ~__FILE__ ~migrate_from ~migrate_to =
  let offsets = [-2; -1; 0; 1; 2] in
  let tests ~migrate_from ~migrate_to ~check_rpc =
    List.iter
      (fun offset ->
        test_start_dal_node_around_migration
          ~__FILE__
          ~migrate_from
          ~migrate_to
          ~offset
          ~check_rpc)
      offsets
  in
  tests ~migrate_from ~migrate_to ~check_rpc:true

let test_dal_node_snapshot_over_migration ~operators ~migration_level
    ~migrate_to parameters node client dal_node =
  let* () = Helpers.init_prover ~__LOC__ () in
  let* old_cryptobox =
    Helpers.make_cryptobox parameters.Dal_common.Parameters.cryptobox
  in
  (* Get the new protocol's DAL parameters and create a second cryptobox *)
  let base = Either.right (migrate_to, None) in
  let* new_proto_parameters = generate_protocol_parameters base migrate_to [] in
  let new_parameters =
    Dal.Parameters.from_protocol_parameters new_proto_parameters
  in
  let* new_cryptobox = Helpers.make_cryptobox new_parameters.cryptobox in
  let* () = bake_for client in
  let* start = Lwt.map succ (Node.get_level node) in
  let to_bake = 10 in
  let old_attestation_lag = parameters.Dal_common.Parameters.attestation_lag in
  let new_attestation_lag =
    new_parameters.Dal_common.Parameters.attestation_lag
  in
  let snapshot_window = start + to_bake + 2 in
  (* We add +2 because, DAL node deals with finalized blocks, which are
     described by the DAL node's block_hanadler as: A slot header is considered
     finalized when it is in a block with at least two other blocks on top of
     it, as guaranteed by Tenderbake. *)
  (* The snapshot export caps max_published_level at:
       last_processed_level - (validation_slack + head_attestation_lag + 1)
     where head_attestation_lag is the NEW protocol's attestation_lag.
     We also account for finality (DAL node processes blocks 2 behind head). *)
  let snapshot_exclusion_window =
    new_attestation_lag + Tezos_dal_node_lib.Constants.validation_slack
    + 2 (* finality: DAL node processes finalized blocks, 2 behind head *)
    + 1 (* the export's frozen level formula uses attestation_lag + slack + 1 *)
  in
  let stop = snapshot_window + snapshot_exclusion_window in
  let num_bakers = Array.length Account.Bootstrap.keys in
  let num_slots = List.length operators in
  let rec publish_loop acc level =
    if level > stop then return acc
    else
      let current_cryptobox =
        if level <= migration_level then old_cryptobox else new_cryptobox
      in
      let current_params =
        if level <= migration_level then parameters else new_parameters
      in
      let index =
        List.nth operators (slot_idx current_params level mod num_slots)
      in
      let source = Account.Bootstrap.keys.(level mod num_bakers) in
      let slot_content =
        Format.asprintf "content at level %d index %d" level index
      in
      let* predecessor = Node.wait_for_level node (pred level) in
      if predecessor >= level then
        Test.fail
          "Tried to publish a slot header at level %d but the predecessor \
           level is %d"
          level
          predecessor ;
      let* _op_hash =
        publish_dummy_slot
          ~source
          ~index
          ~message:slot_content
          current_cryptobox
          client
      in
      let slot_size = current_params.Dal.Parameters.cryptobox.slot_size in
      let* _commitment, _proof =
        Helpers.(
          store_slot dal_node ~slot_index:index
          @@ make_slot ~slot_size slot_content)
      in
      let* () =
        bake_for ~dal_node_endpoint:(Dal_node.rpc_endpoint dal_node) client
      and* _ = Node.wait_for_level node level
      and* () =
        if level > 3 then wait_for_layer1_final_block dal_node (level - 2)
        else unit
      in
      publish_loop ((level, index) :: acc) (level + 1)
  in
  let* published = publish_loop [] start in
  (* Select an arbitrary subset of slots to actually export. *)
  let slots_exported = [List.hd operators] in
  let expected ~level ~index =
    let is_index_ok i = List.mem i slots_exported in
    (* Level is ok when it fits the snapshot window and is not excluded by the
       attestation_lag window (in which slots are unattested). *)
    let is_level_ok i =
      (i < migration_level - old_attestation_lag || i > migration_level)
      && i >= start && i <= snapshot_window
    in
    is_index_ok index && is_level_ok level
  in
  let expected_exported_levels =
    List.filter (fun (level, index) -> expected ~level ~index) published
  in
  let expected_missing_levels =
    (* We exclude the [stop] level in which no slot was published*)
    List.filter
      (fun ((level, _) as e) ->
        (not (List.mem e expected_exported_levels))
        (* Remove all unattested_levels*)
        && level != stop
        && level != start
        && (level < migration_level - old_attestation_lag
           || level > migration_level))
      published
  in
  Log.info
    "Expected exported levels/slots: %a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%d,%d)" x y))
    expected_exported_levels ;
  Log.info
    "Expected missing levels/slots: %a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%d,%d)" x y))
    expected_missing_levels ;
  let min_published_level = Int32.of_int start in
  let max_published_level = Int32.of_int stop in
  let file = Temp.file "snapshot-over_migration" in
  let* () =
    Dal_node.snapshot_export
      ~min_published_level
      ~max_published_level
      ~slots:slots_exported
      ~endpoint:(Node.as_rpc_endpoint node)
      dal_node
      file
  in
  let fresh_dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config ~operator_profiles:operators fresh_dal_node in
  let* () =
    Dal_node.snapshot_import
      ~no_check:true (* Snapshot data validation is not implemented yet *)
      ~min_published_level
      ~max_published_level
      ~slots:slots_exported
      ~endpoint:(Node.as_rpc_endpoint node)
      fresh_dal_node
      file
  in
  let* () = Dal_node.run fresh_dal_node in
  (* Compare slot statuses between the original node and the fresh one built from snapshot. *)
  let* () =
    Log.info
      "Checking that the DAL node bootstrapped from snapshot has the expected \
       slot data..." ;
    Lwt_list.iter_s
      (fun (slot_level, slot_index) ->
        let* status_orig =
          Dal_RPC.(
            call dal_node @@ get_level_slot_status ~slot_level ~slot_index)
        in
        let* status_fresh =
          Dal_RPC.(
            call fresh_dal_node @@ get_level_slot_status ~slot_level ~slot_index)
        in
        let pp_status s = Format.asprintf "%a" Dal_RPC.pp_slot_id_status s in
        Check.(status_fresh = status_orig)
          ~__LOC__
          Dal.Check.slot_id_status_typ
          ~error_msg:
            (Format.sprintf
               "Snapshot import mismatch for slot (level=%d,index=%d): got %s, \
                expected %s"
               slot_level
               slot_index
               (pp_status status_fresh)
               (pp_status status_orig)) ;
        let* content_orig =
          Dal_RPC.(
            call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
          |> Lwt.map Helpers.content_of_slot
        in
        let* content_fresh =
          Dal_RPC.(
            call fresh_dal_node
            @@ get_level_slot_content ~slot_level ~slot_index)
          |> Lwt.map Helpers.content_of_slot
        in
        Check.(content_fresh = content_orig)
          ~__LOC__
          Check.string
          ~error_msg:
            (Format.sprintf
               "Snapshot import mismatch for slot (level=%d,index=%d)"
               slot_level
               slot_index) ;
        unit)
      expected_exported_levels
  in
  let unexpected_success = Failure "Should not succeed" in
  let* () =
    Log.info
      "Checking that the DAL node bootstrapped from snapshot lacks the \
       expected slot data..." ;
    Lwt_list.iter_s
      (fun (slot_level, slot_index) ->
        (* Check that the source node has data *)
        let* _ =
          Dal_RPC.(
            call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
        in
        (* Check that the node bootstrapped from snapshot does not have data *)
        let* () =
          Lwt.catch
            (fun () ->
              let* _ =
                Dal_RPC.(
                  call fresh_dal_node
                  @@ get_level_slot_content ~slot_level ~slot_index)
              in
              Lwt.fail unexpected_success)
            (fun e ->
              if e = unexpected_success then
                Test.fail
                  "After snapshot import: expected failure for \
                   /levels/%d/slot/%d/content"
                  slot_level
                  slot_index
              else Lwt.return_unit)
        in
        unit)
      expected_missing_levels
  in
  Dal_node.terminate fresh_dal_node

let test_snapshot_export_over_migration ~__FILE__ ~migrate_from ~migrate_to =
  let operators = [0; 3] in
  test_l1_migration_scenario
    ~__FILE__
    ~tags:[Tag.slow]
    ~description:"snapshot over migration"
    ~migrate_from
    ~migrate_to
    ~migration_level:10
    ~operator_profiles:operators
    ~scenario:(fun ~migration_level dal_params client node dal_node ->
      test_dal_node_snapshot_over_migration
        ~operators
        ~migration_level
        ~migrate_to
        dal_params
        node
        client
        dal_node)
    ()

(* Test that a DAL node that is stopped before a migration and restarted
   after it correctly catches up through the migration boundary. *)
let test_restart_dal_node_across_migration ~__FILE__ ~migrate_from ~migrate_to =
  let slot_index = 0 in
  let scenario ~migration_level dal_parameters client _node dal_node =
    let lag = dal_parameters.Dal.Parameters.attestation_lag in
    let slot_size = dal_parameters.cryptobox.slot_size in
    let* _commitment =
      Helpers.publish_and_store_slot
        client
        dal_node
        Constant.bootstrap1
        ~index:slot_index
      @@ Helpers.make_slot ~slot_size "slot before migration"
    in
    let* () = bake_for client in
    let* published_level = Client.level client in
    Log.info "Published slot at level %d" published_level ;
    let* () = bake_for ~count:2 client in
    Log.info "Stopping DAL node" ;
    let* () = Dal_node.terminate dal_node in
    let* current_level = Client.level client in
    let target_level = migration_level + lag + 3 in
    let* () =
      repeat (target_level - current_level) (fun () -> bake_for client)
    in
    Log.info "Baked through migration to level %d" target_level ;
    Log.info "Restarting DAL node" ;
    let* () = Dal_node.run dal_node ~wait_ready:true in
    let* () = bake_until_processed ~level:target_level client [dal_node] in
    Log.info "DAL node caught up" ;
    (* Querying a post-migration level exercises the full catch-up path:
       plugin resolution, parameter fetching, committee lookup, and store. *)
    let* _ =
      Dal_RPC.(
        call dal_node
        @@ get_attestable_slots
             ~attester:Constant.bootstrap1
             ~attested_level:(migration_level + lag))
    in
    Log.info "Post-migration attestable_slots RPC succeeded" ;
    unit
  in
  test_l1_migration_scenario
    ~__FILE__
    ~migrate_from
    ~migrate_to
    ~migration_level:10
    ~scenario
    ~tags:["restart"; "dal"; "migration"]
    ~description:"restart DAL node across migration"
    ~operator_profiles:[slot_index]
    ()

let test_traps_fraction_uses_published_level ~__FILE__ ~migrate_from ~migrate_to
    =
  (* Fail early if Mainnet traps_fraction differs between the two protocols.
     If this triggers, the trap count assertions below may need updating. *)
  let get_mainnet_traps_fraction protocol =
    let params =
      JSON.parse_file
        (Protocol.parameter_file ~constants:Constants_mainnet protocol)
    in
    let tf = JSON.(params |-> "dal_parametric" |-> "traps_fraction") in
    let num = JSON.(tf |-> "numerator" |> as_string) in
    let den = JSON.(tf |-> "denominator" |> as_string) in
    sf "%s/%s" num den
  in
  let from_tf = get_mainnet_traps_fraction migrate_from in
  let to_tf = get_mainnet_traps_fraction migrate_to in
  Check.(
    (from_tf = to_tf)
      string
      ~__LOC__
      ~error_msg:
        "Mainnet traps_fraction changed across migration (%L != %R): update \
         this test") ;
  let slot_index = 0 in
  let scenario ~migration_level dal_parameters client _node dal_node =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let pkh = Constant.bootstrap1.public_key_hash in
    (* Publish a slot before migration *)
    let* current_level = Client.level client in
    let* () =
      repeat (migration_level - 2 - current_level) (fun () -> bake_for client)
    in
    let* _commitment =
      Helpers.publish_and_store_slot
        client
        dal_node
        Constant.bootstrap1
        ~index:slot_index
        ~force:true
      @@ Helpers.make_slot ~slot_size "pre-migration"
    in
    let* () = bake_for client in
    let* pre_published_level = Client.level client in
    Log.info "Published pre-migration slot at level %d" pre_published_level ;
    (* Bake through migration *)
    let* current_level = Client.level client in
    let* () =
      repeat (migration_level + 3 - current_level) (fun () -> bake_for client)
    in
    Log.info "Migrated at level %d" migration_level ;
    (* Publish a slot after migration *)
    let* _commitment =
      Helpers.publish_and_store_slot
        client
        dal_node
        Constant.bootstrap1
        ~index:slot_index
        ~force:true
      @@ Helpers.make_slot ~slot_size "post-migration"
    in
    let* () = bake_for client in
    let* post_published_level = Client.level client in
    Log.info "Published post-migration slot at level %d" post_published_level ;
    (* Bake until the post-migration slot is finalized *)
    let* () =
      bake_until_processed ~level:post_published_level client [dal_node]
    in
    let* pre_traps =
      Dal_RPC.(
        call dal_node
        @@ get_published_level_known_traps
             ~published_level:pre_published_level
             ~pkh
             ~slot_index)
    in
    Check.(
      (List.length pre_traps = 0)
        int
        ~__LOC__
        ~error_msg:
          "Pre-migration slot should have 0 traps (traps_fraction was 0), got \
           %L") ;
    let* post_traps =
      Dal_RPC.(
        call dal_node
        @@ get_published_level_known_traps
             ~published_level:post_published_level
             ~pkh
             ~slot_index)
    in
    (* Post-migration traps_fraction is also 0 because the stitching code
       preserves the value from the previous protocol. With traps_fraction = 0,
       no shard can be a trap. If the stitching code or Mainnet constants change,
       this assertion may fail, signaling that the test needs updating. *)
    Check.(
      (List.length post_traps = 0)
        int
        ~__LOC__
        ~error_msg:
          "Post-migration slot should have 0 traps (traps_fraction preserved \
           as 0 across migration), got %L") ;
    unit
  in
  test_l1_migration_scenario
    ~__FILE__
    ~migrate_from
    ~migrate_to
    ~migration_level:13
    ~scenario
    ~tags:["traps_fraction"]
    ~description:
      "test traps_fraction uses published level value across migration"
    ~traps_fraction:Q.zero
    ~operator_profiles:[slot_index]
    ()

let register_migration ~__FILE__ ~migrate_from ~migrate_to =
  test_migration_plugin ~__FILE__ ~migration_level:11 ~migrate_from ~migrate_to ;
  tests_start_dal_node_around_migration ~__FILE__ ~migrate_from ~migrate_to ;
  test_restart_dal_node_across_migration ~__FILE__ ~migrate_from ~migrate_to ;
  test_migration_accuser_issue
    ~__FILE__
    ~migration_level:4
    ~migrate_from
    ~migrate_to ;
  test_traps_fraction_uses_published_level ~__FILE__ ~migrate_from ~migrate_to ;
  test_migration_with_attestation_lag_change ~__FILE__ ~migrate_from ~migrate_to ;
  test_accusation_migration_with_attestation_lag_decrease
    ~__FILE__
    ~migrate_from
    ~migrate_to ;
  test_migration_with_rollup ~__FILE__ ~migrate_from ~migrate_to ;
  test_refutation_with_dal_page_import_across_migration
    ~__FILE__
    ~migrate_from
    ~migrate_to ;
  test_skip_list_store_with_migration
    ~__FILE__
    ~migration_level:11
    ~migrate_from
    ~migrate_to ;
  test_snapshot_export_over_migration ~__FILE__ ~migrate_from ~migrate_to
