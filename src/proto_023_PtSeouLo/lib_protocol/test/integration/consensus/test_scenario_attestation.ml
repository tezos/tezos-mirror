(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol ((pre)attestations)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_scenario_attestation.ml

    Subject:      Test various scenarios with attestations, preattestations, aggregation, DAL bitset,
                  consensus and companion keys, forbidden and inactive delegates, and metadata.
*)

open Scenario

let check_delegate_attested ~check_not_found ~kind delegate =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let delegate = State.find_account delegate state in
      let* consensus_key_info =
        Context.Delegate.consensus_key (B state.grandparent) delegate.pkh
      in
      let consensus_key = consensus_key_info.active in
      let* consensus_key = Account.find consensus_key.consensus_key_pkh in
      let metadata = Stdlib.Option.get state.previous_metadata in
      check_attestation_metadata
        ~check_not_found
        ~kind
        delegate.pkh
        consensus_key.pkh
        metadata
        (block, state))

let check_delegate_didnt_preattest delegate =
  check_delegate_attested ~check_not_found:true ~kind:Preattestation delegate

let check_delegate_didnt_attest delegate =
  check_delegate_attested ~check_not_found:true ~kind:Attestation delegate

let check_delegate_preattested delegate =
  check_delegate_attested ~check_not_found:false ~kind:Preattestation delegate

let check_delegate_attested delegate =
  check_delegate_attested ~check_not_found:false ~kind:Attestation delegate

let check_aggregated_committee ~check_not_found ~kind delegates =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let* delegates =
        List.map_es
          (fun delegate_name ->
            let delegate = State.find_account delegate_name state in
            let* consensus_key_info =
              Context.Delegate.consensus_key (B state.grandparent) delegate.pkh
            in
            let consensus_key = consensus_key_info.active in
            let* consensus_key = Account.find consensus_key.consensus_key_pkh in
            return (delegate.pkh, consensus_key.pkh))
          delegates
      in
      let metadata = Stdlib.Option.get state.previous_metadata in
      check_attestation_aggregate_metadata
        ~check_not_found
        ~kind
        delegates
        metadata
        (block, state))

let check_aggregated_wrong_committee =
  check_aggregated_committee ~check_not_found:true

let check_aggregated_committee =
  check_aggregated_committee ~check_not_found:false

(* === Simple tests ===  *)

let test_attest_simple =
  init_constants ()
  --> begin_test ["delegate"; "dummy"] ~force_attest_all:false
  --> next_block --> attest_with "delegate" --> next_block
  (* Sanity checks. The positive checks are done every time there is an attestation
     or a preattestation. *)
  --> check_delegate_attested "delegate"
  --> check_delegate_didnt_attest "dummy"

let test_preattest_simple =
  init_constants ()
  --> begin_test ["delegate"] ~force_preattest_all:false
  --> set_baked_round 1 --> next_block
  --> finalize_payload ~payload_round:0 ()
  --> preattest_with "delegate" --> finalize_block
  (* Sanity checks *)
  --> check_delegate_preattested "delegate"
  --> check_delegate_didnt_preattest "dummy"

let test_preattest_less_simple =
  init_constants ()
  --> begin_test ["delegate1"; "delegate2"] ~force_preattest_all:false
  --> set_baked_round 1 --> next_block --> start_payload
  --> transfer "delegate1" "delegate2" (Amount (Tez_helpers.of_mutez 100L))
  --> transfer "delegate2" "delegate1" (Amount (Tez_helpers.of_mutez 99L))
  --> finalize_payload ~payload_round:0 ()
  --> preattest_with "delegate1" --> preattest_with "delegate2"
  --> finalize_block
  (* Sanity checks *)
  --> check_delegate_preattested "delegate1"
  --> check_delegate_preattested "delegate2"

let test_attest_all =
  init_constants ()
  --> begin_test ["delegate1"; "delegate2"] ~force_attest_all:true
  --> next_block (* This block does not contain attestations; check next. *)
  --> next_block
  (* Sanity checks *)
  --> check_delegate_attested "delegate1"
  --> check_delegate_attested "delegate2"
  --> check_aggregated_wrong_committee
        ~kind:Attestation
        ["delegate1"; "delegate2"]

let test_preattest_all =
  init_constants ()
  --> begin_test ["delegate1"; "delegate2"] ~force_preattest_all:true
  --> set_baked_round ~payload_round:0 1
  --> next_block
  (* Sanity checks *)
  --> check_delegate_preattested "delegate1"
  --> check_delegate_preattested "delegate2"
  --> check_aggregated_wrong_committee
        ~kind:Preattestation
        ["delegate1"; "delegate2"]

let test_attest_aggreg =
  init_constants ()
  --> begin_test ["delegate1"; "delegate2"] ~algo:Bls ~force_attest_all:false
  --> next_block
  --> attest_aggreg_with ["delegate1"; "delegate2"]
  --> next_block
  (* Sanity checks. Aggregated committees are always handled separately. *)
  --> check_aggregated_committee ~kind:Attestation ["delegate1"; "delegate2"]
  (* Check that bls attestations cannot be found alone, i.e. non aggregated. *)
  --> check_delegate_didnt_attest "delegate1"
  --> check_delegate_didnt_attest "delegate2"

let test_preattest_aggreg =
  init_constants ()
  --> begin_test ["delegate1"; "delegate2"] ~algo:Bls ~force_preattest_all:false
  --> set_baked_round 1 --> next_block
  --> finalize_payload ~payload_round:0 ()
  --> preattest_aggreg_with ["delegate1"; "delegate2"]
  --> finalize_block
  (* Sanity checks *)
  --> check_aggregated_committee ~kind:Preattestation ["delegate1"; "delegate2"]
  (* Check that bls attestations cannot be found alone, i.e. non aggregated. *)
  --> check_delegate_didnt_preattest "delegate1"
  --> check_delegate_didnt_preattest "delegate2"

(* === Reward tests === *)

let init_constants_for_attestation_rewards =
  init_constants ()
  --> set
        S.issuance_weights
        {
          base_total_issued_per_minute = Tez_helpers.of_mutez 1_000_000_007L;
          baking_reward_fixed_portion_weight = 0;
          baking_reward_bonus_weight = 0;
          attesting_reward_weight = 1;
          seed_nonce_revelation_tip_weight = 0;
          vdf_revelation_tip_weight = 0;
          dal_rewards_weight = 0;
        }

let test_attestation_rewards =
  init_constants_for_attestation_rewards
  (* Default checks are disabled because rewards have been changed *)
  --> (Tag "not tz4"
       --> begin_test
             ["delegate"]
             ~disable_default_checks:true
             ~force_attest_all:true
      |+ Tag "tz4 (solo)"
         --> begin_test
               ["delegate"]
               ~algo:Bls
               ~disable_default_checks:true
               ~force_attest_all:true
      |+ Tag "tz4 (with others)"
         --> begin_test
               ["delegate"; "bozo1"; "bozo2"]
               ~algo:Bls
               ~disable_default_checks:true
               ~force_attest_all:true)
  --> dawn_of_next_cycle
  --> exec_metadata (check_attestation_rewards "delegate")
  --> exec_metadata
        (check_missed_attestation_rewards ~check_not_found:true "delegate")

let test_missed_attestations_rewards =
  init_constants_for_attestation_rewards
  (* Default checks are disabled because rewards have been changed *)
  --> begin_test ["delegate"] ~disable_default_checks:true
  --> snapshot_balances "init" ["delegate"]
  --> next_block
  --> (Tag "attest once" --> attest_with "delegate" |+ Tag "no attest" --> noop)
  --> dawn_of_next_cycle
  --> exec_metadata (check_missed_attestation_rewards "delegate")
  (* Check balance of "delegate" hasn't changed *)
  --> exec_metadata (check_attestation_rewards ~check_not_found:true "delegate")

let test_missed_attestations_rewards_tz4 =
  init_constants_for_attestation_rewards
  (* Default checks are disabled because rewards have been changed *)
  --> begin_test
        ["delegate"; "bozo1"; "bozo2"]
        ~algo:Bls
        ~disable_default_checks:true
  --> snapshot_balances "init" ["delegate"]
  --> next_block
  --> (Tag "attest once (solo)" --> attest_aggreg_with ["delegate"]
      |+ Tag "attest once (with others)"
         --> attest_aggreg_with ["delegate"; "bozo1"; "bozo2"]
      |+ Tag "no attest" --> noop)
  --> dawn_of_next_cycle
  --> exec_metadata (check_missed_attestation_rewards "delegate")
  (* Check balance of "delegate" hasn't changed *)
  --> exec_metadata (check_attestation_rewards ~check_not_found:true "delegate")

(* === Forbidden tests === *)

let test_forbidden_delegate_tries_to_attest_but_fails_miserably =
  let expected_error (_, state) errs =
    let delegate = State.find_account "delegate" state in
    Error_helpers.expect_forbidden_delegate
      ~loc:__LOC__
      ~delegate:delegate.contract
      errs
  in
  init_constants ()
  --> begin_test
        ["delegate"; "baker"]
        ~force_preattest_all:false
        ~force_attest_all:false
  --> set_baker ~min_round:1 "baker"
  --> double_attest "delegate" --> make_denunciations () --> next_block
  (* Cannot bake *)
  --> assert_failure ~expected_error (next_block_with_baker "delegate")
  (* Cannot preattest *)
  --> assert_failure
        ~expected_error
        (finalize_payload ~payload_round:0 ()
        --> preattest_with ~payload_round:0 "delegate")
  (* Cannot attest *)
  --> assert_failure ~expected_error (attest_with "delegate" --> next_block)

let test_forbidden_delegate_tries_to_attest_but_fails_miserably_tz4_edition =
  let expected_error (_, state) errs =
    let delegate = State.find_account "delegate" state in
    Error_helpers.expect_forbidden_delegate
      ~loc:__LOC__
      ~delegate:delegate.contract
      errs
  in
  init_constants ()
  --> begin_test
        ["delegate"; "baker"; "attester"]
        ~algo:Bls
        ~force_preattest_all:false
        ~force_attest_all:false
  --> set_baker ~min_round:1 "baker"
  --> double_attest "delegate" --> make_denunciations () --> next_block
  (* Cannot bake *)
  --> assert_failure ~expected_error (next_block_with_baker "delegate")
  (* Cannot preattest *)
  --> assert_failure
        ~expected_error
        (finalize_payload ~payload_round:0 ()
        --> preattest_aggreg_with ~payload_round:0 ["delegate"])
  --> assert_failure
        ~expected_error
        (finalize_payload ~payload_round:0 ()
        --> preattest_aggreg_with ~payload_round:0 ["delegate"; "attester"])
  (* Cannot attest *)
  --> assert_failure
        ~expected_error
        (attest_aggreg_with ["delegate"] --> next_block)
  --> assert_failure
        ~expected_error
        (attest_aggreg_with ["delegate"; "attester"] --> next_block)

let tests =
  tests_of_scenarios
  @@ [
       ("Test simple attestation", test_attest_simple);
       ("Test simple preattestation", test_preattest_less_simple);
       ("Test less simple preattestation", test_preattest_less_simple);
       ("Test attest all", test_attest_all);
       ("Test preattest all", test_preattest_all);
       ("Test attest aggreg", test_attest_aggreg);
       ("Test preattest aggreg", test_preattest_aggreg);
       ("Test attestation rewards", test_attestation_rewards);
       ("Test missed attestation rewards", test_missed_attestations_rewards);
       ( "Test missed attestation rewards (tz4)",
         test_missed_attestations_rewards_tz4 );
       ( "Test forbidden delegate cannot attest",
         test_forbidden_delegate_tries_to_attest_but_fails_miserably );
       ( "Test forbidden delegate cannot attest (tz4)",
         test_forbidden_delegate_tries_to_attest_but_fails_miserably_tz4_edition
       );
     ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "consensus"] tests
