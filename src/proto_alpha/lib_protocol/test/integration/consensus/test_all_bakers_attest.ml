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
                  -- --file test_all_bakers_attest.ml

    Subject:      Test all bakers attest feature and activation.
*)

open Scenario

(* === Observables === *)
let check_tz4_baker_number_ratio ~loc expected_values =
  let open Lwt_result_syntax in
  exec_unit (fun (block, _state) ->
      Log.info
        ~color:Log_helpers.check_color
        "Checking tz4 ratio, expecting the following values: [%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           Format.pp_print_string)
        expected_values ;
      let current_cycle = Block.current_cycle block in
      List.iteri_es
        (fun offset expected ->
          let cycle = Protocol.Alpha_context.Cycle.add current_cycle offset in
          let* rpc_ratio =
            Plugin.RPC.tz4_baker_number_ratio Block.rpc_ctxt ~cycle block
          in
          Assert.equal
            ~loc
            String.equal
            (Format.asprintf
               "Unexpected tz4 baker ratio at cycle %a"
               Protocol.Alpha_context.Cycle.pp
               cycle)
            Format.pp_print_string
            expected
            rpc_ratio)
        expected_values)

let check_abaab_activation_level ~loc expected_level =
  let open Lwt_result_wrap_syntax in
  exec_unit (fun (block, _state) ->
      Log.info
        ~color:Log_helpers.check_color
        "Checking activation level for All Bakers Attest, expecting %s"
        (match expected_level with
        | None -> "none"
        | Some level -> Format.asprintf "level %ld" level) ;
      let* ctxt = Context.get_alpha_ctxt (B block) in
      let (ctxt : Protocol.Raw_context.t) =
        Protocol.Alpha_context.Internal_for_tests.to_raw
          (ctxt : Protocol.Alpha_context.t)
      in
      let*@ activation_level =
        Protocol.Storage.All_bakers_attest_activation.find ctxt
      in
      match (activation_level, expected_level) with
      | None, None -> return_unit
      | Some level, None ->
          failwith
            "Activation level is set to %a, expected None"
            Protocol.Level_repr.pp
            level
      | None, Some level ->
          failwith "Activation level is not set, expected level %ld" level
      | Some rpc_level, Some expected ->
          let rpc_level =
            Protocol.Raw_level_repr.to_int32 rpc_level.Protocol.Level_repr.level
          in
          Assert.equal_int32 ~loc expected rpc_level)

let check_all_bakers_attested ~kind =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let get_active_bakers block =
        (* gets the stake info of the block's cycle. It includes all active
           bakers with a stake. *)
        let* _, bakers =
          Plugin.RPC.baking_power_distribution_for_current_cycle
            Block.rpc_ctxt
            block
        in
        return (List.map fst bakers)
      in
      let* active_bakers =
        match kind with
        | Attestation ->
            if Block.last_block_of_cycle state.State.grandparent then
              get_active_bakers state.State.grandgrandparent
            else get_active_bakers state.State.grandparent
        | Preattestation ->
            if Block.last_block_of_cycle block then
              get_active_bakers state.State.grandparent
            else get_active_bakers block
      in
      Log.info
        ~color:Log_helpers.check_color
        "Checking all bakers %s. Expected committee: %a"
        (match kind with
        | Attestation -> "attested the previous block"
        | Preattestation -> "preattested the current block")
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           Format.pp_print_string)
        (List.map
           (fun (x : Protocol.Alpha_context.Consensus_key.t) ->
             fst (State.find_account_from_pkh x.delegate state))
           active_bakers) ;
      let active_bakers_tz4, active_bakers_non_tz4 =
        List.partition
          (fun (x : Protocol.Alpha_context.Consensus_key.t) ->
            match x.consensus_pkh with Bls _ -> true | _ -> false)
          active_bakers
      in
      let* () =
        List.iter_es
          (fun (x : Protocol.Alpha_context.Consensus_key.t) ->
            check_attestation_metadata
              ~kind
              x.delegate
              x.consensus_pkh
              (Stdlib.Option.get state.State.previous_metadata)
              (block, state))
          active_bakers_non_tz4
      in
      check_attestation_aggregate_metadata
        ~kind
        active_bakers_tz4
        (Stdlib.Option.get state.State.previous_metadata)
        (block, state))

let check_all_bakers_preattested =
  check_all_bakers_attested ~kind:Preattestation

let check_all_bakers_attested = check_all_bakers_attested ~kind:Attestation

(* === Tests === *)

(* The activation level should only be set if more than half of the registered bakers use
   BLS to sign. There are four ways to trigger this:
       - an existing registered non-tz4 baker registers a tz4 consensus key
       - a tz4 account starts baking
       - an existing registered non-tz4 baker gets deactivated
       - an existing registered non-tz4 bakers unstakes and gets removed from the consensus.
     The storage update is done at the end of the cycle.
     If a new tz4 starts baking, but then a tz4 also stops baking (different block but same cycle),
   then the feature does not activate.
     On the activation cycle, there is at least half of the bakers that *actively* sign with a tz4.
   On the previous cycle, there is strictly less. In other words, even if there is a delay
   to activate a tz4, there is no further delay to activate the feature.
     What should be observed is that small bakers can attest after the feature,
   but not (or rarely) before
     The deactivation case is done in [test_activation_threshold_with_deactivated_baker]
*)
let test_activation_threshold =
  let all_bakers_attest_activation_threshold =
    Default_parameters.constants_mainnet.all_bakers_attest_activation_threshold
  in
  let big = 800_000_000_000_000L in
  (* 800M tez *)
  let small =
    Tez_helpers.to_mutez Default_parameters.constants_mainnet.minimal_stake
    |> Int64.mul 2L
    (* 12000 tez (2 rolls) *)
  in
  init_constants ()
  (* Ensures to set value to mainnet default, regardless of [init_constants] default *)
  --> set
        S.all_bakers_attest_activation_threshold
        all_bakers_attest_activation_threshold
  (* set consensus_committee_size very low to make small bakers very unlikely
     to have any slots before abaab, making observation easier.
     It should still be higher than the number of DAL slots. *)
  --> set S.consensus_committee_size 20
  (* this requires the number of DAL shards to be reduced *)
  --> set S.Dal.Cryptobox_parameters.number_of_shards 10
  (* set threshold size to 0, making attestations not mandatory *)
  --> set S.consensus_threshold_size 0
  (* set a reasonable, human friendly number of blocks per cycle *)
  --> set S.blocks_per_cycle 10l
  (* set inactivity threshold to very high values, so that
     they never get deactivated *)
  --> set S.tolerated_inactivity_period 99
  (* Begin test: setup different profiles
     7 bakers, 4 non-BLS, 3 BLS, threshold is not reached yet. *)
  --> begin_test
        ~force_attest_all:true
        ~bootstrap_info_list:
          [
            (* bakers *)
            make "baker_big" ~algo:Not_Bls ~balance:big;
            make "baker_small_1" ~algo:Not_Bls ~balance:small;
            make "baker_small_2" ~algo:Not_Bls ~balance:small;
            make "tz4_baker_big" ~algo:Bls ~balance:big;
            make "tz4_baker_small" ~algo:Bls ~balance:small;
            make
              "non_tz4_with_tz4_ck_baker_big"
              ~algo:Not_Bls
              ~consensus_key:(Some Bls)
              ~balance:big;
            make
              "tz4_with_non_tz4_ck_baker_big"
              ~algo:Bls
              ~consensus_key:(Some Not_Bls)
              ~balance:big;
            (* delegators *)
            make
              "tz4_delegator_big"
              ~algo:Bls
              ~balance:big
              ~delegate:"non_tz4_with_tz4_ck_baker_big";
            make
              "tz4_delegator_small"
              ~algo:Bls
              ~balance:small
              ~delegate:"baker_big";
          ]
        []
  (* 3/7 = 42.8571%. We can check for the current cycle and the next two
     (because consensus_rights_delay = 2).
     Note: the RPC doesn't round to the nearest, it's always flooring.
  *)
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["42.85%"; "42.85%"; "42.85%"]
  (* And the activation level is not set yet *)
  --> check_abaab_activation_level ~loc:__LOC__ None
  (* Check also at next cycle *)
  --> next_cycle (* Cycle 1, level pos 0: level 10 *)
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["42.85%"; "42.85%"; "42.85%"]
  --> check_abaab_activation_level ~loc:__LOC__ None
  (* Check that not all bakers attest *)
  (* Delegators overdelegate, so they don't count.
     Total stake = N = 2_400_036_000 tez * 11/20
     Small staker has a stake of 12_000 tez * 11/20.
     Probability that a given small staker has a slot (out of the 20 available slots):
         1 - (1 - 12000/N) ^ 20 = p
     Probability that all three small bakers have a slot for the given block at the same
     time: p^3 = 10^-12 *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_attested
  --> (Tag "add and remove tz4"
       (* Case where the feature does not activate: we add a tz4, then remove a tz4 in
          the same cycle. The feature does not activate. *)
       --> add_account ~algo:Bls "consensus_tz4"
       --> add_account ~algo:P256 "consensus_non_tz4"
       (* First, add a tz4 consensus key to a non tz4 baker *)
       --> update_consensus_key ~ck_name:"consensus_tz4" "baker_big"
       (* Bake a block, bide your time *)
       --> next_block
       (* Then, add a non-tz4 consensus key to a tz4 baker *)
       --> update_consensus_key ~ck_name:"consensus_non_tz4" "tz4_baker_small"
       (* Check RPCs *)
       --> check_tz4_baker_number_ratio
             ~loc:__LOC__
             ["42.85%"; "42.85%"; "42.85%"]
       --> check_abaab_activation_level ~loc:__LOC__ None
       (* Check RPCs next cycle too *)
       --> next_cycle
       --> check_tz4_baker_number_ratio
             ~loc:__LOC__
             ["42.85%"; "42.85%"; "42.85%"]
       --> check_abaab_activation_level ~loc:__LOC__ None
       (* Below are cases where the feature activates *)
      |+ (Tag "new tz4 baker"
          (* We register a tz4 delegator as a delegate. This should put the ratio
             to 50%, which should be enough to trigger the feature *)
          --> set_delegate "tz4_delegator_small" (Some "tz4_delegator_small")
          --> stake
                "tz4_delegator_small"
                (Amount (Tez_helpers.of_mutez 10_000_000_000L))
          (* Nothing changed yet *)
          --> check_tz4_baker_number_ratio
                ~loc:__LOC__
                ["42.85%"; "42.85%"; "42.85%"]
          --> check_abaab_activation_level ~loc:__LOC__ None
          (* next cycle *)
          --> next_cycle (* Cycle 2, level pos 0: level 20 *)
          (* The distribution for cycle 4 should have been done now, so we should see an activation
             at the beginning of cycle 4.
             4 tz4 bakers out of 8 total bakers: 50% *)
          --> check_tz4_baker_number_ratio
                ~loc:__LOC__
                ["42.85%"; "42.85%"; "50.00%"]
          --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
          (* Wait until activation *)
          --> next_cycle
          --> check_tz4_baker_number_ratio
                ~loc:__LOC__
                ["42.85%"; "50.00%"; "50.00%"]
          --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
          --> next_cycle
          --> check_tz4_baker_number_ratio
                ~loc:__LOC__
                ["50.00%"; "50.00%"; "50.00%"]
          --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
         |+ Tag "new tz4 consensus key"
            --> add_account ~algo:Bls "consensus_tz4"
            --> update_consensus_key ~ck_name:"consensus_tz4" "baker_small_1"
            (* Nothing changed yet *)
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["42.85%"; "42.85%"; "42.85%"]
            --> check_abaab_activation_level ~loc:__LOC__ None
            (* next cycle *)
            --> next_cycle (* Cycle 2, level pos 0: level 20 *)
            (* The distribution for cycle 4 should have been done now, so we should see an activation
               at the beginning of cycle 4.
               4 tz4 bakers out of 7 total bakers: 57.1428% *)
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["42.85%"; "42.85%"; "57.14%"]
            --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
            --> (no_tag
                 (* Wait until activation *)
                 --> next_cycle
                 --> check_tz4_baker_number_ratio
                       ~loc:__LOC__
                       ["42.85%"; "57.14%"; "57.14%"]
                 --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
                 --> next_cycle
                 --> check_tz4_baker_number_ratio
                       ~loc:__LOC__
                       ["57.14%"; "57.14%"; "57.14%"]
                 --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
                |+ Tag "go below threshold but still activate"
                   --> add_account ~algo:P256 "consensus_non_tz4"
                   --> update_consensus_key
                         ~ck_name:"consensus_non_tz4"
                         "tz4_baker_big"
                   --> next_cycle
                   --> check_tz4_baker_number_ratio
                         ~loc:__LOC__
                         ["42.85%"; "57.14%"; "42.85%"]
                   (* We still activate *)
                   --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
                   --> next_cycle
                   --> check_tz4_baker_number_ratio
                         ~loc:__LOC__
                         ["57.14%"; "42.85%"; "42.85%"]
                   (* We still activate *)
                   --> check_abaab_activation_level ~loc:__LOC__ (Some 40l))
         |+ Tag "non-tz4 baker unstake all"
            --> unstake "baker_small_2" All
            (* Nothing changed yet *)
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["42.85%"; "42.85%"; "42.85%"]
            --> check_abaab_activation_level ~loc:__LOC__ None
            (* next cycle *)
            --> next_cycle (* Cycle 2, level pos 0: level 20 *)
            (* The distribution for cycle 4 should have been done now, so we should see an activation
               at the beginning of cycle 4.
               3 tz4 bakers out of 6 total bakers: 50% *)
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["42.85%"; "42.85%"; "50.00%"]
            --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
            (* Wait until activation *)
            --> next_cycle
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["42.85%"; "50.00%"; "50.00%"]
            --> check_abaab_activation_level ~loc:__LOC__ (Some 40l)
            --> next_cycle
            --> check_tz4_baker_number_ratio
                  ~loc:__LOC__
                  ["50.00%"; "50.00%"; "50.00%"]
            --> check_abaab_activation_level ~loc:__LOC__ (Some 40l))
         -->
         (* The feature *just* activated. The last baked block can be attested by all
            bakers already. *)
         force_attest_all true --> force_preattest_all true
         --> set_baked_round ~payload_round:0 1
         --> next_block --> check_all_bakers_attested
         --> check_all_bakers_preattested
         --> exec bake_until_dawn_of_next_cycle
         --> check_all_bakers_attested --> check_all_bakers_preattested
         --> next_block --> check_all_bakers_attested
         --> check_all_bakers_preattested)

let test_activation_threshold_with_deactivated_baker =
  let all_bakers_attest_activation_threshold =
    Default_parameters.constants_mainnet.all_bakers_attest_activation_threshold
  in
  let balance = 4_000_000_000_000L in
  (* 4M tez *)
  init_constants ()
  (* Ensures to set value to mainnet default, regardless of [init_constants] default *)
  --> set
        S.all_bakers_attest_activation_threshold
        all_bakers_attest_activation_threshold
  (* set consensus_committee_size to the default *)
  --> set S.consensus_committee_size 7000
  (* set threshold size to 0, making attestations not mandatory *)
  --> set S.consensus_threshold_size 0
  (* set a reasonable, human friendly number of blocks per cycle  *)
  --> set S.blocks_per_cycle 10l
  (* set inactivity threshold very low to wait for a baker to be deactivated *)
  --> set S.tolerated_inactivity_period 1
  (* Begin test: setup bootstrap accounts
     3 bakers, 2 non-BLS, 1 BLS, threshold is not reached yet. *)
  --> begin_test
        ~force_attest_all:true
        ~bootstrap_info_list:
          [
            (* bakers *)
            make "baker_1" ~algo:Not_Bls ~balance;
            make "baker_2" ~algo:Not_Bls ~balance;
            make "tz4_baker" ~algo:Bls ~balance;
          ]
        []
  (* 1/3 = 33.3333%. We can check for the current cycle and the next two
     (because consensus_rights_delay = 2).
     Note: the RPC doesn't round to the nearest, it's always flooring.
  *)
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["33.33%"; "33.33%"; "33.33%"]
  (* And the activation level is not set yet *)
  --> check_abaab_activation_level ~loc:__LOC__ None
  (* Check also at next cycle *)
  --> next_cycle (* Cycle 1, level pos 0: level 10 *)
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["33.33%"; "33.33%"; "33.33%"]
  --> check_abaab_activation_level ~loc:__LOC__ None
  (* make "baker_2" inactive. We have to wait for next cycle because
     the baker might have attested the first block of the current cycle. *)
  --> dawn_of_next_cycle (* Cycle 1, level pos 9: level 19 *)
  --> force_attest_all false --> exclude_bakers ["baker_2"]
  (* Wait for inactivation, which should be at cycle 4 *)
  --> exec (bake_until (`Cycle (4, `First_level)))
  (* Cycle 4, level pos 0: level 40 *)
  (* "baker_2" neither baked nor attested for two cycles, it should be deactivated.
     It should also be the only deactivated baker since the other bakers baked. *)
  --> check_is_not_active ~loc:__LOC__ "baker_2"
  --> check_is_active ~loc:__LOC__ "baker_1"
  --> check_is_active ~loc:__LOC__ "tz4_baker"
  (* The distribution for cycle 6 should have been done now, so we should see an activation
     at the beginning of cycle 6.
     1 tz4 bakers out of 2 total bakers: 50% *)
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["33.33%"; "33.33%"; "50.00%"]
  --> check_abaab_activation_level ~loc:__LOC__ (Some 60l)
  (* Wait until activation *)
  --> next_cycle
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["33.33%"; "50.00%"; "50.00%"]
  --> check_abaab_activation_level ~loc:__LOC__ (Some 60l)
  --> next_cycle
  --> check_tz4_baker_number_ratio ~loc:__LOC__ ["50.00%"; "50.00%"; "50.00%"]
  --> check_abaab_activation_level ~loc:__LOC__ (Some 60l)
  (* Reset state parameters *)
  --> force_attest_all true
  --> reset_baking_policy
  -->
  (* The feature *just* activated. The last baked block can be attested by all
            bakers already. *)
  force_attest_all true --> force_preattest_all true
  --> set_baked_round ~payload_round:0 1
  --> next_block --> check_all_bakers_attested --> check_all_bakers_preattested
  --> exec bake_until_dawn_of_next_cycle
  --> check_all_bakers_attested --> check_all_bakers_preattested --> next_block
  --> check_all_bakers_attested --> check_all_bakers_preattested

let test_activation_level level =
  let big = 800_000_000_000_000L in
  (* 800M tez *)
  let small =
    Tez_helpers.to_mutez Default_parameters.constants_mainnet.minimal_stake
    |> Int64.mul 2L
    (* 12000 tez (2 rolls) *)
  in
  init_constants ()
  (* set consensus_committee_size very low to make small bakers very unlikely
     to have any slots before abaab, making observation easier.
     It should still be higher than the number of DAL slots. *)
  --> set S.consensus_committee_size 20
  (* this requires the number of DAL shards to be reduced *)
  --> set S.Dal.Cryptobox_parameters.number_of_shards 10
  (* set threshold size to 0, making attestations not mandatory *)
  --> set S.consensus_threshold_size 0
  (* set a reasonable, human friendly number of blocks per cycle *)
  --> set S.blocks_per_cycle 10l
  (* set inactivity period to very high values, so that
     bakers never get deactivated *)
  --> set S.tolerated_inactivity_period 99
  (* Begin test: setup different profiles
     6 bakers, 2 big, 4 small. *)
  --> begin_test
        ~force_attest_all:true
        ~force_preattest_all:true
        ~abaab_activation_levels:[Some level]
        ~bootstrap_info_list:
          [
            make "big_1" ~algo:Not_Bls ~balance:big;
            make "big_2" ~algo:Bls ~balance:big;
            make "small_1" ~algo:Not_Bls ~balance:small;
            make
              "small_2"
              ~algo:Not_Bls
              ~consensus_key:(Some Bls)
              ~balance:small;
            make "small_3" ~algo:Bls ~balance:small;
            make
              "small_4"
              ~algo:Bls
              ~consensus_key:(Some Not_Bls)
              ~balance:small;
          ]
        []
  (* sanity check *)
  --> check_abaab_activation_level ~loc:__LOC__ (Some (Int32.of_int level))
  --> set_baked_round ~payload_round:0 1
  (* next two blocks because cannot attest genesis *)
  --> next_block
  --> next_block
  (* Total stake: N = 1_600_048_000 tez * 11/20
     Probability that a given small staker has a slot (out of the 20 available slots):
         1 - (1 - 12000/N) ^ 20 = p
     Probability that all three small bakers have a slot for the given block at the same
     time: p^4 = 5.10^-16 *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_attested
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_preattested
  (* Bake until almost the activation level *)
  --> exec (bake_until (`Level (level - 1)))
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_attested
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_preattested
  (* Next level: activation level. All bakers can preattest, but attestations
     are for the predecessor, which is not under abaab. *)
  --> next_block
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ _ -> Lwt_result.return ())
        check_all_bakers_attested
  --> check_all_bakers_preattested
  (* Next level: abaab is fully activated *)
  --> next_block
  --> check_all_bakers_attested --> check_all_bakers_preattested
  (* We bake a bit, at least a cycle *)
  --> loop
        12
        (next_block --> check_all_bakers_attested
       --> check_all_bakers_preattested)

let test_activation_level =
  fold_tag_f
    test_activation_level
    (fun level -> Format.asprintf "level %d" level)
    [8; 9; 10; 11; 12]

let test_activation_with_zero_threshold =
  init_constants ()
  --> set
        S.all_bakers_attest_activation_threshold
        {numerator = 0; denominator = 1}
  --> begin_test ~abaab_activation_levels:[None] ["bootstrap"]
  --> check_abaab_activation_level ~loc:__LOC__ (Some 0l)

let test_total_consensus_power =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  init_constants ()
  --> set
        S.all_bakers_attest_activation_threshold
        {numerator = 2; denominator = 1}
  --> set
        S.consensus_committee_size
        Default_parameters.constants_mainnet.consensus_committee_size
  --> set
        S.consensus_threshold_size
        Default_parameters.constants_mainnet.consensus_threshold_size
  --> begin_test
        ~abaab_activation_levels:[Some 0; None]
        ~bootstrap_info_list:
          [
            make "baker" ~balance:12_000_000_000L;
            make
              "future_big_baker"
              ~delegate:"baker"
              ~balance:12_000_000_000_000L;
          ]
        []
  --> force_attest_all true
  --> set_delegate "future_big_baker" (Some "future_big_baker")
  --> stake "future_big_baker" All
  --> exec (bake_until (`Cycle (consensus_rights_delay, `Last_level)))
  --> force_attest_all false --> attest_with "baker"
  (* future big baker cannot attest yet *)
  -->
  (* We expect the next block to pass the required threshold,
     even though future big baker is now active, increasing the total stake *)
  next_block

let tests =
  tests_of_scenarios
  @@ [
       ("Test abaab activation threshold", test_activation_threshold);
       ( "Test abaab activation threshold when a non BLS baker gets deactivated",
         test_activation_threshold_with_deactivated_baker );
       ("Test abaab activation level", test_activation_level);
       ( "Test abaab threshold zero activation",
         test_activation_with_zero_threshold );
       ("Test abaab total consensus power", test_total_consensus_power);
     ]

let () =
  register_tests
    ~__FILE__
    ~tags:["protocol"; "scenario"; "consensus"; "all_bakers_attest"]
    tests
