(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    Adaptive Issuance, launch vote
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_adaptive_issuance_launch.ml
    Subject:      Test the launch vote feature of Adaptive Issuance.
*)

open Adaptive_issuance_helpers

let assert_level ~loc (blk : Block.t) expected =
  let current_level = blk.header.shell.level in
  Assert.equal_int32 ~loc current_level expected

let assert_is_not_yet_set_to_launch ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_issuance_launch_cycle (B blk) in
  Assert.is_none
    ~loc
    ~pp:(fun fmt cycle ->
      Format.fprintf
        fmt
        "Activation cycle is set to %a but we expected it to be unset"
        Protocol.Alpha_context.Cycle.pp
        cycle)
    launch_cycle_opt

let assert_cycle_eq ~loc c1 c2 =
  Assert.equal
    ~loc
    Protocol.Alpha_context.Cycle.( = )
    "cycle equality"
    Protocol.Alpha_context.Cycle.pp
    c1
    c2

let assert_current_cycle ~loc (blk : Block.t) expected =
  let current_cycle = Block.current_cycle blk in
  assert_cycle_eq ~loc current_cycle expected

(** Assert that the staking balance is the expected one. *)
let assert_total_frozen_stake ~loc block expected =
  let open Lwt_result_syntax in
  let* actual = Context.get_total_frozen_stake (B block) in
  Assert.equal_tez ~loc actual expected

(* Assert that the current voting power of a delegate is the expected
   one. The expectation is computed based on the expected self-staked,
   delegated, and staked tez. The delegate's own liquid balance is
   measured. *)
let assert_voting_power ~loc block delegate ~ai_enabled ~expected_staked
    ~expected_delegated ~expected_ext_staked =
  let open Lwt_result_syntax in
  let* balance = Context.Contract.balance (B block) (Implicit delegate) in
  let balance = Protocol.Alpha_context.Tez.to_mutez balance in
  let expected_liquid = Int64.add balance expected_delegated in
  let expected_frozen = Int64.add expected_staked expected_ext_staked in
  let* constants = Context.get_constants (B block) in
  let edge_of_staking_over_delegation =
    Int64.of_int
      constants.parametric.adaptive_issuance.edge_of_staking_over_delegation
  in
  let expected_voting_power = Int64.add expected_frozen expected_liquid in
  let expected_baking_power =
    if ai_enabled then
      Int64.add
        expected_frozen
        (Int64.div expected_liquid edge_of_staking_over_delegation)
    else expected_voting_power
  in
  let* actual_voting_power =
    Context.get_current_voting_power (B block) delegate
  in
  let* () = Assert.equal_int64 ~loc actual_voting_power expected_voting_power in
  let* actual_baking_power =
    Context.get_current_baking_power (B block) delegate
  in
  Assert.equal_int64 ~loc actual_baking_power expected_baking_power

(* Test that:
   - the EMA of the adaptive issuance vote reaches the threshold after the
     expected duration,
   - the launch cycle is set as soon as the threshold is reached,
   - the launch cycle is not reset before it is reached,
   - once the launch cycle is reached, staking is allowed,
   - staking increases total_frozen_stake. *)
let test_launch threshold expected_vote_duration () =
  let open Lwt_result_wrap_syntax in
  let assert_ema_above_threshold ~loc
      (metadata : Protocol.Main.block_header_metadata) =
    let ema =
      Protocol.Alpha_context.Per_block_votes.Adaptive_issuance_launch_EMA
      .to_int32
        metadata.adaptive_issuance_vote_ema
    in
    Assert.lt_int32 ~loc threshold ema
  in
  (* Initialize the state with a single delegate. *)
  let constants =
    let default_constants = Default_parameters.constants_test in
    let adaptive_issuance =
      {
        default_constants.adaptive_issuance with
        launch_ema_threshold = threshold;
        activation_vote_enable = true;
        autostaking_enable = false;
      }
    in
    let cost_per_byte = Tez.zero in
    let issuance_weights =
      {
        Default_parameters.constants_test.issuance_weights with
        base_total_issued_per_minute = Tez.zero;
      }
    in
    let consensus_threshold = 0 in
    {
      default_constants with
      consensus_threshold;
      adaptive_issuance;
      issuance_weights;
      cost_per_byte;
    }
  in
  let preserved_cycles = constants.preserved_cycles in
  let* block, delegate = Context.init_with_constants1 constants in
  let delegate_pkh = Context.Contract.pkh delegate in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 200_000_000_000L)
  in

  (* To test that adaptive issuance is active, we test that
     staking, a feature only available after the activation, is
     allowed. But by default, delegates reject stakers, they must
     explicitely set a positive limit_of_staking_over_baking to allow
     them. Setting this limit does not immediately take effect but can
     be done before the activation. For these reasons, we set it at
     the beginning.

     The baking_over_staking_edge indicates the portion of the rewards
     sent to the delegate's liquid balance. It's expressed in
     billionth, with 0 meaning that everything is frozen and one
     billion meaning that everything is liquid. We send all rewards to
     the liquid part to ease reasoning about the total frozen
     stake. *)
  let* block =
    let* operation =
      set_delegate_parameters
        (B block)
        delegate
        ~parameters:
          {
            limit_of_staking_over_baking = Q.one;
            edge_of_baking_over_staking = Q.one;
          }
    in
    Block.bake ~operation ~adaptive_issuance_vote:Per_block_vote_on block
  in

  (* Initialization of a delegator account which will attempt to
     stake. *)
  let wannabe_staker_account = Account.new_account () in
  let wannabe_staker =
    Protocol.Alpha_context.Contract.Implicit
      Account.(wannabe_staker_account.pkh)
  in

  (* To set up the wannabe staker, we need three operations: a
     transfer from the delegate to initialize its balance, a
     revelation of its public key, and a delegation toward the
     delegate. For simplicity we put these operations in different
     blocks. *)
  let* block =
    let* operation =
      Op.transaction
        (B block)
        delegate
        wannabe_staker
        (Protocol.Alpha_context.Tez.of_mutez_exn 2_000_000_000_000L)
    in
    Block.bake ~operation ~adaptive_issuance_vote:Per_block_vote_on block
  in
  let* block =
    let* operation =
      Op.revelation
        ~fee:Protocol.Alpha_context.Tez.zero
        (B block)
        wannabe_staker_account.pk
    in
    Block.bake ~operation ~adaptive_issuance_vote:Per_block_vote_on block
  in
  let* block =
    let* operation =
      Op.delegation
        ~fee:Protocol.Alpha_context.Tez.zero
        (B block)
        wannabe_staker
        (Some delegate_pkh)
    in
    Block.bake ~operation ~adaptive_issuance_vote:Per_block_vote_on block
  in
  (* Self-staking most of the remaining balance. *)
  let* block =
    let* operation =
      stake
        (B block)
        delegate
        (Protocol.Alpha_context.Tez.of_mutez_exn 1_800_000_000_000L)
    in
    Block.bake ~operation ~adaptive_issuance_vote:Per_block_vote_on block
  in

  (* We are now ready to activate the feature through by baking many
     more blocks voting in favor of the activation until the EMA
     threshold is reached. *)
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in

  let* block, _ =
    Block.bake_while_with_metadata
      ~adaptive_issuance_vote:Per_block_vote_on
      (fun _block metadata ->
        let ema =
          Protocol.Alpha_context.Per_block_votes.Adaptive_issuance_launch_EMA
          .to_int32
            metadata.adaptive_issuance_vote_ema
        in
        let launch_cycle = metadata.adaptive_issuance_launch_cycle in
        let cond = Compare.Int32.(ema < threshold) in
        assert (
          if cond then Option.is_none launch_cycle
          else Option.is_some launch_cycle) ;
        cond)
      block
  in
  (* At this point we are on the last block before the end of the vote. *)
  let* () =
    assert_level ~loc:__LOC__ block (Int32.pred expected_vote_duration)
  in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  (* We bake one more block to end the vote and set the feature to launch. *)
  let* block, (metadata, _) =
    Block.bake_n_with_metadata ~adaptive_issuance_vote:Per_block_vote_on 1 block
  in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  let* () = assert_level ~loc:__LOC__ block expected_vote_duration in
  (* At this point the feature is not launched yet, it is simply
     planned to be launched. *)
  (* We check that the feature is not yet active by attempting a
     stake operation. *)
  let* () =
    let* operation =
      stake
        (B block)
        wannabe_staker
        (Protocol.Alpha_context.Tez.of_mutez_exn 10L)
    in
    let* i = Incremental.begin_construction block in
    let*! i = Incremental.add_operation i operation in
    Assert.proto_error_with_info
      ~loc:__LOC__
      i
      "Staking for a delegator while external staking is disabled"
  in
  let* () =
    assert_voting_power
      ~loc:__LOC__
      block
      delegate_pkh
      ~ai_enabled:false
      ~expected_staked:2_000_000_000_000L
      ~expected_delegated:2_000_000_000_000L
      ~expected_ext_staked:0L
  in

  let* launch_cycle = get_launch_cycle ~loc:__LOC__ block in
  let* () =
    (* Check that the block metadata information about the launch cycle
       is consistent with the RPC. *)
    let* cycle =
      Assert.get_some ~loc:__LOC__ metadata.adaptive_issuance_launch_cycle
    in
    assert_cycle_eq ~loc:__LOC__ launch_cycle cycle
  in
  (* Bake until the activation. *)
  let* block = Block.bake_until_cycle launch_cycle block in
  let* block, (metadata, _) = Block.bake_n_with_metadata 1 block in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  (* Check that keeping the EMA above the threshold did not postpone
     the activation. *)
  let* launch_cycle_bis = get_launch_cycle ~loc:__LOC__ block in
  let* () = assert_cycle_eq ~loc:__LOC__ launch_cycle launch_cycle_bis in
  (* Check that the current cycle is the one at which the launch is
     planned to happen. *)
  let* () = assert_current_cycle ~loc:__LOC__ block launch_cycle in
  (* At this point, only the delegate has frozen any stake and its
     frozen balance is about 2 million tez (it started with 4 million,
     sent half to its delegate, and staked the rest). *)
  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 2_000_000_000_000L)
  in
  let* () =
    assert_voting_power
      ~loc:__LOC__
      block
      delegate_pkh
      ~ai_enabled:true
      ~expected_staked:2_000_000_000_000L
      ~expected_delegated:2_000_000_000_000L
      ~expected_ext_staked:0L
  in

  (* Test that the wannabe staker is now allowed to stake almost all
     its balance. It cannot totally stake it however because this is
     considered by the protocol as an attempt to empty the account, and
     emptying delegated accounts is forbidden. *)
  let* balance = Context.Contract.balance (B block) wannabe_staker in
  let*?@ balance_to_stake = Protocol.Alpha_context.Tez.(balance -? one) in
  let* operation = stake (B block) wannabe_staker balance_to_stake in
  let* block = Block.bake ~operation block in
  (* The staking operation leads to an increase of the
     total_frozen_stake but only preserved_cycles after the
     operation. *)
  let start_cycle = Block.current_cycle block in
  let* block =
    Block.bake_while
      ~invariant:(fun block ->
        assert_total_frozen_stake
          ~loc:__LOC__
          block
          (Protocol.Alpha_context.Tez.of_mutez_exn 2_000_000_000_000L))
      (fun block ->
        let current_cycle = Block.current_cycle block in
        Protocol.Alpha_context.Cycle.(
          current_cycle <= add start_cycle preserved_cycles))
      block
  in
  let* block = Block.bake block in
  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 3_999_999_000_000L)
  in
  let* () =
    assert_voting_power
      ~loc:__LOC__
      block
      delegate_pkh
      ~ai_enabled:true
      ~expected_staked:2_000_000_000_000L
      ~expected_delegated:1_000_000L
      ~expected_ext_staked:1_999_999_000_000L
  in
  return_unit

(* Test that, with the feature flag unset:
   - the EMA of the adaptive issuance vote reaches the threshold after the
     expected duration,
   - the feature does not activate. *)
let test_does_not_launch_without_feature_flag threshold vote_duration () =
  let open Lwt_result_wrap_syntax in
  let assert_ema_above_threshold ~loc
      (metadata : Protocol.Main.block_header_metadata) =
    let ema =
      Protocol.Alpha_context.Per_block_votes.Adaptive_issuance_launch_EMA
      .to_int32
        metadata.adaptive_issuance_vote_ema
    in
    Assert.lt_int32 ~loc threshold ema
  in
  (* Initialize the state with a single delegate. *)
  let constants =
    let default_constants = Default_parameters.constants_test in
    let adaptive_issuance =
      {
        default_constants.adaptive_issuance with
        launch_ema_threshold = threshold;
        activation_vote_enable = false;
      }
    in
    let consensus_threshold = 0 in
    {default_constants with consensus_threshold; adaptive_issuance}
  in
  let* block, _delegate = Context.init_with_constants1 constants in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 200_000_000_000L)
  in
  (* Bake many more blocks voting in favor of the activation until the
     EMA threshold is reached. *)
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  let* block, _ =
    Block.bake_while_with_metadata
      ~adaptive_issuance_vote:Per_block_vote_on
      (fun _block metadata ->
        let ema =
          Protocol.Alpha_context.Per_block_votes.Adaptive_issuance_launch_EMA
          .to_int32
            metadata.adaptive_issuance_vote_ema
        in
        let launch_cycle = metadata.adaptive_issuance_launch_cycle in
        let cond = Compare.Int32.(ema < threshold) in
        assert (Option.is_none launch_cycle) ;
        cond)
      block
  in
  (* At this point we are on the last block before the end of the vote. *)
  let* () = assert_level ~loc:__LOC__ block (Int32.pred vote_duration) in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  (* We bake one more block, this would set the feature to launch if
     the vote was taken into account. *)
  let* block, (metadata, _) =
    Block.bake_n_with_metadata ~adaptive_issuance_vote:Per_block_vote_on 1 block
  in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  let* () = assert_level ~loc:__LOC__ block vote_duration in
  let* launch_cycle_opt =
    Context.get_adaptive_issuance_launch_cycle (B block)
  in
  let* () = Assert.is_none ~loc:__LOC__ ~pp:Cycle.pp launch_cycle_opt in
  let* () =
    Assert.is_none
      ~loc:__LOC__
      ~pp:Cycle.pp
      metadata.adaptive_issuance_launch_cycle
  in
  return_unit

(* Test that with force_activation feature flag set, the feature activates
   without waiting for the activation vote *)
let test_launch_without_vote () =
  let open Lwt_result_wrap_syntax in
  (* Initialize the state with a single delegate. *)
  let constants =
    let default_constants = Default_parameters.constants_test in
    let issuance_weights =
      {
        Default_parameters.constants_test.issuance_weights with
        base_total_issued_per_minute = Tez.zero;
      }
    in
    let adaptive_issuance =
      {default_constants.adaptive_issuance with force_activation = true}
    in
    let consensus_threshold = 0 in
    {
      default_constants with
      consensus_threshold;
      issuance_weights;
      adaptive_issuance;
    }
  in
  let* block, delegate = Context.init_with_constants1 constants in
  let delegate_pkh = Context.Contract.pkh delegate in
  let* block = Block.bake block in

  (* AI should be activated and launch cycle is current cycle (0) *)
  let* launch_cycle_opt =
    Context.get_adaptive_issuance_launch_cycle (B block)
  in
  let* launch_cycle = Assert.get_some ~loc:__LOC__ launch_cycle_opt in
  let* () = Assert.equal_int32 ~loc:__LOC__ (Cycle.to_int32 launch_cycle) 0l in

  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 200_000_000_000L)
  in
  (* feature flag is set, AI should be active, let's use the stake function to check *)
  let* operation =
    stake
      (B block)
      delegate
      (Protocol.Alpha_context.Tez.of_mutez_exn 180_000_000_000L)
  in
  let* block = Block.bake ~operation block in
  (* Wait until total frozen stake is updated *)
  let start_cycle = Block.current_cycle block in
  let* block =
    Block.bake_while
      ~invariant:(fun block ->
        assert_total_frozen_stake
          ~loc:__LOC__
          block
          (Protocol.Alpha_context.Tez.of_mutez_exn 200_000_000_000L))
      (fun block ->
        let current_cycle = Block.current_cycle block in
        Protocol.Alpha_context.Cycle.(
          current_cycle <= add start_cycle constants.preserved_cycles))
      block
  in
  let* block = Block.bake block in

  let* () =
    assert_total_frozen_stake
      ~loc:__LOC__
      block
      (Protocol.Alpha_context.Tez.of_mutez_exn 380_000_000_000L)
  in
  let* () =
    assert_voting_power
      ~loc:__LOC__
      block
      delegate_pkh
      ~ai_enabled:true
      ~expected_staked:380_000_000_000L
      ~expected_delegated:0L
      ~expected_ext_staked:0L
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "Launch with force_activation feature flag set activates AI immediately"
      `Quick
      test_launch_without_vote;
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       issuance launches (vote enabled)"
      `Quick
      (test_launch
         1000000l (* This means that the threshold is set at 0.05% *)
         88l);
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       issuance does not launch (vote disabled)"
      `Quick
      (test_does_not_launch_without_feature_flag
         1000000l (* This means that the threshold is set at 0.05% *)
         88l);
  ]

(* These tests have been unplugged because they are too long for the
   CI (more than 3 minutes). For unit tests of the fact that the 80%
   threshold can be reached by iteration of the EMA update function,
   see ../unit/test_adaptive_issuance_ema.ml *)
let _unplugged_tests =
  [
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       issuance launches (realistic threshold, vote enabled)"
      `Slow
      (test_launch
         Default_parameters.constants_test.adaptive_issuance
           .launch_ema_threshold
         280894l
         (* This vote duration is consistent with the result of the
            unit test for this EMA in
            ../unit/test_adaptive_issuance_ema.ml*));
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       issuance does not launch (realistic threshold, vote disabled)"
      `Slow
      (test_does_not_launch_without_feature_flag
         Default_parameters.constants_test.adaptive_issuance
           .launch_ema_threshold
         280894l
         (* This vote duration is consistent with the result of the
            unit test for this EMA in
            ../unit/test_adaptive_issuance_ema.ml*));
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("adaptive issuance launch", tests)]
  |> Lwt_main.run
