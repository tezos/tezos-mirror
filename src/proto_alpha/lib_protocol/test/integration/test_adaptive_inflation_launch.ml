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
    Component:    Adaptive Inflation, launch vote
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_adaptive_inflation_launch.ml
    Subject:      Test the launch vote feature of Adaptive Inflation.
*)

let assert_level ~loc (blk : Block.t) expected =
  let current_level = blk.header.shell.level in
  Assert.equal_int32 ~loc current_level expected

let get_launch_cycle ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_inflation_launch_cycle (B blk) in
  Assert.get_some ~loc launch_cycle_opt

let assert_is_not_yet_set_to_launch ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_inflation_launch_cycle (B blk) in
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
  let open Lwt_result_syntax in
  let* current_cycle = Block.current_cycle blk in
  assert_cycle_eq ~loc current_cycle expected

let stake ctxt contract amount =
  let open Lwt_result_wrap_syntax in
  let*?@ entrypoint =
    Protocol.Alpha_context.Entrypoint.of_string_strict ~loc:0 "stake"
  in
  Op.transaction ctxt ~entrypoint contract contract amount

let set_delegate_parameters ctxt delegate ~staking_over_baking_limit
    ~baking_over_staking_edge =
  let open Lwt_result_wrap_syntax in
  let*?@ entrypoint =
    Protocol.Alpha_context.Entrypoint.of_string_strict
      ~loc:0
      "set_delegate_parameters"
  in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string
         (Printf.sprintf
            "Pair %d (Pair %d Unit)"
            staking_over_baking_limit
            baking_over_staking_edge))
  in
  Op.transaction
    ctxt
    ~entrypoint
    ~parameters
    delegate
    delegate
    Protocol.Alpha_context.Tez.zero

(* Test that:
   - the EMA of the adaptive inflation vote reaches the threshold after the
     expected duration,
   - the launch cycle is set as soon as the threshold is reached,
   - the launch cycle is not reset before it is reached,
   - once the launch cycle is reached, costaking is allowed. *)
let test_launch threshold expected_vote_duration () =
  let open Lwt_result_wrap_syntax in
  let assert_ema_above_threshold ~loc
      (metadata : Protocol.Main.block_header_metadata) =
    let ema =
      Protocol.Alpha_context.Toggle_votes.Adaptive_inflation_launch_EMA.to_int32
        metadata.adaptive_inflation_toggle_ema
    in
    Assert.lt_int32 ~loc threshold ema
  in
  (* Initialize the state with a single delegate. *)
  let constants =
    let default_constants = Default_parameters.constants_test in
    let adaptive_inflation =
      {
        default_constants.adaptive_inflation with
        launch_ema_threshold = threshold;
      }
    in
    let consensus_threshold = 0 in
    {default_constants with consensus_threshold; adaptive_inflation}
  in
  let* block, delegate = Context.init_with_constants1 constants in
  let delegate_pkh = Context.Contract.pkh delegate in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in

  (* To test that adaptive inflation is active, we test that
     costaking, a feature only available after the activation, is
     allowed. But by default, delegates reject costakers, they must
     explicitely set a positive staking_over_baking_limit to allow
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
        ~staking_over_baking_limit:1_000_000
        ~baking_over_staking_edge:1_000_000_000
    in
    Block.bake ~operation ~adaptive_inflation_vote:Toggle_vote_on block
  in

  (* Initialization of a delegator account which will attempt to
     costake. *)
  let wannabe_costaker_account = Account.new_account () in
  let wannabe_costaker =
    Protocol.Alpha_context.Contract.Implicit
      Account.(wannabe_costaker_account.pkh)
  in

  (* To set up the wannabe costaker, we need three operations: a
     transfer from the delegate to initialize its balance, a
     revelation of its public key, and a delegation toward the
     delegate. For simplicity we put these operations in different
     blocks. *)
  let* block =
    let* operation =
      Op.transaction
        (B block)
        delegate
        wannabe_costaker
        (Protocol.Alpha_context.Tez.of_mutez_exn 2_000_000_000_000L)
    in
    Block.bake ~operation ~adaptive_inflation_vote:Toggle_vote_on block
  in
  let* block =
    let* operation = Op.revelation (B block) wannabe_costaker_account.pk in
    Block.bake ~operation ~adaptive_inflation_vote:Toggle_vote_on block
  in
  let* block =
    let* operation =
      Op.delegation (B block) wannabe_costaker (Some delegate_pkh)
    in
    Block.bake ~operation ~adaptive_inflation_vote:Toggle_vote_on block
  in
  (* Self-staking most of the remaining balance. *)
  let* block =
    let* operation =
      stake
        (B block)
        delegate
        (Protocol.Alpha_context.Tez.of_mutez_exn 1_800_000_000_000L)
    in
    Block.bake ~operation ~adaptive_inflation_vote:Toggle_vote_on block
  in

  (* We are now ready to activate the feature through by baking many
     more blocks voting in favor of the activation until the EMA
     threshold is reached. *)
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in

  let* block =
    Block.bake_while_with_metadata
      ~adaptive_inflation_vote:Toggle_vote_on
      (fun _block metadata ->
        let ema =
          Protocol.Alpha_context.Toggle_votes.Adaptive_inflation_launch_EMA
          .to_int32
            metadata.adaptive_inflation_toggle_ema
        in
        Compare.Int32.(ema < threshold))
      block
  in
  (* At this point we are on the last block before the end of the vote. *)
  let* () =
    assert_level ~loc:__LOC__ block (Int32.pred expected_vote_duration)
  in
  let* () = assert_is_not_yet_set_to_launch ~loc:__LOC__ block in
  (* We bake one more block to end the vote and set the feature to launch. *)
  let* block, metadata =
    Block.bake_n_with_metadata ~adaptive_inflation_vote:Toggle_vote_on 1 block
  in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  let* () = assert_level ~loc:__LOC__ block expected_vote_duration in
  (* At this point the feature is not launched yet, it is simply
     planned to be launched. *)
  (* We check that the feature is not yet active by attempting a
     costake operation. *)
  let* () =
    let* operation =
      stake
        (B block)
        wannabe_costaker
        (Protocol.Alpha_context.Tez.of_mutez_exn 10L)
    in
    let* i = Incremental.begin_construction block in
    let*! i = Incremental.add_operation i operation in
    Assert.proto_error_with_info
      ~loc:__LOC__
      i
      "Staking for a non-delegate while co-staking is disabled"
  in

  let* launch_cycle = get_launch_cycle ~loc:__LOC__ block in
  (* Bake until the activation. *)
  let* block = Block.bake_until_cycle launch_cycle block in
  let* block, metadata = Block.bake_n_with_metadata 1 block in
  let* () = assert_ema_above_threshold ~loc:__LOC__ metadata in
  (* Check that keeping the EMA above the threshold did not postpone
     the activation. *)
  let* launch_cycle_bis = get_launch_cycle ~loc:__LOC__ block in
  let* () = assert_cycle_eq ~loc:__LOC__ launch_cycle launch_cycle_bis in
  (* Check that the current cycle is the one at which the launch is
     planned to happen. *)
  let* () = assert_current_cycle ~loc:__LOC__ block launch_cycle in

  (* Test that the wannabe costaker is now allowed to stake almost all
     its balance. It cannot totally costake it however because this is
     considered by the protocol as an attempt to empty the account, and
     emptying delegated accounts is forbidden. *)
  let* balance = Context.Contract.balance (B block) wannabe_costaker in
  let*?@ balance_to_stake = Protocol.Alpha_context.Tez.(balance -? one) in
  let* operation = stake (B block) wannabe_costaker balance_to_stake in
  let* (_block : Block.t) = Block.bake ~operation block in
  return_unit

let tests =
  [
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       inflation launch cycle is set (very low threshold)"
      `Quick
      (test_launch
         1000000l (* This means that the threshold is set at 0.05% *)
         59l);
    Tztest.tztest
      "the EMA reaches the vote threshold at the expected level and adaptive \
       inflation launch cycle is set (realistic threshold)"
      `Slow
      (test_launch
         Default_parameters.constants_test.adaptive_inflation
           .launch_ema_threshold
         187259l
         (* This vote duration is consistent with the result of the
            unit test for this EMA in
            ../unit/test_adaptive_inflation_ema.ml*));
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("adaptive inflation launch", tests)]
  |> Lwt_main.run
