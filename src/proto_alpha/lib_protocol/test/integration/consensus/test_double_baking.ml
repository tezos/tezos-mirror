(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:    Protocol (double baking)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe
    Subject:      A double baking evidence operation may be injected when it has
                  been observed that a baker baked two different blocks at the
                  same level and same round.
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

(** Bake two block at the same level using the same policy (i.e. same
    baker). *)
let block_fork ?policy (contract_a, contract_b) b =
  Op.transaction (B b) contract_a contract_b Alpha_context.Tez.one_cent
  >>=? fun operation ->
  Block.bake ?policy ~operation b >>=? fun blk_a ->
  Block.bake ?policy b >|=? fun blk_b -> (blk_a, blk_b)

let order_block_hashes ~correct_order bh1 bh2 =
  let hash1 = Block_header.hash bh1 in
  let hash2 = Block_header.hash bh2 in
  let c = Block_hash.compare hash1 hash2 in
  if correct_order then if c < 0 then (bh1, bh2) else (bh2, bh1)
  else if c < 0 then (bh2, bh1)
  else (bh1, bh2)

let double_baking ctxt ?(correct_order = true) bh1 bh2 =
  let bh1, bh2 = order_block_hashes ~correct_order bh1 bh2 in
  Op.double_baking ctxt bh1 bh2

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

(** Simple scenario where two blocks are baked by a same baker and
    exposed by a double baking evidence operation. *)
let test_valid_double_baking_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, contracts) ->
  Context.get_constants (B genesis)
  >>=? fun Constants.{parametric = {double_baking_punishment; _}; _} ->
  Context.get_first_different_bakers (B genesis) >>=? fun (baker1, baker2) ->
  block_fork ~policy:(By_account baker1) contracts genesis
  >>=? fun (blk_a, blk_b) ->
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~policy:(By_account baker2) ~operation blk_a >>=? fun blk_final ->
  (* Check that the frozen deposits are slashed *)
  Context.Delegate.current_frozen_deposits (B blk_a) baker1
  >>=? fun frozen_deposits_before ->
  Context.Delegate.current_frozen_deposits (B blk_final) baker1
  >>=? fun frozen_deposits_after ->
  let slashed_amount =
    Test_tez.(frozen_deposits_before -! frozen_deposits_after)
  in
  Assert.equal_tez ~loc:__LOC__ slashed_amount double_baking_punishment
  >>=? fun () ->
  (* Check that the initial frozen deposits has not changed *)
  Context.Delegate.initial_frozen_deposits (B blk_final) baker1
  >>=? fun initial_frozen_deposits ->
  Assert.equal_tez ~loc:__LOC__ initial_frozen_deposits frozen_deposits_before

(* auxiliary function used in [double_endorsement] *)
let order_endorsements ~correct_order op1 op2 =
  let oph1 = Operation.hash op1 in
  let oph2 = Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

(* auxiliary function used in
   [test_valid_double_baking_followed_by_double_endorsing] and
   [test_valid_double_endorsing_followed_by_double_baking] *)
let double_endorsement ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_endorsements ~correct_order op1 op2 in
  Op.double_endorsement ctxt e1 e2

let test_valid_double_baking_followed_by_double_endorsing () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, contracts) ->
  Context.get_first_different_bakers (B genesis) >>=? fun (baker1, baker2) ->
  Block.bake genesis >>=? fun b ->
  Context.Delegate.current_frozen_deposits (B b) baker1
  >>=? fun frozen_deposits_before ->
  block_fork ~policy:(By_account baker1) contracts b >>=? fun (blk_a, blk_b) ->
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~policy:(By_account baker2) ~operation blk_a
  >>=? fun blk_with_db_evidence ->
  Context.get_first_different_endorsers (B blk_a) >>=? fun (e1, e2) ->
  let delegate =
    if Signature.Public_key_hash.( = ) e1.delegate baker1 then e1.delegate
    else e2.delegate
  in
  Op.raw_endorsement ~delegate blk_a >>=? fun endorsement_a ->
  Op.raw_endorsement ~delegate blk_b >>=? fun endorsement_b ->
  let operation = double_endorsement (B genesis) endorsement_a endorsement_b in
  Block.bake ~policy:(By_account baker1) ~operation blk_with_db_evidence
  >>=? fun blk_final ->
  Context.Delegate.current_frozen_deposits (B blk_final) baker1
  >>=? fun frozen_deposits_after ->
  Context.get_constants (B genesis) >>=? fun csts ->
  let r =
    csts.parametric.ratio_of_frozen_deposits_slashed_per_double_endorsement
  in
  let expected_frozen_deposits_after_de =
    Test_tez.(
      frozen_deposits_before
      *! Int64.of_int (r.denominator - r.numerator)
      /! Int64.of_int r.denominator)
  in
  (* the deposit after double baking and double endorsing equals the
     expected deposit after double endorsing minus the double baking
     punishment *)
  Assert.equal_tez
    ~loc:__LOC__
    Test_tez.(
      expected_frozen_deposits_after_de
      -! csts.parametric.double_baking_punishment)
    frozen_deposits_after

(* auxiliary function used in [test_valid_double_endorsing_followed_by_double_baking] *)
let block_fork_diff b =
  Context.get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >|=? fun blk_b -> (blk_a, blk_b)

let test_valid_double_endorsing_followed_by_double_baking () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, contracts) ->
  Context.get_first_different_bakers (B genesis) >>=? fun (baker1, baker2) ->
  block_fork_diff genesis >>=? fun (blk_1, blk_2) ->
  Context.Delegate.current_frozen_deposits (B genesis) baker1
  >>=? fun frozen_deposits_before ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_first_different_endorsers (B blk_a) >>=? fun (e1, e2) ->
  let delegate =
    if Signature.Public_key_hash.( = ) e1.delegate baker1 then e1.delegate
    else e2.delegate
  in
  Op.raw_endorsement ~delegate blk_a >>=? fun endorsement_a ->
  Op.raw_endorsement ~delegate blk_b >>=? fun endorsement_b ->
  let operation = double_endorsement (B genesis) endorsement_a endorsement_b in
  Block.bake ~policy:(By_account baker1) ~operation blk_a
  >>=? fun blk_with_de_evidence ->
  block_fork ~policy:(By_account baker1) contracts blk_1
  >>=? fun (blk_a, blk_b) ->
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~policy:(By_account baker2) ~operation blk_with_de_evidence
  >>=? fun blk_with_db_evidence ->
  Context.Delegate.current_frozen_deposits (B blk_with_db_evidence) baker1
  >>=? fun frozen_deposits_after ->
  Context.get_constants (B genesis) >>=? fun csts ->
  let r =
    csts.parametric.ratio_of_frozen_deposits_slashed_per_double_endorsement
  in
  let expected_frozen_deposits_after_de =
    Test_tez.(
      frozen_deposits_before
      *! Int64.of_int (r.denominator - r.numerator)
      /! Int64.of_int r.denominator)
  in
  (* the deposit after double baking and double endorsing equals the
     expected deposit after double endorsing minus the double baking
     punishment *)
  Assert.equal_tez
    ~loc:__LOC__
    Test_tez.(
      expected_frozen_deposits_after_de
      -! csts.parametric.double_baking_punishment)
    frozen_deposits_after

(** Test that the payload producer of the block containing a double
   baking evidence (and not the block producer, if different) receives
   the reward. *)
let test_payload_producer_gets_evidence_rewards () =
  Context.init_n ~consensus_threshold:0 10 () >>=? fun (genesis, contracts) ->
  Context.get_constants (B genesis)
  >>=? fun Constants.
             {
               parametric =
                 {double_baking_punishment; baking_reward_fixed_portion; _};
               _;
             } ->
  Context.get_first_different_bakers (B genesis) >>=? fun (baker1, baker2) ->
  let c1_c2 =
    match contracts with c1 :: c2 :: _ -> (c1, c2) | _ -> assert false
  in
  block_fork ~policy:(By_account baker1) c1_c2 genesis >>=? fun (b1, b2) ->
  double_baking (B b1) b1.header b2.header |> fun db_evidence ->
  Block.bake ~policy:(By_account baker2) ~operation:db_evidence b1
  >>=? fun b_with_evidence ->
  Context.get_endorsers (B b_with_evidence) >>=? fun endorsers ->
  List.map_es
    (function
      | {Plugin.RPC.Validators.delegate; slots; _} -> return (delegate, slots))
    endorsers
  >>=? fun preendorsers ->
  List.map_ep
    (fun (endorser, _slots) ->
      Op.preendorsement ~delegate:endorser b_with_evidence)
    preendorsers
  >>=? fun preendos ->
  Block.bake
    ~payload_round:(Some Round.zero)
    ~locked_round:(Some Round.zero)
    ~policy:(By_account baker1)
    ~operations:(preendos @ [db_evidence])
    b1
  >>=? fun b' ->
  (* the frozen deposits of the double-signer [baker1] are slashed *)
  Context.Delegate.current_frozen_deposits (B b1) baker1
  >>=? fun frozen_deposits_before ->
  Context.Delegate.current_frozen_deposits (B b') baker1
  >>=? fun frozen_deposits_after ->
  let slashed_amount =
    Test_tez.(frozen_deposits_before -! frozen_deposits_after)
  in
  Assert.equal_tez ~loc:__LOC__ slashed_amount double_baking_punishment
  >>=? fun () ->
  (* [baker2] included the double baking evidence in [b_with_evidence]
     and so it receives the reward for the evidence included in [b']
     (besides the reward for proposing the payload). *)
  Context.Delegate.full_balance (B b1) baker2 >>=? fun full_balance ->
  let evidence_reward = Test_tez.(slashed_amount /! 2L) in
  let expected_reward =
    Test_tez.(baking_reward_fixed_portion +! evidence_reward)
  in
  Context.Delegate.full_balance (B b') baker2
  >>=? fun full_balance_with_rewards ->
  let real_reward = Test_tez.(full_balance_with_rewards -! full_balance) in
  Assert.equal_tez ~loc:__LOC__ expected_reward real_reward >>=? fun () ->
  (* [baker1] did not produce the payload, it does not receive the reward for the
     evidence *)
  Context.Delegate.full_balance (B b1) baker1 >>=? fun full_balance_at_b1 ->
  Context.Delegate.full_balance (B b') baker1 >>=? fun full_balance_at_b' ->
  Assert.equal_tez
    ~loc:__LOC__
    full_balance_at_b'
    Test_tez.(full_balance_at_b1 -! double_baking_punishment)

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that a double baking operation fails if it exposes the same two
    blocks. *)
let test_same_blocks () =
  Context.init2 () >>=? fun (b, _contracts) ->
  Block.bake b >>=? fun ba ->
  double_baking (B ba) ba.header ba.header |> fun operation ->
  Block.bake ~operation ba >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that an double baking operation that is invalid due to
   incorrect ordering of the block headers fails. *)
let test_incorrect_order () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, contracts) ->
  block_fork ~policy:(By_round 0) contracts genesis >>=? fun (blk_a, blk_b) ->
  double_baking (B genesis) ~correct_order:false blk_a.header blk_b.header
  |> fun operation ->
  Block.bake ~operation genesis >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that a double baking operation exposing two blocks with
    different levels fails. *)
let test_different_levels () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, contracts) ->
  block_fork ~policy:(By_round 0) contracts b >>=? fun (blk_a, blk_b) ->
  Block.bake blk_b >>=? fun blk_b_2 ->
  double_baking (B blk_a) blk_a.header blk_b_2.header |> fun operation ->
  Block.bake ~operation blk_a >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that a double baking operation exposing two yet-to-be-baked
    blocks fails. *)
let test_too_early_double_baking_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, contracts) ->
  Block.bake_until_cycle_end genesis >>=? fun b ->
  block_fork ~policy:(By_round 0) contracts b >>=? fun (blk_a, blk_b) ->
  double_baking (B b) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~operation genesis >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Too_early_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Block ->
          true
      | _ -> false)

(** Check that after [max_slashing_period * blocks_per_cycle + 1] blocks -- corresponding to 2 cycles
   --, it is not possible to create a double baking operation anymore. *)
let test_too_late_double_baking_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, contracts) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {max_slashing_period; _}; _} ->
  block_fork ~policy:(By_round 0) contracts b >>=? fun (blk_a, blk_b) ->
  Block.bake_until_n_cycle_end max_slashing_period blk_a >>=? fun blk ->
  double_baking (B blk) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Outdated_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Block ->
          true
      | _ -> false)

(** Check that before [max_slashing_period * blocks_per_cycle] blocks
   -- corresponding to 2 cycles --, it is still possible to create a
   double baking operation. *)
let test_just_in_time_double_baking_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, contracts) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  block_fork ~policy:(By_round 0) contracts b >>=? fun (blk_a, blk_b) ->
  Block.bake_until_cycle_end blk_a >>=? fun blk ->
  Block.bake_n Int32.(sub blocks_per_cycle 2l |> to_int) blk >>=? fun blk ->
  let operation = double_baking (B blk) blk_a.header blk_b.header in
  (* We include the denunciation in the previous to last block of the
     cycle. *)
  Block.bake ~operation blk >>=? fun (_ : Block.t) -> return_unit

(** Check that an invalid double baking evidence that exposes two
    block baking with same level made by different bakers fails. *)
let test_different_delegates () =
  Context.init2 () >>=? fun (b, _contracts) ->
  Context.get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >>=? fun blk_b ->
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  Block.bake ~operation blk_a >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** This test is supposed to mimic that a block cannot be baked by one baker and
    signed by another. The way it tries to show this is by using a
    Double_baking_evidence operation:
    - say [baker_1] bakes block blk_a so blk_a has a header with baker_1's
    signature
    - say we create an artificial [header_b] for a block b' with timestamp [ts]
    at the same level as [blk_a], and the header is created such that it says that
    b' is baked by the same [baker_1] and signed by [baker_2]
    - because [header_b] says that b' is baked by [baker_0], b' has the same
    round as [blk_a], which together with the fact that b' and [blk_a] have the
    same level, means that double_baking is valid: we have [blk_a] and b' at the
    same level and round, but with different timestamps and signed by different
    bakers.
    This test fails with an error stating that block is signed by the wrong
    baker. *)
let test_wrong_signer () =
  let header_custom_signer baker baker_2 timestamp b =
    Block.Forge.forge_header ~policy:(By_account baker) ~timestamp b
    >>=? fun header ->
    Block.Forge.set_baker baker_2 header |> Block.Forge.sign_header
  in
  Context.init2 () >>=? fun (b, _contracts) ->
  Context.get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  let ts = Timestamp.of_seconds_string (Int64.to_string 10L) in
  match ts with
  | None -> assert false
  | Some ts ->
      header_custom_signer baker_1 baker_2 ts b >>=? fun header_b ->
      double_baking (B blk_a) blk_a.header header_b |> fun operation ->
      Block.bake ~operation blk_a >>= fun e ->
      Assert.proto_error_with_info ~loc:__LOC__ e "Invalid block signature"

(** an evidence can only be accepted once (this also means that the
   same evidence doesn't lead to slashing the offender twice) *)
let test_double_evidence () =
  Context.init3 ~consensus_threshold:0 () >>=? fun (blk, (c1, c2, _c3)) ->
  block_fork (c1, c2) blk >>=? fun (blk_a, blk_b) ->
  Block.bake_until_cycle_end blk_a >>=? fun blk ->
  double_baking (B blk) blk_a.header blk_b.header |> fun evidence ->
  Block.bake ~operations:[evidence; evidence] blk >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Conflicting_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Block ->
          true
      | _ -> false)
  >>=? fun () ->
  Block.bake ~operation:evidence blk >>=? fun blk ->
  double_baking (B blk) blk_b.header blk_a.header |> fun evidence ->
  Block.bake ~operation:evidence blk >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Already_denounced _ -> true
      | _ -> false)

let tests =
  [
    Tztest.tztest
      "valid double baking evidence"
      `Quick
      test_valid_double_baking_evidence;
    Tztest.tztest
      "payload producer receives the rewards for double baking evidence"
      `Quick
      test_payload_producer_gets_evidence_rewards;
    (* Should fail*)
    Tztest.tztest "same blocks" `Quick test_same_blocks;
    Tztest.tztest "incorrect order" `Quick test_incorrect_order;
    Tztest.tztest "different levels" `Quick test_different_levels;
    Tztest.tztest
      "too early double baking evidence"
      `Quick
      test_too_early_double_baking_evidence;
    Tztest.tztest
      "too late double baking evidence"
      `Quick
      test_too_late_double_baking_evidence;
    Tztest.tztest
      "just in time double baking evidence"
      `Quick
      test_just_in_time_double_baking_evidence;
    Tztest.tztest "different delegates" `Quick test_different_delegates;
    Tztest.tztest "wrong delegate" `Quick test_wrong_signer;
    Tztest.tztest
      "reject double injection of an evidence"
      `Quick
      test_double_evidence;
    Tztest.tztest
      "double baking followed by double endorsing"
      `Quick
      test_valid_double_baking_followed_by_double_endorsing;
    Tztest.tztest
      "double endorsing followed by double baking"
      `Quick
      test_valid_double_endorsing_followed_by_double_baking;
  ]
