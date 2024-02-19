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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_double_baking.ml
    Subject:      A double baking evidence operation may be injected when it has
                  been observed that a baker baked two different blocks at the
                  same level and same round.
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

(** Bake two blocks at the same level using the same policy (i.e. same
    baker). *)
let block_fork ?policy (contract_a, contract_b) b =
  let open Lwt_result_syntax in
  let* operation =
    Op.transaction (B b) contract_a contract_b Alpha_context.Tez.one_cent
  in
  let* blk_a = Block.bake ?policy ~operation b in
  let+ blk_b = Block.bake ?policy b in
  (blk_a, blk_b)

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
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init2 ~consensus_threshold:0 () in
  let* c = Context.get_constants (B genesis) in
  let p =
    c.parametric.percentage_of_frozen_deposits_slashed_per_double_baking
  in
  let* baker1, baker2 = Context.get_first_different_bakers (B genesis) in
  let* blk_fst_cycle, _, _ =
    Block.bake_until_cycle_end_with_metadata ~policy:(By_account baker2) genesis
  in
  let* blk_a, blk_b =
    block_fork ~policy:(By_account baker1) contracts blk_fst_cycle
  in
  let operation = double_baking (B blk_a) blk_a.header blk_b.header in
  let* blk_final = Block.bake ~policy:(By_account baker2) ~operation blk_a in
  (* Check that the frozen deposits haven't been slashed, yet. *)
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_a) baker1
  in
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B blk_final) baker1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_before
      frozen_deposits_right_after
  in
  (* Check that the initial frozen deposits has not changed *)
  let* initial_frozen_deposits_before =
    Context.Delegate.initial_frozen_deposits (B blk_a) baker1
  in
  let* initial_frozen_deposits_after =
    Context.Delegate.initial_frozen_deposits (B blk_final) baker1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      initial_frozen_deposits_before
      initial_frozen_deposits_after
  in
  (* Check that the frozen deposits have been slashed at the end of the cycle. *)
  let* blk_eoc, end_cycle_metadata, _next_cycle =
    Block.bake_until_n_cycle_end_with_metadata
      ~policy:(By_account baker2)
      2
      blk_final
  in
  let end_cycle_metadata =
    Option.value_f ~default:(fun () -> assert false) end_cycle_metadata
  in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_eoc) baker1
  in
  let autostaked = Block.autostaked baker1 end_cycle_metadata in
  let Q.{num; den} = Percentage.to_q p in
  let expected_frozen_deposits_after =
    Test_tez.(
      frozen_deposits_before
      -! (initial_frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
      +! autostaked)
  in
  Assert.equal_tez
    ~loc:__LOC__
    frozen_deposits_after
    expected_frozen_deposits_after

(* auxiliary function used in [double_attestation] *)
let order_attestations ~correct_order op1 op2 =
  let oph1 = Operation.hash op1 in
  let oph2 = Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

(* auxiliary function used in
   [test_valid_double_baking_followed_by_double_attesting] and
   [test_valid_double_attesting_followed_by_double_baking] *)
let double_attestation ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_attestation ctxt e1 e2

let test_valid_double_baking_followed_by_double_attesting () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init2 ~consensus_threshold:0 () in
  let* baker1, baker2 = Context.get_first_different_bakers (B genesis) in
  let* b = Block.bake genesis in
  let* blk_a, blk_b = block_fork ~policy:(By_account baker1) contracts b in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_a) baker1
  in
  let* initial_frozen_deposits_before =
    Context.Delegate.initial_frozen_deposits (B blk_a) baker1
  in
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  let* blk_with_db_evidence =
    Block.bake ~policy:(By_account baker2) ~operation blk_a
  in
  let* e1, e2 = Context.get_first_different_attesters (B blk_a) in
  let delegate =
    if Signature.Public_key_hash.( = ) e1.delegate baker1 then e1.delegate
    else e2.delegate
  in
  let* attestation_a = Op.raw_attestation ~delegate blk_a in
  let* attestation_b = Op.raw_attestation ~delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* blk_final =
    Block.bake ~policy:(By_account baker2) ~operation blk_with_db_evidence
  in
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B blk_final) baker1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_before
      frozen_deposits_right_after
  in
  let* blk_eoc, metadata, _ =
    Block.bake_until_n_cycle_end_with_metadata
      ~policy:(By_account baker2)
      2
      blk_final
  in
  let metadata = Option.value_f ~default:(fun () -> assert false) metadata in
  let autostaked = Block.autostaked baker1 metadata in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_eoc) baker1
  in
  let* csts = Context.get_constants (B genesis) in
  let p_de =
    csts.parametric.percentage_of_frozen_deposits_slashed_per_double_attestation
  in
  let p_db =
    csts.parametric.percentage_of_frozen_deposits_slashed_per_double_baking
  in
  let p = Percentage.add_bounded p_de p_db in
  let Q.{num; den} = Percentage.to_q p in
  let expected_frozen_deposits_after =
    Test_tez.(
      frozen_deposits_before
      -! (initial_frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
      +! autostaked)
  in
  (* Both slashings are computed on the initial amount of frozen deposits so
     the percentages are additive, not multiplicative. *)
  Assert.equal_tez
    ~loc:__LOC__
    expected_frozen_deposits_after
    frozen_deposits_after

(* auxiliary function used in [test_valid_double_attesting_followed_by_double_baking] *)
let block_fork_diff b =
  let open Lwt_result_syntax in
  let* baker_1, baker_2 = Context.get_first_different_bakers (B b) in
  let* blk_a = Block.bake ~policy:(By_account baker_1) b in
  let* blk_b = Block.bake ~policy:(By_account baker_2) b in
  return (blk_a, blk_b)

let test_valid_double_attesting_followed_by_double_baking () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init2 ~consensus_threshold:0 () in
  let* baker1, baker2 = Context.get_first_different_bakers (B genesis) in
  let* blk_1, blk_2 = block_fork_diff genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_a) baker1
  in
  let* initial_frozen_deposits_before =
    Context.Delegate.initial_frozen_deposits (B blk_a) baker1
  in
  let* e1, e2 = Context.get_first_different_attesters (B blk_a) in
  let delegate =
    if Signature.Public_key_hash.( = ) e1.delegate baker1 then e1.delegate
    else e2.delegate
  in
  let* attestation_a = Op.raw_attestation ~delegate blk_a in
  let* attestation_b = Op.raw_attestation ~delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* blk_with_de_evidence =
    Block.bake ~policy:(By_account baker2) ~operation blk_a
  in
  let* blk_a, blk_b = block_fork ~policy:(By_account baker1) contracts blk_1 in
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  let* blk_with_db_evidence =
    Block.bake ~policy:(By_account baker2) ~operation blk_with_de_evidence
  in
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B blk_with_db_evidence) baker1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_before
      frozen_deposits_right_after
  in
  let* blk_eoc, end_cycle_metadata, _ =
    Block.bake_until_n_cycle_end_with_metadata
      ~policy:(By_account baker2)
      2
      blk_with_db_evidence
  in
  let end_cycle_metadata =
    Option.value_f ~default:(fun () -> assert false) end_cycle_metadata
  in
  let autostaked = Block.autostaked baker1 end_cycle_metadata in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_eoc) baker1
  in
  let* csts = Context.get_constants (B genesis) in
  let p_de =
    csts.parametric.percentage_of_frozen_deposits_slashed_per_double_attestation
  in
  let p_db =
    csts.parametric.percentage_of_frozen_deposits_slashed_per_double_baking
  in
  let p = Percentage.add_bounded p_de p_db in
  let Q.{num; den} = Percentage.to_q p in
  let expected_frozen_deposits_after =
    Test_tez.(
      frozen_deposits_before
      -! (initial_frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
      +! autostaked)
  in
  (* Both slashings are computed on the initial amount of frozen deposits so
     the percentages are additive, not multiplicative. *)
  Assert.equal_tez
    ~loc:__LOC__
    expected_frozen_deposits_after
    frozen_deposits_after

(** Test that the payload producer of the block containing a double
   baking evidence (and not the block producer, if different) receives
   the reward. *)
let test_payload_producer_gets_evidence_rewards () =
  let open Lwt_result_syntax in
  let* genesis, contracts =
    Context.init_n ~consensus_threshold:0 ~consensus_committee_size:64 10 ()
  in
  let* c = Context.get_constants (B genesis) in
  let p =
    c.parametric.percentage_of_frozen_deposits_slashed_per_double_baking
  in
  let* baking_reward_fixed_portion =
    Context.get_baking_reward_fixed_portion (B genesis)
  in
  let* baker1, baker2 = Context.get_first_different_bakers (B genesis) in
  let c1_c2 =
    match contracts with c1 :: c2 :: _ -> (c1, c2) | _ -> assert false
  in
  let* b1, b2 = block_fork ~policy:(By_account baker1) c1_c2 genesis in
  double_baking (B b1) b1.header b2.header |> fun db_evidence ->
  let* b_with_evidence =
    Block.bake ~policy:(By_account baker2) ~operation:db_evidence b1
  in
  let* attesters = Context.get_attesters (B b_with_evidence) in
  let* preattesters =
    List.map_es
      (function
        | {Plugin.RPC.Validators.delegate; slots; _} -> return (delegate, slots))
      attesters
  in
  let* preattestations =
    List.map_ep
      (fun (attester, _slots) ->
        Op.preattestation ~delegate:attester b_with_evidence)
      preattesters
  in
  let* b' =
    Block.bake
      ~payload_round:(Some Round.zero)
      ~locked_round:(Some Round.zero)
      ~policy:(By_account baker1)
      ~operations:(preattestations @ [db_evidence])
      b1
  in
  (* The denunciation happened but no slashing nor reward happened yet. *)
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B b1) baker1
  in
  let* initial_frozen_deposits_before =
    Context.Delegate.initial_frozen_deposits (B b1) baker1
  in
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B b') baker1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_right_after
      frozen_deposits_before
  in
  let* full_balance = Context.Delegate.full_balance (B b1) baker2 in
  let expected_reward_right_after = baking_reward_fixed_portion in
  let* full_balance_with_rewards_right_after =
    Context.Delegate.full_balance (B b') baker2
  in
  let real_reward_right_after =
    Test_tez.(full_balance_with_rewards_right_after -! full_balance)
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      expected_reward_right_after
      real_reward_right_after
  in
  (* Slashing and rewarding happen at the end of the cycle. *)
  let* b', end_cycle_metadata, _ =
    Block.bake_until_n_cycle_end_with_metadata ~policy:(By_account baker2) 2 b'
  in
  let end_cycle_metadata =
    Option.value_f ~default:(fun () -> assert false) end_cycle_metadata
  in
  let autostaked = Block.autostaked baker1 end_cycle_metadata in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B b') baker1
  in
  let Q.{num; den} = Percentage.to_q p in
  let expected_frozen_deposits_after =
    Test_tez.(
      frozen_deposits_before
      -! (initial_frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
      +! autostaked)
  in
  (* the frozen deposits of the double-signer [baker1] are slashed *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_after
      expected_frozen_deposits_after
  in
  let slashed_amount =
    Test_tez.(frozen_deposits_before -! (frozen_deposits_after -! autostaked))
  in
  (* [baker2] included the double baking evidence in [b_with_evidence]
     and so it receives the reward for the evidence included in [b']
     (besides the reward for proposing the payload). *)
  let divider =
    Int64.add
      2L
      (Int64.of_int
         c.parametric.adaptive_issuance.global_limit_of_staking_over_baking)
  in
  let evidence_reward = Test_tez.(slashed_amount /! divider) in
  let baked_blocks =
    Int64.of_int
      (Int32.to_int b'.header.shell.level - Int32.to_int b1.header.shell.level)
  in
  let expected_reward =
    Test_tez.((baking_reward_fixed_portion *! baked_blocks) +! evidence_reward)
  in
  let* full_balance_with_rewards =
    Context.Delegate.full_balance (B b') baker2
  in
  let real_reward = Test_tez.(full_balance_with_rewards -! full_balance) in
  let* () = Assert.equal_tez ~loc:__LOC__ expected_reward real_reward in
  (* [baker1] did not produce the payload, it does not receive the reward for the
     evidence *)
  let* full_balance_at_b1 = Context.Delegate.full_balance (B b1) baker1 in
  let* full_balance_at_b' = Context.Delegate.full_balance (B b') baker1 in
  Assert.equal_tez
    ~loc:__LOC__
    full_balance_at_b'
    Test_tez.(full_balance_at_b1 -! slashed_amount)

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that a double baking operation fails if it exposes the same two
    blocks. *)
let test_same_blocks () =
  let open Lwt_result_syntax in
  let* b, _contracts = Context.init2 () in
  let* ba = Block.bake b in
  double_baking (B ba) ba.header ba.header |> fun operation ->
  let*! res = Block.bake ~operation ba in
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that an double baking operation that is invalid due to
   incorrect ordering of the block headers fails. *)
let test_incorrect_order () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init2 ~consensus_threshold:0 () in
  let* blk_a, blk_b = block_fork ~policy:(By_round 0) contracts genesis in
  double_baking (B genesis) ~correct_order:false blk_a.header blk_b.header
  |> fun operation ->
  let*! res = Block.bake ~operation genesis in
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that a double baking operation exposing two blocks with
    different levels fails. *)
let test_different_levels () =
  let open Lwt_result_syntax in
  let* b, contracts = Context.init2 ~consensus_threshold:0 () in
  let* blk_a, blk_b = block_fork ~policy:(By_round 0) contracts b in
  let* blk_b_2 = Block.bake blk_b in
  double_baking (B blk_a) blk_a.header blk_b_2.header |> fun operation ->
  let*! res = Block.bake ~operation blk_a in
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_double_baking_evidence _ -> true
      | _ -> false)

(** Check that a double baking operation exposing two yet-to-be-baked
    blocks fails. *)
let test_too_early_double_baking_evidence () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init2 ~consensus_threshold:0 () in
  let* b = Block.bake_until_cycle_end genesis in
  let* blk_a, blk_b = block_fork ~policy:(By_round 0) contracts b in
  double_baking (B b) blk_a.header blk_b.header |> fun operation ->
  let*! res = Block.bake ~operation genesis in
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Too_early_denunciation
          {kind = Misbehaviour.Double_baking; _} ->
          true
      | _ -> false)

(** Check that after [max_slashing_period * blocks_per_cycle + 1] blocks -- corresponding to 2 cycles
   --, it is not possible to create a double baking operation anymore. *)
let test_too_late_double_baking_evidence () =
  let open Lwt_result_syntax in
  let max_slashing_period = Constants.max_slashing_period in
  let* b, contracts = Context.init2 ~consensus_threshold:0 () in
  let* blk_a, blk_b = block_fork ~policy:(By_round 0) contracts b in
  let* blk = Block.bake_until_n_cycle_end max_slashing_period blk_a in
  double_baking (B blk) blk_a.header blk_b.header |> fun operation ->
  let*! res = Block.bake ~operation blk in
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Outdated_denunciation
          {kind = Misbehaviour.Double_baking; _} ->
          true
      | _ -> false)

(** Check that before [blocks_per_cycle] blocks
   -- corresponding to 2 cycles --, it is still possible to create a
   double baking operation. *)
let test_just_in_time_double_baking_evidence () =
  let open Lwt_result_syntax in
  let* b, contracts = Context.init2 ~consensus_threshold:0 () in
  let* Constants.{parametric = {blocks_per_cycle; _}; _} =
    Context.get_constants (B b)
  in
  let* blk_a, blk_b = block_fork ~policy:(By_round 0) contracts b in
  let* blk = Block.bake_until_cycle_end blk_a in
  let* blk = Block.bake_n Int32.(sub blocks_per_cycle 2l |> to_int) blk in
  let operation = double_baking (B blk) blk_a.header blk_b.header in
  (* We include the denunciation in the previous to last block of the
     cycle. *)
  let* (_ : Block.t) = Block.bake ~operation blk in
  return_unit

(** Check that an invalid double baking evidence that exposes two
    block baking with same level made by different bakers fails. *)
let test_different_delegates () =
  let open Lwt_result_syntax in
  let* b, _contracts = Context.init2 () in
  let* baker_1, baker_2 = Context.get_first_different_bakers (B b) in
  let* blk_a = Block.bake ~policy:(By_account baker_1) b in
  let* blk_b = Block.bake ~policy:(By_account baker_2) b in
  double_baking (B blk_a) blk_a.header blk_b.header |> fun operation ->
  let*! e = Block.bake ~operation blk_a in
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
  let open Lwt_result_syntax in
  let header_custom_signer baker baker_2 timestamp b =
    let* header =
      Block.Forge.forge_header ~policy:(By_account baker) ~timestamp b
    in
    Block.Forge.set_baker baker_2 header |> Block.Forge.sign_header
  in
  let* b, _contracts = Context.init2 () in
  let* baker_1, baker_2 = Context.get_first_different_bakers (B b) in
  let* blk_a = Block.bake ~policy:(By_account baker_1) b in
  let ts = Timestamp.of_seconds_string (Int64.to_string 10L) in
  match ts with
  | None -> assert false
  | Some ts ->
      let* header_b = header_custom_signer baker_1 baker_2 ts b in
      double_baking (B blk_a) blk_a.header header_b |> fun operation ->
      let*! e = Block.bake ~operation blk_a in
      Assert.proto_error_with_info ~loc:__LOC__ e "Invalid block signature"

(** an evidence can only be accepted once (this also means that the
   same evidence doesn't lead to slashing the offender twice) *)
let test_double_evidence () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2, _c3) = Context.init3 ~consensus_threshold:0 () in
  let* blk_a, blk_b = block_fork (c1, c2) blk in
  let* blk = Block.bake_until_cycle_end blk_a in
  double_baking (B blk) blk_a.header blk_b.header |> fun evidence ->
  let*! e = Block.bake ~operations:[evidence; evidence] blk in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
        | Validate_errors.Anonymous.Conflicting_denunciation
            {kind = Misbehaviour.Double_baking; _} ->
            true
        | _ -> false)
  in
  let* blk = Block.bake ~operation:evidence blk in
  double_baking (B blk) blk_b.header blk_a.header |> fun evidence ->
  let*! e = Block.bake ~operation:evidence blk in
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
      "double baking followed by double attesting"
      `Quick
      test_valid_double_baking_followed_by_double_attesting;
    Tztest.tztest
      "double attesting followed by double baking"
      `Quick
      test_valid_double_attesting_followed_by_double_baking;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("double baking", tests)]
  |> Lwt_main.run
