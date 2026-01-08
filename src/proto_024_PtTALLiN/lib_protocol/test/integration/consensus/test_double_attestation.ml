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
    Component:    Protocol (double attestation)
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_double_attestation.ml
    Subject:      Double attestation evidence operation may happen when an
                  attester attested two different blocks on the same level.
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let block_fork ?excluding b =
  let open Lwt_result_syntax in
  let* baker_1, baker_2 = Context.get_first_different_bakers ?excluding (B b) in
  let* blk_a = Block.bake ~policy:(By_account baker_1) b in
  let+ blk_b = Block.bake ~policy:(By_account baker_2) b in
  (blk_a, blk_b)

(* Checks that there is exactly one denunciation for the given delegate *)
let check_denunciations ~loc b delegate duplicate_op =
  let open Lwt_result_syntax in
  let* denunciations = Context.get_denunciations (B b) in
  match denunciations with
  | [(d, item)] ->
      let* () = Assert.equal_pkh ~loc d delegate in
      Slashing_helpers.Misbehaviour_repr.check_from_duplicate_operation
        ~loc
        item.misbehaviour
        duplicate_op
  | _ -> Test.fail ~__LOC__:loc "expected exactly one denunciation"

let check_empty_denunciations b =
  let open Lwt_result_syntax in
  let* denunciations = Context.get_denunciations (B b) in
  match denunciations with [] -> return_unit | _ -> assert false

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

let order_attestations ~correct_order op1 op2 =
  let oph1 = Operation.hash op1 in
  let oph2 = Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

let double_attestation ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_attestation ctxt e1 e2

let double_preattestation ctxt ?(correct_order = true) op1 op2 =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_preattestation ctxt e1 e2

(** This test verifies that when a "cheater" double attests and
    doesn't have enough tokens to re-freeze of full deposit, we only
    freeze what we can (i.e. the remaining balance) but we check that
    another denunciation will slash 50% of the initial (expected) amount
    of the deposit. *)

(** Simple scenario where two attestations are made from the same
    delegate and exposed by a double_attestation operation. Also verify
    that punishment is operated. *)
let test_valid_double_attestation_evidence () =
  let open Lwt_result_wrap_syntax in
  let constants =
    {
      Default_parameters.constants_test with
      issuance_weights =
        {
          Default_parameters.constants_test.issuance_weights with
          base_total_issued_per_minute = Tez.zero;
        };
      consensus_threshold_size = 0;
    }
  in
  let* genesis, _contracts = Context.init_with_constants2 constants in
  let* blk_1, blk_2 = block_fork genesis in
  (* from blk_1 we bake blk_a and from blk_2 we bake blk_b so that
     the same delegate attests blk_a and blk_b and these 2 form
     a valid double attestation evidence;
     - note that we cannot have double attestation evidence
       at the level of blk_1, blk_2 because both have as parent genesis
       and so the attestations are identical because the blocks blk_1, blk_2
       are identical. *)
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* {Context.delegate; _} = Context.get_attester (B blk_a) in
  let* attestation_a = Op.raw_attestation ~manager_pkh:delegate blk_a in
  let* attestation_b = Op.raw_attestation ~manager_pkh:delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* bakers = Context.get_bakers (B blk_a) in
  let baker = Context.get_first_different_baker delegate bakers in
  let* full_balance = Context.Delegate.full_balance (B blk_a) baker in
  let* () = check_empty_denunciations blk_a in
  let* blk_final = Block.bake ~policy:(By_account baker) ~operation blk_a in
  let* () = check_denunciations ~loc:__LOC__ blk_final delegate attestation_a in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_a) delegate
  in
  let* initial_frozen_deposits =
    Context.Delegate.initial_frozen_deposits (B blk_final) delegate
  in
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B blk_final) delegate
  in
  (* Check that the initial frozen deposits have not changed *)
  let* () =
    Assert.equal_tez ~loc:__LOC__ initial_frozen_deposits frozen_deposits_before
  in
  (* Similarly for current frozen deposits because slashing is deferred to the
     end of the cycle. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_right_after
      frozen_deposits_before
  in
  (* Check that the right portion of the frozen deposits is slashed at
     the end of the cycle. *)
  let* blk_eoc, metadata, _ =
    Block.bake_until_n_cycle_end_with_metadata
      ~policy:(By_account baker)
      2
      blk_final
  in
  let metadata = Option.value_f ~default:(fun () -> assert false) metadata in
  let autostaked = Block.autostaked delegate metadata in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_eoc) delegate
  in
  let frozen_deposits_after =
    Tez_helpers.(frozen_deposits_after -! autostaked)
  in
  let* slashing_pct =
    Slashing_helpers.slashing_percentage
      (Slashing_helpers.Misbehaviour_repr.from_duplicate_operation
         attestation_a)
      ~block_before_slash:blk_a
      ~all_culprits:[delegate]
  in
  let one_minus_p = Percentage.neg slashing_pct in
  let {Q.num; den} = Percentage.to_q one_minus_p in
  let expected_frozen_deposits_after =
    Tez_helpers.(frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      expected_frozen_deposits_after
      frozen_deposits_after
  in
  (* Check that [baker] is rewarded with:
     - baking_reward_fixed_portion for baking and,
     - half of the frozen_deposits for including the evidence *)
  let*?@ baking_reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      constants
      ~reward_kind:Baking_reward_fixed_portion
  in
  let divider =
    Int64.add
      2L
      (Int64.of_int
         constants.adaptive_issuance.global_limit_of_staking_over_baking)
  in
  let evidence_reward =
    Tez_helpers.((frozen_deposits_before -! frozen_deposits_after) /! divider)
  in
  let expected_reward = Tez_helpers.(baking_reward +! evidence_reward) in
  let* full_balance_with_rewards =
    Context.Delegate.full_balance (B blk_eoc) baker
  in
  let real_reward = Tez_helpers.(full_balance_with_rewards -! full_balance) in
  Log.info
    "@[<v>frozen_deposits_before = %a@,\
     frozen_deposits_after = %a@,\
     baking_reward = %a@,\
     divider = %Ld@,\
     evidence_reward = %a@,\
     expected_reward = %a@,\
     real_reward = %a@]"
    Tez.pp
    frozen_deposits_before
    Tez.pp
    frozen_deposits_after
    Tez.pp
    baking_reward
    divider
    Tez.pp
    evidence_reward
    Tez.pp
    expected_reward
    Tez.pp
    real_reward ;
  Assert.equal_tez ~loc:__LOC__ expected_reward real_reward

(** Check that a double (pre)attestation evidence with equivalent
    attestations but on different branches succeeds. *)
let test_different_branch () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* blk = Block.bake genesis in
  let* {Context.delegate = manager_pkh; _} = Context.get_attester (B blk) in
  let* attestation_a = Op.raw_attestation ~manager_pkh blk in
  let* attestation_b =
    Op.raw_attestation ~branch:Block_hash.zero ~manager_pkh blk
  in
  let operation = double_attestation (B blk) attestation_a attestation_b in
  let* _blk = Block.bake ~operation blk in
  let* preattestation_a = Op.raw_preattestation ~manager_pkh blk in
  let* preattestation_b =
    Op.raw_preattestation ~branch:Block_hash.zero ~manager_pkh blk
  in
  let operation =
    double_preattestation (B blk) preattestation_a preattestation_b
  in
  let* _blk = Block.bake ~operation blk in
  return_unit

(** Check that a double (pre)attestation evidence fails when the
    operations have distinct slots (that both belong to the delegate)
    and are otherwise identical. *)
let test_different_slots () =
  let open Lwt_result_wrap_syntax in
  (* TODO ABAAB: doesn't work with ABAAB *)
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* blk = Block.bake genesis in
  let* attesters = Context.get_attesters (B blk) in
  let* csts = Context.get_constants (B blk) in
  let committee_size = csts.parametric.consensus_committee_size in
  let consensus_pkh, round1, round2 =
    (* Find an attester with more than 1 slot. *)
    WithExceptions.Option.get
      ~loc:__LOC__
      (List.find_map
         (fun (attester : RPC.Validators.delegate) ->
           match attester.rounds with
           | round1 :: round2 :: _ ->
               Some (attester.consensus_key, round1, round2)
           | _ -> None)
         attesters)
  in
  let*?@ slot1 = Round.to_slot ~committee_size round1 in
  let*?@ slot2 = Round.to_slot ~committee_size round2 in
  let attesting_slot1 = {Op.slot = slot1; consensus_pkh} in
  let attesting_slot2 = {Op.slot = slot2; consensus_pkh} in
  let* attestation1 = Op.raw_attestation ~attesting_slot:attesting_slot1 blk in
  let* attestation2 = Op.raw_attestation ~attesting_slot:attesting_slot2 blk in
  let double_attestation =
    double_attestation (B blk) attestation1 attestation2
  in
  let* preattestation1 =
    Op.raw_preattestation ~attesting_slot:attesting_slot1 blk
  in
  let* preattestation2 =
    Op.raw_preattestation ~attesting_slot:attesting_slot2 blk
  in
  let double_preattestation =
    double_preattestation (B blk) preattestation1 preattestation2
  in
  let*! res = Block.bake ~operation:double_attestation blk in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_denunciation _ -> true
      | _ -> false)
  in
  let*! res = Block.bake ~operation:double_preattestation blk in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_denunciation _ -> true
      | _ -> false)
  in
  return_unit

(** Say a delegate double-attests twice and say the 2 evidences are timely
   included. Then the delegate can no longer bake. *)
let test_two_double_attestation_evidences_leadsto_no_bake () =
  let open Lwt_result_wrap_syntax in
  let issuance_weights =
    {
      Default_parameters.constants_test.issuance_weights with
      base_total_issued_per_minute = Tez.zero;
    }
  in
  let* genesis, _contracts =
    Context.init3 ~consensus_threshold_size:0 ~issuance_weights ()
  in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* {Context.delegate; _} = Context.get_attester (B blk_a) in
  let* attestation_a = Op.raw_attestation ~manager_pkh:delegate blk_a in
  let* attestation_b = Op.raw_attestation ~manager_pkh:delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* bakers = Context.get_bakers (B blk_a) in
  let baker = Context.get_first_different_baker delegate bakers in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_a) delegate
  in
  let* blk_with_evidence1 =
    Block.bake ~policy:(By_account baker) ~operation blk_a
  in
  let* blk_30, blk_40 = block_fork ~excluding:[delegate] blk_with_evidence1 in
  let* blk_3 = Block.bake ~policy:(Excluding [delegate]) blk_30 in
  let* blk_4 = Block.bake ~policy:(Excluding [delegate]) blk_40 in
  let* attestation_3 = Op.raw_attestation ~manager_pkh:delegate blk_3 in
  let* attestation_4 = Op.raw_attestation ~manager_pkh:delegate blk_4 in
  let operation =
    double_attestation (B blk_with_evidence1) attestation_3 attestation_4
  in
  let* blk_with_evidence2, (_blk_metadata, operations_recpts) =
    Block.bake_with_metadata ~policy:(By_account baker) ~operation blk_3
  in
  Log.info "Baked block with double attestation evidence" ;
  let rcpt =
    List.find
      (fun (rcpt : operation_receipt) ->
        match rcpt with
        | Operation_metadata
            {
              contents =
                Apply_results.Single_result
                  (Apply_results.Double_consensus_operation_evidence_result
                     {punished_delegate; _});
            } ->
            Signature.Public_key_hash.(punished_delegate = delegate)
        | _ -> false)
      operations_recpts
  in
  let* _ = Assert.get_some ~loc:__LOC__ rcpt in
  (* Check that the frozen deposits haven't changed yet. *)
  let* frozen_deposits_right_after =
    Context.Delegate.current_frozen_deposits (B blk_with_evidence2) delegate
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      frozen_deposits_before
      frozen_deposits_right_after
  in
  let* is_forbidden =
    Context.Delegate.is_forbidden (B blk_with_evidence2) delegate
  in
  let* () = Assert.is_true ~loc:__LOC__ is_forbidden in
  let*! b = Block.bake ~policy:(By_account delegate) blk_with_evidence2 in
  (* a delegate with 0 frozen deposits cannot bake *)
  let* () =
    Assert.proto_error ~loc:__LOC__ b (function
      | Validate_errors.Consensus.Forbidden_delegate _ -> true
      | _ -> false)
  in
  (* Bake until the slashing and check the frozen deposits. *)
  let* b =
    Block.bake_until_n_cycle_end
      ~policy:(By_account baker)
      (Constants.slashing_delay + 1)
      blk_with_evidence2
  in
  let* slashing_pct1 =
    Slashing_helpers.slashing_percentage
      (Slashing_helpers.Misbehaviour_repr.from_duplicate_operation
         attestation_a)
      ~block_before_slash:blk_a
      ~all_culprits:[delegate]
  in
  let* slashing_pct2 =
    Slashing_helpers.slashing_percentage
      (Slashing_helpers.Misbehaviour_repr.from_duplicate_operation
         attestation_3)
      ~block_before_slash:blk_3
      ~all_culprits:[delegate]
  in
  let one_minus_p =
    Percentage.(neg (add_bounded slashing_pct1 slashing_pct2))
  in
  let {Q.num; den} = Percentage.to_q one_minus_p in
  let expected_frozen_deposits_after =
    Tez_helpers.(frozen_deposits_before *! Z.to_int64 num /! Z.to_int64 den)
  in
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B b) delegate
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      expected_frozen_deposits_after
      frozen_deposits_after
  in
  return_unit

(** Say a delegate double-attests twice in a cycle,
    and say the 2 evidences are included in different (valid) cycles.
    Then the delegate is forbidden and can no longer bake. *)
let test_two_double_attestation_evidences_staggered () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init3 ~consensus_threshold_size:0 () in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* {Context.delegate; _} = Context.get_attester (B blk_a) in
  let* attestation_a = Op.raw_attestation ~manager_pkh:delegate blk_a in
  let* attestation_b = Op.raw_attestation ~manager_pkh:delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* bakers = Context.get_bakers (B blk_a) in
  let baker = Context.get_first_different_baker delegate bakers in
  let* (_full_balance : Tez.t) =
    Context.Delegate.full_balance (B blk_a) baker
  in
  let* blk_with_evidence1 =
    Block.bake ~policy:(By_account baker) ~operation blk_a
  in

  let* blk_30, blk_40 = block_fork ~excluding:[delegate] blk_with_evidence1 in
  let* blk_3 = Block.bake ~policy:(Excluding [delegate]) blk_30 in
  let* blk_4 = Block.bake ~policy:(Excluding [delegate]) blk_40 in
  let* attestation_3 = Op.raw_attestation ~manager_pkh:delegate blk_3 in
  let* attestation_4 = Op.raw_attestation ~manager_pkh:delegate blk_4 in
  let operation_evidence2 =
    double_attestation (B blk_with_evidence1) attestation_3 attestation_4
  in

  let* operation =
    Adaptive_issuance_helpers.stake
      (B blk_with_evidence1)
      (Protocol.Alpha_context.Contract.Implicit delegate)
      (Tez.of_mutez_exn 1_000_000_000L)
  in
  let* blk_with_stake =
    Block.bake ~policy:(By_account baker) ~operation blk_with_evidence1
  in
  let* blk_new_cycle, _metadata, _ =
    Block.bake_until_cycle_end_with_metadata
      ~policy:(By_account baker)
      blk_with_stake
  in
  let* blk_with_evidence2 =
    Block.bake
      ~policy:(By_account baker)
      ~operation:operation_evidence2
      blk_new_cycle
  in
  (* Check that NOT all the frozen deposits are slashed *)
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_with_evidence2) delegate
  in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_new_cycle) delegate
  in
  Log.info
    "Tez before slashing: %a @.After slashing %a @."
    Tez.pp
    frozen_deposits_before
    Tez.pp
    frozen_deposits_after ;
  let* () = Assert.not_equal_tez ~loc:__LOC__ Tez.zero frozen_deposits_after in
  let* is_forbidden =
    Context.Delegate.is_forbidden (B blk_with_evidence2) delegate
  in
  let* () = Assert.is_true ~loc:__LOC__ is_forbidden in
  let*! b = Block.bake ~policy:(By_account delegate) blk_with_evidence2 in
  (* A forbidden delegate cannot bake *)
  Assert.proto_error ~loc:__LOC__ b (function
    | Validate_errors.Consensus.Forbidden_delegate _ -> true
    | _ -> false)

(** Say a delegate double-attests twice in two consecutive cycles,
    and say the 2 evidences are timely included. Then the delegate
    is forbidden and can no longer bake. *)
let test_two_double_attestation_evidences_consecutive_cycles () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init3 ~consensus_threshold_size:0 () in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* {Context.delegate; _} = Context.get_attester (B blk_a) in
  let* attestation_a = Op.raw_attestation ~manager_pkh:delegate blk_a in
  let* attestation_b = Op.raw_attestation ~manager_pkh:delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let* bakers = Context.get_bakers (B blk_a) in
  let baker = Context.get_first_different_baker delegate bakers in
  let* (_full_balance : Tez.t) =
    Context.Delegate.full_balance (B blk_a) baker
  in
  let* blk_with_evidence1 =
    Block.bake ~policy:(By_account baker) ~operation blk_a
  in
  let* operation =
    Adaptive_issuance_helpers.stake
      (B blk_with_evidence1)
      (Protocol.Alpha_context.Contract.Implicit delegate)
      (Tez.of_mutez_exn 1_000_000_000L)
  in
  let* blk_with_stake =
    Block.bake ~policy:(By_account baker) ~operation blk_with_evidence1
  in
  let* blk_new_cycle =
    Block.bake_until_cycle_end ~policy:(Excluding [delegate]) blk_with_stake
  in
  let* blk_30, blk_40 = block_fork ~excluding:[delegate] blk_new_cycle in
  let* blk_3 = Block.bake ~policy:(Excluding [delegate]) blk_30 in
  let* blk_4 = Block.bake ~policy:(Excluding [delegate]) blk_40 in
  let* attestation_3 = Op.raw_attestation ~manager_pkh:delegate blk_3 in
  let* attestation_4 = Op.raw_attestation ~manager_pkh:delegate blk_4 in
  let operation =
    double_attestation (B blk_new_cycle) attestation_3 attestation_4
  in
  let* blk_with_evidence2 =
    Block.bake ~policy:(By_account baker) ~operation blk_3
  in
  (* Check that NOT all the frozen deposits are slashed *)
  let* frozen_deposits_after =
    Context.Delegate.current_frozen_deposits (B blk_with_evidence2) delegate
  in
  let* frozen_deposits_before =
    Context.Delegate.current_frozen_deposits (B blk_3) delegate
  in
  Log.info
    "Tez before slashing: %a @.After slashing %a @."
    Tez.pp
    frozen_deposits_before
    Tez.pp
    frozen_deposits_after ;
  let* () = Assert.not_equal_tez ~loc:__LOC__ Tez.zero frozen_deposits_after in
  let* is_forbidden =
    Context.Delegate.is_forbidden (B blk_with_evidence2) delegate
  in
  let* () = Assert.is_true ~loc:__LOC__ is_forbidden in
  let*! b = Block.bake ~policy:(By_account delegate) blk_with_evidence2 in
  (* A forbidden delegate cannot bake *)
  Assert.proto_error ~loc:__LOC__ b (function
    | Validate_errors.Consensus.Forbidden_delegate _ -> true
    | _ -> false)

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that an invalid double attestation operation that exposes a
      valid attestation fails. *)
let test_invalid_double_attestation () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n ~consensus_threshold_size:0 10 () in
  let* b = Block.bake genesis in
  let* attestation = Op.raw_attestation b in
  let* b = Block.bake ~operation:(Operation.pack attestation) b in
  Op.double_attestation (B b) attestation attestation |> fun operation ->
  let*! res = Block.bake ~operation b in
  Assert.proto_error ~loc:__LOC__ res (function
    | Validate_errors.Anonymous.Invalid_denunciation
        Misbehaviour.Double_attesting ->
        true
    | _ -> false)

(** Check that an double attestation operation that is invalid due to
   incorrect ordering of the attestations fails. *)
let test_invalid_double_attestation_variant () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* b = Block.bake_until_cycle_end genesis in
  let* blk_1, blk_2 = block_fork b in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* attestation_a = Op.raw_attestation blk_a in
  let* attestation_b = Op.raw_attestation blk_b in
  double_attestation
    (B genesis)
    ~correct_order:false
    attestation_a
    attestation_b
  |> fun operation ->
  let*! res = Block.bake ~operation genesis in
  Assert.proto_error ~loc:__LOC__ res (function
    | Validate_errors.Anonymous.Invalid_denunciation
        Misbehaviour.Double_attesting ->
        true
    | _ -> false)

let invalid_denunciation kind = function
  | Validate_errors.Anonymous.Invalid_denunciation kind' ->
      Misbehaviour.equal_kind kind kind'
  | _ -> false

(** Check that a double attestation operation fails if the same slot
    is duplicated in the committee of one of the evidences. *)
let test_invalid_double_attestation_duplicate_in_committee () =
  let open Lwt_result_wrap_syntax in
  let* _genesis, block =
    Test_aggregate.init_genesis_with_some_bls_accounts
      ~aggregate_attestation:true
      ()
  in
  let* b = Block.bake_until_cycle_end block in
  let* blk_1, blk_2 = block_fork b in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* attesting_slot =
    Op.get_attesting_slot_with_bls_key ~attested_block:blk_a
  in
  let* op1 = Op.raw_attestation ~attesting_slot blk_a in
  let* op2 =
    Op.raw_attestations_aggregate
      ~committee:[attesting_slot; attesting_slot]
      blk_b
  in
  let slot = attesting_slot.slot in
  let op =
    let contents =
      if Operation_hash.(Operation.hash op1 < Operation.hash op2) then
        Single (Double_consensus_operation_evidence {slot; op1; op2})
      else
        Single
          (Double_consensus_operation_evidence {slot; op1 = op2; op2 = op1})
    in
    let branch = Context.branch (B blk_a) in
    {
      shell = {branch};
      protocol_data = Operation_data {contents; signature = None};
    }
  in
  let* () =
    Op.check_validation_and_application_all_modes
      ~loc:__LOC__
      ~error:(invalid_denunciation Double_attesting)
      ~predecessor:blk_a
      op
  in
  (* Also check with duplicate slots with different dal contents *)
  let* op2 =
    let number_of_slots =
      Default_parameters.constants_test.dal.number_of_slots
    in
    let*?@ slot_index = Dal.Slot_index.of_int ~number_of_slots 3 in
    let dal_content =
      {attestation = Dal.Attestation.(commit empty slot_index)}
    in
    Op.raw_attestations_aggregate
      ~committee_with_dal:
        [(attesting_slot, None); (attesting_slot, Some dal_content)]
      blk_b
  in
  let op =
    let contents =
      if Operation_hash.(Operation.hash op1 < Operation.hash op2) then
        Single (Double_consensus_operation_evidence {slot; op1; op2})
      else
        Single
          (Double_consensus_operation_evidence {slot; op1 = op2; op2 = op1})
    in
    let branch = Context.branch (B blk_a) in
    {
      shell = {branch};
      protocol_data = Operation_data {contents; signature = None};
    }
  in
  Op.check_validation_and_application_all_modes
    ~loc:__LOC__
    ~error:(invalid_denunciation Double_attesting)
    ~predecessor:blk_a
    op

(** Check that a future-cycle double attestation fails. *)
let test_too_early_double_attestation_evidence () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* b = Block.bake_until_cycle_end genesis in
  let* blk_1, blk_2 = block_fork b in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* attestation_a = Op.raw_attestation blk_a in
  let* attestation_b = Op.raw_attestation blk_b in
  double_attestation (B genesis) attestation_a attestation_b |> fun operation ->
  let*! res = Block.bake ~operation genesis in
  Assert.proto_error ~loc:__LOC__ res (function
    | Validate_errors.Anonymous.Too_early_denunciation
        {kind = Misbehaviour.Double_attesting; _} ->
        true
    | _ -> false)

(** Check that after [max_slashing_period * blocks_per_cycle + 1], it is not possible
    to create a double_attestation anymore. *)
let test_too_late_double_attestation_evidence () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* attestation_a = Op.raw_attestation blk_a in
  let* attestation_b = Op.raw_attestation blk_b in
  let* blk =
    Block.bake_until_n_cycle_end (Constants.denunciation_period + 1) blk_a
  in
  let* blk = Block.bake blk in
  double_attestation (B blk) attestation_a attestation_b |> fun operation ->
  let*! res = Block.bake ~operation blk in
  Assert.proto_error ~loc:__LOC__ res (function
    | Validate_errors.Anonymous.Outdated_denunciation
        {kind = Misbehaviour.Double_attesting; _} ->
        true
    | _ -> false)

(** Check that an invalid double attestation evidence that exposes two
    attestations made by two different attesters fails. *)
let test_different_delegates () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* genesis = Block.bake genesis in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* attester_a, attester_b =
    Context.get_first_different_attesters (B blk_b)
  in
  let* e_a =
    Op.raw_attestation
      ~attesting_slot:(Op.attesting_slot_of_attester attester_a)
      blk_a
  in
  let* e_b =
    Op.raw_attestation
      ~attesting_slot:(Op.attesting_slot_of_attester attester_b)
      blk_b
  in
  let* (_ : Block.t) = Block.bake ~operation:(Operation.pack e_b) blk_b in
  double_attestation (B blk_b) e_a e_b |> fun operation ->
  let*! res = Block.bake ~operation blk_b in
  Assert.proto_error ~loc:__LOC__ res (function
    | Validate_errors.Anonymous.Invalid_denunciation
        Misbehaviour.Double_attesting ->
        true
    | _ -> false)

let test_freeze_more_with_low_balance =
  let open Lwt_result_syntax in
  let double_attest_and_punish b2 account1 =
    let open Lwt_result_syntax in
    (* Bake a block on top of [b2] that includes a double-attestation
       denunciation of [account1]. *)
    let* blk_d1, blk_d2 = block_fork b2 in
    let* blk_a = Block.bake ~policy:(Block.By_account account1) blk_d1 in
    let* blk_b = Block.bake ~policy:(Block.By_account account1) blk_d2 in
    let* attestation_a = Op.raw_attestation ~manager_pkh:account1 blk_a in
    let* attestation_b = Op.raw_attestation ~manager_pkh:account1 blk_b in
    let denunciation = double_attestation (B b2) attestation_a attestation_b in
    let* b =
      Block.bake ~policy:(Excluding [account1]) b2 ~operations:[denunciation]
    in
    return (b, attestation_a)
  in
  let check_unique_attester b account2 =
    let* attesters_list = Context.get_attesters (B b) in
    match attesters_list with
    | [{delegate; _}] when Signature.Public_key_hash.equal account2 delegate ->
        return_unit
    | _ -> failwith "We are supposed to only have account2 as attester."
  in
  fun () ->
    let constants =
      {
        Default_parameters.constants_test with
        issuance_weights =
          {
            Default_parameters.constants_test.issuance_weights with
            base_total_issued_per_minute = Tez.zero;
          };
        consensus_threshold_size = 0;
        origination_size = 0;
        consensus_rights_delay = 5;
      }
    in
    let* genesis, (c1, c2) = Context.init_with_constants2 constants in
    let account1 = Context.Contract.pkh c1 in
    let account2 = Context.Contract.pkh c2 in
    (* we empty the available balance of [account1]. *)
    let* info1 = Context.Delegate.info (B genesis) account1 in
    let* op =
      Op.transaction
        (B genesis)
        (Contract.Implicit account1)
        (Contract.Implicit account2)
        Tez_helpers.(info1.full_balance -! info1.frozen_deposits)
    in
    let* b2 =
      Block.bake ~policy:(Block.By_account account2) genesis ~operations:[op]
    in
    let* info2 = Context.Delegate.info (B b2) account1 in
    (* after block [b2], the spendable balance of [account1] is 0tz. So, given
       that we have the invariant full_balance = spendable balance +
       frozen_deposits, in this particular case, full_balance = frozen_deposits
       for [account1], and the frozen_deposits didn't change since genesis. *)
    let* () =
      Assert.equal_tez ~loc:__LOC__ info2.full_balance info2.frozen_deposits
    in
    let* () =
      Assert.equal_tez ~loc:__LOC__ info1.frozen_deposits info2.frozen_deposits
    in
    let* b3, duplicate_op = double_attest_and_punish b2 account1 in
    (* Denunciation has happened but slashing hasn't yet.
       We check that frozen deposits haven't changed and still correspond to the
       full balance and itself hasn't changed. *)
    let* info3 = Context.Delegate.info (B b3) account1 in
    let* () =
      Assert.equal_tez
        ~loc:__LOC__
        info3.frozen_deposits
        info3.current_frozen_deposits
    in
    let* () =
      Assert.equal_tez ~loc:__LOC__ info3.full_balance info3.frozen_deposits
    in
    let* () =
      Assert.equal_tez ~loc:__LOC__ info3.full_balance info2.full_balance
    in
    (* We now bake until end of cycle only with [account2]:
       block of the new cycle are called cX below. *)
    let* c1 = Block.bake_until_cycle_end ~policy:(By_account account2) b3 in
    (* Denunciation has happened: we check that the full balance of [account1]
       is (still) equal to its deposit. *)
    let* info4 = Context.Delegate.info (B c1) account1 in
    let* () =
      Assert.equal_tez
        ~loc:__LOC__
        info4.full_balance
        info4.current_frozen_deposits
    in
    (* We also check that compared to deposits at block [b2], [account1] lost
       50% of its deposits. *)
    let* slashing_pct =
      Slashing_helpers.slashing_percentage
        (Slashing_helpers.Misbehaviour_repr.from_duplicate_operation
           duplicate_op)
        ~block_before_slash:b3
        ~all_culprits:[account1]
    in
    let one_minus_slash_percentage = Percentage.neg slashing_pct in
    let {Q.num; den} = Percentage.to_q one_minus_slash_percentage in
    let expected_frozen_deposits_after =
      Tez_helpers.(info2.frozen_deposits *! Z.to_int64 num /! Z.to_int64 den)
    in
    let* () =
      Assert.equal_tez
        ~loc:__LOC__
        expected_frozen_deposits_after
        info4.current_frozen_deposits
    in
    let* c2, _ = double_attest_and_punish c1 account1 in
    (* Second denunciation has happened but slashing not yet, again.
       Current frozen deposits reflect the slashing of 50% of the original
       deposits. *)
    let* info5 = Context.Delegate.info (B c2) account1 in
    let* () =
      Assert.not_equal_tez
        ~loc:__LOC__
        info5.current_frozen_deposits
        info5.frozen_deposits
    in
    let* () =
      Assert.equal_tez
        ~loc:__LOC__
        info5.current_frozen_deposits
        info4.current_frozen_deposits
    in
    let* () =
      Assert.equal_tez
        ~loc:__LOC__
        info5.full_balance
        info5.current_frozen_deposits
    in
    let*! c3 = Block.bake c2 ~policy:(By_account account1) in
    (* Once the denunciations has summed up to 100%, the baker cannot bake anymore *)
    let* () =
      Assert.proto_error ~loc:__LOC__ c3 (function
        | Validate_errors.Consensus.Forbidden_delegate _ -> true
        | _ -> false)
    in
    let* c3 = Block.bake_until_cycle_end c2 ~policy:(By_account account2) in
    (* Second slashing has happened: we check that the full balance of
       [account1] reflects the slashing of 50% of the original deposit. Its
       current deposits are thus 0tz. *)
    let* info6 = Context.Delegate.info (B c3) account1 in
    let* () = Assert.equal_tez ~loc:__LOC__ info6.full_balance Tez.zero in
    let* () =
      Assert.equal_tez ~loc:__LOC__ info6.current_frozen_deposits Tez.zero
    in
    (* We bake [2 * consensus_rights_delay - 1] additional cycles only with [account2].
       Because [account1] does not bake during this period, it loses its rights.
    *)
    let* d1 =
      Block.bake_until_n_cycle_end
        ~policy:(By_account account2)
        ((2 * constants.consensus_rights_delay) - 1)
        c3
    in
    let* info7 = Context.Delegate.info (B d1) account1 in
    (* [account1] is only deactivated after 1 + [2 * consensus_rights_delay] (see
       [Delegate_activation_storage.set_active] since the last time it was
       active, that is, since the first cycle. Thus the cycle at which
       [account1] is deactivated is 2 + [2 * consensus_rights_delay] from genesis. *)
    let* () = Assert.equal_bool ~loc:__LOC__ info7.deactivated false in
    (* account1 is still active, but has no rights. *)
    let* () = check_unique_attester d1 account2 in
    let* e1 = Block.bake_until_cycle_end ~policy:(By_account account2) d1 in
    (* account1 has no rights and furthermore is no longer active. *)
    let* () = check_unique_attester e1 account2 in
    let* info8 = Context.Delegate.info (B e1) account1 in
    Assert.equal_bool ~loc:__LOC__ info8.deactivated true

(** Injecting a valid double attestation multiple times raises an error. *)
let test_two_double_attestation_evidences_leads_to_duplicate_denunciation () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold_size:0 () in
  let* blk_1, blk_2 = block_fork genesis in
  let* blk_a = Block.bake blk_1 in
  let* blk_b = Block.bake blk_2 in
  let* {Context.delegate; _} = Context.get_attester (B blk_a) in
  let* attestation_a = Op.raw_attestation ~manager_pkh:delegate blk_a in
  let* attestation_b = Op.raw_attestation ~manager_pkh:delegate blk_b in
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let operation2 = double_attestation (B genesis) attestation_b attestation_a in
  let* bakers = Context.get_bakers (B blk_a) in
  let baker = Context.get_first_different_baker delegate bakers in
  let* (_full_balance : Tez.t) =
    Context.Delegate.full_balance (B blk_a) baker
  in
  let*! e =
    Block.bake
      ~policy:(By_account baker)
      ~operations:[operation; operation2]
      blk_a
  in
  let* () =
    Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Conflicting_denunciation
          {kind = Misbehaviour.Double_attesting; _} ->
          true
      | _ -> false)
  in
  let* blk_with_evidence1 =
    Block.bake ~policy:(By_account baker) ~operation blk_a
  in
  let*! e =
    Block.bake ~policy:(By_account baker) ~operation blk_with_evidence1
  in
  Assert.proto_error ~loc:__LOC__ e (function
    | Validate_errors.Anonymous.Already_denounced
        {kind = Misbehaviour.Double_attesting; _} ->
        true
    | _ -> false)

(** Check that a double attestation evidence fails under aggregate_attestation
    feature flag when operations have distinct slots and are otherwise
    identical. *)
let different_slots_under_feature_flag () =
  let open Lwt_result_wrap_syntax in
  (* TODO ABAAB: doesn't work with ABAAB *)
  let* genesis, _ =
    Context.init2 ~consensus_threshold_size:0 ~aggregate_attestation:true ()
  in
  let* block = Block.bake genesis in
  let* attesters = Context.get_attesters (B block) in
  let* csts = Context.get_constants (B block) in
  let committee_size = csts.parametric.consensus_committee_size in
  let consensus_pkh, round1, round2 =
    (* Find an attester with more than 1 slot. *)
    WithExceptions.Option.get
      ~loc:__LOC__
      (List.find_map
         (fun (attester : RPC.Validators.delegate) ->
           match attester.rounds with
           | round1 :: round2 :: _ ->
               Some (attester.consensus_key, round1, round2)
           | _ -> None)
         attesters)
  in
  let*?@ slot1 = Round.to_slot ~committee_size round1 in
  let*?@ slot2 = Round.to_slot ~committee_size round2 in
  let attesting_slot1 = {Op.slot = slot1; consensus_pkh} in
  let attesting_slot2 = {Op.slot = slot2; consensus_pkh} in
  let* attestation1 =
    Op.raw_attestation ~attesting_slot:attesting_slot1 block
  in
  let* attestation2 =
    Op.raw_attestation ~attesting_slot:attesting_slot2 block
  in
  let double_attestation_evidence =
    double_attestation (B block) attestation1 attestation2
  in
  let*! res = Block.bake ~operation:double_attestation_evidence block in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_denunciation
          Misbehaviour.Double_attesting ->
          true
      | _ -> false)
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "valid double attestation evidence"
      `Quick
      test_valid_double_attestation_evidence;
    Tztest.tztest
      "valid evidence with same (pre)attestations on different branches"
      `Quick
      test_different_branch;
    Tztest.tztest
      "valid evidence with same (pre)attestations on different slots"
      `Quick
      test_different_slots;
    Tztest.tztest
      "2 valid double attestation evidences lead to not being able to bake"
      `Quick
      test_two_double_attestation_evidences_leadsto_no_bake;
    Tztest.tztest
      "2 valid double attestation evidences for double signings in consecutive \
       cycles lead to forbidding"
      `Quick
      test_two_double_attestation_evidences_consecutive_cycles;
    Tztest.tztest
      "2 valid double attestation evidences in consecutive cycles for double \
       signing in same cycle lead to forbidding"
      `Quick
      test_two_double_attestation_evidences_staggered;
    Tztest.tztest
      "valid double attestation injected multiple time"
      `Quick
      test_two_double_attestation_evidences_leads_to_duplicate_denunciation;
    Tztest.tztest
      "invalid double attestation evidence"
      `Quick
      test_invalid_double_attestation;
    Tztest.tztest
      "another invalid double attestation evidence"
      `Quick
      test_invalid_double_attestation_variant;
    Tztest.tztest
      "invalid double attestation evidence: duplicate slot in committee"
      `Quick
      test_invalid_double_attestation_duplicate_in_committee;
    Tztest.tztest
      "too early double attestation evidence"
      `Quick
      test_too_early_double_attestation_evidence;
    Tztest.tztest
      "too late double attestation evidence"
      `Quick
      test_too_late_double_attestation_evidence;
    Tztest.tztest "different delegates" `Quick test_different_delegates;
    Tztest.tztest
      "different slots under feature flag"
      `Quick
      different_slots_under_feature_flag;
    (* This test has been deactivated following the changes of the
       forbidding mechanism that now forbids delegates right after the
       first denunciation, it should be fixed and reactivated
       https://gitlab.com/tezos/tezos/-/issues/6904 *)
    (* Tztest.tztest *)
    (*   "freeze available balance after slashing" *)
    (*   `Quick *)
    (*   test_freeze_more_with_low_balance; *)
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("double attestation", tests)]
  |> Lwt_main.run
