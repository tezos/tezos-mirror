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
    Invocation:   dune exec src/proto_018_Proxford/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_double_attestation.ml
    Subject:      Double attestation evidence operation may happen when an
                  attester attested two different blocks on the same level.
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let block_fork b =
  Context.get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >|=? fun blk_b -> (blk_a, blk_b)

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
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  (* from blk_1 we bake blk_a and from blk_2 we bake blk_b so that
     the same delegate attests blk_a and blk_b and these 2 form
     a valid double attestation evidence;
     - note that we cannot have double attestation evidence
       at the level of blk_1, blk_2 because both have as parent genesis
       and so the attestations are identical because the blocks blk_1, blk_2
       are identical. *)
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_attester (B blk_a) >>=? fun (delegate, _) ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  Context.get_bakers (B blk_a) >>=? fun bakers ->
  let baker = Context.get_first_different_baker delegate bakers in
  Context.Delegate.full_balance (B blk_a) baker >>=? fun full_balance ->
  Block.bake ~policy:(By_account baker) ~operation blk_a >>=? fun blk_final ->
  (* Check that parts of the frozen deposits are slashed *)
  Context.Delegate.current_frozen_deposits (B blk_a) delegate
  >>=? fun frozen_deposits_before ->
  Context.Delegate.current_frozen_deposits (B blk_final) delegate
  >>=? fun frozen_deposits_after ->
  Context.get_constants (B genesis) >>=? fun csts ->
  let p =
    csts.parametric.percentage_of_frozen_deposits_slashed_per_double_attestation
  in
  let expected_frozen_deposits_after =
    Test_tez.(frozen_deposits_before *! Int64.of_int (100 - p) /! 100L)
  in
  Assert.equal_tez
    ~loc:__LOC__
    expected_frozen_deposits_after
    frozen_deposits_after
  >>=? fun () ->
  (* Check that the initial frozen deposits has not changed *)
  Context.Delegate.initial_frozen_deposits (B blk_final) delegate
  >>=? fun initial_frozen_deposits ->
  Assert.equal_tez ~loc:__LOC__ initial_frozen_deposits frozen_deposits_before
  >>=? fun () ->
  (* Check that [baker] is rewarded with:
     - baking_reward_fixed_portion for baking and,
     - half of the frozen_deposits for including the evidence *)
  let baking_reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts.parametric
      ~reward_kind:Baking_reward_fixed_portion
  in
  let divider =
    Int64.add
      2L
      (Int64.of_int
         csts.parametric.adaptive_issuance.global_limit_of_staking_over_baking)
  in
  let evidence_reward = Test_tez.(frozen_deposits_after /! divider) in
  let expected_reward = Test_tez.(baking_reward +! evidence_reward) in
  Context.Delegate.full_balance (B blk_final) baker
  >>=? fun full_balance_with_rewards ->
  let real_reward = Test_tez.(full_balance_with_rewards -! full_balance) in
  Assert.equal_tez ~loc:__LOC__ expected_reward real_reward

(** Check that a double (pre)attestation evidence with equivalent
    attestations but on different branches succeeds. *)
let test_different_branch () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun blk ->
  Context.get_attester (B blk) >>=? fun (attester, _slots) ->
  Op.raw_attestation ~delegate:attester blk >>=? fun attestation_a ->
  Op.raw_attestation ~branch:Block_hash.zero ~delegate:attester blk
  >>=? fun attestation_b ->
  let operation = double_attestation (B blk) attestation_a attestation_b in
  Block.bake ~operation blk >>=? fun _blk ->
  Op.raw_preattestation ~delegate:attester blk >>=? fun preattestation_a ->
  Op.raw_preattestation ~branch:Block_hash.zero ~delegate:attester blk
  >>=? fun preattestation_b ->
  let operation =
    double_preattestation (B blk) preattestation_a preattestation_b
  in
  Block.bake ~operation blk >>=? fun _blk -> return_unit

(** Check that a double (pre)attestation evidence succeeds when the
    operations have distinct slots (that both belong to the delegate)
    and are otherwise identical. *)
let test_different_slots () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init2 ~consensus_threshold:0 () in
  let* blk = Block.bake genesis in
  let* attesters = Context.get_attesters (B blk) in
  let delegate, slot1, slot2 =
    (* Find an attester with more than 1 slot. *)
    WithExceptions.Option.get
      ~loc:__LOC__
      (List.find_map
         (fun (attester : RPC.Validators.t) ->
           match attester.slots with
           | slot1 :: slot2 :: _ -> Some (attester.delegate, slot1, slot2)
           | _ -> None)
         attesters)
  in
  let* attestation1 = Op.raw_attestation ~delegate ~slot:slot1 blk in
  let* attestation2 = Op.raw_attestation ~delegate ~slot:slot2 blk in
  let doubleA = double_attestation (B blk) attestation1 attestation2 in
  let* (_ : Block.t) = Block.bake ~operation:doubleA blk in
  let* preattestation1 = Op.raw_preattestation ~delegate ~slot:slot1 blk in
  let* preattestation2 = Op.raw_preattestation ~delegate ~slot:slot2 blk in
  let doubleB = double_preattestation (B blk) preattestation1 preattestation2 in
  let* (_ : Block.t) = Block.bake ~operation:doubleB blk in
  return_unit

(** Say a delegate double-attests twice and say the 2 evidences are timely
   included. Then the delegate can no longer bake. *)
let test_two_double_attestation_evidences_leadsto_no_bake () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_attester (B blk_a) >>=? fun (delegate, _) ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  Context.get_bakers (B blk_a) >>=? fun bakers ->
  let baker = Context.get_first_different_baker delegate bakers in
  Context.Delegate.full_balance (B blk_a) baker
  >>=? fun (_full_balance : Tez.t) ->
  Block.bake ~policy:(By_account baker) ~operation blk_a
  >>=? fun blk_with_evidence1 ->
  block_fork blk_with_evidence1 >>=? fun (blk_30, blk_40) ->
  Block.bake blk_30 >>=? fun blk_3 ->
  Block.bake blk_40 >>=? fun blk_4 ->
  Op.raw_attestation blk_3 >>=? fun attestation_3 ->
  Op.raw_attestation blk_4 >>=? fun attestation_4 ->
  let operation =
    double_attestation (B blk_with_evidence1) attestation_3 attestation_4
  in
  Block.bake ~policy:(By_account baker) ~operation blk_3
  >>=? fun blk_with_evidence2 ->
  (* Check that all the frozen deposits are slashed *)
  Context.Delegate.current_frozen_deposits (B blk_with_evidence2) delegate
  >>=? fun frozen_deposits_after ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero frozen_deposits_after >>=? fun () ->
  Block.bake ~policy:(By_account delegate) blk_with_evidence2 >>= fun b ->
  (* a delegate with 0 frozen deposits cannot bake *)
  Assert.proto_error_with_info ~loc:__LOC__ b "Zero frozen deposits"

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that an invalid double attestation operation that exposes a
      valid attestation fails. *)
let test_invalid_double_attestation () =
  Context.init_n ~consensus_threshold:0 10 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun b ->
  Op.raw_attestation b >>=? fun attestation ->
  Block.bake ~operation:(Operation.pack attestation) b >>=? fun b ->
  Op.double_attestation (B b) attestation attestation |> fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_denunciation kind
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

(** Check that an double attestation operation that is invalid due to
   incorrect ordering of the attestations fails. *)
let test_invalid_double_attestation_variant () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  Block.bake_until_cycle_end genesis >>=? fun b ->
  block_fork b >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  double_attestation
    (B genesis)
    ~correct_order:false
    attestation_a
    attestation_b
  |> fun operation ->
  Block.bake ~operation genesis >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Invalid_denunciation kind
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

(** Check that a future-cycle double attestation fails. *)
let test_too_early_double_attestation_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  Block.bake_until_cycle_end genesis >>=? fun b ->
  block_fork b >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  double_attestation (B genesis) attestation_a attestation_b |> fun operation ->
  Block.bake ~operation genesis >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Too_early_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

(** Check that after [max_slashing_period * blocks_per_cycle + 1], it is not possible
    to create a double_attestation anymore. *)
let test_too_late_double_attestation_evidence () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  Context.get_constants (B genesis)
  >>=? fun Constants.
             {parametric = {max_slashing_period; blocks_per_cycle; _}; _} ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  Block.bake_n ((max_slashing_period * Int32.to_int blocks_per_cycle) + 1) blk_a
  >>=? fun blk ->
  double_attestation (B blk) attestation_a attestation_b |> fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Outdated_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

(** Check that an invalid double attestation evidence that exposes two
    attestations made by two different attesters fails. *)
let test_different_delegates () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  Block.bake genesis >>=? fun genesis ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_first_different_attesters (B blk_b)
  >>=? fun (attester_a, attester_b) ->
  Op.raw_attestation ~delegate:attester_a.delegate blk_a >>=? fun e_a ->
  Op.raw_attestation ~delegate:attester_b.delegate blk_b >>=? fun e_b ->
  Block.bake ~operation:(Operation.pack e_b) blk_b >>=? fun (_ : Block.t) ->
  double_attestation (B blk_b) e_a e_b |> fun operation ->
  Block.bake ~operation blk_b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Inconsistent_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

(** Check that a double attestation evidence that exposes a ill-formed
    attestation fails. *)
let test_wrong_delegate () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_attester (B blk_a) >>=? fun (attester_a, _a_slots) ->
  Op.raw_attestation ~delegate:attester_a blk_a >>=? fun attestation_a ->
  Context.get_attester_n (B blk_b) 0 >>=? fun (attester0, _slots0) ->
  Context.get_attester_n (B blk_b) 1 >>=? fun (attester1, _slots1) ->
  let attester_b =
    if Signature.Public_key_hash.equal attester_a attester0 then attester1
    else attester0
  in
  Op.raw_attestation ~delegate:attester_b blk_b >>=? fun attestation_b ->
  double_attestation (B blk_b) attestation_a attestation_b |> fun operation ->
  Block.bake ~operation blk_b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Anonymous.Inconsistent_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

let test_freeze_more_with_low_balance =
  let get_attesting_slots_for_account ctxt account =
    (* Get the slots of the given account in the given context. *)
    Context.get_attesters ctxt >>=? function
    | [d1; d2] ->
        return
          (if Signature.Public_key_hash.equal account d1.delegate then d1
          else if Signature.Public_key_hash.equal account d2.delegate then d2
          else assert false)
            .slots
    | _ -> assert false
    (* there are exactly two attesters for this test. *)
  in
  let double_attest_and_punish b2 account1 =
    (* Bake a block on top of [b2] that includes a double-attestation
       denunciation of [account1]. *)
    block_fork b2 >>=? fun (blk_d1, blk_d2) ->
    Block.bake ~policy:(Block.By_account account1) blk_d1 >>=? fun blk_a ->
    Block.bake ~policy:(Block.By_account account1) blk_d2 >>=? fun blk_b ->
    get_attesting_slots_for_account (B blk_a) account1 >>=? fun slots_a ->
    let slot =
      match List.hd slots_a with None -> assert false | Some s -> s
    in
    Op.raw_attestation ~delegate:account1 ~slot blk_a >>=? fun attestation_a ->
    get_attesting_slots_for_account (B blk_b) account1 >>=? fun slots_b ->
    let slot =
      match List.hd slots_b with None -> assert false | Some s -> s
    in
    Op.raw_attestation ~delegate:account1 ~slot blk_b >>=? fun attestation_b ->
    let denunciation = double_attestation (B b2) attestation_a attestation_b in
    Block.bake ~policy:(Excluding [account1]) b2 ~operations:[denunciation]
  in
  let check_unique_attester b account2 =
    Context.get_attesters (B b) >>=? function
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
        consensus_threshold = 0;
        origination_size = 0;
        preserved_cycles = 5;
        percentage_of_frozen_deposits_slashed_per_double_attestation =
          (* enforce that percentage is 50% in the test's params. *)
          50;
      }
    in
    Context.init_with_constants2 constants >>=? fun (genesis, (c1, c2)) ->
    let account1 = Context.Contract.pkh c1 in
    let account2 = Context.Contract.pkh c2 in
    (* we empty the available balance of [account1]. *)
    Context.Delegate.info (B genesis) account1 >>=? fun info1 ->
    Op.transaction
      (B genesis)
      (Contract.Implicit account1)
      (Contract.Implicit account2)
      Test_tez.(info1.full_balance -! info1.frozen_deposits)
    >>=? fun op ->
    Block.bake ~policy:(Block.By_account account2) genesis ~operations:[op]
    >>=? fun b2 ->
    Context.Delegate.info (B b2) account1 >>=? fun info2 ->
    (* after block [b2], the spendable balance of [account1] is 0tz. So, given
       that we have the invariant full_balance = spendable balance +
       frozen_deposits, in this particular case, full_balance = frozen_deposits
       for [account1], and the frozen_deposits didn't change since genesis. *)
    Assert.equal_tez ~loc:__LOC__ info2.full_balance info2.frozen_deposits
    >>=? fun () ->
    Assert.equal_tez ~loc:__LOC__ info1.frozen_deposits info2.frozen_deposits
    >>=? fun () ->
    double_attest_and_punish b2 account1 >>=? fun b3 ->
    (* Denunciation has happened: we check that the full balance of [account1]
       is (still) equal to its deposit. *)
    Context.Delegate.info (B b3) account1 >>=? fun info3 ->
    Assert.equal_tez
      ~loc:__LOC__
      info3.full_balance
      info3.current_frozen_deposits
    >>=? fun () ->
    (* We also check that compared to deposits at block [b2], [account1] lost
       50% of its deposits. *)
    let slash_percentage =
      constants.percentage_of_frozen_deposits_slashed_per_double_attestation
    in
    let expected_frozen_deposits_after =
      Test_tez.(
        info2.frozen_deposits *! Int64.of_int (100 - slash_percentage) /! 100L)
    in
    Assert.equal_tez
      ~loc:__LOC__
      expected_frozen_deposits_after
      info3.current_frozen_deposits
    >>=? fun () ->
    (* We now bake until end of cycle only with [account2]:
       block of the new cycle are called cX below. *)
    Block.bake_until_cycle_end ~policy:(By_account account2) b3 >>=? fun c1 ->
    double_attest_and_punish c1 account1 >>=? fun c2 ->
    (* Second denunciation has happened: we check that the full balance of
       [account1] reflects the slashing of 50% of the original deposit. Its
       current deposits are thus 0tz. *)
    Context.Delegate.info (B c2) account1 >>=? fun info4 ->
    Assert.equal_tez ~loc:__LOC__ info4.full_balance Tez.zero >>=? fun () ->
    Assert.equal_tez ~loc:__LOC__ info4.current_frozen_deposits Tez.zero
    >>=? fun () ->
    Block.bake c2 ~policy:(By_account account1) >>= fun c3 ->
    (* Once the deposits dropped to 0, the baker cannot bake anymore *)
    Assert.proto_error_with_info ~loc:__LOC__ c3 "Zero frozen deposits"
    >>=? fun () ->
    (* We bake [2 * preserved_cycles] additional cycles only with [account2].
       Because [account1] does not bake during this period, it loses its rights.
    *)
    Block.bake_until_n_cycle_end
      ~policy:(By_account account2)
      (2 * constants.preserved_cycles)
      c2
    >>=? fun d1 ->
    Context.Delegate.info (B d1) account1 >>=? fun info5 ->
    (* [account1] is only deactivated after 1 + [2 * preserved_cycles] (see
       [Delegate_activation_storage.set_active] since the last time it was
       active, that is, since the first cycle. Thus the cycle at which
       [account1] is deactivated is 2 + [2 * preserved_cycles] from genesis. *)
    Assert.equal_bool ~loc:__LOC__ info5.deactivated false >>=? fun () ->
    (* account1 is still active, but has no rights. *)
    check_unique_attester d1 account2 >>=? fun () ->
    Block.bake_until_cycle_end ~policy:(By_account account2) d1 >>=? fun e1 ->
    (* account1 has no rights and furthermore is no longer active. *)
    check_unique_attester e1 account2 >>=? fun () ->
    Context.Delegate.info (B e1) account1 >>=? fun info6 ->
    Assert.equal_bool ~loc:__LOC__ info6.deactivated true

(** Injecting a valid double attestation multiple times raises an error. *)
let test_two_double_attestation_evidences_leads_to_duplicate_denunciation () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (genesis, _contracts) ->
  block_fork genesis >>=? fun (blk_1, blk_2) ->
  Block.bake blk_1 >>=? fun blk_a ->
  Block.bake blk_2 >>=? fun blk_b ->
  Context.get_attester (B blk_a) >>=? fun (delegate, _) ->
  Op.raw_attestation blk_a >>=? fun attestation_a ->
  Op.raw_attestation blk_b >>=? fun attestation_b ->
  let operation = double_attestation (B genesis) attestation_a attestation_b in
  let operation2 = double_attestation (B genesis) attestation_b attestation_a in
  Context.get_bakers (B blk_a) >>=? fun bakers ->
  let baker = Context.get_first_different_baker delegate bakers in
  Context.Delegate.full_balance (B blk_a) baker
  >>=? fun (_full_balance : Tez.t) ->
  Block.bake
    ~policy:(By_account baker)
    ~operations:[operation; operation2]
    blk_a
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Conflicting_denunciation {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)
  >>=? fun () ->
  Block.bake ~policy:(By_account baker) ~operation blk_a
  >>=? fun blk_with_evidence1 ->
  Block.bake ~policy:(By_account baker) ~operation blk_with_evidence1
  >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Validate_errors.Anonymous.Already_denounced {kind; _}
        when kind = Validate_errors.Anonymous.Attestation ->
          true
      | _ -> false)

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
      "too early double attestation evidence"
      `Quick
      test_too_early_double_attestation_evidence;
    Tztest.tztest
      "too late double attestation evidence"
      `Quick
      test_too_late_double_attestation_evidence;
    Tztest.tztest "different delegates" `Quick test_different_delegates;
    Tztest.tztest "wrong delegate" `Quick test_wrong_delegate;
    Tztest.tztest
      "freeze available balance after slashing"
      `Quick
      test_freeze_more_with_low_balance;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("double attestation", tests)]
  |> Lwt_main.run
