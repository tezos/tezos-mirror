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
    Component:  Protocol (attestation)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_attestation.ml
    Subject:    Attesting a block adds an extra layer of confidence
                to the Tezos' PoS algorithm. The block attesting
                operation must be included in the following block.
*)

open Protocol
open Alpha_context

let init_genesis ?policy ?dal_enable () =
  let open Lwt_result_syntax in
  let* genesis, _contracts =
    Context.init_n ?dal_enable ~consensus_threshold_size:0 5 ()
  in
  let* b = Block.bake ?policy genesis in
  return (genesis, b)

(** {1 Positive tests} *)

(** Correct attestation from the slot 0 attester. *)
let test_simple_attestation () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block = init_genesis () in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block
    Attestation

(** Test that the attestation's branch does not affect its
    validity. *)
let test_arbitrary_branch () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block = init_genesis () in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block
    ~branch:Block_hash.zero
    Attestation

(** Correct attestation with a level and a round that are both
    different from {!test_simple_attestation}. *)
let test_non_zero_round () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* attested_block = Block.bake ~policy:(By_round 10) b in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block
    Attestation

(** Fitness gap: this is a straightforward update from Emmy to Tenderbake,
    that is, check that the level is incremented in a child block. *)
let test_fitness_gap () =
  let open Lwt_result_syntax in
  let* _genesis, pred_b = init_genesis () in
  let* operation = Op.attestation pred_b in
  let* b = Block.bake ~operation pred_b in
  let fitness =
    match Fitness.from_raw b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let pred_fitness =
    match Fitness.from_raw pred_b.header.shell.fitness with
    | Ok fitness -> fitness
    | _ -> assert false
  in
  let level = Fitness.level fitness in
  let pred_level = Fitness.level pred_fitness in
  let level_diff =
    Int32.sub (Raw_level.to_int32 level) (Raw_level.to_int32 pred_level)
  in
  Assert.equal_int32 ~loc:__LOC__ level_diff 1l

(** Return a delegate and its second smallest slot for the level of [block]. *)
let delegate_and_second_slot block =
  let open Lwt_result_wrap_syntax in
  let* attesters = Context.get_attesters (B block) in
  let* csts = Context.get_constants (B block) in
  let committee_size = csts.parametric.consensus_committee_size in
  let delegate, rounds =
    (* Find an attester with more than 1 round. *)
    WithExceptions.Option.get
      ~loc:__LOC__
      (List.find_map
         (fun {RPC.Validators.delegate; rounds; _} ->
           if Compare.List_length_with.(rounds > 1) then Some (delegate, rounds)
           else None)
         attesters)
  in
  (* Check that the rounds are sorted and have no duplicates. *)
  let rec check_sorted = function
    | [] | [_] -> true
    | x :: (y :: _ as t) -> Round.compare x y < 0 && check_sorted t
  in
  assert (check_sorted rounds) ;
  let* slot =
    match rounds with
    | [] | [_] -> assert false
    | _ :: round :: _ ->
        let*?@ slot = Round.to_slot ~committee_size round in
        return slot
  in
  return (delegate, slot)

let error_wrong_slot = function
  | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation {kind; _}
    when kind = Validate_errors.Consensus.Attestation ->
      true
  | _ -> false

(** {1 Negative tests}

    The following test scenarios are supposed to raise errors. *)

(** {2 Wrong slot} *)

(** Apply an attestation with a negative slot. *)
let test_negative_slot () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n 5 () in
  let* b = Block.bake genesis in
  let* attester = Context.get_attester (B b) in
  Lwt.catch
    (fun () ->
      let* (_ : packed_operation) =
        let slot =
          Slot.Internal_for_tests.of_int_unsafe_only_use_for_tests (-1)
        in
        Op.attestation
          ~attesting_slot:{slot; consensus_pkh = attester.consensus_key}
          b
      in
      failwith "negative slot should not be accepted by the binary format")
    (function
      | Data_encoding.Binary.Write_error _ -> return_unit | e -> Lwt.reraise e)

(** Attestation with a non-normalized slot (that is, a slot that
    belongs to the delegate but is not the delegate's smallest slot).
    This should fail in all validation modes. *)
let test_not_smallest_slot () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* attesting_slot = Op.get_non_canonical_attesting_slot ~attested_block:b in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:b
    ~attesting_slot
    ~application_error:error_wrong_slot
    ~construction_error:error_wrong_slot
    ~mempool_error:error_wrong_slot
    Attestation

let attesting_slot_with_someone_elses_slot block =
  let open Lwt_result_syntax in
  let* attesters = Context.get_attesters (B block) in
  match attesters with
  | [] | [_] -> Test.fail ~__LOC__ "Expected at least two delegates with rights"
  | {consensus_key = consensus_pkh; _} :: {attestation_slot; _} :: _ ->
      return {Op.slot = attestation_slot; consensus_pkh}

(** Attestation with a slot that does not belong to the delegate. *)
let test_not_own_slot () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* attesting_slot = attesting_slot_with_someone_elses_slot b in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block:b
    ~attesting_slot
    ~error:(function
      | Alpha_context.Operation.Invalid_signature -> true | _ -> false)
    Attestation

(** In mempool mode, also test attestations with a slot that does not
    belong to the delegate for various allowed levels and rounds. *)
let test_mempool_not_own_slot () =
  let open Lwt_result_syntax in
  let* _genesis, grandparent = init_genesis ~policy:(By_round 2) () in
  let* predecessor = Block.bake grandparent ~policy:(By_round 1) in
  let* future_block = Block.bake predecessor in
  let check_not_own_slot_fails loc b =
    let* attesting_slot = attesting_slot_with_someone_elses_slot b in
    Consensus_helpers.test_consensus_operation
      ~loc
      ~attested_block:b
      ~attesting_slot
      ~error:(function
        | Alpha_context.Operation.Invalid_signature -> true | _ -> false)
      Attestation
      Mempool
  in
  let* () = check_not_own_slot_fails __LOC__ grandparent in
  let* () = check_not_own_slot_fails __LOC__ predecessor in
  check_not_own_slot_fails __LOC__ future_block

(** {2 Wrong level} *)

let error_old_level = function
  | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
    when kind = Validate_errors.Consensus.Attestation ->
      true
  | _ -> false

(** Attestation that is one level too old, aka grandparent attestation
    (the attestation is expected to point to the level of the
    predecessor of the block/mempool containing the attestation, but
    instead it points to the grandparent's level).

    This attestation should fail in a block (application or
    construction), but be accepted in mempool mode. *)
let test_one_level_too_old () =
  let open Lwt_result_syntax in
  let* _genesis, grandparent = init_genesis () in
  let* predecessor = Block.bake grandparent in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:grandparent
    ~predecessor
    ~application_error:error_old_level
    ~construction_error:error_old_level
    ?mempool_error:None
    Attestation

(** Attestation that is two levels too old (pointing to the
    great-grandparent instead of the predecessor). It should fail in
    all modes. *)
let test_two_levels_too_old () =
  let open Lwt_result_syntax in
  let* _genesis, greatgrandparent = init_genesis () in
  let* grandparent = Block.bake greatgrandparent in
  let* predecessor = Block.bake grandparent in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block:greatgrandparent
    ~predecessor
    ~error:error_old_level
    Attestation

let error_future_level = function
  | Validate_errors.Consensus.Consensus_operation_for_future_level {kind; _}
    when kind = Validate_errors.Consensus.Attestation ->
      true
  | _ -> false

(** Attestation that is one level in the future (pointing to the same
    level as the block/mempool containing the attestation instead of
    its predecessor/head). It should fail in a block (application or
    construction) but succeed in a mempool. *)
let test_one_level_in_the_future () =
  let open Lwt_result_syntax in
  let* _genesis, predecessor = init_genesis () in
  let* next_level_block = Block.bake predecessor in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:next_level_block
    ~predecessor
    ~application_error:error_future_level
    ~construction_error:error_future_level
    ?mempool_error:None
    Attestation

(** Attestation that is two levels in the future. It should fail in
    all modes. *)
let test_two_levels_future () =
  let open Lwt_result_syntax in
  let* _genesis, predecessor = init_genesis () in
  let* next_level_block = Block.bake predecessor in
  let* after_next_level_block = Block.bake next_level_block in
  Consensus_helpers.test_consensus_operation_all_modes
    ~loc:__LOC__
    ~attested_block:after_next_level_block
    ~predecessor
    ~error:error_future_level
    Attestation

(** {2 Wrong round} *)

let error_old_round = function
  | Validate_errors.Consensus.Consensus_operation_for_old_round {kind; _}
    when kind = Validate_errors.Consensus.Attestation ->
      true
  | _ -> false

(** Attestation that is one round too old. It should fail in a block
    (application or construction) but succeed in a mempool. *)
let test_one_round_too_old () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* round0_block = Block.bake b in
  let* predecessor = Block.bake ~policy:(By_round 1) b in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:round0_block
    ~predecessor
    ~application_error:error_old_round
    ~construction_error:error_old_round
    ?mempool_error:None
    Attestation

(** Attestation that is many rounds too old. It should fail in a block
    (application or construction) but succeed in a mempool. *)
let test_many_rounds_too_old () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* round5_block = Block.bake ~policy:(By_round 5) b in
  let* predecessor = Block.bake ~policy:(By_round 15) b in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:round5_block
    ~predecessor
    ~application_error:error_old_round
    ~construction_error:error_old_round
    ?mempool_error:None
    Attestation

let error_future_round = function
  | Validate_errors.Consensus.Consensus_operation_for_future_round {kind; _}
    when kind = Validate_errors.Consensus.Attestation ->
      true
  | _ -> false

(** Attestation that is one round in the future. It should fail in a
    block (application or construction) but succeed in a mempool. *)
let test_one_round_in_the_future () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* predecessor = Block.bake b in
  let* round1_block = Block.bake ~policy:(By_round 1) b in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:round1_block
    ~predecessor
    ~application_error:error_future_round
    ~construction_error:error_future_round
    ?mempool_error:None
    Attestation

(** Attestation that is many rounds in the future. It should fail in a
    block (application or construction) but succeed in a mempool. *)
let test_many_rounds_future () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* predecessor = Block.bake ~policy:(By_round 5) b in
  let* round15_block = Block.bake ~policy:(By_round 15) b in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block:round15_block
    ~predecessor
    ~application_error:error_future_round
    ~construction_error:error_future_round
    ?mempool_error:None
    Attestation

(** {2 Wrong payload hash} *)

(** Attestation with an incorrect payload hash. It should fail in a
    block (application or construction) but succeed in a mempool. *)
let test_wrong_payload_hash () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block = init_genesis () in
  let error_wrong_payload_hash = function
    | Validate_errors.Consensus.Wrong_payload_hash_for_consensus_operation
        {kind; _}
      when kind = Validate_errors.Consensus.Attestation ->
        true
    | _ -> false
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~block_payload_hash:Block_payload_hash.zero
    ~application_error:error_wrong_payload_hash
    ~construction_error:error_wrong_payload_hash
    ?mempool_error:None
    Attestation

(** {1 Conflict tests}

    Some positive and some negative tests. *)

let assert_conflict_error ~loc res =
  Assert.proto_error ~loc res (function
    | Validate_errors.Consensus.Conflicting_consensus_operation {kind; _}
      when kind = Validate_errors.Consensus.Attestation ->
        true
    | _ -> false)

(** Test that attestations conflict with:
    - an identical attestation, and
    - an attestation on the same block with a different branch.

    In mempool mode, also test that they conflict with an attestation
    on the same level and round but with a different payload hash
    (such an attestation is invalid in application and construction modes). *)
let test_conflict () =
  let open Lwt_result_syntax in
  let* _genesis, b = init_genesis () in
  let* op = Op.attestation b in
  let* op_different_branch = Op.attestation ~branch:Block_hash.zero b in
  (* Test in application and construction (aka baking) modes *)
  let assert_conflict loc baking_mode tested_op =
    let*! block = Block.bake ~baking_mode ~operations:[op; tested_op] b in
    assert_conflict_error ~loc block
  in
  let* () = assert_conflict __LOC__ Application op in
  let* () = assert_conflict __LOC__ Application op_different_branch in
  let* () = assert_conflict __LOC__ Baking op in
  let* () = assert_conflict __LOC__ Baking op_different_branch in
  (* Test in mempool mode. *)
  let* inc = Incremental.begin_construction ~mempool_mode:true b in
  let* inc = Incremental.validate_operation inc op in
  let assert_mempool_conflict loc tested_op =
    let*! result = Incremental.validate_operation inc tested_op in
    assert_conflict_error ~loc result
  in
  let* () = assert_mempool_conflict __LOC__ op in
  let* () = assert_mempool_conflict __LOC__ op_different_branch in
  let* op_different_payload_hash =
    Op.attestation ~block_payload_hash:Block_payload_hash.zero b
  in
  let* () = assert_mempool_conflict __LOC__ op_different_payload_hash in
  return_unit

(** In mempool mode, test that grandparent attestations conflict with:
    - an identical attestation,
    - an attestation on the same block with a different branch, and
    - an attestation on the same block with a different payload hash.

    This test would make no sense in application or construction modes,
    since grandparent attestations fail anyway (as can be observed in
    {!test_one_level_too_old}). *)
let test_grandparent_conflict () =
  let open Lwt_result_syntax in
  let* _genesis, grandparent = init_genesis () in
  let* predecessor = Block.bake grandparent in
  let* op = Op.attestation grandparent in
  let* op_different_branch =
    Op.attestation ~branch:Block_hash.zero grandparent
  in
  let* op_different_payload_hash =
    Op.attestation ~block_payload_hash:Block_payload_hash.zero grandparent
  in
  let* inc = Incremental.begin_construction ~mempool_mode:true predecessor in
  let* inc = Incremental.validate_operation inc op in
  let assert_conflict loc tested_op =
    let*! result = Incremental.validate_operation inc tested_op in
    assert_conflict_error ~loc result
  in
  let* () = assert_conflict __LOC__ op in
  let* () = assert_conflict __LOC__ op_different_branch in
  let* () = assert_conflict __LOC__ op_different_payload_hash in
  return_unit

(** In mempool mode, test that attestations with the same future level
    and same non-zero round conflict. This is not tested in application
    and construction modes since such attestations would be invalid. *)
let test_future_level_conflict () =
  let open Lwt_result_syntax in
  let* _genesis, predecessor = init_genesis () in
  let* future_block = Block.bake ~policy:(By_round 10) predecessor in
  let* op = Op.attestation future_block in
  let* op_different_branch =
    Op.attestation ~branch:Block_hash.zero future_block
  in
  let* op_different_payload_hash =
    Op.attestation ~block_payload_hash:Block_payload_hash.zero future_block
  in
  let* inc = Incremental.begin_construction ~mempool_mode:true predecessor in
  let* inc = Incremental.validate_operation inc op in
  let assert_conflict loc tested_op =
    let*! result = Incremental.validate_operation inc tested_op in
    assert_conflict_error ~loc result
  in
  let* () = assert_conflict __LOC__ op in
  let* () = assert_conflict __LOC__ op_different_branch in
  let* () = assert_conflict __LOC__ op_different_payload_hash in
  return_unit

(** In mempool mode, test that there is no conflict between an
    attestation and a preattestation for the same slot (here the first
    slot), same level, and same round. *)
let test_no_conflict_with_preattestation_mempool () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block = init_genesis () in
  let* op_attestation = Op.attestation attested_block in
  let* op_preattestation = Op.preattestation attested_block in
  let* inc = Incremental.begin_construction ~mempool_mode:true attested_block in
  let* inc = Incremental.add_operation inc op_attestation in
  let* inc = Incremental.add_operation inc op_preattestation in
  let* _inc = Incremental.finalize_block inc in
  return_unit

(** In application and construction (aka baking) modes, test that
    there is no conflict between an attestation and a preattestation
    for the same slot (here the first slot). Note that the operations
    don't have the same level because the required levels for them to
    be valid are different. *)
let test_no_conflict_with_preattestation_block () =
  let open Lwt_result_syntax in
  let* _genesis, predecessor = init_genesis () in
  let* round0_block = Block.bake predecessor in
  let* op_attestation = Op.attestation predecessor in
  let* op_preattestation = Op.preattestation round0_block in
  let bake_both_ops baking_mode =
    Block.bake
      ~baking_mode
      ~payload_round:Round.zero
      ~locked_round:Round.zero
      ~policy:(By_round 1)
      ~operations:[op_attestation; op_preattestation]
      predecessor
  in
  let* (_ : Block.t) = bake_both_ops Application in
  let* (_ : Block.t) = bake_both_ops Baking in
  return_unit

(** In mempool mode, test that there is no conflict between
    attestations for the same slot (here the first slot) with various
    allowed levels and rounds.

    There are no similar tests in application and construction modes
    because valid attestations always have the same level and round. *)
let test_no_conflict_various_levels_and_rounds () =
  let open Lwt_result_syntax in
  let* genesis, grandparent = init_genesis () in
  let* predecessor = Block.bake grandparent in
  let* future_block = Block.bake predecessor in
  let* alt_grandparent = Block.bake ~policy:(By_round 1) genesis in
  let* alt_predecessor = Block.bake ~policy:(By_round 1) grandparent in
  let* alt_future = Block.bake ~policy:(By_round 10) alt_predecessor in
  let* inc = Incremental.begin_construction ~mempool_mode:true predecessor in
  let add_attestation inc attested_block =
    let* (op : packed_operation) = Op.attestation attested_block in
    let (Operation_data protocol_data) = op.protocol_data in
    let content =
      match protocol_data.contents with
      | Single (Attestation {consensus_content = content; _}) -> content
      | _ -> assert false
    in
    Format.eprintf
      "level: %ld, round: %ld@."
      (Raw_level.to_int32 content.level)
      (Round.to_int32 content.round) ;
    Incremental.add_operation inc op
  in
  let* inc = add_attestation inc grandparent in
  let* inc = add_attestation inc predecessor in
  let* inc = add_attestation inc future_block in
  let* inc = add_attestation inc alt_grandparent in
  let* inc = add_attestation inc alt_predecessor in
  let* inc = add_attestation inc alt_future in
  let* _inc = Incremental.finalize_block inc in
  return_unit

(** {1 Consensus threshold tests}

    Both positive and negative tests. *)

(** Check that:
    - a block with not enough attestation cannot be baked;
    - a block with enough attestation is baked. *)
let test_attestation_threshold ~sufficient_threshold () =
  let open Lwt_result_wrap_syntax in
  (* We choose a relative large number of accounts so that the probability that
     any delegate has [consensus_threshold_size] slots is low and most delegates have
     about 1 slot so we can get closer to the limit of [consensus_threshold_size]: we
     check that a block with attesting power [consensus_threshold_size - 1] won't be
     baked. *)
  (* TODO ABAAB: adapt test with new threshold under flag *)
  let* genesis, _contracts = Context.init_n 10 () in
  let* b = Block.bake genesis in
  let* {parametric = {consensus_threshold_size; _}; _} =
    Context.get_constants (B b)
  in
  let consensus_threshold_size = Int64.of_int consensus_threshold_size in
  let* attesters = Context.get_attesters (B b) in
  let*?@ round = Block.get_round b in
  let* _, attestations =
    List.fold_left_es
      (fun (counter, attestations) (attester : Context.attester) ->
        let new_counter = Int64.add counter attester.attesting_power in
        if
          (sufficient_threshold && counter < consensus_threshold_size)
          || (not sufficient_threshold)
             && new_counter < consensus_threshold_size
        then
          let attesting_slot = Op.attesting_slot_of_attester attester in
          let* attestation = Op.attestation ~attesting_slot ~round b in
          return (new_counter, attestation :: attestations)
        else return (counter, attestations))
      (0L, [])
      attesters
  in
  let*! b = Block.bake ~operations:attestations b in
  if sufficient_threshold then return_unit
  else
    Assert.proto_error ~loc:__LOC__ b (function
      | Validate_errors.Block.Not_enough_attestations _ -> true
      | _ -> false)

let test_two_attestations_with_same_attester () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block = init_genesis ~dal_enable:true () in
  let* op1 = Op.raw_attestation attested_block in
  let dal_content =
    let attestations = Dal_helpers.dal_attestations [Dal.Slot_index.zero] in
    {attestations}
  in
  let* op2 = Op.raw_attestation ~dal_content attested_block in
  let*! res =
    Block.bake
      ~baking_mode:Application
      ~operations:[Operation.pack op1; Operation.pack op2]
      attested_block
  in
  let error = function
    | Validate_errors.(
        Consensus.Conflicting_consensus_operation
          {
            kind = Attestation;
            conflict = Operation_conflict {existing; new_operation};
          }) ->
        Operation_hash.equal existing (Operation.hash op1)
        && Operation_hash.equal new_operation (Operation.hash op2)
    | _ -> false
  in
  Assert.proto_error ~loc:__LOC__ res error

(* Check that if an attester includes some non-empty DAL content but they have
   no assigned shards, then an error is returned at block validation.

   Note that we change the value of [consensus_committee_size] because with the
   default test parameters, [consensus_committee_size = 25 < 64 =
   number_of_shards], so that test would not work! *)
let test_attester_with_no_assigned_shards () =
  let open Lwt_result_syntax in
  let bal_high = 80_000_000_000L in
  let bal_low = 08_000_000_000L in
  (* Create many accounts with balance [bal_high] and one with [bal_low]. *)
  let n = 10 in
  let bootstrap_balances = bal_low :: Stdlib.List.init n (fun _ -> bal_high) in
  let dal =
    Tezos_protocol_alpha_parameters.Default_parameters.constants_sandbox.dal
  in
  let dal =
    (* We need to take a small number of shards to be sure there is a
       really high probability an attester is not part of the DAL
       committee in the first hundred blocks. *)
    {
      dal with
      cryptobox_parameters =
        {dal.cryptobox_parameters with number_of_shards = 64};
    }
  in
  let* genesis, contracts =
    Context.init_gen
      ~dal_enable:true
      ~dal
      ~consensus_committee_size:100
      ~consensus_threshold_size:0
      ~bootstrap_balances
      (Context.TList (n + 1))
      ()
  in
  let pkh = Stdlib.List.hd contracts |> Context.Contract.pkh in
  let rec iter b i =
    let* committee = Context.get_attesters (B b) in
    let in_committee =
      List.exists
        (fun del ->
          Signature.Public_key_hash.equal pkh del.RPC.Validators.delegate)
        committee
    in
    let* has_assigned_shards = Dal_helpers.has_assigned_shards (B b) pkh in
    if in_committee && not has_assigned_shards then
      let slot_index =
        Stdlib.Option.get
          (Dal.Slot_index.of_int_opt ~number_of_slots:dal.number_of_slots 0)
      in
      let number_of_lags = List.length dal.attestation_lags in
      let attestations =
        Dal.Attestations.(
          commit
            empty
            ~number_of_slots:dal.number_of_slots
            ~number_of_lags
            ~lag_index:(number_of_lags - 1)
            slot_index)
      in
      let dal_content = {attestations} in
      let* op = Op.attestation ~manager_pkh:pkh ~dal_content b in
      let* ctxt = Incremental.begin_construction b in
      let expect_apply_failure = function
        | [
            Environment.Ecoproto_error
              (Alpha_context.Dal_errors
               .Dal_data_availibility_attester_not_in_committee
                 {attester; committee_level; attested_level; lag_index});
          ]
          when Signature.Public_key_hash.equal attester pkh
               && Raw_level.to_int32 committee_level = b.header.shell.level
               && attested_level = Raw_level.succ committee_level
               && lag_index = number_of_lags - 1 ->
            return_unit
        | errs ->
            failwith
              "Error trace:@, %a does not match the expected one"
              Error_monad.pp_print_trace
              errs
      in
      Incremental.add_operation ctxt op ~expect_failure:expect_apply_failure
    else
      let* b = Block.bake b in
      if i = 100 then
        failwith
          "The account is in all DAL committees for 100 levels! The test needs \
           to be adapted."
      else iter b (i + 1)
  in
  let* b = Block.bake genesis in
  (* We bake a few blocks so that there is a published level for all tested
     attested levels. *)
  let* b = Block.bake_n dal.attestation_lag b in
  let* _ = iter b 0 in
  return_unit

let test_dal_attestation_threshold () =
  let open Lwt_result_wrap_syntax in
  let n = 100 in
  let* genesis, contracts = Context.init_n n ~consensus_threshold_size:0 () in
  let contract = Stdlib.List.hd contracts in
  let* {
         parametric =
           {
             dal =
               {
                 attestation_lag;
                 attestation_lags;
                 attestation_threshold;
                 cryptobox_parameters = {number_of_shards; _};
                 _;
               };
             _;
           };
         _;
       } =
    Context.get_constants (B genesis)
  in
  let slot_index = Dal.Slot_index.zero in
  let commitment = Alpha_context.Dal.Slot.Commitment.zero in
  let commitment_proof = Alpha_context.Dal.Slot.Commitment_proof.zero in
  let slot_header =
    Dal.Operations.Publish_commitment.{slot_index; commitment; commitment_proof}
  in
  let* op = Op.dal_publish_commitment (B genesis) contract slot_header in
  let* b = Block.bake genesis ~operation:op in
  let* b = Block.bake_n (attestation_lag - 1) b in
  let* dal_committee = Context.Dal.shards (B b) () in
  let attestations = Dal_helpers.dal_attestations [slot_index] in
  let dal_content = {attestations} in
  let min_power = attestation_threshold * number_of_shards / 100 in
  Log.info "Number of minimum required attested shards: %d" min_power ;
  let* _ =
    List.fold_left_es
      (fun (acc_ops, acc_power)
           ({delegate; indexes} : RPC.Dal.S.shards_assignment)
         ->
        let* op = Op.attestation ~manager_pkh:delegate ~dal_content b in
        let ops = op :: acc_ops in
        let power = acc_power + List.length indexes in
        let* _b, (metadata, _ops) =
          Block.bake_with_metadata ~operations:ops b
        in
        let attested_expected = power >= min_power in
        let number_of_slots = genesis.constants.dal.number_of_slots in
        let number_of_lags = List.length attestation_lags in
        let attested =
          Dal.Slot_availability.is_attested
            metadata.dal_slot_availability
            ~number_of_slots
            ~number_of_lags
            ~lag_index:(number_of_lags - 1)
            slot_index
        in
        Log.info "With %d power, the slot is attested: %b " power attested ;
        Check.(attested = attested_expected)
          Check.bool
          ~error_msg:
            "Unexpected attestation status for slot 0: got %L, expected %R " ;
        return (ops, power))
      ([], 0)
      dal_committee
  in
  return_unit

(* The BLS mode encoding differs from the regular attestation encoding
   in that slots are omitted. This test verifies that an attestation's signature
   check remains valid even after replacing its slot with a different one. *)
let slot_substitution_do_not_affect_signature_check () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n 5 ~aggregate_attestation:true () in
  let* b = Block.bake genesis in
  let* {Context.consensus_key = consensus_pkh; _} =
    Context.get_attester (B b)
  in
  let* signer = Account.find consensus_pkh in
  let watermark = Operation.to_watermark (Attestation Chain_id.zero) in
  let slot_swap_and_check_signature slot
      ({shell = {branch}; protocol_data = {contents; _}} :
        Kind.attestation operation) =
    match contents with
    | Single (Attestation {consensus_content; dal_content}) ->
        let bytes =
          Data_encoding.Binary.to_bytes_exn
            Operation.bls_mode_unsigned_encoding
            ({branch}, Contents_list contents)
        in
        let signature = Signature.sign ~watermark signer.sk bytes in
        let contents =
          Single
            (Attestation
               {consensus_content = {consensus_content with slot}; dal_content})
        in
        let bytes_with_different_slot =
          Data_encoding.Binary.to_bytes_exn
            Operation.bls_mode_unsigned_encoding
            ({branch}, Contents_list contents)
        in
        if
          Signature.check
            ~watermark:(Operation.to_watermark (Attestation Chain_id.zero))
            signer.pk
            signature
            bytes_with_different_slot
        then return_unit
        else Test.fail "Unexpected signature check failure"
  in
  let* attestation_without_dal =
    Op.raw_attestation ~attesting_slot:{slot = Slot.zero; consensus_pkh} b
  in
  let* () =
    slot_swap_and_check_signature
      (Slot.Internal_for_tests.of_int_unsafe_only_use_for_tests 1)
      attestation_without_dal
  in
  let* attestation_with_dal =
    (* attestation with dal signed with slot zero *)
    Op.raw_attestation
      ~attesting_slot:{slot = Slot.zero; consensus_pkh}
      ~dal_content:{attestations = Dal.Attestations.empty}
      b
  in
  let* () =
    slot_swap_and_check_signature
      (Slot.Internal_for_tests.of_int_unsafe_only_use_for_tests 1)
      attestation_with_dal
  in
  return_unit

(* The BLS mode encoding differs from the regular attestation encoding in that
   slots are omitted. This test ensures that an attestation's signature cannot
   be mismatched between signing and verification. *)
let encoding_incompatibility () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n 5 ~aggregate_attestation:true () in
  let* b = Block.bake genesis in
  let* attester = Context.get_attester (B b) in
  let* signer = Account.find attester.consensus_key in
  let check_encodings_incompatibily
      ({shell = {branch}; protocol_data = {contents; signature = _}} :
        Kind.attestation operation) =
    let bytes_without_slot =
      Data_encoding.Binary.to_bytes_exn
        Operation.bls_mode_unsigned_encoding
        ({branch}, Contents_list contents)
    in
    let bytes_with_slot =
      Data_encoding.Binary.to_bytes_exn
        Operation.unsigned_encoding
        ({branch}, Contents_list contents)
    in
    let watermark = Operation.to_watermark (Attestation Chain_id.zero) in
    let signature_without_slot =
      Signature.sign ~watermark signer.sk bytes_without_slot
    in
    let signature_with_slot =
      Signature.sign ~watermark signer.sk bytes_with_slot
    in
    (* Sanity checks *)
    let* () =
      if
        Signature.check
          ~watermark
          signer.pk
          signature_without_slot
          bytes_without_slot
      then return_unit
      else
        Test.fail "Unexpected signature check failure (signature_without_slot)"
    in
    let* () =
      if
        Signature.check ~watermark signer.pk signature_with_slot bytes_with_slot
      then return_unit
      else Test.fail "Unexpected signature check failure (signature_with_slot)"
    in
    (* Encodings incompatibility checks *)
    let* () =
      if
        Signature.check
          ~watermark
          signer.pk
          signature_without_slot
          bytes_with_slot
      then Test.fail "Unexpected signature check success (bytes_with_slot)"
      else return_unit
    in
    if
      Signature.check
        ~watermark
        signer.pk
        signature_with_slot
        bytes_without_slot
    then Test.fail "Unexpected signature check success (bytes_without_slot)"
    else return_unit
  in
  let attesting_slot = Op.attesting_slot_of_attester attester in
  let* raw_attestation_without_dal = Op.raw_attestation ~attesting_slot b in
  let* () = check_encodings_incompatibily raw_attestation_without_dal in
  let* raw_attestation_with_dal =
    Op.raw_attestation
      ~attesting_slot
      ~dal_content:{attestations = Dal.Attestations.empty}
      b
  in
  let* () = check_encodings_incompatibily raw_attestation_with_dal in
  return_unit

let tests =
  [
    (* Positive tests *)
    Tztest.tztest "Simple attestation" `Quick test_simple_attestation;
    Tztest.tztest "Arbitrary branch" `Quick test_arbitrary_branch;
    Tztest.tztest "Non-zero round" `Quick test_non_zero_round;
    Tztest.tztest "Fitness gap" `Quick test_fitness_gap;
    (* Negative tests *)
    (* Wrong slot *)
    Tztest.tztest "Attestation with slot -1" `Quick test_negative_slot;
    Tztest.tztest "Non-normalized slot" `Quick test_not_smallest_slot;
    Tztest.tztest "Not own slot" `Quick test_not_own_slot;
    Tztest.tztest "Mempool: not own slot" `Quick test_mempool_not_own_slot;
    (* Wrong level *)
    Tztest.tztest "One level too old" `Quick test_one_level_too_old;
    Tztest.tztest "Two levels too old" `Quick test_two_levels_too_old;
    Tztest.tztest "One level in the future" `Quick test_one_level_in_the_future;
    Tztest.tztest "Two levels in the future" `Quick test_two_levels_future;
    (* Wrong round *)
    Tztest.tztest "One round too old" `Quick test_one_round_too_old;
    Tztest.tztest "Many rounds too old" `Quick test_many_rounds_too_old;
    Tztest.tztest "One round in the future" `Quick test_one_round_in_the_future;
    Tztest.tztest "Many rounds in the future" `Quick test_many_rounds_future;
    (* Wrong payload hash *)
    Tztest.tztest "Wrong payload hash" `Quick test_wrong_payload_hash;
    (* Conflict tests (some negative, some positive) *)
    Tztest.tztest "Conflict" `Quick test_conflict;
    Tztest.tztest "Grandparent conflict" `Quick test_grandparent_conflict;
    Tztest.tztest "Future level conflict" `Quick test_future_level_conflict;
    Tztest.tztest
      "No conflict with preattestation (mempool)"
      `Quick
      test_no_conflict_with_preattestation_mempool;
    Tztest.tztest
      "No conflict with preattestation (block)"
      `Quick
      test_no_conflict_with_preattestation_block;
    Tztest.tztest
      "No conflict with various levels and rounds"
      `Quick
      test_no_conflict_various_levels_and_rounds;
    (* Consensus threshold tests (one positive and one negative) *)
    Tztest.tztest
      "sufficient attestation threshold"
      `Quick
      (test_attestation_threshold ~sufficient_threshold:true);
    Tztest.tztest
      "insufficient attestation threshold"
      `Quick
      (test_attestation_threshold ~sufficient_threshold:false);
    Tztest.tztest
      "two attestations with same attester in a block"
      `Quick
      test_two_attestations_with_same_attester;
    Tztest.tztest
      "attester without assigned shards"
      `Quick
      test_attester_with_no_assigned_shards;
    Tztest.tztest
      "DAL attestation_threshold"
      `Quick
      test_dal_attestation_threshold;
    (* slots are not part of the signed payload *)
    Tztest.tztest
      "slot substitution do not affect signature check"
      `Quick
      slot_substitution_do_not_affect_signature_check;
    (* bls_mode_unsigned_encoding cannot be mismatched with unsigned_encoding *)
    Tztest.tztest "encoding incompatitiblity" `Quick encoding_incompatibility;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("attestation", tests)]
  |> Lwt_main.run
