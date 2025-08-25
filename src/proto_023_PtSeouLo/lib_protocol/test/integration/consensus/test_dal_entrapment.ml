(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (double baking)
    Invocation:   dune exec src/proto_023_PtSeouLo/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_dal_entrapment.ml

    Subject:      A DAL entrapment operation may be injected when it has been observed that
                  a baker has attested a slot for which one the shards is a trap.
*)

open Protocol
open Alpha_context

let commitment_and_proofs_cache = ref None

type injection_time = Now | Last_valid | First_invalid

module IntMap = Map.Make (Int)
module PkhMap = Map.Make (Signature.Public_key_hash)

let get_traps_and_non_traps ~traps_fraction shard_assignment shards_with_proofs
    =
  List.of_seq shards_with_proofs
  |> List.fold_left
       (fun map (shard, proof) ->
         let pkh_opt =
           IntMap.find shard.Tezos_crypto_dal.Cryptobox.index shard_assignment
         in
         match pkh_opt with
         | None ->
             Test.fail
               ~__LOC__
               "Did not find a delegate for shard index %d"
               shard.index
         | Some pkh -> (
             let res =
               Environment.Dal.share_is_trap pkh shard.share ~traps_fraction
             in
             match res with
             | Ok b ->
                 let shard_with_proof = Dal.Shard_with_proof.{shard; proof} in
                 let traps, not_traps =
                   match PkhMap.find_opt pkh map with
                   | None -> ([], [])
                   | Some v -> v
                 in
                 let traps, not_traps =
                   if b then (shard_with_proof :: traps, not_traps)
                   else (traps, shard_with_proof :: not_traps)
                 in
                 PkhMap.add pkh (traps, not_traps) map
             | Error `Decoding_error ->
                 Test.fail ~__LOC__ "Decoding error in [share_is_trap]"))
       PkhMap.empty

let indexes_to_delegates =
  List.fold_left
    (fun map Plugin.RPC.Dal.S.{delegate; indexes} ->
      List.fold_left
        (fun map index -> IntMap.add index delegate map)
        map
        indexes)
    IntMap.empty

(** This function checks various accusation operation injection scenarios.

    The simplest scenario, when all optional arguments are not given, is as
    follows:
    1. Bake a block that publishes a slot.
    2. Bake [lag - 1] blocks.
    3. Build an attestation.
    4. Bake one block before accusing.
    5. Retrieve traps and build an accusation.
    6. Bake a block with the accusation.

    This scenario varies slightly depending on the optional arguments. For their
    use, look first at the relevant tests.
*)
let test_accusation_injection ?initial_blocks_to_bake ?expect_failure
    ?(publish_slot = true) ?(with_dal_content = true) ?(attest_slot = true)
    ?(inclusion_time = Now) ?(not_trap = false) ?(wrong_owner = false)
    ?(traps_fraction = Q.(1 // 2)) () =
  let open Lwt_result_syntax in
  let c = Default_parameters.constants_test in
  let dal = {c.dal with incentives_enable = true; traps_fraction} in
  let cryptobox_parameters = dal.cryptobox_parameters in
  let number_of_slots = dal.number_of_slots in
  let lag = dal.attestation_lag in
  let slot_size = cryptobox_parameters.slot_size in
  let* cryptobox = Dal_helpers.mk_cryptobox cryptobox_parameters in
  let constants =
    {
      c with
      dal;
      consensus_committee_size = cryptobox_parameters.number_of_shards;
      consensus_threshold_size = 0;
    }
  in
  let dal_slot_index =
    Dal.Slot_index.of_int_opt ~number_of_slots 3 |> Stdlib.Option.get
  in
  let other_slot_index =
    Dal.Slot_index.of_int_opt ~number_of_slots 4 |> Stdlib.Option.get
  in
  let commitment, commitment_proof, shards_with_proofs =
    match !commitment_and_proofs_cache with
    | Some result -> result
    | None ->
        Log.info "generate slot and compute commitments and shards" ;
        let slot =
          Tezos_crypto_dal.Cryptobox.Internal_for_tests.generate_slot ~slot_size
        in
        let result =
          Tezt_tezos.Dal_common.Helpers.get_commitment_and_shards_with_proofs
            cryptobox
            ~slot
        in
        (* Cache the result *)
        commitment_and_proofs_cache := Some result ;
        result
  in
  let* genesis, (contract, _contract2) =
    Context.init_with_constants2 constants
  in
  let* blk =
    let blocks_to_bake =
      match inclusion_time with
      | Now -> Option.value ~default:0 initial_blocks_to_bake
      | _ ->
          (* In this case we want the attestation to be as far as possible from
             the accusation, so we want the attestation level to be the first
             level of a cycle. Checking this "extreme" case ensure that slot
             headers are not deleted too soon. *)
          assert (Option.is_none initial_blocks_to_bake) ;
          let blocks_per_cycle = genesis.constants.blocks_per_cycle in
          Int32.(
            rem (sub blocks_per_cycle (of_int lag)) blocks_per_cycle |> to_int)
    in
    if blocks_to_bake > 0 then Log.info "0. Bake %d blocks" blocks_to_bake ;
    Block.bake_n blocks_to_bake genesis
  in

  let slot_header =
    Dal.Operations.Publish_commitment.
      {slot_index = dal_slot_index; commitment; commitment_proof}
  in
  let* op = Op.dal_publish_commitment (B genesis) contract slot_header in
  let* blk =
    if publish_slot then (
      Log.info "1. Bake a block with a publish operation" ;
      Block.bake blk ~operation:op)
    else (
      Log.info "1. Bake a block without a publish operation" ;
      Block.bake blk)
  in

  Log.info "2. Bake 'attestation_lag - 1' blocks" ;
  let* blk = Block.bake_n (lag - 1) blk in

  Log.info "3. Build an attestation" ;
  (match inclusion_time with
  | Now -> ()
  | _ ->
      let position = Block.cycle_position blk |> Int32.to_int in
      Check.(
        (position = 0)
          int
          ~__LOC__
          ~error_msg:"Expected cycle position to be 0, got %L")) ;
  let dal_content =
    if with_dal_content then
      let attestation =
        if attest_slot then Dal.Attestation.(commit empty dal_slot_index)
        else Dal.Attestation.(commit empty other_slot_index)
      in
      Some {attestation}
    else None
  in
  let* shard_assignment = Context.Dal.shards (B blk) () in
  let indexes_to_delegates = indexes_to_delegates shard_assignment in
  let delegate_to_shards_map =
    get_traps_and_non_traps
      ~traps_fraction:blk.constants.dal.traps_fraction
      indexes_to_delegates
      shards_with_proofs
  in
  let delegate =
    match PkhMap.min_binding_opt delegate_to_shards_map with
    | None -> Test.fail ~__LOC__ "Unexpected case: there are no delegates"
    | Some (pkh, _) -> pkh
  in
  let* attester = Context.get_attester ~manager_pkh:delegate (B blk) in
  let attesting_slot = Op.attesting_slot_of_attester attester in
  let* attestation = Op.raw_attestation blk ~attesting_slot ?dal_content in
  let consensus_slot = attesting_slot.slot in
  let attestation_level = blk.header.shell.level in

  let* blk =
    let blocks_to_bake =
      let position = Block.cycle_position blk |> Int32.to_int in
      let blocks_per_cycle = blk.constants.blocks_per_cycle |> Int32.to_int in
      match inclusion_time with
      | Now ->
          (* bake one block such that accusation level is different from the
             attestation level; though this does not really matter *)
          1
      | Last_valid -> (2 * blocks_per_cycle) - 2 - position
      | First_invalid -> (2 * blocks_per_cycle) - 1 - position
    in
    Log.info "4. Bake %d blocks before including an accusation" blocks_to_bake ;
    Block.bake_n blocks_to_bake blk
  in

  Log.info "5. Retrieve traps and build accusation" ;
  let shard_with_proof =
    let owner =
      if wrong_owner then (
        match PkhMap.max_binding delegate_to_shards_map with
        | None -> Test.fail ~__LOC__ "Unexpected case: there are no delegates"
        | Some (pkh, _) ->
            if Signature.Public_key_hash.equal delegate pkh then
              Test.fail
                ~__LOC__
                "Unexpected case: there should be at least two delegates" ;
            pkh)
      else delegate
    in
    match PkhMap.find owner delegate_to_shards_map with
    | None ->
        Test.fail
          ~__LOC__
          "Unexpected case: delegate %a not found in map"
          Signature.Public_key_hash.pp
          owner
    | Some (traps, not_traps) ->
        if not_trap then Stdlib.List.hd not_traps else Stdlib.List.hd traps
  in
  let accusation =
    Op.dal_entrapment
      (B blk)
      attestation
      ~consensus_slot
      dal_slot_index
      shard_with_proof
  in

  Log.info "6. Bake a block with the accusation" ;
  let* blk =
    match expect_failure with
    | None -> Block.bake ~operation:accusation blk
    | Some f ->
        let expect_failure = f attestation_level in
        let* ctxt = Incremental.begin_construction blk in
        let* _ = Incremental.add_operation ctxt accusation ~expect_failure in
        Incremental.finalize_block ctxt
  in
  (* Re-include the accusation in the following cycle and check that it is
     rejected as a duplicate. *)
  match (inclusion_time, expect_failure) with
  | Now, None ->
      let expect_failure = function
        | [
            Environment.Ecoproto_error
              (Validate_errors.Anonymous.Dal_already_denounced {level; _});
          ]
          when Raw_level.to_int32 level = attestation_level ->
            Lwt_result_syntax.return_unit
        | errs ->
            Test.fail
              ~__LOC__
              "Error trace:@, %a does not match the expected one"
              Error_monad.pp_print_trace
              errs
      in
      let* ctxt = Incremental.begin_construction blk in
      let* _ = Incremental.add_operation ctxt accusation ~expect_failure in
      return_unit
  | _ -> return_unit

let test_invalid_accusation_no_dal_content =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_no_dal_content
             {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~with_dal_content:false ~expect_failure

let test_invalid_accusation_slot_not_attested =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_slot_not_attested
             {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~attest_slot:false ~expect_failure

let test_invalid_accusation_slot_not_published =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_slot_not_published
             {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~publish_slot:false ~expect_failure

let test_invalid_accusation_include_late =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Outdated_dal_denunciation {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~inclusion_time:First_invalid ~expect_failure

let test_invalid_accusation_shard_is_not_trap =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_shard_is_not_trap
             {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~not_trap:true ~expect_failure

let test_invalid_accusation_wrong_shard_owner =
  let expect_failure attestation_level = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_wrong_shard_owner
             {level; _});
      ]
      when Raw_level.to_int32 level = attestation_level ->
        Lwt_result_syntax.return_unit
    | errs ->
        Test.fail
          ~__LOC__
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  (* By using [traps_fraction = 1] we are sure that the shard is a trap for the
     attester. Otherwise the validation may fail with "shard is not trap" error
     (which is emitted first). *)
  test_accusation_injection
    ~wrong_owner:true
    ~traps_fraction:Q.one
    ~expect_failure

let tests =
  [
    Tztest.tztest "test valid accusation" `Quick test_accusation_injection;
    Tztest.tztest
      "test invalid accusation (no_dal_content)"
      `Quick
      test_invalid_accusation_no_dal_content;
    Tztest.tztest
      "test invalid accusation (slot_not_attested)"
      `Quick
      test_invalid_accusation_slot_not_attested;
    Tztest.tztest
      "test invalid accusation (slot_not_published)"
      `Quick
      test_invalid_accusation_slot_not_published;
    Tztest.tztest
      "test invalid accusation (include last)"
      `Quick
      (test_accusation_injection ~inclusion_time:Last_valid);
    Tztest.tztest
      "test invalid accusation (include late)"
      `Quick
      test_invalid_accusation_include_late;
    Tztest.tztest
      "test invalid accusation (shard is not trap)"
      `Quick
      test_invalid_accusation_shard_is_not_trap;
    Tztest.tztest
      "test invalid accusation (wrong shard owner)"
      `Quick
      test_invalid_accusation_wrong_shard_owner;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("DAL entrapment", tests)]
  |> Lwt_main.run
