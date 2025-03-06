(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (double baking)
    Invocation:   dune exec src/proto_022_PsRiotum/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_dal_entrapment.ml

    Subject:      A DAL entrapment operation may be injected when it has been observed that
                  a baker has attested a slot for which one the shards is a trap.
*)

open Protocol
open Alpha_context

let commitment_and_proofs_cache = ref None

(** Check various accusation operation injection scenarios. *)
let test_accusation_injection ?(initial_blocks_to_bake = 2) ?expect_failure
    ?(publish_slot = true) ?(with_dal_content = true) ?(attest_slot = true) () =
  let open Lwt_result_syntax in
  let c = Default_parameters.constants_test in
  let dal = {c.dal with incentives_enable = true; traps_fraction = Q.one} in
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
  let slot_index =
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
  let* genesis, contract = Context.init_with_constants1 constants in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7686
     We bake two blocks because we need the accusation to be introduced at level
     at least 10 (2 = 10 - attestation_lag). In protocol S we will not need this
     restriction. *)
  let* blk = Block.bake_n initial_blocks_to_bake genesis in
  let slot_header =
    Dal.Operations.Publish_commitment.{slot_index; commitment; commitment_proof}
  in
  let* op = Op.dal_publish_commitment (B genesis) contract slot_header in
  let* blk =
    if publish_slot then Block.bake blk ~operation:op else Block.bake blk
  in
  let* blk = Block.bake_n (lag - 1) blk in
  let dal_content =
    if with_dal_content then
      let attestation =
        if attest_slot then Dal.Attestation.(commit empty slot_index)
        else Dal.Attestation.(commit empty other_slot_index)
      in
      Some {attestation}
    else None
  in
  let* attestation = Op.raw_attestation blk ?dal_content in
  let (shard, proof), _ = Seq.uncons shards_with_proofs |> Stdlib.Option.get in
  let shard_with_proof = Dal.Shard_with_proof.{shard; proof} in
  let operation =
    Op.dal_entrapment (B blk) attestation slot_index shard_with_proof
  in
  match expect_failure with
  | None ->
      let* _blk_final = Block.bake ~operation blk in
      return_unit
  | Some f ->
      let expect_failure = f blk in
      let* ctxt = Incremental.begin_construction blk in
      let* _ = Incremental.add_operation ctxt operation ~expect_failure in
      return_unit

let test_invalid_accusation_too_close_to_migration =
  let expect_failure blk = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous
           .Denunciations_not_allowed_just_after_migration {level; _});
      ]
      when Raw_level.to_int32 level = blk.Block.header.shell.level ->
        Lwt_result_syntax.return_unit
    | errs ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~initial_blocks_to_bake:1 ~expect_failure

let test_invalid_accusation_no_dal_content =
  let expect_failure blk = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_no_dal_content
            {level; _});
      ]
      when Raw_level.to_int32 level = blk.Block.header.shell.level ->
        Lwt_result_syntax.return_unit
    | errs ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~with_dal_content:false ~expect_failure

let test_invalid_accusation_slot_not_attested =
  let expect_failure blk = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_slot_not_attested
            {level; _});
      ]
      when Raw_level.to_int32 level = blk.Block.header.shell.level ->
        Lwt_result_syntax.return_unit
    | errs ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~attest_slot:false ~expect_failure

let test_invalid_accusation_slot_not_published =
  let expect_failure blk = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Anonymous.Invalid_accusation_slot_not_published
            {level; _});
      ]
      when Raw_level.to_int32 level = blk.Block.header.shell.level ->
        Lwt_result_syntax.return_unit
    | errs ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          errs
  in
  test_accusation_injection ~publish_slot:false ~expect_failure

let tests =
  [
    Tztest.tztest "test valid accusation" `Quick test_accusation_injection;
    Tztest.tztest
      "test invalid accusation (too_close_to_migration)"
      `Quick
      test_invalid_accusation_too_close_to_migration;
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
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("DAL entrapment", tests)]
  |> Lwt_main.run
