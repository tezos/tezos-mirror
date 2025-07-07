(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (aggregate)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_aggregate.ml
*)

open Protocol

(* Init genesis with 8 accounts including at least 5 BLS *)
let init_genesis_with_some_bls_accounts ?policy ?dal_enable
    ?aggregate_attestation () =
  let open Lwt_result_syntax in
  let*? random_accounts = Account.generate_accounts 3 in
  let*? bls_accounts =
    List.init ~when_negative_length:[] 5 (fun _ ->
        Account.new_account ~algo:Signature.Bls ())
  in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts (random_accounts @ bls_accounts)
  in
  let* genesis =
    Block.genesis
      ?dal_enable
      ?aggregate_attestation
      ~consensus_threshold_size:0
      bootstrap_accounts
  in
  let* b = Block.bake ?policy genesis in
  return (genesis, b)

let aggregate_in_mempool_error = function
  | Validate_errors.Consensus.Aggregate_in_mempool -> true
  | _ -> false

let aggregate_disabled_error = function
  | Validate_errors.Consensus.Aggregate_disabled -> true
  | _ -> false

let aggregate_unimplemented_error = function
  | Validate_errors.Consensus.Aggregate_not_implemented -> true
  | _ -> false

let signature_invalid_error = function
  | Operation_repr.Invalid_signature -> true
  | _ -> false

let non_bls_in_aggregate = function
  | Validate_errors.Consensus.Non_bls_key_in_aggregate -> true
  | _ -> false

let conflicting_consensus_operation ?kind = function
  | Validate_errors.Consensus.Conflicting_consensus_operation {kind = kind'; _}
    ->
      Option.fold ~none:true ~some:(fun kind -> kind = kind') kind
  | _ -> false

let unaggregated_eligible_attestation ?kind = function
  | Validate_errors.Consensus.Unaggregated_eligible_operation {kind = kind'; _}
    ->
      Option.fold ~none:true ~some:(fun kind -> kind = kind') kind
  | _ -> false

let empty_aggregation_committee = function
  | Validate_errors.Consensus.Empty_aggregation_committee -> true
  | _ -> false

let find_preattestations_aggregate_result receipt =
  let result_opt =
    List.find_map
      (function
        | Tezos_protocol_alpha__Protocol.Apply_results.Operation_metadata
            {
              contents =
                Single_result (Preattestations_aggregate_result _ as result);
            } ->
            Some result
        | _ -> None)
      receipt
  in
  match result_opt with
  | Some res -> res
  | None -> Test.fail "No preattestations aggregate result found"

let find_attestations_aggregate_result receipt =
  let result_opt =
    List.find_map
      (function
        | Tezos_protocol_alpha__Protocol.Apply_results.Operation_metadata
            {
              contents =
                Single_result (Attestations_aggregate_result _ as result);
            } ->
            Some result
        | _ -> None)
      receipt
  in
  match result_opt with
  | Some res -> res
  | None -> Test.fail "No attestations aggregate result found"

type 'kind aggregate =
  | Preattestation : Alpha_context.Kind.preattestations_aggregate aggregate
  | Attestation : Alpha_context.Kind.attestations_aggregate aggregate

let check_aggregate_result (type kind) (kind : kind aggregate) ~attesters
    (result : kind Tezos_protocol_alpha__Protocol.Apply_results.contents_result)
    =
  let open Lwt_result_syntax in
  match (kind, result) with
  | ( Preattestation,
      Preattestations_aggregate_result
        {
          balance_updates;
          committee = resulting_committee;
          total_consensus_power;
        } )
  | ( Attestation,
      Attestations_aggregate_result
        {
          balance_updates;
          committee = resulting_committee;
          total_consensus_power;
        } ) ->
      (* Check balance updates *)
      let* () =
        match balance_updates with
        | [] -> return_unit
        | _ -> Test.fail "Unexpected non-empty balance updates list"
      in
      (* Check total voting power *)
      let* () =
        let voting_power =
          List.fold_left
            (fun acc (delegate : RPC.Validators.t) ->
              List.length delegate.slots + acc)
            0
            attesters
        in
        if voting_power = total_consensus_power then return_unit
        else
          Test.fail
            "Wrong voting power : expected %d, found %d"
            voting_power
            total_consensus_power
      in
      (* Check committee *)
      let expected_committee =
        List.map
          (fun {Context.delegate; consensus_key = consensus_pkh; slots; _} ->
            let power = List.length slots in
            ({Alpha_context.Consensus_key.delegate; consensus_pkh}, power))
          attesters
      in
      if
        List.equal
          (fun ( {Alpha_context.Consensus_key.delegate = d1; consensus_pkh = c1},
                 power1 )
               ({delegate = d2; consensus_pkh = c2}, power2) ->
            Signature.Public_key_hash.equal d1 d2
            && Signature.Public_key_hash.equal c1 c2
            && Int.equal power1 power2)
          resulting_committee
          expected_committee
      then return_unit
      else
        let pp =
          Format.pp_print_list (fun fmt (consensus_key, power) ->
              Format.fprintf
                fmt
                "%a with power %d"
                Alpha_context.Consensus_key.pp
                consensus_key
                power)
        in
        Test.fail
          "@[<v 0>Wrong committee@,\
           @[<v 2>expected:@,\
           %a@]@,\
           @[<v 2>found:@,\
           %a@]@]"
          pp
          expected_committee
          pp
          resulting_committee

(* [check_preattestations_aggregate_result ~committee result] verifies that
   [result] has the following properties:
   - [balance_update] is empty;
   - [voting_power] equals the sum of slots owned by attesters in [committee];
   - the public key hashes in [result] committee match those of [committee]. *)
let check_preattestations_aggregate_result ~attesters
    (result :
      Alpha_context.Kind.preattestations_aggregate
      Tezos_protocol_alpha__Protocol.Apply_results.contents_result) =
  check_aggregate_result Preattestation ~attesters result

(* [check_attestations_aggregate_result ~committee result] verifies that
   [result] has the following properties:
   - [balance_update] is empty;
   - [voting_power] equals the sum of slots owned by attesters in [committee];
   - the public key hashes in [result] committee match those of [committee]. *)
let check_attestations_aggregate_result ~attesters
    (result :
      Alpha_context.Kind.attestations_aggregate
      Tezos_protocol_alpha__Protocol.Apply_results.contents_result) =
  check_aggregate_result Attestation ~attesters result

let test_aggregate_feature_flag_enabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let test_aggregate_feature_flag_disabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:false ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~application_error:aggregate_disabled_error
    ~construction_error:aggregate_disabled_error
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let test_attestations_aggregate_with_a_single_delegate () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attester = Context.get_attester_with_bls_key (B attested_block) in
  let attesting_slot = Op.attesting_slot_of_attester attester in
  let* operation =
    Op.attestations_aggregate ~committee:[(attesting_slot, None)] attested_block
  in
  let* _, (_, receipt) = Block.bake_with_metadata ~operation attested_block in
  let result = find_attestations_aggregate_result receipt in
  check_attestations_aggregate_result ~attesters:[attester] result

let test_preattestations_aggregate_with_a_single_delegate () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* block' = Block.bake block in
  let* attester = Context.get_attester_with_bls_key (B block') in
  let attesting_slot = Op.attesting_slot_of_attester attester in
  let* operation =
    Op.preattestations_aggregate ~committee:[attesting_slot] block'
  in
  let* _, (_, receipt) =
    let round_zero = Alpha_context.Round.zero in
    Block.bake_with_metadata
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operation
      block
  in
  let result = find_preattestations_aggregate_result receipt in
  check_preattestations_aggregate_result ~attesters:[attester] result

let test_attestations_aggregate_with_multiple_delegates () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesters = Context.get_attesters_with_bls_key (B block) in
  let committee =
    List.map
      (fun attester -> (Op.attesting_slot_of_attester attester, None))
      attesters
  in
  let* operation = Op.attestations_aggregate ~committee block in
  let* _, (_, receipt) = Block.bake_with_metadata ~operation block in
  let result = find_attestations_aggregate_result receipt in
  check_attestations_aggregate_result ~attesters result

let test_preattestations_aggregate_with_multiple_delegates () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* block' = Block.bake block in
  let* attesters = Context.get_attesters_with_bls_key (B block') in
  let committee = List.map Op.attesting_slot_of_attester attesters in
  let* operation = Op.preattestations_aggregate ~committee block' in
  let* _, (_, receipt) =
    let round_zero = Alpha_context.Round.zero in
    Block.bake_with_metadata
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operation
      block
  in
  let result = find_preattestations_aggregate_result receipt in
  check_preattestations_aggregate_result ~attesters result

let test_attestations_aggregate_invalid_signature () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* aggregate = Op.attestations_aggregate block in
  (* Swap the signature for Signature.Bls.zero *)
  match aggregate.protocol_data with
  | Operation_data {contents; _} ->
      let aggregate_with_incorrect_signature =
        {
          aggregate with
          protocol_data =
            Operation_data {contents; signature = Some (Bls Signature.Bls.zero)};
        }
      in
      (* Bake a block containing this operation and expect an error *)
      let*! res =
        Block.bake ~operation:aggregate_with_incorrect_signature block
      in
      Assert.proto_error ~loc:__LOC__ res signature_invalid_error

let test_preattestations_aggregate_invalid_signature () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* block' = Block.bake block in
  let* aggregate = Op.preattestations_aggregate block' in
  (* Swap the aggregate signature for Signature.Bls.zero *)
  match aggregate.protocol_data with
  | Operation_data {contents; _} ->
      let aggregate_with_incorrect_signature =
        {
          aggregate with
          protocol_data =
            Operation_data {contents; signature = Some (Bls Signature.Bls.zero)};
        }
      in
      (* Bake a block containing this operation and expect an error *)
      let*! res =
        let round_zero = Alpha_context.Round.zero in
        Block.bake
          ~policy:(By_round 1)
          ~payload_round:round_zero
          ~locked_round:round_zero
          ~operation:aggregate_with_incorrect_signature
          block
      in
      Assert.proto_error ~loc:__LOC__ res signature_invalid_error

let test_preattestations_aggregate_non_bls_delegate () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* block' = Block.bake block in
  (* Find an attester with a non-BLS consensus key. *)
  let* attesting_slot =
    Op.get_attesting_slot_with_non_bls_key ~attested_block:block'
  in
  (* Craft a preattestation for this attester to retrieve a signature and a
     triplet {level, round, block_payload_hash} *)
  let* {shell; protocol_data = {contents; signature}} =
    Op.raw_preattestation ~attesting_slot block'
  in
  match contents with
  | Single (Preattestation consensus_content) ->
      let {level; round; block_payload_hash; _} :
          Alpha_context.consensus_content =
        consensus_content
      in
      (* Craft an aggregate including the attester slot and signature *)
      let consensus_content : Alpha_context.consensus_aggregate_content =
        {level; round; block_payload_hash}
      in
      let contents : _ Alpha_context.contents_list =
        Single
          (Preattestations_aggregate
             {consensus_content; committee = [attesting_slot.slot]})
      in
      let operation : operation =
        {shell; protocol_data = Operation_data {contents; signature}}
      in
      (* Bake a block containing this aggregate and expect an error *)
      let*! res =
        let round_zero = Alpha_context.Round.zero in
        Block.bake
          ~policy:(By_round 1)
          ~payload_round:round_zero
          ~locked_round:round_zero
          ~operation
          block
      in
      Assert.proto_error ~loc:__LOC__ res non_bls_in_aggregate

let test_attestations_aggregate_non_bls_delegate () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  (* Find an attester with a non-BLS consensus key. *)
  let* attesting_slot =
    Op.get_attesting_slot_with_non_bls_key ~attested_block:block
  in
  (* Craft an attestation for this attester to retrieve a signature and a
     triplet {level, round, block_payload_hash} *)
  let* {shell; protocol_data = {contents; signature}} =
    Op.raw_attestation ~attesting_slot block
  in
  let (Single
        (Attestation
          {consensus_content = {level; round; block_payload_hash; _}; _})) =
    contents
  in
  (* Craft an aggregate including the attester slot and signature and
     various dal_contents *)
  let consensus_content : Alpha_context.consensus_aggregate_content =
    {level; round; block_payload_hash}
  in
  let check_non_bls_aggregate_refused dal_content =
    let contents : _ Alpha_context.contents_list =
      Single
        (Attestations_aggregate
           {consensus_content; committee = [(attesting_slot.slot, dal_content)]})
    in
    let operation : operation =
      {shell; protocol_data = Operation_data {contents; signature}}
    in
    Op.check_validation_and_application_all_modes_different_outcomes
      ~loc:__LOC__
      ~application_error:non_bls_in_aggregate
      ~construction_error:non_bls_in_aggregate
      ~mempool_error:aggregate_in_mempool_error
      ~predecessor:block
      operation
  in
  List.iter_es check_non_bls_aggregate_refused Dal_helpers.various_dal_contents

let test_multiple_aggregates_per_block_forbidden () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  (* Retrieve delegates with BLS keys that have at least one slot *)
  let* committee = Op.default_committee ~attested_block:block in
  (* Craft one attestations_aggregate per attester *)
  let* aggregates =
    List.map_es
      (fun attesting_slot ->
        Op.attestations_aggregate ~committee:[(attesting_slot, None)] block)
      committee
  in
  (* Bake a block containing the multiple aggregates and expect an error *)
  let*! res = Block.bake ~operations:aggregates block in
  let* () =
    Assert.proto_error
      ~loc:__LOC__
      res
      (conflicting_consensus_operation
         ~kind:Validate_errors.Consensus.Attestations_aggregate)
  in
  (* Craft one preattestations_aggregate per attester *)
  let* block' = Block.bake block in
  let* committee = Op.default_committee ~attested_block:block' in
  let* aggregates =
    List.map_es
      (fun attesting_slot ->
        Op.preattestations_aggregate ~committee:[attesting_slot] block')
      committee
  in
  (* Bake a block containing the multiple aggregates and expect an error *)
  let round_zero = Alpha_context.Round.zero in
  let*! res =
    Block.bake
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operations:aggregates
      block
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (conflicting_consensus_operation
       ~kind:Validate_errors.Consensus.Preattestations_aggregate)

let test_eligible_preattestation_must_be_aggregated () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* block' = Block.bake block in
  let* attesting_slot =
    Op.get_attesting_slot_with_bls_key ~attested_block:block'
  in
  let* operation = Op.preattestation ~attesting_slot block' in
  (* Operation is valid in the Mempool *)
  let* inc = Incremental.begin_construction ~mempool_mode:true block in
  let* inc = Incremental.add_operation inc operation in
  let* _ = Incremental.finalize_block inc in
  (* Operation is invalid in a block *)
  let*! res =
    let round_zero = Alpha_context.Round.zero in
    Block.bake
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operation
      block
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (unaggregated_eligible_attestation
       ~kind:Validate_errors.Consensus.Preattestation)

let test_eligible_attestation_must_be_aggregated () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesting_slot =
    Op.get_attesting_slot_with_bls_key ~attested_block:block
  in
  let* operation = Op.attestation ~attesting_slot block in
  (* Operation is valid in the Mempool *)
  let* inc = Incremental.begin_construction ~mempool_mode:true block in
  let* inc = Incremental.add_operation inc operation in
  let* _ = Incremental.finalize_block inc in
  (* Operation is invalid in a block *)
  let*! res = Block.bake ~operation block in
  Assert.proto_error
    ~loc:__LOC__
    res
    (unaggregated_eligible_attestation
       ~kind:Validate_errors.Consensus.Attestation)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/7827
   Also test this behaviour for attestations with DAL contents. *)

let test_empty_committee () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  (* Crafting an attestations_aggregate with an empty committee *)
  let* consensus_content =
    let* attestation = Op.raw_attestation block in
    match attestation.protocol_data with
    | {contents = Single (Attestation {consensus_content; _}); _} ->
        let Alpha_context.{level; round; block_payload_hash; slot = _} =
          consensus_content
        in
        return Alpha_context.{level; round; block_payload_hash}
  in
  let contents =
    Alpha_context.Attestations_aggregate {consensus_content; committee = []}
  in
  let signature = Some Signature.(of_bls Signature.Bls.zero) in
  let operation = Op.pack_operation (B block) signature (Single contents) in
  (* Baking with the attestations_aggregate and expecting an error *)
  let*! res = Block.bake ~operation block in
  let* () = Assert.proto_error ~loc:__LOC__ res empty_aggregation_committee in
  (* Crafting a preattestations_aggregate with an empty committee *)
  let* consensus_content =
    let* block = Block.bake block in
    let* preattestation = Op.raw_preattestation block in
    match preattestation.protocol_data with
    | {contents = Single (Preattestation consensus_content); _} ->
        let Alpha_context.{level; round; block_payload_hash; slot = _} =
          consensus_content
        in
        return Alpha_context.{level; round; block_payload_hash}
  in
  let contents =
    Alpha_context.Preattestations_aggregate {consensus_content; committee = []}
  in
  let operation = Op.pack_operation (B block) signature (Single contents) in
  (* Baking with the preattestations_aggregate and expecting an error *)
  let round_zero = Alpha_context.Round.zero in
  let*! res =
    Block.bake
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operation
      block
  in
  let* () = Assert.proto_error ~loc:__LOC__ res empty_aggregation_committee in
  return_unit

let test_metadata_committee_is_correctly_ordered () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  (* Craft an attestations_aggregate including at least 3 delegates *)
  let* attestations, attestation_committee =
    let* attesting_slots = Op.default_committee ~attested_block:block in
    let committee =
      List.map (fun attesting_slot -> (attesting_slot, None)) attesting_slots
    in
    assert (List.length committee > 2) ;
    let* aggregate = Op.attestations_aggregate ~committee block in
    return (aggregate, attesting_slots)
  in
  (* Craft a preattestations_aggregate including at least 3 delegates *)
  let* preattestations, preattestation_committee =
    let* block' = Block.bake block in
    let* committee = Op.default_committee ~attested_block:block' in
    assert (List.length committee > 2) ;
    let* aggregate = Op.preattestations_aggregate ~committee block' in
    return (aggregate, committee)
  in
  (* Bake a block including both aggregates *)
  let* _, (_, receipt) =
    let round_zero = Alpha_context.Round.zero in
    Block.bake_with_metadata
      ~policy:(By_round 1)
      ~payload_round:round_zero
      ~locked_round:round_zero
      ~operations:[attestations; preattestations]
      block
  in
  (* [check_committees] checks that the operation committee and the operation
     result committee coincide *)
  let check_committees ~loc committee result_committee =
    let result_committee =
      List.map
        (fun ((key : Alpha_context.Consensus_key.t), _) -> key.consensus_pkh)
        result_committee
    in
    let* () =
      Assert.assert_equal_list
        ~loc
        Signature.Public_key_hash.equal
        "committee"
        Signature.Public_key_hash.pp
        committee
        result_committee
    in
    return_unit
  in
  (* Check that the attestations_aggregate committees coincide *)
  let* () =
    let attestations_aggregate_result =
      find_attestations_aggregate_result receipt
    in
    match (attestations.protocol_data, attestations_aggregate_result) with
    | ( Operation_data
          {contents = Single (Attestations_aggregate {committee; _}); _},
        Attestations_aggregate_result {committee = result_committee; _} ) ->
        let committee =
          List.map
            (fun (slot, _) ->
              let owner =
                WithExceptions.Option.get ~loc:__LOC__
                @@ List.find_opt
                     (fun attesting_slot ->
                       Alpha_context.Slot.equal attesting_slot.Op.slot slot)
                     attestation_committee
              in
              owner.consensus_pkh)
            committee
        in
        check_committees ~loc:__LOC__ committee result_committee
    | _ -> assert false
  in
  (* Check that the preattestations_aggregate committees coincide *)
  let* () =
    let preattestations_aggregate_result =
      find_preattestations_aggregate_result receipt
    in
    match (preattestations.protocol_data, preattestations_aggregate_result) with
    | ( Operation_data
          {contents = Single (Preattestations_aggregate {committee; _}); _},
        Preattestations_aggregate_result {committee = result_committee; _} ) ->
        let committee =
          List.map
            (fun slot ->
              let owner =
                WithExceptions.Option.get ~loc:__LOC__
                @@ List.find_opt
                     (fun attesting_slot ->
                       Alpha_context.Slot.equal attesting_slot.Op.slot slot)
                     preattestation_committee
              in
              owner.consensus_pkh)
            committee
        in
        check_committees ~loc:__LOC__ committee result_committee
    | _ -> assert false
  in
  return_unit

let test_preattestation_signature_for_attestation ~attested_block
    ~attesting_slot =
  let open Lwt_result_syntax in
  let* op_preattestation = Op.preattestation ~attesting_slot attested_block in
  let* op_attestation = Op.attestation ~attesting_slot attested_block in
  let op_attestation_with_preattestation_signature =
    Op.copy_op_signature ~src:op_preattestation ~dst:op_attestation
  in
  let* () =
    Op.check_validation_and_application
      ~loc:__LOC__
      ~predecessor:attested_block
      ~error:signature_invalid_error
      Mempool
      op_attestation_with_preattestation_signature
  in
  let op_preattestation_with_attestation_signature =
    Op.copy_op_signature ~dst:op_preattestation ~src:op_attestation
  in
  let* () =
    Op.check_validation_and_application
      ~loc:__LOC__
      ~predecessor:attested_block
      ~error:signature_invalid_error
      Mempool
      op_preattestation_with_attestation_signature
  in
  return_unit

let test_preattestation_signature_for_attestation_non_bls () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n 5 ~aggregate_attestation:true () in
  let* attested_block = Block.bake genesis in
  let* attesting_slot =
    Op.get_attesting_slot_with_non_bls_key ~attested_block
  in
  test_preattestation_signature_for_attestation ~attesting_slot ~attested_block

let test_preattestation_signature_for_attestation_bls () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesting_slot = Op.get_attesting_slot_with_bls_key ~attested_block in
  test_preattestation_signature_for_attestation ~attesting_slot ~attested_block

let test_signature_bls_attestation_with_different_slot () =
  let open Lwt_result_syntax in
  let* _genesis, block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attester = Context.get_attester_with_bls_key (B block) in
  let consensus_pkh = attester.consensus_key in
  let slot1, slot2 =
    match attester.slots with
    | slot1 :: slot2 :: _ -> (slot1, slot2)
    | _ -> Test.fail ~__LOC__ "Delegate must have at least two slots"
  in
  let* op_attestation1 =
    Op.attestation ~attesting_slot:{slot = slot1; consensus_pkh} block
  in
  let* op_attestation2 =
    Op.attestation ~attesting_slot:{slot = slot2; consensus_pkh} block
  in
  Assert.equal
    ~loc:__LOC__
    (Option.equal Signature.equal)
    "Signatures must be equal"
    (Format.pp_print_option Signature.pp)
    (Op.get_op_signature op_attestation1)
    (Op.get_op_signature op_attestation2)

let test_signature_bls_attestation_with_different_level () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  let* attesting_slot = Op.get_attesting_slot_with_bls_key ~attested_block in
  let*? level1 = Context.get_level (B attested_block) in
  let level2 = Alpha_context.Raw_level.add level1 1 in
  let* op_attestation1 =
    Op.attestation ~level:level1 ~attesting_slot attested_block
  in
  let* op_attestation2 =
    Op.attestation ~level:level2 ~attesting_slot attested_block
  in
  Assert.not_equal
    ~loc:__LOC__
    (Option.equal Signature.equal)
    "Signatures must be not equal"
    (Format.pp_print_option Signature.pp)
    (Op.get_op_signature op_attestation1)
    (Op.get_op_signature op_attestation2)

let tests =
  [
    Tztest.tztest
      "test_aggregate_feature_flag_enabled"
      `Quick
      test_aggregate_feature_flag_enabled;
    Tztest.tztest
      "test_aggregate_feature_flag_disabled"
      `Quick
      test_aggregate_feature_flag_disabled;
    Tztest.tztest
      "test_preattestations_aggregate_with_a_single_delegate"
      `Quick
      test_preattestations_aggregate_with_a_single_delegate;
    Tztest.tztest
      "test_attestations_aggregate_with_a_single_delegate"
      `Quick
      test_attestations_aggregate_with_a_single_delegate;
    Tztest.tztest
      "test_preattestations_aggregate_with_multiple_delegates"
      `Quick
      test_preattestations_aggregate_with_multiple_delegates;
    Tztest.tztest
      "test_attestations_aggregate_with_multiple_delegates"
      `Quick
      test_attestations_aggregate_with_multiple_delegates;
    Tztest.tztest
      "test_preattestations_aggregate_invalid_signature"
      `Quick
      test_preattestations_aggregate_invalid_signature;
    Tztest.tztest
      "test_attestations_aggregate_invalid_signature"
      `Quick
      test_attestations_aggregate_invalid_signature;
    Tztest.tztest
      "test_preattestations_aggregate_non_bls_delegate"
      `Quick
      test_preattestations_aggregate_non_bls_delegate;
    Tztest.tztest
      "test_attestations_aggregate_non_bls_delegate"
      `Quick
      test_attestations_aggregate_non_bls_delegate;
    Tztest.tztest
      "test_multiple_aggregates_per_block_forbidden"
      `Quick
      test_multiple_aggregates_per_block_forbidden;
    Tztest.tztest
      "test_eligible_preattestation_must_be_aggregated"
      `Quick
      test_eligible_preattestation_must_be_aggregated;
    Tztest.tztest
      "test_eligible_attestation_must_be_aggregated"
      `Quick
      test_eligible_attestation_must_be_aggregated;
    Tztest.tztest "test_empty_committee" `Quick test_empty_committee;
    Tztest.tztest
      "test_metadata_committee_is_correctly_ordered"
      `Quick
      test_metadata_committee_is_correctly_ordered;
    Tztest.tztest
      "Use preattestation signature for attestation (non BLS)"
      `Quick
      test_preattestation_signature_for_attestation_non_bls;
    Tztest.tztest
      "Use preattestation signature for attestation (BLS)"
      `Quick
      test_preattestation_signature_for_attestation_bls;
    Tztest.tztest
      "Signatures for bls attestations with different slots are equal"
      `Quick
      test_signature_bls_attestation_with_different_slot;
    Tztest.tztest
      "Signatures for bls attestations with different levels are different"
      `Quick
      test_signature_bls_attestation_with_different_level;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("aggregate", tests)]
  |> Lwt_main.run
