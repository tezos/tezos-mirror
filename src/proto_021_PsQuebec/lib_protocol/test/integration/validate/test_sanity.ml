(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (validate manager)
    Invocation: dune exec src/proto_021_PtQenaB1/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_sanity.ml
    Subject:    Validation of operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

let register_test =
  Tezt_helpers.register_test_es ~__FILE__ ~file_tags:["validation"; "operation"]

(** The goal of this test is to ensure that [select_op] generate the
   wanted kind of manager operation

    Note: if a new manager operation kind is added in the protocol,
   [Manager_operation_helpers.manager_operation_kind] should be
   extended. You will also have to extend
   [Manager_operation_helpers.select_op] with a new `mk` for this new
   operation. Finally the list [Manager_operation_helpers.subjects]
   should also be extended to run the validate test on the new manager
   operation kind. *)
let ensure_kind infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {(operation_req_default kind) with force_reveal = Some false}
      infos
  in
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Manager_operation {operation; _}) -> (
      match (operation, kind) with
      | Transaction _, K_Transaction
      | Reveal _, K_Reveal
      | Origination _, K_Origination
      | Delegation _, K_Delegation
      | Delegation _, K_Undelegation
      | Delegation _, K_Self_delegation
      | Register_global_constant _, K_Register_global_constant
      | Set_deposits_limit _, K_Set_deposits_limit
      | Update_consensus_key _, K_Update_consensus_key
      | Increase_paid_storage _, K_Increase_paid_storage
      | Transfer_ticket _, K_Transfer_ticket
      | Sc_rollup_originate _, K_Sc_rollup_origination
      | Sc_rollup_add_messages _, K_Sc_rollup_add_messages
      | Sc_rollup_cement _, K_Sc_rollup_cement
      | Sc_rollup_publish _, K_Sc_rollup_publish
      | Sc_rollup_refute _, K_Sc_rollup_refute
      | Sc_rollup_timeout _, K_Sc_rollup_timeout
      | Sc_rollup_execute_outbox_message _, K_Sc_rollup_execute_outbox_message
      | Sc_rollup_recover_bond _, K_Sc_rollup_recover_bond
      | Dal_publish_commitment _, K_Dal_publish_commitment
      | Zk_rollup_origination _, K_Zk_rollup_origination
      | Zk_rollup_publish _, K_Zk_rollup_publish
      | Zk_rollup_update _, K_Zk_rollup_update ->
          return_unit
      | ( ( Transaction _ | Origination _ | Register_global_constant _
          | Delegation _ | Set_deposits_limit _ | Update_consensus_key _
          | Increase_paid_storage _ | Reveal _ | Transfer_ticket _
          | Sc_rollup_originate _ | Sc_rollup_publish _ | Sc_rollup_cement _
          | Sc_rollup_add_messages _ | Sc_rollup_refute _ | Sc_rollup_timeout _
          | Sc_rollup_execute_outbox_message _ | Sc_rollup_recover_bond _
          | Dal_publish_commitment _ | Zk_rollup_origination _
          | Zk_rollup_publish _ | Zk_rollup_update _ ),
          _ ) ->
          assert false)
  | Single _ -> assert false
  | Cons _ -> assert false

let ensure_manager_operation_coverage () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  List.iter_es (fun kind -> ensure_kind infos kind) subjects

open Generator_descriptors
open Valid_operations_generators

(** This test ensures that it exists a valid operation generator for
    each operation.

    Note: When adding a new operation, one should refer to {!
    Generator_descriptor} to see how to add its valid operation
    generator. *)
let covalidation_sanity () =
  let open Lwt_result_syntax in
  let max_batch_size = 1 in
  let nb_bootstrap = 7 in
  List.iter_es
    (fun kind ->
      let* _, candidates = covalid [kind] ~nb_bootstrap ~max_batch_size in
      match List.hd candidates with
      | None ->
          failwith "no candidates was generated for kind '%a'" pp_op_kind kind
      | Some {protocol_data = Operation_data {contents; _}; _} -> (
          match (contents, kind) with
          | Single (Preattestation _), KPreattestation -> return_unit
          | Single (Preattestation _), _ -> assert false
          | Single (Attestation _), KAttestation -> return_unit
          | Single (Attestation _), _ -> assert false
          | Single (Seed_nonce_revelation _), KNonce -> return_unit
          | Single (Seed_nonce_revelation _), _ -> assert false
          | Single (Vdf_revelation _), KVdf -> return_unit
          | Single (Vdf_revelation _), _ -> assert false
          | Single (Double_attestation_evidence _), KDbl_consensus ->
              return_unit
          | Single (Double_attestation_evidence _), _ -> assert false
          | Single (Double_preattestation_evidence _), KDbl_consensus ->
              return_unit
          | Single (Double_preattestation_evidence _), _ -> assert false
          | Single (Double_baking_evidence _), KDbl_baking -> return_unit
          | Single (Double_baking_evidence _), _ -> assert false
          | Single (Activate_account _), KActivate -> return_unit
          | Single (Activate_account _), _ -> assert false
          | Single (Proposals _), KProposals -> return_unit
          | Single (Proposals _), _ -> assert false
          | Single (Ballot _), (KBallotExp | KBallotProm) -> return_unit
          | Single (Ballot _), _ -> assert false
          | Single (Drain_delegate _), KDrain -> return_unit
          | Single (Drain_delegate _), _ -> assert false
          | Single (Manager_operation _), KManager
          | Cons (Manager_operation _, _), KManager ->
              return_unit
          | Single (Manager_operation _), _ | Cons (Manager_operation _, _), _
            ->
              assert false
          | Single (Failing_noop _), _ -> assert false))
    all_kinds

let () =
  register_test
    ~title:"manager operation coverage"
    ensure_manager_operation_coverage

let () = register_test ~title:"covalidation coverage" covalidation_sanity
