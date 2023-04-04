(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Alpha_context

type error +=
  | (* Permanent *) Sc_rollup_invalid_parameters_type
  | (* Permanent *) Sc_rollup_invalid_last_cemented_commitment
  | (* Permanent *) Sc_rollup_invalid_output_proof
  | (* Permanent *) Sc_rollup_invalid_outbox_level

type execute_outbox_message_result = {
  paid_storage_size_diff : Z.t;
  ticket_receipt : Ticket_receipt.t;
  operations : Script_typed_ir.packed_internal_operation list;
}

let () =
  let description = "Invalid parameters type for smart rollup" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_invalid_parameters_type"
    ~title:"Invalid parameters type"
    ~description
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" description)
    Data_encoding.unit
    (function Sc_rollup_invalid_parameters_type -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_parameters_type) ;
  let description = "Invalid last-cemented-commitment" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_invalid_last_cemented_commitment"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function
      | Sc_rollup_invalid_last_cemented_commitment -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_last_cemented_commitment) ;
  let description = "Invalid output proof" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_invalid_output_proof"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_invalid_output_proof -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_output_proof) ;
  let description = "Invalid outbox level" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_invalid_outbox_level"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Sc_rollup_invalid_outbox_level -> Some () | _ -> None)
    (fun () -> Sc_rollup_invalid_outbox_level)

type origination_result = {
  address : Sc_rollup.Address.t;
  size : Z.t;
  genesis_commitment_hash : Sc_rollup.Commitment.Hash.t;
}

type 'ret continuation = unit -> 'ret tzresult

(* Only a subset of types are supported for rollups.
   This function checks whether or not a type can be used for a rollup. *)
let rec validate_ty :
    type a ac ret.
    (a, ac) Script_typed_ir.ty ->
    a Script_typed_ir.entrypoints_node ->
    ret continuation ->
    ret tzresult =
 fun ty {nested = nested_entrypoints; at_node} k ->
  let open Script_typed_ir in
  match at_node with
  | Some {name = _; original_type_expr = _} ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4023
         We currently don't support entrypoints as the entrypoint information
         for L1 to L2 messages is not propagated to the rollup. *)
      error Sc_rollup_invalid_parameters_type
  | None -> (
      match ty with
      (* Valid primitive types. *)
      | Unit_t -> (k [@ocaml.tailcall]) ()
      | Int_t -> (k [@ocaml.tailcall]) ()
      | Nat_t -> (k [@ocaml.tailcall]) ()
      | Signature_t -> (k [@ocaml.tailcall]) ()
      | String_t -> (k [@ocaml.tailcall]) ()
      | Bytes_t -> (k [@ocaml.tailcall]) ()
      | Key_hash_t -> (k [@ocaml.tailcall]) ()
      | Key_t -> (k [@ocaml.tailcall]) ()
      | Timestamp_t -> (k [@ocaml.tailcall]) ()
      | Address_t -> (k [@ocaml.tailcall]) ()
      | Bls12_381_g1_t -> (k [@ocaml.tailcall]) ()
      | Bls12_381_g2_t -> (k [@ocaml.tailcall]) ()
      | Bls12_381_fr_t -> (k [@ocaml.tailcall]) ()
      | Bool_t -> (k [@ocaml.tailcall]) ()
      | Never_t -> (k [@ocaml.tailcall]) ()
      | Tx_rollup_l2_address_t -> (k [@ocaml.tailcall]) ()
      | Chain_id_t -> (k [@ocaml.tailcall]) ()
      (* Valid collection types. *)
      | Ticket_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty no_entrypoints k
      | Set_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty no_entrypoints k
      | Option_t (ty, _, _) ->
          (validate_ty [@ocaml.tailcall]) ty no_entrypoints k
      | List_t (ty, _) -> (validate_ty [@ocaml.tailcall]) ty no_entrypoints k
      | Pair_t (ty1, ty2, _, _) ->
          (* Entrypoints may not be nested in pairs, hence the no_entrypoints
             value. *)
          (validate_two_tys [@ocaml.tailcall])
            ty1
            ty2
            no_entrypoints
            no_entrypoints
            k
      | Or_t (ty1, ty2, _, _) ->
          let entrypoints_l, entrypoints_r =
            match nested_entrypoints with
            | Entrypoints_None -> (no_entrypoints, no_entrypoints)
            | Entrypoints_Or {left; right} -> (left, right)
          in
          (validate_two_tys [@ocaml.tailcall])
            ty1
            ty2
            entrypoints_l
            entrypoints_r
            k
      | Map_t (key_ty, val_ty, _) ->
          (* Entrypoints may not be nested in maps, hence the no_entrypoints
             value. *)
          (validate_two_tys [@ocaml.tailcall])
            key_ty
            val_ty
            no_entrypoints
            no_entrypoints
            k
      (* Invalid types. *)
      | Mutez_t -> error Sc_rollup_invalid_parameters_type
      | Big_map_t (_key_ty, _val_ty, _) ->
          error Sc_rollup_invalid_parameters_type
      | Contract_t _ -> error Sc_rollup_invalid_parameters_type
      | Sapling_transaction_t _ -> error Sc_rollup_invalid_parameters_type
      | Sapling_transaction_deprecated_t _ ->
          error Sc_rollup_invalid_parameters_type
      | Sapling_state_t _ -> error Sc_rollup_invalid_parameters_type
      | Operation_t -> error Sc_rollup_invalid_parameters_type
      | Chest_t -> error Sc_rollup_invalid_parameters_type
      | Chest_key_t -> error Sc_rollup_invalid_parameters_type
      | Lambda_t (_, _, _) -> error Sc_rollup_invalid_parameters_type)

and validate_two_tys :
    type a ac b bc ret.
    (a, ac) Script_typed_ir.ty ->
    (b, bc) Script_typed_ir.ty ->
    a Script_typed_ir.entrypoints_node ->
    b Script_typed_ir.entrypoints_node ->
    ret continuation ->
    ret tzresult =
 fun ty1 ty2 entrypoints1 entrypoints2 k ->
  (validate_ty [@ocaml.tailcall]) ty1 entrypoints1 (fun () ->
      (validate_ty [@ocaml.tailcall]) ty2 entrypoints2 k)

let validate_parameters_ty :
    type a ac.
    context ->
    (a, ac) Script_typed_ir.ty ->
    a Script_typed_ir.entrypoints_node ->
    context tzresult =
 fun ctxt parameters_ty entrypoints ->
  let open Result_syntax in
  let* ctxt =
    Gas.consume
      ctxt
      (Sc_rollup_costs.is_valid_parameters_ty_cost
         ~ty_size:Script_typed_ir.(ty_size parameters_ty |> Type_size.to_int))
  in
  let+ () = validate_ty parameters_ty entrypoints ok in
  ctxt

let validate_untyped_parameters_ty ctxt parameters_ty =
  let open Result_syntax in
  (* Parse the type and check that the entrypoints are well-formed. Using
     [parse_parameter_ty_and_entrypoints] restricts to [passable] types
     (everything but operations), which is OK since [validate_ty] constraints
     the type further. *)
  let* ( Ex_parameter_ty_and_entrypoints
           {
             arg_type;
             entrypoints =
               {Script_typed_ir.root = entrypoint; original_type_expr = _};
           },
         ctxt ) =
    Script_ir_translator.parse_parameter_ty_and_entrypoints
      ctxt
      ~legacy:false
      (Micheline.root parameters_ty)
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4023
     We currently don't support entrypoints as the entrypoint information
     for L1 to L2 messages is not propagated to the rollup. *)
  validate_parameters_ty ctxt arg_type entrypoint

let check_origination_proof (type state proof output)
    ~(pvm : (state, proof, output) Sc_rollup.PVM.implementation) boot_sector
    origination_proof =
  let open Lwt_result_syntax in
  let (module PVM) = pvm in
  let*! is_valid = PVM.verify_origination_proof origination_proof boot_sector in
  let* () =
    fail_when
      (not is_valid)
      (Sc_rollup_proof_repr.Sc_rollup_proof_check "invalid origination proof")
  in
  return PVM.(proof_stop_state origination_proof)

let originate ctxt ~kind ~boot_sector ~origination_proof ~parameters_ty =
  let open Lwt_result_syntax in
  let (Packed ((module PVM) as pvm)) = Sc_rollup.Kind.pvm_of kind in
  let*? ctxt =
    let open Result_syntax in
    let* parameters_ty, ctxt =
      Script.force_decode_in_context
        ~consume_deserialization_gas:When_needed
        ctxt
        parameters_ty
    in
    validate_untyped_parameters_ty ctxt parameters_ty
  in
  let*? origination_proof =
    Sc_rollup.Proof.unserialize_pvm_step ~pvm origination_proof
  in
  let* genesis_hash =
    check_origination_proof ~pvm boot_sector origination_proof
  in
  let genesis_commitment =
    Sc_rollup.Commitment.genesis_commitment
      ~genesis_state_hash:genesis_hash
      ~origination_level:(Level.current ctxt).level
  in
  let+ address, size, genesis_commitment_hash, ctxt =
    Sc_rollup.originate ctxt ~kind ~parameters_ty ~genesis_commitment
  in
  ({address; size; genesis_commitment_hash}, ctxt)

let to_transaction_operation ctxt rollup
    (Sc_rollup_management_protocol.Transaction
      {destination; entrypoint; parameters_ty; parameters; unparsed_parameters})
    =
  let open Result_syntax in
  let* ctxt, nonce = fresh_internal_nonce ctxt in
  (* Validate the type of the parameters. Only types that can be transferred
     from Layer 1 to Layer 2 are permitted.

     In principle, we could allow different types to be passed to the rollup and
     from the rollup. In order to avoid confusion, and given that we don't
     have any use case where they differ, we keep these sets identical.

     We don't check whether the type contains any entrypoints at this stage.
     It has already been done during origination.
  *)
  let* ctxt =
    validate_parameters_ty ctxt parameters_ty Script_typed_ir.no_entrypoints
  in
  let operation =
    Script_typed_ir.Transaction_to_smart_contract
      {
        destination;
        amount = Tez.zero;
        entrypoint;
        location = Micheline.dummy_location;
        parameters_ty;
        parameters;
        unparsed_parameters;
      }
  in
  return
    ( Script_typed_ir.Internal_operation
        {source = Destination.Sc_rollup rollup; operation; nonce},
      ctxt )

let transfer_ticket_tokens ctxt ~source_destination ~acc_storage_diff
    {Ticket_operations_diff.ticket_token; total_amount = _; destinations} =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (acc_storage_diff, ctxt)
         (target_destination, (amount : Script_typed_ir.ticket_amount)) ->
      let* ctxt, storage_diff =
        Ticket_transfer.transfer_ticket
          ctxt
          ~src:source_destination
          ~dst:target_destination
          ticket_token
          Ticket_amount.((amount :> t))
      in
      return (Z.(add acc_storage_diff storage_diff), ctxt))
    (acc_storage_diff, ctxt)
    destinations

let validate_and_decode_output_proof ctxt ~cemented_commitment rollup
    ~output_proof =
  let open Lwt_result_syntax in
  (* Lookup the PVM of the rollup. *)
  let* ctxt, Packed (module PVM) =
    let+ ctxt, kind = Sc_rollup.kind ctxt rollup in
    (ctxt, Sc_rollup.Kind.pvm_of kind)
  in
  let output_proof_length = String.length output_proof in
  let*? ctxt =
    Gas.consume
      ctxt
      (Sc_rollup_costs.cost_deserialize_output_proof
         ~bytes_len:output_proof_length)
  in
  let*? output_proof =
    match
      Data_encoding.Binary.of_string_opt PVM.output_proof_encoding output_proof
    with
    | Some x -> ok x
    | None -> error Sc_rollup_invalid_output_proof
  in
  let output = PVM.output_of_output_proof output_proof in
  (* Verify that the states match. *)
  let* {Sc_rollup.Commitment.compressed_state; _}, ctxt =
    Sc_rollup.Commitment.get_commitment ctxt rollup cemented_commitment
  in
  let* () =
    let output_proof_state = PVM.state_of_output_proof output_proof in
    fail_unless
      Sc_rollup.State_hash.(output_proof_state = compressed_state)
      Sc_rollup_invalid_output_proof
  in
  (* Consume cost of output proof verification. *)
  let*? ctxt =
    Gas.consume
      ctxt
      (Sc_rollup_costs.cost_verify_output_proof ~bytes_len:output_proof_length)
  in
  (* Verify that the proof is valid. *)
  let* () =
    let*! proof_is_valid = PVM.verify_output_proof output_proof in
    fail_unless proof_is_valid Sc_rollup_invalid_output_proof
  in
  return (output, ctxt)

let validate_outbox_level ctxt ~outbox_level ~lcc_level =
  (* Check that outbox level is within the bounds of:
       [min_level < outbox_level <= lcc_level]
     Where
       [min_level = lcc_level - max_active_levels]

      This prevents the rollup from putting messages at a level that is greater
      than its corresponding inbox-level. It also prevents execution
      of messages that are older than the maximum number of active levels.
  *)
  let max_active_levels =
    Int32.to_int (Constants.sc_rollup_max_active_outbox_levels ctxt)
  in
  let outbox_level_is_active =
    let min_allowed_level =
      Int32.sub (Raw_level.to_int32 lcc_level) (Int32.of_int max_active_levels)
    in
    Compare.Int32.(min_allowed_level < Raw_level.to_int32 outbox_level)
  in
  fail_unless
    (Raw_level.(outbox_level <= lcc_level) && outbox_level_is_active)
    Sc_rollup_invalid_outbox_level

let execute_outbox_message ctxt ~validate_and_decode_output_proof rollup
    ~cemented_commitment ~output_proof =
  let open Lwt_result_syntax in
  (* Get inbox level of last cemented commitment, needed to validate that the
     outbox message is active. This call also implicitly checks that the rollup
     exists. *)
  let* lcc_hash, lcc_level, ctxt =
    Sc_rollup.Commitment.last_cemented_commitment_hash_with_level ctxt rollup
  in
  (* Check that the commitment is a cemented commitment still stored in the
     context. We start from the [lcc_hash] of the rollup, which we know to be
     stored in context. *)
  let* is_cemented_commitment_in_context, ctxt =
    Sc_rollup.Commitment.check_if_commitments_are_related
      ctxt
      rollup
      ~descendant:lcc_hash
      ~ancestor:cemented_commitment
  in
  let* () =
    fail_unless
      is_cemented_commitment_in_context
      Sc_rollup_invalid_last_cemented_commitment
  in
  (* Validate and decode the output proofs. *)
  let* Sc_rollup.{outbox_level; message_index; message}, ctxt =
    validate_and_decode_output_proof
      ctxt
      ~cemented_commitment
      rollup
      ~output_proof
  in
  (* Validate that the outbox level is within valid bounds. *)
  let* () = validate_outbox_level ctxt ~outbox_level ~lcc_level in
  let* ( Sc_rollup_management_protocol.Atomic_transaction_batch {transactions},
         ctxt ) =
    Sc_rollup_management_protocol.outbox_message_of_outbox_message_repr
      ctxt
      message
  in
  (* Turn the transaction batch into a list of operations. *)
  let*? ctxt, operations =
    List.fold_left_map_e
      (fun ctxt transaction ->
        let open Result_syntax in
        let+ op, ctxt = to_transaction_operation ctxt rollup transaction in
        (ctxt, op))
      ctxt
      transactions
  in
  (* Record that the message for the given level has been applied. This fails
     in case a message for the rollup, outbox-level and message index has
     already been executed. The storage diff returned may be negative.
  *)
  let* applied_msg_size_diff, ctxt =
    Sc_rollup.Outbox.record_applied_message
      ctxt
      rollup
      outbox_level
      ~message_index:(Z.to_int message_index)
  in
  (* TODO: #3121
     Implement a more refined model. For instance a water-mark based one.
     For now we only charge for positive contributions. It means that over time
     we are overcharging for storage space.
  *)
  let paid_storage_size_diff = Z.max Z.zero applied_msg_size_diff in
  (* Extract the ticket-token diffs from the operations. We here make sure that
     there are no tickets with amount zero. Zero-amount tickets are not allowed
     as they cannot be tracked by the ticket-balance table.
  *)
  let* ticket_token_diffs, ctxt =
    Ticket_operations_diff.ticket_diffs_of_operations ctxt operations
  in
  (* Update the ticket-balance table by transferring ticket-tokens to new
     destinations for each transaction. This fails in case the rollup does not
     hold a sufficient amount of any of the ticket-tokens transferred.

     The updates must happen before any of the operations are executed to avoid
     a case where ticket-transfers are funded as a result of prior operations
     depositing new tickets to the rollup.
  *)
  let* paid_storage_size_diff, ctxt =
    let source_destination = Destination.Sc_rollup rollup in
    List.fold_left_es
      (fun (acc_storage_diff, ctxt) ticket_token_diff ->
        transfer_ticket_tokens
          ctxt
          ~source_destination
          ~acc_storage_diff
          ticket_token_diff)
      (paid_storage_size_diff, ctxt)
      ticket_token_diffs
  in
  let* ctxt, ticket_receipt =
    List.fold_left_map_es
      (fun ctxt
           Ticket_operations_diff.
             {ticket_token = ex_token; total_amount; destinations = _} ->
        let+ ticket_token, ctxt = Ticket_token_unparser.unparse ctxt ex_token in
        (* Here we only show the outgoing (negative) balance wrt to the rollup
           address. The positive balances for the receiving contracts are
           contained in the ticket updates for the internal operations. *)
        let item =
          Ticket_receipt.
            {
              ticket_token;
              updates =
                [
                  {
                    account = Destination.Sc_rollup rollup;
                    amount = Z.neg (Script_int.to_zint total_amount);
                  };
                ];
            }
        in
        (ctxt, item))
      ctxt
      ticket_token_diffs
  in
  return ({paid_storage_size_diff; ticket_receipt; operations}, ctxt)

module Internal_for_tests = struct
  let execute_outbox_message = execute_outbox_message
end

let execute_outbox_message ctxt =
  execute_outbox_message ctxt ~validate_and_decode_output_proof
