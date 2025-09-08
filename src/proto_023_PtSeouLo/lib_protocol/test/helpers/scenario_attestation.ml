(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open State
open Scenario_dsl
open Log_helpers
open Scenario_base
open Protocol

let update_activity name block state : State.t =
  State.update_account_f
    name
    (Account_helpers.update_activity
       state.constants
       (Block.cycle_of_next_block block))
    state

type kind = Preattestation | Attestation

let string_of_kind = function
  | Preattestation -> "preattestation"
  | Attestation -> "attestation"

(** --- Attestations --- *)

let check_attestation_metadata ?(check_not_found = false) ~kind delegate_pkh
    consensus_key_pkh : Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (_block_header_metadata, op_metadata) (_block, _state) ->
  let open Lwt_result_syntax in
  Log.debug
    ~color:low_debug_color
    "Check metadata: %s for %a (consensus key : %a)"
    (string_of_kind kind)
    Signature.Public_key_hash.pp
    delegate_pkh
    Signature.Public_key_hash.pp
    consensus_key_pkh ;
  let id_or_not, error_prefix =
    if check_not_found then (not, "Not expected but found in metadata")
    else (Fun.id, "Expected but not found in metadata")
  in
  if
    id_or_not
    @@ List.exists
         (fun metadata ->
           match (kind, metadata) with
           | ( Attestation,
               Protocol.Apply_results.Operation_metadata
                 {
                   contents =
                     Single_result
                       (Attestation_result
                          {
                            (* This list is always empty *)
                            balance_updates = [];
                            delegate;
                            consensus_key;
                            consensus_power = _;
                          });
                 } )
           | ( Preattestation,
               Protocol.Apply_results.Operation_metadata
                 {
                   contents =
                     Single_result
                       (Preattestation_result
                          {
                            (* This list is always empty *)
                            balance_updates = [];
                            delegate;
                            consensus_key;
                            consensus_power = _;
                          });
                 } ) ->
               Signature.Public_key_hash.(
                 equal delegate delegate_pkh
                 && equal consensus_key consensus_key_pkh)
           | _ -> false)
         op_metadata
  then return_unit
  else
    failwith
      "%s: %s for %a (consensus key : %a)@.Full metadata:@.%s"
      error_prefix
      (string_of_kind kind)
      Signature.Public_key_hash.pp
      delegate_pkh
      Signature.Public_key_hash.pp
      consensus_key_pkh
      (List.fold_left
         (fun acc op_metadata ->
           let s =
             Data_encoding.Json.(
               construct operation_receipt_encoding op_metadata |> to_string)
           in
           acc ^ "\n" ^ s)
         ""
         op_metadata)

let check_attestation_aggregate_metadata ?(check_not_found = false) ~kind
    ?(expect_same_order = true) committee_expect :
    Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (_block_header_metadata, op_metadata) (_block, _state) ->
  let open Lwt_result_syntax in
  Log.debug ~color:low_debug_color "Check metadata: aggregated attestation" ;
  let id_or_not, error_prefix =
    if check_not_found then (not, "Not expected but found in metadata")
    else (Fun.id, "Expected but not found in metadata")
  in
  let may_sort committee =
    if expect_same_order then committee
    else
      List.sort
        (fun {Alpha_context.Consensus_key.delegate = d1; _}
             {delegate = d2; _}
           -> Signature.Public_key_hash.compare d1 d2)
        committee
  in
  let committee_expect = may_sort committee_expect in
  if
    id_or_not
    @@ List.exists
         (fun metadata ->
           match (kind, metadata) with
           | ( Attestation,
               Protocol.Apply_results.Operation_metadata
                 {
                   contents =
                     Single_result
                       (Attestations_aggregate_result
                          {
                            (* This list is always empty *)
                            balance_updates = [];
                            committee;
                            total_consensus_power = _;
                          });
                 } )
           | ( Preattestation,
               Protocol.Apply_results.Operation_metadata
                 {
                   contents =
                     Single_result
                       (Preattestations_aggregate_result
                          {
                            (* This list is always empty *)
                            balance_updates = [];
                            committee;
                            total_consensus_power = _;
                          });
                 } ) ->
               let committee = List.map fst committee |> may_sort in
               Log.debug
                 "@[<v 2>Actual committee:@,%a@]"
                 (Format.pp_print_list Alpha_context.Consensus_key.pp)
                 committee ;
               List.equal
                 (fun {
                        Alpha_context.Consensus_key.delegate = d1;
                        consensus_pkh = c1;
                      }
                      {delegate = d2; consensus_pkh = c2}
                    ->
                   Signature.Public_key_hash.equal d1 d2
                   && Signature.Public_key_hash.equal c1 c2)
                 committee
                 committee_expect
           | _ -> false)
         op_metadata
  then return_unit
  else
    failwith
      "%s: %s aggregate for committee@.[%a]"
      error_prefix
      (string_of_kind kind)
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "; ")
          Alpha_context.Consensus_key.pp)
      committee_expect

let check_attestation_rewards ?(check_not_found = false) delegate_name :
    Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (_block_header_metadata, _op_metadata) (block, state) ->
  let open Lwt_result_syntax in
  Log.debug ~color:low_debug_color "Check metadata: attestation rewards" ;
  let* () =
    if Block.last_block_of_cycle block then return_unit
    else
      failwith
        "check_attestation_rewards must be called at the end of a cycle \
         (current block position in cycle: %d, expecting %d)"
        (Int32.to_int (Block.cycle_position block))
        (Int32.to_int state.constants.blocks_per_cycle - 1)
  in
  (* In the block metadata, there is no exact correspondence between attestation rewards
     and the receivers. For now, we do a simple check.
     TODO: make a check function that covers all end-of-cycle rewards. This should
     also include staking rewards, and rewards of amount zero should not appear. *)
  let id_or_not, error_suffix =
    if check_not_found then
      ( not,
        "balance increased, suggesting rewards were distributed, which is not \
         expected." )
    else
      ( Fun.id,
        "balance did not increase, suggesting rewards were not distributed, \
         which is not expected." )
  in
  let delegate = State.find_account delegate_name state in
  let* previous_balance =
    Context.Contract.full_balance (B state.grandparent) delegate.contract
  in
  let* current_balance =
    Context.Contract.full_balance (B block) delegate.contract
  in
  if id_or_not @@ Tez.(current_balance > previous_balance) then return_unit
  else failwith "Check attestation rewards: %s's %s" delegate_name error_suffix

let check_missed_attestation_rewards delegate_name ?(check_not_found = false) :
    Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (block_header_metadata, _op_metadata) (block, state) ->
  let open Lwt_result_syntax in
  Log.debug ~color:low_debug_color "Check metadata: missed attestation rewards" ;
  let* () =
    if Block.last_block_of_cycle block then return_unit
    else
      failwith
        "check_missed_attestation_rewards must be called at the end of a cycle \
         (current block position in cycle: %d, expecting %d)"
        (Int32.to_int (Block.cycle_position block))
        (Int32.to_int state.constants.blocks_per_cycle - 1)
  in
  let id_or_not, error_prefix =
    if check_not_found then (not, "Not expected but found in metadata")
    else (Fun.id, "Expected but not found in metadata")
  in
  let delegate = State.find_account delegate_name state in
  if
    id_or_not
    @@ List.exists
         (function
           | Alpha_context.Receipt.Balance_update_item
               ( Lost_attesting_rewards (pkh, _participation, _revelation),
                 Credited _,
                 Block_application ) ->
               Signature.Public_key_hash.equal delegate.pkh pkh
           | _ -> false)
         block_header_metadata.balance_updates
  then return_unit
  else
    failwith "%s: missed attestation receipt for %s" error_prefix delegate_name

let attest_with ?dal_content (delegate_name : string) : (t, t) scenarios =
  exec (fun (block, state) ->
      let open Lwt_result_syntax in
      let kind = Attestation in
      Log.info ~color:action_color "[Attesting with \"%s\"]" delegate_name ;
      if state.force_attest_all then
        failwith "Cannot manually attest if force_attest_all is true"
      else
        let delegate = State.find_account delegate_name state in
        let* consensus_key_info =
          Context.Delegate.consensus_key (B state.grandparent) delegate.pkh
        in
        let consensus_key = consensus_key_info.active in
        let* consensus_key = Account.find consensus_key.consensus_key_pkh in
        let* dal_content =
          Option.map_es Dal_helpers.dal_content_of_z dal_content
        in
        (* Fails to produce an attestation if the delegate has no slot for the block *)
        let* op = Op.attestation ?dal_content ~manager_pkh:delegate.pkh block in
        (* Update the activity of the delegate *)
        let state = update_activity delegate_name block state in
        let state = State.add_pending_operations [op] state in
        (* Check metadata *)
        let state =
          State.add_current_block_check
            (check_attestation_metadata ~kind delegate.pkh consensus_key.pkh)
            state
        in
        return (block, state))

(** (tz4 only) Creates an aggregated attestation from the attestations of the given delegates.
    Fails if one of the delegates has no slot for the given block, or if one of the
    delegates' consensus key is not a tz4 *)
let attest_aggreg_with ?(delegates_with_dal = ([] : (string * Z.t) list))
    (delegates : string list) : (t, t) scenarios =
  exec (fun (block, state) ->
      let open Lwt_result_wrap_syntax in
      let kind = Attestation in
      let* delegates =
        List.map (fun (x, dal) -> (x, Some dal)) delegates_with_dal
        @ List.map (fun x -> (x, None)) delegates
        |> List.map_es (fun (x, dal) ->
               let* dal = Option.map_es Dal_helpers.dal_content_of_z dal in
               return (x, dal))
      in
      Log.info
        ~color:action_color
        "[Aggregated attesting with \"%a\"]"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string)
        (List.map fst delegates) ;
      if state.force_attest_all then
        failwith "Cannot manually attest if force_attest_all is true"
      else
        let* state, committee_with_dal, expected_metadata_committee =
          List.fold_left_es
            (fun (state, committee, expected_metadata_committee)
                 (delegate_name, dal_content)
               ->
              let delegate = State.find_account delegate_name state in
              let* consensus_key_info =
                Context.Delegate.consensus_key
                  (B state.grandparent)
                  delegate.pkh
              in
              let consensus_pkh = consensus_key_info.active.consensus_key_pkh in
              let* () =
                if Signature.Public_key_hash.is_bls consensus_pkh then
                  return_unit
                else failwith "Cannot aggregate with non-BLS key"
              in
              (* Update the activity of the committee *)
              let state = update_activity delegate_name block state in
              let* attesting_slot =
                Op.get_attesting_slot_of_delegate
                  ~manager_pkh:delegate.pkh
                  ~attested_block:block
              in
              let key_in_metadata =
                {
                  Alpha_context.Consensus_key.delegate = delegate.pkh;
                  consensus_pkh;
                }
              in
              return
                ( state,
                  (attesting_slot, dal_content) :: committee,
                  key_in_metadata :: expected_metadata_committee ))
            (state, [], [])
            delegates
        in
        (* Fails to produce an attestation if one of the delegates has no slot for the block *)
        let* op = Op.attestations_aggregate ~committee_with_dal block in
        (* Check metadata *)
        let state =
          State.add_current_block_check
            (check_attestation_aggregate_metadata
               ~kind
               expected_metadata_committee)
            state
        in
        let state = State.add_pending_operations [op] state in
        return (block, state))

let key_for_metadata_of_delegate_rights
    {RPC.Attestation_rights.delegate; consensus_key; _} =
  {Alpha_context.Consensus_key.delegate; consensus_pkh = consensus_key}

let attest_with_all_ : t -> t tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun (block, state) ->
    Log.info ~color:action_color "[Attesting with all eligible delegates]" ;
    let kind = Attestation in
    let* rights = Plugin.RPC.Attestation_rights.get Block.rpc_ctxt block in
    let delegates_rights =
      match rights with
      | [{level = _; delegates_rights; estimated_time = _}] -> delegates_rights
      | _ ->
          (* Cannot happen: RPC called to return only current level,
             so the returned list should only contain one element. *)
          assert false
    in
    let* non_forbidden_delegates_rights =
      List.filter_es
        (fun {
               RPC.Attestation_rights.delegate;
               consensus_key = _;
               first_slot = _;
               attestation_power;
             }
           ->
          Tezt.Check.(
            (attestation_power > 0)
              int
              ~__LOC__
              ~error_msg:"Attestation power should be greater than 0, got %L") ;
          let* is_forbidden =
            Context.Delegate.is_forbidden (B block) delegate
          in
          return (not is_forbidden))
        delegates_rights
    in
    let* state, bls_committee =
      List.fold_left_es
        (fun (state, bls_committee)
             ({
                RPC.Attestation_rights.delegate = manager_pkh;
                consensus_key = consensus_pkh;
                first_slot = slot;
                attestation_power = _;
              } as delegate_rights)
           ->
          (* Update delegate activity in any case. *)
          let delegate_name, _ =
            State.find_account_from_pkh manager_pkh state
          in
          let state = update_activity delegate_name block state in
          if
            state.constants.aggregate_attestation
            && Signature.Public_key_hash.is_bls consensus_pkh
          then
            (* Just add the delegate to the committee; aggregation and
               metadata check will be handled below. *)
            return (state, delegate_rights :: bls_committee)
          else
            (* Add standalone attestation and metadata check. *)
            let attesting_slot = {Op.slot; consensus_pkh} in
            let* op = Op.attestation ~attesting_slot block in
            let state = State.add_pending_operations [op] state in
            let state =
              State.add_current_block_check
                (check_attestation_metadata ~kind manager_pkh consensus_pkh)
                state
            in
            return (state, bls_committee))
        (state, [])
        non_forbidden_delegates_rights
    in
    let* state =
      if List.is_empty bls_committee then return state
      else
        (* Add aggregated attestation and metadata check. *)
        let committee =
          List.map Op.attesting_slot_of_delegate_rights bls_committee
        in
        let* op = Op.attestations_aggregate ~committee block in
        let state = State.add_pending_operations [op] state in
        let expected_committee =
          List.map key_for_metadata_of_delegate_rights bls_committee
        in
        let state =
          State.add_current_block_check
            (check_attestation_aggregate_metadata ~kind expected_committee)
            state
        in
        return state
    in
    return (block, state)

let attest_with_all = exec attest_with_all_

(** --- Preattestations --- *)

let make_fake_block ?payload_round incr =
  let open Lwt_result_wrap_syntax in
  let* int_round, round =
    match payload_round with
    | Some payload_round ->
        let int_round = payload_round in
        let*?@ round = Alpha_context.Round.of_int int_round in
        return (int_round, round)
    | None ->
        let round =
          (Incremental.header incr).protocol_data.contents.payload_round
        in
        let*?@ int_round = Alpha_context.Round.to_int round in
        return (int_round, round)
  in
  let operations =
    Block.Forge.classify_operations (List.rev @@ Incremental.rev_operations incr)
  in
  let non_consensus_operations =
    List.concat (match List.tl operations with None -> [] | Some l -> l)
  in
  Block.bake
    ~policy:(By_round int_round)
    ~payload_round:round
    ~operations:non_consensus_operations
    (Incremental.predecessor incr)

let preattest_with ?payload_round (delegate_name : string) :
    (t_incr, t_incr) scenarios =
  exec (fun (incr, state) ->
      let open Lwt_result_wrap_syntax in
      Log.info ~color:action_color "[Preattesting with \"%s\"]" delegate_name ;
      if state.force_preattest_all then
        failwith "Cannot manually preattest if force_preattest_all is true"
      else
        let kind = Preattestation in
        let* fake_block = make_fake_block ?payload_round incr in
        let delegate = State.find_account delegate_name state in
        let* consensus_key_info =
          Context.Delegate.consensus_key (I incr) delegate.pkh
        in
        let consensus_key = consensus_key_info.active in
        let* consensus_key = Account.find consensus_key.consensus_key_pkh in
        (* Fails to produce an attestation if the delegate has no slot for the block *)
        let* op = Op.preattestation ~manager_pkh:delegate.pkh fake_block in
        (* Update the activity of the delegate *)
        let state =
          update_activity delegate_name (Incremental.predecessor incr) state
        in
        (* Check metadata *)
        let state =
          State.add_current_block_check
            (check_attestation_metadata ~kind delegate.pkh consensus_key.pkh)
            state
        in
        let* incr = Incremental.add_operation incr op in
        return (incr, state))

(** (tz4 only) Creates an aggregated preattestation from the preattestations of the given delegates.
    Fails if one of the delegates has no slot for the given block, or if one of the
    delegates' consensus key is not a tz4 *)
let preattest_aggreg_with ?payload_round (delegates : string list) :
    (t_incr, t_incr) scenarios =
  exec (fun (incr, state) ->
      let open Lwt_result_wrap_syntax in
      Log.info
        ~color:action_color
        "[Aggregated preattesting with \"%a\"]"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string)
        delegates ;
      if state.force_preattest_all then
        failwith "Cannot manually preattest if force_preattest_all is true"
      else
        let kind = Preattestation in
        let* fake_block = make_fake_block ?payload_round incr in
        let* state, committee, expected_metadata_committee =
          List.fold_left_es
            (fun (state, committee, expected_metadata_committee)
                 delegate_name
               ->
              let delegate = State.find_account delegate_name state in
              let* consensus_key_info =
                Context.Delegate.consensus_key (I incr) delegate.pkh
              in
              let consensus_pkh = consensus_key_info.active.consensus_key_pkh in
              let* () =
                if Signature.Public_key_hash.is_bls consensus_pkh then
                  return_unit
                else failwith "Cannot aggregate non-BLS preattestation"
              in
              (* Update the activity of the committee *)
              let state =
                update_activity
                  delegate_name
                  (Incremental.predecessor incr)
                  state
              in
              (* Fails if the delegate has no slot for the block *)
              let* attesting_slot =
                Op.get_attesting_slot_of_delegate
                  ~manager_pkh:delegate.pkh
                  ~attested_block:fake_block
              in
              let key_for_metadata =
                {
                  Alpha_context.Consensus_key.delegate = delegate.pkh;
                  consensus_pkh;
                }
              in
              return
                ( state,
                  attesting_slot :: committee,
                  key_for_metadata :: expected_metadata_committee ))
            (state, [], [])
            delegates
        in
        let* op = Op.preattestations_aggregate ~committee fake_block in
        (* Check metadata *)
        let state =
          State.add_current_block_check
            (check_attestation_aggregate_metadata
               ~kind
               expected_metadata_committee)
            state
        in
        let* incr = Incremental.add_operation incr op in
        return (incr, state))

let preattest_with_all_ ?payload_round : t_incr -> t_incr tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun (incr, state) ->
    Log.info ~color:action_color "[Preattesting with all eligible delegates]" ;
    let kind = Preattestation in
    let* fake_block = make_fake_block ?payload_round incr in
    let* rights = Plugin.RPC.Attestation_rights.get Block.rpc_ctxt fake_block in
    let delegates_rights =
      match rights with
      | [{level = _; delegates_rights; estimated_time = _}] -> delegates_rights
      | _ ->
          (* Cannot happen: RPC called to return only current level,
             so the returned list should only contain one element. *)
          assert false
    in
    let* non_forbidden_delegates_rights =
      List.filter_es
        (fun {
               Plugin.RPC.Attestation_rights.delegate;
               consensus_key = _;
               first_slot = _;
               attestation_power;
             }
           ->
          Tezt.Check.(
            (attestation_power > 0)
              int
              ~__LOC__
              ~error_msg:"Attestation power should be greater than 0, got %L") ;
          let* is_forbidden = Context.Delegate.is_forbidden (I incr) delegate in
          return (not is_forbidden))
        delegates_rights
    in
    let* incr, state, bls_committee =
      List.fold_left_es
        (fun (incr, state, bls_committee)
             ({
                Plugin.RPC.Attestation_rights.delegate = manager_pkh;
                consensus_key = consensus_pkh;
                first_slot = slot;
                attestation_power = _;
              } as delegate_rights)
           ->
          (* Update delegate activity in any case. *)
          let delegate_name, _ =
            State.find_account_from_pkh manager_pkh state
          in
          let state =
            update_activity delegate_name (Incremental.predecessor incr) state
          in
          if
            state.constants.aggregate_attestation
            && Signature.Public_key_hash.is_bls consensus_pkh
          then
            (* Just add the delegate to the committee; aggregation and
               metadata check will be handled below. *)
            return (incr, state, delegate_rights :: bls_committee)
          else
            (* Add standalone preattestation and metadata check. *)
            let attesting_slot = {Op.slot; consensus_pkh} in
            let* op = Op.preattestation ~attesting_slot fake_block in
            let* incr = Incremental.add_operation incr op in
            let state =
              State.add_current_block_check
                (check_attestation_metadata ~kind manager_pkh consensus_pkh)
                state
            in
            return (incr, state, bls_committee))
        (incr, state, [])
        non_forbidden_delegates_rights
    in
    if List.is_empty bls_committee then return (incr, state)
    else
      (* Add aggregated preattestation and metadata check. *)
      let committee =
        List.map Op.attesting_slot_of_delegate_rights bls_committee
      in
      let* op = Op.preattestations_aggregate ~committee fake_block in
      let* incr = Incremental.add_operation incr op in
      let expected_committee =
        List.map key_for_metadata_of_delegate_rights bls_committee
      in
      let state =
        State.add_current_block_check
          (check_attestation_aggregate_metadata ~kind expected_committee)
          state
      in
      return (incr, state)
