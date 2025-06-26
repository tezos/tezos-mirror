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

let update_activity name block state : State.t =
  State.update_account_f
    name
    (Account_helpers.update_activity
       state.constants
       (Block.cycle_of_next_block block))
    state

(** --- Attestations --- *)

let check_attestation_metadata delegate_pkh consensus_key_pkh :
    Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (_block_header_metadata, op_metadata) (_block, _state) ->
  let open Lwt_result_syntax in
  Log.debug
    ~color:low_debug_color
    "Check metadata: attestation for %a (consensus key : %a)"
    Signature.Public_key_hash.pp
    delegate_pkh
    Signature.Public_key_hash.pp
    consensus_key_pkh ;
  if
    List.exists
      (function
        | Protocol.Apply_results.Operation_metadata
            {
              contents =
                Single_result
                  (Attestation_result
                    {
                      balance_updates = _;
                      delegate;
                      consensus_key;
                      consensus_power = _;
                    });
            } ->
            Signature.Public_key_hash.(
              equal delegate delegate_pkh
              && equal consensus_key consensus_key_pkh)
        | _ -> false)
      op_metadata
  then return_unit
  else
    failwith
      "Attestation for %a (consensus key : %a) expected but not found in \
       metadata"
      Signature.Public_key_hash.pp
      delegate_pkh
      Signature.Public_key_hash.pp
      consensus_key_pkh

let check_attestation_aggregate_metadata committee_expect :
    Block.full_metadata -> t -> unit tzresult Lwt.t =
 fun (_block_header_metadata, op_metadata) (_block, _state) ->
  let open Lwt_result_syntax in
  Log.debug ~color:low_debug_color "Check metadata: aggregated attestation" ;
  if
    List.exists
      (function
        | Protocol.Apply_results.Operation_metadata
            {
              contents =
                Single_result
                  (Attestations_aggregate_result
                    {balance_updates = _; committee; total_consensus_power = _});
            } ->
            let committee =
              List.map
                (fun ((ck : Protocol.Alpha_context.Consensus_key.t), _) ->
                  ck.delegate)
                committee
              |> List.sort Signature.Public_key_hash.compare
            in
            let committee_expect =
              List.map fst committee_expect
              |> List.sort Signature.Public_key_hash.compare
            in
            List.equal
              Signature.Public_key_hash.equal
              committee
              committee_expect
        | _ -> false)
      op_metadata
  then return_unit
  else
    failwith
      "Attestation aggregate for committee@.[%a]@.expected but not found \
       (exactly) in metadata"
      Format.(
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "; ")
          Signature.Public_key_hash.pp)
      (List.map fst committee_expect)

let attest_with (delegate_name : string) : (t, t) scenarios =
  exec (fun (block, state) ->
      let open Lwt_result_wrap_syntax in
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
        (* Fails to produce an attestation if the delegate has no slot for the block *)
        let* op = Op.attestation ~delegate:consensus_key.pkh block in
        (* Update the activity of the delegate *)
        let state = update_activity delegate_name block state in
        let state = State.add_pending_operations [op] state in
        (* Check metadata *)
        let state =
          State.add_temp_check
            (check_attestation_metadata delegate.pkh consensus_key.pkh)
            state
        in
        return (block, state))

(** (tz4 only) Creates an aggregated attestation from the attestations of the given delegates.
    Fails if one of the delegates has no slot for the given block, or if one of the
    delegates' consensus key is not a tz4 *)
let attest_aggreg_with (delegates : string list) : (t, t) scenarios =
  exec (fun (block, state) ->
      let open Lwt_result_wrap_syntax in
      Log.info
        ~color:action_color
        "[Aggregated attesting with \"%a\"]"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string)
        delegates ;
      if state.force_attest_all then
        failwith "Cannot manually attest if force_attest_all is true"
      else
        let* state, committee, delegate_and_ck_committee =
          List.fold_left_es
            (fun (state, committee, delegate_and_ck) delegate_name ->
              let delegate = State.find_account delegate_name state in
              let* consensus_key_info =
                Context.Delegate.consensus_key
                  (B state.grandparent)
                  delegate.pkh
              in
              let consensus_key_pkh =
                consensus_key_info.active.consensus_key_pkh
              in
              (* Update the activity of the committee *)
              let state = update_activity delegate_name block state in
              return
                ( state,
                  consensus_key_pkh :: committee,
                  (delegate.pkh, consensus_key_pkh) :: delegate_and_ck ))
            (state, [], [])
            delegates
        in
        let* () =
          if
            not
            @@ List.for_all
                 (function
                   | (Bls _ : Signature.public_key_hash) -> true | _ -> false)
                 committee
          then failwith "Cannot aggregate non-BLS attestation"
          else return_unit
        in
        (* Fails to produce an attestation if one of the delegates has no slot for the block *)
        let* op = Op.attestations_aggregate ~committee block in
        (* Check metadata *)
        let state =
          State.add_temp_check
            (check_attestation_aggregate_metadata delegate_and_ck_committee)
            state
        in
        let state = State.add_pending_operations [op] state in
        return (block, state))

let attest_with_all_ : t -> t tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun (block, state) ->
    Log.info ~color:action_color "[Attesting with all eligible delegates]" ;
    let* rights = Plugin.RPC.Attestation_rights.get Block.rpc_ctxt block in
    let delegates_rights =
      match rights with
      | [{level = _; delegates_rights; estimated_time = _}] -> delegates_rights
      | _ ->
          (* Cannot happen: RPC called to return only current level,
             so the returned list should only contain one element. *)
          assert false
    in
    let* dlgs =
      List.map
        (fun {
               Plugin.RPC.Attestation_rights.delegate;
               consensus_key = _;
               first_slot;
               attestation_power;
             } ->
          Tezt.Check.(
            (attestation_power > 0)
              int
              ~__LOC__
              ~error_msg:"Attestation power should be greater than 0, got %L") ;
          (delegate, first_slot))
        delegates_rights
      |> List.filter_es (fun (delegate, _slot) ->
             let* is_forbidden =
               Context.Delegate.is_forbidden (B block) delegate
             in
             return (not is_forbidden))
    in
    let* to_aggregate, ops =
      List.fold_left_es
        (fun (to_aggregate, regular) (delegate, slot) ->
          let* consensus_key_info =
            Context.Delegate.consensus_key (B state.grandparent) delegate
          in
          let consensus_key = consensus_key_info.active in
          let* consensus_key = Account.find consensus_key.consensus_key_pkh in
          let* op =
            Op.raw_attestation ~delegate:consensus_key.pkh ~slot block
          in
          match (state.constants.aggregate_attestation, consensus_key.pk) with
          | true, Bls _ ->
              return ((op, delegate, consensus_key.pkh) :: to_aggregate, regular)
          | _ ->
              return
                ( to_aggregate,
                  ( Protocol.Alpha_context.Operation.pack op,
                    delegate,
                    consensus_key.pkh )
                  :: regular ))
        ([], [])
        dlgs
    in
    let aggregated =
      Op.aggregate (List.map (fun (x, _, _) -> x) to_aggregate)
    in
    let state =
      match aggregated with
      | None -> state
      | Some op ->
          (* Update the activity of the committee *)
          let state, delegate_and_ck_committee =
            List.fold_left
              (fun (state, delegate_and_ck) (_, delegate_pkh, consensus_key_pkh) ->
                let delegate_name, _ =
                  State.find_account_from_pkh delegate_pkh state
                in
                ( update_activity delegate_name block state,
                  (delegate_pkh, consensus_key_pkh) :: delegate_and_ck ))
              (state, [])
              to_aggregate
          in
          (* Check metadata *)
          let state =
            State.add_temp_check
              (check_attestation_aggregate_metadata delegate_and_ck_committee)
              state
          in
          let state = State.add_pending_operations [op] state in
          state
    in
    (* Update the activity of the rest of the committee, and check metadata *)
    let state =
      List.fold_left
        (fun state (_, delegate_pkh, consensus_key_pkh) ->
          let delegate_name, _ =
            State.find_account_from_pkh delegate_pkh state
          in
          let state = update_activity delegate_name block state in
          (* Check metadata *)
          let state =
            State.add_temp_check
              (check_attestation_metadata delegate_pkh consensus_key_pkh)
              state
          in
          state)
        state
        ops
    in
    let state =
      State.add_pending_operations (List.map (fun (x, _, _) -> x) ops) state
    in
    return (block, state)

let attest_with_all = exec attest_with_all_

(** --- Preattestations --- *)

let preattest_with_all_ : t_incr -> t_incr tzresult Lwt.t =
 fun (incr, state) ->
  let open Lwt_result_syntax in
  (* TODO *) return (incr, state)
