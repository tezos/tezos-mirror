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

open Protocol
open Alpha_context
open Baking_state

let create_state cctxt ?synchronize ?monitor_node_mempool ~config
    ~current_proposal delegates =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let monitor_node_operations = monitor_node_mempool in
  let*! operation_worker =
    Operation_worker.create ?monitor_node_operations cctxt
  in
  Baking_scheduling.create_initial_state
    cctxt
    ?synchronize
    ~chain
    config
    operation_worker
    ~current_proposal
    delegates

let get_current_proposal cctxt ?cache () =
  let open Lwt_result_syntax in
  let* block_stream, _block_stream_stopper =
    Node_rpc.monitor_heads cctxt ?cache ~chain:cctxt#chain ()
  in
  let*! current_head = Lwt_stream.peek block_stream in
  match current_head with
  | Some current_head -> return (block_stream, current_head)
  | None -> failwith "head stream unexpectedly ended"

module Events = Baking_events.Lib

let preattest (cctxt : Protocol_client_context.full) ?(force = false) delegates
    =
  let open State_transitions in
  let open Lwt_result_syntax in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ~force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  let proposal = state.level_state.latest_proposal in
  let*! () =
    Events.(
      emit attempting_preattest_proposal state.level_state.latest_proposal)
  in
  let* () =
    if force then return_unit
    else
      let*! proposal_acceptance =
        is_acceptable_proposal_for_current_level state proposal
      in
      match proposal_acceptance with
      | Invalid -> cctxt#error "Cannot preattest an invalid proposal"
      | Outdated_proposal -> cctxt#error "Cannot preattest an outdated proposal"
      | Valid_proposal -> return_unit
  in
  let consensus_list = make_consensus_list state proposal in
  let*! () =
    cctxt#message
      "@[<v 2>Preattesting for:@ %a@]"
      Format.(
        pp_print_list
          ~pp_sep:pp_print_space
          Baking_state.pp_consensus_key_and_delegate)
      (List.map fst consensus_list)
  in
  Baking_actions.inject_consensus_votes state consensus_list `Preattestation

let attest (cctxt : Protocol_client_context.full) ?(force = false) delegates =
  let open State_transitions in
  let open Lwt_result_syntax in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ~force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  let proposal = state.level_state.latest_proposal in
  let*! () =
    Events.(emit attempting_attest_proposal state.level_state.latest_proposal)
  in
  let* () =
    if force then return_unit
    else
      let*! proposal_acceptance =
        is_acceptable_proposal_for_current_level state proposal
      in
      match proposal_acceptance with
      | Invalid -> cctxt#error "Cannot attest an invalid proposal"
      | Outdated_proposal -> cctxt#error "Cannot attest an outdated proposal"
      | Valid_proposal -> return_unit
  in
  let consensus_list = make_consensus_list state proposal in
  let*! () =
    cctxt#message
      "@[<v 2>Attesting for:@ %a@]"
      Format.(
        pp_print_list
          ~pp_sep:pp_print_space
          Baking_state.pp_consensus_key_and_delegate)
      (List.map fst consensus_list)
  in
  let* () =
    Baking_state.may_record_new_state ~previous_state:state ~new_state:state
  in
  Baking_actions.inject_consensus_votes state consensus_list `Attestation

let bake_at_next_level state =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let*! baking_time =
    Baking_scheduling.compute_next_potential_baking_time_at_next_level state
  in
  match baking_time with
  | None -> cctxt#error "No baking slot found for the delegates"
  | Some (timestamp, round) ->
      let*! () =
        cctxt#message
          "Waiting until %a for round %a"
          Timestamp.pp
          timestamp
          Round.pp
          round
      in
      let*! () =
        Option.value
          ~default:Lwt.return_unit
          (Baking_scheduling.sleep_until timestamp)
      in
      return (Baking_state.Timeout (Time_to_bake_next_level {at_round = round}))

(* Simulate the end of the current round to bootstrap the automaton
   or attest the block if necessary *)
let first_automaton_event state =
  match state.level_state.elected_block with
  | None -> Lwt.return (Baking_scheduling.compute_bootstrap_event state)
  | Some _elected_block ->
      (* If there is an elected block we can directly bake at next
         level after waiting its date *)
      bake_at_next_level state

let attestations_attesting_power state attestations =
  let get_attestation_voting_power {slot; _} =
    match
      Delegate_slots.voting_power state.level_state.delegate_slots ~slot
    with
    | None -> 0 (* cannot happen *)
    | Some attesting_power -> attesting_power
  in
  List.sort_uniq compare attestations
  |> List.fold_left
       (fun power attestation ->
         power + get_attestation_voting_power attestation)
       0

let generic_attesting_power (filter : packed_operation list -> 'a list)
    (extract : 'a -> consensus_content) state =
  let current_mempool =
    Operation_worker.get_current_operations state.global_state.operation_worker
  in
  let latest_proposal = state.level_state.latest_proposal in
  let block_round = latest_proposal.block.round in
  let shell_level = latest_proposal.block.shell.level in
  let attestations =
    filter (Operation_pool.Operation_set.elements current_mempool.consensus)
  in
  let attestations_in_mempool =
    List.filter_map
      (fun v ->
        let consensus_content = extract v in
        if
          Round.(consensus_content.round = block_round)
          && Compare.Int32.(
               Raw_level.to_int32 consensus_content.level = shell_level)
        then Some consensus_content
        else None)
      attestations
  in
  let power = attestations_attesting_power state attestations_in_mempool in
  (power, attestations)

let state_attesting_power =
  generic_attesting_power
    Operation_pool.filter_attestations
    (fun
      ({
         protocol_data =
           {
             contents = Single (Attestation {consensus_content; dal_content = _});
             _;
           };
         _;
       } :
        Kind.attestation operation)
    -> consensus_content)

let do_action (state, action) =
  let state_recorder ~new_state =
    Baking_state.may_record_new_state ~previous_state:state ~new_state
  in
  Baking_actions.perform_action ~state_recorder state action

let propose_at_next_level ~minimal_timestamp state =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  assert (Option.is_some state.level_state.elected_block) ;
  if minimal_timestamp then
    let* minimal_round, delegate =
      match
        Baking_scheduling.first_potential_round_at_next_level
          state
          ~earliest_round:Round.zero
      with
      | None -> cctxt#error "No potential baking slot for the given delegates."
      | Some first_potential_round -> return first_potential_round
    in
    let pool =
      Operation_worker.get_current_operations
        state.global_state.operation_worker
    in
    let kind = Baking_actions.Fresh pool in
    let block_to_bake : Baking_actions.block_to_bake =
      {
        Baking_actions.predecessor = state.level_state.latest_proposal.block;
        round = minimal_round;
        delegate;
        kind;
        force_apply = state.global_state.config.force_apply;
      }
    in
    let state_recorder ~new_state =
      Baking_state.may_record_new_state ~previous_state:state ~new_state
    in
    let* state =
      Baking_actions.perform_action
        ~state_recorder
        state
        (Inject_block
           {kind = Forge_and_inject block_to_bake; updated_state = state})
    in
    let*! () =
      cctxt#message
        "Proposed block at round %a on top of %a "
        Round.pp
        block_to_bake.round
        Block_hash.pp
        block_to_bake.predecessor.hash
    in
    return state
  else
    let* event = bake_at_next_level state in
    let* state =
      let*! action = State_transitions.step state event in
      do_action action
    in
    let*! () = cctxt#message "Proposal injected" in
    return state

let attestation_quorum state =
  let power, attestations = state_attesting_power state in
  if
    Compare.Int.(
      power >= state.global_state.constants.parametric.consensus_threshold)
  then Some (power, attestations)
  else None

(* Here's the sketch of the algorithm:
   Do I have an attestation quorum for the current block or an elected block?
   - Yes :: wait and propose at next level
   - No  ::
     Is the current proposal at the right round?
     - Yes :: fail propose
     - No  ::
       Is there a preattestation quorum or does the last proposal contain a prequorum?
       - Yes :: repropose block with right payload and preattestations for current round
       - No  :: repropose fresh block for current round *)
let propose (cctxt : Protocol_client_context.full) ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?force_apply ?force
    ?(minimal_timestamp = false) ?extra_operations ?context_path ?state_recorder
    delegates =
  let open Lwt_result_syntax in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let config =
    Baking_configuration.make
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?context_path
      ?force_apply
      ?force
      ?extra_operations
      ?state_recorder
      ()
  in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  (* Make sure the operation worker is populated to avoid empty blocks
     being proposed. *)
  let* () =
    Operation_worker.retrieve_pending_operations
      cctxt
      state.global_state.operation_worker
  in
  let* _ =
    match state.level_state.elected_block with
    | Some _ -> propose_at_next_level ~minimal_timestamp state
    | None -> (
        match attestation_quorum state with
        | Some (_voting_power, attestation_qc) ->
            let state =
              {
                state with
                round_state =
                  {
                    state.round_state with
                    current_phase = Baking_state.Awaiting_attestations;
                  };
              }
            in
            let latest_proposal = state.level_state.latest_proposal.block in
            let candidate =
              {
                Operation_worker.hash = latest_proposal.hash;
                round_watched = latest_proposal.round;
                payload_hash_watched = latest_proposal.payload_hash;
              }
            in
            let* state =
              let*! action =
                State_transitions.step
                  state
                  (Baking_state.Quorum_reached (candidate, attestation_qc))
              in
              do_action action
              (* this will register the elected block *)
            in
            propose_at_next_level ~minimal_timestamp state
        | None -> (
            let*? event = Baking_scheduling.compute_bootstrap_event state in
            let*! state, _action = State_transitions.step state event in
            let latest_proposal = state.level_state.latest_proposal in
            let open State_transitions in
            let round = state.round_state.current_round in
            let*! proposal_acceptance =
              is_acceptable_proposal_for_current_level state latest_proposal
            in
            match proposal_acceptance with
            | Invalid | Outdated_proposal -> (
                match round_proposer state ~level:`Current round with
                | Some {consensus_key_and_delegate; _} ->
                    let*! action =
                      State_transitions.propose_block_action
                        state
                        consensus_key_and_delegate
                        round
                        ~last_proposal:state.level_state.latest_proposal
                    in
                    let* state = do_action (state, action) in
                    let*! () =
                      cctxt#message
                        "Reproposed block at level %ld on round %a"
                        state.level_state.current_level
                        Round.pp
                        state.round_state.current_round
                    in
                    return state
                | None -> cctxt#error "No slots for current round")
            | Valid_proposal ->
                cctxt#error
                  "Cannot propose: there's already a valid proposal for the \
                   current round %a"
                  Round.pp
                  round))
  in
  return_unit

let repropose (cctxt : Protocol_client_context.full) ?force ?force_round
    delegates =
  let open Lwt_result_syntax in
  let open Baking_state in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ?force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  (* Make sure the operation worker is populated to avoid empty blocks
     being proposed. *)
  let*? event = Baking_scheduling.compute_bootstrap_event state in
  let*! state, _action = State_transitions.step state event in
  let latest_proposal = state.level_state.latest_proposal in
  let open State_transitions in
  let round =
    match force_round with
    | Some x -> x
    | None -> state.round_state.current_round
  in
  let*! proposal_validity =
    is_acceptable_proposal_for_current_level state latest_proposal
  in
  match proposal_validity with
  | Invalid | Outdated_proposal -> (
      match Baking_state.round_proposer state ~level:`Current round with
      | Some {consensus_key_and_delegate; _} ->
          let*! action =
            State_transitions.propose_block_action
              state
              consensus_key_and_delegate
              round
              ~last_proposal:state.level_state.latest_proposal
          in
          let* state = do_action (state, action) in
          let*! () =
            cctxt#message
              "Reproposed block at level %ld on round %a"
              state.level_state.current_level
              Round.pp
              state.round_state.current_round
          in
          return_unit
      | None -> cctxt#error "No slots for current round")
  | Valid_proposal ->
      cctxt#error
        "Cannot propose: there's already a valid proposal for the current \
         round %a"
        Round.pp
        round

let bake_using_automaton ~count config state heads_stream =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let* initial_event = first_automaton_event state in
  let current_level = state.level_state.latest_proposal.block.shell.level in
  let loop_state =
    Baking_scheduling.create_loop_state
      ~heads_stream
      state.global_state.operation_worker
  in
  let stop_on_next_level_block = function
    | New_head_proposal proposal ->
        Compare.Int32.(
          proposal.block.shell.level >= Int32.(add current_level (of_int count)))
    | _ -> false
  in
  let* event_opt =
    Baking_scheduling.automaton_loop
      ~stop_on_event:stop_on_next_level_block
      ~config
      ~on_error:(fun err -> Lwt.return (Error err))
      loop_state
      state
      initial_event
  in
  match event_opt with
  | Some (New_head_proposal proposal) ->
      let*! () =
        cctxt#message
          "Last injected block: %a (level %ld)"
          Block_hash.pp
          proposal.block.hash
          proposal.block.shell.level
      in
      return_unit
  | _ -> cctxt#error "Baking loop unexpectedly ended"

(* attest the latest proposal and bake with it *)
let rec baking_minimal_timestamp ~count state
    (block_stream : proposal Lwt_stream.t) =
  let open Lwt_result_syntax in
  let cctxt = state.global_state.cctxt in
  let latest_proposal = state.level_state.latest_proposal in
  let own_attestations =
    State_transitions.make_consensus_list state latest_proposal
  in
  let current_mempool =
    Operation_worker.get_current_operations state.global_state.operation_worker
  in
  let attestations_in_mempool =
    Operation_pool.(
      filter_attestations (Operation_set.elements current_mempool.consensus))
    |> List.filter_map
         (fun
           ({
              protocol_data =
                {contents = Single (Attestation {consensus_content; _}); _};
              _;
            } :
             Kind.attestation operation)
         ->
           if
             Round.(consensus_content.round = latest_proposal.block.round)
             && Compare.Int32.(
                  Raw_level.to_int32 consensus_content.level
                  = latest_proposal.block.shell.level)
           then Some consensus_content
           else None)
  in
  let total_voting_power =
    List.fold_left
      (fun attestations own -> snd own :: attestations)
      attestations_in_mempool
      own_attestations
    |> attestations_attesting_power state
  in
  let consensus_threshold =
    state.global_state.constants.parametric.consensus_threshold
  in
  let* () =
    if Compare.Int.(total_voting_power < consensus_threshold) then
      cctxt#error
        "Delegates do not have enough voting power. Only %d is available while \
         %d is required."
        total_voting_power
        consensus_threshold
    else return_unit
  in
  let* minimal_round, delegate =
    match
      Baking_scheduling.first_potential_round_at_next_level
        state
        ~earliest_round:Round.zero
    with
    | None -> cctxt#error "No potential baking slot for the given delegates."
    | Some first_potential_round -> return first_potential_round
  in
  let* signed_attestations =
    Baking_actions.sign_consensus_votes state own_attestations `Attestation
  in
  let pool =
    Operation_pool.add_operations
      current_mempool
      (List.map (fun (_, x, _, _) -> x) signed_attestations)
  in
  let* own_dal_attestations = Baking_actions.get_dal_attestations state in
  let* signed_dal_attestations =
    Baking_actions.sign_dal_attestations state own_dal_attestations
  in
  let pool =
    Operation_pool.add_operations
      pool
      (List.map
         (fun (_delegate, op, _bitset, _published_level) -> op)
         signed_dal_attestations)
  in
  let kind = Baking_actions.Fresh pool in
  let block_to_bake : Baking_actions.block_to_bake =
    {
      Baking_actions.predecessor = latest_proposal.block;
      round = minimal_round;
      delegate;
      kind;
      force_apply = state.global_state.config.force_apply;
    }
  in
  let state_recorder ~new_state =
    Baking_state.may_record_new_state ~previous_state:state ~new_state
  in
  let* new_state =
    Baking_actions.perform_action
      ~state_recorder
      state
      (Inject_block
         {kind = Forge_and_inject block_to_bake; updated_state = state})
  in
  let*! () = cctxt#message "Injected block at minimal timestamp" in
  if count <= 1 then return_unit
  else
    let*! () =
      let attestation_level = Int32.succ latest_proposal.block.shell.level in
      Lwt_stream.junk_while_s
        (fun proposal ->
          Lwt.return
            Compare.Int32.(
              proposal.Baking_state.block.shell.level <> attestation_level))
        block_stream
    in
    let*! next_level_proposal =
      let*! r = Lwt_stream.get block_stream in
      match r with
      | None -> cctxt#error "Stream unexpectedly ended"
      | Some b -> Lwt.return b
    in
    let*! new_state, action =
      State_transitions.step new_state (New_head_proposal next_level_proposal)
    in
    let* new_state =
      match action with
      | Update_to_level update ->
          let* new_state, _preattest_action =
            Baking_actions.update_to_level new_state update
          in
          return
            {
              new_state with
              round_state =
                {
                  Baking_state.current_round = Round.zero;
                  current_phase = Idle;
                  delayed_quorum = None;
                };
            }
      | _ ->
          (* Algorithmically, this will always be an update_to_level
             action. *)
          assert false
    in
    baking_minimal_timestamp ~count:(pred count) new_state block_stream

let bake (cctxt : Protocol_client_context.full) ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?force_apply ?force
    ?(minimal_timestamp = false) ?extra_operations
    ?(monitor_node_mempool = true) ?context_path ?dal_node_endpoint ?(count = 1)
    ?votes ?state_recorder delegates =
  let open Lwt_result_syntax in
  let config =
    Baking_configuration.make
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?context_path
      ?force_apply
      ?force
      ?extra_operations
      ?dal_node_endpoint
      ?votes
      ?state_recorder
      ()
  in
  let cache = Baking_cache.Block_cache.create 10 in
  let* block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let* state =
    create_state
      cctxt
      ~monitor_node_mempool
      ~synchronize:(not minimal_timestamp)
      ~config
      ~current_proposal
      delegates
  in
  let* () =
    when_ monitor_node_mempool (fun () ->
        (* Make sure the operation worker is populated to avoid empty
           blocks being baked *)
        Operation_worker.retrieve_pending_operations
          cctxt
          state.global_state.operation_worker)
  in
  if not minimal_timestamp then
    bake_using_automaton ~count config state block_stream
  else baking_minimal_timestamp ~count state block_stream
