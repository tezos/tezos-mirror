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
open Baking_state_types
module Events = Baking_events.Lib

let sleep_until_block_timestamp prepared_block =
  let open Lwt_syntax in
  match
    Baking_scheduling.sleep_until
      prepared_block.signed_block_header.shell.timestamp
  with
  | Some waiter ->
      let* () =
        Events.(
          emit
            waiting_block_timestamp
            ( prepared_block.signed_block_header.shell.timestamp,
              Ptime.diff
                (Time.System.of_protocol_exn
                   prepared_block.signed_block_header.shell.timestamp)
                (Ptime_clock.now ()) ))
      in
      waiter
  | None -> Lwt.return_unit

let create_state cctxt ?dal_node_rpc_ctxt ?synchronize ?monitor_node_mempool
    ~config ~current_proposal delegates =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let monitor_node_operations = monitor_node_mempool in
  let* chain_id = Node_rpc.chain_id cctxt ~chain in
  let* constants =
    Node_rpc.constants cctxt ~chain:(`Hash chain_id) ~block:(`Head 0)
  in
  let*? round_durations =
    Round.Durations.create
      ~first_round_duration:constants.parametric.minimal_block_delay
      ~delay_increment_per_round:constants.parametric.delay_increment_per_round
    |> Environment.wrap_tzresult
  in
  let*! operation_worker =
    Operation_worker.run ?monitor_node_operations ~round_durations cctxt
  in
  let dal_attestable_slots_worker =
    Dal_attestable_slots_worker.create
      ~attestation_lag:constants.parametric.dal.attestation_lag
      ~number_of_slots:constants.parametric.dal.number_of_slots
  in
  let* state =
    Baking_scheduling.create_initial_state
      cctxt
      ?dal_node_rpc_ctxt
      ?synchronize
      ~chain
      config
      operation_worker
      dal_attestable_slots_worker
      round_durations
      ~current_proposal
      ~constants
      delegates
  in
  let*! () =
    Baking_actions.only_if_dal_feature_enabled
      state
      ~default_value:()
      (fun dal_node_rpc_ctxt ->
        let*! delegates =
          List.map_s
            (Baking_scheduling.try_resolve_consensus_keys cctxt)
            delegates
        in
        let delegate_ids =
          List.map Baking_state_types.Delegate_id.of_pkh delegates
        in
        (* Ensures the DAL attestable slots cache is populated in time for the
           first block’s attestation. *)
        Dal_attestable_slots_worker.update_streams_subscriptions
          state.global_state.dal_attestable_slots_worker
          dal_node_rpc_ctxt
          ~delegate_ids)
  in
  return state

let get_current_proposal cctxt ?cache () =
  let open Lwt_result_syntax in
  let* block_stream, _block_stream_stopper =
    Node_rpc.monitor_heads cctxt ?cache ~chain:cctxt#chain ()
  in
  let*! current_head = Lwt_stream.peek block_stream in
  match current_head with
  | Some current_head -> return (block_stream, current_head)
  | None -> failwith "head stream unexpectedly ended"

let preattest (cctxt : Protocol_client_context.full) ?(force = false) delegates
    =
  let open State_transitions in
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ~force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  let proposal = state.level_state.latest_proposal in
  let*! () =
    Events.(
      emit
        attempting_to_vote_for_proposal
        (Preattestation, state.level_state.latest_proposal))
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
  let consensus_batch =
    make_consensus_vote_batch state proposal Preattestation
  in
  let*! () =
    cctxt#message
      "@[<v 2>Preattesting for:@ %a@]"
      Format.(
        pp_print_list ~pp_sep:pp_print_space Baking_state_types.Delegate.pp)
      (List.map
         (fun ({delegate; _} : unsigned_consensus_vote) -> delegate)
         consensus_batch.unsigned_consensus_votes)
  in
  let* signed_consensus_batch =
    Baking_actions.sign_consensus_votes state.global_state consensus_batch
  in
  Baking_actions.inject_consensus_votes state signed_consensus_batch

let attest (cctxt : Protocol_client_context.full) ?(force = false) delegates =
  let open State_transitions in
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ~force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  let proposal = state.level_state.latest_proposal in
  let*! () =
    Events.(
      emit
        attempting_to_vote_for_proposal
        (Attestation, state.level_state.latest_proposal))
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
  let consensus_batch = make_consensus_vote_batch state proposal Attestation in
  let*! () =
    cctxt#message
      "@[<v 2>Attesting for:@ %a@]"
      Format.(
        pp_print_list ~pp_sep:pp_print_space Baking_state_types.Delegate.pp)
      (List.map
         (fun ({delegate; _} : unsigned_consensus_vote) -> delegate)
         consensus_batch.unsigned_consensus_votes)
  in
  let* signed_consensus_batch =
    Baking_actions.sign_consensus_votes state.global_state consensus_batch
  in
  let* () =
    Baking_state.may_record_new_state ~previous_state:state ~new_state:state
  in
  Baking_actions.inject_consensus_votes state signed_consensus_batch

let do_action (state, action) =
  let open Lwt_result_syntax in
  let* new_state = Baking_actions.perform_action state action in
  let* () =
    Baking_state.may_record_new_state ~previous_state:state ~new_state
  in
  return new_state

let bake_at_next_level_event state =
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
      return
        (Baking_state.Timeout
           (Time_to_prepare_next_level_block {at_round = round}))

let bake_at_next_level state =
  let open Lwt_result_syntax in
  let* event = bake_at_next_level_event state in
  let*! state, action = State_transitions.step state event in
  match action with
  | Prepare_block {block_to_bake} ->
      let* prepared_block =
        Baking_actions.prepare_block state.global_state block_to_bake
      in
      let*! () = sleep_until_block_timestamp prepared_block in
      let* new_state =
        do_action
          ( state,
            Inject_block
              {prepared_block; force_injection = false; asynchronous = false} )
      in
      return new_state
  | _ -> assert false

(* Simulate the end of the current round to bootstrap the automaton
   or attest the block if necessary *)
let first_automaton_event state =
  match state.level_state.elected_block with
  | None -> Lwt.return (Baking_scheduling.compute_bootstrap_event state)
  | Some _elected_block ->
      (* If there is an elected block we can directly bake at next
         level after waiting its date *)
      bake_at_next_level_event state

let attestations_attesting_power state attestations =
  let get_attestation_voting_power {slot; _} =
    match
      Delegate_infos.voting_power state.level_state.delegate_infos ~slot
    with
    | None -> 0L (* cannot happen *)
    | Some attesting_power -> attesting_power
  in
  List.sort_uniq compare attestations
  |> List.fold_left
       (fun power attestation ->
         Int64.add power (get_attestation_voting_power attestation))
       0L

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
    let kind = Fresh pool in
    let force_apply =
      Round.(minimal_round >= state.global_state.config.force_apply_from_round)
    in
    let block_to_bake =
      {
        predecessor = state.level_state.latest_proposal.block;
        round = minimal_round;
        delegate;
        kind;
        force_apply;
      }
    in
    let* prepared_block =
      Baking_actions.prepare_block state.global_state block_to_bake
    in
    let*! () = sleep_until_block_timestamp prepared_block in
    let* state =
      do_action
        ( state,
          Inject_block
            {
              prepared_block;
              force_injection = minimal_timestamp;
              asynchronous = false;
            } )
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
    let* state = bake_at_next_level state in
    let*! () = cctxt#message "Proposal injected" in
    return state

let attestation_quorum state =
  let power, attestations = state_attesting_power state in
  let consensus_threshold =
    Delegate_infos.consensus_threshold state.level_state.delegate_infos
  in
  if Compare.Int64.(power >= consensus_threshold) then Some (power, attestations)
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
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
    ?force_apply_from_round ?(force = false) ?(minimal_timestamp = false)
    ?extra_operations ?data_dir ?state_recorder delegates =
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let config =
    Baking_configuration.make
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?data_dir
      ?force_apply_from_round
      ~force
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
                level_watched = latest_proposal.shell.level;
                round_watched = latest_proposal.round;
                payload_hash_watched = latest_proposal.payload_hash;
                branch_watched =
                  (if
                     state.global_state.constants.parametric
                       .aggregate_attestation
                   then Some latest_proposal.grandparent
                   else None);
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
                | Some {delegate; _} ->
                    let*! action =
                      State_transitions.propose_block_action
                        state
                        delegate
                        round
                        ~last_proposal:state.level_state.latest_proposal
                    in
                    let* state =
                      match action with
                      | Prepare_block {block_to_bake} ->
                          let* prepared_block =
                            Baking_actions.prepare_block
                              state.global_state
                              block_to_bake
                          in
                          let*! () =
                            sleep_until_block_timestamp prepared_block
                          in
                          let* state =
                            do_action
                              ( state,
                                Inject_block
                                  {
                                    prepared_block;
                                    force_injection = force;
                                    asynchronous = false;
                                  } )
                          in
                          return state
                      | Inject_block {prepared_block; _} ->
                          let*! () =
                            sleep_until_block_timestamp prepared_block
                          in
                          let* state =
                            do_action
                              ( state,
                                Inject_block
                                  {
                                    prepared_block;
                                    force_injection = force;
                                    asynchronous = false;
                                  } )
                          in
                          return state
                      | _ -> assert false
                    in
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

let mk_prequorum state latest_proposal =
  let open Lwt_result_syntax in
  let* batch =
    State_transitions.make_consensus_vote_batch
      state
      latest_proposal
      Preattestation
    |> Baking_actions.sign_consensus_votes state.global_state
  in
  let {level; round; block_payload_hash} : batch_content =
    batch.batch_content
  in
  let preattestations =
    List.map (fun op -> op.signed_operation) batch.signed_consensus_votes
  in
  return
    {
      level = Raw_level.to_int32 level;
      round;
      block_payload_hash;
      preattestations;
    }

let repropose (cctxt : Protocol_client_context.full) ?(force = false)
    ?force_round ?(minimal_timestamp = false) ?(force_reproposal = false)
    delegates =
  let open Lwt_result_syntax in
  let open Baking_state in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let cache = Baking_cache.Block_cache.create 10 in
  let* _block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let config = Baking_configuration.make ~force () in
  let* state = create_state cctxt ~config ~current_proposal delegates in
  (* Make sure the operation worker is populated to avoid empty blocks
     being proposed. *)
  let*? event = Baking_scheduling.compute_bootstrap_event state in
  let*! state, _action = State_transitions.step state event in
  let latest_proposal = state.level_state.latest_proposal in
  let* state =
    if force_reproposal then
      (* We forge a prequorum including all available delegates
         assuming they have enough consensus power to reach the
         consensus threshold *)
      let* prequorum = mk_prequorum state latest_proposal in
      let attestable_payload = Some {proposal = latest_proposal; prequorum} in
      return
        {state with level_state = {state.level_state with attestable_payload}}
    else return state
  in
  let open State_transitions in
  let round =
    match (force_round, minimal_timestamp) with
    | Some round, _ -> round
    | None, true -> (
        let next_round = Round.succ latest_proposal.block.round in
        match
          Baking_scheduling.first_potential_round_at_current_level
            ~earliest_round:next_round
            state
        with
        | Some (round, _) -> round
        | None -> next_round)
    | None, false -> state.round_state.current_round
  in
  let*! proposal_validity =
    is_acceptable_proposal_for_current_level state latest_proposal
  in
  match proposal_validity with
  | Invalid | Outdated_proposal -> (
      match Baking_state.round_proposer state ~level:`Current round with
      | Some {delegate; _} ->
          let*! action =
            State_transitions.propose_block_action
              state
              delegate
              round
              ~last_proposal:state.level_state.latest_proposal
          in
          let* signed_block =
            match action with
            | Prepare_block {block_to_bake} ->
                let* signed_block =
                  Baking_actions.prepare_block state.global_state block_to_bake
                in
                let*! () = sleep_until_block_timestamp signed_block in
                let* _state =
                  do_action
                    ( state,
                      Inject_block
                        {
                          prepared_block = signed_block;
                          force_injection = force;
                          asynchronous = false;
                        } )
                in
                return signed_block
            | _ -> assert false
          in
          let*! () =
            cctxt#message
              "Reproposed block at level %ld on round %a"
              signed_block.signed_block_header.shell.level
              Round.pp
              signed_block.round
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
  let forge_event_stream =
    state.global_state.forge_worker_hooks.get_forge_event_stream ()
  in
  let loop_state =
    Baking_scheduling.create_loop_state
      ~heads_stream
      ~forge_event_stream
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
    State_transitions.make_consensus_vote_batch
      state
      latest_proposal
      Attestation
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
      (fun attestations own ->
        own.Baking_state.vote_consensus_content :: attestations)
      attestations_in_mempool
      own_attestations.unsigned_consensus_votes
    |> attestations_attesting_power state
  in
  let consensus_threshold =
    Delegate_infos.consensus_threshold state.level_state.delegate_infos
  in
  let* () =
    if Compare.Int64.(total_voting_power < consensus_threshold) then
      cctxt#error
        "Delegates do not have enough voting power. Only %Ld is available \
         while %Ld is required."
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
    let*! own_attestations_with_dal =
      dal_content_map_p
        (Baking_actions.may_get_dal_content state)
        own_attestations
    in
    Baking_actions.sign_consensus_votes
      state.global_state
      own_attestations_with_dal
  in
  let pool =
    Operation_pool.add_operations
      current_mempool
      (List.map
         (fun signed_consensus -> signed_consensus.signed_operation)
         signed_attestations.signed_consensus_votes)
  in
  let kind = Fresh pool in
  let force_apply =
    Round.(minimal_round >= state.global_state.config.force_apply_from_round)
  in
  let block_to_bake =
    {
      predecessor = latest_proposal.block;
      round = minimal_round;
      delegate;
      kind;
      force_apply;
    }
  in
  let* prepared_block =
    Baking_actions.prepare_block state.global_state block_to_bake
  in
  let*! () = sleep_until_block_timestamp prepared_block in
  let* new_state =
    do_action
      ( state,
        Inject_block
          {prepared_block; force_injection = true; asynchronous = false} )
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
              proposal.Baking_state_types.block.shell.level <> attestation_level))
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
                  early_attestations = [];
                  awaiting_unlocking_pqc = false;
                };
            }
      | _ ->
          (* Algorithmically, this will always be an update_to_level
             action. *)
          assert false
    in
    baking_minimal_timestamp ~count:(pred count) new_state block_stream

let bake (cctxt : Protocol_client_context.full) ?dal_node_rpc_ctxt ?minimal_fees
    ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte
    ?force_apply_from_round ?force ?(minimal_timestamp = false)
    ?extra_operations ?(monitor_node_mempool = true) ?data_dir ?(count = 1)
    ?votes ?state_recorder delegates =
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let config =
    Baking_configuration.make
      ?minimal_fees
      ?minimal_nanotez_per_gas_unit
      ?minimal_nanotez_per_byte
      ?data_dir
      ?force_apply_from_round
      ?force
      ?extra_operations
      ?votes
      ?state_recorder
      ()
  in
  let cache = Baking_cache.Block_cache.create 10 in
  let* block_stream, current_proposal = get_current_proposal cctxt ~cache () in
  let* state =
    create_state
      cctxt
      ?dal_node_rpc_ctxt
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
