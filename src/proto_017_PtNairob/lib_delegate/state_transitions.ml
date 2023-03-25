(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Baking_actions
module Events = Baking_events.State_transitions

let do_nothing state = Lwt.return (state, Do_nothing)

type proposal_acceptance = Invalid | Outdated_proposal | Valid_proposal

let is_acceptable_proposal_for_current_level state
    (proposal : Baking_state.proposal) =
  let current_round = state.round_state.current_round in
  if Round.(current_round < proposal.block.round) then
    Events.(
      emit unexpected_proposal_round (current_round, proposal.block.round))
    >>= fun () -> Lwt.return Invalid
  else if Round.(current_round > proposal.block.round) then
    Lwt.return Outdated_proposal
  else
    (* current_round = proposal.round *)
    let previous_proposal = state.level_state.latest_proposal in
    if
      Round.(proposal.block.round = previous_proposal.block.round)
      && Block_hash.(proposal.block.hash <> previous_proposal.block.hash)
      && Block_hash.(
           proposal.predecessor.hash = previous_proposal.predecessor.hash)
    then
      (* An existing proposal was found at the same round: the
         proposal is bad and should be punished by the accuser *)
      Events.(
        emit
          proposal_for_round_already_seen
          (proposal.block.hash, current_round, previous_proposal.block.hash))
      >>= fun () -> Lwt.return Invalid
    else
      (* current_round = proposal.block.round ∧
         proposal.block.round <> previous_proposal.block.round
         =>
         proposal.block.round > previous_proposal.block.round

         The proposal has the expected round and the previous proposal
         is a predecessor therefore the proposal is valid *)
      Lwt.return Valid_proposal

let make_consensus_list state proposal =
  (* TODO efficiently iterate on the slot map instead of removing
     duplicate endorsements *)
  let level =
    Raw_level.of_int32 state.level_state.current_level |> function
    | Ok l -> l
    | _ -> assert false
  in
  let round = proposal.block.round in
  let block_payload_hash = proposal.block.payload_hash in
  SlotMap.fold
    (fun _slot (consensus_key_and_delegate, slots) acc ->
      ( consensus_key_and_delegate,
        {slot = slots.first_slot; level; round; block_payload_hash} )
      :: acc)
    state.level_state.delegate_slots.own_delegate_slots
    []
  |> List.sort_uniq compare

(* If we do not have any slots, we won't inject any operation but we
   will still participate to determine an elected block *)
let make_preendorse_action state proposal =
  let preendorsements : (consensus_key_and_delegate * consensus_content) list =
    make_consensus_list state proposal
  in
  Inject_preendorsements {preendorsements}

let update_proposal ~is_proposal_applied state proposal =
  Events.(emit updating_latest_proposal proposal.block.hash) >>= fun () ->
  let prev_proposal = state.level_state.latest_proposal in
  let is_latest_proposal_applied =
    (* mark as applied if it is indeed applied or if this specific proposal was
       already marked as applied *)
    is_proposal_applied
    || prev_proposal.block.hash = proposal.block.hash
       && state.level_state.is_latest_proposal_applied
  in
  let new_level_state =
    {
      state.level_state with
      is_latest_proposal_applied;
      latest_proposal = proposal;
    }
  in
  Lwt.return {state with level_state = new_level_state}

let may_update_proposal ~is_proposal_applied state (proposal : proposal) =
  assert (
    Compare.Int32.(
      state.level_state.latest_proposal.block.shell.level
      = proposal.block.shell.level)) ;
  if
    Round.(state.level_state.latest_proposal.block.round < proposal.block.round)
  then update_proposal ~is_proposal_applied state proposal
  else Lwt.return state

let preendorse state proposal =
  if Baking_state.is_first_block_in_protocol proposal then
    (* We do not preendorse the first transition block *)
    let new_state = update_current_phase state Idle in
    Lwt.return (new_state, Do_nothing)
  else
    Events.(emit attempting_preendorse_proposal proposal.block.hash)
    >>= fun () ->
    let new_state =
      (* We await for the block to be applied before updating its
         locked values. *)
      if state.level_state.is_latest_proposal_applied then
        update_current_phase state Awaiting_preendorsements
      else update_current_phase state Awaiting_application
    in
    Lwt.return (new_state, make_preendorse_action state proposal)

let extract_pqc state (new_proposal : proposal) =
  match new_proposal.block.prequorum with
  | None -> None
  | Some pqc ->
      let add_voting_power acc (op : Kind.preendorsement Operation.t) =
        let open Protocol.Alpha_context.Operation in
        let {
          shell = _;
          protocol_data = {contents = Single (Preendorsement {slot; _}); _};
          _;
        } =
          op
        in
        match
          SlotMap.find slot state.level_state.delegate_slots.all_delegate_slots
        with
        | None ->
            (* cannot happen if the map is correctly populated *)
            acc
        | Some {endorsing_power; _} -> acc + endorsing_power
      in
      let voting_power =
        List.fold_left add_voting_power 0 pqc.preendorsements
      in
      let consensus_threshold =
        state.global_state.constants.parametric.consensus_threshold
      in
      if Compare.Int.(voting_power >= consensus_threshold) then
        Some (pqc.preendorsements, pqc.round)
      else None

let may_update_endorsable_payload_with_internal_pqc state
    (new_proposal : proposal) =
  match
    (new_proposal.block.prequorum, state.level_state.endorsable_payload)
  with
  | None, _ ->
      (* The proposal does not contain a PQC: no need to update *)
      state
  | Some {round = new_round; _}, Some {prequorum = {round = old_round; _}; _}
    when Round.(new_round < old_round) ->
      (* The proposal pqc is outdated, do not update *)
      state
  | Some better_prequorum, _ ->
      assert (
        Block_payload_hash.(
          better_prequorum.block_payload_hash = new_proposal.block.payload_hash)) ;
      assert (
        Compare.Int32.(better_prequorum.level = new_proposal.block.shell.level)) ;
      let new_endorsable_payload =
        Some {proposal = new_proposal; prequorum = better_prequorum}
      in
      let new_level_state =
        {state.level_state with endorsable_payload = new_endorsable_payload}
      in
      {state with level_state = new_level_state}

let may_update_is_latest_proposal_applied ~is_proposal_applied state
    new_proposal =
  let current_proposal = state.level_state.latest_proposal in
  if
    is_proposal_applied
    && Block_hash.(current_proposal.block.hash = new_proposal.block.hash)
  then
    let new_level_state =
      {state.level_state with is_latest_proposal_applied = true}
    in
    let new_state = {state with level_state = new_level_state} in
    new_state
  else state

let has_already_been_handled state new_proposal =
  let current_proposal = state.level_state.latest_proposal in
  Block_hash.(current_proposal.block.hash = new_proposal.block.hash)
  && state.level_state.is_latest_proposal_applied

let rec handle_proposal ~is_proposal_applied state (new_proposal : proposal) =
  let current_level = state.level_state.current_level in
  let new_proposal_level = new_proposal.block.shell.level in
  let current_proposal = state.level_state.latest_proposal in
  let state =
    may_update_is_latest_proposal_applied
      ~is_proposal_applied
      state
      new_proposal
  in
  if Compare.Int32.(current_level > new_proposal_level) then
    (* The baker is ahead, a reorg may have happened. Do nothing:
       wait for the node to send us the branch's head. This new head
       should have a fitness that is greater than our current
       proposal and thus, its level should be at least the same as
       our current proposal's level. *)
    Events.(emit baker_is_ahead_of_node (current_level, new_proposal_level))
    >>= fun () -> do_nothing state
  else if Compare.Int32.(current_level = new_proposal_level) then
    if
      (* The received head is a new proposal for the current level:
         let's check if it's a valid one for us. *)
      Block_hash.(
        current_proposal.predecessor.hash <> new_proposal.predecessor.hash)
    then
      Events.(
        emit
          new_proposal_is_on_another_branch
          (current_proposal.predecessor.hash, new_proposal.predecessor.hash))
      >>= fun () -> may_switch_branch ~is_proposal_applied state new_proposal
    else
      is_acceptable_proposal_for_current_level state new_proposal >>= function
      | Invalid ->
          (* The proposal is invalid: we ignore it *)
          Events.(emit skipping_invalid_proposal ()) >>= fun () ->
          do_nothing state
      | Outdated_proposal ->
          (* Check whether we need to update our endorsable payload  *)
          let state =
            may_update_endorsable_payload_with_internal_pqc state new_proposal
          in
          (* The proposal is outdated: we update to be able to extract
             its included endorsements but we do not endorse it *)
          Events.(emit outdated_proposal new_proposal.block.hash) >>= fun () ->
          may_update_proposal ~is_proposal_applied state new_proposal
          >>= fun state -> do_nothing state
      | Valid_proposal -> (
          (* Valid_proposal => proposal.round = current_round *)
          (* Check whether we need to update our endorsable payload  *)
          let new_state =
            may_update_endorsable_payload_with_internal_pqc state new_proposal
          in
          may_update_proposal ~is_proposal_applied new_state new_proposal
          >>= fun new_state ->
          (* The proposal is valid but maybe we already locked on a payload *)
          match new_state.level_state.locked_round with
          | Some locked_round -> (
              if
                Block_payload_hash.(
                  locked_round.payload_hash = new_proposal.block.payload_hash)
              then
                (* when the new head has the same payload as our
                   [locked_round], we accept it and preendorse *)
                preendorse new_state new_proposal
              else
                (* The payload is different *)
                match new_proposal.block.prequorum with
                | Some {round; _} when Round.(locked_round.round < round) ->
                    (* This PQC is above our locked_round, we can preendorse it *)
                    preendorse new_state new_proposal
                | _ ->
                    (* We shouldn't preendorse this proposal, but we
                       should at least watch (pre)quorums events on it
                       but only when it is applied otherwise we await
                       for the proposal to be applied. *)
                    if is_proposal_applied then
                      let new_state =
                        update_current_phase new_state Awaiting_preendorsements
                      in
                      Lwt.return (new_state, Watch_proposal)
                    else do_nothing new_state)
          | None ->
              (* Otherwise, we did not lock on any payload, thus we can
                 preendorse it *)
              preendorse new_state new_proposal)
  else
    (* Last case: new_proposal_level > current_level *)
    (* Possible scenarios:
       - we received a block for a next level
       - we received our own block
         This is where we update our [level_state] (and our [round_state]) *)
    Events.(emit new_head_with_increasing_level ()) >>= fun () ->
    let new_level = new_proposal.block.shell.level in
    let compute_new_state ~current_round ~delegate_slots
        ~next_level_delegate_slots =
      let round_state =
        {current_round; current_phase = Idle; delayed_prequorum = None}
      in
      let level_state =
        {
          current_level = new_level;
          latest_proposal = new_proposal;
          is_latest_proposal_applied = is_proposal_applied;
          (* Unlock values *)
          locked_round = None;
          endorsable_payload = None;
          elected_block = None;
          delegate_slots;
          next_level_delegate_slots;
          next_level_proposed_round = None;
        }
      in
      (* recursive call with the up-to-date state to handle the new
         level proposals *)
      handle_proposal
        ~is_proposal_applied
        {state with level_state; round_state}
        new_proposal
    in
    let action =
      Update_to_level {new_level_proposal = new_proposal; compute_new_state}
    in
    Lwt.return (state, action)

and may_switch_branch ~is_proposal_applied state new_proposal =
  let switch_branch state =
    Events.(emit switching_branch ()) >>= fun () ->
    (* If we are on a different branch, we also need to update our
       [round_state] accordingly.
       The recursive call to [handle_proposal] cannot end up
       with an invalid proposal as it's on a different branch, thus
       there is no need to backtrack to the former state as the new
       proposal must end up being the new [latest_proposal]. That's
       why we update it here. *)
    let round_update =
      {
        Baking_actions.new_round_proposal = new_proposal;
        handle_proposal =
          (fun state -> handle_proposal ~is_proposal_applied state new_proposal);
      }
    in
    update_proposal ~is_proposal_applied state new_proposal >>= fun new_state ->
    (* TODO if the branch proposal is outdated, we should
       trigger an [End_of_round] to participate *)
    Lwt.return (new_state, Synchronize_round round_update)
  in
  let current_endorsable_payload = state.level_state.endorsable_payload in
  match (current_endorsable_payload, new_proposal.block.prequorum) with
  | None, Some _ | None, None ->
      Events.(emit branch_proposal_has_better_fitness ()) >>= fun () ->
      (* The new branch contains a PQC (and we do not) or a better
         fitness, we switch. *)
      switch_branch state
  | Some _, None ->
      (* We have a better PQC, we don't switch as we are able to
         propose a better chain if we stay on our current one. *)
      Events.(emit branch_proposal_has_no_prequorum ()) >>= fun () ->
      do_nothing state
  | Some {prequorum = current_pqc; _}, Some new_pqc ->
      if Round.(current_pqc.round > new_pqc.round) then
        Events.(emit branch_proposal_has_lower_prequorum ()) >>= fun () ->
        (* The other's branch PQC is lower than ours, do not
           switch *)
        do_nothing state
      else if Round.(current_pqc.round < new_pqc.round) then
        Events.(emit branch_proposal_has_better_prequorum ()) >>= fun () ->
        (* Their PQC is better than ours: we switch *)
        switch_branch state
      else
        (* current_pqc.round = new_pqc *)
        (* There is a PQC on two branches with the same round and
           the same level but not the same predecessor : it's
           impossible unless if there was some double-baking. This
           shouldn't happen but do nothing anyway. *)
        Events.(emit branch_proposal_has_same_prequorum ()) >>= fun () ->
        do_nothing state

let may_register_early_prequorum state ((candidate, _) as received_prequorum) =
  if
    Block_hash.(
      candidate.Operation_worker.hash
      <> state.level_state.latest_proposal.block.hash)
  then
    Events.(
      emit
        unexpected_pqc_while_waiting_for_application
        (candidate.hash, state.level_state.latest_proposal.block.hash))
    >>= fun () -> do_nothing state
  else
    Events.(emit pqc_while_waiting_for_application candidate.hash) >>= fun () ->
    let new_round_state =
      {state.round_state with delayed_prequorum = Some received_prequorum}
    in
    let new_state = {state with round_state = new_round_state} in
    do_nothing new_state

(** In the association map [delegate_slots], the function returns an
    optional pair ([delegate], [endorsing_slot]) if for the current
    [round], the validator [delegate] has a endorsing slot. *)
let round_proposer state delegate_slots round =
  (* TODO: make sure that for each slots all rounds in the map are filled *)
  (* !FIXME! Endorsers and proposer are differents sets *)
  (* !FIXME! the slotmap may be inconsistent & may sure to document
     the invariants *)
  let round_mod =
    Int32.to_int (Round.to_int32 round)
    mod state.global_state.constants.parametric.consensus_committee_size
  in
  SlotMap.find
    state.level_state.delegate_slots.all_slots_by_round.(round_mod)
    delegate_slots

(** Inject a fresh block proposal containing the current operations of the
    mempool in [state] and the additional [endorsements] and [dal_attestations]
    for [delegate] at round [round]. *)
let propose_fresh_block_action ~endorsements ~dal_attestations ?last_proposal
    ~(predecessor : block_info) state delegate round =
  (* TODO check if there is a trace where we could not have updated the level *)
  (* The block to bake embeds the operations gathered by the
     worker. However, consensus operations that are not relevant for
     this block are filtered out. In the case of proposing a new fresh
     block, the block is supposed to carry only endorsements for the
     previous level. *)
  let operation_pool =
    (* 1. Fetch operations from the mempool. *)
    let current_mempool =
      let pool =
        Operation_worker.get_current_operations
          state.global_state.operation_worker
      in
      (* Considered the operations in the previous proposal as well *)
      match last_proposal with
      | Some proposal ->
          let {
            Operation_pool.votes_payload;
            anonymous_payload;
            managers_payload;
          } =
            proposal.payload
          in
          List.fold_left
            Operation_pool.add_operations
            pool
            [votes_payload; anonymous_payload; managers_payload]
      | None -> pool
    in
    (* 2. Filter and only retain relevant endorsements. *)
    let relevant_consensus_operations =
      let endorsement_filter =
        {
          Operation_pool.level = predecessor.shell.level;
          round = predecessor.round;
          payload_hash = predecessor.payload_hash;
        }
      in
      Operation_pool.filter_with_relevant_consensus_ops
        ~endorsement_filter
        ~preendorsement_filter:None
        current_mempool.consensus
    in
    let filtered_mempool =
      {current_mempool with consensus = relevant_consensus_operations}
    in
    (* 3. Add the additional given [endorsements] and [dal_attestations].
         N.b. this is a set: there won't be duplicates *)
    let pool =
      Operation_pool.add_operations
        filtered_mempool
        (List.map Operation.pack endorsements)
    in
    Operation_pool.add_operations
      pool
      (List.map Operation.pack dal_attestations)
  in
  let kind = Fresh operation_pool in
  Events.(emit proposing_fresh_block (delegate, round)) >>= fun () ->
  let force_apply =
    state.global_state.config.force_apply || Round.(round <> zero)
    (* This is used as a safety net by applying blocks on round > 0, in case
       validation-only did not produce a correct round-0 block. *)
  in
  let block_to_bake = {predecessor; round; delegate; kind; force_apply} in
  let updated_state = update_current_phase state Idle in
  Lwt.return @@ Inject_block {block_to_bake; updated_state}

let propose_block_action state delegate round (proposal : proposal) =
  (* Possible cases:
     1. There was a proposal but the PQC was not reached.
     2. There was a proposal and the PQC was reached. We repropose the
     [endorsable_payload] if it exists, not the [locked_round] as it
     may be older. *)
  match state.level_state.endorsable_payload with
  | None ->
      Events.(emit no_endorsable_payload_fresh_block ()) >>= fun () ->
      (* For case 1, we may re-inject with the same payload or a fresh
         one. We make the choice of baking a fresh one: the previous
         proposal may have been rejected because the block may have been
         valid but may be considered "bad" (censored operations, empty
         block, etc.) by the other validators. *)
      (* Invariant: there is no locked round if there is no endorsable
         payload *)
      assert (state.level_state.locked_round = None) ;
      let endorsements_in_last_proposal = proposal.block.quorum in
      (* Also insert the DAL attestations from the proposal, because the mempool
         may not contain them anymore *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4671
         The block may therefore contain multiple attestations for the same delegate. *)
      let dal_attestations_in_last_proposal = proposal.block.dal_attestations in
      propose_fresh_block_action
        ~endorsements:endorsements_in_last_proposal
        ~dal_attestations:dal_attestations_in_last_proposal
        state
        ~last_proposal:proposal.block
        ~predecessor:proposal.predecessor
        delegate
        round
  | Some {proposal; prequorum} ->
      Events.(emit repropose_block proposal.block.payload_hash) >>= fun () ->
      (* For case 2, we re-inject the same block as [endorsable_round]
         but we may add some left-overs endorsements. Therefore, the
         operations we need to include are:
         - the proposal's included endorsements
         - the potential missing new endorsements for the
           previous block
         - the PQC of the endorsable payload *)
      let consensus_operations =
        (* Fetch preendorsements and endorsements from the mempool
           (that could be missing from the proposal), filter, then add
           consensus operations of the proposal itself, and convert
           into [packed_operation trace]. *)
        let mempool_consensus_operations =
          (Operation_worker.get_current_operations
             state.global_state.operation_worker)
            .consensus
        in
        let all_consensus_operations =
          (* Add the proposal and pqc consensus operations to the
             mempool *)
          List.fold_left
            (fun set op -> Operation_pool.Operation_set.add op set)
            mempool_consensus_operations
            (List.map Operation.pack proposal.block.quorum
            @ List.map Operation.pack prequorum.preendorsements)
        in
        let endorsement_filter =
          {
            Operation_pool.level = proposal.predecessor.shell.level;
            round = proposal.predecessor.round;
            payload_hash = proposal.predecessor.payload_hash;
          }
        in
        let preendorsement_filter =
          Some
            {
              Operation_pool.level = prequorum.level;
              round = prequorum.round;
              payload_hash = prequorum.block_payload_hash;
            }
        in
        Operation_pool.(
          filter_with_relevant_consensus_ops
            ~endorsement_filter
            ~preendorsement_filter
            all_consensus_operations
          |> Operation_set.elements)
      in
      let payload_hash = proposal.block.payload_hash in
      let payload_round = proposal.block.payload_round in
      let payload = proposal.block.payload in
      let kind =
        Reproposal {consensus_operations; payload_hash; payload_round; payload}
      in
      let force_apply =
        true
        (* This is used as a safety net by applying blocks on round > 0, in case
           validation-only did not produce a correct round-0 block. *)
      in
      let block_to_bake =
        {predecessor = proposal.predecessor; round; delegate; kind; force_apply}
      in
      let updated_state = update_current_phase state Idle in
      Lwt.return @@ Inject_block {block_to_bake; updated_state}

let end_of_round state current_round =
  let new_round = Round.succ current_round in
  let new_round_state = {state.round_state with current_round = new_round} in
  let new_state = {state with round_state = new_round_state} in
  (* we need to check if we need to bake for this round or not *)
  match
    round_proposer
      new_state
      new_state.level_state.delegate_slots.own_delegate_slots
      new_state.round_state.current_round
  with
  | None ->
      Events.(
        emit
          no_proposal_slot
          (current_round, state.level_state.current_level, new_round))
      >>= fun () ->
      (* We don't have any delegate that may propose a new block for
         this round -- We will wait for preendorsements when the next
         level block arrive. Meanwhile, we are idle *)
      let new_state = update_current_phase new_state Idle in
      do_nothing new_state
  | Some (delegate, _) ->
      let latest_proposal = state.level_state.latest_proposal in
      if Baking_state.is_first_block_in_protocol latest_proposal then
        (* Do not inject a block for the previous protocol! (Let the
           baker of the previous protocol do it.) *)
        do_nothing new_state
      else
        Events.(
          emit
            proposal_slot
            (current_round, state.level_state.current_level, new_round, delegate))
        >>= fun () ->
        (* We have a delegate, we need to determine what to inject *)
        propose_block_action
          new_state
          delegate
          new_round
          state.level_state.latest_proposal
        >>= fun action -> Lwt.return (new_state, action)

let time_to_bake_at_next_level state at_round =
  (* It is now time to update the state level *)
  (* We need to keep track for which block we have 2f+1 *endorsements*, that is,
     which will become the new predecessor_block *)
  (* Invariant: endorsable_round >= round(elected block) >= locked_round *)
  let round_proposer_opt =
    round_proposer
      state
      state.level_state.next_level_delegate_slots.own_delegate_slots
      at_round
  in
  match (state.level_state.elected_block, round_proposer_opt) with
  | None, _ | _, None ->
      (* Unreachable: the [Time_to_bake_next_level] event can only be
         triggered when we have a slot and an elected block *)
      assert false
  | Some elected_block, Some (delegate, _) ->
      let endorsements = elected_block.endorsement_qc in
      let dal_attestations =
        (* Unlike endorsements, we don't watch and store DAL attestations for
           each proposal, we'll retrieve them from the mempool *)
        []
      in
      let new_level_state =
        {state.level_state with next_level_proposed_round = Some at_round}
      in
      let new_state = {state with level_state = new_level_state} in
      propose_fresh_block_action
        ~endorsements
        ~dal_attestations
        ~predecessor:elected_block.proposal.block
        new_state
        delegate
        at_round
      >>= fun action -> Lwt.return (new_state, action)

let update_locked_round state round payload_hash =
  let locked_round = Some {payload_hash; round} in
  let new_level_state = {state.level_state with locked_round} in
  {state with level_state = new_level_state}

let make_endorse_action state proposal =
  let endorsements : (consensus_key_and_delegate * consensus_content) list =
    make_consensus_list state proposal
  in
  Inject_endorsements {endorsements}

let prequorum_reached_when_awaiting_preendorsements state candidate
    preendorsements =
  let latest_proposal = state.level_state.latest_proposal in
  if Block_hash.(candidate.Operation_worker.hash <> latest_proposal.block.hash)
  then
    Events.(
      emit
        unexpected_prequorum_received
        (candidate.hash, latest_proposal.block.hash))
    >>= fun () -> do_nothing state
  else if not state.level_state.is_latest_proposal_applied then
    Events.(emit handling_prequorum_on_non_applied_proposal ()) >>= fun () ->
    do_nothing state
  else
    let prequorum =
      {
        level = latest_proposal.block.shell.level;
        round = latest_proposal.block.round;
        block_payload_hash = latest_proposal.block.payload_hash;
        preendorsements
        (* preendorsements may be nil when [consensus_threshold] is 0 *);
      }
    in
    let new_endorsable_payload = {proposal = latest_proposal; prequorum} in
    let new_level_state =
      let level_state_with_new_payload =
        {
          state.level_state with
          endorsable_payload = Some new_endorsable_payload;
        }
      in
      match state.level_state.endorsable_payload with
      | None -> level_state_with_new_payload
      | Some endorsable_payload ->
          if
            Round.(
              endorsable_payload.prequorum.round
              < new_endorsable_payload.prequorum.round)
          then level_state_with_new_payload
          else state.level_state
    in
    let new_state = {state with level_state = new_level_state} in
    let new_state =
      update_locked_round
        new_state
        latest_proposal.block.round
        latest_proposal.block.payload_hash
    in
    let new_state = update_current_phase new_state Awaiting_endorsements in
    Lwt.return (new_state, make_endorse_action new_state latest_proposal)

let quorum_reached_when_waiting_endorsements state candidate endorsement_qc =
  let latest_proposal = state.level_state.latest_proposal in
  if Block_hash.(candidate.Operation_worker.hash <> latest_proposal.block.hash)
  then
    Events.(
      emit
        unexpected_quorum_received
        (candidate.hash, latest_proposal.block.hash))
    >>= fun () -> do_nothing state
  else
    let new_level_state =
      match state.level_state.elected_block with
      | None ->
          let elected_block =
            Some {proposal = latest_proposal; endorsement_qc}
          in
          {state.level_state with elected_block}
      | Some _ ->
          (* If we already have an elected block, do not update it: the
             earliest, the better. *)
          state.level_state
    in
    let new_round_state = {state.round_state with current_phase = Idle} in
    let new_state =
      {state with round_state = new_round_state; level_state = new_level_state}
    in
    do_nothing new_state

let handle_expected_applied_proposal (state : Baking_state.t) =
  let new_level_state =
    {state.level_state with is_latest_proposal_applied = true}
  in
  let new_state = {state with level_state = new_level_state} in
  match new_state.round_state.delayed_prequorum with
  | None ->
      (* The application arrived before the prequorum: just wait for the prequorum. *)
      let new_state = update_current_phase new_state Awaiting_preendorsements in
      do_nothing new_state
  | Some (candidate, preendorsement_qc) ->
      (* The application arrived after the prequorum: handle the
         prequorum received earlier.
         Start by resetting the delayed_prequorum *)
      let new_round_state =
        {new_state.round_state with delayed_prequorum = None}
      in
      let new_state =
        {
          state with
          level_state = new_level_state;
          round_state = new_round_state;
        }
      in
      prequorum_reached_when_awaiting_preendorsements
        new_state
        candidate
        preendorsement_qc

(* Hypothesis:
   - The state is not to be modified outside this module
     (NB: there are exceptions in Baking_actions: the corner cases
     [update_to_level] and [synchronize_round] and
     the hack used by [inject_block])

   - new_proposal's received blocks are expected to belong to our current
     round

   - [Prequorum_reached] can only be received when we've seen a new head

   - [Quorum_reached] can only be received when we've seen a
     [Prequorum_reached] *)
let step (state : Baking_state.t) (event : Baking_state.event) :
    (Baking_state.t * Baking_actions.t) Lwt.t =
  let phase = state.round_state.current_phase in
  Events.(emit step_current_phase (phase, event)) >>= fun () ->
  match (phase, event) with
  (* Handle timeouts *)
  | _, Timeout (End_of_round {ending_round}) ->
      (* If the round is ending, stop everything currently going on and
         increment the round. *)
      end_of_round state ending_round
  | _, Timeout (Time_to_bake_next_level {at_round}) ->
      (* If it is time to bake the next level, stop everything currently
         going on and propose the next level block *)
      time_to_bake_at_next_level state at_round
  | Idle, New_head_proposal proposal ->
      Events.(
        emit
          new_head
          (proposal.block.hash, proposal.block.shell.level, proposal.block.round))
      >>= fun () -> handle_proposal ~is_proposal_applied:true state proposal
  | Awaiting_application, New_head_proposal proposal ->
      if
        Block_hash.(
          state.level_state.latest_proposal.block.hash <> proposal.block.hash)
      then
        Events.(
          emit
            new_head
            ( proposal.block.hash,
              proposal.block.shell.level,
              proposal.block.round ))
        >>= fun () ->
        Events.(emit unexpected_new_head_while_waiting_for_application ())
        >>= fun () -> handle_proposal ~is_proposal_applied:true state proposal
      else
        Events.(emit applied_expected_proposal_received proposal.block.hash)
        >>= fun () -> handle_expected_applied_proposal state
  | Awaiting_endorsements, New_head_proposal proposal
  | Awaiting_preendorsements, New_head_proposal proposal ->
      Events.(
        emit
          new_head
          (proposal.block.hash, proposal.block.shell.level, proposal.block.round))
      >>= fun () ->
      Events.(emit new_head_while_waiting_for_qc ()) >>= fun () ->
      handle_proposal ~is_proposal_applied:true state proposal
  | Idle, New_valid_proposal proposal ->
      Events.(
        emit
          new_valid_proposal
          (proposal.block.hash, proposal.block.shell.level, proposal.block.round))
      >>= fun () -> handle_proposal ~is_proposal_applied:false state proposal
  | Awaiting_application, New_valid_proposal proposal
  | Awaiting_endorsements, New_valid_proposal proposal
  | Awaiting_preendorsements, New_valid_proposal proposal ->
      Events.(
        emit
          new_valid_proposal
          (proposal.block.hash, proposal.block.shell.level, proposal.block.round))
      >>= fun () ->
      if has_already_been_handled state proposal then
        Events.(emit valid_proposal_received_after_application ()) >>= fun () ->
        do_nothing state
      else
        Events.(emit new_valid_proposal_while_waiting_for_qc ()) >>= fun () ->
        handle_proposal ~is_proposal_applied:false state proposal
  | Awaiting_application, Prequorum_reached (candidate, preendorsement_qc) ->
      may_register_early_prequorum state (candidate, preendorsement_qc)
  | Awaiting_preendorsements, Prequorum_reached (candidate, preendorsement_qc)
    ->
      prequorum_reached_when_awaiting_preendorsements
        state
        candidate
        preendorsement_qc
  | Awaiting_endorsements, Quorum_reached (candidate, endorsement_qc) ->
      quorum_reached_when_waiting_endorsements state candidate endorsement_qc
  (* Unreachable cases *)
  | Idle, (Prequorum_reached _ | Quorum_reached _)
  | Awaiting_preendorsements, Quorum_reached _
  | Awaiting_endorsements, Prequorum_reached _
  | Awaiting_application, Quorum_reached _ ->
      (* This cannot/should not happen *)
      do_nothing state
