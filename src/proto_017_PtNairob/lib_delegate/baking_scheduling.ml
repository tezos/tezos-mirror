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

open Protocol.Alpha_context
module Events = Baking_events.Scheduling
open Baking_state

type loop_state = {
  heads_stream : Baking_state.proposal Lwt_stream.t;
  get_valid_blocks_stream : Baking_state.proposal Lwt_stream.t Lwt.t;
  qc_stream : Operation_worker.event Lwt_stream.t;
  future_block_stream :
    [`New_future_head of proposal | `New_future_valid_proposal of proposal]
    Lwt_stream.t;
  push_future_block :
    [`New_future_head of proposal | `New_future_valid_proposal of proposal] ->
    unit;
  mutable last_get_head_event :
    [`New_head_proposal of proposal option] Lwt.t option;
  mutable last_get_valid_block_event :
    [`New_valid_proposal of proposal option] Lwt.t option;
  mutable last_future_block_event :
    [`New_future_head of proposal | `New_future_valid_proposal of proposal]
    Lwt.t
    option;
  mutable last_get_qc_event :
    [`QC_reached of Operation_worker.event option] Lwt.t option;
}

type events =
  [ `New_future_head of proposal
  | `New_future_valid_proposal of proposal
  | `New_valid_proposal of proposal option
  | `New_head_proposal of proposal option
  | `QC_reached of Operation_worker.event option
  | `Termination
  | `Timeout of timeout_kind ]
  Lwt.t

let create_loop_state ?get_valid_blocks_stream ~heads_stream operation_worker =
  let future_block_stream, push_future_block = Lwt_stream.create () in
  let get_valid_blocks_stream =
    match get_valid_blocks_stream with
    | None -> Lwt.return (Lwt_stream.create () |> fst)
    | Some vbs_t -> vbs_t
  in
  {
    heads_stream;
    get_valid_blocks_stream;
    qc_stream = Operation_worker.get_quorum_event_stream operation_worker;
    future_block_stream;
    push_future_block = (fun x -> push_future_block (Some x));
    last_get_head_event = None;
    last_get_valid_block_event = None;
    last_future_block_event = None;
    last_get_qc_event = None;
  }

let find_in_known_round_intervals known_round_intervals ~predecessor_timestamp
    ~predecessor_round ~now =
  let open Baking_cache in
  Round_timestamp_interval_cache.(
    find_opt
      known_round_intervals
      {predecessor_timestamp; predecessor_round; time_interval = (now, now)})

(** Memoization wrapper for [Round.timestamp_of_round]. *)
let timestamp_of_round state ~predecessor_timestamp ~predecessor_round ~round =
  let open Baking_cache in
  let known_timestamps = state.global_state.cache.known_timestamps in
  match
    Timestamp_of_round_cache.find_opt
      known_timestamps
      (predecessor_timestamp, predecessor_round, round)
  with
  (* Compute and register the timestamp if not already existing. *)
  | None ->
      Protocol.Alpha_context.Round.timestamp_of_round
        state.global_state.round_durations
        ~predecessor_timestamp
        ~predecessor_round
        ~round
      >>? fun ts ->
      Timestamp_of_round_cache.replace
        known_timestamps
        (predecessor_timestamp, predecessor_round, round)
        ts ;
      ok ts
  (* If it already exists, just fetch from the memoization table. *)
  | Some ts -> ok ts

(** The function is blocking until it is [time]. *)
let sleep_until time =
  (* Sleeping is a system op, baking is a protocol op, this is where we convert *)
  let time = Time.System.of_protocol_exn time in
  let delay = Ptime.diff time (Time.System.now ()) in
  if Ptime.Span.compare delay Ptime.Span.zero < 0 then None
  else Some (Lwt_unix.sleep (Ptime.Span.to_float_s delay))

(* Only allocate once the termination promise *)
let terminated = Lwt_exit.clean_up_starts >|= fun _ -> `Termination

let rec wait_next_event ~timeout loop_state =
  (* TODO? should we prioritize head events/timeouts to resynchronize if needs be ? *)
  let get_head_event () =
    (* n.b. we should also consume the available elements in the
       block_stream before starting baking. *)
    match loop_state.last_get_head_event with
    | None ->
        let t =
          Lwt_stream.get loop_state.heads_stream >>= fun e ->
          Lwt.return (`New_head_proposal e)
        in
        loop_state.last_get_head_event <- Some t ;
        t
    | Some t -> t
  in
  let get_valid_block_event () =
    match loop_state.last_get_valid_block_event with
    | None ->
        let t =
          loop_state.get_valid_blocks_stream >>= fun valid_blocks_stream ->
          Lwt_stream.get valid_blocks_stream >|= fun e -> `New_valid_proposal e
        in
        loop_state.last_get_valid_block_event <- Some t ;
        t
    | Some t -> t
  in
  let get_future_block_event () =
    (* n.b. we should also consume the available elements in the
       block_stream before starting baking. *)
    match loop_state.last_future_block_event with
    | None ->
        let t =
          Lwt_stream.get loop_state.future_block_stream >|= function
          | None ->
              (* unreachable, we never close the stream *)
              assert false
          | Some future_proposal -> future_proposal
        in
        loop_state.last_future_block_event <- Some t ;
        t
    | Some t -> t
  in
  let get_qc_event () =
    match loop_state.last_get_qc_event with
    | None ->
        let t =
          Lwt_stream.get loop_state.qc_stream >|= fun e -> `QC_reached e
        in
        loop_state.last_get_qc_event <- Some t ;
        t
    | Some t -> t
  in
  (* event construction *)
  let open Baking_state in
  Lwt.choose
    [
      terminated;
      (get_head_event () :> events);
      (get_valid_block_event () :> events);
      (get_future_block_event () :> events);
      (get_qc_event () :> events);
      (timeout :> events);
    ]
  >>= function
  (* event matching *)
  | `Termination ->
      (* Exit the loop *)
      return_none
  | `New_valid_proposal None ->
      (* Node connection lost *)
      loop_state.last_get_valid_block_event <- None ;
      fail Baking_errors.Node_connection_lost
  | `New_head_proposal None ->
      (* Node connection lost *)
      loop_state.last_get_head_event <- None ;
      fail Baking_errors.Node_connection_lost
  | `QC_reached None ->
      (* Not supposed to happen: exit the loop *)
      loop_state.last_get_qc_event <- None ;
      return_none
  | `New_valid_proposal (Some proposal) -> (
      loop_state.last_get_valid_block_event <- None ;
      (* Is the block in the future? *)
      match sleep_until proposal.block.shell.timestamp with
      | Some waiter ->
          (* If so, wait until its timestamp is reached before advertising it *)
          Events.(emit proposal_in_the_future proposal.block.hash) >>= fun () ->
          Lwt.dont_wait
            (fun () ->
              waiter >>= fun () ->
              loop_state.push_future_block (`New_future_valid_proposal proposal) ;
              Lwt.return_unit)
            (fun _exn -> ()) ;
          wait_next_event ~timeout loop_state
      | None -> return_some (New_valid_proposal proposal))
  | `New_head_proposal (Some proposal) -> (
      loop_state.last_get_head_event <- None ;
      (* Is the block in the future? *)
      match sleep_until proposal.block.shell.timestamp with
      | Some waiter ->
          (* If so, wait until its timestamp is reached before advertising it *)
          Events.(emit proposal_in_the_future proposal.block.hash) >>= fun () ->
          Lwt.dont_wait
            (fun () ->
              waiter >>= fun () ->
              loop_state.push_future_block (`New_future_head proposal) ;
              Lwt.return_unit)
            (fun _exn -> ()) ;
          wait_next_event ~timeout loop_state
      | None -> return_some (New_head_proposal proposal))
  | `New_future_head proposal ->
      Events.(emit process_proposal_in_the_future proposal.block.hash)
      >>= fun () ->
      loop_state.last_future_block_event <- None ;
      return_some (New_head_proposal proposal)
  | `New_future_valid_proposal proposal ->
      Events.(emit process_proposal_in_the_future proposal.block.hash)
      >>= fun () ->
      loop_state.last_future_block_event <- None ;
      return_some (New_valid_proposal proposal)
  | `QC_reached
      (Some (Operation_worker.Prequorum_reached (candidate, preendorsement_qc)))
    ->
      loop_state.last_get_qc_event <- None ;
      return_some (Prequorum_reached (candidate, preendorsement_qc))
  | `QC_reached
      (Some (Operation_worker.Quorum_reached (candidate, endorsement_qc))) ->
      loop_state.last_get_qc_event <- None ;
      return_some (Quorum_reached (candidate, endorsement_qc))
  | `Timeout e -> return_some (Timeout e)

(** From the current [state], the function returns an optional
    association pair, which consists of the next round timestamp and its
    round. *)
let compute_next_round_time state =
  let open Baking_state in
  let proposal =
    match state.level_state.endorsable_payload with
    | None -> state.level_state.latest_proposal
    | Some {proposal; _} -> proposal
  in
  if Baking_state.is_first_block_in_protocol proposal then None
  else
    match state.level_state.next_level_proposed_round with
    | Some _proposed_round ->
        (* TODO? do something, if we don't, we won't be able to
           repropose a block at next level. *)
        None
    | None -> (
        let predecessor_timestamp = proposal.predecessor.shell.timestamp in
        let predecessor_round = proposal.predecessor.round in
        let next_round = Round.succ state.round_state.current_round in
        match
          timestamp_of_round
            state
            ~predecessor_timestamp
            ~predecessor_round
            ~round:next_round
        with
        | Ok timestamp -> Some (timestamp, next_round)
        | _ -> assert false)

(** [first_potential_round_at_next_level state ~earliest_round] yields
    an optional pair of the earliest possible round (at or after
    [earliest_round]), along with the delegate having the slot to
    propose.

    In particular when the required round value is higher than the
    consensus committee size, an Euclidean division allows to
    recycle. Then, the earliest round when it exists is extracted. This
    is meant to be multiplied back again to find the round value. *)
let first_potential_round_at_next_level state ~earliest_round =
  let open Baking_state in
  let slots = state.level_state.next_level_delegate_slots.own_delegate_slots in
  let rounds =
    state.level_state.next_level_delegate_slots.all_slots_by_round
    |> Array.to_seqi
    |> Seq.fold_left
         (fun acc (round, slot) ->
           if SlotMap.mem slot slots then (round, slot) :: acc else acc)
         []
    |> List.rev
  in
  match Round.to_int earliest_round with
  | Error _ -> None
  | Ok earliest_round -> (
      let consensus_committee_size =
        state.global_state.constants.parametric.consensus_committee_size
      in
      let q = earliest_round / consensus_committee_size in
      let r = earliest_round mod consensus_committee_size in
      let first_round = List.find (fun (round, _) -> round >= r) rounds in
      match first_round with
      | None -> None
      | Some (round, slot) -> (
          SlotMap.find slot slots |> function
          | None -> None
          | Some (delegate, _) -> (
              (* TODO? check with [Node_rpc.first_proposer_round] if we also need the q+1 *)
              match Round.of_int ((q * consensus_committee_size) + round) with
              | Error _ -> None
              | Ok first_potential_round ->
                  Some (first_potential_round, delegate))))

(** From the current [state], the function returns an optional
    association pair, which consists of the next baking timestamp and
    its baking round. In that case, an elected block must exist. *)
let compute_next_potential_baking_time_at_next_level state =
  let open Protocol.Alpha_context in
  let open Baking_state in
  match state.level_state.elected_block with
  | None -> Lwt.return_none
  | Some elected_block -> (
      Events.(
        emit
          compute_next_timeout_elected_block
          ( elected_block.proposal.block.shell.level,
            elected_block.proposal.block.round ))
      >>= fun () ->
      (* Do we have baking rights for the next level ? *)
      (* Determine the round for the next level *)
      let predecessor_timestamp =
        elected_block.proposal.block.shell.timestamp
      in
      let predecessor_round = elected_block.proposal.block.round in
      let now = Time.System.now () |> Time.System.to_protocol in
      (* Lookup the next slot information if already stored in the
         memoization table [Round_timestamp_interval_tbl]. *)
      match
        find_in_known_round_intervals
          state.global_state.cache.round_timestamps
          ~predecessor_timestamp
          ~predecessor_round
          ~now
      with
      | Some (first_potential_baking_time, first_potential_round, delegate) -> (
          (* Check if we already have proposed something at next
             level *)
          match state.level_state.next_level_proposed_round with
          | Some proposed_round
            when Round.(proposed_round >= first_potential_round) ->
              Events.(emit proposal_already_injected ()) >>= fun () ->
              Lwt.return_none
          | None | Some _ ->
              Events.(
                emit
                  next_potential_slot
                  ( Int32.succ state.level_state.current_level,
                    first_potential_round,
                    first_potential_baking_time,
                    delegate ))
              >>= fun () ->
              Lwt.return_some
                (first_potential_baking_time, first_potential_round))
      | None -> (
          let round_durations = state.global_state.round_durations in
          (* Compute the timestamp at which the new level will start at
             round 0.*)
          Round.timestamp_of_round
            round_durations
            ~predecessor_timestamp
            ~predecessor_round
            ~round:Round.zero
          |> function
          | Error _ -> Lwt.return_none
          | Ok min_possible_time -> (
              (* If this timestamp exists and is not yet outdated, the
                 earliest round to bake is thereby 0. Otherwise, we
                 compute the round from the current timestamp. This
                 possibly means the baker has been late. *)
              (if Time.Protocol.(now < min_possible_time) then ok Round.zero
              else
                Environment.wrap_tzresult
                @@ Round.round_of_timestamp
                     round_durations
                     ~predecessor_timestamp
                     ~predecessor_round
                     ~timestamp:now)
              |> function
              | Error _ -> Lwt.return_none
              | Ok earliest_round -> (
                  (* There does not necessarily exists a slot that is
                     equal to [earliest_round]. We must find the earliest
                     slot after this value for which a validator is
                     designated to propose. *)
                  match
                    first_potential_round_at_next_level state ~earliest_round
                  with
                  | None -> Lwt.return_none
                  | Some (first_potential_round, delegate) -> (
                      (* Check if we already have proposed something at next
                         level. If so, we can skip. Otherwise, we recompute
                         the timestamp for the
                         [first_potential_round]. Finally, from this
                         [first_potential_baking_time], we can return. *)
                      match state.level_state.next_level_proposed_round with
                      | Some proposed_round
                        when Round.(proposed_round >= first_potential_round) ->
                          Events.(emit proposal_already_injected ())
                          >>= fun () -> Lwt.return_none
                      | None | Some _ -> (
                          timestamp_of_round
                            state
                            ~predecessor_timestamp
                            ~predecessor_round
                            ~round:first_potential_round
                          |> function
                          | Error _ -> Lwt.return_none
                          | Ok first_potential_baking_time ->
                              Events.(
                                emit
                                  next_potential_slot
                                  ( Int32.succ state.level_state.current_level,
                                    first_potential_round,
                                    first_potential_baking_time,
                                    delegate ))
                              >>= fun () ->
                              (* memoize this *)
                              let () =
                                let this_round_duration =
                                  Round.round_duration
                                    round_durations
                                    first_potential_round
                                in
                                let end_first_potential_baking_time =
                                  Timestamp.(
                                    first_potential_baking_time
                                    +? this_round_duration)
                                  |> function
                                  | Ok x -> x
                                  | Error _ -> assert false
                                in
                                Baking_cache.(
                                  Round_timestamp_interval_cache.replace
                                    state.global_state.cache.round_timestamps
                                    {
                                      predecessor_timestamp;
                                      predecessor_round;
                                      time_interval =
                                        ( first_potential_baking_time,
                                          end_first_potential_baking_time );
                                    }
                                    ( first_potential_baking_time,
                                      first_potential_round,
                                      delegate ))
                              in
                              Lwt.return_some
                                ( first_potential_baking_time,
                                  first_potential_round )))))))

(** From the current [state], the function returns an Lwt promise that
    fulfills once the nearest timeout is expired and at which the state
    machine will react.

    Both subfunctions [wait_baking_time] and [wait_end_of_round] are
    using the blocking function
    [Baking_scheduling.sleep_until]. However, this call is binded into
    a Lwt promise. Hence, it just won't get fulfilled until sleep time
    has elapsed. Once the promise is fulfilled,
    [Baking_scheduling.wait_next_event] handles with [Lwt.choose] to
    react and trigger event [Timeout]. *)
let compute_next_timeout state : Baking_state.timeout_kind Lwt.t tzresult Lwt.t
    =
  (* FIXME: this function (may) try to instantly repropose a block *)
  let open Baking_state in
  let wait_end_of_round ?(delta = 0L) (next_round_time, next_round) =
    let next_time = Time.Protocol.add next_round_time delta in
    let now = Time.System.now () in
    let delay = Ptime.diff (Time.System.of_protocol_exn next_time) now in
    let current_round = Int32.pred @@ Round.to_int32 next_round in
    (if delta = 0L then
     Events.(emit waiting_end_of_round (delay, current_round, next_time))
    else
      Events.(
        emit
          waiting_delayed_end_of_round
          (delay, current_round, next_time, delta)))
    >>= fun () ->
    let end_of_round =
      Lwt.return
      @@ End_of_round {ending_round = state.round_state.current_round}
    in
    match sleep_until next_time with
    | None -> return end_of_round
    | Some t -> return (t >>= fun () -> end_of_round)
  in
  let wait_baking_time_next_level (next_baking_time, next_baking_round) =
    let now = Time.System.now () in
    let delay = Ptime.diff (Time.System.of_protocol_exn next_baking_time) now in
    match sleep_until next_baking_time with
    | None ->
        Events.(emit no_need_to_wait_for_proposal ()) >>= fun () ->
        return
          (Lwt.return (Time_to_bake_next_level {at_round = next_baking_round}))
    | Some t ->
        Events.(emit waiting_time_to_bake (delay, next_baking_time))
        >>= fun () ->
        return
          ( t >>= fun () ->
            Lwt.return (Time_to_bake_next_level {at_round = next_baking_round})
          )
  in
  let delay_next_round_timeout next_round =
    (* we only delay if it's our turn to bake *)
    match
      State_transitions.round_proposer
        state
        state.level_state.delegate_slots.own_delegate_slots
        (snd next_round)
    with
    | Some _ ->
        let delta =
          state.global_state.constants.parametric.minimal_block_delay
          |> Period.to_seconds
          |> fun d -> Int64.div d 5L
        in
        (* NB: this means 6 seconds delay, if the first round duration is
           30. *)
        wait_end_of_round ~delta next_round
    | None -> wait_end_of_round next_round
  in
  (* TODO: re-use what has been done in round_synchronizer.ml *)
  (* Compute the timestamp of the next possible round. *)
  let next_round = compute_next_round_time state in
  compute_next_potential_baking_time_at_next_level state >>= fun next_baking ->
  match (next_round, next_baking) with
  | None, None ->
      Events.(emit waiting_for_new_head ()) >>= fun () ->
      return (Lwt_utils.never_ending () >>= fun () -> assert false)
  (* We have no slot at the next level in the near future, we will
     patiently wait for the next round. *)
  | Some next_round, None -> (
      (* If there is an elected block, then we make the assumption
         that the bakers at the next level have also received an
         endorsement quorum, and we delay a bit injecting at the next
         round, so that there are not two blocks injected at the same
         time. *)
      match state.level_state.elected_block with
      | None -> wait_end_of_round next_round
      | Some _elected_block -> delay_next_round_timeout next_round)
  (* There is no timestamp for a successor round but there is for a
     future baking slot, we will wait to bake. *)
  | None, Some next_baking -> wait_baking_time_next_level next_baking
  (* We choose the earliest timestamp between waiting to bake and
     waiting for the next round. *)
  | ( Some ((next_round_time, next_round) as next_round_info),
      Some ((next_baking_time, _) as next_baking) ) ->
      (* If we can bake at the next level before the end of the next
         round, then do so. This is because the proposed block will have
         a smaller timestamp than the earliest block at next level built
         on top of the proposal made at the next round (at the current
         level). *)
      let round_durations = state.global_state.round_durations in
      let next_round_duration =
        Round.round_duration round_durations next_round |> Period.to_seconds
      in
      if
        Time.Protocol.(
          next_baking_time < add next_round_time next_round_duration)
      then wait_baking_time_next_level next_baking
      else
        (* same observation is in the [(Some next_round, None)] case *)
        delay_next_round_timeout next_round_info

(* initialises endorsable_payload with the PQC included in the latest block
   if there is one and if it's more recent than the one loaded from disk
   if any *)
let may_initialise_with_latest_proposal_pqc state =
  let p = state.level_state.latest_proposal in
  match p.block.prequorum with
  | None -> return state
  | Some pqc -> (
      match state.level_state.endorsable_payload with
      | Some ep when ep.prequorum.round >= pqc.round ->
          (*do not change the endorsable_payload loaded from disk if it's
            more recent *)
          return state
      | Some _ | None ->
          return
            {
              state with
              level_state =
                {
                  state.level_state with
                  endorsable_payload = Some {prequorum = pqc; proposal = p};
                };
            })

let create_round_durations constants =
  let first_round_duration =
    constants.Constants.parametric.minimal_block_delay
  in
  let delay_increment_per_round =
    constants.parametric.delay_increment_per_round
  in
  Environment.wrap_tzresult
    (Round.Durations.create ~first_round_duration ~delay_increment_per_round)

let create_dal_node_rpc_ctxt endpoint =
  let open Tezos_rpc_http_client_unix in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  let media_types =
    Tezos_rpc_http.Media_type.Command_line.of_command_line rpc_config.media_type
  in
  new RPC_client_unix.http_ctxt rpc_config media_types

let create_initial_state cctxt ?(synchronize = true) ~chain config
    operation_worker ~(current_proposal : Baking_state.proposal) delegates =
  (* FIXME? consider saved endorsable value *)
  let open Protocol in
  let open Baking_state in
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  Alpha_services.Constants.all cctxt (`Hash chain_id, `Head 0)
  >>=? fun constants ->
  create_round_durations constants >>?= fun round_durations ->
  Baking_state.(
    match config.Baking_configuration.validation with
    | Node -> return Node
    | Local {context_path} ->
        Baking_simulator.load_context ~context_path >>=? fun index ->
        return (Local index)
    | ContextIndex index -> return (Local index))
  >>=? fun validation_mode ->
  let cache = Baking_state.create_cache () in
  let global_state =
    {
      cctxt;
      chain_id;
      config;
      constants;
      round_durations;
      operation_worker;
      validation_mode;
      delegates;
      cache;
      dal_node_rpc_ctxt =
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4674
           Treat case when no endpoint was given and DAL is enabled *)
        Option.map create_dal_node_rpc_ctxt config.dal_node_endpoint;
    }
  in
  let chain = `Hash chain_id in
  let current_level = current_proposal.block.shell.level in
  Baking_state.compute_delegate_slots
    cctxt
    delegates
    ~level:current_level
    ~chain
  >>=? fun delegate_slots ->
  Baking_state.compute_delegate_slots
    cctxt
    delegates
    ~level:(Int32.succ current_level)
    ~chain
  >>=? fun next_level_delegate_slots ->
  let elected_block =
    if Baking_state.is_first_block_in_protocol current_proposal then
      (* If the last block is a protocol transition, we admit it as a
         final block *)
      Some {proposal = current_proposal; endorsement_qc = []}
    else None
  in
  let level_state =
    {
      current_level = current_proposal.block.shell.level;
      latest_proposal = current_proposal;
      is_latest_proposal_applied =
        true (* this proposal is expected to be the current head *);
      locked_round = None;
      endorsable_payload = None;
      elected_block;
      delegate_slots;
      next_level_delegate_slots;
      next_level_proposed_round = None;
    }
  in
  (if synchronize then
   create_round_durations constants >>? fun round_durations ->
   Baking_actions.compute_round current_proposal round_durations
   >>? fun current_round ->
   ok {current_round; current_phase = Idle; delayed_prequorum = None}
  else
    ok
      {
        Baking_state.current_round = Round.zero;
        current_phase = Idle;
        delayed_prequorum = None;
      })
  >>?= fun round_state ->
  let state = {global_state; level_state; round_state} in
  (* Try loading locked round and endorsable round from disk *)
  Baking_state.may_load_endorsable_data state >>=? fun state ->
  may_initialise_with_latest_proposal_pqc state

let compute_bootstrap_event state =
  let open Baking_state in
  (* Check if we are in the current round *)
  if
    Round.(
      state.level_state.latest_proposal.block.round
      = state.round_state.current_round)
  then
    (* If so, then trigger the new proposal event to possibly preendorse *)
    ok @@ Baking_state.New_head_proposal state.level_state.latest_proposal
  else
    (* Otherwise, trigger the end of round to check whether we
       need to propose at this level or not *)
    Environment.wrap_tzresult @@ Round.pred state.round_state.current_round
    >>? fun ending_round ->
    ok @@ Baking_state.Timeout (End_of_round {ending_round})

let rec automaton_loop ?(stop_on_event = fun _ -> false) ~config ~on_error
    loop_state state event =
  let state_recorder ~new_state =
    match config.Baking_configuration.state_recorder with
    | Baking_configuration.Filesystem ->
        Baking_state.may_record_new_state ~previous_state:state ~new_state
    | Baking_configuration.Memory -> return_unit
  in
  State_transitions.step state event >>= fun (state', action) ->
  (Baking_actions.perform_action ~state_recorder state' action >>= function
   | Ok state'' -> return state''
   | Error error ->
       on_error error >>=? fun () ->
       (* Still try to record the intermediate state; ignore potential
          errors. *)
       state_recorder ~new_state:state' >>= fun _ -> return state')
  >>=? fun state'' ->
  compute_next_timeout state'' >>=? fun next_timeout ->
  wait_next_event ~timeout:(next_timeout >|= fun e -> `Timeout e) loop_state
  >>=? function
  | None ->
      (* Termination *)
      return_none
  | Some event ->
      if stop_on_event event then return_some event
      else
        automaton_loop ~stop_on_event ~config ~on_error loop_state state'' event

let perform_sanity_check cctxt ~chain_id =
  let open Baking_errors in
  let prefix_base_dir f = Filename.Infix.(cctxt#get_base_dir // f) in
  let nonces_location = Baking_files.resolve_location ~chain_id `Nonce in
  Baking_nonces.load cctxt nonces_location
  |> trace
       (Cannot_load_local_file
          (prefix_base_dir (Baking_files.filename nonces_location) ^ "s"))
  >>=? fun _ ->
  let highwatermarks_location =
    Baking_files.resolve_location ~chain_id `Highwatermarks
  in
  Baking_highwatermarks.load cctxt highwatermarks_location
  |> trace
       (Cannot_load_local_file
          (prefix_base_dir (Baking_files.filename highwatermarks_location) ^ "s"))
  >>=? fun _ ->
  let state_location = Baking_files.resolve_location ~chain_id `State in
  Baking_state.load_endorsable_data cctxt state_location
  |> trace
       (Cannot_load_local_file
          (prefix_base_dir (Baking_files.filename state_location)))
  >>=? fun _ -> return_unit

let run cctxt ?canceler ?(stop_on_event = fun _ -> false)
    ?(on_error = fun _ -> return_unit) ~chain config delegates =
  let open Lwt_result_syntax in
  Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
  perform_sanity_check cctxt ~chain_id >>=? fun () ->
  let cache = Baking_cache.Block_cache.create 10 in
  Node_rpc.monitor_heads cctxt ~cache ~chain ()
  >>=? fun (heads_stream, _block_stream_stopper) ->
  (Lwt_stream.get heads_stream >>= function
   | Some current_head -> return current_head
   | None -> failwith "head stream unexpectedly ended")
  >>=? fun current_proposal ->
  Operation_worker.create cctxt >>= fun operation_worker ->
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          Operation_worker.shutdown_worker operation_worker >>= fun _ ->
          Lwt.return_unit))
    canceler ;
  create_initial_state
    cctxt
    ~chain
    config
    operation_worker
    ~current_proposal
    delegates
  >>=? fun initial_state ->
  let cloned_block_stream = Lwt_stream.clone heads_stream in
  Baking_nonces.start_revelation_worker
    cctxt
    initial_state.global_state.config.nonce
    initial_state.global_state.chain_id
    initial_state.global_state.constants
    cloned_block_stream
  >>= fun revelation_worker_canceler ->
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          Lwt_canceler.cancel revelation_worker_canceler >>= fun _ ->
          Lwt.return_unit))
    canceler ;
  (* FIXME: currently, the client streamed RPC call will hold until at
     least one element is present in the stream. This is fixed by:
     https://gitlab.com/nomadic-labs/resto/-/merge_requests/50. Until
     then, we await the promise completion of the RPC call later
     on. *)
  let get_valid_blocks_stream =
    let*! vbs = Node_rpc.monitor_valid_proposals cctxt ~cache ~chain () in
    match vbs with
    | Error _ -> Stdlib.failwith "Failed to get the validated blocks stream"
    | Ok (vbs, _) -> Lwt.return vbs
  in
  let loop_state =
    create_loop_state
      ~get_valid_blocks_stream
      ~heads_stream
      initial_state.global_state.operation_worker
  in
  let on_error err =
    Events.(emit error_while_baking err) >>= fun () ->
    (* TODO? retry a bounded number of time *)
    (* let retries = config.Baking_configuration.retries_on_failure in *)
    on_error err
  in
  compute_bootstrap_event initial_state >>?= fun initial_event ->
  protect
    ~on_error:(fun err ->
      Option.iter_es Lwt_canceler.cancel canceler >>= fun _ ->
      Lwt.return_error err)
    (fun () ->
      automaton_loop
        ~stop_on_event
        ~config
        ~on_error
        loop_state
        initial_state
        initial_event
      >>=? fun _ignored_event -> return_unit)
