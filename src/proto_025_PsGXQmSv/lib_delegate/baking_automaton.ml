(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context
module Events = Baking_events.Scheduling
open Baking_state
open Baking_state_types

module Profiler = struct
  include (val Profiler.wrap Baking_profiler.baker_profiler)

  let[@warning "-32"] reset_block_section =
    (* The section_maker must be created here and not inside the pattern
       matching because it instantiates a reference that needs to live the
       whole lifetime of the profiler and will be used to test if the
       section should be closed and re-opened or not. *)
    let section =
      Tezos_profiler.Profiler.section_maker
        ~verbosity:Notice
        ( = )
        Block_hash.to_b58check
        Baking_profiler.baker_profiler
    in
    function
    | Baking_state.New_head_proposal proposal, metadata
    | Baking_state.New_valid_proposal proposal, metadata ->
        section (proposal.block.hash, metadata)
    | _ -> ()
end

type loop_state = {
  heads_stream : proposal Lwt_stream.t;
  on_head_proposal_callback : Protocol_client_context.full -> proposal -> unit;
  get_valid_blocks_stream : proposal Lwt_stream.t Lwt.t;
  qc_stream : Operation_worker.event Lwt_stream.t;
  forge_event_stream : forge_event Lwt_stream.t;
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
  mutable last_forge_event :
    [`New_forge_event of forge_event option] Lwt.t option;
}

type events =
  [ `New_future_head of proposal
  | `New_future_valid_proposal of proposal
  | `New_valid_proposal of proposal option
  | `New_head_proposal of proposal option
  | `QC_reached of Operation_worker.event option
  | `New_forge_event of forge_event option
  | `Termination
  | `Timeout of timeout_kind ]
  Lwt.t

let create_loop_state ?get_valid_blocks_stream ~heads_stream ~forge_event_stream
    ~on_head_proposal_callback operation_worker =
  let future_block_stream, push_future_block = Lwt_stream.create () in
  let get_valid_blocks_stream =
    match get_valid_blocks_stream with
    | None -> Lwt.return (Lwt_stream.create () |> fst)
    | Some vbs_t -> vbs_t
  in
  {
    heads_stream;
    on_head_proposal_callback;
    get_valid_blocks_stream;
    qc_stream = Operation_worker.get_quorum_event_stream operation_worker;
    forge_event_stream;
    future_block_stream;
    push_future_block = (fun x -> push_future_block (Some x));
    last_get_head_event = None;
    last_get_valid_block_event = None;
    last_future_block_event = None;
    last_get_qc_event = None;
    last_forge_event = None;
  }

let sleep_until_ptime ptime =
  let delay = Ptime.diff ptime (Time.System.now ()) in
  if Ptime.Span.compare delay Ptime.Span.zero < 0 then None
  else Some (Lwt_unix.sleep (Ptime.Span.to_float_s delay))

(** The function is blocking until it is [time]. *)
let sleep_until time =
  (* Sleeping is a system op, baking is a protocol op, this is where we convert *)
  let time = Time.System.of_protocol_exn time in
  sleep_until_ptime time

(* Only allocate once the termination promise *)
let terminated =
  let open Lwt_syntax in
  let+ _ = Lwt_exit.clean_up_starts in
  `Termination

let rec wait_next_event ~timeout ~cctxt loop_state =
  let open Lwt_result_syntax in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7388
     should we prioritize head events/timeouts to resynchronize if needs be ? *)
  let get_head_event () =
    (* n.b. we should also consume the available elements in the
       block_stream before starting baking. *)
    match loop_state.last_get_head_event with
    | None ->
        let t =
          let*! e = Lwt_stream.get loop_state.heads_stream in
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
          let*! valid_blocks_stream = loop_state.get_valid_blocks_stream in
          let*! e = Lwt_stream.get valid_blocks_stream in
          Lwt.return (`New_valid_proposal e)
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
          let*! future_proposal =
            Lwt_stream.get loop_state.future_block_stream
          in
          Lwt.return
          @@
          match future_proposal with
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
          let*! e = Lwt_stream.get loop_state.qc_stream in
          Lwt.return (`QC_reached e)
        in
        loop_state.last_get_qc_event <- Some t ;
        t
    | Some t -> t
  in
  let get_forge_event () =
    match loop_state.last_forge_event with
    | None ->
        let t =
          let*! e = Lwt_stream.get loop_state.forge_event_stream in
          Lwt.return (`New_forge_event e)
        in
        loop_state.last_forge_event <- Some t ;
        t
    | Some t -> t
  in
  (* event construction *)
  let open Baking_state in
  let*! result =
    Lwt.choose
      [
        terminated;
        (get_head_event () :> events);
        (get_valid_block_event () :> events);
        (get_future_block_event () :> events);
        (get_qc_event () :> events);
        (get_forge_event () :> events);
        (timeout :> events);
      ]
  in
  match result with
  (* event matching *)
  | `Termination ->
      (* Exit the loop *)
      return_none
  | `New_valid_proposal None ->
      (* Node connection lost *)
      loop_state.last_get_valid_block_event <- None ;
      tzfail Baking_errors.Node_connection_lost
  | `New_head_proposal None ->
      (* Node connection lost *)
      loop_state.last_get_head_event <- None ;
      tzfail Baking_errors.Node_connection_lost
  | `QC_reached (Some Operation_worker.Shutdown) ->
      (* Operation worker has closed the quorum stream, indicating a fatal error
         (e.g., node unreachable). In multi-node setups, fail the automaton so
         the supervisor can restart it. In single-node setups, this should not
         happen as the worker uses Lwt_exit.exit_and_raise. *)
      loop_state.last_get_qc_event <- None ;
      tzfail Baking_errors.Node_connection_lost
  | `QC_reached None ->
      (* Not supposed to happen: exit the loop *)
      loop_state.last_get_qc_event <- None ;
      return_none
  | `New_forge_event None ->
      (* Not supposed to happen: exit the loop *)
      loop_state.last_forge_event <- None ;
      return_none
  | `New_valid_proposal (Some proposal) -> (
      loop_state.last_get_valid_block_event <- None ;
      (* Is the block in the future? *)
      match sleep_until proposal.block.shell.timestamp with
      | Some waiter ->
          (* If so, wait until its timestamp is reached before advertising it *)
          let*! () = Events.(emit proposal_in_the_future proposal.block.hash) in
          Lwt.dont_wait
            (fun () ->
              let*! () = waiter in
              loop_state.push_future_block (`New_future_valid_proposal proposal) ;
              Lwt.return_unit)
            (fun _exn -> ()) ;
          wait_next_event ~timeout ~cctxt loop_state
      | None -> return_some (New_valid_proposal proposal))
  | `New_head_proposal (Some proposal) -> (
      loop_state.last_get_head_event <- None ;
      loop_state.on_head_proposal_callback cctxt proposal ;
      (* Is the block in the future? *)
      match sleep_until proposal.block.shell.timestamp with
      | Some waiter ->
          (* If so, wait until its timestamp is reached before advertising it *)
          let*! () = Events.(emit proposal_in_the_future proposal.block.hash) in
          Lwt.dont_wait
            (fun () ->
              let*! () = waiter in
              loop_state.push_future_block (`New_future_head proposal) ;
              Lwt.return_unit)
            (fun _exn -> ()) ;
          wait_next_event ~timeout ~cctxt loop_state
      | None -> return_some (New_head_proposal proposal))
  | `New_future_head proposal ->
      let*! () =
        Events.(emit process_proposal_in_the_future proposal.block.hash)
      in
      loop_state.last_future_block_event <- None ;
      return_some (New_head_proposal proposal)
  | `New_future_valid_proposal proposal ->
      let*! () =
        Events.(emit process_proposal_in_the_future proposal.block.hash)
      in
      loop_state.last_future_block_event <- None ;
      return_some (New_valid_proposal proposal)
  | `QC_reached
      (Some (Operation_worker.Prequorum_reached (candidate, preattestation_qc)))
    ->
      loop_state.last_get_qc_event <- None ;
      return_some (Prequorum_reached (candidate, preattestation_qc))
  | `QC_reached
      (Some (Operation_worker.Quorum_reached (candidate, attestation_qc))) ->
      loop_state.last_get_qc_event <- None ;
      return_some (Quorum_reached (candidate, attestation_qc))
  | `New_forge_event (Some event) ->
      loop_state.last_forge_event <- None ;
      return_some (New_forge_event event)
  | `Timeout e -> return_some (Timeout e)

let first_potential_round ~committee_size ~owned_slots ~earliest_round =
  let open Option_syntax in
  let ( let*? ) res f = match res with Error _ -> None | Ok x -> f x in
  (* Rounds attribution cycles with a period of [committee_size].
     To find the next owned round, we translate [earliest_round] into a slot
     within the range [0 ... committee_size], look for the first subsequent slot
     we own, and finally translate it back to a round by restoring the original
     offset. *)
  let*? earliest_round_int = Round.to_int earliest_round in
  let period_offset = earliest_round_int / committee_size in
  let*? round_rem = Round.of_int (earliest_round_int mod committee_size) in
  match Delegate_infos.find_first_round_from owned_slots ~round:round_rem with
  | Some (round, delegate) ->
      let*? round = Round.to_int round in
      let*? round = Round.of_int (round + (committee_size * period_offset)) in
      Some (round, delegate.delegate)
  | None ->
      let* round, delegate = Delegate_infos.min_round owned_slots in
      let*? round = Round.to_int round in
      let*? round =
        Round.of_int (round + (committee_size * (1 + period_offset)))
      in
      Some (round, delegate.delegate)

let first_potential_round_at_next_level state ~earliest_round =
  let committee_size =
    state.global_state.constants.Constants.parametric.consensus_committee_size
  in
  let owned_slots = state.level_state.next_level_delegate_infos in
  first_potential_round ~committee_size ~owned_slots ~earliest_round

let first_potential_round_at_current_level state ~earliest_round =
  let committee_size =
    state.global_state.constants.Constants.parametric.consensus_committee_size
  in
  let owned_slots = state.level_state.delegate_infos in
  first_potential_round ~committee_size ~owned_slots ~earliest_round

(** [current_round_at_next_level] converts the current system timestamp
    into the first non-expired round at the next level *)
let current_round_at_next_level ~round_durations ~predecessor_timestamp
    ~predecessor_round =
  let open Result_syntax in
  let now = Time.System.now () |> Time.System.to_protocol in
  let* round_zero =
    Environment.wrap_tzresult
    @@ Round.timestamp_of_round
         round_durations
         ~predecessor_timestamp
         ~predecessor_round
         ~round:Round.zero
  in
  if Time.Protocol.(now < round_zero) then return Round.zero
  else
    Environment.wrap_tzresult
    @@ Round.round_of_timestamp
         round_durations
         ~predecessor_timestamp
         ~predecessor_round
         ~timestamp:now

(** [compute_next_potential_baking_time state] From the current [state], the
    function returns an optional association pair, which consists of the next
    baking timestamp and its baking round. In that case, an elected block must
    exist. *)
let compute_next_potential_baking_time_at_next_level state =
  let open Lwt_option_syntax in
  let open Protocol.Alpha_context in
  let open Baking_state in
  match state.level_state.elected_block with
  | None -> Lwt.return_none
  | Some elected_block ->
      let*! () =
        Events.(
          emit
            compute_next_timeout_elected_block
            ( elected_block.proposal.block.shell.level,
              elected_block.proposal.block.round ))
      in
      let predecessor_timestamp =
        elected_block.proposal.block.shell.timestamp
      in
      let predecessor_round = elected_block.proposal.block.round in
      let round_durations = state.global_state.round_durations in
      (* Converts the current system timestamp into the corresponding round at
         the next level. *)
      let*? current_round_at_next_level =
        current_round_at_next_level
          ~round_durations
          ~predecessor_timestamp
          ~predecessor_round
        |> Result.to_option
      in
      (* Compute the maximum of [current_round_at_next_level] and
         [next_level_latest_forge_request + 1]. It is the smallest round at next
         level that is not expired and not already sent to the forge worker. *)
      let earliest_round =
        match state.level_state.next_level_latest_forge_request with
        | None -> current_round_at_next_level
        | Some latest_forged_round ->
            Round.(max current_round_at_next_level (succ latest_forged_round))
      in
      (* Find the first baking round we own at next level that is greater or
         equal than [earliest_round] *)
      let*? first_potential_round, delegate =
        first_potential_round_at_next_level state ~earliest_round
      in
      let*? first_potential_baking_time =
        timestamp_of_round
          state
          ~predecessor_timestamp
          ~predecessor_round
          ~round:first_potential_round
        |> Result.to_option
      in
      let*! () =
        Events.(
          emit
            next_potential_slot
            ( Int32.succ state.level_state.current_level,
              first_potential_round,
              first_potential_baking_time,
              delegate ))
      in
      return (first_potential_baking_time, first_potential_round)

(** From the current [state], the function returns an Lwt promise that
    fulfills once the nearest timeout is expired and at which the state
    machine will react.

    Both subfunctions [wait_baking_time] and [wait_end_of_round] are
    using the blocking function
    [sleep_until]. However, this call is binded into
    a Lwt promise. Hence, it just won't get fulfilled until sleep time
    has elapsed. Once the promise is fulfilled,
    [wait_next_event] handles with [Lwt.choose] to
    react and trigger event [Timeout]. *)
let compute_next_timeout state : Baking_state.timeout_kind Lwt.t tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7389
     this function (may) try to instantly repropose a block *)
  let open Baking_state in
  let wait_end_of_round ?(delta = 0L) (next_round_time, next_round) =
    let next_time = Time.Protocol.add next_round_time delta in
    let now = Time.System.now () in
    let delay = Ptime.diff (Time.System.of_protocol_exn next_time) now in
    let current_round = Int32.pred @@ Round.to_int32 next_round in
    let*! () =
      if delta = 0L then
        Events.(emit waiting_end_of_round (delay, current_round, next_time))
      else
        Events.(
          emit
            waiting_delayed_end_of_round
            (delay, current_round, next_time, delta))
    in
    let end_of_round =
      Lwt.return
      @@ End_of_round {ending_round = state.round_state.current_round}
    in
    match sleep_until next_time with
    | None -> return end_of_round
    | Some t ->
        return
          (let*! () = t in
           end_of_round)
  in
  let wait_baking_time_next_level (next_baking_time, next_baking_round) =
    let now = Time.System.now () in
    let delay = Ptime.diff (Time.System.of_protocol_exn next_baking_time) now in
    match sleep_until next_baking_time with
    | None ->
        let*! () = Events.(emit no_need_to_wait_for_proposal ()) in
        return
          (Lwt.return
             (Time_to_prepare_next_level_block {at_round = next_baking_round}))
    | Some t ->
        let*! () =
          Events.(emit waiting_time_to_bake (delay, next_baking_time))
        in
        return
          (let*! () = t in
           Lwt.return
             (Time_to_prepare_next_level_block {at_round = next_baking_round}))
  in
  let delay_next_round_timeout next_round =
    (* we only delay if it's our turn to bake *)
    match round_proposer state ~level:`Current (snd next_round) with
    | Some _ ->
        let delta =
          max
            1L
            ( state.global_state.constants.parametric.minimal_block_delay
              |> Period.to_seconds
            |> fun d -> Int64.div d 5L )
        in
        (* NB: this means 6 seconds delay, if the first round duration is
           30. *)
        wait_end_of_round ~delta next_round
    | None -> wait_end_of_round next_round
  in
  let should_wait_to_forge_block (_next_baking_time, next_baking_round) =
    Option.is_some state.level_state.elected_block
    && Round.equal next_baking_round Round.zero
  in
  let waiting_to_forge_block (next_baking_time, next_baking_round) =
    let*! () = Events.(emit first_baker_of_next_level ()) in
    let now = Time.System.now () in
    let next_baking_ptime = Time.System.of_protocol_exn next_baking_time in
    let pre_emptive_forge_time =
      state.global_state.config.pre_emptive_forge_time
    in
    let next_forging_ptime =
      match Ptime.sub_span next_baking_ptime pre_emptive_forge_time with
      | Some ptime -> ptime
      | None ->
          (* This branch can only be reached if the Ptime operations above
             fail. In practice, it should be unreachable. *)
          assert false
    in
    let delay = Ptime.diff next_forging_ptime now in
    match sleep_until_ptime next_forging_ptime with
    | None ->
        let*! () = Events.(emit no_need_to_wait_to_forge_block ()) in
        return
          (Lwt.return
             (Time_to_prepare_next_level_block {at_round = next_baking_round}))
    | Some t ->
        let*! () =
          Events.(
            emit
              waiting_to_forge_block
              (delay, Time.System.to_protocol next_forging_ptime))
        in
        return
          (let*! () = t in
           Lwt.return
             (Time_to_prepare_next_level_block {at_round = next_baking_round}))
  in
  (* Compute the timestamp of the next possible round. *)
  let next_round = compute_next_round_time state in
  let*! next_baking = compute_next_potential_baking_time_at_next_level state in
  match (next_round, next_baking) with
  | None, None ->
      let*! () = Events.(emit waiting_for_new_head ()) in
      return
        (let*! () = Lwt_utils.never_ending () in
         assert false)
  (* We have no slot at the next level in the near future, we will
     patiently wait for the next round. *)
  | Some next_round, None -> (
      (* If there is an elected block, then we make the assumption
         that the bakers at the next level have also received an
         attestation quorum, and we delay a bit injecting at the next
         round, so that there are not two blocks injected at the same
         time. *)
      match state.level_state.elected_block with
      | None -> wait_end_of_round next_round
      | Some _elected_block -> delay_next_round_timeout next_round)
  (* There is no timestamp for a successor round but there is for a
     future baking slot. If we are the next level baker at round 0,
     quorum has been reached for this level, and no block being forged,
     we will wait to forge, otherwise we will wait to bake *)
  | None, Some next_baking ->
      if should_wait_to_forge_block next_baking then
        waiting_to_forge_block next_baking
      else wait_baking_time_next_level next_baking
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
      then
        if should_wait_to_forge_block next_baking then
          waiting_to_forge_block next_baking
        else wait_baking_time_next_level next_baking
      else
        (* same observation is in the [(Some next_round, None)] case *)
        delay_next_round_timeout next_round_info

let compute_bootstrap_event state =
  let open Result_syntax in
  let open Baking_state in
  (* Check if we are in the current round *)
  if
    Round.(
      state.level_state.latest_proposal.block.round
      = state.round_state.current_round)
  then
    (* If so, then trigger the new proposal event to possibly preattest *)
    return @@ Baking_state.New_head_proposal state.level_state.latest_proposal
  else
    (* Otherwise, trigger the end of round to check whether we
       need to propose at this level or not *)
    let* ending_round =
      Environment.wrap_tzresult @@ Round.pred state.round_state.current_round
    in
    return @@ Baking_state.Timeout (End_of_round {ending_round})

let rec automaton_loop ?(stop_on_event = fun _ -> false) ~config ~on_error
    loop_state state event =
  let open Lwt_result_syntax in
  let state_recorder ~new_state =
    match config.Baking_configuration.state_recorder with
    | Baking_configuration.Filesystem ->
        Baking_state.may_record_new_state ~previous_state:state ~new_state
    | Baking_configuration.Memory -> return_unit
  in
  () [@profiler.overwrite Profiler.reset_block_section (event, [])] ;
  (let*! state', action =
     (State_transitions.step
        state
        event
      [@profiler.record_s
        {verbosity = Notice} (Format.asprintf "Step : %a" pp_short_event event)])
   in
   let* state'' =
     let*! state_res =
       let* state'' =
         (Baking_actions.perform_action
            state'
            action
          [@profiler.record_s
            {verbosity = Notice}
              (Format.asprintf "Action : %a" Baking_actions.pp_action action)])
       in
       let* () =
         may_record_new_state ~previous_state:state ~new_state:state''
       in
       return state''
     in
     match state_res with
     | Ok state'' -> return state''
     | Error error ->
         let* () = on_error error in
         (* Still try to record the intermediate state; ignore potential
            errors. *)
         let*! _ = state_recorder ~new_state:state' in
         return state'
   in
   let* next_timeout =
     (compute_next_timeout
        state''
      [@profiler.record_s {verbosity = Notice} "Timeout : compute next timeout"])
   in
   let* event_opt =
     (wait_next_event
        ~timeout:
          (let*! e = next_timeout in
           Lwt.return (`Timeout e))
        ~cctxt:state.automaton_state.cctxt
        loop_state [@profiler.record_s {verbosity = Notice} "Wait : next event"])
   in
   () [@profiler.stop] ;
   match event_opt with
   | None ->
       (* Termination *)
       return_none
   | Some event ->
       if stop_on_event event then return_some event
       else
         automaton_loop
           ~stop_on_event
           ~config
           ~on_error
           loop_state
           state''
           event)
  [@profiler.record_s {verbosity = Notice} "Scheduler loop"]

let retry ?is_error ?emit (cctxt : #Protocol_client_context.full) ?max_delay
    ~delay ~factor ?tries ?(msg = fun _errs -> "Connection failed. ") f x =
  let is_error =
    Option.value
      ~default:(function
        | RPC_client_errors.Request_failed {error = Connection_failed _; _} ->
            true
        | _ -> false)
      is_error
  in
  let emit = Option.value ~default:(cctxt#message "%s") emit in
  Utils.retry ~emit ?max_delay ~delay ~factor ?tries ~msg ~is_error f x

(* This function attempts to resolve the primary delegate associated with the given [key].

   - If the given key is a primary baking key, the function returns it directly.
   - If the given key is a consensus key, it searches for corresponding attestation rights
     over the past 50 levels (HEAD, HEAD~1, ..., HEAD~50). If attestation rights are found,
     it retrieves and returns the associated primary delegate.
   - The function follows a best-effort approach: if the key has no attestation rights yet,
     no rights within the last 50 levels, or is neither a registered delegate nor a consensus key,
     it simply returns the PKH of the given key. *)
let try_resolve_consensus_keys cctxt key =
  let open Lwt_syntax in
  let levels_to_inspect = 50 in
  let pkh = Key_id.to_pkh key.Key.id in
  let* res =
    Node_rpc.delegate_deactivated cctxt ~chain:`Main ~block:(`Head 0) pkh
  in
  match res with
  | Ok _deactivated ->
      (* [pkh] is actually the primary registered key as delegate. *)
      return pkh
  | Error _ ->
      (* [pkh] is probably a consensus key. Try resolving it. *)
      let rec try_find_delegate_key head_offset =
        if head_offset <= 0 then
          (* Went trough the loop and didn't find any assigned slot for the pkh.
             return the given pkh. *)
          return pkh
        else
          let* attesting_rights =
            Node_rpc.get_validators
              cctxt
              ~chain:`Main
              ~block:(`Head head_offset)
              ~consensus_keys:[pkh]
              ()
          in
          match attesting_rights with
          | Error _ | Ok [Plugin.RPC.Validators.{delegates = []; _}] | Ok [] ->
              try_find_delegate_key (head_offset - 1)
          | Ok
              Plugin.RPC.Validators.
                [
                  {
                    delegates =
                      {
                        delegate;
                        consensus_key = _;
                        companion_key = _;
                        rounds = _;
                        attesting_power = _;
                        attestation_slot = _;
                      }
                      :: _;
                    _;
                  };
                ] ->
              (* The primary registered key as delegate found. Return it. *)
              return delegate
          | Ok (_ :: _ :: _) -> assert false
        (* we query only one level, the returned list length must be 1 *)
      in
      try_find_delegate_key levels_to_inspect

let register_dal_profiles cctxt dal_node_rpc_ctxt dal_attestable_slots_worker
    delegates =
  let open Lwt_result_syntax in
  let*! delegates = List.map_s (try_resolve_consensus_keys cctxt) delegates in
  let register dal_ctxt =
    let* profiles = Node_rpc.get_dal_profiles dal_ctxt in
    let warn =
      Events.emit Baking_events.Scheduling.dal_node_no_attester_profile
    in
    let*! () =
      match profiles with
      | Tezos_dal_node_services.Types.Bootstrap -> warn ()
      | Controller controller_profile ->
          let attesters =
            Tezos_dal_node_services.Controller_profiles.attesters
              controller_profile
          in
          if Tezos_crypto.Signature.Public_key_hash.Set.is_empty attesters then
            warn ()
          else Lwt.return_unit
    in
    let* () = Node_rpc.register_dal_profiles dal_ctxt delegates in
    let delegate_ids = List.map Delegate_id.of_pkh delegates in
    let*! () =
      (* This is the earliest moment we know the final attesters and we have a live DAL RPC. *)
      Dal_attestable_slots_worker.update_streams_subscriptions
        dal_attestable_slots_worker
        dal_ctxt
        ~delegate_ids
    in
    (* Emit event after successful registration and subscription *)
    let*! () =
      let dal_endpoint = dal_ctxt#base |> Uri.to_string in
      let automaton_name =
        Dal_attestable_slots_worker.get_automaton_name
          dal_attestable_slots_worker
      in
      Dal_attestable_slots_worker.emit_dal_worker_started
        automaton_name
        dal_endpoint
    in
    return_unit
  in
  Option.iter_es
    (fun dal_ctxt ->
      retry
        cctxt
        ~max_delay:2.
        ~delay:1.
        ~factor:2.
        ~msg:(fun _errs ->
          "Failed to register profiles, DAL node is not reachable. ")
        (fun () -> register dal_ctxt)
        ())
    dal_node_rpc_ctxt

let create_automaton_state ?canceler ?monitor_node_operations ~multi_node_setup
    ~global_state cctxt =
  let open Lwt_result_syntax in
  let name = Uri.to_string cctxt#base in
  (* Create DAL worker for this automaton *)
  let dal_attestable_slots_worker =
    Dal_attestable_slots_worker.create
      ~automaton_name:name
      ~attestation_lag:global_state.constants.parametric.dal.attestation_lag
      ~attestation_lags:global_state.constants.parametric.dal.attestation_lags
      ~number_of_slots:global_state.constants.parametric.dal.number_of_slots
  in
  (* Register DAL profiles and subscribe to attestable slots *)
  let _promise =
    register_dal_profiles
      cctxt
      global_state.dal_node_rpc_ctxt
      dal_attestable_slots_worker
      global_state.delegates
  in
  let*! operation_worker =
    Operation_worker.run
      ?monitor_node_operations
      ~multi_node_setup
      ~round_durations:global_state.round_durations
      cctxt
  in
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          let*! () =
            Dal_attestable_slots_worker.shutdown_worker
              dal_attestable_slots_worker
          in
          let*! _ = Operation_worker.shutdown_worker operation_worker in
          Lwt.return_unit))
    canceler ;
  let* validation_mode =
    Baking_state.(
      match global_state.config.Baking_configuration.validation with
      | Node -> return Node
      | Local {data_dir} ->
          let* index = Baking_simulator.load_context ~data_dir in
          return (Local index)
      | ContextIndex index -> return (Local index))
  in
  let forge_event_stream, push_forge_event =
    let stream, push = Lwt_stream.create () in
    (stream, fun e -> push (Some e))
  in
  return
    ({
       name;
       cctxt;
       validation_mode;
       operation_worker;
       dal_attestable_slots_worker;
       forge_event_stream;
       push_forge_event;
     }
      : Baking_state.automaton_state)
