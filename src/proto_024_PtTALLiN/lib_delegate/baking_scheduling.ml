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
open Baking_automaton
open Baking_state
open Baking_state_types
module Events = Baking_events.Scheduling

let perform_sanity_check cctxt ~chain_id =
  let open Lwt_result_syntax in
  let open Baking_errors in
  let prefix_base_dir f = Filename.Infix.(cctxt#get_base_dir // f) in
  let stateful_location =
    Baking_files.resolve_location ~chain_id `Stateful_nonce
  in
  let* _ =
    Baking_nonces.load cctxt ~stateful_location
    |> trace
         (Cannot_load_local_file
            (prefix_base_dir (Baking_files.filename stateful_location) ^ "s"))
  in
  let highwatermarks_location =
    Baking_files.resolve_location ~chain_id `Highwatermarks
  in
  let* _ =
    Baking_highwatermarks.load cctxt highwatermarks_location
    |> trace
         (Cannot_load_local_file
            (prefix_base_dir (Baking_files.filename highwatermarks_location)
            ^ "s"))
  in
  let state_location = Baking_files.resolve_location ~chain_id `State in
  let* _ =
    Baking_state.load_attestable_data cctxt state_location
    |> trace
         (Cannot_load_local_file
            (prefix_base_dir (Baking_files.filename state_location)))
  in
  return_unit

let create_round_durations constants =
  let first_round_duration =
    constants.Constants.parametric.minimal_block_delay
  in
  let delay_increment_per_round =
    constants.parametric.delay_increment_per_round
  in
  Environment.wrap_tzresult
    (Round.Durations.create ~first_round_duration ~delay_increment_per_round)

let register_dal_profiles cctxt dal_node_rpc_ctxt delegates =
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
    Node_rpc.register_dal_profiles dal_ctxt delegates
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

(* initialises attestable_payload with the PQC included in the latest block
   if there is one and if it's more recent than the one loaded from disk
   if any *)
let may_initialise_with_latest_proposal_pqc state =
  let open Lwt_result_syntax in
  let p = state.level_state.latest_proposal in
  match p.block.prequorum with
  | None -> return state
  | Some pqc -> (
      match state.level_state.attestable_payload with
      | Some ep when ep.prequorum.round >= pqc.round ->
          (*do not change the attestable_payload loaded from disk if it's
            more recent *)
          return state
      | Some _ | None ->
          return
            {
              state with
              level_state =
                {
                  state.level_state with
                  attestable_payload = Some {prequorum = pqc; proposal = p};
                };
            })

let create_global_state ?dal_node_rpc_ctxt ?constants ~chain cctxt config
    delegates =
  let open Lwt_result_syntax in
  let* chain_id = Node_rpc.chain_id cctxt ~chain in
  let* constants =
    match constants with
    | Some c -> return c
    | None -> Node_rpc.constants cctxt ~chain:(`Hash chain_id) ~block:(`Head 0)
  in
  let*? round_durations = create_round_durations constants in
  let cache = Baking_state.create_cache () in
  let global_state =
    {
      chain_id;
      config;
      constants;
      round_durations;
      forge_worker_hooks =
        {
          push_request = (fun _ -> assert false);
          cancel_all_pending_tasks = (fun _ -> assert false);
        };
      delegates;
      cache;
      dal_node_rpc_ctxt;
    }
  in
  (* Trick to provide the global state to the forge worker without
     introducing a circular dependency. *)
  let* forge_worker = Forge_worker.start global_state in
  global_state.forge_worker_hooks <-
    {
      push_request = Forge_worker.push_request forge_worker;
      cancel_all_pending_tasks =
        (fun () -> Forge_worker.cancel_all_pending_tasks forge_worker);
    } ;
  return global_state

let create_level_state ~chain_id ~cctxt ~current_proposal ~delegates
    ~dal_node_rpc_ctxt =
  let open Lwt_result_syntax in
  let current_level = current_proposal.block.shell.level in
  let chain = `Hash chain_id in
  let* delegate_infos =
    Baking_state.compute_delegate_infos
      cctxt
      delegates
      ~level:current_level
      ~chain
  in
  let* next_level_delegate_infos =
    Baking_state.compute_delegate_infos
      cctxt
      delegates
      ~level:(Int32.succ current_level)
      ~chain
  in
  let elected_block =
    if Baking_state.is_first_block_in_protocol current_proposal then
      (* If the last block is a protocol transition, we admit it as a
         final block *)
      Some {proposal = current_proposal; attestation_qc = []}
    else None
  in
  let dal_attestable_slots =
    Option.fold
      ~none:[]
      ~some:(fun dal_node_rpc_ctxt ->
        Node_rpc.dal_attestable_slots
          dal_node_rpc_ctxt
          ~attestation_level:current_level
          (Delegate_infos.own_delegates delegate_infos))
      dal_node_rpc_ctxt
  in
  let next_level_dal_attestable_slots =
    Option.fold
      ~none:[]
      ~some:(fun dal_node_rpc_ctxt ->
        Node_rpc.dal_attestable_slots
          dal_node_rpc_ctxt
          ~attestation_level:(Int32.succ current_level)
          (Delegate_infos.own_delegates next_level_delegate_infos))
      dal_node_rpc_ctxt
  in
  return
    {
      current_level;
      latest_proposal = current_proposal;
      is_latest_proposal_applied =
        true (* this proposal is expected to be the current head *);
      locked_round = None;
      attestable_payload = None;
      elected_block;
      delegate_infos;
      next_level_delegate_infos;
      next_level_latest_forge_request = None;
      dal_attestable_slots;
      next_level_dal_attestable_slots;
    }

let create_round_state ~global_state ~synchronize ~current_proposal =
  let open Result_syntax in
  let* current_round =
    if synchronize then
      Baking_actions.compute_round current_proposal global_state.round_durations
    else return Round.zero
  in
  return
    {
      current_round;
      current_phase = Idle;
      delayed_quorum = None;
      early_attestations = [];
      awaiting_unlocking_pqc = false;
    }

let create_initial_state ?canceler cctxt ?(synchronize = true)
    ?monitor_node_operations ~global_state ~(current_proposal : proposal)
    delegates =
  let open Lwt_result_syntax in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7391
     consider saved attestable value *)
  let open Baking_state in
  let* level_state =
    create_level_state
      ~chain_id:global_state.chain_id
      ~cctxt
      ~current_proposal
      ~delegates
      ~dal_node_rpc_ctxt:global_state.dal_node_rpc_ctxt
  in
  let*? round_state =
    create_round_state ~global_state ~synchronize ~current_proposal
  in
  let* automaton_state =
    Baking_automaton.create_automaton_state
      ?canceler
      ?monitor_node_operations
      ~global_state
      cctxt
  in
  let state = {global_state; automaton_state; level_state; round_state} in
  (* Try loading locked round and attestable round from disk *)
  let* state = Baking_state.may_load_attestable_data state in
  may_initialise_with_latest_proposal_pqc state

module Supervisor = struct
  let run ~delay_between_restarts ~run_automaton cctxts =
    let rec loop running_automatons_count pending_promises =
      let open Lwt_syntax in
      (* waits until one (or many) [pending_promises] gets fulfilled *)
      let* fulfilled_promises, pending_promises =
        Lwt.nchoose_split pending_promises
      in
      (* handle fulfilled promises *)
      let* running_automatons_count, pending_promises =
        List.fold_left_s
          (fun (running_automatons_count, pending_promises) fulfilled_promise ->
            match fulfilled_promise with
            | `Start cctxt ->
                let uri = Uri.to_string cctxt#base in
                let* () = Events.(emit supervisor_starting_automaton uri) in
                let promise =
                  (* promise that resolves to (`Crash cctxt)
                     when [run_automaton cctxt] resumes *)
                  let* res = run_automaton cctxt in
                  return (`Crash (cctxt, res))
                in
                return
                  (succ running_automatons_count, promise :: pending_promises)
            | `Crash (cctxt, _res) ->
                let uri = Uri.to_string cctxt#base in
                let* () =
                  Events.(
                    emit
                      supervisor_automaton_crashed
                      (uri, delay_between_restarts))
                in
                let promise =
                  (* promise that resolves to (`Start cctxt)
                     after [delay_between_restarts] seconds *)
                  let* () = Lwt_unix.sleep delay_between_restarts in
                  return (`Start cctxt)
                in
                return
                  (pred running_automatons_count, promise :: pending_promises))
          (running_automatons_count, pending_promises)
          fulfilled_promises
      in
      (* We deliberately shutdown the baker when all automatons are
         simultaneously waiting for a restart. This is done in order to escalate
         the situation and draw attention to it. *)
      if running_automatons_count <= 0 then
        let* () = Events.(emit supervisor_all_down ()) in
        Lwt_result_syntax.tzfail Baking_errors.Node_connection_lost
      else loop running_automatons_count pending_promises
    in
    loop 0 (List.map (fun cctxt -> Lwt.return (`Start cctxt)) cctxts)
end

let initialize_automaton ?canceler ?(synchronize = true) ~chain ~cache
    ~on_head_proposal_callback global_state delegates cctxt =
  let open Lwt_result_syntax in
  let* heads_stream, _ = Node_rpc.monitor_heads cctxt ~cache ~chain () in
  let* current_proposal =
    let*! head_opt = Lwt_stream.get heads_stream in
    match head_opt with
    | Some head -> return head
    | None -> failwith "monitor_heads stream unexpectedly ended."
  in
  let* state =
    create_initial_state
      ?canceler
      cctxt
      ~synchronize
      ~global_state
      ~current_proposal
      delegates
  in
  let loop_state =
    let get_valid_blocks_stream =
      let*! vbs = Node_rpc.monitor_valid_proposals cctxt ~cache ~chain () in
      match vbs with
      | Error _ -> Stdlib.failwith "Failed to get the validated blocks stream"
      | Ok (vbs, _) -> Lwt.return vbs
    in
    create_loop_state
      ~get_valid_blocks_stream
      ~on_head_proposal_callback
      ~forge_event_stream:state.automaton_state.forge_event_stream
      ~heads_stream
      state.automaton_state.operation_worker
  in
  let*? event = compute_bootstrap_event state in
  return (state, event, loop_state)

let run cctxt ~extra_nodes ?dal_node_rpc_ctxt ?canceler
    ?(stop_on_event = fun _ -> false)
    ?(on_error = fun _ -> Lwt_result_syntax.return_unit) ?constants ~chain
    config delegates =
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let* chain_id = Node_rpc.chain_id cctxt ~chain in
  let*! () = Events.emit Node_rpc_events.chain_id chain_id in
  let* () = perform_sanity_check cctxt ~chain_id in
  let cache = Baking_cache.Block_cache.create 10 in
  let* global_state =
    create_global_state
      ?dal_node_rpc_ctxt
      ?constants
      ~chain
      cctxt
      config
      delegates
  in
  let _promise =
    register_dal_profiles cctxt global_state.dal_node_rpc_ctxt delegates
  in
  let*! revelation_worker_canceler, revelation_worker_push_proposal =
    Baking_nonces.start_revelation_worker
      cctxt
      global_state.config.nonce
      global_state.chain_id
      global_state.constants
  in
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          let*! _ = Lwt_canceler.cancel revelation_worker_canceler in
          Lwt.return_unit))
    canceler ;
  let on_error err =
    let*! () = Events.(emit error_while_baking err) in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7393
       retry a bounded number of time *)
    (* let retries = config.Baking_configuration.retries_on_failure in *)
    on_error err
  in
  let run_automaton cctxt =
    let* initial_state, initial_event, loop_state =
      initialize_automaton
        ?canceler
        ~chain
        ~cache
        ~on_head_proposal_callback:revelation_worker_push_proposal
        global_state
        delegates
        cctxt
    in
    automaton_loop
      ~stop_on_event
      ~config
      ~on_error
      loop_state
      initial_state
      initial_event
  in
  (* profiler_section is defined here because ocamlformat and ppx mix badly here *)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/8258
  let[@warning "-26"] profiler_section =
    New_valid_proposal initial_state.level_state.latest_proposal
  in
  () [@profiler.stop] ;
  () [@profiler.overwrite Profiler.reset_block_section (profiler_section, [])] ;
*)
  protect
    ~on_error:(fun err ->
      let*! _ = Option.iter_es Lwt_canceler.cancel canceler in
      Lwt.return_error err)
    (fun () ->
      match extra_nodes with
      | [] ->
          let* _event = run_automaton cctxt in
          return_unit
      | _ ->
          let* () =
            (* sanity check: all nodes must target the same chain *)
            List.iter_es
              (fun cctxt ->
                let* cctxt_chain_id = Node_rpc.chain_id ~chain cctxt in
                fail_unless
                  (Chain_id.equal cctxt_chain_id global_state.chain_id)
                  (Baking_errors.Inconsistent_chain_id
                     {
                       uri = Uri.to_string cctxt#base;
                       expected = global_state.chain_id;
                       found = cctxt_chain_id;
                     }))
              extra_nodes
          in
          Supervisor.run
            ~delay_between_restarts:20.
            ~run_automaton
            (cctxt :: extra_nodes))
