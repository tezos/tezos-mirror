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

let create_global_state cctxt ?dal_node_rpc_ctxt ?constants ~chain config
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
          get_forge_event_stream = (fun _ -> assert false);
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
      get_forge_event_stream =
        (fun () -> Forge_worker.get_event_stream forge_worker);
      cancel_all_pending_tasks =
        (fun () -> Forge_worker.cancel_all_pending_tasks forge_worker);
    } ;
  return global_state

let create_initial_state ?canceler cctxt ?dal_node_rpc_ctxt
    ?(synchronize = true) ?monitor_node_operations ~chain config
    ~(current_proposal : proposal) ?constants delegates =
  let open Lwt_result_syntax in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7391
     consider saved attestable value *)
  let open Baking_state in
  let* global_state =
    create_global_state
      cctxt
      ?dal_node_rpc_ctxt
      ?constants
      ~chain
      config
      delegates
  in
  let chain = `Hash global_state.chain_id in
  let current_level = current_proposal.block.shell.level in
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
  let current_level = current_proposal.block.shell.level in
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
  let level_state =
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
  in
  let* round_state =
    if synchronize then
      let*? current_round =
        Baking_actions.compute_round
          current_proposal
          global_state.round_durations
      in
      return
        {
          current_round;
          current_phase = Idle;
          delayed_quorum = None;
          early_attestations = [];
          awaiting_unlocking_pqc = false;
        }
    else
      return
        {
          Baking_state.current_round = Round.zero;
          current_phase = Idle;
          delayed_quorum = None;
          early_attestations = [];
          awaiting_unlocking_pqc = false;
        }
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

let run cctxt ~extra_nodes:_ ?dal_node_rpc_ctxt ?canceler
    ?(stop_on_event = fun _ -> false)
    ?(on_error = fun _ -> Lwt_result_syntax.return_unit) ?constants ~chain
    config delegates =
  let open Lwt_result_syntax in
  let*! () = Events.(emit Baking_events.Launch.keys_used delegates) in
  let* chain_id = Node_rpc.chain_id cctxt ~chain in
  let*! () = Events.emit Node_rpc_events.chain_id chain_id in
  let* () = perform_sanity_check cctxt ~chain_id in
  let cache = Baking_cache.Block_cache.create 10 in
  let* heads_stream, _block_stream_stopper =
    Node_rpc.monitor_heads cctxt ~cache ~chain ()
  in
  let* current_proposal =
    let*! proposal = Lwt_stream.get heads_stream in
    match proposal with
    | Some current_head -> return current_head
    | None -> failwith "head stream unexpectedly ended"
  in
  let* initial_state =
    create_initial_state
      ?canceler
      cctxt
      ?dal_node_rpc_ctxt
      ~chain
      config
      ~current_proposal
      ?constants
      delegates
  in
  let _promise =
    register_dal_profiles
      cctxt
      initial_state.global_state.dal_node_rpc_ctxt
      delegates
  in
  let cloned_block_stream = Lwt_stream.clone heads_stream in
  let*! revelation_worker_canceler =
    Baking_nonces.start_revelation_worker
      cctxt
      initial_state.global_state.config.nonce
      initial_state.global_state.chain_id
      initial_state.global_state.constants
      cloned_block_stream
  in
  Option.iter
    (fun canceler ->
      Lwt_canceler.on_cancel canceler (fun () ->
          let*! _ = Lwt_canceler.cancel revelation_worker_canceler in
          Lwt.return_unit))
    canceler ;
  let get_valid_blocks_stream =
    let*! vbs = Node_rpc.monitor_valid_proposals cctxt ~cache ~chain () in
    match vbs with
    | Error _ -> Stdlib.failwith "Failed to get the validated blocks stream"
    | Ok (vbs, _) -> Lwt.return vbs
  in
  let forge_event_stream =
    initial_state.global_state.forge_worker_hooks.get_forge_event_stream ()
  in
  let loop_state =
    create_loop_state
      ~get_valid_blocks_stream
      ~forge_event_stream
      ~heads_stream
      initial_state.automaton_state.operation_worker
  in
  let on_error err =
    let*! () = Events.(emit error_while_baking err) in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7393
       retry a bounded number of time *)
    (* let retries = config.Baking_configuration.retries_on_failure in *)
    on_error err
  in
  let*? initial_event = compute_bootstrap_event initial_state in
  (* profiler_section is defined here because ocamlformat and ppx mix badly here *)
  let[@warning "-26"] profiler_section = New_valid_proposal current_proposal in
  () [@profiler.stop] ;
  () [@profiler.overwrite Profiler.reset_block_section (profiler_section, [])] ;
  protect
    ~on_error:(fun err ->
      let*! _ = Option.iter_es Lwt_canceler.cancel canceler in
      Lwt.return_error err)
    (fun () ->
      let* _ignored_event =
        automaton_loop
          ~stop_on_event
          ~config
          ~on_error
          loop_state
          initial_state
          initial_event
      in
      return_unit)
