(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Client_baking_blocks
module Events = Baking_events.VDF
module D_Events = Delegate_events.Denunciator

type vdf_solution = Seed_repr.vdf_solution

type vdf_setup = Seed_repr.vdf_setup

type status =
  | Not_started
  | Started
  | Finished of vdf_solution
  | Injected
  | Invalid

type 'a state = {
  cctxt : Protocol_client_context.full;
  constants : Constants.t;
  mutable block_stream : (block_info, 'a) result Lwt_stream.t;
  mutable stream_stopper : Tezos_rpc.Context.stopper option;
  mutable cycle : Cycle.t option;
  mutable computation_status : status;
  mutable vdf_setup : vdf_setup option;
}

let init_block_stream_with_stopper cctxt chain =
  Client_baking_blocks.monitor_applied_blocks
    ~next_protocols:(Some [Protocol.hash])
    cctxt
    ~chains:[chain]
    ()

let stop_block_stream state =
  Option.iter
    (fun stopper ->
      stopper () ;
      state.stream_stopper <- None)
    state.stream_stopper

let restart_block_stream cctxt chain state =
  let open Lwt_result_syntax in
  stop_block_stream state ;
  let retries_on_failure = 10 in
  let rec try_start_block_stream retries_on_failure =
    let*! p = init_block_stream_with_stopper cctxt chain in
    match p with
    | Ok (block_stream, stream_stopper) ->
        state.block_stream <- block_stream ;
        state.stream_stopper <- Some stream_stopper ;
        return_unit
    | Error e ->
        if retries_on_failure > 0 then
          let*! () = Lwt_unix.sleep 10. in
          try_start_block_stream (retries_on_failure - 1)
        else fail e
  in
  let* () = try_start_block_stream retries_on_failure in
  return_unit

let log_errors_and_continue ~name p =
  let open Lwt_syntax in
  let* p in
  match p with
  | Ok () -> return_unit
  | Error errs -> Events.(emit vdf_daemon_error) (name, errs)

let get_seed_computation cctxt chain_id hash =
  let chain = `Hash chain_id in
  let block = `Hash (hash, 0) in
  Alpha_services.Seed_computation.get cctxt (chain, block)

let get_level_info cctxt level =
  let open Lwt_result_syntax in
  let level = Raw_level.to_int32 level in
  let* {protocol_data = {level_info; _}; _} =
    Protocol_client_context.Alpha_block_services.metadata
      cctxt
      ~chain:cctxt#chain
      ~block:(`Level level)
      ()
  in
  return level_info

let is_in_nonce_revelation_period state (level_info : Level.t) =
  let open Lwt_result_syntax in
  let {Constants.parametric = {nonce_revelation_threshold; _}; _} =
    state.constants
  in
  let position_in_cycle = level_info.cycle_position in
  return (Int32.compare position_in_cycle nonce_revelation_threshold < 0)

let check_new_cycle state (level_info : Level.t) =
  let open Lwt_result_syntax in
  let current_cycle = level_info.cycle in
  match state.cycle with
  | None ->
      state.cycle <- Some current_cycle ;
      return_unit
  | Some cycle ->
      if Cycle.(succ cycle <= current_cycle) then (
        (* The cycle of this block is different from the cycle of the last
         * processed block. Emit an event if the VDF for the previous cycle
         * has not been injected and reset the computation status. *)
        let* () =
          match state.computation_status with
          | Injected -> return_unit
          | _ ->
              let cycle_str = Int32.to_string (Cycle.to_int32 cycle) in
              let*! () =
                Events.(emit vdf_info)
                  ("VDF revelation was NOT injected for cycle " ^ cycle_str)
              in
              return_unit
        in
        state.cycle <- Some current_cycle ;
        state.computation_status <- Not_started ;
        return_unit)
      else return_unit

let inject_vdf_revelation cctxt hash chain_id solution =
  let open Lwt_result_syntax in
  let chain = `Hash chain_id in
  let block = `Hash (hash, 0) in
  let* bytes =
    Plugin.RPC.Forge.vdf_revelation
      cctxt
      (chain, block)
      ~branch:hash
      ~solution
      ()
  in
  let bytes = Signature.concat bytes Signature.zero in
  Shell_services.Injection.operation cctxt ~chain bytes

(* Checks if the VDF setup saved in the state is equal to the one computed
   from a seed *)
let eq_vdf_setup state seed_discriminant seed_challenge =
  let open Environment.Vdf in
  match state.vdf_setup with
  | None -> assert false
  | Some (saved_discriminant, saved_challenge) ->
      let discriminant, challenge =
        Seed.generate_vdf_setup ~seed_discriminant ~seed_challenge
      in
      Bytes.equal
        (discriminant_to_bytes discriminant)
        (discriminant_to_bytes saved_discriminant)
      && Bytes.equal
           (challenge_to_bytes challenge)
           (challenge_to_bytes saved_challenge)

let process_new_block (cctxt : #Protocol_client_context.full) state
    {hash; chain_id; protocol; next_protocol; level; _} =
  let open Lwt_result_syntax in
  let* level_info = get_level_info cctxt level in
  let level_str = Int32.to_string (Raw_level.to_int32 level) in
  let* () = check_new_cycle state level_info in
  if Protocol_hash.(protocol <> next_protocol) then
    let*! () = D_Events.(emit protocol_change_detected) () in
    return_unit
  else
    let* out = is_in_nonce_revelation_period state level_info in
    if out then
      let*! () =
        Events.(emit vdf_info)
          ("Skipping, still in nonce revelation period (level " ^ level_str
         ^ ")")
      in
      return_unit
      (* enter main loop if we are not in the nonce revelation period and
         the expected protocol has been activated *)
    else
      match state.computation_status with
      | Started ->
          let*! () =
            Events.(emit vdf_info)
              ("Skipping, already started VDF (level " ^ level_str ^ ")")
          in
          return_unit
      | Not_started -> (
          let chain = `Hash chain_id in
          let* seed_computation = get_seed_computation cctxt chain_id hash in
          match seed_computation with
          | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
              state.computation_status <- Started ;
              let*! () =
                Events.(emit vdf_info)
                  ("Started to compute VDF (level " ^ level_str ^ ")")
              in
              let vdf_setup =
                Seed.generate_vdf_setup ~seed_discriminant ~seed_challenge
              in
              state.vdf_setup <- Some vdf_setup ;
              stop_block_stream state ;
              let* () =
                Lwt.catch
                  (fun () ->
                    let discriminant, challenge = vdf_setup in
                    (* `Vdf.prove` is a long computation. We reset the block
                     * stream in order to not process all the blocks added
                     * to the chain during this time and skip straight to
                     * the current head. *)
                    let solution =
                      Environment.Vdf.prove
                        discriminant
                        challenge
                        state.constants.parametric.vdf_difficulty
                    in
                    state.computation_status <- Finished solution ;
                    let*! () = Events.(emit vdf_info) "VDF solution computed" in
                    return_unit)
                  (fun _ ->
                    (* VDF computation failed with an error thrown by the external
                     * library. We set the status back to Not_started in order to
                     * retry computing it if still possible. *)
                    state.computation_status <- Not_started ;
                    let*! () =
                      Events.(emit vdf_info)
                        ("Failed to compute VDF solution (level " ^ level_str
                       ^ ")")
                    in
                    return_unit)
              in
              restart_block_stream cctxt chain state
          | Nonce_revelation_stage | Computation_finished ->
              (* Daemon started too early or too late in a cycle, skipping. *)
              return_unit)
      | Finished solution -> (
          let*! () =
            Events.(emit vdf_info) ("Finished VDF (level " ^ level_str ^ ")")
          in
          let chain = `Hash chain_id in
          let* seed_computation = get_seed_computation cctxt chain_id hash in
          match seed_computation with
          | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
              (* If a solution has been computed that is consistent with the VDF
               * setup for the current cycle and we are still in the VDF
               * revelation stage, inject the operation. *)
              if eq_vdf_setup state seed_discriminant seed_challenge then (
                let* op_hash =
                  inject_vdf_revelation cctxt hash chain_id solution
                in
                state.computation_status <- Injected ;
                let*! () =
                  Events.(emit vdf_revelation_injected)
                    ( Cycle.to_int32 level_info.cycle,
                      Chain_services.to_string chain,
                      op_hash )
                in
                return_unit)
              else (
                state.computation_status <- Invalid ;
                let*! () =
                  Events.(emit vdf_info)
                    ("Error injecting VDF: setup has been updated (level "
                   ^ level_str ^ ")")
                in
                return_unit)
          | Nonce_revelation_stage ->
              state.computation_status <- Not_started ;
              let*! () =
                Events.(emit vdf_info)
                  ("Error injecting VDF: new cycle started (level " ^ level_str
                 ^ ")")
              in
              return_unit
          | Computation_finished ->
              state.computation_status <- Injected ;
              let*! () =
                Events.(emit vdf_info)
                  ("Error injecting VDF: already injected (level " ^ level_str
                 ^ ")")
              in
              return_unit)
      | Injected ->
          let*! () =
            Events.(emit vdf_info)
              ("Skipping, already injected VDF (level " ^ level_str ^ ")")
          in
          return_unit
      | Invalid ->
          let*! () =
            Events.(emit vdf_info)
              ("Skipping, failed to compute VDF (level " ^ level_str ^ ")")
          in
          return_unit

let start_vdf_worker (cctxt : Protocol_client_context.full) ~canceler constants
    chain =
  let open Lwt_result_syntax in
  let* block_stream, stream_stopper =
    init_block_stream_with_stopper cctxt chain
  in
  let state =
    {
      cctxt;
      constants;
      block_stream;
      stream_stopper = Some stream_stopper;
      cycle = None;
      computation_status = Not_started;
      vdf_setup = None;
    }
  in
  Lwt_canceler.on_cancel canceler (fun () ->
      stop_block_stream state ;
      Lwt.return_unit) ;
  let rec worker_loop () =
    let*! b =
      Lwt.choose
        [
          (Lwt_exit.clean_up_starts >|= fun _ -> `Termination);
          (Lwt_stream.get state.block_stream >|= fun e -> `Block e);
        ]
    in
    match b with
    | `Termination -> return_unit
    | `Block (None | Some (Error _)) ->
        (* exit when the node is unavailable *)
        stop_block_stream state ;
        let*! () = Events.(emit vdf_daemon_connection_lost) name in
        tzfail Baking_errors.Node_connection_lost
    | `Block (Some (Ok bi)) ->
        let*! () =
          log_errors_and_continue ~name @@ process_new_block cctxt state bi
        in
        worker_loop ()
  in
  let*! () = Events.(emit vdf_daemon_start) name in
  worker_loop ()
