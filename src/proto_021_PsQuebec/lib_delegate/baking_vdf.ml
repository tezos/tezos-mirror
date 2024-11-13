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

type vdf_solution = Seed_repr.vdf_solution

type vdf_setup = Seed_repr.vdf_setup

type forked_process = {pid : int; ch_in : Lwt_io.input_channel}

type status =
  | Not_started
  | Started of vdf_setup * forked_process
  | Finished of vdf_setup * vdf_solution
  | Injected
  | Invalid

type 'a state = {
  cctxt : Protocol_client_context.full;
  constants : Constants.t;
  mutable block_stream : (block_info, 'a) result Lwt_stream.t;
  mutable stream_stopper : Tezos_rpc.Context.stopper option;
  mutable cycle : Cycle.t option;
  mutable computation_status : status;
}

let init_block_stream_with_stopper cctxt chain =
  Client_baking_blocks.monitor_heads
    cctxt
    ~next_protocols:(Some [Protocol.hash])
    chain

let stop_block_stream state =
  Option.iter
    (fun stopper ->
      stopper () ;
      state.stream_stopper <- None)
    state.stream_stopper

let emit_with_level msg level =
  let level_i32 = Raw_level.to_int32 level in
  Events.(emit vdf_info) (Printf.sprintf "%s (level %ld)" msg level_i32)

let emit_revelation_not_injected cycle =
  let open Lwt_result_syntax in
  let*! () =
    Events.(emit vdf_info)
      (Printf.sprintf
         "VDF revelation was NOT injected for cycle %ld"
         (Cycle.to_int32 cycle))
  in
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

let is_in_nonce_revelation_stage constants (level : Level.t) =
  let open Lwt_result_syntax in
  let {Constants.parametric = {nonce_revelation_threshold; _}; _} = constants in
  return
    (Vdf_helpers.is_in_nonce_revelation_stage
       ~nonce_revelation_threshold
       ~level)

(* Checks if the VDF setup saved in the state is equal to the one computed
   from a seed *)
let eq_vdf_setup vdf_setup seed_discriminant seed_challenge =
  let open Environment.Vdf in
  let saved_discriminant, saved_challenge = vdf_setup in
  let discriminant, challenge =
    Seed.generate_vdf_setup ~seed_discriminant ~seed_challenge
  in
  Bytes.equal
    (discriminant_to_bytes discriminant)
    (discriminant_to_bytes saved_discriminant)
  && Bytes.equal
       (challenge_to_bytes challenge)
       (challenge_to_bytes saved_challenge)

(* Forge the VDF revelation operation and inject it if:
 *   - it is correct wrt the VDF setup for the current cycle
 *   - we are still in the VDF revelation stage
 * If successful or if the seed no longer needs to be injected,
 * update the computation status. *)
let inject_vdf_revelation cctxt state setup solution chain_id hash
    (level_info : Level.t) =
  let open Lwt_result_syntax in
  let chain = `Hash chain_id in
  let block = `Hash (hash, 0) in
  let level = level_info.level in
  let* seed_computation = get_seed_computation cctxt chain_id hash in
  match seed_computation with
  | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
      if eq_vdf_setup setup seed_discriminant seed_challenge then (
        let* op_bytes =
          Plugin.RPC.Forge.vdf_revelation
            cctxt
            (chain, block)
            ~branch:hash
            ~solution
            ()
        in
        let op_bytes = Tezos_crypto.Signature.V_latest.(concat op_bytes zero) in
        let* op_hash =
          Shell_services.Injection.operation cctxt ~chain op_bytes
        in
        (* If injection is successful, update the status to [Injected]. *)
        state.computation_status <- Injected ;
        let*! () =
          Events.(emit vdf_revelation_injected)
            ( Cycle.to_int32 level_info.cycle,
              Chain_services.to_string chain,
              op_hash )
        in
        return_unit)
      else (
        (* The VDF setup saved in the state is different from the one computed
         * from the on-chain seed. In practice this would indicate a bug, since
         * it would either mean that the cycle has changed and we have not
         * detected it or that the VDF setup changed mid-cycle. *)
        state.computation_status <- Invalid ;
        let*! () =
          emit_with_level "Error injecting VDF: setup has been updated" level
        in
        return_unit)
  | Nonce_revelation_stage ->
      state.computation_status <- Not_started ;
      let*! () = emit_with_level "Not injecting VDF: new cycle started" level in
      return_unit
  | Computation_finished ->
      state.computation_status <- Injected ;
      let*! () = emit_with_level "Not injecting VDF: already injected" level in
      return_unit

(* Launch the heavy VDF computation as a separate process. This is done in order
 * to not block the main process, allowing it to continue monitoring blocks and
 * to cancel or restart the VDF computation if needed. *)
let fork_vdf_computation state ((discriminant, challenge) as setup) level =
  let open Lwt_syntax in
  let ch_in, forked_out = Lwt_io.pipe () in
  match Lwt_unix.fork () with
  | 0 -> (
      (* In the forked process, try to compute the VDF solution, write it
       * to [forked_out], then exit. *)
      let* () = Lwt_io.close ch_in in
      let solution =
        Environment.Vdf.prove
          discriminant
          challenge
          state.constants.parametric.vdf_difficulty
      in
      match
        Data_encoding.Binary.to_bytes Seed.vdf_solution_encoding solution
      with
      | Ok encoded ->
          let* () = Lwt_io.write_value forked_out encoded in
          exit 0
      | Error _ ->
          let* () = Events.(emit vdf_info) "Error encoding VDF solution" in
          exit 1)
  | pid ->
      (* In the main process, change the computation status to [Started],
         record the forked process data, and continue. *)
      let* () = Lwt_io.close forked_out in
      state.computation_status <- Started (setup, {pid; ch_in}) ;
      let* () =
        emit_with_level
          (Printf.sprintf "Started to compute VDF, pid: %d" pid)
          level
      in
      return_unit

(* Check whether the VDF computation process has exited and read the result.
 * Update the computation status accordingly. *)
let get_vdf_solution_if_ready cctxt state proc setup chain_id hash
    (level_info : Level.t) =
  let open Lwt_result_syntax in
  let level = level_info.level in
  let*! status = Lwt_unix.waitpid [WNOHANG] proc.pid in
  match status with
  | 0, _ ->
      (* If the process is still running, continue *)
      let*! () = emit_with_level "Skipping, VDF computation launched" level in
      return_unit
  | _, WEXITED 0 -> (
      (* If the process has exited normally, read the solution, update
       * the status to [Finished], and attempt to inject the VDF
       * revelation. *)
      let*! encoded_solution = Lwt_io.read_value proc.ch_in in
      match
        Data_encoding.Binary.of_bytes
          Seed.vdf_solution_encoding
          encoded_solution
      with
      | Ok solution ->
          let*! () = Lwt_io.close proc.ch_in in
          state.computation_status <- Finished (setup, solution) ;
          let*! () = emit_with_level "Finished VDF computation" level in
          inject_vdf_revelation
            cctxt
            state
            setup
            solution
            chain_id
            hash
            level_info
      | Error _ ->
          let*! () = Events.(emit vdf_info) "Error decoding VDF solution" in
          state.computation_status <- Not_started ;
          return_unit)
  | _, WEXITED _ | _, WSIGNALED _ | _, WSTOPPED _ ->
      (* If process has exited abnormally, reset the computation status to
       * [Not_started] and continue *)
      state.computation_status <- Not_started ;
      let*! () =
        Events.(emit vdf_info) "VDF computation process exited abnormally"
      in
      return_unit

let kill_forked_process {pid; _} =
  let open Lwt_syntax in
  let* () =
    match Unix.kill pid Sys.sigterm with
    | () ->
        Events.(emit vdf_info)
          (Printf.sprintf
             "Sent SIGTERM to VDF computation process (pid %d)"
             pid)
    | exception Unix.Unix_error (err, _, _) ->
        let msg = Printf.sprintf "%s (pid %d)" (Unix.error_message err) pid in
        Events.(emit vdf_daemon_cannot_kill_computation) msg
  in
  let* pid, status = Lwt_unix.waitpid [] pid in
  let status =
    match status with
    | WEXITED n -> Printf.sprintf "WEXITED %d" n
    | WSIGNALED n -> Printf.sprintf "WSIGNALED %d" n
    | WSTOPPED n -> Printf.sprintf "WSTOPPED %d" n
  in
  Events.(emit vdf_info)
    (Printf.sprintf
       "Exit status for child VDF computation process %d: %s"
       pid
       status)

(* Kill the VDF computation process if one was launched. *)
let maybe_kill_running_vdf_computation state =
  let open Lwt_syntax in
  match state.computation_status with
  | Started (_, proc) ->
      let* () = kill_forked_process proc in
      return_unit
  | _ -> return_unit

(* Checks if the cycle of the last processed block is different from the cycle
 * of the block at [level_info]. *)
let check_new_cycle state (level_info : Level.t) =
  let open Lwt_result_syntax in
  let current_cycle = level_info.cycle in
  match state.cycle with
  | None ->
      (* First processed block, initialise [state.cycle] *)
      state.cycle <- Some current_cycle ;
      return_unit
  | Some cycle ->
      if Cycle.(cycle < current_cycle) then (
        (* The cycle of this block is different from the cycle of the last
         * processed block. Emit an event if the VDF for the previous cycle
         * has not been injected, kill any running VDF computation, and
         * reset the computation status. *)
        let* () =
          match state.computation_status with
          | Injected -> return_unit
          | Started ((_ : vdf_setup), proc) ->
              let*! () = kill_forked_process proc in
              emit_revelation_not_injected cycle
          | Not_started | Finished _ | Invalid ->
              emit_revelation_not_injected cycle
        in
        state.cycle <- Some current_cycle ;
        state.computation_status <- Not_started ;
        return_unit)
      else return_unit

(* The daemon's main job is to launch the VDF computation as soon as it
 * can (i.e., when the nonce revelation stage ends) and to inject
 * the VDF solution as soon as it finishes computing it.
 * Additionally, it must cancel a running VDF computation if its result
 * is no longer required and restart a computation if it failed.
 * The daemon processes the stream of blocks and monitors both
 * the level of the head within a cycle and the [Seed_computation] RPC.
 * The core of this function is a pattern match on the product of
 * [seed_computation] (the on-chain status of the seed computation)
 * and [state.computation_status] (the internal status of the daemon).
 *
 * [seed_computation] is reset at the beginning of a cycle to
 * [Nonce_revelation_stage], mirroring the on-chain change of the computation
 * status. No action is taken while in this state.
 * After [nonce_revelation_threshold] blocks, the status becomes
 * [Vdf_revelation_stage]. A call to the RPC confirms this and provides the seed
 * required to launch the VDF computation.
 * If a VDF revelation operation is injected before the end of the cycle,
 * the status is updated to [Computation_finished]. If a VDF computation is
 * running at that point (i.e., another daemon injected first),
 * it is canceled. *)
let process_new_block (cctxt : #Protocol_client_context.full) state
    {hash; chain_id; protocol; next_protocol; level; _} =
  let open Lwt_result_syntax in
  if Protocol_hash.(protocol <> next_protocol) then
    (* If the protocol has changed, emit an event on every new block and take
     * no further action. It is expected that the daemon corresponding to
     * the new protocol is used instead. *)
    let*! () = Delegate_events.Denunciator.(emit protocol_change_detected) () in
    return_unit
  else
    let* level_info = get_level_info cctxt level in
    (* If head is in a new cycle record it in [state.cycle] and reset
     * [state.computation_status] to [Not_started]. *)
    let* () = check_new_cycle state level_info in
    (* If the chain is in the nonce revelation stage, there is nothing to do. *)
    let* out = is_in_nonce_revelation_stage state.constants level_info in
    if out then
      let*! () =
        emit_with_level "Skipping, still in nonce revelation stage" level
      in
      return_unit
    else
      (* Enter main loop if we are not in the nonce revelation stage and
       * the expected protocol has been activated. *)
      match state.computation_status with
      | Not_started -> (
          let* seed_computation = get_seed_computation cctxt chain_id hash in
          match seed_computation with
          | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
              (* The chain is in the VDF revelation stage and the computation
               * has not been started, so it is started here, in a separate
               * process. The computation status is updated to [Started]. *)
              let setup =
                Seed.generate_vdf_setup ~seed_discriminant ~seed_challenge
              in
              let*! () = fork_vdf_computation state setup level in
              return_unit
          | Computation_finished ->
              let*! () =
                emit_with_level
                  "Skipping, VDF solution has already been injected"
                  level
              in
              return_unit
          | Nonce_revelation_stage ->
              (* At this point the chain cannot be in the nonce revelation
               * stage. This is checked in [is_in_nonce_revelation_stage]. *)
              assert false)
      | Started (setup, proc) -> (
          let* seed_computation = get_seed_computation cctxt chain_id hash in
          match seed_computation with
          | Vdf_revelation_stage _ ->
              (* The chain is in the VDF computation stage and we have
               * previously started the computation. Check whether it is
               * finished and, if so, update the computation status to
               * [Finished] and immediately inject the solution. *)
              let* () =
                get_vdf_solution_if_ready
                  cctxt
                  state
                  proc
                  setup
                  chain_id
                  hash
                  level_info
              in
              return_unit
          | Computation_finished ->
              (* The chain is no longer in the VDF revelation stage because
               * the solution has already been injected: abort the running
               * computation. *)
              let*! () = kill_forked_process proc in
              let*! () =
                emit_with_level
                  "VDF solution already injected, aborting VDF computation"
                  level
              in
              state.computation_status <- Injected ;
              return_unit
          | Nonce_revelation_stage ->
              (* At this point the chain cannot be in the nonce revelation
               * stage. This is checked in [is_in_nonce_revelation_stage]. *)
              assert false)
      | Finished (setup, solution) ->
          (* VDF solution computed, but not injected. We are only in this case
           * if the first attempt to inject, right after getting the solution,
           * was unsuccessful. While the chain is in the VDF revelation stage,
           * and the solution has not been injected (computation status is
           * [Finished]), we try to inject. If successful, the computation
           * status is updated to [Injected]. *)
          inject_vdf_revelation
            cctxt
            state
            setup
            solution
            chain_id
            hash
            level_info
      | Injected ->
          let*! () =
            emit_with_level "Skipping, VDF solution already injected" level
          in
          return_unit
      | Invalid ->
          let*! () = emit_with_level "Skipping, failed to compute VDF" level in
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
    }
  in
  Lwt_canceler.on_cancel canceler (fun () ->
      let*! () = maybe_kill_running_vdf_computation state in
      stop_block_stream state ;
      Lwt.return_unit) ;
  let rec worker_loop () =
    let*! b =
      Lwt.choose
        [
          (let*! _ = Lwt_exit.clean_up_starts in
           Lwt.return `Termination);
          (let*! e = Lwt_stream.get state.block_stream in
           Lwt.return (`Block e));
        ]
    in
    match b with
    | `Termination -> return_unit
    | `Block (None | Some (Error _)) ->
        (* Exit when the node is unavailable *)
        stop_block_stream state ;
        let*! () = Events.(emit vdf_daemon_connection_lost) name in
        tzfail Baking_errors.Node_connection_lost
    | `Block (Some (Ok bi)) ->
        let*! () =
          log_errors_and_continue ~name @@ process_new_block cctxt state bi
        in
        worker_loop ()
  in
  worker_loop ()
