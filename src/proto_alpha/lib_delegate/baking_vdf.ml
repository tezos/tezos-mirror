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
module Events = Baking_events.VDF
module D_Events = Delegate_events.Denunciator
open Client_baking_blocks

type status = Not_started | Started | Finished

type 'a state = {
  cctxt : Protocol_client_context.full;
  constants : Constants.t;
  block_stream : (block_info, 'a) result Lwt_stream.t;
  mutable cycle : Cycle_repr.t option;
  mutable computation_status : status;
}

let rec wait_for_first_block ~name stream =
  Lwt_stream.get stream >>= function
  | None | Some (Error _) ->
      Delegate_events.Baking_scheduling.(emit cannot_fetch_event) name
      >>= fun () ->
      (* NOTE: this is not a tight loop because of Lwt_stream.get *)
      wait_for_first_block ~name stream
  | Some (Ok bi) -> Lwt.return bi

let log_errors_and_continue ~name p =
  p >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> Events.(emit vdf_daemon_error) (name, errs)

let cycle_of_level state level =
  let {Constants.parametric = {blocks_per_cycle; _}; _} = state.constants in
  let level = Raw_level.to_int32 level in
  Int32.(div (pred level) blocks_per_cycle)

let is_in_nonce_revelation_period state level =
  let {
    Constants.parametric = {blocks_per_cycle; nonce_revelation_threshold; _};
    _;
  } =
    state.constants
  in
  let current_cycle = cycle_of_level state level in
  let level = Raw_level.to_int32 level in
  let position_in_cycle =
    Int32.(sub level (mul current_cycle blocks_per_cycle))
  in
  Int32.compare position_in_cycle nonce_revelation_threshold < 0

let check_new_cycle state level =
  let current_cycle = Cycle_repr.of_int32_exn (cycle_of_level state level) in
  match state.cycle with
  | None -> state.cycle <- Some current_cycle
  | Some cycle ->
      if Cycle_repr.(succ cycle = current_cycle) then (
        state.cycle <- Some current_cycle ;
        state.computation_status <- Not_started)

let process_new_block (cctxt : #Protocol_client_context.full) state
    {hash; chain_id; protocol; next_protocol; level; _} =
  let open Lwt_result_syntax in
  let level_str = Int32.to_string (Raw_level.to_int32 level) in
  check_new_cycle state level ;
  if Protocol_hash.(protocol <> next_protocol) then
    D_Events.(emit protocol_change_detected) () >>= fun () -> return_unit
  else if is_in_nonce_revelation_period state level then
    Events.(emit vdf_info)
      ("Skipping, still in nonce revelation period (level " ^ level_str ^ ")")
    >>= fun _ -> return_unit
  else if state.computation_status = Finished then
    Events.(emit vdf_info)
      ("Skipping, computation finished (level " ^ level_str ^ ")")
    >>= fun _ -> return_unit
  else
    let chain = `Hash chain_id in
    let block = `Hash (hash, 0) in
    Alpha_services.Seed_computation.get cctxt (chain, block) >>=? fun x ->
    (match x with
    | Vdf_revelation_stage {seed_discriminant; seed_challenge} ->
        if state.computation_status = Started then
          Events.(emit vdf_info)
            ("Skipping, computation already started (level " ^ level_str ^ ")")
          >>= fun () -> return_unit
        else
          Events.(emit vdf_info)
            ("Started to compute VDF (level " ^ level_str ^ ")")
          >>= fun () ->
          state.computation_status <- Started ;
          let discriminant, challenge =
            Seed.generate_vdf_setup ~seed_discriminant ~seed_challenge
          in
          let solution =
            Environment.Vdf.prove
              discriminant
              challenge
              state.constants.parametric.vdf_difficulty
          in
          let* bytes =
            Plugin.RPC.Forge.vdf_revelation
              cctxt
              (chain, block)
              ~branch:hash
              ~solution
              ()
          in
          let bytes = Signature.concat bytes Signature.zero in
          let* op_hash =
            Shell_services.Injection.operation cctxt ~chain bytes
          in
          Events.(emit vdf_revelation_injected)
            (cycle_of_level state level, Chain_services.to_string chain, op_hash)
          >>= fun () -> return_unit
    | Nonce_revelation_stage ->
        (* this should never actually happen *)
        Events.(emit vdf_info)
          ("Nonce revelation stage (level " ^ level_str ^ ")")
        >>= fun () -> return_unit
    | Computation_finished ->
        (* this should happen at most once per cycle *)
        state.computation_status <- Finished ;
        Events.(emit vdf_info) ("Computation finished (level " ^ level_str ^ ")")
        >>= fun () -> return_unit)
    >>= fun _ -> return_unit

let start_vdf_worker (cctxt : Protocol_client_context.full) ~canceler constants
    (block_stream : Client_baking_blocks.block_info tzresult Lwt_stream.t) =
  let state =
    {
      cctxt;
      constants;
      block_stream;
      cycle = None;
      computation_status = Not_started;
    }
  in
  Lwt_canceler.on_cancel canceler (fun () -> Lwt.return_unit) ;
  wait_for_first_block ~name state.block_stream >>= fun _first_event ->
  let rec worker_loop () =
    Lwt.choose
      [
        (Lwt_exit.clean_up_starts >|= fun _ -> `Termination);
        (Lwt_stream.get state.block_stream >|= fun e -> `Block e);
      ]
    >>= function
    | `Termination -> return_unit
    | `Block (None | Some (Error _)) ->
        (* exit when the node is unavailable *)
        Events.(emit vdf_daemon_connection_lost) name >>= fun () ->
        fail Baking_errors.Node_connection_lost
    | `Block (Some (Ok bi)) ->
        log_errors_and_continue ~name @@ process_new_block cctxt state bi
        >>= fun () -> worker_loop ()
  in
  Events.(emit vdf_daemon_start) name >>= fun () -> worker_loop ()
