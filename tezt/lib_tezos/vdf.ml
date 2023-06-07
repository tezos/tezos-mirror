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

module Parameters = struct
  type persistent_state = {
    protocol : Protocol.t;
    runner : Runner.t option;
    node : Node.t;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "vdf"

  let default_colors =
    Log.Color.
      [|
        BG.magenta ++ FG.black;
        BG.magenta ++ FG.gray;
        BG.magenta ++ FG.bright_white;
      |]
end

open Parameters
include Daemon.Make (Parameters)

let node_rpc_port accuser = Node.rpc_port accuser.persistent_state.node

let trigger_ready vdf_baker value =
  let pending = vdf_baker.persistent_state.pending_ready in
  vdf_baker.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready vdf_baker =
  (match vdf_baker.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready vdf_baker (Some ())

let handle_raw_stdout vdf_baker line =
  if line =~ rex "^VDF daemon v.+ for .+ started.$" then set_ready vdf_baker

let create ~protocol ?name ?color ?event_pipe ?runner node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let vdf_baker =
    create
      ~path:(Protocol.baker protocol)
      ?name:(Some name)
      ?color
      ?event_pipe
      ?runner
      {protocol; runner; node; pending_ready = []}
  in
  on_stdout vdf_baker (handle_raw_stdout vdf_baker) ;
  vdf_baker

let run (vdf_baker : t) =
  (match vdf_baker.status with
  | Not_running -> ()
  | Running _ -> Test.fail "VDF daemon %s is already running" vdf_baker.name) ;
  let runner = vdf_baker.persistent_state.runner in
  let node_runner = Node.runner vdf_baker.persistent_state.node in
  let node_rpc_port = node_rpc_port vdf_baker in
  let address = "http://" ^ Runner.address ?from:runner node_runner ^ ":" in
  let arguments = ["-E"; address ^ string_of_int node_rpc_port; "run"; "vdf"] in
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready vdf_baker None ;
    unit
  in
  run vdf_baker {ready = false} arguments ~on_terminate ?runner

let check_event ?where vdf_baker name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event {daemon = vdf_baker.name; event = name; where})
  | Some x -> return x

let wait_for_ready vdf_baker =
  match vdf_baker.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      vdf_baker.persistent_state.pending_ready <-
        resolver :: vdf_baker.persistent_state.pending_ready ;
      check_event vdf_baker "VDF daemon started." promise

let init ~protocol ?name ?color ?event_pipe ?runner node =
  let* () = Node.wait_for_ready node in
  let vdf_baker = create ~protocol ?name ?color ?event_pipe ?runner node in
  let* () = run vdf_baker in
  let* () = wait_for_ready vdf_baker in
  return vdf_baker

let restart vdf_baker =
  let* () = terminate vdf_baker in
  let* () = run vdf_baker in
  wait_for_ready vdf_baker

module Helpers = struct
  let is_in_nonce_revelation_stage ~nonce_revelation_threshold
      ~(level : RPC.level) =
    let cycle_position = Int32.of_int level.cycle_position in
    Int32.compare cycle_position nonce_revelation_threshold < 0
end
