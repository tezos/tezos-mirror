(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    runner : Runner.t option;
    base_dir : string;
    node : Node.t;
    mutable pending_ready : unit option Lwt.u list;
    preserved_levels : int option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "accuser"

  let default_colors =
    Log.Color.
      [|BG.yellow ++ FG.black; BG.yellow ++ FG.gray; BG.yellow ++ FG.blue|]
end

open Parameters
include Daemon.Make (Parameters)

let node_rpc_port accuser = Node.rpc_port accuser.persistent_state.node

let trigger_ready accuser value =
  let pending = accuser.persistent_state.pending_ready in
  accuser.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready accuser =
  (match accuser.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready accuser (Some ())

let handle_raw_stdout accuser line =
  if line =~ rex "^Waiting for protocol .+ to start...$" then set_ready accuser

let create ~protocol ?name ?color ?event_pipe ?base_dir ?runner
    ?preserved_levels node =
  let name = match name with None -> fresh_name () | Some name -> name in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  let accuser =
    create
      ~path:(Uses.path (Protocol.accuser protocol))
      ?name:(Some name)
      ?color
      ?event_pipe
      ?runner
      {runner; base_dir; node; pending_ready = []; preserved_levels}
  in
  on_stdout accuser (handle_raw_stdout accuser) ;
  accuser

let run ?event_level accuser =
  (match accuser.status with
  | Not_running -> ()
  | Running _ -> Test.fail "accuser %s is already running" accuser.name) ;
  let runner = accuser.persistent_state.runner in
  let node_runner = Node.runner accuser.persistent_state.node in
  let node_rpc_port = node_rpc_port accuser in
  let address = "http://" ^ Runner.address ?from:runner node_runner ^ ":" in
  let preserved_levels =
    Cli_arg.optional_arg
      "preserved-levels"
      string_of_int
      accuser.persistent_state.preserved_levels
  in
  let arguments =
    [
      "-E";
      address ^ string_of_int node_rpc_port;
      "--base-dir";
      accuser.persistent_state.base_dir;
      "run";
    ]
    @ preserved_levels
  in
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready accuser None ;
    unit
  in
  run ?event_level accuser {ready = false} arguments ~on_terminate ?runner

let check_event ?where accuser name promise =
  let* result = promise in
  match result with
  | None ->
      raise
        (Terminated_before_event {daemon = accuser.name; event = name; where})
  | Some x -> return x

let wait_for_ready accuser =
  match accuser.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      accuser.persistent_state.pending_ready <-
        resolver :: accuser.persistent_state.pending_ready ;
      check_event accuser "Accuser started." promise

let init ~protocol ?name ?color ?event_pipe ?event_level ?base_dir ?runner
    ?preserved_levels node =
  let* () = Node.wait_for_ready node in
  let accuser =
    create
      ~protocol
      ?name
      ?color
      ?event_pipe
      ?base_dir
      ?runner
      ?preserved_levels
      node
  in
  let* () = run ?event_level accuser in
  let* () = wait_for_ready accuser in
  return accuser

let restart accuser =
  let* () = terminate accuser in
  let* () = run accuser in
  wait_for_ready accuser
