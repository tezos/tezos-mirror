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

type liquidity_baking_vote = Off | On | Pass

let liquidity_baking_vote_to_string = function
  | Off -> "off"
  | On -> "on"
  | Pass -> "pass"

let liquidity_baking_vote_of_string_opt = function
  | "off" -> Some Off
  | "on" -> Some On
  | "pass" -> Some Pass
  | _ -> None

module Parameters = struct
  type persistent_state = {
    protocol : Protocol.t;
    delegates : string list;
    runner : Runner.t option;
    node : Node.t;
    client : Client.t;
    mutable pending_ready : unit option Lwt.u list;
    votefile : string option;
    liquidity_baking_toggle_vote : liquidity_baking_vote option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "baker"

  let default_colors = Log.Color.[|FG.green|]
end

open Parameters
include Daemon.Make (Parameters)

let trigger_ready baker value =
  let pending = baker.persistent_state.pending_ready in
  baker.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready baker =
  (match baker.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready baker (Some ())

let handle_raw_stdout baker line =
  if line =~ rex "^Baker v.+ for .+ started.$" then set_ready baker

let liquidity_baking_votefile ?path vote =
  let votefile =
    Option.value
      path
      ~default:(Temp.file "liquidity_baking_toggle_votefile.json")
  in
  JSON.encode_to_file_u
    votefile
    (`O
      [
        ( "liquidity_baking_toggle_vote",
          `String (liquidity_baking_vote_to_string vote) );
      ]) ;
  votefile

let create ~protocol ?name ?color ?event_pipe ?runner ?(delegates = [])
    ?votefile ?(liquidity_baking_toggle_vote = Some Pass) node client =
  let baker =
    create
      ~path:(Protocol.baker protocol)
      ?name
      ?color
      ?event_pipe
      ?runner
      {
        protocol;
        delegates;
        runner;
        node;
        client;
        pending_ready = [];
        votefile;
        liquidity_baking_toggle_vote;
      }
  in
  on_stdout baker (handle_raw_stdout baker) ;
  baker

let run (baker : t) =
  (match baker.status with
  | Not_running -> ()
  | Running _ -> Test.fail "baker %s is already running" baker.name) ;
  let delegates = baker.persistent_state.delegates in
  let runner = baker.persistent_state.runner in
  let node = baker.persistent_state.node in
  let client = baker.persistent_state.client in
  let node_addr =
    Printf.sprintf "http://%s:%d" (Node.rpc_host node) (Node.rpc_port node)
  in
  let votefile =
    Cli_arg.optional_arg "votefile" Fun.id baker.persistent_state.votefile
  in
  let liquidity_baking_toggle_vote =
    Cli_arg.optional_arg
      "liquidity-baking-toggle-vote"
      liquidity_baking_vote_to_string
      baker.persistent_state.liquidity_baking_toggle_vote
  in
  let arguments =
    [
      "--endpoint";
      node_addr;
      "--base-dir";
      Client.base_dir client;
      "run";
      "with";
      "local";
      "node";
      Node.data_dir node;
    ]
    @ liquidity_baking_toggle_vote @ votefile @ delegates
  in
  let on_terminate _ =
    (* Cancel all [Ready] event listeners. *)
    trigger_ready baker None ;
    unit
  in
  run baker {ready = false} arguments ~on_terminate ?runner

let check_event ?where baker name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = baker.name; event = name; where})
  | Some x -> return x

let wait_for_ready baker =
  match baker.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      baker.persistent_state.pending_ready <-
        resolver :: baker.persistent_state.pending_ready ;
      check_event baker "Baker started." promise

let init ~protocol ?name ?color ?event_pipe ?runner ?(delegates = []) ?votefile
    ?liquidity_baking_toggle_vote node client =
  let* () = Node.wait_for_ready node in
  let baker =
    create
      ~protocol
      ?name
      ?color
      ?event_pipe
      ?runner
      ?votefile
      ?liquidity_baking_toggle_vote
      ~delegates
      node
      client
  in
  let* () = run baker in
  let* () = wait_for_ready baker in
  return baker
