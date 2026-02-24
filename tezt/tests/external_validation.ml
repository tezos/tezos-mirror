(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Node's external validator
   Invocation:   dune exec tezt/tests/main.exe -- --file
                 external_validation.ml
   Subject:      Tests the resilience of the external validator
                 failures
*)

let team = Tag.layer1

let wait_for_external_validator_pid node =
  let filter json = JSON.(json |> as_int_opt) in
  Node.wait_for node "validator_hypervisee_initialized.v0" filter

(* Typical signals that could be sent. This could be enriched but the
   effects are expected to be similar.
   Note that the behaviour of SIGSTOP is undefined here, as the node
   will hang forever.

   TODO/FIXME: https://gitlab.com/tezos/tezos/-/issues/6675
   It was reported that the SIGQUIT signal makes the "external
   validator kill" test particularly flaky. Thus, we choose to
   deactivate it.
*)
type signal = SIGABRT | SIGINT | SIGKILL | SIGTERM

let all_signals = [SIGABRT; SIGINT; SIGKILL; SIGTERM]

let signal_to_int = function
  | SIGABRT -> Sys.sigabrt
  | SIGINT -> Sys.sigint
  | SIGKILL -> Sys.sigkill
  | SIGTERM -> Sys.sigterm

let pp_signal ppf signal =
  let str =
    match signal with
    | SIGABRT -> "sigabrt"
    | SIGINT -> "sigint"
    | SIGKILL -> "sigkill"
    | SIGTERM -> "sigterm"
  in
  Format.fprintf ppf "%s" str

let kill_process ~pid ~signal =
  Log.info
    "Kill the external validator (pid %d) with signal %a"
    pid
    pp_signal
    signal ;
  Unix.kill pid (signal_to_int signal)

let rec wait_for_killing pid =
  let alive =
    try
      Unix.kill pid 0 ;
      true
    with _ -> false
  in
  if alive then
    let* () = Lwt_unix.sleep 0.2 in
    wait_for_killing pid
  else Lwt.return_unit

let test_kill =
  Protocol.register_test
    ~__FILE__
    ~title:"external validator kill"
    ~tags:[team; "node"; "external"; "validator"; "kill"]
  @@ fun protocol ->
  let node = Node.create [] in
  let wait_for_validator_pid = wait_for_external_validator_pid node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* validator_pid = wait_for_validator_pid in
  let* () = Client.activate_protocol_and_wait ~protocol client in
  Log.info "Wait for level 1" ;
  let* (_ : int) = Node.wait_for_level node 1 in
  let kill_loop (level, validator_pid) signal =
    (* Starts with a running process. *)
    let wait_for_new_validator_pid = wait_for_external_validator_pid node in
    let () = kill_process ~pid:validator_pid ~signal in
    let* () = wait_for_killing validator_pid in
    Log.info "External validator was killed by %a" pp_signal signal ;
    Log.info "Baking a block with a dead validator" ;
    let* () = Client.bake_for_and_wait client in
    let* new_validator_pid = wait_for_new_validator_pid in
    let* level = Node.wait_for_level node (level + 1) in
    return (level, new_validator_pid)
  in
  let* (_ : int * int) =
    Lwt_list.fold_left_s kill_loop (1, validator_pid) all_signals
  in
  unit

(* Test that canceling a bootstrap pipeline (e.g. due to a peer
   disconnection) does not restart the external validator process.
   This is a regression test for a bug where Lwt.Canceled exceptions
   in external_process.ml:send_request were mistaken for process
   crashes, causing unnecessary validator restarts that could lead to
   the node getting permanently stuck. *)
let test_cancel_does_not_restart_validator =
  Protocol.register_test
    ~__FILE__
    ~title:"external validator survives pipeline cancellation"
    ~tags:[team; "node"; "external"; "validator"; "cancel"; "bootstrap"]
  @@ fun protocol ->
  (* Setup: baker node that produces blocks, syncing node that bootstraps.
     Synchronisation_threshold 0 ensures client commands don't block
     waiting for bootstrapped status. *)
  let node_args = Node.[Synchronisation_threshold 0; Connections 1] in
  let baker_node = Node.create node_args in
  let syncing_node = Node.create node_args in
  (* Generate identity files so read_identity.v0 is emitted at startup,
     which is needed for Node.wait_for_identity to work. *)
  let* () = Node.identity_generate baker_node in
  let* () = Node.identity_generate syncing_node in
  let* () = Node.config_init baker_node [] in
  let* () = Node.config_init syncing_node [] in
  (* Set up event waiter before running so we don't miss the event *)
  let wait_for_validator_pid = wait_for_external_validator_pid syncing_node in
  let* () = Node.run baker_node [] in
  (* Add a delay to block application on syncing_node so that the
     pipeline cancellation (triggered by peer kick) happens while
     validation is still in progress. Without this, sandbox block
     validation is too fast (~3ms) and completes before the kick. *)
  let syncing_env = String_map.singleton "TEZOS_TEST_APPLY_DELAY" "2.0" in
  let* () = Node.run ~env:syncing_env syncing_node [] in
  let* () = Node.wait_for_ready baker_node in
  let* () = Node.wait_for_ready syncing_node in
  let* baker_client = Client.init ~endpoint:(Node baker_node) () in
  let* syncing_client = Client.init ~endpoint:(Node syncing_node) () in
  let* () = Client.activate_protocol_and_wait ~protocol baker_client in
  let* validator_pid = wait_for_validator_pid in
  Log.info "External validator pid: %d" validator_pid ;
  (* Connect and sync to level 1 *)
  let* () = Client.Admin.connect_address baker_client ~peer:syncing_node in
  let* (_ : int) = Node.wait_for_level syncing_node 1 in
  (* Disconnect syncing_node from baker *)
  let* baker_id = Node.wait_for_identity baker_node in
  let* () = Client.Admin.kick_peer ~peer:baker_id syncing_client in
  (* Bake blocks on baker_node while syncing_node is disconnected *)
  let blocks_to_bake = 8 in
  let* () =
    repeat blocks_to_bake (fun () ->
        Client.bake_for_and_wait ~minimal_timestamp:true baker_client)
  in
  Log.info "Baked %d blocks on baker node" blocks_to_bake ;
  (* Set up event monitors BEFORE reconnecting.
     - validator_restarted: detects if the external validator process
       was unnecessarily restarted (the bug this test guards against)
     - request_canceled: confirms the cancellation reached the
       send_request handler in external_process.ml *)
  let validator_restarted =
    Node.wait_for
      syncing_node
      "validator_hypervisee_initialized.v0"
      (fun json ->
        let new_pid = JSON.(json |> as_int) in
        if new_pid <> validator_pid then Some new_pid else None)
  in
  let request_canceled =
    Node.wait_for syncing_node "validator_request_canceled.v0" (fun _json ->
        Some ())
  in
  let validating_block =
    Node.wait_for syncing_node "validating_block.v0" (fun _json -> Some ())
  in
  (* Reconnect — triggers bootstrap pipeline *)
  let* () = Client.Admin.connect_address syncing_client ~peer:baker_node in
  Log.info "Reconnected syncing node to baker node" ;
  (* Wait for validation to start on syncing node *)
  let* () = validating_block in
  Log.info "Validation started on syncing node" ;
  (* Kick baker peer to cancel the bootstrap pipeline during validation *)
  let* () = Client.Admin.kick_peer ~peer:baker_id syncing_client in
  Log.info "Kicked baker peer to trigger pipeline cancellation" ;
  (* Wait for the request_canceled event, which confirms the
     cancellation was handled by the fix in external_process.ml *)
  let* () = Lwt_unix.sleep 3.0 in
  (match Lwt.state request_canceled with
  | Lwt.Return () ->
      Log.info "Cancellation handled correctly by external process"
  | Lwt.Fail _ | Lwt.Sleep ->
      Log.info "No cancellation event (validation may have completed first)") ;
  (* Assert the external validator was NOT restarted *)
  (match Lwt.state validator_restarted with
  | Lwt.Return new_pid ->
      Test.fail
        "External validator was restarted (old pid: %d, new pid: %d). \
         Lwt.Canceled should not trigger a validator restart."
        validator_pid
        new_pid
  | Lwt.Fail _ | Lwt.Sleep -> ()) ;
  Log.info "External validator was not restarted (good)" ;
  (* Reconnect and verify the node can still sync *)
  let* () = Client.Admin.connect_address syncing_client ~peer:baker_node in
  let* (_ : int) = Node.wait_for_level syncing_node (1 + blocks_to_bake) in
  Log.info "Syncing node caught up to level %d" (1 + blocks_to_bake) ;
  unit

let register ~protocols =
  (* If the singleprocess was not activated through the
     schedule_extended_validation_test pipeline, no need to run any
     check, the test is not even registered. *)
  if Node.enable_singleprocess then ()
  else (
    test_kill protocols ;
    test_cancel_does_not_restart_validator protocols)
