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

let register ~protocols =
  (* If the singleprocess was not activated through the
     schedule_extended_validation_test pipeline, no need to run any
     check, the test is not even registered. *)
  if Node.enable_singleprocess then () else test_kill protocols
