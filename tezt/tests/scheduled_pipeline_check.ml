(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Scheduled pipeline enabling check
   Invocation:   dune exec tezt/tests/main.exe -- --file
                 scheduled_pipeline_check.ml
   Subject:      Tests the scheduled extended test parameters enabling
*)

let test_scheduled_extended_rpc_test () =
  Test.register
    ~title:"Test scheduled extended rpc test enabling"
    ~tags:["scheduled"; "pipeline"; "rpc"; "external"]
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
  @@ fun () ->
  (* If the flag was enabled, we check that the node is running with
     the external RPC process, as expected. *)
  let node = Node.create [] in
  let wait_external_rpc_process =
    Node.wait_for node "starting_rpc_server.v0" @@ fun (_ : JSON.t) -> Some ()
  in
  Log.info "Starting the node with the default config" ;
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let timeout =
    let* () = Lwt_unix.sleep 1. in
    Test.fail "Missing external RPC server event"
  in
  Log.info "Wait for the external RPC server event" ;
  let* () = Lwt.choose [wait_external_rpc_process; timeout] in
  let* () = Node.terminate node in
  unit

let test_scheduled_extended_validation_test () =
  Test.register
    ~title:"Test scheduled extended validation test enabling"
    ~tags:["scheduled"; "pipeline"; "singleprocess"; "validation"]
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
  @@ fun () ->
  (* If the flag was enabled, we check that the node is running with
     the singleprocess, as expected. *)
  let node = Node.create [] in
  let wait_singleprocess =
    Node.wait_for node "seq_initialized.v0" @@ fun (_ : JSON.t) -> Some ()
  in
  Log.info "Starting the node with the default config" ;
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  (* Use a timeout to avoid a hanging test if the event is
     missing. *)
  let timeout =
    let* () = Lwt_unix.sleep 1. in
    Test.fail "Missing single process event"
  in
  Log.info "Wait for the single process event" ;
  let* () = Lwt.choose [wait_singleprocess; timeout] in
  let* () = Node.terminate node in
  unit

(* These tests aim to check the behaviour of the Tezt nodes in the
   context of extended tests that are running in a scheduled
   pipeline. In such a pipeline, the TZ_SCHEDULE_KIND environment
   variable is set to a specific value (such as "EXTENDED_RPC_TESTS"
   or "EXTENDED_VALIDATION_TESTS") so that the Tezt nodes must enable
   a given feature by default (such as the external RPC process or
   the single process validation). This test checks this assumption to
   avoid false positives where the scheduled pipelines may not enable
   the feature, as expected. *)
let register_protocol_independent () =
  (* If the external RPC process was not activated through the
     schedule_extended_rpc_test pipeline, no need to run any check,
     the test is not even registered. *)
  if not Node.enable_external_rpc_process then ()
  else test_scheduled_extended_rpc_test () ;
  (* If the singleprocess was not activated through the
     schedule_extended_validation_test pipeline, no need to run any
     check, the test is not even registered. *)
  if not Node.enable_singleprocess then ()
  else test_scheduled_extended_validation_test ()
