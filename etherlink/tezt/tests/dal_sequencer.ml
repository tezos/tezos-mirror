(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer + DAL
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make -f etherlink.mk octez-dsn-node
                 ./scripts/install_dal_trusted_setup.sh
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file dal_sequencer.ml
*)

open Helpers
open Setup
open Rpc.Syntax

let register_test =
  register_test_for_kernels
    ~__FILE__
    ~enable_dal:true
    ~threshold_encryption:false

(* This test is similar to {Evm_sequencer.test_publish_blueprints} but it also checks
   that all 5 blueprints sent from the sequencer were published on the
   DAL (and none on the inbox). *)
let test_publish_blueprints_on_dal =
  register_test
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer publishes the blueprints to the DAL"
  (* We want this test in the CI so we put no extra tags when DAL
     is active to avoid having the [ci_disabled] or [slow] tag. *)
  @@
  fun {sequencer; proxy; client; sc_rollup_node; enable_dal; _} _protocol ->
  let number_of_blueprints = 5 in

  let number_of_blueprints_sent_to_inbox = ref 0 in
  let number_of_blueprints_sent_to_dal = ref 0 in
  let number_of_signals = ref 0 in

  let count_event event counter =
    Evm_node.wait_for sequencer event (fun _json ->
        incr counter ;
        (* We return None here to keep the loop running *)
        None)
  in

  let inbox_counter_p =
    count_event
      "blueprint_injection_on_inbox.v0"
      number_of_blueprints_sent_to_inbox
  in

  let dal_counter_p =
    count_event "blueprint_injection_on_DAL.v0" number_of_blueprints_sent_to_dal
  in

  let signal_counter_p =
    count_event "signal_publisher_signal_signed.v0" number_of_signals
  in

  let* _ =
    repeat number_of_blueprints (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in

  (* Wait more to avoid flakiness, in particular with DAL *)
  let timeout = if enable_dal then 50. else 5. in
  let* () =
    Evm_node.wait_for_blueprint_injected ~timeout sequencer number_of_blueprints
  in

  (* At this point, the evm node should call the batcher endpoint to publish
     all the blueprints. Stopping the node is then not a problem. *)
  let* () =
    bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer ~proxy ()
  in

  let* () =
    (* bake 2 block when DAL is enabled so evm_node sees it as
       finalized in `rollup_node_follower` *)
    if enable_dal then
      repeat 2 (fun () ->
          let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in
          unit)
    else unit
  in

  (* We have unfortunately noticed that the test can be flaky. Sometimes,
     the following RPC is done before the proxy being initialised, even though
     we wait for it. The source of flakiness is unknown but happens very rarely,
     we put a small sleep to make the least flaky possible. *)
  let* () = Lwt_unix.sleep 2. in
  let* () = check_head_consistency ~left:sequencer ~right:proxy () in
  let expected_nb_of_bp_on_dal, expected_nb_of_bp_on_inbox =
    if enable_dal then (number_of_blueprints, 0) else (0, number_of_blueprints)
  in
  let expected_nb_of_signals = expected_nb_of_bp_on_dal in
  Check.(expected_nb_of_bp_on_dal = !number_of_blueprints_sent_to_dal)
    ~__LOC__
    Check.int
    ~error_msg:
      "Wrong number of blueprints published on the DAL; Expected %L, got %R." ;
  Check.(expected_nb_of_signals = !number_of_signals)
    ~__LOC__
    Check.int
    ~error_msg:"Wrong number of signals signed; Expected %L, got %R." ;
  Check.(expected_nb_of_bp_on_inbox = !number_of_blueprints_sent_to_inbox)
    ~__LOC__
    Check.int
    ~error_msg:
      "Wrong number of blueprints published on the inbox; Expected %L, got %R." ;
  Lwt.cancel dal_counter_p ;
  Lwt.cancel inbox_counter_p ;
  Lwt.cancel signal_counter_p ;
  unit

let protocols = Protocol.all

let () = test_publish_blueprints_on_dal protocols
