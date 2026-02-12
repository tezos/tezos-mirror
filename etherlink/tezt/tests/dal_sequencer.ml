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
                 ./scripts/install_dal_trusted_setup.sh
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file dal_sequencer.ml
*)

open Test_helpers
open Setup
open Rpc.Syntax

let register_test =
  register_test_for_kernels ~__FILE__ ~enable_dal:true ~enable_multichain:false

let count_event ?(get_count_from_event = fun _event -> 1) sequencer event
    counter =
  Evm_node.wait_for sequencer event (fun json ->
      counter := !counter + get_count_from_event json ;
      (* We return None here to keep the loop running *)
      None)

let count_blueprint_sent_on_inbox sequencer counter =
  count_event sequencer "blueprint_injection_on_inbox.v0" counter

(* This test is similar to {Evm_sequencer.test_publish_blueprints} but it also checks
   that all 5 blueprints sent from the sequencer were published on the
   DAL (and none on the inbox). *)
let test_publish_blueprints_on_dal ~dal_slot =
  register_test
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"]
    ~title:
      (sf "Sequencer publishes the blueprints on DAL slot index %d" dal_slot)
    ~dal_slots:(Some [dal_slot])
  (* We want this test in the CI so we put no extra tags when DAL
     is active to avoid having the [ci_disabled] or [slow] tag. *)
  @@ fun {sequencer; client; sc_rollup_node; enable_dal; _} _protocol ->
  let number_of_blueprints = 5 in

  let number_of_blueprints_sent_to_inbox = ref 0 in
  let number_of_blueprints_sent_to_dal = ref 0 in
  let number_of_signals = ref 0 in

  let inbox_counter_p =
    count_blueprint_sent_on_inbox sequencer number_of_blueprints_sent_to_inbox
  in

  let dal_counter_p =
    count_event
      sequencer
      "blueprint_injection_on_DAL.v0"
      number_of_blueprints_sent_to_dal
  in

  let signal_counter_p =
    count_event
      ~get_count_from_event:(fun event ->
        JSON.(event |-> "signals" |> as_list |> List.length))
      sequencer
      "signal_publisher_signal_signed.v0"
      number_of_signals
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
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer () in

  let* () =
    (* bake 2 block when DAL is enabled so evm_node sees it as
       finalized in `rollup_node_follower` *)
    if enable_dal then
      repeat 2 (fun () ->
          let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          unit)
    else unit
  in

  let* () =
    check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()
  in
  let expected_nb_of_bp_on_dal, expected_nb_of_bp_on_inbox =
    if enable_dal then (number_of_blueprints, 0) else (0, number_of_blueprints)
  in
  let expected_nb_of_signals =
    1
    (* We sent few blueprints and they are empty. They all fit in a single DAL slot. *)
  in
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

(* This test verifies DAL publishers whitelist for protocols > 024.
   It checks that only whitelisted publishers can have their DAL slots processed. *)
let test_publish_blueprints_on_dal_with_whitelist ~dal_slot
    ~dal_publishers_whitelist =
  let default_publisher = Constant.bootstrap1.public_key_hash in
  let publisher_whitelisted =
    List.mem default_publisher dal_publishers_whitelist
  in
  let number_of_blueprints = 5 in
  (* When publisher is not whitelisted, reduce lag threshold to trigger catch-up
     quickly.  Catch-up triggers when [lag > max_blueprints_lag], so use
     [number_of_blueprints - 1] to ensure it triggers before we run out of
     blueprints. *)
  let max_blueprints_lag =
    if publisher_whitelisted then None else Some (number_of_blueprints - 1)
  in
  register_test
    ~time_between_blocks:Nothing
    ?max_blueprints_lag
    ~tags:["evm"; "sequencer"; "data"; "whitelist"]
    ~title:
      (sf
         "Sequencer publishes the blueprints on DAL slot index %d; publisher \
          whitelisted: %b (whitelist length: %d)"
         dal_slot
         publisher_whitelisted
         (List.length dal_publishers_whitelist))
    ~dal_slots:(Some [dal_slot])
    ~dal_publishers_whitelist
  @@ fun {sequencer; client; sc_rollup_node; enable_dal; _} _protocol ->
  Log.info "Verify whitelist is correctly configured in kernel storage" ;
  let* () =
    Log.info "Checking DAL publishers whitelist in kernel storage" ;
    let* whitelist_hex =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:"/evm/dal_publishers_whitelist"
           ()
    in
    match whitelist_hex with
    | Some hex ->
        (* Decode RLP-encoded whitelist *)
        let rlp_bytes = Hex.to_bytes (`Hex hex) in
        let open Evm_node_lib_dev_encoding.Rlp in
        let rlp_item = decode_exn rlp_bytes in
        let stored_keys =
          match rlp_item with
          | List items ->
              List.map
                (fun item ->
                  match item with
                  | Value pkh_bytes ->
                      (* Decode binary PublicKeyHash to base58check *)
                      let pkh =
                        Data_encoding.Binary.of_bytes_exn
                          Tezos_crypto.Signature.Public_key_hash.encoding
                          pkh_bytes
                      in
                      Tezos_crypto.Signature.Public_key_hash.to_b58check pkh
                  | List _ ->
                      Test.fail "Expected Value in whitelist RLP, got List")
                items
          | Value _ -> Test.fail "Expected List in whitelist RLP, got Value"
        in
        Check.(
          (stored_keys = dal_publishers_whitelist)
            ~__LOC__
            (list string)
            ~error_msg:"Expected %R in whitelist, got %L") ;
        Log.info "Whitelist in storage: [%s]" (String.concat ", " stored_keys) ;
        unit
    | None -> Test.fail "Empty whitelist in kernel storage!"
  in

  let number_of_blueprints_sent_to_inbox = ref 0 in
  let number_of_blueprints_sent_to_dal = ref 0 in

  let inbox_counter_p =
    count_blueprint_sent_on_inbox sequencer number_of_blueprints_sent_to_inbox
  in

  let dal_counter_p =
    count_event
      sequencer
      "blueprint_injection_on_DAL.v0"
      number_of_blueprints_sent_to_dal
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
     all the blueprints. *)
  let* () =
    if publisher_whitelisted then
      (* Whitelisted: sync should happen quickly via DAL *)
      let* () =
        bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer ()
      in
      (* bake 2 blocks so evm_node sees it as finalized in `rollup_node_follower` *)
      repeat 2 (fun () ->
          let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          unit)
    else
      let* () =
        Log.info
          "Baking blocks (expecting desync due to whitelist rejection, \
           catch-up at lag=%d)"
          (number_of_blueprints - 1) ;
        repeat 8 (fun () ->
            let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
            unit)
      in
      Log.info
        "Waiting for catch-up mechanism to republish %d blueprints to inbox..."
        number_of_blueprints ;
      let rec wait_for_inbox_blueprints attempts =
        if !number_of_blueprints_sent_to_inbox >= number_of_blueprints then (
          Log.info
            "Catch-up complete: %d blueprints sent to inbox"
            !number_of_blueprints_sent_to_inbox ;
          unit)
        else if attempts <= 0 then
          Test.fail
            "Catch-up incomplete after timeout: only %d/%d blueprints sent to \
             inbox"
            !number_of_blueprints_sent_to_inbox
            number_of_blueprints
        else
          let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          wait_for_inbox_blueprints (attempts - 1)
      in
      wait_for_inbox_blueprints 10
  in

  (* Note: check_rollup_head_consistency may fail when not whitelisted due to desync *)
  let* () =
    if publisher_whitelisted then
      check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()
    else (
      Log.info
        "Skipping consistency check (desync expected when publisher not \
         whitelisted)" ;
      unit)
  in

  (* Expected behavior:
     - Batcher always publishes to DAL (independent of kernel whitelist)
     - Kernel processes DAL slots only if publisher is whitelisted
     - If rejected by whitelist, catch-up mechanism republishes via inbox *)
  let expected_nb_of_bp_on_dal = number_of_blueprints in
  let expected_nb_of_bp_on_inbox =
    if publisher_whitelisted then 0 else number_of_blueprints
  in

  Log.info
    "Blueprints sent to DAL: %d (expected: %d)"
    !number_of_blueprints_sent_to_dal
    expected_nb_of_bp_on_dal ;
  Log.info
    "Blueprints sent to inbox: %d (expected: %d)"
    !number_of_blueprints_sent_to_inbox
    expected_nb_of_bp_on_inbox ;

  Check.(expected_nb_of_bp_on_dal = !number_of_blueprints_sent_to_dal)
    ~__LOC__
    Check.int
    ~error_msg:
      "Wrong number of blueprints published on the DAL; Expected %L, got %R." ;

  (* For inbox: when not whitelisted, catch-up may republish, so check >= *)
  if publisher_whitelisted then
    Check.(expected_nb_of_bp_on_inbox = !number_of_blueprints_sent_to_inbox)
      ~__LOC__
      Check.int
      ~error_msg:
        "Wrong number of blueprints published on the inbox; Expected %L, got \
         %R."
  else
    Check.(
      (!number_of_blueprints_sent_to_inbox >= expected_nb_of_bp_on_inbox) int)
      ~error_msg:
        "Expected at least %R blueprints on inbox (catch-up may republish), \
         got %L" ;

  Lwt.cancel dal_counter_p ;
  Lwt.cancel inbox_counter_p ;
  unit

(* We aim to send data big enough to trigger splitting in multiple
   chunks, or multiple slots. Chunks size depends on the size of
   messages in the inbox, which is for now of 4096 bytes. We build
   transactions that are far bigger so that we can be sure they will
   be added to a blueprint that will be chunked. *)
let build_transaction_with_large_data
    ?(source_private_key = Eth_account.bootstrap_accounts.(0).private_key)
    ?(to_public_key = Eth_account.bootstrap_accounts.(0).address) sequencer =
  let data = String.make 100_000 '0' in
  Eth_cli.transaction_send
    ~source_private_key
    ~to_public_key
    ~value:Wei.zero
    ~data
    ~endpoint:(Evm_node.endpoint sequencer)

let test_chunked_blueprints_on_dal =
  register_test
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "chunks"]
    ~title:
      "Sequencer publishes entire blueprints of more than one chunk to the DAL"
  @@ fun {sequencer; sc_rollup_node; client; _} _protocol ->
  let number_of_blueprints_sent_to_inbox = ref 0 in

  let inbox_counter_p =
    count_blueprint_sent_on_inbox sequencer number_of_blueprints_sent_to_inbox
  in

  let* _tx_hash =
    send_transaction_to_sequencer
      (build_transaction_with_large_data sequencer)
      sequencer
  and* _level, nb_chunks =
    Evm_node.wait_for_blueprint_injected_on_dal sequencer
  in
  Check.(
    (nb_chunks >= 2)
      int
      ~error_msg:
        "The number of chunks injected should be at least %R but it is %L") ;
  (* bake until the sequencer and the rollup are in sync, to assess the input
     has been read correctly. *)
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer () in
  Check.(
    (!number_of_blueprints_sent_to_inbox = 0)
      int
      ~error_msg:
        "No blueprint should've been sent through the inbox, otherwise it \
         means that the data has been sent via the catchup mechanism") ;
  Lwt.cancel inbox_counter_p ;
  unit

let test_more_than_one_slot_per_l1_level =
  register_test
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "chunks"]
    ~title:
      "Sequencer publishes entire blueprints on more than one slot to the DAL"
  @@ fun {sequencer; sc_rollup_node; client; _} protocol ->
  let number_of_blueprints_sent_to_inbox = ref 0 in
  let inbox_counter_p =
    count_blueprint_sent_on_inbox sequencer number_of_blueprints_sent_to_inbox
  in
  let nb_transac = 8 in
  let sends =
    List.init nb_transac (fun i () ->
        build_transaction_with_large_data
          ~source_private_key:Eth_account.bootstrap_accounts.(i).private_key
          sequencer
          ())
  in
  let* n, _txs = send_transactions_to_sequencer ~sends sequencer in
  Check.((n = nb_transac) int)
    ~error_msg:
      (Format.sprintf "Expected %d transactions in the block" nb_transac) ;
  (* For protocols <= 024 (Tallinn), legacy DAL signals are used and we can wait for them.
     For protocols > 024, signals are disabled and replaced with DalAttestedSlots. *)
  if Protocol.number protocol <= 024 then (
    let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer ()
    and* _smart_rollup_address, signals =
      Evm_node.wait_for_signal_signed sequencer
    in
    (* We check that there was a level at which at least 2 slots were
       published. We do so by turning the list of signals into a map of
       levels (ML) associating to each level a set of the signaled slot
       (SSL) indices published at this level. *)
    let module SSL = Set.Make (Int) in
    let module ML = Map.Make (Int) in
    let m =
      List.fold_left
        (fun acc (slot_index, published_level) ->
          ML.update
            published_level
            (Option.fold
               ~none:(Some (SSL.singleton slot_index))
               ~some:(fun slots -> Some (SSL.add slot_index slots)))
            acc)
        ML.empty
        signals
    in
    let more_than_one_slot =
      ML.exists (fun _ signaled_slots -> SSL.cardinal signaled_slots >= 2) m
    in
    Check.(
      is_true
        more_than_one_slot
        ~error_msg:
          "Only one DAL slot has been used for each L1 level, expected at \
           least two.") ;
    Check.(
      (!number_of_blueprints_sent_to_inbox = 0)
        int
        ~error_msg:
          "No blueprint should've been sent through the inbox, otherwise it \
           means that the data has been sent via the catchup mechanism") ;
    Lwt.cancel inbox_counter_p ;
    unit)
  else
    (* When legacy signals are disabled, just check that baking completes successfully
       and no blueprints were sent to inbox. The DalAttestedSlots mechanism
       doesn't emit signals to check. *)
    let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer () in
    (* Bake 2 more blocks so DalAttestedSlots messages are processed with finality *)
    let* () =
      repeat 2 (fun () ->
          let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          unit)
    in
    Check.(
      (!number_of_blueprints_sent_to_inbox = 0)
        int
        ~error_msg:
          "No blueprint should've been sent through the inbox, otherwise it \
           means that the data has been sent via the catchup mechanism") ;
    Lwt.cancel inbox_counter_p ;
    unit

let protocols = Protocol.all

(* Split protocols: legacy DAL signals (≤ 024) vs DalAttestedSlots with
   whitelist (> 024) *)
let protocols_legacy_dal =
  List.filter (fun p -> Protocol.number p <= 024) protocols

let protocols_with_whitelist =
  List.filter (fun p -> Protocol.number p > 024) protocols

let () =
  (* Test legacy DAL signals for protocols ≤ 024 *)
  test_publish_blueprints_on_dal ~dal_slot:4 protocols_legacy_dal ;
  (* Also run the test for slot index 0 because it is a particular
     case in the RLP encoding used to send signals to the rollup. *)
  test_publish_blueprints_on_dal ~dal_slot:0 protocols_legacy_dal ;

  (* Test DalAttestedSlots with whitelist for protocols after Tallinn (> 024) *)
  (* Test acceptance: bootstrap1 is whitelisted *)
  test_publish_blueprints_on_dal_with_whitelist
    ~dal_slot:4
    ~dal_publishers_whitelist:[Constant.bootstrap1.public_key_hash]
    protocols_with_whitelist ;
  (* Test rejection: only bootstrap2 is whitelisted, so bootstrap1's slots are rejected *)
  test_publish_blueprints_on_dal_with_whitelist
    ~dal_slot:0
    ~dal_publishers_whitelist:[Constant.bootstrap2.public_key_hash]
    protocols_with_whitelist ;
  (* Test rejection: empty whitelist, bootstrap1's slots are rejected *)
  test_publish_blueprints_on_dal_with_whitelist
    ~dal_slot:0
    ~dal_publishers_whitelist:[]
    protocols_with_whitelist ;
  (* Test acceptance with multiple publishers in whitelist *)
  test_publish_blueprints_on_dal_with_whitelist
    ~dal_slot:0
    ~dal_publishers_whitelist:
      [Constant.bootstrap2.public_key_hash; Constant.bootstrap1.public_key_hash]
    protocols_with_whitelist ;

  test_chunked_blueprints_on_dal protocols ;
  test_more_than_one_slot_per_l1_level protocols
