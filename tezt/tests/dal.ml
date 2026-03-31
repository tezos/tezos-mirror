(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Data-availability layer
   Requirement:  make -f kernels.mk build

                 For dev-purpose you can also run the follwing to enable debug outputs and faster build time:

                 cd src/kernel_tx_demo/kernel
                 cargo build --release --target wasm32-unknown-unknown --features debug --features dal
                 wasm-strip ../target/wasm32-unknown-unknown/release/tx_kernel.wasm
                 cp ../target/wasm32-unknown-unknown/release/tx_kernel.wasm ../../../tx_kernel_dal.wasm

                 ./scripts/install_dal_trusted_setup.sh

   Invocation:   dune exec tezt/tests/main.exe -- --file dal.ml
   Subject: Integration tests related to the data-availability layer
*)

open Dal_helpers
module Dal = Dal_common
module Profiler = Tezos_profiler.Profiler

let dal_crypto_benchmark () =
  Test.register
    ~__FILE__
    ~title:"Benchmark of the DAL cryptographic primitives"
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~tags:["dal"; "benchmark"; "crypto"; Tag.slow; team]
  @@ fun () ->
  let open Dal.Cryptobox in
  let driver =
    Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
  in
  let file =
    match Cli.Logs.level with
    | Info | Debug -> "/dev/stdout"
    | _ -> Temp.file "profiler-output"
  in
  let instance = Profiler.instance driver (file, Debug) in
  Profiler.plug Profiler.main instance ;
  let ( let*? ) x f =
    match x with
    | Error err -> Test.fail "Unexpected error:@.%a@." Cryptobox.pp_error err
    | Ok x -> f x
  in
  (* the defaults are the Rio parameters *)
  let number_of_shards = Cli.get_int ~default:512 "nb_shards" in
  let slot_size = Cli.get_int ~default:126_944 "slot_size" in
  let redundancy_factor = Cli.get_int ~default:8 "redundancy" in
  let page_size = Cli.get_int ~default:3967 "page_size" in
  let traps_fraction =
    Cli.get_float ~default:0.0005 "traps_fraction" |> Q.of_float
  in
  let* () =
    let parameters =
      {number_of_shards; redundancy_factor; page_size; slot_size}
    in
    let message =
      Format.asprintf
        "(shards: %d, slot size: %d, redundancy_factor: %d, page size: %d)"
        number_of_shards
        slot_size
        redundancy_factor
        page_size
    in
    let* () =
      Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("SRS", [])
      @@ fun () ->
      Log.info "Loading SRS..." ;
      let* result =
        init_prover_dal
          ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
          ~fetch_trusted_setup:false
          ()
      in
      Log.info "SRS loaded." ;
      let*? () =
        Result.map_error
          (fun x ->
            `Fail
              (Format.asprintf
                 "%a"
                 Tezos_error_monad.Error_monad.pp_print_trace
                 x))
          result
      in
      unit
    in
    Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
    @@ fun () ->
    match make parameters with
    | Error (`Fail msg) ->
        let message = Format.asprintf "Fail: %s" msg in
        Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
        @@ fun () -> Lwt.return_unit
    | Ok _ ->
        let*? dal =
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("make", [])
          @@ fun () -> make parameters
        in
        let*? precomputation =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("shard precomputation", [])
          @@ fun () -> precompute_shards_proofs dal
        in
        let slot =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("slot generation", [])
          @@ fun () -> Cryptobox.Internal_for_tests.generate_slot ~slot_size
        in
        let*? polynomial =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("polynomial from slot", [])
          @@ fun () -> polynomial_from_slot dal slot
        in
        let _slot =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("polynomial to slot", [])
          @@ fun () -> polynomial_to_slot dal polynomial
        in
        let*? commitment =
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug ("commit", [])
          @@ fun () -> commit dal polynomial
        in
        let*? commitment_proof =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("prove commitment", [])
          @@ fun () -> prove_commitment dal polynomial
        in
        let shards =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("shards from polynomial", [])
          @@ fun () -> shards_from_polynomial dal polynomial
        in
        let shard_proofs =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("prove shards", [])
          @@ fun () ->
          prove_shards dal ~precomputation ~polynomial |> Array.to_seq
        in
        let _polynomial =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("reconstruct polynomial", [])
          @@ fun () -> polynomial_from_shards dal shards
        in
        let nb_pages = slot_size / page_size in
        let page_proofs =
          Seq.ints 0 |> Seq.take 1
          |> Seq.map (fun i ->
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   ("prove page", [])
                 @@ fun () ->
                 let*? page_proof = prove_page dal polynomial i in
                 page_proof)
        in
        let is_valid =
          Profiler.record_f
            ~cpu:(Some true)
            Profiler.main
            Debug
            ("verify commitment", [])
          @@ fun () -> verify_commitment dal commitment commitment_proof
        in
        assert is_valid ;
        let () =
          Seq.zip shards shard_proofs
          |> Seq.take 1
          |> Seq.iter (fun (shard, shard_proof) ->
                 let message =
                   Format.asprintf
                     "verify shard (size: %d)"
                     (Bytes.length
                        (Data_encoding.Binary.to_bytes_exn
                           share_encoding
                           shard.share))
                 in
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   (message, [])
                 @@ fun () ->
                 let*? () = verify_shard dal commitment shard shard_proof in
                 ())
        in
        let bench_i_shard_verif i =
          Seq.zip shards shard_proofs |> Seq.take i |> List.of_seq |> List.split
          |> fun (shard_list, shard_proof_list) ->
          let message =
            Format.asprintf
              "verify shard multi (size: %d) (number_of_shards:%d)"
              (Bytes.length
                 (Data_encoding.Binary.to_bytes_exn
                    share_encoding
                    (List.hd shard_list).share))
              (List.length shard_list)
          in
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
          @@ fun () ->
          let*? () =
            verify_shard_multi dal commitment shard_list shard_proof_list
          in
          ()
        in
        bench_i_shard_verif 1 ;
        bench_i_shard_verif 5 ;
        bench_i_shard_verif 20 ;
        bench_i_shard_verif 100 ;
        bench_i_shard_verif 2048 ;
        let pages =
          Seq.ints 0 |> Seq.take nb_pages
          |> Seq.map (fun i -> Bytes.sub slot (i * page_size) page_size)
        in
        let () =
          Seq.zip (Seq.ints 0 |> Seq.take nb_pages) (Seq.zip pages page_proofs)
          |> Seq.take 1
          |> Seq.iter (fun (page_index, (page, page_proof)) ->
                 Profiler.record_f
                   ~cpu:(Some true)
                   Profiler.main
                   Debug
                   ("verify page", [])
                 @@ fun () ->
                 let*? () =
                   verify_page dal commitment ~page_index page page_proof
                 in
                 ())
        in
        let () =
          let message =
            sf "share_is_trap (number_of_shards:%d)" (Seq.length shards)
          in
          Profiler.record_f ~cpu:(Some true) Profiler.main Debug (message, [])
          @@ fun () ->
          shards
          |> Seq.iter (fun {share; index = _} ->
                 let res =
                   Tezos_crypto_dal.Trap.share_is_trap
                     Tezos_crypto.Signature.Public_key_hash.zero
                     share
                     ~traps_fraction
                 in
                 match res with
                 | Ok _is_trap -> ()
                 | Error err ->
                     Test.fail
                       "Unexpected error:@.%a@."
                       Data_encoding.Binary.pp_write_error
                       err)
        in
        Lwt.return_unit
  in
  Profiler.close_and_unplug Profiler.main instance ;
  Lwt.return_unit

(*
 * Test proposing to setup a node, and dal node, a baker, in multiple steps.
 * The scenario tries to follow the tutorial steps described in
 *    https://docs.tezos.com/tutorials/join-dal-baker
 * that concerns weeklynet. We use a sandbox node to simplify
 * Regressions in this test can highlight the tutorial is out of sync with
 * the octez components behavior, and the tutorial might have to be fixed.
 * an issue/pull request should be opened to
 * https://github.com/trilitech/tezos-developer-docs/ in these cases, in order
 * to change the tutorial
 *)
let scenario_tutorial_dal_baker =
  let description = "Test following dal and baker tutorial commands" in
  test
    ~regression:true
    ~__FILE__
    ~tags:[team; "tutorial"; "dal"; "baker"]
    ~uses:(fun _protocol ->
      [Constant.octez_agnostic_baker; Constant.octez_dal_node])
    (Printf.sprintf "%s" description)
    (fun protocol ->
      (* Note: Step 1 consists in setting up docker which we don't use
       * in this test
       *)
      Log.info "Step 2: Running octez node with adaptive issuance" ;
      with_layer1 ~event_sections_levels:[("prevalidator", `Debug)] ~protocol
      @@ fun _parameters _cryptobox node client _key ->
      Log.info "Step 3: setup a baker account" ;
      (* Generate the "my_baker" user *)
      let* my_baker = Client.gen_and_show_keys ~alias:"my_baker" client in

      (* Transfer 500k to "my_baker"
       * This value differs from the tutorial to obtain attestation rights *)
      let stake = Tez.of_int 500_000 in
      let* () =
        Client.transfer
          ~hooks
          ~giver:Constant.bootstrap1.alias
          ~receiver:my_baker.alias
          ~amount:stake
          ~burn_cap:Tez.one
          client
      in
      let* () = bake_for client in

      (* Check adaptive issuance is enabled, should be 0 *)
      let* adaptive_issuance_launch_cycle =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
      in
      Check.(
        (Option.get adaptive_issuance_launch_cycle = 0)
          ~__LOC__
          Check.int
          ~error_msg:
            "Adaptive issuance will only be launched at cycle %L, expected: %R") ;

      let* balance = Client.get_balance_for ~account:my_baker.alias client in
      Check.(balance = stake)
        ~__LOC__
        Tez.typ
        ~error_msg:"Unexpected balance for 'mybaker'. Expected: %L. Got: %R" ;

      (* Register my_baker as a baker *)
      let* () = Client.register_key ~hooks my_baker.alias client in
      let* () = bake_for client in

      (* As in the tutorial, we stake 99.8% of the balance. *)
      let* () =
        Client.stake
          ~hooks
          Tez.(stake - of_int 100)
          ~staker:my_baker.alias
          client
      in
      let* () = bake_for client in

      let* _ =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_delegate my_baker.public_key_hash
      in

      (* Calculate how many cycles to wait for my_baker to be a baker *)
      let* proto_params =
        Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
      in
      let blocks_per_cycle =
        JSON.(proto_params |-> "blocks_per_cycle" |> as_int)
      in
      let num_cycles =
        7
        (* Value specified in tutorial *)
      in
      Log.info
        "Bake for %d cycles for %s to be a baker"
        num_cycles
        my_baker.alias ;
      let* () = bake_for ~count:(num_cycles * blocks_per_cycle) client in

      let* attestation_rights =
        Node.RPC.call node
        @@ RPC.get_chain_block_helper_attestation_rights
             ~delegate:my_baker.public_key_hash
             ()
      in

      let attestation_power =
        attestation_rights |> JSON.as_list |> List.length
      in
      Log.info
        "Attestation rights: %s\n%!"
        (if attestation_power > 0 then "OK" else "KO") ;
      Check.(attestation_power > 0)
        Check.int
        ~__LOC__
        ~error_msg:"Attestation rights should be acquired" ;

      (* Only test the request can be processed *)
      let* _ =
        Node.RPC.call node @@ RPC.get_chain_block_context_dal_shards ()
      in

      (* Launch dal node (Step 4) *)
      Log.info "Step 4: Run an Octez dal node" ;
      let* dal_node = make_dal_node node in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics = 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a empty list of topics") ;

      let wait_join_event_promise =
        Dal_node.wait_for dal_node "dal_gs_join.v0" (fun _ -> Some ())
      in

      let all_delegates =
        Account.Bootstrap.keys |> Array.to_list |> List.cons my_baker
        |> List.map (fun key -> key.Account.alias)
      in
      Log.info "Step 5: Run an Octez baking daemon" ;
      let* _baker =
        let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
        Agnostic_baker.init
          ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
          ~dal_node_rpc_endpoint
          ~delegates:all_delegates
          ~liquidity_baking_toggle_vote:(Some On)
          ~state_recorder:true
          ~force_apply_from_round:0
          node
          client
      in
      (* Wait for subscribed events, as expected in the tutorial *)
      let* () = wait_join_event_promise in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics > 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a non-empty list of topcis") ;
      unit)

(** This test injects a DAL slot to (DAL and L1) network(s) via the rollup node
    using {!post_local_dal_injection} rollup RPC. It then checks that the slot
    is attested, which implies that the commitment is published to L1 and that
    the shards of the slot are declared available by the DAL node.  *)
let rollup_node_injects_dal_slots protocol parameters dal_node sc_node
    sc_rollup_address node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let slot_index = 0 in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[slot_index]
  in
  let wait_injected =
    Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
  in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection
         ~messages:["Hello DAL from a Smart Rollup"]
  in
  let* () = wait_injected in

  (* We check for each attestation lag whether the slot was attested. *)
  let* level = Client.level client in
  let published_level = level + 1 in
  let rec loop current_level lag_index lags =
    match lags with
    | [] -> Test.fail "Slot not attested"
    | lag :: lags ->
        let attested_level = published_level + lag in
        let* () = bake_for ~count:(attested_level - current_level) client in
        let* level = Client.level client in
        assert (level = attested_level) ;
        let* _level =
          Sc_rollup_node.wait_for_level ~timeout:10. sc_node level
        in
        let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
        let* obtained_dal_attestation =
          match metadata.dal_attestation with
          | None ->
              (* Field is part of the encoding when the feature flag is true *)
              Test.fail
                "Field dal_attestation in block headers is mandatory when DAL \
                 is activated"
          | Some str ->
              let* decoded =
                Dal.Slot_availability.decode
                  protocol
                  (Node.as_rpc_endpoint node)
                  parameters
                  str
              in
              return decoded.(lag_index)
        in
        if obtained_dal_attestation.(slot_index) then (
          let expected_attestation =
            expected_attestation parameters [slot_index]
          in
          Check.(
            (expected_attestation = obtained_dal_attestation)
              (array bool)
              ~error_msg:"Expected attestation bitset %L, got %R") ;
          let* statuses =
            Sc_rollup_node.RPC.call sc_node
            @@ Sc_rollup_rpc.get_dal_injected_operations_statuses ()
          in
          match statuses with
          | [status_with_hash] ->
              let status = JSON.get "status" status_with_hash in
              let status_str = JSON.(status |-> "status" |> as_string) in
              if status_str <> "included" && status_str <> "committed" then
                Test.fail
                  "Unexpected injector operation status %s. Expecting \
                   'included' or 'committed'"
                  status_str ;
              unit
          | _ ->
              Test.fail
                "Expecting a status for 1 operation, got %d@."
                (List.length statuses))
        else loop level (lag_index + 1) lags
  in
  loop level 0 parameters.attestation_lags

(** This test verifies the optimal publication of DAL slots from a batch of
    messages into the DAL node and L1 via the rollup node DAL injector.

    The test ensures that:
    - DAL slots are published at the appropriate slot indices (0, 1, 2) and levels;
    - Messages are grouped efficiently into DAL slots.

    More precisely, we inject a list of messages into the rollup node's DAL
    injector worker and check that:

    - The first four chunks (with a name prefix [slot0_] below) fit exactly into
    one slot. They are published at some level L, with slot index 0.

    - The next three chunks (with a name prefix [slot1_]), which don't fully
    fill a DAL slot, are combined into a slot and published at level L, with slot
    index 1. In fact, the following message in the queue (namely [slot2_ck1]) is
    one byte too big to fit in the same slot as these messages. So, the latter
    is published into slot index 2 of the same level L (it cannot be combined
    with the remaining messages as well).

    - All available slot indices being used at level L, the remaining messages
    are considered at level L+1. The two last messages are published at slot
    indices 0 and 1, respectively. Message [slot3_ck_full] fits in a slot, so
    there is no need to combine it with other slots. As for
    [slot4_ck_almost_empty], which only occupies 1 byte, there are no remaining
    messages that could be combined with it.

    The test checks that the correct number of DAL slots are published and
    verifies the length of the messages within the slots.
*)
let rollup_batches_and_publishes_optimal_dal_slots _protocol parameters dal_node
    sc_node sc_rollup_address _node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in

  (* We create some messages below with various sizes. *)
  Check.(
    (slot_size mod 4 = 0)
      int
      ~error_msg:"Expected DAL slots size to be a multiple of 4") ;
  let chunks_size = slot_size / 4 in

  let slot0_ck1 = String.make chunks_size 'A' in
  let slot0_ck2 = slot0_ck1 in
  let slot0_ck3 = slot0_ck2 in
  let slot0_ck4 = slot0_ck3 in

  let slot1_ck1 = String.make chunks_size 'B' in
  let slot1_ck2 = slot1_ck1 in
  let slot1_ck3 = slot1_ck2 in
  let slot2_ck1 = String.make (chunks_size + 1) 'C' in

  let slot3_ck_full = String.make slot_size 'D' in
  let slot4_ck_almost_empty = String.make 1 'E' in

  (* We define an order of addition of the messages into the DAL injection queue
     and group them in the way they are expected to be packed into DAL slots. *)
  let annotated_messages =
    [
      [slot0_ck1; slot0_ck2; slot0_ck3; slot0_ck4];
      [slot1_ck1; slot1_ck2; slot1_ck3];
      [slot2_ck1];
      [slot3_ck_full];
      [slot4_ck_almost_empty];
    ]
  in

  (* We derive from [annotated_messages] the raw list of messages that will be
     injected with RPC [post_local_dal_batcher_injection] below.*)
  let messages = List.concat annotated_messages in

  (* We define the expected number of total, valid and discarded messages as well
     as the number of expected DAL slots to publish. *)
  let total_num_messages = List.length messages in
  let expected_number_of_injected_dal_slots = List.length annotated_messages in

  (* We start by informing the DAL injector of the rollup node that we want to
     publish on slots indices 0, 1 and 2. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[0; 1; 2]
  in

  (* This promise will count the number of successfully injected messages. We
     expected seeing [total_num_messages] events [dal_message_received] before
     the promise resolves. *)
  let wait_dal_message_received =
    let countdown = ref total_num_messages in
    Sc_rollup_node.wait_for sc_node "dal_message_received.v0" (fun _ ->
        decr countdown ;
        if !countdown = 0 then Some () else None)
  in
  (* Given a number [expected_num_published_slots] of slots to publish, this
     function returns a promise that resolves once the corresponding number of
     expected events [inject_dal_slot_from_messages] are seen. The function
     returns the payloads of the events in a list as tuples: [(data_size,
     num_messages, published_level, slot_index)]. *)
  let wait_inject_dal_slot_from_messages ~expected_num_published_slots =
    let open JSON in
    let countdown = ref expected_num_published_slots in
    let published = ref [] in
    let* () =
      Sc_rollup_node.wait_for
        sc_node
        "inject_dal_slot_from_messages.v0"
        (fun json ->
          let data_size = get "data_size" json |> as_int in
          let num_messages = get "num_messages" json |> as_int in
          let level = get "level" json |> as_int in
          (* The slot was injected at leve [level]. We assume that it will be
             included at the next level, which will be the "published
             level". *)
          let published_level = level + 1 in
          let slot_index = get "slot_index" json |> as_int in
          published :=
            (data_size, num_messages, published_level, slot_index) :: !published ;
          decr countdown ;
          if !countdown = 0 then Some () else None)
    in
    Lwt.return (List.rev !published)
  in
  (* We finally inject the messages via post_local_dal_batcher_injection into
     the DAL injection queue. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection ~messages
  in

  (* We wait until all injected messages are processed. *)
  let* () = wait_dal_message_received in

  (* Before baking one block and triggering the three first slots publication,
     we create the promise that will track the [inject_dal_slot_from_messages]
     events and return injected slots information. *)
  let* published_slots_1 =
    let wait_inject_dal_slot_from_messages_first_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:3
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:15. sc_node level in
    wait_inject_dal_slot_from_messages_first_level
  in

  (* Once the three first slots are published, we do the same thing for the two
     other expected slots at the next level. *)
  let* published_slots_2 =
    let wait_inject_dal_slot_from_messages_second_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:2
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:10. sc_node level in
    wait_inject_dal_slot_from_messages_second_level
  in

  (* We sort the resulting published slots information by level and slot
     indices. *)
  let published_slots =
    published_slots_2 @ published_slots_1
    |> List.fast_sort
         (fun
           (_data_size, _num_messages, level, slot_index)
           (_data_size', _num_messages', level', slot_index')
         ->
           let c = level - level' in
           if c <> 0 then c else slot_index - slot_index')
  in

  (* We check that all messages are published. *)
  let count_published_messages =
    List.fold_left
      (fun acc (_data_size, num_messages, _level, _slot_index) ->
        num_messages + acc)
      0
      published_slots
  in
  Check.(
    (total_num_messages = count_published_messages)
      int
      ~error_msg:"Expected number of batched messages %L. Got %R") ;

  (* We check that the published number of DAL slots is as expected. *)
  Check.(
    (expected_number_of_injected_dal_slots = List.length published_slots)
      int
      ~error_msg:"Expected published DAL slots is %L. Got %R") ;

  (* Finally, we check that published slots have the expected size w.r.t. to our
     initial packs of messages. *)
  let rec check_packs published_slots annotated_messages =
    match (published_slots, annotated_messages) with
    | [], [] -> ()
    | ( (data_size, _num_messages, _level, _slot_index) :: published_slots,
        pack :: annotated_messages ) ->
        let sz = List.fold_left (fun sz msg -> sz + String.length msg) 0 pack in
        Check.(
          (sz = data_size)
            int
            ~error_msg:"Expected published DAL slots size is %L. Got %R") ;
        check_packs published_slots annotated_messages
    | [], _ | _, [] -> Test.fail "Unreachable case"
  in
  check_packs published_slots annotated_messages ;
  unit

let register ~protocols =
  (* Tests with Layer1 node only *)
  Dal_l1.register ~__FILE__ ~protocols ;
  Dal_attestation.register ~__FILE__ ~protocols ;
  Dal_denunciation.register ~__FILE__ ~protocols ;
  Dal_node_tests.register ~__FILE__ ~protocols ;

  (* Tests with layer1 and dal nodes (with p2p/GS) *)
  Dal_p2p.register ~__FILE__ ~protocols ;

  Dal_skip_list.register ~__FILE__ ~protocols ;
  Dal_amplification.register ~__FILE__ ~protocols ;
  Dal_gc.register ~__FILE__ ~protocols ;

  Dal_tx_kernel.register ~__FILE__ ~protocols ;

  (* Register tutorial test *)
  scenario_tutorial_dal_baker protocols ;

  scenario_with_all_nodes
    ~__FILE__
    "Rollup injects DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_node_injects_dal_slots
    ~operator_profiles:[0]
      (* It it sufficient for a single baker here to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  scenario_with_all_nodes
    ~__FILE__
    "Rollup batches and injects optimal DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_batches_and_publishes_optimal_dal_slots
    ~operator_profiles:[0; 1; 2]
      (* It it sufficient for a single baker to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  dal_crypto_benchmark ()

let register_migration ~migrate_from ~migrate_to =
  Dal_skip_list.register_migration ~__FILE__ ~migrate_from ~migrate_to ;
  Dal_amplification.register_migration ~__FILE__ ~migrate_from ~migrate_to ;
  Dal_migration.register_migration ~__FILE__ ~migrate_from ~migrate_to

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: debug print store schemas"
    ~tags:["dal"; "store"; "schemas"]
    ~uses:[Constant.octez_dal_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Dal_node.debug_print_store_schemas ~hooks ()

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: P2P message encoding"
    ~tags:["dal"; "gossipsub"; "p2p"]
    ~uses:[Constant.octez_codec]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* output = Codec.describe_binary_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  let* output = Codec.describe_json_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  unit
