(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL garbage collection tests. *)

open Dal_helpers
module Dal = Dal_common

(* Test the GC feature: once a period (set by history_mode) is passed after
     receiving the shards, each node deletes them from its storage. *)

let wait_remove_shards ~published_level ~slot_index node =
  Dal_node.wait_for node "dal_removed_slot_shards.v0" (fun event ->
      if
        (published_level = JSON.(event |-> "published_level" |> as_int))
        && slot_index = JSON.(event |-> "slot_index" |> as_int)
      then Some ()
      else None)

let wait_for_first_shard ~published_level ~slot_index node =
  Dal_node.wait_for node "dal_cached_slot_shard.v0" (fun event ->
      let actual_published_level =
        JSON.(event |-> "published_level" |> as_int)
      in
      let actual_slot_index = JSON.(event |-> "slot_index" |> as_int) in
      if published_level = actual_published_level then
        if slot_index = actual_slot_index then
          let shard_index = JSON.(event |-> "shard_index" |> as_int) in
          Some shard_index
        else (
          Log.info
            "bad slot index : received : %d, expected : %d"
            actual_slot_index
            slot_index ;
          None)
      else (
        Log.info
          "bad slot level : received : %d, expected : %d"
          actual_published_level
          published_level ;
        None))

let get_shard_rpc ~slot_level ~slot_index ~shard_index node =
  Dal_RPC.(
    call node
    @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index)

let get_shard_rpc_failure_expected ~slot_level ~slot_index node =
  let* shard_observer_response =
    Dal_RPC.(
      call_raw node
      @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index:0)
  in
  Check.(shard_observer_response.code = 404)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected RPC response, expected: %R, actual: %L" ;
  unit

(* Assert that the slot at [slot_level/slot_index] is still on disk by
   asking the node for its content. *)
let get_slot_content_rpc_success_expected ~slot_level ~slot_index node =
  let* _content =
    Dal_RPC.(call node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  unit

(* Assert that the slot at [slot_level/slot_index] is no longer on disk:
   [GET .../content] should answer 404. *)
let get_slot_content_rpc_failure_expected ~slot_level ~slot_index node =
  let* response =
    Dal_RPC.(call_raw node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  Check.(response.code = 404)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected slot-content RPC response, expected: %R, actual: %L" ;
  unit

(* This simple test checks the basic feature, with just a producer that
     produces shards & deletes them *)
let test_gc_simple_producer _protocol dal_parameters _cryptobox node client
    slot_producer =
  let slot_index = 0 in
  (* Parameters and constants. *)
  let Dal.Parameters.{cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
    dal_parameters
  in
  Log.info "Producer DAL node is running" ;

  let* current_level = Client.level client in
  let wait_for_producer =
    wait_for_layer1_final_block slot_producer (current_level + 1)
  in
  let* published_level, _commitment, () =
    publish_store_and_wait_slot
      node
      client
      slot_producer
      Constant.bootstrap1
      ~index:slot_index
      ~wait_slot:(fun ~published_level:_ ~slot_index:_ -> unit)
      ~number_of_extra_blocks_to_bake:2
    @@ Helpers.make_slot ~slot_size "content"
  in
  let* () = wait_for_producer in

  Log.info "RPC to producer for first shard" ;
  (* Check the producer stored the first shard *)
  let* _first_shard =
    Dal_RPC.(
      call slot_producer
      @@ get_level_slot_shard_content
           ~slot_level:published_level
           ~slot_index
           ~shard_index:0)
  in

  let wait_remove_shards_promise =
    Log.info "Waiting for the shards of the commitment to be removed" ;
    wait_remove_shards ~published_level ~slot_index slot_producer
  in

  Log.info "Every shard published for the commitment; baking blocks" ;
  let* () = bake_for ~count:dal_parameters.attestation_lag client in
  (* The slot wasn't attested (because there is no attester running
       in this scenario) so it got removed despite not being older
       than the history length of the slot producer DAL node. *)
  Log.info "Blocks baked." ;

  Log.info "Wait for the shards of the commitment to be removed" ;
  let* () = wait_remove_shards_promise in

  Log.info "RPC deleted shard producer" ;
  let* () =
    get_shard_rpc_failure_expected
      ~slot_level:published_level
      ~slot_index
      slot_producer
  in

  Log.info "Producer's own slot payload survives the unattested-cleanup pass" ;
  (* With the slot/shard retention split, the slot at an operator slot
     index is retained even when it goes unattested, so the operator can
     still inspect what it published. Only the shards are dropped
     immediately. *)
  let* () =
    get_slot_content_rpc_success_expected
      ~slot_level:published_level
      ~slot_index
      slot_producer
  in

  Log.info "End of test" ;
  unit

(* Common function for testing GC with different DAL node configurations.
     The [include_observer] parameter controls whether an observer node is created
     and tested alongside the producer and attester. *)
let test_gc_common ~include_observer _protocol dal_parameters _cryptobox node
    client dal_bootstrap =
  let slot_index = 0 in
  (* Parameters and constants. *)
  let Dal.Parameters.
        {number_of_slots; cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
    dal_parameters
  in

  (* Note: we don't need to configure and start the DAL bootstrap
       node because it is the node started by the
       scenario_with_layer1_and_dal_nodes wrapper. Instead, we check
       that the dal node passed as argument to this test function is a
       running bootstrap DAL node. If not, this means that we forgot
       to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "bootstrap DAL node is running" ;

  (* When configuring the other DAL nodes, we use dal_bootstrap as
       the single peer. *)
  let peers = [Dal_node.listen_addr dal_bootstrap] in

  (* Create & configure attester *)
  let attester = Dal_node.create ~name:"attester" ~node () in
  let client = Client.with_dal_node client ~dal_node:attester in

  let bootstrap_pkhs =
    Array.to_seq Account.Bootstrap.keys
    |> Seq.map (fun account -> account.Account.public_key_hash)
    |> List.of_seq
  in
  let* () =
    Dal_node.init_config ~attester_profiles:bootstrap_pkhs ~peers attester
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug attester in
  Log.info "attester DAL node ready" ;

  let slot_producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers slot_producer
  in
  (* Promise which will be resolved once the slot producer will be
       connected to all the other DAL nodes (attester + observer (if any)
       + bootstrap). *)
  let num_connections = if include_observer then 3 else 2 in
  let wait_for_connections_of_producer_promise =
    Dal_node.wait_for_connections slot_producer num_connections
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug slot_producer in

  (* Create & configure observer if needed *)
  let* observer_opt =
    if include_observer then
      (* We disable the amplification for this observer, since it is not the
           purpose of the current test to test amplification, and if
           amplification is triggered, it makes the DAL node very late compared
           to the L1 node, making the test flaky. *)
      let observer =
        Dal_node.create ~disable_amplification:true ~name:"observer" ~node ()
      in
      let* () =
        Dal_node.init_config ~observer_profiles:[slot_index] ~peers observer
      in
      let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug observer in
      return (Some observer)
    else return None
  in

  (* Check that the DAL nodes have the expected profiles. *)
  let* () =
    check_profiles
      ~__LOC__
      slot_producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  let* () =
    match observer_opt with
    | Some observer ->
        let* () =
          check_profiles
            ~__LOC__
            observer
            ~expected:Dal_RPC.(Controller [Observer slot_index])
        in
        Log.info "Observer DAL node is running" ;
        unit
    | None -> unit
  in

  let* () =
    check_profiles
      ~__LOC__
      attester
      ~expected:
        (Dal_RPC.Controller
           (List.map (fun pkh -> Dal_RPC.Attester pkh) bootstrap_pkhs))
  in

  Log.info "Attester DAL node is running" ;

  (* Now that all the DAL nodes are running, we need some of them to
       establish grafted connections. *)
  let check_graft_promises =
    if include_observer then
      (* For the case with observer, we use the more complex grafting
           check with already_seen_slots. The connections between the
           attester and the slot producer have no reason to be grafted on
           other slot indices than the one the slot producer is subscribed
           to, so we instruct [check_events_with_topic] to skip all events
           but the one for [slot_index]. *)
      let observer = Option.get observer_opt in
      let already_seen_slots =
        Array.init number_of_slots (fun index -> slot_index <> index)
      in
      let check_graft node1 node2 attester_pkh =
        let check_graft ~from:node1 node2 =
          let* node1_id = Dal_node.read_identity node1 in
          check_events_with_topic
            ~event_with_topic:(Graft node1_id)
            node2
            ~num_slots:number_of_slots
            ~already_seen_slots
            attester_pkh
        in
        Lwt.pick [check_graft ~from:node1 node2; check_graft ~from:node2 node1]
      in
      List.map (check_graft slot_producer attester) bootstrap_pkhs
      @ List.map (check_graft observer attester) bootstrap_pkhs
      @ List.map (check_graft slot_producer observer) bootstrap_pkhs
    else
      (* For the simple case, use basic grafting check *)
      let check_graft node1 node2 attester_pkh =
        let* id1 = Dal_node.read_identity node1 in
        let* id2 = Dal_node.read_identity node2 in
        Lwt.pick
        @@ check_grafts
             ~number_of_slots
             ~slot_index
             (node1, id1)
             (node2, id2)
             attester_pkh
      in
      List.map (check_graft slot_producer attester) bootstrap_pkhs
  in
  Log.info "Waiting for grafting of the DAL nodes connections" ;
  (* The attester DAL node won't join the DAL network until it has
       processed some finalized L1 block in which the associated baker
       is reported to have some rights. For this to happen, we need to
       bake a few blocks. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Attester - producer connections grafted" ;

  let* () = wait_for_connections_of_producer_promise in

  Log.info "DAL network is ready" ;

  let wait_slot ~published_level ~slot_index =
    (* We don't wait for the producer because at this step it already stored
         its shards *)
    if include_observer then (
      let observer = Option.get observer_opt in
      let wait_for_first_shard_observer_promise =
        Log.info "Waiting for first shard to be stored by the observer" ;
        wait_for_first_shard ~published_level ~slot_index observer
      in
      let wait_for_first_shard_attester_promise =
        Log.info "Waiting for first shard to be stored by the attester" ;
        wait_for_first_shard ~published_level ~slot_index attester
      in
      let* shard_index_observer = wait_for_first_shard_observer_promise in
      Log.info "Shards were received by the observer" ;
      let* shard_index_attester = wait_for_first_shard_attester_promise in
      Log.info "Shards were received by the attester" ;
      return (Some shard_index_observer, shard_index_attester))
    else (
      Log.info "Waiting for first shard to be stored by the attester" ;
      let* shard_index_attester =
        wait_for_first_shard ~published_level ~slot_index attester
      in
      return (None, shard_index_attester))
  in

  let* current_level = Client.level client in
  let wait_for_producer =
    wait_for_layer1_final_block slot_producer current_level
  in
  let wait_for_observer_opt =
    match observer_opt with
    | Some observer -> Some (wait_for_layer1_final_block observer current_level)
    | None -> None
  in
  let wait_for_attester = wait_for_layer1_final_block attester current_level in
  let* ( published_level,
         _commitment,
         (shard_index_observer_opt, shard_index_attester) ) =
    publish_store_and_wait_slot
      node
      client
      slot_producer
      Constant.bootstrap1
      ~index:slot_index
      ~wait_slot
      ~number_of_extra_blocks_to_bake:1
    @@ Helpers.make_slot ~slot_size "content"
  in
  Log.info "Published a slot at level %d" published_level ;
  let* () = wait_for_producer in
  Log.info "Producer reached expected level" ;
  let* () =
    match wait_for_observer_opt with
    | Some wait_promise ->
        let* () = wait_promise in
        Log.info "Observer reached expected level" ;
        unit
    | None -> unit
  in
  let* () = wait_for_attester in
  Log.info "Attester reached expected level" ;

  let* () =
    match shard_index_observer_opt with
    | Some shard_index_observer ->
        Log.info "RPC first shard observer" ;
        let* _shard_observer =
          get_shard_rpc
            ~slot_level:published_level
            ~slot_index
            ~shard_index:shard_index_observer
            (Option.get observer_opt)
        in
        unit
    | None -> unit
  in
  Log.info "RPC first shard producer" ;
  let* _shard_producer =
    get_shard_rpc
      ~slot_level:published_level
      ~slot_index
      ~shard_index:0
      slot_producer
  in
  Log.info "RPC first shard attester" ;
  let* _shard_attester =
    get_shard_rpc
      ~slot_level:published_level
      ~slot_index
      ~shard_index:shard_index_attester
      attester
  in

  let wait_block_p =
    List.map
      (fun dal_node ->
        wait_for_layer1_final_block
          dal_node
          (published_level + dal_parameters.attestation_lag))
      [attester; dal_bootstrap; slot_producer]
  in
  (* For the slot to be attested (that is, to reduce flakiness), we wait
       between levels, giving the time for the attester DAL nodes to attest. *)
  let* () =
    repeat (dal_parameters.attestation_lag + 1) (fun () ->
        let* () = bake_for client in
        Lwt_unix.sleep 1.)
  in
  let* () = Lwt.join wait_block_p in
  Log.info "Checking that the slot was attested" ;
  let* () =
    check_slot_status
      ~__LOC__
      slot_producer
      ~expected_status:(Dal_RPC.Attested dal_parameters.attestation_lag)
      ~check_attested_lag:`At_most
      ~slot_level:published_level
      ~slot_index
  in

  let wait_remove_shards_attester_promise =
    Log.info "Waiting for first shard to be removed by the attester" ;
    wait_remove_shards ~published_level ~slot_index attester
  in
  let wait_remove_shards_producer_promise =
    Log.info "Waiting for first shard to be removed by the producer" ;
    wait_remove_shards ~published_level ~slot_index slot_producer
  in

  (* 150 because the shard retention period is 150 levels regardless of
     profile. *)
  let blocks_to_bake = 150 in
  Log.info
    "All nodes received a shard, waiting for %d more blocks to be baked"
    blocks_to_bake ;
  let wait_block_p =
    List.map
      (fun dal_node ->
        wait_for_layer1_head dal_node (published_level + blocks_to_bake))
      [attester; dal_bootstrap; slot_producer]
  in
  (* Bake in [blocks_per_cycle]-sized chunks, waiting for the DAL nodes to
     catch up between chunks: a single [bake_for ~count:blocks_to_bake]
     floods L1 across many cycles instantly, and a DAL node processing the
     backlog then asks L1 for a committee whose cycle seed the protocol has
     already cleared from its context, crashing it
     ("Missing key 'cycle/N/random_seed'"). Capping each chunk at
     [blocks_per_cycle] keeps the DAL nodes within ~one cycle of L1 — well
     within the seed-retention window — while remaining much faster than
     baking one block at a time. *)
  let chunk =
    8
    (* = sandbox [blocks_per_cycle] *)
  in
  let dal_nodes = [attester; dal_bootstrap; slot_producer] in
  let rec bake_in_chunks remaining =
    if remaining <= 0 then unit
    else
      let n = min chunk remaining in
      let* pre = Client.level client in
      let target_final = pre + n - 2 in
      let chunk_waits =
        List.map
          (fun dal_node -> wait_for_layer1_final_block dal_node target_final)
          dal_nodes
      in
      let* () = bake_for ~count:n client in
      let* () = Lwt.join chunk_waits in
      bake_in_chunks (remaining - n)
  in
  let* () = bake_in_chunks blocks_to_bake in
  let* () = Lwt.join wait_block_p in
  Log.info "Blocks baked !" ;

  Log.info "Wait for shards to be removed by the attester" ;
  let* () = wait_remove_shards_attester_promise in
  Log.info "Wait for shards to be removed by the producer" ;
  let* () = wait_remove_shards_producer_promise in

  Log.info "RPC deleted shard attester" ;
  let* () =
    get_shard_rpc_failure_expected
      ~slot_level:published_level
      ~slot_index
      attester
  in

  let* () =
    match observer_opt with
    | Some observer ->
        Log.info "RPC deleted shard observer" ;
        get_shard_rpc_failure_expected
          ~slot_level:published_level
          ~slot_index
          observer
    | None -> unit
  in
  Log.info "RPC deleted shard producer" ;
  let* () =
    get_shard_rpc_failure_expected
      ~slot_level:published_level
      ~slot_index
      slot_producer
  in

  (* The default history mode is [archive], so the producer's slot at an
     operator slot index outlives its shards. *)
  Log.info "RPC slot payload still stored by producer" ;
  let* () =
    get_slot_content_rpc_success_expected
      ~slot_level:published_level
      ~slot_index
      slot_producer
  in

  Log.info "End of test" ;
  unit

(* This simple test checks the basic feature, with just a producer that
     produces shards & deletes them and an attester with attests them. *)
let test_gc_producer_and_attester protocol parameters cryptobox node client
    dal_node =
  test_gc_common
    ~include_observer:false
    protocol
    parameters
    cryptobox
    node
    client
    dal_node

(* For this test, we create a network of three DAL nodes — 1 slot producer,
     1 attester, 1 observer (so we can check for each profile). History mode is
     [Auto].
     The slot producer will send shards from one slot; once a node receives it,
     a request is sent for a received shard, to make sure for reception. After
     180 blocks baked, we check via RPC that attester deleted its shards and the
     others did not.
  *)
let test_gc_with_all_profiles protocol parameters cryptobox node client dal_node
    =
  test_gc_common
    ~include_observer:true
    protocol
    parameters
    cryptobox
    node
    client
    dal_node

(* Verifies that a slot at an operator slot index is GC'd once the rolling
   retention (set via [--history-mode <n>]) elapses. Exercises the
   slot-payload pass of [remove_old_level_stored_data]: once a finalized
   block at level [published_level + slot_retention] is processed by the
   DAL node, the slot at [published_level] for an operator-profile slot
   index is removed.

   This test deliberately does not assert anything about shard removal
   timing; shard cleanup is already covered by {!test_gc_simple_producer}. *)
let test_gc_slot_rolling ~__FILE__ ~protocols =
  let title =
    "garbage collection of operator slot payload under rolling history"
  in
  let tags = Tag.[tezos2; "dal"; "gc"; "history_mode"; "rolling"] in
  Protocol.register_test
    ~__FILE__
    ~tags
    ~uses:(fun _protocol -> [Constant.octez_dal_node])
    ~title
    ~supports:(Protocol.From_protocol 19)
    (fun protocol ->
      (* Pick small values for the refutation-related parameters so that the
         operator's [storage_period = 2 * (challenge + commitment +
         validity_lag) = 10] is small. The rolling slot retention must be at
         least [storage_period], so [Custom 30] is allowed. *)
      let a = 1 in
      let b = 2 in
      let c = 2 in
      let slot_retention = 30 in
      let parameter_overrides =
        make_int_parameter ["smart_rollup_commitment_period_in_blocks"] (Some a)
        @ make_int_parameter
            ["smart_rollup_challenge_window_in_blocks"]
            (Some b)
        @ make_int_parameter
            [
              "smart_rollup_reveal_activation_level";
              "dal_attested_slots_validity_lag";
            ]
            (Some c)
      in
      let* node, client, dal_parameters =
        setup_node
          ~parameter_overrides
          ~protocol
          ~l1_history_mode:Default_with_refutation
          ()
      in
      let Dal.Parameters.{cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
        dal_parameters
      in
      let slot_index = 0 in
      let dal_node = Dal_node.create ~name:"producer" ~node () in
      let* () =
        Dal_node.init_config
          ~operator_profiles:[slot_index]
          ~history_mode:(Dal_node.Custom slot_retention)
          dal_node
      in
      let* () = Dal_node.run dal_node ~wait_ready:true in
      Log.info
        "Producer DAL node ready (history-mode = Custom %d)"
        slot_retention ;

      let* current_level = Client.level client in
      let wait_for_producer =
        wait_for_layer1_final_block dal_node (current_level + 1)
      in
      let* published_level, _commitment, () =
        publish_store_and_wait_slot
          node
          client
          dal_node
          Constant.bootstrap1
          ~index:slot_index
          ~wait_slot:(fun ~published_level:_ ~slot_index:_ -> unit)
          ~number_of_extra_blocks_to_bake:2
        @@ Helpers.make_slot ~slot_size "content"
      in
      let* () = wait_for_producer in
      Log.info "Slot published at level %d" published_level ;

      (* Sanity: the slot payload is present right after publication. *)
      let* () =
        get_slot_content_rpc_success_expected
          ~slot_level:published_level
          ~slot_index
          dal_node
      in

      (* For the slot-payload pass to fire at level [published_level] the
         producer must process a finalized block at
         [published_level + slot_retention]. Finalization lags head by 2,
         so we need [head >= published_level + slot_retention + 2]. *)
      let target_finalized = published_level + slot_retention in
      let target_head = target_finalized + 2 in
      let* current_level = Client.level client in
      let blocks_to_bake = max 0 (target_head - current_level) in
      Log.info
        "Baking %d more blocks to reach finalized level %d (head %d)"
        blocks_to_bake
        target_finalized
        target_head ;
      let wait_for_target =
        wait_for_layer1_final_block dal_node target_finalized
      in
      (* Bake in [blocks_per_cycle]-sized chunks so the producer DAL node
         stays within ~one cycle of L1; otherwise a flooded backlog would
         hit a cleared cycle seed and crash the node. *)
      let chunk =
        8
        (* = sandbox [blocks_per_cycle] *)
      in
      let rec bake_in_chunks remaining =
        if remaining <= 0 then unit
        else
          let n = min chunk remaining in
          let* pre = Client.level client in
          let target_chunk_final = pre + n - 2 in
          let chunk_wait =
            wait_for_layer1_final_block dal_node target_chunk_final
          in
          let* () = bake_for ~count:n client in
          let* () = chunk_wait in
          bake_in_chunks (remaining - n)
      in
      let* () = bake_in_chunks blocks_to_bake in
      let* () = wait_for_target in

      Log.info
        "RPC slot payload should now be removed (rolling window of %d levels \
         elapsed)"
        slot_retention ;
      get_slot_content_rpc_failure_expected
        ~slot_level:published_level
        ~slot_index
        dal_node)
    protocols

let test_gc_skip_list_cells ~__FILE__ ~protocols =
  let title = "garbage collection of skip list cells" in
  let tags = Tag.[tezos2; "dal"; "gc"; "skip_list"] in
  Protocol.register_test
    ~__FILE__
    ~tags
    ~uses:(fun _protocol -> [Constant.octez_dal_node])
    ~title
    ~supports:(Protocol.From_protocol 19)
    (fun protocol ->
      (* We choose some arbitrary, small values *)
      let a = 1 in
      let b = 2 in
      let c = 2 in
      (* The period in blocks for which cells are kept. *)
      let non_gc_period = 2 * (a + b + c) in
      Log.info "The \"non-GC period\" is %d" non_gc_period ;
      let parameter_overrides =
        make_int_parameter ["smart_rollup_commitment_period_in_blocks"] (Some a)
        @ make_int_parameter
            ["smart_rollup_challenge_window_in_blocks"]
            (Some b)
        @ make_int_parameter
            [
              "smart_rollup_reveal_activation_level";
              "dal_attested_slots_validity_lag";
            ]
            (Some c)
      in
      let* node, client, dal_parameters =
        setup_node
          ~parameter_overrides
          ~protocol
          ~l1_history_mode:Default_with_refutation
          ()
      in
      let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
      let lag = dal_parameters.attestation_lag in
      let dal_node = Dal_node.create ~node () in
      (* Skip-list cell retention is the bounded default store period
         regardless of [history_mode], so the default ([Archive]) is fine
         here. *)
      let* () = Dal_node.init_config ~operator_profiles:[1] dal_node in
      let* () = Dal_node.run dal_node ~wait_ready:true in
      Log.info
        "The first level with stored cells is 2, the last is is 1 + lag = %d. \
         We bake till that level is final, that is until level %d."
        (lag + 1)
        (lag + 3) ;
      let wait_for_dal_node = wait_for_layer1_final_block dal_node (lag + 1) in
      let* current_level = Client.level client in
      assert (current_level = 1) ;
      let* () = bake_for client ~count:(lag + 2) in
      let* () = wait_for_dal_node in
      Log.info
        "Check that the skip list store contains the right entries for level \
         lag + 1." ;
      let* () =
        let expected_levels =
          if Protocol.number protocol >= 025 then
            List.map string_of_int (List.init lag (fun i -> i + 2))
          else ["1"]
        in
        check_skip_list_store dal_node ~number_of_slots ~expected_levels
      in
      (* We just want to observe the GC taking effect, so we need to bake at
           least [non_gc_period] blocks. *)
      Log.info "We bake %d more blocks" non_gc_period ;
      let last_final_level = lag + 1 + non_gc_period in
      let wait_for_dal_node =
        wait_for_layer1_final_block dal_node last_final_level
      in
      let* () = bake_for client ~count:non_gc_period in
      let* () = wait_for_dal_node in
      Log.info
        "Check that the skip list store contains cells for [non_gc_period] \
         levels." ;
      (* Example: say head level = 21. The node GCs the cells at level 19 - 10
           = 9, and it injects at level 19. So we have cells for levels 10 to
           19, that is, 10 levels. *)
      let expected_levels =
        if Protocol.number protocol < 025 then
          (* The first published level with stored cells is [last_final_level
               - non_gc_period + 1 - lag = 2]. *)
          List.init non_gc_period (fun i -> string_of_int (i + 2))
        else
          (* The first published level with stored cells is [last_final_level
               - non_gc_period + 1 - lag = 2]. The last is [last_final_level] *)
          List.init (non_gc_period + lag) (fun i -> string_of_int (i + 2))
      in
      check_skip_list_store dal_node ~number_of_slots ~expected_levels)
    protocols

let register ~__FILE__ ~protocols =
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["gc"; "simple"; Tag.memory_hungry]
    ~operator_profiles:[0]
    ~number_of_slots:1
    "garbage collection of shards for producer"
    test_gc_simple_producer
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["gc"; "attester"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "garbage collection of shards for producer and attester"
    test_gc_producer_and_attester
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~__FILE__
    ~tags:["gc"; "multi"; Tag.memory_hungry]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "garbage collection of shards for all profiles"
    test_gc_with_all_profiles
    protocols ;
  test_gc_skip_list_cells ~__FILE__ ~protocols ;
  test_gc_slot_rolling ~__FILE__ ~protocols
