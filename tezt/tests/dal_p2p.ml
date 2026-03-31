(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL P2P and Gossipsub tests. *)

open Dal_helpers
module Dal = Dal_common

let test_dal_node_p2p_connection_and_disconnection _protocol _parameters
    _cryptobox node _client dal_node1 =
  let dal_node2 = Dal_node.create ~node () in
  (* Connect the nodes *)
  let* () =
    Dal_common.Helpers.connect_nodes_via_p2p
      ~init_config:true
      dal_node1
      dal_node2
  in
  let* peer_id = Dal_node.read_identity dal_node2 in
  (* kill dal_node2 and check "disconnection" event in node1. *)
  let disconn_ev_in_node1 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id
  in
  let* () = Dal_node.kill dal_node2 in
  disconn_ev_in_node1

let test_dal_node_join_topic _protocol parameters _cryptobox _node _client
    dal_node1 =
  let pkh1 = Constant.bootstrap1.public_key_hash in
  let profile1 = Dal_RPC.Attester pkh1 in
  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let event_waiter =
    check_events_with_topic ~event_with_topic:Join dal_node1 ~num_slots pkh1
  in
  let* () = Dal_RPC.(call dal_node1 @@ patch_profiles [profile1]) in
  event_waiter

(** This generic test function is used to test messages exchanges between two
    DAL nodes via the P2P/GS layers, once connections are established and topics
    are joined.

   The [mk_dal_node2] function is used to create a second DAL node. We may
   decide to create a regular/normal or a modified dal node.

   The [expect_app_notification] flag is used to tell whether we should wait for
   the application layer of the second DAL node to be notified with received messages.
   In case we don't expect the application layer to be notified (e.g. messages are invalid),
   set to [false].

   The [is_first_slot_attestable] flag is used to tell whether the first slot
   (that has been injected by this function) can be attested by the considered
   attester or not. In particular, it should be set to [false] if the
   application layer of the second DAL node was not notified about the messages
   sent by the first DAL node.
*)
let generic_gs_messages_exchange protocol parameters _cryptobox node client
    ?batching_time_interval dal_node1 ~mk_dal_node2 ~expect_app_notification
    ~is_first_slot_attestable =
  let* node2, dal_node2 = mk_dal_node2 protocol parameters in
  let* () =
    Dal_node.init_config
      ?batching_time_interval
      ~peers:[Dal_node.listen_addr dal_node1]
      dal_node2
  in
  let* () = Dal_common.Helpers.connect_nodes_via_p2p dal_node1 dal_node2 in
  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let number_of_shards = parameters.Dal.Parameters.cryptobox.number_of_shards in
  let account1 = Constant.bootstrap1 in
  let pkh1 = account1.public_key_hash in
  (* The two nodes join the same topics *)
  let* () = nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 in

  (* Posting a DAL message to DAL node and to L1 *)
  let crypto_params = parameters.cryptobox in
  let slot_index = 0 in
  let* slot_commitment =
    let slot_size = crypto_params.slot_size in
    let slot_content = generate_dummy_slot slot_size in
    Helpers.publish_and_store_slot
      client
      dal_node1
      Constant.bootstrap1
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size slot_content
  in

  (* Preparing event waiters for different shards that will be published by
     dal_node1 once the slot header is included in an L1 block.

     We also prepare a waiter event for:
     - the shards that will be received by dal_node2 on its topics;
     - the messages (shards) that will be notified to dal_node2 on its topic. *)
  let* publish_level = next_level node in
  let attested_level = publish_level + parameters.attestation_lag in
  let attestation_level = attested_level - 1 in
  let* committee = Dal.Committee.at_level node ~level:attestation_level () in

  let waiter_publish_list =
    waiters_publish_shards
      committee
      dal_node1
      slot_commitment
      ~publish_level
      ~slot_index
      ~number_of_shards
  in
  let waiter_receive_shards =
    let* from_peer = Dal_node.read_identity dal_node1 in
    waiter_receive_shards
      committee
      dal_node2
      slot_commitment
      ~publish_level
      ~slot_index
      ~pkh:pkh1
      ~from_peer
      ~number_of_shards
  in
  let waiter_app_notifs =
    if expect_app_notification then
      waiter_successful_shards_app_notification
        committee
        dal_node2
        slot_commitment
        ~publish_level
        ~slot_index
        ~pkh:pkh1
        ~number_of_shards
    else unit
  in

  (* We bake a block that includes a [slot_header] operation. And then another
     two blocks so that this operation is final. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join waiter_publish_list
  and* () = waiter_receive_shards
  and* () = waiter_app_notifs in

  let* () =
    match node2 with
    | None -> unit
    | Some node2 ->
        (* We need that this node reaches level 2; otherwise, the RPC call to
           [get_attestable_slots] may fail. *)
        let* _level = Node.wait_for_level node2 2 in
        unit
  in

  (* Check that dal_node2 has the shards needed by attester account1/pkh1 to
     attest the slot with index 0. *)
  let* res =
    Dal_RPC.(
      call dal_node2 @@ get_attestable_slots ~attester:account1 ~attested_level)
  in
  match res with
  | Not_in_committee -> Test.fail "attester %s not in committee" account1.alias
  | Attestable_slots slots ->
      (* only slot 0 is attestable. Others are set to false. *)
      let expected =
        is_first_slot_attestable :: List.init (num_slots - 1) (fun _ -> false)
      in
      Check.(
        (expected = slots)
          (list bool)
          ~error_msg:"Expected %L attestable slots list flags, got %R") ;
      unit

let test_dal_node_gs_valid_messages_exchange ?batching_time_interval _protocol
    parameters _cryptobox node client dal_node1 =
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    ?batching_time_interval
    dal_node1
    ~mk_dal_node2:(fun _protocol _parameters ->
      let dal_node2 = Dal_node.create ~node () in
      (None, dal_node2) |> return)
    ~expect_app_notification:true
    ~is_first_slot_attestable:true

(* Create a DAL node whose DAL parameters are not compatible with those in
   [parameters]. For that, the redundancy_factor field is multiplied by 2. *)
let make_invalid_dal_node node1 protocol parameters =
  (* Create another L1 node with different DAL parameters. *)
  let* node2, _client2, _xdal_parameters2 =
    let crypto_params = parameters.Dal.Parameters.cryptobox in
    let parameter_overrides =
      dal_enable_param (Some true)
      @ redundancy_factor_param (Some (crypto_params.redundancy_factor / 2))
      @ slot_size_param (Some (crypto_params.slot_size / 2))
    in
    setup_node ~protocol ~parameter_overrides ()
  in
  Node.add_peer node2 node1 ;
  let* () = Node.terminate node2 in
  let* () = Node.run node2 [] in
  (* Create a second DAL node with node2 and client2 as argument (so different
     DAL parameters compared to dal_node1. *)
  let dal_node2 = Dal_node.create ~node:node2 () in
  return (Some node2, dal_node2)

let test_dal_node_gs_invalid_messages_exchange ?batching_time_interval _protocol
    parameters _cryptobox node client dal_node1 =
  (* Messages are invalid, so the app layer is not notified. *)
  let expect_app_notification = false in
  (* The first slot published by [generic_gs_messages_exchange] is not
     attestable by the considered attester pk1 = bootstrap1, because the shards
     received by the Gossipsub layer are classified as 'Invalid'. *)
  let is_first_slot_attestable = false in
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    ?batching_time_interval
    dal_node1
    ~mk_dal_node2:(make_invalid_dal_node node)
    ~expect_app_notification
    ~is_first_slot_attestable

(* Checks that:
   * the baker does not crash when there's a DAL node specified, but it is not
   running
   * the baker register profiles when the DAL node restarts. *)
let test_baker_registers_profiles protocol _parameters _cryptobox l1_node client
    dal_node =
  let delegates =
    List.to_seq Constant.all_secret_keys |> Seq.take 3 |> List.of_seq
  in
  let profiles =
    List.map (fun key -> Dal_RPC.Attester key.Account.public_key_hash) delegates
  in
  let delegates = List.map (fun key -> key.Account.alias) delegates in

  Log.info
    "Terminate the DAL node and then start the baker; the baker cannot attest \
     but can advance" ;
  let* () = Dal_node.terminate dal_node in
  let baker =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client ~delegates
  in
  let wait_for_attestation_event =
    let attestation_event =
      if Protocol.number protocol >= 025 then
        "no_attestable_dal_slots_for_levels.v0"
      else "failed_to_get_attestations.v0"
    in
    Agnostic_baker.wait_for baker attestation_event (fun _json -> Some ())
  in
  let* () = Agnostic_baker.run baker in
  let* () = wait_for_attestation_event in
  let* _lvl = Node.wait_for_level l1_node 3 in

  Log.info "Start (again) the DAL node" ;
  let* () = Dal_node.run dal_node in
  Log.info "Wait 2 seconds; by then, profiles should have been registered" ;
  (* Note: this constant depends on how often the baker retries to register
     profiles (see [max_delay] in [Baking_scheduling.register_dal_profiles]); if
     the baker behavior changes in this respect, the constant may need
     adjusting. *)
  let* () = Lwt_unix.sleep 2.0 in
  check_profiles ~__LOC__ dal_node ~expected:(Controller profiles)

(** This helper funciton terminates dal_node2 and dal_node3 (in addition to
    those in [extra_nodes_to_restart]), and restart them after creating two
    connection events to check that dal_node2 and dal_node3 find each other. *)
let observe_nodes_connection_via_bootstrap ?(extra_nodes_to_restart = []) client
    dal_node2 dal_node3 =
  let nodes = dal_node2 :: dal_node3 :: extra_nodes_to_restart in
  let* () = List.map Dal_node.terminate nodes |> Lwt.join in
  let* dal_node2_identity = Dal_node.read_identity dal_node2 in
  let* dal_node3_identity = Dal_node.read_identity dal_node3 in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:dal_node3_identity
      ()
  in
  let check_conn_event_from_3_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node3
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:dal_node2_identity
      ()
  in
  let* () = List.map (Dal_node.run ~wait_ready:true) nodes |> Lwt.join in
  Log.info "Bake a block and then another two to finalize it." ;
  let* () = bake_for ~count:3 client in
  Log.info "Wait for dal_node2 and dal_node3 to find each other." ;

  let* () =
    Lwt.join [check_conn_event_from_2_to_3; check_conn_event_from_3_to_2]
  in
  unit

(** This function tests that a peer can discover another peer via a bootstrap
    node and that discovery works even when the bootstrap is (re-)started at the
    same time (or after) the two other nodes we want to connect. *)
let test_peer_discovery_via_bootstrap_node _protocol _parameters _cryptobox node
    client dal_node1 =
  (* Phase 1: dal_node1 is already running. Start dal_node2 and dal_node3 and
     use dal_node1 to establish connections between them. *)
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  (* Here, we observe a first nodes connection via bootstrap nodes thanks to
     peers exchange. *)
  let* () = observe_nodes_connection_via_bootstrap client dal_node2 dal_node3 in

  (* In this variant, we also restart the bootstrap node [dal_node1]. So,
     connections to it from dal_node2 and dal_node3 are always done at startup,
     but Gossipsub worker might be needed to retry connection. *)
  observe_nodes_connection_via_bootstrap
    ~extra_nodes_to_restart:[dal_node1]
    client
    dal_node2
    dal_node3

(** Connect two nodes [dal_node2] and [dal_node3] via a trusted bootstrap peer
    [dal_node1]. Then, disconnect all the nodes (without restarting them) and
    wait for reconnection. *)
let test_peers_reconnection _protocol _parameters _cryptobox node client
    dal_node1 =
  (* Connect two nodes via bootstrap peer. *)
  Log.info "Connect two nodes via bootstrap peer." ;
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  let* () =
    (* Here, we observe a first nodes connection via bootstrap nodes thanks to
       peers exchange. Below, we disconnect the nodes without restarting
       them. *)
    observe_nodes_connection_via_bootstrap client dal_node2 dal_node3
  in

  (* Get the nodes' identities. *)
  let id dal_node = Dal_node.read_identity dal_node in
  let* id_dal_node1 = id dal_node1 in
  let* id_dal_node2 = id dal_node2 in
  let* id_dal_node3 = id dal_node3 in

  (* Prepare disconnection events to observe. *)
  let disconn_ev_in_node1_2 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node2
  in
  let disconn_ev_in_node1_3 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node3
  in
  let disconn_ev_in_node2_3 =
    Dal_common.Helpers.check_disconnection_event dal_node2 ~peer_id:id_dal_node3
  in

  (* Prepare reconnection events checks between node1 and node2 (resp. 3). *)
  let check_conn_event_from_1_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:id_dal_node2
      ()
  in
  let check_conn_event_from_1_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in

  (* Disconnect all the nodes. *)
  let* () =
    Lwt_list.iter_p
      (fun peer_id ->
        Lwt_list.iter_p
          (fun dal_node ->
            Dal_RPC.(call dal_node @@ delete_p2p_peer_disconnect ~peer_id))
          [dal_node1; dal_node2; dal_node3])
      [id_dal_node1; id_dal_node2; id_dal_node3]
  in

  (* Observe disconnection. *)
  Log.info "Wait for disconnection" ;
  let* () =
    Lwt.join
      [disconn_ev_in_node1_2; disconn_ev_in_node1_3; disconn_ev_in_node2_3]
  in
  Log.info "Disconnection done. Wait for reconnection." ;
  let* () =
    Lwt.join
      [
        check_conn_event_from_1_to_2;
        check_conn_event_from_1_to_3;
        check_conn_event_from_2_to_3;
      ]
  in
  Log.info "Recconnection done." ;
  unit

let test_attestation_through_p2p ~batching_time_interval _protocol
    dal_parameters _cryptobox node client dal_bootstrap =
  (* In this test we have three DAL nodes:
     - a boostrap one to connect the other two (producer and attester),
     - a slot producer on slot 0,
     - an attester for all the bootstrap pkh.

     We check that when the slot producer publishes a slot, it ends up
     being attested.
  *)
  let index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let* bootstrap_peer_id = peer_id dal_bootstrap in

  (* Check that the attestation threshold for this test is 100%. If
     not, this means that we forgot to register the test with
     ~attestation_threshold:100 *)
  Check.((dal_parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  (* Check that the dal node passed as argument to this test function
     is a running bootstrap DAL node. If not, this means that we
     forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config
      ~batching_time_interval
      ~operator_profiles:[index]
      ~peers
      producer
  in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in
  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~batching_time_interval
      ~attester_profiles:all_pkhs
      ~peers
      attester
  in
  let* () = Dal_node.run ~event_level:`Debug ~wait_ready:true attester in
  let* attester_peer_id = peer_id attester in

  let client = Client.with_dal_node client ~dal_node:attester in

  (* Wait for a GRAFT message between the attester and the producer,
     in any direction. *)
  let check_graft pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (attester, attester_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_graft_promises = List.map check_graft all_pkhs in
  Log.info "Waiting for grafting of the attester - producer connection" ;
  let* () =
    check_profiles
      ~__LOC__
      attester
      ~expected:
        Dal_RPC.(Controller (List.map (fun pkh -> Attester pkh) all_pkhs))
  in
  (* We need to bake some blocks until the L1 node notifies the
     attester DAL nodes that some L1 block is final and they have DAL
     attestation rights in it. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Attester - producer connection grafted" ;

  (* Attester should be connected to:
     - the bootstrap DAL node on all the topics,
     - the producer on all topics with slot_index=index *)
  let* () =
    let expected topic_pkh =
      Seq.ints 0 |> Seq.take number_of_slots
      |> Seq.map (fun topic_slot_index ->
             ( {Dal_RPC.topic_slot_index; topic_pkh},
               bootstrap_peer_id
               :: (if topic_slot_index = index then [producer_peer_id] else [])
             ))
      |> List.of_seq
    in
    let expected = List.concat (List.map expected all_pkhs) in
    check_topics_peers ~__LOC__ attester ~expected
  in

  (* The slot producer should be connected to:
     - the attester and the bootstrap DAL node on all topics with slot_index=index,
  *)
  let* () =
    let expected =
      all_pkhs
      |> List.map (fun topic_pkh ->
             ( {Dal_RPC.topic_slot_index = index; topic_pkh},
               [bootstrap_peer_id; attester_peer_id] ))
    in
    check_topics_peers ~__LOC__ producer ~expected
  in

  (* Produce and publish a slot *)
  let source = Constant.bootstrap1 in
  let slot_content = "content" in

  let all_shard_indices =
    Seq.ints 0 |> Seq.take number_of_shards |> List.of_seq
  in
  let wait_slot ~published_level ~slot_index =
    Lwt.join
    @@ List.map
         (fun shard_index ->
           wait_for_cached_slot
             ~shard_index
             attester
             ~published_level
             ~slot_index)
         all_shard_indices
  in
  let* publication_level, _commitment, () =
    publish_store_and_wait_slot
      node
      client
      producer
      source
      ~index
      ~wait_slot
      ~number_of_extra_blocks_to_bake:2
    @@ Helpers.make_slot ~slot_size slot_content
  in

  Log.info "Slot produced and published" ;

  let* () =
    bake_until_processed
      ~level:(publication_level + attestation_lag)
      client
      [attester]
  in

  let* () =
    check_slot_status
      ~__LOC__
      attester
      ~expected_status:(Dal_RPC.Attested attestation_lag)
      ~check_attested_lag:`At_most
      ~slot_level:publication_level
      ~slot_index:index
  in
  Log.info "Slot sucessfully attested" ;
  unit
