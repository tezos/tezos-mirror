(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* DAL shard amplification and reconstruction tests. *)

open Dal_helpers
module Dal = Dal_common

let step_counter = ref 0

let info msg =
  let prefix : string = Format.asprintf "Step %d" !step_counter in
  let () = incr step_counter in
  Log.info ~prefix msg

(* Test the amplification feature: once it has seen enough (but not
     necessarily all) shards of a given slot, an observer DAL node is
     able to reconstruct the complete slot and send the missing shards
     on the DAL network.

     To test this feature, we need a DAL network on which some shards
     have been lost and need to be recomputed. This is achieved by
     building a network with a single slot producer and several
     attesters corresponding to different bakers; the slot producer is
     only connected to some attesters so the shards needed by the
     other attesters are lost.

     More precisely, the DAL network we build is composed of (omitting
     the bootstrap DAL node):

     slot producer --- 3 attesters --- observer --- 2 attesters

     This topology is obtained by making the slot producer ban the
     nodes which should not be connected to it (the observer and 2
     attesters). For this reason, we call "banned attesters" the 2
     attesters which are only connected to the observer node and
     "non-banned attesters" the 3 attesters which are connected to
     both the slot producer and the observer.
  *)

(* In amplification tests, we have one attester DAL node per
     boostrap baker. For convenience, we bundle the dal node process
     together with information about the corresponding bootstrap
     baker. *)
type attester = {
  dal_node : Dal_node.t;
  account : Account.key;
  pkh : string;
  index : int; (* From 0 to 4. *)
  name : string; (* From "attester1" to "attester5" *)
  mutable peer_id : string option;
      (* Delayed and cached call to Dal_node.read_identity *)
}

let attester_peer_id (attester : attester) =
  match attester.peer_id with
  | Some pid -> Lwt.return pid
  | None ->
      let* pid = Dal_node.read_identity attester.dal_node in
      attester.peer_id <- Some pid ;
      Lwt.return pid

let test_amplification _protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  let number_of_banned_attesters = 2 in
  let slot_index = 0 in
  (* Parameters and constants. *)
  let {
    Dal.Parameters.attestation_threshold;
    number_of_slots;
    cryptobox =
      {Dal.Cryptobox.slot_size; number_of_shards; redundancy_factor; _};
    _;
  } =
    dal_parameters
  in

  (* Note: we don't need to configure and start the DAL bootstrap
       node because it is the node started by the
       scenario_with_layer1_and_dal_nodes wrapper. Instead, we check
       that the dal node passed as argument to this test function is a
       running bootstrap DAL node. If not, this means that we forgot
       to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  info "bootstrap DAL node is running" ;

  (* When configuring the other DAL nodes, we use dal_bootstrap as
       the single peer. *)
  let peers = [Dal_node.listen_addr dal_bootstrap] in

  (* Create and configure all nodes: a slot producer, an observer,
       and one attester per bootstrap baker. *)
  let make_attester index (account : Account.key) : attester Lwt.t =
    let name = Printf.sprintf "attester-%d" (index + 1) in
    let pkh = account.public_key_hash in
    let dal_node = Dal_node.create ~name ~node () in
    let* () = Dal_node.init_config ~attester_profiles:[pkh] ~peers dal_node in
    (* Setting [event_level] to `Debug so that the [stored_slot_shard] event
         will be emitted. *)
    let* () = Dal_node.run ~event_level:`Debug ~wait_ready:true dal_node in
    return {name; index; dal_node; pkh; account; peer_id = None}
  in
  let* all_attesters =
    Lwt_list.mapi_p make_attester (Array.to_list Account.Bootstrap.keys)
  in
  info "attester DAL node ready" ;
  let banned_attesters, non_banned_attesters =
    List.partition
      (fun attester -> attester.index < number_of_banned_attesters)
      all_attesters
  in
  let slot_producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers slot_producer
  in
  (* Promise which will be resolved once the slot producer will be
       connected to all the other DAL nodes (all attesters + observer
       + bootstrap). We will wait for this to happen before banning
       the observer and some of the attesters. *)
  let wait_for_connections_of_producer_promise =
    Dal_node.wait_for_connections slot_producer (2 + List.length all_attesters)
  in

  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug slot_producer in
  let observer = Dal_node.create ~name:"observer" ~node () in
  let* () =
    Dal_node.init_config ~observer_profiles:[slot_index] ~peers observer
  in
  (* Setting [event_level] to `Debug so that the [stored_slot_shard] event
       will be emitted. *)
  let* () = Dal_node.run ~event_level:`Debug ~wait_ready:true observer in

  (* Check that the DAL nodes have the expected profiles. *)
  let* () =
    check_profiles
      ~__LOC__
      slot_producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  let* slot_producer_peer_id = Dal_node.read_identity slot_producer in
  info "Slot producer DAL node is running" ;

  let* () =
    check_profiles
      ~__LOC__
      observer
      ~expected:Dal_RPC.(Controller [Observer slot_index])
  in
  let* observer_peer_id = Dal_node.read_identity observer in
  info "Observer DAL node is running" ;

  let* () =
    Lwt.join
    @@ List.map
         (fun attester ->
           check_profiles
             ~__LOC__
             attester.dal_node
             ~expected:Dal_RPC.(Controller [Attester attester.pkh]))
         all_attesters
  in
  info "Attesters are running" ;

  (* Now that all the DAL nodes are running, we need some of them to
       establish grafted connections. The connections between
       attesters and the slot producer have no reason to be grafted on
       other slot indices than the one the slot producer is subscribed
       to, so we instruct [check_events_with_topic] to skip all events
       but the one for [slot_index]. *)
  (* Wait for a GRAFT message between an attester and either an operator
       (legacy producer) or an observer, in any direction. *)
  let check_graft_promise (operator_or_observer, peer_id) attester =
    let* attester_peer_id = attester_peer_id attester in
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index
         (operator_or_observer, peer_id)
         (attester.dal_node, attester_peer_id)
         attester.pkh
  in
  (* We don't care if the slot producer establishes full connections
       with the DAL nodes it is about to ban so we only wait for the
       GRAFT messages between:
       - the slot producer and the non-banned attesters,
       - the observer and all the attesters *)
  let check_graft_promises =
    List.map
      (check_graft_promise (slot_producer, slot_producer_peer_id))
      non_banned_attesters
    @ List.map (check_graft_promise (observer, observer_peer_id)) all_attesters
  in
  info "Waiting for grafting of the attester - producer/observer connections" ;
  (* Each attester DAL node won't join the DAL network until it has processed
       some finalized L1 block in which the associated baker is reported to have
       some rights. For this to happen, we need to bake a few blocks.
       If this test is used at a migration, one has to restart the client to use
       the right protocol, instead of using [bake_for ~count:n]. *)
  let* () = repeat 3 (fun () -> bake_for client) in
  let* level = next_level node in
  let* () = bake_for client
  and* () = wait_for_layer1_final_block dal_bootstrap (level - 2) in
  let* () = Lwt.join check_graft_promises in
  info "Attester - producer connections grafted" ;

  (* Wait for producer to be connected to all the dal nodes it wants
       to ban because they cannot be banned earlier. *)
  let* () = wait_for_connections_of_producer_promise in

  let ban_p2p_peer this_node ~peer =
    let this_dal_node, this_id = this_node in
    let peer_dal_node, peer_id = peer in
    let acl = "ban" in
    let p =
      let* () = Dal_node.wait_for_disconnection this_dal_node ~peer_id
      and* () =
        Dal_node.wait_for_disconnection peer_dal_node ~peer_id:this_id
      in
      unit
    in
    let* () =
      Dal_RPC.call this_dal_node
      @@ Dal_RPC.patch_p2p_peers_by_id ~peer_id ~acl ()
    in
    p
  in

  (* Slot producer bans observer and banned_attesters. *)
  let* () =
    ban_p2p_peer
      (slot_producer, slot_producer_peer_id)
      ~peer:(observer, observer_peer_id)
  in
  let* () =
    Lwt.join
    @@ List.map
         (fun attester ->
           let* attester_peer_id = attester_peer_id attester in
           ban_p2p_peer
             (slot_producer, slot_producer_peer_id)
             ~peer:(attester.dal_node, attester_peer_id))
         banned_attesters
  in
  info "DAL network is ready" ;

  let* level_before_publication = Client.level client in
  let publication_level = level_before_publication + 1 in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let attestation_lag =
    JSON.(proto_params |-> "dal_parametric" |-> "attestation_lag" |> as_int)
  in
  let attested_level = publication_level + attestation_lag in
  let attestation_level = attested_level - 1 in

  let* assigned_shard_indices_non_banned =
    Lwt_list.map_p
      (fun attester ->
        Dal_RPC.(
          call attester.dal_node
          @@ get_assigned_shard_indices
               ~level:attestation_level
               ~pkh:attester.pkh))
      non_banned_attesters
  in
  let* assigned_shard_indices_banned =
    Lwt_list.map_p
      (fun attester ->
        Dal_RPC.(
          call attester.dal_node
          @@ get_assigned_shard_indices
               ~level:attestation_level
               ~pkh:attester.pkh))
      banned_attesters
  in
  let total_number_of_assigned_shards =
    List.fold_left
      (fun acc l -> acc + List.length l)
      0
      assigned_shard_indices_non_banned
  in
  (* Minimum number of shards required to reconstruct a slot.
       This is also the (minimum) count that relevant profiles (non-attester,
       non-bootstrap) must store on disk. *)
  let num_minimal_shards_to_reconstruct =
    number_of_shards / redundancy_factor
  in
  (* Check that the non-banned attesters have collectively enough
       assigned shards to reconstruct the slot. *)
  Check.(num_minimal_shards_to_reconstruct < total_number_of_assigned_shards)
    ~__LOC__
    Check.int
    ~error_msg:
      "Non-banned attesters don't have enough rights to reconstruct slots, \
       (needed: %L, actual: %R)" ;
  (* Check that no non-banned attester has enough
       assigned shards to attest the slot alone. *)
  List.iter2
    (fun assigned_shard_indices attester ->
      Check.(
        attestation_threshold * number_of_shards / 100
        > List.length assigned_shard_indices)
        ~__LOC__
        Check.int
        ~error_msg:
          ("Attester " ^ attester.name
         ^ " has enough right to attest alone, (needed: %L shards, actual: %R)"
          ))
    assigned_shard_indices_non_banned
    non_banned_attesters ;
  (* Check that no initial attester has enough
       assigned shards to reconstruct. *)
  List.iter2
    (fun assigned_shard_indices attester ->
      Check.(
        num_minimal_shards_to_reconstruct > List.length assigned_shard_indices)
        ~__LOC__
        Check.int
        ~error_msg:
          ("Attester " ^ attester.name
         ^ " has enough rights to reconstruct alone, (needed: %L, actual: %R)"))
    assigned_shard_indices_non_banned
    non_banned_attesters ;

  let wait_for_shards ~storage_profile ~dal_node ~shards ~published_level
      ~slot_index =
    let* () =
      wait_for_shards_promises
        ~storage_profile
        ~dal_node
        ~shards
        ~published_level
        ~slot_index
    in
    let () =
      Log.debug "Dal node %s has received its shards" (Dal_node.name dal_node)
    in
    unit
  in
  info "Waiting for shards" ;

  let wait_shards_reach_attesters ~published_level ~slot_index attesters
      assigned_shard_indices message =
    let* () =
      Lwt.join
      @@ List.map2
           (fun {dal_node; _} shards ->
             wait_for_shards
               ~storage_profile:`Cache_only
               ~dal_node
               ~shards
               ~published_level
               ~slot_index)
           attesters
           assigned_shard_indices
    in
    info message ;
    unit
  in
  let wait_shards_reach_observer ~published_level ~slot_index =
    let* () =
      Lwt.join
      @@ List.map2
           (fun attester shards ->
             let* () =
               wait_for_shards
                 ~storage_profile:(`Disk num_minimal_shards_to_reconstruct)
                 ~dal_node:observer
                 ~shards
                 ~published_level
                 ~slot_index
             in
             let () =
               Log.info
                 "Dal node %s has sent its shards to observer"
                 attester.name
             in
             unit)
           non_banned_attesters
           assigned_shard_indices_non_banned
    in
    info "Non-banned attesters have transmitted their shards to the observer" ;
    unit
  in

  (* Wait until everyone has received the needed shards (first the
       non-banned attesters, then the observer, and finally the banned
       attesters). *)
  let wait_slot ~published_level ~slot_index =
    let wait_non_banned_promise =
      wait_shards_reach_attesters
        ~published_level
        ~slot_index
        non_banned_attesters
        assigned_shard_indices_non_banned
        "Non-banned attesters have received their shards"
    in

    let wait_banned_promise =
      wait_shards_reach_attesters
        ~published_level
        ~slot_index
        banned_attesters
        assigned_shard_indices_banned
        "Banned attesters have received their shards"
    in

    let wait_observer_promise =
      wait_shards_reach_observer ~published_level ~slot_index
    in

    let* () = wait_non_banned_promise in
    let* () = wait_observer_promise in

    wait_banned_promise
  in
  let* publication_level_bis, _commitment, () =
    publish_store_and_wait_slot
      node
      client
      slot_producer
      Constant.bootstrap1
      ~index:slot_index
      ~wait_slot
      ~number_of_extra_blocks_to_bake:2
    @@ Helpers.make_slot ~slot_size "content"
  in
  Check.(publication_level_bis = publication_level)
    ~__LOC__
    Check.int
    ~error_msg:
      "Commitment published at unexpected level: expected: %R, actual: %L" ;
  unit

(* A simpler test in which a slot producer is directly connected to
     an observer node and sends all shards. *)
let test_amplification_without_lost_shards _protocol dal_parameters _cryptobox
    node client dal_bootstrap =
  (* In this test we have three DAL nodes:
       - a bootstrap one to connect the other two (producer and observer),
       - a slot producer on slot 0,
       - an observer on slot 0.

       We check that when the slot producer publishes a slot, the
       observer performs an amplification. *)
  let index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in

  (* Check that the dal node passed as argument to this test function
       is a running bootstrap DAL node. If not, this means that we
       forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let observer = Dal_node.create ~name:"observer" ~node () in
  let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug observer in
  let* observer_peer_id = peer_id observer in
  let* () =
    check_profiles
      ~__LOC__
      observer
      ~expected:Dal_RPC.(Controller [Observer index])
  in
  Log.info "Observer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in

  let check_graft_promises =
    List.map
      (fun pkh ->
        Lwt.pick
        @@ check_grafts
             ~number_of_slots
             ~slot_index:index
             (observer, observer_peer_id)
             (producer, producer_peer_id)
             pkh)
      all_pkhs
  in
  Log.info "Waiting for grafting of the observer - producer connection" ;

  (* We need to bake some blocks until the L1 node notifies the DAL
       nodes that some L1 block is final so that the topic pkhs are
       known. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Observer - producer connection grafted" ;

  (* Produce and publish a slot *)
  let source = Constant.bootstrap1 in
  let slot_content = "content" in
  let* before_publication_level = Client.level client in
  let publication_level = 1 + before_publication_level in
  let wait_slot ~published_level:_ ~slot_index:_ =
    Log.info "Waiting for first reconstruction event" ;
    let* () =
      Dal_node.wait_for observer "dal_reconstruct_starting_in.v0" (fun event ->
          if
            JSON.(
              event |-> "level" |> as_int = publication_level
              && event |-> "slot_index" |> as_int = index)
          then Some ()
          else None)
    in
    Log.info
      "Waiting for reconstruction to be canceled because all shards were \
       received, fail if the reconstruction finishes" ;
    let promise_reconstruction_cancelled =
      Dal_node.wait_for
        observer
        "dal_reconstruct_no_missing_shard.v0"
        (fun event ->
          if
            JSON.(
              event |-> "level" |> as_int = publication_level
              && event |-> "slot_index" |> as_int = index)
          then Some true
          else None)
    in
    let promise_reconstruction_finished =
      Dal_node.wait_for observer "dal_reconstruct_finished.v0" (fun event ->
          if
            JSON.(
              event |-> "level" |> as_int = publication_level
              && event |-> "slot_index" |> as_int = index)
          then Some false
          else None)
    in
    let* success =
      Lwt.pick
        [promise_reconstruction_cancelled; promise_reconstruction_finished]
    in
    if success then unit
    else Test.fail ~__LOC__ "Reconstruction was not cancelled."
  in
  let* publication_level_bis, _commitment, () =
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
  Check.(publication_level_bis = publication_level)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected publication level, actual:%L, expected:%R" ;

  unit

let check_slot_attested node protocol parameters ~attested_level ~lag_index
    ~expected_attestation =
  let* metadata =
    Node.RPC.call node
    @@ RPC.get_chain_block_metadata ~block:(string_of_int attested_level) ()
  in
  let* attestation =
    match metadata.dal_attestation with
    | None ->
        Test.fail
          "Missing dal_attestation field in the metadata of the block at level \
           %d"
          attested_level
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
  return (attestation = expected_attestation)

let test_by_ignoring_topics protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  let peer_id dal_node = Dal_node.read_identity dal_node in

  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let attestation_lag = dal_parameters.attestation_lag in

  let index = 0 in
  let peers = [Dal_node.listen_addr dal_bootstrap] in

  let producer =
    Dal_node.create
      ~name:"producer"
      ~node
      ~ignore_pkhs:
        [
          Constant.bootstrap1.Account.public_key_hash;
          Constant.bootstrap2.Account.public_key_hash;
          Constant.bootstrap3.Account.public_key_hash;
        ]
      ()
  in
  let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
  let* () =
    let env =
      String_map.singleton Dal_node.ignore_topics_environment_variable "yes"
    in
    Dal_node.run ~env ~wait_ready:true ~event_level:`Debug producer
  in
  let* producer_peer_id = peer_id producer in

  let observer = Dal_node.create ~name:"observer" ~node () in
  let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug observer in
  let* observer_peer_id = peer_id observer in

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in

  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () = Dal_node.init_config ~attester_profiles:all_pkhs ~peers attester in
  let* () = Dal_node.run ~wait_ready:true ~event_level:`Debug attester in
  let* attester_peer_id = peer_id attester in

  Log.info
    "Waiting for grafting of the observer - producer connection, and observer \
     - attester connection" ;
  let check_producer_observer_grafts pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (observer, observer_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_observer_attester_grafts pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (observer, observer_peer_id)
         (attester, attester_peer_id)
         pkh
  in
  let check_graft_promises =
    List.map check_producer_observer_grafts all_pkhs
    @ List.map check_observer_attester_grafts all_pkhs
  in

  (* We need to bake some blocks until the L1 node notifies the DAL
       nodes that some L1 block is final so that the topic pkhs are
       known. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Connections grafted" ;

  let* before_publication_level = Client.level client in
  let published_slots = 3 in
  let published_levels =
    List.init published_slots (fun offset ->
        before_publication_level + offset + 1)
  in
  Log.info
    "Produce and publish %d slots at levels %a."
    published_slots
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
       Format.pp_print_int)
    published_levels ;
  let source = Constant.bootstrap1 in
  (* Build the [wait_for] promises. *)
  let wait_reconstruction ~published_level ~slot_index =
    Dal_node.wait_for observer "dal_reconstruct_finished.v0" (fun event ->
        if
          JSON.(
            event |-> "level" |> as_int = published_level
            && event |-> "slot_index" |> as_int = slot_index)
        then (
          Log.info
            "Finished reconstruction for slot at level %d"
            published_level ;
          Some ())
        else None)
  in
  let wait_for_reconstruction_promises =
    List.map
      (fun published_level ->
        wait_reconstruction ~published_level ~slot_index:index)
      published_levels
  in
  let wait_for_shards_promises =
    List.map
      (fun pkh ->
        List.map
          (fun published_level ->
            let level = published_level + attestation_lag - 1 in
            let* assigned_shard_indexes =
              Dal_RPC.(call attester @@ get_assigned_shard_indices ~level ~pkh)
            in
            wait_for_shards_promises
              ~dal_node:attester
              ~storage_profile:`Cache_only
              ~shards:assigned_shard_indexes
              ~published_level
              ~slot_index:index)
          published_levels)
      all_pkhs
    |> List.flatten
  in

  let rec repeat_publish offset =
    if offset >= published_slots then unit
    else (
      Log.info
        "Publish a slot at level %d"
        (before_publication_level + offset + 1) ;
      let* (`OpHash op_hash) =
        let content =
          Helpers.make_slot ~slot_size ("slot " ^ string_of_int offset)
        in
        let* commitment, proof =
          Helpers.store_slot producer ~slot_index:index content
        in
        (* TODO: we should not need to set a fee! *)
        publish_commitment ~source ~index ~commitment ~proof client ~fee:20_000
      in
      (* Bake a block to include the operation. *)
      let* () = bake_for client in
      (* Check that the operation is included. *)
      let* included_manager_operations =
        let manager_operation_pass = 3 in
        Node.RPC.(
          call node
          @@ get_chain_block_operation_hashes_of_validation_pass
               manager_operation_pass)
      in
      let () =
        Check.list_mem
          Check.string
          ~__LOC__
          op_hash
          included_manager_operations
          ~error_msg:
            "DAL commitment publishment operation not found in head block."
      in
      repeat_publish (offset + 1))
  in
  let* () = repeat_publish 0 in
  (* We bake one block so that the last publish operation becomes final, and
       therefore all reconstructions have started. *)
  let* () = bake_for client in
  Log.info "Waiting for finished reconstruction events" ;
  let* () = Lwt.join wait_for_reconstruction_promises in
  Log.info "Waiting for shard receipt events" ;
  let* () = Lwt.join wait_for_shards_promises in

  Log.info "Bake [attestation_lag] blocks and check that the slots are attested" ;
  let* () =
    let dal_node_endpoint =
      Dal_node.as_rpc_endpoint attester |> Endpoint.as_string
    in
    (* TODO https://gitlab.com/tezos/tezos/-/issues/8138
         When all bakers use the stream RPC, update this:
         Using [bake_for ~count:attestation_lag] makes the test fail, because
         "unable to get DAL attestation for <baker> in time". To be on the safe
         side, we wait a bit before baking the next block. *)
    repeat attestation_lag (fun () ->
        let* () =
          if Protocol.number protocol < 025 then Lwt_unix.sleep 0.1 else unit
        in
        bake_for client ~dal_node_endpoint)
  in

  let expected_attestation =
    assert (index = 0) ;
    Array.init number_of_slots (fun i -> i = index)
  in
  let rec check_attestation offset =
    if offset >= published_slots then unit
    else
      let* results =
        Lwt_list.mapi_s
          (fun lag_index attestation_lag ->
            let attested_level =
              before_publication_level + attestation_lag + offset + 1
            in
            check_slot_attested
              node
              protocol
              dal_parameters
              ~attested_level
              ~lag_index
              ~expected_attestation)
          dal_parameters.attestation_lags
      in
      if List.exists Fun.id results then check_attestation (offset + 1)
      else Test.fail "Expected attestation not found at any attested level"
  in
  check_attestation 0
