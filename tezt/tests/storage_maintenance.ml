(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)
(* Testing
   -------
   Component: Storage maintenance
   Invocation: dune exec tezt/tests/main.exe -- -f storage_maintenance.ml
   Subject: Tests the storage maintenance behaviour
*)

let team = Tag.layer1

let bake_blocks node client ~blocks_to_bake =
  Log.info "Baking a batch of %d blocks on %s" blocks_to_bake (Node.name node) ;
  repeat blocks_to_bake @@ fun () ->
  Client.bake_for_and_wait
    ~endpoint:(Node node)
    ~node
    ~minimal_timestamp:true
    client

let wait_for_context_gc node ~expected =
  Node.wait_for node "start_context_gc.v0" @@ fun _json ->
  if expected then Some ()
  else Test.fail "Unexpected start_context_gc event caught"

let wait_for_context_split node ~expected =
  let filter json =
    let level = JSON.(json |> as_int) in
    if expected then Some level
    else Test.fail "Unexpected start_context_split event caught"
  in

  Node.wait_for node "start_context_split.v0" filter

let test_context_pruning_call =
  Protocol.register_test
    ~__FILE__
    ~title:"storage context pruning call"
    ~tags:[team; "storage"; "maintenance"; "context"; "pruning"]
  @@ fun protocol ->
  let* node1, client =
    Client.init_with_protocol
      ~node_name:"no_gc"
      ~nodes_args:Node.[Synchronisation_threshold 0; Disable_context_pruning]
      `Client
      ~protocol
      ()
  in
  (* As the context pruning is enabled by default,we specify nothing
     on the command line. *)
  let* node2 =
    Node.init
      ~name:"with_gc"
      (* Disable the storage maintenance delay to have a deterministic
         behavior. *)
      Node.[Synchronisation_threshold 0; Storage_maintenance_delay "disabled"]
  in
  let* () = Client.Admin.connect_address ~peer:node2 client in
  let* (_ : int) = Node.wait_for_level node2 1 in
  let blocks_per_cycle = 8 in
  let blocks_to_bake = 2 * blocks_per_cycle in
  let (_wait_no_context_gc : unit Lwt.t) =
    wait_for_context_gc node1 ~expected:false
  in
  let (_wait_no_context_split : int Lwt.t) =
    wait_for_context_split node1 ~expected:false
  in
  let (wait_context_gc : unit Lwt.t) =
    wait_for_context_gc node2 ~expected:true
  in
  let (wait_context_split : int Lwt.t) =
    wait_for_context_split node2 ~expected:true
  in
  let* () = bake_blocks node1 client ~blocks_to_bake in
  let* () = wait_context_gc and* (_ : int) = wait_context_split in
  unit

let wait_for_complete_storage_maintenance node target =
  let filter json =
    let level = JSON.(json |> as_int) in
    if level = target then Some () else None
  in
  let wait_for_ending_merge () =
    Node.wait_for node "end_merging_stores.v0" @@ fun _json -> Some ()
  in
  let* () = Node.wait_for node "start_merging_stores.v0" filter in
  wait_for_ending_merge ()

let test_disabled_maintenance_delay =
  Protocol.register_test
    ~__FILE__
    ~title:"storage maintenance disabled delay"
    ~tags:[team; "storage"; "maintenance"; "delay"; "disabled"]
  @@ fun protocol ->
  let* node =
    Node.init
      Node.[Synchronisation_threshold 0; Storage_maintenance_delay "disabled"]
  in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol_and_wait ~protocol ~node client in
  let blocks_per_cycle = 8 in
  let blocks_to_bake = 2 * blocks_per_cycle in
  let wait_merge =
    wait_for_complete_storage_maintenance node (blocks_per_cycle + 1)
  in
  let* () = bake_blocks node client ~blocks_to_bake in
  Log.info "Waiting for the merge" ;
  let* () = wait_merge in
  unit

(* This test aims to check the behavior of the custom delayed storage
   maintenance.  To do so, it will start 2 nodes, 1 with the delay set
   to 2 and 1 without any delay and check the following behavior:

                    regular_node           delayed_node
    (A) LEVEL 1 : --------------------------------------
         /\              |                       |
    blocks_per_cycle     |                       |
         \/              |                       |
    (B) LEVEL 9 :  (storage maint.)              |
         /\              |                       |
     custom_delay        |                       |
         \/              |                       |
        LEVEL 11 :       |           (delayed storage maint.)
         /\              |                       |
   (blocks_per_cycle     |                       |
          -              |                       |
     custom_delay)       |                       |
         \/              |                       |
    (C) LEVEL 17 : (storage maint.)          (restart)
         /\              |                       |
     custom_delay        |                       |
         \/              |                       |
        LEVEL 19 :       |           (delayed storage maint.)
         /\              |                       |
   (blocks_per_cycle     |                       |
          -              |                       |
     custom_delay)       |                       |
        LEVEL 25 : (storage maint.)   (restart + disable delay)
        LEVEL 26 :       |                 (storage maint.)
*)
let test_custom_maintenance_delay =
  Protocol.register_test
    ~__FILE__
    ~title:"storage maintenance custom delay"
    ~tags:[team; "storage"; "maintenance"; "delay"; "custom"]
  @@ fun protocol ->
  let custom_delay = 2 in
  let* delayed_node =
    Node.init
      ~name:"delayed_node"
      Node.
        [
          Synchronisation_threshold 0;
          Storage_maintenance_delay (string_of_int custom_delay);
        ]
  in
  let* regular_node =
    Node.init
      ~name:"regular_node"
      (* Disable the storage maintenance delay to have a deterministic
         behavior. *)
      Node.[Synchronisation_threshold 0; Storage_maintenance_delay "disabled"]
  in
  let* client = Client.init ~endpoint:(Node delayed_node) () in
  let* () = Client.Admin.connect_address ~peer:regular_node client in
  let* () =
    Client.activate_protocol_and_wait ~protocol ~node:delayed_node client
  in
  let* (_ : int) = Node.wait_for_level regular_node 1 in
  let blocks_per_cycle = 8 in
  (* Step A: We bake enough blocks to trigger the first storage
     maintenance. We expect the regular node to merge immediately and
     the delayed node to merge after the given delay. *)
  let merge_level_A = 1 in
  let wait_context_split_A =
    wait_for_context_split delayed_node ~expected:true
  in
  let wait_delayed_storage_maintenance_A =
    wait_for_complete_storage_maintenance delayed_node merge_level_A
  in
  let wait_regular_storage_maintenance_A =
    wait_for_complete_storage_maintenance regular_node merge_level_A
  in
  let* () =
    let nb = blocks_per_cycle in
    Log.info "Baking %d blocks to trigger the regular storage maintenance" nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  Log.info "Waiting for the regular storage maintenance" ;
  let* () = wait_regular_storage_maintenance_A in
  (* We must ensure that the context split is not delayed. Indeed, for
     the sake of performance, the context split aims to be called on the
     block candidate to a future GC. See
     [Lib_context.Sigs.Context.split] for more details. *)
  Log.info "Waiting for the context split event" ;
  let* split_level_A = wait_context_split_A in
  Check.(
    (merge_level_A = split_level_A)
      int
      ~error_msg:"split level was expected on level %L but found on %R") ;
  let* () =
    let nb = custom_delay in
    Log.info "Baking %d blocks to trigger the delayed storage maintenance" nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  Log.info "Waiting for the delayed storage maintenance" ;
  let* () = wait_delayed_storage_maintenance_A in
  (* Step B: We bake enough blocks to trigger the delayed maintenance
     but restart the node to check that the delay is maintained. *)
  let merge_level_B = merge_level_A + blocks_per_cycle in
  let wait_context_split_B =
    wait_for_context_split delayed_node ~expected:true
  in
  let wait_regular_storage_maintenance_B =
    wait_for_complete_storage_maintenance regular_node merge_level_B
  in
  let* () =
    let nb = blocks_per_cycle - custom_delay in
    Log.info "Baking %d blocks to trigger the regular storage maintenance" nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  Log.info "Waiting for the context split event" ;
  let* split_level_B = wait_context_split_B in
  Check.(
    (merge_level_B = split_level_B)
      int
      ~error_msg:"split level was expected on level %L but found on %R") ;
  let* () = wait_regular_storage_maintenance_B in
  Log.info "Restarting delayed storage maintenance node" ;
  let* () = Node.terminate delayed_node in
  let* () = Node.run delayed_node [] in
  let* () = Node.wait_for_ready delayed_node in
  let* () =
    let nb = 1 in
    Log.info
      "Baking %d blocks should not trigger delayed storage maintenance"
      nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  let wait_delayed_storage_maintenance_B =
    wait_for_complete_storage_maintenance delayed_node merge_level_B
  in
  let* () =
    let nb = 1 in
    Log.info "Baking %d blocks should trigger delayed storage maintenance" nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  let* () = wait_delayed_storage_maintenance_B in
  let merge_level_C = merge_level_B + blocks_per_cycle in
  let* () =
    let nb = blocks_per_cycle - custom_delay in
    Log.info "Baking %d blocks to trigger the delayed storage maintenance" nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  Log.info "Restarting node and disable storage maintenance delay" ;
  let* () = Node.terminate delayed_node in
  let* () = Node.run delayed_node [Storage_maintenance_delay "disabled"] in
  let* () = Node.wait_for_ready delayed_node in
  let wait_delayed_storage_maintenance_C =
    wait_for_complete_storage_maintenance delayed_node merge_level_C
  in
  let* () =
    let nb = 1 in
    Log.info
      "Baking %d blocks should reset delayed storage maintenance and trigger \
       the merge"
      nb ;
    bake_blocks delayed_node client ~blocks_to_bake:nb
  in
  let* () = wait_delayed_storage_maintenance_C in
  unit

(* The exclusion and limit are computed according to the
   `Lib_shell_services.Storage_maintenance.default_auto_delay`
   function. *)
let check_auto_delay_value ~blocks_per_cycle ~target ~level =
  let exclusion = max 1 (blocks_per_cycle / 20) in
  let limit = blocks_per_cycle / 2 in
  let generated_delay = target - level in
  Check.(
    (generated_delay >= exclusion)
      int
      ~error_msg:"delay is %L but is expected to be above %R") ;
  Check.(
    (generated_delay <= limit)
      int
      ~error_msg:"delay is %L but is expected to be below %R") ;
  unit

let wait_for_auto_delayed_maintenance node =
  let filter json =
    let mode = JSON.(json |-> "mode" |> as_string) in
    let target = JSON.(json |-> "level" |> as_int) in
    if mode = "auto" then Some target else None
  in
  Node.wait_for node "start_delayed_maintenance.v0" filter

let get_blocks_per_cycle client =
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(constants |-> "blocks_per_cycle" |> as_int) in
  return blocks_per_cycle

(* This test aims to check the behaviour of the auto mode of the
   storage maintenance. As this "auto" parameter introduces
   randomness, the test will run several steps to, hopefully, have
   various delayed values.
   A step consist in:
   - bake several blocks to trigger a delayed maintenance,
   - check that the delay respects the constraints,
   - bake several blocks to actually merge.
*)
let test_auto_maintenance_delay =
  Protocol.register_test
    ~__FILE__
    ~title:"storage maintenance auto delay"
    ~tags:[team; "storage"; "maintenance"; "delay"; "auto"]
  @@ fun protocol ->
  let* delayed_node =
    Node.init
      ~name:"delayed_node"
      ~event_sections_levels:[("node.store", `Info)]
      (* No need to set the storage maintenance flag, it is expected
         to be set to "auto", by default, by the node. *)
      Node.[Synchronisation_threshold 0]
  in
  let* client = Client.init ~endpoint:(Node delayed_node) () in
  let* () =
    Client.activate_protocol_and_wait ~protocol ~node:delayed_node client
  in
  let* blocks_per_cycle = get_blocks_per_cycle client in
  let step cpt ~next_cycle_dist =
    Log.info "Starting step %d" cpt ;
    let wait_context_split =
      wait_for_context_split delayed_node ~expected:true
    in
    let wait_delayed_maintenance =
      wait_for_auto_delayed_maintenance delayed_node
    in
    Log.info "Bake %d blocks to trigger a store merge" next_cycle_dist ;
    let* () = bake_blocks delayed_node client ~blocks_to_bake:next_cycle_dist in
    Log.info "Wait for the maintenance delay event" ;
    let* next_target = wait_delayed_maintenance in
    let merge_target_level = (cpt * blocks_per_cycle) + 1 in
    let merge_trigger_level = ((cpt + 1) * blocks_per_cycle) + 1 in
    (* We must ensure that the context split is not delayed. Indeed, for
       the sake of performance, the context split aims to be called on the
       block candidate to a future GC. See
       [Lib_context.Sigs.Context.split] for more details. *)
    Log.info "Waiting for the context split event" ;
    let* split_level = wait_context_split in
    Check.(
      (merge_target_level = split_level)
        int
        ~error_msg:"split level was expected on level %L but found on %R") ;
    let* () =
      check_auto_delay_value
        ~blocks_per_cycle
        ~target:next_target
        ~level:merge_trigger_level
    in
    let* current_level = Node.get_level delayed_node in
    let just_before_delayed_trigger_level = next_target - current_level - 1 in
    Log.info
      "Bake enough blocks (%d) to be 1 block before the trigger of the delayed \
       maintenance"
      just_before_delayed_trigger_level ;
    let* () =
      bake_blocks
        delayed_node
        client
        ~blocks_to_bake:just_before_delayed_trigger_level
    in
    let wait_merge =
      wait_for_complete_storage_maintenance delayed_node merge_target_level
    in
    Log.info "Bake 1 block to trigger the actual delayed maintenance" ;
    let* () = bake_blocks delayed_node client ~blocks_to_bake:1 in
    let* () = wait_merge in
    let to_next_cycle =
      blocks_per_cycle - just_before_delayed_trigger_level - 1
    in
    Log.info "Step %d finished" cpt ;
    return to_next_cycle
  in
  let run_steps nb =
    let rec loop cpt acc =
      if cpt >= nb then unit
      else
        let* acc = step cpt ~next_cycle_dist:acc in
        loop (cpt + 1) acc
    in
    loop 0 8
  in
  (* Run several steps to get various "auto" (random) delays. *)
  run_steps 3

let register ~protocols =
  test_context_pruning_call protocols ;
  test_disabled_maintenance_delay protocols ;
  test_custom_maintenance_delay protocols ;
  test_auto_maintenance_delay protocols
