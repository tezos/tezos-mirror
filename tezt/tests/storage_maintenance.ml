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
  Node.wait_for node "start_context_split.v0" @@ fun _json ->
  if expected then Some ()
  else Test.fail "Unexpected start_context_split event caught"

let test_context_pruning_call =
  Protocol.register_test
    ~__FILE__
    ~title:(Format.asprintf "storage context pruning call")
    ~tags:[team; "storage"; "maintenance"; "context"; "pruning"]
  @@ fun protocol ->
  let* node1, client =
    Client.init_with_protocol
      ~nodes_args:Node.[Synchronisation_threshold 0; Context_pruning "disabled"]
      `Client
      ~protocol
      ()
  in
  (* As the context pruning is enabled by default,we specify nothing
     on the command line. *)
  let* node2 = Node.init ~name:"with_gc" Node.[Synchronisation_threshold 0] in
  let* () = Client.Admin.connect_address ~peer:node2 client in
  let* (_ : int) = Node.wait_for_level node2 1 in
  let blocks_per_cycle = 8 in
  let blocks_to_bake = 2 * blocks_per_cycle in
  let (_wait_no_context_gc : unit Lwt.t) =
    wait_for_context_gc node1 ~expected:false
  in
  let (_wait_no_context_split : unit Lwt.t) =
    wait_for_context_split node1 ~expected:false
  in
  let (wait_context_gc : unit Lwt.t) =
    wait_for_context_gc node2 ~expected:true
  in
  let (wait_context_split : unit Lwt.t) =
    wait_for_context_split node2 ~expected:true
  in
  let* () = bake_blocks node1 client ~blocks_to_bake in
  let* () = wait_context_gc and* () = wait_context_split in
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
    ~title:(Format.asprintf "storage maintenance disabled delay")
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

(* This is a temporary test to ensure that only the disabled mode is
   implemented. *)
let test_only_disabled_is_implemented =
  Protocol.register_test
    ~__FILE__
    ~title:(Format.asprintf "storage maintenance allows disabled only")
    ~tags:[team; "storage"; "maintenance"; "delay"; "enable"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun _protocol ->
  let node = Node.create Node.[Synchronisation_threshold 0] in
  let* () =
    Node.spawn_config_update node [Storage_maintenance_delay "enabled"]
    |> Process.check_error
         ~msg:(rex "only supports \"disabled\" mode")
         ~exit_code:124
  in
  unit

let register ~protocols =
  test_context_pruning_call protocols ;
  test_disabled_maintenance_delay protocols ;
  test_only_disabled_is_implemented protocols
