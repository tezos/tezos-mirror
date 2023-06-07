(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Synchronization state, prevalidator status
   Invocation:   dune exec tezt/tests/main.exe -- -v --file synchronisation_heuristic.ml
   Subject:      Check synchronization state and prevalidator status
*)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4459
   Re-enable the "synchronisation_threshold" tests in the CI once the flakiness has
   been fixed. *)

open Base

let wait_for ~statuses node =
  let filter json =
    let status = JSON.as_string json in
    Log.info "%s: %s" (Node.name node) status ;
    if List.exists (fun st -> String.equal st status) statuses then Some ()
    else None
  in
  Node.wait_for node "synchronisation_status.v0" filter

let wait_for_sync node =
  let filter json =
    let status = JSON.as_string json in
    Log.info "%s: %s" (Node.name node) status ;
    if String.equal status "synced" then Some () else None
  in
  let event = Node.wait_for node "synchronisation_status.v0" filter in
  (* A node may be synchronised before it is considered "ready". We check
     whether the node is (already) synchronized via an RPC. *)
  let is_synchronised =
    let* client = Client.init ~endpoint:(Node node) () in
    let* is_bootstrapped =
      RPC.Client.call client @@ RPC.get_chain_is_bootstrapped ()
    in
    if is_bootstrapped.sync_state = Synced then Lwt.return_unit
    else fst @@ Lwt.task ()
  in
  Lwt.pick [event; is_synchronised]

(* This test starts 4 + 1 nodes. The main nodes activate the alpha
   protocol. All the other nodes connects to the main nodes and
   synchronize themselves. Then we restart all the nodes and check
   they are all in the state mode `sync`. *)

let check_node_synchronization_state =
  let n = 4 in
  let blocks_to_bake = 5 in
  Protocol.register_test
    ~__FILE__
    ~title:"check synchronization state"
    ~tags:
      ["ci_disable"; "synchronisation_threshold"; "bootstrap"; "node"; "sync"]
  @@ fun protocol ->
  let* main_node = Node.init ~name:"main_node" [] in
  let* nodes =
    Lwt_list.map_p
      (fun i -> Node.init ~name:("node" ^ string_of_int i) [])
      (range 1 n)
  in
  Log.info "%d nodes initialized." (n + 1) ;
  let* client = Client.init ~endpoint:(Node main_node) () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  let* () =
    repeat blocks_to_bake (fun () ->
        Client.bake_for_and_wait ~minimal_timestamp:true client)
  in
  Log.info "Baked %d blocks." blocks_to_bake ;
  let* () =
    Lwt_list.iter_p
      (fun node ->
        Log.info "%s connects to %s." (Node.name main_node) (Node.name node) ;
        Client.Admin.connect_address client ~peer:node)
      nodes
  in
  let* () =
    Lwt_list.iter_p
      (fun node ->
        let* _ = Node.wait_for_level node (blocks_to_bake + 1) in
        unit)
      nodes
  in
  Log.info "Restarting the nodes..." ;
  let* _ =
    Lwt_list.iter_p (fun node -> Node.terminate node) (main_node :: nodes)
  in
  (* We register this event before restarting the node to avoid to register it too late. *)
  let synchronisation_events =
    List.map
      (fun node -> wait_for ~statuses:["synced"; "stuck"] node)
      (main_node :: nodes)
  in
  let* _ =
    Lwt_list.iter_p (fun node -> Node.run node []) (main_node :: nodes)
  in
  Log.info "Waiting for nodes to be synchronized." ;
  let* () = Lwt.join synchronisation_events in
  unit

(* In order to check that the prevalidator is not alive, we cannot
   rely on events because it's indecidable, thus we query a RPC that
   is reachable only if the prevalidator is running. *)
let check_is_prevalidator_started endpoint =
  let client = Client.create ~endpoint () in
  let process =
    Client.spawn_rpc
      GET
      (String.split_on_char '/' "chains/main/mempool/filter")
      client
  in
  Process.check ~expect_failure:false process

let check_is_prevalidator_closed endpoint =
  let client = Client.create ~endpoint () in
  let process =
    Client.spawn_rpc
      GET
      (String.split_on_char '/' "chains/main/mempool/filter")
      client
  in
  Process.check ~expect_failure:true process

(* This test starts 3 nodes and test whether or not their prevalidator
   activates. They should not be activated until they are
   bootstrapped. The first node activates alpha and bake a few
   blocks. All nodes synchronize themselves and we check if their
   validator are activated:
   - node 1 and 2 have bootstrapped successfully at the end and their validator
     is expected to have started,
   - node 3 has a synchronisation heuristic that is too high to successfully
     bootstrap in this test scenario: its validator is expected to not have
     started. *)
let check_prevalidator_start =
  Protocol.register_test
    ~__FILE__
    ~title:"Check prevalidator start"
    ~tags:
      [
        "ci_disable";
        "synchronisation_threshold";
        "bootstrap";
        "node";
        "prevalidator";
      ]
  @@ fun protocol ->
  let init_node threshold = Node.init [Synchronisation_threshold threshold] in
  let* node1 = init_node 0 in
  let* node2 = init_node 1 in
  let* node3 = init_node 10 in
  let nodes = [node1; node2; node3] in
  Log.info "%d nodes initialized." 3 ;
  let waiter_sync =
    Lwt_list.iter_p (fun node -> wait_for_sync node) [node1; node2]
  in
  let* client = Client.init ~endpoint:(Node node1) () in
  let* () = Client.activate_protocol ~protocol client ~timestamp:Now in
  Log.info "Activated protocol." ;
  let* () = Client.bake_for_and_wait ~minimal_timestamp:false client in
  let connect node node' =
    Log.info "%s connects to %s." (Node.name node) (Node.name node') ;
    Client.Admin.connect_address client ~peer:node'
  in
  let* () = connect node1 node2 in
  let* () = connect node2 node3 in
  let* () = connect node1 node3 in
  let* () =
    Lwt_list.iter_p
      (fun node ->
        let* _ = Node.wait_for_level node 1 in
        unit)
      nodes
  in
  Log.info "Waiting for nodes to be synchronized." ;
  let* () = waiter_sync in
  Log.info "Asserting that the prevalidator started for node 2." ;
  let* () = check_is_prevalidator_started (Node node2) in
  Log.info "Asserting that the prevalidator did not start for node 3." ;
  let* () = check_is_prevalidator_closed (Node node3) in
  unit

let sync_state_typ =
  let open RPC in
  Check.(
    convert
      (function
        | Synced -> "synced" | Unsynced -> "unsynced" | Stuck -> "stuck")
      string)

let check_sync_state ?__LOC__ ?endpoint client expected_state =
  let* sync_state =
    RPC.Client.call ?endpoint client @@ RPC.get_chain_is_bootstrapped ()
  in
  Check.(sync_state.sync_state = expected_state)
    sync_state_typ
    ~error_msg:"Expected node to be %R, was %L" ;
  unit

(* Threshold 0, peer always bootstrapped. *)
let test_threshold_zero =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: test threshold zero"
    ~tags:["ci_disable"; "synchronisation_threshold"; "bootstrap"; "threshold"]
  @@ fun protocol ->
  Log.info "Setup network" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 0; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* _ = Baker.init ~protocol node client in

  Log.info "Check that the node is bootstrapped" ;
  let* () = check_sync_state client Synced in
  unit

let check_is_bootstrapped ?__LOC__ ?endpoint client =
  let* sync_state =
    RPC.Client.call ?endpoint client @@ RPC.get_chain_is_bootstrapped ()
  in
  if not sync_state.bootstrapped then
    Test.fail "Expected node to be bootstrapped" ;
  unit

let connect_clique client =
  Cluster.meta_clique_lwt @@ fun peer peer' ->
  Log.debug "Connecting %s to %s" (Node.name peer) (Node.name peer') ;
  Client.Admin.connect_address ~endpoint:(Node peer) ~peer:peer' client

(* First peer has threshold zero, second peer has threshold one *)
let test_threshold_one =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: test threshold one"
    ~tags:["bootstrap"; "threshold"]
  @@ fun protocol ->
  Log.info "Add a first peer with threshold zero" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 1; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* _ = Baker.init ~protocol node client in

  Log.info "Check synchronisation state of first peer" ;
  let* () = check_sync_state client Synced in

  Log.info "Add a second peer with threshold one, and connect to the first" ;
  let* node1, client1 =
    Client.init_with_node
      ~nodes_args:[Connections 1; Synchronisation_threshold 1; Sync_latency 3]
      `Client
      ()
  in
  let* () = Client.Admin.connect_address client ~peer:node1 in

  Log.info "Check bootstrapped state of second peer" ;
  let* () = Client.bootstrapped client1 in

  unit

(* First peer has threshold zero, second peer has threshold one *)
let test_threshold_two =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: test threshold two"
    ~tags:["ci_disable"; "synchronisation_threshold"; "bootstrap"; "threshold"]
  @@ fun protocol ->
  Log.info "Add a first peer with threshold zero" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 3; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* _ = Baker.init ~protocol node client in

  Log.info "Add nodes and connect in clique" ;

  let* node1, client1 =
    Client.init_with_node
      ~nodes_args:[Connections 3; Synchronisation_threshold 2; Sync_latency 3]
      `Client
      ()
  in
  let* node2, client2 =
    Client.init_with_node
      ~nodes_args:[Connections 3; Synchronisation_threshold 2; Sync_latency 3]
      `Client
      ()
  in
  let* node3, client3 =
    Client.init_with_node
      ~nodes_args:[Connections 3; Synchronisation_threshold 1; Sync_latency 3]
      `Client
      ()
  in
  let nodes = [node; node1; node2; node3] in

  let* () = connect_clique client nodes in

  Log.info "Check bootstrapped state of second peer" ;
  (* - if we assume that all nodes are connected, and that
     [Sync_Latency] has not elapsed, then all node should be synced
     and hence bootstrapped.

     - if [Sync_latency] has elapsed, then all nodes will be [Stuck],
     and hence bootstrapped.
  *)
  let* () =
    let level = Node.get_level node in
    Lwt_list.iter_p
      (fun n ->
        let* (_ : int) = Node.wait_for_level n (level + 1) in
        unit)
      [node; node1; node2; node3]
  in

  let* () = Client.bootstrapped client in
  let* () = Client.bootstrapped client1 in
  let* () = Client.bootstrapped client2 in
  let* () = Client.bootstrapped client3 in

  unit

let test_threshold_stuck =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: test threshold stuck"
    ~tags:["ci_disable"; "synchronisation_threshold"; "bootstrap"; "threshold"]
  @@ fun protocol ->
  let sync_latency = 3 in

  Log.info "Add a first peer with threshold zero" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.
          [
            Connections 3; Synchronisation_threshold 0; Sync_latency sync_latency;
          ]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* baker = Baker.init ~protocol node client in

  Log.info "Bake a few blocks and kill baker" ;
  let* (level : int) = Node.wait_for_level node (Node.get_level node + 3) in
  let* () = Baker.terminate baker in

  Log.info "Add two additional peers" ;
  let* node1, client1 =
    Client.init_with_node
      ~nodes_args:
        [Connections 3; Synchronisation_threshold 2; Sync_latency sync_latency]
      `Client
      ()
  in
  let* node2, client2 =
    Client.init_with_node
      ~nodes_args:
        [Connections 3; Synchronisation_threshold 2; Sync_latency sync_latency]
      `Client
      ()
  in

  Log.info "Delay until sync_latency has expired" ;
  let* () = Lwt_unix.sleep (float_of_int (2 * sync_latency)) in

  Log.info "Connect nodes." ;
  let* () = connect_clique client [node; node1; node2] in

  Log.info "Wait for nodes 1 and 2 to catch up." ;
  let* _lvl1 = Node.wait_for_level node1 level
  and* _lvl2 = Node.wait_for_level node2 level in

  Log.info "Check that additional peers are bootstrapped and stuck" ;
  let* () = check_sync_state client1 Stuck in
  let* () = check_sync_state client2 Stuck in
  let* () = Client.bootstrapped client1 in
  let* () = Client.bootstrapped client2 in

  unit

let test_threshold_split_view =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: test threshold split view"
    ~tags:["ci_disable"; "synchronisation_threshold"; "bootstrap"; "threshold"]
  @@ fun protocol ->
  Log.info
    "Add two peers with threshold zero, and one with threshold 2 and a high \
     latency" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 3; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* node1, client1 =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 3; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* node2, client2 =
    Client.init_with_protocol
      ~nodes_args:
        Node.[Connections 3; Synchronisation_threshold 2; Sync_latency 15]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* _ = Baker.init ~protocol node client in
  let* () = connect_clique client [node; node1; node2] in

  Log.info "Test that all nodes bootstrap" ;
  let* () = check_sync_state client Synced in
  let* () = check_sync_state client1 Synced in
  let* () = Client.bootstrapped client2 in

  Log.info "Disconnect node1 from baker" ;
  let* () =
    let* node1_id = Node.wait_for_identity node1 in
    Client.Admin.ban_peer client ~peer:node1_id
  in
  let* () =
    let* node_id = Node.wait_for_identity node in
    Client.Admin.ban_peer client1 ~peer:node_id
  in

  Log.info "Delay for a few blocks" ;
  let* (_ : int) = Node.wait_for_level node (3 + Node.get_level node) in

  Log.info "Check that additional peers are bootstrapped and synced" ;
  let* () = check_sync_state client Synced in
  let* () = check_sync_state client1 Synced in
  let* () = check_sync_state client2 Synced in

  unit

(* Run many nodes, bake for a while, add a node and check it's bootstrapped
   when it should be. *)
let test_many_nodes_bootstrap =
  Protocol.register_test
    ~__FILE__
    ~title:"bootstrap: many nodes bootstrap"
    ~tags:["ci_disable"; "synchronisation_threshold"; "bootstrap"; "threshold"]
  @@ fun protocol ->
  let num_nodes = 8 in
  let running_time = 10.0 in

  Log.info "Add two peers" ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:
        Node.
          [Connections num_nodes; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* _ = Baker.init ~protocol node client in
  let* node1, client1 =
    Client.init_with_protocol
      ~nodes_args:
        Node.
          [Connections num_nodes; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* node2, client2 =
    Client.init_with_protocol
      ~nodes_args:
        Node.
          [Connections num_nodes; Synchronisation_threshold 0; Sync_latency 3]
      `Client
      ~protocol
      ~timestamp:Now
      ()
  in
  let* () = connect_clique client [node; node1; node2] in

  Log.info "Let the two peers bootstrap" ;
  let* () = Client.bootstrapped client in
  let* () = Client.bootstrapped client1 in

  Log.info "Add nodes" ;
  let* nodes =
    Lwt_list.map_s
      (fun _i ->
        Node.init
          Node.
            [Connections num_nodes; Synchronisation_threshold 0; Sync_latency 3])
      (Base.range 2 (num_nodes - 1))
  in
  let* () = connect_clique client nodes in
  let* () =
    Lwt_list.iter_s
      (fun peer -> Client.Admin.connect_address client ~peer)
      nodes
  in
  let* () =
    Lwt_list.iter_s
      (fun peer -> Client.Admin.connect_address client1 ~peer)
      nodes
  in
  let* () =
    Lwt_list.iter_s
      (fun peer -> Client.Admin.connect_address client2 ~peer)
      nodes
  in

  Log.info "Delay a bit" ;
  let* () = Lwt_unix.sleep running_time in

  Log.info "Add another node, connect it and check that it bootstraps" ;
  let* _node_last, client_last =
    Client.init_with_node
      ~nodes_args:
        Node.
          [
            Connections num_nodes;
            Synchronisation_threshold num_nodes;
            Sync_latency 3;
          ]
      `Client
      ()
  in
  let* () =
    Lwt_list.iter_s
      (fun peer -> Client.Admin.connect_address client_last ~peer)
      ([node; node1; node2] @ nodes)
  in
  let* () = Client.bootstrapped client_last in

  unit

let register ~protocols =
  check_node_synchronization_state protocols ;
  test_threshold_zero protocols ;
  test_threshold_one protocols ;
  test_threshold_two protocols ;
  test_threshold_stuck protocols ;
  test_threshold_split_view protocols ;
  test_many_nodes_bootstrap protocols ;
  check_prevalidator_start protocols
