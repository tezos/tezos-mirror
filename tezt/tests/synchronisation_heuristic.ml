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

open Base

let wait_for ~statuses node =
  let filter json =
    match JSON.(json |=> 1 |-> "event" |> as_string_opt) with
    | None ->
        Log.info "%s: none" (Node.name node) ;
        None
    | Some status ->
        Log.info "%s: %s" (Node.name node) status ;
        if List.exists (fun st -> String.equal st status) statuses then Some ()
        else None
  in
  Node.wait_for node "node_chain_validator.v0" filter

let wait_for_sync node =
  let filter json =
    match JSON.(json |=> 1 |-> "event" |> as_string_opt) with
    | None ->
        Log.info "%s: none" (Node.name node) ;
        None
    | Some status ->
        Log.info "%s: %s" (Node.name node) status ;
        if String.equal status "synced" then Some () else None
  in
  let event = Node.wait_for node "node_chain_validator.v0" filter in
  (* A node may be synchronised before it is considered "ready". We check
     whether the node is (already) synchronized via an RPC. *)
  let is_synchronised =
    let* client = Client.init ~endpoint:(Node node) () in
    let* json = RPC.is_bootstrapped client in
    if String.equal JSON.(json |-> "sync_state" |> as_string) "synced" then
      Lwt.return_unit
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
    ~tags:["bootstrap"; "node"; "sync"]
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
        Client.bake_for ~minimal_timestamp:true client)
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
    ~tags:["bootstrap"; "node"; "prevalidator"]
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
  let* () = Client.activate_protocol ~protocol client ~timestamp_delay:0. in
  Log.info "Activated protocol." ;
  let* () = Client.bake_for ~minimal_timestamp:false client in
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

let register ~protocols =
  check_node_synchronization_state ~protocols ;
  check_prevalidator_start ~protocols
