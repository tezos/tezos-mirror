(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Node daemon
   Invocation:   dune exec tezt/tests/main.exe -- --file node_event_level.ml
   Subject:      Configuration of node event level. Incidentally, test
                 injection of transfer operations, baking, and a couple RPCs
                 that retrieve operations.
*)

let get_request_level = function
  | "debug" -> "request_completed_debug.v0"
  | "info" -> "request_completed_info.v0"
  | "notice" -> "request_completed_notice.v0"
  | level -> Test.fail "Incorrect %s level for request_completed event" level

(* Wait for a request event of the specified kind.
   Example of request of kind "inject":

     "event": {
       "request": {
         "request": "inject",
         "operation": {
           "branch": "BLMR9qLoCjnLrdc5br7sD7hZ6RfqfUZZaHgijQGkpzGeZFYm8ib",
           "data": "6c0002298c03ed7d454a101eb7022bc95f7e5f41ac78930301f70b00c0843d0000e7670f32038107a59a2b9cfefae36ea21f5aa63c006e4d3c29e6eaefddefead7225b2c2743afa3154de65393a5f6f857c15dc80b45b503a51eb30ead5d3e553050f66d001c48e4ce9e37eca415c3843f9143287c0f"
         }
       },
       "status": {
         "pushed": "2021-04-26T16:00:46.968-00:00",
         "treated": 1.9067e-05,
         "completed": 0.002623382
       }
     },
     "level": "notice"
*)
let wait_for_request_kind ~level (request_kind : string) node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when String.equal s request_kind -> Some ()
    | Some _ | None -> None
  in
  Node.wait_for node (get_request_level level) filter

(* Wait for the request signaling the injection of a new operation in
   the mempool. This event has level "info".
*)
let wait_for_injection = wait_for_request_kind ~level:"info" "inject"

(* Wait for the request signaling a flush of the state of the mempool.
   This event has level "notice".
*)
let wait_for_flush = wait_for_request_kind ~level:"info" "flush"

(* Wait for the request signaling the arrival of an operation in the mempool
   from a peer.
   Note: this event has level "debug", so the node needs to have event
   level set to "debug" for such an event to exist.
*)
let wait_for_arrival = wait_for_request_kind ~level:"debug" "arrived"

(* Inject a transfer operation from [client] and wait for an injection
   event on [node] (which should be the node associated to [client]).
*)
let transfer_and_wait_for_injection ?(wait_for_injection = wait_for_injection)
    node client amount_int giver_key receiver_key =
  let wait_for = wait_for_injection node in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int amount_int)
      ~giver:Account.(giver_key.alias)
      ~receiver:Account.(receiver_key.alias)
      client
  in
  let* () = wait_for in
  unit

(* Bake for [client] and wait for a flush event on [node] (which should
   be the node associated to this client). If [level] is provided, also
   wait for the node to reach this level. A specific [mempool] can be provided.
*)
let bake_wait_log ?level ?protocol ?mempool ?ignore_node_mempool node client =
  let baked = wait_for_flush node in
  let* () =
    Client.bake_for_and_wait ?protocol ?mempool ?ignore_node_mempool client
  in
  let* _ = baked in
  Log.info "Baked." ;
  match level with
  | Some lvl ->
      let* _ = Node.wait_for_level node lvl in
      Log.info "Node at level %d." lvl ;
      unit
  | _ -> unit

(* Get the hash of an operation from the json representing the operation. *)
let get_hash op = JSON.(op |-> "hash" |> as_string)

(* Get the list of hashes of the mempool's applied operations (using
   RPC get /chains/main/mempool/pending_operations that provides all
   the operations in the mempool). *)
let get_applied_operation_hash_list client =
  let* pending_ops =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  return (List.map get_hash JSON.(pending_ops |-> "applied" |> as_list))

(* Assert that [json] represents an empty list. *)
let check_json_is_empty_list ?(fail_msg = "") json =
  match JSON.(json |> as_list_opt) with
  | Some [] -> ()
  | _ ->
      let msg =
        if String.equal fail_msg "" then
          Format.sprintf "Expected an empty list, got: %s." (JSON.encode json)
        else fail_msg
      in
      Test.fail "%s" msg

(* Test.

   Aim: check that a node launched with "debug" event level performs
   various functions correctly: operation injection, baking, and a
   couple RPCs:
   - get /chains/main/mempool/pending_operations
     (RPC.get_mempool_pending_operations)
   - get /chains/main/blocks/head/operations (RPC.get_operations)

   Scenario:
   - Step 1: Start a node with event_level:debug, activate the protocol.
   - Step 2: Inject a transfer operation, test RPCs.
     2a) pending_operations should contain one applied operation.
     2b) operations in block should be empty.
   - Step 3: Bake, test RPCs.
     3a) pending_operations should be empty.
     3b) operations in block should contain the previously pending operation.
*)
let test_debug_level_misc =
  Protocol.register_test
    ~__FILE__
    ~title:"event level debug"
    ~tags:["node"; "event"]
  @@ fun protocol ->
  Log.info "Step 1: Start a node with event_level:debug, activate the protocol." ;
  let* node_1 = Node.init ~event_level:`Debug [Synchronisation_threshold 0] in
  let endpoint_1 = Client.(Node node_1) in
  let* client_1 = Client.init ~endpoint:endpoint_1 () in
  let* () = Client.activate_protocol_and_wait ~protocol client_1 in
  let* level = Node.get_level node_1 in
  Log.info "Node at level %d" level ;
  Log.info "Step 2: Inject a transfer operation, test RPCs." ;
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      1
      Constant.bootstrap1
      Constant.bootstrap2
  in
  Log.info "Injection done." ;
  Log.info "2a) pending_operations should contain one applied operation." ;
  let* applied_ophs = get_applied_operation_hash_list client_1 in
  Log.info "RPC.get_mempool_pending_operations done." ;
  let oph1 =
    match applied_ophs with
    | [x] -> x
    | _ -> Test.fail "Expected exactly one applied operation in mempool."
  in
  Log.info "Hash of injected operation: %s" oph1 ;
  Log.info "2b) operations in block should be empty." ;
  let* ops = Client.RPC.call client_1 @@ RPC.get_chain_block_operations () in
  Log.info "RPC.get_operations done." ;
  check_json_is_empty_list
    ?fail_msg:
      (Some
         "RPC.operations should return an empty list for the protocol \
          activation block.")
    ops ;
  Log.info "Step 3: Bake, test RPCs." ;
  let level = level + 1 in
  let* () = bake_wait_log ?level:(Some level) node_1 client_1 in
  Log.info "3a) pending_operations should be empty." ;
  let* applied_ophs = get_applied_operation_hash_list client_1 in
  Log.info "RPC.get_mempool_pending_operations done." ;
  (match applied_ophs with
  | [] -> ()
  | _ -> Test.fail "List of applied operations in mempool should be empty.") ;
  Log.info
    "3b) operations in block should contain the previously pending operation." ;
  let* ops = Client.RPC.call client_1 @@ RPC.get_chain_block_operations () in
  Log.info "RPC.get_operations done." ;
  (match JSON.(ops |> as_list_opt) with
  | Some [x1; x2; x3; x4] -> (
      List.iter check_json_is_empty_list [x1; x2; x3] ;
      match JSON.(x4 |> as_list_opt) with
      | Some [x] -> (
          match JSON.(x |-> "hash" |> as_string_opt) with
          | Some s when String.equal s oph1 -> ()
          | _ ->
              Test.fail
                "Fourth list returned by RPC.operations should contain only \
                 the previously applied operation.")
      | _ ->
          Test.fail
            "Fourth list returned by RPC.operations should contain exactly one \
             operation.")
  | _ -> Test.fail "RPC.operations should return a list of length 4.") ;
  unit

(* Wait for an event of name "set_head.v0".
   Note: this event has level "info", so the node needs to have event
   level set to either "debug" or "info" for such an event to exist.
*)
let wait_for_set_head node = Node.wait_for node "set_head.v0" (fun _ -> Some ())

(* Event handler that ensures there is no "set_head.v0" event (this event has
   level "info", so should not happen for nodes of event level "notice").
*)
let check_no_set_head event =
  if String.equal Node.(event.name) "set_head.v0" then
    Test.fail
      "Witnessed a set_head event (of level info), which should not happen for \
       a node configured with event level notice."

(* Assert that [incoming_level] is possible for events received from a
   node initialized with [config_level].
*)
let check_event_level ~config_level ~incoming_level =
  let config_level = Daemon.Level.to_string config_level in
  let to_int level =
    match String.lowercase_ascii level with
    | "debug" -> 0
    | "info" -> 1
    | "notice" -> 2
    | "warning" -> 3
    | "error" -> 4
    | "fatal" -> 5
    | _ ->
        Test.fail
          "Unexpected event level: %s (should be debug, info, notice, warning, \
           error, or fatal)."
          level
  in
  let config_int = to_int config_level
  and incoming_int = to_int incoming_level in
  if config_int >= 3 then
    Test.fail "Event level of a node should be notice, info, or debug." ;
  if config_int > incoming_int then
    Test.fail
      "Received an event of level %s while node was configured with event \
       level %s."
      incoming_level
      config_level ;
  ()

(* Ensure that all events of [node] have a level compatible with
   [config_level].
*)
let check_level_of_all_events node config_level =
  let handler event =
    match JSON.(Node.(event.value) |=> 1 |-> "level" |> as_string_opt) with
    | Some incoming_level -> check_event_level ~config_level ~incoming_level
    | None -> ()
  in
  Node.on_event node handler

(* Test.

   Aim: check that events of the right level are visible.

   Note: Possible levels are debug, info, notice, warning, error,
   and fatal. However, event level configuration for a node cannot
   be stricter than warning (see tezt/lib_tezos/node.mli:run).

   Scenario:
   - Step 1: Start three nodes with respective event levels debug, info, and
     notice.
   - Step 2: Setup event handlers to ensure that nodes never send events with
     level lower than their configuration.
   - Step 3: Connect the nodes and activate the protocol. Nodes 1 and 2
     should send a set_head event (of level info), but node 3 should not.
   - Step 4: inject a transfer operation from node 1, and witness an
     injection event from this node.
   - Step 5: Bake from node 3. Witness flush request (level notice) from all
     nodes, and set_head event (level info) from nodes 1 and 2.
   - Step 6: inject a transfer operations from nodes 2 and 3. Witness
     injection events (level notice) from these respective nodes, and operation
     arrival requests (level debug) from node 1.
*)
let test_event_levels =
  Protocol.register_test ~__FILE__ ~title:"event levels" ~tags:["node"; "event"]
  @@ fun protocol ->
  Log.info
    "Step 1: Start three nodes with respective event levels debug, info, and \
     notice." ;
  let node_1_event_level = `Debug in
  let node_2_event_level = `Info in
  let node_3_event_level = `Notice in
  let* node_1 =
    Node.init ~event_level:node_1_event_level [Synchronisation_threshold 0]
  and* node_2 =
    Node.init ~event_level:node_2_event_level [Synchronisation_threshold 0]
  and* node_3 =
    Node.init ~event_level:node_3_event_level [Synchronisation_threshold 0]
  in
  let endpoint_1 = Client.(Node node_1)
  and endpoint_2 = Client.(Node node_2)
  and endpoint_3 = Client.(Node node_3) in
  let* client_1 = Client.init ~endpoint:endpoint_1 ()
  and* client_2 = Client.init ~endpoint:endpoint_2 ()
  and* client_3 = Client.init ~endpoint:endpoint_3 () in
  Log.info
    "Step 2: Setup event handlers to ensure that nodes never send events with \
     level lower than their configuration." ;
  let () = check_level_of_all_events node_1 node_1_event_level in
  let () = check_level_of_all_events node_2 node_2_event_level in
  let () = check_level_of_all_events node_3 node_3_event_level in
  Log.info
    "Step 3: Connect the nodes and activate the protocol. Nodes 1 and 2 should \
     send a set_head event (of level info), but node 3 should not." ;
  let* () = Client.Admin.connect_address client_1 ~peer:node_2
  and* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let wait1a = wait_for_set_head node_1 in
  let wait2a = wait_for_set_head node_2 in
  let () = Node.on_event node_3 check_no_set_head in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Protocol activated." ;
  let* () = wait1a in
  let* () = wait2a in
  Log.info "set_head received from nodes 1 and 2." ;
  let level = 1 in
  let* _ = Node.wait_for_level node_1 level
  and* _ = Node.wait_for_level node_2 level
  and* _ = Node.wait_for_level node_3 level in
  Log.info "All nodes at level %d." level ;
  Log.info
    "Step 4: inject a transfer operation from node 1, and witness an injection \
     event from this node." ;
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      1
      Constant.bootstrap1
      Constant.bootstrap2
  in
  Log.info "NOTHING HAPPENS" ;
  Log.info "Injection event received from node 1." ;
  Log.info
    "Step 5: Bake from node 3. Witness flush request (level notice) nodes, and \
     set_head event (level info) from nodes 1 and 2." ;
  let wait1b = wait_for_flush node_1 in
  let wait2b = wait_for_flush node_2 in
  let wait1c = wait_for_set_head node_1 in
  let wait2c = wait_for_set_head node_2 in
  let* () = Client.bake_for_and_wait client_3 in
  let* () = wait1b in
  let* () = wait2b in
  let* () = wait1c in
  let* () = wait2c in
  Log.info "Flush and set_head received." ;
  Log.info
    "Step 6: inject a transfer operations from nodes 2 and 3. Witness \
     injection events (level notice) from the respective nodes, and operation \
     arrival requests (level debug) from node 1." ;
  let wait1d = wait_for_arrival node_1 in
  let* () =
    transfer_and_wait_for_injection
      node_2
      client_2
      2
      Constant.bootstrap2
      Constant.bootstrap3
  in
  let* () = wait1d in
  Log.info "Injection and arrival received for first transfer." ;
  let wait1e = wait_for_arrival node_1 in
  Log.info "BEFORE TRANSFER" ;

  (* Since this node has its event at notice level we cannot use the
     request_completed_info event to wait for the injection. Instead we use the
     operation_injected event at notice level. *)
  let wait_for_injection node =
    Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
  in
  let* () =
    transfer_and_wait_for_injection
      ~wait_for_injection
      node_3
      client_3
      3
      Constant.bootstrap3
      Constant.bootstrap4
  in
  Log.info "BEFORE ARRIVAL" ;
  let* () = wait1e in
  Log.info "Injection and arrival received for second transfer." ;
  unit

let register ~protocols =
  test_debug_level_misc protocols ;
  test_event_levels protocols
