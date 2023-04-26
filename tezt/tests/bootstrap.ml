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

let is_connected node ~peer_id =
  let* response = RPC.get_network_connection peer_id |> RPC.call_raw node in
  match response.code with
  | 200 -> return true
  | 404 -> return false
  | code ->
      Test.fail "unexpected response code in Bootstrap.is_connected: %d" code

let wait_for_insufficient_history node =
  Node.wait_for node "insufficient_history.v0" (fun _ -> Some ())

(* FIXME: this is not robust since we cannot catch the bootstrapped event precisely. *)
let bootstrapped_event =
  let fulfilled = ref false in
  fun resolver Node.{name; _} ->
    if name = "bootstrapped.v0" && not !fulfilled then (
      fulfilled := true ;
      Lwt.wakeup resolver ())

(* This test replicates part of the flextesa test "command_node_synchronization".
   It checks the synchronization of two nodes depending on their history mode.
   The beginning of the scenario is the following:

   1. Node 1 bakes some blocks and Node 2 catches up with Node 1;

   2. Node 2 is killed;

   3. Node 1 bakes many blocks (longer than a cycle).

   What follows depends on the history mode of Node 1.

   a) In rolling mode, synchronization should fail because Node 1 and Node 2 have
   no common ancestor. We check that the caboose for Node 1 is indeed higher than
   when Node 2 was killed.

   b) Otherwise, we check that both nodes synchronize. In full mode, we also check
   that the savepoint is higher than when Node 2 was killed. *)
let check_bootstrap_with_history_modes hmode1 hmode2 =
  (* Number of calls to [octez-client bake for] once the protocol is activated,
     before we kill [node_2]. *)
  let bakes_before_kill = 9 in

  let max_operations_ttl = 1 in

  (* TODO-TB: update the doc strings below, written for
     max_operations_ttl = 0. *)

  (* Number of calls to [octez-client bake for] while [node_2] is not
     running. This number is high enough so that it is bigger than the
     Last-Allowed-Fork-Level or the caboose.

     Since the caboose depends on [max_op_ttl] which is set to [0]
     with the consensus algorithm Emmy* we bake [1 + bakes_before_kill
     + bakes_during_kill = 49] blocks.

     The rationale behind this number is the following:

     The test depends on two special blocks called [checkpoint] (which
     in this case is the same as the [last allowed fork level] since
     no target is not set on the command-line) and [caboose]. The
     [checkpoint] is the block level (often we refer it as the level
     directly) for which there is not reorganisation below this point.
     The [caboose] is the lowest level for which the store knows the
     [block_header] associated. These values are updated by the shell
     using information from the economic protocol when a new cycle
     starts.

     - When the checkpoint is set, its level is [preserved_blocks =
     preserved_cycles * blocks_per_cycle] behind the current level of
     the head

     - When the [caboose] is set, its level is [max(0, (checkpoint -
     max(preserved_blocks,max_op_ttl)))]. In [Full] and [Archive]
     mode, the [caboose] value is always [0]. For this test, we ensure
     that [preserved_blocks] and [max_op_ttl] are also [0] to save
     some computational time.

     These values are set when the head changes with level [level = 1
     mod blocks_per_cycle] (the modulo 1 comes from the activation
     block).

     In sandbox mode, we have [preserved_cycles = 2] and
     [blocks_per_cycle = 8].

     Hence, when [node_1] has baked [49] blocks, the checkpoint should
     be at level [49 - 16 = 33] and the [caboose] in rolling history
     mode should be at level [33 - 0 = 33] (and [0] for the other
     history modes). The [0] comes from as explained above that
     [max_op_ttl] and [preserved_blocks] are both set to [0].

     When the [node_1] is in rolling mode, we want to ensure that it
     can't synchronise with [node_2]. Consequently, we want to ensure
     that the [caboose] of [node_1] is above the level of [node_2]
     which is [1+bakes_before_kill = 10].

     Since, the [caboose] is updated at every cycle, the values it
     takes in sandbox mode are [1;9;17; ...]. [17] is the first value
     for which it prevents the synchronisation with [node_2].

     Consequently, to have a [caboose] at level [17] we need a
     checkpoint at level [17] and so the head should be, at least, at
     level [33].

     However, the [caboose] is set asynchronously and checking the
     level of the node is not enough. We have to ensure that the
     [caboose] was set too. This should be done when the [store]
     finishes its merge. This is why we bake [2] more cycles and that
     finally the number of baked blocks is [49].

     To ensure that, we need first to catch an event which says which
     cycle (up to which block) is being merged and then wait for the
     event which indicates the merge is over.

     This may be flaky if merging a store is way slower than baking
     blocks since the store does not trigger a merge when there is
     already one merge in progress. However, we do not observe such
     behavior for this test and we do not handle that currently. *)

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/1337

     This bug may create flakyness. We avoid it by baking [16] more
     blocks. *)
  let bakes_during_kill = 7 + 16 in
  let last_cycle_being_merged = ref false in
  let on_starting_merge_event node =
    Node.on_event node @@ fun Node.{name; value; timestamp = _} ->
    if name = "start_merging_stores.v0" then
      let level = JSON.(value |> as_int) in
      if level = bakes_during_kill + 1 + bakes_before_kill - 16 then
        last_cycle_being_merged := true
  in
  let wait_for_end_merge_event node last_cycle_being_merged =
    Node.wait_for node "end_merging_stores.v0" @@ fun _json ->
    if !last_cycle_being_merged then Some () else None
  in
  let hmode1s = Node.show_history_mode hmode1 in
  let hmode2s = Node.show_history_mode hmode2 in
  Protocol.register_test
    ~__FILE__
    ~title:(Format.sprintf "node synchronization (%s / %s)" hmode1s hmode2s)
    ~tags:
      [
        "bootstrap";
        "node";
        "sync";
        "activate";
        "bake";
        "primary_" ^ hmode1s;
        "secondary_" ^ hmode2s;
      ]
  @@ fun protocol ->
  (* Initialize nodes and client. *)
  let* node_1 =
    Node.init [Synchronisation_threshold 0; Connections 1; History_mode hmode1]
  and* node_2 = Node.init [Connections 1; History_mode hmode2] in
  let endpoint_1 = Client.(Node node_1) in
  let* node2_identity = Node.wait_for_identity node_2 in
  let* client = Client.init ~endpoint:endpoint_1 () in
  (* Connect node 1 to node 2 and start baking. *)
  let* () = Client.Admin.connect_address client ~peer:node_2 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      [(["max_operations_time_to_live"], `Int max_operations_ttl)]
  in
  let* () = Client.activate_protocol ~protocol client ~parameter_file in
  Log.info "Activated protocol." ;
  let* () =
    repeat bakes_before_kill (fun () -> Client.bake_for_and_wait client)
  in
  let* _ = Node.wait_for_level node_1 (bakes_before_kill + 1)
  and* _ = Node.wait_for_level node_2 (bakes_before_kill + 1) in
  Log.info "Both nodes are at level %d." (bakes_before_kill + 1) ;
  let _ = on_starting_merge_event node_1 in
  let wait_for_last_cycle =
    wait_for_end_merge_event node_1 last_cycle_being_merged
  in
  (* Kill node 2 and continue baking without it. *)
  let* () = Node.terminate node_2 in
  let* () =
    repeat bakes_during_kill (fun () -> Client.bake_for_and_wait client)
  in
  (* Restart node 2 and let it catch up. *)
  Log.info "Baked %d times with node_2 down, restart node_2." bakes_during_kill ;
  let final_level = 1 + bakes_before_kill + bakes_during_kill in
  let* _ = Node.wait_for_level node_1 final_level in
  let* () = wait_for_last_cycle in
  let* () = Node.run node_2 [Synchronisation_threshold 1; Connections 1] in
  let* _ = Node.wait_for_ready node_2 in
  (* Register the unknown ancestor event before connecting node 2 to node 1
     to ensure that we don't miss it because of a race condition. *)
  let node_2_catched_up =
    match hmode1 with
    | Full _ | Archive ->
        let* _ = Node.wait_for_level node_2 final_level in
        unit
    | Rolling _ ->
        (* In rolling mode, node 2 cannot catch up. We get an unknown ancestor event instead. *)
        Lwt.pick
          [
            (let* _ = Node.wait_for_level node_2 (bakes_before_kill + 2) in
             Test.fail
               "node_2 is not supposed to progress when node_1 is in rolling \
                mode");
            wait_for_insufficient_history node_2;
          ]
  in
  let* () = Client.Admin.connect_address client ~peer:node_2 in
  let* () = node_2_catched_up in
  (* Node 2 has caught up, check its checkpoint level depending on history mode. *)
  let* () =
    match hmode1 with
    | Full _ ->
        let* {level = savepoint; _} =
          RPC.Client.call ~endpoint:endpoint_1 client
          @@ RPC.get_chain_level_checkpoint ()
        in
        if savepoint <= bakes_before_kill then
          Test.fail
            "checkpoint level (%d) is lower or equal to the starting level (%d)"
            savepoint
            bakes_before_kill ;
        return ()
    | Rolling _ ->
        let* {level = caboose; _} =
          RPC.Client.call ~endpoint:endpoint_1 client
          @@ RPC.get_chain_level_caboose ()
        in
        if caboose <= bakes_before_kill then
          Test.fail
            "caboose level (%d) is lower or equal to the starting level (%d)"
            caboose
            bakes_before_kill ;
        return ()
    | _ -> return ()
  in
  (* Check whether the nodes are still connected. *)
  match hmode1 with
  | Full _ | Archive ->
      let* b = is_connected node_1 ~peer_id:node2_identity in
      if not b then Test.fail "expected the two nodes to be connected" else unit
  | Rolling _ ->
      let* b = is_connected node_1 ~peer_id:node2_identity in
      if b then Test.fail "expected the two nodes NOT to be connected" else unit

let check_rpc_force_bootstrapped () =
  Test.register
    ~__FILE__
    ~title:(sf "RPC force bootstrapped")
    ~tags:["rpc"; "bootstrapped"]
  @@ fun () ->
  Log.info "Start a node." ;
  let* node = Node.init [Synchronisation_threshold 255] in
  let* client = Client.init ~endpoint:(Node node) () in
  let bootstrapped_promise, bootstrapped_resolver = Lwt.task () in
  Node.on_event node (bootstrapped_event bootstrapped_resolver) ;
  Log.info "Force the node to be bootstrapped." ;
  let* _ = RPC.Client.call client @@ RPC.patch_chain_bootstrapped true in
  Log.info "Waiting for the node to be bootstrapped." ;
  let* () = bootstrapped_promise in
  unit

let register ~protocols =
  let archive = Node.Archive in
  let full = Node.Full None in
  let rolling = Node.Rolling None in
  (* This parameter is used in the special case we run two rolling
     nodes. To ensure two nodes cannot reconnect, we need to bake some
     blocks. Putting the number `0` in parameters allows to
     save 16 blocks. *)
  let rolling_0 = Node.Rolling (Some 0) in
  check_bootstrap_with_history_modes archive archive protocols ;
  check_bootstrap_with_history_modes archive full protocols ;
  check_bootstrap_with_history_modes archive rolling protocols ;
  check_bootstrap_with_history_modes full archive protocols ;
  check_bootstrap_with_history_modes full full protocols ;
  check_bootstrap_with_history_modes full rolling protocols ;
  check_bootstrap_with_history_modes rolling_0 Archive protocols ;
  check_bootstrap_with_history_modes rolling_0 rolling_0 protocols ;
  check_bootstrap_with_history_modes rolling_0 full protocols

let register_protocol_independent () = check_rpc_force_bootstrapped ()
