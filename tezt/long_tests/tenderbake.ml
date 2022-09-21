(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component: Baker / Consensus
   Invocation: dune exec tezt/long_tests/main.exe -- --file tenderbake.ml
   Subject: Checking performance for Tenderbake bakers
*)

module Time = Tezos_base.Time.System

let protocol = Protocol.Alpha

let len = Array.length Account.Bootstrap.keys

let nodes_num = len

let levels = 5

let repeat = 5

let time_to_reach_measurement = sf "time-to-reach-%d" levels

let test_proto_basic_title =
  sf "check that we reach level %d on all %d nodes" levels nodes_num

let grafana_panels : Grafana.panel list =
  [
    Row "Test: baker";
    Grafana.simple_graph
      ~title:(sf "The time it takes the cluster to reach level %d" levels)
      ~measurement:time_to_reach_measurement
      ~test:test_proto_basic_title
      ~field:"duration"
      ();
    Grafana.graphs_per_tags
      ~title:
        "The time it takes node 0 to reach level N: should look like an even \
         rainbow."
      ~yaxis_format:" s"
      ~measurement:"node-0-reaches-level"
      ~field:"duration"
      ~test:test_proto_basic_title
      ~tags:
        (List.map (fun level -> ("level", string_of_int level))
        @@ range 2 levels)
      ();
    Grafana.graphs_per_tags
      ~title:"The time it takes node 0 to reach level N."
      ~yaxis_format:" s"
      ~measurement:"node-0-reaches-level"
      ~field:"duration"
      ~test:test_proto_basic_title
      ~tags:
        (List.map (fun level -> ("level", string_of_int level))
        @@ range 2 levels)
      ();
    Grafana.graphs_per_tags
      ~title:
        "Round in which consensus was reached on level N (should be 0 all the \
         way through)."
      ~yaxis_format:" rounds"
      ~measurement:"node-0-reaches-level"
      ~field:"round"
      ~test:test_proto_basic_title
      ~tags:
        (List.map (fun level -> ("level", string_of_int level))
        @@ range 2 levels)
      ();
  ]

(* Run a simple ring topology with as many nodes/bakers/clients as
   bootstrap accounts, each with a single delegate. Measure and track
   regressions in the time it takes a node to reach level 5. *)
let test_proto_basic ~executors =
  let minimal_block_delay = 4 in
  let delay_increment_per_round = 1 in
  let timeout =
    (* All blocks must come from round 0 *)
    let max_time_per_round = minimal_block_delay + delay_increment_per_round in
    (levels - 1) * max_time_per_round
  in

  Long_test.register
    ~__FILE__
    ~title:test_proto_basic_title
    ~tags:["tenderbake"; "basic"]
    ~executors
    ~timeout:(Long_test.Seconds (repeat * 8 * timeout))
  @@ fun () ->
  Log.info
    "Setting up protocol parameters and %d nodes, clients & bakers"
    nodes_num ;

  if nodes_num < 1 then
    Test.fail "[nodes_num] must be strictly positive, was %d" nodes_num ;

  Long_test.measure_and_check_regression_lwt ~repeat time_to_reach_measurement
  @@ fun () ->
  (* One client to activate the protocol later on *)
  let activator_client = ref None in

  let daemons = ref [] in
  let* node_hd, nodes_tl =
    let create i =
      Log.debug "Creating node, client, baker triplet #%d" i ;
      let node = Node.create [Synchronisation_threshold 0] in
      let* client = Client.init ~endpoint:(Client.Node node) () in
      let delegates = [Account.Bootstrap.keys.(i mod len).alias] in
      let baker = Baker.create ~protocol node client ~delegates in
      let* () = Baker.run baker in
      activator_client := Some client ;
      daemons := (node, baker) :: !daemons ;
      Lwt.return node
    in
    Lwt.both (create 0) (Lwt_list.map_p create (range 1 (nodes_num - 1)))
  in
  let nodes = node_hd :: nodes_tl in

  (* Topology does not really matter here, as long as there is a path from any
     node to another one; let's use a ring. *)
  Log.info "Setting up nodes in ring topology" ;
  Cluster.ring nodes ;
  let* () = Cluster.start ~wait_connections:true nodes in

  Log.info "Generating test-specific parameter file" ;
  let* parameter_file =
    let base = Either.Right (protocol, None) in
    let original_parameters_file = Protocol.parameter_file protocol in
    let parameters = JSON.parse_file original_parameters_file in
    let consensus_threshold =
      let consensus_committee_size =
        JSON.(get "consensus_committee_size" parameters |> as_int)
      in
      (consensus_committee_size * 2 / 3) + 1
    in

    Protocol.write_parameter_file
      ~base
      [
        (["minimal_block_delay"], `String_of_int minimal_block_delay);
        (["delay_increment_per_round"], `String_of_int delay_increment_per_round);
        (["consensus_threshold"], `Int consensus_threshold);
      ]
  in

  let rpc_get_timestamp node block_level =
    let* header =
      RPC.call node
      @@ RPC.get_chain_block_header ~block:(string_of_int block_level) ()
    in
    let timestamp_s = JSON.(header |-> "timestamp" |> as_string) in
    return (timestamp_s |> Time.of_notation_exn |> Ptime.to_float_s)
  in

  Log.info "Activating protocol" ;
  let* () =
    Client.activate_protocol_and_wait
      ~protocol
      ~timestamp:Client.Now
      ~parameter_file
      (Option.get !activator_client)
  in
  let start = Unix.gettimeofday () in
  let* start_block_timestamp = rpc_get_timestamp node_hd 1 in
  (* Let's time the baker *)
  Log.info
    "Waiting for level %d to be reached (should happen in less than %d seconds)"
    levels
    timeout ;
  let* (_ : unit list) =
    Lwt.all
    @@ (Fun.flip List.concat_map) (range 2 levels)
    @@ fun level ->
    (Fun.flip List.mapi) nodes @@ fun i node ->
    let* (_ : int) = Node.wait_for_level node level in
    let event_reached = Unix.gettimeofday () -. start in
    if i = 0 then (
      let* block_delay =
        let* block_timestamp = rpc_get_timestamp node level in
        return (block_timestamp -. start_block_timestamp)
      in
      let* round =
        RPC.call node
        @@ RPC.get_chain_block_helper_round ~block:(string_of_int level) ()
      in
      Log.info
        "Node %s reached level %d in %f seconds (round: %d, block delay: %f)"
        (Node.name node)
        level
        event_reached
        round
        block_delay ;
      let data_point =
        InfluxDB.data_point
          ~other_fields:[("round", Float (float_of_int round))]
          ~tags:[("level", string_of_int level)]
          "node-0-reaches-level"
          ("duration", Float event_reached)
      in
      Long_test.add_data_point data_point ;
      unit)
    else (
      Log.debug
        "Node %s reached level %d in %f seconds"
        (Node.name node)
        level
        event_reached ;
      unit)
  in
  let time = Unix.gettimeofday () -. start in

  let* () =
    Lwt_list.iter_p
      (fun (node, baker) ->
        let* () = Baker.terminate baker and* () = Node.terminate node in
        unit)
      !daemons
  in

  Lwt.return time

let register () = test_proto_basic
