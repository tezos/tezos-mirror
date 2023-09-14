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

let lwt_ignore p =
  let* _ = p in
  unit

let protocol = Protocol.Alpha

let repeat = Cli.get_int ~default:5 "repeat"

let num_bootstrap_accounts = Array.length Account.Bootstrap.keys

let rpc_get_timestamp node block_level =
  let* header =
    Node.RPC.call node
    @@ RPC.get_chain_block_header ~block:(string_of_int block_level) ()
  in
  let timestamp_s = JSON.(header |-> "timestamp" |> as_string) in
  return (timestamp_s |> Time.of_notation_exn |> Ptime.to_float_s)

let write_parameter_file ?consensus_threshold protocol ~minimal_block_delay
    ~delay_increment_per_round =
  let base = Either.Right (protocol, None) in
  let original_parameters_file = Protocol.parameter_file protocol in
  let parameters = JSON.parse_file original_parameters_file in
  let consensus_threshold =
    let default =
      let consensus_committee_size =
        JSON.(get "consensus_committee_size" parameters |> as_int)
      in
      (consensus_committee_size * 2 / 3) + 1
    in
    Option.value ~default consensus_threshold
  in
  let overrides =
    [
      (["minimal_block_delay"], `String_of_int minimal_block_delay);
      (["delay_increment_per_round"], `String_of_int delay_increment_per_round);
      (["consensus_threshold"], `Int consensus_threshold);
    ]
  in
  Log.info
    "Writing parameters: [%s]"
    (String.concat ", "
    @@ List.map
         (fun (key, value) ->
           sf
             "%s=%d"
             (String.concat "." key)
             (match value with `String_of_int d -> d | `Int d -> d))
         overrides) ;
  Protocol.write_parameter_file ~base overrides

let nodes_measure_levels nodes levels =
  let start = Unix.gettimeofday () in
  let* start_block_timestamp = rpc_get_timestamp (List.hd nodes) 1 in
  let* (_ : unit list) =
    Lwt.all
    @@ (Fun.flip List.concat_map) levels
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
        Node.RPC.call node
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
  return (Unix.gettimeofday () -. start)

(* Infinite lazy sequences *)
module Inf = struct
  (* ['a t] is an lazy, infinite stream of values of type ['a] *)
  type 'a t = Inf of (unit -> 'a * 'a t)

  let inf f = Inf f

  (* [shuffle xs] is a lazy, infinite stream of non-repeated random
     values from [xs].

     Successive values are from distinct indices of [xs]. [xs] must contain
     at least two items. *)
  let shuffle : 'a list -> 'a t = function
    | [] | [_] ->
        raise
          (Invalid_argument "Inf.shuffle: list must contain at least two items")
    | xs ->
        let arr = Array.of_list xs in
        let len = Array.length arr in
        let rec next prev =
          inf @@ fun () ->
          let nxt = (prev + 1 + Random.int (len - 1)) mod len in
          (arr.(nxt), next nxt)
        in
        inf @@ fun () ->
        let fst = Random.int len in
        (arr.(fst), next fst)

  (* [cycle ls] is a lazy, infinite analogue of {!Seq.cycle}.

     [ls] cannot be empty. *)
  let cycle : 'a list -> 'a t = function
    | [] -> raise (Invalid_argument "Inf.cycle: list cannot be empty")
    | y :: ys ->
        let rec aux = function
          | [] -> inf @@ fun () -> (y, aux ys)
          | x :: xs -> inf @@ fun () -> (x, aux xs)
        in
        aux (y :: ys)

  (* [next i] is the next element in the infinite sequence [i] *)
  let next : 'a t -> 'a * 'a t = function Inf f -> f ()
end

module Sandbox = struct
  type state = {
    mutable daemons : (Node.t * Baker.t option * Client.t option) array;
    mutable delegates : Account.key Inf.t;
  }

  type handle = int

  let make delegates = {daemons = [||]; delegates}

  let get_delegate (st : state) =
    let delegate, delegates' = Inf.next st.delegates in
    st.delegates <- delegates' ;
    delegate.alias

  let nodes (st : state) =
    Array.(map (fun (node, _, _) -> node) st.daemons |> to_list)

  let clients (st : state) =
    Array.to_list st.daemons |> List.filter_map (fun (_, _, client) -> client)

  let bakers (st : state) =
    Array.to_list st.daemons |> List.filter_map (fun (_, baker, _) -> baker)

  let handles (st : state) = Base.range 0 (Array.length st.daemons - 1)

  let add_baker_to_node (st : state) (h : handle) =
    match Array.get st.daemons h with
    | exception Invalid_argument _ ->
        raise (Invalid_argument (sf "add_baker_to_node: invalid handle %d" h))
    | _, Some _, _ ->
        raise
          (Invalid_argument
             (sf "add_baker_to_node: node %d already has a baker" h))
    | node, _, _ ->
        let* client = Client.init ~endpoint:(Client.Node node) () in
        let delegates = [get_delegate st] in
        let baker = Baker.create ~protocol node client ~delegates in
        let* () = Baker.run baker in
        st.daemons.(h) <- (node, Some baker, Some client) ;
        return (client, baker)

  let remove_baker_from_node (st : state) (h : handle) =
    match Array.get st.daemons h with
    | exception Invalid_argument _ ->
        raise
          (Invalid_argument (sf "remove_baker_from_node: invalid handle %d" h))
    | _, None, _ ->
        raise
          (Invalid_argument
             (sf "remove_baker_from_node: node %d has no baker" h))
    | node, Some baker, client_opt ->
        let* () = Baker.terminate baker in
        st.daemons.(h) <- (node, None, client_opt) ;
        return baker

  let add_node (st : state) =
    let node = Node.create [Synchronisation_threshold 0] in
    let handle = Array.length st.daemons in
    Log.info "Creating node #%d (%s)" handle (Node.name node) ;
    st.daemons <- Array.append st.daemons [|(node, None, None)|] ;
    return (handle, node)

  let add_node_with_baker (st : state) =
    let* handle, node = add_node st in
    let* client, baker = add_baker_to_node st handle in
    Log.info "Creating baker #%d (%s)" handle (Baker.name baker) ;
    return (handle, node, client, baker)

  let terminate (st : state) =
    Lwt_list.iter_p
      (fun (node, baker_opt, _) ->
        let* () = Node.terminate node
        and* () = Option.fold ~none:unit ~some:Baker.terminate baker_opt in
        unit)
      (Array.to_list st.daemons)
end

module Rounds = struct
  let nodes_num = num_bootstrap_accounts

  let levels = 5

  let time_to_reach_measurement = sf "time-to-reach-%d" levels

  let test = sf "check that we reach level %d on all %d nodes" levels nodes_num

  let grafana_panels : Grafana.panel list =
    let open Grafana in
    [
      Row ("Test: " ^ test);
      simple_graph
        ~title:(sf "The time it takes the cluster to reach level %d" levels)
        ~measurement:time_to_reach_measurement
        ~test
        ~field:"duration"
        ();
      graphs_per_tags
        ~title:"The time it takes node 0 to reach level N."
        ~yaxis_format:" s"
        ~measurement:"node-0-reaches-level"
        ~field:"duration"
        ~test
        ~tags:
          (List.map (fun level -> ("level", string_of_int level))
          @@ range 2 levels)
        ();
      graphs_per_tags
        ~title:
          "Round in which consensus was reached on level N (should be 0 all \
           the way through)."
        ~yaxis_format:" rounds"
        ~measurement:"node-0-reaches-level"
        ~field:"round"
        ~test
        ~tags:
          (List.map (fun level -> ("level", string_of_int level))
          @@ range 2 levels)
        ();
    ]

  (* Check that in a simple ring topology with as many nodes/bakers/clients as
     bootstrap accounts, each with a single delegate, all blocks occurs within
     round 0. *)
  let register ~executors =
    let minimal_block_delay = 4 in
    let delay_increment_per_round = 1 in
    let timeout =
      (* All blocks must come from round 0 *)
      let max_time_per_round =
        minimal_block_delay + delay_increment_per_round
      in
      (levels - 1) * max_time_per_round
    in

    Long_test.register
      ~__FILE__
      ~title:test
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
    let sandbox =
      Sandbox.make (Inf.cycle (Array.to_list Account.Bootstrap.keys))
    in
    let* _, _, activator_client, _ = Sandbox.add_node_with_baker sandbox in
    let* () =
      Base.repeat (nodes_num - 1) @@ fun () ->
      let* _ = Sandbox.add_node_with_baker sandbox in
      unit
    in
    let nodes = Sandbox.nodes sandbox in

    (* Topology does not really matter here, as long as there is a path from any
       node to another one; let's use a ring. *)
    Log.info "Setting up nodes in ring topology" ;
    Cluster.ring nodes ;
    let* () = Cluster.start ~wait_connections:true nodes in

    let* parameter_file =
      write_parameter_file
        protocol
        ~minimal_block_delay
        ~delay_increment_per_round
    in

    Log.info "Activating protocol" ;
    let* () =
      Client.activate_protocol_and_wait
        ~protocol
        ~timestamp:Client.Now
        ~parameter_file
        activator_client
    in
    (* Let's time the baker *)
    Log.info
      "Waiting for level %d to be reached (should happen in less than %d \
       seconds)"
      levels
      timeout ;
    let* time = nodes_measure_levels nodes (range 2 levels) in

    let* () = Sandbox.terminate sandbox in

    Lwt.return time
end

module Long_dynamic_bake = struct
  type topology = Clique | Ring

  let cluster_of_topology = function
    | Clique -> Cluster.clique
    | Ring -> Cluster.ring

  let string_of_topology = function Clique -> "clique" | Ring -> "ring"

  let test topology = "long dynamic bake: " ^ string_of_topology topology

  (* Test runs for [test_levels] *)
  let test_levels = Cli.get_int "test_levels" ~default:120

  (* add an operation every [add_operation_period] seconds *)
  let add_operation_period = Cli.get_float "add_operation_period" ~default:2.0

  (* cycle bakers every [kill_baker_period] seconds *)
  let kill_baker_period = Cli.get_float "kill_baker_period" ~default:8.0

  (* test runs until [max_level] is reached (which one for genesis block + [test_levels]) *)
  let max_level = 1 + test_levels

  let time_to_reach_max_level_measurement =
    sf "dynamic-bake-time-to-reach-%d" max_level

  (* that is, decision expected in at most 3 rounds. only used for display *)
  let expected_max_rounds = 3

  let minimal_block_delay = 1

  let delay_increment_per_round = 1

  (* c.f. https://tezos.gitlab.io/active/consensus.html *)
  let round_duration r = minimal_block_delay + (r * delay_increment_per_round)

  let grafana_panels topology =
    let test = test topology in
    let open Grafana in
    [
      Row ("Test: " ^ test);
      simple_graph
        ~title:(sf "The time it takes the cluster to reach level %d" max_level)
        ~yaxis_format:" s"
        ~measurement:time_to_reach_max_level_measurement
        ~field:"duration"
        ~test
        ();
      graphs_per_tags
        ~title:"The time it takes node 0 to reach level N."
        ~yaxis_format:" s"
        ~measurement:"node-0-reaches-level"
        ~field:"duration"
        ~test
        ~tags:
          (List.map (fun level -> ("level", string_of_int level))
          @@ range 2 max_level)
        ();
      graphs_per_tags
        ~title:
          (sf
             "Round in which consensus was reached on level N (should be less \
              than %d all the way through)."
             expected_max_rounds)
        ~yaxis_format:" rounds"
        ~measurement:"node-0-reaches-level"
        ~field:"round"
        ~test
        ~tags:
          (List.map (fun level -> ("level", string_of_int level))
          @@ range 2 max_level)
        ();
    ]

  (* This test runs [num_nodes] nodes for [test_levels]. All but one
     node has a baker attached. Every [kill_baker_period] seconds, a
     baker is added to the node lacking one and one of the bakers is
     killed.

     In addition, every [add_operation_period] seconds, a random
     transaction is injected.

     We measure and check for regressions in the time it takes the
     cluster to reach the final level [max_level]. *)
  let register topology ~executors =
    let num_nodes = 5 in
    let timeout = max_level * round_duration expected_max_rounds in

    Long_test.register
      ~__FILE__
      ~title:(test topology)
      ~tags:["tenderbake"; "dynamic"; string_of_topology topology]
      ~executors
      ~timeout:(Long_test.Seconds (repeat * 8 * timeout))
    @@ fun () ->
    let* parameter_file =
      write_parameter_file
        protocol
        ~minimal_block_delay
        ~delay_increment_per_round
    in

    Long_test.measure_and_check_regression_lwt
      ~repeat
      time_to_reach_max_level_measurement
    @@ fun () ->
    Log.info "Setup daemons" ;

    (* Create [num_nodes - 1] nodes with bakers *)
    let sandbox =
      Sandbox.make (Inf.cycle (Array.to_list Account.Bootstrap.keys))
    in
    (* One client to activate the protocol later on *)
    let* _, node_hd, activator_client, _ =
      Sandbox.add_node_with_baker sandbox
    in
    let* () =
      Base.repeat (num_nodes - 2) @@ fun () ->
      lwt_ignore @@ Sandbox.add_node_with_baker sandbox
    in
    (* Add a node without a baker *)
    let* dead_baker_handle, _ = Sandbox.add_node sandbox in
    let nodes = Sandbox.nodes sandbox in

    Log.info
      "Setting up node %s topology: %s"
      (string_of_topology topology)
      (String.concat ", " @@ List.map Node.name nodes) ;
    (cluster_of_topology topology) nodes ;

    Log.info "Starting nodes" ;
    let* () = Cluster.start ~wait_connections:true nodes in

    Log.info "Activating protocol" ;
    let* () =
      Client.activate_protocol_and_wait
        ~protocol
        ~timestamp:Client.Now
        ~parameter_file
        activator_client
    in

    (* Kill a baker and spawn a new every [kill_baker_period] *)
    let rec loop_kill_bakers cycle (dead_baker_handle, kill_queue) =
      let* () = Lwt_unix.sleep kill_baker_period in
      let* head_level = Node.get_level node_hd in
      if head_level < max_level then (
        (* cyclically kill bakers *)
        let dead_baker_handle', kill_queue = Inf.next kill_queue in
        let* killed_baker =
          Sandbox.remove_baker_from_node sandbox dead_baker_handle'
        in
        let* head_level = Node.get_level node_hd in
        Log.info
          "Cycle %d (head level %d): killed baker %s from handle #%d"
          cycle
          head_level
          (Baker.name killed_baker)
          dead_baker_handle' ;

        let revive_delay = 1.0 in
        let* () = Lwt_unix.sleep revive_delay in

        (* wake up the dead baker *)
        let* _, new_baker =
          Sandbox.add_baker_to_node sandbox dead_baker_handle
        in
        let* head_level = Node.get_level node_hd in
        Log.info
          "Cycle %d (head level %d): added baker %s to handle #%d"
          cycle
          head_level
          (Baker.name new_baker)
          dead_baker_handle ;

        (* set the next baker to die *)
        loop_kill_bakers (cycle + 1) (dead_baker_handle', kill_queue))
      else unit
    in

    (* Inject a transaction every [add_operation_period] *)
    let rec loop_generate_operations keys clients =
      let* () = Lwt_unix.sleep add_operation_period in
      let* head_level = Node.get_level node_hd in
      if head_level < max_level then
        let client, clients = Inf.next clients in
        let* keys =
          let giver_index, keys = Inf.next keys in
          let Account.{alias = giver; _} =
            Account.Bootstrap.keys.(giver_index)
          in
          let receiver_index =
            let bootstrap_keys = Array.length Account.Bootstrap.keys in
            (giver_index + 1 + Random.int (bootstrap_keys - 1))
            mod bootstrap_keys
          in
          let Account.{alias = receiver; _} =
            Account.Bootstrap.keys.(receiver_index)
          in
          let amount = Tez.of_int (1 + Random.int 10) in
          let* () = Client.transfer ~wait:"1" ~amount ~giver ~receiver client in
          let* head_level = Node.get_level node_hd in
          Log.info
            "Level %d: sent %s from %s to %s with client %s"
            head_level
            (Tez.to_string amount)
            giver
            receiver
            (Client.name client) ;
          return keys
        in
        loop_generate_operations keys clients
      else unit
    in

    (* Wait and measure time for [max_level] to be reached *)
    let* time = nodes_measure_levels nodes (range 2 max_level)
    and* () =
      let kill_queue = Inf.cycle (Sandbox.handles sandbox) in
      loop_kill_bakers 0 (dead_baker_handle, kill_queue)
    and* () =
      let clients = Inf.shuffle (Sandbox.clients sandbox) in
      let keys =
        Inf.shuffle (range 0 (Array.length Account.Bootstrap.keys - 1))
      in
      loop_generate_operations keys clients
    in
    Log.info "Test terminated in %f seconds, expected max time: %d" time timeout ;

    (* Clean up for repeat *)
    let* () = Sandbox.terminate sandbox in

    Lwt.return time
end

let grafana_panels : Grafana.panel list =
  Rounds.grafana_panels
  @ Long_dynamic_bake.grafana_panels Clique
  @ Long_dynamic_bake.grafana_panels Ring

let register ~executors () =
  Rounds.register ~executors ;
  Long_dynamic_bake.register Clique ~executors ;
  Long_dynamic_bake.register Ring ~executors
