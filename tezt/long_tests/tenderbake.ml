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

let repeat = Cli.get_int ~default:5 "repeat"

let num_bootstrap_accounts = Array.length Account.Bootstrap.keys

let rpc_get_timestamp node block_level =
  let* header =
    RPC.call node
    @@ RPC.get_chain_block_header ~block:(string_of_int block_level) ()
  in
  let timestamp_s = JSON.(header |-> "timestamp" |> as_string) in
  return (timestamp_s |> Time.of_notation_exn |> Ptime.to_float_s)

let write_parameter_file protocol ~minimal_block_delay
    ~delay_increment_per_round =
  let base = Either.Right (protocol, None) in
  let original_parameters_file = Protocol.parameter_file protocol in
  let parameters = JSON.parse_file original_parameters_file in
  let consensus_threshold =
    let consensus_committee_size =
      JSON.(get "consensus_committee_size" parameters |> as_int)
    in
    (consensus_committee_size * 2 / 3) + 1
  in
  let overrides =
    [
      (["minimal_block_delay"], `String_of_int minimal_block_delay);
      (["delay_increment_per_round"], `String_of_int delay_increment_per_round);
      (["consensus_threshold"], `Int consensus_threshold);
    ]
  in
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
          (Invalid_argument "Inf.shuffle: array must contain at least two items")
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

  let add_node (st : state) =
    let node = Node.create [Synchronisation_threshold 0] in
    let handle = Array.length st.daemons in
    Log.info "Creating node #%d (%s)" handle (Node.name node) ;
    st.daemons <- Array.append st.daemons [|(node, None, None)|] ;
    return (handle, node)

  let add_node_with_baker (st : state) =
    let* handle, node = add_node st in
    let* client, baker = add_baker_to_node st handle in
    Log.info
      "Creating baker #%d (%s)"
      (Array.length st.daemons)
      (Baker.name baker) ;
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

let grafana_panels : Grafana.panel list = Rounds.grafana_panels

let register ~executors () = Rounds.register ~executors
