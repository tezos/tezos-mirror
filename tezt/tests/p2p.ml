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
   Component:    P2P
   Invocation:   dune exec tezt/tests/main.exe -- --file p2p.ml
   Subject:      Integration tests of p2p layer.
*)

module ACL = struct
  (* Test.

     Check IP address greylisting mechanism with unauthenticated connection.

     1. Start a node,
     2. Write noise on the welcome worker of this node,
     3. Check the IP greylist with a RPC,
     4. Try to connect to a greylisted node. *)
  let check_ip_greylisting () =
    let pp_list ~elt_pp l =
      let rec pp_rec ~elt_pp ppf = function
        | [] -> ()
        | [elt] -> Format.fprintf ppf "%a" elt_pp elt
        | head :: tail ->
            Format.fprintf ppf "%a, " elt_pp head ;
            pp_rec ~elt_pp ppf tail
      in
      Format.asprintf "[%a]" (pp_rec ~elt_pp) l
    in
    Test.register
      ~__FILE__
      ~title:"check ip greylisting"
      ~tags:["p2p"; "acl"; "greylist"]
    @@ fun () ->
    let localhost_ips =
      [
        (* 127.0.0.1 *)
        Unix.inet_addr_loopback;
        (* ::1 *)
        Unix.inet6_addr_loopback;
        Unix.inet_addr_of_string "::ffff:127.0.0.1";
        Unix.inet_addr_of_string "::ffff:7f00:0001";
      ]
    in
    let* target = Node.init [] in
    let* node = Node.init [] in
    let* client = Client.init ~endpoint:(Node target) () in
    let* () =
      Node.send_raw_data
        target
        ~data:"\000\010Hello, world. This is garbage, greylist me !"
    in
    let* json = RPC.get_greylist_ips client in
    let greylisted_ips = JSON.(as_list (json |-> "ips")) in
    let nb_greylisted_ips = List.length greylisted_ips in
    if nb_greylisted_ips <> 1 then
      Test.fail
        "The number of greylisted IPs is incorrect (actual: %d, expected: 1)."
        nb_greylisted_ips ;
    let greylisted_ip =
      Unix.inet_addr_of_string (JSON.as_string (List.hd greylisted_ips))
    in
    if List.for_all (( <> ) greylisted_ip) localhost_ips then
      Test.fail
        "The greylisted IP is incorrect (actual: %s, expected: one of %s)."
        (Unix.string_of_inet_addr greylisted_ip)
        (pp_list
           ~elt_pp:(fun ppf ip ->
             Format.fprintf ppf "%s" (Unix.string_of_inet_addr ip))
           localhost_ips) ;
    let process = Client.Admin.spawn_connect_address ~peer:node client in
    let error_rex =
      rex "Error:(\n|.)*The address you tried to connect \\(.*\\) is banned."
    in
    Process.check_error ~msg:error_rex process

  let tests () = check_ip_greylisting ()
end

(* [wait_for_accepted_peer_ids] waits until the node connects to a peer for
   which an expected [peer_id] was set. *)
let wait_for_accepted_peer_ids node =
  let filter _ = Some () in
  Node.wait_for node "authenticate_status_peer_id_correct.v0" filter

(* Test.

   We start two nodes. We connect one node with the other using the
   `--peer` option and by setting an expected peer_id. To check that the nodes
   are connected, we activate the protocol and check that the block 1 has been
   propagated. *)
let check_peer_option =
  Protocol.register_test
    ~__FILE__
    ~title:"check peer option"
    ~tags:["p2p"; "cli"; "peer"]
  @@ fun protocol ->
  let* node_1 = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node_1) () in
  let* () = Client.activate_protocol ~protocol client in
  let node_2 = Node.create [] in
  let wait = wait_for_accepted_peer_ids node_2 in
  let* () = Node.identity_generate node_2 in
  let* () = Node.config_init node_2 [] in
  let* () = Node.add_peer_with_id node_2 node_1 in
  let* () = Node.run node_2 [] in
  let* () = wait in
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  unit

(* Test.

   We create one node with the `--connections` option set to 1 and another one
   with no specification. Then, we use the `--peer` option to let the p2p
   maintenance of the first node establishes a connection with the other node.
   To check the nodes are connected, we activate the protocol and check that
   the block 1 has been propagated. *)

let test_one_connection =
  let nb_connection = 1 in
  Protocol.register_test
    ~__FILE__
    ~title:"check --connection=1 option"
    ~tags:["p2p"; "cli"; "connections"]
  @@ fun protocol ->
  let* node_1 = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node_1) () in
  let* () = Client.activate_protocol ~protocol client in
  let node_2 = Node.create [Connections nb_connection] in
  let wait = wait_for_accepted_peer_ids node_2 in
  let* () = Node.identity_generate node_2 in
  let* () = Node.config_init node_2 [] in
  let* () = Node.add_peer_with_id node_2 node_1 in
  let* () = Node.run node_2 [] in
  let* () = wait in
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  unit

(* [wait_pred] waits until [pred arg] is true. An active wait with Lwt
   cooperation points is used. *)
let rec wait_pred ~pred ~arg =
  let* () = Lwt.pause () in
  let* cond = pred arg in
  if not cond then wait_pred ~pred ~arg else Lwt.return_unit

(* [get_connections_points ~client] returns the list of active connections
   point of the node linked to [client]. *)
let get_connections_points ~client =
  let* connections = RPC.get_connections client in
  let connections = JSON.as_list connections in
  return
  @@
  let open JSON in
  List.map
    (fun conn_info ->
      ( as_string (conn_info |-> "id_point" |-> "addr"),
        as_int (conn_info |-> "id_point" |-> "port") ))
    connections

(* [get_nb_connections ~client] returns the number of active connections of the
   node  to [client]. *)
let get_nb_connections ~client =
  let* ports = get_connections_points ~client in
  return @@ List.length ports

(* [wait_connections ~client n] waits until the node related to [client] has at
   least [n] active connections. *)
let wait_connections ~client nb_conns_target =
  wait_pred
    ~pred:(fun () ->
      let* nb_conns = get_nb_connections ~client in
      return @@ (nb_conns >= nb_conns_target))
    ~arg:()

module Maintenance = struct
  (* Test.
     Initialize a node and verify that the number of active connections
     established by the maintenance is correct. *)
  let test_expected_connections () =
    (* The value of [expected_connections] is fixed to 6 in this test for two
       reasons. Firstly, the consumption of each node is substantial and there
       will be [expected_connection*2/3+1] nodes launched. This explains why the
       number of [expected_connections] is quite small. Secondly, since the
       values of the maintenance configuration are integers, there will be an
       approximation and then for a small value, it is required to have
       [expected_connections mod 6 = 0]. *)
    let expected_connections = 6 in
    Test.register
      ~__FILE__
      ~title:"p2p-maintenance-init-expected_connections"
      ~tags:["p2p"; "node"; "maintenance"]
    @@ fun () ->
    (* Connections values evaluated from --connections option. *)
    let min_connections = expected_connections / 2 in
    let max_connections = 3 * expected_connections / 2 in
    (* Connections values evaluated from P2p_maintenance.config. *)
    let step_min = (expected_connections - min_connections) / 3
    and step_max = (max_connections - expected_connections) / 3 in
    let min_threshold = min_connections + step_min in
    (* The target variables are used to define the goal interval of active
       connections reached by the maintenance. *)
    let min_target = min_connections + (2 * step_min) in
    let max_target = max_connections - (2 * step_max) in
    let max_threshold = max_connections - step_max in
    Log.info
      "Configuration values (min: %d, min_threshold: %d, min_target: %d, \
       expected: %d, max_target: %d, max_threshold: %d, max: %d)."
      min_connections
      min_threshold
      min_target
      expected_connections
      max_target
      max_threshold
      max_connections ;
    let* target_node = Node.init [Connections expected_connections] in
    let* target_client = Client.init ~endpoint:(Node target_node) () in
    Log.info "Target created." ;
    let nodes =
      Cluster.create max_connections [Connections (max_connections - 1)]
    in
    Cluster.clique nodes ;
    let* () = Cluster.start ~public:true nodes in
    Log.info "Complete network of nodes created." ;
    let maintenance_ended_promise =
      Node.wait_for target_node "maintenance_ended.v0" (fun _ -> Some ())
    in
    let* () =
      Client.Admin.connect_address target_client ~peer:(List.hd nodes)
    in
    Log.info "Target is connected to the network." ;
    let* () = wait_connections ~client:target_client min_target in
    Log.info "Enough connections has been established." ;
    let* () = maintenance_ended_promise in
    Log.info "The maintenance ended." ;
    let* nb_active_connections = get_nb_connections ~client:target_client in
    if nb_active_connections > max_target then
      Test.fail
        "There are too many active connections (actual: %d, expected less than \
         %d)"
        nb_active_connections
        max_target ;
    Lwt.return_unit

  let tests () = test_expected_connections ()
end

let port_from_peers_file file_name =
  JSON.parse_file file_name |> JSON.geti 0
  |> JSON.get "last_established_connection"
  |> JSON.geti 0 |> JSON.get "port" |> JSON.as_int

(*
  - node_2 connects to the peer node_1 and advertises different listening port
  - after node_1 receives advertised net port from node_2 (maintenance_ended),
    terminate node_1 to force it saving peer metadata;
  - check that saved node_1 peer metadata contains net port advertised by node_2,
    not its actual listening net port
*)
let test_advertised_port () =
  Test.register
    ~__FILE__
    ~title:"check --advertised-net-port=PORT option"
    ~tags:["p2p"; "cli"; "connections"]
  @@ fun () ->
  let* node_1 = Node.init [Connections 1] in
  let maintenance_p =
    Node.wait_for node_1 "maintenance_ended.v0" (fun _ -> Some ())
  in

  let advertised_net_port = Node.fresh_port () in
  let node_2 = Node.create ~advertised_net_port [] in
  let* () = Node.identity_generate node_2 in
  let* () = Node.config_init node_2 [] in
  let () = Node.add_peer node_2 node_1 in

  let* () = Node.run node_2 [] in
  let* () = maintenance_p in

  let wait_for_save_p =
    Node.wait_for node_1 "save_metadata.v0" (fun json ->
        Some (JSON.as_string json))
  in

  let* () = Node.terminate node_1 in

  let* path = wait_for_save_p in
  let advertised_port_from_peers_file = port_from_peers_file path in
  if advertised_port_from_peers_file <> advertised_net_port then
    Test.fail
      "advertised-net-port: Unexcpected port number received (got %d, expeted \
       %d)"
      advertised_port_from_peers_file
      advertised_net_port
  else () ;

  unit

let register_protocol_independent () =
  Maintenance.tests () ;
  ACL.tests () ;
  test_advertised_port ()

let register ~(protocols : Protocol.t list) =
  check_peer_option ~protocols ;
  test_one_connection ~protocols
