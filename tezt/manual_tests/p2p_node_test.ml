(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    P2P node (octez-p2p-node)
   Invocation:   dune build src/bin_p2p_node
                 dune exec tezt/manual_tests/main.exe -- --file p2p_node_test.ml
   Subject:      Ensure that the octez P2P node behaves as expected

   Tests
   -----
   - P2P node starts and stops
   - P2P nodes connect with each other
   - P2P nodes exchange messages
   - P2P node can greylist other P2P node
   - Manual test (for avoiding regressions in the CLI)
*)

let team = Tag.layer1

let wait_for_connection_established p2p_node target =
  let open P2p_node in
  let address = net_addr target ^ ":" ^ string_of_int @@ net_port target in
  let where = sf "address = %s" address in
  wait_for p2p_node "connection_established.v0" ~where (fun _json -> Some ())

let wait_for_text_message_received p2p_node =
  P2p_node.wait_for p2p_node "text_message_received.v0" (fun _json -> Some ())

let test_start_and_stop () =
  Test.register
    ~__FILE__
    ~title:"P2P node starts and stops"
    ~tags:[team; "p2p_node"; "init"]
    ~uses:[Constant.octez_p2p_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* p2p_node = P2p_node.init () in
  let* () = P2p_node.wait_for_ready p2p_node in
  let* () = P2p_node.terminate p2p_node in
  unit

let test_connect () =
  Test.register
    ~__FILE__
    ~title:"P2P nodes connect correctly"
    ~tags:[team; "p2p_node"; "connect"]
    ~uses:[Constant.octez_p2p_node]
    ~uses_node:false
  @@ fun () ->
  let* p2p_node_1 = P2p_node.init () in
  let* p2p_node_2 = P2p_node.init () in

  let* client_1 =
    Client.init
      ~endpoint:(Foreign_endpoint (P2p_node.as_rpc_endpoint p2p_node_1))
      ()
  in

  let wait_for_connection_established_1 =
    wait_for_connection_established p2p_node_1 p2p_node_2
  in
  let wait_for_connection_established_2 =
    wait_for_connection_established p2p_node_2 p2p_node_1
  in

  Log.info "Connect the two P2P nodes." ;
  let* () = Client.Admin.connect_p2p_node_address ~peer:p2p_node_2 client_1 in

  let* () = wait_for_connection_established_1 in
  let* () = wait_for_connection_established_2 in

  let* () = P2p_node.terminate p2p_node_1 in
  let* () = P2p_node.terminate p2p_node_2 in

  unit

let test_message_exchange () =
  Test.register
    ~__FILE__
    ~title:"P2P nodes exchange messages correctly"
    ~tags:[team; "p2p_node"; "message"; "exchange"]
    ~uses:[Constant.octez_p2p_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let net_port_1 = Port.fresh () in
  let net_port_2 = Port.fresh () in

  let net_addr_1 = Constant.default_host ^ ":" ^ string_of_int net_port_1 in
  let net_addr_2 = Constant.default_host ^ ":" ^ string_of_int net_port_2 in

  let ping_interval = 5.0 in

  let* p2p_node_1 =
    P2p_node.init ~net_port:net_port_1 ~peers:[net_addr_2] ~ping_interval ()
  in
  let* p2p_node_2 =
    P2p_node.init ~net_port:net_port_2 ~peers:[net_addr_1] ~ping_interval ()
  in

  let wait_for_text_message_received_1 =
    wait_for_text_message_received p2p_node_1
  in
  let wait_for_text_message_received_2 =
    wait_for_text_message_received p2p_node_2
  in

  Log.info "Wait for %f seconds to have exchanged messages." ping_interval ;
  let* () = Lwt_unix.sleep ping_interval in

  let* () = wait_for_text_message_received_1 in
  let* () = wait_for_text_message_received_2 in

  let* () = P2p_node.terminate p2p_node_1 in
  let* () = P2p_node.terminate p2p_node_2 in

  unit

(* Test adapted from [p2p.ml]. *)
let test_greylist () =
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
    ~title:"P2P node greylist"
    ~tags:[team; "p2p_node"; "rpc"]
    ~uses:[Constant.octez_p2p_node]
    ~uses_node:false
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
  let* target = P2p_node.init () in
  let* p2p_node = P2p_node.init () in

  let* client =
    Client.init
      ~endpoint:(Foreign_endpoint (P2p_node.as_rpc_endpoint target))
      ()
  in
  let* () =
    P2p_node.send_raw_data
      target
      ~data:"\000\010Hello, world. This is garbage, greylist me !"
  in
  let* json = Client.RPC.call client RPC.get_network_greylist_ips in
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
  let process =
    Client.Admin.spawn_connect_p2p_node_address ~peer:p2p_node client
  in
  let error_rex =
    rex "Error:(\n|.)*The address you tried to connect \\(.*\\) is banned."
  in
  Process.check_error ~msg:error_rex process

let test_man () =
  Regression.register
    ~__FILE__
    ~title:"P2P node man"
    ~tags:[team; "p2p_node"; "man"]
    ~uses:[Constant.octez_p2p_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  let* () = Process.run ~hooks (Uses.path Constant.octez_p2p_node) [] in
  let* () = Process.run ~hooks (Uses.path Constant.octez_p2p_node) ["--help"] in
  let* () = Process.run ~hooks (Uses.path Constant.octez_p2p_node) ["man"] in
  unit

let register () =
  test_start_and_stop () ;
  test_connect () ;
  test_message_exchange () ;
  test_greylist () ;
  test_man ()
