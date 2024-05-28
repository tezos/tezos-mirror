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
   Component:    Node config commands
   Invocation:   dune exec tezt/tests/main.exe -- --file config.ml
   Subject:      Check various usages of the node's config command
*)

let config_init node args =
  let* () = Node.config_init node args in
  Node.config_show node

let config_update node args =
  let* () = Node.config_update node args in
  Node.config_show node

let config_reset node args =
  let* () = Node.config_reset node args in
  Node.config_show node

(* Checks that the config have the expected keys *)
let check_config_keys config expected_keys =
  let keys =
    JSON.as_object config |> List.map fst |> List.sort String.compare
  in
  Check.((keys = List.sort String.compare expected_keys) ~__LOC__ (list string))
    ~error_msg:"Config should contain keys %R but contains keys %L."

(* Checks that the p2p config have the expected keys *)
let check_p2p_config config expected_p2p_keys =
  let p2p = JSON.(config |-> "p2p") in
  let p2p_keys =
    JSON.as_object p2p |> List.map fst |> List.sort String.compare
  in
  Check.(
    (p2p_keys = List.sort String.compare expected_p2p_keys)
      ~__LOC__
      (list string))
    ~error_msg:"P2P config should contain keys %R but contains keys %L."

let check_default_config config =
  check_config_keys config ["data-dir"; "network"; "p2p"] ;
  check_p2p_config
    config
    ["bootstrap-peers"; "expected-proof-of-work"; "listen-addr"] ;
  (* Checks p2p value *)
  let addr = JSON.(config |-> "p2p" |-> "listen-addr" |> as_string) in
  Check.((addr = "[::]:9732") ~__LOC__ string)
    ~error_msg:"P2P listening address should be %R but is %L."

let test_config_init () =
  let node = Node.create [] in
  let* config = config_init node [] in
  check_default_config config ;
  unit

let test_config_update () =
  let node = Node.create [] in
  let* () = Node.config_init node [] in
  let* updated_config = config_update node [Metrics_addr ":1234"] in
  (* Checks the consistency of the reset config *)
  check_config_keys
    updated_config
    ["data-dir"; "network"; "p2p"; "metrics_addr"] ;
  check_p2p_config
    updated_config
    ["bootstrap-peers"; "expected-proof-of-work"; "listen-addr"] ;
  (* Checks the updated value *)
  let metrics_addr =
    JSON.(updated_config |-> "metrics_addr" |=> 0 |> as_string)
  in
  return
  @@ Check.((metrics_addr = ":1234") string)
       ~error_msg:"config.metrics-addrs[0] contains %L but should contain %R."

let test_config_update_network () =
  let* node, client =
    Client.init_with_protocol ~protocol:Protocol.Alpha `Client ()
  in
  let* () = Client.bake_for_and_wait client in
  let* () = Node.terminate node in
  let* () =
    Node.spawn_config_update node [Network "mainnet"]
    |> Process.check_error ~exit_code:124
  in
  let* _ = Lwt_unix.system ("rm -r " ^ Node.data_dir node ^ "/context") in
  let* _ = Lwt_unix.system ("rm -r " ^ Node.data_dir node ^ "/store") in
  Node.config_update node [Network "mainnet"]

let test_config_reset () =
  let node = Node.create [] in
  let* config = config_reset node [Metrics_addr ":1234"] in
  check_config_keys config ["data-dir"; "network"; "p2p"; "metrics_addr"] ;
  check_p2p_config
    config
    ["bootstrap-peers"; "expected-proof-of-work"; "listen-addr"] ;
  let network = JSON.(config |-> "network" |> as_string) in
  return
  @@ Check.((network = "sandbox") string)
       ~error_msg:"config.network is %L but should be %R."

let test_config_reset_consistency () =
  let node = Node.create [] in
  let* initial_config = config_init node [] in
  let* reset_config = config_reset node [Metrics_addr ":1234"] in
  (* Checks the new value *)
  let metrics_addr =
    JSON.(reset_config |-> "metrics_addr" |=> 0 |> as_string)
  in
  Check.((metrics_addr = ":1234") string)
    ~error_msg:"config.metrics-addrs[0] contains %L but should contain %R." ;
  (* Reset again and and check the equality with initial config *)
  let* final_config = config_reset node [] in
  return
  @@ Check.((JSON.encode initial_config = JSON.encode final_config) string)
       ~error_msg:
         "Configs after reset should be identical. Was %L before, and now %R."

let test_config_reset_network () =
  let node = Node.create [Network "mainnet"] in
  let* () = Node.config_reset node [Network "sandbox"] in
  let* () = Node.identity_generate node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () =
    Client.activate_protocol_and_wait
      ~endpoint:(Node node)
      ~protocol:Protocol.Alpha
      ~node
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () = Node.terminate node in
  let* () =
    Node.spawn_config_reset node [Network "mainnet"] |> Process.check_error
  in
  let* _ = Lwt_unix.system ("rm -r " ^ Node.data_dir node ^ "/context") in
  let* _ = Lwt_unix.system ("rm -r " ^ Node.data_dir node ^ "/store") in
  Node.config_reset node [Network "mainnet"]

let register () =
  Test.register
    ~__FILE__
    ~title:"config init"
    ~tags:["config"; "init"]
    ~uses_client:false
    ~uses_admin_client:false
    test_config_init ;
  Test.register
    ~__FILE__
    ~title:"config update"
    ~tags:["config"; "update"]
    ~uses_client:false
    ~uses_admin_client:false
    test_config_update ;
  Test.register
    ~__FILE__
    ~title:"config update network"
    ~tags:["config"; "update"]
    test_config_update_network ;
  Test.register
    ~__FILE__
    ~title:"config reset"
    ~tags:["config"; "reset"]
    ~uses_client:false
    ~uses_admin_client:false
    test_config_reset ;
  Test.register
    ~__FILE__
    ~title:"config reset consistency"
    ~tags:["config"; "reset"]
    ~uses_client:false
    ~uses_admin_client:false
    test_config_reset_consistency ;
  Test.register
    ~__FILE__
    ~title:"config reset network"
    ~tags:["config"; "reset"; "network"]
    test_config_reset_network
