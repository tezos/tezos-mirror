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
   Component: Remote connection tests
   Invocation: dune exec tezt/remote_tests/main.exe runner connection
   Dependencies: A runner_config.json where you describe the configuration.
   Subject: The goal is to check if two nodes can communicate through
            ssh tests.
*)

(* This module provides an example of a common test you can do using the remote
   runner interface. *)

let runner = Runner_config.runner

let path = Runner_config.node_path

(* Wait for the remote node to accept a peer. *)
let wait_for_accepted_peer_ids node =
  let filter _ = Some () in
  Node.wait_for node "authenticate_status_peer_id_correct.v0" filter

(* Test.

   Create two nodes and connect them together. *)
let run_node =
  Protocol.register_test
    ~__FILE__
    ~title:"check remote node connection"
    ~tags:["runner"; "remote"; "connection"]
  @@ fun protocol ->
  Log.debug "Init node config" ;
  let* node_1 = Node.init ~runner ~path [Synchronisation_threshold 0] in
  let endpoint_1 = Client.(Node node_1) in
  let* client = Client.init ~endpoint:endpoint_1 () in
  let* () = Client.activate_protocol ~protocol client in
  let node_2 = Node.create [Connections 1] in
  let wait = wait_for_accepted_peer_ids node_2 in
  let* () = Node.identity_generate node_2 in
  let* () = Node.config_init node_2 [] in
  let* () = Node.add_peer_with_id node_2 node_1 in
  let* () = Node.run node_2 [] in
  let* () = wait in
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  unit

let register ~protocols = run_node protocols
