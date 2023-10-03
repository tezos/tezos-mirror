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
   Invocation: dune exec tezt/remote_tests/main.exe runner accuser
   Dependencies: A runner_config.json where you describe the configuration.
   Subject: Check if the accuser works through ssh remote tests. *)

(** Test.

    This test is a copy of the [tests/double_baking.ml] test but on
    a remote machine. *)

let runner = Runner_config.runner

let path = Runner_config.node_path

let is_operation_in_operations ops oph =
  let open JSON in
  let ops_list = ops |=> 2 |> as_list in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) ops_list

let is_operation_in_applied_mempool mempool oph =
  let open JSON in
  let applied_list = as_list (mempool |-> "applied") in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list

let wait_for_denunciation accuser =
  let filter json = JSON.(json |-> "hash" |> as_string_opt) in
  Accuser.wait_for accuser "double_baking_denounced.v0" filter

let wait_for_denunciation_injection node client oph_promise =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_notice.v0" filter in
  let* oph = oph_promise in
  let* mempool =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  if is_operation_in_applied_mempool mempool oph then some oph else none

let double_bake =
  Protocol.register_test
    ~__FILE__
    ~title:"double baking with accuser"
    ~tags:["remote"; "runner"; "bake"; "accuser"]
  @@ fun protocol ->
  let* node_1 =
    Node.init ~runner ~path [Synchronisation_threshold 0; Private_mode]
  and* node_2 =
    Node.init ~runner ~path [Synchronisation_threshold 0; Private_mode]
  in
  let endpoint_1 = Client.(Node node_1) and endpoint_2 = Client.(Node node_2) in
  let* client_1 = Client.init ~endpoint:endpoint_1 ()
  and* client_2 = Client.init ~endpoint:endpoint_2 () in
  let* () =
    Client.Admin.trust_address client_1 ~endpoint:endpoint_1 ~peer:node_2
  and* () =
    Client.Admin.trust_address client_2 ~endpoint:endpoint_2 ~peer:node_1
  in
  let* () =
    Client.Admin.connect_address client_1 ~endpoint:endpoint_1 ~peer:node_2
  in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let common_ancestor = 0 in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* _ = Node.wait_for_level node_1 (common_ancestor + 1)
  and* _ = Node.wait_for_level node_2 (common_ancestor + 1) in
  Log.info "Both nodes are at level %d." (common_ancestor + 1) ;
  let* () = Node.terminate node_2 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* _ = Node.wait_for_level node_1 (common_ancestor + 3) in
  let* () = Node.terminate node_1 in
  let* () = Node.run node_2 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_2 in
  let* () = Client.bake_for ~keys:[bootstrap2_key] client_2 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_2 in
  let* _ = Node.wait_for_level node_2 (common_ancestor + 3) in
  let* () = Node.run node_1 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_1 in
  let* node_3 =
    Node.init ~runner ~path [Synchronisation_threshold 0; Private_mode]
  in
  let endpoint_3 = Client.(Node node_3) in
  let* client_3 = Client.init ~endpoint:endpoint_3 () in
  let* accuser_3 = Accuser.init ~protocol node_3 in
  let denunciation = wait_for_denunciation accuser_3 in
  let denunciation_injection =
    wait_for_denunciation_injection node_3 client_3 denunciation
  in
  let* () =
    Client.Admin.trust_address client_1 ~endpoint:endpoint_1 ~peer:node_3
  and* () =
    Client.Admin.trust_address client_3 ~endpoint:endpoint_3 ~peer:node_1
  and* () =
    Client.Admin.trust_address client_2 ~endpoint:endpoint_2 ~peer:node_3
  and* () =
    Client.Admin.trust_address client_3 ~endpoint:endpoint_3 ~peer:node_2
  in
  let* () =
    Client.Admin.connect_address client_1 ~endpoint:endpoint_1 ~peer:node_3
  and* () =
    Client.Admin.connect_address client_2 ~endpoint:endpoint_2 ~peer:node_3
  in
  let* level = Node.wait_for_level node_3 (common_ancestor + 2) in
  Log.info "Level of node3 is %d, waiting for denunciation operation..." level ;
  let* denunciation_oph = denunciation in
  let* _ = denunciation_injection in
  let* () =
    Client.bake_for ~endpoint:endpoint_3 ~keys:[bootstrap1_key] client_3
  in
  let* _ = Node.wait_for_level node_2 (common_ancestor + 4)
  and* _ = Node.wait_for_level node_3 (common_ancestor + 4) in
  let* ops = Client.RPC.call client_1 @@ RPC.get_chain_block_operations () in
  let* () = Accuser.terminate accuser_3 in
  if is_operation_in_operations ops denunciation_oph then unit
  else Test.fail "Double baking evidence was not found"

let register ~protocols = double_bake protocols
