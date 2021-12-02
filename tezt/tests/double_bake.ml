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
   Component:    Accuser
   Invocation:   dune exec tezt/tests/main.exe -- double baking accuser
   Subject:      Detect double baking through the accuser.
*)

(* This test is duplicated in [remote_tests/double_baking.ml]. Any modification
   to this test should be reported there too. *)

let is_operation_in_operations ops oph =
  let open JSON in
  let ops_list = ops |=> 2 |> as_list in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) ops_list

let is_operation_in_applied_mempool mempool oph =
  let open JSON in
  let applied_list = as_list (mempool |-> "applied") in
  List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list

(* Matches events where the message is of the form:
   "double baking evidence injected <operation_hash>".
   For example:

    "event": {
      "double_baking_denounced.v0": {
        "hash": "onkfjSun49iRrGtuN9FwtiCqDAEgzPKzg1BSa7BSHnaAkButUxx",
        "bytes": "..."
      }
    }
 *)
let wait_for_denunciation accuser =
  let filter json = JSON.(json |-> "hash" |> as_string_opt) in
  Accuser.wait_for accuser "double_baking_denounced.v0" filter

(* Matches events which contain an injection request.
   For example:

   "event": {
     "node_prevalidator.v0": [
       "2020-09-11T12:32:05.353-00:00",
       {
         "event": {
           "request": {
             "request": "inject",
             "operation": {
               "branch": "BM3J62AvjnjJKfinoq1op2uw5Hdn3YGMQmusnLdrfCd1yrpftG2",
               "data": "030000...00000"
             }
           },
           "status": {
             "pushed": "2020-09-11T12:32:05.343-00:00",
             "treated": 4.5947e-05,
             "completed": 0.009614550999999999
           }
         },
         "level": "notice"
       }
     ]
   }
 *)
let wait_for_denunciation_injection node client oph_promise =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_notice.v0" filter in
  let* oph = oph_promise in
  let* mempool = RPC.get_mempool_pending_operations client in
  if is_operation_in_applied_mempool mempool oph then some oph else none

(* This tests aims to detect a double baking evidence with an accuser. The
   scenario is the following:

   1. Node 1 activates a protocol, and bakes validators_selection_offset blocks
   (this is because for a double-signing operation to be valid, the
   double-signer must have some frozen balance (see the call of
   Unrequired_double_x_evidence in apply.ml) while in Tenderbake, for the first
   validators_selection_offset levels, no deposit is taken (see
   [handle_deposits] in apply.ml.

   2. Node 2 catches up with Node 1,

   3. Node 2 is terminated. Then, Node 1 bakes two blocks from level 1 with
   bootstrap1 key,

   4. Node 1 is terminated. Then, Node 2 is restarted and bakes two blocks from
   level 1; the first one with the bootstrap2 key and the second one with the
   bootstrap1 key. Thus, the block at level 3 is double baked (and we ensure
   that the double baked blocks are different as they emanate from two distinct
   branches),

   5. Node 1 came back in the dance,

   6. Node 3 is run along with its accuser and catches up. The accuser must
   detect the double baking evidence and generate an operation accordingly,

   7. A block is baked.

   The test is successful if the double baking evidence can be found in the last
   baked block. *)
let double_bake =
  Protocol.register_test
    ~__FILE__
    ~title:"double baking with accuser"
    ~tags:["double"; "baking"; "accuser"; "node"]
  @@ fun protocol ->
  (* Step 1 and 2 *)
  (* Note: we start all nodes with [--private] to prevent the [connect address]
     command from [node_2] to [node_3] from failing due to an "already connected"
     error that could otherwise non-deterministically occur due to P2P propagation.
     This means that we need to use [trust address] too. *)
  let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode]
  and* node_2 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let bootstrap1_key = Constant.bootstrap1.alias in
  let bootstrap2_key = Constant.bootstrap2.alias in
  let common_ancestor = 0 in
  let* _ = Node.wait_for_level node_1 (common_ancestor + 1)
  and* _ = Node.wait_for_level node_2 (common_ancestor + 1) in
  Log.info "Both nodes are at level %d." (common_ancestor + 1) ;
  (* Step 3 *)
  let* () = Node.terminate node_2 in
  (* Craft a branch of size 2, baked by bootstrap1 *)
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_1 in
  let* _ = Node.wait_for_level node_1 (common_ancestor + 3) in
  (* Step 4 *)
  let* () = Node.terminate node_1 in
  let* () = Node.run node_2 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_2 in
  (* Craft a branch of size 2, the first block is baked by bootstrap2 *)
  let* () = Client.bake_for ~keys:[bootstrap2_key] client_2 in
  (* The second block is double baked by bootstrap1 to simulate a
     double bake *)
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_2 in
  let* _ = Node.wait_for_level node_2 (common_ancestor + 3) in
  (* Step 5 *)
  let* () = Node.run node_1 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node_1 in
  (* Step 6 *)
  let* node_3 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_3 = Client.init ~endpoint:(Node node_3) () in
  let* accuser_3 = Accuser.init ~protocol node_3 in
  let denunciation = wait_for_denunciation accuser_3 in
  let denunciation_injection =
    wait_for_denunciation_injection node_3 client_3 denunciation
  in
  let* () = Client.Admin.trust_address client_1 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_1
  and* () = Client.Admin.trust_address client_2 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_3
  and* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let* level = Node.wait_for_level node_3 (common_ancestor + 2) in
  (* Ensure that the denunciation was emitted by the accuser *)
  Log.info "Level of node3 is %d, waiting for denunciation operation..." level ;
  let* denunciation_oph = denunciation in
  (* Ensure that the denunciation is in node_3's mempool *)
  let* _ = denunciation_injection in
  (* Step 7 *)
  let* () = Client.bake_for ~keys:[bootstrap1_key] client_3 in
  let* _ = Node.wait_for_level node_1 (common_ancestor + 4)
  and* _ = Node.wait_for_level node_2 (common_ancestor + 4)
  and* _ = Node.wait_for_level node_3 (common_ancestor + 4) in
  (* Getting the operations of the current head *)
  let* ops = RPC.get_operations client_1 in
  let* () = Accuser.terminate accuser_3 in
  if is_operation_in_operations ops denunciation_oph then unit
  else Test.fail "Double baking evidence was not found"

let register ~protocols = double_bake ~protocols
