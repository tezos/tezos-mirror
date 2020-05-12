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

open Tezos_tezt
open Base

(* This test replicates part of the flextesa test "command_node_synchronization".
   To fully replicate it, the following are missing:
   - test several archive mode combinations for node_1 and node_2;
   - if node_1 mode is rolling, check that caboose > starting level;
   - if node_1 mode is archive or full, check that save_point > starting level;
   - if node_1 mode is rolling, the second synchronization phase (after node_2 was restarted)
     should fail, so "wait_for_event_or_fail" should never terminate;
   - explicitely test whether nodes are connected (with /network/connections);
   - node_1 should not have banned node_2. *)
let run () =
  (* Number of calls to [tezos-client bake for] once the protocol is activated,
     before we kill [node_2]. *)
  let bakes_before_kill = 9 in
  (* Number of calls to [tezos-client bake for] while [node_2] is not running. *)
  let bakes_during_kill = 100 in
  Test.run
    ~title:"node synchronization"
    ~tags:["bootstrap"; "node"; "sync"; "activate"; "bake"]
  @@ fun () ->
  let* node_1 = Node.init ~bootstrap_threshold:0 ~connections:1 ()
  and* node_2 = Node.init ~bootstrap_threshold:1 ~connections:1 () in
  let* client = Client.init ~node:node_1 () in
  let* () = Client.Admin.connect_address client ~peer:node_2 in
  let* () = Client.activate_protocol client in
  Log.info "Activated protocol." ;
  let* () = repeat bakes_before_kill (fun () -> Client.bake_for client) in
  let* _ = Node.wait_for_level node_1 (bakes_before_kill + 1)
  and* _ = Node.wait_for_level node_2 (bakes_before_kill + 1) in
  Log.info "Both nodes are at level %d." (bakes_before_kill + 1) ;
  let* () = Node.terminate node_2 in
  let* () = repeat bakes_during_kill (fun () -> Client.bake_for client) in
  Log.info "Baked %d times with node_2 down, restart node_2." bakes_during_kill ;
  Node.run ~bootstrap_threshold:1 ~connections:1 node_2 ;
  let* _ = Node.wait_for_ready node_2 in
  let* () = Client.Admin.connect_address client ~peer:node_2 in
  let final_level = 1 + bakes_before_kill + bakes_during_kill in
  let* _ = Node.wait_for_level node_1 final_level
  and* _ = Node.wait_for_level node_2 final_level in
  Log.info "Both nodes are at level %d." final_level ;
  unit
