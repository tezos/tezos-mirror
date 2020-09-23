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

open Base

let wait_for_sync node =
  let filter json =
    match JSON.(json |=> 1 |-> "event" |> as_string_opt) with
    | None ->
        None
    | Some status ->
        if status = "synced" then Some () else None
  in
  Node.wait_for node "node_chain_validator.v0" filter

(* This test starts 4 + 1 nodes. The main nodes activate the alpha
   protocol. All the other nodes connects to the main nodes and
   synchronize themselves. Then we restart all the nodes and check
   they are all in the state mode `sync`. *)

let check_node_synchronization_state () =
  let n = 4 in
  let blocks_to_bake = 5 in
  Test.run
    ~__FILE__
    ~title:"Check synchronization state"
    ~tags:["bootstrap"; "node"; "sync"]
  @@ fun () ->
  let* main_node = Node.init ~name:"main_node" () in
  let* nodes =
    Lwt_list.map_p
      (fun i -> Node.init ~name:("node" ^ string_of_int i) ())
      (range 1 n)
  in
  Log.info "%d nodes initiated." (n + 1) ;
  let* client = Client.init ~node:main_node () in
  let* () =
    Client.activate_protocol
      client
      ~timestamp_delay:(float_of_int blocks_to_bake)
  in
  Log.info "Activated protocol." ;
  let* () = repeat blocks_to_bake (fun () -> Client.bake_for client) in
  Log.info "Baked %d blocks." blocks_to_bake ;
  let* () =
    Lwt_list.iter_p
      (fun node ->
        Log.info "%s connects to %s." (Node.name main_node) (Node.name node) ;
        Client.Admin.connect_address client ~peer:node)
      nodes
  in
  let* () =
    Lwt_list.iter_p
      (fun node ->
        let* _ = Node.wait_for_level node (blocks_to_bake + 1) in
        unit)
      nodes
  in
  Log.info "Restarting the nodes..." ;
  let* _ =
    Lwt_list.iter_p (fun node -> Node.restart node) (main_node :: nodes)
  in
  Log.info "Waiting for nodes to be synchronized." ;
  let* () =
    Lwt_list.iter_p (fun node -> wait_for_sync node) (main_node :: nodes)
  in
  unit

let run () = check_node_synchronization_state ()
