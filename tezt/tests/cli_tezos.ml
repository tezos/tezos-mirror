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

let cap = 100

let check_connections_below_cap () =
  Test.register
    ~__FILE__
    ~title:"CLI under connections cap"
    ~tags:["cli"; "connections"]
  @@ fun () ->
  let* _node = Node.init [Connections cap] in
  unit

let on_terminate resolver status =
  match status with
  | Unix.WEXITED x when x = 1 -> Lwt.wakeup resolver ()
  | _ -> ()

let check_connections_above_cap () =
  Test.register
    ~__FILE__
    ~title:"CLI above connections cap"
    ~tags:["cli"; "connections"; "bad"]
  @@ fun () ->
  let (has_failed, on_failure) = Lwt.task () in
  let node = Node.create [] in
  let* _node =
    Node.run
      ~on_terminate:(on_terminate on_failure)
      node
      [Connections (cap * 2)]
  in
  let is_ready_p =
    let* () = Node.wait_for_ready node in
    Lwt.return_true
  in
  let has_failed_p =
    let* () = has_failed in
    Lwt.return_false
  in
  let* is_ready = Lwt.pick [is_ready_p; has_failed_p] in
  if is_ready then Test.fail "The node should fail and should not be ready"
  else unit

let check_node_addr_colision_message net_port1 net_port2 rpc_port1 rpc_port2
    title msg =
  Test.register ~__FILE__ ~title ~tags:["cli"; "address"] @@ fun () ->
  let* _node1 = Node.init ~net_port:net_port1 ~rpc_port:rpc_port1 [] in
  let node2 = Node.create ~net_port:net_port2 ~rpc_port:rpc_port2 [] in
  let readiness =
    let* () = Node.wait_for_ready node2 in
    return
    @@ Test.fail
         "Node was unexpectedly started instead of failing because of network \
          binding collision"
  in
  let error_checking =
    let* () = Node.identity_generate node2 in
    let* () = Node.config_init node2 [] in
    let* () = Node.run node2 [] in
    let error_rex = rex ~opts:[`Dotall] msg in
    Node.check_error ~msg:error_rex node2
  in
  Lwt.pick [error_checking; readiness]

let check_node_net_addr_colision_message () =
  let p1 = Node.fresh_port () in
  let p2 = Node.fresh_port () in
  let p3 = Node.fresh_port () in
  check_node_addr_colision_message
    p1
    p1
    p2
    p3
    "CLI --net-addr collision message"
    (Printf.sprintf
       "127\\.0\\.0\\.1:%d.*Another tezos node is probably running on this \
        address.*P2P"
       p1)

let register_protocol_independent () =
  check_connections_below_cap () ;
  check_connections_above_cap () ;
  check_node_net_addr_colision_message ()
