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
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* _node = Node.init [Connections cap] in
  unit

let on_terminate resolver status =
  match status with
  | Unix.WEXITED x when x = 1 -> Lwt.wakeup resolver ()
  | _ -> ()

(* Wait for a node to be ready or to exit, and fail if the node became ready.

   In this module we sometimes have to wait for an error to happen
   before the node is ready. After this error happens the node exits.
   If the node becomes ready, we know that what we are waiting for will not happen,
   so we raise an error instead of waiting forever.

   But [Node.wait_for_ready] also raises an error if the node exits
   before the node is ready. If we're unlucky with concurrency, we could notice
   that the node exited before reading the error that we expect.
   We make sure this does not happen by catching [Node.Terminated_before_event]. *)
let fail_if_ready node error_message =
  Lwt.catch (fun () ->
      let* () = Node.wait_for_ready node in
      return @@ Test.fail "%s" error_message)
  @@ function
  | Node.Terminated_before_event _ -> unit
  | exn -> raise exn

let check_connections_above_cap () =
  Test.register
    ~__FILE__
    ~title:"CLI above connections cap"
    ~tags:["cli"; "connections"; "bad"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let has_failed, on_failure = Lwt.task () in
  let node = Node.create [] in
  let* _node =
    Node.run
      ~on_terminate:(on_terminate on_failure)
      node
      [Connections (cap * 2)]
  in
  let is_ready =
    fail_if_ready node "The node should fail and should not be ready"
  in
  Lwt.pick [is_ready; has_failed]

let check_node_net_addr_colision_message () =
  Test.register
    ~__FILE__
    ~title:"CLI --net-addr collision message"
    ~tags:["cli"; "address"]
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let net_port = Port.fresh () in
  let* _node1 = Node.init ~net_port [] in
  let node2 = Node.create ~net_port [] in
  let readiness =
    fail_if_ready
      node2
      "Node was unexpectedly started instead of failing because of network \
       binding collision"
  in
  let error_checking =
    let* () = Node.identity_generate node2 in
    let* () = Node.config_init node2 [] in
    let* () = Node.run node2 [] in
    let error_rex =
      rex
        ~opts:[`Dotall]
        (Printf.sprintf
           "127\\.0\\.0\\.1:%d.*Another tezos node is probably running on this \
            address.*P2P"
           net_port)
    in
    Node.check_error ~msg:error_rex node2
  in
  Lwt.pick [error_checking; readiness]

let register_protocol_independent () =
  check_connections_below_cap () ;
  check_connections_above_cap () ;
  check_node_net_addr_colision_message ()
