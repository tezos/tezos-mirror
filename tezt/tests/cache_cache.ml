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
   Component:    Shell / Cache
   Invocation:   dune exec tezt/tests/main.exe -- --file cache_cache.ml
   Subject:      Check cache('s cache) consistency.
*)

(* Check that annotations in contracts do not break cache consistency. *)
let cache_annotation_consistency =
  Protocol.register_test
    ~__FILE__
    ~title:"cache annotation consistency"
    ~tags:["protocol_cache"; "cache"; "node"; "baker"]
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let data_dir = Node.data_dir node in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* _alias, contract_hash =
    Client.originate_contract_at
      ~init:"{}"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "cache_consistency"]
      protocol
  in
  let* () = wait_injection in
  (* We use [context_path] to ensure the baker will not use the
     preapply RPC. Indeed, this test was introduced because of a bug
     that happens when the baker does not use the preapply RPC. *)
  let* () =
    Client.bake_for_and_wait ~context_path:(data_dir // "context") client
  in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* (`OpHash _todo) =
    Operation.inject_contract_call
      ~amount:0
      ~source:Constant.bootstrap1
      ~dest:contract_hash
      ~entrypoint:"renew"
      ~arg:(`Michelson "Unit")
      client
  in
  let* () = wait_injection in
  let* () =
    Client.bake_for_and_wait ~context_path:(data_dir // "context") client
  in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* (`OpHash _op_hash) =
    Operation.inject_contract_call
      ~amount:0
      ~source:Constant.bootstrap1
      ~dest:contract_hash
      ~entrypoint:"keep"
      ~arg:(`Michelson "Unit")
      client
  in
  let* () = wait_injection in
  Client.bake_for_and_wait ~context_path:(data_dir // "context") client

let singleprocess_reorg =
  Protocol.register_test
    ~__FILE__
    ~title:"cache consistency on singleprocess reorg"
    ~tags:["protocol_cache"; "singleprocess"; "reorg"]
  @@ fun protocol ->
  let minimal_block_delay = 5 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["minimal_block_delay"], `String (string_of_int minimal_block_delay))]
  in
  let* node1, client1 =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      ~timestamp:Now
      ~parameter_file
      `Client
      ()
  in
  let* node2, _client2 =
    Client.init_with_node
      ~nodes_args:[Singleprocess; Synchronisation_threshold 0]
      `Client
      ()
  in
  let error_events =
    [
      "block_validation_inconsistent_cache.v0";
      "validation_failure_after_precheck.v0";
      "precheck_failure.v0";
      "validation_failed.v0";
    ]
  in
  List.iter
    (fun name ->
      let fail_on_event event =
        if event.Node.name = name then
          Test.fail "Received an inconsistent hash while validating a block"
      in
      Node.on_event node1 fail_on_event ;
      Node.on_event node2 fail_on_event)
    error_events ;
  Log.info
    "Connecting nodes and waiting for the activation block to be propagated" ;
  let* () = Client.Admin.connect_address ~peer:node2 client1 in
  let* (_level : int) = Node.wait_for_level node2 1 in
  Log.info "Proposing a new block with round 0" ;
  let* () = Client.propose_for ~minimal_timestamp:false ~key:[] client1 in
  let* (_level : int) = Node.wait_for_level node1 2 in
  let* (_level : int) = Node.wait_for_level node2 2 in
  Log.info "Waiting until we are sure to repropose a block at the same level..." ;
  let* () = Lwt_unix.sleep (float minimal_block_delay) in
  let wait_for_reorg_event node =
    Node.wait_for node "branch_switch.v0" (fun _ -> Some ())
  in
  let waiter = Lwt.join (List.map wait_for_reorg_event [node1; node2]) in
  Log.info "Bake a block on a different branch" ;
  let* () = Client.bake_for ~minimal_timestamp:false ~keys:[] client1 in
  Log.info "Waiting for both node to switch heads" ;
  let* () = waiter in
  Log.info "Baking block at level 3" ;
  let* () = Client.bake_for ~minimal_timestamp:false ~keys:[] client1 in
  Log.info "Waiting for both nodes to increase their head" ;
  let* (_level : int) = Node.wait_for_level node1 3 in
  let* (_level : int) = Node.wait_for_level node2 3 in
  unit

let register ~protocols =
  cache_annotation_consistency protocols ;
  singleprocess_reorg protocols
