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
   Component:    Validation components
   Invocation:   dune exec tezt/tests/main.exe -- --file "precheck.ml"
   Subject:      Check the precheck of blocks.
*)

open Lwt.Infix

type state = Prechecked | Validated

let on_validation_event state node Node.{name; value; timestamp = _} =
  match name with
  | "prechecked_block.v0" -> (
      let hash = JSON.(value |> as_string) in
      match Hashtbl.find_opt state (node, hash) with
      | None -> Hashtbl.replace state (node, hash) Prechecked
      | Some Prechecked -> Test.fail "A block should not be prechecked twice"
      | Some Validated ->
          Test.fail "A block should not be prechecked after being validated")
  | "validation_success.v0" -> (
      let hash = JSON.(value |-> "block" |> as_string) in
      match Hashtbl.find_opt state (node, hash) with
      | None -> Test.fail "A block should be prechecked before being validated"
      | Some Prechecked -> Hashtbl.replace state (node, hash) Validated
      | Some Validated -> Test.fail "A block should not be validated twice")
  | _ -> ()

let wait_for_cluster_at_level cluster level =
  Lwt_list.iter_p
    (fun node ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit)
    cluster

let precheck_block =
  Protocol.register_test
    ~__FILE__
    ~title:"precheck block"
    ~tags:["node"; "precheck"]
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let n1 = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [n1] [n2] ;
  let cluster = n1 :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let block_to_bake = 4 in
  let state = Hashtbl.create 11 in
  List.iter
    (fun node -> Node.on_event node (on_validation_event state node))
    cluster ;
  let* c1 = Client.(init ~endpoint:(Node n1) ()) in
  let* () = Client.activate_protocol ~protocol c1 in
  let current_level = ref 1 in
  let* () =
    repeat block_to_bake (fun () ->
        let* () = Client.bake_for c1 in
        incr current_level ;
        wait_for_cluster_at_level cluster !current_level)
  in
  let validated =
    Hashtbl.fold (fun _ value b -> value = Validated && b) state true
  in
  let expected_table_length = (block_to_bake + 1) * List.length cluster in
  if Hashtbl.length state <> expected_table_length || not validated then
    Test.fail "prechecking of block did not executed as expected"
  else return ()

let forge_block ?client node ~key ~with_op =
  Log.info "Creating another node to forge a block" ;
  let* client =
    match client with
    | None -> Client.(init ~endpoint:(Node node) ())
    | Some c -> return c
  in
  let* node_level = Client.level client in
  let* node2 = Node.init [Synchronisation_threshold 0] in
  let* client2 = Client.(init ~endpoint:(Node node2) ()) in
  let* () =
    Client.Admin.trust_address ~endpoint:(Node node) ~peer:node2 client
  in
  let* () = Client.Admin.connect_address ~peer:node2 client in
  let* _ = Node.wait_for_level node2 node_level in
  let* node2_id = Node.wait_for_identity node2 in
  let* () = Client.Admin.kick_peer ~peer:node2_id client in
  let* () =
    if with_op then
      let* _ = Operation.Manager.(inject [make @@ transfer ()] client2) in
      unit
    else unit
  in
  let* () =
    (* We want an empty block, in tenderbake, we can simply propose
       so that there is no attestation operations. *)
    Client.propose_for ~key:[key] ~force:true client2
  in
  let* shell =
    Client.shell_header client2 >>= fun shell ->
    JSON.parse ~origin:"forge_fake_block" shell |> return
  in
  let* protocol_data =
    Client.RPC.call client2 @@ RPC.get_chain_block_header_protocol_data_raw ()
  in
  Log.info
    "Sufficient information retrieved: shutting down second node, restarting \
     first node" ;
  let* () = Node.terminate node2 in
  let block_header =
    JSON.update
      "protocol_data"
      (fun _ ->
        JSON.annotate ~origin:"block header crafting" (`String protocol_data))
      shell
  in
  return block_header

let propagate_precheckable_bad_block =
  let blocks_to_bake = 4 in
  Protocol.register_test
    ~__FILE__
    ~title:"forge fake block"
    ~tags:["precheck"; "fake_block"; "propagation"; Tag.memory_3k]
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let node_client = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [node_client] [n2] ;
  let cluster = node_client :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let* client = Client.(init ~endpoint:(Node node_client) ()) in
  let* () = Client.activate_protocol ~protocol client in
  let bootstrap1 = Constant.bootstrap1.alias in
  let* () =
    List.init blocks_to_bake Fun.id
    |> List.map succ
    |> Lwt_list.iter_s (fun i ->
           let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
           wait_for_cluster_at_level cluster i)
  in
  let* block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:false
  in
  (* Put a bad context *)
  Log.info "Crafting a block header with a bad context hash" ;
  let dummy_context_hash =
    `String "CoUeJrcPBj3T3iJL3PY4jZHnmZa5rRZ87VQPdSBNBcwZRMWJGh9j"
  in
  let bad_block_header =
    JSON.update
      "context"
      (fun _ -> JSON.annotate ~origin:"bad context hash" dummy_context_hash)
      block_header
  in
  let* block_header_hex =
    Codec.encode ~name:"block_header" (JSON.unannotate bad_block_header)
    >>= fun hex -> String.trim hex |> return
  in
  Log.info "Re-signing the bad block header" ;
  (* Remove the signature *)
  let unsigned_block_header_hex =
    String.sub block_header_hex 0 (String.length block_header_hex - 128)
  in
  let* signature =
    Client.sign_block client unsigned_block_header_hex ~delegate:bootstrap1
    >>= fun s -> String.trim s |> return
  in
  let signed_bad_block_header_hex =
    String.concat "" [unsigned_block_header_hex; signature]
  in
  let injection_json : RPC_core.data =
    Data
      (`O
        [
          ("data", `String signed_bad_block_header_hex);
          ("operations", `A (List.init 4 (fun _ -> `A [])));
        ])
  in
  (* Wait all nodes to precheck the block but fail on validation *)
  let expect_precheck_failure node =
    Node.wait_for node "precheck_failure.v0" (fun _ -> Some ())
  in
  let precheck_waiter =
    (* Post Lima: the precheck is not an over-approximation
       anymore and cannot even be considered precheckable. *)
    expect_precheck_failure node_client
  in
  let p =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
  in
  let* () = Process.check_error ~msg:(rex "Inconsistent hash") p in
  let* () =
    Lwt.pick
      [
        ( Lwt_unix.sleep 30. >>= fun () ->
          Test.fail "timeout while waiting for precheck" );
        precheck_waiter;
      ]
  in
  (* Also check that re-injecting the bad block fails *)
  let* () =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
    |> Process.check_error ~msg:(rex "Inconsistent hash")
  in
  Log.info
    "Bake a valid block and check the cluster receives it (ensuring the \
     cluster is still connected)." ;
  (* One final bake to ensure everyone is at the same level *)
  let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
  (* activation block + four blocks + the final bake *)
  wait_for_cluster_at_level cluster (1 + blocks_to_bake + 1)

let propagate_precheckable_bad_block_payload =
  let blocks_to_bake = 4 in
  Protocol.register_test
    ~__FILE__
    ~title:"forge block with wrong payload"
    ~tags:["precheck"; "fake_block"; "propagation"; "payload"; Tag.memory_3k]
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let node_client = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [node_client] [n2] ;
  let cluster = node_client :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let* client = Client.(init ~endpoint:(Node node_client) ()) in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      [(["proof_of_work_threshold"], `String "-1")]
  in
  let* () = Client.activate_protocol ~parameter_file ~protocol client in
  let bootstrap1 = Constant.bootstrap1.alias in
  let* () =
    List.init blocks_to_bake Fun.id
    |> List.map succ
    |> Lwt_list.iter_s (fun i ->
           let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
           wait_for_cluster_at_level cluster i)
  in
  let* op_block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:true
  in
  let* block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:false
  in
  (* Put a bad context *)
  Log.info "Crafting a block header with a bad context hash" ;
  let bad_block_header =
    JSON.update
      "protocol_data"
      (fun _ ->
        JSON.annotate
          ~origin:"bad context hash"
          (`String JSON.(op_block_header |-> "protocol_data" |> as_string)))
      block_header
  in
  let* bad_block_header_hex =
    Codec.encode ~name:"block_header" (JSON.unannotate bad_block_header)
    >>= fun hex -> String.trim hex |> return
  in
  (* Remove the signature *)
  let unsigned_bad_block_header_hex =
    String.sub bad_block_header_hex 0 (String.length bad_block_header_hex - 128)
  in
  let* signature =
    Client.sign_block client unsigned_bad_block_header_hex ~delegate:bootstrap1
    >>= fun s -> String.trim s |> return
  in
  let signed_bad_block_header_hex =
    String.concat "" [unsigned_bad_block_header_hex; signature]
  in
  let injection_json : RPC_core.data =
    Data
      (`O
        [
          ("data", `String signed_bad_block_header_hex);
          ("operations", `A (List.init 4 (fun _ -> `A [])));
        ])
  in
  let expect_precheck_failure node =
    Node.wait_for node "precheck_failure.v0" (fun _ -> Some ())
  in
  let precheck_waiter =
    (* Post Kathmandu: the precheck is not an over-approximation
       anymore and cannot even be considered precheckable. *)
    expect_precheck_failure node_client
  in
  let p =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
  in
  let* () = Process.check_error ~msg:(rex "Invalid payload hash") p in
  let* () =
    Lwt.pick
      [
        ( Lwt_unix.sleep 10. >>= fun () ->
          Test.fail "timeout while waiting for precheck" );
        precheck_waiter;
      ]
  in
  (* Also check that re-injecting the bad block fails *)
  let* () =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
    |> Process.check_error ~msg:(rex "Invalid payload hash")
  in
  let* _ = Client.RPC.call client @@ RPC.get_chain_block () in
  Log.info
    "Bake a valid block and check the cluster receives it (ensuring the \
     cluster is still connected)." ;
  (* One final bake to ensure everyone is at the same level *)
  let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
  (* activation block + four blocks + the final bake *)
  wait_for_cluster_at_level cluster (1 + blocks_to_bake + 1)

let register ~protocols =
  precheck_block protocols ;
  propagate_precheckable_bad_block protocols ;
  propagate_precheckable_bad_block_payload protocols
