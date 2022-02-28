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

let on_validation_event state node Node.{name; value} =
  if name = "node_block_validator.v0" then
    match JSON.(value |=> 1 |-> "event" |-> "kind" |> as_string_opt) with
    | Some "prechecked" -> (
        let hash = JSON.(value |=> 1 |-> "event" |-> "block" |> as_string) in
        match Hashtbl.find_opt state (node, hash) with
        | None -> Hashtbl.replace state (node, hash) Prechecked
        | Some Prechecked -> Test.fail "A block should not be prechecked twice"
        | Some Validated ->
            Test.fail "A block should not be prechecked after being validated")
    | Some _ -> ()
    | None -> (
        match
          JSON.(value |=> 1 |-> "event" |-> "successful_validation" |> as_opt)
        with
        | None -> ()
        | Some _ -> (
            let hash =
              JSON.(
                value |=> 1 |-> "event" |-> "successful_validation" |-> "block"
                |> as_string)
            in
            match Hashtbl.find_opt state (node, hash) with
            | None ->
                Test.fail "A block should be prechecked before being validated"
            | Some Prechecked -> Hashtbl.replace state (node, hash) Validated
            | Some Validated ->
                Test.fail "A block should not be validated twice"))

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
  let n1 = Node.create [Private_mode] in
  let ring = Cluster.create ~name:"ring" 4 [Private_mode] in
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

let forge_block ~protocol ?client node ~key =
  Log.info "Creating another node to forge a block" ;
  let* client =
    match client with
    | None -> Client.(init ~endpoint:(Node node) ())
    | Some c -> return c
  in
  let* node_level = Client.level client in
  let* node2 = Node.init [] in
  let* client2 = Client.(init ~endpoint:(Node node2) ()) in
  let* () =
    Client.Admin.trust_address ~endpoint:(Node node) ~peer:node2 client
  in
  let* () = Client.Admin.connect_address ~peer:node2 client in
  let* _ = Node.wait_for_level node2 node_level in
  let* node2_id = Node.wait_for_identity node2 in
  let* () = Client.Admin.kick_peer ~peer:node2_id client in
  let* () =
    let open Protocol in
    if protocol = Hangzhou then Client.bake_for ~keys:[key] client2
    else
      (* We want an empty block, in tenderbake, we can simply propose
         so that there is no endorsement operations. *)
      Client.propose_for ~key:[key] ~force:true client2
  in
  let* shell =
    Client.shell_header client2 >>= fun shell ->
    JSON.parse ~origin:"forge_fake_block" shell |> return
  in
  let* protocol_data = RPC.raw_protocol_data client2 in
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
    ~tags:["precheck"; "fake_block"; "propagation"]
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let n1 = Node.create [] in
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
  let* client = Client.(init ~endpoint:(Node n1) ()) in
  let* () = Client.activate_protocol ~protocol client in
  let bootstrap1 = Constant.bootstrap1.alias in
  let* () =
    List.init blocks_to_bake Fun.id
    |> List.map succ
    |> Lwt_list.iter_s (fun i ->
           let* () = Client.bake_for ~keys:[bootstrap1] client in
           wait_for_cluster_at_level cluster i)
  in
  let* block_header = forge_block ~protocol ~client n1 ~key:bootstrap1 in
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
  let injection_json =
    `O
      [
        ("data", `String signed_bad_block_header_hex);
        ("operations", `A (List.init 4 (fun _ -> `A [])));
      ]
  in
  let wait_precheck_but_validation_fail node =
    let got_prechecked = ref false in
    Node.wait_for node "node_block_validator.v0" (fun value ->
        match JSON.(value |=> 1 |-> "event" |-> "kind" |> as_string_opt) with
        | Some "prechecked" ->
            got_prechecked := true ;
            None
        | Some _ -> None
        | None -> (
            match
              JSON.(
                value |=> 1 |-> "event" |-> "failed_validation_after_precheck"
                |> as_opt)
            with
            | None -> None
            | Some _ -> if !got_prechecked then Some () else assert false))
  in
  (* Wait all nodes to precheck the block but fail on validation *)
  let precheck_waiter =
    Lwt_list.iter_p wait_precheck_but_validation_fail cluster
  in
  let p =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
  in
  let* () = Process.check_error p in
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
    |> Process.check_error
  in
  Log.info "Bake a valid block and check the cluster receives it" ;
  (* One final bake to ensure everyone is at the same level *)
  let* () = Client.bake_for ~keys:[bootstrap1] client in
  (* activation block + four blocks + the final bake *)
  wait_for_cluster_at_level cluster (1 + blocks_to_bake + 1)

let register ~protocols =
  precheck_block protocols ;
  propagate_precheckable_bad_block protocols
