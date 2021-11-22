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
   Component:    Precheck
   Invocation:   dune exec tezt/tests/main.exe -- --file manager_operations.ml
   Subject:      Tests that covers differents cases of manager operations
*)

open Tezt_tezos
open Lwt.Infix

(** A node with an associated client (with endpoint this node).  *)
type node_client = {node : Node.t; client : Client.t}

(** A pair of connected nodes. [main] is the node/client with which the tests interact
    directlty (for injections, baking, queries, etc.)
    [observer] acts as a node on which we observe events of the prevalidator. *)
type two_nodes = {main : node_client; observer : node_client}

module Operation = struct
  include Operation

  (* We always inject operations in async and force mode for these tests. *)
  let forge_and_inject_operation =
    forge_and_inject_operation ~async:true ~force:true
end

module Log = struct
  include Log

  let section fmt = Log.info ~color:Log.Color.BG.green fmt

  let ok fmt = Log.info ~color:Log.Color.FG.green fmt
end

(* Hardcoded predefined contracts *)
module Contracts = struct
  (* parameter unit ;
     storage nat ;
     code { DROP ; PUSH int 0 ; NIL operation ; PAIR } *)
  let illtyped_contract_body_1 =
    Ezjsonm.from_string
      {|
[ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
  { "prim": "storage", "args": [ { "prim": "nat" } ] },
  { "prim": "code",
    "args":
      [ [ { "prim": "DROP" },
          { "prim": "PUSH", "args": [ { "prim": "int" }, { "int": "0" } ] },
          { "prim": "NIL", "args": [ { "prim": "operation" } ] },
          { "prim": "PAIR" } ] ] } ]
|}

  (* parameter unit ;
     storage nat ;
     code { DROP ; PUSH nat 0 ; NEG; NIL operation ; PAIR } *)
  let illtyped_contract_body_2 =
    Ezjsonm.from_string
      {|
[ { "prim": "parameter", "args": [ { "prim": "unit" } ] },
  { "prim": "storage", "args": [ { "prim": "nat" } ] },
  { "prim": "code",
    "args":
      [ [ { "prim": "DROP" },
          { "prim": "PUSH", "args": [ { "prim": "nat" }, { "int": "0" } ] },
          { "prim": "NEG" },
          { "prim": "NIL", "args": [ { "prim": "operation" } ] },
          { "prim": "PAIR" } ] ] } ]
|}
end

(** This module provides helper functions and wrappers to check the
    classification of operations in the mempool, their propagation and
    their inclusion in a block. *)
module Memchecks = struct
  (** Bake a block and wait for the node to switch to this head *)
  let bake_and_wait_block {client; node} =
    let* level_json = RPC.get_current_level client in
    let level = JSON.(level_json |-> "level" |> as_int) in
    let* () = Client.bake_for client in
    let* _i = Node.wait_for_level node (level + 1) in
    Lwt.return_unit

  (** Wait for a node to be notified of a mempool. *)
  let wait_for_notify node =
    let filter json =
      let open JSON in
      match
        ( json |-> "view" |-> "request" |> as_string_opt,
          json |-> "view" |-> "mempool" |-> "known_valid" |> as_list_opt,
          json |-> "view" |-> "mempool" |-> "pending" |> as_list_opt )
      with
      | (Some "notify", Some [], Some []) -> None
      | (Some "notify", Some known_valid, Some pending) ->
          let known_valid = List.map JSON.as_string known_valid in
          let pending = List.map JSON.as_string pending in
          Some (known_valid, pending)
      | _ -> None
    in
    Node.wait_for node "request_completed_debug.v0" filter

  (** Wait for a node to be have processed a new block. *)
  let wait_for_processed_block node =
    let filter json =
      let open JSON in
      match
        json |=> 1 |-> "event" |-> "processed_block" |-> "request" |-> "hash"
        |> as_string_opt
      with
      | None -> None
      | Some s when s.[0] = 'B' -> Some (Some s)
      | Some _ -> Some None
    in
    Node.wait_for node "node_chain_validator.v0" filter

  (** Wait for a node to be notified of a mempool or have processed a
      new block. *)
  let wait_for_notify_or_processed_block node =
    let notify_promise =
      wait_for_notify node >|= fun mempool -> `Notify_mempool mempool
    in
    let processed_promise =
      wait_for_processed_block node >|= fun b -> `New_block_hash b
    in
    Lwt.npick [notify_promise; processed_promise]

  let check_operation_is_in_applied_mempool ~__LOC__ ?(explain = "")
      (mempool : Mempool.t) oph =
    if List.mem oph mempool.Mempool.applied then
      Log.ok "  - %s found in mempool.applied %s" oph explain
    else Test.fail ~__LOC__ "%s was not found in mempool.applied %s" oph explain

  let check_operation_not_in_applied_mempool ~__LOC__ ?(explain = "")
      (mempool : Mempool.t) oph =
    if List.mem oph mempool.Mempool.applied then
      Test.fail
        ~__LOC__
        "%s is in mempool.applied %s but should not be"
        oph
        explain
    else Log.ok "  - %s was not in mempool.applied %s" oph explain

  let get_op_status op =
    JSON.(op |-> "metadata" |-> "operation_result" |-> "status" |> as_string)

  (** Wait for nodes to be synchronized, i.e. for node observer to be
     at the same level as main node (the baker only bakes on main node
     in these tests). *)
  let wait_sync nodes =
    let* level1_json = RPC.get_current_level nodes.main.client in
    let level_main = JSON.(level1_json |-> "level" |> as_int) in
    Node.wait_for_level nodes.observer.node level_main

  (** Check that operation whose hash is [oph] is included in the
      block [block] or the head, with the statuses [expected_statuses]
      (there can be several is the operation is a batch). *)
  let check_status_in_block ~who ~oph ~expected_statuses ?block client =
    Log.info "- Checking inclusion and status of operation in %s's block" who ;
    let* head = RPC.get_block ?block client in
    let ops = JSON.(head |-> "operations" |=> 3 |> as_list) in
    let head_hash = JSON.(head |-> "hash" |> as_string) in
    let op_contents =
      match
        List.find_opt (fun op -> oph = JSON.(op |-> "hash" |> as_string)) ops
      with
      | None -> []
      | Some op -> JSON.(op |-> "contents" |> as_list)
    in
    Check.((List.length op_contents = List.length expected_statuses) int)
      ~error_msg:"expected contents to contain %R values, got %L" ;
    List.iter2
      (fun op expected_status ->
        let status = get_op_status op in
        if not (String.equal status expected_status) then
          Test.fail
            ~__LOC__
            "Unexpected operation status: %s got %s instead of %s for operation:\n\
             %s"
            who
            status
            expected_status
            (JSON.encode op))
      op_contents
      expected_statuses ;
    return head_hash

  (** Inject an opertion while performing a series of checks:
      - Start waiting for the observer node to receive operations
      - Inject operation
      - Check classification in mempool of main node
      - Wait for the operations to be propagated to observer
      - Check that observer was indeed notified of the operation
      - Check classification in mempool of observer node
      - Bake a block on main node
      - Check that the operation is included in said block
      - Check that the operation is not in the mempool anymore
  *)
  let with_applied_checks ~__LOC__ nodes ~expected_statuses inject =
    Log.section "Checking applied operation" ;
    let* _ = wait_sync nodes in
    let client = nodes.main.client in
    let wait_observer = wait_for_notify nodes.observer.node in
    Log.info "- Injecting operation" ;
    let* oph = inject () in
    let* mempool_after_injection = RPC.get_mempool client in
    check_operation_is_in_applied_mempool
      ~__LOC__
      ~explain:"after injection"
      mempool_after_injection
      oph ;
    Log.info "- Waiting for observer to be notified of operation" ;
    let* observer_result = wait_observer in
    Log.info "- Checking observer received operations" ;
    let (known_valid, pending) = observer_result in
    if List.mem oph known_valid then
      Log.ok "  - %s was propagated to observer node as valid" oph
    else if List.mem oph pending then
      Test.fail ~__LOC__ "%s was propagated to observer node as pending" oph ;
    let* mempool_observer = RPC.get_mempool nodes.observer.client in
    check_operation_is_in_applied_mempool
      ~__LOC__
      ~explain:"in observer"
      mempool_observer
      oph ;
    Log.info "- Baking (should include operation %s)" oph ;
    let* () = bake_and_wait_block nodes.main in
    let* _head_hash =
      check_status_in_block ~oph ~expected_statuses ~who:"main" client
    in
    let* mempool_after_baking = RPC.get_mempool client in
    check_operation_not_in_applied_mempool
      ~__LOC__
      ~explain:"after baking"
      mempool_after_baking
      oph ;
    return oph
end

(** Helper functions specific to these tests *)
module Helpers = struct
  (** Initialize a network with two nodes *)
  let init ?(event_sections_levels = [("prevalidator", `Debug)]) ~protocol () =
    let node1 = Node.create [Synchronisation_threshold 0; Connections 1] in
    let node2 = Node.create [Synchronisation_threshold 0; Connections 1] in
    let* client1 = Client.init ~endpoint:(Node node1) ()
    and* client2 = Client.init ~endpoint:(Node node2) () in
    let nodes =
      {
        main = {node = node1; client = client1};
        observer = {node = node2; client = client2};
      }
    in
    Cluster.symmetric_add_peer node1 node2 ;
    let* () = Cluster.start ~event_sections_levels [node1; node2] in
    let* () = Client.activate_protocol ~protocol client1 in
    Log.info "Activated protocol" ;
    let* _ = Node.wait_for_level node1 1 and* _ = Node.wait_for_level node2 1 in
    return nodes

  let originate_contract ?protocol ~source ~code ~init_storage ?counter ?fee
      ?gas_limit ?storage_limit nc =
    let* op =
      Operation.mk_origination
        ~source
        ?counter
        ?fee
        ?gas_limit
        ?storage_limit
        ~code
        ~init_storage
        nc.client
    in
    Operation.forge_and_inject_operation
      ?protocol
      ~batch:[op]
      ~signer:source
      nc.client
end

module Illtyped_originations = struct
  let contract_body_illtyped_1 =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's body illtyped 1"
      ~tags:["precheck"; "illtyped"; "origination"; "typing"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _oph =
      Memchecks.with_applied_checks ~__LOC__ nodes ~expected_statuses:["failed"]
      @@ fun () ->
      Helpers.originate_contract
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`O [("int", `String "0")])
        ~code:Contracts.illtyped_contract_body_1
        nodes.main
    in
    unit

  let contract_body_illtyped_2 =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's body illtyped 2"
      ~tags:["precheck"; "illtyped"; "origination"; "typing"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _oph =
      Memchecks.with_applied_checks ~__LOC__ nodes ~expected_statuses:["failed"]
      @@ fun () ->
      Helpers.originate_contract
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`O [("int", `String "0")])
        ~code:Contracts.illtyped_contract_body_2
        nodes.main
    in
    unit

  let initial_storage_illtyped =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's initial storage illtyped"
      ~tags:["precheck"; "illtyped"; "origination"; "typing"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* code =
      Client.convert_script_to_json
        nodes.main.client
        ~script:"./tezt/tests/contracts/proto_alpha/parsable_contract.tz"
    in
    let* _oph =
      Memchecks.with_applied_checks ~__LOC__ nodes ~expected_statuses:["failed"]
      @@ fun () ->
      Helpers.originate_contract
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`O [("int", `String "-10")])
        ~code
        nodes.main
    in
    unit

  let register ~protocols =
    contract_body_illtyped_1 ~protocols ;
    contract_body_illtyped_2 ~protocols ;
    initial_storage_illtyped ~protocols
end

let register ~protocols = Illtyped_originations.register ~protocols
