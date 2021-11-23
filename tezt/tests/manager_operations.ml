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
    let* () =
      Client.bake_for ~context_path:(Node.data_dir node // "context") client
      (* We need to have the client build the block without the
         /helpers/preapply/block RPC to the node because this RPC serializes the
         operations before sending them off to Block_validator.preapply.

         This is needed to expose the bug where a baker could build an invalid
         block (wrt. the context hash), if it got the operation deserialized from
         the mempool and then builds a block without accounting for the
         deserialization cost of the parameters. (This is captured by the test
         Deserialization.test_deserialization_gas_accounting.)
      *)
    in
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

  let check_operation_is_in_mempool ~__LOC__ classification ?(explain = "")
      (mempool : Mempool.t) oph =
    let (mempool_classified_ops, classification_str) =
      match classification with
      | `Applied -> (mempool.applied, "mempool.applied")
      | `Refused -> (mempool.refused, "mempool.refused")
      | `Branch_refused -> (mempool.branch_refused, "mempool.branch_refused")
      | `Branch_delayed -> (mempool.branch_delayed, "mempool.branch_delayed")
      | `Outdated -> (mempool.outdated, "mempool.outdated")
    in
    if List.mem oph mempool_classified_ops then
      Log.ok "  - %s found in %s %s." oph classification_str explain
    else
      Test.fail
        ~__LOC__
        "%s was not found in %s %s"
        oph
        classification_str
        explain

  let check_operation_not_in_applied_mempool ~__LOC__ ?(explain = "")
      (mempool : Mempool.t) oph =
    if List.mem oph mempool.Mempool.applied then
      Test.fail
        ~__LOC__
        "%s is in mempool.applied %s but should not be"
        oph
        explain
    else Log.ok "  - %s was not in mempool.applied %s." oph explain

  let get_op_status op =
    JSON.(op |-> "metadata" |-> "operation_result" |-> "status" |> as_string)

  let get_op_errors_ids op =
    let errs =
      JSON.(op |-> "metadata" |-> "operation_result" |-> "errors" |> as_list)
    in
    List.map (fun err -> JSON.(err |-> "id" |> as_string)) errs

  (** Wait for nodes to be synchronized, i.e. for node observer to be
     at the same level as main node (the baker only bakes on main node
     in these tests). *)
  let wait_sync nodes =
    let* level1_json = RPC.get_current_level nodes.main.client in
    let level_main = JSON.(level1_json |-> "level" |> as_int) in
    Node.wait_for_level nodes.observer.node level_main

  let is_in_block ?block client oph =
    let* head = RPC.get_block ?block client in
    let ops = JSON.(head |-> "operations" |=> 3 |> as_list) in
    Lwt.return
    @@ List.exists (fun op -> oph = JSON.(op |-> "hash" |> as_string)) ops

  let check_op_not_included_in_block ?block ?(explain = "") client oph =
    let* in_block = is_in_block ?block client oph in
    if in_block then Test.fail ~__LOC__ "%s found in head block %s" oph explain
    else Log.ok "  - %s not included in block %s." oph explain ;
    unit

  let check_op_not_propagated ?(explain = "") observer oph observer_result =
    Lwt_list.iter_p
      (function
        | `Notify_mempool (known_valid, pending) ->
            if List.mem oph known_valid then
              Test.fail
                "%s was propagated to observer node as valid %s"
                oph
                explain
            else if List.mem oph pending then
              Test.fail
                "%s was propagated to observer node as pending %s"
                oph
                explain
            else () ;
            return ()
        | `New_block_hash block ->
            check_op_not_included_in_block
              ?block
              observer.client
              oph
              ~explain:("of observer node " ^ explain))
      observer_result
    >|= fun () ->
    Log.ok "  - %s was not propagated to observer node %s." oph explain

  (** Check that operation whose hash is [oph] is included in the
      block [block] or the head, with the statuses [expected_statuses]
      (there can be several if the operation is a batch).

      If [expected_statuses] is empty, check that the operation is not
      included in the block.

      Also check (when argument [expected_errors] is provided) that it
      is included with these errors (all elements in each item of the list
      must be a substring of one of the errors). If one of the list of
      errors is empty, then the operation at this position in the batch must
      be included without errors.
      For instance [~expected_errors:[["ill_typed"]]] checks that there is
      only one operation in the batch and that the result {i contains} an error
      with ["ill_typed"] somewhere in its identifier.
  *)
  let check_status_in_block ~who ~oph ~expected_statuses ?expected_errors ?block
      client =
    Log.info "- Checking inclusion and status of operation in %s's block." who ;
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
    (* Check errors *)
    (match expected_errors with
    | None -> ()
    | Some expected_errors ->
        Check.((List.length op_contents = List.length expected_errors) int)
          ~error_msg:
            "expected contents to contain %R values (wrt. expected errors), \
             got %L" ;
        List.iter2
          (fun op expected_errors ->
            let errors = get_op_errors_ids op in
            match expected_errors with
            | [] ->
                if errors <> [] then
                  Test.fail
                    ~__LOC__
                    "Operation should not have any errors:\n%s"
                    (JSON.encode op)
            | _ ->
                List.iter
                  (fun expected_error ->
                    if
                      not
                      @@ List.exists (fun s -> s =~ rex expected_error) errors
                    then
                      Test.fail
                        ~__LOC__
                        "Operation is not included with error %s. Errors are \
                         [%s]"
                        expected_error
                        (String.concat ", " errors))
                  expected_errors)
          op_contents
          expected_errors) ;
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
  let with_applied_checks ~__LOC__ nodes ~expected_statuses ?expected_errors
      ?(bake = true) inject =
    Log.section "Checking applied operation." ;
    let* _ = wait_sync nodes in
    let client = nodes.main.client in
    let wait_observer = wait_for_notify nodes.observer.node in
    Log.info "- Injecting operation." ;
    let* oph = inject () in
    let* mempool_after_injection = RPC.get_mempool client in
    check_operation_is_in_mempool
      `Applied
      ~__LOC__
      ~explain:"after injection"
      mempool_after_injection
      oph ;
    Log.info "- Waiting for observer to be notified of operation." ;
    let* observer_result = wait_observer in
    Log.info "- Checking observer received operations." ;
    let (known_valid, pending) = observer_result in
    if List.mem oph known_valid then
      Log.ok "  - %s was propagated to observer node as valid." oph
    else if List.mem oph pending then
      Test.fail ~__LOC__ "%s was propagated to observer node as pending" oph ;
    let* mempool_observer = RPC.get_mempool nodes.observer.client in
    check_operation_is_in_mempool
      `Applied
      ~__LOC__
      ~explain:"in observer"
      mempool_observer
      oph ;
    if bake then (
      Log.info "- Baking (should include operation %s)." oph ;
      let* () = bake_and_wait_block nodes.main in
      let* _head_hash =
        check_status_in_block
          ~oph
          ~expected_statuses
          ?expected_errors
          ~who:"main"
          client
      in
      let* mempool_after_baking = RPC.get_mempool client in
      check_operation_not_in_applied_mempool
        ~__LOC__
        ~explain:"after baking"
        mempool_after_baking
        oph ;
      return oph)
    else return oph

  let with_refused_checks ~__LOC__ nodes inject =
    Log.section "Checking refused operation." ;
    let* _ = wait_sync nodes in
    let client = nodes.main.client in
    let wait_observer =
      wait_for_notify_or_processed_block nodes.observer.node
    in
    Log.info "- Injecting operation." ;
    let* oph = inject () in
    let* mempool_after_injection = RPC.get_mempool client in
    check_operation_is_in_mempool
      `Refused
      ~__LOC__
      mempool_after_injection
      oph
      ~explain:"after injection" ;
    check_operation_not_in_applied_mempool
      ~__LOC__
      mempool_after_injection
      oph
      ~explain:"after injection" ;
    Log.info "- Baking (should not include operation %s)." oph ;
    let* () = bake_and_wait_block nodes.main in
    Log.info "- Waiting for observer to see operation or block." ;
    let* observer_result = wait_observer in
    Log.info "- Checking mempool of main node." ;
    let* mempool_after_baking = RPC.get_mempool client in
    check_operation_not_in_applied_mempool
      ~__LOC__
      mempool_after_baking
      oph
      ~explain:"after baking" ;
    check_operation_is_in_mempool
      `Refused
      ~__LOC__
      mempool_after_baking
      oph
      ~explain:"after baking" ;
    Log.info "- Checking that observer did not observe operation." ;
    let* () = check_op_not_included_in_block client oph ~explain:"newly baked"
    and* () =
      check_op_not_propagated
        nodes.observer
        oph
        observer_result
        ~explain:"(refused)"
    in
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
    Log.info "Activated protocol." ;
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

module Deserialisation = struct
  let milligas_per_byte = 20 (* As per the protocol *)

  (** Returns the gas needed for the deserialization of an argument of
      size [size_kB] in kilobytes. *)
  let deserialization_gas ~size_kB = size_kB * milligas_per_byte

  (** Returns an hexadecimal representation of a zero byte sequence of
     size [size_kB]. *)
  let make_zero_hex ~size_kB =
    (* A hex representation for a byte sequence of n bytes is 2n long,
       so for n kB it is 2000n long *)
    String.make (size_kB * 2000) '0'

  (* Originate a contract that takes a byte sequence as argument and does nothing *)
  let originate_noop_contract nc =
    let* contract =
      Client.originate_contract
        ~wait:"none"
        ~init:"Unit"
        ~alias:"deserialization_gas"
        ~amount:Tez.zero
        ~burn_cap:Tez.one
        ~src:Constant.bootstrap1.alias
        ~prg:"parameter bytes; storage unit; code {CDR; NIL operation; PAIR}"
        nc.client
    in
    let* () = Memchecks.bake_and_wait_block nc in
    return contract

  let inject_call_with_bytes ?(source = Constant.bootstrap5) ?protocol ~contract
      ~size_kB ~gas_limit client =
    let* op =
      Operation.mk_call
        ~entrypoint:"default"
        ~arg:(`O [("bytes", `String (make_zero_hex ~size_kB))])
        ~gas_limit
        ~dest:contract
        ~source
        client
    in
    Operation.forge_and_inject_operation
      ?protocol
      ~batch:[op]
      ~signer:source
      client

  let test_deserialization_gas_canary =
    Protocol.register_test
      ~__FILE__
      ~title:
        "Smart contract call that should succeeds with the provided gas limit"
      ~tags:["precheck"; "gas"; "deserialization"; "canary"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract nodes.main in
    let size_kB = 20 in
    let min_deserialization_gas = deserialization_gas ~size_kB in
    let gas_for_the_rest = 2049 in
    (* This is specific to this contract, obtained empirically *)
    let* _ =
      Memchecks.with_applied_checks
        ~__LOC__
        nodes
        ~expected_statuses:["applied"]
      @@ fun () ->
      inject_call_with_bytes
        ~protocol
        ~contract
        ~size_kB:20
        ~gas_limit:(min_deserialization_gas + gas_for_the_rest)
        (* Enough gas to deserialize and do the application *)
        nodes.main.client
    in
    unit

  let test_not_enough_gas_deserialization =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract call with not enough gas to deserialize argument"
      ~tags:["precheck"; "gas"; "deserialization"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract nodes.main in
    let size_kB = 20 in
    let min_deserialization_gas = deserialization_gas ~size_kB in
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      inject_call_with_bytes
        ~protocol
        ~contract
        ~size_kB
        ~gas_limit:(min_deserialization_gas - 1)
        nodes.main.client
    in
    unit

  let test_deserialization_gas_accounting =
    Protocol.register_test
      ~__FILE__
      ~title:
        "Smart contract call that would succeed if we did not account \
         deserializtion gas correctly"
      ~tags:["precheck"; "gas"; "deserialization"; "lazy_expr"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract nodes.main in
    let size_kB = 20 in
    let min_deserialization_gas = deserialization_gas ~size_kB in
    let gas_for_the_rest = 2049 in
    (* This is specific to this contract, obtained empirically *)
    let* _ =
      Memchecks.with_applied_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["gas_exhausted.operation"]]
      @@ fun () ->
      inject_call_with_bytes
        ~protocol
        ~contract
        ~size_kB:20
        ~gas_limit:(min_deserialization_gas + gas_for_the_rest - 1)
        (* Enough gas to deserialize or to do the rest, but not to do both *)
        nodes.main.client
    in
    unit

  let register ~protocols =
    test_deserialization_gas_canary ~protocols ;
    test_not_enough_gas_deserialization ~protocols ;
    test_deserialization_gas_accounting ~protocols:[Alpha]
end

let register ~protocols =
  Illtyped_originations.register ~protocols ;
  Deserialisation.register ~protocols
