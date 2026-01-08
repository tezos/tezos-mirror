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

let team = Tag.layer1

(** A node with an associated client (with endpoint this node).  *)
type node_client = {node : Node.t; client : Client.t}

(** A pair of connected nodes. [main] is the node/client with which the tests interact
    directlty (for injections, baking, queries, etc.)
    [observer] acts as a node on which we observe events of the prevalidator. *)
type two_nodes = {main : node_client; observer : node_client}

(* Default transferred amounts and paid fees for the tests declared in this
   file. *)

(** Default fee (1tz) for operations injected in this module.
    This is a large enough value so that all operations should have enough
    fee to cover their gas (unless otherwise specified).
 *)
let fee = 1_000_000

(** Default amount (10tz) for operations injected in this module. *)
let amount = 10_000_000

module Log = struct
  include Log

  let section fmt =
    Log.info ~color:Log.Color.(bold ++ BG.blue ++ FG.bright_white) fmt

  let subsection fmt = Log.info ~color:Log.Color.(bold ++ FG.blue) fmt

  let ok fmt = Log.info ~color:Log.Color.FG.green fmt
end

module Events = struct
  type wait_observer_result =
    [ `New_block_hash of string option
    | `Notify_mempool of string list * string list ]

  let wait_for_injection ?signed_op node =
    let filter json =
      let ev_req = JSON.(json |-> "view" |-> "request" |> as_string_opt) in
      let ev_op =
        JSON.(json |-> "view" |-> "operation" |-> "data" |> as_string_opt)
      in
      match ev_req with
      | Some "inject" -> (
          match signed_op with
          | None -> Some ()
          | Some sop -> (
              (* remove branch field *)
              if String.length sop < 64 then
                Test.fail "signed operation too short" ;
              let sop = String.sub sop 64 (String.length sop - 64) in
              match ev_op with Some op when op = sop -> Some () | _ -> None))
      | _ -> None
    in
    let* () = Node.wait_for node "request_completed_notice.v0" filter in
    return ()

  (** Wait for a node to be notified of a mempool. *)
  let wait_for_notify node =
    let filter json =
      let open JSON in
      match
        ( json |-> "view" |-> "request" |> as_string_opt,
          json |-> "view" |-> "mempool" |-> "known_valid" |> as_list_opt,
          json |-> "view" |-> "mempool" |-> "pending" |> as_list_opt )
      with
      | Some "notify", Some [], Some [] -> None
      | Some "notify", Some known_valid, Some pending ->
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
      match json |-> "view" |-> "hash" |> as_string_opt with
      | None -> None
      | Some s when s.[0] = 'B' -> Some (Some s)
      | Some _ -> Some None
    in
    Lwt.pick
      [
        Node.wait_for node "head_increment.v0" filter;
        Node.wait_for node "branch_switch.v0" filter;
        Node.wait_for node "ignore_head.v0" filter;
      ]

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

  (** Wait for nodes to be synchronized, i.e. for node observer to be

     at the same level as main node (the baker only bakes on main node
     in these tests). *)
  let wait_sync nodes =
    let* level = Node.get_level nodes.main.node in
    Node.wait_for_level nodes.observer.node level
end

module Operation = struct
  include Operation

  (* We always inject operations in async and force mode for these tests. *)

  let inject_transfer
      ?(gas_limit =
        3520 (* We make transfers to non allocated contracts in these tests *))
      =
    inject_transfer ~gas_limit ~async:true ~force:true

  let inject_transfers =
    inject_transfers
      ~gas_limit:3520
        (* We make transfers to non allocated contracts in these tests *)
      ~async:true
      ~force:true

  let inject_contract_call = inject_contract_call ~async:true ~force:true

  let inject_public_key_revelation =
    inject_public_key_revelation ~async:true ~force:true

  let inject_origination = inject_origination ~async:true ~force:true

  let inject_transfer_ticket = inject_transfer_ticket ~async:true ~force:true

  let forge_and_inject_operation ?protocol ?branch ~batch ~signer
      ?patch_unsigned client =
    let* branch = get_injection_branch ?branch client in
    let* unsigned_op = forge_operation ?protocol ~batch ~branch client in
    let unsigned_op =
      match patch_unsigned with
      | None -> unsigned_op
      | Some patch ->
          Log.debug "Unsigned op before patching: %a." Hex.pp unsigned_op ;
          let unsigned_op = patch unsigned_op in
          Log.debug "Unsigned op after patching: %a." Hex.pp unsigned_op ;
          unsigned_op
    in
    let signature = sign_manager_op_hex ~signer unsigned_op in
    inject_operation ~async:true ~force:true ~unsigned_op ~signature client
end

(** Helper functions specific to these tests *)
module Helpers = struct
  (** Bake a block and wait for the node to switch to this head *)
  let bake_and_wait_block {client; node} =
    (* We need to have the client build the block without the
       /helpers/preapply/block RPC to the node because this RPC serializes the
       operations before sending them off to Block_validator.preapply.

       This is needed to expose the bug where a baker could build an invalid
       block (wrt. the context hash), if it got the operation deserialized from
       the mempool and then builds a block without accounting for the
       deserialization cost of the parameters. (This is captured by the test
       Deserialization.test_deserialization_gas_accounting.)
    *)
    Client.bake_for_and_wait ~context_path:(Node.data_dir node) client

  (** Initialize a network with two nodes *)
  let init ?(event_sections_levels = [("prevalidator", `Debug)]) ~protocol () =
    let args = [Node.Synchronisation_threshold 0; Connections 1] in
    let node1 = Node.create args in
    let node2 = Node.create args in
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
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    Log.info "Activated protocol" ;
    let* _ = Node.wait_for_level node2 1 in
    let* _ = Events.wait_sync nodes in
    return nodes

  (** Generate a new account (key pair) and credit it with [amount] mutez. If
      the [reveal] flag is [true], the public key of the new account in also
      revealed in a subsequent operation. A block is baked after each
      operation. *)
  let init_fresh_account ?(reveal = false) ?protocol ~amount ~fee nodes =
    let* key = Client.gen_and_show_keys nodes.main.client in
    Log.section "Initializing fresh account %s" key.public_key_hash ;
    let* _oph =
      Operation.inject_transfer
        ?protocol
        nodes.main.client
        ~source:Constant.bootstrap2
        ~dest:key
        ~gas_limit:3500
        ~amount
        ~fee
    in
    let* () = bake_and_wait_block nodes.main in
    let* () =
      if not reveal then unit
      else
        let* _oph =
          Operation.inject_public_key_revelation
            ?protocol
            nodes.main.client
            ~source:key
            ~fee
        in
        bake_and_wait_block nodes.main
    in
    Lwt.return key

  let originate_contract protocol nodes script_name =
    Log.info
      "- Auxiliary step: originate contract %s."
      Michelson_script.(find script_name protocol |> name_s) ;
    let* _alias, contract =
      Client.originate_contract_at
        ~wait:"none"
        ~init:"{}"
        ~amount:Tez.zero
        ~burn_cap:(Tez.of_int 10)
        ~src:Constant.bootstrap1.alias
        nodes.main.client
        script_name
        protocol
    in
    let* () = bake_and_wait_block nodes.main in
    Log.info "  - Contract address is %s." contract ;
    return contract

  type hard_gas_limits = {
    hard_gas_limit_per_operation : int;
    hard_gas_limit_per_block : int;
  }

  let gas_limits client =
    let* constants =
      Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
    in
    let hard_gas_limit_per_operation =
      JSON.(constants |-> "hard_gas_limit_per_operation" |> as_int)
    in
    let hard_gas_limit_per_block =
      JSON.(constants |-> "hard_gas_limit_per_block" |> as_int)
    in
    return {hard_gas_limit_per_operation; hard_gas_limit_per_block}
end

(** This module provides helper functions and wrappers to check the
    classification of operations in the mempool, their propagation and
    their inclusion in a block. *)
module Memchecks = struct
  let string_of_classification = function
    | `Validated -> "validated"
    | `Refused -> "refused"
    | `Branch_refused -> "branch_refused"
    | `Branch_delayed -> "branch_delayed"
    | `Outdated -> "outdated"
    | `Unprocessed -> "unprocessed"

  let string_of_ext_classification = function
    | `Absent -> "absent"
    | `Not c -> "not " ^ string_of_classification c
    | ( `Validated | `Refused | `Branch_refused | `Branch_delayed | `Outdated
      | `Unprocessed ) as c ->
        string_of_classification c

  let mempool_get_operations ?classification (mempool : Mempool.t) =
    match classification with
    | None ->
        List.concat
          [
            mempool.validated;
            mempool.refused;
            mempool.branch_refused;
            mempool.branch_delayed;
            mempool.outdated;
            mempool.unprocessed;
          ]
    | Some c -> (
        match c with
        | `Validated -> mempool.validated
        | `Refused -> mempool.refused
        | `Branch_refused -> mempool.branch_refused
        | `Branch_delayed -> mempool.branch_delayed
        | `Outdated -> mempool.outdated
        | `Unprocessed -> mempool.unprocessed)

  let check_operation_is_in_mempool ~__LOC__ classification ?(explain = "")
      mempool oph =
    Check.(list_mem string)
      ~__LOC__
      oph
      (mempool_get_operations ~classification mempool)
      ~error_msg:
        (sf
           "expected to find %%L in mempool.%s = %%R %s"
           (string_of_classification classification)
           explain)

  let check_operation_not_in_mempool ~__LOC__ ?classification ?(explain = "")
      mempool oph =
    let classification_str =
      match classification with
      | None -> "mempool"
      | Some c -> string_of_classification c
    in
    Check.(list_not_mem string)
      ~__LOC__
      oph
      (mempool_get_operations ?classification mempool)
      ~error_msg:
        (sf "expected %%L to not be in %s = %%R %s" classification_str explain)

  type classification =
    [ `Validated
    | `Branch_delayed
    | `Branch_refused
    | `Outdated
    | `Unprocessed
    | `Refused ]

  type extended_classification =
    [`Absent | classification | `Not of classification]

  let check_operation_classification ~__LOC__ classification ?explain mempool
      oph =
    match classification with
    | ( `Validated | `Refused | `Branch_refused | `Branch_delayed | `Outdated
      | `Unprocessed ) as classification ->
        check_operation_is_in_mempool
          ~__LOC__
          classification
          ?explain
          mempool
          oph
    | `Absent -> check_operation_not_in_mempool ~__LOC__ ?explain mempool oph
    | `Not classification ->
        check_operation_not_in_mempool
          ~__LOC__
          ?explain
          ~classification
          mempool
          oph

  let get_op_status op =
    JSON.(op |-> "metadata" |-> "operation_result" |-> "status" |> as_string)

  let get_op_errors_ids op =
    let errs =
      JSON.(op |-> "metadata" |-> "operation_result" |-> "errors" |> as_list)
    in
    List.map (fun err -> JSON.(err |-> "id" |> as_string)) errs

  let is_in_block ?block client oph =
    let* head = Client.RPC.call client @@ RPC.get_chain_block ?block () in
    let ops = JSON.(head |-> "operations" |=> 3 |> as_list) in
    Lwt.return
    @@ List.exists (fun op -> oph = JSON.(op |-> "hash" |> as_string)) ops

  let check_op_in_block ~__LOC__ ?block ?(explain = "") ~should_include client
      oph =
    let* in_block = is_in_block ?block client oph in
    if (not in_block) && should_include then
      Test.fail ~__LOC__ "%s not included in block %s." oph explain ;
    if in_block && not should_include then
      Test.fail ~__LOC__ "%s found in head block %s" oph explain ;
    unit

  let check_op_not_propagated ~__LOC__ ?(explain = "") ~should_include observer
      oph observer_result =
    Lwt_list.iter_p
      (function
        | `Notify_mempool (known_valid, pending) ->
            if List.mem oph known_valid then
              Test.fail
                ~__LOC__
                "%s was propagated to observer node as valid %s"
                oph
                explain
            else if List.mem oph pending then
              Test.fail
                ~__LOC__
                "%s was propagated to observer node as pending %s"
                oph
                explain
            else () ;
            return ()
        | `New_block_hash block ->
            check_op_in_block
              ~should_include
              ~__LOC__
              ?block
              observer.client
              oph
              ~explain:("of observer node " ^ explain))
      observer_result
    >|= fun () ->
    Log.ok "  - %s was not propagated to observer node %s." oph explain

  (** Check that errors appear in the list of errors for an operation included
      in a block, where [op_content] is the operation and associated metadata as
      found in a block (in JSON).

      If [expected_errors] is [None], don't check the errors. If
      [expected_errors = Some []], ensures there are no errors.  Otherwise all
      the specified errors must appear (in any order) as substrings of the errors
      associated to this opeation in the block's metadata. *)
  let check_op_errors ~__LOC__ ~op_contents ~expected_errors =
    match expected_errors with
    | None -> ()
    | Some expected_errors ->
        Check.((List.length op_contents = List.length expected_errors) int)
          ~error_msg:
            "expected content length = expected error count (%R), but got %L" ;
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
          expected_errors

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
  let check_status_in_block ~__LOC__ ~who ~oph ~expected_statuses
      ?expected_errors ?block client =
    Log.info "- Checking inclusion and status of operation in %s's block." who ;
    let* head = Client.RPC.call client @@ RPC.get_chain_block ?block () in
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
    check_op_errors ~__LOC__ ~op_contents ~expected_errors ;
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
  let with_validated_checks ~__LOC__ nodes ~expected_statuses ?expected_errors
      ?(bake = true) ?(observer_classification = `Validated) inject =
    Log.subsection "Checking validated operation" ;
    let* _ = Events.wait_sync nodes in
    let client = nodes.main.client in
    let wait_observer = Events.wait_for_notify nodes.observer.node in
    Log.info "- Injecting operation." ;
    let* (`OpHash oph) = inject () in
    let* mempool_after_injection = Mempool.get_mempool client in
    check_operation_is_in_mempool
      `Validated
      ~__LOC__
      ~explain:"after injection"
      mempool_after_injection
      oph ;
    Log.info "- Waiting for observer to be notified of operation." ;
    let* observer_result = wait_observer in
    Log.info "- Checking observer received operations." ;
    let known_valid, pending = observer_result in
    if List.mem oph known_valid then
      Log.ok "  - %s was propagated to observer node as valid." oph
    else if List.mem oph pending then
      Test.fail ~__LOC__ "%s was propagated to observer node as pending" oph ;
    let* mempool_observer = Mempool.get_mempool nodes.observer.client in
    let check_observer_mempool =
      match observer_classification with
      | ( `Validated | `Refused | `Branch_refused | `Branch_delayed | `Outdated
        | `Unprocessed ) as classification ->
          check_operation_is_in_mempool classification
      | `Absent -> check_operation_not_in_mempool ?classification:None
    in
    check_observer_mempool ~__LOC__ ~explain:"in observer" mempool_observer oph ;
    if bake then (
      Log.info "- Baking (should include operation %s)." oph ;
      let* () = Helpers.bake_and_wait_block nodes.main in
      let* _head_hash =
        check_status_in_block
          ~__LOC__
          ~oph
          ~expected_statuses
          ?expected_errors
          ~who:"main"
          client
      in
      let* mempool_after_baking = Mempool.get_mempool client in
      check_operation_not_in_mempool
        ~__LOC__
        ~classification:`Validated
        ~explain:"after baking"
        mempool_after_baking
        oph ;
      return oph)
    else return oph

  let with_checks ~__LOC__ ?(bake = true) ~classification
      ?(classification_after_flush = classification) ~should_propagate
      ?(should_include = should_propagate) nodes inject =
    Log.subsection
      "Checking %s operation"
      (string_of_ext_classification classification) ;
    let* _ = Events.wait_sync nodes in
    let client = nodes.main.client in
    let wait_observer =
      Events.wait_for_notify_or_processed_block nodes.observer.node
    in
    Log.info "- Injecting operation." ;
    let* (`OpHash oph) = inject () in
    let* mempool_after_injection = Mempool.get_mempool client in
    check_operation_classification
      classification
      ~__LOC__
      mempool_after_injection
      oph
      ~explain:"after injection" ;
    Log.info
      "- Baking (should%s include operation %s)."
      (if should_include then "" else " not")
      oph ;
    if not bake then return oph
    else
      let* () = Helpers.bake_and_wait_block nodes.main in
      Log.info "- Waiting for observer to see operation or block." ;
      let* observer_result = wait_observer in
      Log.info "- Checking mempool of main node." ;
      let* mempool_after_baking = Mempool.get_mempool client in
      check_operation_classification
        classification_after_flush
        ~__LOC__
        mempool_after_baking
        oph
        ~explain:"after baking" ;
      Log.info "- Checking that observer did not observe operation." ;
      let* () =
        check_op_in_block
          ~__LOC__
          client
          oph
          ~should_include
          ~explain:"newly baked"
      and* () =
        check_op_not_propagated
          ~__LOC__
          nodes.observer
          oph
          observer_result
          ~should_include
          ~explain:(string_of_ext_classification classification)
      in
      return oph

  let with_refused_checks =
    with_checks ~classification:`Refused ~should_propagate:false

  let with_branch_refused_checks =
    with_checks ~classification:`Branch_refused ~should_propagate:false

  let with_branch_delayed_checks =
    with_checks ~classification:`Branch_delayed ~should_propagate:false

  let with_absent_checks =
    with_checks ~classification:`Absent ~should_propagate:false

  let check_balance ~__LOC__ {client; _} key amount =
    let* bal =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:key.Account.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    if bal <> amount then
      Test.fail
        ~__LOC__
        "Unexpected balance. Got %d instead of %d mutez"
        bal
        amount ;
    unit

  let check_revealed ~__LOC__ {client; _} key ~revealed =
    let* res =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_manager_key
           ~id:key.Account.public_key_hash
           ()
    in
    let is_revealed = not (JSON.is_null res) in
    if is_revealed && JSON.as_string res <> key.public_key then
      Test.fail
        ~__LOC__
        "Bad revealed public key: %s but should be %s"
        (JSON.as_string res)
        key.public_key ;
    if is_revealed == revealed then
      Log.ok
        "  - Public key is %srevealed."
        (if is_revealed then "" else "not ")
    else
      Test.fail
        ~__LOC__
        "should_be_revealed = %b but is_revealed = %b"
        revealed
        is_revealed ;
    unit
end
(* of Memchecks*)

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

module Illtyped_originations = struct
  let contract_body_illtyped_1 =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's body illtyped 1"
      ~tags:[team; "precheck"; "illtyped"; "origination"; "typing"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _oph =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["ill_typed_contract"; "bad_return"]]
      @@ fun () ->
      Operation.inject_origination
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`Json (`O [("int", `String "0")]))
        ~code:(`Json Contracts.illtyped_contract_body_1)
        nodes.main.client
    in
    unit

  let contract_body_illtyped_2 =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's body illtyped 2"
      ~tags:[team; "precheck"; "illtyped"; "origination"; "typing"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _oph =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["ill_typed_contract"; "bad_return"]]
      @@ fun () ->
      Operation.inject_origination
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`Json (`O [("int", `String "0")]))
        ~code:(`Json Contracts.illtyped_contract_body_2)
        nodes.main.client
    in
    unit

  let initial_storage_illtyped =
    Protocol.register_test
      ~__FILE__
      ~title:"Contract's initial storage illtyped"
      ~tags:[team; "precheck"; "illtyped"; "origination"; "typing"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _oph =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["ill_typed_data"; "invalid_constant"]]
      @@ fun () ->
      Operation.inject_origination
        ~protocol
        ~source:Constant.bootstrap1
        ~init_storage:(`Json (`O [("int", `String "-10")]))
        ~code:
          (`File
             Michelson_script.(
               find ["mini_scenarios"; "parsable_contract"] protocol |> path))
        nodes.main.client
    in
    unit

  let register ~protocols =
    contract_body_illtyped_1 protocols ;
    contract_body_illtyped_2 protocols ;
    initial_storage_illtyped protocols
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
  let originate_noop_contract protocol nc =
    let* _alias, contract =
      Client.originate_contract_at
        ~wait:"none"
        ~init:"Unit"
        ~amount:Tez.zero
        ~burn_cap:Tez.one
        ~src:Constant.bootstrap1.alias
        nc.client
        ["mini_scenarios"; "noop_bytes"]
        protocol
    in
    let* () = Helpers.bake_and_wait_block nc in
    return contract

  (* Gas to execute call to noop contract without deserialization *)
  let gas_to_execute_rest_noop (_ : Protocol.t) = 1941

  let inject_call_with_bytes ?(source = Constant.bootstrap5) ?protocol ~contract
      ~size_kB ~gas_limit client =
    let* op =
      Operation.mk_call
        ~entrypoint:"default"
        ~arg:(`Json (`O [("bytes", `String (make_zero_hex ~size_kB))]))
        ~gas_limit
        ~dest:contract
        ~source
        client
    in
    Operation.forge_and_inject_operation
      ?protocol
      ~batch:(`Manager [op])
      ~signer:source
      client

  let test_deserialization_gas_canary =
    Protocol.register_test
      ~__FILE__
      ~title:
        "Smart contract call that should succeeds with the provided gas limit"
      ~tags:[team; "precheck"; "gas"; "deserialization"; "canary"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract protocol nodes.main in
    let size_kB = 20 in
    let min_deserialization_gas = deserialization_gas ~size_kB in
    let gas_for_the_rest = gas_to_execute_rest_noop protocol in
    (* This is specific to this contract, obtained empirically *)
    let* _ =
      Memchecks.with_validated_checks
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
      ~supports:(Protocol.From_protocol 14)
      ~tags:[team; "precheck"; "gas"; "deserialization"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract protocol nodes.main in
    let size_kB = 20 in
    let min_deserialization_gas =
      Constant.manager_operation_gas_cost ~protocol
      + deserialization_gas ~size_kB
    in
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
         deserialization gas correctly"
      ~tags:[team; "precheck"; "gas"; "deserialization"; "lazy_expr"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract = originate_noop_contract protocol nodes.main in
    let size_kB = 21 in
    let min_deserialization_gas = deserialization_gas ~size_kB in
    let gas_for_the_rest = gas_to_execute_rest_noop protocol in
    (* This is specific to this contract, obtained empirically *)
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["gas_exhausted.operation"]]
      @@ fun () ->
      inject_call_with_bytes
        ~protocol
        ~contract
        ~size_kB
        ~gas_limit:(min_deserialization_gas + gas_for_the_rest - 1)
        (* Enough gas to deserialize or to do the rest, but not to do both *)
        nodes.main.client
    in
    unit

  let register ~protocols =
    test_deserialization_gas_canary protocols ;
    test_not_enough_gas_deserialization protocols ;
    test_deserialization_gas_accounting protocols
end

module Gas_limits = struct
  (** Build a batch of transfers with specific gas limit for every one of
      them. *)
  let mk_batch ?(source = Constant.bootstrap2) ?(dest = Constant.bootstrap3)
      ~operations_gas_limit client =
    let open Operation.Manager in
    let fee = 1_000_000 in
    let* counter = get_next_counter client ~source:Constant.bootstrap1 in
    List.mapi
      (fun i gas_limit ->
        let counter = counter + i in
        let payload = transfer ~dest () in
        make ~source ~fee ~gas_limit ~counter payload)
      operations_gas_limit
    |> return

  let block_below_ops_below =
    Protocol.register_test
      ~__FILE__
      ~title:"Batch below block limit with each operation below limit"
      ~tags:[team; "precheck"; "batch"; "gas"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* limits = Helpers.gas_limits nodes.main.client in
    let gas_limit =
      limits.hard_gas_limit_per_block - limits.hard_gas_limit_per_operation
    in
    (* Gas limit per op is ok *)
    let operations_gas_limit, expected_statuses =
      if Protocol.(number protocol <= 023) then
        ( [limits.hard_gas_limit_per_operation; gas_limit],
          ["applied"; "applied"] )
      else
        (* with 6s block time, [hard_gas_limit_per_operation] =
           [hard_gas_limit_per_block] *)
        ([limits.hard_gas_limit_per_operation], ["applied"])
    in
    let* batch = mk_batch ~operations_gas_limit nodes.main.client in
    let* _oph =
      Memchecks.with_validated_checks ~__LOC__ nodes ~expected_statuses
      @@ fun () -> Operation.Manager.inject batch nodes.main.client
    in
    unit

  let block_below_ops_over =
    Protocol.register_test
      ~__FILE__
      ~title:"Batch below block limit with operations over limit"
      ~tags:[team; "precheck"; "batch"; "gas"; "op_gas"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* limits = Helpers.gas_limits nodes.main.client in
    let gas_limit =
      limits.hard_gas_limit_per_block - limits.hard_gas_limit_per_operation
    in
    let operations_gas_limit =
      if Protocol.(number protocol <= 023) then
        [limits.hard_gas_limit_per_operation + 1; gas_limit - 2]
      else
        (* with 6s block time, [hard_gas_limit_per_operation] =
           [hard_gas_limit_per_block] *)
        [limits.hard_gas_limit_per_operation + 1]
    in
    let* batch = mk_batch ~operations_gas_limit nodes.main.client in
    let* _oph =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      (* Gas limit per op is too high *)
      Operation.Manager.inject ~force:true batch nodes.main.client
    in
    unit

  let block_over_ops_below =
    Protocol.register_test
      ~__FILE__
      ~title:"Batch over block limit with operations below limit"
      ~tags:[team; "precheck"; "batch"; "gas"; "block_gas"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* limits = Helpers.gas_limits nodes.main.client in
    (* Gas limit per block is too high *)
    let too_many_ops =
      (limits.hard_gas_limit_per_block / limits.hard_gas_limit_per_operation)
      + 1
    in
    let operations_gas_limit =
      List.init too_many_ops (fun _i -> limits.hard_gas_limit_per_operation)
    in
    let* batch = mk_batch ~operations_gas_limit nodes.main.client in
    let* _oph =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.Manager.inject ~force:true batch nodes.main.client
    in
    unit

  let register ~protocols =
    block_below_ops_below protocols ;
    block_below_ops_over protocols ;
    block_over_ops_below protocols
end

module Reveal = struct
  (* This auxiliary function forges and injects a batched operation
     made of two revelations pk1 and pk2. The transaction is signed by
     the given key. *)
  let mk_reveal_twice {client; _} key pk1 pk2 =
    let* cpt = Operation.get_counter client ~source:key in
    let s1 = {key with Account.public_key = pk1} in
    let s2 = {key with Account.public_key = pk2} in
    let* op1 = Operation.mk_reveal ~source:s1 ~counter:(cpt + 1) ~fee client in
    let* op2 = Operation.mk_reveal ~source:s2 ~counter:(cpt + 2) ~fee client in
    Operation.forge_and_inject_operation
      ~batch:(`Manager [op1; op2])
      ~signer:key
      client

  let simple_reveal_bad_pk =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple revelation with a wrong public key"
      ~tags:[team; "reveal"; "revelation"; "batch"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    let* key_to_reveal = Client.gen_and_show_keys nodes.main.client in
    Log.section "Make the revelation" ;
    let* _oph =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_public_key_revelation
        ~protocol
        ~source:key
        ~public_key:
          key_to_reveal.public_key (* key_to_reveal is different from key *)
        ~fee
        nodes.main.client
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key amount in
    Memchecks.check_revealed ~__LOC__ nodes.main key ~revealed:false

  let simple_reveal_not_a_pk =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple revelation with something that is not a public key"
      ~tags:[team; "reveal"; "revelation"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    Log.section "Make the revelation" ;
    let* op = Operation.mk_reveal ~source:key ~fee nodes.main.client in
    let patch_unsigned (`Hex op) =
      (* public key is in the last field, add extra byte *)
      `Hex (op ^ "00")
    in
    let* _oph =
      Memchecks.with_absent_checks ~__LOC__ nodes @@ fun () ->
      Operation.forge_and_inject_operation
        ~protocol
        ~batch:(`Manager [op])
        ~signer:key
        ~patch_unsigned
        nodes.main.client
    in
    unit

  let revealed_twice_in_batch =
    Protocol.register_test
      ~__FILE__
      ~title:"Correct public key revealed twice in a batch"
      ~tags:[team; "reveal"; "revelation"; "batch"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
      ~supports:(Protocol.From_protocol 14)
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    Log.section "Make the revelation" ;
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      mk_reveal_twice nodes.main key key.public_key key.public_key
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key amount in
    Memchecks.check_revealed ~__LOC__ nodes.main key ~revealed:false

  (* After the work in !5182, which enforces that reveal operations
     can only be placed at the head of the batch, this test should
     fail with a permanent, Apply.Incorrect_reveal_position error (see
     #2774). *)
  let revealed_twice_in_batch_bad_first_key =
    Protocol.register_test
      ~__FILE__
      ~title:"Two reveals in a batch. First key is wrong"
      ~tags:[team; "reveal"; "revelation"; "batch"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    Log.section "Make the revelation" ;
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      mk_reveal_twice
        nodes.main
        key
        Constant.bootstrap1.public_key
        key.public_key
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key amount in
    Memchecks.check_revealed ~__LOC__ nodes.main key ~revealed:false

  let revealed_twice_in_batch_bad_second_key =
    Protocol.register_test
      ~__FILE__
      ~title:"Two reveals in a batch. Second key is wrong"
      ~tags:[team; "reveal"; "revelation"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
      ~supports:(Protocol.From_protocol 14)
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    Log.section "Make the revelation" ;
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      mk_reveal_twice
        nodes.main
        key
        key.public_key
        Constant.bootstrap1.public_key
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key amount in
    Memchecks.check_revealed ~__LOC__ nodes.main key ~revealed:false

  let register ~protocols =
    simple_reveal_bad_pk protocols ;
    simple_reveal_not_a_pk protocols ;
    revealed_twice_in_batch protocols ;
    revealed_twice_in_batch_bad_first_key protocols ;
    revealed_twice_in_batch_bad_second_key protocols
end

module Simple_transfers = struct
  let test_simple_transfer_applied =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer applied"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key = Helpers.init_fresh_account ~protocol nodes ~amount ~fee in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key amount in
    Memchecks.check_revealed ~__LOC__ nodes.main key ~revealed:false

  let test_simple_transfer_low_balance_to_pay_fees =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer not enough balance to pay fees"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* bal =
      Client.RPC.call nodes.main.client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    let* _ =
      Memchecks.with_branch_delayed_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee:((2 * bal) + 1) (* Too high fee *)
        ~amount:1
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main Constant.bootstrap2 bal
    in
    Memchecks.check_revealed
      ~__LOC__
      nodes.main
      Constant.bootstrap2
      ~revealed:true

  let test_simple_transfer_low_balance_to_make_transfer =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer not enough balance to make transfer"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* bal =
      Client.RPC.call nodes.main.client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["balance_too_low"]]
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee:(bal - 1) (* fee and amount too large: cannot pay [fee + amount] *)
        ~amount:(bal / 2)
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main Constant.bootstrap2 1
    in
    Memchecks.check_revealed
      ~__LOC__
      nodes.main
      Constant.bootstrap2
      ~revealed:true

  let test_simple_transfer_counter_in_the_past =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer counter in the past"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* counter =
      Operation.get_counter nodes.main.client ~source:Constant.bootstrap2
    in
    let* bal =
      Client.RPC.call nodes.main.client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    let* _ =
      Memchecks.with_branch_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee:(fee + 1)
        ~amount:(bal - fee)
        ~counter (* Specifying existing counter: wrong *)
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main Constant.bootstrap2 bal
    in
    Memchecks.check_revealed
      ~__LOC__
      nodes.main
      Constant.bootstrap2
      ~revealed:true

  let test_simple_transfer_counter_in_the_future =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer counter in the future"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* counter =
      Operation.get_counter nodes.main.client ~source:Constant.bootstrap2
    in
    let* bal =
      Client.RPC.call nodes.main.client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    let* _ =
      Memchecks.with_branch_delayed_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee:(fee + 1)
        ~amount:(bal - fee)
        ~counter:(counter + 5)
          (* Counter too large (aka "in the future"): wrong *)
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main Constant.bootstrap2 bal
    in
    Memchecks.check_revealed
      ~__LOC__
      nodes.main
      Constant.bootstrap2
      ~revealed:true

  let test_simple_transfer_wrong_signature =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer with wrong signature"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* bal =
      Client.RPC.call nodes.main.client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap2.public_key_hash
           ()
    in
    let bal = Tez.to_mutez bal in
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~signer:Constant.bootstrap3 (* signer is different from source *)
        ~amount
        ~fee
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main Constant.bootstrap2 bal
    in
    Memchecks.check_revealed
      ~__LOC__
      nodes.main
      Constant.bootstrap2
      ~revealed:true

  let test_simple_transfer_not_enough_gas =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer with not enough gas"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
      ~supports:(Protocol.From_protocol 14)
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee
        ~amount
        nodes.main.client
        ~gas_limit:1
      (* Gas too small *)
    in
    unit

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2077
     Once this issue is fixed change the test to check that the operation is refused
     and not propagated.

     Note: At the moment, the pre-filter (hence pre-check) of the mempool is not
     called for operations injected to the node directly (but rather for the
     ones that are received from another node) otherwise these would have been
     classified as "rejected".
  *)
  let test_simple_transfer_not_enough_fees_for_gas =
    Protocol.register_test
      ~__FILE__
      ~title:"Simple transfer with not enough fees to cover gas"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:[]
        ~observer_classification:`Refused
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~fee:150
        ~amount
        nodes.main.client
    in
    unit

  let test_simple_transfer_low_balance_to_pay_allocation_1 =
    Protocol.register_test
      ~__FILE__
      ~title:"Test simple transfer with low balance to pay allocation (1)"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key1 =
      Helpers.init_fresh_account ~protocol ~reveal:true nodes ~amount ~fee
    in
    let* key2 = Client.gen_and_show_keys nodes.main.client in
    let balance = amount - fee in
    (* subtract fees payed for revelation *)
    let to_transfer = balance - fee - 1 in
    (* In theory, if the operation succeeds, there will remain 1 mutez on the account *)
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["backtracked"]
        ~expected_errors:[["contract.cannot_pay_storage_fee"]]
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:key1
        ~dest:key2
        ~gas_limit:3500
        ~fee
        ~amount:to_transfer
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main key1 (balance - fee)
    in
    let* () =
      Memchecks.check_revealed ~__LOC__ nodes.main key1 ~revealed:true
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key2 0 in
    Memchecks.check_revealed ~__LOC__ nodes.main key2 ~revealed:false

  let test_simple_transfer_low_balance_to_pay_allocation_2 =
    Protocol.register_test
      ~__FILE__
      ~title:"Test simple transfer with low balance to pay allocation (2)"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key1 =
      Helpers.init_fresh_account ~protocol ~reveal:true nodes ~amount ~fee
    in
    let* key2 = Client.gen_and_show_keys nodes.main.client in
    let balance = amount - fee in
    (* subtract revelation fees *)
    let to_transfer = balance - fee in
    (* In theory, if the operation succeeds, there will remain 0 mutez on the account *)
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["backtracked"]
        ~expected_errors:[["contract.cannot_pay_storage_fee"]]
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:key1
        ~dest:key2
        ~gas_limit:3900
        ~fee
        ~amount:to_transfer
        nodes.main.client
    in
    let* () =
      Memchecks.check_balance ~__LOC__ nodes.main key1 (balance - fee)
    in
    let* () =
      Memchecks.check_revealed ~__LOC__ nodes.main key1 ~revealed:true
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key2 0 in
    Memchecks.check_revealed ~__LOC__ nodes.main key2 ~revealed:false

  let test_simple_transfer_of_the_whole_balance =
    Protocol.register_test
      ~__FILE__
      ~title:"Test simple transfer of the whole balance"
      ~tags:[team; "transaction"; "transfer"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* key1 =
      Helpers.init_fresh_account ~protocol ~reveal:true nodes ~amount ~fee
    in
    let balance = amount - fee in
    (* subtract revelation fees *)
    let to_transfer = balance - fee in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["applied"]
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:key1
        ~dest:Constant.bootstrap2
        ~gas_limit:3900
        ~fee
        ~amount:to_transfer
        nodes.main.client
    in
    let* () = Memchecks.check_balance ~__LOC__ nodes.main key1 0 in
    let* () =
      Memchecks.check_revealed ~__LOC__ nodes.main key1 ~revealed:false
    in
    unit

  let test_simple_transfers_successive_wrong_counters =
    Protocol.register_test
      ~__FILE__
      ~title:"Test succesive injections with same manager"
      ~supports:(Protocol.From_protocol 14)
      ~tags:[team; "transaction"; "transfer"; "counters"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* counter =
      Operation.get_counter nodes.main.client ~source:Constant.bootstrap2
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:[]
        ~bake:false
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~counter:(counter + 1) (* Reuse counter: wrong *)
        ~amount:1
        nodes.main.client
    in
    let* _ =
      Memchecks.with_branch_delayed_checks
        ~__LOC__
        nodes
        ~classification_after_flush:`Branch_refused
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~counter:(counter + 1)
        ~amount:2
        nodes.main.client
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:[]
        ~bake:false
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~counter:(counter + 2)
        ~amount:1
        nodes.main.client
    in
    let* _ =
      Memchecks.with_branch_delayed_checks
        ~__LOC__ (* ~classification_after_flush:`Branch_delayed *)
        ~classification_after_flush:`Validated
        ~should_include:false (* validated after flush *)
        nodes
      @@ fun () ->
      Operation.inject_transfer
        ~protocol
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap3
        ~counter:(counter + 3)
        ~amount:2
        nodes.main.client
    in
    unit

  let test_batch_simple_transfers_wrong_counters =
    Protocol.register_test
      ~__FILE__
      ~title:"Test batch with wrong counters (+1, +2, +2)"
      ~tags:[team; "transaction"; "transfer"; "counters"; "batch"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* counter =
      Operation.get_counter nodes.main.client ~source:Constant.bootstrap2
    in
    let make_transfer ~counter =
      Operation.Manager.(
        make ~source:Constant.bootstrap2 ~counter
        @@ transfer ~dest:Constant.bootstrap3 ())
    in
    let op1 = make_transfer ~counter:(counter + 1) in
    let op2 = make_transfer ~counter:(counter + 2) in
    let op3 = op2 in
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.Manager.inject ~force:true [op1; op2; op3] nodes.main.client
    in
    unit

  let test_batch_simple_transfers_wrong_counters_2 =
    Protocol.register_test
      ~__FILE__
      ~title:"Test batch with wrong counters (+1, +2, +4)"
      ~tags:[team; "transaction"; "transfer"; "counters"; "batch"]
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* counter =
      Operation.get_counter nodes.main.client ~source:Constant.bootstrap2
    in
    let make_transfer ~counter =
      Operation.Manager.(
        make ~source:Constant.bootstrap2 ~counter
        @@ transfer ~dest:Constant.bootstrap3 ())
    in
    let op1 = make_transfer ~counter:(counter + 1) in
    let op2 = make_transfer ~counter:(counter + 2) in
    let op3 = make_transfer ~counter:(counter + 4) in
    let* _ =
      Memchecks.with_refused_checks ~__LOC__ nodes @@ fun () ->
      Operation.Manager.inject ~force:true [op1; op2; op3] nodes.main.client
    in
    unit

  let register ~protocols =
    test_simple_transfer_applied protocols ;
    test_simple_transfer_low_balance_to_pay_fees protocols ;
    test_simple_transfer_low_balance_to_make_transfer protocols ;
    test_simple_transfer_counter_in_the_past protocols ;
    test_simple_transfer_counter_in_the_future protocols ;
    test_simple_transfer_wrong_signature protocols ;
    test_simple_transfer_not_enough_gas protocols ;
    test_simple_transfer_not_enough_fees_for_gas protocols ;
    test_simple_transfer_low_balance_to_pay_allocation_1 protocols ;
    test_simple_transfer_low_balance_to_pay_allocation_2 protocols ;
    test_simple_transfer_of_the_whole_balance protocols ;
    test_simple_transfers_successive_wrong_counters protocols ;
    test_batch_simple_transfers_wrong_counters protocols ;
    test_batch_simple_transfers_wrong_counters_2 protocols
end

module Simple_contract_calls = struct
  let sucessful_smart_contract_call =
    Protocol.register_test
      ~__FILE__
      ~title:"Successful smart contract call"
      ~tags:[team; "simple_contract_calls"; "smart"; "contract"; "call"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract =
      Helpers.originate_contract
        protocol
        nodes
        ["mini_scenarios"; "parsable_contract"]
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["applied"]
      @@ fun () ->
      Operation.inject_contract_call
        ~protocol
        ~entrypoint:"default"
        ~arg:(`Michelson "76")
        ~dest:contract
        ~source:Constant.bootstrap1
        nodes.main.client
    in
    unit

  let call_with_illtyped_argument =
    Protocol.register_test
      ~__FILE__
      ~title:"Smart contract call with illtyped argument"
      ~tags:[team; "simple_contract_calls"; "smart"; "contract"; "call"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract =
      Helpers.originate_contract
        protocol
        nodes
        ["mini_scenarios"; "parsable_contract"]
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["bad_contract_parameter"; "invalid_constant"]]
      @@ fun () ->
      Operation.inject_contract_call
        ~protocol
        ~entrypoint:"default"
        ~arg:(`Michelson "Unit")
        ~dest:contract
        ~source:Constant.bootstrap1
        nodes.main.client
    in
    unit

  let test_contract_call_with_failwith =
    Protocol.register_test
      ~__FILE__
      ~title:"Smart contract call that throws a failwith"
      ~tags:[team; "simple_contract_calls"; "smart"; "contract"; "call"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract =
      Helpers.originate_contract
        protocol
        nodes
        ["mini_scenarios"; "parsable_contract"]
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["runtime_error"; "script_rejected"]]
      @@ fun () ->
      Operation.inject_contract_call
        ~protocol
        ~entrypoint:"default"
        ~arg:(`Michelson "-33")
        ~dest:contract
        ~source:Constant.bootstrap1
        nodes.main.client
    in
    unit

  let test_contract_call_with_loop_gas_exhaution =
    Protocol.register_test
      ~__FILE__
      ~title:
        "Smart contract call that loops/fails with 'not enough gas' at exec"
      ~tags:[team; "simple_contract_calls"; "smart"; "contract"; "call"]
      ~uses:(fun _protocol -> [Constant.octez_codec])
    @@ fun protocol ->
    let* nodes = Helpers.init ~protocol () in
    let* contract =
      Helpers.originate_contract
        protocol
        nodes
        ["mini_scenarios"; "parsable_contract"]
    in
    let* _ =
      Memchecks.with_validated_checks
        ~__LOC__
        nodes
        ~expected_statuses:["failed"]
        ~expected_errors:[["gas_exhausted.operation"]]
      @@ fun () ->
      Operation.inject_contract_call
        ~protocol
        ~entrypoint:"default"
        ~arg:(`Michelson "0")
        ~dest:contract
        ~source:Constant.bootstrap1
        nodes.main.client
    in
    unit

  let register ~protocols =
    sucessful_smart_contract_call protocols ;
    call_with_illtyped_argument protocols ;
    test_contract_call_with_failwith protocols ;
    test_contract_call_with_loop_gas_exhaution protocols
end

let register ~protocols =
  Illtyped_originations.register ~protocols ;
  Deserialisation.register ~protocols ;
  Gas_limits.register ~protocols ;
  Reveal.register ~protocols ;
  Simple_transfers.register ~protocols ;
  Simple_contract_calls.register ~protocols
