(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Tx_rollup_node
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup_l2_node.ml
   Subject:      Various test scenarios for the Tx rollup node
*)

module Rollup = Rollup.Tx_rollup
module Parameters = Rollup.Parameters

let check_json =
  Check.equalable
    (fun ppf j -> Format.pp_print_string ppf (JSON.encode j))
    (fun a b -> JSON.unannotate a = JSON.unannotate b)

let get_block_hash block_json = JSON.(block_json |-> "hash" |> as_string)

let wait_tezos_node_level tx_node node =
  let level = Node.get_level node in
  Tx_rollup_node.wait_for_tezos_level tx_node level

(* Wait for the rollup node to be notified of a new tezos block *)
let wait_for_notified_block node =
  Tx_rollup_node.wait_for node "tx_rollup_node_new_block.v0" (fun _ -> Some ())

(* Wait for the [batch_success] event from the rollup node batcher. *)
let wait_for_batch_success_event node =
  Tx_rollup_node.wait_for node "batch_success.v0" (fun _ -> Some ())

(* Wait for the [injecting_pending] event from the injector. *)
let wait_for_injecting_event ?(tags = []) ?count node =
  Tx_rollup_node.wait_for node "injecting_pending.v0" @@ fun json ->
  let event_tags = JSON.(json |-> "tags" |> as_list |> List.map as_string) in
  let event_count = JSON.(json |-> "count" |> as_int) in
  match count with
  | Some c when c <> event_count -> None
  | _ ->
      if List.for_all (fun t -> List.mem t event_tags) tags then
        Some event_count
      else None

(* Wait for the [request_completed] event from the injector. *)
let wait_for_request_completed ?(tags = []) node request =
  Tx_rollup_node.wait_for node "request_completed_notice.v0" @@ fun json ->
  let event_request = JSON.(json |-> "view" |-> "request" |> as_string) in
  if request <> event_request then None
  else
    let event_tags = JSON.(json |-> "tags" |> as_list |> List.map as_string) in
    if List.for_all (fun t -> List.mem t event_tags) tags then Some () else None

(* Check that all messages in the inbox have been successfully applied. *)
let check_inbox_success (inbox : Tx_rollup_node.Inbox.t) =
  let ( |->? ) json field =
    let res = JSON.(json |-> field) in
    match JSON.unannotate res with `Null -> None | _ -> Some res
  in
  List.iteri
    (fun i msg ->
      let result =
        (* Pair of result and withdraws *)
        JSON.(msg.Tx_rollup_node.Inbox.result |=> 0)
      in
      match result |->? "deposit_result" with
      | None ->
          (* Not a deposit, must be a batch *)
          let batch_result =
            match result |->? "batch_v1_result" with
            | Some r -> r
            | None -> JSON.(result |-> "batch_v2_result")
          in
          let results = JSON.(batch_result |-> "results" |> as_list) in
          List.iteri
            (fun j tr_json ->
              match JSON.(tr_json |=> 1 |> as_string_opt) with
              | Some "transaction_success" -> (* OK *) ()
              | _ ->
                  Test.fail
                    "Transaction at position %d of batch %d failed: %s"
                    j
                    i
                    (JSON.encode tr_json))
            results
      | Some result -> (
          match result |->? "deposit_success" with
          | Some _ -> (* OK *) ()
          | None ->
              Test.fail
                "Deposit at position %d failed: %s"
                i
                (JSON.encode result)))
    inbox

(** Helper function to check if the tx_node does an injection with
    after executing [f].  *)
let check_injection ?(timeout = 5.0) tx_node tag f =
  let wait_injected =
    let* count = wait_for_injecting_event ~tags:[tag] tx_node in
    return (`Injected count)
  in
  let wait_timeout =
    let* () = Lwt_unix.sleep timeout in
    return `Timeout
  in
  let* () = f in
  let* injected = Lwt.choose [wait_injected; wait_timeout] in
  match injected with
  | `Injected count ->
      Log.info "Injected %d %ss" count tag ;
      unit
  | `Timeout -> Test.fail "No %s injected after %f" tag timeout

let check_l1_block_contains ~kind ~what ?(extra = fun _ -> true) block =
  let ops = JSON.(block |-> "operations" |=> 3 |> as_list) in
  match
    List.find_opt
      (fun op ->
        JSON.(op |-> "contents" |=> 0 |-> "kind" |> as_string) = kind
        && extra op)
      ops
  with
  | None -> Test.fail "Block does not contain %s" what
  | Some op ->
      let status =
        JSON.(
          op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
          |-> "status" |> as_string)
      in
      Check.((status = "applied") string)
        ~error_msg:(sf "%s status in block is %%L instead of %%R" what)

let check_l1_block_contains_commitment ~level block =
  check_l1_block_contains
    ~kind:"tx_rollup_commit"
    ~what:(sf "commitment of level %d" level)
    ~extra:(fun op ->
      JSON.(op |-> "contents" |=> 0 |-> "commitment" |-> "level" |> as_int)
      = level)
    block

let check_l1_block_contains_finalize ~level block =
  check_l1_block_contains
    ~kind:"tx_rollup_finalize_commitment"
    ~what:(sf "finalization of level %d" level)
    ~extra:(fun op ->
      JSON.(
        op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
        |-> "level" |> as_int)
      = level)
    block

let check_l1_block_contains_dispatch block =
  check_l1_block_contains
    ~kind:"tx_rollup_dispatch_tickets"
    ~what:"dispatch tickets"
    block

let check_l1_block_contains_rejection ~level block =
  check_l1_block_contains
    ~kind:"tx_rollup_rejection"
    ~what:(sf "rejection of level %d" level)
    ~extra:(fun op ->
      JSON.(op |-> "contents" |=> 0 |-> "level" |> as_int) = level)
    block

let check_l2_level block expected_level =
  let level = JSON.(block |-> "header" |-> "level" |> as_int) in
  Check.((level = expected_level) int)
    ~error_msg:"L2 level is %L but expected %R" ;
  Log.info "Rollup level %d" level

let check_commitments_inclusion ~tx_node list =
  let check_commitment_included block expected =
    let level = JSON.(block |-> "header" |-> "level") in
    let not_included =
      JSON.(block |-> "metadata" |-> "commitment_included" |> is_null)
    in
    if not_included = expected then
      Test.fail
        "Commitment for level %s is %sincluded but should %sbe"
        (JSON.encode level)
        (if not_included then "not " else "")
        (if expected then "" else "not ")
  in
  Lwt_list.iter_p
    (fun (block, included) ->
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block in
      check_commitment_included block included ;
      unit)
    list

(* Checks that the configuration is stored and that the  required
   fields are present. *)
let test_node_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: configuration"
    ~tags:["tx_rollup"; "configuration"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      (* Originate a rollup with a given operator *)
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* () =
        Tx_rollup_node.create
          ~protocol
          Operator
          ~rollup_id:tx_rollup_hash
          client
          node
        |> Tx_rollup_node.spawn_init_config
        |> Process.check_error ~exit_code:1 ~msg:(rex "Missing signers")
      in
      let tx_rollup_node =
        Tx_rollup_node.create
          ~protocol
          Observer
          ~rollup_id:tx_rollup_hash
          ~allow_deposit:true
          client
          node
      in
      let* filename = Tx_rollup_node.init_config tx_rollup_node in
      Log.info "Tx_rollup configuration file was successfully created" ;
      let () =
        let open Ezjsonm in
        let req = ["mode"; "signers"; "rollup_id"; "rpc_addr"] in
        (* TODO: add optional args checks *)
        match from_channel @@ open_in filename with
        | `O fields ->
            List.iter
              (fun k ->
                if List.exists (fun (key, _v) -> String.equal k key) fields then
                  ()
                else Test.fail "unexpected configuration field")
              req
        | _ -> Test.fail "Unexpected configuration format"
      in
      unit)

let init_and_run_rollup_node ~protocol ~originator ?operator ?batch_signer
    ?finalize_commitment_signer ?remove_commitment_signer
    ?dispatch_withdrawals_signer ?rejection_signer
    ?(allow_deposit = operator <> None) ?(bake_origination = true) node client =
  let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:originator client in
  let* () =
    if bake_origination then Client.bake_for_and_wait client else unit
  in
  Log.info "Tx_rollup %s was successfully originated" tx_rollup_hash ;
  let tx_node =
    Tx_rollup_node.create
      ~protocol
      Custom
      ~rollup_id:tx_rollup_hash
      ?operator
      ?batch_signer
      ?finalize_commitment_signer
      ?remove_commitment_signer
      ?dispatch_withdrawals_signer
      ?rejection_signer
      ~allow_deposit
      client
      node
  in
  let* _ = Tx_rollup_node.init_config tx_node in
  let* () = Tx_rollup_node.run tx_node in
  Log.info "Tx_rollup node is now running" ;
  let* () = Tx_rollup_node.wait_for_ready tx_node in
  Lwt.return (tx_rollup_hash, tx_node)

(* Checks that the tx_node is ready after originating an associated
   rollup key. *)
let test_tx_node_origination =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test if the node is ready"
    ~tags:["tx_rollup"; "ready"; "originate"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* _tx_node =
        init_and_run_rollup_node ~protocol ~originator node client
      in
      unit)

let test_not_allow_deposit =
  Protocol.register_test
    ~__FILE__
    ~title:
      "TX_rollup: test that the node refuses to start if not allowed to deposit"
    ~tags:["tx_rollup"; "node"; "allow"; "deposit"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let operator = Constant.bootstrap2.public_key_hash in
      let*! tx_rollup_hash =
        Client.Tx_rollup.originate ~src:originator client
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "Tx_rollup %s was successfully originated" tx_rollup_hash ;
      let tx_node =
        Tx_rollup_node.create
          ~protocol
          Custom
          ~rollup_id:tx_rollup_hash
          ~operator
          ~allow_deposit:false
          client
          node
      in
      let* _ = Tx_rollup_node.init_config tx_node in
      let* () = Tx_rollup_node.run tx_node in
      Log.info "Tx_rollup node is now running" ;
      let ready =
        let* () = Tx_rollup_node.wait_for_ready tx_node in
        Test.fail
          "Rollup node shouldn't start when not allowed to make deposits"
      in
      let dies =
        let node_process = Option.get @@ Tx_rollup_node.process tx_node in
        Process.check_error
          ~exit_code:1
          ~msg:(rex "This rollup node is not authorized to make a deposit")
          node_process
      in
      let* () = Lwt.choose [ready; dies] in
      unit)

let test_allow_deposit =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test that the node starts if allowed to deposit"
    ~tags:["tx_rollup"; "node"; "allow"; "deposit"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let operator = Constant.bootstrap2.public_key_hash in
      let* _tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~operator
          ~allow_deposit:true
          node
          client
      in
      unit)

(*FIXME/TORU: add additional checks such as contents, context_hash, …*)
let check_inbox_equality (i1 : Rollup.inbox) (i2 : Rollup.inbox) =
  Check.(
    (( = )
       i1.cumulated_size
       i2.cumulated_size
       ~error_msg:
         "Cumulated size of inboxes computed by the rollup node should be \
          equal to the cumulated size given by the RPC")
      int)

let tx_client_get_inbox_as_json ~tx_client ~block =
  let* out = Tx_rollup_client.get_inbox ~block tx_client in
  let json = JSON.parse ~origin:"tx_client_get_inbox_as_json" out in
  return json

let tx_client_get_inbox ~tx_client ~tezos_client ~block =
  let* json = tx_client_get_inbox_as_json ~tx_client ~block in
  let parse_message json =
    if JSON.(is_null (json |-> "batch")) then
      Test.fail "This case is not handled yet"
    else JSON.(json |-> "batch" |> as_string |> fun x -> `Batch (`Hex x))
  in
  let messages =
    JSON.(
      json |> as_list |> List.map (fun x -> x |-> "message" |> parse_message))
  in
  Rollup.compute_inbox_from_messages messages tezos_client

(* Checks that an inbox received by the tx_rollup node is well stored
   and available in a percistent way, even if the inbox is faulty
   (which is the case in this test). *)
let test_tx_node_store_inbox =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: store inbox"
    ~tags:["tx_rollup"; "store"; "inbox"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! rollup = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for_and_wait client in
      let tx_node =
        Tx_rollup_node.create
          ~protocol
          Observer
          ~rollup_id:rollup
          ~allow_deposit:true
          client
          node
      in
      let* _ = Tx_rollup_node.init_config tx_node in
      let* () = Tx_rollup_node.run tx_node in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Submit a batch *)
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_1" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 3 in
      let* tx_node_inbox_1 =
        tx_client_get_inbox ~tx_client ~tezos_client:client ~block:"0"
      in
      let*! expected_inbox_1 = Rollup.get_inbox ~rollup ~level:0 client in
      (* Ensure that stored inboxes on daemon's side are equivalent of
         inboxes returned by the rpc call. *)
      Check.(Some tx_node_inbox_1 = expected_inbox_1)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_2" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_3" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup
          ~src:Constant.bootstrap3.public_key_hash
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 4 in
      let* tx_node_inbox_2 =
        tx_client_get_inbox ~tx_client ~tezos_client:client ~block:"1"
      in
      let*! expected_inbox_2 = Rollup.get_inbox ~rollup ~level:1 client in
      (* Ensure that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(Some tx_node_inbox_2 = expected_inbox_2)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      (* Stop the node and try to get the inbox once again*)
      let* () = Tx_rollup_node.terminate tx_node in
      let* () = Tx_rollup_node.run tx_node in
      let* () = Tx_rollup_node.wait_for_ready tx_node in
      let*! inbox_after_restart = Rollup.get_inbox ~rollup ~level:1 client in
      Check.(Some tx_node_inbox_2 = inbox_after_restart)
        (Check.option Rollup.Check.inbox)
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      unit)

let test_node_cannot_connect =
  Protocol.register_test
    ~__FILE__
    ~title:
      "TX_rollup: test that the rollup node exits when it cannot connect to a \
       Tezos node on startup"
    ~tags:["tx_rollup"; "node"; "connect"]
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let originator = Constant.bootstrap1.public_key_hash in
  Log.info "Originate rollup" ;
  let*! rollup_id = Client.Tx_rollup.originate ~src:originator client in
  let* () = Client.bake_for_and_wait client in
  Log.info "Stopping Tezos node" ;
  let* () = Node.terminate node in
  let tx_node =
    Tx_rollup_node.create
      ~protocol
      Custom
      ~rollup_id
      ~allow_deposit:false
      client
      node
  in
  let* _ = Tx_rollup_node.init_config tx_node in
  let* () = Tx_rollup_node.run tx_node in
  let ready =
    let* () = Tx_rollup_node.wait_for_ready tx_node in
    Test.fail
      "Rollup node shouldn't start when it cannot connect to a Tezos node"
  in
  let dies =
    let node_process = Option.get @@ Tx_rollup_node.process tx_node in
    Process.check_error
      ~exit_code:1
      ~msg:(rex "Unable to connect to the node")
      node_process
  in
  let* () = Lwt.choose [ready; dies] in
  unit

let test_node_disconnect =
  Protocol.register_test
    ~__FILE__
    ~title:
      "TX_rollup: test that the we recover when disconnecting from Tezos node"
    ~tags:["tx_rollup"; "node"; "disconnect"]
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let originator = Constant.bootstrap1.public_key_hash in
  let* rollup, tx_node =
    init_and_run_rollup_node ~protocol ~originator node client
  in
  (* Submit a batch *)
  let (`Batch content) = Rollup.make_batch "tezos_l2_batch_1" in
  let*! () =
    Client.Tx_rollup.submit_batch
      ~content
      ~rollup
      ~src:Constant.bootstrap2.public_key_hash
      client
  in
  let block_notify_promise = wait_for_notified_block tx_node in
  let* () = Client.bake_for client in
  let* () = block_notify_promise in
  Log.info "Brutally killing Tezos node" ;
  let* () = Node.kill node in
  let* () = Lwt_unix.sleep 2. in
  let* () = Node.run node Node.[Connections 0; Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node in
  let* () = Client.bake_for_and_wait client in
  Log.info "Rollup node should reconnect and see the new L1 block" ;
  let () =
    match Tx_rollup_node.process tx_node with
    | None -> Test.fail "Rollup node stopped"
    | Some _ -> ()
  in
  let* _ = wait_tezos_node_level tx_node node in
  unit

(* The contract is expecting a parameter of the form:
   (Pair string amount tx_rollup_tz4_address tx_rollup_txr1_address) *)
let make_tx_rollup_deposit_argument tickets_content tickets_amount tz4 txr1 =
  Format.sprintf
    {|(Pair "%s" %d "%s" "%s" )|}
    tickets_content
    tickets_amount
    tz4
    txr1

type commitment_info = {
  roots : string list;
  context_hashes : string list;
  inbox_merkle_root : string;
  predecessor : string option;
}

(** Build a {!commitment_info} for the inbox at a given level but do not inject
    the operation.

    Note that the field [commitment_info.context_hashes] is not used
    in the commitment but is necessary for subsequent rejections. *)
let build_commitment_info ~tx_level ~tx_rollup_hash ~tx_node ~client =
  let*! inbox_opt =
    Rollup.get_inbox ~rollup:tx_rollup_hash ~level:tx_level client
  in
  let inbox =
    match inbox_opt with
    | Some x -> x
    | None ->
        failwith ("There is no inbox at the level " ^ string_of_int tx_level)
  in
  let inbox_merkle_root = inbox.merkle_root in
  let* rollup_inbox =
    Tx_rollup_node.Client.get_inbox ~tx_node ~block:(string_of_int tx_level)
  in
  let context_hashes =
    List.map
      (fun x -> x.Tx_rollup_node.Inbox.l2_context_hash.tree_hash)
      rollup_inbox
  in
  let* roots =
    Lwt_list.map_p
      (fun context_hash ->
        let*! root =
          Rollup.message_result_hash
            ~context_hash
            ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list_hash
            client
        in
        return root)
      context_hashes
  in
  let* predecessor =
    if tx_level > 0 then
      let*! prev_commitment_opt =
        Rollup.get_commitment
          ~rollup:tx_rollup_hash
          ~level:(tx_level - 1)
          client
      in
      match prev_commitment_opt with
      | None -> failwith "Failed to find the previous commitment"
      | Some x -> return (Some x.commitment_hash)
    else return None
  in
  return {roots; context_hashes; predecessor; inbox_merkle_root}

let check_commitment_content block ~tx_level ~tx_rollup_hash ~tx_node ~client =
  let* expected =
    build_commitment_info ~tx_level ~tx_rollup_hash ~tx_node ~client
  in
  let commitment = JSON.(block |-> "commitment") in
  let predecessor = JSON.(commitment |-> "predecessor" |> as_string_opt) in
  (if predecessor <> expected.predecessor then
   let to_string = Option.value ~default:"None" in
   Test.fail
     "Commitment predecessor for level %d is %s but should be %s"
     tx_level
     (to_string predecessor)
     (to_string expected.predecessor)) ;
  let messages =
    JSON.(commitment |-> "messages" |> as_list |> List.map as_string)
  in
  (if messages <> expected.roots then
   let to_string =
     let pp = Format.pp_print_list Format.pp_print_string in
     Format.asprintf "%a" pp
   in
   Test.fail
     "Commitment roots for level %d is %s but should be %s"
     tx_level
     (to_string messages)
     (to_string expected.roots)) ;
  let inbox = JSON.(commitment |-> "inbox_merkle_root" |> as_string) in
  if inbox <> expected.inbox_merkle_root then
    Test.fail
      "Commitment inbox merkle root for level %d is %s but should be %s"
      tx_level
      inbox
      expected.inbox_merkle_root ;
  unit

let check_commitments_content ~tx_node ~tx_rollup_hash ~client list =
  Lwt_list.iter_p
    (fun block ->
      let tx_level = int_of_string block in
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block in
      check_commitment_content block ~tx_level ~tx_rollup_hash ~tx_node ~client)
    list

type rejection_info = {
  proof : string;
  message : string;
  path : string;
  rejected_message_result_hash : string;
  rejected_message_result_path : string;
  context_hash : string;
  withdraw_list_hash : string;
  agreed_message_result_path : string;
}

(** Build a rejection for the inbox at a given level but do not inject the
    operation.

    Note that if [message_pos = 0] and [tx_level > 0], we need information from
    the previous commitment: [agreed_context_hash] and
    [agreed_message_result_path]. If they are absent in this case, the function
    will fail.

    TODO/TORU: the withdraw_list_hash is currently always the empty list hash.
*)
let build_rejection ~tx_level ~tx_node ~message_pos ~client ?agreed_context_hash
    ?agreed_message_result_path commitment_info : rejection_info Lwt.t =
  let* rollup_inbox =
    Tx_rollup_node.Client.get_inbox ~tx_node ~block:(string_of_int tx_level)
  in
  let* hashes =
    Lwt_list.map_p
      (fun content ->
        let message = content.Tx_rollup_node.Inbox.message in
        let message =
          (match JSON.(message |-> "batch" |> as_string_opt) with
           | Some x -> `Batch (`Hex x)
           | None ->
               let deposit = JSON.(message |-> "deposit") in
               let sender = JSON.(deposit |-> "sender" |> as_string) in
               let destination =
                 JSON.(deposit |-> "destination" |> as_string)
               in
               let ticket_hash =
                 JSON.(deposit |-> "ticket_hash" |> as_string)
               in
               let amount = JSON.(deposit |-> "amount" |> as_int64) in
               Rollup.make_deposit ~sender ~destination ~ticket_hash ~amount
            :> Rollup.message)
        in
        let*! message_hash = Rollup.message_hash ~message client in
        return message_hash)
      rollup_inbox
  in
  let message =
    List.nth rollup_inbox message_pos |> fun content ->
    JSON.encode content.message
  in
  let* message_path =
    let*! message_path =
      Rollup.inbox_merkle_tree_path
        ~message_hashes:hashes
        ~position:message_pos
        client
    in
    return (JSON.encode message_path)
  in
  let* agreed_context_hash, agreed_message_result_path =
    if message_pos = 0 && tx_level = 0 then
      return (Constant.tx_rollup_empty_l2_context, "[]")
    else if message_pos = 0 then
      (* If the message position is 0, we need information from the previous
         commitment. *)
      return
        (Option.get agreed_context_hash, Option.get agreed_message_result_path)
    else
      (* Else, we take the information from the previous message in the inbox. *)
      let agreed_context_hash =
        List.nth commitment_info.context_hashes (message_pos - 1)
      in
      let*! agreed_result_path =
        Rollup.commitment_merkle_tree_path
          ~message_result_hashes:
            (List.map (fun x -> `Hash x) commitment_info.roots)
          ~position:(message_pos - 1)
          client
      in
      return (agreed_context_hash, JSON.encode agreed_result_path)
  in
  let rejected_message_result_hash =
    List.nth commitment_info.roots message_pos
  in
  let* rejected_message_result_path =
    let*! rejected_message_result_path =
      Rollup.commitment_merkle_tree_path
        ~message_result_hashes:
          (List.map (fun x -> `Hash x) commitment_info.roots)
        ~position:message_pos
        client
    in
    return (JSON.encode rejected_message_result_path)
  in
  let* proof =
    let* proof =
      Tx_rollup_node.Client.get_merkle_proof
        ~tx_node
        ~block:(string_of_int tx_level)
        ~message_pos:(string_of_int message_pos)
    in
    return (JSON.encode proof)
  in
  return
    {
      proof;
      message;
      path = message_path;
      rejected_message_result_hash;
      rejected_message_result_path;
      context_hash = agreed_context_hash;
      withdraw_list_hash = Constant.tx_rollup_empty_withdraw_list_hash;
      agreed_message_result_path;
    }

let check_tz4_balance ~tx_client ~block ~ticket_id ~tz4_address
    ~expected_balance =
  let* tz4_balance =
    Tx_rollup_client.get_balance tx_client ~block ~tz4_address ~ticket_id
  in
  Check.(
    ( = )
      tz4_balance
      expected_balance
      int
      ~error_msg:
        (Format.sprintf
           "The balance of %s was expected to be %d instead of %d."
           tz4_address
           expected_balance
           tz4_balance)) ;
  unit

let get_ticket_hash_from_deposit (d : Tx_rollup_node.Inbox.message) : string =
  JSON.(d.message |-> "deposit" |-> "ticket_hash" |> as_string)

let get_ticket_hash_from_deposit_json inbox =
  JSON.(inbox |=> 0 |-> "message" |-> "deposit" |-> "ticket_hash" |> as_string)

(* Checks that the a ticket can be transfered from the L1 to the rollup. *)
let test_ticket_deposit_from_l1_to_l2 =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: deposit ticket from l1 to l2"
    ~tags:["tx_rollup"; "deposit"; "ticket"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node ~protocol ~originator:operator node client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      let* _alias, contract_id =
        Client.originate_contract_at
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
          ["mini_scenarios"; "tx_rollup_deposit"]
          protocol
      in
      let* () = Client.bake_for_and_wait client in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      let* bls_key = Client.bls_gen_and_show_keys client in
      let tickets_content = "toru" in
      let tickets_amount = 10 in
      let arg =
        make_tx_rollup_deposit_argument
          tickets_content
          tickets_amount
          bls_key.aggregate_public_key_hash
          tx_rollup_hash
      in
      (* This smart contract call will transfer 10 tickets to the given
         address. *)
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 4 in
      (* Get the operation containing the ticket transfer. We assume
         that only one operation is issued in this block. *)
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      check_tz4_balance
        ~tx_client
        ~block:"head"
        ~ticket_id
        ~tz4_address:bls_key.aggregate_public_key_hash
        ~expected_balance:10)

let json_of_transactions_and_sig ~origin transaction signatures =
  JSON.(
    annotate
      ~origin
      (`O
        [
          ("transaction", transaction);
          ("signatures", `A (List.map (fun s -> `String s) signatures));
        ]))

let craft_tx_transfers_and_sign ?counter tx_client ~signer transfers =
  let* transaction =
    Tx_rollup_client.craft_tx_transfers
      ~signer:signer.Account.aggregate_public_key
      ?counter
      tx_client
      transfers
  in
  let* signature =
    Tx_rollup_client.sign_transaction
      ~transaction
      ~signers:[signer.aggregate_alias]
      tx_client
  in
  return (transaction, signature)

let craft_tx_and_sign ?counter tx_client ~qty ~signer ~dest ~ticket =
  let* transaction =
    Tx_rollup_client.craft_tx_transaction
      tx_client
      ?counter
      (`Transfer {qty; destination = dest; ticket})
      ~signer:signer.Account.aggregate_public_key
  in
  let* signature =
    Tx_rollup_client.sign_transaction
      ~transaction
      ~signers:[signer.aggregate_alias]
      tx_client
  in
  return (transaction, signature)

let craft_withdraw_and_sign ?counter tx_client ~qty ~signer ~dest ~ticket =
  let* transaction =
    Tx_rollup_client.craft_tx_withdraw
      tx_client
      ?counter
      ~qty
      ~signer:signer.Account.aggregate_public_key
      ~dest
      ~ticket
  in
  let* signature =
    Tx_rollup_client.sign_transaction
      ~transaction
      ~signers:[signer.Account.aggregate_alias]
      tx_client
  in
  return (transaction, signature)

let craft_batch_for_one_tx ?counter tx_client ~qty ~signer ~dest ~ticket =
  let* transaction, signature =
    craft_tx_and_sign ?counter tx_client ~qty ~signer ~dest ~ticket
  in
  let transactions_and_sig =
    json_of_transactions_and_sig
      ~origin:"signed_l2_transaction"
      (JSON.unannotate transaction)
      [signature]
  in
  let* batch =
    Tx_rollup_client.craft_tx_batch
      ~show_hex:true
      tx_client
      ~transactions_and_sig
  in
  match batch with
  | `Json _j -> assert false
  | `Hex _hex as batch -> return batch

let inject_transfer ?counter tx_client ~source ~qty ~dest ~ticket =
  Tx_rollup_client.transfer ?counter tx_client ~source
  @@ `Transfer {qty; destination = dest; ticket}

let inject_withdraw ?counter tx_client ~source ~qty ~dest ~ticket =
  Tx_rollup_client.withdraw
    ?counter
    tx_client
    ~source
    (`Withdraw {qty; destination = dest; ticket})

let tx_client_get_block ~tx_client ~block =
  Tx_rollup_client.get_block ~block tx_client

(* Checks that the a ticket can be transfered within the rollup. *)
let test_l2_to_l2_transaction =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: l2 to l2 transaction"
    ~tags:["tx_rollup"; "rollup"; "internal"; "transaction"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node ~protocol ~originator node client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      let* _alias, contract_id =
        Client.originate_contract_at
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
          ["mini_scenarios"; "tx_rollup_deposit"]
          protocol
      in
      let* () = Client.bake_for_and_wait client in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Genarating some identities *)
      let* bls_keys_1 = Client.bls_gen_and_show_keys client in
      let* bls_keys_2 = Client.bls_gen_and_show_keys client in
      let tickets_content = "toru" in
      let tickets_amount = 10 in
      let arg_1 =
        make_tx_rollup_deposit_argument
          tickets_content
          tickets_amount
          bls_keys_1.aggregate_public_key_hash
          tx_rollup_hash
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_keys_1.aggregate_public_key_hash
          ~expected_balance:10
      in
      let tickets_content = "toru" in
      let tickets_amount = 10 in
      let arg_2 =
        make_tx_rollup_deposit_argument
          tickets_content
          tickets_amount
          bls_keys_2.aggregate_public_key_hash
          tx_rollup_hash
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_2
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 5 in
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_keys_2.aggregate_public_key_hash
          ~expected_balance:10
      in
      Log.info "Crafting a l2 transaction" ;
      let* batch =
        craft_batch_for_one_tx
          tx_client
          ~qty:1L
          ~signer:bls_keys_1
          ~dest:bls_keys_2.aggregate_public_key_hash
          ~ticket:ticket_id
      in
      Log.info "Submiting a batch" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:batch
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      Log.info "Baking the batch" ;
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 6 in
      (* The decoding fails because of the buggy JSON encoding. This
         line can be uncommented once it is fixed.*)
      let* _node_inbox =
        tx_client_get_inbox ~tx_client ~tezos_client:client ~block:"head"
      in
      let* _ = tx_client_get_block ~tx_client ~block:"head" in
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_keys_1.aggregate_public_key_hash
          ~expected_balance:9
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_keys_2.aggregate_public_key_hash
          ~expected_balance:11
      in
      unit)

let tx_client_inject_transaction ~tx_client ?failswith transaction signature =
  let transactions_and_sig =
    json_of_transactions_and_sig
      ~origin:"signed_l2_transaction"
      (JSON.unannotate transaction)
      signature
  in
  let expect_failure = Option.is_some failswith in
  let* stdout, stderr =
    Tx_rollup_client.inject_batcher_transaction
      ~expect_failure
      tx_client
      ~transactions_and_sig
  in
  match failswith with
  | None -> (
      let json = JSON.parse ~origin:"tx_client_inject_transaction" stdout in
      try return JSON.(json |> as_string)
      with _ ->
        Test.fail "Transaction injection failed with: %s" (JSON.encode json))
  | Some expected_id ->
      let error_id = stderr =~* rex "\"id\": \"([^\"]+)" in
      let error_id =
        match error_id with
        | None ->
            Test.fail
              "Injection should have failed with *%s* but didn't fail"
              expected_id
        | Some e -> e
      in
      if not (error_id =~ rex expected_id) then
        Test.fail
          "Injection should have failed with *%s* but failed with %s"
          expected_id
          error_id ;
      (* Dummy value for operation hash *)
      return ""

let craft_tx_and_inject ?failswith ?counter tx_client ~qty ~signer ~dest ~ticket
    =
  let* transaction, signature =
    craft_tx_and_sign ?counter tx_client ~qty ~signer ~dest ~ticket
  in
  tx_client_inject_transaction ~tx_client ?failswith transaction [signature]

let tx_client_get_queue ~tx_client =
  let* out = Tx_rollup_client.get_batcher_queue tx_client in
  let json = JSON.parse ~origin:"tx_client_get_queue" out in
  return json

let tx_client_get_transaction_in_queue ~tx_client transaction_hash =
  let* out =
    Tx_rollup_client.get_batcher_transaction tx_client ~transaction_hash
  in
  let json = JSON.parse ~origin:"tx_client_get_transaction" out in
  return json

(* Returns the ticket hash, if any, of a given operation. *)
let get_ticket_hash_from_op op =
  let metadata = JSON.(op |-> "contents" |=> 0 |-> "metadata") in
  let result =
    JSON.(metadata |-> "internal_operation_results" |=> 0 |-> "result")
  in
  let kind =
    JSON.(
      metadata |-> "internal_operation_results" |=> 0 |-> "parameters"
      |-> "entrypoint" |> as_string)
  in
  let deposit = "deposit" in
  if not String.(equal kind deposit) then
    Test.fail
      "The internal operation was expected to be a %s but is a %s"
      deposit
      kind ;
  let status = JSON.(result |-> "status" |> as_string_opt) in
  match status with
  | Some v when String.(equal v "applied") ->
      JSON.(result |-> "ticket_hash" |> as_string)
  | None | Some _ -> Test.fail "The contract origination failed"

(** Originate a contract and make a deposit for [dest] and optionally
    for a list of destination in [dests]. *)
let make_deposit ~protocol ~source ~tx_rollup_hash ~tx_node ~client
    ?(dests = []) ~tickets_amount dest =
  let* _alias, contract_id =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:source
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "tx_rollup_deposit"]
      protocol
  in
  let* level = Client.level client in
  let* () = Client.bake_for_and_wait client in
  let level = succ level in
  let* _ = Tx_rollup_node.wait_for_tezos_level tx_node level in
  Log.info
    "The tx_rollup_deposit %s contract was successfully originated"
    contract_id ;
  let dests = dest :: dests in
  let tickets_content = "toru" in
  let* level =
    Lwt_list.fold_left_s
      (fun level dest ->
        let arg =
          make_tx_rollup_deposit_argument
            tickets_content
            tickets_amount
            dest
            tx_rollup_hash
        in
        let* () =
          Client.transfer
            ~gas_limit:100_000
            ~fee:Tez.one
            ~amount:Tez.zero
            ~burn_cap:Tez.one
            ~storage_limit:10_000
            ~giver:source
            ~receiver:contract_id
            ~arg
            client
        in
        let* () = Client.bake_for_and_wait client in
        let level = succ level in
        let* _ = Tx_rollup_node.wait_for_tezos_level tx_node level in
        return level)
      level
      dests
  in
  return (level, contract_id)

(* Checks that the rollup node can receive L2 transactions in its queue, batch
   them and inject them in the Tezos node. *)
let test_batcher ~test_persistence =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "TX_rollup: L2 %s"
         (if test_persistence then "persistent injector" else "batcher"))
    ~tags:
      (["tx_rollup"; "node"; "batcher"; "transaction"; "injector"]
      @ if test_persistence then ["persistent"] else ["nostop"])
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap2.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Genarating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:100_000
          bls_pkh_1
          ~dests:[bls_pkh_2]
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:100_000
      in
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:100_000
      in
      Log.info
        "Crafting and injecting a l2 transaction: %s transfers 1 to %s"
        bls_pkh_1
        bls_pkh_2 ;
      let* txh1 =
        inject_transfer
          tx_client
          ~qty:1L
          ~source:bls_key_1.aggregate_alias
          ~dest:bls_pkh_2
          ~ticket:ticket_id
      in

      Log.info
        "Crafting and injecting a l2 transaction: %s transfers 5 to %s"
        bls_pkh_2
        bls_pkh_1 ;
      let* txh2 =
        inject_transfer
          tx_client
          ~qty:5L
          ~source:bls_key_2.aggregate_alias
          ~dest:bls_pkh_1
          ~ticket:ticket_id
      in

      Log.info "Crafting a l2 transaction with wrong counter" ;
      let* _txh =
        craft_tx_and_inject
          tx_client
          ~failswith:"tx_rollup_operation_counter_mismatch"
          ~qty:5L
          ~counter:5L
          ~signer:bls_key_2
          ~dest:bls_key_1.aggregate_public_key_hash
          ~ticket:ticket_id
      in

      Log.info "Crafting a l2 transaction with wrong signature" ;
      let* _txh =
        (* craft a transaction, but ignore the signature *)
        let* transaction, _signature =
          craft_tx_and_sign
            tx_client
            ~qty:1L
            ~signer:bls_key_1
            ~dest:bls_key_2.aggregate_public_key_hash
            ~ticket:ticket_id
        in
        (* craft a signature, for an ignored transaction *)
        let* _transaction, signature =
          craft_tx_and_sign
            tx_client
            ~qty:2L
            ~signer:bls_key_1
            ~dest:bls_key_2.aggregate_public_key_hash
            ~ticket:ticket_id
        in
        (* mix both *)
        tx_client_inject_transaction
          ~tx_client
          ~failswith:"tx_rollup_incorrect_aggregated_signature"
          transaction
          [signature]
      in

      Log.info "Crafting a l2 transaction with too big amount" ;
      let* _txh =
        craft_tx_and_inject
          tx_client
          ~qty:1_000_000L
          ~signer:bls_key_1
          ~dest:bls_key_2.aggregate_public_key_hash
          ~ticket:ticket_id
          ~counter:2L
          ~failswith:"tx_rollup_balance_too_low"
      in

      Log.info "Checking rollup node queue" ;
      let* q = tx_client_get_queue ~tx_client in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = 2) int) ~error_msg:"Queue length is %L but should be %R" ;
      Log.info "Checking rollup node queue transactions" ;
      let* _t1 = tx_client_get_transaction_in_queue ~tx_client txh1
      and* _t2 = tx_client_get_transaction_in_queue ~tx_client txh2 in
      let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 7 in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:100_004
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:99_996
      in

      let inject_tx ?(amount = 1L) ~counter ~from ~dest () =
        inject_transfer
          ~counter
          tx_client
          ~source:from.Account.aggregate_alias
          ~qty:amount
          ~dest
          ~ticket:ticket_id
      in

      let nbtxs1 = 70 in
      let nbtxs2 = 30 in
      let batch_success_promise = wait_for_batch_success_event tx_node in
      let* () =
        Log.info "Injecting %d transactions to queue" nbtxs1 ;
        Lwt_list.iter_s
          (fun counter ->
            let* _ = inject_tx ~counter ~from:bls_key_1 ~dest:bls_pkh_2 () in
            unit)
          (List.init nbtxs1 (fun i -> Int64.of_int (i + 2)))
      and* () =
        Log.info "Injecting %d transactions to queue" nbtxs2 ;
        Lwt_list.iter_s
          (fun counter ->
            let* _ = inject_tx ~counter ~from:bls_key_2 ~dest:bls_pkh_1 () in
            unit)
          (List.init nbtxs2 (fun i -> Int64.of_int (i + 2)))
      in
      let* q = tx_client_get_queue ~tx_client in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = nbtxs1 + nbtxs2) int)
        ~error_msg:"Queue length is %L but should be %R" ;
      let* () = Client.bake_for_and_wait client in
      Log.info "Waiting for batching on L1 to succeed" ;
      let* () = batch_success_promise in
      Log.info "Batching succeeded" ;
      let* () =
        if test_persistence then (
          Log.info "Stopping node to prevent injection" ;
          let* () = Node.terminate node in
          (* We wait a bit to allow for the rollup node injector to write its
             state on disk (if we kill it right away, we may end up with
             corrupted files).
             Note: This can be a source of flakiness. *)
          let* () = Lwt_unix.sleep 2.0 in
          Log.info "Stopping rollup node" ;
          let* () = Tx_rollup_node.terminate tx_node in
          let* () =
            Node.run node Node.[Connections 0; Synchronisation_threshold 0]
          in
          let* () = Node.wait_for_ready node in
          let* () = Tx_rollup_node.run tx_node in
          let* () = Tx_rollup_node.wait_for_ready tx_node in
          Client.bake_for_and_wait client)
        else unit
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = wait_tezos_node_level tx_node node in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:(100_004 - nbtxs1 + nbtxs2)
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:(99_996 + nbtxs1 - nbtxs2)
      in
      unit)

(* Check that the rollup node is successfully doing reorganizations.
   To do so, we are going to:
   - create a branch of size one on node 1 (updating a L2 balance)
   - create a branch of size two on node 2 (with no L2 operations)
   - connecting node 1 and node 2
   - check that operation modifying the L2 balance was not applied
*)
let test_reorganization =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: L2 rollup node reorganization"
    ~tags:["tx_rollup"; "node"; "reorganization"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let nodes_args = Node.[Connections 2; Synchronisation_threshold 0] in
      let* node1, client1 =
        Client.init_with_protocol
          ~nodes_args
          ~parameter_file
          `Client
          ~protocol
          ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node ~protocol ~originator:operator node1 client1
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client1)
          tx_node
      in
      (* Genarating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client1 in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client1 in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client:client1
          ~tickets_amount:10
          bls_pkh_1
      in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      (* Run the node that will be used to forge an alternative branch *)
      let* node2 = Node.init nodes_args in
      let* client2 = Client.init ~endpoint:Client.(Node node2) () in
      let* () = Client.Admin.connect_address client2 ~peer:node1 in
      let* _ = Node.wait_for_level node2 4 in
      Log.info "Nodes are synchronized, shutting down node 2" ;
      let* () = Node.terminate node2 in
      Log.info "Check that L2 balance is 10" ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:10
      in
      Log.info "crafting a branch of size 1 on node 1 with a L2 transfer" ;
      let* batch =
        craft_batch_for_one_tx
          tx_client
          ~qty:10L
          ~signer:bls_key_1
          ~dest:bls_pkh_2
          ~ticket:ticket_id
      in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:batch
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap2.public_key_hash
          client1
      in
      let* () = Client.bake_for_and_wait client1 in
      let* _ = Node.wait_for_level node1 5 in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 5 in
      Log.info "Check that L2 balance is now 99_990" ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:0
      in
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:10
      in
      Log.info "Running the node2 in private mode and craft a branch of size 2" ;
      let* () = Node.run node2 (Node.Private_mode :: nodes_args) in
      let* () = Node.wait_for_ready node2 in
      let* () =
        repeat 2 (fun () ->
            Client.bake_for_and_wait
              ~keys:[Constant.bootstrap2.public_key_hash]
              client2)
      in
      let* _ = Node.wait_for_level node2 6 in
      Log.info "Reconnecting node 1 and 2" ;
      let* () = Client.Admin.trust_address client2 ~peer:node1 in
      let* () = Client.Admin.connect_address client2 ~peer:node1 in
      let* _ = Node.wait_for_level node1 6 in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 6 in
      (* Check that the balance is untouched, that is to say that the
         rollup node had backtracked the operation from the
         alternative branch. *)
      Log.info "Check that L2 balance is back to 10" ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:10
      in
      unit)

let test_l2_proof_rpc_position =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: reject messages at position 0 and 1"
    ~tags:["tx_rollup"; "node"; "proofs"; "rejection"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let originator = Constant.bootstrap2.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node ~protocol ~originator node client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:100_000
          bls_pkh_1
      in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox) in
      Log.info "Ticket %s was successfully emitted" ticket_id ;

      Log.info "Commitment for rollup level: 0" ;
      let* {
             roots;
             context_hashes = context_hashes_level0;
             inbox_merkle_root;
             predecessor;
           } =
        build_commitment_info ~tx_level:0 ~tx_rollup_hash ~tx_node ~client
      in

      let*! () =
        Client.Tx_rollup.submit_commitment
          ~level:0
          ~roots
          ~inbox_merkle_root
          ?predecessor
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap3.public_key_hash
          client
      in
      let* batch1 =
        craft_batch_for_one_tx
          tx_client
          ~counter:1L
          ~signer:bls_key_1
          ~dest:bls_pkh_2
          ~ticket:ticket_id
          ~qty:5L
      in
      let* batch2 =
        craft_batch_for_one_tx
          tx_client
          ~counter:1L
          ~signer:bls_key_2
          ~dest:bls_pkh_1
          ~ticket:ticket_id
          ~qty:10L
      in
      Log.info "Submiting two batches" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:batch1
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:batch2
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap2.public_key_hash
          client
      in
      Log.info "Baking the batches" ;
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 5 in
      Log.info "Commitment for rollup level: 1" ;
      let* ({
              roots;
              context_hashes = _context_hashes_level1;
              inbox_merkle_root;
              predecessor;
            } as commitment_info) =
        build_commitment_info ~tx_level:1 ~tx_rollup_hash ~tx_node ~client
      in
      let*! () =
        Client.Tx_rollup.submit_commitment
          ~level:1
          ~roots
          ~inbox_merkle_root
          ?predecessor
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 6 in
      Log.info "Try to reject a good commitment at level 1, message 0" ;
      let last_prev_pos = List.length context_hashes_level0 - 1 in
      let agreed_context_hash = List.nth context_hashes_level0 last_prev_pos in
      let* agreed_message_result_path =
        let*! agreed_message_result_path =
          Rollup.commitment_merkle_tree_path
            ~message_result_hashes:
              (List.map (fun x -> `Hash x) commitment_info.roots)
            ~position:last_prev_pos
            client
        in
        return (JSON.encode agreed_message_result_path)
      in
      let* {
             proof;
             message;
             path;
             rejected_message_result_hash;
             rejected_message_result_path;
             context_hash;
             withdraw_list_hash;
             agreed_message_result_path;
           } =
        build_rejection
          ~tx_level:1
          ~tx_node
          ~message_pos:0
          ~client
          ~agreed_context_hash
          ~agreed_message_result_path
          commitment_info
      in
      let*? process =
        Client.Tx_rollup.submit_rejection
          ~src:operator
          ~proof
          ~rollup:tx_rollup_hash
          ~level:1
          ~message
          ~position:0
          ~path
          ~message_result_hash:rejected_message_result_hash
          ~rejected_message_result_path
          ~context_hash
          ~withdraw_list_hash
          ~agreed_message_result_path
          client
      in
      let* () =
        Process.check_error
          ~msg:(rex "tx_rollup_proof_produced_rejected_state")
          process
      in
      Log.info "Try to reject a good commitment at level 1, message 1" ;
      let* {
             proof;
             message;
             path;
             rejected_message_result_hash;
             rejected_message_result_path;
             context_hash;
             withdraw_list_hash;
             agreed_message_result_path;
           } =
        build_rejection
          ~tx_level:1
          ~tx_node
          ~message_pos:1
          ~client
          ~agreed_context_hash
          ~agreed_message_result_path
          commitment_info
      in
      let*? process =
        Client.Tx_rollup.submit_rejection
          ~src:operator
          ~proof
          ~rollup:tx_rollup_hash
          ~level:1
          ~message
          ~position:1
          ~path
          ~message_result_hash:rejected_message_result_hash
          ~rejected_message_result_path
          ~context_hash
          ~withdraw_list_hash
          ~agreed_message_result_path
          client
      in
      let* () =
        Process.check_error
          ~msg:(rex "tx_rollup_proof_produced_rejected_state")
          process
      in
      unit)

let test_reject_bad_commitment =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: reject bad commitment"
    ~tags:["tx_rollup"; "node"; "proofs"; "rejection"; "slashed"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let operator = Constant.bootstrap3.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node ~protocol ~originator node client
      in
      (* Generating some identities *)
      let* bls_key1 = Client.bls_gen_and_show_keys client in
      let pkh1_str = bls_key1.aggregate_public_key_hash in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:10
          pkh1_str
      in
      let* ({roots = _; context_hashes = _; inbox_merkle_root; predecessor} as
           commitment_info) =
        build_commitment_info ~tx_level:0 ~tx_rollup_hash ~tx_node ~client
      in
      (* Change the roots to produce an invalid commitment. *)
      let roots = [Constant.tx_rollup_initial_message_result] in
      let*! () =
        Client.Tx_rollup.submit_commitment
          ~level:0
          ~roots
          ~inbox_merkle_root
          ?predecessor
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 4 in
      let* {
             proof;
             message;
             path;
             rejected_message_result_hash = _;
             rejected_message_result_path;
             context_hash;
             withdraw_list_hash;
             agreed_message_result_path;
           } =
        build_rejection
          ~tx_level:0
          ~tx_node
          ~message_pos:0
          ~client
          commitment_info
      in
      Log.info "Stopping rollup node" ;
      let* () = Tx_rollup_node.terminate tx_node in
      Log.info "Restarting rollup node with committer/operator" ;
      let* () =
        Tx_rollup_node.change_signers ~operator:(Some operator) tx_node
      in
      let* () = Tx_rollup_node.run tx_node in
      let* () = Tx_rollup_node.wait_for_ready tx_node in
      Log.info "Injecting rejection" ;
      let*! () =
        Client.Tx_rollup.submit_rejection
          ~src:Constant.bootstrap4.public_key_hash
          ~proof
          ~rollup:tx_rollup_hash
          ~level:0
          ~message
          ~position:0
          ~path
          ~message_result_hash:Constant.tx_rollup_initial_message_result
          ~rejected_message_result_path
          ~context_hash
          ~withdraw_list_hash
          ~agreed_message_result_path
          client
      in
      let node_process = Option.get @@ Tx_rollup_node.process tx_node in
      (* Baking one block for rejection *)
      let* () = Client.bake_for_and_wait client in
      Log.info "Rollup node must exist with error message because of slashing" ;
      Process.check_error
        ~exit_code:1
        ~msg:(rex "The deposit for our operator was slashed")
        node_process)

let test_committer =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: injecting commitments automatically"
    ~tags:["tx_rollup"; "node"; "commitments"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let originator = Constant.bootstrap2.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~operator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* tzlevel, _ =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:100_000
          bls_pkh_1
      in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit (List.hd inbox) in
      let inject_tx ?counter ~from ~dest ?(amount = 1L) () =
        inject_transfer
          tx_client
          ~qty:amount
          ?counter
          ~source:from.Account.aggregate_alias
          ~dest
          ~ticket:ticket_id
      in
      let* () = check_commitments_inclusion ~tx_node [("0", false)] in
      Log.info "Sending some L2 transactions" ;
      let* _ = inject_tx ~from:bls_key_1 ~dest:bls_pkh_2 ~amount:1000L () in
      let* _ = inject_tx ~from:bls_key_2 ~dest:bls_pkh_1 ~amount:2L () in
      let* () = Client.bake_for_and_wait client in
      let* tzlevel =
        Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 1)
      in
      let* () = check_commitments_inclusion ~tx_node [("0", true)] in
      let* () =
        check_injection tx_node "commitment" @@ Client.bake_for_and_wait client
      in
      let* tzlevel =
        Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 1)
      in
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
      check_l2_level block 1 ;
      let* () =
        check_commitments_inclusion ~tx_node [("0", true); ("1", false)]
      in
      Log.info "Sending some more L2 transactions" ;
      let* _ = inject_tx ~from:bls_key_1 ~dest:bls_pkh_2 ~amount:3L () in
      let* _ = inject_tx ~from:bls_key_2 ~dest:bls_pkh_1 ~amount:4L () in
      let* () = Client.bake_for_and_wait client in
      let* tzlevel =
        Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 1)
      in
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
      check_l2_level block 1 ;
      let* () =
        check_commitments_inclusion ~tx_node [("0", true); ("1", true)]
      in
      Log.info "Sending some more L2 transactions" ;
      let* _ = inject_tx ~from:bls_key_1 ~dest:bls_pkh_2 ~amount:5L () in
      let* _ = inject_tx ~from:bls_key_2 ~dest:bls_pkh_1 ~amount:6L () in
      let* () =
        check_injection tx_node "commitment" @@ Client.bake_for_and_wait client
      in
      let* tzlevel =
        Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 1)
      in
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
      check_l2_level block 2 ;
      let* () =
        check_commitments_inclusion
          ~tx_node
          [("0", true); ("1", true); ("2", false)]
      in
      let* () =
        check_injection tx_node "commitment" @@ Client.bake_for_and_wait client
      in
      let* _tzlevel =
        Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 1)
      in
      let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
      check_l2_level block 3 ;
      let* () =
        check_commitments_inclusion
          ~tx_node
          [("0", true); ("1", true); ("2", true); ("3", false)]
      in
      let* () =
        check_commitments_content
          ~tx_node
          ~tx_rollup_hash
          ~client
          ["0"; "1"; "2"; "3"]
      in
      unit)

let test_tickets_context =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: tickets hashes to tickets context"
    ~tags:["tx_rollup"; "tickets"; "context"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* _level, contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:100_000
          bls_pkh_1
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      Log.info "Checking ticket availability in head context" ;
      let* ticket =
        Tx_rollup_node.Client.get_ticket ~tx_node ~block:"head" ~ticket_id
      in
      let expected_ticket =
        JSON.annotate ~origin:"expected"
        @@ `O
             [
               ("ticketer", `String contract_id);
               ("ty", `O [("prim", `String "string")]);
               ("contents", `O [("string", `String "toru")]);
               ("hash", `String ticket_id);
             ]
      in
      Check.(ticket = expected_ticket)
        check_json
        ~error_msg:"Ticket is %L but expected %R" ;
      Log.info "Checking ticket can be retrieved by index" ;
      let* ticket_index =
        Tx_rollup_node.Client.get_ticket_index ~tx_node ~block:"head" ~ticket_id
      in
      let* ticket =
        Tx_rollup_node.Client.get_ticket
          ~tx_node
          ~block:"head"
          ~ticket_id:(string_of_int ticket_index)
      in
      Check.(ticket = expected_ticket)
        check_json
        ~error_msg:"Ticket is %L but expected %R" ;
      Log.info "Submitting transactions to queue" ;
      let* _txh1 =
        inject_transfer
          tx_client
          ~source:bls_key_1.aggregate_alias
          ~dest:bls_pkh_2
          ~ticket:ticket_id
          ~qty:10L
      in
      let* _txh2 =
        inject_transfer
          tx_client
          ~source:bls_key_2.aggregate_alias
          ~dest:bls_pkh_1
          ~ticket:ticket_id
          ~qty:5L
      in
      Log.info "Waiting for new L2 block" ;
      let* () = Client.bake_for_and_wait client in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 6 in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:(100_000 - 5)
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:5
      in
      Log.info "Ticket still available in later contexts" ;
      let* ticket =
        Tx_rollup_node.Client.get_ticket ~tx_node ~block:"head" ~ticket_id
      in
      Check.(ticket = expected_ticket)
        check_json
        ~error_msg:"Ticket is %L but expected %R" ;
      unit)

let test_round_trip ~title ?before_init
    ?(withdraw_dest = Constant.bootstrap2.public_key_hash) ~originator ~operator
    ~batch_signer ~finalize_commitment_signer ~dispatch_withdrawals_signer () =
  Protocol.register_test
    ~__FILE__
    ~title
    ~tags:["tx_rollup"; "dispatch"; "withdrawals"]
    (fun protocol ->
      let* parameter_file =
        Parameters.parameter_file
          ~parameters:Parameters.{finality_period = 2; withdraw_period = 2}
          protocol
      in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let* () = match before_init with None -> unit | Some f -> f client in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~operator
          ~batch_signer
          ~finalize_commitment_signer
          ~dispatch_withdrawals_signer
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      let check_l2_block_finalized block =
        let finalized =
          JSON.(block |-> "metadata" |-> "finalized" |> as_bool)
        in
        Check.((finalized = true) bool) ~error_msg:"L2 Block is not finalized"
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
      let* _level, deposit_contract =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:100_000
          bls_pkh_1
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      Log.info "Submitting transactions to queue" ;
      let* _txh1 =
        inject_transfer
          tx_client
          ~source:bls_key_1.aggregate_alias
          ~dest:bls_pkh_2
          ~ticket:ticket_id
          ~qty:10L
      in
      let* _txh2 =
        inject_transfer
          tx_client
          ~source:bls_key_2.aggregate_alias
          ~dest:bls_pkh_1
          ~ticket:ticket_id
          ~qty:5L
      in
      Log.info "Waiting for new L2 block" ;
      let* () = Client.bake_for_and_wait client in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 6 in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:(100_000 - 5)
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:5
      in
      Log.info "Submitting withdrawals to queue" ;
      let* tx, signature =
        craft_withdraw_and_sign
          tx_client
          ~signer:bls_key_2
          ~dest:withdraw_dest
          ~ticket:ticket_id
          ~qty:5L
      in
      let* _ = tx_client_inject_transaction ~tx_client tx [signature] in
      let* tx, signature =
        craft_withdraw_and_sign
          tx_client
          ~signer:bls_key_1
          ~dest:withdraw_dest
          ~ticket:ticket_id
          ~qty:10L
      in
      let* _ = tx_client_inject_transaction ~tx_client tx [signature] in
      Log.info "Waiting for new L2 block" ;
      let* () = Client.bake_for_and_wait client in
      let* () = Client.bake_for_and_wait client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node 8 in
      let* inbox = Tx_rollup_node.Client.get_inbox ~tx_node ~block:"head" in
      check_inbox_success inbox ;
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1
          ~expected_balance:(100_000 - 5 - 10)
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2
          ~expected_balance:0
      in
      Log.info "Baking 1 L1 block for inclusion of commitment" ;
      let* () = Client.bake_for_and_wait client in
      let* block = RPC.Client.call client @@ RPC.get_chain_block () in
      check_l1_block_contains_commitment ~level:2 block ;
      Log.info "Baking 2 L1 blocks for finalization of commitment" ;
      let* () =
        check_injection tx_node "finalize_commitment"
        @@ Client.bake_for_and_wait client
      in
      let* () =
        check_injection tx_node "dispatch_withdrawals"
        @@ Client.bake_for_and_wait client
      in
      let* block = RPC.Client.call client @@ RPC.get_chain_block () in
      check_l1_block_contains_finalize ~level:2 block ;
      let* l2_head = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
      check_l2_block_finalized l2_head ;
      Log.info "Baking 1 L1 block for dispatch to be included" ;
      let* () = Client.bake_for_and_wait client in
      let* block = RPC.Client.call client @@ RPC.get_chain_block () in
      check_l1_block_contains_dispatch block ;
      Log.info "Originate contract to withdraw tickets" ;
      let* _alias, withdraw_contract =
        Client.originate_contract_at
          ~amount:Tez.zero
          ~src:originator
          ~init:"None"
          ~burn_cap:Tez.one
          client
          ["mini_scenarios"; "tickets_receive_and_store"]
          protocol
      in
      let* () = Client.bake_for_and_wait client in
      Log.info "Transfer the tickets to withdraw contract" ;
      let*! () =
        Client.transfer_tickets
          ~qty:15L
          ~src:withdraw_dest
          ~destination:withdraw_contract
          ~entrypoint:"default"
          ~contents:{|"toru"|}
          ~ty:"string"
          ~ticketer:deposit_contract
          ~burn_cap:Tez.one
          client
      in
      let* () = Client.bake_for_and_wait client in
      unit)

let test_withdrawals =
  test_round_trip
    ~title:"TX_rollup: dispatch withdrawals"
    ~originator:Constant.bootstrap2.public_key_hash
    ~operator:Constant.bootstrap1.public_key_hash
    ~batch_signer:Constant.bootstrap5.public_key_hash
    ~finalize_commitment_signer:Constant.bootstrap4.public_key_hash
    ~dispatch_withdrawals_signer:Constant.bootstrap3.public_key_hash
    ()

let test_single_signer =
  let operator = Constant.bootstrap1.public_key_hash in
  test_round_trip
    ~title:"TX_rollup: single signer for everything"
    ~originator:Constant.bootstrap2.public_key_hash
    ~operator
    ~batch_signer:operator
    ~finalize_commitment_signer:operator
    ~dispatch_withdrawals_signer:operator
    ()

let test_signer_reveals =
  let operator = "operator" in
  let other = Constant.bootstrap5.public_key_hash in
  let before_init client =
    let* _ = Client.gen_keys ~alias:operator client in
    let* () =
      Client.transfer
        ~amount:(Tez.of_int 20_000)
        ~giver:Constant.bootstrap1.public_key_hash
        ~receiver:operator
        ~burn_cap:Tez.one
        client
    in
    Client.bake_for_and_wait client
  in
  test_round_trip
    ~title:"TX_rollup: operator needs to be revealed"
    ~originator:Constant.bootstrap2.public_key_hash
    ~operator
    ~batch_signer:other
    ~finalize_commitment_signer:other
    ~dispatch_withdrawals_signer:other
    ~before_init
    ()

let test_accuser =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: accuser"
    ~tags:["tx_rollup"; "node"; "accuser"; "rejection"]
  @@ fun protocol ->
  let* parameter_file =
    Parameters.parameter_file
      ~parameters:Parameters.{finality_period = 5; withdraw_period = 5}
      protocol
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let originator = Constant.bootstrap2.public_key_hash in
  let operator = Constant.bootstrap1.public_key_hash in
  let* tx_rollup_hash, tx_node =
    (* Starting without committer/operator *)
    init_and_run_rollup_node
      ~protocol
      ~originator
      ~batch_signer:Constant.bootstrap5.public_key_hash
      ~rejection_signer:operator
      node
      client
  in
  (* Generating one identity *)
  let* bls_key_1 = Client.bls_gen_and_show_keys client in
  let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
  let* _level, _deposit_contract =
    make_deposit
      ~protocol
      ~source:Constant.bootstrap2.public_key_hash
      ~tx_rollup_hash
      ~tx_node
      ~client
      ~tickets_amount:100_000
      bls_pkh_1
  in
  let* ({roots = _; context_hashes = _; inbox_merkle_root; predecessor} as
       _commitment_info) =
    build_commitment_info ~tx_level:0 ~tx_rollup_hash ~tx_node ~client
  in
  (* Change the roots to produce an invalid commitment. *)
  let roots = [Constant.tx_rollup_initial_message_result] in
  Log.info "Injecting bad commitment 'by hand'" ;
  let*! () =
    Client.Tx_rollup.submit_commitment
      ~level:0
      ~roots
      ~inbox_merkle_root
      ?predecessor
      ~rollup:tx_rollup_hash
      ~src:Constant.bootstrap3.public_key_hash
      client
  in
  (* Bake for rollup node to see bad commitment and inject rejection *)
  let* () =
    check_injection tx_node "rejection" @@ Client.bake_for_and_wait client
  in
  let* block = RPC.Client.call client @@ RPC.get_chain_block () in
  check_l1_block_contains_commitment ~level:0 block ;
  Log.info "Baking 1 L1 block for rejection to be included" ;
  let* () = Client.bake_for_and_wait client in
  let* block = RPC.Client.call client @@ RPC.get_chain_block () in
  let* _ =
    RPC.Client.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  check_l1_block_contains_rejection ~level:0 block ;
  unit

let test_batcher_large_message =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: submit a large message to the batcher"
    ~tags:["tx_rollup"; "node"; "batcher"]
    (fun protocol ->
      let* parameter_file =
        Parameters.parameter_file
          ~parameters:Parameters.{finality_period = 5; withdraw_period = 5}
          protocol
      in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* _tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      let* bls_key = Client.bls_gen_and_show_keys client in
      let pkh1_str = bls_key.aggregate_public_key_hash in
      let contents : Rollup.l2_transfer list =
        let transfer_content : Rollup.l2_transfer =
          let destination = pkh1_str in
          let ticket =
            Tezos_protocol_alpha.Protocol.Alpha_context.Ticket_hash.(
              to_b58check zero)
          in
          let qty = 1L in
          `Transfer {destination; ticket; qty}
        in
        List.init 200 (fun _ -> transfer_content)
      in
      let* tx, signature =
        craft_tx_transfers_and_sign
          ~counter:1L
          ~signer:bls_key
          tx_client
          contents
      in
      let* _ =
        tx_client_inject_transaction
          ~tx_client
          tx
          [signature]
          ~failswith:"tx_rollup.node.transaction_too_large"
      in
      unit)

let test_transfer_command =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: inject transaction with transfer command"
    ~tags:["tx_rollup"; "client"; "transfer"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let* bls_key_2 = Client.bls_gen_and_show_keys client in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:10
          bls_key_1.aggregate_public_key_hash
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      let* _ =
        inject_transfer
          tx_client
          ~source:bls_key_1.aggregate_alias
          ~qty:1L
          ~dest:bls_key_2.aggregate_alias
          ~ticket:ticket_id
      in
      let* () = Client.bake_for_and_wait client in
      let* () = Client.bake_for_and_wait client in
      let* level = Client.level client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node level in
      let* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_key_1.aggregate_public_key_hash
          ~expected_balance:9
      and* () =
        check_tz4_balance
          ~tx_client
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_key_2.aggregate_public_key_hash
          ~expected_balance:1
      in
      unit)

let test_withdraw_command =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: inject transaction with withdraw command"
    ~tags:["tx_rollup"; "client"; "withdraw"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let originator = Constant.bootstrap1.public_key_hash in
      let* tx_rollup_hash, tx_node =
        init_and_run_rollup_node
          ~protocol
          ~originator
          ~batch_signer:Constant.bootstrap5.public_key_hash
          node
          client
      in
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      (* Generating some identities *)
      let* bls_key_1 = Client.bls_gen_and_show_keys client in
      let* _level, _contract_id =
        make_deposit
          ~protocol
          ~source:Constant.bootstrap2.public_key_hash
          ~tx_rollup_hash
          ~tx_node
          ~client
          ~tickets_amount:10
          bls_key_1.aggregate_public_key_hash
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      let* _ =
        inject_withdraw
          ~counter:1L
          tx_client
          ~source:bls_key_1.aggregate_alias
          ~qty:1L
          ~dest:Constant.bootstrap2.alias
          ~ticket:ticket_id
      in
      let* _ =
        let dest =
          match protocol with
          | Lima -> Constant.bootstrap2.public_key_hash
          | Mumbai | Nairobi | Alpha -> Constant.tz4_account.public_key_hash
        in
        inject_withdraw
          ~counter:2L
          tx_client
          ~source:bls_key_1.aggregate_alias
          ~qty:1L
          ~dest
          ~ticket:ticket_id
      in
      let* () = Client.bake_for_and_wait client in
      let* () = Client.bake_for_and_wait client in
      let* level = Client.level client in
      let* _ = Tx_rollup_node.wait_for_tezos_level tx_node level in
      check_tz4_balance
        ~tx_client
        ~block:"head"
        ~ticket_id
        ~tz4_address:bls_key_1.aggregate_public_key_hash
        ~expected_balance:8)

let test_catch_up =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: catch-up on commitments"
    ~tags:["tx_rollup"; "node"; "catch_up"; "commitments"]
  @@ fun protocol ->
  let* parameter_file =
    Parameters.parameter_file
      ~parameters:Parameters.{finality_period = 5; withdraw_period = 5}
      protocol
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let originator = Constant.bootstrap2.public_key_hash in
  let operator = Constant.bootstrap1.public_key_hash in
  let* tx_rollup_hash, tx_node =
    (* Starting without committer/operator *)
    init_and_run_rollup_node
      ~protocol
      ~originator
      ~batch_signer:Constant.bootstrap5.public_key_hash
      node
      client
  in
  let tx_client =
    Tx_rollup_client.create
      ~protocol
      ~wallet_dir:(Client.base_dir client)
      tx_node
  in
  (* Generating some identities *)
  let* bls_key_1 = Client.bls_gen_and_show_keys client in
  let bls_pkh_1 = bls_key_1.aggregate_public_key_hash in
  let* bls_key_2 = Client.bls_gen_and_show_keys client in
  let bls_pkh_2 = bls_key_2.aggregate_public_key_hash in
  let* tzlevel, _deposit_contract =
    make_deposit
      ~protocol
      ~source:Constant.bootstrap2.public_key_hash
      ~tx_rollup_hash
      ~tx_node
      ~client
      ~tickets_amount:100_000
      bls_pkh_1
  in
  let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
  let ticket_id = get_ticket_hash_from_deposit_json inbox in
  Log.info "Ticket %s was successfully emitted" ticket_id ;
  let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
  check_l2_level block 0 ;
  let inject_tx ?counter ~from ~dest ?(amount = 1L) () =
    inject_transfer
      tx_client
      ~qty:amount
      ?counter
      ~source:from.Account.aggregate_alias
      ~dest
      ~ticket:ticket_id
  in
  Log.info "Making L1 block 1" ;
  let* _ = inject_tx ~from:bls_key_1 ~dest:bls_pkh_2 ~amount:1000L () in
  let* _ = inject_tx ~from:bls_key_2 ~dest:bls_pkh_1 ~amount:2L () in
  let* () = Client.bake_for_and_wait client in
  let* () = Client.bake_for_and_wait client in
  let* tzlevel = Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 2) in
  let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
  check_l2_level block 1 ;
  Log.info "Making L1 block 2" ;
  let* _ = inject_tx ~from:bls_key_1 ~dest:bls_pkh_2 ~amount:3L () in
  let* _ = inject_tx ~from:bls_key_2 ~dest:bls_pkh_1 ~amount:4L () in
  let* () = Client.bake_for_and_wait client in
  let* () = Client.bake_for_and_wait client in
  let* _tzlevel = Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 2) in
  let* block = Tx_rollup_node.Client.get_block ~tx_node ~block:"head" in
  check_l2_level block 2 ;
  Log.info "Check no block is committed" ;
  let* () =
    check_commitments_inclusion
      ~tx_node
      [("0", false); ("1", false); ("2", false)]
  in
  Log.info "Stopping rollup node" ;
  let* () = Tx_rollup_node.terminate tx_node in
  Log.info "Restarting rollup node with committer/operator" ;
  let* () =
    Tx_rollup_node.change_signers
      ~operator:(Some operator)
      ~allow_deposit:true
      tx_node
  in
  let* () = Tx_rollup_node.run tx_node in
  let* () = Tx_rollup_node.wait_for_ready tx_node in
  (* Baking one block to make sure catch up phase is over *)
  let* () = Client.bake_for_and_wait client in
  let* () = Client.bake_for_and_wait client in
  let* _tzlevel = Tx_rollup_node.wait_for_tezos_level tx_node (tzlevel + 2) in
  let* () =
    check_commitments_inclusion ~tx_node [("0", true); ("1", true); ("2", true)]
  in
  unit

let test_origination_deposit_same_block =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: origination and first deposit in same block"
    ~tags:["tx_rollup"; "origination"; "genesis"; "deposit"]
    (fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* node, client =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let* _alias, contract_id =
        Client.originate_contract_at
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
          ["mini_scenarios"; "tx_rollup_deposit"]
          protocol
      in
      let* () = Client.bake_for_and_wait client in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      let originator = Constant.bootstrap1.public_key_hash in
      Log.info "Originating rollup" ;
      let*! tx_rollup_hash =
        Client.Tx_rollup.originate
          ~fee:(Tez.of_int 100)
            (* High fee to ensure the origination appears in the block before the
               deposit *)
          ~src:originator
          ~alias:"tx_rollup"
          client
      in
      (* Do not bake after origination, we will also inject deposit *)
      let tx_node =
        Tx_rollup_node.create
          ~protocol
          Observer
          ~rollup_id:tx_rollup_hash
          ~allow_deposit:false
          client
          node
      in
      let* _ = Tx_rollup_node.init_config tx_node in
      let* bls_key = Client.bls_gen_and_show_keys client in
      let tickets_content = "toru" in
      let tickets_amount = 10 in
      Log.info "Making deposit" ;
      let arg =
        make_tx_rollup_deposit_argument
          tickets_content
          tickets_amount
          bls_key.aggregate_public_key_hash
          tx_rollup_hash
      in
      (* This smart contract call will transfer 10 tickets to the given
         address. *)
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:Constant.bootstrap2.alias
          ~receiver:contract_id
          ~arg
          client (* Must not simulate because rollup does not yet exist on L1 *)
          ~simulation:false
          ~force:true
      in
      let* () = Client.bake_for_and_wait client in
      let* block = RPC.Client.call client @@ RPC.get_chain_block () in
      Log.info
        "Checking that block contains both the rollup origination and the \
         deposit" ;
      check_l1_block_contains
        block
        ~kind:"tx_rollup_origination"
        ~what:"Origination of rollup" ;
      check_l1_block_contains
        block
        ~kind:"transaction"
        ~what:"Ticket deposit to L2" ;
      Log.info "Starting Rollup node" ;
      let* () = Tx_rollup_node.run tx_node in
      let* () = Tx_rollup_node.wait_for_ready tx_node in
      Log.info "Tx_rollup node is now ready" ;
      let* _ = wait_tezos_node_level tx_node node in
      (* Get the operation containing the ticket transfer. We assume
         that only one operation is issued in this block. *)
      let tx_client =
        Tx_rollup_client.create
          ~protocol
          ~wallet_dir:(Client.base_dir client)
          tx_node
      in
      let* inbox = tx_client_get_inbox_as_json ~tx_client ~block:"head" in
      let ticket_id = get_ticket_hash_from_deposit_json inbox in
      Log.info "Ticket %s was successfully deposited" ticket_id ;
      check_tz4_balance
        ~tx_client
        ~block:"head"
        ~ticket_id
        ~tz4_address:bls_key.aggregate_public_key_hash
        ~expected_balance:10)

let register ~protocols =
  test_node_configuration protocols ;
  test_not_allow_deposit protocols ;
  test_allow_deposit protocols ;
  test_tx_node_origination protocols ;
  test_tx_node_store_inbox protocols ;
  test_node_cannot_connect protocols ;
  test_node_disconnect protocols ;
  test_ticket_deposit_from_l1_to_l2 protocols ;
  test_l2_to_l2_transaction protocols ;
  test_batcher ~test_persistence:false protocols ;
  test_batcher ~test_persistence:true protocols ;
  test_reorganization protocols ;
  test_l2_proof_rpc_position protocols ;
  test_reject_bad_commitment protocols ;
  test_committer protocols ;
  test_tickets_context protocols ;
  test_withdrawals protocols ;
  test_single_signer protocols ;
  test_signer_reveals protocols ;
  test_accuser protocols ;
  test_batcher_large_message protocols ;
  test_transfer_command protocols ;
  test_withdraw_command protocols ;
  test_catch_up protocols ;
  test_origination_deposit_same_block protocols
