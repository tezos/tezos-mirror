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
   Component: Mempool
   Invocation: dune exec tezt/tests/main.exe -- --file mempool.ml
   Subject: .
*)

let check_operation_is_in_applied_mempool ops oph =
  let open JSON in
  let ops_list = as_list (ops |-> "applied") in
  let res =
    List.exists (fun e -> e |-> "hash" |> as_string = as_string oph) ops_list
  in
  if not res then
    Test.fail "Operation %s was not found in the mempool" (JSON.encode oph)

type mempool_count = {
  applied : int;
  branch_delayed : int;
  branch_refused : int;
  refused : int;
  unprocessed : int;
  total : int;
}

let count_mempool mempool =
  let open JSON in
  let applied = as_list (mempool |-> "applied") |> List.length in
  let branch_delayed = as_list (mempool |-> "branch_delayed") |> List.length in
  let branch_refused = as_list (mempool |-> "branch_refused") |> List.length in
  let refused = as_list (mempool |-> "refused") |> List.length in
  let unprocessed = as_list (mempool |-> "unprocessed") |> List.length in
  let total =
    applied + branch_delayed + branch_refused + refused + unprocessed
  in
  {applied; branch_delayed; branch_refused; refused; unprocessed; total}

let pp_mempool_count fmt
    {applied; branch_delayed; branch_refused; refused; unprocessed; total} =
  Format.fprintf
    fmt
    "total: %d - applied: %d, branch_delayed: %d, branch_refused: %d, refused: \
     %d, unprocessed: %d"
    total
    applied
    branch_delayed
    branch_refused
    refused
    unprocessed

(* Matches events which contain an injection request.
   For example:

     "event": {
       "request": {
         "request": "inject",
         "operation": {
           "branch": "BL2FDpiSbzxkXpefiSRCpBHGhZ1kDpEUzWswSCABvGKr3hF6xre",
           "data": "6c0002298c03ed7d454a101eb7022bc95f7e5f41ac78940a0280bd3f00e8070000e7670f32038107a59a2b9cfefae36ea21f5aa63c00cf958f834a8d89a88068d7da1209db3c8dc6f5a0c88fb7df0fc8b910f5e100c1179e0862993fd2abadcc47eb4710ad41b68603983559b5fb68bb98499aa1800d"
         }
       },
       "status": {
         "pushed": "2021-05-03T17:16:03.826-00:00",
         "treated": 3.0033e-05,
         "completed": 0.00190934
       }
     },
     "level": "notice"
   }
 *)
let wait_for_injection node =
  let filter json =
    match
      JSON.(json |=> 1 |-> "event" |-> "request" |-> "request" |> as_string_opt)
    with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "node_prevalidator.v0" filter in
  return ()

(* Matches events which contain an flush request.
   For example:

     "event": {
       "request": {
         "request": "flush",
         "block": "BLTv3VhCAVzMVxbXhTRqGf6M7oyxeeH2eBzdf9onbD9ULyFgo7d"
       },
       "status": {
         "pushed": "2021-04-26T16:00:50.859-00:00",
         "treated": 4.5676e-05,
         "completed": 0.01316594
       }
     },
     "level": "notice"
 *)
let wait_for_flush node =
  let filter json =
    match
      JSON.(json |=> 1 |-> "event" |-> "request" |-> "request" |> as_string_opt)
    with
    | Some s when s = "flush" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "node_prevalidator.v0" filter in
  return ()

let operation_json ~fee ~gas_limit ~source ~destination ~counter =
  Format.sprintf
    {|{
             "kind": "transaction",
             "source": "%s",
             "fee": "%d",
             "counter": "%d",
             "gas_limit": "%d",
             "storage_limit": "0",
             "amount": "1000",
             "destination": "%s"}|}
    source
    fee
    counter
    gas_limit
    destination

let operation_json_branch ~branch operations_json =
  Format.sprintf
    {|{"branch": "%s",
           "contents": [%s]}|}
    branch
    operations_json

let sign_operation_bytes (signer : Constant.key) (msg : Bytes.t) =
  let open Tezos_crypto in
  let b58_secret_key =
    match String.split_on_char ':' signer.secret with
    | ["unencrypted"; rest] -> rest
    | _ -> Test.fail "Could not parse secret key"
  in
  let sk = Signature.Secret_key.of_b58check_exn b58_secret_key in
  Signature.(sign ~watermark:Generic_operation sk msg)

let forge_and_inject_operation ~branch ~fee ~gas_limit ~source ~destination
    ~counter ~signer ~client =
  let op_json = operation_json ~fee ~gas_limit ~source ~destination ~counter in
  let op_json_branch = operation_json_branch ~branch op_json in
  let* op_hex =
    RPC.post_forge_operations ~data:(Ezjsonm.from_string op_json_branch) client
  in
  let op_str_hex = JSON.as_string op_hex in
  let signature =
    sign_operation_bytes signer (Hex.to_bytes (`Hex op_str_hex))
  in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  let signed_op = op_str_hex ^ signature in
  let* res = RPC.inject_operation ~data:(`String signed_op) client in
  return (Some res)

let forge_and_inject_n_operations ~branch ~fee ~gas_limit ~source ~destination
    ~counter ~signer ~client ~node n =
  let rec loop ((oph_list, counter) as acc) = function
    | 0 -> return acc
    | n ->
        let transfer_1 = wait_for_injection node in
        let* oph =
          forge_and_inject_operation
            ~branch
            ~fee
            ~gas_limit
            ~source
            ~destination
            ~counter
            ~signer
            ~client
        in
        let* () = transfer_1 in
        let oph_list =
          match oph with None -> oph_list | Some oph -> oph :: oph_list
        in
        loop (oph_list, counter + 1) (pred n)
  in
  loop ([], counter + 1) n

(* This test tries to manually inject some operations

   Scenario:

   1. Node 1 activates a protocol

   2. Retrieve the counter and the branch for bootstrap1

   3. Forge and inject <n> operations in the node

   4. Check that the operations are in the mempool

   5. Bake an empty block

   6. Check that we did not lose any operations in the flush

   7. Inject an endorsement

   8. Check that we have one more operation in the mempool and that
      the endorsement is applied

   9. Bake an empty block

   8. Check that we did not lose any operations in the flush
 *)

let flush_mempool =
  Protocol.register_test
    ~__FILE__
    ~title:"Flush mempool"
    ~tags:["flush"; "mempool"]
  @@ fun protocol ->
  (* Step 1 *)
  (* A Node is started and we activate the protocol and wait the node to be synced *)
  let* node_1 = Node.init [Synchronisation_threshold 0] in
  let endpoint_1 = Client.(Node node_1) in
  let* client_1 = Client.init ~endpoint:endpoint_1 () in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 in
  Log.info "Node is at level %d." 1 ;
  (* Step 2 *)
  (* Get the counter and the current branch *)
  let* counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int counter in
  let* branch = RPC.get_branch client_1 in
  let branch = JSON.as_string branch in
  (* Step 3 *)
  (* Forge operation, inject them and check injection *)
  let number_of_transactions =
    (* the batch of operation that a flush handles is 50 by default,
       we choose a value that by-pass this limit *)
    (* TODO: pass a lower limit to the node config init so we may inject less operations *)
    60
  in
  let* (ophs, _counter) =
    forge_and_inject_n_operations
      ~branch
      ~fee:1000 (* Minimal fees to successfully apply the transfer *)
      ~gas_limit:1040 (* Minimal gas to successfully apply the transfer *)
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter
      ~signer:Constant.bootstrap1
      ~client:client_1
      ~node:node_1
      number_of_transactions
  in
  (* Step 4 *)
  (* Check that forged operation are in the mempool *)
  let* mempool_after_injections = RPC.get_mempool_pending_operations client_1 in
  let mempool_count_after_injections = count_mempool mempool_after_injections in
  Format.kasprintf
    (Log.info "%s")
    "Mempool count after injections: %a"
    pp_mempool_count
    mempool_count_after_injections ;
  List.iter
    (fun oph ->
      check_operation_is_in_applied_mempool mempool_after_injections oph)
    ophs ;
  Log.info "Every forged operation are applied in the mempool" ;
  (* Step 5 *)
  (* Bake with an empty mempool to force synchronisation *)
  let empty_mempool_file = Temp.file "mempool.json" in
  let* _ =
    let empty_mempool =
      {|{"applied":[],"refused":[],"branch_refused":[],"branch_delayed":[],"unprocessed":[]}"|}
    in
    Lwt_io.with_file ~mode:Lwt_io.Output empty_mempool_file (fun oc ->
        Lwt_io.write oc empty_mempool)
  in
  let flush_waiter = wait_for_flush node_1 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter in
  (* Step 6 *)
  (* Inject endorsement operation *)
  let* mempool_before_endorsement =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_before_endorsement =
    count_mempool mempool_before_endorsement
  in
  Format.kasprintf
    (Log.info "%s")
    "Mempool count before endorsement: %a"
    pp_mempool_count
    mempool_count_before_endorsement ;
  (* Check that we did not lost any operation during the flush *)
  if
    mempool_count_after_injections.total
    <> mempool_count_before_endorsement.total
  then
    Test.fail
      "Operations were lost after the flush: expected %d, got %d"
      mempool_count_after_injections.total
      mempool_count_before_endorsement.total ;
  let* () = Client.endorse_for client_1 in
  Log.info "Endorsement injected" ;
  let* mempool_after_endorsement =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_after_endorsement =
    count_mempool mempool_after_endorsement
  in
  Format.kasprintf
    (Log.info "%s")
    "Mempool count after endorsement: %a"
    pp_mempool_count
    mempool_count_after_endorsement ;
  (* Check that we have one more operation and that it was correctly applied *)
  if
    succ mempool_count_before_endorsement.total
    <> mempool_count_after_endorsement.total
  then
    Test.fail
      "Operations were lost after injecting the endorsement: expected %d, got \
       %d"
      (succ mempool_count_before_endorsement.total)
      mempool_count_after_endorsement.total ;
  if
    succ mempool_count_before_endorsement.applied
    <> mempool_count_after_endorsement.applied
  then Test.fail "Endorsement was not applied" ;
  (* Step 7 *)
  (* Bake with an empty mempool to force synchronisation *)
  let flush_waiter = wait_for_flush node_1 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter in
  Log.info "Baking done and mempool flushed" ;
  let* mempool_after_second_flush =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_after_second_flush =
    count_mempool mempool_after_second_flush
  in
  Format.kasprintf
    (Log.info "%s")
    "Mempool count after second flush: %a"
    pp_mempool_count
    mempool_count_after_second_flush ;
  (* Check that we did not lost any operation during the second flush *)
  if
    mempool_count_after_endorsement.total
    <> mempool_count_after_second_flush.total
  then
    Test.fail
      "Operations were lost after the second flush: expected %d, got %d"
      mempool_count_after_endorsement.total
      mempool_count_after_second_flush.total ;
  unit

(* TODO: add a test than ensure that we cannot have more than 1000
   branch delayed/branch refused/refused *)

let forge_run_and_inject_n_batched_operation n ~branch ~fee ~gas_limit ~source
    ~destination ~counter ~signer ~client =
  let ops_json =
    String.concat ", "
    @@ List.map
         (fun counter ->
           operation_json ~fee ~gas_limit ~source ~destination ~counter)
         (range (counter + 1) (counter + n))
  in
  let op_json_branch = operation_json_branch ~branch ops_json in
  let* op_hex =
    RPC.post_forge_operations ~data:(Ezjsonm.from_string op_json_branch) client
  in
  let op_str_hex = JSON.as_string op_hex in
  let signature =
    sign_operation_bytes signer (Hex.to_bytes (`Hex op_str_hex))
  in
  let* _run =
    let* chain_id = RPC.get_chain_id client in
    let op_runnable =
      Format.asprintf
        {|{ "operation":
            {"branch": "%s",
             "contents": [ %s ],
             "signature": "%a" },
            "chain_id": %s }|}
        branch
        ops_json
        Tezos_crypto.Signature.pp
        signature
        (JSON.encode chain_id)
    in
    RPC.post_run_operation ~data:(Ezjsonm.from_string op_runnable) client
  in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  let signed_op = op_str_hex ^ signature in
  let* res = RPC.inject_operation ~data:(`String signed_op) client in
  return res

let check_batch_operations_are_in_applied_mempool ops oph n =
  let open JSON in
  let ops_list = as_list (ops |-> "applied") in
  let res =
    List.exists
      (fun e ->
        let contents = as_list (e |-> "contents") in
        let h = as_string (e |-> "hash") in
        List.length contents = n && h = as_string oph)
      ops_list
  in
  if not res then
    Test.fail
      "Batch Operation %s was not found in the mempool or it does not contain \
       %d operations"
      (JSON.encode oph)
      n

(* This test tries to run manually forged operations before injecting them

   Scenario:

   1. Node 1 activates a protocol

   2. Retrieve the counter and the branch for bootstrap1

   3. Forge, run and inject <n> operations in the node

   4. Check that the batch is correctly injected
 *)
let run_batched_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"Run batched operations before injecting them"
    ~tags:["forge"; "mempool"; "batch"; "run_operation"]
  @@ fun protocol ->
  (* Step 1 *)
  (* A Node is started and we activate the protocol and wait for the node to be synced *)
  let* node_1 = Node.init [Synchronisation_threshold 0] in
  let* client_1 = Client.init ~endpoint:(Node node_1) () in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 in
  Log.info "Node is at level %d." 1 ;
  (* Step 2 *)
  (* Get the counter and the current branch *)
  let* counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int counter in
  let* branch = RPC.get_branch client_1 in
  let branch = JSON.as_string branch in
  (* Step 3 *)
  (* Forge operations, run and inject them *)
  let number_of_transactions = 3 in
  let* oph =
    forge_run_and_inject_n_batched_operation
      number_of_transactions
      ~branch
      ~fee:1000 (* Minimal fees to successfully apply the transfer *)
      ~gas_limit:1040 (* Minimal gas to successfully apply the transfer *)
      ~source:Constant.bootstrap2.identity
      ~destination:Constant.bootstrap1.identity
      ~counter
      ~signer:Constant.bootstrap2
      ~client:client_1
  in
  Log.info "Operations forged, signed, run and injected" ;
  (* Step 4 *)
  (* Check that the batch is correctly injected *)
  let* mempool_after_batch = RPC.get_mempool_pending_operations client_1 in
  check_batch_operations_are_in_applied_mempool
    mempool_after_batch
    oph
    number_of_transactions ;
  Log.info
    "%d operations are applied as a batch in the mempool"
    number_of_transactions ;
  unit

let get_endorsement_hash ops =
  let open JSON in
  let ops_list = as_list (ops |-> "applied") in
  match ops_list with
  | [op] -> op |-> "hash" |> as_string
  | _ -> Test.fail "Only one operation must be applied"

let check_if_op_is_branch_refused ops oph =
  let open JSON in
  let ops_list = as_list (ops |-> "branch_refused") in
  match ops_list with
  | [br] -> (
      match as_list br with
      | br_oph :: _ ->
          let br_oph = as_string br_oph in
          let res = br_oph = oph in
          if not res then
            Test.fail "Found %s in branch_refused instead of %s" br_oph oph
      | [] ->
          (* Can't happen *)
          assert false)
  | _ -> Test.fail "Only one operation must be branch_refused1"

(* This test checks that branch_refused endorsement are still propagated

   Scenario:

   1. 3 Nodes are chained connected and activate a protocol

   2. Disconnect node_1 from node_2 and bake on both node.

   3. Reconnect node_1 and node_2

   4. Endorse on node_1

   5. Check that endorsement is applied on node_1 and refused on node_2 and node_3
*)
let endorsement_flushed_branch_refused =
  Protocol.register_test
    ~__FILE__
    ~title:"Ensure that branch_refused endorsement are transmited"
    ~tags:["endorsement"; "mempool"; "branch_refused"]
  @@ fun protocol ->
  (* Step 1 *)
  (* 3 Nodes are started and we activate the protocol and wait the nodes to be synced *)
  let* node_1 = Node.init [Bootstrap_threshold 0; Private_mode]
  and* node_2 = Node.init [Bootstrap_threshold 0; Private_mode]
  and* node_3 = Node.init [Bootstrap_threshold 0; Private_mode] in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) ()
  and* client_3 = Client.init ~endpoint:(Node node_3) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1
  and* () = Client.Admin.trust_address client_2 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2
  and* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1
  and* _ = Node.wait_for_level node_2 1
  and* _ = Node.wait_for_level node_3 1 in
  Log.info "All nodes are at level %d." 1 ;
  (* Step 2 *)
  (* Disconnect node_1 and node_2 and bake on both node. This will force different branches *)
  let* node_2_id = Node.wait_for_identity node_2
  and* node_1_id = Node.wait_for_identity node_1 in
  let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id
  and* () = Client.Admin.kick_peer client_2 ~peer:node_1_id in
  let bake_waiter_1 = wait_for_flush node_1
  and bake_waiter_2 = wait_for_flush node_2 in
  let* () = Client.bake_for client_1
  and* () = Client.bake_for ~key:Constant.bootstrap3.identity client_2 in
  let* () = bake_waiter_1 and* () = bake_waiter_2 in
  (* Step3 *)
  (* Reconnect node_1 and node_2 *)
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  (* Step 4 *)
  (* Endorse on node_1 *)
  let endorser_waiter = wait_for_injection node_1 in
  let* () = Client.endorse_for client_1 in
  let* () = endorser_waiter in
  Log.info "Endorsement on node_1 done" ;
  (* Step 5 *)
  (* Check that endorsement is applied on node_1 and refused on node_2 and node_3 *)
  let* pending_op_1 = RPC.get_mempool_pending_operations client_1 in
  let oph = get_endorsement_hash pending_op_1 in
  Log.info "Endorsement found in node_1 applied mempool" ;
  let* pending_op_2 = RPC.get_mempool_pending_operations client_2 in
  let () = check_if_op_is_branch_refused pending_op_2 oph in
  Log.info "Endorsement found in branch_refused of node_2 mempool" ;
  (* The only way node_3 gets the endorsement is that node_2 has
     propagated the operation. *)
  let* pending_op_3 = RPC.get_mempool_pending_operations client_3 in
  let () = check_if_op_is_branch_refused pending_op_3 oph in
  Log.info "Endorsement found in branch_refused of node_3 mempool" ;
  unit

let check_empty_operation__ddb ddb =
  let open JSON in
  let op_db_length = as_int (ddb |-> "operation_db" |-> "table_length") in
  if op_db_length > 0 then
    Test.fail
      "Operation Ddb should be empty, contains : %d elements"
      op_db_length

(* This test checks that pre-filtered operations are cleaned from the ddb

   Scenario:

   1. 3 Nodes are chained connected and activate a protocol

   2. Get the counter and the current branch

   3. Forge operation, inject it and check injection on node_1
      This operation is pre-filtered on node_2

   4. Bake 1 block

   5. Get client_2 ddb and check that it contains no operation
*)
let forge_pre_filtered_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"Forge pre-filtered operation and check mempool"
    ~tags:["forge"; "mempool"; "pre_filtered"]
  @@ fun protocol ->
  (* Step 1 *)
  (* Two Nodes are started and we activate the protocol and wait the nodes to be synced *)
  let* node_1 = Node.init [Bootstrap_threshold 0; Private_mode]
  and* node_2 = Node.init [Bootstrap_threshold 0; Private_mode] in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  Log.info "All nodes are at level %d." 1 ;
  (* Step 2 *)
  (* Get the counter and the current branch *)
  let* base_counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int base_counter in
  let* branch = RPC.get_branch client_1 in
  (* Step 3 *)
  (* Forge operation, inject it and check injection *)
  let* _op =
    forge_and_inject_operation
      ~branch:(JSON.as_string branch)
      ~fee:1
      ~gas_limit:1040000
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter:(counter + 1)
      ~signer:Constant.bootstrap1
      ~client:client_1
  in
  Log.info "Op forged and injected" ;
  (* Step 4 *)
  (* Bake 1 block *)
  let* () = Client.bake_for client_2 in
  (* Step 5 *)
  (* Get client_2 ddb and check that it contains no operation *)
  let* ddb2 = RPC.get_ddb client_2 in
  check_empty_operation__ddb ddb2 ;
  Log.info "Operation Ddb of client_2 does not contain any operation" ;
  unit

let register ~protocols =
  flush_mempool ~protocols ;
  run_batched_operation ~protocols ;
  (* TODO: The following test was broken by changes in semantics
     from the 9.7 fixes and needs to be fixed. *)
  (* endorsement_flushed_branch_refused ~protocols ; *)
  forge_pre_filtered_operation ~protocols
