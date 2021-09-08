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
   Component:    Mempool
   Invocation:   dune exec tezt/tests/main.exe -- --file mempool.ml
   Subject:      .
*)

(* FIXME https://gitlab.com/tezos/tezos/-/issues/1657

   Some refactorisation is needed. *)

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

(** Matches events which contain an injection request.
   For example:

  {[
    { "event": {
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
  ]}
 *)
let wait_for_injection node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_notice.v0" filter in
  return ()

(** Matches events which contain an flush request.
   For example:

  {[
    { "event": {
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
    }
  ]}
*)
let wait_for_flush node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "flush" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_notice.v0" filter in
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

let sign_operation ~signer op_str_hex =
  let signature =
    sign_operation_bytes signer (Hex.to_bytes (`Hex op_str_hex))
  in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  signature

let forge_operation ~branch ~fee ~gas_limit ~source ~destination ~counter
    ~client =
  let op_json = operation_json ~fee ~gas_limit ~source ~destination ~counter in
  let op_json_branch = operation_json_branch ~branch op_json in
  let* op_hex =
    RPC.post_forge_operations ~data:(Ezjsonm.from_string op_json_branch) client
  in
  return (JSON.as_string op_hex)

let inject_operation ~client op_str_hex signature =
  let signed_op = op_str_hex ^ signature in
  let* res = RPC.inject_operation ~data:(`String signed_op) client in
  return res

let forge_and_inject_operation ~branch ~fee ~gas_limit ~source ~destination
    ~counter ~signer ~client =
  let* op_str_hex =
    forge_operation
      ~branch
      ~fee
      ~gas_limit
      ~source
      ~destination
      ~counter
      ~client
  in
  let signature = sign_operation ~signer op_str_hex in
  let* oph = inject_operation ~client op_str_hex signature in
  return oph

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
        let oph_list = oph :: oph_list in
        loop (oph_list, counter + 1) (pred n)
  in
  loop ([], counter + 1) n

(** This test tries to manually inject some operations

   Scenario:

   + Node 1 activates a protocol

   + Retrieve the counter and the branch for bootstrap1

   + Forge and inject <n> operations in the node

   + Check that the operations are in the mempool

   + Bake an empty block

   + Check that we did not lose any operations in the flush

   + Inject an endorsement

   + Check that we have one more operation in the mempool and that
      the endorsement is applied

   + Bake an empty block

   + Check that we did not lose any operations in the flush
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
  (* Bake twice with an empty mempool to force synchronisation *)
  let flush_waiter = wait_for_flush node_1 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter in
  let flush_waiter = wait_for_flush node_1 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter in
  Log.info "Baking done and mempool flushed" ;
  let* mempool_after_third_flush =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_after_third_flush =
    count_mempool mempool_after_third_flush
  in
  Format.kasprintf
    (Log.info "%s")
    "Mempool count after third flush: %a"
    pp_mempool_count
    mempool_count_after_third_flush ;
  let error_msg =
    "An operation in the mempool was lost after the third flush: expected %L, \
     got %R"
  in
  Check.(
    (mempool_count_after_endorsement.total
   = mempool_count_after_third_flush.total)
      int
      ~error_msg) ;
  (* Check that we did not lost the endorsement after the third flush
     because it was classified as refused. *)
  let error_msg =
    "the endorsement was declared as outdated (classified as refused) after \
     the third flush: expected %L, got %R"
  in
  Check.((mempool_count_after_third_flush.refused = 1) int ~error_msg) ;
  unit

(** This test tries to ban an operation and check that a branch
   refused operation is classified again.

   Scenario:

   + Node 1 activates a protocol and synchronise with Node 2

   + Forge and inject an operation on Node 1 and Node 2 with the same
   source and same counter

   + Check that the operations are in the mempool

   + Bake an empty block to enforce mempool synchronisation between Node 1 and Node 2

   + Ban the operation which was applied on Node 1 (or Node 2)

   + Check that the other operation which was branch refused becomes applied *)
let ban_operation_branch_refused_reevaluated =
  Protocol.register_test
    ~__FILE__
    ~title:"ban_operation_branch_refused_reevaluated"
    ~tags:["flush"; "mempool"; "ban"]
  @@ fun protocol ->
  let* node_1 = Node.init [Synchronisation_threshold 0] in
  let* node_2 = Node.init [Synchronisation_threshold 0] in
  let endpoint_1 = Client.(Node node_1) in
  let endpoint_2 = Client.(Node node_2) in
  let* client_1 = Client.init ~endpoint:endpoint_1 () in
  let* client_2 = Client.init ~endpoint:endpoint_2 () in
  let* () = Client.Admin.connect_address ~peer:node_2 client_1 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "activated protocol" ;
  let* _ = Node.wait_for_level node_1 1 in
  let* _ = Node.wait_for_level node_2 1 in
  let* node2_identity = Node.wait_for_identity node_2 in
  let* () = Client.Admin.kick_peer ~peer:node2_identity client_1 in
  Log.info "nodes are at level 1" ;
  let* counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int counter in
  let* branch = RPC.get_branch client_1 in
  let branch = JSON.as_string branch in
  let injection_waiter = wait_for_injection node_1 in
  let* oph =
    forge_and_inject_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter:(counter + 1)
      ~signer:Constant.bootstrap1
      ~client:client_1
  in
  let* () = injection_waiter in
  Log.info "%s injected on node 1" JSON.(oph |> as_string) ;
  let* mempool_after_injections = RPC.get_mempool_pending_operations client_1 in
  check_operation_is_in_applied_mempool mempool_after_injections oph ;
  Log.info "Forged operation are applied in the mempool" ;
  let injection_waiter = wait_for_injection node_2 in
  let* oph2 =
    forge_and_inject_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap3.identity
      ~counter:(counter + 1)
      ~signer:Constant.bootstrap1
      ~client:client_2
  in
  let* () = injection_waiter in
  Log.info
    "%s injected on node 2 with the same counter as %s"
    JSON.(oph2 |> as_string)
    JSON.(oph |> as_string) ;
  let* () = Client.Admin.connect_address ~peer:node_2 client_1 in
  let empty_mempool_file = Temp.file "mempool.json" in
  let* _ =
    let empty_mempool =
      {|{"applied":[],"refused":[],"branch_refused":[],"branch_delayed":[],"unprocessed":[]}"|}
    in
    Lwt_io.with_file ~mode:Lwt_io.Output empty_mempool_file (fun oc ->
        Lwt_io.write oc empty_mempool)
  in
  let flush_waiter_1 = wait_for_flush node_1 in
  let flush_waiter_2 = wait_for_flush node_2 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter_1 and* () = flush_waiter_2 in
  Log.info "bake block to ensure mempool synchronisation" ;
  let* mempool_after_injections_1 =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_after_injections_1 =
    count_mempool mempool_after_injections_1
  in
  let* mempool_after_injections_2 =
    RPC.get_mempool_pending_operations client_2
  in
  let mempool_count_after_injections_2 =
    count_mempool mempool_after_injections_2
  in
  let (oph_to_ban, oph_remaining, client) =
    (* It may be possible that node 1 is never aware of the second
       operation if:

       - Node 2 receives the blocks

       - Node 2 evaluates the operations and classify the second
       operation as branch refused.

       However, this is unlikely for the moment since node 2 will
       advertise its mempool before flushing. *)
    let (mempool, client) =
      if mempool_count_after_injections_1.total = 2 then
        (mempool_after_injections_1, client_1)
      else if mempool_count_after_injections_2.total = 2 then
        (mempool_after_injections_2, client_2)
      else
        Test.fail
          "A problem occured during the test (probably a flakyness issue)."
    in
    let oph_applied = JSON.(mempool |-> "applied" |=> 0 |-> "hash") in
    let oph_refused = JSON.(mempool |-> "branch_refused" |=> 0 |=> 0) in
    (oph_refused, oph_applied, client)
  in
  Log.info "ban operation %s" JSON.(oph_to_ban |> as_string) ;
  let* _ =
    RPC.mempool_ban_operation
      ~data:(`String JSON.(oph_to_ban |> as_string))
      client
  in
  let* mempool_after_ban = RPC.get_mempool_pending_operations client in
  Log.info "check operation %s is applied" JSON.(oph_remaining |> as_string) ;
  check_operation_is_in_applied_mempool mempool_after_ban oph_remaining ;
  Lwt.return_unit

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

(** This test tries to run manually forged operations before injecting them

   Scenario:

   + Node 1 activates a protocol

   + Retrieve the counter and the branch for bootstrap1

   + Forge, run and inject <n> operations in the node

   + Check that the batch is correctly injected
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
  | _ -> Test.fail "Only one operation must be branch_refused"

(** This test checks that branch_refused endorsement are still propagated

   Scenario:

   + 3 Nodes are chained connected and activate a protocol

   + Disconnect node_1 from node_2 and bake on both node.

   + Reconnect node_1 and node_2

   + Endorse on node_1

   + Check that endorsement is applied on node_1 and refused on node_2 and node_3
*)

(* This test is no longer correct and cannot be adapated easily. *)
(* let endorsement_flushed_branch_refused = *)
(*   Protocol.register_test *)
(*     ~__FILE__ *)
(*     ~title:"Ensure that branch_refused endorsement are transmited" *)
(*     ~tags:["endorsement"; "mempool"; "branch_refused"] *)
(*   @@ fun protocol -> *)
(*   (\* Step 1 *\) *)
(*   (\* 3 Nodes are started and we activate the protocol and wait the nodes to be synced *\) *)
(*   let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode] *)
(*   and* node_2 = Node.init [Synchronisation_threshold 0; Private_mode] *)
(*   and* node_3 = Node.init [Synchronisation_threshold 0; Private_mode] in *)
(*   let* client_1 = Client.init ~endpoint:(Node node_1) () *)
(*   and* client_2 = Client.init ~endpoint:(Node node_2) () *)
(*   and* client_3 = Client.init ~endpoint:(Node node_3) () in *)
(*   let* () = Client.Admin.trust_address client_1 ~peer:node_2 *)
(*   and* () = Client.Admin.trust_address client_2 ~peer:node_1 *)
(*   and* () = Client.Admin.trust_address client_2 ~peer:node_3 *)
(*   and* () = Client.Admin.trust_address client_3 ~peer:node_2 in *)
(*   let* () = Client.Admin.connect_address client_1 ~peer:node_2 *)
(*   and* () = Client.Admin.connect_address client_2 ~peer:node_3 in *)
(*   let* () = Client.activate_protocol ~protocol client_1 in *)
(*   Log.info "Activated protocol." ; *)
(*   let* _ = Node.wait_for_level node_1 1 *)
(*   and* _ = Node.wait_for_level node_2 1 *)
(*   and* _ = Node.wait_for_level node_3 1 in *)
(*   Log.info "All nodes are at level %d." 1 ; *)
(*   (\* Step 2 *\) *)
(*   (\* Disconnect node_1 and node_2 and bake on both node. This will force different branches *\) *)
(*   let* node_2_id = Node.wait_for_identity node_2 *)
(*   and* node_1_id = Node.wait_for_identity node_1 in *)
(*   let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id *)
(*   and* () = Client.Admin.kick_peer client_2 ~peer:node_1_id in *)
(*   let bake_waiter_1 = wait_for_flush node_1 *)
(*   and bake_waiter_2 = wait_for_flush node_2 in *)
(*   let* () = Client.bake_for client_1 *)
(*   and* () = Client.bake_for ~key:Constant.bootstrap3.identity client_2 in *)
(*   let* () = bake_waiter_1 and* () = bake_waiter_2 in *)
(*   (\* Step3 *\) *)
(*   (\* Reconnect node_1 and node_2 *\) *)
(*   let* () = Client.Admin.trust_address client_1 ~peer:node_2 *)
(*   and* () = Client.Admin.trust_address client_2 ~peer:node_1 in *)
(*   let* () = Client.Admin.connect_address client_1 ~peer:node_2 in *)
(*   (\* Step 4 *\) *)
(*   (\* Endorse on node_1 *\) *)
(*   let endorser_waiter = wait_for_injection node_1 in *)
(*   let* () = Client.endorse_for client_1 in *)
(*   let* () = endorser_waiter in *)
(*   Log.info "Endorsement on node_1 done" ; *)
(*   (\* Step 5 *\) *)
(*   (\* Check that endorsement is applied on node_1 and refused on node_2 and node_3 *\) *)
(*   let* pending_op_1 = RPC.get_mempool_pending_operations client_1 in *)
(*   let oph = get_endorsement_hash pending_op_1 in *)
(*   Log.info "Endorsement found in node_1 applied mempool" ; *)
(*   let* pending_op_2 = RPC.get_mempool_pending_operations client_2 in *)
(*   let () = check_if_op_is_branch_refused pending_op_2 oph in *)
(*   Log.info "Endorsement found in branch_refused of node_2 mempool" ; *)
(*   (\* The only way node_3 gets the endorsement is that node_2 has *)
(*      propagated the operation. *\) *)
(*   let* pending_op_3 = RPC.get_mempool_pending_operations client_3 in *)
(*   let () = check_if_op_is_branch_refused pending_op_3 oph in *)
(*   Log.info "Endorsement found in branch_refused of node_3 mempool" ; *)
(*   unit *)

let check_empty_operation__ddb ddb =
  let open JSON in
  let op_db_length = as_int (ddb |-> "operation_db" |-> "table_length") in
  if op_db_length > 0 then
    Test.fail
      "Operation Ddb should be empty, contains : %d elements"
      op_db_length

(** This test checks that pre-filtered operations are cleaned from the ddb

   Scenario:

   + 3 Nodes are chained connected and activate a protocol

   + Get the counter and the current branch

   + Forge operation, inject it and check injection on node_1
     This operation is pre-filtered on node_2

   + Bake 1 block

   + Get client_2 ddb and check that it contains no operation
*)
let forge_pre_filtered_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"Forge pre-filtered operation and check mempool"
    ~tags:["forge"; "mempool"; "pre_filtered"]
  @@ fun protocol ->
  (* Step 1 *)
  (* Two Nodes are started and we activate the protocol and wait the nodes to be synced *)
  let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode]
  and* node_2 = Node.init [Synchronisation_threshold 0; Private_mode] in
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

(** Matches events which contain a failed fetch.
   For example:

  {[
    {  "event": {
           "operation_not_fetched": "onuvmuCS5NqtJG65BJWqH44bzwiXLw4tVpfNqRQvkgorv5LoejA"
       },
       "level": "debug"
    }
  ]}
*)
let wait_for_failed_fetch node =
  Node.wait_for node "operation_not_fetched.v0" (fun _ -> Some ())

let set_config_operations_timeout node timeout =
  let chain_validator_config =
    let open JSON in
    Node.Config_file.read node |-> "shell" |-> "chain_validator"
  in
  let updated_shell_config =
    JSON.annotate
      ~origin:"shell"
      (Ezjsonm.from_string
         (Format.asprintf
            {|{"prevalidator": { "operations_request_timeout" : %f },
            "peer_validator" : { "new_head_request_timeout" : 5 },
            "chain_validator": %s}|}
            timeout
            (JSON.encode chain_validator_config)))
  in
  Node.Config_file.update node (JSON.put ("shell", updated_shell_config))

(** This test checks that failed fetched operations can be refetched successfully

   Scenario:

   + initialise two nodes and activate the protocol. The second node is initialise with specific configuration

   + Get the counter and the current branch

   + Forge operation and inject it in node_1, checks that the fetch fail in node_2

   + Ensure that the injected operation is in node_1 mempool

   + Ensure that the mempool of node_2 is empty

   + Inject the previous operation in node_2

   + Ensure that the operation is injected in node_2 mempool
*)
let refetch_failed_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"Fetch failed operation"
    ~tags:["fetch"; "mempool"]
  @@ fun protocol ->
  (* Step 1 *)
  (* initialise both nodes and activate protocol
     node_2 uses specific configuration to force timeout in fetching *)
  let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let node_2 = Node.create [Synchronisation_threshold 0; Private_mode] in
  let* () = Node.config_init node_2 [] in
  (* Set a low operations_request_timeout to force timeout at fetching *)
  set_config_operations_timeout node_2 0.00001 ;
  (* Run the node with the new config.
     event_level is set to debug to catch fetching event at this level *)
  let* () = Node.run ~event_level:"debug" node_2 [] in
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
  (* get counter and branches *)
  let* counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int counter in
  let* branch = RPC.get_branch client_1 in
  let branch = JSON.as_string branch in
  (* Step 3 *)
  (* Forge operation and inject it in node_1, checks that the fetch fail in node_2 *)
  let* op_str_hex =
    forge_operation
      ~branch
      ~fee:1000 (* Minimal fees to successfully apply the transfer *)
      ~gas_limit:1040 (* Minimal gas to successfully apply the transfer *)
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter:(counter + 1)
      ~client:client_1
  in
  let signature = sign_operation ~signer:Constant.bootstrap1 op_str_hex in
  let failed_fetching_waiter = wait_for_failed_fetch node_2 in
  let* oph = inject_operation ~client:client_1 op_str_hex signature in
  let* () = failed_fetching_waiter in
  (* Step 4 *)
  (* Ensure that the injected operation is in node_1 mempool *)
  let* mempool_node_1 = RPC.get_mempool_pending_operations client_1 in
  check_operation_is_in_applied_mempool mempool_node_1 oph ;
  (* Step 5 *)
  (* Ensure that the mempool of node_2 is empty *)
  let* mempool_count_after_failed_fetch =
    RPC.get_mempool_pending_operations client_2
  in
  let count_failed_fetching = count_mempool mempool_count_after_failed_fetch in
  if count_failed_fetching.total <> 0 then
    Test.fail "The mempool of node 2 should be empty" ;
  (* Step 6 *)
  (* Inject the previous operation in node_2 *)
  let* oph2 = inject_operation ~client:client_2 op_str_hex signature in
  if oph <> oph2 then
    Test.fail
      "The operation injected in node_2 should be the same as the one injected \
       in node_1" ;
  (* Step 7 *)
  (* Ensure that the operation is injected in node_2 mempool *)
  let* mempool_inject_on_node_2 = RPC.get_mempool_pending_operations client_2 in
  check_operation_is_in_applied_mempool mempool_inject_on_node_2 oph ;
  unit

let get_op_hash_to_ban client =
  let* pending_ops = RPC.get_mempool_pending_operations client in
  let open JSON in
  let ops_list = pending_ops |-> "applied" |> as_list in
  match ops_list with
  | [] -> Test.fail "No operation has been found"
  | op :: _ ->
      let oph = op |-> "hash" |> as_string in
      return oph

let check_op_removed client op =
  let* pending_ops = RPC.get_mempool_pending_operations client in
  let open JSON in
  let ops_list = pending_ops |-> "applied" |> as_list in
  let res = List.exists (fun e -> e |-> "hash" |> as_string = op) ops_list in
  if res then Test.fail "%s found after removal" op ;
  unit

let check_op_not_in_baked_block client op =
  let* ops = RPC.get_operations client in
  let open JSON in
  let ops_list = ops |=> 3 |> as_list in
  let res = List.exists (fun e -> e |-> "hash" |> as_string = op) ops_list in
  if res then Test.fail "%s found in Baked block" op ;
  unit

(** Waits for the event signaling the injection of a banned operation of
    hash [oph]. For example:
{[
   [
     "2021-06-25T15:28:48.613-00:00",
     {
       "event": {
         "banned_operation_encountered": {
           "situation": "injected",
           "operation": "oo5wP7Qzqhsm6FTnz1dNJtvcF6AVozHNfa6W85dBKBGGogi3G5q"
         }
       },
       "level": "notice"
     }
   ]
]} *)
let wait_for_banned_operation_injection node oph =
  let filter json =
    match
      JSON.(json |-> "origin" |> as_string_opt, json |-> "oph" |> as_string_opt)
    with
    | (Some "injected", Some h) when String.equal h oph -> Some ()
    | _ -> None
  in
  Node.wait_for node "banned_operation_encountered.v0" filter

(** Bakes with an empty mempool to force synchronisation between nodes. *)
let bake_empty_mempool ?endpoint client =
  let mempool_str =
    {|{"applied":[],"refused":[],"branch_refused":[],"branch_delayed":[],"unprocessed":[]}"|}
  in
  let mempool = Temp.file "mempool.json" in
  let* _ =
    Lwt_io.with_file ~mode:Lwt_io.Output mempool (fun oc ->
        Lwt_io.write oc mempool_str)
  in
  let* () = Client.bake_for ?endpoint ~mempool client in
  unit

(** This test bans an operation and tests the ban.

    Scenario:

    - Step 1: Node 1 activates the protocol and Node 2 catches up with
      Node 1.

    - Step 2: Injection of two operations (transfers).

    - Step 3: Get the hash of the first operation and ban it on Node 2.

    - Step 4: Try to reinject the banned operation in Node 2.

    - Step 5: Add Node 3 connected only to Node 2.

    - Step 6: Bake on Node 2 with an empty mempool to force synchronisation
      with Node 3. Check that the banned operation is not in the mempool
      of Node 3.

    - Step 7: Bake on Node 2 (with its mempool). Check that the banned
      operation is not in the baked block nor in the mempool of Node 2.
*)
let ban_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool ban operation"
    ~tags:["mempool"; "node"]
  @@ fun protocol ->
  Log.info
    "Step 1: Node 1 activates the protocol and Node 2 catches up with Node 1." ;
  (* Note: we start node_1 and node_2 and connect them. We wait until
     they are synced to the same level *)
  let* node_1 = Node.init [Synchronisation_threshold 0; Connections 1]
  and* node_2 =
    Node.init
      ?event_level:(Some "debug") (* to witness operation arrival event *)
      [Synchronisation_threshold 0; Connections 2]
  in
  let* client_1 = Client.init ~endpoint:Client.(Node node_1) ()
  and* client_2 = Client.init ~endpoint:Client.(Node node_2) () in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let level = 1 in
  let* _ = Node.wait_for_level node_1 level
  and* _ = Node.wait_for_level node_2 level in
  Log.info "Both nodes are at level %d." level ;
  Log.info "Step 2: Injection of two operations (transfers)." ;
  let operation_arrival_in_node_2 = Node_event_level.wait_for_arrival node_2 in
  let transfer_1 = wait_for_injection node_1 in
  let inject_op1 client =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~counter:1
      client
  in
  let _ = inject_op1 client_1 in
  let* () = transfer_1 in
  Log.info "First transfer done." ;
  let transfer_2 = wait_for_injection node_1 in
  let _ =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap3.alias
      ~receiver:Constant.bootstrap2.alias
      ~counter:1
      client_1
  in
  let* () = transfer_2 in
  Log.info "Second transfer done." ;
  Log.info "Step 3: Get the hash of the first operation and ban it on Node 2." ;
  (* choose an op hash to ban from the mempool of client_2 operations *)
  (* We ensure that at least one operation has arrived in node_2,
     so that [get_op_hash_to_ban client_2] does not fail.
     (Technically, we should wait for the mempool to apply the operation
     rather than only receive it, but this seems sufficient in practice.
     Also, the test relies on the operation selected by [get_op_hash_to_ban]
     always beeing the first transfer, as it is the one we later try to
     reinject.) *)
  let* () = operation_arrival_in_node_2 in
  let* oph_to_ban = get_op_hash_to_ban client_2 in
  Log.info "Op Hash to ban : %s" oph_to_ban ;
  (* ban operation *)
  let* _ = RPC.mempool_ban_operation ~data:(`String oph_to_ban) client_2 in
  Log.info "Ban %s" oph_to_ban ;
  (* Check that the operation is removed from the mempool *)
  let* () = check_op_removed client_2 oph_to_ban in
  Log.info "%s op has been correctly removed" oph_to_ban ;
  Log.info "Step 4: Try to reinject the banned operation in Node 2." ;
  let banned_transfer = wait_for_banned_operation_injection node_2 oph_to_ban in
  let _ = inject_op1 client_2 in
  let* () = banned_transfer in
  let* () = check_op_removed client_2 oph_to_ban in
  Log.info "%s op is still banned" oph_to_ban ;
  Log.info "Step 5: Add Node 3 connected only to Node 2." ;
  let* node_3 = Node.init [Synchronisation_threshold 0; Connections 1] in
  let* client_3 = Client.init ~endpoint:Client.(Node node_3) () in
  let* () = Client.Admin.connect_address client_3 ~peer:node_2 in
  let* _ = Node.wait_for_level node_3 level in
  Log.info
    "Step 6: Bake on Node 2 with an empty mempool to force synchronisation \
     with Node 3. Check that the banned operation is not in the mempool of \
     Node 3." ;
  let dummy_baking = wait_for_flush node_2 in
  let* () = bake_empty_mempool client_2 in
  let* () = dummy_baking in
  let* _ = check_op_removed client_3 oph_to_ban in
  Log.info "Check that banned op is not in node_3" ;
  Log.info
    "Step 7: Bake on Node 2 (with its mempool). Check that the banned \
     operation is not in the baked block nor in the mempool of Node 2." ;
  let baking = wait_for_flush node_2 in
  let* () = Client.bake_for client_2 in
  let* _ = baking in
  Log.info "Client2 baked" ;
  let* _ = check_op_not_in_baked_block client_2 oph_to_ban in
  Log.info "%s op is not in baked block" oph_to_ban ;
  let* () = check_op_removed client_2 oph_to_ban in
  Log.info "%s op is still not in the mempool" oph_to_ban ;
  unit

(* for functions [transfer_and_wait_for_injection], [wait_for_arrival],
   and [get_applied_operation_hash_list] *)
open Node_event_level

(** Injects a transfer operation from [client] and waits for an operation
    to arrive from the network on [node] (which should not be the node
    associated to [client], but there should be a connection path between
    them).
    Note: the event for operation arrival has level "debug", so [node]
    needs to have event level set to "debug" for it to exist. Otherwise,
    this function will block. *)
let transfer_and_wait_for_arrival node client amount_int giver_key receiver_key
    =
  let wait_for = wait_for_arrival node in
  let _ =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int amount_int)
      ~giver:Constant.(giver_key.alias)
      ~receiver:Constant.(receiver_key.alias)
      client
  in
  let* () = wait_for in
  unit

(** Gets the list of hashes of the mempool's applied operations,
    displays it, and returns it. *)
let get_and_log_applied client =
  let* ophs = get_applied_operation_hash_list client in
  Log.info "Applied operations in mempool:" ;
  List.iter (Log.info "- %s") ophs ;
  return ophs

(** Boolean indicating whether two lists of operation hashes (strings)
   are equal (returns [false] if they have different lengths, instead
   of raising [invalid_arg] as using [List.for_all2] directly would
   do). We use a naive way to check both lists are equal because
   1. performances for small lists does not matter and 2. the mempool
   does not specify how operations previously applied will be applied
   again after banning one operation. *)
let oph_list_equal l1 l2 =
  Int.equal (List.length l1) (List.length l2)
  && List.for_all (fun x -> List.mem x l2) l1
  && List.for_all (fun x -> List.mem x l1) l2

(** Gets the list of hashes of the mempool's applied operations,
    and asserts that it is equal to the given list [expected_ophs]. *)
let check_applied_ophs_is client expected_ophs =
  let* ophs = get_applied_operation_hash_list client in
  if oph_list_equal ophs expected_ophs then (
    Log.info "Checking applied operations in mempool:" ;
    List.iter (Log.info "- %s") ophs ;
    unit)
  else (
    Log.info "Expected applied operations:" ;
    List.iter (Log.info "- %s") expected_ophs ;
    Log.info "Actual applied operations:" ;
    List.iter (Log.info "- %s") ophs ;
    Test.fail
      "Wrong list of applied operations in mempool (use --info to see expected \
       and actual lists).")

(** Test.

    Aim: check that, when banning an operation that was applied in the
    mempool, the other applied operations are correctly reapplied (in
    the same order).

    Scenario:
    - Step 1: Start two nodes, connect them, activate the protocol.
    - Step 2: Inject five operations (transfers from five different sources,
      injected by both nodes in alternance).
    - Step 3: Ban one of these operations from node_1 (arbitrarily, the third
      in the list of applied operations in the mempool of node_1).
    - Step 4: Check that applied operations in node_1 are still applied

    Note: the chosen operations are commutative, so that none of them
    becomes branch_delayed instead of applied when one of them is banned.
*)
let ban_operation_and_check_applied =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool ban operation and check applied"
    ~tags:["mempool"; "node"]
  @@ fun protocol ->
  Log.info "Step 1: Start two nodes, connect them, activate the protocol." ;
  let* node_1 =
    Node.init
      ?event_level:(Some "debug") (* to witness operation arrival events *)
      [Synchronisation_threshold 0; Connections 1]
  and* node_2 = Node.init [Synchronisation_threshold 0; Connections 1] in
  let* client_1 = Client.init ~endpoint:Client.(Node node_1) ()
  and* client_2 = Client.init ~endpoint:Client.(Node node_2) () in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  Log.info "Both nodes are at level 1." ;
  Log.info
    "Step 2: Inject five operations (transfers from five different sources, \
     injected by both nodes in alternance)." ;
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      1
      Constant.bootstrap1
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_arrival
      node_1
      client_2
      2
      Constant.bootstrap2
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      3
      Constant.bootstrap3
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_arrival
      node_1
      client_2
      4
      Constant.bootstrap4
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      5
      Constant.bootstrap5
      Constant.bootstrap1
  in
  Log.info
    "Step 3: Ban one of these operations from node_1 (arbitrarily, the third \
     in the list of applied operations in the mempool of node_1)." ;
  let* applied_ophs = get_and_log_applied client_1 in
  if not (Int.equal (List.length applied_ophs) 5) then
    (* This could theoretically happen: we wait for each transfer to
       be present in the mempool as "pending", but not to be classified
       as "applied". In practice, this does not seem to be a problem. *)
    Test.fail
      "Found only %d applied operations in node_1, expected 5."
      (List.length applied_ophs) ;
  let oph_to_ban = List.nth applied_ophs 2 in
  Log.info "Operation to ban: %s" oph_to_ban ;
  let* _ = RPC.mempool_ban_operation ~data:(`String oph_to_ban) client_1 in
  Log.info "Operation %s is now banned." oph_to_ban ;
  Log.info "Step 4: Check that applied operations in node_1 are still applied." ;
  let expected_reapplied_ophs =
    List.filter (fun oph -> not (String.equal oph_to_ban oph)) applied_ophs
  in
  let* () = check_applied_ophs_is client_1 expected_reapplied_ophs in
  unit

(** Test.

    Aim: check that unbanned operations can be injected again.
    Also check that unbanning an operation does not unban another
    one, that banning an operation several times does not prevent
    it from being unbanned, and that an unbanned operation can
    be banned again.

    Scenario:
    - Step 1: Start a single node and activate the protocol.
    - Step 2: Inject two transfers op1 and op2, ban op1 (twice), inject
      a third transfer op3, ban op2, ban op1 again. Regularly
      check that the mempool contains the right operation(s).
      Now op1 and op2 are banned, and the mempool contains only op3.
    - Step 3: Check that reinjecting op1 fails.
    - Step 4: Unban op1, successfully reinject op1.
    - Step 5: Check that reinjecting op2 still fails.
    - Step 6: Unban op2, successfully reinject op2.
    - Step 7: Ban op1 again, check that reinjecting it fails.
    - Step 8: Unban op3 and op2 (which are not currently banned), check that
      nothing changes.
*)
let unban_operation_and_reinject =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool unban operation and reinject"
    ~tags:["mempool"; "node"]
  @@ fun protocol ->
  Log.info "Step 1: Start a single node and activate the protocol." ;
  let* node_1 = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client_1 = Client.init ~endpoint:Client.(Node node_1) () in
  let* () = Client.activate_protocol ~protocol client_1 in
  let* _ = Node.wait_for_level node_1 1 in
  let check_applied_ophs_is = check_applied_ophs_is client_1 in
  Log.info
    "Step 2: Inject two transfers op1 and op2, ban op1 (twice), inject a third \
     transfer op3, ban op2, ban op1 again." ;
  (* As in previous tests, it seems sufficient to wait for operations
     to arrive in the mempool, even though we actually need them to be
     classified. *)
  let wait1 = wait_for_injection node_1 in
  let inject_op1 () =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap5.alias
      ~counter:1
      client_1
  in
  let _ = inject_op1 () in
  let* () = wait1 in
  Log.info "Op1 injected." ;
  let* ophs_only1 = get_and_log_applied client_1 in
  let oph1 =
    match ophs_only1 with
    | [x] -> x
    | _ -> Test.fail "Expected 1 applied operation."
  in
  let wait2 = wait_for_injection node_1 in
  let inject_op2 () =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap5.alias
      ~counter:1
      client_1
  in
  let _ = inject_op2 () in
  let* () = wait2 in
  Log.info "Op2 injected." ;
  let* ophs_1_2 = get_and_log_applied client_1 in
  let oph2 =
    match ophs_1_2 with
    | [x1; x2] ->
        if not (String.equal x1 oph1) then
          Test.fail "Wrong first operation (expected op1: %s)." oph1 ;
        x2
    | _ -> Test.fail "Expected 2 applied operations."
  in
  let* _ = RPC.mempool_ban_operation ~data:(`String oph1) client_1 in
  Log.info "Op1 (%s) banned." oph1 ;
  let* _ = RPC.mempool_ban_operation ~data:(`String oph1) client_1 in
  Log.info "Op1 (%s) banned again." oph1 ;
  let* () = check_applied_ophs_is [oph2] in
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      3
      Constant.bootstrap3
      Constant.bootstrap5
  in
  Log.info "Op3 injected." ;
  let* ophs_2_3 = get_and_log_applied client_1 in
  let oph3 =
    match ophs_2_3 with
    | [x1; x2] ->
        if not (String.equal x1 oph2) then
          Test.fail "Wrong first operation (expected op2: %s)." oph2 ;
        x2
    | _ -> Test.fail "Expected 2 applied operations."
  in
  let* _ = RPC.mempool_ban_operation ~data:(`String oph2) client_1 in
  Log.info "Op2 (%s) banned." oph2 ;
  let* _ = RPC.mempool_ban_operation ~data:(`String oph1) client_1 in
  Log.info "Op1 (%s) banned again." oph1 ;
  let* () = check_applied_ophs_is [oph3] in
  Log.info "Now op1 and op2 are banned, and the mempool contains only op3." ;
  Log.info "Step 3: Check that reinjecting op1 fails." ;
  let wait_reinject_op1_banned =
    wait_for_banned_operation_injection node_1 oph1
  in
  let _ = inject_op1 () in
  let* () = wait_reinject_op1_banned in
  Log.info "Op1 (%s) could not be reinjected as it is banned." oph1 ;
  let* () = check_applied_ophs_is [oph3] in
  Log.info "Step 4: Unban op1, successfully reinject op1." ;
  let* _ = RPC.mempool_unban_operation ~data:(`String oph1) client_1 in
  Log.info "Op1 (%s) unbanned." oph1 ;
  let wait_reinject_op1_ok = wait_for_injection node_1 in
  let _ = inject_op1 () in
  let* () = wait_reinject_op1_ok in
  Log.info "Op1 (%s) reinjected." oph1 ;
  let* () = check_applied_ophs_is [oph3; oph1] in
  Log.info "Step 5: Check that reinjecting op2 still fails." ;
  let wait_reinject_op2_banned =
    wait_for_banned_operation_injection node_1 oph2
  in
  let _ = inject_op2 () in
  let* () = wait_reinject_op2_banned in
  Log.info "Op2 (%s) could not be reinjected as it is banned." oph2 ;
  let* () = check_applied_ophs_is [oph3; oph1] in
  Log.info "Step 6: Unban op2, successfully reinject op2." ;
  let* _ = RPC.mempool_unban_operation ~data:(`String oph2) client_1 in
  Log.info "Op2 (%s) unbanned." oph2 ;
  let wait_reinject_op2_ok = wait_for_injection node_1 in
  let _ = inject_op2 () in
  let* () = wait_reinject_op2_ok in
  Log.info "Op2 (%s) reinjected." oph2 ;
  let* () = check_applied_ophs_is [oph3; oph1; oph2] in
  Log.info "Step 7: Ban op1 again, check that reinjecting it fails." ;
  let* _ = RPC.mempool_ban_operation ~data:(`String oph1) client_1 in
  Log.info "Op1 (%s) banned once again." oph1 ;
  let wait_reinject_op1_banned_again =
    wait_for_banned_operation_injection node_1 oph1
  in
  let _ = inject_op1 () in
  let* () = wait_reinject_op1_banned_again in
  Log.info "Op1 (%s) could not be reinjected as it is banned." oph1 ;
  let* () = check_applied_ophs_is [oph3; oph2] in
  Log.info
    "Step 8: Unban op3 and op2 (which are not currently banned), check that \
     nothing changes." ;
  let* _ = RPC.mempool_unban_operation ~data:(`String oph3) client_1 in
  let* _ = RPC.mempool_unban_operation ~data:(`String oph2) client_1 in
  Log.info "Op3 and op2 unbanned." ;
  let* () = check_applied_ophs_is [oph3; oph2] in
  unit

(** Waits for an event in [node] signaling the arrival in the mempool
    of an operation of hash [ophash].
    Note: this event has level "debug", so the node needs to have event
    level set to "debug" for such an event to exist. *)
let wait_for_arrival_of_ophash ophash node =
  let filter json =
    let open JSON in
    match
      ( json |-> "view" |-> "request" |> as_string_opt,
        json |-> "view" |-> "operation_hash" |> as_string_opt )
    with
    | (Some "arrived", Some s) when String.equal s ophash ->
        Log.info "Witnessed arrival of operation %s" ophash ;
        Some ()
    | _ -> None
  in
  Node.wait_for node "request_completed_debug.v0" filter

(** Test.

    Aim: test the unban_all_operations RPC. Moreover, to expand the tests in
    general, instead of reinjecting unbanned operations as in
    unban_operation_and_reinject above, we get them back from a second node.

    Scenario:
    - Step 1: Start two nodes, connect them, activate the protocol.
    - Step 2: Inject four transfer operations (two from node 1, two from node 2).
    - Step 3: Ban the first three of these operations from node 1, check that only
      the fourth operation remains in mempool.
    - Step 4: Unban all operations from node 1. Bake with an empty mempool to
      force synchronisation with node 2. Unbanned operations should come back
      to node 1.
    - Step 5: Check that node 1 contains the right applied operations.
*)
let unban_all_operations =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool unban all operations"
    ~tags:["mempool"; "node"]
  @@ fun protocol ->
  Log.info "Step 1: Start two nodes, connect them, activate the protocol." ;
  let* node_1 =
    Node.init
      ?event_level:(Some "debug") (* to witness operation arrival events *)
      [Synchronisation_threshold 0; Connections 2]
  and* node_2 = Node.init [Synchronisation_threshold 0; Connections 2] in
  let* client_1 = Client.init ~endpoint:Client.(Node node_1) ()
  and* client_2 = Client.init ~endpoint:Client.(Node node_2) () in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  let level = 1 in
  let* _ = Node.wait_for_level node_1 level
  and* _ = Node.wait_for_level node_2 level in
  Log.info
    "Step 2: Inject four transfer operations (two from node 1, two from node \
     2)." ;
  (* As in previous tests, it seems sufficient to wait for operations
     to arrive in the mempool, even though we actually need them to be
     classified. *)
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      1
      Constant.bootstrap1
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_injection
      node_1
      client_1
      2
      Constant.bootstrap2
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_arrival
      node_1
      client_2
      3
      Constant.bootstrap3
      Constant.bootstrap5
  in
  let* () =
    transfer_and_wait_for_arrival
      node_1
      client_2
      4
      Constant.bootstrap4
      Constant.bootstrap5
  in
  Log.info
    "Step 3: Ban the first three of these operations from node 1, check that \
     only the fourth operation remains in mempool." ;
  let* ophs = get_and_log_applied client_1 in
  let (oph1, oph2, oph3, oph4) =
    match ophs with
    | [x1; x2; x3; x4] -> (x1, x2, x3, x4)
    | _ ->
        Test.fail
          "There should be four applied operations in mempool of node_1."
  in
  let* _ = RPC.mempool_ban_operation ~data:(`String oph1) client_1 in
  let* _ = RPC.mempool_ban_operation ~data:(`String oph2) client_1 in
  let* _ = RPC.mempool_ban_operation ~data:(`String oph3) client_1 in
  Log.info "Three operations are now banned: %s, %s, and %s." oph1 oph2 oph3 ;
  let* () = check_applied_ophs_is client_1 [oph4] in
  Log.info
    "Step 4: Unban all operations from node 1. Bake with an empty mempool to \
     force synchronisation with node 2. Unbanned operations should come back \
     to node 1." ;
  let wait1 = wait_for_arrival_of_ophash oph1 node_1
  and wait2 = wait_for_arrival_of_ophash oph2 node_1
  and wait3 = wait_for_arrival_of_ophash oph3 node_1 in
  let* _ = RPC.mempool_unban_all_operations client_1 in
  Log.info "All operations are now unbanned." ;
  let* () = bake_empty_mempool client_1 in
  let* () = wait1 and* () = wait2 and* () = wait3 in
  Log.info "Step 5: Check that node 1 contains the right applied operations." ;
  let* final_ophs = get_and_log_applied client_1 in
  let () =
    match final_ophs with
    | hd :: tl ->
        if not (String.equal hd oph4) then
          Test.fail "First applied operation should be %s." oph4 ;
        if not (List.for_all (fun oph -> List.mem oph tl) [oph1; oph2; oph3])
        then
          Test.fail
            "Applied operations should include all three unbanned operations: \
             %s, %s, and %s."
            oph1
            oph2
            oph3 ;
        if not (Int.equal (List.length tl) 3) then
          Test.fail "There should only be 4 applied operations."
    | _ -> Test.fail "List of applied operations should not be empty."
  in
  unit

let wait_for_flushed_event node =
  let filter json = JSON.(json |> as_int_opt) in
  Node.wait_for node "operations_not_flushed.v0" filter

(** This test tries to check that branch_refused operation stays in the mempool after an head increment but is removed from it when a new branch is received.

   Scenario:

   + Node 1 and node 2 activates a protocol

   + Disconnection of node_1 and node_2

   + Recover counter and branch

   + Inject operation on node_1

   + Check that this operation is applied

   + Inject operation on node 2 with the same counter as previous operation from the same source

   + Reconnect and sync nodes

   + Check that node_1 mempool's contain one applied and one branch_refused operation

   + Disconnect node_1 and node_2

   + Bake on node_1 (head increment) and checks that branch_refused operation is not reclassify

   + Check that branch_refused operation is still branch_refused after head increment

   + Bake on node_2 to force higher fitness

   + Reconnect node_1 and node_2

   + Check that branch_refused operation is set to be reclassified on new head
*)
let recycling_branch_refused =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Ensure that branch_refused operation is not recycled when we increment \
       our head"
    ~tags:["recycle"; "mempool"; "branch_refused"]
  @@ fun protocol ->
  (* Step 1 *)
  (* Connect and initialise two nodes *)
  let* node_1 =
    Node.init
      ?event_level:(Some "debug")
      [Synchronisation_threshold 0; Private_mode]
  and* node_2 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  (* Step 2 *)
  (* Disconnect nodes *)
  let* node2_identity = Node.wait_for_identity node_2 in
  let* () = Client.Admin.kick_peer ~peer:node2_identity client_1 in
  Log.info "nodes are at level 1" ;
  (* Step 3 *)
  (* Recover counter and branch *)
  let* counter =
    RPC.Contracts.get_counter ~contract_id:Constant.bootstrap1.identity client_1
  in
  let counter = JSON.as_int counter in
  let* branch = RPC.get_branch client_1 in
  let branch = JSON.as_string branch in
  (* Step 4 *)
  (* Inject operation on node 1 *)
  let injection_waiter = wait_for_injection node_1 in
  let* oph =
    forge_and_inject_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap2.identity
      ~counter:(counter + 1)
      ~signer:Constant.bootstrap1
      ~client:client_1
  in
  let* () = injection_waiter in
  Log.info "%s injected on node 1" JSON.(oph |> as_string) ;
  (* Step 5 *)
  (* Check that operation is applied *)
  let* mempool_after_injections = RPC.get_mempool_pending_operations client_1 in
  check_operation_is_in_applied_mempool mempool_after_injections oph ;
  Log.info "Forged operation is applied in the mempool" ;
  (* Step 6 *)
  (* Inject operation on node 2 with the same counter from the same source *)
  let injection_waiter = wait_for_injection node_2 in
  let* oph2 =
    forge_and_inject_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.identity
      ~destination:Constant.bootstrap3.identity
      ~counter:(counter + 1)
      ~signer:Constant.bootstrap1
      ~client:client_2
  in
  let* () = injection_waiter in
  Log.info
    "%s injected on node 2 with the same counter as %s"
    JSON.(oph2 |> as_string)
    JSON.(oph |> as_string) ;
  (* Step 7 *)
  (* Reconnect and sync nodes *)
  let* () = Client.Admin.connect_address ~peer:node_2 client_1 in
  let empty_mempool_file = Temp.file "mempool.json" in
  let* _ =
    let empty_mempool =
      {|{"applied":[],"refused":[],"branch_refused":[],"branch_delayed":[],"unprocessed":[]}"|}
    in
    Lwt_io.with_file ~mode:Lwt_io.Output empty_mempool_file (fun oc ->
        Lwt_io.write oc empty_mempool)
  in
  let flush_waiter_1 = wait_for_flush node_1 in
  let flush_waiter_2 = wait_for_flush node_2 in
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* () = flush_waiter_1 and* () = flush_waiter_2 in
  Log.info "bake block to ensure mempool synchronisation" ;
  (* Step 8 *)
  (* Check that node_1 mempool's contain one applied and one branch_refused operation *)
  let* mempool = RPC.get_mempool_pending_operations client_1 in
  let mempool_count = count_mempool mempool in
  assert (mempool_count.branch_refused = 1 && mempool_count.applied = 1) ;
  let _oph_applied = JSON.(mempool |-> "applied" |=> 0 |-> "hash") in
  let oph_branch_refused =
    JSON.(mempool |-> "branch_refused" |=> 0 |=> 0 |> as_string)
  in
  (* Step 9 *)
  (* Disconnect node_1 and node_2 *)
  let* () = Client.Admin.kick_peer ~peer:node2_identity client_1 in
  (* Step 10 *)
  (*  Bake on node_1 (head increment) and checks that branch_refused operation is not reclassify *)
  let bake_waiter_1 = wait_for_flushed_event node_1 in
  let* () = Client.bake_for client_1 in
  let* pending = bake_waiter_1 in
  if pending > 1 then Test.fail "Branch_refused operation should not be pending" ;
  (* Step 11 *)
  (* Check that branch_refused operation is still branch_refused after head increment *)
  let* pending_op = RPC.get_mempool_pending_operations client_1 in
  let () = check_if_op_is_branch_refused pending_op oph_branch_refused in
  (* Step 12  *)
  (* Bake on node_2 to force higher fitness  *)
  let* () =
    repeat 2 (fun () ->
        Node_event_level.bake_wait_log
          ~mempool:empty_mempool_file
          node_2
          client_2)
  in
  (* Step 13 *)
  (* Reconnect node_1 and node_2  *)
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let bake_waiter_1 = wait_for_flushed_event node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  (* Step 14 *)
  (* Check that branch_refused operation is set to be reclassified on new head*)
  let* () = Client.bake_for ~mempool:empty_mempool_file client_1 in
  let* pending = bake_waiter_1 in
  if pending <> 2 then Test.fail "the two operations should be reclassified" ;
  unit

let register ~protocols =
  flush_mempool ~protocols ;
  run_batched_operation ~protocols ;
  forge_pre_filtered_operation ~protocols ;
  refetch_failed_operation ~protocols ;
  ban_operation ~protocols ;
  ban_operation_and_check_applied ~protocols ;
  unban_operation_and_reinject ~protocols ;
  unban_all_operations ~protocols ;
  ban_operation_branch_refused_reevaluated ~protocols ;
  recycling_branch_refused ~protocols
