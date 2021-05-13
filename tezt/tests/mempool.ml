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
    "total: %d - applied: %d, branch_delayed: %d, branch_refused: %d, \
     refused: %d, unprocessed: %d"
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
      JSON.(
        json |=> 1 |-> "event" |-> "request" |-> "request" |> as_string_opt)
    with
    | Some s when s = "inject" ->
        Some s
    | Some _ | None ->
        None
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
      JSON.(
        json |=> 1 |-> "event" |-> "request" |-> "request" |> as_string_opt)
    with
    | Some s when s = "flush" ->
        Some s
    | Some _ | None ->
        None
  in
  let* _ = Node.wait_for node "node_prevalidator.v0" filter in
  return ()

let operation_json branch sender receiver counter =
  Format.sprintf
    {|{"branch": %s,
           "contents": [{
             "kind": "transaction",
             "source": "%s",
             "fee": "10",
             "counter": "%d",
             "gas_limit": "1040",
             "storage_limit": "0",
             "amount": "1000",
             "destination": "%s"}]}|}
    branch
    sender
    counter
    receiver

let sign_operation_bytes (signer : Constant.key) (msg : Bytes.t) =
  let open Tezos_crypto in
  let b58_secret_key =
    match String.split_on_char ':' signer.secret with
    | ["unencrypted"; rest] ->
        rest
    | _ ->
        Test.fail "Could not parse secret key"
  in
  let sk = Signature.Secret_key.of_b58check_exn b58_secret_key in
  Signature.(sign ~watermark:Generic_operation sk msg)

let counter = ref 0

let forge_and_inject_operation ~branch ~sender ~receiver ~client =
  incr counter ;
  let op_json = operation_json branch sender receiver !counter in
  let* op_hex =
    RPC.post_forge_operations ~data:(Ezjsonm.from_string op_json) client
  in
  let op_str_hex = JSON.as_string op_hex in
  let signature =
    sign_operation_bytes Constant.bootstrap1 (Hex.to_bytes (`Hex op_str_hex))
  in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  let signed_op = op_str_hex ^ signature in
  let* res = RPC.inject_operation ~data:(`String signed_op) client in
  return (Some res)

let forge_and_inject_n_operations ~branch ~sender ~receiver ~client ~node n =
  let rec loop acc = function
    | 0 ->
        return acc
    | n ->
        let transfer_1 = wait_for_injection node in
        let* oph =
          forge_and_inject_operation ~branch ~sender ~receiver ~client
        in
        let* () = transfer_1 in
        let acc = match oph with None -> acc | Some oph -> oph :: acc in
        loop acc (pred n)
  in
  loop [] n

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
  let* client_1 = Client.init ~node:node_1 () in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 in
  Log.info "Node is at level %d." 1 ;
  (* Step 2 *)
  (* Get the counter and the current branch *)
  let* base_counter =
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.identity
      client_1
  in
  counter := JSON.as_int base_counter ;
  let* branch = RPC.get_branch client_1 in
  let branch_s = JSON.encode branch in
  (* Step 3 *)
  (* Forge operation, inject them and check injection *)
  let number_of_transactions =
    (* the batch of operation that a flush handles is 50 by default,
       we choose a value that by-pass this limit *)
    (* TODO: pass a lower limit to the node config init so we may inject less operations *)
    60
  in
  let* ophs =
    forge_and_inject_n_operations
      ~branch:branch_s
      ~sender:Constant.bootstrap1.identity
      ~receiver:Constant.bootstrap2.identity
      ~client:client_1
      ~node:node_1
      number_of_transactions
  in
  (* Step 4 *)
  (* Check that forged operation are in the mempool *)
  let* mempool_after_injections =
    RPC.get_mempool_pending_operations client_1
  in
  let mempool_count_after_injections =
    count_mempool mempool_after_injections
  in
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

let register ~protocols = flush_mempool ~protocols
