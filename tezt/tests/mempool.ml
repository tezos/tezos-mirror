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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1657

   Some refactorisation is needed. All new tests should be in the Revamped
   module (which will be erased once we have rewrote all the Legacy tests. *)

module Mempool = Tezt_tezos.Mempool

module Revamped = struct
  let log_step counter msg =
    let color = Log.Color.(bold ++ FG.blue) in
    let prefix = "step" ^ string_of_int counter in
    Log.info ~color ~prefix msg

  (* We override the default [bake_for] comment to wait on a [flush]
     event from the mempool because the [set_head] event used by the
     default [bake_for] functions happens before a flush of the
     mempool. For mempool tests, we generally prefer to ensure that a
     [flush] did happen than a [set_head].

     Optionnaly, we can decide whether the block should be baked
     without taking the operations of the mempool. *)
  let bake_for ~empty ~protocol node client =
    let mempool_flush_waiter = Node.wait_for_request ~request:`Flush node in
    let* () =
      if empty then
        let* empty_mempool_file = Client.empty_mempool_file () in
        Client.bake_for
          ~mempool:empty_mempool_file
          ~monitor_node_mempool:false
          ~protocol
          client
      else Client.bake_for client
    in
    mempool_flush_waiter

  (** {2 Tests } *)

  (** This test injects some transfer operations and checks that the mempool does
    not lose any operation after a flush even if it contains more than
    operations_batch_size operations. *)
  let flush_mempool =
    let operations_batch_size = 5 in
    let number_of_operations = operations_batch_size * 2 in
    let nb_additional_bootstrap_accounts = number_of_operations in
    Protocol.register_test
      ~__FILE__
      ~title:"Flush mempool"
      ~tags:["mempool"; "flush"]
    @@ fun protocol ->
    log_step
      1
      "Initialize a node with 'operations_batch_size=%d' and %d more bootstrap \
       accounts."
      operations_batch_size
      (number_of_operations - 5) ;
    let node = Node.create [Connections 0; Synchronisation_threshold 0] in
    let* () = Node.config_init node [] in
    Node.Config_file.(update node (set_prevalidator ~operations_batch_size)) ;
    let* () = Node.run node [] in
    let* () = Node.wait_for_ready node in
    let* client = Client.init ~endpoint:(Node node) () in
    let nb_existing_bootstrap_accounts =
      List.length Constant.all_bootstrap_keys
    in
    let* additional_bootstrap_accounts =
      Lwt_list.map_s
        (fun i ->
          let alias = sf "bootstrap%d" i in
          let* key = Client.gen_and_show_secret_keys ~alias client in
          return (key, None))
        (range
           (nb_existing_bootstrap_accounts + 1)
           (nb_existing_bootstrap_accounts + nb_additional_bootstrap_accounts))
    in
    let* parameter_file =
      Protocol.write_parameter_file
        ~additional_bootstrap_accounts
        ~base:(Either.right protocol)
        []
    in
    let* () = Client.activate_protocol ~parameter_file ~protocol client in
    let* _ = Node.wait_for_level node 1 in

    log_step 2 "Inject %d transfer operations." number_of_operations ;
    let* _ =
      Tezos_base__TzPervasives.List.iter_s
        (fun ((key : Account.key), _) ->
          Client.transfer
            ~amount:(Tez.of_int 1)
            ~giver:key.alias
            ~receiver:Constant.bootstrap1.alias
            client)
        additional_bootstrap_accounts
    in

    log_step 3 "Check operations are all classified as 'Applied'." ;
    let* mempool = RPC.get_mempool client in
    let error_msg =
      "some operations not classified as 'applied: expected length %R, got %L"
    in
    Check.((List.length mempool.applied = number_of_operations) int ~error_msg) ;

    log_step 4 "Bake a block with an empty mempool." ;
    let* () = bake_for ~empty:true ~protocol node client in
    let* mempool_after_empty_block = RPC.get_mempool client in

    log_step 5 "Check that we did not lose any operation." ;
    let error_msg =
      "operations were lost after the flush: expected %L, got %R"
    in
    Check.((mempool = mempool_after_empty_block) Mempool.typ ~error_msg) ;

    log_step 6 "Inject endorsement operations." ;
    let* () = Client.endorse_for client ~protocol ~force:true in
    let* mempool_with_endorsement = RPC.get_mempool client in

    log_step 7 "Check endorsement is applied." ;
    let mempool_diff =
      Mempool.symmetric_diff mempool_after_empty_block mempool_with_endorsement
    in
    (* [mempool_diff] should contain only the applied endorsement. *)
    let mempool_expected =
      let open Mempool in
      try {empty with applied = [List.hd mempool_diff.applied]}
      with Not_found -> {empty with applied = ["<applied field was empty>"]}
    in
    let error_msg = "endorsement is not applied: expected %L, got %R" in
    Check.((mempool_expected = mempool_diff) Mempool.typ ~error_msg) ;

    log_step 8 "Bake with an empty mempool twice." ;
    let* () = repeat 2 (fun () -> bake_for ~protocol ~empty:true node client) in
    let* last_mempool = RPC.get_mempool client in

    log_step 9 "Check endorsement is classified 'Outdated'." ;
    let error_msg = "one applied operation was lost: expected %L, got %R" in
    Check.((mempool_with_endorsement = last_mempool) Mempool.typ ~error_msg) ;
    let error_msg =
      "endorsement is not classified as 'outdated': length expected %L, got %R"
    in
    Check.(
      (List.compare_length_with last_mempool.outdated 1 = 0) int ~error_msg) ;
    unit
end

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
  outdated : int;
  unprocessed : int;
  total : int;
}

let count_mempool mempool =
  let open JSON in
  let applied = as_list (mempool |-> "applied") |> List.length in
  let branch_delayed = as_list (mempool |-> "branch_delayed") |> List.length in
  let branch_refused = as_list (mempool |-> "branch_refused") |> List.length in
  let refused = as_list (mempool |-> "refused") |> List.length in
  let outdated = as_list (mempool |-> "outdated") |> List.length in
  let unprocessed = as_list (mempool |-> "unprocessed") |> List.length in
  let total =
    applied + branch_delayed + branch_refused + refused + outdated + unprocessed
  in
  {
    applied;
    branch_delayed;
    branch_refused;
    refused;
    outdated;
    unprocessed;
    total;
  }

let pp_mempool_count fmt
    {
      applied;
      branch_delayed;
      branch_refused;
      refused;
      outdated;
      unprocessed;
      total;
    } =
  Format.fprintf
    fmt
    "total: %d - applied: %d, branch_delayed: %d, branch_refused: %d, refused: \
     %d, outdated: %d, unprocessed: %d"
    total
    applied
    branch_delayed
    branch_refused
    refused
    outdated
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

let sign_operation ~signer op_str_hex =
  let signature =
    Operation.sign_bytes
      ~watermark:Generic_operation
      signer
      (Hex.to_bytes (`Hex op_str_hex))
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

(** Bakes with an empty mempool to force synchronisation between nodes. *)
let bake_empty_mempool ?endpoint client =
  let* mempool = Client.empty_mempool_file () in
  Client.bake_for ?endpoint ~mempool client

(** [bake_empty_mempool_and_wait_for_flush client node] bakes for [client]
    with an empty mempool, then waits for a [flush] event on [node] (which
    will usually be the node corresponding to [client], but could be any
    node with a connection path to it). *)
let _bake_empty_mempool_and_wait_for_flush ?(log = false) client node =
  let waiter = wait_for_flush node in
  let* () = bake_empty_mempool client in
  if log then
    Log.info "Baked for %s with an empty mempool." (Client.name client) ;
  waiter

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
  let* node_1 =
    Node.init [Synchronisation_threshold 0; Disable_operations_precheck]
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
     We use the disable-precheck option to force application of operation in
     the prevalidator and to force classification in branch_refused. *)
  let* node_2 =
    Node.init [Synchronisation_threshold 0; Disable_operations_precheck]
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
     We use the disable-precheck option to force application of operation in
     the prevalidator and to force classification in branch_refused. *)
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
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client_1
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap2.public_key_hash
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap3.public_key_hash
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
  let* empty_mempool_file = Client.empty_mempool_file () in
  let flush_waiter_1 = wait_for_flush node_1 in
  let flush_waiter_2 = wait_for_flush node_2 in
  let* () =
    Client.bake_for
      ~protocol
      ~mempool:empty_mempool_file
      ~monitor_node_mempool:false
      client_1
  in
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
    Operation.sign_bytes
      ~watermark:Generic_operation
      signer
      (Hex.to_bytes (`Hex op_str_hex))
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
        List.compare_length_with contents n = 0 && h = as_string oph)
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
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client_1
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
      ~source:Constant.bootstrap2.public_key_hash
      ~destination:Constant.bootstrap1.public_key_hash
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

let check_if_op_is_in_mempool client ~classification oph =
  let* ops = RPC.get_mempool_pending_operations ~version:"1" client in
  let open JSON in
  let search_in ops c =
    List.exists
      (fun op -> get "hash" op |> as_string = oph)
      (ops |-> c |> as_list)
  in
  match classification with
  | Some c ->
      let res = search_in ops c in
      if not res then Test.fail "%s not found in %s" oph c else unit
  | None ->
      let res =
        List.exists
          (fun c -> search_in ops c)
          ["applied"; "branch_refused"; "branch_delayed"; "refused"; "outdated"]
      in
      if res then Test.fail "%s found in mempool" oph else unit

let get_endorsement_has_bytes ~protocol client =
  let* mempool = RPC.get_mempool_pending_operations client in
  let open JSON in
  let ops_list = as_list (mempool |-> "applied") in
  let op =
    match ops_list with
    | [op] -> op
    | _ ->
        Test.fail
          "Applied field of mempool should contain one and only one operation"
  in
  let hash = JSON.get "hash" op |> as_string in
  let shell =
    let branch = JSON.as_string (JSON.get "branch" op) in
    match Data_encoding.Json.from_string (sf {|{"branch":"%s"}|} branch) with
    | Ok b ->
        Data_encoding.Json.destruct Tezos_base.Operation.shell_header_encoding b
    | Error e -> Test.fail "Data_encoding branch from %s error %s" branch e
  in
  let contents =
    match JSON.as_list (JSON.get "contents" op) with
    | [content] -> content
    | _ -> Test.fail "Contents should countain only one element"
  in
  let slot = JSON.get "slot" contents |> JSON.as_int in
  let get_signature op =
    let signature = JSON.get "signature" op |> JSON.as_string in
    match Data_encoding.Json.from_string (sf {|"%s"|} signature) with
    | Ok s -> Data_encoding.Json.destruct Tezos_crypto.Signature.encoding s
    | Error e ->
        Test.fail
          "Data_encoding signature from string %s : error %s"
          signature
          e
  in
  let wrapped_bytes =
    match protocol with
    | Protocol.Alpha ->
        let signature = get_signature op in
        let kind = JSON.get "kind" contents |> JSON.as_string in
        if not (kind = "endorsement") then
          Test.fail "Operation kind should be endorsement, got %s" kind ;
        let level =
          Tezos_protocol_alpha.Protocol.Raw_level_repr.of_int32_exn
            (Int32.of_int (JSON.get "level" contents |> JSON.as_int))
        in
        let round =
          let round = JSON.get "round" contents |> JSON.as_int in
          match
            Tezos_protocol_alpha.Protocol.Round_repr.of_int32
              (Int32.of_int round)
          with
          | Ok round -> round
          | Error _ ->
              Test.fail
                "Could not create a round with %d (from the mempool result) "
                round
        in
        let block_payload_hash =
          let block_payload_hash =
            JSON.get "block_payload_hash" contents |> JSON.as_string
          in
          Tezos_protocol_alpha.Protocol.Block_payload_hash.of_b58check_exn
            block_payload_hash
        in
        let wrapped =
          Tezos_protocol_alpha.Protocol.Operation_repr.
            {
              shell;
              protocol_data =
                Operation_data
                  {
                    contents =
                      Single
                        (Endorsement {slot; round; level; block_payload_hash});
                    signature = Some signature;
                  };
            }
        in
        Data_encoding.Binary.to_bytes_exn
          Tezos_protocol_alpha.Protocol.Operation_repr.encoding
          wrapped
    | Protocol.Granada | Protocol.Hangzhou ->
        let endorsement = JSON.get "endorsement" contents in
        let signature = get_signature endorsement in
        let level =
          Tezos_protocol_010_PtGRANAD.Protocol.Raw_level_repr.of_int32_exn
            (Int32.of_int
               (JSON.get "operations" endorsement
               |> JSON.get "level" |> JSON.as_int))
        in
        let wrapped =
          Tezos_protocol_010_PtGRANAD.Protocol.Operation_repr.
            {
              shell;
              protocol_data =
                Operation_data
                  {
                    contents =
                      Single
                        (Endorsement_with_slot
                           {
                             endorsement =
                               {
                                 shell;
                                 protocol_data =
                                   {
                                     contents =
                                       Single
                                         (Tezos_protocol_010_PtGRANAD.Protocol
                                          .Operation_repr
                                          .Endorsement
                                            {level});
                                     signature = Some signature;
                                   };
                               };
                             slot;
                           });
                    signature = None;
                  };
            }
        in
        Data_encoding.Binary.to_bytes_exn
          Tezos_protocol_010_PtGRANAD.Protocol.Operation_repr.encoding
          wrapped
  in
  Lwt.return (wrapped_bytes, hash)

let wait_for_synch node =
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "notify" -> Some s
    | Some _ | None -> None
  in
  let* _ = Node.wait_for node "request_completed_debug.v0" filter in
  return ()

let mempool_synchronisation client node =
  let waiter = wait_for_synch node in
  let* _ = RPC.mempool_request_operations client in
  waiter

(** This test checks that future endorsement are still propagated when
    the head is  incremented *)
let propagation_future_endorsement =
  let step1_msg =
    "Step 1: 3 nodes are initialised, chain connected and the protocol is \
     activated."
  in
  let step2_msg = "Step 2: disconnect the nodes" in
  let step3_msg = "Step 3: bake one block on node_1" in
  let step4_msg = "Step 4: Endorsement on node_1 injected" in
  let step5_msg =
    "Step 5: recover hash endorsement and bytes representing the endorsement"
  in
  let step6_msg =
    "Step 6: ban the endorsement on node_1 to ensure it will not be propagated \
     from this node"
  in
  let step7_msg = "Step 7: Endorsement has been inject on node_2" in
  let step8_msg =
    "Step 8: Reconnect node_2 and node_3 and synchronise their mempool"
  in
  let step9_msg =
    "Step 9: ensure that endorsement is in node_2 mempool and classified as \
     branch_delayed"
  in
  let step10_msg =
    "Step 10: ensure that endorsement is not in node_3 mempool"
  in
  let step11_msg = "Step 11: Reconnect node_1 and node_2, new head on node_2" in
  let step12_msg =
    "Step 12: Synchronise mempool on node_2 and check that endorsement is now \
     applied"
  in
  let step13_msg =
    "Step 13: Synchronise mempool on node_3 and check that endorsement has \
     been propagated"
  in
  Protocol.register_test
    ~__FILE__
    ~title:"Ensure that future endorsement are propagated"
    ~tags:["endorsement"; "mempool"; "branch_delayed"]
  @@ fun protocol ->
  let* node_1 = Node.init [Synchronisation_threshold 0; Private_mode]
  and* node_2 =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
      [Synchronisation_threshold 0; Private_mode]
  and* node_3 =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
      [Synchronisation_threshold 0; Private_mode]
  in
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
  let* _ = Node.wait_for_level node_1 1
  and* _ = Node.wait_for_level node_2 1
  and* _ = Node.wait_for_level node_3 1 in
  Log.info "%s" step1_msg ;
  let* node_1_id = Node.wait_for_identity node_1
  and* node_2_id = Node.wait_for_identity node_2
  and* node_3_id = Node.wait_for_identity node_3 in
  let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id
  and* () = Client.Admin.kick_peer client_2 ~peer:node_1_id
  and* () = Client.Admin.kick_peer client_2 ~peer:node_3_id
  and* () = Client.Admin.kick_peer client_3 ~peer:node_2_id in
  Log.info "%s" step2_msg ;
  let* () = Node_event_level.bake_wait_log node_1 client_1 in
  Log.info "%s" step3_msg ;
  let endorser_waiter = wait_for_injection node_1 in
  let* () = Client.endorse_for client_1 ~force:true ~protocol in
  let* () = endorser_waiter in
  Log.info "%s" step4_msg ;
  let* (bytes, hash) = get_endorsement_has_bytes ~protocol client_1 in
  Log.info "%s" step5_msg ;
  let* _ = RPC.mempool_ban_operation ~data:(`String hash) client_1 in
  Log.info "%s" step6_msg ;
  let (`Hex bytes) = Hex.of_bytes bytes in
  let injection_waiter = wait_for_injection node_2 in
  let* _ = RPC.private_inject_operation ~data:(`String bytes) client_2 in
  let* () = injection_waiter in
  Log.info "%s" step7_msg ;
  let* () = Client.Admin.trust_address client_2 ~peer:node_3
  and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
  let* () = Client.Admin.connect_address client_2 ~peer:node_3 in
  let* _ = mempool_synchronisation client_3 node_3 in
  Log.info "%s" step8_msg ;
  let* _ =
    check_if_op_is_in_mempool
      client_2
      ~classification:(Some "branch_delayed")
      hash
  in
  Log.info "%s" step9_msg ;
  let* _ = check_if_op_is_in_mempool client_3 ~classification:None hash in
  Log.info "%s" step10_msg ;
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  Log.info "%s" step11_msg ;
  let* _ = mempool_synchronisation client_2 node_2 in
  let* _ =
    check_if_op_is_in_mempool client_2 ~classification:(Some "applied") hash
  in
  Log.info "%s" step12_msg ;
  let* _ = mempool_synchronisation client_3 node_3 in
  let* _ =
    check_if_op_is_in_mempool client_3 ~classification:(Some "applied") hash
  in
  Log.info "%s" step13_msg ;
  unit

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
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client_1
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap2.public_key_hash
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
  let* () =
    Node.run ~event_sections_levels:[("prevalidator", `Debug)] node_2 []
  in
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
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client_1
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap2.public_key_hash
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
let bake_empty_mempool ?protocol ?endpoint client =
  let* mempool = Client.empty_mempool_file () in
  Client.bake_for
    ?protocol
    ?endpoint
    ~mempool
    ~monitor_node_mempool:false
    client

(** [bake_empty_mempool_and_wait_for_flush client node] bakes for [client]
    with an empty mempool, then waits for a [flush] event on [node] (which
    will usually be the node corresponding to [client], but could be any
    node with a connection path to it). *)
let bake_empty_mempool_and_wait_for_flush ~protocol ?(log = false) client node =
  let waiter = wait_for_flush node in
  let* () = bake_empty_mempool ~protocol client in
  if log then
    Log.info "Baked for %s with an empty mempool." (Client.name client) ;
  waiter

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
      ~event_sections_levels:
        [("prevalidator", `Debug)] (* to witness operation arrival event *)
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
  let* () = bake_empty_mempool_and_wait_for_flush ~protocol client_2 node_2 in
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
  let* () =
    Client.transfer
      ~amount:(Tez.of_int amount_int)
      ~giver:Account.(giver_key.alias)
      ~receiver:Account.(receiver_key.alias)
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
  Int.equal (List.compare_lengths l1 l2) 0
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
      ~event_sections_levels:
        [("prevalidator", `Debug)] (* to witness operation arrival events *)
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
  if not (Int.equal (List.compare_length_with applied_ophs 5) 0) then
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
        Log.info "Witnessed arrival of operation %s." ophash ;
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
      ~event_sections_levels:
        [("prevalidator", `Debug)] (* to witness operation arrival events *)
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
  let* () = bake_empty_mempool ~protocol client_1 in
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
        if not (Int.equal (List.compare_length_with tl 3) 0) then
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
      ~event_sections_levels:[("prevalidator", `Debug)]
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
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client_1
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap2.public_key_hash
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
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap3.public_key_hash
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
  let* empty_mempool_file = Client.empty_mempool_file () in
  let flush_waiter_1 = wait_for_flush node_1 in
  let flush_waiter_2 = wait_for_flush node_2 in
  let* () =
    Client.bake_for
      ~protocol
      ~mempool:empty_mempool_file
      ~monitor_node_mempool:false
      client_1
  in
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
  let* () =
    check_if_op_is_in_mempool
      client_1
      ~classification:(Some "branch_refused")
      oph_branch_refused
  in
  (* Step 12  *)
  (* Bake on node_2 to force higher fitness  *)
  let* () =
    repeat 2 (fun () ->
        Node_event_level.bake_wait_log
          ~protocol
          ~mempool:empty_mempool_file
          ~monitor_node_mempool:false
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
  let* () =
    Client.bake_for
      ~protocol
      ~mempool:empty_mempool_file
      ~monitor_node_mempool:false
      client_1
  in
  let* pending = bake_waiter_1 in
  if pending <> 2 then Test.fail "the two operations should be reclassified" ;
  unit

(** Calls RPC [POST /chains/main/mempool/filter] from [client], with [data]
    formatted from string [config_str]. If [log] is [true], also logs this
    string. *)
let set_filter ?(log = false) config_str client =
  let* res =
    RPC.post_mempool_filter ~data:(Ezjsonm.from_string config_str) client
  in
  if log then Log.info "Updated filter config with: %s." config_str ;
  return res

(** [set_filter_no_fee_requirement client] sets all fields [minimal_*]
    to 0 in the filter configuration of [client]'s mempool. *)
let set_filter_no_fee_requirement =
  set_filter
    {|{ "minimal_fees": "0", "minimal_nanotez_per_gas_unit": [ "0", "1" ], "minimal_nanotez_per_byte": [ "0", "1" ] }|}

(** Checks that arguments [applied] and [refused] are the number of operations
    in the mempool of [client] with the corresponding classification,
    that both sets of operations are disjoint, and that there is no
    [branch_delayed], [branch_refused], or [unprocessed] operation.
    If [log] is [true], also logs the hash and fee of all applied
    and refused operations. *)
let check_mempool_ops ?(log = false) client ~applied ~refused =
  let name = Client.name client in
  let log_op =
    if log then fun classification hash fee ->
      Log.info
        ~color:Log.Color.FG.yellow
        ~prefix:(name ^ ", " ^ classification)
        "%s (fee: %d)"
        hash
        fee
    else fun _ _ _ -> ()
  in
  let* ops = RPC.get_mempool_pending_operations client in
  let open JSON in
  (* get (and log) applied operations *)
  let applied_ophs =
    let classification = "applied" in
    List.map
      (fun op ->
        let oph = get_hash op in
        log_op classification oph (op |-> "contents" |=> 0 |-> "fee" |> as_int) ;
        oph)
      (ops |-> classification |> as_list)
  in
  (* get (and log) refused operations *)
  let refused_ophs =
    let classification = "refused" in
    List.map
      (fun op ->
        match op |> as_list with
        | [oph; descr] ->
            let oph = as_string oph in
            log_op
              classification
              oph
              (descr |-> "contents" |=> 0 |-> "fee" |> as_int) ;
            oph
        | _ ->
            Test.fail
              "Unexpected JSON structure for refused operation in %s's mempool."
              name)
      (ops |-> classification |> as_list)
  in
  (* various checks about applied and refused operations *)
  Check.(
    (List.compare_length_with applied_ophs applied = 0)
      int
      ~error_msg:(name ^ ": found %L applied operation(s), expected %R.")) ;
  Check.(
    (List.compare_length_with refused_ophs refused = 0)
      int
      ~error_msg:(name ^ ": found %L refused operation(s), expected %R.")) ;
  List.iter
    (fun oph ->
      if List.mem oph refused_ophs then
        Test.fail "%s: operation %s is both applied and refused" name oph)
    applied_ophs ;
  (* check that other classifications are empty *)
  List.iter
    (fun classification ->
      match ops |-> classification |> as_list with
      | [] -> ()
      | _ ->
          Test.fail
            "%s: unexpectedly found %s operation(s): %s"
            name
            classification
            (ops |-> classification |> encode))
    ["outdated"; "branch_refused"; "branch_delayed"; "unprocessed"] ;
  unit

(** Waits for [node] to receive a notification from a peer of a mempool
    containing exactly [n_ops] valid operations. *)
let wait_for_notify_n_valid_ops node n_ops =
  Node.wait_for node "node_prevalidator.v0" (fun event ->
      let open JSON in
      let view = event |=> 1 |-> "request_view" in
      match view |-> "request" |> as_string_opt with
      | Some "notify" ->
          let valid_ophs = view |-> "mempool" |-> "known_valid" |> as_list in
          if Int.equal (List.compare_length_with valid_ophs n_ops) 0 then
            Some ()
          else None
      | _ -> None)

(** Checks that the last block of [client] contains exactly
    [n_manager_ops] manager operations (which includes the transfer
    operations). *)
let check_n_manager_ops_in_block ?(log = false) client n_manager_ops =
  let* baked_ops = RPC.get_operations client in
  let baked_manager_ops = JSON.(baked_ops |=> 3 |> as_list) in
  Check.(
    (List.compare_length_with baked_manager_ops n_manager_ops = 0)
      int
      ~error_msg:
        "The baked block contains %L manager operation(s), expected %R.") ;
  if log then
    Log.info "The baked block contains %d manager operation(s)." n_manager_ops ;
  unit

let iter2_p f l1 l2 = Lwt.join (List.map2 f l1 l2)

(** Test.

    Aim: test that a refused operation is not reclassified even though
    it would now be valid.

    Scenario:
    - Step 1: Start two nodes, connect them, and activate the protocol.
    - Step 2: In [node2]'s mempool filter configuration, set all fields
      [minimal_*] to 0, so that [node2] accepts operations with any fee.
    - Step 3: Inject two operations (transfers) in [node2] with respective
      fees 1000 and 10 mutez. Check that both operations are [applied] in
      [node2]'s mempool.
    - Step 4: Bake with an empty mempool for [node1] to force synchronization
      with [node2]. Check that the mempool of [node1] has one applied and one
      refused operation. Indeed, [node1] has the default filter config with
      [minimal_fees] at 100 mutez.
    - Step 5: In [node1]'s mempool filter configuration, set all fields
      [minimal_*] to 0. Inject a new operation with fee 5 in [node2], then
      bake with an empty mempool. Check that [node1] contains two applied
      operations (the ones with fee 1000 and 5) and one refused operation.
      Indeed, the operation with fee 10 would now be valid, but it has already
      been refused so it must not be revalidated.
    - Step 6: Bake for [node1] (normally, i.e. without enforcing a given
      mempool). Check that the baked block contains exactly one manager
      operation (the category containing transfer operations). Indeed, the
      filter used to determine which operations are included in the block does
      not share its configuration with the mempool's filter, so only the
      operation of fee 1000 is included. Check that [node1] contains one
      applied operation (fee 5) and one refused operation (fee 10), and that
      [node2] contains two applied operations. *)
let test_do_not_reclassify =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool do not reclassify"
    ~tags:["mempool"; "node"; "filter"; "refused"; "applied"]
  @@ fun protocol ->
  let step_color = Log.Color.BG.blue in
  Log.info
    ~color:step_color
    "Step 1: Start two nodes, connect them, and activate the protocol." ;
  let* node1 =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
      [Synchronisation_threshold 0; Connections 1]
  and* node2 = Node.init [Synchronisation_threshold 0; Connections 1] in
  let* client1 = Client.init ~endpoint:Client.(Node node1) ()
  and* client2 = Client.init ~endpoint:Client.(Node node2) () in
  let* () = Client.Admin.connect_address client1 ~peer:node2
  and* () = Client.activate_protocol ~protocol client1 in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node1 proto_activation_level
  and* _ = Node.wait_for_level node2 proto_activation_level in
  Log.info "Both nodes are at level %d." proto_activation_level ;
  Log.info
    ~color:step_color
    "Step 2: In [node2]'s mempool filter configuration, set all fields \
     [minimal_*] to 0, so that [node2] accepts operations with any fee." ;
  let* _ = set_filter_no_fee_requirement client2 in
  Log.info "Node2 filter config: all [minimal_*] set to 0." ;
  Log.info
    ~color:step_color
    "Step 3: Inject two operations (transfers) in [node2] with respective fees \
     1000 and 10 mutez. Check that both operations are [applied] in [node2]'s \
     mempool." ;
  let waiter_arrival_node1 = wait_for_arrival node1 in
  let inject_transfer from_key ~fee =
    let waiter = wait_for_injection node2 in
    let _ =
      Client.transfer
        ~wait:"0"
        ~amount:(Tez.of_int 1)
        ~giver:from_key.Account.alias
        ~receiver:Constant.bootstrap5.alias
        ~fee:(Tez.of_mutez_int fee)
        client2
    in
    waiter
  in
  let bootstraps = Constant.[bootstrap1; bootstrap2] in
  let fees = [1000; 10] in
  let* () = iter2_p (fun key fee -> inject_transfer key ~fee) bootstraps fees in
  Log.info
    "Injected transfers in node2 with fees: %s."
    (String.concat "; " (List.map Int.to_string fees)) ;
  let* () = check_mempool_ops ~log:true client2 ~applied:2 ~refused:0 in
  Log.info
    ~color:step_color
    "Step 4: Bake with an empty mempool for [node1] to force synchronization \
     with [node2]. Check that the mempool of [node1] has one applied and one \
     refused operation. Indeed, [node1] has the default filter config with \
     [minimal_fees] at 100 mutez." ;
  let* () =
    bake_empty_mempool_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  let* () = waiter_arrival_node1 in
  let* () = check_mempool_ops ~log:true client1 ~applied:1 ~refused:1 in
  Log.info
    ~color:step_color
    "Step 5: In [node1]'s mempool filter configuration, set all fields \
     [minimal_*] to 0. Inject a new operation with fee 5 in [node2], then bake \
     with an empty mempool. Check that [node1] contains two applied operations \
     (the ones with fee 1000 and 5) and one refused operation. Indeed, the \
     operation with fee 10 would now be valid, but it has already been refused \
     so it must not be revalidated." ;
  let* _ = set_filter_no_fee_requirement client1 in
  let* () = inject_transfer Constant.bootstrap3 ~fee:5 in
  let waiter_notify_3_valid_ops = wait_for_notify_n_valid_ops node1 3 in
  let* () =
    bake_empty_mempool_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  (* Wait for [node1] to receive a mempool containing 3 operations (the
     number of [applied] operations in [node2]), among which will figure
     the operation with fee 10 that has already been [refused] in [node1]. *)
  let* () = waiter_notify_3_valid_ops in
  let* () = check_mempool_ops ~log:true client1 ~applied:2 ~refused:1 in
  Log.info
    ~color:step_color
    "Step 6: Bake for [node1] (normally, i.e. without enforcing a given \
     mempool). Check that the baked block contains exactly one manager \
     operation (the category containing transfer operations). Indeed, the \
     filter used to determine which operations are included in the block does \
     not share its configuration with the mempool's filter, so only the \
     operation of fee 1000 is included. Check that [node1] contains one \
     applied operation (fee 5) and one refused operation (fee 10), and that \
     [node2] contains 2 applied operations." ;
  let* () = bake_wait_log ~protocol node1 client1 in
  let* () = check_n_manager_ops_in_block ~log:true client1 1 in
  let* () = check_mempool_ops ~log:true client1 ~applied:1 ~refused:1 in
  let* () = check_mempool_ops ~log:true client2 ~applied:2 ~refused:0 in
  unit

let get_refused_operation_hash_list_v0 mempool =
  List.map
    (fun op -> JSON.(op |=> 0 |> as_string))
    JSON.(mempool |-> "refused" |> as_list)

let get_refused_operation_hash_list_v1 mempool =
  List.map get_hash JSON.(mempool |-> "refused" |> as_list)

(** This test tries to check the format of different versions of
    pending_operations RPC.

   Scenario:

   + Node 1 activates a protocol

   + Inject operation on node_1 with low fees

   + Bake empty block to classify operation as refused

   + Get the hash of the operation using different versions of pending_operation RPC
     and check that they are the same *)
let test_pending_operation_version =
  Protocol.register_test
    ~__FILE__
    ~title:""
    ~tags:["mempool"; "pending_operations"; "version"]
  @@ fun protocol ->
  let open Lwt in
  (* Step 1 *)
  (* Initialise one node *)
  let* node_1 =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
      [Synchronisation_threshold 0; Private_mode]
  in
  let* client_1 = Client.init ~endpoint:(Node node_1) () in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 in
  (* Step 2 *)
  (* Inject refused operation *)
  let* branch = RPC.get_branch client_1 >|= JSON.as_string in
  let* _ =
    forge_and_inject_operation
      ~branch
      ~fee:10
      ~gas_limit:1040
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap2.public_key_hash
      ~counter:1
      ~signer:Constant.bootstrap1
      ~client:client_1
  in
  (* Step 3 *)
  (* Bake empty block to force operation to be classify as refused *)
  let dummy_baking = wait_for_flush node_1 in
  let* () = bake_empty_mempool client_1 in
  let* () = dummy_baking in
  (* Step 4 *)
  (* Get pending operations using different version of the RPC and check  *)
  let* mempool_v0 = RPC.get_mempool_pending_operations client_1 in
  let* mempool_v1 = RPC.get_mempool_pending_operations ~version:"1" client_1 in
  let ophs_refused_v0 = get_refused_operation_hash_list_v0 mempool_v0 in
  let ophs_refused_v1 = get_refused_operation_hash_list_v1 mempool_v1 in
  try
    if not (List.for_all2 String.equal ophs_refused_v0 ophs_refused_v1) then
      Format.kasprintf
        (Test.fail "%s")
        "Refused operation hash list should have the same elements. Got : %a \
         (version 1) and %a (version 2)"
        (Format.pp_print_list (fun ppf oph -> Format.fprintf ppf "%s" oph))
        ophs_refused_v0
        (Format.pp_print_list (fun ppf oph -> Format.fprintf ppf "%s" oph))
        ophs_refused_v1 ;
    unit
  with Invalid_argument _ ->
    Format.kasprintf
      (Test.fail "%s")
      "Refused operation hash list should have the same number of elements. \
       Got : %d (version 1) and %d (version 2)"
      (List.length ophs_refused_v0)
      (List.length ophs_refused_v1)

(** This test tries to check that invalid operation can be injected on a local
    node with private/injection/operation RPC *)
let force_operation_injection =
  let step1_msg =
    "Step 1: Create one node with specific configuration that mimic a node \
     with secure ACL policy"
  in
  let step2_msg =
    "Step 2: Initialize a second node, connect both node and activate the \
     protocol"
  in
  let step3_msg = "Step 3: Get the counter and the current branch" in
  let step4_msg = "Step 4: Forge and sign operation with incorrect counter" in
  let step5_msg =
    "Step 5: Inject the operation on the secure node, and check for error \
     because the operation was refused"
  in
  let step6_msg =
    "Step 6: Force injection of operation on the secure node, and check for \
     error because we don't have the right to use this rpc"
  in
  let step7_msg =
    "Step 7: Inject operation on the local node, and check for error because \
     the operation was refused"
  in
  let step8_msg = "Step 8: Force injection of operation on local node" in
  Protocol.register_test
    ~__FILE__
    ~title:"force invalid operation injection"
    ~tags:["force"; "mempool"]
  @@ fun protocol ->
  Log.info "%s" step1_msg ;
  let node1 = Node.create [] in
  let* () = Node.config_init node1 [] in
  let address =
    Node.rpc_host node1 ^ ":" ^ string_of_int (Node.rpc_port node1)
  in
  let acl =
    JSON.annotate ~origin:"whitelist"
    @@ `A
         [
           `O
             [
               ("address", `String address);
               ( "whitelist",
                 `A
                   [
                     (* We do not add all RPC allowed in secure mode,
                        only the ones that are useful for this test. *)
                     `String "POST /injection/operation";
                     `String "GET /chains/*/blocks/*/protocols";
                     `String "GET /describe/**";
                   ] );
             ];
         ]
  in
  Node.Config_file.update node1 (JSON.update "rpc" (JSON.put ("acl", acl))) ;
  let* () = Node.identity_generate node1 in
  let* () = Node.run node1 [Synchronisation_threshold 0] in
  let* () = Node.wait_for_ready node1 in
  Log.info "%s" step2_msg ;
  let* node2 = Node.init [Synchronisation_threshold 0] in
  let* client1 = Client.init ~endpoint:Client.(Node node1) ()
  and* client2 = Client.init ~endpoint:Client.(Node node2) () in
  let* () = Client.Admin.connect_address client2 ~peer:node1
  and* () = Client.activate_protocol ~protocol client2 in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node1 proto_activation_level
  and* _ = Node.wait_for_level node2 proto_activation_level in
  Log.info "Both nodes are at level %d." proto_activation_level ;
  let open Lwt in
  Log.info "%s" step3_msg ;
  let* counter =
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client2
    >|= JSON.as_int
  in
  let* branch = RPC.get_branch client2 >|= JSON.as_string in
  Log.info "%s" step4_msg ;
  let* op_str_hex =
    forge_operation
      ~branch
      ~fee:1000 (* Minimal fees to successfully apply the transfer *)
      ~gas_limit:1040 (* Minimal gas to successfully apply the transfer *)
      ~source:Constant.bootstrap2.public_key_hash
      ~destination:Constant.bootstrap1.public_key_hash
      ~counter (* Invalid counter *)
      ~client:client2
  in
  let signature = sign_operation ~signer:Constant.bootstrap2 op_str_hex in
  let signed_op = op_str_hex ^ signature in
  Log.info "%s" step5_msg ;
  let p = RPC.spawn_inject_operation ~data:(`String signed_op) client1 in
  let injection_error_rex =
    rex
      ~opts:[`Dotall]
      "Fatal error:\n  Command failed: Error while applying operation.*:"
  in
  let* () = Process.check_error ~msg:injection_error_rex p in
  Log.info "%s" step6_msg ;
  let p =
    RPC.spawn_private_inject_operation ~data:(`String signed_op) client1
  in
  let access_error_rex =
    rex ~opts:[`Dotall] "Fatal error:\n  .HTTP 403. Access denied to: .*"
  in
  let* () = Process.check_error ~msg:access_error_rex p in
  Log.info "%s" step7_msg ;
  let p = RPC.spawn_inject_operation ~data:(`String signed_op) client2 in
  let* () = Process.check_error ~msg:injection_error_rex p in
  Log.info "%s" step8_msg ;
  let* _ = RPC.private_inject_operation ~data:(`String signed_op) client2 in
  unit

(** This test tries to inject an operation with an old known branch *)
let injecting_old_operation_fails =
  let step1 = "Initialize node and activate protocol" in
  let step2 = "Recover counter and branch" in
  let step3 = "Bake max_op_ttl block" in
  let step4 = "Forge an operation with the old branch" in
  let step5 = "Inject the operation and wait for failure" in
  let log_step = Log.info "Step %d: %s" in
  let max_operations_ttl = 1 in
  Protocol.register_test
    ~__FILE__
    ~title:"Injecting old operation fails"
    ~tags:["mempool"; "injection"]
  @@ fun protocol ->
  let open Lwt in
  log_step 1 step1 ;
  let* node =
    Node.init [Synchronisation_threshold 0; Private_mode; Connections 0]
  in
  let* client = Client.init ~endpoint:(Node node) () in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right protocol)
      [
        ( ["max_operations_time_to_live"],
          Some (string_of_int max_operations_ttl) );
      ]
  in
  let* () = Client.activate_protocol ~protocol ~parameter_file client in
  let* _ = Node.wait_for_level node 1 in
  log_step 2 step2 ;
  let* counter =
    RPC.Contracts.get_counter
      ~contract_id:Constant.bootstrap1.public_key_hash
      client
    >|= JSON.as_int
  in
  let* branch = RPC.get_branch client >|= JSON.as_string in
  log_step 3 step3 ;
  (* To avoid off-by-one mistakes *)
  let blocks_to_bake = 2 in
  let* () =
    repeat (max_operations_ttl + blocks_to_bake) (fun () ->
        Client.bake_for client)
  in
  (* + 1 for the activation block *)
  let* _ = Node.wait_for_level node (max_operations_ttl + blocks_to_bake + 1) in
  log_step 4 step4 ;
  let* op_str_hex =
    forge_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap3.public_key_hash
      ~counter:(counter + 1)
      ~client
  in
  let signature = sign_operation ~signer:Constant.bootstrap1 op_str_hex in
  log_step 5 step5 ;
  let process =
    RPC.spawn_inject_operation ~data:(`String (op_str_hex ^ signature)) client
  in
  let injection_error_rex =
    rex
      ~opts:[`Dotall]
      "Fatal error:\n\
      \  Command failed: Operation .* is branched on a block .* which is too \
       old"
  in
  Process.check_error ~msg:injection_error_rex process

(** Mempool filter configuration. *)
module Filter_config = struct
  type t = {
    minimal_fees : int option;
    minimal_nanotez_per_gas_unit : (int * int) option;
    minimal_nanotez_per_byte : (int * int) option;
    allow_script_failure : bool option;
  }

  let eq_int_pair (f1, s1) (f2, s2) = Int.equal f1 f2 && Int.equal s1 s2

  let equal
      {
        minimal_fees = mf1;
        minimal_nanotez_per_gas_unit = mng1;
        minimal_nanotez_per_byte = mnb1;
        allow_script_failure = asf1;
      }
      {
        minimal_fees = mf2;
        minimal_nanotez_per_gas_unit = mng2;
        minimal_nanotez_per_byte = mnb2;
        allow_script_failure = asf2;
      } =
    Option.equal Int.equal mf1 mf2
    && Option.equal eq_int_pair mng1 mng2
    && Option.equal eq_int_pair mnb1 mnb2
    && Option.equal Bool.equal asf1 asf2

  let pp fmt
      {
        minimal_fees = mf;
        minimal_nanotez_per_gas_unit = mng;
        minimal_nanotez_per_byte = mnb;
        allow_script_failure = asf;
      } =
    [
      Option.map (sf {|"minimal_fees": "%d"|}) mf;
      Option.map
        (fun (n1, n2) ->
          sf {|"minimal_nanotez_per_gas_unit": [ "%d", "%d" ]|} n1 n2)
        mng;
      Option.map
        (fun (n1, n2) ->
          sf {|"minimal_nanotez_per_byte": [ "%d", "%d" ]|} n1 n2)
        mnb;
      Option.map (sf {|"allow_script_failure": %b|}) asf;
    ]
    |> List.map Option.to_list |> List.flatten |> String.concat ", "
    |> Format.fprintf fmt {|{ %s }|}

  let show : t -> string = Format.asprintf "%a" pp

  let check_equal expected actual =
    Check.(
      (expected = actual)
        (equalable pp equal)
        ~error_msg:"Wrong filter configuration: %R.@.Expected: %L.")

  (** Returns the filter configuration corresponding to [json]. If any field
      of {!filter_config} is missing from [json], it is set to the default
      value (i.e. the corresponding value in {!default_config}. *)
  let of_json json =
    let open JSON in
    let as_int_pair_opt t =
      match as_list_opt t with
      | Some [x; y] -> Some (as_int x, as_int y)
      (* A missing field is interpreted as [`Null], from which [as_list_opt]
         produces [Some []]. *)
      | Some [] -> None
      | Some _ | None ->
          Test.fail
            "Constructing a filter_config from json: %s. Expected a list of \
             length 2, found: %s."
            (encode json)
            (encode t)
    in
    {
      minimal_fees = json |-> "minimal_fees" |> as_int_opt;
      minimal_nanotez_per_gas_unit =
        json |-> "minimal_nanotez_per_gas_unit" |> as_int_pair_opt;
      minimal_nanotez_per_byte =
        json |-> "minimal_nanotez_per_byte" |> as_int_pair_opt;
      allow_script_failure = json |-> "allow_script_failure" |> as_bool_opt;
    }

  (** Default filter configuration for protocol alpha
      (in proto_alpha/lib_plugin/plugin.ml). *)

  let default_minimal_fees = 100

  let default_minimal_nanotez_per_gas_unit = (100, 1)

  let default_minimal_nanotez_per_byte = (1000, 1)

  let default_allow_script_failure = true

  let default =
    {
      minimal_fees = Some default_minimal_fees;
      minimal_nanotez_per_gas_unit = Some default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = Some default_minimal_nanotez_per_byte;
      allow_script_failure = Some default_allow_script_failure;
    }

  (** Returns a copy of the given filter config, where missing fields
      (i.e. containing [None]) have been set to their default value. *)
  let fill_with_default
      {
        minimal_fees = mf;
        minimal_nanotez_per_gas_unit = mng;
        minimal_nanotez_per_byte = mnb;
        allow_script_failure = asf;
      } =
    Option.
      {
        minimal_fees = Some (value mf ~default:default_minimal_fees);
        minimal_nanotez_per_gas_unit =
          Some (value mng ~default:default_minimal_nanotez_per_gas_unit);
        minimal_nanotez_per_byte =
          Some (value mnb ~default:default_minimal_nanotez_per_byte);
        allow_script_failure =
          Some (value asf ~default:default_allow_script_failure);
      }

  (** Returns a copy of the given filter config, where fields equal
      to their default value have been removed (i.e. set to [None]). *)
  let clear_default
      {
        minimal_fees = mf;
        minimal_nanotez_per_gas_unit = mng;
        minimal_nanotez_per_byte = mnb;
        allow_script_failure = asf;
      } =
    let clear_if_default eq_fun default = function
      | Some x when eq_fun default x -> None
      | x -> x
    in
    {
      minimal_fees = clear_if_default Int.equal default_minimal_fees mf;
      minimal_nanotez_per_gas_unit =
        clear_if_default eq_int_pair default_minimal_nanotez_per_gas_unit mng;
      minimal_nanotez_per_byte =
        clear_if_default eq_int_pair default_minimal_nanotez_per_byte mnb;
      allow_script_failure =
        clear_if_default Bool.equal default_allow_script_failure asf;
    }

  (** Checks that RPC [GET /chains/main/mempool/filter] returns the
      appropriate result for [expected_config], testing all possibilities
      for optional argument [include_default] (omitted/[true]/[false]). *)
  let check_RPC_GET_all_variations ?(log = false) expected_config client =
    let expected_full = fill_with_default expected_config in
    let* json = RPC.get_mempool_filter client in
    check_equal expected_full (of_json json) ;
    let* json = RPC.get_mempool_filter ~include_default:true client in
    check_equal expected_full (of_json json) ;
    let expected_partial = clear_default expected_config in
    let* json = RPC.get_mempool_filter ~include_default:false client in
    check_equal expected_partial (of_json json) ;
    if log then
      Log.info
        "GET /chains/main/mempool/filter returned expected configurations \
         (respectively including/excluding default fields): %s and %s."
        (show expected_full)
        (show expected_partial) ;
    unit
end

(* Probably to be replaced during upcoming mempool tests refactoring *)
let init_single_node_and_activate_protocol
    ?(arguments = Node.[Synchronisation_threshold 0; Connections 0])
    ?event_sections_levels protocol =
  let* node = Node.init ?event_sections_levels arguments in
  let* client = Client.init ~endpoint:Client.(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node proto_activation_level in
  return (node, client)

(* Probably to be replaced during upcoming mempool tests refactoring *)
let init_two_connected_nodes_and_activate_protocol ?event_sections_levels1
    ?event_sections_levels2 protocol =
  let arguments = Node.[Synchronisation_threshold 0; Connections 1] in
  let* node1 = Node.init ?event_sections_levels:event_sections_levels1 arguments
  and* node2 =
    Node.init ?event_sections_levels:event_sections_levels2 arguments
  in
  let* client1 = Client.init ~endpoint:Client.(Node node1) ()
  and* client2 = Client.init ~endpoint:Client.(Node node2) () in
  let* () = Client.Admin.connect_address client1 ~peer:node2
  and* () = Client.activate_protocol ~protocol client1 in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node1 proto_activation_level
  and* _ = Node.wait_for_level node2 proto_activation_level in
  return (node1, client1, node2, client2)

(* TMP: to be replaced in !3418 *)
let log_step n msg = Log.info ~color:Log.Color.BG.blue "Step %d: %s" n msg

(** Aim: test RPCs [GET|POST /chains/<chain>/mempool/filter]. *)
let test_get_post_mempool_filter =
  let title = "get post mempool filter" in
  let tags = ["mempool"; "node"; "filter"] in
  let step1_msg = "Start a single node and activate the protocol." in
  let step2_msg =
    "Call RPC [GET /chains/main/mempool/filter], check that we obtain the \
     default configuration (the full configuration when the query parameter \
     [include_default] is either absent or set to [true], or an empty \
     configuration if [include_default] is [false])."
  in
  let step3_msg =
    "Call RPC [POST /chains/main/mempool/filter] for various configurations. \
     Each time, call [GET /chains/main/mempool/filter] with optional parameter \
     include_default omitted/[true]/[false] and check that we obtain the right \
     configuration."
  in
  let step4_msg =
    "Step 4: Post invalid config modifications, check that config is unchanged \
     and event [invalid_mempool_filter_configuration] is witnessed."
  in
  let step5_msg =
    "Step 5: Set the filter to {} and check that this restored the default \
     config. Indeed, fields that are not provided are set to their default \
     value."
  in
  Protocol.register_test ~__FILE__ ~title ~tags @@ fun protocol ->
  let open Filter_config in
  log_step 1 step1_msg ;
  let* (node1, client1) =
    (* We need event level [debug] for event
       [invalid_mempool_filter_configuration]. *)
    init_single_node_and_activate_protocol
      ~event_sections_levels:[("prevalidator", `Debug)]
      protocol
  in
  log_step 2 step2_msg ;
  let* () = check_RPC_GET_all_variations ~log:true default client1 in
  log_step 3 step3_msg ;
  let set_config_and_check msg config =
    Log.info "%s" msg ;
    let* output = set_filter ~log:true (show config) client1 in
    check_equal (fill_with_default config) (of_json output) ;
    check_RPC_GET_all_variations ~log:true config client1
  in
  let* () =
    set_config_and_check
      "Config1: not all fields provided (missing fields should be set to \
       default)."
      {
        minimal_fees = Some 25;
        minimal_nanotez_per_gas_unit = None;
        minimal_nanotez_per_byte = Some (1050, 1);
        allow_script_failure = Some false;
      }
  in
  let* () =
    set_config_and_check
      "Config2: all fields provided and distinct from default."
      {
        minimal_fees = Some 1;
        minimal_nanotez_per_gas_unit = Some (2, 3);
        minimal_nanotez_per_byte = Some (4, 5);
        allow_script_failure = Some false;
      }
  in
  let config3 =
    {
      minimal_fees = None;
      minimal_nanotez_per_gas_unit = Some default_minimal_nanotez_per_gas_unit;
      minimal_nanotez_per_byte = Some (4, 2);
      allow_script_failure = Some default_allow_script_failure;
    }
  in
  let* () =
    set_config_and_check
      "Config3: some of the provided fields equal to default."
      config3
  in
  log_step 4 step4_msg ;
  let config3_full = fill_with_default config3 in
  let test_invalid_config invalid_config_str =
    let waiter =
      Node.wait_for
        node1
        (* This event has level [debug]. *)
        "invalid_mempool_filter_configuration.v0"
        (Fun.const (Some ()))
    in
    let* output = set_filter invalid_config_str client1 in
    check_equal config3_full (of_json output) ;
    let* () = waiter in
    let* output = RPC.get_mempool_filter client1 in
    check_equal config3_full (of_json output) ;
    Log.info "Tested invalid config: %s." invalid_config_str ;
    unit
  in
  let* () =
    Tezos_base__TzPervasives.List.iter_s
      test_invalid_config
      [
        {|{ "minimal_fees": "100", "minimal_nanotez_per_byte": [ "1050", "1" ], "allow_script_failure": false, "invalid_field_name": 0 }|};
        {|{ "minimal_fees": true}|};
        {|{ "minimal_nanotez_per_gas_unit": [ "100" ]}|};
        {|{ "minimal_nanotez_per_gas_unit": [ "100", "1", "10" ]}|};
      ]
  in
  log_step 5 step5_msg ;
  let* output = set_filter ~log:true "{}" client1 in
  check_equal default (of_json output) ;
  check_RPC_GET_all_variations ~log:true default client1

(** Similar to [Node_event_level.transfer_and_wait_for_injection] but more general.
    Should be merged with it during upcoming mempool tests refactoring. *)
let inject_transfer ?(amount = 1) ?(giver_key = Constant.bootstrap1)
    ?(receiver_key = Constant.bootstrap5) ?fee ?(wait_for = wait_for_injection)
    ?node client =
  let waiter = match node with None -> unit | Some node -> wait_for node in
  let _ =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int amount)
      ~giver:giver_key.Account.alias
      ~receiver:receiver_key.Account.alias
      ?fee:(Option.map Tez.of_mutez_int fee)
      client
  in
  waiter

(** Gets the fee of an operation from the json representing the operation. *)
let get_fee op = JSON.(op |-> "contents" |=> 0 |-> "fee" |> as_int)

let check_unordered_int_list_equal expected actual ~error_msg =
  let unordered_int_list_equal l1 l2 =
    let sort = List.sort Int.compare in
    List.equal Int.equal (sort l1) (sort l2)
  in
  Check.(
    (expected = actual)
      (equalable
         Format.(
           pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_int)
         unordered_int_list_equal)
      ~error_msg)

(** Checks that in the mempool of [client], [applied] is the list of
    respective fees of the applied operations (the order of the list
    is not required to be right), and [refused] is the list of respective
    fees of the refused operations. Also logs the hash and fee of all
    these operations. Moreover, check that there is no branch_delayed,
    branch_refused, or unprocessed operation. *)
let check_mempool_ops_fees ~(applied : int list) ~(refused : int list) client =
  let client_name = Client.name client in
  let* ops = RPC.get_mempool_pending_operations ~version:"1" client in
  let check_fees classification expected =
    let classification_ops = JSON.(ops |-> classification |> as_list) in
    let actual =
      List.map
        (fun op ->
          let fee = get_fee op in
          Log.info
            ~color:Log.Color.FG.yellow
            ~prefix:(client_name ^ ", " ^ classification)
            "%s (fee: %d)"
            (get_hash op)
            fee ;
          fee)
        classification_ops
    in
    check_unordered_int_list_equal
      expected
      actual
      ~error_msg:
        (sf
           "In the mempool of %s, %s operations should have respective fees: \
            [%s] but found: [%s]."
           client_name
           classification
           "%L"
           "%R")
  in
  check_fees "applied" applied ;
  check_fees "refused" refused ;
  (* Check that other classifications are empty *)
  List.iter
    (fun classification ->
      match JSON.(ops |-> classification |> as_list) with
      | [] -> ()
      | _ ->
          Test.fail
            "Unexpectedly found %s operation(s) in the mempool of %s:\n%s"
            classification
            client_name
            (JSON.encode ops))
    ["branch_refused"; "branch_delayed"; "unprocessed"] ;
  unit

(** Aim: test that when we modify the filter configuration of the mempool
    using the RPC [POST /chains/<chain>/mempool/filter], this correctly
    impacts the classification of the operations that arrive from a peer. *)
let test_mempool_filter_operation_arrival =
  let title = "mempool filter arrival" in
  let tags = ["mempool"; "node"; "filter"; "refused"; "applied"] in
  let show_fees fees = String.concat "; " (List.map Int.to_string fees) in
  let step1 = "Start two nodes, connect them, and activate the protocol." in
  let step2 =
    "In [node2]'s mempool filter configuration, set all fields [minimal_*] to \
     0, so that [node2] accepts operations with any fee."
  in
  let fee1 = 1000 and fee2 = 101 in
  let feesA = [fee1; fee2] in
  let appliedA2 = feesA in
  let step3 =
    sf
      "Inject two operations (transfers) in [node2] with respective fees (in \
       mutez): %s. Check that both operations are [applied] in [node2]'s \
       mempool."
      (show_fees feesA)
  in
  let appliedA1 = [fee1] and refusedA1 = [fee2] in
  let step4 =
    sf
      "Bake with an empty mempool for [node1] to force synchronization with \
       [node2]. Check that in the mempool of [node1], the operation with fee \
       %d is applied and the one with fee %d is refused. Indeed, [node1] has \
       the default filter config: (minimal fees (mutez): 100, minimal nanotez \
       per gas unit: 100, minimal nanotez per byte: 1000). Moreover, the fee \
       must overcome the SUM of minimal fees, minimal nanotez per gas unit \
       multiplied by the operation's gas, and minimal nanotez per byte \
       multiplied by the operation's size; therefore the operation with fee %d \
       does not qualify."
      fee1
      fee2
      fee2
  in
  let fee3 = 100 and fee4 = 99 in
  let feesB = [fee3; fee4] in
  let appliedB1 = fee3 :: appliedA1 and refusedB1 = fee4 :: refusedA1 in
  let step5 =
    sf
      "Set [minimal_nanotez_per_gas_unit] and [minimal_nanotez_per_byte] to 0 \
       in [node1]. Inject new operations in [node2] with respective fees: %s. \
       Bake again with an empty mempool. Check the operations in the mempool \
       of [node1]: the operation with fee %d should be [applied], while the \
       one with fee %d should be [refused]. Note that the operation with fee \
       %d would now be valid, but it has already been [refused] and cannot be \
       revalidated."
      (show_fees feesB)
      fee3
      fee4
      fee2
  in
  let applied_after_bake_2 = [fee2; fee3; fee4] in
  let step6 =
    sf
      "Bake for [node2] normally (without enforcing a given mempool). Note \
       that the filter used to determine which operations are included in the \
       block does not share its configuration with the mempool's filter, so \
       only the operation with fee %d is included. This will allow us to reuse \
       [bootstrap1] (the author of this operation) to issue a new transfer. \
       Check that [node2] has three [applied] operations left with fees: %s."
      fee1
      (show_fees applied_after_bake_2)
  in
  let fee5 = 10 and fee6 = 0 in
  let feesC = [fee5; fee6] in
  let appliedC2 = applied_after_bake_2 @ feesC in
  let appliedC1 = [fee5; fee3] and refusedC1 = fee6 :: refusedB1 in
  let step7 =
    sf
      "Set [minimal_fees] to 10 in the mempool filter configuration of \
       [node1], while keeping [minimal_nanotez_per_gas_unit] and \
       [minimal_nanotez_per_byte] at 0. Inject operations with fees: %s in \
       [node2], and check that all operations are [applied] in [node2]. Bake \
       again with on empty mempool, and check the operations in [node1]."
      (show_fees feesC)
  in
  Protocol.register_test ~__FILE__ ~title ~tags @@ fun protocol ->
  log_step 1 step1 ;
  let* (node1, client1, node2, client2) =
    init_two_connected_nodes_and_activate_protocol
    (* Need event level [debug] to receive operation arrival events in [node1]. *)
      ~event_sections_levels1:[("prevalidator", `Debug)]
      protocol
  in
  log_step 2 step2 ;
  let* _ = set_filter_no_fee_requirement client2 in
  log_step 3 step3 ;
  let inject_transfers ?receiver_key giver_keys fees =
    iter2_p
      (fun giver_key fee ->
        inject_transfer ?receiver_key ~giver_key ~fee ~node:node2 client2)
      giver_keys
      fees
  in
  let waiter_arrival_node1 = wait_for_arrival node1 in
  let* () = inject_transfers Constant.[bootstrap1; bootstrap2] feesA in
  let* () = check_mempool_ops_fees ~applied:appliedA2 ~refused:[] client2 in
  log_step 4 step4 ;
  let* () =
    bake_empty_mempool_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  let* () = waiter_arrival_node1 in
  let* () =
    check_mempool_ops_fees ~applied:appliedA1 ~refused:refusedA1 client1
  in
  log_step 5 step5 ;
  let* _ =
    set_filter
      ~log:true
      {|{ "minimal_nanotez_per_gas_unit": [ "0", "1" ], "minimal_nanotez_per_byte": [ "0", "1" ] }|}
      client1
  in
  let waiterB = wait_for_arrival node1 in
  let* () = inject_transfers Constant.[bootstrap3; bootstrap4] feesB in
  let* () =
    bake_empty_mempool_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  let* () = waiterB in
  let* () =
    check_mempool_ops_fees ~applied:appliedB1 ~refused:refusedB1 client1
  in
  log_step 6 step6 ;
  let* () = bake_wait_log node2 client2 in
  let* () =
    check_mempool_ops_fees ~applied:applied_after_bake_2 ~refused:[] client2
  in
  log_step 7 step7 ;
  let* _ =
    set_filter
      {|{ "minimal_fees": "10", "minimal_nanotez_per_gas_unit": [ "0", "1" ], "minimal_nanotez_per_byte": [ "0", "1" ] }|}
      client1
  in
  let waiterC = wait_for_arrival node1 in
  let* () =
    inject_transfers
      ~receiver_key:Constant.bootstrap2
      Constant.[bootstrap5; bootstrap1]
      feesC
  in
  let* () = check_mempool_ops_fees ~applied:appliedC2 ~refused:[] client2 in
  let* () =
    bake_empty_mempool_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  let* () = waiterC in
  check_mempool_ops_fees ~applied:appliedC1 ~refused:refusedC1 client1

let test_request_operations_peer =
  let step1_msg = "Step 1: Connect and initialise two nodes " in
  let step2_msg = "Step 2: Disconnect nodes " in
  let step3_msg = "Step 3: Inject an operation " in
  let step4_msg =
    "Step 4: Reconnect nodes, request operations and witness arrival of \
     operation previously injected "
  in
  Protocol.register_test
    ~__FILE__
    ~title:"Test request_operations rpc"
    ~tags:["mempool"; "request_operations"]
  @@ fun protocol ->
  Log.info "%s" step1_msg ;
  let init_node () =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
      [Synchronisation_threshold 0; Private_mode]
  in
  let* node_1 = init_node () and* node_2 = init_node () in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_1 1 and* _ = Node.wait_for_level node_2 1 in
  Log.info "%s" step2_msg ;
  let* node2_identity = Node.wait_for_identity node_2 in
  let* () = Client.Admin.kick_peer ~peer:node2_identity client_1 in
  Log.info "%s" step3_msg ;
  let transfer_1 = wait_for_injection node_1 in
  let _ =
    Client.transfer
      ~wait:"0"
      ~amount:(Tez.of_int 1)
      ~giver:Constant.bootstrap1.alias
      ~receiver:Constant.bootstrap2.alias
      ~counter:1
      client_1
  in
  let* _ = transfer_1 in
  let* oph =
    let* ophs = get_applied_operation_hash_list client_1 in
    match ophs with
    | [oph] -> return oph
    | _ -> Test.fail "Applied mempool should contain exactly one operation"
  in
  Log.info "%s" step4_msg ;
  let wait_mempool = wait_for_arrival_of_ophash oph node_2 in
  let* () = Client.Admin.connect_address ~peer:node_1 client_2 in
  let* node1_identity = Node.wait_for_identity node_1 in
  let* _ = RPC.mempool_request_operations ~peer:node1_identity client_2 in
  let* () = wait_mempool in
  unit

let register ~protocols =
  Revamped.flush_mempool ~protocols ;
  run_batched_operation ~protocols ;
  propagation_future_endorsement ~protocols ;
  forge_pre_filtered_operation ~protocols ;
  refetch_failed_operation ~protocols ;
  ban_operation ~protocols ;
  ban_operation_and_check_applied ~protocols ;
  unban_operation_and_reinject ~protocols ;
  unban_all_operations ~protocols ;
  ban_operation_branch_refused_reevaluated ~protocols ;
  recycling_branch_refused ~protocols ;
  test_do_not_reclassify ~protocols ;
  test_pending_operation_version ~protocols ;
  force_operation_injection ~protocols ;
  injecting_old_operation_fails ~protocols ;
  test_get_post_mempool_filter ~protocols ;
  test_mempool_filter_operation_arrival ~protocols ;
  test_request_operations_peer ~protocols
