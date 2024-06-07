(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:  Store/Shell/Liveblocks
   Invocation: dune exec tezt/tests/main.exe -- --file operations_liveness.ml
   Subject:    Checks liveblocks computation consistency
*)

let team = Tag.layer1

let check_mempool ?validated ?branch_delayed ?branch_refused ?refused ?outdated
    ?unprocessed client =
  let* mempool = Mempool.get_mempool client in
  return
    (Mempool.check_mempool
       ?validated
       ?branch_delayed
       ?branch_refused
       ?refused
       ?outdated
       ?unprocessed
       mempool)

(* Copy-pasted from tenderbake.ml. *)
let baker_at_round_n ?level round client =
  let* json =
    Client.RPC.call client @@ RPC.get_chain_block_helper_baking_rights ?level ()
  in
  match JSON.(json |=> round |-> "delegate" |> as_string_opt) with
  | Some delegate_id -> return delegate_id
  | None ->
      Test.fail
        "Could not find the baker at round %d for level %s"
        round
        (match level with None -> "head" | Some level -> string_of_int level)

let wait_for_branch_switch node level =
  let filter json =
    match JSON.(json |-> "level" |> as_int_opt) with
    | Some l when l = level -> Some ()
    | Some _ -> None
    | None -> None
  in
  Node.wait_for node "branch_switch.v0" filter

let get_max_op_ttl_constant client =
  let* constants =
    Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
  in
  let max_op_ttl =
    JSON.(constants |-> "max_operations_time_to_live" |> as_int)
  in
  return max_op_ttl

let is_in_block ?block client oph =
  let* manager_ophs =
    Client.RPC.call client
    @@ RPC.get_chain_block_operation_hashes_of_validation_pass ?block 3
  in
  Lwt.return @@ List.exists (fun op -> oph = op) manager_ophs

let operation_liveness_reorg =
  Protocol.register_test
    ~__FILE__
    ~title:"Check operation liveness reorg"
    ~tags:[team; "shell"; "liveblocks"; "operations"; "reorg"]
  @@ fun protocol ->
  let nodes_args = Node.[Synchronisation_threshold 0; Connections 1] in
  let* node1, client1 =
    Client.init_with_protocol ~protocol ~nodes_args `Client ()
  in
  let* node2, client2 = Client.init_with_node ~nodes_args `Client () in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in
  let* max_op_ttl = get_max_op_ttl_constant client1 in
  (* If this assertion fails, rework the test. *)
  let () = assert (max_op_ttl = 8) in
  let dest = Constant.bootstrap2 in
  let fee = 1_000 in
  (* Bake max_op_ttl blocks so that block 1 is the most recent
     non-liveblock and head is at level max_op_ttl + 2 = 10.*)
  let* () =
    repeat (max_op_ttl + 1) (fun () -> Client.bake_for_and_wait client1)
  in
  Log.info "Disconnect node2 to create the fork" ;
  let* current_level = Node.get_level node1 in
  let* _ = Node.wait_for_level node2 current_level in
  let* node2_identity = Node.wait_for_identity node2 in
  let* () =
    Client.Admin.kick_peer ~endpoint:(Node node1) ~peer:node2_identity client1
  in

  (* Baking 1 more block on each node so that block 3 is the oldest
     live block, and thus, block 2 is the most recent
     non-liveblock. *)

  (* Node 2 progresses on a branch with a high fitness. *)
  let* baker_high_round =
    baker_at_round_n ~level:(current_level + 1) 1 client2
  in
  let* () = Client.bake_for_and_wait ~keys:[baker_high_round] client2 in

  (* Node 1 progresses on a branch with a low fitness. *)
  let* baker_low_round =
    baker_at_round_n ~level:(current_level + 1) 0 client1
  in
  let* () = Client.bake_for_and_wait ~keys:[baker_low_round] client1 in
  (* Creating op targeting a block that is not live anymore. *)
  let* block_2_hash =
    Client.RPC.call client1 @@ RPC.get_chain_block_hash ~block:"2" ()
  in
  let* non_live_op =
    Operation.Manager.(
      operation
        ~branch:block_2_hash
        ~signer:Constant.bootstrap1
        [make ~source:Constant.bootstrap1 ~fee @@ transfer ~dest ()])
      client1
  in
  Log.info "Injecting op targeting a non-liveblock (expected to fail)" ;
  let* _ =
    Operation.inject_and_capture1_stderr
      ~rex:Operation_core.injection_error_unknown_branch
      non_live_op
      client1
  in
  Log.info "Injecting op targeting a non-liveblock (forced)" ;
  let* (`OpHash non_live_op_hash) =
    Operation.inject ~force:true non_live_op client1
  in
  Log.info "Operation %s injected on a non-liveblock" non_live_op_hash ;
  let* block_3_hash =
    Client.RPC.call client1 @@ RPC.get_chain_block_hash ~block:"3" ()
  in
  let* barely_alive_op =
    Operation.Manager.(
      operation
        ~branch:block_3_hash
        ~signer:Constant.bootstrap4
        [make ~source:Constant.bootstrap4 ~fee @@ transfer ~dest ()])
      client1
  in
  let* head_hash =
    Client.RPC.call client1 @@ RPC.get_chain_block_hash ~block:"head" ()
  in
  let* too_alive_op =
    Operation.Manager.(
      operation
        ~branch:head_hash
        ~signer:Constant.bootstrap5
        [make ~source:Constant.bootstrap5 ~fee @@ transfer ~dest ()])
      client1
  in
  let* (`OpHash barely_alive_op_hash) =
    Operation.inject barely_alive_op client1
  in
  Log.info "Operation %s injected on a barely live block" barely_alive_op_hash ;
  let* (`OpHash too_alive_op_hash) = Operation.inject too_alive_op client1 in
  Log.info "Operation %s injected on a very alive block" too_alive_op_hash ;

  (* Creating op targeting a block that is live. *)
  let* old_live_op =
    Operation.Manager.(
      operation
        ~branch:block_3_hash
        ~signer:Constant.bootstrap2
        [make ~source:Constant.bootstrap2 ~fee @@ transfer ~dest ()])
      client1
  in
  Log.info "Injecting op targeting a the oldest liveblock" ;
  let* (`OpHash old_live_op_hash) = Operation.inject old_live_op client1 in
  Log.info "Operation %s injected on a liveblock" old_live_op_hash ;

  Log.info "Reconnect nodes" ;
  (* Inspect operations after node1 switches to the high fitness
     branch. *)
  let wait_switch = wait_for_branch_switch node1 (current_level + 1) in
  let* () =
    Client.Admin.connect_address ~endpoint:(Node node1) ~peer:node2 client1
  in
  Log.info
    "Waiting for the branch switch on node1 (level %d round>0)"
    (current_level + 1) ;
  let* () = wait_switch in
  let* mempool = Mempool.get_mempool client1 in
  Check.is_true
    (List.mem old_live_op_hash mempool.validated)
    ~error_msg:(sf "%s not found in validated operations" old_live_op_hash) ;
  Check.is_false
    (List.mem non_live_op_hash mempool.validated)
    ~error_msg:(sf "%s was found in validated operations" non_live_op_hash) ;
  Check.is_true
    (List.mem barely_alive_op_hash mempool.validated)
    ~error_msg:(sf "%s not found in validated operations" barely_alive_op_hash) ;
  Check.is_false
    (List.mem too_alive_op_hash mempool.validated)
    ~error_msg:(sf "%s was found in validated operations" too_alive_op_hash) ;

  Log.info "Injecting op targeting the oldest liveblock" ;
  (* Creating op targeting a block that is live. *)
  let* old_live_op_2 =
    Operation.Manager.(
      operation
        ~branch:block_3_hash
        ~signer:Constant.bootstrap3
        [make ~source:Constant.bootstrap3 ~fee @@ transfer ~dest ()])
      client1
  in
  let* (`OpHash old_live_op_hash_2) =
    Operation.inject ~force:true old_live_op_2 client1
  in

  Log.info "Injecting op targeting a non-liveblock" ;
  let* _ =
    Operation.inject_and_capture1_stderr
      ~rex:Operation_core.injection_error_unknown_branch
      non_live_op
      client1
  in

  Log.info "Bake one last block with an op targeting the oldest liveblock" ;
  let* () = Client.bake_for_and_wait client1 in
  Log.info "Check that op on the oldest liveblock was included" ;
  let* is_in_block = is_in_block client1 old_live_op_hash_2 in
  Check.is_true
    is_in_block
    ~error_msg:
      (sf "Operation %s is expected to be found in block" old_live_op_hash_2) ;

  unit

(* This test aims to check that injecting an operation targeting a
   block that is not live anymore fails. Additionally, if the
   injection is forced, the operation won't stay in the mempool nor be
   included in a block. *)
let operation_liveness =
  Protocol.register_test
    ~__FILE__
    ~title:"Check operation liveness"
    ~tags:[team; "shell"; "liveblocks"; "operations"; "mempool"]
  @@ fun protocol ->
  let nodes_args = Node.[Synchronisation_threshold 0; Connections 1] in
  let* _node1, client1 =
    Client.init_with_protocol ~protocol ~nodes_args `Client ()
  in
  let* max_op_ttl = get_max_op_ttl_constant client1 in
  (* If this assertion fails, rework the test. *)
  let () = assert (max_op_ttl = 8) in
  let source = Constant.bootstrap1 in
  let signer = Constant.bootstrap1 in
  let dest = Constant.bootstrap2 in
  let fee = 1_000 in
  (* Baking (maxopttl + 1) blocks so that block 2 is the oldest live
     block, and thus, block 1 is the most recent non-liveblock. *)
  let* () =
    repeat (max_op_ttl + 1) (fun () -> Client.bake_for_and_wait client1)
  in
  let* block_1_hash =
    Client.RPC.call client1 @@ RPC.get_chain_block_hash ~block:"1" ()
  in

  (* Creating op targeting block 1, that is not live anymore. *)
  let* non_live_op =
    Operation.Manager.(
      operation
        ~branch:block_1_hash
        ~signer
        [make ~source ~fee @@ transfer ~dest ()])
      client1
  in
  Log.info "Injecting op targeting a non-liveblock (expected to fail)" ;
  let* _ =
    Operation.inject_and_capture1_stderr
      ~rex:Operation_core.injection_error_unknown_branch
      non_live_op
      client1
  in

  (* Creating op targeting block 2, that is live. *)
  let* block_2_hash =
    Client.RPC.call client1 @@ RPC.get_chain_block_hash ~block:"2" ()
  in
  let* op =
    Operation.Manager.(
      operation
        ~branch:block_2_hash
        ~signer
        [make ~source ~fee @@ transfer ~dest ()])
      client1
  in
  Log.info "Injecting op targeting a liveblock" ;
  let* (`OpHash live_op_hash_1) = Operation.inject op client1 in
  let* () = check_mempool ~validated:[live_op_hash_1] client1 in
  Log.info
    "Bake an empty block to avoid %s to get included in the block"
    live_op_hash_1 ;
  let* () =
    let empty_mempool_file = Client.empty_mempool_file () in
    Client.bake_for_and_wait
      ~mempool:empty_mempool_file
      ~ignore_node_mempool:true
      client1
  in
  Log.info "Check that %s was dropped as not live anymore" live_op_hash_1 ;
  let* () = check_mempool client1 in

  Log.info "Injecting, by force, op targeting a non-liveblock" ;
  let* (`OpHash live_op_hash_2) = Operation.inject ~force:true op client1 in
  let* () = check_mempool ~validated:[live_op_hash_2] client1 in
  let* () = Client.bake_for_and_wait client1 in
  let* is_in_block = is_in_block client1 live_op_hash_2 in
  Check.is_false
    is_in_block
    ~error_msg:
      (sf "Operation %s is not expected to be found in block" live_op_hash_2) ;

  unit

let register ~protocols =
  operation_liveness protocols ;
  operation_liveness_reorg protocols
