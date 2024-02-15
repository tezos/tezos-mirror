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
   Invocation:   dune exec tezt/tests/main.exe -- --file prevalidator.ml
   Subject:      .
*)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1657

   Some refactorisation is needed. All new tests should be in the Revamped
   module (which will be erased once we have rewrote all the Legacy tests. *)

module Revamped = struct
  let log_step counter msg =
    let color = Log.Color.(bold ++ FG.blue) in
    let prefix = "step" ^ string_of_int counter in
    Log.info ~color ~prefix msg

  (* We override the default [bake_for] command to wait on the next level
     incremented after the new block. If [wait_for_flush] is set we wait on
     a [flush] event from the mempool because the [set_head] event used by the
     default [bake_for] functions happens before a flush of the mempool.
     For mempool tests, we generally prefer to ensure that a [flush] did
     happen than a [set_head].

     Optionally, we can decide whether the block should be baked
     without taking the operations of the mempool.

     This function returns the level of the client after the bake. *)
  let bake_for ?keys ?(wait_for_flush = false) ?(empty = false) ?protocol node
      client =
    let flush_waiter =
      if wait_for_flush then Node.wait_for_request ~request:`Flush node
      else unit
    in
    let* level = Client.level client in
    let* () =
      if empty then
        let empty_mempool_file = Client.empty_mempool_file () in
        Client.bake_for
          ~mempool:empty_mempool_file
          ~ignore_node_mempool:true
          ?protocol
          ?keys
          client
      else Client.bake_for ?keys client
    in
    let* () = flush_waiter in
    Node.wait_for_level node (level + 1)

  (* Wait for the [operation_to_reclassify] event from the prevalidator and
     return the number of operations that were set to be reclassified. *)
  let wait_for_operations_not_flushed_event node =
    let filter json = JSON.(json |> as_int_opt) in
    Node.wait_for node "operations_to_reclassify.v0" filter

  (* Wait for the [banned_operation_encountered] event from the
     prevalidator and yield if the operation hash is the same as
     [oph]. *)
  let wait_for_banned_operation_injection node oph =
    let filter json =
      match
        JSON.
          (json |-> "origin" |> as_string_opt, json |-> "oph" |> as_string_opt)
      with
      | Some "injected", Some h when String.equal h oph -> Some ()
      | _ -> None
    in
    Node.wait_for node "banned_operation_encountered.v0" filter

  (* [synchronize_mempool client node] calls the [request_operations] RPC from
     the [client] to retrieve mempool from its peers and waits for a [notify]
     event on the [node] (debug events must be enabled). *)
  let synchronize_mempool client node =
    let mempool_notify_waiter = Node.wait_for_request ~request:`Notify node in
    let* _ =
      Client.RPC.call client @@ RPC.post_chain_mempool_request_operations ()
    in
    mempool_notify_waiter

  (* Call the [/chains/[chain]/mempool/pending_operations] RPC and
     check that in the returned mempool, each field [validated],
     [branch_delayed], etc. contains exactly the operation hashes
     listed in the argument of the same name. Omitted arguments
     default to the empty list. *)
  let check_mempool ?validated ?branch_delayed ?branch_refused ?refused
      ?outdated ?unprocessed client =
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
    let* node =
      Node.init
        ~patch_config:(Node.Config_file.set_prevalidator ~operations_batch_size)
        [Connections 0; Synchronisation_threshold 0]
    in
    let* client = Client.init ~endpoint:(Node node) () in
    let bootstrap_accounts = Account.Bootstrap.keys |> Array.length in
    let* additional_bootstrap_accounts =
      Lwt_list.map_s
        (fun i ->
          let alias = Account.Bootstrap.alias i in
          let* key = Client.gen_and_show_keys ~alias client in
          return (key, None, true))
        (range
           (1 + bootstrap_accounts)
           (bootstrap_accounts + nb_additional_bootstrap_accounts))
    in
    let* parameter_file =
      Protocol.write_parameter_file
        ~additional_bootstrap_accounts
        ~base:(Either.right (protocol, None))
        []
    in
    let* () =
      Client.activate_protocol_and_wait ~parameter_file ~protocol client
    in

    log_step 2 "Inject %d transfer operations." number_of_operations ;
    let* _ =
      Tezos_base__TzPervasives.List.iter_s
        (fun ((key : Account.key), _, _) ->
          Client.transfer
            ~amount:(Tez.of_int 1)
            ~giver:key.alias
            ~receiver:Constant.bootstrap1.alias
            client)
        additional_bootstrap_accounts
    in

    log_step 3 "Check operations are all classified as 'Validated'." ;
    let* mempool = Mempool.get_mempool client in
    let error_msg =
      "some operations not classified as 'validated: expected length %R, got %L"
    in
    Check.(
      (List.length mempool.validated = number_of_operations) int ~error_msg) ;

    log_step 4 "Bake a block with an empty mempool." ;
    let* _ = bake_for ~wait_for_flush:true ~empty:true ~protocol node client in
    let* mempool_after_empty_block = Mempool.get_mempool client in

    log_step 5 "Check that we did not lose any operation." ;
    let error_msg =
      "operations were lost after the flush: expected %L, got %R"
    in
    Check.((mempool = mempool_after_empty_block) Mempool.typ ~error_msg) ;

    log_step 6 "Inject attestation operations." ;
    let* () = Client.attest_for client ~protocol ~force:true in
    let* mempool_with_attestation = Mempool.get_mempool client in

    log_step 7 "Check attestation is validated." ;
    let mempool_diff =
      Mempool.symmetric_diff mempool_after_empty_block mempool_with_attestation
    in
    (* [mempool_diff] should contain only the validated attestation. *)
    let mempool_expected =
      let open Mempool in
      try {empty with validated = [List.hd mempool_diff.validated]}
      with Not_found ->
        {empty with validated = ["<validated field was empty>"]}
    in
    let error_msg = "attestation is not validated: expected %L, got %R" in
    Check.((mempool_expected = mempool_diff) Mempool.typ ~error_msg) ;

    log_step 8 "Bake with an empty mempool twice." ;
    let* () =
      repeat 2 (fun () ->
          let* _ =
            bake_for ~wait_for_flush:true ~protocol ~empty:true node client
          in
          unit)
    in
    let* last_mempool = Mempool.get_mempool client in

    log_step 9 "Check attestation is classified 'Outdated'." ;
    let error_msg = "one validated operation was lost: expected %L, got %R" in
    Check.((mempool_with_attestation = last_mempool) Mempool.typ ~error_msg) ;
    let error_msg =
      "attestation is not classified as 'outdated': length expected %L, got %R"
    in
    Check.(
      (List.compare_length_with last_mempool.outdated 1 = 0) int ~error_msg) ;
    unit

  (** This test tries to check that a branch_refused operation is not
      reclassified in the mempool after a head increment, but is reclassified
      after a branch switch. *)
  let recycling_branch_refused =
    Protocol.register_test
      ~__FILE__
      ~title:
        "Ensure that branch_refused operation is not recycled when we \
         increment our head"
      ~tags:["mempool"; "recycle"; "branch_refused"]
    @@ fun protocol ->
    log_step 1 "Connect and initialise two nodes." ;
    let* node1 =
      Node.init
        ~event_sections_levels:[("prevalidator", `Debug)]
        [Synchronisation_threshold 0; Private_mode]
    and* node2 = Node.init [Synchronisation_threshold 0; Private_mode] in
    let* client1 = Client.init ~endpoint:(Node node1) ()
    and* client2 = Client.init ~endpoint:(Node node2) () in
    let* () = Client.Admin.trust_address client1 ~peer:node2
    and* () = Client.Admin.trust_address client2 ~peer:node1 in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    let* _ = Node.wait_for_level node2 1 in

    log_step 2 "Inject a transfer operation on node1." ;
    let* (`OpHash oph) =
      Operation.inject_transfer
        ~wait_for_injection:node1
        ~amount:1
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client1
    in
    Log.info "%s injected on node1." oph ;

    log_step 3 "Check that the operation %s is classified as 'Validated'." oph ;
    let* () = check_mempool ~validated:[oph] client1 in

    log_step
      4
      "Bake a block that includes %s and wait node2 to be synchronised."
      oph ;
    let* level =
      bake_for
        ~keys:[Constant.bootstrap4.public_key_hash]
        ~empty:false
        ~protocol
        node1
        client1
    in
    let* _ = Node.wait_for_level node2 level in

    log_step 5 "Disconnect nodes." ;
    let* node2_identity = Node.wait_for_identity node2 in
    let* () = Client.Admin.kick_peer ~peer:node2_identity client1 in

    log_step
      6
      "Force inject a transfer with the same counter and the same source as %s \
       on node1."
      oph ;
    let* counter_json =
      Client.RPC.call client1
      @@ RPC.get_chain_block_context_contract_counter
           ~id:Constant.bootstrap1.public_key_hash
           ()
    in
    let counter = JSON.as_int counter_json in
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node1
        ~force:true
        ~counter
        ~amount:2
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client1
    in
    Log.info "%s injected on node1." oph2 ;

    log_step 7 "Check that the operation %s is branch_refused." oph2 ;
    let* mempool_after_second_injection = Mempool.get_mempool client1 in
    let expected_mempool_after_second_injection =
      let open Mempool in
      {empty with branch_refused = [oph2]}
    in
    let error_msg =
      "expected mempool from node1 after injection was %L got %R"
    in
    Check.(
      (expected_mempool_after_second_injection = mempool_after_second_injection)
        Mempool.classified_typ
        ~error_msg) ;

    log_step 8 "Bake on node1 (head increment)." ;
    let bake_waiter1 = wait_for_operations_not_flushed_event node1 in
    let* () =
      Client.bake_for_and_wait
        ~keys:[Constant.bootstrap4.public_key_hash]
        client1
    in
    let* pending = bake_waiter1 in

    log_step 9 "Checks that %s is not reclassified." oph2 ;
    let error_msg =
      "no operation should have been reclassified on head increment got %L \
       operations"
    in
    Check.((pending = 0) int ~error_msg) ;

    log_step
      10
      "Check that %s is still branch_refused after head increment."
      oph2 ;
    let* mempool_after_head_increment = Mempool.get_mempool client1 in
    let error_msg =
      "expected mempool from node1 after head increment was %L got %R"
    in
    Check.(
      (expected_mempool_after_second_injection = mempool_after_head_increment)
        Mempool.classified_typ
        ~error_msg) ;

    log_step
      11
      "Bake on node2 to force higher fitness and to force a switch of head \
       when node2 will reconnect with node1." ;
    let* () =
      repeat 4 (fun () ->
          let* _ = bake_for ~empty:false ~protocol node2 client2 in
          unit)
    in

    log_step 12 "Reconnect node1 and node2." ;
    let bake_waiter1 = wait_for_operations_not_flushed_event node1 in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in

    (* TODO: this test should be adapt once the [bake for] command will have
       an option to not automatically add an attestation to a block that is
       being bake. Only one operation will be reclassified after that. *)
    log_step
      13
      "Check that %s is set to be reclassified on new branch as well as the \
       attestation from the head increment on node1."
      oph2 ;
    let* pending = bake_waiter1 in
    let error_msg =
      "two operations should be reclassified on new branch got %L operations"
    in
    Check.((pending = 2) int ~error_msg) ;

    log_step
      14
      "Check that the mempool of node1 still contains %s as branch_refused \
       operation and that the attestation from the head increment block is now \
       outdated."
      oph2 ;
    let* mempool = Mempool.get_mempool client1 in
    let expected_mempool =
      let open Mempool in
      let outdated =
        try [List.hd mempool.outdated]
        with Not_found -> ["<outdated field was empty>"]
      in
      {empty with branch_refused = [oph2]; outdated}
    in
    let error_msg = "expected mempool from node1 was %L got %R" in
    Check.((expected_mempool = mempool) Mempool.classified_typ ~error_msg) ;
    unit

  (** This test bans an operation and checks that a branch_delayed operation
      is classified again. *)
  let ban_operation_branch_delayed_reevaluated =
    Protocol.register_test
      ~__FILE__
      ~title:"ban_operation_branch_delayed_reevaluated"
      ~tags:["mempool"; "ban"; "branch_delayed"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* node, client = Client.init_with_protocol ~protocol `Client () in

    log_step 2 "Forge and inject an operation on the node." ;
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~amount:1
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client
    in

    log_step
      3
      "Check that the operation %s is validated in the node's mempool."
      oph1 ;
    let* () = check_mempool ~validated:[oph1] client in

    log_step 4 "Forge and inject an operation with the same manager." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~force:true
        ~amount:2
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client
    in

    log_step
      5
      "Check that the operation %s is branch_delayed in the node's mempool."
      oph2 ;
    let* () = check_mempool ~validated:[oph1] ~branch_delayed:[oph2] client in

    log_step 6 "Ban the operation %s." oph1 ;
    let* _ =
      Client.RPC.call client
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in

    log_step 7 "Check that the node's mempool contains %s as validated." oph2 ;
    let* mempool = Mempool.get_mempool client in
    let expected_mempool = {Mempool.empty with validated = [oph2]} in
    Check.(
      (expected_mempool = mempool)
        Mempool.classified_typ
        ~error_msg:"mempool expected to be %L, got %R") ;
    unit

  (** This test checks the one operation per manager per block restriction on
      injection.
      We inject two operations with the same manager and check that the second
      one is classified as branch_delayed at post_filtering since the pre-filter
      is not run at injection. Only the first operation is sent to a second
      node that bake a block. The second operation is then reclassified. *)
  let one_operation_per_manager_per_block_restriction_injection =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_injection"
      ~tags:["mempool"; "manager_restriction"; "injection"]
    @@ fun protocol ->
    log_step 1 "Initialize two nodes and connect them." ;
    let* node1, client1 =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in
    let* node2, client2 =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in
    let* () = Client.Admin.connect_address ~peer:node2 client1 in

    log_step 2 "Forge and inject an operation on node1." ;
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~wait_for_injection:node1
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client1
    in

    log_step
      3
      "Forge and inject an operation on node1 with the same source but \
       different destination." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~force:true
        ~wait_for_injection:node1
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap3
        client1
    in

    log_step
      4
      "Ensure that the first operation is validated and that the second is \
       branch_delayed on node1." ;
    let* () = check_mempool ~validated:[oph1] ~branch_delayed:[oph2] client1 in

    log_step
      5
      "Ensure that the first operation is validated on node2 and that no other \
       operation is in the mempool." ;
    let* () = check_mempool ~validated:[oph1] client2 in

    log_step 6 "Bake a block on node2." ;
    let* _ = bake_for ~empty:false ~protocol node1 client2 in

    log_step
      7
      "Check that the second operation has not been baked because it was not \
       propagated to node2. And check that it is now branch_refused in node1 \
       because it used the same counter as the validated operation." ;
    check_mempool ~branch_refused:[oph2] client1

  (** This test checks the one operation per manager per block restriction on
      propagation.
      We inject two operations with the same manager (and same counter) on two
      differents nodes.
      The first operation is propagated to a third node and classified as
      validated. Then the second operation is propagated to the third node we
      check that the second operation is classified as branch_delayed *)
  let one_operation_per_manager_per_block_restriction_propagation =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_propagation"
      ~tags:["mempool"; "manager_restriction"; "propagation"]
    @@ fun protocol ->
    log_step 1 "Initialize three nodes with the protocol." ;
    let* node1, client1 =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0; Private_mode]
        ~protocol
        `Client
        ()
    in
    let* node2, client2 =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0; Private_mode]
        ~protocol
        `Client
        ()
    in
    let* node3, client3 =
      Client.init_with_protocol
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in

    log_step 2 "Forge and inject an operation on node1." ;
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~wait_for_injection:node1
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client1
    in

    log_step
      3
      "Forge and inject an operation on node2 with the same manager and \
       counter but a different destination." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node2
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap3
        client2
    in

    log_step
      4
      "Propagate %s from node1 to node3 and check that it is classified as \
       validated."
      oph1 ;
    let* () = Client.Admin.trust_address client3 ~peer:node1
    and* () = Client.Admin.trust_address client1 ~peer:node3 in
    let* () = Client.Admin.connect_address ~peer:node1 client3 in
    let* () = synchronize_mempool client3 node3 in
    let* () = check_mempool ~validated:[oph1] client3 in

    log_step
      5
      "Propagate %s from node2 to node3 and check that it is classified as \
       branch_delayed."
      oph2 ;
    let* () = Client.Admin.trust_address client3 ~peer:node2
    and* () = Client.Admin.trust_address client2 ~peer:node3 in
    let* () = Client.Admin.connect_address ~peer:node2 client3 in
    let* () = synchronize_mempool client3 node3 in
    check_mempool ~validated:[oph1] ~branch_delayed:[oph2] client3

  (** This test checks that an operation branch_delayed is still branch_delayed
      after a flush either because of the one operation per manager per block or
      the previous reason it was branch_delayed for. *)
  let one_operation_per_manager_per_block_flush =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_flush"
      ~tags:["mempool"; "manager_restriction"; "flush"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* node, client =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in

    log_step 2 "Force inject a transfer with a counter in the futur." ;
    let* counter_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_counter
           ~id:Constant.bootstrap1.public_key_hash
           ()
    in
    let counter = JSON.as_int counter_json in
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~force:true
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~counter:(counter + 2)
        client
    in

    log_step 3 "Inject a transfer with a correct counter." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~counter:(counter + 1)
        client
    in

    log_step
      4
      "Inject a transfer with a correct counter but different destination." ;
    (* with force to avoid failure *)
    let* (`OpHash oph3) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap3
        ~counter:(counter + 1)
        ~force:true
        client
    in

    log_step
      5
      "Check that the mempool contains %s as validated and %s as \
       branch_delayed."
      oph2
      oph1 ;
    let* () =
      check_mempool ~validated:[oph2] ~branch_delayed:[oph1; oph3] client
    in

    log_step 6 "Flush the mempool." ;
    let* _ = bake_for ~wait_for_flush:true ~empty:true ~protocol node client in

    log_step
      7
      "Check that the mempool still contains %s as branch_delayed after the \
       flush."
      oph1 ;
    let* mempool = Mempool.get_mempool client in
    Check.(
      (List.mem oph1 mempool.branch_delayed = true)
        bool
        ~error_msg:(sf "%s should be in branch_delayed" oph1)) ;

    log_step
      8
      "Check that if %s is validated then %s is branch_delayed or the other \
       way around."
      oph2
      oph3 ;
    let* mempool = Mempool.get_mempool client in
    Check.(
      (((List.mem oph2 mempool.branch_delayed && List.mem oph3 mempool.validated)
       || List.mem oph3 mempool.branch_delayed
          && List.mem oph2 mempool.validated)
      = true)
        bool
        ~error_msg:
          (sf
             "validated should contain either %s or %s and branch_delayed \
              should contain the other one"
             oph2
             oph3)) ;
    unit

  (** Test the one-operation-per-manager-per-block restriction (1M)
      during the injection of operations in an isolated node.

      Check that:

      - operations from distinct managers are [validated] without issue;

      - a second operation from the same manager with the same fee
        cannot be injected at all with [~force:false];

      - another operation from the same manager with the same fee,
        injected with [~force:true], gets classified as [branch_delayed];

      - another operation from the same manager with twice the fee
        gets [validated] and causes the old operation to be reclassified as
        [outdated]. *)
  let one_operation_per_manager_per_block_inject_isolated_node =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_inject_isolated_node"
      ~tags:["mempool"; "manager_restriction"; "inject"; "isolated_node"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* _node, client =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in

    let source1 = Constant.bootstrap1 in
    let source2 = Constant.bootstrap2 in
    let fee = 1_000 in
    let expected_fee_needed_to_replace = 1_050 in
    log_step
      2
      "Inject transfers from [source1] and [source2] with fee [%d] and correct \
       counters. Check that both are validated (i.e. the manager restriction \
       does not prevent similar operations from distinct managers)."
      fee ;
    let* (`OpHash oph1) =
      Operation.Manager.(
        inject
          [make ~source:source1 ~fee @@ transfer ~dest:Constant.bootstrap3 ()]
          client)
    in
    let* (`OpHash oph2) =
      Operation.Manager.(
        inject
          [make ~source:source2 ~fee @@ transfer ~dest:Constant.bootstrap3 ()]
          client)
    in
    let* () = check_mempool ~validated:[oph1; oph2] client in

    let signer =
      if Protocol.number protocol >= 15 then Constant.bootstrap1
        (* Since protocol 15, the 1M restriction check is done
           after the validation of the op (and includes the
           signature checks), therefore we need a valid
           signature *)
      else Constant.bootstrap3
      (* By putting the wrong signature, we also ensure that the
         signature is checked only after the 1M restriction check. *)
    in
    log_step
      3
      "Inject another transfer from [source1] with the same fee and a correct \
       counter (but a different destination so that it is not the same \
       operation). Check that it fails and the mempool is unchanged. Indeed, \
       the [force] argument of [inject] defaults to [false] so the faulty \
       injected operation is discarded." ;
    let* op3 =
      Operation.Manager.(
        operation
          ~signer
          [make ~source:source1 ~fee (transfer ~dest:Constant.bootstrap4 ())])
        client
    in
    let* _oph3, fee_needed_to_replace =
      Operation.inject_and_capture2_stderr
        ~rex:Operation.conflict_error_with_needed_fee
        op3
        client
    in
    Check.(
      (int_of_string fee_needed_to_replace = expected_fee_needed_to_replace)
        int
        ~error_msg:"The recommended fee is %L but expected %R.") ;
    let* () = check_mempool ~validated:[oph1; oph2] client in

    log_step
      4
      "Inject yet another transfer from [source1] with the same fee and a \
       correct counter, but this time with [~force:true]. Check that the new \
       operation is included in the mempool as [branch_delayed]." ;
    let* (`OpHash oph1bis) =
      Operation.Manager.(
        inject
          ~force:true
          ~signer
          [make ~source:source1 ~fee @@ transfer ~dest:Constant.bootstrap5 ()]
          client)
    in
    let* () =
      check_mempool ~validated:[oph1; oph2] ~branch_delayed:[oph1bis] client
    in

    log_step
      5
      "Inject a new transfer from [source2] with a much higher fee than the \
       first one from the same source. Check that the new operation is \
       [validated] while the old one has become [outdated]." ;
    let* (`OpHash oph2bis) =
      Operation.Manager.(
        inject
          [
            make ~source:source2 ~fee:(2 * fee)
            @@ transfer ~dest:Constant.bootstrap4 ();
          ]
          client)
    in
    check_mempool
      ~validated:[oph1; oph2bis]
      ~branch_delayed:[oph1bis; oph2]
      client

  (** This test checks that an operation validated is not reclassified and stays
      validated after the ban of a branch_delayed operation. *)
  let one_operation_per_manager_per_block_ban =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_ban"
      ~tags:["mempool"; "manager_restriction"; "ban"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* node, client =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in

    log_step 2 "Force inject a transfer with a counter in the futur." ;
    let* counter_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_counter
           ~id:Constant.bootstrap1.public_key_hash
           ()
    in
    let counter = JSON.as_int counter_json in
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~force:true
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~counter:(counter + 2)
        client
    in

    log_step 3 "Inject a transfer with a correct counter." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~counter:(counter + 1)
        client
    in

    log_step
      4
      "Check that the mempool contains %s as validated and %s as \
       branch_delayed."
      oph2
      oph1 ;
    let* () = check_mempool ~validated:[oph2] ~branch_delayed:[oph1] client in

    log_step 5 "Ban the operation %s." oph1 ;
    let to_reclassified = ref false in
    let _ =
      Node.wait_for node "operations_to_reclassify.v0" (fun _ ->
          to_reclassified := true ;
          Some ())
    in
    let* _ =
      Client.RPC.call client
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in

    log_step
      6
      "Check that the mempool contains %s as validated and that %s is not in \
       the mempool anymore."
      oph2
      oph1 ;
    let* () = check_mempool ~validated:[oph2] client in

    log_step 7 "Check that no flush have been triggered after the ban." ;
    Check.(
      (!to_reclassified = false)
        bool
        ~error_msg:"A flush has been triggered after the ban.") ;
    unit

  (* This test checks that on a ban of a validated operation, the flush respects
     the 1M invariant. *)
  let one_operation_per_manager_per_block_flush_on_ban =
    Protocol.register_test
      ~__FILE__
      ~title:"Manager_restriction_flush_on_ban"
      ~tags:["mempool"; "manager_restriction"; "flush"; "ban"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* node, client =
      Client.init_with_protocol
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in
    log_step 2 "Inject a transfer." ;
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap2
        client
    in

    log_step 3 "Inject a transfer with a different source." ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client
    in

    log_step
      4
      "Inject a transfer with the same source but different destination. This \
       operation should be classified as branch_delayed with the 1M \
       restriction." ;
    (* with force to avoid failure *)
    let* (`OpHash oph3) =
      Operation.inject_transfer
        ~force:true
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap3
        client
    in

    log_step
      5
      "Check that the mempool contains %s and %s as validated, %s as \
       branch_delayed."
      oph1
      oph2
      oph3 ;
    let* () =
      check_mempool ~validated:[oph1; oph2] ~branch_delayed:[oph3] client
    in

    log_step 5 "Ban the operation %s." oph1 ;
    let* _ =
      Client.RPC.call client
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in

    log_step
      6
      "Check that %s is not in the mempool anymore and that one operation is \
       validated and the other is branch_delayed between %s and %s."
      oph1
      oph2
      oph3 ;
    let* mempool = Mempool.get_mempool client in
    Check.(
      (List.length mempool.validated = 1)
        int
        ~error_msg:"validated mempool should contain only one operation, got %L") ;
    Check.(
      (List.length mempool.branch_delayed = 1)
        int
        ~error_msg:
          "branch_delayed mempool should contain only one operation, got %L") ;
    unit

  let max_refused_operations ~protocol classification =
    let max_refused_operations = 1 in
    let source1 = Constant.bootstrap1 in
    let source2 = Constant.bootstrap2 in
    let dest = Constant.bootstrap3 in
    let string_of_classification =
      match classification with
      | `Branch_delayed -> "branch_delayed"
      | `Branch_refused -> "branch_refused"
      | `Refused -> "refused"
    in
    let operation_fees_from_classification = function
      | `Branch_delayed | `Branch_refused -> None
      | `Refused -> Some 0
      (* fees_too_low *)
    in
    let counter_shift_from_classification counter = function
      | `Branch_delayed -> Some (counter + 2)
      (* counter in the future *)
      | `Branch_refused -> Some counter
      (* counter in the past *)
      | `Refused -> None
    in
    log_step
      1
      "Initialize a node with 'max_refused_operations=%d'."
      max_refused_operations ;
    let* node =
      Node.init
        ~patch_config:
          (Node.Config_file.set_prevalidator ~max_refused_operations)
        [Connections 0; Synchronisation_threshold 0]
    in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol_and_wait ~protocol client in

    log_step
      2
      "Forge and inject operations. Then, bake to increment the counter of \
       boostrap1 and bootstrap2 in the context" ;
    let* _ =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:source1
        ~dest
        ~amount:1
        client
    in
    let* _ =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:source2
        ~dest
        ~amount:1
        client
    in
    let* _ = bake_for ~wait_for_flush:true ~empty:false ~protocol node client in

    log_step 3 "Forge and force inject an operation." ;
    let* counter_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_counter
           ~id:source1.public_key_hash
           ()
    in
    let counter =
      counter_shift_from_classification
        (JSON.as_int counter_json)
        classification
    in
    let fee = operation_fees_from_classification classification in

    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~force:true
        ~source:source1
        ~dest
        ?counter
        ?fee
        client
    in

    log_step
      3
      "Flush the mempool and check that %s is classified as %s."
      oph1
      string_of_classification ;
    let* _ = bake_for ~empty:true ~protocol ~wait_for_flush:true node client in
    let* mempool = Mempool.get_mempool client in
    let expected_mempool =
      match classification with
      | `Branch_delayed -> {Mempool.empty with branch_delayed = [oph1]}
      | `Branch_refused -> {Mempool.empty with branch_refused = [oph1]}
      | `Refused -> {Mempool.empty with refused = [oph1]}
    in
    Check.(
      (mempool = expected_mempool)
        Mempool.classified_typ
        ~error_msg:"mempool expected to be %L, got %R") ;

    log_step 4 "Forge and force inject an operation." ;
    let* counter_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_counter
           ~id:source2.public_key_hash
           ()
    in
    let counter =
      counter_shift_from_classification
        (JSON.as_int counter_json)
        classification
    in
    let fee = operation_fees_from_classification classification in
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~force:true
        ~source:source2
        ~dest
        ?counter
        ?fee
        client
    in

    log_step
      5
      "Flush the mempool to classify %s as %s and check that the mempool \
       contains only one operation %s."
      oph2
      string_of_classification
      string_of_classification ;
    let* _ = bake_for ~empty:true ~protocol ~wait_for_flush:true node client in
    let* mempool = Mempool.get_mempool client in
    let mempool_classification, mempool_without_classification =
      match classification with
      | `Branch_delayed ->
          (mempool.branch_delayed, {Mempool.empty with branch_delayed = []})
      | `Branch_refused ->
          (mempool.branch_refused, {Mempool.empty with branch_refused = []})
      | `Refused -> (mempool.refused, {Mempool.empty with refused = []})
    in
    Check.(
      (max_refused_operations = List.length mempool_classification)
        int
        ~error_msg:"number of operation in mempool expected to be %L, got %R") ;
    Check.(
      (Mempool.empty = mempool_without_classification)
        Mempool.classified_typ
        ~error_msg:"the rest of the mempool should be empty got %R") ;
    unit

  (** This test checks max_refused_operations for branch_delayed
     classification. *)
  let max_refused_operations_branch_delayed =
    Protocol.register_test
      ~__FILE__
      ~title:"Max refused operations branch_delayed"
      ~tags:["mempool"; "refused"; "max"; "branch_delayed"]
    @@ fun protocol -> max_refused_operations ~protocol `Branch_delayed

  (** This test checks max_refused_operations for branch_refused
     classification. *)
  let max_refused_operations_branch_refused =
    Protocol.register_test
      ~__FILE__
      ~title:"Max refused operations branch_refused"
      ~tags:["mempool"; "refused"; "max"; "branch_refused"]
    @@ fun protocol -> max_refused_operations ~protocol `Branch_refused

  (** This test checks max_refused_operations for refused classification. *)
  let max_refused_operations_refused =
    Protocol.register_test
      ~__FILE__
      ~title:"Max refused operations refused"
      ~tags:["mempool"; "refused"; "max"]
    @@ fun protocol -> max_refused_operations ~protocol `Refused

  (** This test checks max_refused_operations for outdated classification. *)
  let max_refused_operations_outdated =
    let max_refused_operations = 1 in
    Protocol.register_test
      ~__FILE__
      ~title:"Max refused operations outdated"
      ~tags:["mempool"; "refused"; "max"; "outdated"]
    @@ fun protocol ->
    log_step
      1
      "Initialize a node with 'max_refused_operations=%d'."
      max_refused_operations ;
    let* node =
      Node.init
        ~patch_config:
          (Node.Config_file.set_prevalidator ~max_refused_operations)
        [Connections 0; Synchronisation_threshold 0]
    in
    let* client = Client.init ~endpoint:(Node node) () in
    let* () = Client.activate_protocol_and_wait ~protocol client in

    log_step 2 "Bake an empty block to be able to attest it." ;
    let* _ = bake_for ~empty:true ~protocol ~wait_for_flush:true node client in

    log_step 3 "Attest with bootstrap1." ;
    let* _ =
      Client.attest_for
        ~protocol
        ~key:[Constant.bootstrap1.alias]
        ~force:true
        client
    in

    log_step 3 "Attest with bootstrap2." ;
    let* _ =
      Client.attest_for
        ~protocol
        ~key:[Constant.bootstrap2.alias]
        ~force:true
        client
    in

    log_step 4 "Check that both attestations are in the validated mempool." ;
    let* mempool = Mempool.get_mempool client in
    Check.(
      (2 = List.length mempool.validated)
        int
        ~error_msg:
          "number of mempool validated operations expected to be %L, got %R") ;

    log_step 5 "Bake two empty block to force attestations to be outdated." ;
    let* _ = bake_for ~empty:true ~protocol ~wait_for_flush:true node client in
    let* _ = bake_for ~empty:true ~protocol ~wait_for_flush:true node client in

    log_step 4 "Check that only one attestation is in the outdated mempool." ;
    let* mempool = Mempool.get_mempool client in
    Check.(
      (max_refused_operations = List.length mempool.outdated)
        int
        ~error_msg:
          "number of mempool outdated operations expected to be %L, got %R") ;
    Check.(
      (Mempool.empty = {mempool with outdated = []})
        Mempool.classified_typ
        ~error_msg:"the rest of the mempool should be empty got %R") ;
    unit

  (* We check ban operations cannot be propagated and stay banned
     after a flush. *)
  let ban_operation =
    Protocol.register_test
      ~__FILE__
      ~title:"mempool ban operation"
      ~tags:["mempool"; "node"; "ban"]
    @@ fun protocol ->
    log_step
      1
      "Node 1 activates the protocol and Node 2 catches up with Node 1." ;
    let* node1, client1 =
      Client.init_with_node
        ~nodes_args:[Synchronisation_threshold 0; Connections 1]
        `Client
        ()
    in
    let* node2, client2 =
      Client.init_with_node
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0; Connections 2]
        `Client
        ()
    in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    let* _ = Node.wait_for_level node2 1 in

    log_step 2 "Injection of two operations (transfers)." ;
    let notify_in_node2 = Node.wait_for_request ~request:`Notify node2 in
    let inject_op1 node client =
      Operation.inject_transfer
        ~wait_for_injection:node
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        client
    in
    let* (`OpHash oph1) = inject_op1 node1 client1 in
    let* () = notify_in_node2 in
    let notify_in_node2 = Node.wait_for_request ~request:`Notify node2 in
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~wait_for_injection:node1
        ~source:Constant.bootstrap3
        ~dest:Constant.bootstrap5
        client1
    in
    let* () = notify_in_node2 in
    let* () = check_mempool ~validated:[oph2; oph1] client2 in

    log_step 3 "Ban %s on node 2." oph1 ;
    let* _ =
      Client.RPC.call client2
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in
    let* () = check_mempool ~validated:[oph2] client2 in

    log_step 4 "Try to reinject the banned operation in Node 2." ;
    let* _ = inject_op1 node2 client2 in
    let* () = check_mempool ~validated:[oph2] client2 in

    log_step 5 "Add node3 connected only to node2." ;
    let* node3, client3 =
      Client.init_with_node
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0; Connections 1]
        `Client
        ()
    in
    let* () = Client.Admin.connect_address client3 ~peer:node2 in
    let* _ = Node.wait_for_level node3 1 in

    log_step 6 "Check %s is not in node3's mempool." oph1 ;
    let* () = synchronize_mempool client3 node3 in
    let* () = check_mempool ~validated:[oph2] client2 in

    log_step
      7
      "Bake and check %s was not included and is not in node2's mempool either."
      oph1 ;
    let baking = Node.wait_for_request ~request:`Flush node2 in
    let* () = Client.bake_for_and_wait client2 in
    let* _ = baking in
    (* empty mempool *)
    let* () = check_mempool client2 in
    let* ops = Client.RPC.call client2 @@ RPC.get_chain_block_operations () in
    let open JSON in
    let ops_list = ops |=> 3 |> as_list in
    let res =
      List.exists (fun e -> e |-> "hash" |> as_string = oph1) ops_list
    in
    Check.((res = false) bool ~error_msg:(sf "%s was found in block" oph1)) ;
    unit

  (* Check that we can reinject and reclassify an operation that was
     ban and then unbanned.*)
  let unban_operation_and_reinject =
    Protocol.register_test
      ~__FILE__
      ~title:"mempool unban operation and reinject"
      ~tags:["mempool"; "node"; "ban"; "reinject"]
    @@ fun protocol ->
    log_step 1 "Start a single node and activate the protocol." ;
    let* node1, client1 =
      Client.init_with_node
        ~nodes_args:[Synchronisation_threshold 0; Connections 0]
        `Client
        ()
    in
    let inject_op ~wait op =
      let source =
        match op with
        | `A -> Constant.bootstrap1
        | `B -> Constant.bootstrap2
        | `C -> Constant.bootstrap3
      in
      let dest = Constant.bootstrap5 in
      let wait_for_injection = if wait then Some node1 else None in
      Operation.inject_transfer ?wait_for_injection ~source ~dest client1
    in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in

    log_step 2 "Inject two transfers op1 and op2." ;
    let* (`OpHash oph1) = inject_op ~wait:true `A in
    let* (`OpHash oph2) = inject_op ~wait:true `B in
    let* () = check_mempool ~validated:[oph1; oph2] client1 in

    log_step 3 "Ban op1 and ensure the operation is not in the mempool." ;
    (* We ban twice to check banning an operation is idempotent. *)
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in
    let* () = check_mempool ~validated:[oph2] client1 in

    log_step 4 "Inject op3." ;
    let* (`OpHash oph3) = inject_op ~wait:true `C in
    let* () = check_mempool ~validated:[oph3; oph2] client1 in

    log_step 5 "Ban op2 and op1 again." ;
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph2)) ()
    in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in
    let* () = check_mempool ~validated:[oph3] client1 in

    log_step 6 "Check that reinjecting op1 fails." ;
    let wait_reinject_op1_banned =
      wait_for_banned_operation_injection node1 oph1
    in
    let _ = inject_op ~wait:false `A in
    let* () = wait_reinject_op1_banned in
    let* () = check_mempool ~validated:[oph3] client1 in

    log_step 7 "Unban op1, successfully reinject op1." ;
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_unban_operation ~data:(Data (`String oph1)) ()
    in
    let* _ = inject_op ~wait:true `A in
    let* () = check_mempool ~validated:[oph3; oph1] client1 in

    log_step 8 "Check that reinjecting op2 still fails." ;
    let wait_reinject_op2_banned =
      wait_for_banned_operation_injection node1 oph2
    in
    let _ = inject_op ~wait:false `B in
    let* () = wait_reinject_op2_banned in

    log_step 9 "Unban op2, successfully reinject op2." ;
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_unban_operation ~data:(Data (`String oph2)) ()
    in

    let* _ = inject_op ~wait:true `B in
    let* () = check_mempool ~validated:[oph3; oph2; oph1] client1 in

    log_step 10 "Ban op1 again, check that reinjecting it fails." ;
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in

    let wait_reinject_op1_banned_again =
      wait_for_banned_operation_injection node1 oph1
    in
    let _ = inject_op ~wait:false `A in
    let* () = wait_reinject_op1_banned_again in
    let* () = check_mempool ~validated:[oph3; oph2] client1 in

    log_step 11 "Try unban op3 and op2 check that nothing changes." ;
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_unban_operation ~data:(Data (`String oph3)) ()
    in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_unban_operation ~data:(Data (`String oph2)) ()
    in
    check_mempool ~validated:[oph3; oph2] client1

  (* This tests ban operations and then use the `unban_all` RPC on
     those operation. We check those operations are then propagated if
     we synchronise the mempools. *)
  let unban_all_operations =
    Protocol.register_test
      ~__FILE__
      ~title:"mempool unban all operations"
      ~tags:["mempool"; "node"; "ban"]
    @@ fun protocol ->
    log_step 1 "Start two nodes, connect them, activate the protocol." ;
    let* node1, client1 =
      Client.init_with_node
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0; Connections 1]
        `Client
        ()
    in
    let* node2, client2 =
      Client.init_with_node
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~nodes_args:[Synchronisation_threshold 0; Connections 1]
        `Client
        ()
    in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    let* _ = Node.wait_for_level node2 1 in

    log_step 2 "Inject four transfer operations" ;
    let inject_op ~wait op client =
      let source =
        match op with
        | `A -> Constant.bootstrap1
        | `B -> Constant.bootstrap2
        | `C -> Constant.bootstrap3
        | `D -> Constant.bootstrap4
      in
      let node =
        Client.get_mode client |> Client.mode_to_endpoint |> function
        | Some (Node node) -> node
        | _ -> assert false
      in
      let dest = Constant.bootstrap5 in
      let wait_for_injection = if wait then Some node else None in
      Operation.inject_transfer ?wait_for_injection ~source ~dest client
    in
    let* (`OpHash oph1) = inject_op ~wait:true `A client1 in
    let* (`OpHash oph2) = inject_op ~wait:true `B client1 in
    let* (`OpHash oph3) = inject_op ~wait:true `C client2 in
    let* (`OpHash oph4) = inject_op ~wait:true `D client2 in

    log_step 3 "Ban the first three of these operations." ;
    let* () = check_mempool ~validated:[oph4; oph3; oph2; oph1] client1 in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph1)) ()
    in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph2)) ()
    in
    let* _ =
      Client.RPC.call client1
      @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph3)) ()
    in
    let* () = check_mempool ~validated:[oph4] client1 in

    log_step 4 "Unban all operations." ;
    (* We expect to receive three operations are the synchronisation. *)
    let wait1 = Node.wait_for_request ~request:`Arrived node1
    and wait2 = Node.wait_for_request ~request:`Arrived node1
    and wait3 = Node.wait_for_request ~request:`Arrived node1 in
    let* _ =
      Client.RPC.call client1 @@ RPC.post_chain_mempool_unban_all_operations ()
    in
    let* () = synchronize_mempool client1 node1 in
    let* () = wait1 and* () = wait2 and* () = wait3 in

    Log.info
      "Step 5: Check that node 1 contains the right validated operations." ;
    let* () = check_mempool ~validated:[oph4; oph3; oph2; oph1] client1 in
    check_mempool ~validated:[oph4; oph3; oph2; oph1] client2

  let test_full_mempool_propagation =
    Protocol.register_test
      ~__FILE__
      ~title:"test full mempool propagation"
      ~tags:["mempool"; "gc"; "limit"; "bounding"; "full"]
    @@ fun protocol ->
    Log.info
      "Test the bound on operation count in the mempool, the propagation of \
       operations by a full mempool, and the reclassification on flush." ;
    (* We configure the filter with a limit of 4 operations, so that
       we can easily inject more with our 5 bootstrap accounts. *)
    let max_operations = 4 in
    (* Control fees and gas limits to easily influence weight (i.e. ratio) *)
    let fee = 1000 in
    let gas_limit = 1500 in

    log_step 0 "Initialize and connect two nodes." ;
    let* node1 =
      Node.init
        ~event_sections_levels:[("prevalidator", `Debug)]
        [Synchronisation_threshold 0; Private_mode]
    and* node2 = Node.init [Synchronisation_threshold 0; Private_mode] in
    let* client1 = Client.init ~endpoint:(Node node1) ()
    and* client2 = Client.init ~endpoint:(Node node2) () in
    let* () = Client.Admin.trust_address client1 ~peer:node2
    and* () = Client.Admin.trust_address client2 ~peer:node1 in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    let* _ = Node.wait_for_level node2 1 in

    log_step
      1
      "Update the mempool filter to allow at most %d valid operations."
      max_operations ;
    let* () = Mempool.Config.set_filter ~log:true ~max_operations client1
    and* () = Mempool.Config.set_filter ~log:true ~max_operations client2 in

    log_step 2 "Inject max_operations = %d operations." max_operations ;
    let* ops =
      Lwt.all
      @@ List.mapi
           (fun i source ->
             let* (`OpHash oph) =
               Operation.inject_transfer
                 ~source
                 ~dest:Constant.bootstrap2
                 ~wait_for_injection:node1
                 ~amount:1
                 ~fee:(fee + i)
                 ~gas_limit
                 client1
             in
             return oph)
           Constant.[bootstrap1; bootstrap2; bootstrap3; bootstrap4]
    in

    log_step 3 "Check these operations are validated in mempool." ;
    let* () = check_mempool ~validated:ops client1 in

    log_step
      4
      "The client should report when the mempool is full and not enough fees \
       are provided." ;
    let* op4 =
      Operation.Manager.(
        operation
          [
            make
              ~source:Constant.bootstrap5
              ~fee:(fee - 1)
              ~gas_limit
              (transfer ());
          ])
        client1
    in
    let* _oph, recommended_fee =
      Operation.inject_and_capture2_stderr
        ~rex:Operation.rejected_by_full_mempool_with_needed_fee
        op4
        client1
    in
    Check.(
      (int_of_string recommended_fee = fee + 1)
        int
        ~error_msg:"The recommended fee is %L but expected %R.") ;

    log_step 5 "Force inject an extra operation with not enough fees." ;
    let* (`OpHash oph5) =
      Operation.inject_transfer
        ~force:true
        ~source:Constant.bootstrap5
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee:(fee - 1)
        ~gas_limit
        client1
    in

    log_step 6 "Check that this extra operation is branch_delayed." ;
    let* () = check_mempool ~validated:ops ~branch_delayed:[oph5] client1 in

    log_step
      7
      "Check that the new operation is not propagated as part of a mempool." ;
    let* () = check_mempool ~validated:ops client2 in

    log_step 8 "Inject an extra operation with more fees for same gas." ;
    let* (`OpHash oph6) =
      Operation.inject_transfer
        ~source:Constant.bootstrap5
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee:(fee + 5)
        ~gas_limit
        client1
    in

    log_step
      9
      "Check that this extra operation is validated and replaces one with \
       lower fees." ;
    let removed_oph, kept_ops =
      match ops with
      | [] -> assert false
      | removed :: validated -> (removed, validated)
    in
    let* () =
      check_mempool
        ~validated:(oph6 :: kept_ops)
        ~branch_delayed:[removed_oph; oph5]
        client1
    in

    log_step 10 "Check that this new operation is propagated." ;
    let* () =
      check_mempool
        ~validated:(oph6 :: kept_ops)
        ~branch_delayed:[removed_oph]
        client2
    in

    log_step 11 "Check reclassification after flush." ;
    let* _level =
      bake_for
        ~keys:[Constant.bootstrap1.public_key_hash]
        ~empty:true
        ~protocol
        node1
        client1
    in
    let* () =
      check_mempool
        ~validated:(oph6 :: kept_ops)
        ~branch_delayed:[removed_oph; oph5]
        client1
    in
    let* _level =
      bake_for
        ~keys:[Constant.bootstrap1.public_key_hash]
        ~empty:false
        ~protocol
        node1
        client1
    in

    log_step 12 "Check mempool after flush." ;
    check_mempool ~branch_refused:[oph5] client1

  let test_full_mempool_and_replace_same_manager =
    Protocol.register_test
      ~__FILE__
      ~title:"test full mempool and replace same manager"
      ~tags:["mempool"; "gc"; "limit"; "bounding"; "full"; "replace"; "remove"]
    @@ fun protocol ->
    Log.info
      "Test the interaction between the mempool operation bound and \
       same-manager replace-by-fees." ;
    (* We configure the filter with a limit of 1 *)
    let max_operations = 1 in
    (* Control fees and gas limits to easily influence weight (i.e. ratio) *)
    let fee = 1000 in
    let gas_limit = 1500 in
    log_step 0 "Initialize and connect two nodes." ;
    let* node1 =
      Node.init
        ~event_sections_levels:[("prevalidator", `Debug)]
        [Synchronisation_threshold 0; Private_mode]
    and* node2 = Node.init [Synchronisation_threshold 0; Private_mode] in
    let* client1 = Client.init ~endpoint:(Node node1) ()
    and* client2 = Client.init ~endpoint:(Node node2) () in
    let* () = Client.Admin.trust_address client1 ~peer:node2
    and* () = Client.Admin.trust_address client2 ~peer:node1 in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* () = Client.activate_protocol_and_wait ~protocol client1 in
    let* _ = Node.wait_for_level node2 1 in

    log_step
      1
      "Update the mempool filter to allow at most %d valid operations."
      max_operations ;
    let* () = Mempool.Config.set_filter ~log:true ~max_operations client1
    and* () = Mempool.Config.set_filter ~log:true ~max_operations client2 in

    log_step 2 "Inject an operation" ;
    let* (`OpHash oph1) =
      Operation.inject_transfer
        ~force:true
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee
        ~gas_limit
        client1
    in
    let* () = check_mempool ~validated:[oph1] client1 in

    log_step 3 "Inject an operation with more fees, with different source" ;
    let* (`OpHash oph2) =
      Operation.inject_transfer
        ~force:true
        ~source:Constant.bootstrap3
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee:(fee + 1)
        ~gas_limit
        client1
    in
    let* () = check_mempool ~validated:[oph2] ~branch_delayed:[oph1] client1 in

    log_step
      4
      "Inject an operation with more fees, with same source as the first one \
       (removed)" ;
    let* (`OpHash oph3) =
      Operation.inject_transfer
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee:(fee + 2)
        ~gas_limit
        client1
    in
    let* () =
      check_mempool ~validated:[oph3] ~branch_delayed:[oph1; oph2] client1
    in

    log_step
      5
      "Inject an operation with more than 5%% more fees, to replace previous \
       one" ;
    let* (`OpHash oph4) =
      Operation.inject_transfer
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap2
        ~wait_for_injection:node1
        ~amount:1
        ~fee:(fee * 2)
        ~gas_limit
        client1
    in
    let* () =
      check_mempool ~validated:[oph4] ~branch_delayed:[oph1; oph2; oph3] client1
    in
    unit

  let test_full_mempool_attestation_vs_manager =
    Protocol.register_test
      ~__FILE__
      ~title:"test full mempool attestation vs manager"
      ~tags:
        ["mempool"; "gc"; "limit"; "bounding"; "full"; "attestation"; "manager"]
    @@ fun protocol ->
    Log.info
      "Test the bound on operation count in the mempool with both manager and \
       non-manager operations." ;
    (* Baseline fee *)
    let fee = 1000 in
    log_step
      0
      "Initialize a node and activate the protocol. Bake an additional block \
       to be able to inject valid attestations." ;
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let* level = bake_for ~wait_for_flush:true node client in
    let max_operations = 2 in
    log_step
      1
      "Set the mempool config to at most %d operations. Inject two transfers \
       and check that they are validated in the mempool."
      max_operations ;
    let* () = Mempool.Config.set_filter ~log:true ~max_operations client in
    let* (`OpHash oph1a) =
      Operation.Manager.inject_single_transfer
        ~source:Constant.bootstrap1
        ~dest:Constant.bootstrap5
        ~fee
        client
    in
    let* (`OpHash oph1b) =
      Operation.Manager.inject_single_transfer
        ~source:Constant.bootstrap2
        ~dest:Constant.bootstrap5
        ~fee:(fee + 1)
        client
    in
    let* () = check_mempool ~validated:[oph1a; oph1b] client in
    log_step
      2
      "Inject an attestation and check that it has replaced the lower-fee \
       transfer." ;
    let* block_payload_hash =
      Operation.Consensus.get_block_payload_hash client
    in
    let* slots_json = Operation.Consensus.get_slots ~level client in
    let inject_attestation (delegate : Account.key) =
      Operation.Consensus.inject
        (Operation.Consensus.attestation
           ~use_legacy_name:true
           ~slot:(Operation.Consensus.first_slot ~slots_json delegate)
           ~level
           ~round:0
           ~block_payload_hash
           ())
        ~signer:delegate
        client
    in
    let* (`OpHash oph2) = inject_attestation Constant.bootstrap1 in
    let* () =
      check_mempool ~validated:[oph1b; oph2] ~branch_delayed:[oph1a] client
    in
    log_step
      3
      "Check that injecting a transfer with lower fee than the remaining \
       transfer in the mempool fails, and the recommended fee in the error is \
       higher than that previous transfer's by 1." ;
    let* op3 =
      Operation.Manager.mk_single_transfer
        ~source:Constant.bootstrap3
        ~dest:Constant.bootstrap5
        ~fee:(fee / 2)
        client
    in
    let* () =
      Operation.inject_error_check_recommended_fee
        ~loc:__LOC__
        ~rex:Operation.rejected_by_full_mempool_with_needed_fee
        ~expected_fee:(fee + 2)
        op3
        client
    in
    let* () =
      check_mempool ~validated:[oph1b; oph2] ~branch_delayed:[oph1a] client
    in
    log_step
      4
      "Inject a second attestation and check that it has replaced the \
       remaining transfer." ;
    let* (`OpHash oph3) = inject_attestation Constant.bootstrap2 in
    let* () =
      check_mempool
        ~validated:[oph2; oph3]
        ~branch_delayed:[oph1a; oph1b]
        client
    in
    log_step
      5
      "Check that injecting a transfer fails with no recommened fee in the \
       error, because a manager operation can't replace a consensus one no \
       matter the fee." ;
    let* op5 =
      Operation.Manager.mk_single_transfer
        ~source:Constant.bootstrap4
        ~dest:Constant.bootstrap5
        ~fee
        client
    in
    let* (`OpHash _) =
      Operation.inject
        ~error:Operation.rejected_by_full_mempool_no_possible_fee
        op5
        client
    in
    unit

  let test_full_mempool_max_total_bytes =
    Protocol.register_test
      ~__FILE__
      ~title:"test full mempool max_total_bytes"
      ~tags:["mempool"; "gc"; "limit"; "bounding"; "full"; "max_total_bytes"]
    @@ fun protocol ->
    Log.info "Test the bound on total operation size in the mempool." ;
    (* The test checks that the following sizes are correct; update
       them as needed. Note that the test currently relies on the fact
       that transfer_size > delegation_size >= attestation_size; if
       this is no longer the case, further adjustements will be
       needed. *)
    let transfer_size = 151 in
    let delegation_size = 146 in
    let attestation_size = 139 in
    (* Baseline fee *)
    let fee = 1000 in
    (* Gas limit used for all operations, so that weight (fee/gas
       limit ratio) only depends on fee. *)
    let gas_limit = 1500 in
    log_step
      0
      "Initialize a node and activate the protocol. Bake an additional block \
       to be able to inject valid attestations." ;
    let* node, client = Client.init_with_protocol `Client ~protocol () in
    let* level = bake_for ~wait_for_flush:true node client in

    (* Helpers *)
    let check_size ~loc ~expected op =
      let* size = Operation.byte_size op client in
      Check.(
        (size = expected)
          int
          ~error_msg:("Operation has size %L, expected %R at " ^ loc)) ;
      unit
    in
    let mk_transfer ~loc ~source ~fee =
      let* op =
        Operation.Manager.mk_single_transfer
          ~source
          ~dest:Constant.bootstrap5
          ~fee
          ~gas_limit
          client
      in
      let* () = check_size ~loc ~expected:transfer_size op in
      return op
    in
    let inject_transfer ~loc ~source ~fee =
      let* op = mk_transfer ~loc ~source ~fee in
      Operation.inject op client
    in
    let mk_delegation ~loc ~source ~fee =
      let* op =
        Operation.Manager.(
          operation [make ~source ~fee ~gas_limit (delegation ())])
          client
      in
      let* () = check_size ~loc ~expected:delegation_size op in
      return op
    in

    let max_total_bytes = transfer_size / 2 in
    log_step
      1
      "Update the mempool config to allow at most %d total bytes. Check that \
       injecting a transfer (%d bytes) fails without any recommended fee to \
       make the operation succeed."
      max_total_bytes
      transfer_size ;
    let* () = Mempool.Config.set_filter ~log:true ~max_total_bytes client in
    let* op1 = mk_transfer ~loc:__LOC__ ~source:Constant.bootstrap1 ~fee in
    let* (`OpHash _) =
      Operation.inject
        ~error:Operation.rejected_by_full_mempool_no_possible_fee
        op1
        client
    in
    let max_total_bytes = (2 * transfer_size) + delegation_size in
    log_step
      2
      "Update the mempool config to allow at most %d total bytes. Inject two \
       transfers, which have a size of %d bytes each, for a total of %d/%d. \
       Check that both are validated in the mempool."
      max_total_bytes
      transfer_size
      (2 * transfer_size)
      max_total_bytes ;
    let* () = Mempool.Config.set_filter ~log:true ~max_total_bytes client in
    let* (`OpHash oph2a) =
      inject_transfer ~loc:__LOC__ ~source:Constant.bootstrap1 ~fee
    in
    let* (`OpHash oph2b) =
      inject_transfer ~loc:__LOC__ ~source:Constant.bootstrap2 ~fee:(fee + 1)
    in
    let* () = check_mempool ~validated:[oph2a; oph2b] client in
    log_step
      3
      "Inject a third transfer with lower fee and check that it is rejected by \
       the mempool. Moreover, the minimal fee it would need to replace oph2a \
       is %d"
      (fee + 1) ;
    let* op3 =
      mk_transfer ~loc:__LOC__ ~source:Constant.bootstrap3 ~fee:(fee - 1)
    in
    let* () =
      Operation.inject_error_check_recommended_fee
        ~loc:__LOC__
        ~rex:Operation.rejected_by_full_mempool_with_needed_fee
        ~expected_fee:(fee + 1)
        op3
        client
    in
    log_step
      4
      "Inject another transfer with higher fee and check that it replaces \
       oph2a. Mempool total size is still %d/%d"
      (2 * transfer_size)
      max_total_bytes ;
    let* (`OpHash oph4) =
      inject_transfer ~loc:__LOC__ ~source:Constant.bootstrap4 ~fee:(fee + 10)
    in
    let* () =
      check_mempool ~validated:[oph2b; oph4] ~branch_delayed:[oph2a] client
    in
    log_step
      5
      "Inject a delegation with lower fee and check that it is validated since \
       it is only %d bytes. Mempool total size is now %d/%d."
      delegation_size
      ((2 * transfer_size) + delegation_size)
      max_total_bytes ;
    let* op5 =
      mk_delegation ~loc:__LOC__ ~source:Constant.bootstrap5 ~fee:(fee - 1)
    in
    let* (`OpHash oph5) = Operation.inject op5 client in
    let* () =
      check_mempool
        ~validated:[oph2b; oph4; oph5]
        ~branch_delayed:[oph2a]
        client
    in
    log_step
      6
      "Injecting a transfer with fee higher than the delegation still fails. \
       Injecting a transfer with the recommended fee succeeds and replaces \
       both oph2b (the lowest validated transfer) and oph5 (the delegation). \
       Mempool now contains only oph4 and oph6 for a total size of %d/%d."
      (2 * transfer_size)
      max_total_bytes ;
    let* op6a = mk_transfer ~loc:__LOC__ ~source:Constant.bootstrap3 ~fee in
    let expected_fee = fee + 2 in
    let* () =
      Operation.inject_error_check_recommended_fee
        ~loc:__LOC__
        ~rex:Operation.rejected_by_full_mempool_with_needed_fee
        ~expected_fee
        op6a
        client
    in
    let* (`OpHash oph6) =
      inject_transfer ~loc:__LOC__ ~source:Constant.bootstrap1 ~fee:expected_fee
    in
    let* () =
      check_mempool
        ~validated:[oph4; oph6]
        ~branch_delayed:[oph2a; oph2b; oph5]
        client
    in
    log_step
      7
      "Inject an attestation, which has size %d. Mempool total size is now \
       %d/%d."
      attestation_size
      ((2 * transfer_size) + attestation_size)
      max_total_bytes ;
    let* block_payload_hash =
      Operation.Consensus.get_block_payload_hash client
    in
    let* slots_json = Operation.Consensus.get_slots ~level client in
    let inject_attestation (delegate : Account.key) =
      let* op =
        Operation.Consensus.operation
          (Operation.Consensus.attestation
             ~use_legacy_name:true
             ~slot:(Operation.Consensus.first_slot ~slots_json delegate)
             ~level
             ~round:0
             ~block_payload_hash
             ())
          ~signer:delegate
          client
      in
      let* () = check_size ~loc:__LOC__ ~expected:attestation_size op in
      Operation.inject op client
    in
    let* (`OpHash oph7) = inject_attestation Constant.bootstrap1 in
    let* () =
      check_mempool
        ~validated:[oph4; oph6; oph7]
        ~branch_delayed:[oph2a; oph2b; oph5]
        client
    in
    log_step
      8
      "Inject two other attestations: they should replace oph6 then oph4." ;
    let* (`OpHash oph8a) = inject_attestation Constant.bootstrap2 in
    let* () =
      check_mempool
        ~validated:[oph4; oph7; oph8a]
        ~branch_delayed:[oph2a; oph2b; oph5; oph6]
        client
    in
    let* (`OpHash oph8b) = inject_attestation Constant.bootstrap3 in
    let* () =
      check_mempool
        ~validated:[oph7; oph8a; oph8b]
        ~branch_delayed:[oph2a; oph2b; oph5; oph6; oph4]
        client
    in
    log_step
      9
      "Injecting a delegation fails even with a high fee, and there is no \
       possible fee to recommend that would let the delegation replace an \
       attestation." ;
    let* op9 =
      Operation.Manager.(
        operation [make ~fee:1_000_000_000 ~gas_limit (delegation ())] client)
    in
    (* The operation is a bit larger than previous delegations because
       of the higher fee. *)
    let* () = check_size ~loc:__LOC__ ~expected:(delegation_size + 3) op9 in
    let* (`OpHash _) =
      Operation.inject
        ~error:Operation.rejected_by_full_mempool_no_possible_fee
        op9
        client
    in
    let max_total_bytes = (3 * attestation_size) + delegation_size + 1 in
    log_step
      10
      "Change the max_total_bytes to %d so that there is just enough room to \
       add a delegation but not a transfer. Successfully inject a delegation. \
       Injecting a transfer with higher fee fails with no recommended fee \
       because it would also need to replace an attestation which is never \
       allowed."
      max_total_bytes ;
    let* () = Mempool.Config.set_filter ~log:true ~max_total_bytes client in
    let* op10a = mk_delegation ~loc:__LOC__ ~fee ~source:Constant.bootstrap1 in
    let* (`OpHash oph10a) = Operation.inject op10a client in
    let* () =
      check_mempool
        ~validated:[oph7; oph8a; oph8b; oph10a]
        ~branch_delayed:[oph2a; oph2b; oph5; oph6; oph4]
        client
    in
    let* op10b =
      mk_transfer ~loc:__LOC__ ~source:Constant.bootstrap2 ~fee:(2 * fee)
    in
    let* (`OpHash _) =
      Operation.inject
        ~error:Operation.rejected_by_full_mempool_no_possible_fee
        op10b
        client
    in
    unit

  let precheck_with_empty_balance =
    Protocol.register_test
      ~__FILE__
      ~title:"Precheck refused an operation which empties a balance"
      ~tags:["mempool"; "precheck"; "empty"; "balance"]
    @@ fun protocol ->
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let* balance =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_balance
           ~id:Constant.bootstrap1.public_key_hash
           ()
    in
    let* _op =
      Operation.Manager.(
        inject [make ~fee:(Tez.to_mutez balance) @@ transfer ()] client)
    in
    unit

  let inject_operations =
    Protocol.register_test
      ~__FILE__
      ~title:"Test private/injection/operations RPC"
      ~tags:["mempool"; "injection"; "operations"; "rpc"]
    @@ fun protocol ->
    log_step 1 "Init a node and client with a fresh account" ;
    let* _node, client = Client.init_with_protocol ~protocol `Client () in
    let* new_account = Client.gen_and_show_keys ~alias:"new" client in

    log_step 2 "Forge and sign five operations" ;
    let* ops =
      Lwt_list.map_s
        (fun (account : Account.key) ->
          Operation.Manager.(
            operation
              [make ~source:account @@ transfer ~dest:Constant.bootstrap5 ()]
              client))
        [
          new_account;
          Constant.bootstrap1;
          Constant.bootstrap2;
          Constant.bootstrap3;
          Constant.bootstrap4;
        ]
    in

    log_step
      3
      "Try to inject two operations. Check that the injection failed (return \
       an error) because one operation was invalid. And check that the valid \
       operation is in the mempool" ;
    let* _ophs =
      Operation.(
        inject_operations
          ~error:
            (rex
               ~opts:[`Dotall]
               ".*While injecting several operations, one or several \
                injections failed. Errors are the one below in the \
                trace.*Empty implicit contract.*")
          [List.nth ops 0; List.nth ops 1])
        client
    in
    let* mempool = Mempool.get_mempool client in
    Check.(
      (List.length mempool.validated = 1)
        int
        ~error_msg:"Expected only %R validated op, got %L") ;

    log_step
      4
      "Inject two valid operations and check that they are in the validated \
       mempool" ;
    let* injected_ops =
      Operation.(inject_operations [List.nth ops 2; List.nth ops 3] client)
    in
    let injected_ops = List.map (fun (`OpHash op) -> op) injected_ops in
    let* () =
      check_mempool ~validated:(injected_ops @ mempool.validated) client
    in

    log_step
      5
      "Force inject two operations, an invalid one and a valid one. Check that \
       the first one is `branch_refused` and the second is in the validated \
       mempool" ;
    let* injected_ops2 =
      Operation.(
        inject_operations ~force:true [List.nth ops 0; List.nth ops 4] client)
    in
    let injected_ops2 = List.map (fun (`OpHash op) -> op) injected_ops2 in
    check_mempool
      ~validated:((List.nth injected_ops2 1 :: injected_ops) @ mempool.validated)
      ~branch_refused:[List.nth injected_ops2 0]
      client

  (** This test injects a well-formed batch of manager operations and
      checks that it is [validated] in the mempool. *)
  let test_inject_manager_batch =
    Protocol.register_test
      ~__FILE__
      ~title:"Inject manager batch"
      ~tags:["mempool"; "manager"; "batch"; "injection"; "validated"]
    @@ fun protocol ->
    log_step 1 "Initialize a node and a client." ;
    let* _node, client =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in

    let n_transactions = 3 in
    log_step 2 "Inject a well-formed batch of %d transactions." n_transactions ;
    let* (`OpHash oph) =
      let payload = Operation.Manager.transfer ~dest:Constant.bootstrap2 () in
      let source = Constant.bootstrap1 in
      let* counter = Operation.Manager.get_next_counter ~source client in
      let batch =
        Operation.Manager.make_batch
          ~source
          ~counter
          (List.init n_transactions (fun _ -> payload))
      in
      Operation.Manager.inject batch client
    in

    log_step 3 "Check that the batch is correctly [validated] in the mempool." ;
    let* mempool_json =
      Client.RPC.call client
      @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
    in
    let mempool = Mempool.of_json mempool_json in
    Mempool.check_mempool ~validated:[oph] mempool ;
    Log.info
      "The mempool contains exactly one [validated] operation with the correct \
       hash." ;
    let batch_payloads =
      JSON.(mempool_json |-> "validated" |=> 0 |-> "contents" |> as_list)
    in
    Check.(
      (List.compare_length_with batch_payloads n_transactions = 0)
        int
        ~error_msg:
          "The [validated] batch has a wrong number of manager payloads.") ;
    Log.info "The [validated] batch as the correct number of manager payloads." ;
    unit

  (** Runs a network of three nodes, one of which has a disabled mempool.
      Check that operations do not propagate to the node with disable mempool and
      that this node does not run a prevalidator nor accepts operation injections. *)
  let mempool_disabled =
    Protocol.register_test
      ~__FILE__
      ~title:"Mempool disabled"
      ~tags:["mempool"; "disabled"; "injection"]
    @@ fun protocol ->
    log_step
      1
      "Initialize three nodes and connect them. The mempool of the third node \
       is disabled." ;
    let* node1, client1 =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0]
        ~protocol
        `Client
        ()
    in
    let* node2, client2 =
      Client.init_with_node ~nodes_args:[Synchronisation_threshold 0] `Client ()
    in
    let* node3, client3 =
      Client.init_with_node
        ~nodes_args:[Synchronisation_threshold 0; Disable_mempool]
        `Client
        ()
    in
    (* Connect nodes and wait for them to synchronize on node1's level. *)
    let* () = Client.Admin.connect_address ~peer:node2 client1 in
    let* () = Client.Admin.connect_address ~peer:node3 client1 in
    let* () = Client.Admin.connect_address ~peer:node3 client2 in
    let* lvl1 = Node.get_level node1 in
    let* (_ : int) = Node.wait_for_level node2 lvl1
    and* (_ : int) = Node.wait_for_level node3 lvl1 in
    log_step
      2
      "Verify that prevalidators are running on node1 and node2, but not on \
       node3." ;
    let get_prevalidators client =
      let* prevalidators =
        Client.RPC.call client @@ RPC.get_workers_prevalidators
      in
      return JSON.(as_list prevalidators)
    in
    let* prevalidators1 = get_prevalidators client1 in
    Check.(
      (prevalidators1 <> [])
        (list json)
        ~__LOC__
        ~error_msg:"Expected node1 to have running prevalidators") ;
    let* prevalidators2 = get_prevalidators client2 in
    Check.(
      (prevalidators2 <> [])
        (list json)
        ~__LOC__
        ~error_msg:"Expected node2 to have running prevalidators") ;
    let* prevalidators3 = get_prevalidators client3 in
    Check.(
      (prevalidators3 = [])
        (list json)
        ~__LOC__
        ~error_msg:"Did not expected node3 to have running prevalidators") ;
    log_step 3 "Forge and inject an operation on node1." ;
    let* (`OpHash oph1) =
      Operation.Manager.(inject [make (transfer ())]) client1
    in
    log_step
      4
      "Ensure that the operation is validated on first two nodes, but not on \
       the third." ;
    let* () = check_mempool ~validated:[oph1] client1 in
    let* () = check_mempool ~validated:[oph1] client2 in
    let* () = check_mempool ~validated:[] client3 in
    log_step
      5
      "Check that injecting an operation into the node with disabled mempool \
       fails." ;
    let* (`OpHash _oph2) =
      let error =
        rex "Prevalidator is not running, cannot inject the operation."
      in
      let source = Constant.bootstrap2 in
      let dest = Constant.bootstrap3 in
      Operation.Manager.(inject ~error [make ~source (transfer ~dest ())])
        client3
    in
    unit

  (** This test checks that future attestations are correctly propagated, either
      immediately or when the head is sufficiently incremented. *)
  let propagation_future_attestation =
    Protocol.register_test
      ~__FILE__
      ~title:"Ensure that future attestations are propagated"
      ~tags:["attestation"; "mempool"; "branch_delayed"]
    @@ fun protocol ->
    log_step 1 "Initialize 3 nodes, connect them, and activate the protocol." ;
    let nodes_args = Node.[Synchronisation_threshold 0; Private_mode] in
    let event_sections_levels = [("prevalidator", `Debug)] in
    let* node_1, client_1 = Client.init_with_node ~nodes_args `Client ()
    and* node_2, client_2 =
      Client.init_with_node ~event_sections_levels ~nodes_args `Client ()
    and* node_3, client_3 =
      Client.init_with_node ~event_sections_levels ~nodes_args `Client ()
    in
    let* () = Client.Admin.trust_address client_1 ~peer:node_2
    and* () = Client.Admin.trust_address client_2 ~peer:node_1
    and* () = Client.Admin.trust_address client_2 ~peer:node_3
    and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
    let* () = Client.Admin.connect_address client_1 ~peer:node_2
    and* () = Client.Admin.connect_address client_2 ~peer:node_3 in
    let* () = Client.activate_protocol_and_wait ~protocol client_1 in
    let* _ = Node.wait_for_level node_2 1
    and* _ = Node.wait_for_level node_3 1 in

    log_step 2 "Disconnect all the nodes from each other." ;
    let* node_1_id = Node.wait_for_identity node_1
    and* node_2_id = Node.wait_for_identity node_2
    and* node_3_id = Node.wait_for_identity node_3 in
    let* () = Client.Admin.kick_peer client_1 ~peer:node_2_id
    and* () = Client.Admin.kick_peer client_2 ~peer:node_1_id
    and* () = Client.Admin.kick_peer client_2 ~peer:node_3_id
    and* () = Client.Admin.kick_peer client_3 ~peer:node_2_id in

    let retrieve_attestation client =
      let* mempool =
        Client.RPC.call client
        @@ RPC.get_chain_mempool_pending_operations ~version:"2" ()
      in
      let op = List.hd JSON.(mempool |-> "validated" |> as_list) in
      let oph = JSON.(op |-> "hash" |> as_string) in
      let branch = JSON.(op |-> "branch" |> as_string) in
      let content = JSON.(op |-> "contents" |> as_list |> List.hd) in
      let kind = JSON.(content |-> "kind" |> as_string) in
      let expected_kind =
        if Protocol.number protocol < 018 then "endorsement" else "attestation"
      in
      Check.(
        (expected_kind = kind)
          string
          ~error_msg:"Invalid operation kind, expected: %L, got %R") ;
      let slot = JSON.(content |-> "slot" |> as_int) in
      let level = JSON.(content |-> "level" |> as_int) in
      let round = JSON.(content |-> "round" |> as_int) in
      let block_payload_hash =
        JSON.(content |-> "block_payload_hash" |> as_string)
      in
      let op =
        Operation.Consensus.attestation
          ~use_legacy_name:true
          ~slot
          ~level
          ~round
          ~block_payload_hash
          ()
      in
      return (op, branch, oph)
    in

    log_step
      3
      "Bake a block on node_1 then inject an attestation, which is one level \
       in the future from the perspective of nodes 2 and 3. Retrieve the hash \
       and bytes representing this attestation, called future1 from now on." ;
    let* () = Client.bake_for_and_wait ~node:node_1 client_1 in
    let injection_waiter = Node.wait_for_request ~request:`Inject node_1 in
    let* () = Client.attest_for client_1 ~force:true ~protocol in
    let* () = injection_waiter in
    let* op_future1, branch_future1, oph_future1 =
      retrieve_attestation client_1
    in

    log_step
      4
      "Bake another block on node_1 then inject another attestation, which is \
       two levels in the future from the perspective of nodes 2 and 3. \
       Retrieve the hash and bytes representing this attestation, called \
       future2." ;
    let* () = Node_event_level.bake_wait_log node_1 client_1 in

    let injection_waiter2 = Node.wait_for_request ~request:`Inject node_1 in
    let* () = Client.attest_for client_1 ~force:true ~protocol in
    let* () = injection_waiter2 in
    let* op_future2, branch_future2, oph_future2 =
      retrieve_attestation client_1
    in

    log_step 5 "Inject both attestations in node_2." ;
    let* _ =
      Operation.Consensus.inject
        ~force:true
        ~branch:branch_future1
        ~signer:Constant.bootstrap1
        op_future1
        client_2
    in
    let* _ =
      Operation.Consensus.inject
        ~force:true
        ~branch:branch_future2
        ~signer:Constant.bootstrap1
        op_future2
        client_2
    in

    log_step 6 "Reconnect node_2 and node_3, and synchronize their mempools." ;
    let* () = Client.Admin.trust_address client_2 ~peer:node_3
    and* () = Client.Admin.trust_address client_3 ~peer:node_2 in
    let* () = Client.Admin.connect_address client_2 ~peer:node_3 in
    let* () = synchronize_mempool client_3 node_3 in

    log_step
      7
      "Check that both attestations are in the mempool of node_2: future1 is \
       validated; future2 is branch_delayed." ;
    let* () =
      check_mempool
        ~validated:[oph_future1]
        ~branch_delayed:[oph_future2]
        client_2
    in

    log_step 8 "Check that future1 is validated in node_3 mempool." ;
    let* () = check_mempool ~validated:[oph_future1] client_3 in

    log_step
      9
      "Bake one block on node_2 and synchronize its mempool. Check that \
       future1 is now validated, as well as future2." ;
    let* () = Node_event_level.bake_wait_log node_2 client_2 in
    let* () = synchronize_mempool client_2 node_2 in
    let* () = check_mempool ~validated:[oph_future1; oph_future2] client_2 in

    log_step
      10
      "Synchronize the mempool of node_3. Check that future1 and future2 are \
       validated." ;
    let* () = synchronize_mempool client_3 node_3 in
    let* () = check_mempool ~validated:[oph_future1; oph_future2] client_3 in
    unit

  let test_mempool_config_operation_filtering =
    Protocol.register_test
      ~__FILE__
      ~title:"mempool config operation filtering"
      ~tags:
        [
          "node";
          "mempool";
          "config";
          "filter";
          "operation";
          "arrival";
          "validated";
          "refused";
        ]
    @@ fun protocol ->
    Log.info
      "Aim: test that modifying the mempool configuration via the RPC [POST \
       /chains/<chain>/mempool/filter] correctly impacts the classification of \
       the operations that arrive from a peer." ;
    log_step 1 "Start two nodes, connect them, and activate the protocol." ;
    let nodes_args = Node.[Connections 1; Synchronisation_threshold 0] in
    let* node1, client1 =
      Client.init_with_protocol
        ~nodes_args
          (* Need event level [debug] to receive operation arrival events in [node1]. *)
        ~event_sections_levels:[("prevalidator", `Debug)]
        ~protocol
        `Client
        ()
    in
    let* node2, client2 = Client.init_with_node ~nodes_args `Client () in
    let* () = Client.Admin.connect_address client1 ~peer:node2 in
    let* (_ : int) = Node.wait_for_level node2 1 in
    let fee1 = 1000 and fee2 = 101 in
    log_step
      2
      "Inject two transfers op1 and op2 with respective fees %d and %d (in \
       mutez) in node2."
      fee1
      fee2 ;
    let inject_transfer_node2 source ~fee =
      let waiter = Node.wait_for_request ~request:`Arrived node1 in
      let* ophash =
        Operation.Manager.inject_single_transfer ~source ~fee client2
      in
      let* () = waiter in
      return ophash
    in
    let* (`OpHash oph1) = inject_transfer_node2 Constant.bootstrap1 ~fee:fee1 in
    let* (`OpHash oph2) = inject_transfer_node2 Constant.bootstrap2 ~fee:fee2 in
    log_step
      3
      "Check that both operations are validated in node2's mempool. Indeed, \
       minimal fee checks are skipped by local injection." ;
    let* () = check_mempool ~validated:[oph1; oph2] client2 in
    log_step
      4
      "Check that in the mempool of node1, op1 is validated whereas op2 is \
       refused. Indeed, node1 has the default filter config: minimum 100 mutez \
       PLUS 100 nanotez per gas unit PLUS 1000 nanotez per byte." ;
    let* () = check_mempool ~validated:[oph1] ~refused:[oph2] client1 in
    log_step
      5
      "Set minimal_nanotez_per_gas_unit and minimal_nanotez_per_byte to 0 in \
       node1." ;
    let* () =
      Mempool.Config.set_filter
        ~log:true
        ~minimal_nanotez_per_gas_unit:(0, 1)
        ~minimal_nanotez_per_byte:(0, 1)
        client1
    in
    let fee3 = 100 and fee4 = 99 in
    log_step
      6
      "Inject new transfers op3 and op4 with respective fees %d and %d in \
       node2."
      fee3
      fee4 ;
    let* (`OpHash oph3) = inject_transfer_node2 Constant.bootstrap3 ~fee:fee3 in
    let* (`OpHash oph4) = inject_transfer_node2 Constant.bootstrap4 ~fee:fee4 in
    log_step
      7
      "Check that op3 is validated in the mempool of node1, while op4 is \
       refused. Note that op2 would now be valid, but it has already been \
       refused so it will never be reclassified." ;
    let* () =
      check_mempool ~validated:[oph1; oph3] ~refused:[oph2; oph4] client1
    in
    log_step
      8
      "In node2, set minimal_nanotez_per_gas_unit and minimal_nanotez_per_byte \
       to 0 but minimal_fees to 101." ;
    let* () =
      Mempool.Config.set_filter
        ~log:true
        ~minimal_fees:101
        ~minimal_nanotez_per_gas_unit:(0, 1)
        ~minimal_nanotez_per_byte:(0, 1)
        client2
    in
    log_step
      9
      "Bake from node2. Since the baker filters operations to include in block \
       independently from the mempool configuration, only op1 is included in \
       the new block, and therefore is removed from the mempool. Moreover, \
       operations are filtered again when the mempool gets flushed, so op3 and \
       op4 become refused, and the only validated operation left in node2 is \
       op2." ;
    let* (_level : int) = bake_for ~wait_for_flush:true node2 client2 in
    let* () = check_mempool ~validated:[oph2] ~refused:[oph3; oph4] client2 in
    log_step
      10
      "Meanwhile in node1, op1 is removed from the mempool, op2 and op4 are \
       still refused, and op3 is again classified as validated." ;
    let* () = check_mempool ~validated:[oph3] ~refused:[oph2; oph4] client1 in
    log_step
      11
      "Set minimal_fees to 10 in the mempool configuration of node1, while \
       keeping minimal_nanotez_per_gas_unit and minimal_nanotez_per_byte at 0." ;
    let* () =
      Mempool.Config.set_filter
        ~log:true
        ~minimal_fees:10
        ~minimal_nanotez_per_gas_unit:(0, 1)
        ~minimal_nanotez_per_byte:(0, 1)
        client1
    in
    let fee5 = 10 and fee6 = 0 in
    log_step
      12
      "Inject operations op5 and op6 with respective fees %d and %d in node2, \
       and check that both are validated in node2."
      fee5
      fee6 ;
    let* (`OpHash oph5) = inject_transfer_node2 Constant.bootstrap5 ~fee:fee5 in
    (* op1 from source bootstrap1 has been included in a block, so we
       can safely use the same source again. *)
    let* (`OpHash oph6) = inject_transfer_node2 Constant.bootstrap1 ~fee:fee6 in
    let* () =
      check_mempool ~validated:[oph2; oph5; oph6] ~refused:[oph3; oph4] client2
    in
    log_step 13 "Check that node1 validates op5 but refuses op6." ;
    check_mempool ~validated:[oph3; oph5] ~refused:[oph2; oph4; oph6] client1
end

let check_operation_is_in_validated_mempool ops oph =
  let open JSON in
  let ops_list = as_list (ops |-> "validated") in
  let res =
    List.exists (fun e -> e |-> "hash" |> as_string = as_string oph) ops_list
  in
  if not res then
    Test.fail "Operation %s was not found in the mempool" (JSON.encode oph)

type mempool_count = {
  validated : int;
  branch_delayed : int;
  branch_refused : int;
  refused : int;
  outdated : int;
  unprocessed : int;
  total : int;
}

let count_mempool mempool =
  let open JSON in
  let validated = as_list (mempool |-> "validated") |> List.length in
  let branch_delayed = as_list (mempool |-> "branch_delayed") |> List.length in
  let branch_refused = as_list (mempool |-> "branch_refused") |> List.length in
  let refused = as_list (mempool |-> "refused") |> List.length in
  let outdated = as_list (mempool |-> "outdated") |> List.length in
  let unprocessed = as_list (mempool |-> "unprocessed") |> List.length in
  let total =
    validated + branch_delayed + branch_refused + refused + outdated
    + unprocessed
  in
  {
    validated;
    branch_delayed;
    branch_refused;
    refused;
    outdated;
    unprocessed;
    total;
  }

let pp_mempool_count fmt
    {
      validated;
      branch_delayed;
      branch_refused;
      refused;
      outdated;
      unprocessed;
      total;
    } =
  Format.fprintf
    fmt
    "total: %d - validated: %d, branch_delayed: %d, branch_refused: %d, \
     refused: %d, outdated: %d, unprocessed: %d"
    total
    validated
    branch_delayed
    branch_refused
    refused
    outdated
    unprocessed

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
  let* _ = Node.wait_for node "request_completed_info.v0" filter in
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

let forge_operation ~branch ~fee ~gas_limit ~source ~destination ~counter
    ~client =
  let op_json = operation_json ~fee ~gas_limit ~source ~destination ~counter in
  let op_json_branch = operation_json_branch ~branch op_json in
  let* op_hex =
    Client.RPC.call client
    @@ RPC.post_chain_block_helpers_forge_operations
         ~data:(Data (Ezjsonm.from_string op_json_branch))
         ()
  in
  return (`Hex (JSON.as_string op_hex))

let inject_operation ~client (`Hex op_str_hex) (`Hex signature) =
  let signed_op = op_str_hex ^ signature in
  Client.RPC.call client
  @@ RPC.post_injection_operation (Data (`String signed_op))

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
  let signature = Operation.sign_manager_op_hex ~signer op_str_hex in
  inject_operation ~client op_str_hex signature

(* TODO: add a test than ensure that we cannot have more than 1000
   branch delayed/branch refused/refused *)

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
  let* () = Client.activate_protocol_and_wait ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_2 1 in
  Log.info "All nodes are at level %d." 1 ;
  (* Step 2 *)
  (* Get the counter and the current branch *)
  let* base_counter_json =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.public_key_hash
         ()
  in
  let counter = JSON.as_int base_counter_json in
  let* branch = Operation.Manager.get_branch client_1 in

  (* Step 3 *)
  (* Forge operation, inject it and check injection *)
  let* _op =
    forge_and_inject_operation
      ~branch
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
  let* () = Client.bake_for_and_wait client_2 in
  (* Step 5 *)
  (* Get client_2 ddb and check that it contains no operation *)
  let* ddb2 =
    Client.RPC.call client_2 @@ RPC.get_worker_chain_validator_ddb ()
  in
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

let set_config_operations_timeout timeout json =
  let chain_validator_config =
    let open JSON in
    json |-> "shell" |-> "chain_validator"
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
  JSON.put ("shell", updated_shell_config) json

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
  let* node_2 =
    Node.init
    (* Run the node with the new config.
       event_level is set to debug to catch fetching event at this level *)
      ~event_sections_levels:[("prevalidator", `Debug)]
        (* Set a low operations_request_timeout to force timeout at fetching *)
      ~patch_config:(set_config_operations_timeout 0.00001)
      [Synchronisation_threshold 0; Private_mode]
  in
  let* () = Node.wait_for_ready node_2 in
  let* client_1 = Client.init ~endpoint:(Node node_1) ()
  and* client_2 = Client.init ~endpoint:(Node node_2) () in
  let* () = Client.Admin.trust_address client_1 ~peer:node_2
  and* () = Client.Admin.trust_address client_2 ~peer:node_1 in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol_and_wait ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_2 1 in
  Log.info "All nodes are at level %d." 1 ;
  (* Step 2 *)
  (* get counter and branches *)
  let* counter_json =
    Client.RPC.call client_1
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.public_key_hash
         ()
  in
  let counter = JSON.as_int counter_json in
  let* branch = Operation.Manager.get_branch client_1 in

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
  let signature =
    Operation.sign_manager_op_hex ~signer:Constant.bootstrap1 op_str_hex
  in
  let failed_fetching_waiter = wait_for_failed_fetch node_2 in
  let* oph = inject_operation ~client:client_1 op_str_hex signature in
  let* () = failed_fetching_waiter in
  (* Step 4 *)
  (* Ensure that the injected operation is in node_1 mempool *)
  let* mempool_node_1 =
    Client.RPC.call client_1 @@ RPC.get_chain_mempool_pending_operations ()
  in
  check_operation_is_in_validated_mempool mempool_node_1 oph ;
  (* Step 5 *)
  (* Ensure that the mempool of node_2 is empty *)
  let* mempool_count_after_failed_fetch =
    Client.RPC.call client_2 @@ RPC.get_chain_mempool_pending_operations ()
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
  let* mempool_inject_on_node_2 =
    Client.RPC.call client_2 @@ RPC.get_chain_mempool_pending_operations ()
  in
  check_operation_is_in_validated_mempool mempool_inject_on_node_2 oph ;
  unit

let check_op_removed client op =
  let* pending_ops =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  let open JSON in
  let ops_list = pending_ops |-> "validated" |> as_list in
  let res = List.exists (fun e -> e |-> "hash" |> as_string = op) ops_list in
  if res then Test.fail "%s found after removal" op ;
  unit

(** Bakes with an empty mempool to force synchronisation between nodes. *)
let bake_empty_block ?endpoint ?protocol client =
  let mempool = Client.empty_mempool_file () in
  Client.bake_for_and_wait
    ?protocol
    ?endpoint
    ~mempool
    ~ignore_node_mempool:true
    client

(** [bake_empty_block_and_wait_for_flush client node] bakes for [client]
    with an empty mempool, then waits for a [flush] event on [node] (which
    will usually be the node corresponding to [client], but could be any
    node with a connection path to it). *)
let bake_empty_block_and_wait_for_flush ~protocol ?(log = false) client node =
  let waiter = wait_for_flush node in
  let* () = bake_empty_block ~protocol client in
  if log then
    Log.info "Baked for %s with an empty mempool." (Client.name client) ;
  waiter

(* for functions [transfer_and_wait_for_injection], [wait_for_arrival],
   and [get_validated_operation_hash_list] *)
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

(** Gets the list of hashes of the mempool's validated operations,
    displays it, and returns it. *)
let get_and_log_validated client =
  let* ophs = get_validated_operation_hash_list client in
  Log.info "Validated operations in mempool:" ;
  List.iter (Log.info "- %s") ophs ;
  return ophs

(** Boolean indicating whether two lists of operation hashes (strings)
   are equal (returns [false] if they have different lengths, instead
   of raising [invalid_arg] as using [List.for_all2] directly would
   do). We use a naive way to check both lists are equal because
   1. performances for small lists does not matter and 2. the mempool
   does not specify how operations previously validated will be validated
   again after banning one operation. *)
let oph_list_equal l1 l2 =
  Int.equal (List.compare_lengths l1 l2) 0
  && List.for_all (fun x -> List.mem x l2) l1
  && List.for_all (fun x -> List.mem x l1) l2

(** Gets the list of hashes of the mempool's validated operations,
    and asserts that it is equal to the given list [expected_ophs]. *)
let check_validated_ophs_is client expected_ophs =
  let* ophs = get_validated_operation_hash_list client in
  if oph_list_equal ophs expected_ophs then (
    Log.info "Checking validated operations in mempool:" ;
    List.iter (Log.info "- %s") ophs ;
    unit)
  else (
    Log.info "Expected validated operations:" ;
    List.iter (Log.info "- %s") expected_ophs ;
    Log.info "Actual validated operations:" ;
    List.iter (Log.info "- %s") ophs ;
    Test.fail
      "Wrong list of validated operations in mempool (use --info to see \
       expected and actual lists).")

(** Test.

    Aim: check that, when banning an operation that was validated in the
    mempool, the other validated operations are correctly revalidated (in
    the same order).

    Scenario:
    - Step 1: Start two nodes, connect them, activate the protocol.
    - Step 2: Inject five operations (transfers from five different sources,
      injected by both nodes in alternance).
    - Step 3: Ban one of these operations from node_1 (arbitrarily, the third
      in the list of validated operations in the mempool of node_1).
    - Step 4: Check that validated operations in node_1 are still validated

    Note: the chosen operations are commutative, so that none of them
    becomes branch_delayed instead of validated when one of them is banned.
*)
let ban_operation_and_check_validated =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool ban operation and check validated"
    ~tags:["mempool"; "node"]
  @@ fun protocol ->
  Log.info "Step 1: Start two nodes, connect them, activate the protocol." ;
  let* node_1 =
    Node.init
      ~event_sections_levels:[("prevalidator", `Debug)]
        (* to witness operation arrival events *)
      [Synchronisation_threshold 0; Connections 1]
  and* node_2 = Node.init [Synchronisation_threshold 0; Connections 1] in
  let* client_1 = Client.init ~endpoint:Client.(Node node_1) ()
  and* client_2 = Client.init ~endpoint:Client.(Node node_2) () in
  let* () = Client.Admin.connect_address client_1 ~peer:node_2 in
  let* () = Client.activate_protocol_and_wait ~protocol client_1 in
  let* _ = Node.wait_for_level node_2 1 in
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
     in the list of validated operations in the mempool of node_1)." ;
  let* validated_ophs = get_and_log_validated client_1 in
  if not (Int.equal (List.compare_length_with validated_ophs 5) 0) then
    (* This could theoretically happen: we wait for each transfer to
       be present in the mempool as "pending", but not to be classified
       as "validated". In practice, this does not seem to be a problem. *)
    Test.fail
      "Found only %d validated operations in node_1, expected 5."
      (List.length validated_ophs) ;
  let oph_to_ban = List.nth validated_ophs 2 in
  Log.info "Operation to ban: %s" oph_to_ban ;
  let* _ =
    Client.RPC.call client_1
    @@ RPC.post_chain_mempool_ban_operation ~data:(Data (`String oph_to_ban)) ()
  in
  Log.info "Operation %s is now banned." oph_to_ban ;
  Log.info
    "Step 4: Check that validated operations in node_1 are still validated." ;
  let expected_revalidated_ophs =
    List.filter (fun oph -> not (String.equal oph_to_ban oph)) validated_ophs
  in
  let* () = check_validated_ophs_is client_1 expected_revalidated_ophs in
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
    | Some "arrived", Some s when String.equal s ophash ->
        Log.info "Witnessed arrival of operation %s." ophash ;
        Some ()
    | _ -> None
  in
  Node.wait_for node "request_completed_debug.v0" filter

(** [set_filter_no_fee_requirement client] sets all fields [minimal_*]
    to 0 in the filter configuration of [client]'s mempool. *)
let set_filter_no_fee_requirement =
  Mempool.Config.set_filter
    ~minimal_fees:0
    ~minimal_nanotez_per_gas_unit:(0, 1)
    ~minimal_nanotez_per_byte:(0, 1)

(** Checks that arguments [validated] and [refused] are the number of operations
    in the mempool of [client] with the corresponding classification,
    that both sets of operations are disjoint, and that there is no
    [branch_delayed], [branch_refused], or [unprocessed] operation.
    If [log] is [true], also logs the hash and fee of all validated
    and refused operations. *)
let check_mempool_ops ?(log = false) client ~validated ~refused =
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
  let* ops =
    Client.RPC.call client @@ RPC.get_chain_mempool_pending_operations ()
  in
  let open JSON in
  (* get (and log) validated and refused operations *)
  let get_ophs_and_log_fees classification =
    List.map
      (fun op ->
        let oph = get_hash op in
        log_op classification oph (op |-> "contents" |=> 0 |-> "fee" |> as_int) ;
        oph)
      (ops |-> classification |> as_list)
  in
  let validated_ophs = get_ophs_and_log_fees "validated" in
  let refused_ophs = get_ophs_and_log_fees "refused" in
  (* various checks about validated and refused operations *)
  Check.(
    (* Not using [List.compare_length_with] allows for a more informative
       error message. The lists are expected to be short anyway. *)
    (List.length validated_ophs = validated)
      int
      ~error_msg:(name ^ ": found %L validated operation(s), expected %R.")) ;
  Check.(
    (List.length refused_ophs = refused)
      int
      ~error_msg:(name ^ ": found %L refused operation(s), expected %R.")) ;
  List.iter
    (fun oph ->
      if List.mem oph refused_ophs then
        Test.fail "%s: operation %s is both validated and refused" name oph)
    validated_ophs ;
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
  Node.wait_for node "request_no_errors_prevalidator.v0" (fun event ->
      let open JSON in
      let view = event |-> "view" in
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
  let* baked_ops =
    Client.RPC.call client @@ RPC.get_chain_block_operations ()
  in
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
      fees 1000 and 10 mutez. Check that both operations are [validated] in
      [node2]'s mempool.
    - Step 4: Bake with an empty mempool for [node1] to force synchronization
      with [node2]. Check that the mempool of [node1] has one validated and one
      refused operation. Indeed, [node1] has the default filter config with
      [minimal_fees] at 100 mutez.
    - Step 5: In [node1]'s mempool filter configuration, set all fields
      [minimal_*] to 0. Inject a new operation with fee 5 in [node2], then
      bake with an empty mempool. Check that [node1] contains two validated
      operations (the ones with fee 1000 and 5) and one refused operation.
      Indeed, the operation with fee 10 would now be valid, but it has already
      been refused so it must not be revalidated.
    - Step 6: Bake for [node1] (normally, i.e. without enforcing a given
      mempool). Check that the baked block contains exactly one manager
      operation (the category containing transfer operations). Indeed, the
      filter used to determine which operations are included in the block does
      not share its configuration with the mempool's filter, so only the
      operation of fee 1000 is included. Check that [node1] contains one
      validated operation (fee 5) and one refused operation (fee 10), and that
      [node2] contains two validated operations. *)
let test_do_not_reclassify =
  Protocol.register_test
    ~__FILE__
    ~title:"mempool do not reclassify"
    ~tags:["mempool"; "node"; "filter"; "refused"; "validated"]
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
  and* () = Client.activate_protocol_and_wait ~protocol client1 in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node2 proto_activation_level in
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
     1000 and 10 mutez. Check that both operations are [validated] in \
     [node2]'s mempool." ;
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
  let* () = check_mempool_ops ~log:true client2 ~validated:2 ~refused:0 in
  Log.info
    ~color:step_color
    "Step 4: Bake with an empty mempool for [node1] to force synchronization \
     with [node2]. Check that the mempool of [node1] has one validated and one \
     refused operation. Indeed, [node1] has the default filter config with \
     [minimal_fees] at 100 mutez." ;
  let* () =
    bake_empty_block_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  let* () = waiter_arrival_node1 in
  let* () = check_mempool_ops ~log:true client1 ~validated:1 ~refused:1 in
  Log.info
    ~color:step_color
    "Step 5: In [node1]'s mempool filter configuration, set all fields \
     [minimal_*] to 0. Inject a new operation with fee 5 in [node2], then bake \
     with an empty mempool. Check that [node1] contains two validated \
     operations (the ones with fee 1000 and 5) and one refused operation. \
     Indeed, the operation with fee 10 would now be valid, but it has already \
     been refused so it must not be revalidated." ;
  let* _ = set_filter_no_fee_requirement client1 in
  let* () = inject_transfer Constant.bootstrap3 ~fee:5 in
  let waiter_notify_3_valid_ops = wait_for_notify_n_valid_ops node1 3 in
  let* () =
    bake_empty_block_and_wait_for_flush ~protocol ~log:true client1 node1
  in
  (* Wait for [node1] to receive a mempool containing 3 operations (the
     number of [validated] operations in [node2]), among which will figure
     the operation with fee 10 that has already been [refused] in [node1]. *)
  let* () = waiter_notify_3_valid_ops in
  let* () = check_mempool_ops ~log:true client1 ~validated:2 ~refused:1 in
  Log.info
    ~color:step_color
    "Step 6: Bake for [node1] (normally, i.e. without enforcing a given \
     mempool). Check that the baked block contains exactly one manager \
     operation (the category containing transfer operations). Indeed, the \
     filter used to determine which operations are included in the block does \
     not share its configuration with the mempool's filter, so only the \
     operation of fee 1000 is included. Check that [node1] contains one \
     validated operation (fee 5) and one refused operation (fee 10), and that \
     [node2] contains 2 validated operations." ;
  let* () = bake_wait_log ~protocol node1 client1 in
  let* () = check_n_manager_ops_in_block ~log:true client1 1 in
  let* () = check_mempool_ops ~log:true client1 ~validated:1 ~refused:1 in
  let* () = check_mempool_ops ~log:true client2 ~validated:2 ~refused:0 in
  unit

let get_refused_operation_hash_list_v0 mempool =
  List.map
    (fun op -> JSON.(op |=> 0 |> as_string))
    JSON.(mempool |-> "refused" |> as_list)

let get_refused_operation_hash_list_v1 mempool =
  List.map get_hash JSON.(mempool |-> "refused" |> as_list)

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
  let node1 = Node.create ~allow_all_rpc:false [] in
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
  and* () = Client.activate_protocol_and_wait ~protocol client2 in
  let proto_activation_level = 1 in
  let* _ = Node.wait_for_level node1 proto_activation_level in
  Log.info "Both nodes are at level %d." proto_activation_level ;
  Log.info "%s" step3_msg ;
  let* json =
    Client.RPC.call client2
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.public_key_hash
         ()
  in
  let counter = JSON.as_int json in
  let* branch = Operation.Manager.get_branch client2 in

  Log.info "%s" step4_msg ;
  let* (`Hex op_str_hex as op_hex) =
    forge_operation
      ~branch
      ~fee:1000 (* Minimal fees to successfully apply the transfer *)
      ~gas_limit:1040 (* Minimal gas to successfully apply the transfer *)
      ~source:Constant.bootstrap2.public_key_hash
      ~destination:Constant.bootstrap1.public_key_hash
      ~counter (* Invalid counter *)
      ~client:client2
  in
  let (`Hex signature) =
    Operation.sign_manager_op_hex ~signer:Constant.bootstrap2 op_hex
  in
  let signed_op = op_str_hex ^ signature in
  Log.info "%s" step5_msg ;
  let*? p =
    Client.RPC.spawn client1
    @@ RPC.post_injection_operation (Data (`String signed_op))
  in
  let injection_error_rex =
    rex
      ~opts:[`Dotall]
      "Fatal error:\n\
      \  Command failed: Error while validating injected operation.*:"
  in
  let* () = Process.check_error ~msg:injection_error_rex p in
  Log.info "%s" step6_msg ;
  let*? p =
    Client.RPC.spawn client1
    @@ RPC.post_private_injection_operation (Data (`String signed_op))
  in
  let access_error_rex =
    rex ~opts:[`Dotall] "Fatal error:\n  .HTTP 403. Access denied to: .*"
  in
  let* () = Process.check_error ~msg:access_error_rex p in
  Log.info "%s" step7_msg ;
  let*? p =
    Client.RPC.spawn client2
    @@ RPC.post_injection_operation (Data (`String signed_op))
  in
  let* () = Process.check_error ~msg:injection_error_rex p in
  Log.info "%s" step8_msg ;
  let* _ =
    Client.RPC.call client2
    @@ RPC.post_private_injection_operation (Data (`String signed_op))
  in
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
  log_step 1 step1 ;
  let* node =
    Node.init [Synchronisation_threshold 0; Private_mode; Connections 0]
  in
  let* client = Client.init ~endpoint:(Node node) () in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      [(["max_operations_time_to_live"], `Int max_operations_ttl)]
  in
  let* () =
    Client.activate_protocol_and_wait ~protocol ~parameter_file client
  in
  log_step 2 step2 ;
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_counter
         ~id:Constant.bootstrap1.public_key_hash
         ()
  in
  let counter = JSON.as_int json in
  let* branch = Operation.Manager.get_branch client in
  log_step 3 step3 ;
  (* To avoid off-by-one mistakes *)
  let blocks_to_bake = 2 in
  let* () =
    repeat (max_operations_ttl + blocks_to_bake) (fun () ->
        Client.bake_for_and_wait client)
  in
  (* + 1 for the activation block *)
  let* _ = Node.wait_for_level node (max_operations_ttl + blocks_to_bake + 1) in
  log_step 4 step4 ;
  let* (`Hex op_str_hex as op_hex) =
    forge_operation
      ~branch
      ~fee:1000
      ~gas_limit:1040
      ~source:Constant.bootstrap1.public_key_hash
      ~destination:Constant.bootstrap3.public_key_hash
      ~counter:(counter + 1)
      ~client
  in
  let (`Hex signature) =
    Operation.sign_manager_op_hex ~signer:Constant.bootstrap1 op_hex
  in
  log_step 5 step5 ;
  let*? process =
    Client.RPC.spawn client
    @@ RPC.post_injection_operation (Data (`String (op_str_hex ^ signature)))
  in
  let injection_error_rex =
    rex
      ~opts:[`Dotall]
      "Fatal error:\n\
      \  Command failed: Operation .* is branched on a block .* which is too \
       old"
  in
  Process.check_error ~msg:injection_error_rex process

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
  let* () = Client.activate_protocol_and_wait ~protocol client_1 in
  Log.info "Activated protocol." ;
  let* _ = Node.wait_for_level node_2 1 in
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
    let* ophs = get_validated_operation_hash_list client_1 in
    match ophs with
    | [oph] -> return oph
    | _ -> Test.fail "Validated mempool should contain exactly one operation"
  in
  Log.info "%s" step4_msg ;
  let wait_mempool = wait_for_arrival_of_ophash oph node_2 in
  let* () = Client.Admin.connect_address ~peer:node_1 client_2 in
  let* node1_identity = Node.wait_for_identity node_1 in
  let* _ =
    Client.RPC.call client_2
    @@ RPC.post_chain_mempool_request_operations ~peer:node1_identity ()
  in
  let* () = wait_mempool in
  unit

let register ~protocols =
  Revamped.flush_mempool protocols ;
  Revamped.recycling_branch_refused protocols ;
  Revamped.ban_operation_branch_delayed_reevaluated protocols ;
  Revamped.one_operation_per_manager_per_block_restriction_injection protocols ;
  Revamped.one_operation_per_manager_per_block_restriction_propagation protocols ;
  Revamped.one_operation_per_manager_per_block_flush protocols ;
  Revamped.one_operation_per_manager_per_block_ban protocols ;
  Revamped.one_operation_per_manager_per_block_flush_on_ban protocols ;
  Revamped.one_operation_per_manager_per_block_inject_isolated_node protocols ;
  Revamped.max_refused_operations_branch_delayed protocols ;
  Revamped.max_refused_operations_branch_refused protocols ;
  Revamped.max_refused_operations_refused protocols ;
  Revamped.max_refused_operations_outdated protocols ;
  Revamped.ban_operation protocols ;
  Revamped.unban_operation_and_reinject protocols ;
  Revamped.unban_all_operations protocols ;
  Revamped.test_full_mempool_propagation protocols ;
  Revamped.test_full_mempool_and_replace_same_manager protocols ;
  Revamped.test_full_mempool_attestation_vs_manager protocols ;
  Revamped.test_full_mempool_max_total_bytes protocols ;
  Revamped.precheck_with_empty_balance protocols ;
  Revamped.inject_operations protocols ;
  Revamped.test_inject_manager_batch protocols ;
  Revamped.mempool_disabled protocols ;
  Revamped.propagation_future_attestation protocols ;
  Revamped.test_mempool_config_operation_filtering protocols ;
  forge_pre_filtered_operation protocols ;
  refetch_failed_operation protocols ;
  ban_operation_and_check_validated protocols ;
  test_do_not_reclassify protocols ;
  force_operation_injection protocols ;
  injecting_old_operation_fails protocols ;
  test_request_operations_peer protocols
