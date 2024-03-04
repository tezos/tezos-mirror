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
   Component:    Protocol's plugin
   Invocation:   dune exec tezt/tests/main.exe -- --file "run_operation_RPC.ml"
   Subject:      Test the [run_operation] RPC:
                 [POST /chains/<chain>/blocks/<block>/helpers/scripts/run_operation].
                 These tests focus on the semantics of the RPC, ie. whether
                 the operation is successfully run, rather than on the exact
                 form of the output, which is why they are in their own file
                 instead of [RPC_test.ml].
*)

(** Tags shared by all tests in this file. *)
let run_operation_tags = [Tag.layer1; "rpc"; "run_operation"]

(** Check that the RPC [response]'s code is [500] (Internal Server
    Error), and that its body has an "id" field that ends in
    [expected_proto_error]. *)
let check_response_contains_proto_error ~expected_proto_error
    (response : JSON.t RPC_core.response) =
  Log.info
    "Checking RPC response:\n  code: %s\n  body: %s"
    Cohttp.Code.(string_of_status (status_of_code response.code))
    (JSON.encode response.body) ;
  Check.(
    (response.code = 500)
      int
      ~error_msg:"Expected response code %R, but got %L.") ;
  let response_proto_error =
    try
      let id = JSON.(response.body |=> 0 |-> "id" |> as_string) in
      List.(hd (rev (String.split_on_char '.' id)))
    with exn ->
      Test.fail
        "Failed to parse the following RPC response body:\n\
         %s.\n\
         The following exception was raised:\n\
         %s"
        (JSON.encode response.body)
        (Printexc.to_string exn)
  in
  Check.(
    (response_proto_error = expected_proto_error)
      string
      ~error_msg:"Expected the %R protocol error, but got %L.")

(** Craft a batch that contains the given individual manager
    operation(s), call the [run_operation] RPC on it, and call
    {!check_response_contains_proto_error} and the RPC response. *)
let run_manager_operations_and_check_proto_error ~expected_proto_error
    (manager_operations : Operation_core.Manager.t list) node client =
  let* op = Operation.Manager.operation manager_operations client in
  let* op_json = Operation.make_run_operation_input op client in
  Log.debug
    "Crafted operation: %s"
    (Ezjsonm.value_to_string ~minify:false op_json) ;
  let* response =
    Node.RPC.call_json
      node
      (RPC.post_chain_block_helpers_scripts_run_operation (Data op_json))
  in
  check_response_contains_proto_error ~expected_proto_error response ;
  unit

(** This test checks that the [run_operation] RPC succeeds on a
    correct [proposals] operation.

    It only supports protocol versions from 015 on. Before that, the
    RPC crashed when called both on a non-manager operation and on a
    level 1 block.

    This test needs the initial voting period (right after protocol
    activation) to be a [proposal] period. If this changes in the
    future, this test will need to be reworked. *)
let test_run_proposals =
  Protocol.register_test
    ~__FILE__
    ~supports:(Protocol.From_protocol 015)
    ~title:"Run_operation proposals"
    ~tags:(run_operation_tags @ ["voting"; "proposals"])
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  Log.info
    "Retrieve the current voting period and check that it is of the [proposal] \
     kind." ;
  let* period = Voting.get_current_period client in
  Log.info "period: %a" Voting.pp_period period ;
  (match period.kind with
  | Proposal -> ()
  | Exploration | Cooldown | Promotion | Adoption ->
      Test.fail
        "This test needs the voting period at level 1 to be of kind \
         [proposal], but a period of kind [%s] was found."
        (Voting.period_kind_to_string period.kind)) ;
  Log.info "Craft a [proposals] operation:" ;
  let proposals_repr =
    Operation.Voting.proposals
      Constant.bootstrap1
      period.index
      [Protocol.demo_counter_hash]
  in
  let* op = Operation.Voting.operation ~client proposals_repr in
  let* op_json = Operation.make_run_operation_input op client in
  Log.info "%s" (Ezjsonm.value_to_string ~minify:false op_json) ;
  Log.info "Call the [run_operation] RPC on this operation." ;
  let* _output =
    Node.RPC.call
      node
      (RPC.post_chain_block_helpers_scripts_run_operation (Data op_json))
  in
  unit

(** This test checks that the [run_operation] RPC rejects batches of
    manager operations containing different sources. *)
let test_batch_inconsistent_sources =
  Protocol.register_test
    ~__FILE__
    ~title:"Run_operation inconsistent sources ko"
    ~tags:(run_operation_tags @ ["manager"; "batch"; "inconsistent_sources"])
    (fun protocol ->
      Log.info "Initialize a node and a client." ;
      let* node, client =
        Client.init_with_protocol
          ~nodes_args:[Synchronisation_threshold 0]
          ~protocol
          `Client
          ()
      in
      let source1 = Constant.bootstrap1
      and source2 = Constant.bootstrap2
      and dest = Constant.bootstrap3 in
      Log.info
        "Increment [%s]'s counter so that the batch we craft below has \
         consistent counters. To do this, we inject a transaction from this \
         account and bake a block."
        source2.alias ;
      let* () =
        Client.transfer
          ~amount:Tez.one
          ~giver:source2.alias
          ~receiver:dest.alias
          client
      in
      let* () = Client.bake_for_and_wait ~protocol ~node client in
      Log.info
        "Craft a batch containing an operation from [%s] and an operation from \
         [%s]."
        source1.alias
        source2.alias ;
      let manager_op1 =
        Operation.Manager.(make ~source:source1 (transfer ~dest ()))
      in
      let manager_op2 =
        Operation.Manager.(make ~source:source2 (transfer ~dest ()))
      in
      let* batch =
        Operation.Manager.operation [manager_op1; manager_op2] client
      in
      let* batch_json = Operation.make_run_operation_input batch client in
      Log.info
        "Crafted batch: %s"
        (Ezjsonm.value_to_string ~minify:false batch_json) ;
      let expected_proto_error = "inconsistent_sources" in
      Log.info
        "Call the [run_operation] RPC on this batch, and check that it fails \
         with code [500] (Internal Server Error) and protocol error [%s]."
        expected_proto_error ;
      let* response =
        Node.RPC.call_json
          node
          (RPC.post_chain_block_helpers_scripts_run_operation (Data batch_json))
      in
      check_response_contains_proto_error ~expected_proto_error response ;
      unit)

(** This test calls the [run_operation] RPC on various operations with
    unexpected or inconsistent counters, and checks that the
    appropriate protocol error is returned. *)
let test_inconsistent_counters =
  Protocol.register_test
    ~__FILE__
    ~title:"Run_operation inconsistent counters"
    ~tags:
      (run_operation_tags
      @ [
          "manager";
          "batch";
          "counter";
          "counter_in_the_past";
          "counter_in_the_future";
          "inconsistent_counters";
        ])
    (fun protocol ->
      Log.info "Initialize a node and a client." ;
      let* node, client =
        Client.init_with_protocol
          ~nodes_args:[Synchronisation_threshold 0]
          ~protocol
          `Client
          ()
      in
      let run_manager_operations_and_check_proto_error ~expected_proto_error
          manager_ops =
        run_manager_operations_and_check_proto_error
          ~expected_proto_error
          manager_ops
          node
          client
      in
      let source = Constant.bootstrap1 in
      let* next_counter = Operation.Manager.get_next_counter ~source client in
      Log.info
        "All the operations in this test will be from %s. The expected counter \
         for the next manager operation from this source is %d."
        source.alias
        next_counter ;
      let current_counter = next_counter - 1 in
      Log.info
        "Call [run_operation] on a transaction with counter %d."
        current_counter ;
      let* () =
        let transaction =
          Operation.Manager.(
            make ~source ~counter:current_counter (transfer ()))
        in
        run_manager_operations_and_check_proto_error
          ~expected_proto_error:"counter_in_the_past"
          [transaction]
      in
      let next_plus_one = next_counter + 1 in
      Log.info
        "Call [run_operation] on a transaction with counter %d."
        next_plus_one ;
      let* () =
        let transaction =
          Operation.Manager.(make ~source ~counter:next_plus_one (transfer ()))
        in
        run_manager_operations_and_check_proto_error
          ~expected_proto_error:"counter_in_the_future"
          [transaction]
      in
      Log.info
        "Call [run_operation] on a batch where the first operation has the \
         expected counter %d, but the second operation also has the same \
         counter %d."
        next_counter
        next_counter ;
      let transaction_next_counter =
        Operation.Manager.(
          make
            ~source
            ~counter:next_counter
            (transfer ~dest:Constant.bootstrap2 ()))
      in
      let* () =
        run_manager_operations_and_check_proto_error
          ~expected_proto_error:"inconsistent_counters"
          [transaction_next_counter; transaction_next_counter]
      in
      let next_plus_two = next_counter + 2 in
      Log.info
        "Call [run_operation] on a batch where the first operation has the \
         expected counter %d, but the second operation has the counter %d."
        next_counter
        next_plus_two ;
      let transaction2 =
        Operation.Manager.(
          make
            ~source
            ~counter:next_plus_two
            (transfer ~dest:Constant.bootstrap2 ()))
      in
      run_manager_operations_and_check_proto_error
        ~expected_proto_error:"inconsistent_counters"
        [transaction_next_counter; transaction2])

(** This test calls the [run_operation] RPC on various faulty
    revelations.

    This test only supports protocol versions from Kathmandu (014) on,
    because of changes to the revelation semantic introduced in this
    protocol. *)
let test_bad_revelations =
  Protocol.register_test
    ~__FILE__
    ~supports:(Protocol.From_protocol 014)
    ~title:"Run_operation bad revelations"
    ~tags:(run_operation_tags @ ["manager"; "reveal"; "bad_revelations"])
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  Log.info
    "Create a fresh account: generate a key, inject a transaction that funds \
     it, and bake a block to apply the transaction." ;
  let* fresh_account = Client.gen_and_show_keys client in
  let* _oph =
    Operation.inject_transfer
      client
      ~source:Constant.bootstrap2
      ~dest:fresh_account
      ~gas_limit:1500
      ~amount:10_000_000
  in
  let* () = Client.bake_for_and_wait ~node client in
  let* fresh_account_next_counter =
    Operation.Manager.get_next_counter ~source:fresh_account client
  in
  let incorrect_reveal_position_error = "incorrect_reveal_position" in
  Log.info
    "Call [run_operation] on a batch with a reveal in 2nd position, and check \
     that it returns the [%s] protocol error."
    incorrect_reveal_position_error ;
  let* () =
    let* op =
      let transaction_payload = Operation.Manager.transfer () in
      let reveal_payload = Operation.Manager.reveal fresh_account in
      Operation.Manager.(
        operation
          (make_batch
             ~source:fresh_account
             ~counter:fresh_account_next_counter
             [transaction_payload; reveal_payload])
          client)
    in
    let* op_json = Operation.make_run_operation_input op client in
    let* response =
      Node.RPC.(
        call_json
          node
          (post_chain_block_helpers_scripts_run_operation (Data op_json)))
    in
    check_response_contains_proto_error
      ~expected_proto_error:incorrect_reveal_position_error
      response ;
    unit
  in
  let inconsistent_hash_error = "inconsistent_hash" in
  Log.info
    "Call [run_operation] on a revelation of a public key that is not \
     consistent with the source's public key hash, and check that it returns \
     the [%s] protocol error."
    inconsistent_hash_error ;
  let* () =
    let reveal_manager_op =
      Operation.Manager.(
        make ~source:fresh_account (reveal Constant.bootstrap1))
    in
    let* op = Operation.Manager.operation [reveal_manager_op] client in
    let* op_json = Operation.make_run_operation_input op client in
    let* response =
      Node.RPC.(
        call_json
          node
          (post_chain_block_helpers_scripts_run_operation (Data op_json)))
    in
    check_response_contains_proto_error
      ~expected_proto_error:inconsistent_hash_error
      response ;
    unit
  in
  let previously_revealed_error = "previously_revealed_key" in
  Log.info
    "Call [run_operation] on a revelation of an already revealed key. Check \
     that the call succeeds, but the returned metadata indicate that the \
     operation's application has failed with the [%s] protocol error."
    previously_revealed_error ;
  let* () =
    let source = Constant.bootstrap1 (* this source is already revealed *) in
    let manager_op = Operation.Manager.(make ~source (reveal source)) in
    let* op = Operation.Manager.operation [manager_op] client in
    let* op_json = Operation.make_run_operation_input op client in
    let* output =
      Node.RPC.(
        call
          node
          (post_chain_block_helpers_scripts_run_operation (Data op_json)))
    in
    let operation_result =
      JSON.(output |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
    in
    Log.info
      "Checking metadata.operation_result: %s"
      (JSON.encode operation_result) ;
    Check.(
      (JSON.(operation_result |-> "status" |> as_string) = "failed")
        string
        ~error_msg:"Expected operation_result status to be %R, but got %L.") ;
    let id = JSON.(operation_result |-> "errors" |=> 0 |-> "id" |> as_string) in
    let proto_error =
      try List.(hd (rev (String.split_on_char '.' id)))
      with exn ->
        Test.fail
          "Failed to extract proto_error from %s:\n%s"
          id
          (Printexc.to_string exn)
    in
    Check.(
      (proto_error = previously_revealed_error)
        string
        ~error_msg:"Expected protocol error %R, but got %L.") ;
    unit
  in
  unit

(** This test checks that the [run_operation] RPC succeeds on a
    well-formed batch containing a transaction, a delegation, and a
    second transaction. *)
let test_correct_batch =
  Protocol.register_test
    ~__FILE__
    ~title:"Run_operation correct batch"
    ~tags:
      (run_operation_tags
      @ ["manager"; "batch"; "transaction"; "delegation"; "correct_batch"])
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  Log.info
    "Craft a batch containing: a transaction, a delegation, and a second \
     transaction." ;
  let* batch =
    let source = Constant.bootstrap1 in
    let* counter = Operation.Manager.get_next_counter ~source client in
    let transaction1_payload =
      Operation.Manager.transfer ~dest:Constant.bootstrap2 ()
    in
    let delegation_payload =
      Operation.Manager.delegation ~delegate:Constant.bootstrap3 ()
    in
    let transaction2_payload =
      Operation.Manager.transfer ~dest:Constant.bootstrap4 ()
    in
    Operation.Manager.(
      operation
        (make_batch
           ~source
           ~counter
           [transaction1_payload; delegation_payload; transaction2_payload])
        client)
  in
  let* batch_json = Operation.make_run_operation_input batch client in
  Log.info
    "Crafted batch: %s"
    (Ezjsonm.value_to_string ~minify:false batch_json) ;
  Log.info "Call the [run_operation] RPC on the batch." ;
  let* _output =
    Node.RPC.(
      call
        node
        (post_chain_block_helpers_scripts_run_operation (Data batch_json)))
  in
  unit

(** This test creates a fresh account and calls the [run_operation]
    RPC on the revelation of its public key. Then it actually injects
    this revelation, and calls [run_operation] on a some other manager
    operations from this fresh account. *)
let test_misc_manager_ops_from_fresh_account =
  Protocol.register_test
    ~__FILE__
    ~title:"Run_operation misc manager ops from fresh account"
    ~tags:
      (run_operation_tags
      @ ["fresh_account"; "manager"; "reveal"; "transaction"; "delegation"])
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  let amount = 10_000_000 (* amount of the final transaction (in mutez) *) in
  Log.info
    "Create a fresh account by giving it [2 x amount] mutez, then baking a \
     block to apply the transaction." ;
  let* fresh_account = Client.gen_and_show_keys client in
  let* _oph =
    Operation.inject_transfer
      client
      ~source:Constant.bootstrap2
      ~dest:fresh_account
      ~gas_limit:1500
      ~amount:(2 * amount)
  in
  let* () = Client.bake_for_and_wait ~node client in
  Log.info
    "Craft a revelation of the fresh account's key and call the \
     [run_operation] RPC on it." ;
  let* reveal_op =
    let manager_op =
      Operation.Manager.(make ~source:fresh_account (reveal fresh_account))
    in
    Operation.Manager.operation [manager_op] client
  in
  let* _run_operation_output =
    let* op_json = Operation.make_run_operation_input reveal_op client in
    Node.RPC.(
      call node (post_chain_block_helpers_scripts_run_operation (Data op_json)))
  in
  Log.info "Inject the crafted revelation and bake a block to apply it." ;
  let* _oph = Operation.inject reveal_op client in
  let* () = Client.bake_for_and_wait ~node client in
  Log.info
    "Craft a transaction (of [amount] mutez) from the fresh account and call \
     the [run_operation] RPC on it." ;
  let* () =
    let manager_op =
      Operation.Manager.(
        make
          ~source:fresh_account
          (transfer ~dest:Constant.bootstrap1 ~amount ()))
    in
    let* op = Operation.Manager.operation [manager_op] client in
    let* op_json = Operation.make_run_operation_input op client in
    let* _output =
      Node.RPC.(
        call
          node
          (post_chain_block_helpers_scripts_run_operation (Data op_json)))
    in
    unit
  in
  Log.info
    "Craft a delegation from the fresh account and call the [run_operation] \
     RPC on it." ;
  let* () =
    let manager_op =
      Operation.Manager.(
        make ~source:fresh_account (delegation ~delegate:Constant.bootstrap1 ()))
    in
    let* op = Operation.Manager.operation [manager_op] client in
    let* op_json = Operation.make_run_operation_input op client in
    let* _output =
      Node.RPC.(
        call
          node
          (post_chain_block_helpers_scripts_run_operation (Data op_json)))
    in
    unit
  in
  unit

let test_fail_inject_signed_arbitrary_operation =
  Protocol.register_test
    ~__FILE__
    ~title:"Test that failing_noop operations are never validated"
    ~tags:(run_operation_tags @ ["failing_noop"])
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* _node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in
  [
    ("bootstrap1", "msg1", None);
    ("bootstrap2", "msg2", None);
    ("bootstrap3", "msg3", Some "head");
    ("bootstrap4", "msg4", Some "head");
  ]
  |> Lwt_list.iter_s @@ fun (src, message, branch) ->
     let* signature = Client.sign_message client ?branch message ~src in
     let* chain_id = Client.RPC.call client @@ RPC.get_chain_chain_id () in
     let* head_hash = Client.RPC.call client @@ RPC.get_chain_block_hash () in
     let arbitrary = Hex.(of_string message |> show) in
     let (op_json : JSON.u) =
       `O
         [
           ( "operation",
             `O
               [
                 ("branch", `String head_hash);
                 ( "contents",
                   `A
                     [
                       `O
                         [
                           ("kind", `String "failing_noop");
                           ("arbitrary", `String arbitrary);
                         ];
                     ] );
                 ("signature", `String signature);
               ] );
           ("chain_id", `String chain_id);
         ]
     in
     let*? process =
       Client.RPC.spawn client
       @@ RPC.post_chain_block_helpers_scripts_run_operation (Data op_json)
     in
     let msg = rex "A failing_noop operation can never be validated" in
     let* () = Process.check_error ~msg process in
     unit

let register ~protocols =
  test_run_proposals protocols ;
  test_batch_inconsistent_sources protocols ;
  test_inconsistent_counters protocols ;
  test_bad_revelations protocols ;
  test_correct_batch protocols ;
  test_misc_manager_ops_from_fresh_account protocols ;
  test_fail_inject_signed_arbitrary_operation protocols
