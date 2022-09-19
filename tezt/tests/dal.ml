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
   Component:    Data-availability layer
   Invocation:   dune exec tezt/tests/main.exe -- --file dal.ml
   Subject: Integration tests related to the data-availability layer
*)

let hooks = Tezos_regression.hooks

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3173
   The functions below are duplicated from sc_rollup.ml.
   They should be moved to a common submodule. *)
let make_int_parameter name value =
  Option.map (fun v -> (name, Option.some @@ Int.to_string v)) value
  |> Option.to_list

let make_bool_parameter name value =
  Option.map (fun v -> (name, Option.some @@ Bool.to_string v)) value
  |> Option.to_list

let test ~__FILE__ ?(tags = []) title f =
  let tags = "dal" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "dal" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

let setup ?commitment_period ?challenge_window ?dal_enable f ~protocol =
  let parameters =
    make_int_parameter
      ["sc_rollup_commitment_period_in_blocks"]
      commitment_period
    @ make_int_parameter
        ["sc_rollup_challenge_window_in_blocks"]
        challenge_window
    (* this will produce the empty list if dal_enable is not passed to the function invocation,
       hence the value from the protocol constants will be used. *)
    @ make_bool_parameter ["dal_parametric"; "feature_enable"] dal_enable
    @ [(["sc_rollup_enable"], Some "true")]
  in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let* client = Client.init_mockup ~parameter_file ~protocol () in
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let cryptobox = Rollup.Dal.make parameters in
  let node = Node.create nodes_args in
  let* () = Node.config_init node [] in
  Node.Config_file.update node (fun json ->
      let value =
        JSON.annotate
          ~origin:"dal_initialisation"
          (`O
            [
              ("srs_size", `Float (float_of_int parameters.slot_size));
              ("activated", `Bool true);
            ])
      in
      let json = JSON.put ("dal", value) json in
      json) ;
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol client
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f parameters cryptobox node client bootstrap1_key

type test = {variant : string; tags : string list; description : string}

let with_fresh_rollup f tezos_node tezos_client bootstrap1_key =
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"arith"
      ~boot_sector:""
      ~parameters_ty:"unit"
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      tezos_node
      tezos_client
      ~default_operator:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node rollup_address
  in
  let* () = Client.bake_for tezos_client in
  f rollup_address sc_rollup_node configuration_filename

let test_scenario ?commitment_period ?challenge_window ?dal_enable
    {variant; tags; description} scenario =
  let tags = tags @ [variant] in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      setup ?commitment_period ?challenge_window ~protocol ?dal_enable
      @@ fun _parameters _cryptobox node client ->
      ( with_fresh_rollup @@ fun sc_rollup_address sc_rollup_node _filename ->
        scenario protocol sc_rollup_node sc_rollup_address node client )
        node
        client)

let test_dal_scenario ?dal_enable variant =
  test_scenario
    ?dal_enable
    {
      tags = ["dal"];
      variant;
      description = "Testing data availability layer functionality ";
    }

let subscribe_to_dal_slot client ~sc_rollup_address ~slot_index =
  let* op_hash =
    Operation.Manager.(
      inject
        ~force:true
        [
          make
          @@ sc_rollup_dal_slot_subscribe ~rollup:sc_rollup_address ~slot_index;
        ]
        client)
  in
  let* () = Client.bake_for_and_wait client in
  return op_hash

let test_feature_flag _protocol _sc_rollup_node sc_rollup_address node client =
  (* This test ensures the feature flag works:

     - 1. It checks the feature flag is not enabled by default

     - 2. It checks the new operations added by the feature flag
     cannot be propagated by checking their classification in the
     mempool. *)
  let* protocol_parameters =
    RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
  in
  let feature_flag =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "feature_enable" |> as_bool)
  in
  let number_of_slots =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "number_of_slots" |> as_int)
  in
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let cryptobox = Rollup.Dal.make parameters in
  let header =
    Rollup.Dal.Commitment.dummy_commitment parameters cryptobox "coucou"
  in
  Check.(
    (feature_flag = false)
      bool
      ~error_msg:"Feature flag for the DAL should be disabled") ;
  let* (`OpHash oph1) =
    Operation.Consensus.(
      inject
        ~force:true
        ~signer:Constant.bootstrap1
        (slot_availability ~endorsement:(Array.make number_of_slots false))
        client)
  in
  let* (`OpHash oph2) =
    Operation.Manager.(
      inject
        ~force:true
        [make @@ dal_publish_slot_header ~index:0 ~level:1 ~header]
        client)
  in
  let* (`OpHash oph3) =
    subscribe_to_dal_slot client ~sc_rollup_address ~slot_index:0
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool = Mempool.{empty with refused = [oph1; oph2; oph3]} in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool: %R. Got: %L. (Order does not matter)") ;
  let* () = Client.bake_for_and_wait client in
  let* block_metadata = RPC.(call node @@ get_chain_block_metadata ()) in
  if block_metadata.dal_slot_availability <> None then
    Test.fail "Did not expect to find \"dal_slot_availibility\"" ;
  let* bytes = RPC_legacy.raw_bytes client in
  if not JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Unexpected entry dal in the context when DAL is disabled" ;
  unit

let publish_slot ~source ?fee ~index ~message parameters cryptobox node client =
  let level = Node.get_level node in
  let header =
    Rollup.Dal.Commitment.dummy_commitment parameters cryptobox message
  in
  Operation.Manager.(
    inject
      [make ~source ?fee @@ dal_publish_slot_header ~index ~level ~header]
      client)

let slot_availability ~signer availability client =
  (* FIXME/DAL: fetch the constant from protocol parameters. *)
  let default_size = 256 in
  let endorsement = Array.make default_size false in
  List.iter (fun i -> endorsement.(i) <- true) availability ;
  Operation.Consensus.(inject ~signer (slot_availability ~endorsement) client)

let header_of_slot_metadata {Sc_rollup_client.header; _} = header

type status = Applied | Failed of {error_id : string}

let pp fmt = function
  | Applied -> Format.fprintf fmt "applied"
  | Failed {error_id} -> Format.fprintf fmt "failed: %s" error_id

let status_typ = Check.equalable pp ( = )

let check_manager_operation_status result expected_status oph =
  let manager_operations = JSON.(result |=> 3 |> as_list) in
  let op =
    try
      List.find
        (fun op -> JSON.(op |-> "hash" |> as_string) = oph)
        manager_operations
    with Not_found ->
      Test.fail
        "Test expecting operation %s to be included into the last block."
        oph
  in
  let op_result =
    JSON.(op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let status_kind = JSON.(op_result |-> "status" |> as_string) in
  let status =
    match status_kind with
    | "applied" -> Applied
    | "failed" ->
        let error_id =
          JSON.(op_result |-> "errors" |=> 0 |-> "id" |> as_string)
        in
        Failed {error_id}
    | s -> Test.fail "Unexpected status: %s" s
  in
  let prefix_msg = sf "Unexpected operation result for %s." oph in
  Check.(expected_status = status)
    status_typ
    ~error_msg:(prefix_msg ^ " Expected: %L. Got: %R.")

let check_dal_raw_context node =
  let* dal_raw_json =
    RPC.call node @@ RPC.get_chain_block_context_raw_json ~path:["dal"] ()
  in
  if JSON.is_null dal_raw_json then
    Test.fail "Expected the context to contain information under /dal key."
  else
    let json_to_string j =
      JSON.unannotate j |> Ezjsonm.wrap |> Ezjsonm.to_string
    in
    let* confirmed_slots_opt =
      RPC.call node (RPC.get_chain_block_context_dal_confirmed_slots_history ())
    in
    if JSON.is_null confirmed_slots_opt then
      Test.fail
        "confirmed_slots_history RPC is not expected to return None if DAL is \
         enabled" ;
    let confirmed_slots = json_to_string confirmed_slots_opt in
    let confirmed_slots_from_ctxt =
      json_to_string @@ JSON.(dal_raw_json |-> "slots_history")
    in
    if not (String.equal confirmed_slots confirmed_slots_from_ctxt) then
      Test.fail "Confirmed slots history mismatch." ;
    unit

let test_slot_management_logic =
  Protocol.register_test
    ~__FILE__
    ~title:"dal basic logic"
    ~tags:["dal"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  setup ~dal_enable:true ~protocol
  @@ fun parameters cryptobox node client _bootstrap ->
  let* (`OpHash oph1) =
    publish_slot
      ~source:Constant.bootstrap1
      ~fee:1_000
      ~index:0
      ~message:"a"
      parameters
      cryptobox
      node
      client
  in
  let* (`OpHash oph2) =
    publish_slot
      ~source:Constant.bootstrap2
      ~fee:1_500
      ~index:1
      ~message:"b"
      parameters
      cryptobox
      node
      client
  in
  let* (`OpHash oph3) =
    publish_slot
      ~source:Constant.bootstrap3
      ~fee:2_000
      ~index:0
      ~message:"c"
      parameters
      cryptobox
      node
      client
  in
  let* (`OpHash oph4) =
    publish_slot
      ~source:Constant.bootstrap4
      ~fee:1_200
      ~index:1
      ~message:"d"
      parameters
      cryptobox
      node
      client
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool =
    Mempool.{empty with applied = [oph1; oph2; oph3; oph4]}
  in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected all the operations to be applied. Got %L") ;
  let* () = Client.bake_for_and_wait client in
  let* bytes = RPC_legacy.raw_bytes client in
  if JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Expected the context to contain some information about the DAL" ;
  let* operations_result =
    RPC.Client.call client @@ RPC.get_chain_block_operations ()
  in
  let fees_error =
    Failed {error_id = "proto.alpha.dal_publish_slot_heade_duplicate"}
  in
  (* The baker sorts operations fee wise. Consequently order of
     application for the operations will be: oph3 > oph2 > oph4 > oph1

     For slot 0, oph3 is applied first.

     Flor slot1, oph2 is applied first. *)
  check_manager_operation_status operations_result fees_error oph1 ;
  check_manager_operation_status operations_result fees_error oph4 ;
  check_manager_operation_status operations_result Applied oph3 ;
  check_manager_operation_status operations_result Applied oph2 ;
  let* _ = slot_availability ~signer:Constant.bootstrap1 [1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap2 [1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap3 [1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap4 [1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap5 [1] client in
  let* () = Client.bake_for_and_wait client in
  let* metadata = RPC.call node (RPC.get_chain_block_metadata ()) in
  let dal_slot_availability =
    match metadata.dal_slot_availability with
    | None ->
        assert false
        (* Field is part of the encoding when the feature flag is true *)
    | Some x -> x
  in
  Check.(
    (dal_slot_availability.(0) = false)
      bool
      ~error_msg:"Expected slot 0 to be unavailable") ;
  Check.(
    (dal_slot_availability.(1) = true)
      bool
      ~error_msg:"Expected slot 1 to be available") ;
  check_dal_raw_context node

(* Tests for integration between Dal and Scoru *)
let rollup_node_subscribes_to_dal_slots _protocol sc_rollup_node
    sc_rollup_address _node client =
  (* Steps in this integration test:

     1. Run rollup node for an originated rollup
     2. Fetch the list of subscribed slots, determine that it's empty
     3. Execute a client command to subscribe the rollup to dal slot 0, bake one level
     4. Fetch the list of subscribed slots, determine that it contains slot 0
     5. Execute a client command to subscribe the rollup to dal slot 1, bake one level
     6. Fetch the list of subscribed slots, determine that it contains slots 0 and 1
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* subscribed_slots =
    Sc_rollup_client.dal_slot_subscriptions ~hooks sc_rollup_client
  in
  Check.(subscribed_slots = [])
    (Check.list Check.int)
    ~error_msg:"Unexpected list of slot subscriptions (%L = %R)" ;
  let* (`OpHash _) =
    subscribe_to_dal_slot client ~sc_rollup_address ~slot_index:0
  in
  let* first_subscription_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + 1)
  in
  let* subscribed_slots =
    Sc_rollup_client.dal_slot_subscriptions ~hooks sc_rollup_client
  in
  Check.(subscribed_slots = [0])
    (Check.list Check.int)
    ~error_msg:"Unexpected list of slot subscriptions (%L = %R)" ;
  let* (`OpHash _) =
    subscribe_to_dal_slot client ~sc_rollup_address ~slot_index:1
  in
  let* _second_subscription_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (first_subscription_level + 1)
  in
  let* subscribed_slots =
    Sc_rollup_client.dal_slot_subscriptions ~hooks sc_rollup_client
  in
  Check.(subscribed_slots = [0; 1])
    (Check.list Check.int)
    ~error_msg:"Unexpected list of slot subscriptions (%L = %R)" ;
  return ()

let test_dal_node_slot_management =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node slot management"
    ~tags:["dal"; "dal_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, _client =
    let nodes_args = Node.[Synchronisation_threshold 0] in
    Client.init_with_protocol `Client ~protocol ~nodes_args ()
  in
  let dal_node = Dal_node.create ~node () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = Dal_node.run dal_node in
  let slot_content = "test" in
  let* slot_header =
    RPC.call dal_node (Rollup.Dal.RPC.split_slot slot_content)
  in
  let* received_slot_content =
    RPC.call dal_node (Rollup.Dal.RPC.slot_content slot_header)
  in
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3804
     Only check that the function to retrieve pages succeeds, actual
     contents will be checked in a test upcoming. *)
  let* _slots_as_pages =
    RPC.call dal_node (Rollup.Dal.RPC.slot_pages slot_header)
  in
  assert (slot_content = received_slot_content) ;
  return ()

let test_dal_node_startup =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node startup"
    ~tags:["dal"; "dal_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let run_dal = Dal_node.run ~wait_ready:false in
  let nodes_args = Node.[Synchronisation_threshold 0] in
  let previous_protocol =
    match Protocol.previous_protocol protocol with
    | Some p -> p
    | None -> assert false
  in
  let* node, client =
    Client.init_with_protocol `Client ~protocol:previous_protocol ~nodes_args ()
  in
  let dal_node = Dal_node.create ~node () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = run_dal dal_node in
  let* () =
    Dal_node.wait_for dal_node "dal_node_layer_1_start_tracking.v0" (fun _ ->
        Some ())
  in
  assert (Dal_node.is_running_not_ready dal_node) ;
  let* () = Dal_node.terminate dal_node in
  let* () = Node.terminate node in
  Node.Config_file.update
    node
    (Node.Config_file.set_sandbox_network_with_user_activated_overrides
       [(Protocol.hash previous_protocol, Protocol.hash Alpha)]) ;
  let* () = Node.run node nodes_args in
  let* () = Node.wait_for_ready node in
  let* () = run_dal dal_node in
  let* () =
    Lwt.join
      [
        Dal_node.wait_for dal_node "dal_node_plugin_resolved.v0" (fun _ ->
            Some ());
        Client.bake_for_and_wait client;
      ]
  in
  let* () = Dal_node.terminate dal_node in
  return ()

let rollup_node_stores_dal_slots _protocol sc_rollup_node sc_rollup_address node
    client =
  (* Check that the rollup node stores the slots published in a block,
     along with slot headers:

     1. Run rollup node for an originated rollup
     2. Subscribe rollup node to slot 0, bake one level
     3. Subscribe rollup node to slot 1, bake one level
     4. Execute a client command to publish a slot header for slot 0,
        bake one level
     5. Execute a client command to publish a slot header for slot 1,
        bake one level
     4. Get the list of slot headers subscribed and confirmed for the
        rollup node
     5. Determine that the list contains only the header for slot 1.
  *)
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let cryptobox = Rollup.Dal.make parameters in
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollup_genesis_info sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* (`OpHash _) =
    subscribe_to_dal_slot client ~sc_rollup_address ~slot_index:0
  in
  let* first_subscription_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + 1)
  in
  let* (`OpHash _) =
    subscribe_to_dal_slot client ~sc_rollup_address ~slot_index:1
  in
  let* second_subscription_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (first_subscription_level + 1)
  in
  let* _ =
    publish_slot
      ~source:Constant.bootstrap1
      ~fee:1_200
      ~index:0
      ~message:"CAFEBABE"
      parameters
      cryptobox
      node
      client
  in
  let* _ =
    publish_slot
      ~source:Constant.bootstrap2
      ~fee:1_200
      ~index:1
      ~message:"CAFEDEAD"
      parameters
      cryptobox
      node
      client
  in
  let* _ =
    publish_slot
      ~source:Constant.bootstrap3
      ~fee:1_200
      ~index:2
      ~message:"C0FFEE"
      parameters
      cryptobox
      node
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* slots_published_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (second_subscription_level + 1)
  in
  let* slots_metadata =
    Sc_rollup_client.dal_slots_metadata ~hooks sc_rollup_client
  in
  let slot_headers = slots_metadata |> List.map header_of_slot_metadata in
  let expected_slot_headers =
    List.map
      (fun msg ->
        Tezos_crypto_dal.Cryptobox.Commitment.to_b58check
        @@ Rollup.Dal.Commitment.dummy_commitment parameters cryptobox msg)
      ["CAFEBABE"; "CAFEDEAD"; "C0FFEE"]
  in
  Check.(slot_headers = expected_slot_headers)
    (Check.list Check.string)
    ~error_msg:"Unexpected list of slot headers (%L = %R)" ;
  (* Endorse only slots 1 and 2. *)
  let* _ = slot_availability ~signer:Constant.bootstrap1 [2; 1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap2 [2; 1; 0] client in
  let* _ = slot_availability ~signer:Constant.bootstrap3 [2; 1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap4 [2; 1] client in
  let* _ = slot_availability ~signer:Constant.bootstrap5 [2; 1] client in
  let* () = Client.bake_for_and_wait client in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (slots_published_level + 1)
  in
  Check.(level = slots_published_level + 1)
    Check.int
    ~error_msg:"Current level has moved past slot endorsement level (%L = %R)" ;
  let* confirmed_slots_metadata =
    Sc_rollup_client.dal_confirmed_slots_metadata ~hooks sc_rollup_client
  in
  let confirmed_slot_headers =
    confirmed_slots_metadata |> List.map header_of_slot_metadata
  in
  (* Slot 0 was subscribed to, but not confirmed.
     Slot 1 was subscribed to and confirmed.
     Slot 2 was confirmed, but not subscribed to.
     Only the slot header for slot 1 will appear in the list
     of confirmed slots for the rollup. *)
  let expected_confirmed_slot_headers = [List.nth expected_slot_headers 1] in
  Check.(confirmed_slot_headers = expected_confirmed_slot_headers)
    (Check.list Check.string)
    ~error_msg:"Unexpected list of slot headers (%L = %R)" ;
  return ()

let register ~protocols =
  test_dal_scenario "feature_flag_is_disabled" test_feature_flag protocols ;
  test_dal_scenario
    ~dal_enable:true
    "rollup_node_dal_subscriptions"
    rollup_node_subscribes_to_dal_slots
    protocols ;
  test_slot_management_logic protocols ;
  test_dal_node_slot_management protocols ;
  test_dal_node_startup protocols ;
  test_dal_scenario
    ~dal_enable:true
    "rollup_node_dal_headers_storage"
    rollup_node_stores_dal_slots
    protocols
