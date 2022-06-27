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
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

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
    Sc_rollup_node.create tezos_node tezos_client ~operator_pkh:bootstrap1_key
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
      @@ fun node client ->
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
  let* protocol_parameters = RPC_legacy.get_constants client in
  let feature_flag =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "feature_enable" |> as_bool)
  in
  let number_of_slots =
    JSON.(
      protocol_parameters |-> "dal_parametric" |-> "number_of_slots" |> as_int)
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
        [make @@ dal_publish_slot_header ~index:0 ~level:1 ~header:0]
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
  unit

open Tezt_tezos.Rollup.Dal

let publish_slot ~source ?fee ~index node client =
  let level = Node.get_level node in
  let header = 0 in
  Operation.Manager.(
    inject
      [make ~source ?fee @@ dal_publish_slot_header ~index ~level ~header]
      client)

let slot_availability ~signer availability client =
  let default_size = 256 in
  let endorsement = Array.make default_size false in
  List.iter (fun i -> endorsement.(i) <- true) availability ;
  Operation.Consensus.(inject ~signer (slot_availability ~endorsement) client)

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

let test_slot_management_logic =
  Protocol.register_test
    ~__FILE__
    ~title:"dal basic logic"
    ~tags:["dal"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* (`OpHash oph1) =
    publish_slot ~source:Constant.bootstrap1 ~fee:1_000 ~index:0 node client
  in
  let* (`OpHash oph2) =
    publish_slot ~source:Constant.bootstrap2 ~fee:1_500 ~index:1 node client
  in
  let* (`OpHash oph3) =
    publish_slot ~source:Constant.bootstrap3 ~fee:2_000 ~index:0 node client
  in
  let* (`OpHash oph4) =
    publish_slot ~source:Constant.bootstrap4 ~fee:1_200 ~index:1 node client
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
  let* operations_result = RPC_legacy.get_operations client in
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
  let* bytes = RPC_legacy.raw_bytes client in
  if not JSON.(bytes |-> "dal" |> is_null) then
    Test.fail
      "Expected the context to contain no more information about the DAL \
       anymore." ;
  unit

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
  let* init_level =
    RPC.Client.call ~hooks client
    @@ RPC.Sc_rollup.get_initial_level sc_rollup_address
  in
  let init_level = init_level |> JSON.as_int in
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

let register ~protocols =
  test_dal_scenario "feature_flag_is_disabled" test_feature_flag protocols ;
  test_dal_scenario
    ~dal_enable:true
    "rollup_node_dal_subscriptions"
    rollup_node_subscribes_to_dal_slots
    protocols ;
  test_slot_management_logic protocols
