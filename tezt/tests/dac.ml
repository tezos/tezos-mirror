(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
   Invocation:   dune exec tezt/tests/main.exe -- --file dac.ml
   Subject: Integration tests related to the data-availability layer
*)

let hooks = Tezos_regression.hooks

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3173
   The functions below are duplicated from sc_rollup.ml.
   They should be moved to a common submodule. *)
let make_int_parameter name = function
  | None -> []
  | Some value -> [(name, `Int value)]

let make_bool_parameter name = function
  | None -> []
  | Some value -> [(name, `Bool value)]

let test ~__FILE__ ?(tags = []) ?supports title f =
  let tags = "dac" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags ?supports f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "dac" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

(* Some initialization functions to start needed nodes. *)

let setup_node ?(additional_bootstrap_accounts = 5) ~parameters ~protocol
    ?(event_sections_levels = []) ?(node_arguments = []) () =
  (* Temporary setup to initialise the node. *)
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let* _client = Client.init_mockup ~parameter_file ~protocol () in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let node = Node.create nodes_args in
  let* () = Node.config_init node [] in
  let* () = Node.run node ~event_sections_levels node_arguments in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* additional_account_keys =
    Client.stresstest_gen_keys additional_bootstrap_accounts client
  in
  let additional_bootstrap_accounts =
    List.map (fun x -> (x, None, false)) additional_account_keys
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base
      parameters
  in
  let* () =
    Client.activate_protocol_and_wait ~parameter_file ~protocol client
  in
  return (node, client)

let with_layer1 ?additional_bootstrap_accounts ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments f ~protocol =
  let parameters =
    make_int_parameter
      ["smart_rollup_commitment_period_in_blocks"]
      commitment_period
    @ make_int_parameter
        ["smart_rollup_challenge_window_in_blocks"]
        challenge_window
    @ [(["smart_rollup_arith_pvm_enable"], `Bool true)]
  in
  let* node, client =
    setup_node
      ?additional_bootstrap_accounts
      ?event_sections_levels
      ?node_arguments
      ~parameters
      ~protocol
      ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

let with_fresh_rollup ~protocol ?(pvm_name = "arith") ?dac_node f tezos_node
    tezos_client bootstrap1_key =
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:pvm_name
      ~boot_sector:""
      ~parameters_ty:"string"
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node rollup_address
  in
  let* () =
    match dac_node with
    | None -> return ()
    | Some dac_node ->
        let* () = Dac_node.terminate dac_node in
        let reveal_data_dir =
          Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name
        in
        let* () = Dac_node.Dac.set_parameters ~reveal_data_dir dac_node in
        Dac_node.run dac_node ~wait_ready:true
  in
  let* () = Client.bake_for_and_wait tezos_client in
  f rollup_address sc_rollup_node configuration_filename

let with_dac_node tezos_node tezos_client f key =
  let dac_node = Dac_node.create ~node:tezos_node ~client:tezos_client () in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready:true in
  f key dac_node

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)
let scenario_with_layer1_node ?(tags = ["dac"; "layer1"]) ?commitment_period
    ?challenge_window ?event_sections_levels ?node_arguments variant scenario =
  let description = "Testing DAC L1 integration" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ~protocol
      @@ fun cryptobox node client -> scenario protocol cryptobox node client)

let scenario_with_layer1_and_dac_nodes ?(tags = ["dac"; "layer1"])
    ?commitment_period ?challenge_window variant scenario =
  let description = "Testing DAC node" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client ->
      with_dac_node node client @@ fun _key dac_node ->
      scenario protocol node client dac_node)

let scenario_with_all_nodes ?(tags = ["dac"; "dac_node"]) ?(pvm_name = "arith")
    ?commitment_period ?challenge_window variant scenario =
  let description = "Testing DAC rollup and node with L1" in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client ->
      with_dac_node node client @@ fun key dac_node ->
      ( with_fresh_rollup ~protocol ~pvm_name ~dac_node
      @@ fun sc_rollup_address sc_rollup_node _filename ->
        scenario
          protocol
          dac_node
          sc_rollup_node
          sc_rollup_address
          node
          client
          pvm_name )
        node
        client
        key)

let wait_for_layer1_block_processing dac_node level =
  Dac_node.wait_for dac_node "dac_node_layer_1_new_head.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

type status = Applied | Failed of {error_id : string}

let pp fmt = function
  | Applied -> Format.fprintf fmt "applied"
  | Failed {error_id} -> Format.fprintf fmt "failed: %s" error_id

let status_typ = Check.equalable pp ( = )

let test_dac_node_startup =
  Protocol.register_test
    ~__FILE__
    ~title:"dac node startup"
    ~tags:["dac"; "dac_node"]
  @@ fun protocol ->
  let run_dac = Dac_node.run ~wait_ready:false in
  let nodes_args = Node.[Synchronisation_threshold 0] in
  let previous_protocol =
    match Protocol.previous_protocol protocol with
    | Some p -> p
    | None -> assert false
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol:previous_protocol
      ~event_sections_levels:[("prevalidator", `Debug)]
      ~nodes_args
      ()
  in
  let () = Printf.printf "test was here 0." in
  let dac_node = Dac_node.create ~node ~client () in
  let* _dir = Dac_node.init_config dac_node in
  let* () = run_dac dac_node in
  let () = Printf.printf "test was here 1." in
  let* () =
    Dac_node.wait_for dac_node "dac_node_layer_1_start_tracking.v0" (fun _ ->
        Some ())
  in
  let () = Printf.printf "test was here 2." in
  assert (Dac_node.is_running_not_ready dac_node) ;
  let () = Printf.printf "test was here 3." in
  let* () = Dac_node.terminate dac_node in
  let () = Printf.printf "test was here 4." in
  let* () = Node.terminate node in
  Node.Config_file.update
    node
    (Node.Config_file.set_sandbox_network_with_user_activated_overrides
       [(Protocol.hash previous_protocol, Protocol.hash protocol)]) ;
  let* () = Node.run node nodes_args in
  let* () = Node.wait_for_ready node in
  let () = Printf.printf "test was here 5." in
  let* () = run_dac dac_node in
  let () = Printf.printf "test was here 6." in

  let* () =
    Lwt.join
      [
        Dac_node.wait_for dac_node "dac_node_plugin_resolved.v0" (fun _ ->
            Some ());
        Client.bake_for_and_wait client;
      ]
  in
  let () = Printf.printf "test was here 7." in
  let* () = Dac_node.terminate dac_node in
  let () = Printf.printf "test was here 8." in
  return ()

let send_messages ?(src = Constant.bootstrap2.alias) ?(alter_final_msg = Fun.id)
    client msgs =
  let msg =
    alter_final_msg
    @@ Ezjsonm.(to_string ~minify:true @@ list Ezjsonm.string msgs)
  in
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

let bake_levels n client = repeat n (fun () -> Client.bake_for_and_wait client)

let check_valid_root_hash expected_rh actual_rh =
  Check.(
    (actual_rh = expected_rh)
      string
      ~error_msg:"Invalid root hash returned (Current: %L <> Expected: %R)")

let check_preimage expected_preimage actual_preimage =
  Check.(
    (actual_preimage = expected_preimage)
      string
      ~error_msg:
        "Preimage does not match expected value (Current: %L <> Expected: %R)")

let test_dac_node_handles_dac_store_preimage_merkle_V0 _protocol dac_node
    sc_rollup_node _sc_rollup_address _node client pvm_name =
  (* Terminate the dac node before setting dac parameters. *)
  let* () = Dac_node.terminate dac_node in
  let* dac_member = Client.bls_gen_keys ~alias:"dac_member" client in
  let* dac_member_info = Client.bls_show_address ~alias:dac_member client in
  let dac_member_address = dac_member_info.aggregate_public_key_hash in
  let* () = Dac_node.Dac.set_parameters ~threshold:1 dac_node in
  let* () =
    Dac_node.Dac.add_committee_member ~address:dac_member_address dac_node
  in
  let* () = Dac_node.run dac_node in
  let payload = "test" in
  let* actual_rh, l1_operation =
    RPC.call
      dac_node
      (Rollup.Dac.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Merkle_tree_V0")
  in
  (* Expected reveal hash equals to the result of
     [Tezos_dac_alpha.Dac_pages_encoding.Merkle_tree.V0.serialize_payload "test"].
  *)
  let expected_rh =
    "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
  in
  check_valid_root_hash expected_rh actual_rh ;
  let filename =
    Filename.concat
      (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      actual_rh
  in
  let cin = open_in filename in
  let recovered_payload = really_input_string cin (in_channel_length cin) in
  let () = close_in cin in
  (* Discard first five preamble bytes *)
  let recovered_preimage =
    String.sub recovered_payload 5 (String.length recovered_payload - 5)
  in
  check_preimage payload recovered_preimage ;
  let* is_signature_valid =
    RPC.call dac_node (Rollup.Dac.RPC.dac_verify_signature l1_operation)
  in
  Check.(
    (is_signature_valid = true)
      bool
      ~error_msg:"Signature of external message is not valid") ;
  unit

let test_dac_node_handles_dac_store_preimage_hash_chain_V0 _protocol dac_node
    sc_rollup_node _sc_rollup_address _node _client pvm_name =
  let payload = "test" in
  let* actual_rh, _l1_operation =
    RPC.call
      dac_node
      (Rollup.Dac.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  (* Expected reveal hash equals to the result of
     [Tezos_dac_alpha.Dac_pages_encoding.Hash_chain.V0.serialize_payload "test"].
  *)
  let expected_rh =
    "00928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202"
  in
  check_valid_root_hash expected_rh actual_rh ;
  let filename =
    Filename.concat
      (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      actual_rh
  in
  let cin = open_in filename in
  let recovered_payload = really_input_string cin (in_channel_length cin) in
  let () = close_in cin in
  let recovered_preimage =
    String.sub recovered_payload 0 (String.length payload)
  in
  check_preimage payload recovered_preimage ;
  unit

let test_dac_node_handles_dac_retrieve_preimage_merkle_V0 _protocol dac_node
    sc_rollup_node _sc_rollup_address _node _client pvm_name =
  let payload = "test" in
  let* actual_rh, _l1_operation =
    RPC.call
      dac_node
      (Rollup.Dac.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Merkle_tree_V0")
  in
  (* Expected reveal hash equals to the result of
     [Tezos_dac_alpha.Dac_pages_encoding.Merkle_tree.V0.serialize_payload "test"].
  *)
  let expected_rh =
    "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"
  in
  check_valid_root_hash expected_rh actual_rh ;
  let filename =
    Filename.concat
      (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      actual_rh
  in
  let cin = open_in filename in
  let recovered_payload = really_input_string cin (in_channel_length cin) in
  let () = close_in cin in
  let recovered_preimage = Hex.of_string recovered_payload in
  let* preimage =
    RPC.call dac_node (Rollup.Dac.RPC.dac_retrieve_preimage expected_rh)
  in
  Check.(
    (preimage = Hex.show recovered_preimage)
      string
      ~error_msg:
        "Returned page does not match the expected one (Current: %L <> \
         Expected: %R)") ;
  unit

let test_rollup_arith_uses_reveals protocol dac_node sc_rollup_node
    sc_rollup_address _node client _pvm_name =
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node [] in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
  in
  let nadd = 32 * 1024 in
  let payload =
    let rec aux b n =
      if n > 0 then (
        Buffer.add_string b "1 +" ;
        (aux [@tailcall]) b (n - 1))
      else (
        Buffer.add_string b "value" ;
        String.of_bytes (Buffer.to_bytes b))
    in
    let buf = Buffer.create ((nadd * 3) + 2) in
    Buffer.add_string buf "0 " ;
    aux buf nadd
  in
  let* actual_rh, _l1_operation =
    RPC.call
      dac_node
      (Rollup.Dac.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  let expected_rh =
    "0027782d2a7020be332cc42c4e66592ec50305f559a4011981f1d5af81428e7aa3"
  in
  check_valid_root_hash expected_rh actual_rh ;
  let* () =
    send_messages
      client
      ["hash:" ^ actual_rh]
      ~alter_final_msg:(fun s -> "text:" ^ s)
  in
  let* () = bake_levels 2 client in
  let* _ =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 2)
  in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  let*! encoded_value =
    Sc_rollup_client.state_value ~hooks sc_rollup_client ~key:"vars/value"
  in
  let value =
    match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
    | Error error ->
        failwith
          (Format.asprintf
             "The arithmetic PVM has an unexpected state: %a"
             Data_encoding.Binary.pp_read_error
             error)
    | Ok x -> x
  in
  Check.(
    (value = nadd) int ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
  unit

let test_reveals_fails_on_wrong_hash _protocol dac_node sc_rollup_node
    sc_rollup_address _node client _pvm_name =
  let payload = "Some data that is not related to the hash" in
  let _actual_rh =
    RPC.call
      dac_node
      (Rollup.Dac.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  let errorneous_hash =
    "0027782d2a7020be332cc42c4e66592ec50305f559a4011981f1d5af81428ecafe"
  in
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node [] in
  (* Prepare the handler to wait for the rollup node to fail before
     sending the L1 message that will trigger the failure. This
     ensures that the failure handler can access the status code
     of the rollup node even after it has terminated. *)
  let expect_failure =
    let node_process = Option.get @@ Sc_rollup_node.process sc_rollup_node in
    Process.check_error
      ~exit_code:1
      ~msg:(rex "Could not open file containing preimage of reveal hash")
      node_process
  in
  let* _level =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
  in
  let* () =
    send_messages
      client
      ["hash:" ^ errorneous_hash]
      ~alter_final_msg:(fun s -> "text:" ^ s)
  in
  expect_failure

let test_dac_node_imports_dac_member =
  Protocol.register_test
    ~__FILE__
    ~title:"dac node imports dac members sk_uris"
    ~tags:["dac"; "dac_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let run_dac = Dac_node.run ~wait_ready:false in
  let* dac_member = Client.bls_gen_keys ~alias:"dac_member" client in
  let* dac_member_info = Client.bls_show_address ~alias:dac_member client in
  let dac_member_address = dac_member_info.aggregate_public_key_hash in
  let dac_node = Dac_node.create ~node ~client () in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.Dac.set_parameters ~threshold:1 dac_node in
  let* () =
    Dac_node.Dac.add_committee_member ~address:dac_member_address dac_node
  in
  let ready_promise =
    Dac_node.wait_for dac_node "dac_is_ready.v0" (fun _ -> Some ())
  in
  let* () = run_dac dac_node in
  let* () = ready_promise in
  let* () = Dac_node.terminate dac_node in
  unit

let test_dac_node_dac_threshold_not_reached =
  Protocol.register_test
    ~__FILE__
    ~title:"dac node displays warning if dac threshold is not reached"
    ~tags:["dac"; "dac_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let run_dac = Dac_node.run ~wait_ready:false in
  let dac_node = Dac_node.create ~node ~client () in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.Dac.set_parameters ~threshold:1 dac_node in
  let error_promise =
    Dac_node.wait_for dac_node "dac_threshold_not_reached.v0" (fun _ -> Some ())
  in
  let* () = run_dac dac_node in
  let* () = error_promise in
  Dac_node.terminate dac_node

let register ~protocols =
  (* Tests with layer1 and dac nodes *)
  test_dac_node_startup protocols ;
  test_dac_node_imports_dac_member protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_merkle_tree_v0"
    test_dac_node_handles_dac_store_preimage_merkle_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_hash_chain_v0"
    test_dac_node_handles_dac_store_preimage_hash_chain_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_retrieve_preimage"
    test_dac_node_handles_dac_retrieve_preimage_merkle_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_uses_reveals"
    test_rollup_arith_uses_reveals
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_wrong_hash"
    test_reveals_fails_on_wrong_hash
    protocols
