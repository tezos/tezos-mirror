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

let wait_for_root_hash_pushed_to_data_streamer dac_node root_hash =
  Dac_node.wait_for
    dac_node
    "root_hash_pushed_to_the_data_streamer.v0"
    (fun json -> if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_received_root_hash_processed dac_node root_hash =
  Dac_node.wait_for
    dac_node
    "dac_node_received_root_hash_processed.v0"
    (fun json -> if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_received_root_hash dac_node root_hash =
  Dac_node.wait_for dac_node "dac_node_new_root_hash_received.v0" (fun json ->
      if JSON.(json |> as_string) = root_hash then Some () else None)

let wait_for_handle_new_subscription_to_hash_streamer dac_node =
  Dac_node.wait_for
    dac_node
    "handle_new_subscription_to_hash_streamer.v0"
    (fun _ -> Some ())

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

(** This modules encapsulates tests where we have two dac nodes running in
    the legacy mode interacting with each other. As such one node normally tries
    to mimic the coordinator and the other tries to mimic signer or observer.
    Note that both nodes still run in the [legacy] mode, where as such there is
    no notion of profiles. Once we have a fully working profiles, tests from this
    module should be refactored. *)
module Legacy = struct
  let set_coordinator dac_node coordinator =
    let coordinator =
      `O
        [
          ("rpc-host", `String (Dac_node.rpc_host coordinator));
          ("rpc-port", `Float (float_of_int (Dac_node.rpc_port coordinator)));
        ]
    in
    let mode_updated =
      Dac_node.Config_file.read dac_node
      |> JSON.get "mode"
      |> JSON.put
           ( "dac_cctxt_config",
             JSON.annotate ~origin:"dac_node_config" coordinator )
    in
    Dac_node.Config_file.update dac_node (JSON.put ("mode", mode_updated))

  let coordinator_serializes_payload coordinator ~payload ~expected_rh =
    let* actual_rh, _l1_operation =
      RPC.call
        coordinator
        (Rollup.Dac.RPC.dac_store_preimage
           ~payload
           ~pagination_scheme:"Merkle_tree_V0")
    in
    return @@ check_valid_root_hash expected_rh actual_rh

  let test_streaming_of_root_hashes _protocol node client coordinator =
    (* 1. Create two new dac nodes; [observer_1] and [observer_2].
       2. Initialize their default configuration.
       3. Update their configuration so that their dac node client context
          points to [coordinator]. *)
    let observer_1 = Dac_node.create ~node ~client () in
    let observer_2 = Dac_node.create ~node ~client () in
    let* _ = Dac_node.init_config observer_1 in
    let* _ = Dac_node.init_config observer_2 in
    let () = set_coordinator observer_1 coordinator in
    let () = set_coordinator observer_2 coordinator in
    let payload_1 = "test_1" in
    let expected_rh_1 =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let payload_2 = "test_2" in
    let expected_rh_2 =
      "00f2f47f480fec0e4180930790e52a54b2dbd7676b5fa2a25dd93bf22969f22e33"
    in
    let push_promise_1 =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh_1
    in
    let push_promise_2 =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh_2
    in
    let observer_1_promise_1 =
      wait_for_received_root_hash observer_1 expected_rh_1
    in
    let observer_1_promise_2 =
      wait_for_received_root_hash observer_1 expected_rh_2
    in
    let observer_2_promise_1 =
      wait_for_received_root_hash observer_2 expected_rh_1
    in
    let observer_2_promise_2 =
      wait_for_received_root_hash observer_2 expected_rh_2
    in

    (* Start running [observer_1]. From now on we expect [observer_1] to
       monitor streamed root hashes produced by [coordinator]. [coordinator]
       produces and pushes them as a side effect of serializing dac payload. *)
    let observer_1_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let* () = Dac_node.run observer_1 in
    let* () = observer_1_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [observer_1]. *)
    let* () =
      coordinator_serializes_payload
        coordinator
        ~payload:payload_1
        ~expected_rh:expected_rh_1
    in
    (* Assert [coordinator] emitted event that [expected_rh_1] was pushed
       to the data_streamer. *)
    let* () = push_promise_1 in
    (* Assert [observer_1] emitted event of received [expected_rh_1]. *)
    let* () = observer_1_promise_1 in
    (* Start running [observer_2]. We expect that from now on [observer_2]
       will also monitor streamed root hashes from [coordinator]. *)
    let observer_2_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let* () = Dac_node.run observer_2 in
    let* () = observer_2_is_subscribed in
    (* [coordinator] serializes [payload_2]. We expect it would push
       [expected_rh_2] to all attached subscribers,
       i.e. to both [observer_1] and [observer_2] this time. *)
    let* () =
      coordinator_serializes_payload
        coordinator
        ~payload:payload_2
        ~expected_rh:expected_rh_2
    in
    (* Assert [coordinator] emitted event. *)
    let* () = push_promise_2 in
    (* Assert both [observer_1] and [observer_2] received [expected_rh_2]. *)
    let* () = observer_1_promise_2 in
    let* () = observer_2_promise_2 in
    (* Since [observer_2] was not running when [expected_rh_1] was generated
       and streamed by [coordinator], we expect it never received it.
       We assert this, by making sure that the promise of [observer_2] about
       waiting for the emitted event with payload [expected_rh_1] is still not
       resolved after the promise [observer_2_promise_2] has been resolved. *)
    assert (Lwt.is_sleeping observer_2_promise_1) ;
    unit

  (** [check_downloaded_page coordinator observer page_hash] checks that the
     [observer] has downloaded a page with [page_hash] from the [coordinator],
     that the contents of the page corresponds to the ones of the
     [coordinator]. It returns the list  of the hashes contained in the
     [page_hash], if the page corresponds to a hash page. Otherwise, it returns
     the empty list. *)
  let check_downloaded_page coordinator observer page_hash =
    let* coordinator_hex_encoded_page =
      RPC.call coordinator (Rollup.Dac.RPC.dac_retrieve_preimage page_hash)
    in
    let coordinator_page = Hex.to_string (`Hex coordinator_hex_encoded_page) in
    (* Check that the page has been saved by the observer. *)
    let* observer_hex_encoded_page =
      RPC.call observer (Rollup.Dac.RPC.dac_retrieve_preimage page_hash)
    in
    let observer_page = Hex.to_string (`Hex observer_hex_encoded_page) in
    (* Check that the raw page for the root hash  stored in the coordinator
       is the same as the raw page stored in the observer. *)
    Check.(
      (coordinator_page = observer_page)
        string
        ~error_msg:
          "Returned page does not match the expected one (Current: %L <> \
           Expected: %R)") ;
    let version_tag = observer_page.[0] in
    if version_tag = '\000' then return []
    else
      let hash_size = 33 in
      let preamble_size = 5 in
      let concatenated_hashes =
        String.sub observer_page 5 (String.length observer_page - preamble_size)
      in
      let rec split_hashes concatenated_hashes hashes =
        if String.equal concatenated_hashes "" then hashes
        else
          let next_hash =
            Hex.show @@ Hex.of_string
            @@ String.sub concatenated_hashes 0 hash_size
          in
          let next_concatenated_hashes =
            String.sub
              concatenated_hashes
              hash_size
              (String.length concatenated_hashes - hash_size)
          in
          split_hashes next_concatenated_hashes (next_hash :: hashes)
      in
      return @@ split_hashes concatenated_hashes []

  let check_downloaded_preimage coordinator observer root_hash =
    let rec go hashes =
      match hashes with
      | [] -> return ()
      | hash :: hashes ->
          let* next_hashes = check_downloaded_page coordinator observer hash in
          go (hashes @ next_hashes)
    in
    go [root_hash]

  let sample_payload example_filename =
    let json =
      JSON.parse_file @@ "tezt/tests/dac_example_payloads/" ^ example_filename
      ^ ".json"
    in
    let payload =
      JSON.(json |-> "payload" |> as_string |> fun s -> Hex.to_string (`Hex s))
    in
    let root_hash = JSON.(json |-> "root_hash" |> as_string) in
    (payload, root_hash)

  let test_observer_downloads_pages _protocol node client coordinator =
    (* 1. Create one new dac nodes; [observer_1],
       2. Initialize the default configuration,
       3. Specify a temporary directory within the test data for the observer
          reveal data dir,
       4. Update the configuration of the observer so that the dac node client
          context points to [coordinator]. *)
    let observer = Dac_node.create ~name:"observer" ~node ~client () in
    let* _ = Dac_node.init_config observer in
    let observer_data_dir = Dac_node.data_dir observer in
    let observer_reveal_data_dir =
      Filename.concat observer_data_dir "preimages"
    in
    let* () =
      Dac_node.Dac.set_parameters
        ~reveal_data_dir:observer_reveal_data_dir
        observer
    in
    let () = set_coordinator observer coordinator in
    (* Payload with more than 4091 bytes to check recursive calls of the
       committee member to the coordinator.
       The payload of this JSON file corresponds to the hex encoded version of
       the Inferno, Canto I, by Dante Alighieri. The original text is also used
       in the uit tests
       (see src/proto_alpha/lib_dac/test/test_dac_pages_encoding.ml). Because
       the unit test and the integration test use pages of different size,
       the final root hash obtained is different from the one in the unit
       tests. *)
    let payload, expected_rh = sample_payload "preimage" in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh
    in
    let wait_for_observer_subscribed_to_data_streamer =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let fetch_root_hash_promise =
      wait_for_received_root_hash_processed observer expected_rh
    in

    (* Test starts here *)

    (* Start running [observer_1]. From now on we expect [observer_1] to monitor
       streamed root hashes produced by [coordinator]. [coordinator] produces
       and pushes them as a side effect of serializing dac payload. *)
    let* () = Dac_node.run observer in
    let* () = wait_for_observer_subscribed_to_data_streamer in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [observer_1]. *)
    let* () =
      coordinator_serializes_payload coordinator ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* Assert [observer] emitted event of received [expected_rh]. *)
    let* () = fetch_root_hash_promise in
    check_downloaded_preimage coordinator observer expected_rh
end

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
    protocols ;
  scenario_with_layer1_and_dac_nodes
    ~tags:["dac"; "dac_node"]
    "dac_streaming_of_root_hashes_in_legacy_mode"
    Legacy.test_streaming_of_root_hashes
    protocols ;
  scenario_with_layer1_and_dac_nodes
    ~tags:["dac"; "dac_node"]
    "committee member downloads pages from coordinator"
    Legacy.test_observer_downloads_pages
    protocols
