(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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

let assert_lwt_failure ?__LOC__ msg lwt_under_inspection =
  let* passed =
    Lwt.catch
      (fun () -> Lwt.map (fun _a -> false) lwt_under_inspection)
      (fun _exn -> return true)
  in
  if passed then unit else Test.fail ?__LOC__ msg

let init_hex_root_hash ?payload coordinator_node =
  let payload = Option.value payload ~default:"hello test message" in
  let* root_hash, _l1_op =
    RPC.call
      coordinator_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Merkle_tree_V0")
  in
  let hex_root_hash = `Hex root_hash in
  return hex_root_hash

let assert_verify_aggregate_signature members_keys hex_root_hash agg_sig_b58 =
  let verified =
    let root_hash = Hex.to_bytes hex_root_hash in
    let data =
      List.map
        (fun (member : Account.aggregate_key) ->
          let pk =
            Tezos_crypto.Aggregate_signature.Public_key.of_b58check_exn
              member.aggregate_public_key
          in
          (pk, None, root_hash))
        members_keys
    in
    Tezos_crypto.Aggregate_signature.aggregate_check
      data
      (Tezos_crypto.Aggregate_signature.of_b58check_exn agg_sig_b58)
  in
  Check.(
    (true = verified)
      ~__LOC__
      bool
      ~error_msg:"Failed to verify aggregate signature.")

let assert_witnesses ~__LOC__ expected witnesses =
  Check.(
    (expected = witnesses)
      ~__LOC__
      int
      ~error_msg:"Expected witnesses bitset to be %L. Found: %R")

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

let with_legacy_dac_node tezos_node ?sc_rollup_node ?(pvm_name = "arith")
    ?(wait_ready = true) ~threshold ?committee_member_address ~committee_members
    tezos_client f =
  let range i = List.init i Fun.id in
  let reveal_data_dir =
    Option.map
      (fun sc_rollup_node ->
        Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
      sc_rollup_node
  in
  let* committee_members =
    List.fold_left
      (fun keys i ->
        let* keys in
        let* key =
          Client.bls_gen_and_show_keys
            ~alias:(Format.sprintf "dac-member-%d" i)
            tezos_client
        in
        return (key :: keys))
      (return [])
      (range committee_members)
  in
  let dac_node =
    Dac_node.create_legacy
      ~node:tezos_node
      ~client:tezos_client
      ?reveal_data_dir
      ~threshold
      ?committee_member_address
      ~committee_members:
        (List.map
           (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
           committee_members)
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = Dac_node.run dac_node ~wait_ready in
  f dac_node committee_members

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4706
   Keep pvm name value in Sc_rollup.t. *)
let with_fresh_rollup ~protocol ?(pvm_name = "arith") f tezos_node tezos_client
    bootstrap1_key =
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
  let* () = Client.bake_for_and_wait tezos_client in
  f rollup_address sc_rollup_node configuration_filename

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
      @@ fun node client key -> scenario protocol node client key)

let scenario_with_layer1_and_legacy_dac_nodes
    ?(tags = ["dac"; "layer1"; "legacy"]) ?commitment_period ?challenge_window
    ~threshold ~committee_members variant scenario =
  let description = "Testing DAC node" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client _key ->
      with_legacy_dac_node ~threshold ~committee_members node client
      @@ fun dac_node committee_members ->
      scenario protocol node client dac_node threshold committee_members)

let scenario_with_all_nodes ?(tags = ["dac"; "dac_node"; "legacy"])
    ?(pvm_name = "arith") ?commitment_period ?challenge_window ~threshold
    ?committee_member_address ~committee_members variant scenario =
  let description = "Testing DAC rollup and node with L1" in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol
      @@ fun node client key ->
      with_fresh_rollup
        ~protocol
        ~pvm_name
        (fun sc_rollup_address sc_rollup_node _configuration_filename ->
          with_legacy_dac_node
            node
            ~sc_rollup_node
            ~pvm_name
            ~threshold
            ~committee_members
            ?committee_member_address
            client
          @@ fun dac_node committee_members ->
          scenario
            protocol
            dac_node
            sc_rollup_node
            sc_rollup_address
            node
            client
            pvm_name
            threshold
            committee_members)
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

let wait_for_signature_pushed_to_coordinator dac_node signature =
  Dac_node.wait_for
    dac_node
    "new_signature_pushed_to_coordinator.v0"
    (fun json -> if JSON.(json |> as_string) = signature then Some () else None)

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

let bls_sign_hex_hash (signer : Account.aggregate_key) hex_root_hash =
  let sk =
    match signer.aggregate_secret_key with
    | Unencrypted sk -> sk
    | Encrypted encsk -> raise (Invalid_argument encsk)
  in
  let bytes_root_hash = Hex.to_bytes hex_root_hash in
  let sk = Tezos_crypto.Aggregate_signature.Secret_key.of_b58check_exn sk in
  Tezos_crypto.Aggregate_signature.sign sk bytes_root_hash

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
  let dac_node =
    Dac_node.create_legacy ~node ~client ~threshold:0 ~committee_members:[] ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let* () = run_dac dac_node in
  let* () =
    Dac_node.wait_for dac_node "dac_node_layer_1_start_tracking.v0" (fun _ ->
        Some ())
  in
  assert (Dac_node.is_running_not_ready dac_node) ;
  let* () = Dac_node.terminate dac_node in
  let* () = Node.terminate node in
  Node.Config_file.update
    node
    (Node.Config_file.set_sandbox_network_with_user_activated_overrides
       [(Protocol.hash previous_protocol, Protocol.hash protocol)]) ;
  let* () = Node.run node nodes_args in
  let* () = Node.wait_for_ready node in
  let* () = run_dac dac_node in
  let* () =
    Lwt.join
      [
        Dac_node.wait_for dac_node "dac_node_plugin_resolved.v0" (fun _ ->
            Some ());
        Client.bake_for_and_wait client;
      ]
  in
  let* () = Dac_node.terminate dac_node in
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
    sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
    _committee_members =
  let payload = "test" in
  let* actual_rh, l1_operation =
    RPC.call
      dac_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Merkle_tree_V0")
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
    RPC.call dac_node (Dac_rpc.get_verify_signature l1_operation)
  in
  Check.(
    (is_signature_valid = true)
      bool
      ~error_msg:"Signature of external message is not valid") ;
  unit

let test_dac_node_handles_dac_store_preimage_hash_chain_V0 _protocol dac_node
    sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
    _committee_members =
  let payload = "test" in
  let* actual_rh, _l1_operation =
    RPC.call
      dac_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Hash_chain_V0")
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
    sc_rollup_node _sc_rollup_address _node _client pvm_name _threshold
    _committee_members =
  let payload = "test" in
  let* actual_rh, _l1_operation =
    RPC.call
      dac_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Merkle_tree_V0")
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
  let* preimage = RPC.call dac_node (Dac_rpc.get_preimage expected_rh) in
  Check.(
    (preimage = Hex.show recovered_preimage)
      string
      ~error_msg:
        "Returned page does not match the expected one (Current: %L <> \
         Expected: %R)") ;
  unit

let test_rollup_arith_uses_reveals protocol dac_node sc_rollup_node
    sc_rollup_address _node client _pvm_name _threshold _committee_members =
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
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
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Hash_chain_V0")
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
    sc_rollup_address _node client _pvm_name _threshold _committee_members =
  let payload = "Some data that is not related to the hash" in
  let _actual_rh =
    RPC.call
      dac_node
      (Dac_rpc.post_store_preimage ~payload ~pagination_scheme:"Hash_chain_V0")
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
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
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

let test_dac_node_imports_committee_members =
  Protocol.register_test
    ~__FILE__
    ~title:"dac node imports dac members sk_uris"
    ~tags:["dac"; "dac_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let run_dac = Dac_node.run ~wait_ready:false in
  let* committee_member =
    Client.bls_gen_keys ~alias:"committee_member" client
  in
  let* committee_member_info =
    Client.bls_show_address ~alias:committee_member client
  in
  let committee_member_address =
    committee_member_info.aggregate_public_key_hash
  in
  let dac_node =
    Dac_node.create_legacy
      ~node
      ~client
      ~threshold:1
      ~committee_members:[committee_member_address]
      ()
  in
  let* _dir = Dac_node.init_config dac_node in
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
  let dac_node =
    Dac_node.create_legacy ~node ~client ~threshold:1 ~committee_members:[] ()
  in
  let* _dir = Dac_node.init_config dac_node in
  let run_dac = Dac_node.run ~wait_ready:false in
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
        (Dac_rpc.post_store_preimage
           ~payload
           ~pagination_scheme:"Merkle_tree_V0")
    in
    return @@ check_valid_root_hash expected_rh actual_rh

  let test_streaming_of_root_hashes_as_observer _protocol node client
      coordinator threshold committee_members =
    (* 1. Create two new dac nodes; [observer_1] and [observer_2].
       2. Initialize their default configuration.
       3. Update their configuration so that their dac node client context
          points to [coordinator]. *)
    let committee_members =
      List.map
        (fun (a : Account.aggregate_key) -> a.aggregate_public_key_hash)
        committee_members
    in
    let observer_1 =
      Dac_node.create_legacy ~threshold ~committee_members ~node ~client ()
    in
    let observer_2 =
      Dac_node.create_legacy ~threshold ~committee_members ~node ~client ()
    in
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
    let push_signature =
      wait_for_signature_pushed_to_coordinator observer_1 ""
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
    assert (
      Lwt.is_sleeping observer_2_promise_1 && Lwt.is_sleeping push_signature) ;
    unit

  let test_streaming_of_root_hashes_as_member _protocol node client coordinator
      threshold dac_members =
    (* This test doesn't have any meaning if run without any committee member. *)
    assert (List.length dac_members > 0) ;
    let member_key : Account.aggregate_key = List.nth dac_members 0 in
    let dac_member_pkh = member_key.aggregate_public_key_hash in

    let member =
      Dac_node.create_legacy
        ~threshold
        ~committee_members:[]
        ~node
        ~client
        ?committee_member_address:(Some dac_member_pkh)
        ()
    in
    let* _ = Dac_node.init_config member in
    let () = set_coordinator member coordinator in
    let payload = "test_1" in
    let expected_rh =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let push_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh
    in
    let member_promise = wait_for_received_root_hash member expected_rh in

    (* Start running [member]. From now on we expect [member] to
       monitor streamed root hashes produced by [coordinator]. [coordinator]
       produces and pushes them as a side effect of serializing dac payload. *)
    let member_is_subscribed =
      wait_for_handle_new_subscription_to_hash_streamer coordinator
    in
    let expected_signature = bls_sign_hex_hash member_key (`Hex expected_rh) in
    let* () = Dac_node.run member in
    let* () = member_is_subscribed in
    (* [coordinator] serializes [payload_1]. We expect it would push
       [expected_rh_1] to all attached subscribers, i.e. to [member]. *)
    let* () =
      coordinator_serializes_payload coordinator ~payload ~expected_rh
    in
    (* Assert [coordinator] emitted event that [expected_rh_1] was pushed
       to the data_streamer. *)
    let* () = push_promise in
    (* Assert [member] emitted event of received [expected_rh_1]. *)
    let* () = member_promise in

    (* If the signature inside the emitted event is equal to the [expected_signature]
       test is OK *)
    let* () =
      wait_for_signature_pushed_to_coordinator
        member
        (Tezos_crypto.Aggregate_signature.to_b58check expected_signature)
    in
    unit

  (** [check_downloaded_page coordinator observer page_hash] checks that the
     [observer] has downloaded a page with [page_hash] from the [coordinator],
     that the contents of the page corresponds to the ones of the
     [coordinator]. It returns the list  of the hashes contained in the
     [page_hash], if the page corresponds to a hash page. Otherwise, it returns
     the empty list. *)
  let check_downloaded_page coordinator observer page_hash =
    let* coordinator_hex_encoded_page =
      RPC.call coordinator (Dac_rpc.get_preimage page_hash)
    in
    let coordinator_page = Hex.to_string (`Hex coordinator_hex_encoded_page) in
    (* Check that the page has been saved by the observer. *)
    let* observer_hex_encoded_page =
      RPC.call observer (Dac_rpc.get_preimage page_hash)
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

  let test_observer_downloads_pages _protocol node client coordinator threshold
      committee_members =
    (* 1. Create one new dac nodes; [observer_1],
       2. Initialize the default configuration,
       3. Specify a temporary directory within the test data for the observer
          reveal data dir,
       4. Update the configuration of the observer so that the dac node client
          context points to [coordinator]. *)
    let committee_members =
      List.map
        (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
        committee_members
    in
    let observer =
      Dac_node.create_legacy
        ~threshold
        ~committee_members
        ~name:"observer"
        ~node
        ~client
        ()
    in
    let* _ = Dac_node.init_config observer in
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

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4934
       Once profiles are implemented this should be moved out of the
       `Legacy` module. Additionally, the test should be run using dac
       node running in the coordinator and not legacy mode. *)
  let test_coordinator_post_preimage_endpoint _protocol _node _client
      coordinator _threshold _dac_members =
    (* 1. Send the [payload] to coordinator.
       2. Assert that it returns [expected_rh].
       3. Assert event that root hash has been pushed to data streamer
          was emitted. *)
    let payload = "test_1" in
    let expected_rh =
      "00b29d7d1e6668fb35a9ff6d46fa321d227e9b93dae91c4649b53168e8c10c1827"
    in
    let root_hash_pushed_to_data_streamer_promise =
      wait_for_root_hash_pushed_to_data_streamer coordinator expected_rh
    in
    let* actual_rh =
      RPC.call coordinator (Dac_rpc.Coordinator.post_preimage ~payload)
    in
    let () = check_valid_root_hash expected_rh actual_rh in
    let* () = root_hash_pushed_to_data_streamer_promise in
    Lwt.return_unit
end

module Signature_manager = struct
  module Coordinator = struct
    let test_non_committee_signer_should_fail tz_client
        (coordinator_node, hex_root_hash, _dac_committee) =
      let* invalid_signer_key =
        Client.bls_gen_and_show_keys ~alias:"invalid_signer" tz_client
      in
      let signature = bls_sign_hex_hash invalid_signer_key hex_root_hash in
      let result =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh:invalid_signer_key.aggregate_public_key_hash
             ~signature)
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure with non-committee member signer."
        result

    (* Tests that trying to store a dac member signature for a different key
       - one that was not used for creating the signature - fails. *)
    let test_signature_verification_failure_should_fail
        (coordinator_node, hex_root_hash, dac_committee) =
      let member_i = Random.int (List.length dac_committee) in
      let memberi = List.nth dac_committee member_i in
      let memberj =
        List.find
          (fun (dc : Account.aggregate_key) -> memberi <> dc)
          dac_committee
      in
      let signature = bls_sign_hex_hash memberi hex_root_hash in
      let result =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh:memberj.aggregate_public_key_hash
             ~signature)
      in
      assert_lwt_failure
        ~__LOC__
        "Expected failure when signature verification fails but did not."
        result

    (* Tests that a valid signature over [hex_root_hash] that is submitted to
       the [coordinator_node] is stored. 2 signatures are produced and stored
       in the [coordinator_node]. The effects of this can be asserted
       by checking that the witness bitset is set to 3 *)
    let test_store_valid_signature_should_update_aggregate_signature
        (coordinator_node, hex_root_hash, dac_committee) =
      let members =
        List.map
          (fun i ->
            let key = List.nth dac_committee i in
            let signature = bls_sign_hex_hash key hex_root_hash in
            (key, signature))
          (range 0 1)
      in
      let* members_keys =
        List.fold_left
          (fun keys ((member : Account.aggregate_key), signature) ->
            let* keys in
            let* () =
              RPC.call
                coordinator_node
                (Dac_rpc.put_dac_member_signature
                   ~hex_root_hash
                   ~dac_member_pkh:member.aggregate_public_key_hash
                   ~signature)
            in
            return (member :: keys))
          (return [])
          members
      in
      let* witnesses, certificate, _root_hash =
        RPC.call coordinator_node (Dac_rpc.get_certificate ~hex_root_hash)
      in
      assert_witnesses ~__LOC__ 3 witnesses ;
      assert_verify_aggregate_signature members_keys hex_root_hash certificate ;
      unit

    let test_store_same_signature_more_than_once_should_be_noop
        (coordinator_node, _hex_root_hash, dac_committee) =
      let* hex_root_hash =
        init_hex_root_hash ~payload:"noop test abc 3210" coordinator_node
      in
      let member_i = 2 in
      let member = List.nth dac_committee member_i in
      let signature = bls_sign_hex_hash member hex_root_hash in
      let dac_member_pkh = member.aggregate_public_key_hash in
      let call () =
        RPC.call
          coordinator_node
          (Dac_rpc.put_dac_member_signature
             ~hex_root_hash
             ~dac_member_pkh
             ~signature)
      in
      let* () = call () in
      let* () = call () in
      let* witnesses, certificate, _root_hash =
        RPC.call coordinator_node (Dac_rpc.get_certificate ~hex_root_hash)
      in
      assert_witnesses ~__LOC__ 4 witnesses ;
      assert_verify_aggregate_signature [member] hex_root_hash certificate ;
      unit

    let test_handle_store_signature _protocol _tezos_node tz_client coordinator
        _threshold dac_committee =
      let* hex_root_hash = init_hex_root_hash coordinator in
      let dac_env = (coordinator, hex_root_hash, dac_committee) in
      let* () = test_non_committee_signer_should_fail tz_client dac_env in
      let* () = test_signature_verification_failure_should_fail dac_env in
      let* () =
        test_store_valid_signature_should_update_aggregate_signature dac_env
      in
      let* () =
        test_store_same_signature_more_than_once_should_be_noop dac_env
      in
      unit
  end
end

(* Tests that it's possible to retrieve the witness and certificate after
   storing a dac member signature. Also asserts that the certificate contains
   the member used for signing. *)
let test_get_certificate _protocol _tezos_node _tz_client coordinator _threshold
    dac_committee =
  let i = Random.int (List.length dac_committee) in
  let member = List.nth dac_committee i in
  let* hex_root_hash =
    init_hex_root_hash ~payload:"test get certificate payload 123" coordinator
  in
  let signature = bls_sign_hex_hash member hex_root_hash in
  let* () =
    RPC.call
      coordinator
      (Dac_rpc.put_dac_member_signature
         ~hex_root_hash
         ~dac_member_pkh:member.aggregate_public_key_hash
         ~signature)
  in
  let* witnesses, certificate, _root_hash =
    RPC.call coordinator (Dac_rpc.get_certificate ~hex_root_hash)
  in
  let expected_witnesses = Z.shift_left Z.one i in
  assert_witnesses ~__LOC__ (Z.to_int expected_witnesses) witnesses ;
  assert_verify_aggregate_signature [member] hex_root_hash certificate ;
  unit

(* 1. Observer should fetch missing page from Coordinator when GET /missing_page/{hash}
      is called.
   2. As a side effect, Observer should save fetched page into its page store before
      returning it in the response. This can be observer by checking the result of
      retrieving preimage before and after the GET /missing_page/{hash} call.*)
let test_observer_get_missing_page _protocol node client coordinator threshold
    dac_members =
  let root_hash =
    "00649d431e829f4adc68edecb8d8d8071154b57086cc124b465f6f6600a4bc91c7"
  in
  let root_hash_stream_promise =
    wait_for_root_hash_pushed_to_data_streamer coordinator root_hash
  in
  let* hex_root_hash =
    init_hex_root_hash ~payload:"test payload abc 123" coordinator
  in
  assert (root_hash = Hex.show hex_root_hash) ;
  let* () = root_hash_stream_promise in
  let observer =
    Dac_node.create_legacy
      ~threshold
      ~committee_members:
        (List.map
           (fun (dc : Account.aggregate_key) -> dc.aggregate_public_key_hash)
           dac_members)
      ~node
      ~client
      ()
  in
  let* _ = Dac_node.init_config observer in
  let () = Legacy.set_coordinator observer coordinator in
  let* () = Dac_node.run observer in
  let* () =
    assert_lwt_failure
      ~__LOC__
      "Expected retrieve_preimage"
      (RPC.call observer (Dac_rpc.get_preimage (Hex.show hex_root_hash)))
  in
  let* missing_page =
    RPC.call observer (Dac_rpc.get_missing_page ~hex_root_hash)
  in
  let* coordinator_page =
    RPC.call coordinator (Dac_rpc.get_preimage (Hex.show hex_root_hash))
  in
  check_preimage coordinator_page missing_page ;
  let* observer_preimage =
    RPC.call observer (Dac_rpc.get_preimage (Hex.show hex_root_hash))
  in
  check_preimage coordinator_page observer_preimage ;
  unit

let register ~protocols =
  (* Tests with layer1 and dac nodes *)
  test_dac_node_startup protocols ;
  test_dac_node_imports_committee_members protocols ;
  test_dac_node_dac_threshold_not_reached protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_merkle_tree_v0"
    test_dac_node_handles_dac_store_preimage_merkle_V0
    protocols
    ~threshold:1
    ~committee_members:1 ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_reveals_data_hash_chain_v0"
    test_dac_node_handles_dac_store_preimage_hash_chain_V0
    protocols
    ~threshold:1
    ~committee_members:1 ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    ~threshold:0
    ~committee_members:0
    "dac_retrieve_preimage"
    test_dac_node_handles_dac_retrieve_preimage_merkle_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_uses_reveals"
    test_rollup_arith_uses_reveals
    protocols
    ~threshold:1
    ~committee_members:1 ;
  scenario_with_all_nodes
    ~tags:["dac"; "dac_node"]
    "dac_rollup_arith_wrong_hash"
    test_reveals_fails_on_wrong_hash
    ~threshold:1
    ~committee_members:1
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:0
    ~tags:["dac"; "dac_node"]
    "dac_streaming_of_root_hashes_in_legacy_mode"
    Legacy.test_streaming_of_root_hashes_as_observer
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:1
    ~tags:["dac"; "dac_node"]
    "dac_push_signature_in_legacy_mode_as_member"
    Legacy.test_streaming_of_root_hashes_as_member
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:0
    ~tags:["dac"; "dac_node"]
    "committee member downloads pages from coordinator"
    Legacy.test_observer_downloads_pages
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:2
    ~tags:["dac"; "dac_node"]
    "dac_get_certificate"
    test_get_certificate
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:0
    ~tags:["dac"; "dac_node"]
    "dac_coordinator_post_preimage_endpoint"
    Legacy.test_coordinator_post_preimage_endpoint
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:3
    ~tags:["dac"; "dac_node"]
    "dac_store_member_signature"
    Signature_manager.Coordinator.test_handle_store_signature
    protocols ;
  scenario_with_layer1_and_legacy_dac_nodes
    ~threshold:0
    ~committee_members:1
    ~tags:["dac"; "dac_node"]
    "dac_observer_get_missing_page"
    test_observer_get_missing_page
    protocols
