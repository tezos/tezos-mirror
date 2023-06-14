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

module Cryptobox = Rollup.Dal.Cryptobox

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3173
   The functions below are duplicated from sc_rollup.ml.
   They should be moved to a common submodule. *)
let make_int_parameter name = function
  | None -> []
  | Some value -> [(name, `Int value)]

let make_bool_parameter name = function
  | None -> []
  | Some value -> [(name, `Bool value)]

let make_string_parameter name = function
  | None -> []
  | Some value -> [(name, `String value)]

let test ~__FILE__ ?(tags = []) ?supports title f =
  let tags = "dal" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags ?supports f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "dal" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

let dal_enable_param dal_enable =
  make_bool_parameter ["dal_parametric"; "feature_enable"] dal_enable

let redundancy_factor_param redundancy_factor =
  make_int_parameter ["dal_parametric"; "redundancy_factor"] redundancy_factor

(* Some initialization functions to start needed nodes. *)

let setup_node ?(custom_constants = None) ?(additional_bootstrap_accounts = 5)
    ~parameters ~protocol ?activation_timestamp ?(event_sections_levels = [])
    ?(node_arguments = []) () =
  (* Temporary setup to initialise the node. *)
  let base = Either.right (protocol, custom_constants) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let* client = Client.init_mockup ~parameter_file ~protocol () in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let node = Node.create nodes_args in
  let* () = Node.config_init node [] in
  let* dal_parameters = Rollup.Dal.Parameters.from_client client in
  let config : Cryptobox.Config.t =
    {
      activated = true;
      use_mock_srs_for_testing = Some dal_parameters.cryptobox;
      bootstrap_peers = [];
    }
  in
  Node.Config_file.update
    node
    (Node.Config_file.set_sandbox_network_with_dal_config config) ;
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
    Client.activate_protocol_and_wait
      ?timestamp:activation_timestamp
      ~parameter_file
      ~protocol
      client
  in
  return (node, client, dal_parameters)

let with_layer1 ?custom_constants ?additional_bootstrap_accounts
    ?minimal_block_delay ?delay_increment_per_round ?attestation_lag
    ?attestation_threshold ?commitment_period ?challenge_window ?dal_enable
    ?event_sections_levels ?node_arguments ?activation_timestamp f ~protocol =
  let parameters =
    make_int_parameter ["dal_parametric"; "attestation_lag"] attestation_lag
    @ make_int_parameter
        ["dal_parametric"; "attestation_threshold"]
        attestation_threshold
    @ make_int_parameter
        ["smart_rollup_commitment_period_in_blocks"]
        commitment_period
    @ make_int_parameter
        ["smart_rollup_challenge_window_in_blocks"]
        challenge_window
    (* this will produce the empty list if dal_enable is not passed to the function invocation,
       hence the value from the protocol constants will be used. *)
    @ dal_enable_param dal_enable
    @ [(["smart_rollup_arith_pvm_enable"], `Bool true)]
    @ make_string_parameter ["minimal_block_delay"] minimal_block_delay
    @ make_string_parameter
        ["delay_increment_per_round"]
        delay_increment_per_round
  in
  let* node, client, dal_parameters =
    setup_node
      ?custom_constants
      ?additional_bootstrap_accounts
      ?event_sections_levels
      ?node_arguments
      ?activation_timestamp
      ~parameters
      ~protocol
      ()
  in
  let cryptobox = Rollup.Dal.make dal_parameters.cryptobox in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f dal_parameters cryptobox node client bootstrap1_key

let with_fresh_rollup ~protocol ?(pvm_name = "arith") ?dal_node f tezos_node
    tezos_client bootstrap1_key =
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"rollup"
      ~src:bootstrap1_key
      ~kind:pvm_name
      ~boot_sector:""
      ~parameters_ty:"string"
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~protocol
      ?dal_node
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:bootstrap1_key
  in
  (* Argument ~keys:[] allows to bake with all available delegates. *)
  let* () = Client.bake_for_and_wait tezos_client ~keys:[] in
  f rollup_address sc_rollup_node

let with_dal_node tezos_node tezos_client f key =
  let dal_node = Dal_node.create ~node:tezos_node ~client:tezos_client () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = Dal_node.run dal_node ~wait_ready:true in
  f key dal_node

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)
let scenario_with_layer1_node ?(tags = ["layer1"]) ?attestation_lag
    ?custom_constants ?commitment_period ?challenge_window ?(dal_enable = true)
    ?event_sections_levels ?node_arguments ?activation_timestamp
    ?minimal_block_delay ?delay_increment_per_round variant scenario =
  let description = "Testing DAL L1 integration" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ~custom_constants
        ?minimal_block_delay
        ?delay_increment_per_round
        ?attestation_lag
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ?activation_timestamp
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      scenario protocol parameters cryptobox node client)

let scenario_with_layer1_and_dal_nodes ?(tags = ["layer1"]) ?custom_constants
    ?minimal_block_delay ?delay_increment_per_round ?attestation_lag
    ?attestation_threshold ?commitment_period ?challenge_window
    ?(dal_enable = true) ?activation_timestamp variant scenario =
  let description = "Testing DAL node" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ~custom_constants
        ?minimal_block_delay
        ?delay_increment_per_round
        ?attestation_lag
        ?attestation_threshold
        ?commitment_period
        ?challenge_window
        ?activation_timestamp
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      with_dal_node node client @@ fun _key dal_node ->
      scenario protocol parameters cryptobox node client dal_node)

let scenario_with_all_nodes ?custom_constants ?node_arguments ?attestation_lag
    ?(tags = ["dal_node"]) ?(pvm_name = "arith") ?(dal_enable = true)
    ?commitment_period ?challenge_window ?minimal_block_delay
    ?delay_increment_per_round ?activation_timestamp variant scenario =
  let description = "Testing DAL rollup and node with L1" in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ~custom_constants
        ?node_arguments
        ?attestation_lag
        ?commitment_period
        ?challenge_window
        ?minimal_block_delay
        ?delay_increment_per_round
        ?activation_timestamp
        ~protocol
        ~dal_enable
      @@ fun parameters _cryptobox node client ->
      with_dal_node node client @@ fun key dal_node ->
      ( with_fresh_rollup ~protocol ~pvm_name ~dal_node
      @@ fun sc_rollup_address sc_rollup_node ->
        scenario
          protocol
          parameters
          dal_node
          sc_rollup_node
          sc_rollup_address
          node
          client
          pvm_name )
        node
        client
        key)

let update_neighbors dal_node neighbors =
  let neighbors =
    `A
      (List.map
         (fun dal_node ->
           `O
             [
               ("rpc-addr", `String (Dal_node.rpc_host dal_node));
               ("rpc-port", `Float (float_of_int (Dal_node.rpc_port dal_node)));
             ])
         neighbors)
  in
  Dal_node.Config_file.update
    dal_node
    (JSON.put ("neighbors", JSON.annotate ~origin:"dal_node_config" neighbors))

let update_known_peers dal_node known_peers =
  let peers =
    `A
      (List.map
         (fun dal_node -> `String (Dal_node.listen_addr dal_node))
         known_peers)
  in
  Dal_node.Config_file.update
    dal_node
    (JSON.put ("peers", JSON.annotate ~origin:"dal_node_config" peers))

let wait_for_stored_slot dal_node slot_header =
  Dal_node.wait_for dal_node "stored_slot_shards.v0" (fun e ->
      if JSON.(e |-> "commitment" |> as_string) = slot_header then Some ()
      else None)

let wait_for_layer1_block_processing dal_node level =
  Dal_node.wait_for dal_node "dal_node_layer_1_new_head.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

let test_feature_flag _protocol _parameters _cryptobox node client
    _bootstrap_key =
  (* This test ensures the feature flag works:

     - 1. It checks the feature flag is not enabled by default

     - 2. It checks the new operations added by the feature flag
     cannot be propagated by checking their classification in the
     mempool. *)
  let* params = Rollup.Dal.Parameters.from_client client in
  let cryptobox = Rollup.Dal.make params.cryptobox in
  let commitment, proof =
    Rollup.Dal.Commitment.dummy_commitment cryptobox "coucou"
  in
  Check.(
    (params.feature_enabled = false)
      bool
      ~error_msg:"Feature flag for the DAL should be disabled") ;
  let*? process =
    RPC.(Client.spawn client @@ get_chain_block_context_dal_shards ())
  in
  let* () =
    Process.check_error
      ~msg:(rex "Data-availability layer will be enabled in a future proposal")
      process
  in
  let level = Node.get_level node + 1 in
  let* (`OpHash oph1) =
    Operation.Consensus.(
      inject
        ~force:true
        ~signer:Constant.bootstrap1
        (dal_attestation
           ~level
           ~attestation:(Array.make params.number_of_slots false))
        client)
  in
  let* (`OpHash oph2) =
    Operation.Manager.(
      inject
        ~force:true
        [make @@ dal_publish_slot_header ~index:0 ~level:1 ~commitment ~proof]
        client)
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool = Mempool.{empty with refused = [oph1; oph2]} in
  Check.(
    (mempool = expected_mempool)
      Mempool.classified_typ
      ~error_msg:"Expected mempool: %R. Got: %L. (Order does not matter)") ;
  let* () = Client.bake_for_and_wait client in
  let* block_metadata = RPC.(call node @@ get_chain_block_metadata ()) in
  if block_metadata.dal_attestation <> None then
    Test.fail "Did not expect to find \"dal_attestation\"" ;
  let* bytes = RPC_legacy.raw_bytes client in
  if not JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Unexpected entry dal in the context when DAL is disabled" ;
  unit

let test_one_committee_per_epoch _protocol _parameters _cryptobox node client
    _bootstrap_key =
  let* params = Rollup.Dal.Parameters.from_client client in
  let blocks_per_epoch = params.blocks_per_epoch in
  let* current_level =
    RPC.(call node @@ get_chain_block_helper_current_level ())
  in
  (* The test assumes we are at a level when an epoch starts. And
     that is indeed the case. *)
  assert (current_level.cycle_position = 0) ;
  let* first_committee =
    Rollup.Dal.Committee.at_level node ~level:current_level.level
  in
  (* We iterate through (the committees at) levels [current_level +
     offset], with [offset] from 1 to [blocks_per_epoch]. At offset 0
     we have the [first_committee] (first in the current epoch). The
     committees at offsets 1 to [blocks_per_epoch - 1] should be the
     same as the one at offset 0, the one at [blocks_per_epoch] (first
     in the next epoch) should be different. *)
  let rec iter offset =
    if offset > blocks_per_epoch then unit
    else
      let level = current_level.level + offset in
      let* committee = Rollup.Dal.Committee.at_level node ~level in
      if offset < blocks_per_epoch then (
        Check.((first_committee = committee) Rollup.Dal.Committee.typ)
          ~error_msg:
            "Unexpected different DAL committees at first level: %L, versus \
             current level: %R" ;
        unit)
      else if offset = blocks_per_epoch then (
        Check.((first_committee = committee) Rollup.Dal.Committee.typ)
          ~error_msg:
            "Unexpected equal DAL committees at first levels in subsequent \
             epochs: %L and %R" ;
        unit)
      else iter (offset + 1)
  in
  iter 1

let publish_slot ?counter ?force ~source ?level ?fee ?error ~index ~commitment
    ~proof node client =
  let level =
    match level with Some level -> level | None -> 1 + Node.get_level node
  in
  Operation.Manager.(
    inject
      ?error
      ?force
      [
        make ~source ?fee ?counter
        @@ dal_publish_slot_header ~index ~level ~commitment ~proof;
      ]
      client)

let publish_dummy_slot ~source ?level ?error ?fee ~index ~message cryptobox =
  let commitment, proof =
    Rollup.Dal.(Commitment.dummy_commitment cryptobox message)
  in
  publish_slot ~source ?level ?fee ?error ~index ~commitment ~proof

(* We check that publishing a slot header with a proof for a different
   slot leads to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_same_content ~source ?level ?fee
    ~index cryptobox =
  let commitment, _proof =
    Rollup.Dal.(Commitment.dummy_commitment cryptobox "a")
  in
  let _commitment, proof =
    Rollup.Dal.(Commitment.dummy_commitment cryptobox "b")
  in
  publish_slot ~source ?level ?fee ~index ~commitment ~proof

(* We check that publishing a slot header with a proof for the "same"
   slot contents but represented using a different [slot_size] leads
   to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_different_slot_size ~source ?level
    ?fee ~index parameters cryptobox =
  let cryptobox_params =
    {
      parameters.Rollup.Dal.Parameters.cryptobox with
      slot_size = 2 * parameters.cryptobox.slot_size;
    }
  in
  let cryptobox' = Rollup.Dal.make cryptobox_params in
  let msg = "a" in
  let commitment, _proof =
    Rollup.Dal.(Commitment.dummy_commitment cryptobox msg)
  in
  let _commitment, proof =
    Rollup.Dal.(Commitment.dummy_commitment cryptobox' msg)
  in
  publish_slot ~source ?level ?fee ~index ~commitment ~proof

let publish_slot_header ~source ?(fee = 1200) ~index ~commitment ~proof node
    client =
  let level = 1 + Node.get_level node in
  let commitment = Cryptobox.Commitment.of_b58check_opt commitment in
  let proof =
    Data_encoding.Json.destruct
      Cryptobox.Commitment_proof.encoding
      (`String proof)
  in
  match commitment with
  | Some commitment ->
      Operation.Manager.(
        inject
          [
            make ~source ~fee
            @@ dal_publish_slot_header ~index ~level ~commitment ~proof;
          ]
          client)
  | _ -> assert false

let dal_attestation ?level ?(force = false) ~signer ~nb_slots availability
    client =
  let attestation = Array.make nb_slots false in
  List.iter (fun i -> attestation.(i) <- true) availability ;
  let* level =
    match level with
    | Some level -> return level
    | None ->
        let* level = Client.level client in
        return @@ (level + 1)
  in
  Operation.Consensus.inject
    ~force
    ~signer
    (Operation.Consensus.dal_attestation ~level ~attestation)
    client

let dal_attestations ?level ?force
    ?(signers = Array.to_list Account.Bootstrap.keys) ~nb_slots availability
    client =
  Lwt_list.map_s
    (fun signer ->
      dal_attestation ?level ?force ~signer ~nb_slots availability client)
    signers

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
      RPC.call
        node
        (RPC.get_chain_block_context_dal_confirmed_slot_headers_history ())
    in
    if JSON.is_null confirmed_slots_opt then
      Test.fail
        "confirmed_slots_history RPC is not expected to return None if DAL is \
         enabled" ;
    let confirmed_slots = json_to_string confirmed_slots_opt in
    let confirmed_slots_from_ctxt =
      json_to_string @@ JSON.(dal_raw_json |-> "slot_headers_history")
    in
    if not (String.equal confirmed_slots confirmed_slots_from_ctxt) then
      Test.fail "Confirmed slots history mismatch." ;
    unit

let test_slot_management_logic _protocol parameters cryptobox node client
    _bootstrap_key =
  Log.info "Inject some invalid slot headers" ;
  let*! () = Client.reveal ~src:"bootstrap6" client in
  let* () = Client.bake_for_and_wait client in
  let* (`OpHash _) =
    let error =
      rex ~opts:[`Dotall] "Unexpected level in the future in slot header"
    in
    publish_dummy_slot
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:4
      ~index:2
      ~message:"a"
      ~error
      cryptobox
      node
      client
  in
  let* (`OpHash _) =
    let error =
      rex ~opts:[`Dotall] "Unexpected level in the past in slot header"
    in
    publish_dummy_slot
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:2
      ~index:2
      ~message:"a"
      ~error
      cryptobox
      node
      client
  in
  Log.info "Inject some valid slot headers" ;
  let* (`OpHash oph1) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~fee:1_000
      ~index:0
      ~message:"a"
      cryptobox
      node
      client
  in
  let* (`OpHash oph2) =
    publish_dummy_slot
      ~source:Constant.bootstrap2
      ~fee:1_500
      ~index:1
      ~message:"b"
      cryptobox
      node
      client
  in
  let* (`OpHash oph3) =
    publish_dummy_slot
      ~source:Constant.bootstrap3
      ~fee:2_000
      ~index:0
      ~message:"c"
      cryptobox
      node
      client
  in
  let* (`OpHash oph4) =
    publish_dummy_slot
      ~source:Constant.bootstrap4
      ~fee:1_200
      ~index:1
      ~message:"d"
      cryptobox
      node
      client
  in
  let* (`OpHash oph5) =
    publish_dummy_slot_with_wrong_proof_for_same_content
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:3
      ~index:2
      cryptobox
      node
      client
  in
  (* Check another operation now because we are lacking of bootstrap accounts. *)
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* (`OpHash oph6) =
    publish_dummy_slot_with_wrong_proof_for_different_slot_size
      ~source:bootstrap6
      ~fee:3_000
      ~level:3
      ~index:2
      parameters
      cryptobox
      node
      client
  in
  let* mempool = Mempool.get_mempool client in
  let expected_mempool =
    Mempool.{empty with validated = [oph1; oph2; oph3; oph4; oph5; oph6]}
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
    Failed {error_id = "proto.alpha.dal_publish_slot_header_duplicate"}
  in
  let proof_error =
    Failed {error_id = "proto.alpha.dal_publish_slot_header_invalid_proof"}
  in
  (* The baker sorts operations fee wise. Consequently order of
     application for the operations will be: oph3 > oph2 > oph4 > oph1

     For slot 0, oph3 is applied first.

     Flor slot1, oph2 is applied first. *)
  check_manager_operation_status operations_result fees_error oph1 ;
  check_manager_operation_status operations_result fees_error oph4 ;
  check_manager_operation_status operations_result proof_error oph5 ;
  check_manager_operation_status operations_result proof_error oph6 ;
  check_manager_operation_status operations_result Applied oph3 ;
  check_manager_operation_status operations_result Applied oph2 ;
  let nb_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let* _ =
    dal_attestations
      ~nb_slots
      ~signers:[Constant.bootstrap1; Constant.bootstrap2]
      [1; 0]
      client
  in
  let* _ =
    dal_attestations
      ~nb_slots
      ~signers:[Constant.bootstrap3; Constant.bootstrap4; Constant.bootstrap5]
      [1]
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* metadata = RPC.call node (RPC.get_chain_block_metadata ()) in
  let attestation =
    match metadata.dal_attestation with
    | None ->
        assert false
        (* Field is part of the encoding when the feature flag is true *)
    | Some x -> x
  in
  Check.(
    (attestation.(0) = false)
      bool
      ~error_msg:"Expected slot 0 to be un-attested") ;
  Check.(
    (attestation.(1) = true) bool ~error_msg:"Expected slot 1 to be attested") ;
  check_dal_raw_context node

(** This test tests various situations related to DAL slots attestation. It's
    many made of two parts (A) and (B). See the step inside the test.
*)
let test_slots_attestation_operation_behavior _protocol parameters cryptobox
    node client _bootstrap_key =
  (* Some helpers *)
  let nb_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let lag = parameters.attestation_lag in
  assert (lag > 1) ;
  let attest ~level =
    dal_attestation
      ~force:true
      ~nb_slots
      ~level
      ~signer:Constant.bootstrap2
      [0]
      client
  in
  let mempool_is ~__LOC__ expected_mempool =
    let* mempool = Mempool.get_mempool client in
    Check.(
      (mempool = expected_mempool)
        Mempool.classified_typ
        ~error_msg:(__LOC__ ^ " : Bad mempool !!!. Got %L")) ;
    unit
  in
  let check_slots_availability ~__LOC__ ~attested =
    let* metadata = RPC.call node (RPC.get_chain_block_metadata ()) in
    let dal_attestation =
      (* Field is part of the encoding when the feature flag is true *)
      Option.get metadata.dal_attestation
    in
    List.iter
      (fun i ->
        Check.(
          (Array.get dal_attestation i = true)
            bool
            ~error_msg:
              (Format.sprintf
                 "%s : Slot %d is expected to be confirmed."
                 __LOC__
                 i)))
      attested
    |> return
  in
  (* Just bake some blocks before starting publishing. *)
  let* () = repeat (2 * lag) (fun () -> Client.bake_for_and_wait client) in

  (* Part A.
     - No header published yet, just play with attestations with various levels;
     - Initially, only [h3] is applied, [h1; h2] are outdated, and [h4] is
       branch_delayed. After baking a block, [h3] is included in a block and
       [h4] becomes applied;
     - No slot is confirmed as no slot header is published.
  *)
  let now = Node.get_level node in
  let* (`OpHash h1) = attest ~level:1 in
  let outdated = [h1] in
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated} in
  let* (`OpHash h2) = attest ~level:(now - 1) in
  let outdated = [h1; h2] in
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated} in
  let* (`OpHash h3) = attest ~level:(now + 1) in
  let validated = [h3] in
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated; validated} in
  let* (`OpHash h4) = attest ~level:(now + 2) in
  let branch_delayed = [h4] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  let* () = Client.bake_for_and_wait client in
  let validated = [h4] in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  let* () = check_slots_availability ~__LOC__ ~attested:[] in
  (* Part B.
     - Publish a slot header (index 10) and bake;
     - All delegates attest the slot, but the operation is injected too early.
       The operation is branch_delayed;
     - We bake sufficiently many blocks to get the attestation applied and
       included in a block;
     - We check in the metadata that the slot with index 10 is attested.
  *)
  let* (`OpHash h5) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~fee:1_200
      ~index:10
      ~message:" TEST!!! "
      cryptobox
      node
      client
  in
  let validated = h5 :: validated in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  let* () = Client.bake_for_and_wait client in
  let now = Node.get_level node in
  let* attestation_ops =
    let level = now + lag in
    let* hashes = dal_attestations ~force:true ~nb_slots ~level [10] client in
    return @@ List.map (fun (`OpHash h) -> h) hashes
  in
  let validated = [] in
  let branch_delayed = attestation_ops in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  let* () = repeat (lag - 1) (fun () -> Client.bake_for_and_wait client) in
  let validated = attestation_ops in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  let* () = check_slots_availability ~__LOC__ ~attested:[] in
  let* () = Client.bake_for_and_wait client in
  let validated = [] in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated; branch_delayed}
  in
  check_slots_availability ~__LOC__ ~attested:[10]

(* Tests that DAL attestations are only included in the block
   if the attestation is from a DAL-committee member. *)
let test_slots_attestation_operation_dal_committee_membership_check _protocol
    parameters _cryptobox node client _bootstrap_key =
  let* new_account =
    (* Set up a new account that holds some tez and is revealed. *)
    let* new_account = Client.gen_and_show_keys client in
    let* () =
      Client.transfer
        ~giver:Constant.bootstrap1.alias
        ~receiver:new_account.alias
        ~amount:(Tez.of_int 10)
        ~burn_cap:Tez.one
        client
    in
    let* () = Client.bake_for_and_wait client in
    let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
    let* () = Client.bake_for_and_wait client in
    return new_account
  in
  let nb_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let level = Node.get_level node + 1 in
  let* (`OpHash _oph) =
    (* The attestation from the new account should fail as
       the new account is not an endorser and cannot be on the DAL committee. *)
    Operation.Consensus.(
      inject
        ~error:
          (rex
             (sf
                "%s is not part of the DAL committee for the level %d"
                new_account.public_key_hash
                level))
        ~request:`Notify
        ~signer:new_account
        (dal_attestation ~level ~attestation:(Array.make nb_slots false))
        client)
  in
  let* (`OpHash _oph) =
    (* The attestation from the bootstrap account should succeed as
       the bootstrap node is an endorser and is on the DAL committee by default. *)
    Operation.Consensus.(
      inject
        ~signer:Constant.bootstrap1
        (dal_attestation ~level ~attestation:(Array.make nb_slots false))
        client)
  in
  unit

let split_slot dal_node ~slot_size content =
  let slot = Rollup.Dal.make_slot ~slot_size content in
  let* commitment = RPC.call dal_node @@ Rollup.Dal.RPC.post_commitment slot in
  let* () =
    RPC.call dal_node @@ Rollup.Dal.RPC.put_commitment_shards commitment
  in
  let* proof =
    RPC.call dal_node @@ Rollup.Dal.RPC.get_commitment_proof commitment
  in
  return (commitment, proof)

let test_dal_node_slot_management _protocol parameters _cryptobox _node _client
    dal_node =
  let slot_content = "test with invalid UTF-8 byte sequence \xFA" in
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot_commitment, _proof = split_slot dal_node ~slot_size slot_content in
  let* received_slot =
    RPC.call dal_node (Rollup.Dal.RPC.get_commitment_slot slot_commitment)
  in
  let received_slot_content = Rollup.Dal.content_of_slot received_slot in
  Check.(
    (slot_content = received_slot_content)
      string
      ~error_msg:"Wrong slot content: Expected: %L. Got: %R") ;
  (* Only check that the function to retrieve pages succeeds, actual
     contents are checked in the test `rollup_node_stores_dal_slots`. *)
  let* _slots_as_pages =
    RPC.call dal_node (Rollup.Dal.RPC.slot_pages slot_commitment)
  in
  return ()

let () =
  Printexc.register_printer @@ function
  | Data_encoding.Binary.Read_error e ->
      Some
        (Format.asprintf
           "Failed to decode binary: %a@."
           Data_encoding.Binary.pp_read_error
           e)
  | _ -> None

let publish_and_store_slot ?with_proof ?counter ?force ?level ?(fee = 1_200)
    node client dal_node source index content ~slot_size =
  let slot_level =
    match level with Some level -> level | None -> 1 + Node.get_level node
  in
  let* slot_commitment =
    let slot = Rollup.Dal.make_slot ~slot_size content in
    let* commit = RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot) in
    let* () =
      RPC.call dal_node
      @@ Rollup.Dal.RPC.put_commitment_shards ?with_proof commit
    in
    let* () =
      RPC.call
        dal_node
        (Rollup.Dal.RPC.patch_commitment commit ~slot_level ~slot_index:index)
    in
    return commit
  in
  let commitment =
    Cryptobox.Commitment.of_b58check_opt slot_commitment
    |> mandatory "The b58check-encoded slot commitment is not valid"
  in
  let* proof =
    RPC.call dal_node @@ Rollup.Dal.RPC.get_commitment_proof slot_commitment
  in
  let proof =
    Data_encoding.Json.destruct
      Cryptobox.Commitment_proof.encoding
      (`String proof)
  in
  let* _ =
    publish_slot
      ~level:slot_level
      ?counter
      ?force
      ~source
      ~fee
      ~index
      ~commitment
      ~proof
      node
      client
  in
  return (index, slot_commitment)

let check_get_commitment dal_node ~slot_level check_result slots_info =
  Lwt_list.iter_s
    (fun (slot_index, commitment') ->
      let* response =
        RPC.call_raw
          dal_node
          (Rollup.Dal.RPC.get_level_index_commitment ~slot_index ~slot_level)
      in
      return @@ check_result commitment' response)
    slots_info

let get_commitment_succeeds expected_commitment response =
  let commitment =
    JSON.(parse ~origin:__LOC__ response.RPC_core.body |> as_string)
  in
  Check.(commitment = expected_commitment)
    Check.string
    ~error_msg:
      "The value of a stored commitment should match the one computed locally \
       (current = %L, expected = %R)"

let get_commitment_not_found _commit r = RPC.check_string_response ~code:404 r

let check_get_commitment_headers dal_node ~slot_level check_result slots_info =
  let test check_result ~query_string (slot_index, commit) =
    let slot_level, slot_index =
      if not query_string then (None, None)
      else (Some slot_level, Some slot_index)
    in
    let* response =
      RPC.call_raw
        dal_node
        (Rollup.Dal.RPC.get_commitment_headers ?slot_index ?slot_level commit)
    in
    return @@ check_result response
  in
  let* () = Lwt_list.iter_s (test check_result ~query_string:true) slots_info in
  Lwt_list.iter_s (test check_result ~query_string:false) slots_info

let check_published_levels_headers ~__LOC__ dal_node ~pub_level
    ~number_of_headers =
  let* slot_headers =
    RPC.call dal_node (Rollup.Dal.RPC.get_published_level_headers pub_level)
  in
  Check.(List.length slot_headers = number_of_headers)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected slot headers length (got = %L, expected = %R)" ;
  unit

let get_headers_succeeds expected_status response =
  let headers =
    JSON.(
      parse ~origin:"get_headers_succeeds" response.RPC_core.body
      |> Rollup.Dal.RPC.slot_headers_of_json)
  in
  List.iter
    (fun header ->
      Check.(header.Rollup.Dal.RPC.status = expected_status)
        Check.string
        ~error_msg:
          "The value of the fetched status should match the expected \
           one(current = %L, expected = %R)")
    headers

let test_dal_node_slots_headers_tracking _protocol parameters _cryptobox node
    client dal_node =
  let check_published_levels_headers =
    check_published_levels_headers dal_node
  in
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in

  let level = Node.get_level node in
  let pub_level = level + 1 in
  let publish ?fee =
    publish_and_store_slot ~level:pub_level ?fee node client dal_node ~slot_size
  in
  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in
  let* slot0 = publish Constant.bootstrap1 0 "test0" in
  let* slot1 = publish Constant.bootstrap2 1 "test1" in
  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:2
  in
  let* slot2_a = publish Constant.bootstrap3 4 ~fee:1_200 "test4_a" in
  let* slot2_b = publish Constant.bootstrap4 4 ~fee:1_350 "test4_b" in
  let* slot3 = publish Constant.bootstrap5 5 ~fee:1 "test5" in
  let* slot4 =
    let slot =
      Rollup.Dal.make_slot ~slot_size "never associated to a slot_id"
    in
    let* commit = RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot) in
    return (6, commit)
  in
  (* Just after injecting slots and before baking, all slots have status
     "Unseen". *)
  let* () =
    check_get_commitment_headers
      dal_node
      ~slot_level:level
      (get_headers_succeeds "unseen")
      [slot0; slot1; slot2_a; slot2_b; slot3; slot4]
  in
  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:5
  in
  (* slot2_a and slot3 will not be included as successfull, slot2_b has better
     fees for slot 4. While slot3's fee is too low. slot4 is not injected
     into L1 or Dal nodes.

     We decide to have two failed slots instead of just one to better test some
     internal aspects of failed slots headers recoding (i.e. having a collection
     of data instead of just one). *)
  let wait_block_processing =
    wait_for_layer1_block_processing dal_node pub_level
  in
  let* () = Client.bake_for_and_wait client in
  let* _pub_level = Node.wait_for_level node pub_level in
  let* () = wait_block_processing in

  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:5
  in
  let* slot_headers =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.get_published_level_headers
         ~status:"waiting_attestation"
         pub_level)
  in
  let slot_headers =
    List.map
      (fun sh -> (sh.Rollup.Dal.RPC.slot_index, sh.commitment))
      slot_headers
    |> List.fast_sort (fun (idx1, _) (idx2, _) -> Int.compare idx1 idx2)
  in
  Check.(slot_headers = [slot0; slot1; slot2_b])
    Check.(list (tuple2 int string))
    ~error_msg:
      "Published header is different from stored header (current = %L, \
       expected = %R)" ;
  let check_get_commitment_headers =
    check_get_commitment_headers dal_node ~slot_level:pub_level
  in
  let check_get_commitment =
    check_get_commitment dal_node ~slot_level:pub_level
  in
  let ok = [slot0; slot1; slot2_b] in
  let attested = [slot0; slot2_b] in
  let unattested = [slot1] in
  let ko = slot3 :: slot4 :: List.map (fun (i, c) -> (i + 100, c)) ok in

  (* Slots waiting for attestation. *)
  let* () = check_get_commitment get_commitment_succeeds ok in
  let* () =
    check_get_commitment_headers (get_headers_succeeds "waiting_attestation") ok
  in
  (* slot_2_a is not selected. *)
  let* () =
    check_get_commitment_headers (get_headers_succeeds "not_selected") [slot2_a]
  in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in

  (* Attest slots slot0 = (2, 0) and slot2_b = (2,4), bake and wait
     two seconds so that the DAL node updates its state. *)
  let nb_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let* _op_hash = dal_attestations ~nb_slots (List.map fst attested) client in
  let* () = Client.bake_for_and_wait client in
  let* () = Lwt_unix.sleep 2.0 in

  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:5
  in

  (* Slot confirmed. *)
  let* () = check_get_commitment get_commitment_succeeds ok in
  (* Slot that were waiting for attestation and now attested. *)
  let* () =
    check_get_commitment_headers (get_headers_succeeds "attested") attested
  in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in
  (* Slot that were waiting for attestation and now unattested. *)
  let* () =
    check_get_commitment_headers (get_headers_succeeds "unattested") unattested
  in
  (* slot_2_a is still not selected. *)
  let* () =
    check_get_commitment_headers (get_headers_succeeds "not_selected") [slot2_a]
  in
  (* slot_3 never finished in an L1 block, so the DAL node only saw it as
     Unseen. *)
  let* () =
    check_get_commitment_headers (get_headers_succeeds "unseen") [slot3]
  in
  (* slot_4 is never injected in any of the nodes. So, it's not
     known by the Dal node. *)
  let* () =
    check_get_commitment_headers
      (fun response ->
        Check.(
          (String.trim response.RPC_core.body = "[]")
            string
            ~error_msg:"slot4 is not expected to have a header"))
      [slot4]
  in
  let* () =
    check_published_levels_headers
      ~__LOC__
      ~pub_level:(pub_level - 1)
      ~number_of_headers:0
  in
  let* () =
    check_published_levels_headers ~__LOC__ ~pub_level ~number_of_headers:5
  in
  check_published_levels_headers
    ~__LOC__
    ~pub_level:(pub_level + 1)
    ~number_of_headers:0

let generate_dummy_slot slot_size =
  String.init slot_size (fun i ->
      match i mod 3 with 0 -> 'a' | 1 -> 'b' | _ -> 'c')

let test_dal_node_rebuild_from_shards _protocol parameters _cryptobox node
    client dal_node =
  (* Steps in this integration test:
     1. Run a dal node
     2. Generate and publish a full slot, then bake
     3. Download exactly 1/redundancy_factor shards
        from this slot (it would work with more)
     4. Ensure we can rebuild the original data using the above shards
  *)
  let crypto_params = parameters.Rollup.Dal.Parameters.cryptobox in
  let slot_size = crypto_params.slot_size in
  let slot_content = generate_dummy_slot slot_size in
  let publish = publish_and_store_slot node client dal_node ~slot_size in
  let* _slot_index, slot_header = publish Constant.bootstrap1 0 slot_content in
  let* () = Client.bake_for_and_wait client in
  let* _level = Node.wait_for_level node 1 in
  let number_of_shards =
    (crypto_params.number_of_shards / crypto_params.redundancy_factor) - 1
  in
  let downloaded_shard_ids =
    range 0 number_of_shards
    |> List.map (fun i -> i * crypto_params.redundancy_factor)
  in
  let* shards =
    RPC.call dal_node (Rollup.Dal.RPC.shards ~slot_header downloaded_shard_ids)
  in
  let shard_of_json shard =
    let shard =
      match Data_encoding.Json.from_string shard with
      | Ok s -> s
      | Error _ -> Test.fail "shard RPC sent invalid json"
    in
    let shard = Data_encoding.Json.destruct Cryptobox.shard_encoding shard in
    ({index = shard.index; share = shard.share} : Cryptobox.shard)
  in
  let shards = shards |> List.to_seq |> Seq.map shard_of_json in
  let cryptobox = Rollup.Dal.make parameters.cryptobox in
  let reformed_slot =
    match Cryptobox.polynomial_from_shards cryptobox shards with
    | Ok p -> Cryptobox.polynomial_to_slot cryptobox p |> Bytes.to_string
    | Error _ -> Test.fail "Fail to build polynomial from shards"
  in
  Check.(reformed_slot = slot_content)
    Check.(string)
    ~error_msg:
      "Reconstructed slot is different from original slot (current = %L, \
       expected = %R)" ;
  return ()

let test_dal_node_test_slots_propagation _protocol parameters cryptobox node
    client dal_node1 =
  let dal_node2 = Dal_node.create ~node ~client () in
  let dal_node3 = Dal_node.create ~node ~client () in
  let dal_node4 = Dal_node.create ~node ~client () in
  let* _ = Dal_node.init_config dal_node2 in
  let* _ = Dal_node.init_config dal_node3 in
  let* _ = Dal_node.init_config dal_node4 in
  update_neighbors dal_node3 [dal_node1; dal_node2] ;
  update_neighbors dal_node4 [dal_node3] ;
  let* () = Dal_node.run dal_node2 in
  let* () = Dal_node.run dal_node3 in
  let* () = Dal_node.run dal_node4 in
  let commitment1, _proof1 =
    Rollup.Dal.Commitment.dummy_commitment cryptobox "content1"
  in
  let slot_header1_exp = Rollup.Dal.Commitment.to_string commitment1 in
  let commitment2, _proof2 =
    Rollup.Dal.Commitment.dummy_commitment cryptobox "content2"
  in
  let slot_header2_exp = Rollup.Dal.Commitment.to_string commitment2 in
  let p1 = wait_for_stored_slot dal_node3 slot_header1_exp in
  let p2 = wait_for_stored_slot dal_node3 slot_header2_exp in
  let p3 = wait_for_stored_slot dal_node4 slot_header1_exp in
  let p4 = wait_for_stored_slot dal_node4 slot_header2_exp in
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot_header1, _proof1 = split_slot dal_node1 ~slot_size "content1" in
  let* slot_header2, _proof2 = split_slot dal_node2 ~slot_size "content2" in
  Check.(
    (slot_header1_exp = slot_header1) string ~error_msg:"Expected:%L. Got: %R") ;
  Check.(
    (slot_header2_exp = slot_header2) string ~error_msg:"Expected:%L. Got: %R") ;
  Lwt.join [p1; p2; p3; p4]

let commitment_of_slot cryptobox slot =
  let polynomial =
    Cryptobox.polynomial_from_slot
      cryptobox
      (Rollup.Dal.content_of_slot slot |> Bytes.of_string)
    |> Result.get_ok
  in
  match Cryptobox.commit cryptobox polynomial with
  | Ok cm -> cm
  | Error (`Invalid_degree_strictly_less_than_expected _ as commit_error) ->
      Test.fail "%s" (Cryptobox.string_of_commit_error commit_error)

let test_dal_node_test_post_commitments _protocol parameters cryptobox _node
    _client dal_node =
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let mk_slot size =
    Rollup.Dal.make_slot ~padding:false ~slot_size (generate_dummy_slot size)
  in
  let failing_post_slot_rpc slot =
    let* response =
      RPC.call_raw dal_node @@ Rollup.Dal.RPC.post_commitment slot
    in
    return
    @@ RPC.check_string_response
         ~body_rex:"dal.node.invalid_slot_size"
         ~code:500
         response
  in
  let size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let slot_big = mk_slot (size + 1) in
  let slot_small = mk_slot (size - 1) in
  let slot_ok = mk_slot size in
  let* () = failing_post_slot_rpc slot_big in
  let* () = failing_post_slot_rpc slot_small in
  let* commitment1 =
    RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot_ok)
  in
  let* commitment2 =
    RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot_ok)
  in
  (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/4250
     The second RPC call above succeeeds, but the (untested) returned HTTP status
     should likely be 200 and 201 in the first similar RPC call.
  *)
  Check.(commitment1 = commitment2)
    Check.string
    ~error_msg:
      "Storing a slot twice should return the same commitment (current = %L, \
       expected = %R)" ;
  let commitment3 =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot_ok
  in
  Check.(commitment1 = commitment3)
    Check.string
    ~error_msg:
      "The commitment of a stored commitment should match the one computed \
       locally (current = %L, expected = %R)" ;
  unit

let test_dal_node_test_patch_commitments _protocol parameters cryptobox _node
    _client dal_node =
  let failing_patch_slot_rpc commit ~slot_level ~slot_index =
    let* response =
      RPC.call_raw dal_node
      @@ Rollup.Dal.RPC.patch_commitment commit ~slot_level ~slot_index
    in
    return @@ RPC.check_string_response ~code:404 response
  in
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let slot = Rollup.Dal.make_slot ~slot_size (generate_dummy_slot slot_size) in
  let commitment =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot
  in
  let* () = failing_patch_slot_rpc commitment ~slot_level:0 ~slot_index:0 in
  let* commitment' = RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot) in
  Check.(commitment' = commitment)
    Check.string
    ~error_msg:
      "The commitment of a stored commitment should match the one computed \
       locally (current = %L, expected = %R)" ;
  let patch_slot_rpc ~slot_level ~slot_index =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.patch_commitment commitment ~slot_level ~slot_index)
  in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:0 in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:0 in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:1 in
  patch_slot_rpc ~slot_level:(-4) ~slot_index:3

let test_dal_node_test_get_commitment_slot _protocol parameters cryptobox _node
    _client dal_node =
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let slot = Rollup.Dal.make_slot ~slot_size (generate_dummy_slot slot_size) in
  let commitment =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot
  in
  let* () =
    let* response =
      RPC.call_raw dal_node @@ Rollup.Dal.RPC.get_commitment_slot commitment
    in
    return @@ RPC.check_string_response ~code:404 response
  in
  let* _commitment = RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot) in
  (* commit = _commitment already tested in /POST test. *)
  let* got_slot =
    RPC.call dal_node (Rollup.Dal.RPC.get_commitment_slot commitment)
  in
  Check.(Rollup.Dal.content_of_slot slot = Rollup.Dal.content_of_slot got_slot)
    Check.string
    ~error_msg:
      "The slot content retrieved from the node is not as expected (expected = \
       %L, got = %R)" ;
  unit

let test_dal_node_test_get_commitment_proof _protocol parameters cryptobox _node
    _client dal_node =
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let slot = Rollup.Dal.make_slot ~slot_size (generate_dummy_slot slot_size) in
  let* commitment = RPC.call dal_node (Rollup.Dal.RPC.post_commitment slot) in
  let* proof =
    RPC.call dal_node (Rollup.Dal.RPC.get_commitment_proof commitment)
  in
  let _, expected_proof =
    Rollup.Dal.Commitment.dummy_commitment
      cryptobox
      (generate_dummy_slot slot_size)
  in
  let (`Hex expected_proof) =
    Data_encoding.Binary.to_bytes_exn
      Rollup.Dal.Cryptobox.Commitment_proof.encoding
      expected_proof
    |> Hex.of_bytes
  in
  Check.(proof = expected_proof)
    Check.string
    ~error_msg:"Wrong proof computed (got = %L, expected = %R)" ;
  unit

let test_dal_node_startup =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node startup"
    ~tags:["dal"; "dal_node"]
  @@ fun protocol ->
  let run_dal = Dal_node.run ~wait_ready:false in
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
  let dal_node = Dal_node.create ~node ~client () in
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
       [(Protocol.hash previous_protocol, Protocol.hash protocol)]) ;
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

let send_messages ?(bake = true) ?(src = Constant.bootstrap2.alias)
    ?(alter_final_msg = Fun.id) client msgs =
  let msg =
    alter_final_msg
    @@ Ezjsonm.(to_string ~minify:true @@ list Ezjsonm.string msgs)
  in
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  if bake then Client.bake_for_and_wait client else unit

let bake_levels n client = repeat n (fun () -> Client.bake_for_and_wait client)

let rollup_node_stores_dal_slots ?expand_test protocol parameters dal_node
    sc_rollup_node sc_rollup_address node client _pvm_name =
  (* Check that the rollup node downloaded the confirmed slots to which it is
     subscribed:

     0. run dl node.

     1. Send three slots to dal node and obtain corresponding headers.

     2. Run rollup node for an originated rollup.

     3. Setting the parameters of the PVM. Rollup node subscribes to slots 0, 2,
     4 and 6.

     4. Publish the three slot headers for slots with indexes 0, 1 and 2.

     5. Check that the slot_headers are fetched by the rollup node.

     6. Attest only slots 1 and 2.

     7. Only slots 1 and 2 are attested. No slot is currently pre-downloaded by
     the rollup.

     8. Bake a block so that the rollup node interprets the previously published
     & attested slot(s).

     9. Verify that rollup node has downloaded slot 2. Slot 0 is unconfirmed,
     and slot 1 has not been downloaded.
  *)
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let split_slot = split_slot dal_node ~slot_size in

  (* 0. run dl node. *)

  (* 1. Send three slots to dal node and obtain corresponding headers. *)
  let slot_contents_0 = " 10 " in
  let* commitment_0, proof_0 = split_slot slot_contents_0 in
  let slot_contents_1 = " 200 " in
  let* commitment_1, proof_1 = split_slot slot_contents_1 in
  let slot_contents_2 = " 400 " in
  let* commitment_2, proof_2 = split_slot slot_contents_2 in

  (* 2. Run rollup node for an originated rollup. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in

  Check.(level = init_level)
    Check.int
    ~error_msg:
      "Current level has moved past origination level (current = %L, expected \
       = %R)" ;

  (* 3. Setting the parameters of the PVM. Rollup node subscribes to slots 0, 2,
     4 and 6. *)
  let number_of_slots = parameters.number_of_slots in
  let attestation_lag = 1 in
  let number_of_pages = 256 in
  let subscribed_slots = "0:2:4:6" in
  let messages =
    [
      Format.sprintf
        "dal:%d:%d:%d:%s"
        number_of_slots
        attestation_lag
        number_of_pages
        subscribed_slots;
    ]
  in
  let* () = send_messages client messages in

  (* 4. Publish the three slot headers for slots with indexes 0, 1 and 2. *)
  let* _op_hash =
    publish_slot_header
      ~source:Constant.bootstrap1
      ~index:0
      ~commitment:commitment_0
      ~proof:proof_0
      node
      client
  in
  let* _op_hash =
    publish_slot_header
      ~source:Constant.bootstrap2
      ~index:1
      ~commitment:commitment_1
      ~proof:proof_1
      node
      client
  in
  let* _op_hash =
    publish_slot_header
      ~source:Constant.bootstrap3
      ~index:2
      ~commitment:commitment_2
      ~proof:proof_2
      node
      client
  in
  (* 5. Check that the slot_headers are fetched by the rollup node. *)
  let* () = Client.bake_for_and_wait client in
  let* slots_published_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + 2)
  in
  let*! slots_headers =
    Sc_rollup_client.dal_slot_headers ~hooks sc_rollup_client
  in
  let commitments =
    slots_headers
    |> List.map (fun Sc_rollup_client.{commitment; _} -> commitment)
  in
  let expected_commitments = [commitment_0; commitment_1; commitment_2] in
  Check.(commitments = expected_commitments)
    (Check.list Check.string)
    ~error_msg:"Unexpected list of slot headers (current = %L, expected = %R)" ;

  (* 6. Attest only slots 1 and 2. *)
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let nb_slots = parameters.number_of_slots in
  let* _ops_hashes = dal_attestations ~nb_slots [2; 1] client in
  let* () = Client.bake_for_and_wait client in
  let* slot_confirmed_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (slots_published_level + 1)
  in
  Check.(slot_confirmed_level = slots_published_level + 1)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;

  (* 7. Check that two slots have been attested. *)
  let*! downloaded_slots =
    Sc_rollup_client.get_dal_processed_slots ~hooks sc_rollup_client
  in
  let downloaded_confirmed_slots =
    List.filter (fun (_i, s) -> String.equal s "confirmed") downloaded_slots
  in
  let expected_number_of_confirmed_slots = 2 in

  Check.(
    List.length downloaded_confirmed_slots = expected_number_of_confirmed_slots)
    Check.int
    ~error_msg:
      "Unexpected number of slots that have been either downloaded or \
       unconfirmed (current = %L, expected = %R)" ;

  (* 8. Bake a block so that the rollup node interprets the previously published
     & attested slot(s). *)
  let* () = Client.bake_for_and_wait client in

  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (slot_confirmed_level + 1)
  in
  Check.(level = slot_confirmed_level + 1)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;

  (* 9. Verify that rollup node has downloaded slot 2. Slot 0 is unconfirmed,
     and slot 1 has not been downloaded. *)
  let confirmed_level_as_string = Int.to_string slot_confirmed_level in
  let*! downloaded_slots =
    Sc_rollup_client.get_dal_processed_slots
      ~block:confirmed_level_as_string
      sc_rollup_client
  in
  let downloaded_confirmed_slots =
    List.filter (fun (_i, s) -> String.equal s "confirmed") downloaded_slots
  in
  (* 10. Check the first page of the two attested slots' content. *)
  let expected_number_of_confirmed_slots = 2 in
  Check.(
    List.length downloaded_confirmed_slots = expected_number_of_confirmed_slots)
    Check.int
    ~error_msg:
      "Unexpected number of slots that have been either downloaded or \
       unconfirmed (current = %L, expected = %R)" ;
  let submitted_slots_contents =
    [slot_contents_0; slot_contents_1; slot_contents_2]
  in
  let* () =
    Lwt_list.iteri_s
      (fun index (slot_index, _status) ->
        Check.(
          (index + 1 = slot_index)
            int
            ~error_msg:"unexpected slot index (current = %L, expected = %R)") ;
        let slot_commitment = List.nth commitments slot_index in
        let* slot_pages =
          RPC.call dal_node (Rollup.Dal.RPC.slot_pages slot_commitment)
        in
        let relevant_page = List.nth slot_pages 0 in
        let confirmed_slot_content =
          List.nth submitted_slots_contents slot_index
        in
        let message =
          String.sub relevant_page 0 (String.length confirmed_slot_content)
        in
        Check.(message = confirmed_slot_content)
          Check.string
          ~error_msg:"unexpected message in slot (current = %L, expected = %R)" ;
        unit)
      downloaded_confirmed_slots
  in
  match expand_test with
  | None -> return ()
  | Some f -> f ~protocol client sc_rollup_address sc_rollup_node

let check_saved_value_in_pvm ~name ~expected_value sc_client =
  let*! encoded_value =
    Sc_rollup_client.state_value ~hooks sc_client ~key:(sf "vars/%s" name)
  in
  match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
  | Error error ->
      failwith
        (Format.asprintf
           "The arithmetic PVM has an unexpected state: %a"
           Data_encoding.Binary.pp_read_error
           error)
  | Ok value ->
      Check.(
        (value = expected_value)
          int
          ~error_msg:
            "Invalid value in rollup state (current = %L, expected = %R)") ;
      return ()

let rollup_node_interprets_dal_pages ~protocol client sc_rollup sc_rollup_node =
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
  in

  (* The Dal content is as follows:
      - the page 0 of slot 0 contains 10,
      - the page 0 of slot 1 contains 200,
      - the page 0 of slot 2 contains 400.
     Only slot 1 abd 2 are confirmed. But PVM Arith only interprets even
     slots, we expect to have value = 502
     (including the values 99 and 3 send via Inbox).
  *)
  let expected_value = 502 in
  (* The code should be adapted if the current level changes. *)
  let* () = send_messages client [" 99 3 "; " + + value"] in
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* _lvl =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 1)
  in
  check_saved_value_in_pvm ~name:"value" ~expected_value sc_rollup_client

let check_profiles ~__LOC__ dal_node ~expected =
  let* profiles = RPC.call dal_node (Rollup.Dal.RPC.get_profiles ()) in
  return
    Check.(
      (profiles = expected)
        Rollup.Dal.Check.profiles_typ
        ~error_msg:
          (__LOC__ ^ " : Unexpected profiles (Actual: %L <> Expected: %R)"))

let test_dal_node_test_patch_profile _protocol _parameters _cryptobox _node
    _client dal_node =
  let open Rollup.Dal.RPC in
  let check_bad_attestor_pkh_encoding profile =
    let* response = RPC.call_raw dal_node @@ patch_profiles [profile] in
    return @@ RPC.check_string_response ~code:400 response
  in
  let patch_profile_rpc profile =
    RPC.call dal_node (patch_profiles [profile])
  in
  let profile1 = Attestor Constant.bootstrap1.public_key_hash in
  let profile2 = Attestor Constant.bootstrap2.public_key_hash in
  (* We start with empty profile list *)
  let* () = check_profiles ~__LOC__ dal_node ~expected:[] in
  (* Adding [Attestor] profile with pkh that is not encoded as
     [Tezos_crypto.Signature.Public_key_hash.encoding] should fail. *)
  let* () = check_bad_attestor_pkh_encoding (Attestor "This is invalid PKH") in
  (* Test adding duplicate profiles stores profile only once *)
  let* () = patch_profile_rpc profile1 in
  let* () = patch_profile_rpc profile1 in
  let* () = check_profiles ~__LOC__ dal_node ~expected:[profile1] in
  (* Test adding multiple profiles *)
  let* () = patch_profile_rpc profile2 in
  check_profiles ~__LOC__ dal_node ~expected:[profile1; profile2]

(* Check that result of the DAL node's
   GET /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices
   is consistent with the result of the L1 node GET dal/shards . *)
let test_dal_node_get_assigned_shard_indices _protocol _parameters _cryptobox
    node _client dal_node =
  let pkh = Constant.bootstrap1.public_key_hash in
  let* level =
    RPC.(call node @@ get_chain_block_helper_current_level ())
    |> Lwt.map (fun level -> level.RPC.level)
  in
  let* committee_from_l1 = Rollup.Dal.Committee.at_level node ~level in
  let* shards_from_dal =
    RPC.call dal_node Rollup.Dal.RPC.(get_assigned_shard_indices ~level ~pkh)
  in
  match
    committee_from_l1
    |> List.find_opt (fun member ->
           String.equal member.Rollup.Dal.Committee.attestor pkh)
  with
  | None -> Test.fail ~__LOC__ "pkh %S not found in committee from L1." pkh
  | Some member ->
      let shards_from_l1 =
        List.init member.power (fun i -> member.first_shard_index + i)
      in
      Check.(
        (shards_from_dal = shards_from_l1)
          (list int)
          ~error_msg:
            "Shard indexes does not match between DAL and L1  (From DAL: %L <> \
             From L1: %R)") ;
      unit

let test_dal_node_get_attestable_slots _protocol parameters cryptobox node
    client dal_node =
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let split_slot = split_slot dal_node ~slot_size in
  let number_of_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  Log.info "Inject the shards of slots 1 and 3." ;
  let slot1_content = "slot 1" in
  let slot2_content = "slot 2" in
  let slot3_content = "slot 3" in
  let* _commitment, _proof = split_slot slot1_content in
  let* _commitment, _proof = split_slot slot3_content in
  Log.info "Publish slots 1 and 2 (inject and bake)." ;
  let level = Node.get_level node + 1 in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~level ~index ~message cryptobox node client
    in
    unit
  in
  let* () = publish Constant.bootstrap1 ~index:0 slot1_content in
  let* () = publish Constant.bootstrap2 ~index:2 slot2_content in
  let wait_block_processing = wait_for_layer1_block_processing dal_node level in
  let* () = Client.bake_for_and_wait client in
  let* () = wait_block_processing in
  Log.info "Check attestability of slots." ;
  let attested_level = level + parameters.attestation_lag in
  let rec iter i =
    if i < 0 then unit
    else
      let attestor = Account.Bootstrap.keys.(i) in
      (* Note: we assume that the key has at least an assigned shard index. *)
      let* res =
        RPC.call
          dal_node
          (Rollup.Dal.RPC.get_attestable_slots ~attestor ~attested_level)
      in
      match res with
      | Not_in_committee ->
          Test.fail "attestor %s not in committee" attestor.alias
      | Attestable_slots slots ->
          Check.(
            (number_of_slots = List.length slots)
              int
              ~error_msg:"Expected %L slots (got %R)") ;
          (match slots with
          | true :: rest when List.for_all (fun b -> not b) rest ->
              (* 1st slot is attestable; the rest are not: the 2nd is not because
                 the shards are not available; the rest are not because they are not
                 published *)
              ()
          | _ -> Test.fail "Unexpected result") ;
          iter (i - 1)
  in
  let* () = iter (Array.length Account.Bootstrap.keys - 1) in
  Log.info "Check case when pkh not in the DAL committee." ;
  let* new_account = Client.gen_and_show_keys client in
  let* res =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.get_attestable_slots
         ~attestor:new_account
         ~attested_level)
  in
  match res with
  | Not_in_committee -> return ()
  | Attestable_slots _ ->
      Test.fail "attestor %s is in committee!" new_account.alias

(* This test checks that the attestor correctly emits attestations, by
   publishing a slot per level for a few levels, then checking that the slots
   are attested or not, depending on whether or not all delegates attested the
   slots. We use [attestation_threshold = 100] to this end; with a smaller
   threshold it is harder to control whether the slot will be attested or not
   (one would have to take into account the distribution of shard indexes to
   delegates).

   Slots are published by baking with `bake for`, because when running the baker
   daemon, it is not possible to control the time of publishing of a slot, which
   is successful only if done for the right level.

   There are two variants of the test. (1) Without the baker daemon, the
   attestation lag is 1, meaning that `bake for` also emits the expected
   attestations. (2) With the baker daemon, which is started after all slots are
   published, the attestation lag is chosen big enough so that the baker can
   attest the already published slots while running. *)
let test_attestor ~with_baker_daemon protocol parameters cryptobox node client
    dal_node =
  Check.((parameters.Rollup.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  let dal_node_endpoint = Rollup.Dal.endpoint dal_node in
  let number_of_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let slot_size = parameters.cryptobox.slot_size in
  let slot_idx level = level mod number_of_slots in
  let num_bakers = Array.length Account.Bootstrap.keys in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.alias)
  in
  let publish source ~level ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~level ~index ~message cryptobox node client
    in
    unit
  in
  let publish_and_store level =
    let source = Account.Bootstrap.keys.(level mod num_bakers) in
    let index = slot_idx level in
    let slot_content =
      Format.asprintf "content at level %d index %d" level index
    in
    let* () = publish source ~level ~index slot_content in
    let* _commitment, _proof = split_slot dal_node slot_content ~slot_size in
    unit
  in
  let publish_and_bake ~init_level ~target_level delegates =
    Log.info
      "Publish (inject and bake) a slot header at each level from %d to %d."
      init_level
      target_level ;
    let rec iter level =
      if level > target_level then return ()
      else
        let* () = publish_and_store level in
        let* () =
          Client.bake_for_and_wait ~keys:delegates ~dal_node_endpoint client
        in
        let* _ = Node.wait_for_level node level in
        iter (level + 1)
    in
    iter init_level
  in
  let run_baker delegates target_level =
    let* baker =
      Baker.init
        ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
        ~protocol
        ~dal_node
        ~delegates
        node
        client
    in
    let* _ = Node.wait_for_level node target_level in
    Baker.terminate baker
  in

  (* Test goal: the published slot at levels in [first_level, intermediary_level - 1]
     should be attested, the one at levels in at levels in [intermediary_level,
     max_level - 1] should not be attested. *)
  let first_level = Node.get_level node + 1 in
  let intermediary_level =
    (* We want at least two levels for which the published slot is attested. *)
    first_level + 2
  in
  let max_level =
    (* We want at least two levels for which the published slot is unattested;
       we add 2 for "safety"; see discussion (D) below. *)
    intermediary_level + 2 + 2
  in
  if with_baker_daemon then
    (* We want [max_level - attestation_lag < first_level], so that the
       delegates that attest at the last level baked by `bake for` (that is, at
       [max_level]) have no published slot to attest, in order not interfere
       with the attestations done by the baker daemon. *)
    Check.((parameters.attestation_lag > max_level - first_level) int)
      ~error_msg:
        "attestation_lag (%L) should be higher than [max_level - first_level] \
         (which is %R)"
  else
    Check.((parameters.attestation_lag = 1) int)
      ~error_msg:"attestation_lag (%L) should be 1" ;
  let wait_block_processing =
    wait_for_layer1_block_processing dal_node max_level
  in
  (* Publish and bake. When [with_baker_deamon = false], we have
     [attestation_lag = 1] so `bake for` would lead to the delegates attesting
     for the slots published at the previous level; hence it is important how
     many delegates attest. When [with_baker_deamon = true], this distinction
     does not matter, as there are no published slots to attest during the
     period when `bake for` is used; the delegates will attest the published
     slot later, when the baker daemon is running. *)
  let* () =
    publish_and_bake
      ~init_level:first_level
      ~target_level:intermediary_level
      all_delegates
  in
  let* () =
    publish_and_bake
      ~init_level:(intermediary_level + 1)
      ~target_level:max_level
      (List.tl all_delegates)
  in
  let* () = wait_block_processing in
  let* first_not_attested_published_level =
    if with_baker_daemon then (
      let last_level_of_first_baker =
        intermediary_level + parameters.attestation_lag
      in
      let last_level_of_second_baker = max_level + parameters.attestation_lag in
      Log.info
        "Run the first baker for all delegates till at least level %d."
        last_level_of_first_baker ;
      let* () = run_baker all_delegates last_level_of_first_baker in
      (* (D) We tried to stop the baker as soon as it reaches
         [intermediary_level + attestation_lag], but it may have baked a few
         blocks more *)
      let node_level = Node.get_level node in
      let first_not_attested_published_level =
        node_level + 1 - parameters.attestation_lag
      in
      Log.info
        "The first baker baked till level %d. Therefore \
         first_not_attested_published_level is %d."
        node_level
        first_not_attested_published_level ;
      Log.info
        "Run the second baker for some (not all) delegates till at least level \
         %d."
        last_level_of_second_baker ;
      if first_not_attested_published_level >= max_level then
        Test.fail "test not checking for unattested slots; adjust parameters" ;
      let* () = run_baker (List.tl all_delegates) last_level_of_second_baker in
      return first_not_attested_published_level)
    else return intermediary_level
  in
  Log.info "Check the attestation status of the published slots." ;
  let rec check_attestations level =
    if level >= max_level then return ()
    else
      let* slot_headers =
        RPC.call dal_node (Rollup.Dal.RPC.get_published_level_headers level)
      in
      Check.(
        (1 = List.length slot_headers)
          int
          ~error_msg:"Expected a single header (got %R headers)") ;
      let Rollup.Dal.RPC.{slot_level; slot_index; status; _} =
        List.hd slot_headers
      in
      Check.((level = slot_level) int ~error_msg:"Expected level %L (got %R)") ;
      Check.(
        (slot_idx level = slot_index)
          int
          ~error_msg:"Expected index %L (got %R)") ;
      (if
       level < intermediary_level || level >= first_not_attested_published_level
      then
       (* We cannot know for sure the status of a slot between
          [intermediary_level] and
          [first_not_attested_published_level]. Before
          [intermediary_level], it should be [attested], and above
          [first_not_attested_published_level], it should be
          [unattested]. *)
       let expected_status =
         if level < intermediary_level then "attested" else "unattested"
       in
       Check.(
         (expected_status = status)
           string
           ~error_msg:"Expected status %L (got %R)")) ;
      check_attestations (level + 1)
  in
  check_attestations first_level

(** End-to-end DAL Tests.  *)

(* This function publishes the DAL slot whose index is [slot_index] for the
   levels between [from + beforehand_slot_injection] and
   [into + beforehand_slot_injection] with a payload equal to the slot's
   publish_level.

   The parameter [beforehand_slot_injection] (whose value should be at least
   one) allows, for a published level, to inject the slots an amount of blocks
   ahead, In other terms, we have:

   [published level = injection level + beforehand_slot_injection]

   The publication consists in sending the slot to a DAL node, computing its
   shards on the DAL node and associating it to a slot index and published
   level. On the L1 side, a manager operation publish_slot_header is also sent.

   Having the ability to inject some blocks ahead (thanks to
   [beforehand_slot_injection]) allows some pipelining, as shards computation
   may take an amount of time to complete.

   The function returns the sum of the payloads submitted via DAL.
*)
let slot_producer ?(beforehand_slot_injection = 1) ~slot_index ~slot_size ~from
    ~into dal_node l1_node l1_client =
  Check.(
    (beforehand_slot_injection >= 1)
      int
      ~error_msg:
        "Value of beforehand_slot_injection should be at least 1 (got %L)") ;
  (* We add the successive slots' contents (that is, published_level-s) injected
     into DAL and L1, and store the result in [!sum]. The final value is then
     returned by this function. *)
  let sum = ref 0 in
  let loop ~from ~into ~task =
    fold
      (into - from + 1)
      ()
      (fun index () ->
        let current_level = index + from in
        task current_level)
  in
  let publish_and_store_slot_promises = ref [] in
  (* This is the account used to sign injected slot headers on L1. *)
  let source = Constant.bootstrap2 in
  let* counter = Operation.get_next_counter ~source l1_client in
  let counter = ref counter in
  let task current_level =
    let* level = Node.wait_for_level l1_node current_level in
    (* We expected to advance level by level, otherwise, the test should fail. *)
    Check.(
      (current_level = level) int ~error_msg:"Expected level is %L (got %R)") ;
    let (publish_level as payload) = level + beforehand_slot_injection in
    sum := !sum + payload ;
    Log.info
      "[e2e.slot_producer] publish slot %d for level %d with payload %d at \
       level %d"
      slot_index
      publish_level
      payload
      level ;
    let promise =
      publish_and_store_slot
        ~slot_size
        ~force:true
        ~counter:!counter
        ~level:publish_level
        l1_node
        l1_client
        dal_node
        source
        slot_index
        (sf " %d " payload)
    in
    incr counter ;
    publish_and_store_slot_promises :=
      promise :: !publish_and_store_slot_promises ;
    unit
  in
  let* () = loop ~from ~into ~task in
  let l =
    List.map (fun p ->
        let* _p = p in
        unit)
    @@ List.rev !publish_and_store_slot_promises
  in
  let* () = Lwt.join l in
  Log.info "[e2e.slot_producer] will terminate" ;
  return !sum

(** Given a list of SORU node operators, this function allows to create and run new
    DAL and SORU nodes, where:
    - Each fresh rollup node is configured with a fresh DAL node and a key
    operator;
    - Each fresh DAL node is either connected to the original DAL node if the
    associated operator has an even index in the list of node operators, or to
    the previously generated DAL node otherwise.

    Each element of the [extra_nodes_operators] list can actually be [None], in
    which case the corresponding rollup node is launched in observer mode, or
    [Some account], in which case the [account] is used as the default
    operator of the SORU node. *)
let create_additional_nodes ~protocol ~extra_node_operators rollup_address
    l1_node l1_client dal_node =
  (* The mutable variable [connect_dal_node_to] below is used to diversify a bit
     the topology of the DAL nodes network as follows:

     [node with odd idx] -- [node with even idx] -- init_node

     So, we initialize its value to [dal_node]. *)
  let connect_dal_node_to = ref dal_node in
  Lwt_list.mapi_s
    (fun index key_opt ->
      (* We create a new DAL node and initialize it. *)
      let fresh_dal_node = Dal_node.create ~node:l1_node ~client:l1_client () in
      let* _config_file = Dal_node.init_config fresh_dal_node in

      (* We connect the fresh DAL node to another node, start it and update the
         value of [connect_dal_node_to] to generate the topology above: *)
      update_neighbors fresh_dal_node [!connect_dal_node_to] ;
      let* () = Dal_node.run fresh_dal_node in
      connect_dal_node_to :=
        if index mod 2 = 0 then fresh_dal_node else dal_node ;

      (* We create a new SORU node, connected to the new DAL node, and
         initialize it. *)
      let rollup_mode =
        Sc_rollup_node.(if Option.is_none key_opt then Observer else Operator)
      in
      let sc_rollup_node =
        Sc_rollup_node.create
          ~protocol
          ~dal_node:fresh_dal_node
          rollup_mode
          l1_node
          ~base_dir:(Client.base_dir l1_client)
          ?default_operator:key_opt
      in
      (* We start the rollup node and create a client for it. *)
      let* () = Sc_rollup_node.run sc_rollup_node rollup_address [] in
      let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in
      return (fresh_dal_node, sc_rollup_node, sc_rollup_client))
    extra_node_operators

(* This function allows to run an end-to-end test involving L1, DAL and rollup
   nodes. For that it:

   - It configures the PVM Arith's DAL parameters with those of L1 (via an inbox
   message). The PVM is asked to track slot index 5;

   - It starts a baker daemon with all available bootstrap delegates;

   - It starts a [slot_producer] Lwt promise, that will publish
   [number_of_dal_slots] slots on L1 and on the DAL node. The slots' content is
   basically the level at which they are published;

   - Once the slots are processed and attested; an inbox message is sent to the
   Arith PVM to sum all the received integers and to store the result in a
   variable called "value";

   - We finally check that there is a "value" variable in the PVM whose payload
   is the sum of levels as returned by [slot_producer].
*)
let e2e_test_script ?expand_test:_ ?(beforehand_slot_injection = 1)
    ?(extra_node_operators = []) protocol parameters dal_node sc_rollup_node
    sc_rollup_address l1_node l1_client _pvm_name ~number_of_dal_slots =
  let slot_size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let current_level = Node.get_level l1_node in
  Log.info "[e2e.startup] current level is %d@." current_level ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let sc_rollup_client = Sc_rollup_client.create ~protocol sc_rollup_node in

  (* Generate new DAL and rollup nodes if requested. *)
  let* additional_nodes =
    create_additional_nodes
      ~protocol
      ~extra_node_operators
      sc_rollup_address
      l1_node
      l1_client
      dal_node
  in
  Log.info
    "[e2e.configure_pvm] configure PVM with DAL parameters via inbox message@." ;
  let Rollup.Dal.Parameters.{attestation_lag; cryptobox; _} = parameters in
  (* We ask the Arith PVM of [_sc_rollup_address] to track the slot index 5 of
     the DAL. *)
  let tracked_slot_index = 5 in
  let* () =
    let number_of_slots = parameters.number_of_slots in
    let pages_per_slot = cryptobox.slot_size / cryptobox.page_size in
    let config =
      sf
        " dal:%d:%d:%d:%d "
        number_of_slots
        attestation_lag
        pages_per_slot
        tracked_slot_index
    in
    send_messages ~bake:false l1_client [config]
  in
  (* Argument ~keys:[] allows to bake with all available delegates. *)
  let* () = Client.bake_for_and_wait l1_client ~keys:[] in
  Log.info "[e2e.pvm] PVM Arith configured@." ;

  Log.info
    "[e2e.start_baker] spawn a baker daemon with all bootstrap accounts@." ;
  let* _baker = Baker.init ~dal_node ~protocol l1_node l1_client in

  (* To be sure that we just moved to [start_dal_slots_level], we wait and extra
     level. *)
  let* start_dal_slots_level =
    Node.wait_for_level l1_node (1 + Node.get_level l1_node)
  in
  let end_dal_slots_level = start_dal_slots_level + number_of_dal_slots - 1 in

  Log.info
    "[e2e.start_slot_producer] from level %d to level %d@."
    start_dal_slots_level
    end_dal_slots_level ;
  (* The slot producer will publish [number_of_dal_slots] sucessive slots of
     size [slot_size] with index [tracked_slots]. *)
  let* expected_value =
    slot_producer
      ~beforehand_slot_injection
      ~slot_index:tracked_slot_index
      ~from:start_dal_slots_level
      ~into:end_dal_slots_level
      ~slot_size
      dal_node
      l1_node
      l1_client
  in

  (* Wait until the last published slot is included and attested. *)
  let* _lvl =
    Node.wait_for_level
      l1_node
      (end_dal_slots_level + attestation_lag + beforehand_slot_injection)
  in

  Log.info
    "[e2e.sum_and_store] send an inbox messsage to the PVM to sum the received \
     payloads are store the result in a 'value' variable@." ;
  let* () =
    let level = Node.get_level l1_node in
    (* We send instructions "+...+ value" to the PVM of [number_of_dal_slots -
       1] additions, so that it sums the received slots' contents and store the
       result in a variable called "value". *)
    let do_sum = String.make (number_of_dal_slots - 1) '+' in
    let* () = send_messages ~bake:false l1_client [do_sum ^ " value"] in
    (* Wait sufficiently many levels so that the PVM interprets the message. *)
    let* _lvl = Node.wait_for_level l1_node (level + 2) in
    unit
  in
  Log.info
    "[e2e.final_check_1] check that the sum stored in the PVM is %d@."
    expected_value ;
  let* () =
    check_saved_value_in_pvm ~name:"value" ~expected_value sc_rollup_client
  in

  (* Check the statuses of the additional nodes/PVMs, if any. *)
  Log.info
    "[e2e.final_check_2] Check %d extra nodes@."
    (List.length additional_nodes) ;
  Lwt_list.iter_p
    (fun (_dal_node, _rollup_node, rollup_client) ->
      check_saved_value_in_pvm ~name:"value" ~expected_value rollup_client)
    additional_nodes

(** Register end-to-end DAL Tests. *)

(* These are the parameters we vary for end-to-end tests. *)
type e2e_test = {
  constants : Protocol.constants;
  attestation_lag : int;
  block_delay : int;
  number_of_dal_slots : int;
  beforehand_slot_injection : int;
  num_extra_nodes : int;
}

let e2e_tests =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4858

     Move tests that take time to long tests, in particular those with mainnet
     parameters. *)
  let test1 =
    {
      constants = Protocol.Constants_test;
      attestation_lag = 1;
      block_delay = 6;
      number_of_dal_slots = 2;
      beforehand_slot_injection = 1;
      num_extra_nodes = 2;
    }
  in
  let test2 =
    {
      constants = Protocol.Constants_test;
      attestation_lag = 2;
      block_delay = 2;
      number_of_dal_slots = 5;
      beforehand_slot_injection = 5;
      num_extra_nodes = 2;
    }
  in
  let mainnet1 =
    {
      constants = Protocol.Constants_mainnet;
      attestation_lag = 1;
      block_delay = 15;
      number_of_dal_slots = 1;
      beforehand_slot_injection = 1;
      num_extra_nodes = 1;
    }
  in
  let mainnet2 =
    {
      constants = Protocol.Constants_mainnet;
      attestation_lag = 3;
      block_delay = 5;
      number_of_dal_slots = 5;
      beforehand_slot_injection = 10;
      num_extra_nodes = 0;
    }
  in
  [test1; test2; mainnet1; mainnet2]

let constants_to_string = function
  | Protocol.Constants_mainnet -> "mainnet"
  | Protocol.Constants_sandbox -> "sandbox"
  | Protocol.Constants_test -> "testnet"

(* This function allows to register new (end-to-end) tests using
   [scenario_with_all_nodes] helper. For that, it instantiate function
   [e2e_test_script] with the various configurations in [e2e_tests]. *)
let register_end_to_end_tests ~protocols =
  (* (Mainnet) Tests below are probably flaky because they depend on CPU
     spec/time, as we start a baker daemon, and we set a low block time to avoid
     monopolizing the CI. If it is an issue, those tests could be moved to long
     tests in the future. *)
  List.iter
    (fun test ->
      let {
        constants;
        attestation_lag;
        block_delay;
        number_of_dal_slots;
        beforehand_slot_injection;
        num_extra_nodes;
      } =
        test
      in
      let network = constants_to_string constants in
      let title =
        sf
          "%s_lag-%d_time-%d_preinject-%d_slots-%d"
          network
          attestation_lag
          block_delay
          beforehand_slot_injection
          number_of_dal_slots
      in
      let activation_timestamp =
        (* and starts it in the past enough so that one can bake
           [expected_bake_for_blocks] with the bake for command, without hitting
           a "Block in the future" error *and* continue from there with baker
           daemons (which cannot bake in the past) without a hitch.

           In the test, we have two [bake_for] invocations: one for the rollup
           origination and another to configure the PVM. Baker daemon is then
           started. *)
        let expected_bake_for_occurrences = 2 in
        Ptime.Span.of_int_s (expected_bake_for_occurrences * block_delay)
      in
      (* Preparing the list of node operators for extra nodes. *)
      let extra_node_operators =
        (* So launch the extra SORU nodes in observer mode. So we don't actually
           need to provide public key hashes. *)
        List.init num_extra_nodes (fun _index -> None)
      in
      scenario_with_all_nodes
        ~custom_constants:constants
        ~attestation_lag
        ~activation_timestamp:(Ago activation_timestamp)
        ~minimal_block_delay:(string_of_int block_delay)
        ~tags:["e2e"; network]
        title
        (e2e_test_script
           ~number_of_dal_slots
           ~beforehand_slot_injection
           ~extra_node_operators)
        protocols)
    e2e_tests

let wait_for_gossipsub_worker_event ~name dal_node lambda =
  Dal_node.wait_for dal_node (sf "gossipsub_worker_event-%s.v0" name) lambda

let check_expected expected found = if expected <> found then None else Some ()

let ( let*?? ) a b = Option.bind a b

let check_new_connection_event ~main_node ~other_node ~is_outbound =
  wait_for_gossipsub_worker_event ~name:"new_connection" main_node (fun event ->
      let*?? () =
        check_expected
          JSON.(Dal_node.read_identity other_node |-> "peer_id" |> as_string)
          JSON.(event |-> "peer" |> as_string)
      in
      check_expected is_outbound JSON.(event |-> "outbound" |> as_bool))

let check_disconnection_event dal_node ~peer_id =
  wait_for_gossipsub_worker_event
    ~name:"disconnection"
    dal_node
    (fun peer_event -> check_expected peer_id JSON.(peer_event |> as_string))

type peer_id = string

type event_with_topic =
  | Subscribe of peer_id
  | Unsubscribe of peer_id
  | Graft of peer_id
  | Prune of peer_id
  | Join
  | Leave

let event_with_topic_to_string = function
  | Subscribe _ -> "subscribe"
  | Unsubscribe _ -> "unsubscribe"
  | Graft _ -> "graft"
  | Prune _ -> "prune"
  | Join -> "join"
  | Leave -> "leave"

(** This function monitors the Gossipsub worker events whose name is given by
    [event_with_topic].

    More precisely, since topics depend on a pkh and the number of DAL slots,
    this function monitors all the events {pkh; slot_index = 0} ... {pkh;
    slot_index = num_slots - 1}.

    Depending on the value of [event_with_topic], some extra checks, such as the
    peer id in case of Graft and Subscribe, are also done.
*)
let check_events_with_topic ~event_with_topic dal_node ~num_slots expected_pkh =
  let remaining = ref num_slots in
  let seen = Array.make num_slots false in
  let get_slot_index_opt event =
    let*?? topic =
      match event_with_topic with
      | Subscribe expected_peer
      | Unsubscribe expected_peer
      | Graft expected_peer
      | Prune expected_peer ->
          let*?? () =
            check_expected
              expected_peer
              JSON.(event |-> "peer" |> JSON.as_string)
          in
          Some JSON.(event |-> "topic")
      | Join | Leave -> Some event
    in
    let*?? () =
      check_expected expected_pkh JSON.(topic |-> "pkh" |> JSON.as_string)
    in
    Some JSON.(topic |-> "slot_index" |> as_int)
  in
  wait_for_gossipsub_worker_event
    dal_node
    ~name:(event_with_topic_to_string event_with_topic)
    (fun event ->
      let*?? slot_index = get_slot_index_opt event in
      Check.(
        (seen.(slot_index) = false)
          bool
          ~error_msg:
            (sf "Slot_index %d already seen. Invariant broken" slot_index)) ;
      seen.(slot_index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && Array.for_all (fun b -> b) seen then Some ()
      else None)

type event_with_message = Publish_message | Message_with_header of peer_id

let event_with_message_to_string = function
  | Publish_message -> "publish_message"
  | Message_with_header _ -> "message_with_header"

(** This function monitors the Gossipsub worker events whose name is given by
    [event_with_message]. It's somehow similar to function
    {!check_events_with_topic}, except that what varies here is the shard index
    instead of slot index. Moreover, shards do not necessiraly start from 0 or
    end at number_of_shards - 1. *)
let check_events_with_message ~event_with_message dal_node ~from_shard ~to_shard
    ~expected_commitment ~expected_level ~expected_pkh ~expected_slot =
  let remaining = ref (to_shard - from_shard + 1) in
  let seen = Array.make !remaining false in
  let get_shard_index_opt event =
    let topic_slot_index =
      JSON.(event |-> "topic" |-> "slot_index" |> as_int)
    in
    let topic_pkh = JSON.(event |-> "topic" |-> "pkh" |> as_string) in
    let level = JSON.(event |-> "message_id" |-> "level" |> as_int) in
    let slot_index = JSON.(event |-> "message_id" |-> "slot_index" |> as_int) in
    let shard_index =
      JSON.(event |-> "message_id" |-> "shard_index" |> as_int)
    in
    let pkh = JSON.(event |-> "message_id" |-> "pkh" |> as_string) in
    let commitment =
      JSON.(event |-> "message_id" |-> "commitment" |> as_string)
    in
    let*?? () = check_expected expected_pkh topic_pkh in
    let*?? () = check_expected expected_pkh pkh in
    let*?? () = check_expected expected_level level in
    let*?? () = check_expected expected_slot slot_index in
    let*?? () = check_expected expected_slot topic_slot_index in
    let*?? () = check_expected expected_commitment commitment in
    let*?? () =
      match event_with_message with
      | Publish_message -> Some ()
      | Message_with_header expected_peer_id ->
          check_expected expected_peer_id JSON.(event |-> "peer" |> as_string)
    in
    Some shard_index
  in
  wait_for_gossipsub_worker_event
    dal_node
    ~name:(event_with_message_to_string event_with_message)
    (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      let index = shard_index - from_shard in
      Check.(
        (seen.(index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && Array.for_all (fun b -> b) seen then Some ()
      else None)

type event_with_message_id = IHave of {pkh : string; slot_index : int} | IWant

let event_with_message_id_to_string = function
  | IHave _ -> "ihave"
  | IWant -> "iwant"

(** This function monitors the Gossipsub worker events whose name is given by
    [event_with_message_id]. It's somehow similar to function
    {!check_events_with_message}, but for IHave and IWant messages' events. *)
let check_events_with_message_id ~event_with_message_id dal_node ~from_shard
    ~to_shard ~expected_commitment ~expected_level ~expected_pkh ~expected_slot
    ~expected_peer =
  let remaining = ref (to_shard - from_shard + 1) in
  let seen = Array.make !remaining false in
  let get_shard_indices_of_messages event =
    let*?? () =
      check_expected expected_peer JSON.(event |-> "peer" |> as_string)
    in
    let*?? () =
      match event_with_message_id with
      | IWant -> Some ()
      | IHave {pkh; slot_index} ->
          let topic = JSON.(event |-> "topic") in
          let*?? () = check_expected pkh expected_pkh in
          let*?? () = check_expected pkh JSON.(topic |-> "pkh" |> as_string) in
          check_expected slot_index JSON.(topic |-> "slot_index" |> as_int)
    in
    let message_ids = JSON.(event |-> "message_ids" |> as_list) in
    Option.some
    @@ List.map
         (fun id ->
           let level = JSON.(id |-> "level" |> as_int) in
           let slot_index = JSON.(id |-> "slot_index" |> as_int) in
           let shard_index = JSON.(id |-> "shard_index" |> as_int) in
           let pkh = JSON.(id |-> "pkh" |> as_string) in
           let commitment = JSON.(id |-> "commitment" |> as_string) in
           let*?? () = check_expected expected_pkh pkh in
           let*?? () = check_expected expected_level level in
           let*?? () = check_expected expected_slot slot_index in
           let*?? () = check_expected expected_commitment commitment in
           Some shard_index)
         message_ids
  in
  wait_for_gossipsub_worker_event
    ~name:(event_with_message_id_to_string event_with_message_id)
    dal_node
    (fun event ->
      let*?? shard_indices = get_shard_indices_of_messages event in
      List.iter
        (fun shard_index_opt ->
          match shard_index_opt with
          | None -> ()
          | Some shard_index ->
              let index = shard_index - from_shard in
              Check.(
                (seen.(index) = false)
                  bool
                  ~error_msg:
                    (sf
                       "Shard_index %d already seen. Invariant broken"
                       shard_index)) ;
              seen.(index) <- true ;
              decr remaining)
        shard_indices ;
      if !remaining = 0 && Array.for_all (fun b -> b) seen then Some ()
      else None)

(** This function is quite similar to those above, except that it checks that a
    range of messages (shards) on a tracked topic have been notified by GS to
    the DAL node. This is typically needed to then be able to attest slots. *)
let check_message_notified_to_app_event dal_node ~from_shard ~to_shard
    ~expected_commitment ~expected_level ~expected_pkh ~expected_slot =
  let remaining = ref (to_shard - from_shard + 1) in
  let seen = Array.make !remaining false in
  let get_shard_index_opt event =
    let level = JSON.(event |-> "level" |> as_int) in
    let slot_index = JSON.(event |-> "slot_index" |> as_int) in
    let shard_index = JSON.(event |-> "shard_index" |> as_int) in
    let pkh = JSON.(event |-> "pkh" |> as_string) in
    let commitment = JSON.(event |-> "commitment" |> as_string) in
    let*?? () = check_expected expected_pkh pkh in
    let*?? () = check_expected expected_level level in
    let*?? () = check_expected expected_slot slot_index in
    let*?? () = check_expected expected_commitment commitment in
    Some shard_index
  in
  Dal_node.wait_for
    dal_node
    "gossipsub_transport_event-message_notified_to_app.v0"
    (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      let index = shard_index - from_shard in
      Check.(
        (seen.(index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && Array.for_all (fun b -> b) seen then Some ()
      else None)

(** Connect [dal_node1] and [dal_node2] using the bootstrap peer mechanism.
    [dal_node2] will use [dal_node1] as a bootstrap peer.

    For this to work, [dal_node1] must already be running. *)
let connect_nodes_via_p2p dal_node1 dal_node2 =
  let* _config_file =
    Dal_node.init_config ~peers:[Dal_node.listen_addr dal_node1] dal_node2
  in
  (* We ensure that [dal_node1] connects to [dal_node2]. *)
  let conn_ev_in_node1 =
    check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node2
      ~is_outbound:false
  in
  let* () = Dal_node.run dal_node2 in
  conn_ev_in_node1

(** This helper function makes the nodes [dal_node1] and [dal_node2] join the
    topics of the attestor [pkh], by calling the RPC for tracking the corresponding profile.
    The second node calls the RPC only after receiving the Subscribe messages
    from the first node, so that when it joins the topics, it also sends Graft messages
    in addition to sending Subscribe messages. *)
let nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 =
  let profile1 = Rollup.Dal.RPC.Attestor pkh1 in
  let peer_id1 =
    JSON.(Dal_node.read_identity dal_node1 |-> "peer_id" |> as_string)
  in
  let peer_id2 =
    JSON.(Dal_node.read_identity dal_node2 |-> "peer_id" |> as_string)
  in
  (* node1 joins topic {pkh} -> it sends subscribe messages to node2. *)
  let event_waiter =
    check_events_with_topic
      ~event_with_topic:(Subscribe peer_id1)
      dal_node2
      ~num_slots
      pkh1
  in
  let* () = RPC.call dal_node1 (Rollup.Dal.RPC.patch_profiles [profile1]) in
  let* () = event_waiter in

  (* node2 joins topic {pkh} -> it sends subscribe and graft messages to
     node1. *)
  let event_waiter_subscribe =
    check_events_with_topic
      ~event_with_topic:(Subscribe peer_id2)
      dal_node1
      ~num_slots
      pkh1
  in
  let event_waiter_graft =
    check_events_with_topic
      ~event_with_topic:(Graft peer_id2)
      dal_node1
      ~num_slots
      pkh1
  in
  let* () = RPC.call dal_node2 (Rollup.Dal.RPC.patch_profiles [profile1]) in
  Lwt.join [event_waiter_subscribe; event_waiter_graft]

(** This helper returns the list of promises that allow to wait for the
    publication of a slot's shards into the Gossipsub layer.

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level].
*)
let waiters_publish_shards l1_committee dal_node commitment ~publish_level
    ~slot_index =
  let open Rollup.Dal.Committee in
  List.map
    (fun {attestor; first_shard_index; power} ->
      check_events_with_message
        ~event_with_message:Publish_message
        dal_node
        ~from_shard:first_shard_index
        ~to_shard:(first_shard_index + power - 1)
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attestor
        ~expected_slot:slot_index)
    l1_committee

(** This helper returns the promise that allows to wait for the reception of
    messages of [slot_index] published at level [publish_level] by the attestor
    [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_receive_shards l1_committee dal_node commitment ~publish_level
    ~slot_index ~pkh ~from_peer =
  let open Rollup.Dal.Committee in
  match List.find (fun {attestor; _} -> attestor = pkh) l1_committee with
  | {attestor; first_shard_index; power} ->
      check_events_with_message
        ~event_with_message:(Message_with_header from_peer)
        dal_node
        ~from_shard:first_shard_index
        ~to_shard:(first_shard_index + power - 1)
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attestor
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

(** This helper returns the promise that allows to wait for the successful
    notification of messages of [slot_index] published at level [publish_level]
    to the app layer of the attestor [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_successful_shards_app_notification l1_committee dal_node commitment
    ~publish_level ~slot_index ~pkh =
  let open Rollup.Dal.Committee in
  match List.find (fun {attestor; _} -> attestor = pkh) l1_committee with
  | {attestor; first_shard_index; power} ->
      check_message_notified_to_app_event
        dal_node
        ~from_shard:first_shard_index
        ~to_shard:(first_shard_index + power - 1)
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attestor
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

let test_dal_node_p2p_connection_and_disconnection _protocol _parameters
    _cryptobox node client dal_node1 =
  let dal_node2 = Dal_node.create ~node ~client () in
  (* Connect the nodes *)
  let* () = connect_nodes_via_p2p dal_node1 dal_node2 in
  let peer_id =
    JSON.(Dal_node.read_identity dal_node2 |-> "peer_id" |> as_string)
  in
  (* kill dal_node2 and check "disconnection" event in node1. *)
  let disconn_ev_in_node1 = check_disconnection_event dal_node1 ~peer_id in
  let* () = Dal_node.kill dal_node2 in
  disconn_ev_in_node1

let test_dal_node_join_topic _protocol _parameters _cryptobox _node client
    dal_node1 =
  let pkh1 = Constant.bootstrap1.public_key_hash in
  let profile1 = Rollup.Dal.RPC.Attestor pkh1 in
  let* params = Rollup.Dal.Parameters.from_client client in
  let num_slots = params.number_of_slots in
  let event_waiter =
    check_events_with_topic ~event_with_topic:Join dal_node1 ~num_slots pkh1
  in
  let* () = RPC.call dal_node1 (Rollup.Dal.RPC.patch_profiles [profile1]) in
  event_waiter

(** This generic test function is used to test messages exchanges between two
    DAL nodes via the P2P/GS layers, once connections are established and topics
    are joined.

   The [mk_dal_node2] function is used to create a second DAL node. We may
   decide to create a regular/normal or a modified dal node.

   The [expect_app_notification] flag is used to tell whether we should wait for
   the application layer of the second DAL node to be notified with received messages.
   In case we don't expect the application layer to be notified (e.g. messages are invalid),
   set to [false].

   The [is_first_slot_attestable] flag is used to tell whether the first slot
   (that has been injected by this function) can be attested by the considered
   attestor or not. In particular, it should be set to [false] if the
   application layer of the second DAL node was not notified about the messages
   sent by the first DAL node.
*)
let generic_gs_messages_exchange protocol parameters _cryptobox node client
    dal_node1 ~mk_dal_node2 ~expect_app_notification ~is_first_slot_attestable =
  let* dal_node2 = mk_dal_node2 protocol parameters in

  let* () = connect_nodes_via_p2p dal_node1 dal_node2 in

  let* params = Rollup.Dal.Parameters.from_client client in
  let num_slots = params.number_of_slots in
  let account1 = Constant.bootstrap1 in
  let pkh1 = account1.public_key_hash in
  (* The two nodes join the same topics *)
  let* () = nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 in

  (* Posting a DAL message to DAL node and to L1 *)
  let crypto_params = parameters.Rollup.Dal.Parameters.cryptobox in
  let* slot_index, slot_commitment =
    let slot_size = crypto_params.slot_size in
    let slot_content = generate_dummy_slot slot_size in
    publish_and_store_slot
      ~with_proof:true
      node
      client
      dal_node1
      ~slot_size
      Constant.bootstrap1
      0
      slot_content
  in

  (* Preparing event waiters for different shards that will be published by
     dal_node1 once the slot header is included in an L1 block.

     We also prepare a waiter event for:
     - the shards that will be received by dal_node2 on its topics;
     - the messages (shards) that will be notified to dal_node2 on its topic. *)
  let publish_level = Node.get_level node + 1 in
  let attested_level = publish_level + parameters.attestation_lag in
  let* committee = Rollup.Dal.Committee.at_level node ~level:attested_level in

  let waiter_publish_list =
    waiters_publish_shards
      committee
      dal_node1
      slot_commitment
      ~publish_level
      ~slot_index
  in
  let waiter_receive_shards =
    waiter_receive_shards
      committee
      dal_node2
      slot_commitment
      ~publish_level
      ~slot_index
      ~pkh:pkh1
      ~from_peer:
        JSON.(Dal_node.read_identity dal_node1 |-> "peer_id" |> as_string)
  in
  let waiter_app_notifs =
    if expect_app_notification then
      waiter_successful_shards_app_notification
        committee
        dal_node2
        slot_commitment
        ~publish_level
        ~slot_index
        ~pkh:pkh1
    else unit
  in

  (* We bake a block that includes [slot_header] operation. *)
  let* () = Client.bake_for_and_wait client in
  let* () = Lwt.join waiter_publish_list
  and* () = waiter_receive_shards
  and* () = waiter_app_notifs in

  (* Check that dal_node2 has the shards needed by attestor account1/pkh1 to
     attest the slot with index 0. *)
  let* res =
    RPC.call
      dal_node2
      (Rollup.Dal.RPC.get_attestable_slots ~attestor:account1 ~attested_level)
  in
  match res with
  | Not_in_committee -> Test.fail "attestor %s not in committee" account1.alias
  | Attestable_slots slots ->
      (* only slot 0 is attestable. Others are set to false. *)
      let expected =
        is_first_slot_attestable :: List.init (num_slots - 1) (fun _ -> false)
      in
      Check.(
        (expected = slots)
          (list bool)
          ~error_msg:"Expected %L attestable slots list flags, got %R") ;
      unit

let test_dal_node_gs_valid_messages_exchange _protocol parameters _cryptobox
    node client dal_node1 =
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    dal_node1
    ~mk_dal_node2:(fun _protocol _parameters ->
      Dal_node.create ~node ~client () |> return)
    ~expect_app_notification:true
    ~is_first_slot_attestable:true

(* Create a DAL node whose DAL parameters are not compatible with those in
   [parameters]. For that, the redundancy_factor field is multiplied by 2. *)
let make_invalid_dal_node protocol parameters =
  (* Create another L1 node with different DAL parameters. *)
  let* node2, client2, _xdal_parameters2 =
    let crypto_params = parameters.Rollup.Dal.Parameters.cryptobox in
    let parameters =
      dal_enable_param (Some true)
      @ redundancy_factor_param (Some (2 * crypto_params.redundancy_factor))
    in
    setup_node ~protocol ~parameters ()
  in
  (* Create a second DAL node with node2 and client2 as argument (so different
     DAL parameters compared to dal_node1. *)
  let dal_node2 = Dal_node.create ~node:node2 ~client:client2 () in
  return dal_node2

let test_dal_node_gs_invalid_messages_exchange _protocol parameters _cryptobox
    node client dal_node1 =
  (* Messages are invalid, so the app layer is not notified. *)
  let expect_app_notification = false in
  (* The first slot published by [generic_gs_messages_exchange] is not
     attestable by the considered attestor pk1 = bootstrap1, because the shards
     received by the Gossipsub layer are classified as 'Invalid'. *)
  let is_first_slot_attestable = false in
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    dal_node1
    ~mk_dal_node2:make_invalid_dal_node
    ~expect_app_notification
    ~is_first_slot_attestable

let _test_gs_prune_ihave_and_iwant protocol parameters _cryptobox node client
    dal_node1 =
  let rec repeat_i n f =
    if n <= 0 then unit
    else
      let* () = f n in
      repeat_i (n - 1) f
  in
  let crypto_params = parameters.Rollup.Dal.Parameters.cryptobox in
  let slot_size = crypto_params.slot_size in
  let slot_content = generate_dummy_slot slot_size in

  (* Inject as much slots as possible with available bootstrap accounts.
     The goal is to continuously send invalid messages from dal_node1 to dal_node2,
     thus lowering the score of dal_node1 to the point where it becomes negative. *)
  let* () =
    let num_slots =
      min
        (Array.length Account.Bootstrap.keys)
        parameters.Rollup.Dal.Parameters.number_of_slots
    in
    repeat_i num_slots (fun i ->
        let slot_index = i - 1 in
        let account = Account.Bootstrap.keys.(slot_index) in
        let* _slot_index, _slot_commitment =
          publish_and_store_slot
            ~with_proof:true
            node
            client
            dal_node1
            ~slot_size
            account
            slot_index
            slot_content
        in
        unit)
  in

  (* Create another (invalid) DAL node *)
  let* dal_node2 = make_invalid_dal_node protocol parameters in

  (* Connect the nodes *)
  let* () = connect_nodes_via_p2p dal_node1 dal_node2 in

  let* params = Rollup.Dal.Parameters.from_client client in
  let num_slots = params.number_of_slots in
  let account1 = Constant.bootstrap1 in
  let pkh1 = account1.public_key_hash in

  (* The two nodes join the same topics *)
  let* () = nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 in

  let peer_id1 =
    JSON.(Dal_node.read_identity dal_node1 |-> "peer_id" |> as_string)
  in
  let peer_id2 =
    JSON.(Dal_node.read_identity dal_node2 |-> "peer_id" |> as_string)
  in
  (* Once a block is baked and shards injected into GS, we expect dal_node1 to
     be pruned by dal_node2 because its score will become negative due to
     invalid messages. *)
  let event_waiter_prune =
    check_events_with_topic
      ~event_with_topic:(Prune peer_id2)
      dal_node1
      ~num_slots
      pkh1
  in

  (* We bake a block and wait for the prune events. *)
  let* () = Client.bake_for_and_wait client in
  let* () = event_waiter_prune in

  (* Now, we'll inject a new slot for the next published_level in
     dal_node1. Since it's pruned, dal_node2 will be notified via IHave
     messages, to which it will respond by IWant messages. *)
  let* slot_index, commitment =
    publish_and_store_slot
      ~with_proof:true
      node
      client
      dal_node1
      ~slot_size
      account1
      0
      slot_content
  in

  let publish_level = Node.get_level node + 1 in
  let attested_level = publish_level + parameters.attestation_lag in
  let* committee = Rollup.Dal.Committee.at_level node ~level:attested_level in

  let Rollup.Dal.Committee.{attestor; first_shard_index; power} =
    match
      List.find
        (fun Rollup.Dal.Committee.{attestor; _} -> attestor = pkh1)
        committee
    with
    | {attestor; first_shard_index; power} ->
        {attestor; first_shard_index; power}
    | exception Not_found ->
        Test.fail "Should not happen as %s is part of the committee" pkh1
  in
  let iwant_events_waiter =
    check_events_with_message_id
      ~event_with_message_id:IWant
      dal_node1
      ~from_shard:first_shard_index
      ~to_shard:(first_shard_index + power - 1)
      ~expected_commitment:commitment
      ~expected_level:publish_level
      ~expected_pkh:attestor
      ~expected_slot:slot_index
      ~expected_peer:peer_id2
  in
  let ihave_events_waiter =
    check_events_with_message_id
      ~event_with_message_id:(IHave {pkh = pkh1; slot_index = 0})
      dal_node2
      ~from_shard:first_shard_index
      ~to_shard:(first_shard_index + power - 1)
      ~expected_commitment:commitment
      ~expected_level:publish_level
      ~expected_pkh:attestor
      ~expected_slot:slot_index
      ~expected_peer:peer_id1
  in
  let* () = Client.bake_for_and_wait client in
  let* () = iwant_events_waiter and* () = ihave_events_waiter in
  unit

let test_baker_registers_profiles protocol _parameters _cryptobox l1_node client
    dal_node =
  let delegates =
    List.to_seq Constant.all_secret_keys |> Seq.take 3 |> List.of_seq
  in
  let profiles =
    List.map
      (fun key -> Rollup.Dal.RPC.Attestor key.Account.public_key_hash)
      delegates
  in
  let delegates = List.map (fun key -> key.Account.alias) delegates in
  let* _baker = Baker.init ~dal_node ~protocol l1_node client ~delegates in
  (* [Baker.init] ensures that the baker daemon "started", but this event is
     emitted before the baker builds its initial stated and registers the
     profiles. That's why we wait a bit more, namely for a block to be baked. *)
  let* _lvl = Node.wait_for_level l1_node 2 in
  check_profiles ~__LOC__ dal_node ~expected:profiles

let register ~protocols =
  (* Tests with Layer1 node only *)
  scenario_with_layer1_node
    "dal basic logic"
    test_slot_management_logic
    protocols ;
  scenario_with_layer1_node
    ~attestation_lag:5
    "slots attestation operation behavior"
    test_slots_attestation_operation_behavior
    protocols ;
  scenario_with_layer1_node
    "slots attestation operation dal committee membership check"
    test_slots_attestation_operation_dal_committee_membership_check
    (* We need to set the prevalidator's event level to [`Debug]
       in order to capture the errors thrown in the validation phase. *)
    ~event_sections_levels:[("prevalidator", `Debug)]
    protocols ;
  scenario_with_layer1_node
    ~dal_enable:false
    "feature_flag_is_disabled"
    test_feature_flag
    protocols ;
  scenario_with_layer1_node
    "one_committee_per_epoch"
    test_one_committee_per_epoch
    protocols ;

  (* Tests with layer1 and dal nodes *)
  test_dal_node_startup protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node slot management"
    test_dal_node_slot_management
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node slot headers tracking"
    test_dal_node_slots_headers_tracking
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node shard fetching and slot reconstruction"
    test_dal_node_rebuild_from_shards
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node slots propagation"
    test_dal_node_test_slots_propagation
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node POST /commitments"
    test_dal_node_test_post_commitments
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node PATCH /commitments"
    test_dal_node_test_patch_commitments
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET /commitments/<commitment>/slot"
    test_dal_node_test_get_commitment_slot
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET /commitments/<commitment>/proof"
    test_dal_node_test_get_commitment_proof
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node PATCH+GET /profiles"
    test_dal_node_test_patch_profile
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
    test_dal_node_get_assigned_shard_indices
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/attestable_slots"
    test_dal_node_get_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~attestation_threshold:100
    "dal attestor with bake for"
    (test_attestor ~with_baker_daemon:false)
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~attestation_threshold:100
    ~attestation_lag:8
    ~activation_timestamp:Now
    "dal attestor with baker daemon"
    (test_attestor ~with_baker_daemon:true)
    protocols ;

  (* Tests with layer1 and dal nodes (with p2p/GS) *)
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    "GS/P2P connection and disconnection"
    test_dal_node_p2p_connection_and_disconnection
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    "GS join topic"
    test_dal_node_join_topic
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    "GS valid messages exchange"
    test_dal_node_gs_valid_messages_exchange
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    "GS invalid messages exchange"
    test_dal_node_gs_invalid_messages_exchange
    protocols ;
  (* Will be re-enabled once the debug in
     https://gitlab.com/tezos/tezos/-/merge_requests/9032 allows explaning the
     issue and how to fix
     scenario_with_layer1_and_dal_nodes
       ~tags:["gossipsub"]
       "GS prune due to negative score, ihave and iwant"
       test_gs_prune_ihave_and_iwant
       protocols ;
  *)
  scenario_with_layer1_and_dal_nodes
    "baker registers profiles with dal node"
    ~activation_timestamp:Now
    test_baker_registers_profiles
    protocols ;

  (* Tests with all nodes *)
  scenario_with_all_nodes
    "rollup_node_downloads_slots"
    rollup_node_stores_dal_slots
    protocols ;
  scenario_with_all_nodes
    "rollup_node_applies_dal_pages"
    (rollup_node_stores_dal_slots ~expand_test:rollup_node_interprets_dal_pages)
    protocols ;

  (* Register end-to-end tests *)
  register_end_to_end_tests ~protocols
