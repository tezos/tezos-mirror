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

let test ~__FILE__ ?(tags = []) ?supports title f =
  let tags = "dal" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags ?supports f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "dal" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

let dal_enable_param dal_enable =
  make_bool_parameter ["dal_parametric"; "feature_enable"] dal_enable

(* Some initialization functions to start needed nodes. *)

let setup_node ~parameter_file ~protocol =
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
  Node.Config_file.update node (fun json ->
      let value =
        JSON.annotate
          ~origin:"dal_initialisation"
          (`O
            [
              ( "srs_size",
                `Float (float_of_int dal_parameters.cryptobox.slot_size) );
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
  return (node, client, dal_parameters)

let with_layer1 ?(attestation_lag = 1) ?commitment_period ?challenge_window
    ?dal_enable f ~protocol =
  let parameters =
    make_int_parameter
      ["dal_parametric"; "attestation_lag"]
      (Some attestation_lag)
    @ make_int_parameter
        ["sc_rollup_commitment_period_in_blocks"]
        commitment_period
    @ make_int_parameter
        ["sc_rollup_challenge_window_in_blocks"]
        challenge_window
    (* this will produce the empty list if dal_enable is not passed to the function invocation,
       hence the value from the protocol constants will be used. *)
    @ dal_enable_param dal_enable
    @ [(["sc_rollup_enable"], `Bool true)]
  in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let* node, client, dal_parameters = setup_node ~parameter_file ~protocol in
  let cryptobox = Rollup.Dal.make dal_parameters.cryptobox in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f dal_parameters cryptobox node client bootstrap1_key

let with_fresh_rollup ?(pvm_name = "arith") ?dal_node f tezos_node tezos_client
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
      ?dal_node
      Operator
      tezos_node
      tezos_client
      ~default_operator:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node rollup_address
  in
  let* () =
    match dal_node with
    | None -> return ()
    | Some dal_node ->
        let* () = Dal_node.terminate dal_node in
        let reveal_data_dir =
          Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name
        in
        let* () = Dal_node.Dac.set_parameters ~reveal_data_dir dal_node in
        Dal_node.run dal_node ~wait_ready:true
  in
  let* () = Client.bake_for_and_wait tezos_client in
  f rollup_address sc_rollup_node configuration_filename

let with_dal_node tezos_node tezos_client f key =
  let dal_node = Dal_node.create ~node:tezos_node ~client:tezos_client () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = Dal_node.run dal_node ~wait_ready:true in
  f key dal_node

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)
let scenario_with_layer1_node ?(tags = ["dal"; "layer1"]) ?attestation_lag
    ?commitment_period ?challenge_window ?(dal_enable = true) variant scenario =
  let description = "Testing DAL L1 integration" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?attestation_lag
        ?commitment_period
        ?challenge_window
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      scenario protocol parameters cryptobox node client)

let scenario_with_layer1_and_dal_nodes ?(tags = ["dal"; "layer1"])
    ?attestation_lag ?commitment_period ?challenge_window ?(dal_enable = true)
    variant scenario =
  let description = "Testing DAL node" in
  test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?attestation_lag
        ?commitment_period
        ?challenge_window
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      with_dal_node node client @@ fun _key dal_node ->
      scenario protocol parameters cryptobox node client dal_node)

let scenario_with_all_nodes ?(tags = ["dal"; "dal_node"]) ?(pvm_name = "arith")
    ?(dal_enable = true) ?commitment_period ?challenge_window variant scenario =
  let description = "Testing DAL rollup and node with L1" in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1 ?commitment_period ?challenge_window ~protocol ~dal_enable
      @@ fun _parameters _cryptobox node client ->
      with_dal_node node client @@ fun key dal_node ->
      ( with_fresh_rollup ~pvm_name ~dal_node
      @@ fun sc_rollup_address sc_rollup_node _filename ->
        scenario
          protocol
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

let wait_for_stored_slot dal_node slot_header =
  Dal_node.wait_for dal_node "stored_slot_shards.v0" (fun e ->
      if JSON.(e |-> "commitment" |> as_string) = slot_header then Some ()
      else None)

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

let publish_slot ~source ?level ?fee ?error ~index ~commitment ~proof node
    client =
  let level =
    match level with Some level -> level | None -> 1 + Node.get_level node
  in
  Operation.Manager.(
    inject
      ?error
      [
        make ~source ?fee
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
  let error =
    rex ~opts:[`Dotall] "The slot header's commitment proof does not check"
  in
  publish_slot ~source ?level ?fee ~error ~index ~commitment ~proof

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
  let error =
    rex ~opts:[`Dotall] "The slot header's commitment proof does not check"
  in
  publish_slot ~source ?level ?fee ~error ~index ~commitment ~proof

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
  Operation.Consensus.(
    inject ~force ~signer (dal_attestation ~level ~attestation) client)

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
  let* (`OpHash _) =
    let error =
      rex ~opts:[`Dotall] "Unexpected level in the future in slot header"
    in
    publish_dummy_slot
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:3
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
      ~level:1
      ~index:2
      ~message:"a"
      ~error
      cryptobox
      node
      client
  in
  let* (`OpHash _) =
    publish_dummy_slot_with_wrong_proof_for_same_content
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:2
      ~index:2
      cryptobox
      node
      client
  in
  let* (`OpHash _) =
    publish_dummy_slot_with_wrong_proof_for_different_slot_size
      ~source:Constant.bootstrap5
      ~fee:3_000
      ~level:2
      ~index:2
      parameters
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
  let nb_slots = parameters.Rollup.Dal.Parameters.number_of_slots in
  let* _ =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap1 [1; 0] client
  in
  let* _ =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap2 [1; 0] client
  in
  let* _ = dal_attestation ~nb_slots ~signer:Constant.bootstrap3 [1] client in
  let* _ = dal_attestation ~nb_slots ~signer:Constant.bootstrap4 [1] client in
  let* _ = dal_attestation ~nb_slots ~signer:Constant.bootstrap5 [1] client in
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
  let applied = [h3] in
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated; applied} in
  let* (`OpHash h4) = attest ~level:(now + 2) in
  let branch_delayed = [h4] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
  in
  let* () = Client.bake_for_and_wait client in
  let applied = [h4] in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
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
  let applied = h5 :: applied in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
  in
  let* () = Client.bake_for_and_wait client in
  let now = Node.get_level node in
  let* attestation_ops =
    let open Constant in
    let level = now + lag in
    Lwt_list.map_s
      (fun signer ->
        let* (`OpHash h) =
          dal_attestation ~force:true ~nb_slots ~level ~signer [10] client
        in
        return h)
      [bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5]
  in
  let applied = [] in
  let branch_delayed = attestation_ops in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
  in
  let* () = repeat (lag - 1) (fun () -> Client.bake_for_and_wait client) in
  let applied = attestation_ops in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
  in
  let* () = check_slots_availability ~__LOC__ ~attested:[] in
  let* () = Client.bake_for_and_wait client in
  let applied = [] in
  let branch_delayed = [] in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; applied; branch_delayed}
  in
  check_slots_availability ~__LOC__ ~attested:[10]

let split_slot node client content =
  let* slot = Rollup.Dal.make_slot content client in
  RPC.call node (Rollup.Dal.RPC.split_slot slot)

let test_dal_node_slot_management _protocol _parameters _cryptobox _node client
    dal_node =
  let slot_content = "test with invalid UTF-8 byte sequence \xFA" in
  let* slot_commitment, _proof = split_slot dal_node client slot_content in
  let* received_slot =
    RPC.call dal_node (Rollup.Dal.RPC.slot_content slot_commitment)
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

let publish_and_store_slot ?(fee = 1_200) node client dal_node source index
    content =
  let* slot_commitment, proof = split_slot dal_node client content in
  let commitment =
    Cryptobox.Commitment.of_b58check_opt slot_commitment
    |> mandatory "The b58check-encoded slot commitment is not valid"
  in
  let proof =
    Data_encoding.Json.destruct
      Cryptobox.Commitment_proof.encoding
      (`String proof)
  in
  let* _ = publish_slot ~source ~fee ~index ~commitment ~proof node client in
  return (index, slot_commitment)

let test_dal_node_slots_headers_tracking _protocol _parameters _cryptobox node
    client dal_node =
  let publish ?fee = publish_and_store_slot ?fee node client dal_node in
  let* slot0 = publish Constant.bootstrap1 0 "test0" in
  let* slot1 = publish Constant.bootstrap2 1 "test1" in
  let* slot2_a = publish Constant.bootstrap3 4 ~fee:1_200 "test4_a" in
  let* slot2_b = publish Constant.bootstrap4 4 ~fee:1_200 "test4_b" in
  let* slot2_c = publish Constant.bootstrap5 4 ~fee:1_350 "test4_c" in
  (* slot2_a and slot2_b will be included as failed, slot2_c has better fees for
     slot 4. We decide to have two failed slots instead of just one to better
     test some internal aspects of failed slots headers recoding (i.e. having a
     collection of data instead of just one). *)
  ignore slot2_a ;
  ignore slot2_b ;
  (* TODO: https://gitlab.com/tezos/tezos/-/merge_requests/7049
     Retrieve successfull & failed slots with GET /slots/<commitment>/headers
     in MR !7049. *)
  let* () = Client.bake_for_and_wait client in
  let* _level = Node.wait_for_level node 1 in
  let* block = RPC.call node (RPC.get_chain_block_hash ()) in
  let* slot_headers =
    RPC.call dal_node (Rollup.Dal.RPC.stored_slot_headers block)
  in
  Check.(slot_headers = [slot0; slot1; slot2_c])
    Check.(list (tuple2 int string))
    ~error_msg:
      "Published header is different from stored header (current = %L, \
       expected = %R)" ;
  return ()

let generate_dummy_slot slot_size =
  String.init slot_size (fun i ->
      match i mod 3 with 0 -> 'a' | 1 -> 'b' | _ -> 'c')

let test_dal_node_rebuild_from_shards _protocol _parameters _cryptobox node
    client dal_node =
  (* Steps in this integration test:
     1. Run a dal node
     2. Generate and publish a full slot, then bake
     3. Download exactly 1/redundancy_factor shards
        from this slot (it would work with more)
     4. Ensure we can rebuild the original data using the above shards
  *)
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let crypto_params = parameters.cryptobox in
  let slot_content = generate_dummy_slot crypto_params.slot_size in
  let publish = publish_and_store_slot node client dal_node in
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
  let shards =
    List.fold_left
      (fun acc shard ->
        let shard =
          match Data_encoding.Json.from_string shard with
          | Ok s -> s
          | Error _ -> Test.fail "shard RPC sent invalid json"
        in
        let shard =
          Data_encoding.Json.destruct Cryptobox.shard_encoding shard
        in
        Cryptobox.IntMap.add shard.index shard.share acc)
      Cryptobox.IntMap.empty
      shards
  in
  let cryptobox = Rollup.Dal.make parameters.cryptobox in
  let reformed_slot =
    match Cryptobox.polynomial_from_shards cryptobox shards with
    | Ok p -> Cryptobox.polynomial_to_bytes cryptobox p |> Bytes.to_string
    | Error _ -> Test.fail "Fail to build polynomial from shards"
  in
  Check.(reformed_slot = slot_content)
    Check.(string)
    ~error_msg:
      "Reconstructed slot is different from original slot (current = %L, \
       expected = %R)" ;
  return ()

let test_dal_node_test_slots_propagation _protocol _parameters _cryptobox node
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
  let* dal = Rollup.Dal.Parameters.from_client client in
  let cryptobox = Rollup.Dal.make dal.cryptobox in
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
  let* slot_header1, _proof1 = split_slot dal_node1 client "content1" in
  let* slot_header2, _proof2 = split_slot dal_node2 client "content2" in
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
  Cryptobox.commit cryptobox polynomial

let test_dal_node_test_post_slots _protocol parameters cryptobox _node client
    dal_node =
  let mk_slot size =
    Rollup.Dal.make_slot ~padding:false (generate_dummy_slot size) client
  in
  let failing_post_slot_rpc slot =
    let* response = RPC.call_raw dal_node @@ Rollup.Dal.RPC.post_slot slot in
    return
    @@ RPC.check_string_response
         ~body_rex:"dal.node.invalid_slot_size"
         ~code:500
         response
  in
  let size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot_big = mk_slot (size + 1) in
  let* slot_small = mk_slot (size - 1) in
  let* slot_ok = mk_slot size in
  let* () = failing_post_slot_rpc slot_big in
  let* () = failing_post_slot_rpc slot_small in
  let* commitment1 = RPC.call dal_node (Rollup.Dal.RPC.post_slot slot_ok) in
  let* commitment2 = RPC.call dal_node (Rollup.Dal.RPC.post_slot slot_ok) in
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

let test_dal_node_test_patch_slots _protocol parameters cryptobox _node client
    dal_node =
  let failing_patch_slot_rpc slot ~slot_level ~slot_index =
    let* response =
      RPC.call_raw dal_node
      @@ Rollup.Dal.RPC.patch_slot slot ~slot_level ~slot_index
    in
    return @@ RPC.check_string_response ~code:404 response
  in
  let size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot = Rollup.Dal.make_slot (generate_dummy_slot size) client in
  let commitment =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot
  in
  let* () = failing_patch_slot_rpc commitment ~slot_level:0 ~slot_index:0 in
  let* commitment' = RPC.call dal_node (Rollup.Dal.RPC.post_slot slot) in
  Check.(commitment' = commitment)
    Check.string
    ~error_msg:
      "The commitment of a stored commitment should match the one computed \
       locally (current = %L, expected = %R)" ;
  let patch_slot_rpc ~slot_level ~slot_index =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.patch_slot commitment ~slot_level ~slot_index)
  in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:0 in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:0 in
  let* () = patch_slot_rpc ~slot_level:0 ~slot_index:1 in
  patch_slot_rpc ~slot_level:(-4) ~slot_index:3

let test_dal_node_test_get_slots _protocol parameters cryptobox _node client
    dal_node =
  let size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot = Rollup.Dal.make_slot (generate_dummy_slot size) client in
  let commit =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot
  in
  let* () =
    let* response = RPC.call_raw dal_node @@ Rollup.Dal.RPC.get_slot commit in
    return @@ RPC.check_string_response ~code:404 response
  in
  let* _commitment = RPC.call dal_node (Rollup.Dal.RPC.post_slot slot) in
  (* commit = _commitment already test in /POST test. *)
  let* got_slot = RPC.call dal_node (Rollup.Dal.RPC.get_slot commit) in
  Check.(Rollup.Dal.content_of_slot slot = Rollup.Dal.content_of_slot got_slot)
    Check.string
    ~error_msg:
      "The slot content retrieved from the node is not as expected (expected = \
       %L, got = %R)" ;
  unit

let test_dal_node_test_get_slot_proof _protocol parameters cryptobox _node
    client dal_node =
  let size = parameters.Rollup.Dal.Parameters.cryptobox.slot_size in
  let* slot = Rollup.Dal.make_slot (generate_dummy_slot size) client in
  let* commitment = RPC.call dal_node (Rollup.Dal.RPC.post_slot slot) in
  let* proof = RPC.call dal_node (Rollup.Dal.RPC.get_slot_proof commitment) in
  let _, expected_proof =
    Rollup.Dal.Commitment.dummy_commitment cryptobox (generate_dummy_slot size)
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
    Client.init_with_protocol `Client ~protocol:previous_protocol ~nodes_args ()
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

let send_messages ?(src = Constant.bootstrap2.alias) ?(alter_final_msg = Fun.id)
    client msgs =
  let msg =
    alter_final_msg
    @@ Ezjsonm.(to_string ~minify:true @@ list Ezjsonm.string msgs)
  in
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

let bake_levels n client = repeat n (fun () -> Client.bake_for_and_wait client)

let rollup_node_stores_dal_slots ?expand_test _protocol dal_node sc_rollup_node
    sc_rollup_address node client _pvm_name =
  (* Check that the rollup node stores the slots published in a block, along with slot headers:
     0. Run dal node
     1. Send three slots to dal node and obtain corresponding headers
     2. Run rollup node for an originated rollup
     3. (Rollup node is implicitely subscribed to all slots)
     4. Publish the three slot headers for slots 0, 1, 2
     5. Check that the rollup node fetched the slot headers from L1
     6. After lag levels, attest only slots 1 and 2
     7. Check that no slots are downloaded by the rollup node if
        the PVM does not request them
     8. Send external messages to trigger the request of dal pages for
        slots 0 and 1
     9. Wait for the rollup node PVM to process the input and request
        the slots
     10. Check that requested confirmed slots (slot 1) is downloaded from
        dal node, while unconfirmed slot (slot 0) is not downloaded
  *)

  (* 0. run dl node. *)

  (* 1. Send three slots to dal node and obtain corresponding headers. *)
  let slot_contents_0 = " 10 " in
  let* commitment_0, proof_0 = split_slot dal_node client slot_contents_0 in
  let slot_contents_1 = " 200 " in
  let* commitment_1, proof_1 = split_slot dal_node client slot_contents_1 in
  let slot_contents_2 = " 400 " in
  let* commitment_2, proof_2 = split_slot dal_node client slot_contents_2 in
  (* 2. Run rollup node for an originated rollup. *)
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollups_sc_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () = Sc_rollup_node.run sc_rollup_node [] in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in

  Check.(level = init_level)
    Check.int
    ~error_msg:
      "Current level has moved past origination level (current = %L, expected \
       = %R)" ;

  (* 3. (Rollup node is implicitely subscribed to all slots) *)

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
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + 1)
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
  (* 6. attest only slots 1 and 2. *)
  let* parameters = Rollup.Dal.Parameters.from_client client in
  let nb_slots = parameters.number_of_slots in
  let* _op_hash =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap1 [2; 1] client
  in
  let* _op_hash =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap2 [2; 1] client
  in
  let* _op_hash =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap3 [2; 1] client
  in
  let* _op_hash =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap4 [2; 1] client
  in
  let* _op_hash =
    dal_attestation ~nb_slots ~signer:Constant.bootstrap5 [2; 1] client
  in
  let* () = Client.bake_for_and_wait client in
  let* slot_confirmed_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (slots_published_level + 1)
  in
  Check.(slot_confirmed_level = slots_published_level + 1)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;
  (* 7. Check that no slots have been downloaded *)
  let*! downloaded_confirmed_slots =
    Sc_rollup_client.dal_downloaded_confirmed_slot_pages ~hooks sc_rollup_client
  in
  let expected_number_of_downloaded_or_unconfirmed_slots = 0 in
  Check.(
    List.length downloaded_confirmed_slots
    = expected_number_of_downloaded_or_unconfirmed_slots)
    Check.int
    ~error_msg:
      "Unexpected number of slots that have been either downloaded or \
       unconfirmed (current = %L, expected = %R)" ;
  (* 8 Sends message to import dal pages from slots 0 and 1 of published_level
     to the PVM. *)
  let published_level_as_string = Int.to_string slots_published_level in
  let messages =
    [
      "dal:" ^ published_level_as_string ^ ":0:0";
      "dal:" ^ published_level_as_string ^ ":1:0";
    ]
  in
  let* () = send_messages client messages in
  let* level =
    Sc_rollup_node.wait_for_level sc_rollup_node (slot_confirmed_level + 1)
  in
  Check.(level = slot_confirmed_level + 1)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;
  (* 9. Wait for the rollup node to download the attested slots. *)
  let confirmed_level_as_string = Int.to_string slot_confirmed_level in
  let*! downloaded_confirmed_slots =
    Sc_rollup_client.dal_downloaded_confirmed_slot_pages
      ~block:confirmed_level_as_string
      sc_rollup_client
  in
  (* 10. Verify that rollup node has downloaded slot 1, slot 0 is
        unconfirmed, and slot 2 has not been downloaded *)
  let expected_number_of_downloaded_or_unconfirmed_slots = 1 in
  Check.(
    List.length downloaded_confirmed_slots
    = expected_number_of_downloaded_or_unconfirmed_slots)
    Check.int
    ~error_msg:
      "Unexpected number of slots that have been either downloaded or \
       unconfirmed (current = %L, expected = %R)" ;
  let submitted_slots_contents =
    [slot_contents_0; slot_contents_1; slot_contents_2]
  in
  List.iter
    (fun i ->
      let index = i + 1 in
      let confirmed_slot_index, confirmed_slot_contents =
        List.nth downloaded_confirmed_slots i
      in
      let relevant_page = List.nth confirmed_slot_contents 0 in
      let confirmed_slot_content = List.nth submitted_slots_contents index in
      let message =
        String.sub relevant_page 0 (String.length confirmed_slot_content)
      in
      Check.(confirmed_slot_index = index)
        Check.int
        ~error_msg:
          "Index of confirmed slot is not as expected (current = %L, expected \
           = %R)" ;
      Check.(message = confirmed_slot_content)
        Check.string
        ~error_msg:"unexpected message in slot (current = %L, expected = %R)")
    [0] ;
  match expand_test with
  | None -> return ()
  | Some f -> f client sc_rollup_address sc_rollup_node

let rollup_node_interprets_dal_pages client sc_rollup sc_rollup_node =
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollups_sc_rollup_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node init_level
  in

  (* The Dal content is as follows:
      - the page 0 of slot 0 contains 10,
      - the page 0 of slot 1 contains 200,
      - the page 0 of slot 2 contains 400.
     Only slot 1 is confirmed. Below, we expect to have value = 302. *)
  let expected_value = 302 in
  (* The code should be adapted if the current level changes. *)
  assert (level = 5) ;
  let* () =
    send_messages
      client
      [
        " 99 3 ";
        (* Total sum is now 99 + 3 = 102 *)
        " dal:3:1:0 ";
        (* Page 0 of Slot 1 contains 200, total sum is 302. *)
        " dal:3:1:1 ";
        " dal:3:0:0 ";
        (* Slot 0 is not confirmed, total sum doesn't change. *)
        " dal:3:0:2 ";
        (* Page 2 of Slot 0 empty, total sum unchanged. *)
        (* Page 1 of Slot 1 is empty, total sum unchanged. *)
        " dal:2:1:0 ";
        (* It's too late to import a page published at level 5. *)
        " dal:5:1:0 ";
        (* It's too early to import a page published at level 7. *)
        " dal:3:10000:0 ";
        " dal:3:0:100000 ";
        " dal:3:-10000:0 ";
        " dal:3:0:-100000 ";
        " dal:3:expecting_integer:0 ";
        " dal:3:0:expecting_integer ";
        (* The 6 pages requests above are ignored by the PVM because
           slot/page ID is out of bounds or illformed. *)
        " dal:1002147483647:1:1 "
        (* Level is about Int32.max_int, directive should be ignored. *);
        "   + + value";
      ]
  in

  (* Slot 1 is not confirmed, hence the total sum doesn't change. *)
  let* () = repeat 2 (fun () -> Client.bake_for_and_wait client) in
  let* _lvl =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 1)
  in
  let*! encoded_value =
    Sc_rollup_client.state_value ~hooks sc_rollup_client ~key:"vars/value"
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

(* DAC tests *)
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

let test_dal_node_handles_dac_store_preimage_merkle_V0 _protocol dal_node
    sc_rollup_node _sc_rollup_address _node client pvm_name =
  (* Terminate the dal node before setting dac parameters. *)
  let* () = Dal_node.terminate dal_node in
  let* dac_member = Client.bls_gen_keys ~alias:"dac_member" client in
  let* dac_member_info = Client.bls_show_address ~alias:dac_member client in
  let dac_member_address = dac_member_info.aggregate_public_key_hash in
  let* () = Dal_node.Dac.set_parameters ~threshold:1 dal_node in
  let* () =
    Dal_node.Dac.add_committee_member ~address:dac_member_address dal_node
  in
  let* () = Dal_node.run dal_node in
  let payload = "test" in
  let* actual_rh, l1_operation =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Merkle_tree_V0")
  in
  (* Expected reveal hash equals to the result of
     [Tezos_dal_alpha.Dac_pages_encoding.Merkle_tree.V0.serialize_payload "test"].
  *)
  let expected_rh =
    "scrrh13xwafZc5revvm5mittZFsBWb8ynFdH7ZRwpgUzCbtCCZo2orjs"
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
    RPC.call dal_node (Rollup.Dal.RPC.dac_verify_signature l1_operation)
  in
  Check.(
    (is_signature_valid = true)
      bool
      ~error_msg:"Signature of external message is not valid") ;
  unit

let test_dal_node_handles_dac_store_preimage_hash_chain_V0 _protocol dal_node
    sc_rollup_node _sc_rollup_address _node _client pvm_name =
  let payload = "test" in
  let* actual_rh, _l1_operation =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  (* Expected reveal hash equals to the result of
     [Tezos_dal_alpha.Dac_pages_encoding.Hash_chain.V0.serialize_payload "test"].
  *)
  let expected_rh =
    "scrrh13qW2L3g9ubEMjvWj5Rk2eqknXDsvnkZKVErFsavKTaFLgRgsXe"
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

let test_rollup_arith_uses_reveals _protocol dal_node sc_rollup_node
    sc_rollup_address _node client _pvm_name =
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollups_sc_rollup_genesis_info
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
      dal_node
      (Rollup.Dal.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  let expected_rh =
    "scrrh144gSwSgX2cSw3v8p9h5rf6L6Ryvx4ewEhBDhkz7ErY9EChS47a"
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
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
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

let test_reveals_fails_on_wrong_hash _protocol dal_node sc_rollup_node
    sc_rollup_address _node client _pvm_name =
  let payload = "Some data that is not related to the hash" in
  let _actual_rh =
    RPC.call
      dal_node
      (Rollup.Dal.RPC.dac_store_preimage
         ~payload
         ~pagination_scheme:"Hash_chain_V0")
  in
  let errorneous_hash =
    "scrrh144gSwSgX2cSw3v8p9h5rf6L6Ryvx4ewEhBDhkz7ErY9EChS47a"
  in
  let* genesis_info =
    RPC.Client.call ~hooks client
    @@ RPC.get_chain_block_context_sc_rollups_sc_rollup_genesis_info
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

let test_dal_node_imports_dac_member =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node imports dac members sk_uris"
    ~tags:["dac"; "dal_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let run_dal = Dal_node.run ~wait_ready:false in
  let* dac_member = Client.bls_gen_keys ~alias:"dac_member" client in
  let* dac_member_info = Client.bls_show_address ~alias:dac_member client in
  let dac_member_address = dac_member_info.aggregate_public_key_hash in
  let dal_node = Dal_node.create ~node ~client () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = Dal_node.Dac.set_parameters ~threshold:1 dal_node in
  let* () =
    Dal_node.Dac.add_committee_member ~address:dac_member_address dal_node
  in
  let ready_promise =
    Dal_node.wait_for dal_node "dac_is_ready.v0" (fun _ -> Some ())
  in
  let* () = run_dal dal_node in
  let* () = ready_promise in
  let* () = Dal_node.terminate dal_node in
  unit

let test_dal_node_dac_threshold_not_reached =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node displays warning if dac threshold is not reached"
    ~tags:["dac"; "dal_node"]
    ~supports:Protocol.(From_protocol (Protocol.number Alpha))
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let run_dal = Dal_node.run ~wait_ready:false in
  let dal_node = Dal_node.create ~node ~client () in
  let* _dir = Dal_node.init_config dal_node in
  let* () = Dal_node.Dac.set_parameters ~threshold:1 dal_node in
  let error_promise =
    Dal_node.wait_for dal_node "dac_threshold_not_reached.v0" (fun _ -> Some ())
  in
  let* () = run_dal dal_node in
  let* () = error_promise in
  Dal_node.terminate dal_node

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
  test_dal_node_imports_dac_member protocols ;
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
    "dal node POST /slots"
    test_dal_node_test_post_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node PATCH /slots"
    test_dal_node_test_patch_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET /slots"
    test_dal_node_test_get_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET /slot/<commitment>/proof"
    test_dal_node_test_get_slot_proof
    protocols ;
  (* Tests with all nodes *)
  test_dal_node_dac_threshold_not_reached protocols ;
  scenario_with_all_nodes
    "rollup_node_downloads_slots"
    rollup_node_stores_dal_slots
    protocols ;
  scenario_with_all_nodes
    "rollup_node_applies_dal_pages"
    (rollup_node_stores_dal_slots ~expand_test:rollup_node_interprets_dal_pages)
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dal_node"]
    "dac_reveals_data_merkle_tree_v0"
    test_dal_node_handles_dac_store_preimage_merkle_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dal_node"]
    "dac_reveals_data_hash_chain_v0"
    test_dal_node_handles_dac_store_preimage_hash_chain_V0
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dal_node"]
    "dac_rollup_arith_uses_reveals"
    test_rollup_arith_uses_reveals
    protocols ;
  scenario_with_all_nodes
    ~tags:["dac"; "dal_node"]
    "dac_rollup_arith_wrong_hash"
    test_reveals_fails_on_wrong_hash
    protocols
