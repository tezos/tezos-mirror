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
   Requirement:  make -f kernels.mk build

                 For dev-purpose you can also run the follwing to enable debug outputs and faster build time:

                 cd src/kernel_tx_demo/kernel
                 cargo build --release --target wasm32-unknown-unknown --features debug --features dal
                 wasm-strip ../target/wasm32-unknown-unknown/release/tx_kernel.wasm
                 cp ../target/wasm32-unknown-unknown/release/tx_kernel.wasm ../../../tx_kernel_dal.wasm

                 ./scripts/install_dal_trusted_setup.sh

   Invocation:   dune exec tezt/tests/main.exe -- --file dal.ml
   Subject: Integration tests related to the data-availability layer
*)

let team = Tag.tezos2

let hooks = Tezos_regression.hooks

let rpc_hooks = Tezos_regression.rpc_hooks

module Dal = Dal_common
module Helpers = Dal.Helpers
module Cryptobox = Dal.Cryptobox

module Dal_RPC = struct
  include Dal.RPC

  (* We override call_xx RPCs in Dal.RPC to use a DAL node in this file. *)
  include Dal.RPC.Local
end

let init_logger () =
  let counter = ref 1 in
  fun fmt ->
    let color = Log.Color.(bold ++ FG.blue) in
    let prefix = "step-" ^ string_of_int !counter in
    incr counter ;
    Log.info ~color ~prefix fmt

let read_dir dir =
  let* dir = Lwt_unix.opendir dir in
  let rec read_files acc =
    Lwt.catch
      (fun () ->
        let* entry = Lwt_unix.readdir dir in
        match entry with
        | "." | ".." | ".lock" -> read_files acc
        | file -> read_files (file :: acc))
      (function
        | End_of_file ->
            let* () = Lwt_unix.closedir dir in
            return acc
        | exn -> Lwt.reraise exn)
  in
  read_files []

(* This function checks that in the skip list store of the given
   [dal_node]:
   (1) the 'hashes' coincides with [expected_levels] (up to ordering)
   and,
   (2) as many 'cells' as the [(List.length expected_levels) *
   number_of_slots]. *)
let check_skip_list_store dal_node ~number_of_slots ~expected_levels =
  let path =
    sf "%s/store/skip_list_store/store.sqlite" (Dal_node.data_dir dal_node)
  in
  let db = Sqlite3.db_open path in
  let attested_levels = ref [] in
  let query = "SELECT attested_level FROM skip_list_slots;" in
  let res =
    Sqlite3.exec_not_null_no_headers
      ~cb:(fun row ->
        let attested_level = row.(0) in
        attested_levels := attested_level :: !attested_levels)
      db
      query
  in
  let () =
    match res with
    | Sqlite3.Rc.OK -> ()
    | err ->
        Test.fail
          "Failure fetching the attested levels from the sqlite db: %s"
          (Sqlite3.Rc.to_string err)
  in
  Check.(
    List.sort_uniq String.compare !attested_levels
    = List.sort String.compare expected_levels)
    ~__LOC__
    Check.(list string)
    ~error_msg:"Expected hashes directory content: %R. Got: %L" ;
  let count = ref 0 in
  let query = "SELECT hash FROM skip_list_cells;" in
  let res =
    Sqlite3.exec_not_null_no_headers ~cb:(fun _ -> incr count) db query
  in
  let () =
    match res with
    | Sqlite3.Rc.OK -> ()
    | err ->
        Test.fail
          "Failure fetching the number of cells from the sqlite db: %s"
          (Sqlite3.Rc.to_string err)
  in
  Check.(!count = number_of_slots * List.length expected_levels)
    ~__LOC__
    Check.int
    ~error_msg:"Expected %R cells, got %L" ;
  let _ = Sqlite3.db_close db in
  unit

(* Wait for 'new_head' event. Note that the DAL node processes a new head with a
   delay of one level. Also, this event is emitted before block processing. *)
let wait_for_layer1_head dal_node level =
  Dal_node.wait_for dal_node "dal_new_L1_head_block.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

(* Wait for 'new_final_block' event. This event is emitted after processing a
   final block. *)
let wait_for_layer1_final_block dal_node level =
  Dal_node.wait_for dal_node "dal_new_L1_final_block.v0" (fun e ->
      if JSON.(e |-> "level" |> as_int) = level then Some () else None)

(* We use a custom [bake_for], which by default bakes with all delegates, unlike
   [Client.bake_for], to highlight the following: baking in the past with all
   delegates ensures that the baked block has round 0, which is the default
   round used when injecting DAL attestation operations. Note that it is
   normally not necessary to bake with a particular delegate, therefore there is
   no downside to set the case [`All] as the default. *)
let bake_for ?(delegates = `All) ?count ?dal_node_endpoint client =
  let keys =
    match delegates with
    | `All ->
        (* The argument ~keys:[] allows to bake with all available delegates. *)
        []
    | `For keys -> keys
  in
  Client.bake_for_and_wait client ~keys ?count ?dal_node_endpoint

(* Bake until a block at some given [level] has been finalized and
   processed by the given [dal_nodes]. The head level after this is
   [level + 2]. *)
let bake_until_processed ~level client dal_nodes =
  let* current_level = Client.level client in
  let final_level = level + 2 in
  assert (current_level < final_level) ;
  let p =
    List.map
      (fun dal_node -> wait_for_layer1_final_block dal_node level)
      dal_nodes
  in
  let* () = bake_for ~count:(final_level - current_level) client in
  Lwt.join p

module Client = struct
  include Client

  let msg =
    "Please use 'bake_for' for DAL tests and not 'Client.bake_for' to be sure \
     to read the comment about the 'keys' argument."

  let bake_for _client = Test.fail "%s" msg

  let bake_for_and_wait _client = Test.fail "%s" msg
end

let next_level node =
  let* current_level = Node.get_level node in
  return (current_level + 1)

let check_in_TB_committee ~__LOC__ node ?(inside = true) ?level pkh =
  let* slots =
    Node.RPC.call node
    @@ RPC.get_chain_block_helper_validators ?level ~delegate:pkh ()
  in
  let in_committee = JSON.as_list slots <> [] in
  Check.(
    (in_committee = inside)
      ~__LOC__
      bool
      ~error_msg:"The account is in the TB committee? Expected %R, got %L") ;
  unit

let get_peer_score dal_node peer_id =
  let* scores = Dal_RPC.(call dal_node @@ get_scores ()) in
  let peer_score =
    List.find
      (fun Dal_RPC.{peer; score = _} -> String.equal peer peer_id)
      scores
  in
  return peer_score.score

let wait_for_stored_slot ?shard_index dal_node ~published_level ~slot_index =
  let check_slot_id e =
    JSON.(e |-> "published_level" |> as_int) = published_level
    && JSON.(e |-> "slot_index" |> as_int) = slot_index
  in
  let check_shard_index e =
    match shard_index with
    | None -> true
    | Some shard_index -> JSON.(e |-> "shard_index" |> as_int) = shard_index
  in
  Dal_node.wait_for dal_node "dal_stored_slot_shard.v0" (fun e ->
      if check_slot_id e && check_shard_index e then Some () else None)

(* Wait until the given [dal_node] receives all the shards whose
   indices are [shards] for the given published level and slot index. *)
let wait_for_shards_promises ~dal_node ~shards ~published_level ~slot_index =
  let nshards = List.length shards in
  let count = ref 0 in
  let promises =
    List.map
      (fun shard_index ->
        let* () =
          wait_for_stored_slot
            ~shard_index
            ~published_level
            ~slot_index
            dal_node
        in
        let () = incr count in
        let () =
          Log.debug
            "Dal node %s has received %d/%d shards"
            (Dal_node.name dal_node)
            !count
            nshards
        in
        unit)
      shards
  in
  Lwt.join promises

let wait_for_shard_validation_is_disabled dal_node =
  Dal_node.wait_for dal_node "shard_validation_is_disabled.v0" (fun _json ->
      Some ())

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

let make_q_parameter name = function
  | None -> []
  | Some q ->
      [
        ( name,
          `O
            [
              ("numerator", `String (Q.num q |> Z.to_int |> string_of_int));
              ("denominator", `String (Q.den q |> Z.to_int |> string_of_int));
            ] );
      ]

let test ~__FILE__ ?(regression = false) ?(tags = []) ?uses
    ?(supports = Protocol.From_protocol 19) title f =
  let tags = Tag.tezos2 :: "dal" :: tags in
  let register_test =
    if regression then Protocol.register_regression_test
    else Protocol.register_test
  in
  register_test ~__FILE__ ~title ~tags ?uses ~supports f

let dal_enable_param dal_enable =
  make_bool_parameter ["dal_parametric"; "feature_enable"] dal_enable

let incentives_enable_param enable =
  make_bool_parameter ["dal_parametric"; "incentives_enable"] enable

let sc_rollup_activation_dal_params dal_enable =
  if Option.value dal_enable ~default:false then
    [
      (["smart_rollup_reveal_activation_level"; "dal_parameters"], `Int 0);
      (["smart_rollup_reveal_activation_level"; "dal_page"], `Int 0);
    ]
  else []

let redundancy_factor_param redundancy_factor =
  make_int_parameter ["dal_parametric"; "redundancy_factor"] redundancy_factor

let slot_size_param slot_size =
  make_int_parameter ["dal_parametric"; "slot_size"] slot_size

(* Some initialization functions to start needed nodes. *)
type l1_history_mode =
  | Default_with_refutation
    (* to be used when the node starts for the first time *)
  | Default_with_refutation_full
    (* to be used when the node restarts, and it was first started more than the
       default storage period time ago *)
  | Default_without_refutation
  | Custom of Node.history_mode

let generate_protocol_parameters base protocol parameter_overrides =
  let* parameter_file =
    Protocol.write_parameter_file ~base parameter_overrides
  in
  let* client = Client.init_mockup ~parameter_file ~protocol () in
  Client.RPC.call_via_endpoint client
  @@ RPC.get_chain_block_context_constants ()

(* Compute the L1 history mode. This function may update the protocol parameters
   and this is why it needs additional, a priori unrelated parameters. *)
let history_mode base protocol parameter_overrides proto_parameters
    l1_history_mode =
  let update_some_rollup_params factor =
    let challenge_window =
      JSON.(
        proto_parameters |-> "smart_rollup_challenge_window_in_blocks" |> as_int)
      / factor
    in
    let commitment_period =
      max
        1
        (JSON.(
           proto_parameters |-> "smart_rollup_commitment_period_in_blocks"
           |> as_int)
        / factor)
    in
    let validity_lag =
      JSON.(
        proto_parameters |-> "smart_rollup_reveal_activation_level"
        |-> "dal_attested_slots_validity_lag" |> as_int)
      / factor
    in
    ( ["smart_rollup_reveal_activation_level"; "dal_attested_slots_validity_lag"],
      `Int validity_lag )
    :: (["smart_rollup_challenge_window_in_blocks"], `Int challenge_window)
    :: (["smart_rollup_commitment_period_in_blocks"], `Int commitment_period)
    :: parameter_overrides
  in
  match l1_history_mode with
  | Custom history_mode -> return (parameter_overrides, history_mode)
  | Default_without_refutation ->
      let cycles =
        Dal.Parameters.storage_period_without_refutation_in_cycles
          ~proto_parameters
      in
      let blocks_preservation_cycles =
        JSON.(proto_parameters |-> "blocks_preservation_cycles" |> as_int)
      in
      let additional_cycles = cycles - blocks_preservation_cycles in
      return (parameter_overrides, Node.Rolling (Some additional_cycles))
  | Default_with_refutation ->
      let cycles =
        Dal.Parameters.initial_storage_period_with_refutation_in_cycles
          ~proto_parameters
      in
      let blocks_preservation_cycles =
        JSON.(proto_parameters |-> "blocks_preservation_cycles" |> as_int)
      in
      let additional_cycles = cycles - blocks_preservation_cycles in
      return (parameter_overrides, Node.Rolling (Some additional_cycles))
  | Default_with_refutation_full ->
      let cycles =
        Dal.Parameters.full_storage_period_with_refutation_in_cycles
          ~proto_parameters
      in
      let blocks_preservation_cycles =
        JSON.(proto_parameters |-> "blocks_preservation_cycles" |> as_int)
      in
      let additional_cycles = cycles - blocks_preservation_cycles in
      (* The shell has an upper bound of 1000 stored cycles in Full and Rolling
         mode. In case this limit is crossed, we update some of the relevant
         parameters. *)
      if additional_cycles > 1000 then (
        let factor = 1 + (cycles / 1000) in
        let new_parameter_overrides = update_some_rollup_params factor in
        (* This may not work correctly if the updated parameters were already
           present in [parameters]. *)
        let* proto_parameters =
          generate_protocol_parameters base protocol new_parameter_overrides
        in
        Log.info
          "The DAL node needs the L1 node to store %d cycles of block data."
          cycles ;
        Log.info
          "Reducing 'smart_rollup_challenge_window_in_blocks', \
           'smart_rollup_commitment_period_in_blocks' and \
           'dal_attested_slots_validity_lag' by a factor of %d."
          factor ;
        let cycles =
          Dal.Parameters.full_storage_period_with_refutation_in_cycles
            ~proto_parameters
        in
        Log.info
          "Now the DAL node needs the L1 node to store %d cycles of block data"
          cycles ;
        let additional_cycles = cycles - blocks_preservation_cycles in
        if additional_cycles > 1000 then
          Test.fail "Could not adjust sc_rollup parameters automatically!"
        else
          return (new_parameter_overrides, Node.Rolling (Some additional_cycles)))
      else return (parameter_overrides, Node.Rolling (Some additional_cycles))

let setup_node ?(custom_constants = None) ?(additional_bootstrap_accounts = 0)
    ~parameter_overrides ~protocol ?activation_timestamp
    ?(event_sections_levels = []) ?(node_arguments = [])
    ?(dal_bootstrap_peers = []) ?(l1_history_mode = Default_without_refutation)
    () =
  let base = Either.right (protocol, custom_constants) in
  let* proto_parameters =
    generate_protocol_parameters base protocol parameter_overrides
  in
  let* parameter_overrides, history_mode =
    history_mode
      base
      protocol
      parameter_overrides
      proto_parameters
      l1_history_mode
  in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0;
        No_bootstrap_peers;
        History_mode history_mode;
      ]
  in
  let node = Node.create nodes_args in
  let* () = Node.config_init node [] in
  let dal_parameters =
    Dal.Parameters.from_protocol_parameters proto_parameters
  in
  let config : Cryptobox.Config.t =
    {activated = true; bootstrap_peers = dal_bootstrap_peers}
  in
  let* () = Node.Config_file.update node Node.Config_file.set_sandbox_network in
  let* () =
    Node.Config_file.update
      node
      (Node.Config_file.set_network_with_dal_config config)
  in
  let* () = Node.run node ~event_sections_levels node_arguments in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* additional_account_keys =
    if additional_bootstrap_accounts > 0 then
      Client.stresstest_gen_keys additional_bootstrap_accounts client
    else return []
  in
  let additional_bootstrap_accounts =
    List.map (fun x -> (x, None, false)) additional_account_keys
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts
      ~base
      parameter_overrides
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
    ?consensus_committee_size ?minimal_block_delay ?delay_increment_per_round
    ?attestation_lag ?slot_size ?number_of_slots ?page_size
    ?attestation_threshold ?number_of_shards ?redundancy_factor
    ?commitment_period ?challenge_window ?dal_enable ?incentives_enable
    ?dal_rewards_weight ?traps_fraction ?event_sections_levels ?node_arguments
    ?activation_timestamp ?dal_bootstrap_peers ?(parameters = [])
    ?(prover = true) ?smart_rollup_timeout_period_in_blocks ?l1_history_mode
    ?blocks_per_cycle ?blocks_per_commitment f ~protocol =
  let parameter_overrides =
    make_int_parameter ["dal_parametric"; "attestation_lag"] attestation_lag
    @ make_int_parameter ["dal_parametric"; "number_of_shards"] number_of_shards
    @ make_int_parameter
        ["dal_parametric"; "redundancy_factor"]
        redundancy_factor
    @ make_int_parameter ["dal_parametric"; "slot_size"] slot_size
    @ make_int_parameter ["dal_parametric"; "number_of_slots"] number_of_slots
    @ make_int_parameter ["dal_parametric"; "page_size"] page_size
    @ make_int_parameter
        ["dal_parametric"; "redundancy_factor"]
        redundancy_factor
    @ make_int_parameter
        ["dal_parametric"; "attestation_threshold"]
        attestation_threshold
    @ make_q_parameter ["dal_parametric"; "traps_fraction"] traps_fraction
    @ make_int_parameter
        ["issuance_weights"; "dal_rewards_weight"]
        dal_rewards_weight
    @ make_int_parameter
        ["smart_rollup_commitment_period_in_blocks"]
        commitment_period
    @ make_int_parameter
        ["smart_rollup_challenge_window_in_blocks"]
        challenge_window
    (* this will produce the empty list if dal_enable is not passed to the function invocation,
       hence the value from the protocol constants will be used. *)
    @ dal_enable_param dal_enable
    @ incentives_enable_param incentives_enable
    @ sc_rollup_activation_dal_params dal_enable
    @ [(["smart_rollup_arith_pvm_enable"], `Bool true)]
    @ make_int_parameter ["consensus_committee_size"] consensus_committee_size
    @ make_string_parameter ["minimal_block_delay"] minimal_block_delay
    @ make_string_parameter
        ["delay_increment_per_round"]
        delay_increment_per_round
    @ make_int_parameter
        ["smart_rollup_timeout_period_in_blocks"]
        smart_rollup_timeout_period_in_blocks
    (* AI is already active on mainnet, so it should be active
       immediately in tests *)
    @ make_int_parameter ["blocks_per_cycle"] blocks_per_cycle
    @ make_int_parameter ["blocks_per_commitment"] blocks_per_commitment
    @ parameters
  in

  let* node, client, dal_parameters =
    setup_node
      ?custom_constants
      ?additional_bootstrap_accounts
      ?event_sections_levels
      ?node_arguments
      ?activation_timestamp
      ?dal_bootstrap_peers
      ?l1_history_mode
      ~parameter_overrides
      ~protocol
      ()
  in
  let* () = if prover then Helpers.init_prover ~__LOC__ () else unit in
  let* cryptobox = Helpers.make_cryptobox dal_parameters.cryptobox in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f dal_parameters cryptobox node client bootstrap1_key

let with_fresh_rollup ?(pvm_name = "arith") ?dal_node f tezos_node tezos_client
    bootstrap1_key =
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
      ?dal_node
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~default_operator:bootstrap1_key
  in
  let* () = bake_for tezos_client in
  f rollup_address sc_rollup_node

let make_dal_node ?name ?peers ?attester_profiles ?operator_profiles
    ?bootstrap_profile ?history_mode ?(wait_ready = true) ?env
    ?disable_shard_validation ?(event_level = `Debug) ?slots_backup_uris
    ?trust_slots_backup_uris ?disable_amplification ?ignore_pkhs tezos_node =
  let dal_node =
    Dal_node.create
      ?name
      ?disable_shard_validation
      ?disable_amplification
      ?ignore_pkhs
      ~node:tezos_node
      ()
  in
  let* () =
    Dal_node.init_config
      ?peers
      ?attester_profiles
      ?operator_profiles
      ?bootstrap_profile
      ?history_mode
      ?slots_backup_uris
      ?trust_slots_backup_uris
      dal_node
  in
  let* () = Dal_node.run ?env ~event_level dal_node ~wait_ready in
  return dal_node

let with_dal_node ?peers ?attester_profiles ?operator_profiles
    ?bootstrap_profile ?history_mode ?wait_ready ?env ?disable_shard_validation
    ?disable_amplification ?ignore_pkhs tezos_node f key =
  let* dal_node =
    make_dal_node
      ?peers
      ?attester_profiles
      ?operator_profiles
      ?bootstrap_profile
      ?history_mode
      ?wait_ready
      ?env
      ?disable_shard_validation
      ?disable_amplification
      ?ignore_pkhs
      tezos_node
  in
  f key dal_node

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)
let scenario_with_layer1_node ?attestation_threshold ?regression ?(tags = [])
    ?(uses = fun _ -> []) ?additional_bootstrap_accounts ?attestation_lag
    ?number_of_shards ?number_of_slots ?custom_constants ?commitment_period
    ?challenge_window ?(dal_enable = true) ?incentives_enable ?traps_fraction
    ?dal_rewards_weight ?event_sections_levels ?node_arguments
    ?activation_timestamp ?consensus_committee_size ?minimal_block_delay
    ?delay_increment_per_round ?blocks_per_cycle ?blocks_per_commitment variant
    scenario =
  let description = "Testing DAL L1 integration" in
  let tags = if List.mem team tags then tags else team :: tags in
  test
    ?regression
    ~__FILE__
    ~uses
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      with_layer1
        ?blocks_per_cycle
        ?blocks_per_commitment
        ?attestation_threshold
        ~custom_constants
        ?additional_bootstrap_accounts
        ?consensus_committee_size
        ?minimal_block_delay
        ?delay_increment_per_round
        ?attestation_lag
        ?number_of_shards
        ?number_of_slots
        ?incentives_enable
        ?traps_fraction
        ?dal_rewards_weight
        ?commitment_period
        ?challenge_window
        ?event_sections_levels
        ?node_arguments
        ?activation_timestamp
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      scenario protocol parameters cryptobox node client)

let scenario_with_layer1_and_dal_nodes ?regression ?(tags = [])
    ?(uses = fun _ -> []) ?custom_constants ?minimal_block_delay
    ?delay_increment_per_round ?redundancy_factor ?slot_size ?number_of_shards
    ?number_of_slots ?attestation_lag ?attestation_threshold ?traps_fraction
    ?commitment_period ?challenge_window ?(dal_enable = true) ?incentives_enable
    ?dal_rewards_weight ?activation_timestamp ?bootstrap_profile
    ?event_sections_levels ?operator_profiles ?history_mode ?prover
    ?l1_history_mode ?wait_ready ?env ?disable_shard_validation
    ?disable_amplification ?ignore_pkhs variant scenario =
  let description = "Testing DAL node" in
  let tags = if List.mem team tags then tags else team :: tags in
  test
    ?regression
    ~__FILE__
    ~tags
    ~uses:(fun protocol -> Constant.octez_dal_node :: uses protocol)
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      let l1_history_mode =
        match (l1_history_mode, operator_profiles) with
        | Some mode, _ -> mode
        | None, Some (_ :: _) -> Default_with_refutation
        | _ -> Default_without_refutation
      in
      with_layer1
        ~custom_constants
        ?minimal_block_delay
        ?delay_increment_per_round
        ?redundancy_factor
        ?slot_size
        ?number_of_slots
        ?number_of_shards
        ?attestation_lag
        ?attestation_threshold
        ?traps_fraction
        ?incentives_enable
        ?dal_rewards_weight
        ?commitment_period
        ?challenge_window
        ?activation_timestamp
        ?event_sections_levels
        ?prover
        ~l1_history_mode
        ~protocol
        ~dal_enable
      @@ fun parameters cryptobox node client ->
      with_dal_node
        ?bootstrap_profile
        ?operator_profiles
        ?history_mode
        ?wait_ready
        ?env
        ?disable_shard_validation
        ?disable_amplification
        ?ignore_pkhs
        node
      @@ fun _key dal_node ->
      scenario protocol parameters cryptobox node client dal_node)

let scenario_with_all_nodes ?custom_constants ?node_arguments
    ?consensus_committee_size ?slot_size ?page_size ?number_of_shards
    ?redundancy_factor ?attestation_lag ?(tags = []) ?(uses = fun _ -> [])
    ?(pvm_name = "arith") ?(dal_enable = true) ?incentives_enable
    ?dal_rewards_weight ?commitment_period ?challenge_window
    ?minimal_block_delay ?delay_increment_per_round ?activation_timestamp
    ?bootstrap_profile ?operator_profiles ?smart_rollup_timeout_period_in_blocks
    ?(regression = true) ?prover ?attestation_threshold ?l1_history_mode variant
    ?disable_amplification scenario =
  let description = "Testing DAL rollup and node with L1" in
  let tags = if List.mem team tags then tags else team :: tags in
  test
    ~regression
    ~__FILE__
    ~tags
    ~uses:(fun protocol ->
      Constant.octez_smart_rollup_node :: Constant.octez_dal_node
      :: uses protocol)
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      let l1_history_mode =
        match (l1_history_mode, operator_profiles) with
        | Some mode, _ -> mode
        | None, Some (_ :: _) -> Default_with_refutation
        | _ -> Default_without_refutation
      in
      with_layer1
        ~custom_constants
        ?node_arguments
        ?consensus_committee_size
        ?slot_size
        ?page_size
        ?number_of_shards
        ?redundancy_factor
        ?attestation_lag
        ?incentives_enable
        ?dal_rewards_weight
        ?commitment_period
        ?challenge_window
        ?minimal_block_delay
        ?delay_increment_per_round
        ?activation_timestamp
        ?smart_rollup_timeout_period_in_blocks
        ?prover
        ?attestation_threshold
        ~l1_history_mode
        ~protocol
        ~dal_enable
      @@ fun parameters _cryptobox node client ->
      with_dal_node
        ?bootstrap_profile
        ?operator_profiles
        ?disable_amplification
        node
      @@ fun key dal_node ->
      ( with_fresh_rollup ~pvm_name ~dal_node
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

(* Return the baker at round 0 at the given level. *)
let baker_for_round_zero node ~level =
  let* rights =
    Node.RPC.call node
    @@ RPC.get_chain_block_helper_baking_rights ~level ~max_round:0 ()
  in
  JSON.(List.hd JSON.(rights |> as_list) |-> "delegate" |> as_string) |> return

(* Return a delegate from the list of bootstrap accounts that is different from
   the given delegate. *)
let different_delegate pkh =
  List.find
    (fun del -> not @@ String.equal pkh del.Account.public_key_hash)
    (Array.to_list Account.Bootstrap.keys)

(* Return the delegates from the list of bootstrap accounts that are different
   from the given delegate. *)
let different_delegates pkh =
  List.filter
    (fun del -> not @@ String.equal pkh del.Account.public_key_hash)
    (Array.to_list Account.Bootstrap.keys)

(* We support two formats for specifying the attested slots: either a
   list of slot ids or a bitset. *)
type attestation_availability =
  | Slots of int list
  | Bitset of bool array
  | No_dal_attestation

let craft_dal_attestation ?level ?round ?payload_level ~signer ~nb_slots
    availability client =
  let dal_attestation =
    match availability with
    | Bitset bitset -> Some bitset
    | Slots availability ->
        let dal_attestation = Array.make nb_slots false in
        List.iter (fun i -> dal_attestation.(i) <- true) availability ;
        Some dal_attestation
    | No_dal_attestation -> None
  in
  let* level =
    match level with Some level -> return level | None -> Client.level client
  in
  let* json =
    Client.RPC.call_via_endpoint client
    @@ RPC.get_chain_block_helper_validators
         ~level
         ~delegate:signer.Account.public_key_hash
         ()
  in
  match JSON.(json |> as_list) with
  | [] ->
      Log.info
        "[craft_dal_attestation] %s not in TB committee at level %d"
        signer.public_key_hash
        level ;
      none
  | [validator] ->
      let slots = JSON.(validator |-> "slots" |> as_list) in
      let slot = List.hd slots |> JSON.as_int in
      let* block_payload_hash, round =
        let block =
          Option.value payload_level ~default:level |> string_of_int
        in
        let* block_payload_hash =
          Operation.Consensus.get_block_payload_hash ~block client
        in
        let* round =
          match round with
          | None ->
              Client.RPC.call_via_endpoint client
              @@ RPC.get_chain_block_helper_round ~block ()
          | Some round -> return round
        in
        return (block_payload_hash, round)
      in
      let* op =
        Operation.Consensus.operation
          ~signer
          (Operation.Consensus.attestation
             ~level
             ~round
             ?dal_attestation
             ~slot
             ~block_payload_hash
             ())
          client
      in
      some op
  | _ ->
      Test.fail
        ~__LOC__
        "Unexpected format for get_chain_block_helper_validators RPC"

let craft_dal_attestation_exn ?level ?round ?payload_level ~signer ~nb_slots
    availability client =
  let* res =
    craft_dal_attestation
      ?level
      ?round
      ?payload_level
      ~signer
      ~nb_slots
      availability
      client
  in
  match res with
  | None ->
      Test.fail
        ~__LOC__
        "Unexpected case: pkh %s has no TB slot"
        signer.Account.public_key_hash
  | Some v -> return v

let inject_dal_attestation ?level ?round ?payload_level ?force ?error ?request
    ~signer ~nb_slots availability client =
  let* op_opt =
    craft_dal_attestation
      ?level
      ?round
      ?payload_level
      ~signer
      ~nb_slots
      availability
      client
  in
  match op_opt with
  | None -> none
  | Some op ->
      let* oph = Operation.inject ?force ?error ?request op client in
      some (op, oph)

let inject_dal_attestation_exn ?level ?round ?payload_level ?force ?error
    ?request ~signer ~nb_slots availability client =
  let* res =
    inject_dal_attestation
      ?level
      ?round
      ?payload_level
      ?force
      ?error
      ?request
      ~signer
      ~nb_slots
      availability
      client
  in
  match res with
  | None ->
      Test.fail
        ~__LOC__
        "Unexpected case: pkh %s has no TB slot"
        signer.Account.public_key_hash
  | Some v -> return v

let inject_dal_attestations ?payload_level ?level ?round ?force ?request
    ?(signers = Array.to_list Account.Bootstrap.keys) ~nb_slots availability
    client =
  Lwt_list.filter_map_s
    (fun signer ->
      inject_dal_attestation
        ?payload_level
        ?level
        ?round
        ?force
        ?request
        ~signer
        ~nb_slots
        availability
        client)
    signers

let inject_dal_attestations_and_bake node client ~number_of_slots indexes =
  let* baker =
    let* level = Node.get_level node in
    baker_for_round_zero node ~level:(level + 1)
  in
  let signers = different_delegates baker in
  let* _op_and_op_hash_list =
    inject_dal_attestations ~signers ~nb_slots:number_of_slots indexes client
  in
  bake_for ~delegates:(`For [baker]) client

let inject_dal_attestation_for_assigned_shards ~nb_slots ~attested_level
    ~attester_account ~attester_dal_node client =
  let* attestable_slots =
    Dal_RPC.(
      call attester_dal_node
      @@ get_attestable_slots ~attester:attester_account ~attested_level)
  in
  match attestable_slots with
  | Not_in_committee ->
      Test.fail "attester %s not in committee" attester_account.alias
  | Attestable_slots slots ->
      inject_dal_attestation
        ~level:(attested_level - 1)
        (Bitset (Array.of_list slots))
        ~signer:attester_account
        ~nb_slots
        client

let get_validated_dal_attestations_in_mempool node for_level =
  let* mempool_json =
    Node.RPC.call node
    @@ RPC.get_chain_mempool_pending_operations
         ~version:"2"
         ~validated:true
         ~branch_delayed:false
         ~branch_refused:false
         ~refused:false
         ~outdated:false
         ~validation_passes:[0]
         ()
  in
  let validated = JSON.(mempool_json |-> "validated" |> as_list) in
  List.filter
    (fun op ->
      let contents = JSON.(op |-> "contents" |> geti 0) in
      let level = JSON.(contents |-> "level" |> as_int) in
      level = for_level
      && JSON.(contents |-> "kind" |> as_string) |> fun kind ->
         String.equal kind "attestation_with_dal")
    validated
  |> return

let wait_for_classified oph node =
  let filter json = if JSON.as_string json = oph then Some () else None in
  Node.wait_for node "operation_classified.v0" filter

let test_one_committee_per_level _protocol _parameters _cryptobox node _client
    _bootstrap_key =
  let* current_level =
    Node.RPC.(call node @@ get_chain_block_helper_current_level ())
  in
  (* The test assumes we are at a level when an epoch starts. And
     that is indeed the case. *)
  assert (current_level.cycle_position = 0) ;
  let* current_committee =
    Dal.Committee.at_level node ~level:current_level.level ()
  in
  let* next_committee =
    Dal.Committee.at_level node ~level:(current_level.level + 1) ()
  in
  Check.((current_committee <> next_committee) Dal.Committee.typ)
    ~error_msg:"Unexpected equal DAL committees at subsequent levels: %L and %R" ;
  unit

let publish_dummy_slot ~source ?error ?fee ~index ~message cryptobox =
  let commitment, proof = Dal.(Commitment.dummy_commitment cryptobox message) in
  Helpers.publish_commitment ~source ?fee ?error ~index ~commitment ~proof

(* We check that publishing a slot header with a proof for a different
   slot leads to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_same_content ~source ?fee ~index
    cryptobox =
  let commitment, _proof = Dal.(Commitment.dummy_commitment cryptobox "a") in
  let _commitment, proof = Dal.(Commitment.dummy_commitment cryptobox "b") in
  Helpers.publish_commitment ~source ?fee ~index ~commitment ~proof

(* We check that publishing a slot header with a proof for the "same"
   slot contents but represented using a different [slot_size] leads
   to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_different_slot_size ~source ?fee
    ~index parameters cryptobox ?counter ?force ?error client =
  let cryptobox_params =
    {
      parameters.Dal.Parameters.cryptobox with
      slot_size = 2 * parameters.cryptobox.slot_size;
    }
  in
  let* cryptobox' = Helpers.make_cryptobox cryptobox_params in
  let msg = "a" in
  let commitment, _proof = Dal.(Commitment.dummy_commitment cryptobox msg) in
  let _commitment, proof = Dal.(Commitment.dummy_commitment cryptobox' msg) in
  Helpers.publish_commitment
    ~source
    ?fee
    ~index
    ~commitment
    ~proof
    ?counter
    ?force
    ?error
    client

let publish_commitment ?dont_wait ?counter ?force ~source ?(fee = 1200) ~index
    ~commitment ~proof client =
  let commitment = Dal.Commitment.of_string commitment in
  let proof = Dal.Commitment.proof_of_string proof in
  Helpers.publish_commitment
    ?dont_wait
    ?counter
    ?force
    ~source
    ~fee
    ~index
    ~commitment
    ~proof
    client

(* Produce a slot, store it in the DAL node, and then (try to) publish the same
   commitment for each level between [from] and [into]. *)
let simple_slot_producer ~slot_index ~slot_size ~from ~into dal_node l1_node
    l1_client =
  (* This is the account used to sign injected slot headers on L1. *)
  let source = Constant.bootstrap2 in
  let slot =
    Cryptobox.Internal_for_tests.generate_slot ~slot_size
    |> Bytes.to_string
    |> Helpers.make_slot ~slot_size
  in
  let* commitment, proof = Helpers.store_slot ~slot_index dal_node slot in
  let rec loop current_level =
    if current_level > into then unit
    else
      let* level = Node.wait_for_level l1_node current_level in
      (* Warn when we don't advance level by level, because this means we
         couldn't publish at each level. Also, we force the injection because
         when a level is missed then probably there will be two injection
         attempts at the same level later. *)
      if current_level < level then
        Log.warn
          "[slot_producer] missed some levels (expected level is %d, got %d)"
          current_level
          level ;
      Log.info
        "[slot_producer] publish at slot index %d at level %d"
        slot_index
        level ;
      let* _op_hash =
        publish_commitment
          ~force:true
          ~source
          ~index:slot_index
          ~commitment
          ~proof
          l1_client
      in
      loop (level + 1)
  in
  let* () = loop from in
  Log.info "[slot_producer] will terminate" ;
  unit

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
    Node.RPC.(call node @@ get_chain_block_context_raw_json ~path:["dal"] ())
  in
  if JSON.is_null dal_raw_json then
    Test.fail "Expected the context to contain information under /dal key."
  else
    let json_to_string j =
      JSON.unannotate j |> Ezjsonm.wrap |> Ezjsonm.to_string
    in
    let* confirmed_slots_opt =
      Node.RPC.(call node @@ get_chain_block_context_dal_commitments_history ())
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

let test_slot_management_logic protocol parameters cryptobox node client
    _bootstrap_key =
  let*! () = Client.reveal ~src:"bootstrap6" client in
  let* () = bake_for client in
  Log.info "Inject some valid slot headers" ;
  let* (`OpHash oph1) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~fee:10_000
      ~index:0
      ~message:"a"
      cryptobox
      client
  in
  let* (`OpHash oph2) =
    publish_dummy_slot
      ~source:Constant.bootstrap2
      ~fee:15_000
      ~index:1
      ~message:"b"
      cryptobox
      client
  in
  let* (`OpHash oph3) =
    publish_dummy_slot
      ~source:Constant.bootstrap3
      ~fee:20_000
      ~index:0
      ~message:"c"
      cryptobox
      client
  in
  let* (`OpHash oph4) =
    publish_dummy_slot
      ~source:Constant.bootstrap4
      ~fee:12_000
      ~index:1
      ~message:"d"
      cryptobox
      client
  in
  let* (`OpHash oph5) =
    publish_dummy_slot_with_wrong_proof_for_same_content
      ~source:Constant.bootstrap5
      ~fee:30_000
      ~index:2
      cryptobox
      client
  in
  (* Check another operation now because we are lacking of bootstrap accounts. *)
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* (`OpHash oph6) =
    publish_dummy_slot_with_wrong_proof_for_different_slot_size
      ~source:bootstrap6
      ~fee:30_000
      ~index:2
      parameters
      cryptobox
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
  let* () = bake_for client in
  let* bytes = Node.RPC.call node @@ RPC.get_chain_block_context_raw_bytes () in
  if JSON.(bytes |-> "dal" |> is_null) then
    Test.fail "Expected the context to contain some information about the DAL" ;
  let* operations_result =
    Node.RPC.call node @@ RPC.get_chain_block_operations ()
  in
  let fees_error =
    Failed
      {
        error_id =
          sf
            "proto.%s.dal_publish_commitment_duplicate"
            (Protocol.encoding_prefix protocol);
      }
  in
  let proof_error =
    Failed
      {
        error_id =
          sf
            "proto.%s.dal_publish_commitment_invalid_proof"
            (Protocol.encoding_prefix protocol);
      }
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
  let nb_slots = parameters.Dal.Parameters.number_of_slots in
  let lag = parameters.attestation_lag in
  let* () = repeat (lag - 1) (fun () -> bake_for client) in
  let* _ =
    inject_dal_attestations
      ~nb_slots
      ~signers:[Constant.bootstrap1; Constant.bootstrap2]
      (Slots [1; 0])
      client
  in
  let* _ =
    inject_dal_attestations
      ~nb_slots
      ~signers:[Constant.bootstrap3; Constant.bootstrap4; Constant.bootstrap5]
      (Slots [1])
      client
  in
  let* baker =
    let* level = Node.get_level node in
    baker_for_round_zero node ~level:(level + 1)
  in
  let* () = bake_for ~delegates:(`For [baker]) client in
  let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
  let attestation =
    match metadata.dal_attestation with
    | None ->
        (* Field is part of the encoding when the feature flag is true *)
        Test.fail
          "Field dal_attestation in block headers is mandatory when DAL is \
           activated"
    | Some x -> x
  in
  Check.(
    (Array.length attestation >= 2)
      int
      ~error_msg:"The attestation should refer to at least 2 slots, got %L") ;
  Check.(
    (attestation.(0) = false)
      bool
      ~error_msg:"Expected slot 0 to be un-attested") ;
  Check.(
    (attestation.(1) = true) bool ~error_msg:"Expected slot 1 to be attested") ;
  check_dal_raw_context node

(** This test tests various situations related to DAL slots attestation.
    See the steps inside the test.
*)
let test_slots_attestation_operation_behavior _protocol parameters _cryptobox
    node client _bootstrap_key =
  (* Some helpers *)
  let nb_slots = parameters.Dal.Parameters.number_of_slots in
  let lag = parameters.attestation_lag in
  assert (lag > 1) ;
  let attest ?payload_level ?(signer = Constant.bootstrap2) ~level () =
    let* _op, op_hash =
      inject_dal_attestation_exn
        ?payload_level
        ~force:true
        ~nb_slots
        ~level
        ~signer
        (Slots [0])
        client
    in
    return op_hash
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
    let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
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
  (* Just bake some blocks before starting attesting. *)
  let* () = bake_for ~count:4 client in

  (* No header published, we just play with attestations with various levels;
     - Initially, only [h3] is applied, [h1; h2] are outdated, and [h4] is
       branch_delayed. After baking a block, [h3] is included in a block and
       [h4] becomes applied.
     - No slot is confirmed as no slot header is published.
  *)
  let* now = Node.wait_for_level node 5 in
  let* (`OpHash h1) = attest ~level:2 () in
  let outdated = [h1] in
  Log.info "expected mempool: outdated: h1 = %s" h1 ;
  let* () = mempool_is ~__LOC__ Mempool.{empty with outdated} in
  let* (`OpHash h2) = attest ~level:(now - 1) () in
  (* level [now-1] is allowed in the mempool for attestations *)
  let* (`OpHash h2') = attest ~level:(now - 2) () in
  let outdated = [h1; h2'] in
  Log.info "expected mempool: outdated: h1, h2' = %s, validated: h2 = %s" h2' h2 ;
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h2]}
  in
  let* (`OpHash h3) = attest ~level:now () in
  Log.info "expected mempool: outdated: h1, h2', validated: h2, h3 = %s" h3 ;
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h2; h3]}
  in
  (* Level [now+1] is allowed in the mempool for attestations, so we inject for
     level [now+2]. We also take care that the attester is not the same one as
     the baker (who will also inject an attestation, and the two ops will
     conflict). *)
  let* baker1 = baker_for_round_zero node ~level:(now + 1) in
  let* baker2 = baker_for_round_zero node ~level:(now + 2) in
  let signer1 = different_delegate baker1 in
  let signer2 = different_delegate baker1 in
  let* (`OpHash h4) =
    attest ~payload_level:now ~level:(now + 1) ~signer:signer1 ()
  in
  let* (`OpHash h4') =
    attest ~payload_level:now ~level:(now + 2) ~signer:signer2 ()
  in
  Log.info
    "expected mempool: outdated: h1, h2', validated: h2, h3, h4 = %s, \
     branch_delayed: h4' = %s"
    h4
    h4' ;
  let* () =
    mempool_is
      ~__LOC__
      Mempool.
        {empty with outdated; validated = [h2; h3; h4]; branch_delayed = [h4']}
  in
  let* () = bake_for ~delegates:(`For [baker1]) client in
  let outdated = [h1; h2; h2'] in
  (* [h4] and [h4'] cannot be included because their payload hash is wrong. *)
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h4; h4']}
  in
  let* () = bake_for ~delegates:(`For [baker2]) client in
  let* _json =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let* () =
    mempool_is ~__LOC__ Mempool.{empty with outdated; validated = [h4; h4']}
  in
  check_slots_availability ~__LOC__ ~attested:[]

let test_all_available_slots _protocol parameters cryptobox node client
    _bootstrap_key =
  let nb_slots = parameters.Dal.Parameters.number_of_slots in
  (* We ensure there is at least one account per manager operation to
     be included. This is because of the 1M restrction. Another way
     could use batched operations, but the current DAL helpers are
     difficult to use for batched operations. *)
  let* accounts =
    Seq.ints 0 |> Seq.take nb_slots |> List.of_seq
    |> Lwt_list.map_p (fun index ->
           Client.show_address
             ~alias:("bootstrap" ^ string_of_int (1 + index))
             client)
  in
  let* () =
    Lwt_list.iter_p
      (fun source ->
        let*! () = Client.reveal ~src:source.Account.alias client in
        unit)
      (List.filteri
         (fun i _ -> i >= Array.length Account.Bootstrap.keys)
         accounts)
  in
  let* () = bake_for client in
  let* () =
    Lwt_list.iteri_p
      (fun index source ->
        let* (`OpHash _oph1) =
          publish_dummy_slot
            ~source
            ~fee:13_000
            ~index
            ~message:"a"
            cryptobox
            client
        in
        unit)
      accounts
  in
  let* () = bake_for client in
  let* result =
    Node.RPC.(call ~rpc_hooks node @@ get_chain_block_metadata_raw ())
  in
  JSON.encode result |> hooks.on_log ;
  let* operations = Node.RPC.(call node @@ get_chain_block_operations ()) in
  let () =
    (* Check validity of operations *)
    let manager_operations = JSON.(operations |=> 3 |> as_list) in
    Check.(List.length manager_operations = nb_slots)
      ~__LOC__
      ~error_msg:"Expected %R manager operations.Got %L"
      Check.int ;
    List.iter
      (fun operation ->
        let status =
          JSON.(
            operation |-> "contents" |=> 0 |-> "metadata" |-> "operation_result"
            |-> "status" |> as_string)
        in
        Check.(status = "applied")
          ~__LOC__
          ~error_msg:
            "Expected all slots to be included. At least one operation failed \
             with status: %L"
          Check.string)
      manager_operations
  in
  unit

(* Tests that DAL attestation payloads are only attached if the attestation is
   from a DAL-committee member. This test creates a new account and registers it
   as a baker, and bakes blocks until it reaches a level where the new account
   is in the TB committee but not in the DAL committee).*)
let test_slots_attestation_operation_dal_committee_membership_check _protocol
    parameters _cryptobox node client _bootstrap_key =
  (* The attestation from the bootstrap account should succeed as the bootstrap
     node has sufficient stake to be in the DAL committee. *)
  let nb_slots = parameters.Dal.Parameters.number_of_slots in
  let number_of_shards = parameters.cryptobox.number_of_shards in
  Log.info "number_of_shards = %d" number_of_shards ;
  let* () = bake_for client in
  let* level = Client.level client in
  let* _op, `OpHash _oph =
    inject_dal_attestation_exn
      ~nb_slots
      ~level
      ~signer:Constant.bootstrap1
      (Slots [])
      client
  in
  (* Set up a new account that holds the right amount of tez and make sure it
     can be an attester. *)
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  (* With [consensus_committee_size = 1024] slots in total, the new baker should
     get roughly 1024 / 64 = 16 TB slots on average. So the probability that it
     is on TB committee is high. With [number_of_shards = 256] (which is the
     current default), the new baker should be assigned roughly 256 / 64 = 4
     shards on average. We should encounter relatively quickly a level where it
     is assigned to no shard. *)
  let stake = Tez.of_mutez_int (Protocol.default_bootstrap_balance / 64) in
  let amount = Tez.(stake + of_int 10) in
  let* new_account = Client.gen_and_show_keys client in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client
  in
  let* () = bake_for client in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = bake_for client in
  let* () = Client.register_key new_account.alias client in
  let* () = bake_for client in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client in
  let num_cycles = 2 + consensus_rights_delay in
  Log.info
    "Bake for %d cycles for %s to be a baker"
    num_cycles
    new_account.alias ;
  let* () = bake_for ~count:(num_cycles * blocks_per_cycle) client in
  (* We iterate until we find a level for which the new account has no assigned
     shard. *)
  let rec iter () =
    let* level = Client.level client in
    let* committee = Dal.Committee.at_level node ~level () in
    if
      List.exists
        (fun member ->
          String.equal member.Dal.Committee.attester new_account.public_key_hash)
        committee
    then (
      Log.info "Bake another block to change the DAL committee" ;
      let* () = bake_for client in
      iter ())
    else (
      Log.info "The new account is not in the DAL committee" ;
      Log.info "We check that the new account is in the Tenderbake committee" ;
      let* () =
        check_in_TB_committee ~__LOC__ node new_account.public_key_hash ~level
      in
      let* _op, `OpHash _oph =
        inject_dal_attestation_exn
          ~error:Operation.dal_data_availibility_attester_not_in_committee
          ~nb_slots
          ~level
          ~signer:new_account
          (Slots [])
          client
      in
      (* Bake with all the bootstrap accounts, but not with the new account. *)
      let* () =
        let bootstrap_accounts =
          Array.to_list Account.Bootstrap.keys
          |> List.map (fun a -> a.Account.public_key_hash)
        in
        bake_for ~delegates:(`For bootstrap_accounts) client
      in
      let* json =
        Node.RPC.call node
        @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
      in
      let num_ops = JSON.as_list json |> List.length in
      Check.(
        (num_ops = Array.length Account.Bootstrap.keys)
          int
          ~error_msg:"Expected %R operations, found %L") ;
      unit)
  in
  iter ()

let test_dal_node_slot_management _protocol parameters _cryptobox _node client
    dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_index = 0 in
  let slot_content = "test with invalid UTF-8 byte sequence \xFA" in
  let* _slot_commitment =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      Helpers.(make_slot ~slot_size slot_content)
  in
  let* () = bake_for client in
  let* published_level = Client.level client in
  (* Finalize the publication. *)
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node published_level
  in
  let* () = bake_for ~count:2 client in
  let* () = wait_for_dal_node in
  let* received_slot =
    Dal_RPC.(
      call dal_node
      @@ get_level_slot_content ~slot_level:published_level ~slot_index)
  in
  let received_slot_content = Helpers.content_of_slot received_slot in
  Check.(
    (slot_content = received_slot_content)
      string
      ~error_msg:"Wrong slot content: Expected: %L. Got: %R") ;
  let* _ =
    Dal_RPC.(
      call dal_node
      @@ get_level_slot_status ~slot_level:published_level ~slot_index)
  in
  let* pages =
    Dal_RPC.(call dal_node @@ get_level_slot_pages ~published_level ~slot_index)
  in
  Check.(
    slot_content = Helpers.(content_of_slot @@ slot_of_pages ~slot_size pages))
    Check.string
    ~__LOC__
    ~error_msg:"Unexecpeted slot fetched: Expected: %L. Got: %R" ;
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

(* Similar to [publish_and_store_slot] but additionally bakes [1 +
   number_of_extra_blocks] blocks to trigger the publication of the
   shards of the published slot commitment. Moreover, the [wait_slot]
   argument can be used to wait for the shards to be received by one
   or several other DAL nodes. Returns the published commitment and
   the level at which it was published. *)
let publish_store_and_wait_slot ?counter ?force ?(fee = 12_000) node client
    slot_producer_dal_node source ~index ~wait_slot
    ~number_of_extra_blocks_to_bake ?delegates content =
  let* commitment, proof =
    Helpers.store_slot slot_producer_dal_node ~slot_index:index content
  in
  let* first_level = Client.level client in
  let published_level = first_level + 1 in
  let p = wait_slot ~published_level ~slot_index:index in
  let* (`OpHash ophash) =
    publish_commitment
      ?counter
      ?force
      ~source
      ~fee
      ~index
      ~commitment
      ~proof
      client
  in
  (* Bake a first block to include the operation. *)
  let* () = bake_for ?delegates client in
  (* Check that the operation is included. *)
  let* included_manager_operations =
    let manager_operation_pass = 3 in
    Node.RPC.(
      call node
      @@ get_chain_block_operation_hashes_of_validation_pass
           manager_operation_pass)
  in
  let () =
    Check.list_mem
      Check.string
      ~__LOC__
      ophash
      included_manager_operations
      ~error_msg:"DAL commitment publishment operation not found in head block."
  in
  (* Bake some more blocks to finalize the block containing the publication. *)
  let* () =
    if number_of_extra_blocks_to_bake > 0 then
      bake_for ~count:number_of_extra_blocks_to_bake ?delegates client
    else unit
  in
  (* Wait for the shards to be received *)
  let* res = p in
  return (published_level, commitment, res)

let publish_store_and_attest_slot ?counter ?force ?fee client node dal_node
    source ~index ~content ~attestation_lag ~number_of_slots =
  let* _commitment =
    Helpers.publish_and_store_slot
      ?counter
      ?force
      ?fee
      client
      dal_node
      source
      ~index
      content
  in
  let* () = repeat attestation_lag (fun () -> bake_for client) in
  inject_dal_attestations_and_bake node client ~number_of_slots (Slots [index])

let check_get_commitment dal_node ~slot_level check_result slots_info =
  Lwt_list.iter_s
    (fun (slot_index, commitment') ->
      let* response =
        Dal_RPC.(
          call_raw dal_node
          @@ get_level_index_commitment ~slot_index ~slot_level)
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

let get_commitment_not_found _commit r =
  RPC_core.check_string_response ~code:404 r

let check_stored_level_headers ~__LOC__ dal_node ~pub_level ~number_of_slots
    ~number_of_headers =
  let* number_of_stored_commitments =
    Lwt_list.fold_left_s
      (fun accu slot_index ->
        let* response =
          Dal_RPC.(
            call_raw dal_node
            @@ get_level_index_commitment ~slot_level:pub_level ~slot_index)
        in
        match response.code with
        | 200 -> return (accu + 1)
        | 404 -> return accu
        | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code)
      0
      (List.init number_of_slots Fun.id)
  in
  Check.(number_of_stored_commitments = number_of_headers)
    ~__LOC__
    Check.int
    ~error_msg:"Unexpected slot headers length (got = %L, expected = %R)" ;
  unit

let check_slot_status ~__LOC__ ?expected_status dal_node ~slot_level slots_info
    =
  let test (slot_index, commitment) =
    let* commitment_is_stored =
      let rpc = Dal_RPC.get_level_index_commitment ~slot_level ~slot_index in
      let* response = Dal_RPC.call_raw dal_node rpc in
      match response.code with
      | 200 ->
          let published_commitment =
            rpc.decode @@ JSON.parse ~origin:"RPC response" response.body
          in
          return (commitment = published_commitment)
      | _ -> return false
    in
    match (commitment_is_stored, expected_status) with
    | false, None -> unit
    | true, None ->
        Test.fail
          ~__LOC__
          "It was expected that the given commitment is not stored, but it is."
    | true, Some expected_status ->
        let* status =
          Dal_RPC.(
            call dal_node @@ get_level_slot_status ~slot_level ~slot_index)
        in
        Check.(status = expected_status)
          ~__LOC__
          Check.string
          ~error_msg:
            "The value of the fetched status should match the expected one \
             (current = %L, expected = %R)" ;
        unit
    | false, Some _ ->
        Test.fail
          ~__LOC__
          "It was expected that the given commitment is stored, but it is not."
  in
  Lwt_list.iter_s test slots_info

let test_dal_node_slots_headers_tracking _protocol parameters _cryptobox node
    client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  let check_stored_level_headers =
    check_stored_level_headers dal_node ~number_of_slots
  in
  let* level = Node.get_level node in
  let pub_level = level + 1 in
  let publish ?(fee = 12_000) source ~index content =
    let content = Helpers.make_slot ~slot_size content in
    let* commitment =
      Helpers.publish_and_store_slot ~fee client dal_node source ~index content
    in
    return (index, commitment)
  in
  let* slot0 = publish Constant.bootstrap1 ~index:0 "test0" in
  let* slot1 = publish Constant.bootstrap2 ~index:1 "test1" in
  let* () =
    (* The slot headers are not yet in a block. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in

  (* slot2_a and slot3 will not be included as successful, because slot2_b has
     better fees for slot4, while slot3's fee is too low. slot4 is not injected
     into L1 or DAL nodes.

     We decide to have two failed slots instead of just one to better test some
     internal aspects of failed slots headers recording (i.e. having a collection
     of data instead of just one). *)
  let* slot2_a = publish Constant.bootstrap3 ~index:4 ~fee:12_000 "test4_a" in
  let* slot2_b = publish Constant.bootstrap4 ~index:4 ~fee:13_500 "test4_b" in
  let* slot3 = publish Constant.bootstrap5 ~index:5 ~fee:1 "test5" in
  let* slot4 =
    let slot = Helpers.make_slot ~slot_size "never associated to a slot_id" in
    let* commit, _proof = Helpers.store_slot dal_node ~slot_index:6 slot in
    return (6, commit)
  in

  Log.info "Just after injecting slots and before baking, there are no headers" ;
  (* because headers are stored based on information from finalized blocks *)
  let* () =
    check_slot_status
      dal_node
      ~slot_level:level
      ~__LOC__
      [slot0; slot1; slot2_a; slot2_b; slot3; slot4]
  in
  let* () =
    (* The slot headers are still not yet in a block. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:0
  in

  let wait_block_processing1 = wait_for_layer1_head dal_node pub_level in
  let* () = bake_for client in
  let* () = wait_block_processing1 in

  Log.info
    "After baking one block, there is still no header, because the block is \
     not final" ;
  let* () =
    check_slot_status
      dal_node
      ~slot_level:level
      ~__LOC__
      [slot0; slot1; slot2_a; slot2_b; slot3; slot4]
  in

  let wait_block_processing2 = wait_for_layer1_final_block dal_node pub_level in
  let* () = bake_for ~count:2 client in
  let* () = wait_block_processing2 in

  Log.info
    "After baking two more blocks, the slots' status is as expected (eg for \
     published slots it's Waiting_attestation)" ;
  let ok = [slot0; slot1; slot2_b] in
  let ko = slot3 :: slot4 :: List.map (fun (i, c) -> (i + 100, c)) ok in
  let* () =
    (* There are 3 published slots: slot0, slot1, and slot2_b *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:3
  in
  let* slot_headers =
    Lwt_list.filter_map_s
      (fun slot_index ->
        let commitment_rpc =
          Dal_RPC.get_level_index_commitment ~slot_level:pub_level ~slot_index
        in
        let* response = Dal_RPC.call_raw dal_node commitment_rpc in
        match response.code with
        | 200 ->
            let commitment =
              commitment_rpc.decode
              @@ JSON.parse ~origin:"RPC response" response.body
            in
            let* status =
              Dal_RPC.(
                call dal_node
                @@ get_level_slot_status ~slot_level:pub_level ~slot_index)
            in
            if status = "waiting_attestation" then some (slot_index, commitment)
            else none
        | 404 -> none
        | code -> Test.fail ~__LOC__ "Unexpected HTTP response code %d" code)
      (List.init number_of_slots Fun.id)
  in
  Check.(slot_headers = ok)
    Check.(list (tuple2 int string))
    ~error_msg:
      "Published header is different from stored header (current = %L, \
       expected = %R)" ;
  let check_slot_status ?expected_status l =
    check_slot_status ?expected_status dal_node ~slot_level:pub_level l
  in
  let check_get_commitment =
    check_get_commitment dal_node ~slot_level:pub_level
  in

  (* Slots waiting for attestation. *)
  let* () = check_get_commitment get_commitment_succeeds ok in
  let* () =
    check_slot_status ~__LOC__ ~expected_status:"waiting_attestation" ok
  in
  (* slot_2_a is not selected. *)
  let* () = check_slot_status ~__LOC__ [slot2_a] in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in

  let lag = parameters.attestation_lag in
  Log.info
    "Attest slots slot0 and slot2_b, and wait for the attestations to be final" ;
  let attested = [slot0; slot2_b] in
  let unattested = [slot1] in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  let* () = bake_for ~count:(lag - 3) client in
  let wait_block_processing3 =
    let attested_level = pub_level + lag in
    wait_for_layer1_final_block dal_node attested_level
  in
  let* () =
    inject_dal_attestations_and_bake
      node
      client
      ~number_of_slots
      (Slots (List.map fst attested))
  in
  let* () = bake_for ~count:2 client in
  let* () = wait_block_processing3 in
  let* () =
    (* The unattested slot has been removed. *)
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:2
  in

  Log.info "Check that the store is as expected" ;
  (* Slots confirmed. *)
  let* () = check_get_commitment get_commitment_succeeds attested in
  (* Slots that were waiting for attestation and now attested. *)
  let* () = check_slot_status ~__LOC__ ~expected_status:"attested" attested in
  (* Slots not published or not included in blocks. *)
  let* () = check_get_commitment get_commitment_not_found ko in
  (* Slots that were waiting for attestation and now unattested. *)
  let* () = check_slot_status ~__LOC__ unattested in
  (* slot2_a is still not selected. *)
  let* () = check_slot_status ~__LOC__ [slot2_a] in
  (* slot3 never finished in an L1 block, so the DAL node did not store a status for it. *)
  let* () = check_slot_status ~__LOC__ [slot3] in
  (* slot4 is never injected in any of the nodes. So, it's not
     known by the Dal node. *)
  let* () = check_slot_status ~__LOC__ [slot4] in
  (* The number of stored slots has not changed. *)
  let* () =
    check_stored_level_headers
      ~__LOC__
      ~pub_level:(pub_level - 1)
      ~number_of_headers:0
  in
  let* () =
    check_stored_level_headers ~__LOC__ ~pub_level ~number_of_headers:2
  in
  check_stored_level_headers
    ~__LOC__
    ~pub_level:(pub_level + 1)
    ~number_of_headers:0

let generate_dummy_slot slot_size =
  String.init slot_size (fun i ->
      match i mod 3 with 0 -> 'a' | 1 -> 'b' | _ -> 'c')

let get_shards dal_node ~slot_level ~slot_index downloaded_shard_ids =
  Lwt_list.map_s
    (fun shard_index ->
      Dal_RPC.(
        call dal_node
        @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index))
    downloaded_shard_ids

let test_dal_node_rebuild_from_shards _protocol parameters cryptobox node client
    dal_node =
  (* Steps in this integration test:
     1. Run a dal node
     2. Generate and publish a full slot, then bake
     3. Download exactly 1/redundancy_factor shards
        from this slot (it would work with more)
     4. Ensure we can rebuild the original data using the above shards
  *)
  let crypto_params = parameters.Dal.Parameters.cryptobox in
  let slot_size = crypto_params.slot_size in
  let slot_content = generate_dummy_slot slot_size in
  let publish = Helpers.publish_and_store_slot client dal_node in
  let slot_index = 0 in
  let* _commitment =
    publish Constant.bootstrap1 ~index:slot_index
    @@ Helpers.make_slot ~slot_size slot_content
  in
  let* () = bake_for client in
  let* slot_level = Node.wait_for_level node 1 in
  let* () = bake_until_processed ~level:slot_level client [dal_node] in
  let number_of_shards =
    (crypto_params.number_of_shards / crypto_params.redundancy_factor) - 1
  in
  let downloaded_shard_ids =
    range 0 number_of_shards
    |> List.map (fun i -> i * crypto_params.redundancy_factor)
  in
  let* shards =
    get_shards dal_node ~slot_level ~slot_index downloaded_shard_ids
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

let test_dal_node_rpc_list _protocol _parameters _cryptobox _node client
    dal_node =
  let endpoint = Client.Foreign_endpoint (Dal_node.as_rpc_endpoint dal_node) in
  let* (_ : string) = Client.rpc_list ~hooks ~endpoint client in
  unit

let commitment_of_slot cryptobox slot =
  let polynomial =
    Cryptobox.polynomial_from_slot
      cryptobox
      (Helpers.content_of_slot slot |> Bytes.of_string)
    |> Result.get_ok
  in
  match Cryptobox.commit cryptobox polynomial with
  | Ok cm -> cm
  | Error
      ((`Invalid_degree_strictly_less_than_expected _ | `Prover_SRS_not_loaded)
       as commit_error) ->
      Test.fail "%s" (Cryptobox.string_of_commit_error commit_error)

let test_dal_node_test_post_slot _protocol parameters cryptobox _node client
    dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let mk_slot size =
    Helpers.make_slot ~padding:false ~slot_size (generate_dummy_slot size)
  in
  let failing_post_slot_rpc ?slot_index ~body_rex slot =
    let* response = Dal_RPC.(call_raw dal_node @@ post_slot ?slot_index slot) in
    return @@ RPC_core.check_string_response ~body_rex ~code:500 response
  in
  let size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot_big = mk_slot (size + 1) in
  let slot_small = mk_slot (size - 1) in
  let slot_ok = mk_slot size in
  let* () = failing_post_slot_rpc ~body_rex:"post_slot_too_large" slot_big in
  let* commitment1, _proof = Dal_RPC.(call dal_node @@ post_slot slot_ok) in
  let* commitment2, _proof = Dal_RPC.(call dal_node @@ post_slot slot_ok) in
  (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/4250
     The second RPC call above succeeeds, but the (untested) returned HTTP status
     should likely be 200 and 201 in the first similar RPC call.
  *)
  Check.(commitment1 = commitment2)
    Check.string
    ~error_msg:
      "Storing a slot twice should return the same commitment (current = %L, \
       expected = %R)" ;

  (* The DAL node of this test can only publish on slot index 0. *)
  let* () =
    failing_post_slot_rpc
      ~slot_index:1
      ~body_rex:"cannot_publish_on_slot_index"
      slot_small
  in
  let slot_index = 0 in
  let* commitment3, proof3 =
    Dal_RPC.(call dal_node @@ post_slot ~slot_index slot_small)
  in
  (* To retrieve the content of the slot corresponding to commitment3,
     we need to publish the commitment on the L1 and bake some blocks
     to finalize the publication. *)
  let* _ =
    Dal.Helpers.publish_commitment
      ~source:Constant.bootstrap1
      ~index:slot_index
      ~commitment:(Dal.Commitment.of_string commitment3)
      ~proof:(Dal.Commitment.proof_of_string proof3)
      client
  in
  let* () = bake_for client in
  let* slot_level = Client.level client in
  (* Finalize the publication. *)
  let* () = bake_until_processed ~level:slot_level client [dal_node] in

  (* The POST /slots RPC accepts slots shorter than the size and pads
     them.  The content_of_slot helper removes the padding. *)
  let* padded_slot =
    Dal_RPC.(call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  Check.(generate_dummy_slot (size - 1) = Helpers.content_of_slot padded_slot)
    Check.string
    ~error_msg:
      "A slot shorter than the expected size should be padded by the POST \
       /slots RPC expected %L, got %R." ;
  let commitment4 =
    Cryptobox.Commitment.to_b58check @@ commitment_of_slot cryptobox slot_ok
  in
  Check.(commitment1 = commitment4)
    Check.string
    ~error_msg:
      "The commitment of a stored commitment should match the one computed \
       locally (current = %L, expected = %R)" ;
  unit

let test_dal_node_test_get_level_slot_content _protocol parameters _cryptobox
    _node client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let slot = Helpers.make_slot ~slot_size (generate_dummy_slot slot_size) in
  let slot_index = 0 in
  let* commitment, proof = Dal_RPC.(call dal_node @@ post_slot slot) in
  let* _ =
    Dal.Helpers.publish_commitment
      ~source:Constant.bootstrap1
      ~index:slot_index
      ~commitment:(Dal.Commitment.of_string commitment)
      ~proof:(Dal.Commitment.proof_of_string proof)
      client
  in
  let* () = bake_for client in
  let* slot_level = Client.level client in
  (* Publication is not yet finalized, the RPC should fail with 404
     error code. *)
  let* () =
    let* response =
      Dal_RPC.(
        call_raw dal_node @@ get_level_slot_content ~slot_level ~slot_index)
    in
    return @@ RPC_core.check_string_response ~code:404 response
  in
  (* Finalize the publication. *)
  let* () = bake_until_processed ~level:slot_level client [dal_node] in
  let* got_slot =
    Dal_RPC.(call dal_node @@ get_level_slot_content ~slot_level ~slot_index)
  in
  Check.(Helpers.content_of_slot slot = Helpers.content_of_slot got_slot)
    Check.string
    ~error_msg:
      "The slot content retrieved from the node is not as expected (expected = \
       %L, got = %R)" ;
  unit

let test_dal_node_import_snapshot _protocol parameters _cryptobox node client
    dal_node =
  let* commitment, proof =
    Helpers.(
      store_slot dal_node ~slot_index:0
      @@ make_slot
           ~slot_size:parameters.Dal_common.Parameters.cryptobox.slot_size
           "content1")
  in
  let* _oph =
    publish_commitment
      ~source:Constant.bootstrap1
      ~index:0
      ~commitment
      ~proof
      client
  in
  let* level = Node.get_level node in
  let* () = bake_for client in
  let* export_level = Node.wait_for_level node (level + 1) in
  let file = Temp.file "snapshot" in
  let* () =
    Node.snapshot_export ~history_mode:Rolling_history ~export_level node file
  in
  let node2 = Node.create [] in
  let* () = Node.config_init node2 [] in
  (* We update the configuration because by default on sandbox mode,
     DAL is not activated. *)
  let config : Cryptobox.Config.t = {activated = true; bootstrap_peers = []} in
  let* () =
    Node.Config_file.update node2 Node.Config_file.set_sandbox_network
  in
  let* () =
    Node.Config_file.update
      node2
      (Node.Config_file.set_network_with_dal_config config)
  in
  let* () = Node.snapshot_import node2 file in
  unit

let test_dal_node_startup =
  Protocol.register_test
    ~__FILE__
    ~title:"dal node startup"
    ~tags:[Tag.tezos2; "dal"]
    ~uses:(fun _protocol -> [Constant.octez_dal_node])
    ~supports:Has_predecessor
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
  let dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config dal_node in
  let* () = run_dal dal_node in
  assert (Dal_node.is_running_not_ready dal_node) ;
  let* () = Dal_node.terminate dal_node in
  let* () = Node.terminate node in
  let* () =
    Node.Config_file.update
      node
      (Node.Config_file.set_sandbox_network_with_user_activated_overrides
         [(Protocol.hash previous_protocol, Protocol.hash protocol)])
  in
  let* () = Node.run node nodes_args in
  let* () = Node.wait_for_ready node in
  let* () = run_dal dal_node in
  let* () =
    Lwt.join
      [
        Dal_node.wait_for dal_node "dal_plugin_resolved.v0" (fun _ -> Some ());
        bake_for client;
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
  if bake then bake_for client else unit

let check_saved_value_in_pvm ~rpc_hooks ~name ~expected_value sc_rollup_node =
  let* encoded_value =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_state ~key:(sf "vars/%s" name) ()
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

let rollup_node_interprets_dal_pages_helper ~protocol:_ client sc_rollup
    sc_rollup_node =
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
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
  let* () = repeat 2 (fun () -> bake_for client) in
  let* _lvl =
    Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup_node (level + 1)
  in
  check_saved_value_in_pvm
    ~rpc_hooks
    ~name:"value"
    ~expected_value
    sc_rollup_node

let rollup_node_applies_dal_pages protocol parameters dal_node sc_rollup_node
    sc_rollup_address node client _pvm_name =
  (* Check that the rollup node downloaded the confirmed slots to which it is
     subscribed:

     0. Run a DAL node.

     1. Send three slots to DAL node and obtain corresponding headers.

     2. Run rollup node for an originated rollup.

     3. Setting the parameters of the PVM. Rollup node subscribes to slots 0, 2,
     4 and 6.

     4. Publish the three slot headers for slots with indexes 0, 1 and 2.

     5. Attest only slots 1 and 2.

     6. Bake attestation_lag blocks to take attestations into account.

     7. Verify that the rollup node interprets the DAL pages of the attested DAL
     slots.
  *)
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let page_size = parameters.Dal.Parameters.cryptobox.page_size in
  let store_slot ~slot_index content =
    Helpers.(store_slot ~slot_index dal_node @@ make_slot ~slot_size content)
  in
  Log.info
    "Step 1: send three slots to DAL node and obtain corresponding headers" ;
  let slot_contents_0 = " 10 " in
  let* commitment_0, proof_0 = store_slot ~slot_index:0 slot_contents_0 in
  let slot_contents_1 = " 200 " in
  let* commitment_1, proof_1 = store_slot ~slot_index:1 slot_contents_1 in
  let slot_contents_2 = " 400 " in
  let* commitment_2, proof_2 = store_slot ~slot_index:2 slot_contents_2 in

  Log.info "Step 2: run rollup node for an originated rollup" ;
  let* genesis_info =
    Client.RPC.call ~hooks client
    @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
         sc_rollup_address
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in

  Check.(level = init_level)
    Check.int
    ~error_msg:
      "Current level has moved past origination level (current = %L, expected \
       = %R)" ;

  Log.info
    "Step 3: set the PVM parameters; rollup node subscribes to slots 0, 2, 4 \
     and 6" ;
  let number_of_slots = parameters.number_of_slots in
  let attestation_lag = parameters.attestation_lag in
  let number_of_pages = slot_size / page_size in
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

  Log.info "Step 4: publish the slot headers for indexes 0, 1, and 2" ;
  let*! () =
    Client.publish_dal_commitment
      ~src:Constant.bootstrap1.alias
      ~slot_index:0
      ~commitment:commitment_0
      ~proof:proof_0
      client
  in
  let*! () =
    Client.publish_dal_commitment
      ~src:Constant.bootstrap2.alias
      ~slot_index:1
      ~commitment:commitment_1
      ~proof:proof_1
      client
  in
  let*! () =
    Client.publish_dal_commitment
      ~src:Constant.bootstrap3.alias
      ~slot_index:2
      ~commitment:commitment_2
      ~proof:proof_2
      client
  in

  let* () = bake_for client in
  let* slots_published_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + 2)
  in

  Log.info "Step 5: attest only slots 1 and 2" ;
  let* () = repeat (attestation_lag - 1) (fun () -> bake_for client) in
  let* () =
    inject_dal_attestations_and_bake node client ~number_of_slots (Slots [2; 1])
  in
  let* slot_confirmed_level =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (slots_published_level + attestation_lag)
  in
  Check.(slot_confirmed_level = slots_published_level + attestation_lag)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;

  Log.info
    "Step 6: bake attestation_lag blocks to take attestations into account" ;
  let* () = repeat attestation_lag (fun () -> bake_for client) in

  let* level =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (slot_confirmed_level + attestation_lag)
  in
  Check.(level = slot_confirmed_level + attestation_lag)
    Check.int
    ~error_msg:
      "Current level has moved past slot attestation level (current = %L, \
       expected = %R)" ;

  Log.info "Step 7: Check that the rollup node interprets the DAL pages" ;
  rollup_node_interprets_dal_pages_helper
    ~protocol
    client
    sc_rollup_address
    sc_rollup_node

(* Test that the rollup kernel can fetch and store a requested DAL page. Works as follows:
   - Originate a rollup with a kernel that:
      - Downloads page 0 from slot 0 published at level [current_level - attestation_lag].
      - Writes the downloaded slot contents to "/output" in durable storage.
   - At level N, publish a slot to the L1 and DAL.
   - Bake until [attestation_lag] blocks so the L1 attests the published slot.
   - Confirm that the kernel downloaded the slot and wrote the content to "/output/slot-<index>". *)
let test_reveal_dal_page_in_fast_exec_wasm_pvm _protocol parameters dal_node
    sc_rollup_node _sc_rollup_address node client pvm_name =
  Log.info "Assert attestation_lag value." ;
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6270
     Make the kernel robust against attestation_lag changes. *)
  Check.(
    (parameters.Dal.Parameters.attestation_lag = 4)
      int
      ~error_msg:
        "The kernel used in the test assumes attestation_lag of %R, got %L") ;
  let slot_size = parameters.cryptobox.slot_size in
  Check.(
    (slot_size = 32768)
      int
      ~error_msg:"The kernel used in the test assumes slot_size of %R, got %L") ;
  Check.(
    (parameters.cryptobox.page_size = 128)
      int
      ~error_msg:"The kernel used in the test assumes page_size of %R, got %L") ;
  Log.info "Originate rollup." ;
  let* {boot_sector; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.dal_echo_kernel
  in
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"dal_echo_kernel"
      ~src:Constant.bootstrap1.alias
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"unit"
      client
  in
  let* () = bake_for client in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let slot_size = parameters.cryptobox.slot_size in
  Log.info "Store slot content to DAL node and submit header." ;
  let slot_content = generate_dummy_slot slot_size in
  let* () =
    publish_store_and_attest_slot
      client
      node
      dal_node
      Constant.bootstrap1
      ~index:0
      ~content:(Helpers.make_slot ~slot_size slot_content)
      ~attestation_lag:parameters.attestation_lag
      ~number_of_slots:parameters.number_of_slots
  in
  let* level = Node.get_level node in
  Log.info "Assert that the slot was attested." ;
  let* {dal_attestation; _} =
    Node.RPC.(call node @@ get_chain_block_metadata ())
  in
  Check.((Some [|true|] = dal_attestation) (option (array bool)))
    ~error_msg:"Unexpected DAL attestations: expected %L, got %R" ;
  Log.info "Wait for the rollup node to catch up to the latest level." ;
  let* _ = Sc_rollup_node.wait_for_level ~timeout:3. sc_rollup_node level in
  Log.info "Read and assert against value written in durable storage." ;
  let key = "/output/slot-0" in
  let* value_written =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  let encoded_slot_content =
    match Hex.of_string slot_content with `Hex s -> s
  in
  Check.(
    (Some encoded_slot_content = value_written)
      (option string)
      ~error_msg:"Expected value written in /output/slot-0 to be %L, got %R") ;
  unit

let check_profiles ~__LOC__ dal_node ~expected =
  let* profiles = Dal_RPC.(call dal_node @@ get_profiles ()) in
  return
    Check.(
      (profiles = expected)
        Dal.Check.profiles_typ
        ~error_msg:
          (__LOC__ ^ " : Unexpected profiles (Actual: %L <> Expected: %R)"))

let check_topics_peers ~__LOC__ dal_node ~expected =
  let normalize_peers l = List.sort String.compare l in
  let compare_topics {Dal.RPC.topic_slot_index = s1; topic_pkh = p1}
      {Dal.RPC.topic_slot_index = s2; topic_pkh = p2} =
    let c = Int.compare s1 s2 in
    if c = 0 then String.compare p1 p2 else c
  in
  let normalize_topics_peers l =
    l
    |> List.map (fun (topic, peers) -> (topic, normalize_peers peers))
    |> List.sort (fun (t1, _p1) (t2, _p2) -> compare_topics t1 t2)
  in
  let* topic_peers = Dal_RPC.(call dal_node @@ get_topics_peers ()) in
  return
    Check.(
      (normalize_topics_peers topic_peers = normalize_topics_peers expected)
        Dal.Check.topics_peers_typ
        ~error_msg:
          (__LOC__
         ^ " : Unexpected topic - peers association (Actual: %L <> Expected: \
            %R)"))

let test_dal_node_test_patch_profile _protocol _parameters _cryptobox _node
    _client dal_node =
  let check_bad_attester_pkh_encoding profile =
    let* response = Dal_RPC.(call_raw dal_node @@ patch_profiles [profile]) in
    return @@ RPC_core.check_string_response ~code:400 response
  in
  let patch_profile_rpc profile =
    Dal_RPC.(call dal_node (patch_profiles [profile]))
  in
  let profile1 = Dal_RPC.Attester Constant.bootstrap1.public_key_hash in
  let profile2 = Dal_RPC.Attester Constant.bootstrap2.public_key_hash in
  (* We start with empty profile list *)
  let* () = check_profiles ~__LOC__ dal_node ~expected:(Controller []) in
  (* Adding [Attester] profile with pkh that is not encoded as
     [Tezos_crypto.Signature.Public_key_hash.encoding] should fail. *)
  let* () = check_bad_attester_pkh_encoding (Attester "This is invalid PKH") in
  (* Test adding duplicate profiles stores profile only once *)
  let* () = patch_profile_rpc profile1 in
  let* () = patch_profile_rpc profile1 in
  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1])
  in
  (* Test adding multiple profiles *)
  let* () = patch_profile_rpc profile2 in
  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1; profile2])
  in
  (* Test that the patched profiles are persisted after restart using SIGTERM. *)
  let* () = Dal_node.terminate dal_node in
  Log.info "restart DAL node (1)" ;
  let* () = Dal_node.run dal_node ~wait_ready:true in

  let* () =
    check_profiles ~__LOC__ dal_node ~expected:(Controller [profile1; profile2])
  in
  (* Test whether the patched profiles persist after a restart using SIGSTOP
     (that is, even if we stop the DAL node abruptly). *)
  let profile3 = Dal_RPC.Attester Constant.bootstrap3.public_key_hash in
  let* () = patch_profile_rpc profile3 in
  let* () = Dal_node.stop dal_node in
  let* () = Dal_node.kill dal_node in
  Log.info "restart DAL node (2)" ;
  let* () = Dal_node.run dal_node ~wait_ready:true in
  check_profiles
    ~__LOC__
    dal_node
    ~expected:(Controller [profile1; profile2; profile3])

(* Check that result of the DAL node's
   GET /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices
   is consistent with the result of the L1 node GET dal/shards . *)
let test_dal_node_get_assigned_shard_indices _protocol _parameters _cryptobox
    node _client dal_node =
  let pkh = Constant.bootstrap1.public_key_hash in
  let* {level; _} =
    Node.RPC.(call node @@ get_chain_block_helper_current_level ())
  in
  let* committee_from_l1 = Dal.Committee.at_level node ~level () in
  let* shards_from_dal =
    Dal_RPC.(call dal_node @@ get_assigned_shard_indices ~level ~pkh)
  in
  match
    committee_from_l1
    |> List.find_opt (fun member ->
           String.equal member.Dal.Committee.attester pkh)
  with
  | None -> Test.fail ~__LOC__ "pkh %S not found in committee from L1." pkh
  | Some member ->
      let shards_from_l1 = member.indexes in
      Check.(
        (shards_from_dal = shards_from_l1)
          (list int)
          ~error_msg:
            "Shard indexes does not match between DAL and L1  (From DAL: %L <> \
             From L1: %R)") ;
      unit

let test_dal_node_get_attestable_slots _protocol parameters cryptobox node
    client dal_node =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let store_slot ~slot_index s =
    Helpers.(store_slot dal_node ~slot_index @@ make_slot ~slot_size s)
  in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  Log.info "Inject the shards of slots 1 and 3." ;
  let slot1_content = "slot 1" in
  let slot2_content = "slot 2" in
  let slot3_content = "slot 3" in
  let* _commitment, _proof = store_slot ~slot_index:0 slot1_content in
  let* _commitment, _proof = store_slot ~slot_index:2 slot3_content in
  Log.info "Publish slots 1 and 2 (inject and bake two blocks)." ;
  let* level = next_level node in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let* () = publish Constant.bootstrap1 ~index:0 slot1_content in
  let* () = publish Constant.bootstrap2 ~index:2 slot2_content in
  let wait_block_processing = wait_for_layer1_final_block dal_node level in
  (* bake three blocks: at [level + 1] the commitments are published, at [level +
     3] the commitments become final *)
  let* () = bake_for ~count:3 client in
  let* () = wait_block_processing in
  Log.info "Check attestability of slots." ;
  let attested_level = level + parameters.attestation_lag in
  let rec iter i =
    if i < 0 then unit
    else
      let attester = Account.Bootstrap.keys.(i) in
      (* Note: we assume that the key has at least an assigned shard index. *)
      let* res =
        Dal_RPC.(
          call dal_node @@ get_attestable_slots ~attester ~attested_level)
      in
      match res with
      | Not_in_committee ->
          Test.fail "attester %s not in committee" attester.alias
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
    Dal_RPC.(
      call dal_node
      @@ get_attestable_slots ~attester:new_account ~attested_level)
  in
  match res with
  | Not_in_committee -> return ()
  | Attestable_slots _ ->
      Test.fail "attester %s is in committee!" new_account.alias

(* This test checks that the attester correctly emits attestations, by
   publishing a slot per level for a few levels, then checking that the slots
   are attested or not, depending on whether or not all delegates attested the
   slots. We use [attestation_threshold = 100] to this end; with a smaller
   threshold it is harder to control whether the slot will be attested or not
   (one would have to take into account the distribution of shard indexes to
   delegates).

   There are two variants of the test: with and without the baker daemon. See
   below for the version not using the daemon (only using `bake for`).

   In this version, slots are still published by baking with `bake for`, because
   when running the baker daemon, it is harder to control the time of publishing
   of a slot. See the end-to-end tests for a version without `bake for`.
*)
let test_attester_with_daemon protocol parameters cryptobox node client dal_node
    =
  Check.((parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  let client = Client.with_dal_node client ~dal_node in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  let slot_size = parameters.cryptobox.slot_size in
  let slot_idx level = level mod number_of_slots in
  let num_bakers = Array.length Account.Bootstrap.keys in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.alias)
  in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let publish_and_store level =
    let source = Account.Bootstrap.keys.(level mod num_bakers) in
    let index = slot_idx level in
    let slot_content =
      Format.asprintf "content at level %d index %d" level index
    in
    let* () = publish source ~index slot_content in
    let* _commitment, _proof =
      Helpers.(
        store_slot dal_node ~slot_index:index
        @@ make_slot ~slot_size slot_content)
    in
    unit
  in
  let publish_and_bake ~init_level ~target_level =
    Log.info
      "Publish (inject and bake) a slot header at each level from %d to %d."
      init_level
      target_level ;
    let rec iter level =
      if level > target_level then return ()
      else
        let* () = publish_and_store level in
        (* bake (and attest) with all available delegates to go faster *)
        let* () = bake_for client in
        let* _ = Node.wait_for_level node level in
        iter (level + 1)
    in
    iter init_level
  in
  let run_baker delegates target_level =
    let* baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.init
        ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
        ~dal_node_rpc_endpoint
        ~delegates
        ~state_recorder:true
        node
        client
    in
    let* _ = Node.wait_for_level node target_level in
    Agnostic_baker.terminate baker
  in

  (* Test goal: the published slot at levels in [first_level, intermediary_level - 1]
     should be attested, the one at levels in at levels in [intermediary_level,
     max_level - 1] should not be attested. *)
  let* first_level = next_level node in
  let intermediary_level =
    (* We want at least two levels for which the published slot is attested. *)
    first_level + 2
  in
  let max_level =
    (* We want at least two levels for which the published slot is unattested;
       we add 2 for "safety"; see discussion (D) below. *)
    intermediary_level + 2 + 2
  in
  (* We want [max_level - attestation_lag < first_level], so that the
     delegates that attest at the last level baked by `bake for` (that is, at
     [max_level]) have no published slot to attest, in order not interfere
     with the attestations done by the baker daemon. *)
  Check.((parameters.attestation_lag > max_level - first_level) int)
    ~error_msg:
      "attestation_lag (%L) should be higher than [max_level - first_level] \
       (which is %R)" ;
  let wait_block_processing = wait_for_layer1_head dal_node max_level in
  let* () = publish_and_bake ~init_level:first_level ~target_level:max_level in
  let* () = wait_block_processing in
  let last_level_of_first_baker =
    intermediary_level + parameters.attestation_lag
  in
  let last_level_of_second_baker = max_level + parameters.attestation_lag + 1 in
  (* We need this level to be processed by the DAL node in order to make the
     necessary checks. *)
  let wait_for_dal_node =
    wait_for_layer1_final_block
      dal_node
      (max_level + parameters.attestation_lag - 1)
  in

  Log.info
    "Run the first baker for all delegates till at least level %d."
    last_level_of_first_baker ;
  let* () = run_baker all_delegates last_level_of_first_baker in
  (* (D) We tried to stop the baker as soon as it reaches
     [intermediary_level + attestation_lag], but it may have baked a few
     blocks more *)
  let* actual_intermediary_level = Node.get_level node in
  Log.info "The first baker baked till level %d." actual_intermediary_level ;

  let* dal_attestations =
    get_validated_dal_attestations_in_mempool node actual_intermediary_level
  in
  let num_dal_attestations = List.length dal_attestations in
  Log.info
    "The number of validated DAL attestations in the mempool is %d."
    num_dal_attestations ;

  (* Let L := actual_intermediary_level and PL := L - lag, the corresponding
     publish level.  The second baker may build a new block (1) at level L (and
     a higher round) or (2) directly at level L+1. However, independently of
     this, when it builds the block at level L+1, it should take into account
     the DAL attestations at level L that are present in the mempool. If there
     already 5 attestations then the slot at PL should be attested. If not, it
     cannot be attested because the second baker may only include, when in case
     (2), 4 new attestations. *)
  let first_not_attested_published_level =
    if num_dal_attestations = List.length all_delegates then
      actual_intermediary_level + 2 - parameters.attestation_lag
    else actual_intermediary_level + 1 - parameters.attestation_lag
  in
  Log.info
    "We set first_not_attested_published_level to %d."
    first_not_attested_published_level ;

  Log.info
    "Run the second baker for some (not all) delegates till at least level %d."
    last_level_of_second_baker ;
  let* () = run_baker (List.tl all_delegates) last_level_of_second_baker in
  let* () = wait_for_dal_node in

  Log.info "Check the attestation status of the published slots." ;
  let rec check_attestations level =
    if level >= max_level then return ()
    else
      let* status =
        Dal_RPC.(
          call dal_node
          @@ get_level_slot_status
               ~slot_level:level
               ~slot_index:(slot_idx level))
      in
      (* Before [first_not_attested_published_level], it should be [attested],
         and above (and including) [first_not_attested_published_level], it
         should be [unattested]. *)
      let expected_status =
        if level < first_not_attested_published_level then "attested"
        else "unattested"
      in
      Check.(
        (expected_status = status)
          string
          ~error_msg:"Expected status %L (got %R)") ;
      check_attestations (level + 1)
  in
  check_attestations first_level

(* This is the version of [test_attester_with_daemon] that does not use the
   baker daemon, only `bake for`. *)
let test_attester_with_bake_for _protocol parameters cryptobox node client
    dal_node =
  Check.((parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  let client = Client.with_dal_node client ~dal_node in
  let number_of_slots = parameters.Dal.Parameters.number_of_slots in
  let slot_size = parameters.cryptobox.slot_size in
  let lag = parameters.attestation_lag in
  let slot_idx level = level mod number_of_slots in
  let num_bakers = Array.length Account.Bootstrap.keys in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.alias)
  in
  let not_all_delegates = List.tl all_delegates in

  (* Test goal: the published slot at a level in [first_level,
     intermediary_level] should be attested, the one at a level in
     [intermediary_level + 1, last_checked_level] should not be attested. *)
  let attested_levels = 2 in
  let unattested_levels = 2 in
  let* first_level = next_level node in
  let intermediary_level = first_level + attested_levels - 1 in
  let last_checked_level = intermediary_level + unattested_levels - 1 in
  let last_level = last_checked_level + lag + 1 in

  Log.info "attestation_lag = %d" lag ;

  (* Publish and bake with client *)
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let publish_and_store level =
    let source = Account.Bootstrap.keys.(level mod num_bakers) in
    let index = slot_idx level in
    let slot_content =
      Format.asprintf "content at level %d index %d" level index
    in
    let* () = publish source ~index slot_content in
    let* _commitment, _proof =
      Helpers.(
        store_slot dal_node ~slot_index:index
        @@ make_slot ~slot_size slot_content)
    in
    Log.info "Slot with %d index (normally) published at level %d" index level ;
    unit
  in
  let publish_and_bake ~from_level ~to_level delegates =
    Log.info
      "Publish (inject and bake) a slot header at each level from %d to %d."
      from_level
      to_level ;
    let rec iter level =
      if level > to_level then return ()
      else
        let* () = publish_and_store level in
        let* () = bake_for ~delegates:(`For delegates) client in
        let* _ = Node.wait_for_level node level in
        iter (level + 1)
    in
    iter from_level
  in

  let wait_level = last_level + 1 in
  let wait_block_processing_on_l1 = wait_for_layer1_head dal_node wait_level in

  let wait_block_processing_on_dal =
    wait_for_layer1_final_block dal_node (wait_level - 2)
  in

  let* () =
    publish_and_bake
      ~from_level:first_level
      ~to_level:(intermediary_level + lag)
      all_delegates
  in
  let* () =
    publish_and_bake
      ~from_level:(intermediary_level + lag + 1)
      ~to_level:last_level
      not_all_delegates
  in
  let* () = bake_for client in
  let* _lvl = wait_block_processing_on_l1 in
  let* () = wait_block_processing_on_dal in

  Log.info "Check the attestation status of the published slots." ;
  let rec check_attestations level =
    if level > last_checked_level then return ()
    else
      let* status =
        Dal_RPC.(
          call dal_node
          @@ get_level_slot_status
               ~slot_level:level
               ~slot_index:(slot_idx level))
      in
      let expected_status =
        if level <= intermediary_level then "attested" else "unattested"
      in
      Check.(
        (expected_status = status)
          string
          ~error_msg:"Expected status %L (got %R)") ;
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
   ahead. In other terms, we have:

   [published level = injection level + beforehand_slot_injection]

   The publication consists in sending the slot to a DAL node, computing its
   shards on the DAL node and associating it to a slot index and published
   level. On the L1 side, a manager operation publish_commitment is also sent.

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
      Helpers.publish_and_store_slot
        ~force:true
        ~counter:!counter
        l1_client
        dal_node
        source
        ~index:slot_index
      @@ Helpers.make_slot ~slot_size (sf " %d " payload)
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
let create_additional_nodes ~extra_node_operators rollup_address l1_node
    l1_client dal_node =
  (* The mutable variable [connect_dal_node_to] below is used to diversify a bit
     the topology of the DAL nodes network as follows:

     [node with odd idx] -- [node with even idx] -- init_node

     So, we initialize its value to [dal_node]. *)
  let dal_node_producer = Dal_node.create ~node:l1_node () in
  let* () = Dal_node.init_config ~operator_profiles:[0] dal_node_producer in
  let* () = Dal_node.run dal_node_producer in
  let connect_dal_node_to = ref dal_node in
  let* nodes =
    Lwt_list.mapi_s
      (fun index key_opt ->
        (* We create a new DAL node and initialize it. *)
        let fresh_dal_node = Dal_node.create ~node:l1_node () in
        let* () = Dal_node.init_config fresh_dal_node in

        (* We connect the fresh DAL node to another node, start it and update the
           value of [connect_dal_node_to] to generate the topology above: *)
        update_neighbors fresh_dal_node [!connect_dal_node_to] ;
        update_neighbors fresh_dal_node [dal_node_producer] ;
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
            ~dal_node:fresh_dal_node
            rollup_mode
            l1_node
            ~base_dir:(Client.base_dir l1_client)
            ?default_operator:key_opt
        in
        (* We start the rollup node and create a client for it. *)
        let* () = Sc_rollup_node.run sc_rollup_node rollup_address [] in
        return (fresh_dal_node, sc_rollup_node))
      extra_node_operators
  in
  return (dal_node_producer, nodes)

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
    ?(extra_node_operators = []) ~slot_index _protocol parameters dal_node
    sc_rollup_node sc_rollup_address l1_node l1_client _pvm_name
    ~number_of_dal_slots =
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
  let* current_level = Node.get_level l1_node in
  Log.info "[e2e.startup] current level is %d@." current_level ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let bootstrap_dal_node =
    Dal_node.create ~node:l1_node ~disable_amplification:true ()
  in
  let operator_profiles = [slot_index] in
  let* () =
    Dal_node.init_config
      ~peers:[Dal_node.listen_addr dal_node]
      ~bootstrap_profile:true
      bootstrap_dal_node
  in
  let* () = Dal_node.run ~event_level:`Debug bootstrap_dal_node in
  let bootstrap_dal_node_p2p_endpoint =
    Dal_node.listen_addr bootstrap_dal_node
  in
  (* Generate new DAL and rollup nodes if requested. *)
  let create_additional_nodes ~extra_node_operators sc_rollup_address l1_node
      l1_client =
    extra_node_operators
    |> Lwt_list.map_p (fun key_opt ->
           let fresh_dal_node =
             Dal_node.create ~node:l1_node ~disable_amplification:true ()
           in
           let* () =
             match key_opt with
             | Some _ ->
                 Dal_node.init_config
                   ~peers:[bootstrap_dal_node_p2p_endpoint]
                   ~operator_profiles
                   fresh_dal_node
             | None ->
                 Dal_node.init_config
                   ~peers:[bootstrap_dal_node_p2p_endpoint]
                   ~observer_profiles:operator_profiles
                   fresh_dal_node
           in
           let* () = Dal_node.run ~event_level:`Debug fresh_dal_node in
           let rollup_mode =
             Sc_rollup_node.(
               if Option.is_none key_opt then Observer else Operator)
           in
           let sc_rollup_node =
             Sc_rollup_node.create
               ~dal_node:fresh_dal_node
               rollup_mode
               l1_node
               ~base_dir:(Client.base_dir l1_client)
               ?default_operator:key_opt
           in
           (* We start the rollup node and create a client for it. *)
           let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
           return (fresh_dal_node, sc_rollup_node))
  in
  let* additional_nodes =
    create_additional_nodes
      ~extra_node_operators
      sc_rollup_address
      l1_node
      l1_client
  in
  Log.info "Start running baker node" ;
  let baker_dal_node = Dal_node.create ~node:l1_node () in
  let* () =
    Dal_node.init_config ~peers:[bootstrap_dal_node_p2p_endpoint] baker_dal_node
  in
  let* () = Dal_node.run ~event_level:`Debug baker_dal_node in
  let producer_dal_node = Dal_node.create ~node:l1_node () in
  let* () =
    Dal_node.init_config
      ~peers:[bootstrap_dal_node_p2p_endpoint]
      ~operator_profiles
      producer_dal_node
  in
  let* () = Dal_node.run ~event_level:`Debug producer_dal_node in
  Log.info
    "[e2e.configure_pvm] configure PVM with DAL parameters via inbox message@." ;
  let Dal.Parameters.{attestation_lag; cryptobox; _} = parameters in
  (* We ask the Arith PVM of [_sc_rollup_address] to track the slot index 5 of
     the DAL. *)
  let* () =
    let number_of_slots = parameters.number_of_slots in
    let pages_per_slot = cryptobox.slot_size / cryptobox.page_size in
    let config =
      sf
        " dal:%d:%d:%d:%d "
        number_of_slots
        attestation_lag
        pages_per_slot
        slot_index
    in
    send_messages ~bake:false l1_client [config]
  in
  (* We need to bake some blocks so that the DAL node starts
     processing blocks and register the committee. *)
  let* () = bake_for l1_client ~count:2 in
  Log.info "[e2e.pvm] PVM Arith configured@." ;

  Log.info
    "[e2e.start_baker] spawn a baker daemon with all bootstrap accounts@." ;
  let* _baker =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint baker_dal_node in
    Agnostic_baker.init ~dal_node_rpc_endpoint l1_node l1_client
  in

  (* To be sure that we just moved to [start_dal_slots_level], we wait and extra
     level. *)
  let* start_dal_slots_level =
    let* next_level = next_level l1_node in
    Node.wait_for_level l1_node next_level
  in
  let end_dal_slots_level = start_dal_slots_level + number_of_dal_slots - 1 in

  (* start_dal = 4, end_dal = 5 *)
  Log.info
    "[e2e.start_slot_producer] from level %d to level %d@."
    start_dal_slots_level
    end_dal_slots_level ;
  (* The slot producer will publish [number_of_dal_slots] sucessive slots of
     size [slot_size] with index [tracked_slots]. *)
  let* expected_value =
    slot_producer
      ~beforehand_slot_injection
      ~slot_index
      ~from:start_dal_slots_level
      ~into:end_dal_slots_level
      ~slot_size
      producer_dal_node
      l1_node
      l1_client
  in
  (* Wait until the last published slot is included and attested in a final block. *)
  let* _lvl =
    Node.wait_for_level
      l1_node
      (end_dal_slots_level + attestation_lag + beforehand_slot_injection + 1)
  in

  Log.info
    "[e2e.sum_and_store] send an inbox messsage to the PVM to sum the received \
     payloads, and store the result in a 'value' variable@." ;
  let* level =
    let* level = Node.get_level l1_node in
    (* We send instructions "+...+ value" to the PVM of [number_of_dal_slots -
       1] additions, so that it sums the received slots' contents and store the
       result in a variable called "value". *)
    let do_sum = String.make (number_of_dal_slots - 1) '+' in
    let* () = send_messages ~bake:false l1_client [do_sum ^ " value"] in
    (* Wait sufficiently many levels so that the PVM interprets the message. *)
    let* level = Node.wait_for_level l1_node (level + 2) in
    return level
  in
  let* _ = Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node level in
  Log.info
    "[e2e.final_check_1] check that the sum stored in the PVM is %d@."
    expected_value ;
  let* () =
    check_saved_value_in_pvm
      ~rpc_hooks
      ~name:"value"
      ~expected_value
      sc_rollup_node
  in

  (* Check the statuses of the additional nodes/PVMs, if any. *)
  Log.info
    "[e2e.final_check_2] Check %d extra nodes@."
    (List.length additional_nodes) ;
  Lwt_list.iter_s
    (fun (_dal_node, rollup_node) ->
      check_saved_value_in_pvm
        ~rpc_hooks
        ~name:"value"
        ~expected_value
        rollup_node)
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
  tags : string list;
}

let e2e_tests =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4858

     Move tests that take time to long tests, in particular those with mainnet
     parameters. *)
  let test1 =
    {
      constants = Protocol.Constants_test;
      attestation_lag = 10;
      block_delay = 8;
      number_of_dal_slots = 2;
      beforehand_slot_injection = 1;
      num_extra_nodes = 3;
      tags = [Tag.slow];
    }
  in
  [test1]

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
        tags;
      } =
        test
      in
      let network = Protocol.constants_to_string constants in
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
        Ptime.Span.of_int_s expected_bake_for_occurrences
      in
      (* Preparing the list of node operators for extra nodes. *)
      let extra_node_operators =
        (* So launch the extra SORU nodes in observer mode. So we don't actually
           need to provide public key hashes. *)
        List.init num_extra_nodes (fun _index -> None)
      in
      let slot_index = 5 in
      let operator_profiles = [slot_index] in
      let tags = ["e2e"; network; Tag.extra; Tag.slow] @ tags in
      scenario_with_all_nodes
        ~operator_profiles
        ~custom_constants:constants
        ~slot_size:(1 lsl 17)
        ~page_size:4096
        ~redundancy_factor:8
        ~number_of_shards:512
        ~consensus_committee_size:512
        ~attestation_lag
        ~activation_timestamp:(Ago activation_timestamp)
        ~minimal_block_delay:(string_of_int block_delay)
        ~tags
        ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
        title
        (e2e_test_script
           ~slot_index
           ~number_of_dal_slots
           ~beforehand_slot_injection
           ~extra_node_operators)
        protocols)
    e2e_tests

let check_expected expected found = if expected <> found then None else Some ()

let ( let*?? ) a b = Option.bind a b

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
    slot_index = num_slots - 1}. When the [already_seen_slots] array paramenter
    is used however, the events corresponding to slot indices marked with [true]
    in this array are not monitored.

    Depending on the value of [event_with_topic], some extra checks, such as the
    peer id in case of Graft and Subscribe, are also done.
*)
let check_events_with_topic ~event_with_topic dal_node ~num_slots
    ?(already_seen_slots = Array.make num_slots false) expected_pkh =
  assert (Array.length already_seen_slots = num_slots) ;
  let remaining =
    ref
      (Array.fold_left
         (fun remaining b -> if b then remaining else remaining + 1)
         0
         already_seen_slots)
  in
  let seen = Array.copy already_seen_slots in
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
  Dal_common.Helpers.wait_for_gossipsub_worker_event
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
let check_events_with_message ~event_with_message dal_node ~number_of_shards
    ~shard_indexes ~expected_commitment ~expected_level ~expected_pkh
    ~expected_slot =
  let remaining = ref (List.length shard_indexes) in
  let seen = Array.make number_of_shards false in
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
  let all_seen () =
    seen |> Array.to_seqi
    |> Seq.for_all (fun (i, b) -> if List.mem i shard_indexes then b else true)
  in
  Dal_common.Helpers.wait_for_gossipsub_worker_event
    dal_node
    ~name:(event_with_message_to_string event_with_message)
    (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      Check.(
        (seen.(shard_index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(shard_index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && all_seen () then Some () else None)

type event_with_message_id = IHave of {pkh : string; slot_index : int} | IWant

let event_with_message_id_to_string = function
  | IHave _ -> "ihave"
  | IWant -> "iwant"

(** This function monitors the Gossipsub worker events whose name is given by
    [event_with_message_id]. It's somehow similar to function
    {!check_events_with_message}, but for IHave and IWant messages' events. *)
let check_events_with_message_id ~event_with_message_id dal_node
    ~number_of_shards ~shard_indexes ~expected_commitment ~expected_level
    ~expected_pkh ~expected_slot ~expected_peer =
  let remaining = ref (List.length shard_indexes) in
  let seen = Array.make number_of_shards false in
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
  let all_seen () =
    seen |> Array.to_seqi
    |> Seq.for_all (fun (i, b) -> if List.mem i shard_indexes then b else true)
  in
  Dal_common.Helpers.wait_for_gossipsub_worker_event
    ~name:(event_with_message_id_to_string event_with_message_id)
    dal_node
    (fun event ->
      let*?? shard_indices = get_shard_indices_of_messages event in
      List.iter
        (fun shard_index_opt ->
          match shard_index_opt with
          | None -> ()
          | Some shard_index ->
              Check.(
                (seen.(shard_index) = false)
                  bool
                  ~error_msg:
                    (sf
                       "Shard_index %d already seen. Invariant broken"
                       shard_index)) ;
              seen.(shard_index) <- true ;
              decr remaining)
        shard_indices ;
      if !remaining = 0 && all_seen () then Some () else None)

(** This function is quite similar to those above, except that it checks that a
    range of messages (shards) on a tracked topic have been notified by GS to
    the DAL node. This is typically needed to then be able to attest slots. *)
let check_message_notified_to_app_event dal_node ~number_of_shards
    ~shard_indexes ~expected_commitment ~expected_level ~expected_pkh
    ~expected_slot =
  let remaining = ref (List.length shard_indexes) in
  let seen = Array.make number_of_shards false in
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
  let all_seen () =
    seen |> Array.to_seqi
    |> Seq.for_all (fun (i, b) -> if List.mem i shard_indexes then b else true)
  in
  Dal_node.wait_for dal_node "dal_gs_message_notified_to_app.v0" (fun event ->
      let*?? shard_index = get_shard_index_opt event in
      Check.(
        (seen.(shard_index) = false)
          bool
          ~error_msg:
            (sf "Shard_index %d already seen. Invariant broken" shard_index)) ;
      seen.(shard_index) <- true ;
      let () = remaining := !remaining - 1 in
      if !remaining = 0 && all_seen () then Some () else None)

(** This helper function makes the nodes [dal_node1] and [dal_node2] join the
    topics of the attester [pkh], by calling the RPC for tracking the corresponding profile.
    The second node calls the RPC only after receiving the Subscribe messages
    from the first node, so that when it joins the topics, it also sends Graft messages
    in addition to sending Subscribe messages. *)
let nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 =
  let profile1 = Dal_RPC.Attester pkh1 in
  let* peer_id1 = Dal_node.read_identity dal_node1 in
  let* peer_id2 = Dal_node.read_identity dal_node2 in
  (* node1 joins topic {pkh} -> it sends subscribe messages to node2. *)
  let event_waiter =
    check_events_with_topic
      ~event_with_topic:(Subscribe peer_id1)
      dal_node2
      ~num_slots
      pkh1
  in
  let* () = Dal_RPC.(call dal_node1 @@ patch_profiles [profile1]) in
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
  let* () = Dal_RPC.(call dal_node2 @@ patch_profiles [profile1]) in
  Lwt.join [event_waiter_subscribe; event_waiter_graft]

(** This helper returns the list of promises that allow to wait for the
    publication of a slot's shards into the Gossipsub layer.

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level].
*)
let waiters_publish_shards l1_committee dal_node commitment ~publish_level
    ~slot_index ~number_of_shards =
  let open Dal.Committee in
  List.map
    (fun {attester; indexes} ->
      check_events_with_message
        ~event_with_message:Publish_message
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index)
    l1_committee

(** This helper returns the promise that allows to wait for the reception of
    messages of [slot_index] published at level [publish_level] by the attester
    [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_receive_shards l1_committee dal_node commitment ~publish_level
    ~slot_index ~pkh ~from_peer ~number_of_shards =
  let open Dal.Committee in
  match List.find (fun {attester; _} -> attester = pkh) l1_committee with
  | {attester; indexes} ->
      check_events_with_message
        ~event_with_message:(Message_with_header from_peer)
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

(** This helper returns the promise that allows to wait for the successful
    notification of messages of [slot_index] published at level [publish_level]
    to the app layer of the attester [pkh].

    The [l1_committee] used to determine the topic of published messages is the
    one at the attesattion level corresponding to [publish_level]. *)
let waiter_successful_shards_app_notification l1_committee dal_node commitment
    ~publish_level ~slot_index ~pkh ~number_of_shards =
  let open Dal.Committee in
  match List.find (fun {attester; _} -> attester = pkh) l1_committee with
  | {attester; indexes} ->
      check_message_notified_to_app_event
        dal_node
        ~number_of_shards
        ~shard_indexes:indexes
        ~expected_commitment:commitment
        ~expected_level:publish_level
        ~expected_pkh:attester
        ~expected_slot:slot_index
  | exception Not_found ->
      Test.fail "Should not happen as %s is part of the committee" pkh

(* Create two promises for Grafts between [node1] and [node2] on topic
   [(slot_index, pkh)], one in each direction. *)
let check_grafts ~number_of_slots ~slot_index (node1, node1_peer_id)
    (node2, node2_peer_id) pkh =
  (* The connections have no reason to be grafted on other slot indices than
     the one they are both subscribed to, so we instruct
     [check_events_with_topic] to skip all events but the one for [index]. *)
  let already_seen_slots =
    Array.init number_of_slots (fun index -> slot_index <> index)
  in
  let check_graft on_node to_peer_id pkh =
    check_events_with_topic
      ~event_with_topic:(Graft to_peer_id)
      on_node
      ~num_slots:number_of_slots
      ~already_seen_slots
      pkh
  in
  let graft_from_node1 = check_graft node1 node2_peer_id pkh in
  let graft_from_node2 = check_graft node2 node1_peer_id pkh in
  [graft_from_node1; graft_from_node2]

let test_dal_node_p2p_connection_and_disconnection _protocol _parameters
    _cryptobox node _client dal_node1 =
  let dal_node2 = Dal_node.create ~node () in
  (* Connect the nodes *)
  let* () =
    Dal_common.Helpers.connect_nodes_via_p2p
      ~init_config:true
      dal_node1
      dal_node2
  in
  let* peer_id = Dal_node.read_identity dal_node2 in
  (* kill dal_node2 and check "disconnection" event in node1. *)
  let disconn_ev_in_node1 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id
  in
  let* () = Dal_node.kill dal_node2 in
  disconn_ev_in_node1

let test_dal_node_join_topic _protocol parameters _cryptobox _node _client
    dal_node1 =
  let pkh1 = Constant.bootstrap1.public_key_hash in
  let profile1 = Dal_RPC.Attester pkh1 in
  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let event_waiter =
    check_events_with_topic ~event_with_topic:Join dal_node1 ~num_slots pkh1
  in
  let* () = Dal_RPC.(call dal_node1 @@ patch_profiles [profile1]) in
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
   attester or not. In particular, it should be set to [false] if the
   application layer of the second DAL node was not notified about the messages
   sent by the first DAL node.
*)
let generic_gs_messages_exchange protocol parameters _cryptobox node client
    dal_node1 ~mk_dal_node2 ~expect_app_notification ~is_first_slot_attestable =
  let* node2, dal_node2 = mk_dal_node2 protocol parameters in

  let* () =
    Dal_common.Helpers.connect_nodes_via_p2p
      ~init_config:true
      dal_node1
      dal_node2
  in

  let num_slots = parameters.Dal.Parameters.number_of_slots in
  let number_of_shards = parameters.Dal.Parameters.cryptobox.number_of_shards in
  let account1 = Constant.bootstrap1 in
  let pkh1 = account1.public_key_hash in
  (* The two nodes join the same topics *)
  let* () = nodes_join_the_same_topics dal_node1 dal_node2 ~num_slots ~pkh1 in

  (* Posting a DAL message to DAL node and to L1 *)
  let crypto_params = parameters.cryptobox in
  let slot_index = 0 in
  let* slot_commitment =
    let slot_size = crypto_params.slot_size in
    let slot_content = generate_dummy_slot slot_size in
    Helpers.publish_and_store_slot
      client
      dal_node1
      Constant.bootstrap1
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size slot_content
  in

  (* Preparing event waiters for different shards that will be published by
     dal_node1 once the slot header is included in an L1 block.

     We also prepare a waiter event for:
     - the shards that will be received by dal_node2 on its topics;
     - the messages (shards) that will be notified to dal_node2 on its topic. *)
  let* publish_level = next_level node in
  let attested_level = publish_level + parameters.attestation_lag in
  let attestation_level = attested_level - 1 in
  let* committee = Dal.Committee.at_level node ~level:attestation_level () in

  let waiter_publish_list =
    waiters_publish_shards
      committee
      dal_node1
      slot_commitment
      ~publish_level
      ~slot_index
      ~number_of_shards
  in
  let waiter_receive_shards =
    let* from_peer = Dal_node.read_identity dal_node1 in
    waiter_receive_shards
      committee
      dal_node2
      slot_commitment
      ~publish_level
      ~slot_index
      ~pkh:pkh1
      ~from_peer
      ~number_of_shards
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
        ~number_of_shards
    else unit
  in

  (* We bake a block that includes a [slot_header] operation. And then another
     two blocks so that this operation is final. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join waiter_publish_list
  and* () = waiter_receive_shards
  and* () = waiter_app_notifs in

  let* () =
    match node2 with
    | None -> unit
    | Some node2 ->
        (* We need that this node reaches level 2; otherwise, the RPC call to
           [get_attestable_slots] may fail. *)
        let* _level = Node.wait_for_level node2 2 in
        unit
  in

  (* Check that dal_node2 has the shards needed by attester account1/pkh1 to
     attest the slot with index 0. *)
  let* res =
    Dal_RPC.(
      call dal_node2 @@ get_attestable_slots ~attester:account1 ~attested_level)
  in
  match res with
  | Not_in_committee -> Test.fail "attester %s not in committee" account1.alias
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
      let dal_node2 = Dal_node.create ~node () in
      (None, dal_node2) |> return)
    ~expect_app_notification:true
    ~is_first_slot_attestable:true

(* Create a DAL node whose DAL parameters are not compatible with those in
   [parameters]. For that, the redundancy_factor field is multiplied by 2. *)
let make_invalid_dal_node node1 protocol parameters =
  (* Create another L1 node with different DAL parameters. *)
  let* node2, _client2, _xdal_parameters2 =
    let crypto_params = parameters.Dal.Parameters.cryptobox in
    let parameter_overrides =
      dal_enable_param (Some true)
      @ redundancy_factor_param (Some (crypto_params.redundancy_factor / 2))
      @ slot_size_param (Some (crypto_params.slot_size / 2))
    in
    setup_node ~protocol ~parameter_overrides ()
  in
  Node.add_peer node2 node1 ;
  let* () = Node.terminate node2 in
  let* () = Node.run node2 [] in
  (* Create a second DAL node with node2 and client2 as argument (so different
     DAL parameters compared to dal_node1. *)
  let dal_node2 = Dal_node.create ~node:node2 () in
  return (Some node2, dal_node2)

let test_dal_node_gs_invalid_messages_exchange _protocol parameters _cryptobox
    node client dal_node1 =
  (* Messages are invalid, so the app layer is not notified. *)
  let expect_app_notification = false in
  (* The first slot published by [generic_gs_messages_exchange] is not
     attestable by the considered attester pk1 = bootstrap1, because the shards
     received by the Gossipsub layer are classified as 'Invalid'. *)
  let is_first_slot_attestable = false in
  generic_gs_messages_exchange
    _protocol
    parameters
    _cryptobox
    node
    client
    dal_node1
    ~mk_dal_node2:(make_invalid_dal_node node)
    ~expect_app_notification
    ~is_first_slot_attestable

(* Checks that:
   * the baker does not crash when there's a DAL node specified, but it is not
   running
   * the baker register profiles when the DAL node restarts. *)
let test_baker_registers_profiles _protocol _parameters _cryptobox l1_node
    client dal_node =
  let delegates =
    List.to_seq Constant.all_secret_keys |> Seq.take 3 |> List.of_seq
  in
  let profiles =
    List.map (fun key -> Dal_RPC.Attester key.Account.public_key_hash) delegates
  in
  let delegates = List.map (fun key -> key.Account.alias) delegates in

  Log.info
    "Terminate the DAL node and then start the baker; the baker cannot attest \
     but can advance" ;
  let* () = Dal_node.terminate dal_node in
  let baker =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client ~delegates
  in
  let wait_for_attestation_event =
    Agnostic_baker.wait_for baker "failed_to_get_attestations.v0" (fun _json ->
        Some ())
  in
  let* () = Agnostic_baker.run baker in
  let* () = wait_for_attestation_event in
  let* _lvl = Node.wait_for_level l1_node 3 in

  Log.info "Start (again) the DAL node" ;
  let* () = Dal_node.run dal_node in
  Log.info "Wait 2 seconds; by then, profiles should have been registered" ;
  (* Note: this constant depends on how often the baker retries to register
     profiles (see [max_delay] in [Baking_scheduling.register_dal_profiles]); if
     the baker behavior changes in this respect, the constant may need
     adjusting. *)
  let* () = Lwt_unix.sleep 2.0 in
  check_profiles ~__LOC__ dal_node ~expected:(Controller profiles)

(** This helper funciton terminates dal_node2 and dal_node3 (in addition to
    those in [extra_nodes_to_restart]), and restart them after creating two
    connection events to check that dal_node2 and dal_node3 find each other. *)
let observe_nodes_connection_via_bootstrap ?(extra_nodes_to_restart = []) client
    dal_node2 dal_node3 =
  let nodes = dal_node2 :: dal_node3 :: extra_nodes_to_restart in
  let* () = List.map Dal_node.terminate nodes |> Lwt.join in
  let* dal_node2_identity = Dal_node.read_identity dal_node2 in
  let* dal_node3_identity = Dal_node.read_identity dal_node3 in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:dal_node3_identity
      ()
  in
  let check_conn_event_from_3_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node3
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:dal_node2_identity
      ()
  in
  let* () = List.map (Dal_node.run ~wait_ready:true) nodes |> Lwt.join in
  Log.info "Bake a block and then another two to finalize it." ;
  let* () = bake_for ~count:3 client in
  Log.info "Wait for dal_node2 and dal_node3 to find each other." ;

  let* () =
    Lwt.join [check_conn_event_from_2_to_3; check_conn_event_from_3_to_2]
  in
  unit

(** This function tests that a peer can discover another peer via a bootstrap
    node and that discovery works even when the bootstrap is (re-)started at the
    same time (or after) the two other nodes we want to connect. *)
let test_peer_discovery_via_bootstrap_node _protocol _parameters _cryptobox node
    client dal_node1 =
  (* Phase 1: dal_node1 is already running. Start dal_node2 and dal_node3 and
     use dal_node1 to establish connections between them. *)
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  (* Here, we observe a first nodes connection via bootstrap nodes thanks to
     peers exchange. *)
  let* () = observe_nodes_connection_via_bootstrap client dal_node2 dal_node3 in

  (* In this variant, we also restart the bootstrap node [dal_node1]. So,
     connections to it from dal_node2 and dal_node3 are always done at startup,
     but Gossipsub worker might be needed to retry connection. *)
  observe_nodes_connection_via_bootstrap
    ~extra_nodes_to_restart:[dal_node1]
    client
    dal_node2
    dal_node3

(** Connect two nodes [dal_node2] and [dal_node3] via a trusted bootstrap peer
    [dal_node1]. Then, disconnect all the nodes (without restarting them) and
    wait for reconnection. *)
let test_peers_reconnection _protocol _parameters _cryptobox node client
    dal_node1 =
  (* Connect two nodes via bootstrap peer. *)
  Log.info "Connect two nodes via bootstrap peer." ;
  let* dal_node2 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~attester_profiles:[Constant.bootstrap1.public_key_hash]
      node
  in
  let* dal_node3 =
    make_dal_node
      ~peers:[Dal_node.listen_addr dal_node1]
      ~operator_profiles:[0]
      node
  in
  let* () =
    (* Here, we observe a first nodes connection via bootstrap nodes thanks to
       peers exchange. Below, we disconnect the nodes without restarting
       them. *)
    observe_nodes_connection_via_bootstrap client dal_node2 dal_node3
  in

  (* Get the nodes' identities. *)
  let id dal_node = Dal_node.read_identity dal_node in
  let* id_dal_node1 = id dal_node1 in
  let* id_dal_node2 = id dal_node2 in
  let* id_dal_node3 = id dal_node3 in

  (* Prepare disconnection events to observe. *)
  let disconn_ev_in_node1_2 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node2
  in
  let disconn_ev_in_node1_3 =
    Dal_common.Helpers.check_disconnection_event dal_node1 ~peer_id:id_dal_node3
  in
  let disconn_ev_in_node2_3 =
    Dal_common.Helpers.check_disconnection_event dal_node2 ~peer_id:id_dal_node3
  in

  (* Prepare reconnection events checks between node1 and node2 (resp. 3). *)
  let check_conn_event_from_1_to_2 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node2
      ~is_trusted:false
      ~other_peer_id:id_dal_node2
      ()
  in
  let check_conn_event_from_1_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node1
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in
  let check_conn_event_from_2_to_3 =
    Dal_common.Helpers.check_new_connection_event
      ~main_node:dal_node2
      ~other_node:dal_node3
      ~is_trusted:false
      ~other_peer_id:id_dal_node3
      ()
  in

  (* Disconnect all the nodes. *)
  let* () =
    Lwt_list.iter_p
      (fun peer_id ->
        Lwt_list.iter_p
          (fun dal_node ->
            Dal_RPC.(call dal_node @@ delete_p2p_peer_disconnect ~peer_id))
          [dal_node1; dal_node2; dal_node3])
      [id_dal_node1; id_dal_node2; id_dal_node3]
  in

  (* Observe disconnection. *)
  Log.info "Wait for disconnection" ;
  let* () =
    Lwt.join
      [disconn_ev_in_node1_2; disconn_ev_in_node1_3; disconn_ev_in_node2_3]
  in
  Log.info "Disconnection done. Wait for reconnection." ;
  let* () =
    Lwt.join
      [
        check_conn_event_from_1_to_2;
        check_conn_event_from_1_to_3;
        check_conn_event_from_2_to_3;
      ]
  in
  Log.info "Recconnection done." ;
  unit

(* Adapted from sc_rollup.ml *)
let test_l1_migration_scenario ?(tags = []) ?(uses = []) ~migrate_from
    ~migrate_to ~migration_level ~scenario ~description ?operator_profiles
    ?custom_constants ?attestation_lag ?attestation_threshold ?number_of_slots
    ?number_of_shards ?slot_size ?page_size ?redundancy_factor ?traps_fraction
    ?consensus_committee_size ?blocks_per_cycle ?minimal_block_delay
    ?activation_timestamp () =
  let tags =
    Tag.tezos2 :: "dal" :: Protocol.tag migrate_from :: Protocol.tag migrate_to
    :: "migration" :: tags
  in
  Test.register
    ~__FILE__
    ~tags
    ~uses:(Constant.octez_dal_node :: uses)
    ~title:
      (sf
         "%s->%s: %s"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         description)
  @@ fun () ->
  let parameter_overrides =
    make_int_parameter ["consensus_committee_size"] consensus_committee_size
    @ make_int_parameter ["blocks_per_cycle"] blocks_per_cycle
    @ make_string_parameter ["minimal_block_delay"] minimal_block_delay
    @ make_int_parameter ["dal_parametric"; "attestation_lag"] attestation_lag
    @ make_int_parameter
        ["dal_parametric"; "attestation_threshold"]
        attestation_threshold
    @ make_int_parameter ["dal_parametric"; "number_of_slots"] number_of_slots
    @ make_int_parameter ["dal_parametric"; "number_of_shards"] number_of_shards
    @ make_int_parameter
        ["dal_parametric"; "redundancy_factor"]
        redundancy_factor
    @ make_int_parameter ["dal_parametric"; "slot_size"] slot_size
    @ make_int_parameter ["dal_parametric"; "page_size"] page_size
    @ make_q_parameter ["dal_parametric"; "traps_fraction"] traps_fraction
  in
  let* node, client, dal_parameters =
    setup_node
      ~custom_constants
      ~parameter_overrides
      ~protocol:migrate_from
      ~l1_history_mode:Default_with_refutation
      ?activation_timestamp
      ()
  in

  Log.info "Set user-activated-upgrade at level %d" migration_level ;
  let* () = Node.terminate node in
  let patch_config =
    Node.Config_file.update_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let nodes_args = Node.[Synchronisation_threshold 0; No_bootstrap_peers] in
  let* () = Node.run ~patch_config node nodes_args in
  let* () = Node.wait_for_ready node in

  let dal_node = Dal_node.create ~node () in
  let* () = Dal_node.init_config ?operator_profiles dal_node in
  let* () = Dal_node.run dal_node ~wait_ready:true in

  scenario ~migration_level dal_parameters client node dal_node

let test_migration_plugin ~migrate_from ~migrate_to =
  let tags = ["plugin"]
  and description = "test plugin update"
  and scenario ~migration_level _dal_parameters client node dal_node =
    let* current_level =
      let* json = Node.RPC.(call node @@ get_chain_block_header_shell ()) in
      JSON.(json |-> "level" |> as_int |> return)
    in

    if migration_level <= current_level then
      Test.fail ~__LOC__ "The migration level (%d) is too low" migration_level ;

    let blocks_till_migration = migration_level - current_level in

    let wait_for_plugin =
      Dal_node.wait_for dal_node "dal_plugin_resolved.v0" (fun json ->
          let proto_hash = JSON.(json |-> "proto_hash" |> as_string) in
          if String.equal proto_hash (Protocol.hash migrate_to) then Some ()
          else None)
    in

    Log.info "Bake %d blocks" blocks_till_migration ;
    let* () = repeat blocks_till_migration (fun () -> bake_for client) in

    Log.info "Migrated to %s" (Protocol.name migrate_to) ;
    (* The plugin will change after the two blocks, as the migration block
       is not yet finalized. *)
    let* () = bake_for ~count:2 client in
    wait_for_plugin
  in
  test_l1_migration_scenario
    ~migrate_from
    ~migrate_to
    ~scenario
    ~tags
    ~description
    ()

(* This test checks the following scenario (that occurred on `nextnet--20250203`):
   - start a DAL node attester in protocol [migrate_from]
   - then start a DAL node accuser in protocol [migrate_to]
   - check that no baker is denounced

   This issue was that bakers were denounced, because the DAL node attester was
   started when the DAL incentives were not enabled, so they did not check for
   traps, and the DAL node did not used the new protocol parameters after
   migrating to the new protocol, so they also did not check for traps when the
   DAL incentives were enabled.
*)
let test_migration_accuser_issue ~migrate_from ~migrate_to =
  let slot_index = 0 in
  let tags = ["accuser"; "migration"] in
  let description = "test accuser" in
  let scenario ~migration_level dal_parameters client node dal_node =
    let* proto_params =
      Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
    in
    let blocks_per_cycle =
      JSON.(proto_params |-> "blocks_per_cycle" |> as_int)
    in
    assert (migration_level < blocks_per_cycle) ;
    let* level = Client.level client in
    let first_level = level + 1 in
    let last_level = blocks_per_cycle in
    Log.info
      "Publish slots from first_level = %d to last_level = %d"
      first_level
      last_level ;
    let _promise =
      simple_slot_producer
        ~slot_size:dal_parameters.Dal.Parameters.cryptobox.slot_size
        ~slot_index
        ~from:(migration_level - 1)
        ~into:last_level
        dal_node
        node
        client
    in
    Log.info "Start bakers for the current and the next protocols" ;

    let baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.create ~dal_node_rpc_endpoint node client
    in
    let* () = Agnostic_baker.run baker in

    let* _level = Node.wait_for_level node migration_level in
    Log.info "migrated to next protocol, starting a second DAL node accuser" ;
    let dal_node_p2p_endpoint = Dal_node.listen_addr dal_node in
    let accuser = Dal_node.create ~node () in
    let* () =
      Dal_node.init_config
        ~peers:[dal_node_p2p_endpoint]
        ~operator_profiles:[slot_index]
        accuser
    in
    let* () = Dal_node.run accuser ~wait_ready:true in

    let* _level = Node.wait_for_level node last_level in
    let* () = Agnostic_baker.terminate baker in

    Lwt_list.iter_s
      (fun delegate ->
        let* dal_participation =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_delegate_dal_participation
               ~block:(string_of_int (last_level - 1))
               delegate.Account.public_key_hash
        in
        Check.is_false
          dal_participation.denounced
          ~__LOC__
          ~error_msg:"Expected the delegate to not be denounced" ;
        unit)
      (Array.to_list Account.Bootstrap.keys)
  in
  test_l1_migration_scenario
    ~scenario
    ~tags
    ~description
    ~uses:[Constant.octez_agnostic_baker]
    ~activation_timestamp:Now
    ~operator_profiles:[slot_index]
    ~minimal_block_delay:
      (* to be sure there is enough time to published slots *)
      "2"
    ~blocks_per_cycle:32
    ~consensus_committee_size:512
      (* Use more blocks per cycle and more shards so that we're sure that we
         encounter traps: there are 32 (blocks per cycle) - 4 (migration level)
         - 8 (attestation lag) = 20 attestable levels; so 20 * 512 = 10240
         shards. Since [traps_fraction = 1 / 2000], the probability to not
         encounter at least one trap in these many shards is (1 - 1/2000)^10240
         = 0.6%. *)
    ~number_of_shards:512
    ~migrate_from
    ~migrate_to
    ()

let test_migration_parameters ~migrate_from ~migrate_to =
  let tags = ["migration"; "parameters"] in
  let description = "test parameters at migration" in
  let scenario ~migration_level _dal_parameters client node dal_node =
    let* proto_params =
      Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
    in
    let minimal_block_delay =
      JSON.(proto_params |-> "minimal_block_delay" |> as_int)
    in
    let attestation_lag =
      JSON.(proto_params |-> "dal_parametric" |-> "attestation_lag" |> as_int)
    in
    let baker =
      let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
      Agnostic_baker.create ~dal_node_rpc_endpoint node client
    in
    let* () = Agnostic_baker.run baker in

    let* _level = Node.wait_for_level node migration_level in
    Log.info "migrated to next protocol, starting a second DAL node accuser" ;
    let* new_proto_params =
      Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
    in
    let new_minimal_block_delay =
      JSON.(new_proto_params |-> "minimal_block_delay" |> as_int)
    in
    let new_attestation_lag =
      JSON.(proto_params |-> "dal_parametric" |-> "attestation_lag" |> as_int)
    in
    let same_parameters =
      minimal_block_delay = new_minimal_block_delay
      && attestation_lag = new_attestation_lag
    in
    Check.is_true same_parameters ~error_msg:"Deal with issue #7983" ;
    unit
  in
  test_l1_migration_scenario
    ~scenario
    ~tags
    ~description
    ~uses:[Constant.octez_agnostic_baker]
    ~activation_timestamp:Now
    ~custom_constants:Protocol.Constants_mainnet
    ~migrate_from
    ~migrate_to
    ()

let test_operator_profile _protocol _dal_parameters _cryptobox _node _client
    dal_node =
  let index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator index])) in
  let* () =
    check_profiles
      ~__LOC__
      dal_node
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  unit

let monitor_finalized_levels_events ~__LOC__ ~last_notified_level ~target_level
    dal_node =
  let next_finalized_level = ref (last_notified_level + 1) in
  Dal_node.wait_for dal_node "dal_new_L1_final_block.v0" (fun e ->
      let finalized_level = JSON.(e |-> "level" |> as_int) in
      Check.(
        (finalized_level = !next_finalized_level)
          ~__LOC__
          int
          ~error_msg:"Expected next finalized level to be %R (got %L)") ;
      incr next_finalized_level ;
      if finalized_level = target_level then Some finalized_level else None)

(** The following test checks various things related to the DAL node crawler:
- We check that the content of the file "last_processed_level" storing the last
  processed finalized level by the crawler is updated correctly;
- We restart the L1 node to check that the DAL node doesn't crash;
- We restart the DAL node and bake meanwhile. Then, we check that it's able to
  catch up;
- We check that every finalized level is processed exactly once by the crawler.
*)
let test_dal_node_crawler_reconnects_to_l1 _protocol _dal_parameters _cryptobox
    node client dal_node =
  (* The number of blocks we make in a raw before some additional checks or
     nodes behaviour change. *)
  let num_blocks = 5 in
  (* The following helper function allows baking [num_blocks]. Then, it waits
     for the DAL node's L1 crawler to process those blocks. Finally, it checks
     that disk storage of the last finalized processed level is correctly
     updated. *)
  let bake_delta_blocks_and_check_last_processed_level () =
    let* target_level =
      let* level0 = Client.level client in
      return (level0 + num_blocks)
    in
    let finalized_target = target_level - 2 in
    let wait_l1_crawler_processing =
      wait_for_layer1_head dal_node target_level
    in
    let wait_finalized_target =
      wait_for_layer1_final_block dal_node finalized_target
    in

    let* () = bake_for ~count:num_blocks client in
    let* () = wait_l1_crawler_processing in
    let* () = wait_finalized_target in
    let* written_last_finalized_level =
      Dal_node.load_last_finalized_processed_level dal_node
    in
    let written_last_finalized_level =
      Option.value ~default:0 written_last_finalized_level
    in
    Check.(
      (finalized_target = written_last_finalized_level)
        int
        ~error_msg:"Expected last processed finalized level %R (got %L)") ;
    unit
  in
  let* () = bake_for ~count:num_blocks client in

  (* For this first part of the test, we will bake [num_blocks] blocks
     twice. The two baking sessions are separated by an L1 node restart. *)
  Log.info "1.a Bake some blocks and check the content of last_processed_level" ;
  let* start_level = Client.level client in
  let finalized_levels_events =
    let start_finalized_level = start_level - 2 in
    assert (start_finalized_level > 0) ;
    monitor_finalized_levels_events
      dal_node
      ~__LOC__
      ~last_notified_level:start_finalized_level
      ~target_level:(start_finalized_level + (2 * num_blocks))
  in
  let* () = bake_delta_blocks_and_check_last_processed_level () in
  Log.info "1.b Restart L1 node, bake and check last_processed_level's content" ;
  let* () = Node.terminate ~timeout:10. node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* () = bake_delta_blocks_and_check_last_processed_level () in
  let* last_finalized_level = finalized_levels_events in

  (* For this second part of the test, we stop the DAL node, bake [num_blocks]
     blocks with [bake_for] function, then, we restart the DAL node and bake
     additional [num_blocks] with our internal helper. *)
  Log.info "2. Stop DAL node, bake, restart it and check that it catchs-up" ;
  let* () = Dal_node.terminate ~timeout:10. dal_node in

  (* Here, we bake some blocks to make the DAL node's crawler a little bit
     late. *)
  let* () = bake_for ~count:num_blocks client in

  let* head_level = Client.level client in
  (* before the crawler is started, the bootstrap phase advances the
     [last_processed_level] (so [last_notified_level]) to the following
     value: *)
  let last_notified_level = head_level - 3 in

  (* Restart the DAL node, the finalized events watcher promise, and wait until
     the node is ready. We wait for the node to be ready after spawning an
     instance of seen_finalized_per_level_events to prevent the test from being
     flaky, in case the crawler starts processing finalized blocks and emitting
     "dal_node_layer_1_new_final_block" events before we start observing
     them. *)
  let* () = Dal_node.run ~wait_ready:false dal_node in

  let finalized_levels_events =
    monitor_finalized_levels_events
      dal_node
      ~__LOC__
      ~last_notified_level
      ~target_level:(last_finalized_level + (2 * num_blocks))
  in
  let* () = Dal_node.wait_for_ready dal_node in
  let* () = bake_delta_blocks_and_check_last_processed_level () in

  let* last_finalized_level = finalized_levels_events in
  let* expected_final_finalized_level =
    let* level = Client.level client in
    return @@ (level - 2)
  in
  Check.(
    (last_finalized_level = expected_final_finalized_level)
      int
      ~error_msg:"Expected last processed finalized level %R (got %L)") ;
  unit

(* We run a L1 node and stop the DAL node for a period and then restart it. If
   the DAL node should participate in refutations we restart it after more than
   2 cycles, but less than the history's mode length. Otherwise, we restart it
   less than the L1's history length. In both cases the DAL node should not fail
   when restarted. To make the distinction between the two cases we use the
   profile of the initial DAL node. It is expected to be either a bootstrap
   node, who stores data for [2*attestation_lag] blocks or a producer node who
   stores much more data. *)
let test_restart_dal_node _protocol dal_parameters _cryptobox node client
    dal_node =
  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in
  let* history_mode = Node.RPC.call node @@ RPC.get_config_history_mode in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let blocks_preservation_cycles =
    JSON.(history_mode |-> "blocks_preservation_cycles" |> as_int)
  in
  let additional_cycles =
    JSON.(
      history_mode |-> "history_mode" |-> "rolling" |-> "additional_cycles"
      |> as_int)
  in
  let l1_history_length =
    (blocks_preservation_cycles + additional_cycles) * blocks_per_cycle
  in
  let* profile = Dal_RPC.(call dal_node @@ get_profiles ()) in
  let offline_period =
    if profile = Dal_RPC.Bootstrap then l1_history_length + blocks_per_cycle
    else (* this is just a not too small value *)
      3 * blocks_per_cycle
  in
  let* baker =
    let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.init
      ~delegates:all_pkhs
      ~liquidity_baking_toggle_vote:(Some On)
      ~state_recorder:true
      ~force_apply_from_round:0
      ~dal_node_rpc_endpoint
      node
      client
  in
  let stop_level = 10 in
  let restart_level = stop_level + offline_period in
  Log.info
    "We let the DAL node run for a few levels (till level %d), then we stop \
     it, then we restart it at level %d"
    stop_level
    restart_level ;
  let* _ = Node.wait_for_level node stop_level in
  let* () = Dal_node.terminate dal_node in
  let* _ = Node.wait_for_level node restart_level in
  let* () = Dal_node.run dal_node in

  let last_finalized_level =
    restart_level + dal_parameters.Dal.Parameters.attestation_lag
  in
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node last_finalized_level
  in
  let* _ = Node.wait_for_level node (last_finalized_level + 2) in
  let* () = Agnostic_baker.terminate baker in
  let* () = wait_for_dal_node in
  if profile <> Dal_RPC.Bootstrap then
    let expected_levels =
      let offset = dal_parameters.attestation_lag + 1 in
      List.init
        (last_finalized_level - offset + 1)
        (fun i -> string_of_int (offset + i))
    in
    check_skip_list_store
      dal_node
      ~number_of_slots:dal_parameters.number_of_slots
      ~expected_levels
  else
    check_skip_list_store
      dal_node
      ~number_of_slots:dal_parameters.number_of_slots
      ~expected_levels:[]

(** Test: dal_slots_retrievability

    This test checks that a DAL node can retrieve historical slot data using the
    --slots-backup-uri option, from various archive sources (file:// URIs), and
    appropriately validates slot integrity when --trust-slots-backup-uris is
    disabled.

    The test proceeds in three phases:

    A. Node setup:
       - Launch 3 DAL producer nodes (dal_pub1, dal_pub2, dal_pub3),
         each publishing a distinct slot (index 1, 2, 3 respectively).
       - Launch 4 DAL fetcher nodes:
         - valid_dal_fetcher_1_2: untrusted sources, has access to dal_pub1
           and dal_pub2
         - valid_dal_fetcher_3_trusted: trusted sources, access to dal_pub3
         - invalid_dal_fetcher_bad_uri: untrusted sources, invalid URI
         - invalid_dal_fetcher_bad_uri_trusted: trusted sources, invalid URI

    B. Slot publication:
       - Each dal_pubX publishes one slot at the same published_level.
       - A few blocks are baked to reach finality and ensure the slots are
         attested and commitments are available in memory, sqlite, and L1 context.

    C. Retrieval and validation checks:
       C.1 - Fetch valid slots via memory cache
       C.2 - Fetch valid slots after restarting the DAL node (sqlite skip list)
       C.3 - Fetch valid slots after removing sqlite DB (L1 skip list)
       C.4 - Fetch slot from trusted archive (no validation)
       C.5 - Attempt to fetch from a missing archive (expected 404)
       C.6 - Tamper slot content to trigger commitment mismatch (expected 404)
       C.7 - Tamper slot size to trigger size mismatch (expected 404)
       C.8 - Use invalid archive URI (expected 500 with resolution errors)
       C.9 - Try to fetch slot with no commitment on L1 (expected 500)
       C.10 - Use unregistered plugin for old level (expected 500)
       C.11 - Try future level (expected 404)
       C.12 - Try out-of-bounds slot index (expected 404)
*)
let dal_slots_retrievability =
  (* Helper to run the RPC that fetches a slot from a given DAL node *)
  let get_slot_rpc dal_node ~published_level ~slot_index =
    Dal_RPC.(
      call_raw dal_node
      @@ get_level_slot_content ~slot_level:published_level ~slot_index)
  in

  (* Wait for a specific event in DAL node logs *)
  let wait_for_event_promise dal_node event_name =
    Dal_node.wait_for
      dal_node
      (Format.sprintf "dal_%s.v0" event_name)
      (fun _event -> Some ())
  in

  (* Assert the fetch succeeded with HTTP 200 *)
  let fetch_succeeded ~__LOC__ promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 200 then unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 200.\n%s" code body
  in

  (* Assert the fetch failed with 404, optionally wait for expected event *)
  let fetch_404_expected ~__LOC__ ?expected_event promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 404 then Option.value expected_event ~default:unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 404.\n%s" code body
  in

  (* Assert the fetch failed with 500, optionally check error message and wait for event *)
  let fetch_500_expected ~__LOC__ ?expected_error ?expected_event promise =
    let* RPC_core.{code; body; _} = promise in
    if code = 500 then
      let () =
        match expected_error with
        | Some msg ->
            Check.((body =~ rex msg) ~error_msg:"expected error =~ %R, got %L")
        | None -> ()
      in
      Option.value expected_event ~default:unit
    else
      Test.fail ~__LOC__ "Unexpected response %d instead of 500.\n%s" code body
  in

  (* Main test body *)
  fun _protocol dal_parameters _cryptobox l1_node client original_dal_node ->
    (* A. NODE SETUP *)
    let* () = Dal_node.terminate original_dal_node in
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let number_of_slots = dal_parameters.number_of_slots in
    let attestation_lag = dal_parameters.attestation_lag in

    let mk_dal_pub ~idx ~name =
      make_dal_node ~operator_profiles:[idx] ~name l1_node
    in
    let slots_store_path dal_node =
      Format.sprintf "/%s/store/slot_store" @@ Dal_node.data_dir dal_node
    in
    let slots_store_uri dal_node =
      Format.sprintf "file://%s" @@ slots_store_path dal_node
    in

    let* dal_pub1 = mk_dal_pub ~idx:1 ~name:"dal_pub1" in
    let* dal_pub2 = mk_dal_pub ~idx:2 ~name:"dal_pub2" in
    let* dal_pub3 = mk_dal_pub ~idx:3 ~name:"dal_pub3" in
    let archive1 = slots_store_uri dal_pub1 in
    let archive2 = slots_store_uri dal_pub2 in
    let archive3 = slots_store_uri dal_pub3 in

    (* Launch four fetchers with different backup configurations *)
    let* valid_dal_fetcher_1_2 =
      make_dal_node
        ~operator_profiles:[0]
        ~name:"valid_dal_fetcher_1_2"
        ~slots_backup_uris:[archive1; archive2]
        ~event_level:`Notice
        l1_node
    in
    let* valid_dal_fetcher_3_trusted =
      make_dal_node
        ~operator_profiles:[0]
        ~name:"valid_dal_fetcher_3_trusted"
        ~slots_backup_uris:[archive3]
        ~trust_slots_backup_uris:true
        ~event_level:`Notice
        l1_node
    in
    let* invalid_dal_fetcher_bad_uri =
      make_dal_node
        ~operator_profiles:[5]
        ~name:"invalid_dal_fetcher_bad_uri"
        ~slots_backup_uris:["http://some-fake.endpoint"]
        ~event_level:`Notice
        l1_node
    in
    let* invalid_dal_fetcher_bad_uri_trusted =
      make_dal_node
        ~operator_profiles:[6]
        ~name:"invalid_dal_fetcher_bad_uri_trusted"
        ~slots_backup_uris:["http://some-fake.endpoint"]
        ~trust_slots_backup_uris:true
        ~event_level:`Notice
        l1_node
    in

    (* B. SLOT PUBLICATION *)
    let* start_level = Node.get_level l1_node in
    let published_level = start_level + 1 in
    let* () =
      Lwt_list.iteri_p
        (fun idx dal_publisher ->
          let pub_idx = idx + 1 in
          let source = Account.Bootstrap.keys.(pub_idx) in
          let* _commit =
            Format.sprintf "Slot<%d, %d> is cool ..." published_level pub_idx
            |> Helpers.make_slot ~slot_size
            |> Helpers.publish_and_store_slot
                 client
                 dal_publisher
                 source
                 ~index:pub_idx
          in
          unit)
        [dal_pub1; dal_pub2; dal_pub3]
    in
    let* () = bake_for ~count:(attestation_lag + 2) client in

    (* C. RETRIEVAL & VALIDATION *)
    let check_valid_dal_fetcher_1_2 ~__LOC__ =
      Lwt_list.iter_s
        (fun slot_index ->
          get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index
          |> fetch_succeeded ~__LOC__)
        [1; 2]
    in

    (* C.1 Fetch valid slots via memory cache *)
    let* () =
      Log.info "C.1: memory cache" ;
      check_valid_dal_fetcher_1_2 ~__LOC__
    in

    (* C.2 Restart and fetch via SQLite skip list *)
    let* () =
      Log.info "C.2: sqlite skip list" ;
      let* () = Dal_node.terminate valid_dal_fetcher_1_2 in
      let* () = Dal_node.run valid_dal_fetcher_1_2 in
      check_valid_dal_fetcher_1_2 ~__LOC__
    in

    (* C.3 Remove sqlite DB and fetch via L1 skip list *)
    let* () =
      Log.info "C.3: L1 skip list" ;
      let skip_db =
        Format.sprintf "%s/store/skip_list_store"
        @@ Dal_node.data_dir valid_dal_fetcher_1_2
      in
      let () = Sys.command ("rm -rf " ^ skip_db) |> ignore in
      let* () = Dal_node.terminate valid_dal_fetcher_1_2 in
      let* () = Dal_node.run valid_dal_fetcher_1_2 in
      check_valid_dal_fetcher_1_2 ~__LOC__
    in

    (* C.4 Fetch from trusted archive (no validation) *)
    let* () =
      Log.info "C.4: trusted archive" ;
      get_slot_rpc valid_dal_fetcher_3_trusted ~published_level ~slot_index:3
      |> fetch_succeeded ~__LOC__
    in

    (* C.5 Missing archive for a slot index: expect 404 *)
    let* () =
      Log.info "C.5: missing archive for slot index 3" ;
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:3
      |> fetch_404_expected ~__LOC__
    in

    let slot_path dal ~slot_index =
      Format.sprintf
        "%s/%d_%d_%d"
        (slots_store_path dal)
        published_level
        slot_index
        slot_size
    in
    (* C.6 Tamper slot1 content: expect 404 + event *)
    let* () =
      Log.info "C.6: Tamper slot1 content" ;
      let () =
        Sys.command
          (Format.sprintf
             "cp %s %s"
             (slot_path dal_pub2 ~slot_index:2)
             (slot_path dal_pub1 ~slot_index:1))
        |> fun exit_code -> assert (exit_code = 0)
      in
      let expected_event =
        wait_for_event_promise
          valid_dal_fetcher_1_2
          "slot_from_backup_has_unexpected_commitment"
      in
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:1
      |> fetch_404_expected ~__LOC__ ~expected_event
    in

    (* C.7 Tamper slot2 size: expect 404 + event *)
    let* () =
      Log.info "C.7: Tamper slot2 size" ;
      let () =
        Sys.command ("echo bad > " ^ slot_path dal_pub2 ~slot_index:2) |> ignore
      in
      let expected_event =
        wait_for_event_promise
          valid_dal_fetcher_1_2
          "slot_from_backup_has_unexpected_size"
      in
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level ~slot_index:2
      |> fetch_404_expected ~__LOC__ ~expected_event
    in

    (* C.8 Invalid URI (untrusted): expect 500 *)
    let* () =
      Log.info "C.8: invalid URI trusted & untrusted" ;
      let* () =
        get_slot_rpc invalid_dal_fetcher_bad_uri ~published_level ~slot_index:2
        |> fetch_500_expected
             ~__LOC__
             ~expected_error:"resolution failed: name resolution failed"
      in
      get_slot_rpc
        invalid_dal_fetcher_bad_uri_trusted
        ~published_level
        ~slot_index:2
      |> fetch_500_expected
           ~__LOC__
           ~expected_error:"resolution failed: name resolution failed"
    in

    (* C.9 No commitment on L1 on the given slot index: expect 500 *)
    let* () =
      Log.info "C.9: no commitment on L1" ;
      get_slot_rpc invalid_dal_fetcher_bad_uri ~published_level ~slot_index:4
      |> fetch_500_expected
           ~__LOC__
           ~expected_error:"No_commitment_published_on_l1_for_slot_id"
    in

    (* C.10 No DAL plugin for level 0: expect 500 *)
    let* () =
      Log.info "C.10: no DAL plugin" ;
      get_slot_rpc valid_dal_fetcher_1_2 ~published_level:0 ~slot_index:1
      |> fetch_500_expected ~__LOC__ ~expected_error:"no_plugin_for_given_level"
    in

    (* C.11 Future level: expect 404 *)
    let* () =
      Log.info "C.11: future level" ;
      get_slot_rpc
        valid_dal_fetcher_3_trusted
        ~published_level:1000000
        ~slot_index:1
      |> fetch_404_expected ~__LOC__
    in

    (* C.12 Out-of-bounds slot index: expect 404 *)
    let* () =
      Log.info "C.12: out-of-bounds index" ;
      get_slot_rpc
        valid_dal_fetcher_3_trusted
        ~published_level
        ~slot_index:number_of_slots
      |> fetch_404_expected ~__LOC__
    in

    unit

let test_attestation_through_p2p _protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  (* In this test we have three DAL nodes:
     - a boostrap one to connect the other two (producer and attester),
     - a slot producer on slot 0,
     - an attester for all the bootstrap pkh.

     We check that when the slot producer publishes a slot, it ends up
     being attested.
  *)
  let index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.Dal.Parameters.attestation_lag in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let* bootstrap_peer_id = peer_id dal_bootstrap in

  (* Check that the attestation threshold for this test is 100%. If
     not, this means that we forgot to register the test with
     ~attestation_threshold:100 *)
  Check.((dal_parameters.Dal.Parameters.attestation_threshold = 100) int)
    ~error_msg:"attestation_threshold value (%L) should be 100" ;
  (* Check that the dal node passed as argument to this test function
     is a running bootstrap DAL node. If not, this means that we
     forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
  let* () = Dal_node.run ~wait_ready:true producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in
  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () = Dal_node.init_config ~attester_profiles:all_pkhs ~peers attester in
  let* () = Dal_node.run ~wait_ready:true attester in
  let* attester_peer_id = peer_id attester in

  let client = Client.with_dal_node client ~dal_node:attester in

  (* Wait for a GRAFT message between the attester and the producer,
     in any direction. *)
  let check_graft pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (attester, attester_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_graft_promises = List.map check_graft all_pkhs in
  Log.info "Waiting for grafting of the attester - producer connection" ;
  let* () =
    check_profiles
      ~__LOC__
      attester
      ~expected:
        Dal_RPC.(Controller (List.map (fun pkh -> Attester pkh) all_pkhs))
  in
  (* We need to bake some blocks until the L1 node notifies the
     attester DAL nodes that some L1 block is final and they have DAL
     attestation rights in it. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Attester - producer connection grafted" ;

  (* Attester should be connected to:
     - the bootstrap DAL node on all the topics,
     - the producer on all topics with slot_index=index *)
  let* () =
    let expected topic_pkh =
      Seq.ints 0 |> Seq.take number_of_slots
      |> Seq.map (fun topic_slot_index ->
             ( {Dal_RPC.topic_slot_index; topic_pkh},
               bootstrap_peer_id
               :: (if topic_slot_index = index then [producer_peer_id] else [])
             ))
      |> List.of_seq
    in
    let expected = List.concat (List.map expected all_pkhs) in
    check_topics_peers ~__LOC__ attester ~expected
  in

  (* The slot producer should be connected to:
     - the attester and the bootstrap DAL node on all topics with slot_index=index,
  *)
  let* () =
    let expected =
      all_pkhs
      |> List.map (fun topic_pkh ->
             ( {Dal_RPC.topic_slot_index = index; topic_pkh},
               [bootstrap_peer_id; attester_peer_id] ))
    in
    check_topics_peers ~__LOC__ producer ~expected
  in

  (* Produce and publish a slot *)
  let source = Constant.bootstrap1 in
  let slot_content = "content" in

  let all_shard_indices =
    Seq.ints 0 |> Seq.take number_of_shards |> List.of_seq
  in
  let wait_slot ~published_level ~slot_index =
    Lwt.join
    @@ List.map
         (fun shard_index ->
           wait_for_stored_slot
             ~shard_index
             attester
             ~published_level
             ~slot_index)
         all_shard_indices
  in
  let* publication_level, _commitment, () =
    publish_store_and_wait_slot
      node
      client
      producer
      source
      ~index
      ~wait_slot
      ~number_of_extra_blocks_to_bake:2
    @@ Helpers.make_slot ~slot_size slot_content
  in

  Log.info "Slot produced and published" ;

  let* () =
    bake_until_processed
      ~level:(publication_level + attestation_lag)
      client
      [attester]
  in

  let* status =
    Dal_RPC.(
      call attester
      @@ get_level_slot_status ~slot_level:publication_level ~slot_index:index)
  in
  Check.(status = "attested")
    Check.string
    ~error_msg:"Expected status %R (got %L)" ;
  Log.info "Slot sucessfully attested" ;
  unit

module History_rpcs = struct
  (* In the following function, no migration is performed (expect from genesis
     to alpha) when [migration_level] is equal to or smaller than 1. *)
  let scenario ?(migration_level = 1) ~slot_index ~first_cell_level
      ~first_dal_level ~last_confirmed_published_level protocol dal_parameters
      client node dal_node =
    let module Map_int = Map.Make (Int) in
    Log.info "slot_index = %d" slot_index ;
    let client = Client.with_dal_node client ~dal_node in
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let* starting_level = Client.level client in

    let rec publish ~max_level level commitments =
      (* Try to publish a slot at each level *)
      if level > max_level then return commitments
      else
        let published_level = level + 1 in
        let wait_mempool_injection =
          Node.wait_for node "operation_injected.v0" (fun _ -> Some ())
        in
        let* commitment =
          Helpers.publish_and_store_slot
            client
            dal_node
            Constant.bootstrap1
            ~index:slot_index
            ~force:true
          @@ Helpers.make_slot ~slot_size ("slot " ^ string_of_int level)
        in
        let* () = wait_mempool_injection in
        Log.info
          "Publish commitment %s at published_level %d@."
          commitment
          published_level ;
        let* () = bake_for client in
        let* _level = Node.wait_for_level node (level + 1) in
        publish
          ~max_level
          (level + 1)
          (Map_int.add published_level commitment commitments)
    in
    Log.info "Publishing some commitments in the previous protocol@." ;
    let* commitments =
      publish ~max_level:migration_level starting_level Map_int.empty
    in

    let* dal_parameters = Dal.Parameters.from_client client in
    let lag = dal_parameters.attestation_lag in

    let number_of_slots = dal_parameters.number_of_slots in
    Log.info "attestation_lag = %d, number_of_slots = %d" lag number_of_slots ;

    let last_attested_level = last_confirmed_published_level + lag in
    (* The maximum level that needs to be reached (we use +2 to make last
       attested level final). *)
    let max_level = last_attested_level + 2 in

    let wait_for_dal_node =
      wait_for_layer1_final_block dal_node last_attested_level
    in

    let* first_level_new_proto = Client.level client in
    Log.info "Publishing some commitments in the new protocol@." ;
    let* commitments = publish ~max_level first_level_new_proto commitments in

    let module SeenIndexes = Set.Make (struct
      type t = int

      let compare = compare
    end) in
    let seen_indexes = ref SeenIndexes.empty in
    let at_least_one_attested_status = ref false in
    let rec check_cell cell ~check_level =
      let skip_list_kind = JSON.(cell |-> "kind" |> as_string) in
      let expected_skip_list_kind = "dal_skip_list" in
      Check.(
        (skip_list_kind = expected_skip_list_kind)
          string
          ~error_msg:"Unexpected skip list kind: got %L, expected %R") ;
      let skip_list = JSON.(cell |-> "skip_list") in
      let cell_index = JSON.(skip_list |-> "index" |> as_int) in
      if SeenIndexes.mem cell_index !seen_indexes then unit
      else (
        seen_indexes := SeenIndexes.add cell_index !seen_indexes ;
        let content = JSON.(skip_list |-> "content") in
        let cell_level = JSON.(content |-> "level" |> as_int) in
        (match check_level with
        | Some level ->
            assert (level >= first_dal_level) ;
            let expected_level =
              if level = first_dal_level then (* the "level" of genesis *) 0
              else level - lag
            in
            Check.(
              (cell_level = expected_level)
                int
                ~error_msg:"Unexpected cell level: got %L, expected %R")
        | None -> ()) ;
        let cell_slot_index = JSON.(content |-> "index" |> as_int) in
        let () =
          match check_level with
          | None -> ()
          | Some level ->
              let expected_slot_index =
                if level = first_dal_level then
                  (* the "slot index" of genesis *)
                  0
                else number_of_slots - 1
              in
              Check.(
                (cell_slot_index = expected_slot_index)
                  int
                  ~error_msg:"Unexpected slot index: got %L, expected %R")
        in
        (if cell_index > 0 then
           let expected_cell_index =
             ((cell_level - 1 - first_cell_level) * number_of_slots)
             + cell_slot_index
           in
           Check.(
             (cell_index = expected_cell_index)
               int
               ~error_msg:"Unexpected cell index: got %L, expected %R")) ;
        let cell_kind = JSON.(content |-> "kind" |> as_string) in
        let published cell_level =
          (* - Cond 1: we publish at [slot_index]
             - Cond 2: the (published) [cell_level] is greater than
             [starting_level] *)
          cell_slot_index = slot_index && cell_level > starting_level
        in
        let expected_kind =
          if not (published cell_level) then "unpublished"
          else (
            at_least_one_attested_status := true ;
            "published")
        in
        Check.(
          (cell_kind = expected_kind)
            string
            ~error_msg:"Unexpected cell kind: got %L, expected %R") ;
        (if cell_kind = "published" || cell_kind = "attested" then
           let commitment = JSON.(content |-> "commitment" |> as_string) in
           Check.(
             (commitment = Map_int.find cell_level commitments)
               string
               ~error_msg:"Unexpected commitment: got %L, expected %R")) ;
        let back_pointers =
          JSON.(skip_list |-> "back_pointers" |> as_list)
          |> List.map JSON.as_string
        in
        let expecting_no_back_pointers = cell_index = 0 in
        let no_back_pointers = back_pointers = [] in
        Check.(
          (no_back_pointers = expecting_no_back_pointers)
            bool
            ~error_msg:
              "Unexpected non-existence of back_pointers: got %L, expected %R") ;
        Lwt_list.iter_s
          (fun hash ->
            let* cell =
              Dal_RPC.(
                call dal_node
                @@ get_plugin_commitments_history_hash
                     ~proto_hash:(Protocol.hash protocol)
                     ~hash
                     ())
            in
            check_cell cell ~check_level:None)
          back_pointers)
    in
    let rec check_history level =
      if level > last_attested_level then unit
      else
        let* cell =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_dal_commitments_history
               ~block:(string_of_int level)
               ()
        in
        let* () = check_cell cell ~check_level:(Some level) in
        check_history (level + 1)
    in
    let* () = wait_for_dal_node in
    let* () = check_history first_dal_level in
    Check.(
      (!at_least_one_attested_status = true)
        bool
        ~error_msg:"No cell with the 'attested' status has been visited") ;
    unit

  let test_commitments_history_rpcs protocols =
    let scenario protocol dal_parameters _ client node dal_node =
      scenario
        ~slot_index:3
        ~first_cell_level:0
        ~first_dal_level:1
        ~last_confirmed_published_level:3
        protocol
        dal_parameters
        node
        client
        dal_node
    in
    let tags = ["rpc"; "skip_list"] in
    let description = "commitments history RPCs" in
    scenario_with_layer1_and_dal_nodes
      ~tags
      ~operator_profiles:[3; 15]
      description
      scenario
      protocols

  let test_commitments_history_rpcs_with_migration ~migrate_from ~migrate_to
      ~migration_level =
    let slot_index = 3 in
    let scenario ~migrate_to ~migration_level dal_parameters =
      let lag = dal_parameters.Dal.Parameters.attestation_lag in
      Check.(
        (migration_level > lag)
          int
          ~error_msg:
            "The migration level (%L) should be greater than the attestation \
             lag (%R)") ;
      (* The first cell level has this value, if the previous protocol
         doesn't have the DAL activated. *)
      (* We'll have 3 levels with a published and attested slot. *)
      let last_confirmed_published_level = migration_level + 3 in
      scenario
        ~slot_index
        ~first_cell_level:0
        ~first_dal_level:1
        ~last_confirmed_published_level
        ~migration_level
        migrate_to
        dal_parameters
    in

    let description = "test commitments history with migration" in
    let tags = ["rpc"; "skip_list"] in
    test_l1_migration_scenario
      ~migrate_from
      ~migrate_to
      ~migration_level
      ~scenario:(fun ~migration_level -> scenario ~migrate_to ~migration_level)
      ~tags
      ~description
      ~operator_profiles:[slot_index] (* use the same parameters as Alpha *)
      ~consensus_committee_size:512
      ~attestation_lag:8
      ~number_of_slots:32
      ~number_of_shards:512
      ~slot_size:126944
      ~redundancy_factor:8
      ~page_size:3967
      ()
end

(* This test sets up a migration and starts the DAL around the migration
   block. It mainly tests that the DAL node uses the right protocol plugins at
   migration.

   The [offset] says when to start the DAL node wrt to the migration level. It
   can be negative.

   If [check_rpc] is set, a call is made to [get_attestable_slots] on the
   migration level. This will fail if the encoding of the DAL committee has
   changed between protocols and the node does not use the right plugin. *)
let test_start_dal_node_around_migration ~migrate_from ~migrate_to ~offset
    ~check_rpc =
  Test.register
    ~__FILE__
    ~tags:
      [
        Tag.tezos2;
        "dal";
        Protocol.tag migrate_from;
        Protocol.tag migrate_to;
        "migration";
        "plugin";
      ]
    ~uses:[Constant.octez_dal_node]
    ~title:
      (sf
         "%s->%s: start the DAL node with offset of %d levels wrt to the \
          migration level"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         offset)
  @@ fun () ->
  (* be sure to use the same parameters in both protocols *)
  let consensus_committee_size = Some 512 in
  let attestation_lag = Some 8 in
  let number_of_slots = Some 32 in
  let number_of_shards = Some 512 in
  let slot_size = Some 126944 in
  let redundancy_factor = Some 8 in
  let page_size = Some 3967 in
  let parameter_overrides =
    make_int_parameter ["consensus_committee_size"] consensus_committee_size
    @ make_int_parameter ["dal_parametric"; "attestation_lag"] attestation_lag
    @ make_int_parameter ["dal_parametric"; "number_of_slots"] number_of_slots
    @ make_int_parameter ["dal_parametric"; "number_of_shards"] number_of_shards
    @ make_int_parameter
        ["dal_parametric"; "redundancy_factor"]
        redundancy_factor
    @ make_int_parameter ["dal_parametric"; "slot_size"] slot_size
    @ make_int_parameter ["dal_parametric"; "page_size"] page_size
  in
  let* node, client, dal_parameters =
    setup_node ~parameter_overrides ~protocol:migrate_from ()
  in

  (* The [+2] here is a bit arbitrary. We just want to avoid possible corner
     cases due to a too small migration level, which will not occur in
     practice. *)
  let migration_level = dal_parameters.attestation_lag + 2 in

  Log.info "Set user-activated-upgrade at level %d" migration_level ;
  let* () = Node.terminate node in
  let patch_config =
    Node.Config_file.update_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let nodes_args = Node.[Synchronisation_threshold 0; No_bootstrap_peers] in
  let* () = Node.run ~patch_config node nodes_args in
  let* () = Node.wait_for_ready node in

  let* dal_node =
    if offset >= 0 then (
      (* [bake_for ~count] does not work across the migration block *)
      let* () = bake_for ~count:(migration_level - 1) client in
      let* () = if offset > 0 then bake_for ~count:offset client else unit in

      Log.info "Start the DAL node" ;
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config dal_node in
      let* () = Dal_node.run dal_node ~wait_ready:true in
      return dal_node)
    else
      let* () = bake_for ~count:(migration_level - 1 + offset) client in

      Log.info "Start the DAL node" ;
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config dal_node in
      let* () = Dal_node.run dal_node ~wait_ready:true in

      (* that is, till the migration level *)
      let* () = bake_for ~count:(-offset) client in
      return dal_node
  in
  let* () =
    if check_rpc then
      let* _ =
        Dal_RPC.(
          call dal_node
          @@ get_attestable_slots
               ~attester:Constant.bootstrap1
               ~attested_level:migration_level)
      in
      unit
    else unit
  in
  let wait_for_dal_node =
    wait_for_layer1_final_block dal_node (migration_level + 2)
  in
  let* () = bake_for ~count:4 client in
  let* () = wait_for_dal_node in
  unit

module Amplification = struct
  let step_counter = ref 0

  let info msg =
    let prefix : string = Format.asprintf "Step %d" !step_counter in
    let () = incr step_counter in
    Log.info ~prefix msg

  (* Test the amplification feature: once it has seen enough (but not
     necessarily all) shards of a given slot, an observer DAL node is
     able to reconstruct the complete slot and send the missing shards
     on the DAL network.

     To test this feature, we need a DAL network on which some shards
     have been lost and need to be recomputed. This is achieved by
     building a network with a single slot producer and several
     attesters corresponding to different bakers; the slot producer is
     only connected to some attesters so the shards needed by the
     other attesters are lost.

     More precisely, the DAL network we build is composed of (omitting
     the bootstrap DAL node):

     slot producer --- 3 attesters --- observer --- 2 attesters

     This topology is obtained by making the slot producer ban the
     nodes which should not be connected to it (the observer and 2
     attesters). For this reason, we call "banned attesters" the 2
     attesters which are only connected to the observer node and
     "non-banned attesters" the 3 attesters which are connected to
     both the slot producer and the observer.
  *)

  (* In amplification tests, we have one attester DAL node per
     boostrap baker. For convenience, we bundle the dal node process
     together with information about the corresponding bootstrap
     baker. *)
  type attester = {
    dal_node : Dal_node.t;
    account : Account.key;
    pkh : string;
    index : int; (* From 0 to 4. *)
    name : string; (* From "attester1" to "attester5" *)
    mutable peer_id : string option;
        (* Delayed and cached call to Dal_node.read_identity *)
  }

  let attester_peer_id (attester : attester) =
    match attester.peer_id with
    | Some pid -> Lwt.return pid
    | None ->
        let* pid = Dal_node.read_identity attester.dal_node in
        attester.peer_id <- Some pid ;
        Lwt.return pid

  let test_amplification _protocol dal_parameters _cryptobox node client
      dal_bootstrap =
    let number_of_banned_attesters = 2 in
    let slot_index = 0 in
    (* Parameters and constants. *)
    let {
      Dal.Parameters.attestation_lag;
      attestation_threshold;
      number_of_slots;
      cryptobox =
        {Dal.Cryptobox.slot_size; number_of_shards; redundancy_factor; _};
      _;
    } =
      dal_parameters
    in

    (* Note: we don't need to configure and start the DAL bootstrap
       node because it is the node started by the
       scenario_with_layer1_and_dal_nodes wrapper. Instead, we check
       that the dal node passed as argument to this test function is a
       running bootstrap DAL node. If not, this means that we forgot
       to register the test with ~bootstrap_profile:true *)
    let* () =
      check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap
    in
    info "bootstrap DAL node is running" ;

    (* When configuring the other DAL nodes, we use dal_bootstrap as
       the single peer. *)
    let peers = [Dal_node.listen_addr dal_bootstrap] in

    (* Create and configure all nodes: a slot producer, an observer,
       and one attester per bootstrap baker. *)
    let make_attester index (account : Account.key) : attester Lwt.t =
      let name = Printf.sprintf "attester-%d" (index + 1) in
      let pkh = account.public_key_hash in
      let dal_node = Dal_node.create ~name ~node () in
      let* () = Dal_node.init_config ~attester_profiles:[pkh] ~peers dal_node in
      let* () = Dal_node.run ~wait_ready:true dal_node in
      return {name; index; dal_node; pkh; account; peer_id = None}
    in
    let* all_attesters =
      Lwt_list.mapi_p make_attester (Array.to_list Account.Bootstrap.keys)
    in
    info "attester DAL node ready" ;
    let banned_attesters, non_banned_attesters =
      List.partition
        (fun attester -> attester.index < number_of_banned_attesters)
        all_attesters
    in
    let slot_producer = Dal_node.create ~name:"producer" ~node () in
    let* () =
      Dal_node.init_config ~operator_profiles:[slot_index] ~peers slot_producer
    in
    (* Promise which will be resolved once the slot producer will be
       connected to all the other DAL nodes (all attesters + observer
       + bootstrap). We will wait for this to happen before banning
       the observer and some of the attesters. *)
    let wait_for_connections_of_producer_promise =
      Dal_node.wait_for_connections slot_producer (2 + List.length all_attesters)
    in

    let* () = Dal_node.run ~wait_ready:true slot_producer in
    let observer = Dal_node.create ~name:"observer" ~node () in
    let* () =
      Dal_node.init_config ~observer_profiles:[slot_index] ~peers observer
    in
    let* () = Dal_node.run ~wait_ready:true observer in

    (* Check that the DAL nodes have the expected profiles. *)
    let* () =
      check_profiles
        ~__LOC__
        slot_producer
        ~expected:Dal_RPC.(Controller [Operator slot_index])
    in
    let* slot_producer_peer_id = Dal_node.read_identity slot_producer in
    info "Slot producer DAL node is running" ;

    let* () =
      check_profiles
        ~__LOC__
        observer
        ~expected:Dal_RPC.(Controller [Observer slot_index])
    in
    let* observer_peer_id = Dal_node.read_identity observer in
    info "Observer DAL node is running" ;

    let* () =
      Lwt.join
      @@ List.map
           (fun attester ->
             check_profiles
               ~__LOC__
               attester.dal_node
               ~expected:Dal_RPC.(Controller [Attester attester.pkh]))
           all_attesters
    in
    info "Attesters are running" ;

    (* Now that all the DAL nodes are running, we need some of them to
       establish grafted connections. The connections between
       attesters and the slot producer have no reason to be grafted on
       other slot indices than the one the slot producer is subscribed
       to, so we instruct [check_events_with_topic] to skip all events
       but the one for [slot_index]. *)
    (* Wait for a GRAFT message between an attester and either an operator
       (legacy producer) or an observer, in any direction. *)
    let check_graft_promise (operator_or_observer, peer_id) attester =
      let* attester_peer_id = attester_peer_id attester in
      Lwt.pick
      @@ check_grafts
           ~number_of_slots
           ~slot_index
           (operator_or_observer, peer_id)
           (attester.dal_node, attester_peer_id)
           attester.pkh
    in
    (* We don't care if the slot producer establishes full connections
       with the DAL nodes it is about to ban so we only wait for the
       GRAFT messages between:
       - the slot producer and the non-banned attesters,
       - the observer and all the attesters *)
    let check_graft_promises =
      List.map
        (check_graft_promise (slot_producer, slot_producer_peer_id))
        non_banned_attesters
      @ List.map
          (check_graft_promise (observer, observer_peer_id))
          all_attesters
    in
    info "Waiting for grafting of the attester - producer/observer connections" ;
    (* Each attester DAL node won't join the DAL network until it has
       processed some finalized L1 block in which the associated baker
       is reported to have some rights. For this to happen, we need to
       bake a few blocks. *)
    let* () = bake_for ~count:3 client in
    let* () = Lwt.join check_graft_promises in
    info "Attester - producer connections grafted" ;

    (* Wait for producer to be connected to all the dal nodes it wants
       to ban because they cannot be banned earlier. *)
    let* () = wait_for_connections_of_producer_promise in

    let ban_p2p_peer this_node ~peer =
      let this_dal_node, this_id = this_node in
      let peer_dal_node, peer_id = peer in
      let acl = "ban" in
      let p =
        let* () = Dal_node.wait_for_disconnection this_dal_node ~peer_id
        and* () =
          Dal_node.wait_for_disconnection peer_dal_node ~peer_id:this_id
        in
        unit
      in
      let* () =
        Dal_RPC.call this_dal_node
        @@ Dal_RPC.patch_p2p_peers_by_id ~peer_id ~acl ()
      in
      p
    in

    (* Slot producer bans observer and banned_attesters. *)
    let* () =
      ban_p2p_peer
        (slot_producer, slot_producer_peer_id)
        ~peer:(observer, observer_peer_id)
    in
    let* () =
      Lwt.join
      @@ List.map
           (fun attester ->
             let* attester_peer_id = attester_peer_id attester in
             ban_p2p_peer
               (slot_producer, slot_producer_peer_id)
               ~peer:(attester.dal_node, attester_peer_id))
           banned_attesters
    in
    info "DAL network is ready" ;

    let* level_before_publication = Client.level client in
    let publication_level = level_before_publication + 1 in
    let attested_level = publication_level + attestation_lag in
    let attestation_level = attested_level - 1 in

    let* assigned_shard_indices_non_banned =
      Lwt_list.map_p
        (fun attester ->
          Dal_RPC.(
            call attester.dal_node
            @@ get_assigned_shard_indices
                 ~level:attestation_level
                 ~pkh:attester.pkh))
        non_banned_attesters
    in
    let* assigned_shard_indices_banned =
      Lwt_list.map_p
        (fun attester ->
          Dal_RPC.(
            call attester.dal_node
            @@ get_assigned_shard_indices
                 ~level:attestation_level
                 ~pkh:attester.pkh))
        banned_attesters
    in
    let total_number_of_assigned_shards =
      List.fold_left
        (fun acc l -> acc + List.length l)
        0
        assigned_shard_indices_non_banned
    in

    (* Check that the non-banned attesters have collectively enough
       assigned shards to reconstruct the slot. *)
    Check.(
      number_of_shards / redundancy_factor < total_number_of_assigned_shards)
      ~__LOC__
      Check.int
      ~error_msg:
        "Non-banned attesters don't have enough rights to reconstruct slots, \
         (needed: %L, actual: %R)" ;
    (* Check that no non-banned attester has enough
       assigned shards to attest the slot alone. *)
    List.iter2
      (fun assigned_shard_indices attester ->
        Check.(
          attestation_threshold * number_of_shards / 100
          > List.length assigned_shard_indices)
          ~__LOC__
          Check.int
          ~error_msg:
            ("Attester " ^ attester.name
           ^ " has enough right to attest alone, (needed: %L shards, actual: \
              %R)"))
      assigned_shard_indices_non_banned
      non_banned_attesters ;
    (* Check that no initial attester has enough
       assigned shards to reconstruct. *)
    List.iter2
      (fun assigned_shard_indices attester ->
        Check.(
          number_of_shards / redundancy_factor
          > List.length assigned_shard_indices)
          ~__LOC__
          Check.int
          ~error_msg:
            ("Attester " ^ attester.name
           ^ " has enough rights to reconstruct alone, (needed: %L, actual: %R)"
            ))
      assigned_shard_indices_non_banned
      non_banned_attesters ;

    let wait_for_shards ~dal_node ~shards ~published_level ~slot_index =
      let* () =
        wait_for_shards_promises ~dal_node ~shards ~published_level ~slot_index
      in
      let () =
        Log.debug "Dal node %s has received its shards" (Dal_node.name dal_node)
      in
      unit
    in
    info "Waiting for shards" ;

    let wait_shards_reach_attesters ~published_level ~slot_index attesters
        assigned_shard_indices message =
      let* () =
        Lwt.join
        @@ List.map2
             (fun {dal_node; _} shards ->
               wait_for_shards ~dal_node ~shards ~published_level ~slot_index)
             attesters
             assigned_shard_indices
      in
      info message ;
      unit
    in

    let wait_shards_reach_observer ~published_level ~slot_index =
      let* () =
        Lwt.join
        @@ List.map2
             (fun attester shards ->
               let* () =
                 wait_for_shards
                   ~dal_node:observer
                   ~shards
                   ~published_level
                   ~slot_index
               in
               let () =
                 Log.info
                   "Dal node %s has sent its shards to observer"
                   attester.name
               in
               unit)
             non_banned_attesters
             assigned_shard_indices_non_banned
      in
      info "Non-banned attesters have transmitted their shards to the observer" ;
      unit
    in

    (* Wait until everyone has received the needed shards (first the
       non-banned attesters, then the observer, and finally the banned
       attesters). *)
    let wait_slot ~published_level ~slot_index =
      let wait_non_banned_promise =
        wait_shards_reach_attesters
          ~published_level
          ~slot_index
          non_banned_attesters
          assigned_shard_indices_non_banned
          "Non-banned attesters have received their shards"
      in

      let wait_banned_promise =
        wait_shards_reach_attesters
          ~published_level
          ~slot_index
          banned_attesters
          assigned_shard_indices_banned
          "Banned attesters have received their shards"
      in

      let wait_observer_promise =
        wait_shards_reach_observer ~published_level ~slot_index
      in

      let* () = wait_non_banned_promise in
      let* () = wait_observer_promise in

      wait_banned_promise
    in
    let* publication_level_bis, _commitment, () =
      publish_store_and_wait_slot
        node
        client
        slot_producer
        Constant.bootstrap1
        ~index:slot_index
        ~wait_slot
        ~number_of_extra_blocks_to_bake:2
      @@ Helpers.make_slot ~slot_size "content"
    in
    Check.(publication_level_bis = publication_level)
      ~__LOC__
      Check.int
      ~error_msg:
        "Commitment published at unexpected level: expected: %R, actual: %L" ;
    unit

  (* A simpler test in which a slot producer is directly connected to
     an observer node and sends all shards. *)
  let test_amplification_without_lost_shards _protocol dal_parameters _cryptobox
      node client dal_bootstrap =
    (* In this test we have three DAL nodes:
       - a bootstrap one to connect the other two (producer and observer),
       - a slot producer on slot 0,
       - an observer on slot 0.

       We check that when the slot producer publishes a slot, the
       observer performs an amplification. *)
    let index = 0 in
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let number_of_slots = dal_parameters.number_of_slots in
    let peers = [Dal_node.listen_addr dal_bootstrap] in
    let peer_id dal_node = Dal_node.read_identity dal_node in

    (* Check that the dal node passed as argument to this test function
       is a running bootstrap DAL node. If not, this means that we
       forgot to register the test with ~bootstrap_profile:true *)
    let* () =
      check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap
    in
    Log.info "Bootstrap DAL node is running" ;

    let producer = Dal_node.create ~name:"producer" ~node () in
    let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
    let* () = Dal_node.run ~wait_ready:true producer in
    let* producer_peer_id = peer_id producer in
    let* () =
      check_profiles
        ~__LOC__
        producer
        ~expected:Dal_RPC.(Controller [Operator index])
    in
    Log.info "Slot producer DAL node is running" ;

    let observer = Dal_node.create ~name:"observer" ~node () in
    let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
    let* () = Dal_node.run ~wait_ready:true observer in
    let* observer_peer_id = peer_id observer in
    let* () =
      check_profiles
        ~__LOC__
        observer
        ~expected:Dal_RPC.(Controller [Observer index])
    in
    Log.info "Observer DAL node is running" ;

    let all_pkhs =
      Account.Bootstrap.keys |> Array.to_list
      |> List.map (fun account -> account.Account.public_key_hash)
    in

    let check_graft_promises =
      List.map
        (fun pkh ->
          Lwt.pick
          @@ check_grafts
               ~number_of_slots
               ~slot_index:index
               (observer, observer_peer_id)
               (producer, producer_peer_id)
               pkh)
        all_pkhs
    in
    Log.info "Waiting for grafting of the observer - producer connection" ;

    (* We need to bake some blocks until the L1 node notifies the DAL
       nodes that some L1 block is final so that the topic pkhs are
       known. *)
    let* () = bake_for ~count:3 client in
    let* () = Lwt.join check_graft_promises in
    Log.info "Observer - producer connection grafted" ;

    (* Produce and publish a slot *)
    let source = Constant.bootstrap1 in
    let slot_content = "content" in
    let* before_publication_level = Client.level client in
    let publication_level = 1 + before_publication_level in
    let wait_slot ~published_level:_ ~slot_index:_ =
      Log.info "Waiting for first reconstruction event" ;
      let* () =
        Dal_node.wait_for
          observer
          "dal_reconstruct_starting_in.v0"
          (fun event ->
            if
              JSON.(
                event |-> "level" |> as_int = publication_level
                && event |-> "slot_index" |> as_int = index)
            then Some ()
            else None)
      in
      Log.info
        "Waiting for reconstruction to be canceled because all shards were \
         received, fail if the reconstruction finishes" ;
      let promise_reconstruction_cancelled =
        Dal_node.wait_for
          observer
          "dal_reconstruct_no_missing_shard.v0"
          (fun event ->
            if
              JSON.(
                event |-> "level" |> as_int = publication_level
                && event |-> "slot_index" |> as_int = index)
            then Some true
            else None)
      in
      let promise_reconstruction_finished =
        Dal_node.wait_for observer "dal_reconstruct_finished.v0" (fun event ->
            if
              JSON.(
                event |-> "level" |> as_int = publication_level
                && event |-> "slot_index" |> as_int = index)
            then Some false
            else None)
      in
      let* success =
        Lwt.pick
          [promise_reconstruction_cancelled; promise_reconstruction_finished]
      in
      if success then unit
      else Test.fail ~__LOC__ "Reconstruction was not cancelled."
    in
    let* publication_level_bis, _commitment, () =
      publish_store_and_wait_slot
        node
        client
        producer
        source
        ~index
        ~wait_slot
        ~number_of_extra_blocks_to_bake:2
      @@ Helpers.make_slot ~slot_size slot_content
    in
    Check.(publication_level_bis = publication_level)
      ~__LOC__
      Check.int
      ~error_msg:"Unexpected publication level, actual:%L, expected:%R" ;

    unit

  let check_slot_attested node ~number_of_slots ~attested_level
      ~expected_attestation =
    let* metadata =
      Node.RPC.call node
      @@ RPC.get_chain_block_metadata ~block:(string_of_int attested_level) ()
    in

    let attestation =
      match metadata.dal_attestation with
      | None ->
          Test.fail
            "Missing dal_attestation field in the metadata of the block at \
             level %d"
            attested_level
      | Some v ->
          (* [v]'s length may be smaller than [number_of_slots]; we fill it with [false] *)
          List.init number_of_slots (fun i ->
              if i < Array.length v then v.(i) else false)
    in
    Check.(
      (attestation = expected_attestation)
        (list bool)
        ~error_msg:"Expected %R, got %L") ;
    unit

  let test_by_ignoring_topics _protocol dal_parameters _cryptobox node client
      dal_bootstrap =
    let peer_id dal_node = Dal_node.read_identity dal_node in

    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let number_of_slots = dal_parameters.number_of_slots in
    let attestation_lag = dal_parameters.attestation_lag in

    let index = 0 in
    let peers = [Dal_node.listen_addr dal_bootstrap] in

    let producer =
      Dal_node.create
        ~name:"producer"
        ~node
        ~ignore_pkhs:
          [
            Constant.bootstrap1.Account.public_key_hash;
            Constant.bootstrap2.Account.public_key_hash;
            Constant.bootstrap3.Account.public_key_hash;
          ]
        ()
    in
    let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
    let* () =
      let env =
        String_map.singleton Dal_node.ignore_topics_environment_variable "yes"
      in
      Dal_node.run ~env ~wait_ready:true producer
    in
    let* producer_peer_id = peer_id producer in

    let observer = Dal_node.create ~name:"observer" ~node () in
    let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
    let* () = Dal_node.run ~wait_ready:true observer in
    let* observer_peer_id = peer_id observer in

    let all_pkhs =
      Account.Bootstrap.keys |> Array.to_list
      |> List.map (fun account -> account.Account.public_key_hash)
    in

    let attester = Dal_node.create ~name:"attester" ~node () in
    let* () =
      Dal_node.init_config ~attester_profiles:all_pkhs ~peers attester
    in
    let* () = Dal_node.run ~wait_ready:true attester in
    let* attester_peer_id = peer_id attester in

    Log.info
      "Waiting for grafting of the observer - producer connection, and \
       observer - attester connection" ;
    let check_producer_observer_grafts pkh =
      Lwt.pick
      @@ check_grafts
           ~number_of_slots
           ~slot_index:index
           (observer, observer_peer_id)
           (producer, producer_peer_id)
           pkh
    in
    let check_observer_attester_grafts pkh =
      Lwt.pick
      @@ check_grafts
           ~number_of_slots
           ~slot_index:index
           (observer, observer_peer_id)
           (attester, attester_peer_id)
           pkh
    in
    let check_graft_promises =
      List.map check_producer_observer_grafts all_pkhs
      @ List.map check_observer_attester_grafts all_pkhs
    in

    (* We need to bake some blocks until the L1 node notifies the DAL
       nodes that some L1 block is final so that the topic pkhs are
       known. *)
    let* () = bake_for ~count:3 client in
    let* () = Lwt.join check_graft_promises in
    Log.info "Connections grafted" ;

    let* before_publication_level = Client.level client in
    let published_slots = 3 in
    let published_levels =
      List.init published_slots (fun offset ->
          before_publication_level + offset + 1)
    in
    Log.info
      "Produce and publish %d slots at levels %a."
      published_slots
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ",")
         Format.pp_print_int)
      published_levels ;
    let source = Constant.bootstrap1 in
    (* Build the [wait_for] promises. *)
    let wait_reconstruction ~published_level ~slot_index =
      Dal_node.wait_for observer "dal_reconstruct_finished.v0" (fun event ->
          if
            JSON.(
              event |-> "level" |> as_int = published_level
              && event |-> "slot_index" |> as_int = slot_index)
          then (
            Log.info
              "Finished reconstruction for slot at level %d"
              published_level ;
            Some ())
          else None)
    in
    let wait_for_promises =
      List.map
        (fun published_level ->
          wait_reconstruction ~published_level ~slot_index:index)
        published_levels
    in
    let rec repeat_publish offset =
      if offset > published_slots then unit
      else (
        Log.info
          "Publish a slot at level %d"
          (before_publication_level + offset + 1) ;
        let* (`OpHash op_hash) =
          let content =
            Helpers.make_slot ~slot_size ("slot " ^ string_of_int offset)
          in
          let* commitment, proof =
            Helpers.store_slot producer ~slot_index:index content
          in
          (* TODO: we should not need to set a fee! *)
          publish_commitment
            ~source
            ~index
            ~commitment
            ~proof
            client
            ~fee:20_000
        in
        (* Bake a block to include the operation. *)
        let* () = bake_for client in
        (* Check that the operation is included. *)
        let* included_manager_operations =
          let manager_operation_pass = 3 in
          Node.RPC.(
            call node
            @@ get_chain_block_operation_hashes_of_validation_pass
                 manager_operation_pass)
        in
        let () =
          Check.list_mem
            Check.string
            ~__LOC__
            op_hash
            included_manager_operations
            ~error_msg:
              "DAL commitment publishment operation not found in head block."
        in
        repeat_publish (offset + 1))
    in
    let* () = repeat_publish 1 in
    (* We bake two blocks so that the last publish operation becomes final, and
       therefore all reconstructions have started. *)
    let* () = bake_for client ~count:2 in
    Log.info "Waiting for finished reconstruction events" ;
    let* () = Lwt.join wait_for_promises in

    Log.info
      "Bake [attestation_lag] blocks and check that the slots are attested" ;
    let* () =
      let dal_node_endpoint =
        Dal_node.as_rpc_endpoint attester |> Endpoint.as_string
      in
      (* Using [bake_for ~count:attestation_lag], make the test fail, because
         "unable to get DAL attestation for <baker> in time". To be on the safe
         side, we wait a bit before baking the next block. *)
      repeat attestation_lag (fun () ->
          let* () = Lwt_unix.sleep 0.1 in
          bake_for client ~dal_node_endpoint)
    in

    let expected_attestation =
      assert (index = 0) ;
      List.init number_of_slots (fun i -> i = index)
    in
    let rec check_attestation offset =
      if offset > published_slots then unit
      else
        let attested_level =
          before_publication_level + attestation_lag + offset
        in
        let* () =
          check_slot_attested
            node
            ~number_of_slots
            ~attested_level
            ~expected_attestation
        in
        check_attestation (offset + 1)
    in
    check_attestation 1
end

module Garbage_collection = struct
  (* Test the GC feature: once a period (set by history_mode) is passed after
     receiving the shards, each node deletes them from its storage. *)

  let wait_remove_shards ~published_level ~slot_index node =
    Dal_node.wait_for node "dal_removed_slot_shards.v0" (fun event ->
        if
          (published_level = JSON.(event |-> "published_level" |> as_int))
          && slot_index = JSON.(event |-> "slot_index" |> as_int)
        then Some ()
        else None)

  let wait_for_first_shard ~published_level ~slot_index node =
    Dal_node.wait_for node "dal_stored_slot_shard.v0" (fun event ->
        let actual_published_level =
          JSON.(event |-> "published_level" |> as_int)
        in
        let actual_slot_index = JSON.(event |-> "slot_index" |> as_int) in
        if published_level = actual_published_level then
          if slot_index = actual_slot_index then
            let shard_index = JSON.(event |-> "shard_index" |> as_int) in
            Some shard_index
          else (
            Log.info
              "bad slot index : received : %d, expected : %d"
              actual_slot_index
              slot_index ;
            None)
        else (
          Log.info
            "bad slot level : received : %d, expected : %d"
            actual_published_level
            published_level ;
          None))

  let get_shard_rpc ~slot_level ~slot_index ~shard_index node =
    Dal_RPC.(
      call node
      @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index)

  let get_shard_rpc_failure_expected ~slot_level ~slot_index node =
    let* shard_observer_response =
      Dal_RPC.(
        call_raw node
        @@ get_level_slot_shard_content ~slot_level ~slot_index ~shard_index:0)
    in
    Check.(shard_observer_response.code = 404)
      ~__LOC__
      Check.int
      ~error_msg:"Unexpected RPC response, expected: %R, actual: %L" ;
    unit

  (* This simple test checks the basic feature, with just a producer that
     produces shards & deletes them *)
  let test_gc_simple_producer _protocol dal_parameters _cryptobox node client
      slot_producer =
    let slot_index = 0 in
    (* Parameters and constants. *)
    let Dal.Parameters.{cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
      dal_parameters
    in
    Log.info "Producer DAL node is running" ;

    let* current_level = Client.level client in
    let wait_for_producer =
      wait_for_layer1_final_block slot_producer (current_level + 1)
    in
    let* published_level, _commitment, () =
      publish_store_and_wait_slot
        node
        client
        slot_producer
        Constant.bootstrap1
        ~index:slot_index
        ~wait_slot:(fun ~published_level:_ ~slot_index:_ -> unit)
        ~number_of_extra_blocks_to_bake:2
      @@ Helpers.make_slot ~slot_size "content"
    in
    let* () = wait_for_producer in

    Log.info "RPC to producer for first shard" ;
    (* Check the producer stored the first shard *)
    let* _first_shard =
      Dal_RPC.(
        call slot_producer
        @@ get_level_slot_shard_content
             ~slot_level:published_level
             ~slot_index
             ~shard_index:0)
    in

    let wait_remove_shards_promise =
      Log.info "Waiting for the shards of the commitment to be removed" ;
      wait_remove_shards ~published_level ~slot_index slot_producer
    in

    Log.info "Every shard published for the commitment; baking blocks" ;
    let* () = bake_for ~count:dal_parameters.attestation_lag client in
    (* The slot wasn't attested (because there is no attester running
       in this scenario) so it got removed despite not being older
       than the history length of the slot producer DAL node. *)
    Log.info "Blocks baked." ;

    Log.info "Wait for the shards of the commitment to be removed" ;
    let* () = wait_remove_shards_promise in

    Log.info "RPC deleted shard producer" ;
    let* () =
      get_shard_rpc_failure_expected
        ~slot_level:published_level
        ~slot_index
        slot_producer
    in

    Log.info "End of test" ;
    unit

  (* This simple test checks the basic feature, with just a producer that
     produces shards & deletes them and an attester with attests them. *)
  let test_gc_producer_and_attester _protocol dal_parameters _cryptobox node
      client dal_bootstrap =
    let slot_index = 0 in
    (* Parameters and constants. *)
    let Dal.Parameters.
          {number_of_slots; cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
      dal_parameters
    in

    (* Note: we don't need to configure and start the DAL bootstrap
       node because it is the node started by the
       scenario_with_layer1_and_dal_nodes wrapper. Instead, we check
       that the dal node passed as argument to this test function is a
       running bootstrap DAL node. If not, this means that we forgot
       to register the test with ~bootstrap_profile:true *)
    let* () =
      check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap
    in
    Log.info "bootstrap DAL node is running" ;

    (* When configuring the other DAL nodes, we use dal_bootstrap as
       the single peer. *)
    let peers = [Dal_node.listen_addr dal_bootstrap] in

    (* Create & configure attester *)
    let attester = Dal_node.create ~name:"attester" ~node () in
    let client = Client.with_dal_node client ~dal_node:attester in

    let bootstrap_pkhs =
      Array.to_seq Account.Bootstrap.keys
      |> Seq.map (fun account -> account.Account.public_key_hash)
      |> List.of_seq
    in
    Dal_node.log_events attester ;
    let* () =
      Dal_node.init_config ~attester_profiles:bootstrap_pkhs ~peers attester
    in
    let* () = Dal_node.run ~wait_ready:true attester in
    Log.info "attester DAL node ready" ;

    let slot_producer = Dal_node.create ~name:"producer" ~node () in
    Dal_node.log_events slot_producer ;
    let* () =
      Dal_node.init_config ~operator_profiles:[slot_index] ~peers slot_producer
    in
    (* Promise which will be resolved once the slot producer will be
       connected to all the other DAL nodes (attester + bootstrap). *)
    let wait_for_connections_of_producer_promise =
      Dal_node.wait_for_connections slot_producer 2
    in
    let* () = Dal_node.run ~wait_ready:true slot_producer in

    (* Check that the DAL nodes have the expected profiles. *)
    let* () =
      check_profiles
        ~__LOC__
        slot_producer
        ~expected:Dal_RPC.(Controller [Operator slot_index])
    in
    Log.info "Slot producer DAL node is running" ;

    let* () =
      check_profiles
        ~__LOC__
        attester
        ~expected:
          (Dal_RPC.Controller
             (List.map (fun pkh -> Dal_RPC.Attester pkh) bootstrap_pkhs))
    in

    Log.info "Attester DAL node is running" ;

    (* Now that all the DAL nodes are running, we need some of them to
       establish grafted connections. *)
    (* Wait for a GRAFT message between all nodes. *)
    let check_graft node1 node2 attester_pkh =
      let* id1 = Dal_node.read_identity node1 in
      let* id2 = Dal_node.read_identity node2 in
      Lwt.pick
      @@ check_grafts
           ~number_of_slots
           ~slot_index
           (node1, id1)
           (node2, id2)
           attester_pkh
    in
    let check_graft_promises =
      List.map (check_graft slot_producer attester) bootstrap_pkhs
    in
    Log.info "Waiting for grafting of the DAL nodes connections" ;
    (* The attester DAL node won't join the DAL network until it has
       processed some finalized L1 block in which the associated baker
       is reported to have some rights. For this to happen, we need to
       bake a few blocks. *)
    let* () = bake_for ~count:3 client in
    let* () = Lwt.join check_graft_promises in
    Log.info "Attester - producer connections grafted" ;

    let* () = wait_for_connections_of_producer_promise in

    Log.info "DAL network is ready" ;

    let wait_slot ~published_level ~slot_index =
      (* We don’t wait for the producer because at this step it already stored
         its shards *)
      Log.info "Waiting for first shard to be stored by the attester" ;
      wait_for_first_shard ~published_level ~slot_index attester
    in

    let* current_level = Client.level client in
    let wait_for_producer =
      wait_for_layer1_final_block slot_producer (current_level + 1)
    in
    let wait_for_attester =
      wait_for_layer1_final_block attester (current_level + 1)
    in
    let* published_level, _commitment, shard_index_attester =
      publish_store_and_wait_slot
        node
        client
        slot_producer
        Constant.bootstrap1
        ~index:slot_index
        ~wait_slot
        ~number_of_extra_blocks_to_bake:2
      @@ Helpers.make_slot ~slot_size "content"
    in
    let* () = wait_for_producer in
    let* () = wait_for_attester in

    Log.info "RPC first shard producer" ;
    let* _shard_producer =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:0
        slot_producer
    in
    Log.info "RPC first shard attester" ;
    let* _shard_attester =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:shard_index_attester
        attester
    in

    let wait_remove_shards_attester_promise =
      Log.info "Waiting for first shard to be removed by the attester" ;
      wait_remove_shards ~published_level ~slot_index attester
    in

    Log.info "All nodes received a shard, waiting for blocks to be baked" ;
    let rec bake_loop n =
      if n <= 0 then unit
      else
        let* level = Client.level client in
        let wait_block_p =
          List.map
            (fun dal_node -> wait_for_layer1_head dal_node (level + 1))
            [attester; dal_bootstrap; slot_producer]
        in
        let* () = bake_for client in
        let* () = Lwt.join wait_block_p in
        bake_loop (n - 1)
    in
    let* () = bake_loop 180 in
    Log.info "Blocks baked !" ;

    Log.info "Checking that the slot was attested" ;
    let* () =
      let* status =
        Dal_RPC.(
          call slot_producer
          @@ get_level_slot_status ~slot_level:published_level ~slot_index)
      in
      Check.(status = "attested")
        ~__LOC__
        Check.string
        ~error_msg:
          "The value of the fetched status should match the expected one \
           (current = %L, expected = %R)" ;
      unit
    in

    Log.info "Wait for first shard attester" ;
    let* () = wait_remove_shards_attester_promise in

    Log.info "RPC deleted shard attester" ;
    let* () =
      get_shard_rpc_failure_expected
        ~slot_level:published_level
        ~slot_index
        attester
    in

    Log.info "RPC shard still stored producer" ;
    let* _shard_producer =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:0
        slot_producer
    in

    Log.info "End of test" ;
    unit

  (* For this test, we create a network of three DAL nodes — 1 slot producer,
     1 attester, 1 observer (so we can check for each profile). History mode is
     [Auto].
     The slot producer will send shards from one slot; once a node receives it,
     a request is sent for a received shard, to make sure for reception. After
     25 blocks baked, we check via RPC that attester deleted its shards and the
     others did not.
  *)
  let test_gc_with_all_profiles _protocol dal_parameters _cryptobox node client
      dal_bootstrap =
    let slot_index = 0 in
    (* Parameters and constants. *)
    let Dal.Parameters.
          {number_of_slots; cryptobox = {Dal.Cryptobox.slot_size; _}; _} =
      dal_parameters
    in

    (* Note: we don't need to configure and start the DAL bootstrap
       node because it is the node started by the
       scenario_with_layer1_and_dal_nodes wrapper. Instead, we check
       that the dal node passed as argument to this test function is a
       running bootstrap DAL node. If not, this means that we forgot
       to register the test with ~bootstrap_profile:true *)
    let* () =
      check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap
    in
    Log.info "bootstrap DAL node is running" ;

    (* When configuring the other DAL nodes, we use dal_bootstrap as
       the single peer. *)
    let peers = [Dal_node.listen_addr dal_bootstrap] in
    let bootstrap_pkhs =
      Array.to_seq Account.Bootstrap.keys
      |> Seq.map (fun account -> account.Account.public_key_hash)
      |> List.of_seq
    in

    (* Create & configure attester *)
    let attester = Dal_node.create ~name:"attester" ~node () in
    let* () =
      Dal_node.init_config ~attester_profiles:bootstrap_pkhs ~peers attester
    in
    let* () = Dal_node.run ~wait_ready:true attester in
    let client = Client.with_dal_node client ~dal_node:attester in

    Log.info "attester DAL node ready" ;

    let slot_producer = Dal_node.create ~name:"producer" ~node () in
    let* () =
      Dal_node.init_config ~operator_profiles:[slot_index] ~peers slot_producer
    in
    (* Promise which will be resolved once the slot producer will be
       connected to all the other DAL nodes (attester + observer
       + bootstrap). *)
    let wait_for_connections_of_producer_promise =
      Dal_node.wait_for_connections slot_producer 3
    in
    let* () = Dal_node.run ~wait_ready:true slot_producer in

    (* Create & configure observer *)
    let observer = Dal_node.create ~name:"observer" ~node () in
    let* () =
      Dal_node.init_config ~observer_profiles:[slot_index] ~peers observer
    in
    let* () = Dal_node.run ~wait_ready:true observer in

    (* Check that the DAL nodes have the expected profiles. *)
    let* () =
      check_profiles
        ~__LOC__
        slot_producer
        ~expected:Dal_RPC.(Controller [Operator slot_index])
    in
    Log.info "Slot producer DAL node is running" ;

    let* () =
      check_profiles
        ~__LOC__
        observer
        ~expected:Dal_RPC.(Controller [Observer slot_index])
    in
    Log.info "Observer DAL node is running" ;

    let* () =
      check_profiles
        ~__LOC__
        attester
        ~expected:
          (Dal_RPC.Controller
             (List.map (fun pkh -> Dal_RPC.Attester pkh) bootstrap_pkhs))
    in
    Log.info "Attester DAL node is running" ;

    (* Now that all the DAL nodes are running, we need some of them to
       establish grafted connections. The connections between the
       attester and the slot producer have no reason to be grafted on
       other slot indices than the one the slot producer is subscribed
       to, so we instruct [check_events_with_topic] to skip all events
       but the one for [slot_index]. *)
    let already_seen_slots =
      Array.init number_of_slots (fun index -> slot_index <> index)
    in
    (* Wait for a GRAFT message between all nodes. *)
    let check_graft node1 node2 attester_pkh =
      let check_graft ~from:node1 node2 =
        let* node1_id = Dal_node.read_identity node1 in
        check_events_with_topic
          ~event_with_topic:(Graft node1_id)
          node2
          ~num_slots:number_of_slots
          ~already_seen_slots
          attester_pkh
      in
      Lwt.pick [check_graft ~from:node1 node2; check_graft ~from:node2 node1]
    in
    let check_graft_promises =
      List.map (check_graft slot_producer attester) bootstrap_pkhs
      @ List.map (check_graft observer attester) bootstrap_pkhs
      @ List.map (check_graft slot_producer observer) bootstrap_pkhs
    in
    Log.info "Waiting for grafting of the DAL nodes connections" ;
    (* The attester DAL node won't join the DAL network until it has
       processed some finalized L1 block in which the associated baker
       is reported to have some rights. For this to happen, we need to
       bake a few blocks. *)
    let* () = bake_for ~count:3 client in
    let* () = Lwt.join check_graft_promises in
    Log.info "Attester - producer connections grafted" ;

    let* () = wait_for_connections_of_producer_promise in

    Log.info "DAL network is ready" ;

    let wait_slot ~published_level ~slot_index =
      (* We don’t wait for the producer because at this step it already stored
         its shards *)
      let wait_for_first_shard_observer_promise =
        Log.info "Waiting for first shard to be stored by the observer" ;
        wait_for_first_shard ~published_level ~slot_index observer
      in
      let wait_for_first_shard_attester_promise =
        Log.info "Waiting for first shard to be stored by the attester" ;
        wait_for_first_shard ~published_level ~slot_index attester
      in
      let* shard_index_observer = wait_for_first_shard_observer_promise in
      let* shard_index_attester = wait_for_first_shard_attester_promise in
      return (shard_index_observer, shard_index_attester)
    in

    let* current_level = Client.level client in
    let wait_for_producer =
      wait_for_layer1_final_block slot_producer (current_level + 1)
    in
    let wait_for_observer =
      wait_for_layer1_final_block observer (current_level + 1)
    in
    let wait_for_attester =
      wait_for_layer1_final_block attester (current_level + 1)
    in
    let* ( published_level,
           _commitment,
           (shard_index_observer, shard_index_attester) ) =
      publish_store_and_wait_slot
        node
        client
        slot_producer
        Constant.bootstrap1
        ~index:slot_index
        ~wait_slot
        ~number_of_extra_blocks_to_bake:2
      @@ Helpers.make_slot ~slot_size "content"
    in
    let* () = wait_for_producer in
    let* () = wait_for_observer in
    let* () = wait_for_attester in

    Log.info "RPC first shard observer" ;
    let* _shard_observer =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:shard_index_observer
        observer
    in
    Log.info "RPC first shard producer" ;
    let* _shard_producer =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:0
        slot_producer
    in
    Log.info "RPC first shard attester" ;
    let* _shard_attester =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:shard_index_attester
        attester
    in

    let wait_remove_shards_attester_promise =
      Log.info "Waiting for first shard to be removed by the attester" ;
      wait_remove_shards ~published_level ~slot_index attester
    in

    Log.info "All nodes received a shard, waiting for blocks to be baked" ;
    let rec bake_loop n =
      if n <= 0 then unit
      else
        let* level = Client.level client in
        let wait_block_p =
          List.map
            (fun dal_node -> wait_for_layer1_head dal_node (level + 1))
            [attester; observer; dal_bootstrap; slot_producer]
        in
        let* () = bake_for client in
        let* () = Lwt.join wait_block_p in
        bake_loop (n - 1)
    in
    let* () = bake_loop 180 in
    Log.info "Blocks baked !" ;

    Log.info "Checking that the slot was attested" ;
    let* () =
      let* status =
        Dal_RPC.(
          call slot_producer
          @@ get_level_slot_status ~slot_level:published_level ~slot_index)
      in
      Check.(status = "attested")
        ~__LOC__
        Check.string
        ~error_msg:
          "The value of the fetched status should match the expected one \
           (current = %L, expected = %R)" ;
      unit
    in

    Log.info "Wait for first shard attester" ;
    let* () = wait_remove_shards_attester_promise in

    Log.info "RPC deleted shard attester" ;
    let* () =
      get_shard_rpc_failure_expected
        ~slot_level:published_level
        ~slot_index
        attester
    in

    Log.info "RPC deleted shard observer" ;
    let* _shard_observer =
      get_shard_rpc_failure_expected
        ~slot_level:published_level
        ~slot_index
        observer
    in
    Log.info "RPC shard still stored producer" ;
    let* _shard_producer =
      get_shard_rpc
        ~slot_level:published_level
        ~slot_index
        ~shard_index:0
        slot_producer
    in

    Log.info "End of test" ;
    unit

  let test_gc_skip_list_cells ~protocols =
    let title = "garbage collection of skip list cells" in
    let tags = Tag.[tezos2; "dal"; "gc"; "skip_list"] in
    Protocol.register_test
      ~__FILE__
      ~tags
      ~uses:(fun _protocol -> [Constant.octez_dal_node])
      ~title
      ~supports:(Protocol.From_protocol 19)
      (fun protocol ->
        (* We choose some arbitrary, small values *)
        let a = 1 in
        let b = 2 in
        let c = 2 in
        (* The period in blocks for which cells are kept. *)
        let non_gc_period = 2 * (a + b + c) in
        Log.info "The \"non-GC period\" is %d" non_gc_period ;
        let parameter_overrides =
          make_int_parameter
            ["smart_rollup_commitment_period_in_blocks"]
            (Some a)
          @ make_int_parameter
              ["smart_rollup_challenge_window_in_blocks"]
              (Some b)
          @ make_int_parameter
              [
                "smart_rollup_reveal_activation_level";
                "dal_attested_slots_validity_lag";
              ]
              (Some c)
        in
        let* node, client, dal_parameters =
          setup_node
            ~parameter_overrides
            ~protocol
            ~l1_history_mode:Default_with_refutation
            ()
        in
        let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
        let lag = dal_parameters.attestation_lag in
        let dal_node = Dal_node.create ~node () in
        let* () = Dal_node.init_config ~operator_profiles:[1] dal_node in
        let* () = Dal_node.run dal_node ~wait_ready:true in
        Log.info
          "The first level with stored cells is 1 + lag = %d. We bake till \
           that level is final, that is until level %d."
          (lag + 1)
          (lag + 3) ;
        let wait_for_dal_node =
          wait_for_layer1_final_block dal_node (lag + 1)
        in
        let* current_level = Client.level client in
        assert (current_level = 1) ;
        let* () = bake_for client ~count:(lag + 2) in
        let* () = wait_for_dal_node in
        Log.info
          "Check that the skip list store contains the right files for level \
           lag + 1." ;
        let* () =
          check_skip_list_store
            dal_node
            ~number_of_slots
            ~expected_levels:[string_of_int (lag + 1)]
        in
        (* We just want to observe the GC taking effect, so we need to bake at
           least [non_gc_period] blocks. *)
        Log.info "We bake %d more blocks" non_gc_period ;
        let last_final_level = lag + 1 + non_gc_period in
        let wait_for_dal_node =
          wait_for_layer1_final_block dal_node last_final_level
        in
        let* () = bake_for client ~count:non_gc_period in
        let* () = wait_for_dal_node in
        Log.info
          "Check that the skip list store contains cells for [non_gc_period] \
           levels." ;
        (* Example: say head level = 21. The node GCs the cells at level 19 - 10
           = 9, and it injects at level 19. So we have cells for level 10 to
           19, that is, 10 levels. *)
        let expected_levels =
          (* The first level with stored cells is [last_final_level -
             non_gc_period + 1 = lag + 2]. *)
          let offset = lag + 2 in
          List.init non_gc_period (fun i -> string_of_int (offset + i))
        in
        check_skip_list_store dal_node ~number_of_slots ~expected_levels)
      protocols
end

module Tx_kernel_e2e = struct
  open Tezos_protocol_alpha.Protocol
  open Tezt_tx_kernel

  (** [keys_of_account account] returns a triplet of pk, pkh, sk of [account]  *)
  let keys_of_account (account : Account.key) =
    let pk =
      account.public_key
      |> Tezos_crypto.Signature.Ed25519.Public_key.of_b58check_exn
    in
    let pkh =
      account.public_key_hash
      |> Tezos_crypto.Signature.Ed25519.Public_key_hash.of_b58check_exn
    in
    let sk =
      account.secret_key
      |> Account.require_unencrypted_secret_key ~__LOC__
      |> Tezos_crypto.Signature.Ed25519.Secret_key.of_b58check_exn
    in
    (pk, pkh, sk)

  (** [bake_until cond client sc_rollup_node] bakes until [cond client] retuns [true].
      After baking each block we wait for the [sc_rollup_node] to catch up to L1. *)
  let rec bake_until cond client sc_rollup_node =
    let* stop = cond client in
    if stop then unit
    else
      let* () = bake_for client in
      let* current_level = Client.level client in
      let* _ =
        Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
      in
      bake_until cond client sc_rollup_node

  (** [get_ticket_balance  ~pvm_name ~pkh ~ticket_index] returns
      the L2 balance of the account with [pkh] for the ticket with [ticket_index] *)
  let get_ticket_balance sc_rollup_node ~pvm_name ~pkh ~ticket_index =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:pvm_name
         ~operation:Sc_rollup_rpc.Value
         ~key:
           (sf
              "/accounts/%s/%d"
              (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh)
              ticket_index)
         ()

  (** E2E test using the tx-kernel. Scenario:
      1. Deposit [450] tickets to the L2 [pk1] using the deposit contract.
      2. Construct two batches of L2 transactions to achieve the following:
          1. Transfer [60] tickets from [pk1] to [pk2].
          2. Withdraw [60] tickets from [pk2].
          3. Transfer [90] tickets from [pk1] to [pk2].
          3. Withdraw [40] tickets from [pk2].
      3. Publish the transaction batch to DAL at slot [0].
      4. Bake [attestation_lag] blocks and attest slot [0].
      5. Check that the L2 [pk1] has [300] tickets and [pk2] has [50] tickets.
   *)
  let test_tx_kernel_e2e protocol parameters dal_node sc_rollup_node
      _sc_rollup_address node client pvm_name =
    Log.info "Originate the tx kernel." ;
    let* {boot_sector; _} =
      Sc_rollup_helpers.prepare_installer_kernel
        ~preimages_dir:
          (Filename.concat
             (Sc_rollup_node.data_dir sc_rollup_node)
             "wasm_2_0_0")
        Constant.WASM.tx_kernel_dal
    in
    (* The kernel is badly written and may ask pages in negative
       levels. We ensure it is not possible by baking enough
       blocks. *)
    let* () =
      bake_for ~count:parameters.Dal.Parameters.attestation_lag client
    in
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"tx_kernel_dal"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:"wasm_2_0_0"
        ~boot_sector
        ~parameters_ty:"pair string (ticket string)"
        client
    in
    let* () = bake_for client in
    Log.info "Run the rollup node and ensure origination succeeds." ;
    let* genesis_info =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_genesis_info
           sc_rollup_address
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in
    let* () =
      Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
    in
    let* level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node init_level
    in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;
    Log.info "Create key pairs used for the test." ;
    let pk1, pkh1, sk1 = keys_of_account Constant.bootstrap1 in
    let pk2, pkh2, sk2 = keys_of_account Constant.bootstrap2 in
    Log.info "Prepare contract for minting and depositing." ;
    let* mint_and_deposit_contract =
      Contracts.prepare_mint_and_deposit_contract client protocol
    in
    Log.info "Deposit [450] tickets to the L2 [pk1] using deposit contract." ;
    let ticket_content = "Hello, Ticket!" in
    let* () =
      Contracts.deposit_string_tickets
        client
        ~mint_and_deposit_contract:
          (Contract_hash.to_b58check mint_and_deposit_contract)
        ~sc_rollup_address
        ~destination_l2_addr:
          (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh1)
        ~ticket_content
        ~amount:450
    in
    Log.info "Prepare contract for receiving withdrawn tickets." ;
    let* receive_withdrawn_tickets_contract =
      Contracts.prepare_receive_withdrawn_tickets_contract client protocol
    in
    (* We have slot index [0] hard-coded in the kernel.

       TODO: https://gitlab.com/tezos/tezos/-/issues/6390
       Make it possible to dynamically change tracked slot indexes. *)
    let slot_index = 0 in
    Log.info
      "Construct a batch of L2 transactions that:\n\
       1. Transfers [60] tickets from [pk1] to [pk2].\n\
       3. Withdraw [60] tickets from [pk2] to \
       [receive_withdrawn_tickets_contract]." ;
    let payload1 =
      Transaction_batch.(
        empty
        |> add_transfer
             ~counter:0
             ~signer:(Public_key pk1)
             ~signer_secret_key:sk1
             ~destination:pkh2
             ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
             ~ticket_content
             ~amount:60
        |> add_withdraw
             ~counter:0
             ~signer:(Public_key pk2)
             ~signer_secret_key:sk2
             ~destination:receive_withdrawn_tickets_contract
             ~entrypoint:"receive_tickets"
             ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
             ~ticket_content
             ~amount:60
        |> make_encoded_batch)
    in
    Log.info
      "Construct a batch of L2 transactions that:\n\
       1. Transfers [90] tickets from [pk1] to [pk2].\n\
       3. Withdraw [50] tickets from [pk2] to \
       [receive_withdrawn_tickets_contract]." ;
    let payload2 =
      Transaction_batch.(
        empty
        |> add_transfer
             ~counter:1
             ~signer:(Public_key pk1)
             ~signer_secret_key:sk1
             ~destination:pkh2
             ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
             ~ticket_content
             ~amount:90
        |> add_withdraw
             ~counter:1
             ~signer:(Public_key pk2)
             ~signer_secret_key:sk2
             ~destination:receive_withdrawn_tickets_contract
             ~entrypoint:"receive_tickets"
             ~ticketer:(Contract_hash.to_b58check mint_and_deposit_contract)
             ~ticket_content
             ~amount:40
        |> make_encoded_batch)
    in
    Log.info
      "Publish the [payload1] of size %d to DAL at slot [0]."
      (String.length payload1) ;
    let* () =
      publish_store_and_attest_slot
        client
        node
        dal_node
        Constant.bootstrap1
        ~index:slot_index
        ~content:
          (Helpers.make_slot
             ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
             payload1)
        ~attestation_lag:parameters.attestation_lag
        ~number_of_slots:parameters.number_of_slots
    in
    Log.info
      "Publish the [payload2] of size %d to DAL at slot [0]."
      (String.length payload2) ;
    let* () =
      publish_store_and_attest_slot
        client
        node
        dal_node
        Constant.bootstrap1
        ~index:slot_index
        ~content:
          (Helpers.make_slot
             ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
             payload2)
        ~attestation_lag:parameters.attestation_lag
        ~number_of_slots:parameters.number_of_slots
    in
    Log.info "Wait for the rollup node to catch up." ;
    let* current_level = Node.get_level node in
    let* _level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
    in
    Log.info "Check that [pk1] has [300] tickets." ;
    let* balance =
      get_ticket_balance sc_rollup_node ~pvm_name ~ticket_index:0 ~pkh:pkh1
    in
    Check.(
      (* [2c01000000000000] is [300] when interpreted as little-endian u64. *)
      (balance = Some "2c01000000000000")
        ~__LOC__
        (option string)
        ~error_msg:"Expected %R, got %L") ;
    Log.info "Check that [pk2] has [50] tickets." ;
    let* balance =
      get_ticket_balance sc_rollup_node ~pvm_name ~ticket_index:0 ~pkh:pkh2
    in
    Check.(
      (* [3200000000000000] is [50] when interpreted as little-endian u64. *)
      (balance = Some "3200000000000000")
        (option string)
        ~__LOC__
        ~error_msg:"Expected %R, got %L") ;
    unit

  let test_echo_kernel_e2e _protocol parameters dal_node sc_rollup_node
      _sc_rollup_address node client pvm_name =
    Log.info "Originate the echo kernel." ;
    let* {boot_sector; _} =
      Sc_rollup_helpers.prepare_installer_kernel
        ~preimages_dir:
          (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
        Constant.WASM.dal_echo_kernel
    in
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"dal_echo_kernel"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:pvm_name
        ~boot_sector
        ~parameters_ty:"unit"
        client
    in
    let* () = bake_for client in
    let* () =
      Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
    in
    let* current_level = Node.get_level node in
    let target_level =
      current_level + parameters.Dal.Parameters.attestation_lag + 1
    in
    let payload = "hello" in
    let* () =
      publish_store_and_attest_slot
        client
        node
        dal_node
        Constant.bootstrap1
        ~index:0
        ~content:
          (Helpers.make_slot
             ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
             payload)
        ~attestation_lag:parameters.attestation_lag
        ~number_of_slots:parameters.number_of_slots
    in
    Log.info "Wait for the rollup node to catch up." ;
    let* _level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node target_level
    in
    let key = "/output/slot-0" in
    let* value_written =
      Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:pvm_name
           ~operation:Sc_rollup_rpc.Value
           ~key
           ()
    in
    match value_written with
    | None -> Test.fail "Expected a value to be found. But none was found."
    | Some value ->
        let value = `Hex value |> Hex.to_string in
        Check.(
          (String.length value = parameters.Dal.Parameters.cryptobox.slot_size)
            int
            ~error_msg:"Expected a value of size %R. Got %L") ;
        if String.starts_with ~prefix:payload value then unit
        else
          let message =
            Format.asprintf
              "Expected the payload '%s' to be a prefix of the value written. \
               Instead found: %s"
              payload
              (String.sub value 0 (String.length payload))
          in
          Test.fail "%s" message

  let test_manual_echo_kernel_for_bandwidth _protocol parameters dal_node
      sc_rollup_node _sc_rollup_address node client pvm_name =
    Log.info "Originate the echo kernel." ;
    let config =
      Sc_rollup_helpers.Installer_kernel_config.
        [
          Set
            {
              to_ = "/slots";
              (* Only the slot 0 *)
              value = Z.to_bits Z.one |> Hex.of_string |> Hex.show;
            };
        ]
    in
    let* {boot_sector; _} =
      Sc_rollup_helpers.prepare_installer_kernel
        ~config:(`Config config)
        ~preimages_dir:
          (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) pvm_name)
        Constant.WASM.dal_echo_kernel_bandwidth
    in
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"dal_echo_kernel_bandwidth"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:pvm_name
        ~boot_sector
        ~parameters_ty:"unit"
        client
    in
    let* () = bake_for client in
    let* () =
      Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
    in
    let* current_level = Node.get_level node in
    let target_level =
      current_level + parameters.Dal.Parameters.attestation_lag + 1
    in
    let payload = "hello" in
    let* () =
      publish_store_and_attest_slot
        client
        node
        dal_node
        Constant.bootstrap1
        ~index:0
        ~content:
          (Helpers.make_slot
             ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
             payload)
        ~attestation_lag:parameters.attestation_lag
        ~number_of_slots:parameters.number_of_slots
    in
    Log.info "Wait for the rollup node to catch up." ;
    let* _level =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node target_level
    in
    let key = "/output/slot-0" in
    let* value_written =
      Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:pvm_name
           ~operation:Sc_rollup_rpc.Value
           ~key
           ()
    in
    match value_written with
    | None -> Test.fail "Expected a value to be found. But none was found."
    | Some value ->
        let value = `Hex value |> Hex.to_string |> Z.of_bits in
        Check.(
          (Z.to_int value = parameters.cryptobox.slot_size)
            int
            ~error_msg:"Expected number of bytes length %R. Got %L") ;
        unit
end

module Profiler = Tezos_profiler.Profiler

(* A simpler test whith a bootstrap, a producer and an observer. The goal is to
   check regression on [get_connections] RPC response. *)
let test_rpc_get_connections _protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  (* In this test we have three DAL nodes:
     - a bootstrap one to connect the other two (producer and observer),
     - a slot producer on slot 0,
     - an observer on slot 0. *)
  let index = 0 in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let peers = [Dal_node.listen_addr dal_bootstrap] in
  let peer_id dal_node = Dal_node.read_identity dal_node in

  (* Check that the dal node passed as argument to this test function
     is a running bootstrap DAL node. If not, this means that we
     forgot to register the test with ~bootstrap_profile:true *)
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] ~peers producer in
  let* () = Dal_node.run ~wait_ready:true producer in
  let* producer_peer_id = peer_id producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator index])
  in
  Log.info "Slot producer DAL node is running" ;

  let observer = Dal_node.create ~name:"observer" ~node () in
  let* () = Dal_node.init_config ~observer_profiles:[index] ~peers observer in
  let* () = Dal_node.run ~wait_ready:true observer in
  let* observer_peer_id = peer_id observer in
  let* () =
    check_profiles
      ~__LOC__
      observer
      ~expected:Dal_RPC.(Controller [Observer index])
  in
  Log.info "Observer DAL node is running" ;

  let all_pkhs =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun account -> account.Account.public_key_hash)
  in

  (* Wait for a GRAFT message between the observer and the producer,
     in any direction. *)
  let check_graft pkh =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index:index
         (observer, observer_peer_id)
         (producer, producer_peer_id)
         pkh
  in
  let check_graft_promises = List.map check_graft all_pkhs in
  Log.info "Waiting for grafting of the observer - producer connection" ;

  (* We need to bake some blocks until the L1 node notifies the DAL
     nodes that some L1 block is final so that the topic pkhs are
     known. *)
  let* () = bake_for ~count:3 client in
  let* () = Lwt.join check_graft_promises in
  Log.info "Observer - producer connection grafted" ;

  (* Calling RPC on each node *)
  Log.info "Producer get_connections" ;
  let* producer_connections =
    Dal_RPC.call producer (Dal_common.RPC.get_gossipsub_connections ())
  in
  Log.info "Observer get_connections" ;
  let* observer_connections =
    Dal_RPC.call observer (Dal_common.RPC.get_gossipsub_connections ())
  in
  Log.info "Bootstrap get_connections" ;
  let* bootstrap_connections =
    Dal_RPC.call dal_bootstrap (Dal_common.RPC.get_gossipsub_connections ())
  in

  let sort list =
    List.sort
      (fun (peer1, _, _) (peer2, _, _) -> String.compare peer1 peer2)
      list
    |> List.map (fun (peer, bootstrap, topics) ->
           let topics =
             List.sort
               (fun (i1, pkh1) (i2, pkh2) ->
                 let index = Int.compare i1 i2 in
                 if index = 0 then String.compare pkh1 pkh2 else index)
               topics
           in
           (peer, bootstrap, topics))
  in
  let eq l1 l2 =
    List.equal
      (fun (peer1, bootstrap1, topics1) (peer2, bootstrap2, topics2) ->
        peer1 = peer2 && bootstrap1 = bootstrap2
        && List.equal (fun a b -> a = b) topics1 topics2)
      (sort l1)
      (sort l2)
  in

  let parse_connections connections =
    JSON.(connections |> as_list)
    |> List.map
         JSON.(
           fun json ->
             let peer = json |-> "peer" |-> "peer_id" |> as_string in
             let connection = json |-> "connection" in
             let bootstrap = connection |-> "bootstrap" |> as_bool in
             let topics =
               connection |-> "topics" |> as_list
               |> List.map (fun t ->
                      (t |-> "slot_index" |> as_int, t |-> "pkh" |> as_string))
             in
             (peer, bootstrap, topics))
  in
  let* bootstrap = Dal_node.read_identity dal_bootstrap in
  let* observer = Dal_node.read_identity observer in
  let* producer = Dal_node.read_identity producer in
  let expected_connections peers_id =
    List.map
      (fun peer_id ->
        let bootstrap = peer_id = bootstrap in
        let topics =
          if bootstrap then [] else List.map (fun pkh -> (0, pkh)) all_pkhs
        in
        (peer_id, bootstrap, topics))
      peers_id
  in

  let parsed_bootstrap = parse_connections bootstrap_connections in
  let expected_bootstrap = expected_connections [observer; producer] in
  Check.(eq parsed_bootstrap expected_bootstrap = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for bootstrap get_connections" ;

  let parsed_observer = parse_connections observer_connections in
  let expected_observer = expected_connections [bootstrap; producer] in
  Check.(eq parsed_observer expected_observer = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for observer get_connections" ;

  let parsed_producer = parse_connections producer_connections in
  let expected_producer = expected_connections [observer; bootstrap] in
  Check.(eq parsed_producer expected_producer = true)
    ~__LOC__
    Check.bool
    ~error_msg:"Unexpected result for producer get_connections" ;

  unit

let dal_crypto_benchmark () =
  Test.register
    ~__FILE__
    ~title:"Benchmark of the DAL cryptographic primitives"
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~tags:["dal"; "benchmark"; "crypto"; Tag.slow; team]
  @@ fun () ->
  let open Dal.Cryptobox in
  let driver =
    Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
  in
  let file =
    match Cli.Logs.level with
    | Info | Debug -> "/dev/stdout"
    | _ -> Temp.file "profiler-output"
  in
  let instance = Profiler.instance driver (file, Debug) in
  Profiler.plug Profiler.main instance ;
  let ( let*? ) x f =
    match x with
    | Error err -> Test.fail "Unexpected error:@.%a@." Cryptobox.pp_error err
    | Ok x -> f x
  in
  (* the defaults are the Rio parameters *)
  let number_of_shards = Cli.get_int ~default:512 "nb_shards" in
  let slot_size = Cli.get_int ~default:126_944 "slot_size" in
  let redundancy_factor = Cli.get_int ~default:8 "redundancy" in
  let page_size = Cli.get_int ~default:3967 "page_size" in
  let traps_fraction =
    Cli.get_float ~default:0.0005 "traps_fraction" |> Q.of_float
  in
  let* () =
    let parameters =
      {number_of_shards; redundancy_factor; page_size; slot_size}
    in
    let message =
      Format.asprintf
        "(shards: %d, slot size: %d, redundancy_factor: %d, page size: %d)"
        number_of_shards
        slot_size
        redundancy_factor
        page_size
    in
    let* () =
      Profiler.record_f Profiler.main Debug ("SRS", []) @@ fun () ->
      Log.info "Loading SRS..." ;
      let* result =
        init_prover_dal
          ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
          ~fetch_trusted_setup:false
          ()
      in
      Log.info "SRS loaded." ;
      let*? () =
        Result.map_error
          (fun x ->
            `Fail
              (Format.asprintf
                 "%a"
                 Tezos_error_monad.Error_monad.pp_print_trace
                 x))
          result
      in
      unit
    in
    Profiler.record_f Profiler.main Debug (message, []) @@ fun () ->
    match make parameters with
    | Error (`Fail msg) ->
        let message = Format.asprintf "Fail: %s" msg in
        Profiler.record_f Profiler.main Debug (message, []) @@ fun () ->
        Lwt.return_unit
    | Ok _ ->
        let*? dal =
          Profiler.record_f Profiler.main Debug ("make", []) @@ fun () ->
          make parameters
        in
        let*? precomputation =
          Profiler.record_f Profiler.main Debug ("shard precomputation", [])
          @@ fun () -> precompute_shards_proofs dal
        in
        let slot =
          Profiler.record_f Profiler.main Debug ("slot generation", [])
          @@ fun () -> Cryptobox.Internal_for_tests.generate_slot ~slot_size
        in
        let*? polynomial =
          Profiler.record_f Profiler.main Debug ("polynomial from slot", [])
          @@ fun () -> polynomial_from_slot dal slot
        in
        let _slot =
          Profiler.record_f Profiler.main Debug ("polynomial to slot", [])
          @@ fun () -> polynomial_to_slot dal polynomial
        in
        let*? commitment =
          Profiler.record_f Profiler.main Debug ("commit", []) @@ fun () ->
          commit dal polynomial
        in
        let*? commitment_proof =
          Profiler.record_f Profiler.main Debug ("prove commitment", [])
          @@ fun () -> prove_commitment dal polynomial
        in
        let shards =
          Profiler.record_f Profiler.main Debug ("shards from polynomial", [])
          @@ fun () -> shards_from_polynomial dal polynomial
        in
        let shard_proofs =
          Profiler.record_f Profiler.main Debug ("prove shards", [])
          @@ fun () ->
          prove_shards dal ~precomputation ~polynomial |> Array.to_seq
        in
        let _polynomial =
          Profiler.record_f Profiler.main Debug ("reconstruct polynomial", [])
          @@ fun () -> polynomial_from_shards dal shards
        in
        let nb_pages = slot_size / page_size in
        let page_proofs =
          Seq.ints 0 |> Seq.take 1
          |> Seq.map (fun i ->
                 Profiler.record_f Profiler.main Debug ("prove page", [])
                 @@ fun () ->
                 let*? page_proof = prove_page dal polynomial i in
                 page_proof)
        in
        let is_valid =
          Profiler.record_f Profiler.main Debug ("verify commitment", [])
          @@ fun () -> verify_commitment dal commitment commitment_proof
        in
        assert is_valid ;
        let () =
          Seq.zip shards shard_proofs
          |> Seq.take 1
          |> Seq.iter (fun (shard, shard_proof) ->
                 let message =
                   Format.asprintf
                     "verify shard (size: %d)"
                     (Bytes.length
                        (Data_encoding.Binary.to_bytes_exn
                           share_encoding
                           shard.share))
                 in
                 Profiler.record_f Profiler.main Debug (message, [])
                 @@ fun () ->
                 let*? () = verify_shard dal commitment shard shard_proof in
                 ())
        in
        let bench_i_shard_verif i =
          Seq.zip shards shard_proofs |> Seq.take i |> List.of_seq |> List.split
          |> fun (shard_list, shard_proof_list) ->
          let message =
            Format.asprintf
              "verify shard multi (size: %d) (number_of_shards:%d)"
              (Bytes.length
                 (Data_encoding.Binary.to_bytes_exn
                    share_encoding
                    (List.hd shard_list).share))
              (List.length shard_list)
          in
          Profiler.record_f Profiler.main Debug (message, []) @@ fun () ->
          let*? () =
            verify_shard_multi dal commitment shard_list shard_proof_list
          in
          ()
        in
        bench_i_shard_verif 1 ;
        bench_i_shard_verif 5 ;
        bench_i_shard_verif 20 ;
        bench_i_shard_verif 100 ;
        bench_i_shard_verif 2048 ;
        let pages =
          Seq.ints 0 |> Seq.take nb_pages
          |> Seq.map (fun i -> Bytes.sub slot (i * page_size) page_size)
        in
        let () =
          Seq.zip (Seq.ints 0 |> Seq.take nb_pages) (Seq.zip pages page_proofs)
          |> Seq.take 1
          |> Seq.iter (fun (page_index, (page, page_proof)) ->
                 Profiler.record_f Profiler.main Debug ("verify page", [])
                 @@ fun () ->
                 let*? () =
                   verify_page dal commitment ~page_index page page_proof
                 in
                 ())
        in
        let () =
          let message =
            sf "share_is_trap (number_of_shards:%d)" (Seq.length shards)
          in
          Profiler.record_f Profiler.main Debug (message, []) @@ fun () ->
          shards
          |> Seq.iter (fun {share; index = _} ->
                 let res =
                   Tezos_crypto_dal.Trap.share_is_trap
                     Tezos_crypto.Signature.Public_key_hash.zero
                     share
                     ~traps_fraction
                 in
                 match res with
                 | Ok _is_trap -> ()
                 | Error err ->
                     Test.fail
                       "Unexpected error:@.%a@."
                       Data_encoding.Binary.pp_write_error
                       err)
        in
        Lwt.return_unit
  in
  Profiler.close_and_unplug Profiler.main instance ;
  Lwt.return_unit

(*
 * Test proposing to setup a node, and dal node, a baker, in multiple steps.
 * The scenario tries to follow the tutorial steps described in
 *    https://docs.tezos.com/tutorials/join-dal-baker
 * that concerns weeklynet. We use a sandbox node to simplify
 * Regressions in this test can highlight the tutorial is out of sync with
 * the octez components behavior, and the tutorial might have to be fixed.
 * an issue/pull request should be opened to
 * https://github.com/trilitech/tezos-developer-docs/ in these cases, in order
 * to change the tutorial
 *)
let scenario_tutorial_dal_baker =
  let description = "Test following dal and baker tutorial commands" in
  test
    ~regression:true
    ~__FILE__
    ~tags:[team; "tutorial"; "dal"; "baker"]
    ~uses:(fun _protocol ->
      [Constant.octez_agnostic_baker; Constant.octez_dal_node])
    (Printf.sprintf "%s" description)
    (fun protocol ->
      (* Note: Step 1 consists in setting up docker which we don't use
       * in this test
       *)
      Log.info "Step 2: Running octez node with adaptive issuance" ;
      with_layer1 ~event_sections_levels:[("prevalidator", `Debug)] ~protocol
      @@ fun _parameters _cryptobox node client _key ->
      Log.info "Step 3: setup a baker account" ;
      (* Generate the "my_baker" user *)
      let* my_baker = Client.gen_and_show_keys ~alias:"my_baker" client in

      (* Transfer 500k to "my_baker"
       * This value differs from the tutorial to obtain attestation rights *)
      let stake = Tez.of_int 500_000 in
      let* () =
        Client.transfer
          ~hooks
          ~giver:Constant.bootstrap1.alias
          ~receiver:my_baker.alias
          ~amount:stake
          ~burn_cap:Tez.one
          client
      in
      let* () = bake_for client in

      (* Check adaptive issuance is enabled, should be 0 *)
      let* adaptive_issuance_launch_cycle =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_adaptive_issuance_launch_cycle ()
      in
      Check.(
        (Option.get adaptive_issuance_launch_cycle = 0)
          ~__LOC__
          Check.int
          ~error_msg:
            "Adaptive issuance will only be launched at cycle %L, expected: %R") ;

      let* balance = Client.get_balance_for ~account:my_baker.alias client in
      Check.(balance = stake)
        ~__LOC__
        Tez.typ
        ~error_msg:"Unexpected balance for 'mybaker'. Expected: %L. Got: %R" ;

      (* Register my_baker as a baker *)
      let* () = Client.register_key ~hooks my_baker.alias client in
      let* () = bake_for client in

      (* As in the tutorial, we stake 99.8% of the balance. *)
      let* () =
        Client.stake
          ~hooks
          Tez.(stake - of_int 100)
          ~staker:my_baker.alias
          client
      in
      let* () = bake_for client in

      let* _ =
        Client.RPC.call ~hooks client
        @@ RPC.get_chain_block_context_delegate my_baker.public_key_hash
      in

      (* Calculate how many cycles to wait for my_baker to be a baker *)
      let* proto_params =
        Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
      in
      let blocks_per_cycle =
        JSON.(proto_params |-> "blocks_per_cycle" |> as_int)
      in
      let num_cycles = 7 (* Value specified in tutorial *) in
      Log.info
        "Bake for %d cycles for %s to be a baker"
        num_cycles
        my_baker.alias ;
      let* () = bake_for ~count:(num_cycles * blocks_per_cycle) client in

      let* attestation_rights =
        Node.RPC.call node
        @@ RPC.get_chain_block_helper_attestation_rights
             ~delegate:my_baker.public_key_hash
             ()
      in

      let attestation_power =
        attestation_rights |> JSON.as_list |> List.length
      in
      Log.info
        "Attestation rights: %s\n%!"
        (if attestation_power > 0 then "OK" else "KO") ;
      Check.(attestation_power > 0)
        Check.int
        ~__LOC__
        ~error_msg:"Attestation rights should be acquired" ;

      (* Only test the request can be processed *)
      let* _ =
        Node.RPC.call node @@ RPC.get_chain_block_context_dal_shards ()
      in

      (* Launch dal node (Step 4) *)
      Log.info "Step 4: Run an Octez dal node" ;
      let* dal_node = make_dal_node node in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics = 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a empty list of topics") ;

      let wait_join_event_promise =
        Dal_node.wait_for dal_node "dal_gs_join.v0" (fun _ -> Some ())
      in

      let all_delegates =
        Account.Bootstrap.keys |> Array.to_list |> List.cons my_baker
        |> List.map (fun key -> key.Account.alias)
      in
      Log.info "Step 5: Run an Octez baking daemon" ;
      let* _baker =
        let dal_node_rpc_endpoint = Dal_node.as_rpc_endpoint dal_node in
        Agnostic_baker.init
          ~event_sections_levels:[(Protocol.name protocol ^ ".baker", `Debug)]
          ~dal_node_rpc_endpoint
          ~delegates:all_delegates
          ~liquidity_baking_toggle_vote:(Some On)
          ~state_recorder:true
          ~force_apply_from_round:0
          node
          client
      in
      (* Wait for subscribed events, as expected in the tutorial *)
      let* () = wait_join_event_promise in

      let* topics = Dal_RPC.Local.call dal_node @@ Dal_RPC.get_topics () in
      Check.(
        (List.length topics > 0)
          int
          ~__LOC__
          ~error_msg:"Expecting a non-empty list of topcis") ;
      unit)

module Refutations = struct
  (**
     This test provides a scenario where two rollup nodes are initialized, one
     with a faulty DAL node and the other with a correct one. The test aims to
     verify the behavior of the rollup nodes and the protocol under these
     conditions. That is: starting (and finishing) a refutation game involving a
     DAL import tick.

     The test performs the following steps:
     - Initializes two DAL nodes, one honest and one faulty.
     - Initializes two rollup nodes, one for each DAL node.
     - Prepares and provides the kernel code for both rollup nodes.
     - Originates the dal_echo_kernel smart rollup.
     - Publishes DAL slots at indices 0 and 1 of the same level.
     - Attest the two slots.
     - Stops the faulty DAL node and alters its shards store to make it return
       wrong data for the attested shards upon request.
     - Resumes the faulty DAL node.
     - Starts the rollup nodes.
     - Enter an (infinite) loop where we bake and wait for rollup nodes to
       process the block.

     The function exits when either:
     - One of the rollup nodes is not able to catch up after 120 seconds, and in this case the test fails, or
     - The refutation game is lost by the faulty rollup node.
  *)
  let scenario_with_two_rollups_a_faulty_dal_node_and_a_correct_one
      ~refute_operations_priority _protocol parameters _dal_node _sc_rollup_node
      _sc_rollup_address node client pvm_name =
    (* Initializing the real SRS. *)
    let faulty_operator_key = Constant.bootstrap4.public_key_hash in
    let honest_operator_key = Constant.bootstrap5.public_key_hash in
    (* We have two DAL nodes in producer mode *)
    let* honest_dal_node =
      make_dal_node
        ~name:"dal-honest"
        ~peers:[]
        ~operator_profiles:[0; 1]
        ~disable_amplification:true
        node
    in
    let* faulty_dal_node =
      make_dal_node
        ~name:"dal-faulty"
        ~peers:[Dal_node.listen_addr honest_dal_node]
        ~operator_profiles:[0; 1]
        ~disable_amplification:true
        node
    in

    (* We have two Rollup nodes. One per DAL node *)
    Log.info "Originate the echo kernel." ;
    let honest_sc_rollup_node =
      Sc_rollup_node.create
        ~name:"sc-honest"
        ~dal_node:honest_dal_node
        Operator
        node
        ~base_dir:(Client.base_dir client)
        ~default_operator:honest_operator_key
    in
    let faulty_sc_rollup_node =
      Sc_rollup_node.create
        ~name:"sc-faulty"
        ~dal_node:faulty_dal_node
        Operator
        node
        ~base_dir:(Client.base_dir client)
        ~default_operator:faulty_operator_key
    in

    (* Provide the kernel's code for both rollups *)
    let* boot_sector =
      Lwt_list.fold_left_s
        (fun _boot_sector sc_rollup ->
          let* {boot_sector; _} =
            Sc_rollup_helpers.prepare_installer_kernel
              ~preimages_dir:
                (Filename.concat (Sc_rollup_node.data_dir sc_rollup) pvm_name)
              Constant.WASM.dal_echo_kernel
          in
          return boot_sector)
        ""
        [honest_sc_rollup_node; faulty_sc_rollup_node]
    in

    (* The kernel is badly written and may ask pages in negative
       levels. We ensure it is not possible by baking enough
       blocks. *)
    let* () =
      bake_for ~count:parameters.Dal.Parameters.attestation_lag client
    in

    (* Originate the dal_echo_kernel smart rollup *)
    let* sc_rollup_address =
      Client.Sc_rollup.originate
        ~burn_cap:Tez.(of_int 9999999)
        ~alias:"dal_echo_kernel"
        ~src:Constant.bootstrap1.public_key_hash
        ~kind:pvm_name
        ~boot_sector
        ~parameters_ty:"unit"
        client
    in
    let* () = bake_for client in
    let* published_level =
      let* curr = Node.get_level node in
      return (curr + 1)
    in

    (* Publish two slots and indices 0 and 1, respectively *)
    let publish_and_get_shards_filename source index =
      let* _commitment =
        Helpers.make_slot
          ~slot_size:parameters.Dal.Parameters.cryptobox.slot_size
          (Format.sprintf "Hello slot %d" index)
        |> Helpers.publish_and_store_slot client honest_dal_node source ~index
      in
      (* TODO: We should refactor this to use the function defined in lib_dal_node/store. *)
      return @@ Format.asprintf "%d_%d" published_level index
    in
    let* shards_file0 = publish_and_get_shards_filename Constant.bootstrap1 0 in
    let* shards_file1 = publish_and_get_shards_filename Constant.bootstrap2 1 in

    (* Bake sufficiently many blocks to be able to attest. *)
    let* () = bake_for ~count:parameters.attestation_lag client in
    let* () =
      inject_dal_attestations_and_bake
        node
        client
        ~number_of_slots:parameters.number_of_slots
        (Slots [0; 1])
    in

    (* Stop the faulty DAL node and alter its shards store. *)
    let shards_store =
      Dal_node.data_dir faulty_dal_node ^ "/store/shard_store"
    in

    let* () = Dal_node.terminate faulty_dal_node in
    let* () = Dal_node.kill faulty_dal_node in

    (* We published and attested two slots, with commitments [commitment0] and
       [commitment1] respectively, at the same level above. Below, we alter the
       shards store of the faulty node by interchanging the content/shards of
       [commitment0] and [commitment1]. *)
    let mv_shards from into =
      sf "mv %s/%s %s/%s" shards_store from shards_store into |> Sys.command
      |> function
      | 0 ->
          Log.info "Copied %s/%s into %s/%s" shards_store from shards_store into
      | n ->
          Test.fail
            "Failed to copy file %s/%s. Got a ret code %d@."
            shards_store
            from
            n
    in
    mv_shards shards_file0 "tmp" ;
    mv_shards shards_file1 shards_file0 ;
    mv_shards "tmp" shards_file1 ;

    let* () = Dal_node.run ~wait_ready:true faulty_dal_node in

    (* configure the rollup nodes. *)
    let* () =
      Lwt_list.iter_s
        (fun sc_rollup_node ->
          let* _name =
            Sc_rollup_node.config_init
              ~force:true
              sc_rollup_node
              sc_rollup_address
          in
          unit)
        [faulty_sc_rollup_node; honest_sc_rollup_node]
    in
    (Sc_rollup_helpers.prioritize_refute_operations
    @@
    match refute_operations_priority with
    | `Honest_first -> honest_sc_rollup_node
    | `Faulty_first -> faulty_sc_rollup_node) ;

    (* Start the rollup nodes. *)
    let* () =
      Lwt_list.iter_s
        (fun sc_rollup_node ->
          Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug])
        [faulty_sc_rollup_node; honest_sc_rollup_node]
    in

    let rec loop () =
      let* current_level = Node.get_level node in
      let target_level = current_level + 1 in
      let* () = bake_for client in
      Log.info "Wait for the rollup nodes to catch up at level %d." target_level ;
      let* () =
        Lwt_list.iter_p
          (fun sc_rollup ->
            let* _level =
              Sc_rollup_node.wait_for_level ~timeout:120. sc_rollup target_level
            in
            unit)
          [faulty_sc_rollup_node; honest_sc_rollup_node]
      in
      loop ()
    in
    let (_ : unit Lwt.t) = loop () in
    Sc_rollup_node.check_error
      faulty_sc_rollup_node
      ~exit_code:1
      ~msg:(rex "lost the refutation game")

  let register_scenario_with_two_rollups_a_faulty_dal_node_and_a_correct_one
      ~refute_operations_priority name protocols =
    let scenario =
      scenario_with_two_rollups_a_faulty_dal_node_and_a_correct_one
    in
    scenario_with_all_nodes
      name
      ~regression:false
      ~uses:(fun _protocol ->
        [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel])
      ~pvm_name:"wasm_2_0_0"
      ~commitment_period:5
      ~smart_rollup_timeout_period_in_blocks:20
      ~l1_history_mode:Default_with_refutation
      ~disable_amplification:true
      ~tags:[Tag.slow]
      (scenario ~refute_operations_priority)
      protocols
end

(** This test injects a DAL slot to (DAL and L1) network(s) via the rollup node
    using {!post_local_dal_injection} rollup RPC. It then checks that the slot
    is attested, which implies that the commitment is published to L1 and that
    the shards of the slot are declared available by the DAL node.  *)
let rollup_node_injects_dal_slots _protocol parameters dal_node sc_node
    sc_rollup_address node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[0]
  in
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection
         ~messages:["Hello DAL from a Smart Rollup"]
  in
  (* We need to bake once to get the commitment injected and once more to have it
     included in a block. *)
  let* () =
    repeat 2 (fun () ->
        let* () = bake_for client in
        let* level = Client.level client in
        let* _level =
          Sc_rollup_node.wait_for_level ~timeout:10. sc_node level
        in
        unit)
  in
  let* () =
    repeat parameters.Dal.Parameters.attestation_lag (fun () ->
        let* () = bake_for client in
        let* level = Client.level client in
        let* _level =
          Sc_rollup_node.wait_for_level ~timeout:10. sc_node level
        in
        unit)
  in
  let* metadata = Node.RPC.(call node @@ get_chain_block_metadata ()) in
  let expected_dal_attestation = [|true|] in
  let obtained_dal_attestation =
    match metadata.dal_attestation with
    | None ->
        (* Field is part of the encoding when the feature flag is true *)
        Test.fail
          "Field dal_attestation in block headers is mandatory when DAL is \
           activated"
    | Some x -> x
  in
  Check.(
    (expected_dal_attestation = obtained_dal_attestation)
      (array bool)
      ~error_msg:"Expected attestation bitset %L, got %R") ;
  let* statuses =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.get_dal_injected_operations_statuses ()
  in
  match statuses with
  | [status_with_hash] ->
      let status = JSON.get "status" status_with_hash in
      let status_str = JSON.(status |-> "status" |> as_string) in
      if status_str <> "included" && status_str <> "committed" then
        Test.fail
          "Unexpected injector operation status %s. Expecting 'included' or \
           'committed'"
          status_str ;
      unit
  | _ ->
      Test.fail
        "Expecting a status for 1 operation, got %d@."
        (List.length statuses)

(** This test verifies the optimal publication of DAL slots from a batch of
    messages into the DAL node and L1 via the rollup node DAL injector.

    The test ensures that:
    - DAL slots are published at the appropriate slot indices (0, 1, 2) and levels;
    - Messages are grouped efficiently into DAL slots.

    More precisely, we inject a list of messages into the rollup node's DAL
    injector worker and check that:

    - The first four chunks (with a name prefix [slot0_] below) fit exactly into
    one slot. They are published at some level L, with slot index 0.

    - The next three chunks (with a name prefix [slot1_]), which don't fully
    fill a DAL slot, are combined into a slot and published at level L, with slot
    index 1. In fact, the following message in the queue (namely [slot2_ck1]) is
    one byte too big to fit in the same slot as these messages. So, the latter
    is published into slot index 2 of the same level L (it cannot be combined
    with the remaining messages as well).

    - All available slot indices being used at level L, the remaining messages
    are considered at level L+1. The two last messages are published at slot
    indices 0 and 1, respectively. Message [slot3_ck_full] fits in a slot, so
    there is no need to combine it with other slots. As for
    [slot4_ck_almost_empty], which only occupies 1 byte, there are no remaining
    messages that could be combined with it.

    The test checks that the correct number of DAL slots are published and
    verifies the length of the messages within the slots.
*)
let rollup_batches_and_publishes_optimal_dal_slots _protocol parameters dal_node
    sc_node sc_rollup_address _node client _pvm_name =
  let client = Client.with_dal_node client ~dal_node in
  let* () = Sc_rollup_node.run sc_node sc_rollup_address [] in
  let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in

  (* We create some messages below with various sizes. *)
  Check.(
    (slot_size mod 4 = 0)
      int
      ~error_msg:"Expected DAL slots size to be a multiple of 4") ;
  let chunks_size = slot_size / 4 in

  let slot0_ck1 = String.make chunks_size 'A' in
  let slot0_ck2 = slot0_ck1 in
  let slot0_ck3 = slot0_ck2 in
  let slot0_ck4 = slot0_ck3 in

  let slot1_ck1 = String.make chunks_size 'B' in
  let slot1_ck2 = slot1_ck1 in
  let slot1_ck3 = slot1_ck2 in
  let slot2_ck1 = String.make (chunks_size + 1) 'C' in

  let slot3_ck_full = String.make slot_size 'D' in
  let slot4_ck_almost_empty = String.make 1 'E' in

  (* We define an order of addition of the messages into the DAL injection queue
     and group them in the way they are expected to be packed into DAL slots. *)
  let annotated_messages =
    [
      [slot0_ck1; slot0_ck2; slot0_ck3; slot0_ck4];
      [slot1_ck1; slot1_ck2; slot1_ck3];
      [slot2_ck1];
      [slot3_ck_full];
      [slot4_ck_almost_empty];
    ]
  in

  (* We derive from [annotated_messages] the raw list of messages that will be
     injected with RPC [post_local_dal_batcher_injection] below.*)
  let messages = List.concat annotated_messages in

  (* We define the expected number of total, valid and discarded messages as well
     as the number of expected DAL slots to publish. *)
  let total_num_messages = List.length messages in
  let expected_number_of_injected_dal_slots = List.length annotated_messages in

  (* We start by informing the DAL injector of the rollup node that we want to
     publish on slots indices 0, 1 and 2. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_dal_slot_indices ~slot_indices:[0; 1; 2]
  in

  (* This promise will count the number of successfully injected messages. We
     expected seeing [total_num_messages] events [dal_message_received] before
     the promise resolves. *)
  let wait_dal_message_received =
    let countdown = ref total_num_messages in
    Sc_rollup_node.wait_for sc_node "dal_message_received.v0" (fun _ ->
        decr countdown ;
        if !countdown = 0 then Some () else None)
  in
  (* Given a number [expected_num_published_slots] of slots to publish, this
     function returns a promise that resolves once the corresponding number of
     expected events [inject_dal_slot_from_messages] are seen. The function
     returns the payloads of the events in a list as tuples: [(data_size,
     num_messages, published_level, slot_index)]. *)
  let wait_inject_dal_slot_from_messages ~expected_num_published_slots =
    let open JSON in
    let countdown = ref expected_num_published_slots in
    let published = ref [] in
    let* () =
      Sc_rollup_node.wait_for
        sc_node
        "inject_dal_slot_from_messages.v0"
        (fun json ->
          let data_size = get "data_size" json |> as_int in
          let num_messages = get "num_messages" json |> as_int in
          let level = get "level" json |> as_int in
          (* The slot was injected at leve [level]. We assume that it will be
             included at the next level, which will be the "published
             level". *)
          let published_level = level + 1 in
          let slot_index = get "slot_index" json |> as_int in
          published :=
            (data_size, num_messages, published_level, slot_index) :: !published ;
          decr countdown ;
          if !countdown = 0 then Some () else None)
    in
    Lwt.return (List.rev !published)
  in
  (* We finally inject the messages via post_local_dal_batcher_injection into
     the DAL injection queue. *)
  let* () =
    Sc_rollup_node.RPC.call sc_node
    @@ Sc_rollup_rpc.post_local_dal_batcher_injection ~messages
  in

  (* We wait until all injected messages are processed. *)
  let* () = wait_dal_message_received in

  (* Before baking one block and triggering the three first slots publication,
     we create the promise that will track the [inject_dal_slot_from_messages]
     events and return injected slots information. *)
  let* published_slots_1 =
    let wait_inject_dal_slot_from_messages_first_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:3
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:10. sc_node level in
    wait_inject_dal_slot_from_messages_first_level
  in

  (* Once the three first slots are published, we do the same thing for the two
     other expected slots at the next level. *)
  let* published_slots_2 =
    let wait_inject_dal_slot_from_messages_second_level =
      wait_inject_dal_slot_from_messages ~expected_num_published_slots:2
    in
    let* () = bake_for client in
    let* level = Client.level client in
    let* _level = Sc_rollup_node.wait_for_level ~timeout:10. sc_node level in
    wait_inject_dal_slot_from_messages_second_level
  in

  (* We sort the resulting published slots information by level and slot
     indices. *)
  let published_slots =
    published_slots_2 @ published_slots_1
    |> List.fast_sort
         (fun
           (_data_size, _num_messages, level, slot_index)
           (_data_size', _num_messages', level', slot_index')
         ->
           let c = level - level' in
           if c <> 0 then c else slot_index - slot_index')
  in

  (* We check that all messages are published. *)
  let count_published_messages =
    List.fold_left
      (fun acc (_data_size, num_messages, _level, _slot_index) ->
        num_messages + acc)
      0
      published_slots
  in
  Check.(
    (total_num_messages = count_published_messages)
      int
      ~error_msg:"Expected number of batched messages %L. Got %R") ;

  (* We check that the published number of DAL slots is as expected. *)
  Check.(
    (expected_number_of_injected_dal_slots = List.length published_slots)
      int
      ~error_msg:"Expected published DAL slots is %L. Got %R") ;

  (* Finally, we check that published slots have the expected size w.r.t. to our
     initial packs of messages. *)
  let rec check_packs published_slots annotated_messages =
    match (published_slots, annotated_messages) with
    | [], [] -> ()
    | ( (data_size, _num_messages, _level, _slot_index) :: published_slots,
        pack :: annotated_messages ) ->
        let sz = List.fold_left (fun sz msg -> sz + String.length msg) 0 pack in
        Check.(
          (sz = data_size)
            int
            ~error_msg:"Expected published DAL slots size is %L. Got %R") ;
        check_packs published_slots annotated_messages
    | [], _ | _, [] -> Test.fail "Unreachable case"
  in
  check_packs published_slots annotated_messages ;
  unit

(* We have a bootstrap node, a producer node and an attester node for a new
   attester. We check that as soon as the attester is in the DAL committee it
   attests. *)
let test_new_attester_attests _protocol dal_parameters _cryptobox node client
    dal_bootstrap =
  let peer_id dal_node = Dal_node.read_identity dal_node in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let slot_index = 0 in
  let* () = check_profiles ~__LOC__ dal_bootstrap ~expected:Dal_RPC.Bootstrap in
  Log.info "Bootstrap DAL node is running" ;

  let peers = [Dal_node.listen_addr dal_bootstrap] in

  let producer = Dal_node.create ~name:"producer" ~node () in
  let* () =
    Dal_node.init_config ~operator_profiles:[slot_index] ~peers producer
  in
  let* () = Dal_node.run ~wait_ready:true producer in
  let* () =
    check_profiles
      ~__LOC__
      producer
      ~expected:Dal_RPC.(Controller [Operator slot_index])
  in
  Log.info "Slot producer DAL node is running" ;

  (* Set up a new account that holds a big amount of tez and make sure it can be
     an attester. *)
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* balance =
    Client.get_balance_for ~account:Constant.bootstrap1.alias client
  in
  let amount = Tez.(balance - one) in
  let* new_account = Client.gen_and_show_keys client in
  let* () =
    Client.transfer
      ~giver:Constant.bootstrap1.alias
      ~receiver:new_account.alias
      ~amount
      ~burn_cap:Tez.one
      client
  in
  let* () = bake_for client in
  let*! () = Client.reveal ~fee:Tez.one ~src:new_account.alias client in
  let* () = bake_for client in
  let* () = Client.register_key new_account.alias client in
  let* () = bake_for client in
  let* () = Client.stake ~staker:new_account.alias Tez.(amount /! 2L) client in

  let attester = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:[new_account.public_key_hash]
      ~peers
      attester
  in
  let* () = Dal_node.run attester in
  let client = Client.with_dal_node client ~dal_node:attester in

  let num_cycles = 1 + consensus_rights_delay in
  let* level = Client.level client in

  let lag = dal_parameters.attestation_lag in
  (* We need to publish at the level [n - lag], where [n] is the first level
     where we should see the attestation of the new attester. Level [n] is
     [first_level_in_committee + 1] (because the attestation is included in the
     following block). *)
  let first_level_in_committee =
    (* plus 1 because the first cycle starts at level 1 *)
    (num_cycles * blocks_per_cycle) + 1
  in
  let published_level = first_level_in_committee + 1 - lag in
  Log.info
    "first_level_in_committee = %d; published_level = %d"
    first_level_in_committee
    published_level ;
  Log.info "Bake blocks up to level %d" (published_level - 1) ;
  let* () = bake_for ~count:(published_level - 1 - level) client in

  let* level = Client.level client in
  Log.info
    "Current level is %d, publish a slot for level %d"
    level
    published_level ;
  let* _ =
    Helpers.publish_and_store_slot
      client
      producer
      Constant.bootstrap2
      ~index:slot_index
    @@ Helpers.make_slot ~slot_size "SLOTDATA"
  in
  let* () = bake_for client in
  let* manager_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:3 ()
  in
  Check.(
    (JSON.as_list manager_ops |> List.length <> 0)
      int
      ~error_msg:
        "Expected the commitment to be published, but no manager operation was \
         included.") ;

  let* id_attester = peer_id attester in
  let* id_producer = peer_id producer in
  let check_graft_promises =
    Lwt.pick
    @@ check_grafts
         ~number_of_slots
         ~slot_index
         (attester, id_attester)
         (producer, id_producer)
         new_account.public_key_hash
  in
  let* assigned_shard_indexes =
    Dal_RPC.(
      call attester
      @@ get_assigned_shard_indices
           ~level:first_level_in_committee
           ~pkh:new_account.public_key_hash)
  in
  let wait_for_shards_promises =
    wait_for_shards_promises
      ~dal_node:attester
      ~shards:assigned_shard_indexes
      ~published_level
      ~slot_index
  in

  Log.info
    "Bake another block, so that the attester node fetches the DAL committee \
     for level %d and changes topics"
    first_level_in_committee ;
  let* () = bake_for client in

  Log.info "Waiting for grafting of the attester - producer connection" ;
  (* This is important because the attester and the producer should connect and
     join the relevant mesh, before the producer attempts to send its
     shards. Normally there the time between two levels to do that, but since
     we're baking in the past, this time is very short, so without this explicit
     wait the test would be flaky. *)
  let* () = check_graft_promises in

  Log.info "Bake another block, so that the producer sends the shards" ;
  let* () = bake_for client in
  let () = Log.info "Waiting for the attester to receive its shards" in
  let* () = wait_for_shards_promises in

  Log.info "Bake blocks up to level %d" (first_level_in_committee - 1) ;
  let* () = bake_for ~count:(lag - 4) client in
  let* level = Client.level client in
  Log.info "Current level is %d" level ;
  Check.(
    (level = first_level_in_committee - 1)
      int
      ~error_msg:"Expected current level to be %R, got %L") ;
  Log.info
    "At level %d the new attester is not yet in the committee, at the next \
     level it will be"
    level ;
  let* () =
    check_in_TB_committee
      ~__LOC__
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~inside:false
      ~level
  in
  Log.info "Bake one more block for %s to be in the committee" new_account.alias ;
  let* () = bake_for client in
  let* () =
    check_in_TB_committee
      ~__LOC__
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in
  let* () =
    Dal.Committee.check_is_in
      ~__LOC__
      node
      new_account.public_key_hash
      ~level:(level + 1)
  in

  Log.info "Bake a block with all accounts, including the new account" ;
  let* () =
    let bootstrap_accounts =
      Array.to_list Account.Bootstrap.keys
      |> List.map (fun a -> a.Account.public_key_hash)
    in
    bake_for
      ~delegates:(`For (new_account.public_key_hash :: bootstrap_accounts))
      client
  in
  let* json =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let dal_attestation_opt =
    List.find_map
      (fun json ->
        let contents = JSON.(json |-> "contents" |> as_list) |> List.hd in
        let delegate =
          JSON.(contents |-> "metadata" |-> "delegate" |> as_string)
        in
        let kind = JSON.(contents |-> "kind" |> as_string) in
        if
          delegate = new_account.public_key_hash
          && kind = "attestation_with_dal"
        then Some JSON.(contents |-> "dal_attestation" |> as_string)
        else None)
      (JSON.as_list json)
  in
  Check.(
    (dal_attestation_opt = Some "1")
      (option string)
      ~error_msg:
        "Expected a DAL attestation for slot 0 for the new attester: got %L, \
         expected %R") ;
  unit

let pair_up ~error_msg =
  let rec pair_up = function
    | [] -> []
    | [_] -> Test.fail ~__LOC__ error_msg
    | x :: y :: rest -> (x, y) :: pair_up rest
  in
  pair_up

let extract_dal_balance_updates balance_updates =
  List.filter_map
    (fun (json1, json2) ->
      let change1 = JSON.(json1 |-> "change" |> as_int) in
      let change2 = JSON.(json2 |-> "change" |> as_int) in
      Check.(
        (-change1 = change2)
          ~__LOC__
          int
          ~error_msg:"Expected 'change' to match; got %L and %R") ;
      let kind = JSON.(json1 |-> "kind" |> as_string) in
      if
        (not (String.equal kind "minted"))
        && not (String.equal kind "accumulator")
      then
        Test.fail
          ~__LOC__
          "Expected a 'minted' or 'accumulator' kind, got '%s'"
          kind ;
      let category1 = JSON.(json1 |-> "category" |> as_string) in
      if String.equal category1 "DAL attesting rewards" then
        let kind2 = JSON.(json2 |-> "kind" |> as_string) in
        match kind2 with
        | "burned" ->
            let category2 = JSON.(json2 |-> "category" |> as_string) in
            Check.(
              (category2 = "lost DAL attesting rewards")
                ~__LOC__
                string
                ~error_msg:
                  "Expected a 'lost DAL attesting rewards' category, got %L") ;
            let delegate = JSON.(json2 |-> "delegate" |> as_string) in
            Some (`Lost (delegate, change2, json2))
        | "freezer" ->
            let delegate =
              JSON.(json2 |-> "staker" |-> "baker_own_stake" |> as_string)
            in
            Some (`Got (delegate, change2, json2))
        | "contract" ->
            let delegate = JSON.(json2 |-> "contract" |> as_string) in
            Some (`Got (delegate, change2, json2))
        | _ -> Test.fail ~__LOC__ "Unexpected balance update kind %s" kind2
      else None)
    balance_updates

(* This function checks the fields of the [dal_participation] RPC result. It is
   supposed to be used only by the test scenario below, as it assumes
   [expected_attested_shards] is either 0 or [expected_attestable_shards],
   [denounced] is [false], and there are only two kinds of transfers, both to
   the delegate itself, as there are no co-stakers in the scenario below.. *)
let check_participation_and_rewards participation ~expected_assigned_shards
    ~expected_attestable_slots ~attesting_reward_per_shard dal_balance_updates
    delegate ~sufficient_participation =
  Check.is_false
    participation.RPC.denounced
    ~__LOC__
    ~error_msg:"The delegate was unexpectedly denounced" ;
  Check.(
    (participation.expected_assigned_shards_per_slot = expected_assigned_shards)
      ~__LOC__
      int
      ~error_msg:"Unexpected number of assigned shards. Expected %R, got %L") ;
  let expected_attested_slots =
    if sufficient_participation then expected_attestable_slots else 0
  in
  Check.(
    (participation.delegate_attested_dal_slots = expected_attested_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that the delegate has attested %L slots, got %R") ;
  Check.(
    (participation.delegate_attestable_dal_slots = expected_attestable_slots)
      ~__LOC__
      int
      ~error_msg:"Expected that there are %L attestable slots, got %R") ;
  Check.(
    (participation.sufficient_dal_participation = sufficient_participation)
      ~__LOC__
      bool
      ~error_msg:"Expected sufficient_dal_participation to be %R, got %L") ;
  let dal_rewards = expected_assigned_shards * attesting_reward_per_shard in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = dal_rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R")) ;
  let get_delegate = function
    | `Got (delegate, _amount, _json) | `Lost (delegate, _amount, _json) ->
        delegate
  in
  let dal_rewards =
    List.filter_map
      (fun item ->
        let item_delegate = get_delegate item in
        if String.equal item_delegate delegate then Some item else None)
      dal_balance_updates
  in
  let get_json_list =
    List.map (function `Got (_, _, json) | `Lost (_, _, json) ->
        JSON.encode json)
  in
  let rewards =
    if sufficient_participation then
      match dal_rewards with
      | [`Got (_, amount1, _); `Got (_, amount2, _)] ->
          (* one corresponds to the liquid rewards and one to the frozen ones (but
             we do not care here) *)
          amount1 + amount2
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
    else
      match dal_rewards with
      | [`Lost (_, amount, _)] -> amount
      | _ ->
          Test.fail
            ~__LOC__
            "Unexpected balance updates for the delegate %s: %a"
            delegate
            Format.(pp_print_list pp_print_string)
            (get_json_list dal_rewards)
  in
  Check.(
    (Tez.to_mutez participation.expected_dal_rewards = rewards)
      ~__LOC__
      int
      ~error_msg:
        ("Unexpected rewards for delegate " ^ delegate ^ ": expected %L, got %R"))

let check_participation_and_rewards node ~expected_assigned_shards
    ~expected_attestable_slots =
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let balance_updates =
    pair_up
      balance_updates
      ~error_msg:"The list of balance updates has an odd number of elements"
  in
  let dal_balance_updates =
    extract_dal_balance_updates balance_updates
    |> (* sort them for proper regression output *)
    List.sort (fun e1 e2 ->
        match (e1, e2) with
        | `Got (d1, a1, _), `Got (d2, a2, _)
        | `Lost (d1, a1, _), `Lost (d2, a2, _) ->
            let c = String.compare d1 d2 in
            if c = 0 then a1 - a2 else c
        | `Got _, `Lost _ -> 1
        | `Lost _, `Got _ -> -1)
  in
  List.iter
    (function
      | `Got (_, _, json) | `Lost (_, _, json) ->
          Regression.capture @@ JSON.encode json)
    dal_balance_updates ;
  let* attesting_reward_per_shard =
    let* json =
      Node.RPC.call ~rpc_hooks node
      @@ RPC.get_chain_block_context_issuance_expected_issuance ()
    in
    return @@ JSON.(json |=> 0 |-> "dal_attesting_reward_per_shard" |> as_int)
  in
  return @@ fun delegate ~sufficient_participation ->
  (* Note that at the last level in the cycle we lose information about the
     total_dal_attested_slots *)
  let* participation =
    Node.RPC.call ~rpc_hooks node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         delegate
  in
  return
  @@ check_participation_and_rewards
       participation
       ~expected_assigned_shards
       ~expected_attestable_slots
       ~attesting_reward_per_shard
       dal_balance_updates
       delegate
       ~sufficient_participation

(* We have one DAL attester node. During the second cycle, one of the bakers
   does not DAL attest sufficiently. We check that the attesters receive or not
   the DAL rewards depending on their participation. *)
let test_attesters_receive_dal_rewards _protocol dal_parameters _cryptobox node
    client dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  (* This constraint makes the test simpler; it could be lifted. *)
  assert (blocks_per_cycle = dal_parameters.Dal.Parameters.attestation_lag) ;
  let expected_assigned_shards =
    let number_of_shards = dal_parameters.cryptobox.number_of_shards in
    let num_delegates = Array.length Account.Bootstrap.keys in
    number_of_shards * blocks_per_cycle / num_delegates
  in
  let all_delegates =
    Account.Bootstrap.keys |> Array.to_list
    |> List.map (fun key -> key.Account.public_key_hash)
  in
  let delegate_without_dal = List.hd all_delegates in
  let rest_delegates = List.tl all_delegates in
  let delegate_with_dal = List.hd rest_delegates in
  Log.info
    "delegate not running DAL: %s, delegate running DAL: %s"
    delegate_without_dal
    delegate_with_dal ;
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let blocks_to_bake = blocks_per_cycle - 1 - level.cycle_position in
  Log.info "Publish a slot for %d levels, up to the end of cycle" blocks_to_bake ;
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_to_bake (fun () ->
        let wait_slot ~published_level:_ ~slot_index:_ = unit in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap1
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let* level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (level.cycle_position = blocks_per_cycle - 1) ;

  let* check =
    let expected_attestable_slots = 0 in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  let* () = check delegate_without_dal ~sufficient_participation:true in

  Log.info "Bake for one more cycle" ;
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  let* () =
    bake_for
      ~delegates:(`For rest_delegates)
      ~count:blocks_per_cycle
      ~dal_node_endpoint
      client
  in

  let* check =
    (* [-1] because the participation is obtained one level before the cycle end;
       and another [-1] because the slot for the first level in the cycle was not
       published *)
    let expected_attestable_slots = blocks_per_cycle - 2 in
    check_participation_and_rewards
      node
      ~expected_assigned_shards
      ~expected_attestable_slots
  in
  let* () = check delegate_without_dal ~sufficient_participation:false in
  let* () = check delegate_with_dal ~sufficient_participation:true in
  unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7686
   In the following accusation tests, we bake two blocks because we
   need the accusation to be introduced at level at least 10 (2 = 10 -
   attestation_lag). In protocol S we will not need this
   restriction. *)

let test_inject_accusation protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in
  let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
  let commitment, proof, shards_with_proofs =
    Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
  in
  Log.info "Bake two blocks" ;
  let* () = bake_for ~count:2 client in
  let* _op_hash =
    Helpers.publish_commitment
      ~source:Constant.bootstrap2
      ~index:slot_index
      ~commitment
      ~proof
      client
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* () = bake_for ~count:lag client in
  Log.info "Inject an attestation" ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~signer
      ~nb_slots:number_of_slots
      availability
      client
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[signer.public_key_hash]
         ()
  in
  let shard_index =
    JSON.(
      shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list
      |> List.hd |> as_int)
  in
  Log.info "First shard index of the attester is %d" shard_index ;
  let shard, proof =
    Seq.find
      (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
      shards_with_proofs
    |> Option.get
  in
  let accusation =
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      shard
      proof
  in
  Log.info "Inject an accusation" ;
  let* _op_hash = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list ops) = 1)
    ~__LOC__
    Check.(int)
    ~error_msg:"Expected exactly one anonymous op. Got: %L" ;
  unit

type tz4_account = {delegate_key : Account.key; companion_key : Account.key}

(* [round_robin n l] distributes the values of [l] in an array of [n] elements.
   so [round_robin 3 [a1; ..; a10] = [|[a1;a4;a7;a10]; [a2;a5;a8]; [a3;a6;a9]|]].
*)
let round_robin n l =
  let rec bis acc k = function
    | [] -> Array.map List.rev acc
    | hd :: tl ->
        let acc_local = acc.(k) in
        let () = acc.(k) <- hd :: acc_local in
        bis acc ((k + 1) mod n) tl
  in
  bis (Array.init n (fun _ -> [])) 0 l

let create_tz4_accounts_stake_and_wait ~funders ~client ~node nb_to_create =
  Log.info "Generate tz4 keys" ;
  let* new_tz4_accounts =
    Lwt_list.map_s
      (fun index ->
        Client.gen_and_show_keys
          ~sig_alg:"bls"
          ~alias:("bls_account_" ^ string_of_int (1 + index))
          client)
      (List.init nb_to_create Fun.id)
  in
  let* () = bake_for client in
  Log.info "We have the accounts, let's fund them" ;
  let* _ophs =
    Lwt.all
    @@ List.map2
         (fun accounds_to_fund source ->
           (* We give 500k tez to each account. *)
           let amount = 500_000 * 1_000_000 in
           let transfers =
             List.map
               (fun dest -> Operation.Manager.transfer ~dest ~amount ())
               accounds_to_fund
           in
           let* counter = Operation.Manager.get_next_counter ~source client in
           let batch =
             Operation.Manager.make_batch ~source ~counter transfers
           in
           Operation.Manager.inject batch client)
         (Array.to_list @@ round_robin (List.length funders) new_tz4_accounts)
         funders
  in
  let* () = bake_for client in
  Log.info "All keys funded, let's set them as delegates" ;
  let* () =
    Lwt_list.iter_s
      (fun account ->
        Client.set_delegate
          ~src:account.Account.alias
          ~delegate:account.Account.alias
          client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys are delegate, let's attach a companion key to them" ;
  let* companions =
    Lwt_list.map_s
      (fun account ->
        Client.update_fresh_companion_key ~algo:"bls" account client)
      new_tz4_accounts
  in
  let* () = bake_for client in
  Log.info "All keys have a companion, let's stake" ;
  let* () =
    Lwt_list.iter_s
      (fun source ->
        Client.stake ~staker:source.Account.alias Tez.(of_int 100_000) client)
      new_tz4_accounts
  in
  let* RPC.{cycle; _} =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  Log.info "All keys have staked, let's wait for them to have rights" ;
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let* () =
    Client.bake_until_cycle
      ~target_cycle:(cycle + consensus_rights_delay + 1)
      client
  in
  return
  @@ List.map2
       (fun delegate_key companion_key -> {delegate_key; companion_key})
       new_tz4_accounts
       companions

let test_aggregation_required_to_pass_quorum _protocol dal_parameters _cryptobox
    node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let lag = dal_parameters.attestation_lag in
  let* _tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:(Account.Bootstrap.keys |> Array.to_list)
      ~node
      ~client
      25
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  let* () = bake_for client in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint dal_node |> Endpoint.as_string
  in
  Log.info "Let's wait for the attestation of this publication to be sent." ;
  let* () = bake_for ~count:lag client ~dal_node_endpoint in
  let* attestations =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass ~validation_pass:0 ()
  in
  let aggregated_attestation =
    JSON.(
      List.find
        (fun attestation ->
          attestation |-> "contents" |> as_list |> List.hd |-> "kind"
          |> as_string = "attestations_aggregate")
        (as_list attestations))
  in
  let aggregated_attestation_consensus_power =
    JSON.(
      aggregated_attestation |-> "contents" |> as_list |> List.hd |-> "metadata"
      |-> "total_consensus_power" |> as_int)
  in
  let* constants =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let consensus_committee_size =
    JSON.(constants |-> "consensus_committee_size" |> as_int)
  in
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata () in
  Check.(
    aggregated_attestation_consensus_power
    >= consensus_committee_size
       * (100 - dal_parameters.attestation_threshold)
       / 100)
    ~__LOC__
    Check.int
    ~error_msg:
      "The consensus power of the tz4 accounts is not sufficient to ensure \
       that the ability to read inside the aggregated attestations is required \
       for the slot to be protocol attested." ;
  match metadata.dal_attestation with
  | None ->
      Test.fail
        "Field dal_attestation in block headers is mandatory when DAL is \
         activated"
  | Some bitset ->
      Check.((Array.length bitset = 1) ~__LOC__ int)
        ~error_msg:
          "There should be only one slot DAL attested at protocol level. Got \
           %L." ;
      Check.is_true
        bitset.(slot_index)
        ~__LOC__
        ~error_msg:"Slot was supposed to be protocol attested." ;
      unit

let test_inject_accusation_aggregated_attestation nb_attesting_tz4 protocol
    dal_parameters _cryptobox node client dal_node =
  let slot_index = 0 in
  let* () = Dal_RPC.(call dal_node (patch_profiles [Operator slot_index])) in
  let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
  let number_of_slots = dal_parameters.number_of_slots in
  let lag = dal_parameters.attestation_lag in
  let* tz4_accounts =
    create_tz4_accounts_stake_and_wait
      ~funders:[Constant.bootstrap1]
      ~node
      ~client
      5
  in
  Log.info "Enough waiting, let's publish a commitment" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      dal_node
      Constant.bootstrap1
      ~index:slot_index
      message
  in
  (* We need to bake exactly [lag] blocks so that we can inject an attestation
     at the right level (that attests the published slot). *)
  let* publication_level = Client.bake_for_and_wait_level client in
  Log.info "Attestation lag should pass before sending an attestation" ;
  let* () = bake_for ~count:(lag - 1) client in
  Log.info "Inject an attestation" ;
  let level = publication_level + lag - 1 in
  (* BLS consensus keys are now activated *)
  let* slots = Operation.Consensus.get_slots_by_consensus_key ~level client in
  let* round = Baker_test.fetch_round client in
  let* branch = Operation.Consensus.get_branch ~attested_level:level client in
  let* block_payload_hash =
    Operation.Consensus.get_block_payload_hash
      ~block:(string_of_int level)
      client
  in
  let dal_attestation = Array.init number_of_slots (fun i -> i = slot_index) in
  let faulty_bakers, other_tz4 =
    Tezos_stdlib.TzList.split_n nb_attesting_tz4 tz4_accounts
  in
  let* () =
    Lwt_list.iter_s
      (fun {delegate_key; companion_key} ->
        let first_slot = Operation.Consensus.first_slot ~slots delegate_key in
        let attestation =
          Operation.Consensus.attestation
            ~dal_attestation
            ~slot:first_slot
            ~level
            ~round
            ~block_payload_hash
            ()
        in
        let* _op_hash =
          Operation.Consensus.inject
            ~protocol
            ~branch
            ~signer_companion:companion_key
            ~signer:delegate_key
            attestation
            client
        in
        unit)
      faulty_bakers
  in
  Log.info "Attestation injected" ;
  (* Since we manually injected the attestation for the faulty baker,
     we have to prevent [bake_for] to inject another one in the mempool.
  *)
  let all_other_delegates =
    List.map (fun {delegate_key; _} -> delegate_key) other_tz4
    @ Constant.all_secret_keys
  in
  let* () =
    bake_for
      ~delegates:
        (`For
          (List.map
             (fun account -> account.Account.public_key_hash)
             all_other_delegates))
      client
  in
  (* Let's wait 2 levels for the block to be finalized. *)
  let* () = bake_for ~count:2 client in
  (* Now we expect the DAL node to be crafting the denunciation,
     hence it should be included in next block.
  *)
  let* () = bake_for client in
  let* anonymous_ops =
    Node.RPC.call node
    @@ RPC.get_chain_block_operations_validation_pass
         ~block:"head"
         ~validation_pass:2
         ()
  in
  Check.(List.length (JSON.as_list anonymous_ops) = nb_attesting_tz4)
    ~__LOC__
    Check.int
    ~error_msg:"Expected exactly %R anonymous op. Got: %L" ;
  let contents = JSON.(anonymous_ops |=> 0 |-> "contents" |=> 0) in
  let kind = JSON.(contents |-> "kind" |> as_string) in
  let attestation_kind =
    JSON.(contents |-> "attestation" |-> "operations" |-> "kind" |> as_string)
  in
  Check.(
    ((kind, attestation_kind)
    = ("dal_entrapment_evidence", "attestations_aggregate"))
      ~__LOC__
      (tuple2 string string))
    ~error_msg:
      "Expected the anonymous operation to be the denunciation of attestation \
       aggregation. Got: %L" ;
  unit

(* Publishing and attesting should work.
   A producer DAL node publishes "Hello world!" (produced with key [bootstrap1]) on a slot.
   An attestation, which attests the block is emitted with key [bootstrap2].
   The published commitment is attested.*)
let test_producer_attester (_protocol : Protocol.t)
    (dal_params : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let log_step = init_logger () in
  let index = 3 in
  let slot_size = dal_params.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Two blocks are baked, because why not" ;
  let* () = bake_for ~count:2 client in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* lvl_publish = Client.level client in
  let lag = dal_params.attestation_lag in
  Log.info "We are at level %d and the lag is %d" lvl_publish lag ;
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:lag client in
  log_step "It is now time to attest" ;
  (* We want the attestation to be included in the block [lag] after the one containing the shards,
     so we have to inject them now. *)
  let* _ =
    inject_dal_attestations
      ~nb_slots:dal_params.number_of_slots
        (* Since the attestation threshold of the test is set to 1%,
           having only [bootstrap2] signing is sufficient. *)
      ~signers:[Constant.bootstrap2]
      (Slots [index])
      client
  in
  let* lvl_attest = Client.level client in
  Check.(
    (lvl_publish + lag = lvl_attest)
      int
      ~error_msg:"Current level of the client is unexpected") ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For
          (map_alias Constant.[bootstrap1; bootstrap3; bootstrap4; bootstrap5]))
      client
  in
  log_step "Bake a few more blocks before calling RPC" ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted after publication. *)
      @@ get_level_slot_status ~slot_level:(lvl_publish + 1) ~slot_index:index)
  in
  Log.info "Status is %a" Format.pp_print_string status ;
  log_step "Final check." ;
  Check.(
    (status = "attested")
      string
      ~error_msg:"Published slot was supposed to be attested.") ;
  unit

(* Check if the [attester_did_not_attest] warning is correctly emitted.
   This test is a variation of [test_producer_attester] where an attestation
   not attesting the published DAL slot is injected. *)
let test_attester_did_not_attest (_protocol : Protocol.t)
    (dal_params : Dal_common.Parameters.t) (_cryptobox : Cryptobox.t)
    (node : Node.t) (client : Client.t) (_dal_node : Dal_node.t) : unit Lwt.t =
  let log_step = init_logger () in
  let index = 3 in
  let slot_size = dal_params.cryptobox.slot_size in
  log_step "Declaration of a producer" ;
  let producer_node = Dal_node.create ~name:"producer" ~node () in
  let* () = Dal_node.init_config ~operator_profiles:[index] producer_node in
  let* () = Dal_node.run ~wait_ready:true producer_node in
  log_step "Declaration of an attester" ;
  let attester_node = Dal_node.create ~name:"attester" ~node () in
  let* () =
    Dal_node.init_config
      ~peers:[Dal_node.listen_addr producer_node]
      ~attester_profiles:[Constant.bootstrap2.public_key_hash]
      attester_node
  in
  let* () = Dal_node.run ~wait_ready:true attester_node in
  log_step
    "We promise the attester_did_not_attest_slot will be emitted by the \
     [attester node]" ;
  let not_attested_by_bootstrap2_promise =
    Dal_node.wait_for attester_node "dal_attester_did_not_attest.v0" (fun _ ->
        Some ())
  in
  log_step "The producer crafts a commitment and publish it" ;
  let message = Helpers.make_slot ~slot_size "Hello world!" in
  let* _pid =
    Helpers.publish_and_store_slot
      client
      producer_node
      Constant.bootstrap1
      ~index
      message
  in
  let* lvl_publish = Client.level client in
  let lag = dal_params.attestation_lag in
  Log.info "We are at level %d and the lag is %d" lvl_publish lag ;
  (* If one wants to see all the node events, they should use [Node.log_events node] *)
  (* We bake [lag] blocks, one which will include the publication of the commitment
     and [lag-1] just to wait. *)
  log_step "Let's wait for the attestation lag while baking" ;
  let* () = bake_for ~count:lag client in
  log_step
    "Crafting attestation for [bootstrap3] (with expected DAL attestation)." ;
  let* op1 =
    craft_dal_attestation_exn
      ~nb_slots:dal_params.number_of_slots
      ~signer:Constant.bootstrap3
      (Slots [index])
      client
  in
  let* (`OpHash oph1) = Operation.hash op1 client in
  let op1_promise = wait_for_classified oph1 node in
  log_step "Crafting attestation for [bootstrap2] (with empty DAL attestation)." ;
  let* op2 =
    craft_dal_attestation_exn
      ~nb_slots:dal_params.number_of_slots
      ~signer:Constant.bootstrap2
      (Slots [])
      client
  in
  let* (`OpHash oph2) = Operation.hash op2 client in
  let op2_promise = wait_for_classified oph2 node in
  log_step "Injecting the attestations" ;
  let* _ = Operation.inject op1 client in
  let* _ = Operation.inject op2 client in
  log_step "Waiting for the attestations to be classified." ;
  let* () = Lwt.join [op1_promise; op2_promise] in
  let* lvl_attest = Node.get_level node in
  log_step
    "Attestations should be injected in the validated field of the mempool" ;
  let is_operation_in_validated_mempool mempool oph =
    let open JSON in
    let applied_list = as_list (mempool |-> "validated") in
    List.exists (fun e -> e |-> "hash" |> as_string = oph) applied_list
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (List.for_all (is_operation_in_validated_mempool mempool) [oph1; oph2])
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a block containing this attestation" ;
  let map_alias = List.map (fun x -> x.Account.alias) in
  (* Since the function [bake_for] crafts the attestation for delegates,
     we do not want it to craft a concurrent attestation for [bootstrap2].
     One which attests a DAL slot has already been injected. *)
  let* () =
    bake_for
      ~delegates:
        (`For (map_alias Constant.[bootstrap1; bootstrap4; bootstrap5]))
      client
  in
  log_step "Attestation should be included in the head block." ;
  let* meta =
    Node.RPC.(call node @@ get_chain_block_metadata ~block:"head" ())
  in
  let attestation_is_in_block =
    match meta.dal_attestation with
    | Some dal_attests -> dal_attests.(index)
    | None -> false
  in
  Check.is_true
    attestation_is_in_block
    ~error_msg:"Attestation is not included in the head block." ;
  log_step "Mempool should now be empty" ;
  let validated_mempool_is_empty mempool =
    let open JSON in
    let validated_list = as_list (mempool |-> "validated") in
    List.length validated_list = 0
  in
  let* mempool =
    Node.RPC.(call node @@ get_chain_mempool_pending_operations ())
  in
  Check.is_true
    (validated_mempool_is_empty mempool)
    ~error_msg:
      "After injection of the attestations, no operations should remain in the \
       mempool." ;
  log_step "Bake a few more blocks before calling RPC." ;
  let final_block_promise =
    wait_for_layer1_final_block producer_node (lvl_attest + 2)
  in
  let* () = bake_for ~count:3 client in
  let* () = final_block_promise in
  log_step "Call to the RPC of the DAL node to check that the slot is attested." ;
  let* status =
    Dal_RPC.(
      call producer_node
      (* The level the commitment is included in a block is the first one crafted
         after publication. *)
      @@ get_level_slot_status ~slot_level:(lvl_publish + 1) ~slot_index:index)
  in
  Log.info "Status is %a" Format.pp_print_string status ;
  log_step "Final checks." ;
  Check.(
    (status = "attested")
      string
      ~error_msg:"Published slot was supposed to be attested.") ;
  (* If the [not_attested_by_bootstrap2_promise] is not fulfilled yet, it means that
     the [attester_did_not_attest_slot] warning has never been emitted. *)
  let not_attested_by_bootstrap2 =
    match Lwt.state not_attested_by_bootstrap2_promise with
    | Sleep -> false
    | Return () -> true
    | Fail exn ->
        Test.fail "Unexpected exception: '%s'" (Printexc.to_string exn)
  in
  Check.is_true
    ~error_msg:
      "We expected the slot to be protocol attested, but not by this attester \
       node."
    not_attested_by_bootstrap2 ;
  unit

(* [test_duplicate_denunciations] publishes a commitment for slot [0].
   The test then injects a DAL trapped attestation at level [2] and
   after baking [attestation_lag] blocks, it attempts to:

   - inject a first denunciation that is expected to succeed,

   - inject a second denunciation built using a different shard that
     is a trap as well, but using the same slot and the same l1 level,
     where each try is expected to fail. The injection temptatives are
     done:
     1. at the next block level (expecting delegate already denounced)
     2. at the next cycle (expecting delegate already denounced)
     3. at the next-next cycle (expecting outdated evidence)
*)
let test_duplicate_denunciations protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let slot_index = 0 in
  Log.info "Bake two blocks" ;
  let* () = bake_for ~count:2 client in
  let accused = Constant.bootstrap2 in
  let* current_level = Node.get_level node in
  let* shards_with_proofs =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    Log.info "Publish commitment at level %d" current_level ;
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:dal_parameters.attestation_lag client in
  let* current_level = Node.get_level node in
  Log.info "Injecting an attestation at level %d" current_level ;
  let availability = Slots [slot_index] in
  let signer = Constant.bootstrap2 in
  let* attestation, _op_hash =
    inject_dal_attestation_exn
      ~signer
      ~nb_slots:dal_parameters.number_of_slots
      availability
      client
  in
  let* signature = Operation.sign attestation client in
  let attestation = (attestation, signature) in
  let* shard_assignments =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_dal_shards
         ~delegates:[accused.public_key_hash]
         ()
  in
  let shard_indexes =
    JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
  in
  let[@ocaml.warning "-8"] (shard_index_1 :: shard_index_2 :: _) =
    List.map JSON.as_int shard_indexes
  in
  let accusation =
    let shard1, proof1 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_1)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      shard1
      proof1
  in
  let accusation_dup =
    let shard2, proof2 =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index_2)
        shards_with_proofs
      |> Option.get
    in
    Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
      ~protocol
      ~attestation
      ~slot_index
      shard2
      proof2
  in
  let* level_accusation = Node.get_level node in
  Log.info
    "Injecting a first accusation (level %d) expected to succeed"
    level_accusation ;
  let* (`OpHash _) = Operation.Anonymous.inject accusation client in
  let* () = bake_for client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting the second accusation at the next level (level %d), expected to \
     fail because the faulty delegate is already denounced"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation_core.already_dal_denounced
  in
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the \
     delegate is already denounced for this level"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation_core.already_dal_denounced
  in
  let* () = bake_for ~count:blocks_per_cycle client in
  let* current_level = Node.get_level node in
  Log.info
    "Injecting at the next cycle (level %d), expected to fail because the DAL \
     denunciation is outdated"
    current_level ;
  let* (`OpHash _) =
    Operation.Anonymous.inject
      accusation_dup
      client
      ~error:Operation_core.outdated_dal_denunciation
  in
  unit

(* [test_denunciation_next_cycle] injects two trapped attestations and
   inject an accusation for the first one at cycle `c` and another at
   for the second one at cycle `c + 1`, both injections are expected
   to succeed. *)
let test_denunciation_next_cycle protocol dal_parameters cryptobox node client
    _bootstrap_key =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let slot_index = 0 in
  let accused = Constant.bootstrap2 in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  let* () = bake_for ~count:2 client in
  let* shards_with_proofs1 =
    let* current_level = Node.get_level node in
    Log.info "Publish first commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:1 client in
  let* shards_with_proofs2 =
    let* current_level = Node.get_level node in
    Log.info "Publish second commitment at level %d" current_level ;
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    let slot = Cryptobox.Internal_for_tests.generate_slot ~slot_size in
    let commitment, proof, shards_with_proofs =
      Helpers.get_commitment_and_shards_with_proofs cryptobox ~slot
    in
    let* _op_hash_0 =
      Helpers.publish_commitment
        ~source:accused
        ~index:slot_index
        ~commitment
        ~proof
        client
    in
    return shards_with_proofs
  in
  let* () = bake_for ~count:(dal_parameters.attestation_lag - 1) client in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a first attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~signer:Constant.bootstrap2
        ~nb_slots:dal_parameters.number_of_slots
        availability
        client
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs1
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        shard
        proof
    in
    let* level_accusation = Node.get_level node in
    Log.info
      "Injecting a first accusation (level %d) expected to succeed"
      level_accusation ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  let* () = bake_for ~count:1 client in
  let* attestation =
    let* current_level = Node.get_level node in
    Log.info "Injecting a second attestation at level %d" current_level ;
    let availability = Slots [slot_index] in
    let signer = Constant.bootstrap2 in
    let* attestation, _op_hash =
      inject_dal_attestation_exn
        ~signer
        ~nb_slots:dal_parameters.number_of_slots
        availability
        client
    in
    let* signature = Operation.sign attestation client in
    return (attestation, signature)
  in
  let* () =
    let* shard_assignments =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards
           ~delegates:[accused.public_key_hash]
           ()
    in
    let shard_indexes =
      JSON.(shard_assignments |> as_list |> List.hd |-> "indexes" |> as_list)
    in
    let[@ocaml.warning "-8"] (shard_index :: _) =
      List.map JSON.as_int shard_indexes
    in
    let shard, proof =
      Seq.find
        (fun (Cryptobox.{index; _}, _proof) -> index = shard_index)
        shards_with_proofs2
      |> Option.get
    in
    let accusation =
      Operation.Anonymous.dal_entrapment_evidence_standalone_attestation
        ~protocol
        ~attestation
        ~slot_index
        shard
        proof
    in
    let* () = bake_for ~count:blocks_per_cycle client in
    let* level = Node.get_level node in
    Log.info
      "Injecting the second accusation (level %d) expected to succeed"
      level ;
    let* (`OpHash _) = Operation.Anonymous.inject accusation client in
    unit
  in
  unit

(**  [test_e2e_trap_faulty_dal_node] verifies a scenario where a
     [faulty_delegate] misbehaves. Specifically, it:

     - Creates a DAL node [proxy] whose role is to mimick the honest
     [dal_node] by forwarding to the honest DAL node each RPC call
     except for [/profiles/<tz>/attested_levels/<n>/attestable_slots],
     where [<tz>] is the [faulty_delegate] and [<n>] is the level [2 *
     blocks_per_cycle + 1]. For this RPC path, it will declare for the
     [faulty_delegate] each slot attestable by replacing the honest
     DAL node answer field ['attestable_slots_set'] to an array of
     ['true'] values.

     The test proceeds as follows:

     1. Bakes [blocks_per_cycle] blocks to avoid the period in which
     the DAL node is not able to inject an accusation because of the
     accusation delay introduced by the migration.

     2. Bakes [blocks_per_cycle] blocks while publishing on slot index
     [0] and checks that [faulty_delegate] is not denounced at the end.

     3. Bakes again [blocks_per_cycle] blocks to finally reach [3 *
     blocks_per_cycle] blocks.

     - Finally, the test

        - a) retrieves the balance updates for the last block of
             the third cycle and verifies that:
             - Multiple DAL attesting rewards were minted.
             - The delegates that lost the DAL rewards are the [faulty_delegate]
             and the ones that lost the consensus attesting rewards because they
             haven't revealed nonces.

        - b) retrieves the [faulty_delegate] DAL participation at the
             end of the third cycle, minus one block (i.e "head~1")
             because DAL participation are reset at the end of a block
             preceding a new cycle, and checks that:

             - it has a sufficient DAL participation to get rewards,
             - it is denounced,
             - its expected DAL rewards are [0].
*)
let test_e2e_trap_faulty_dal_node protocol dal_parameters _cryptobox node client
    dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  (* This constraint makes the test simpler; it could be lifted. *)
  assert (blocks_per_cycle = dal_parameters.Dal.Parameters.attestation_lag) ;
  let target_attested_level = (2 * blocks_per_cycle) + 1 in
  let faulty_delegate = Constant.bootstrap1.Account.public_key_hash in
  let proxy =
    let routes =
      [
        (let path =
           Re.Str.regexp
           @@ Format.sprintf
                "/profiles/%s/attested_levels/%d/attestable_slots"
                faulty_delegate
                target_attested_level
         in
         Dal_node.Proxy.route ~path ~callback:(fun ~path:_ ~fetch_answer ->
             let open Ezjsonm in
             let* dal_node_answer = fetch_answer () in
             let v = value dal_node_answer in
             let kind = find v ["kind"] |> get_string in
             let new_v =
               if String.equal kind "attestable_slots_set" then
                 let attestable_slots_set =
                   (* Declare each slot attestable. *)
                   find v ["attestable_slots_set"]
                   |> get_list get_bool
                   |> List.map (fun _ -> true)
                   |> list bool
                 in
                 update v ["attestable_slots_set"] (Some attestable_slots_set)
               else v
             in
             return (Some (`Response (value_to_string new_v)))));
      ]
    in
    Dal_node.Proxy.make ~name:"proxy-dal-node" ~routes
  in
  let faulty_dal_node =
    Dal_node.create
      ~name:"faulty-dal-node"
      ~listen_addr:(Dal_node.listen_addr dal_node)
      ~node
      ()
  in
  let () =
    Dal_node.Proxy.run ~honest_dal_node:dal_node ~faulty_dal_node proxy
  in
  let dal_node_endpoint =
    Dal_node.as_rpc_endpoint faulty_dal_node |> Endpoint.as_string
  in
  let* level =
    let* first_level = Node.get_level node in
    let block = string_of_int first_level in
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ~block ()
  in
  let* () =
    bake_for
      ~count:(blocks_per_cycle - 1 - level.cycle_position)
      ~dal_node_endpoint
      client
  in
  let* _ = Node.wait_for_level node blocks_per_cycle in
  let* () =
    let slot_size = dal_parameters.Dal.Parameters.cryptobox.slot_size in
    repeat blocks_per_cycle (fun () ->
        let wait_slot ~published_level ~slot_index:_ =
          wait_for_layer1_final_block dal_node (published_level - 2)
        in
        let* _res =
          publish_store_and_wait_slot
            node
            client
            dal_node
            Constant.bootstrap2
            ~index:0
            ~wait_slot
            ~number_of_extra_blocks_to_bake:0
          @@ Helpers.make_slot ~slot_size "content"
        in
        unit)
  in
  let level = ref (2 * blocks_per_cycle) in
  let* _ = Node.wait_for_level node !level in
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_false
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to not be denounced" ;
  let wait_for_trap_injection =
    Dal_node.wait_for dal_node "dal_trap_injection.v0" (fun e ->
        let open JSON in
        let delegate = e |-> "delegate" |> as_string in
        let attested_level = e |-> "attested_level" |> as_int in
        if delegate = faulty_delegate && attested_level = target_attested_level
        then Some ()
        else None)
  in
  let* () =
    repeat blocks_per_cycle (fun () ->
        let wait = wait_for_layer1_final_block dal_node (!level - 1) in
        let* () = bake_for ~dal_node_endpoint client in
        incr level ;
        let* () = wait in
        unit)
  in
  let* () = wait_for_trap_injection in
  let* _ = Node.wait_for_level node (3 * blocks_per_cycle) in
  let* metadata = Node.RPC.call node @@ RPC.get_chain_block_metadata_raw () in
  let balance_updates = JSON.(metadata |-> "balance_updates" |> as_list) in
  let dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "minted"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "DAL attesting rewards")
      balance_updates
  in
  Check.(List.length dal_rewards > 1)
    ~__LOC__
    Check.int
    ~error_msg:"Expected %R minted DAL-related balance updates, got %L" ;
  let lost_dal_rewards =
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost DAL attesting rewards")
      balance_updates
  in
  let lost_consensus_rewards =
    (* filter those that lost consensus attesting rewards because they haven't
       revealed their nonces *)
    List.filter
      (fun json ->
        JSON.(json |-> "kind" |> as_string) |> String.equal "burned"
        && JSON.(json |-> "category" |> as_string)
           |> String.equal "lost attesting rewards"
        && (not JSON.(json |-> "participation" |> as_bool))
        && JSON.(json |-> "revelation" |> as_bool))
      balance_updates
  in
  let get_delegates =
    List.map (fun json -> JSON.(json |-> "delegate" |> as_string))
  in
  let sort_unique = List.sort_uniq String.compare in
  let losing_delegates = sort_unique @@ get_delegates lost_dal_rewards in
  let expected_to_lose_delegates =
    if Protocol.number protocol >= 023 then
      sort_unique (faulty_delegate :: get_delegates lost_consensus_rewards)
    else [faulty_delegate]
  in
  Check.(expected_to_lose_delegates = losing_delegates)
    ~__LOC__
    Check.(list string)
    ~error_msg:"Unexpected delegates to lose DAL rewards (got %R expected %L)" ;
  let* faulty_delegate_dal_participation =
    Node.RPC.call node
    @@ RPC.get_chain_block_context_delegate_dal_participation
         ~block:"head~1"
         faulty_delegate
  in
  Check.is_true
    faulty_delegate_dal_participation.denounced
    ~__LOC__
    ~error_msg:"Expected the faulty delegate to be denounced" ;
  Check.is_true
    faulty_delegate_dal_participation.sufficient_dal_participation
    ~__LOC__
    ~error_msg:"Expected sufficiant participation for the faulty delegate" ;
  Check.(
    Tez.to_mutez faulty_delegate_dal_participation.expected_dal_rewards = 0)
    ~__LOC__
    Check.int
    ~error_msg:
      "Expected DAL rewards for for the faulty delegate expected to be %R, but \
       got %L" ;
  unit

let get_tb_expected_attesting_rewards l1_node public_key_hash =
  let* participation =
    Node.RPC.(
      call l1_node
      @@ get_chain_block_context_delegate_participation public_key_hash)
  in
  return participation.expected_attesting_rewards

let create_account ?(source = Constant.bootstrap2) ~amount ~alias client =
  Log.info
    "Create a [%s] account: generate a key, inject a transaction that funds \
     it, and bake a block to apply the transaction."
    alias ;
  let* fresh_account = Client.gen_and_show_keys ~alias client in
  let* _oph =
    Operation.Manager.inject_single_transfer
      client
      ~source
      ~dest:fresh_account
      ~amount
  in
  let* () = bake_for client in
  return fresh_account

let create_account_and_reveal ?source ~amount ~alias client =
  let* fresh_account = create_account ?source ~amount ~alias client in
  Log.info "Reveal pkh of [%s] account." alias ;
  let op_reveal =
    Operation.Manager.(make ~source:fresh_account (reveal fresh_account ()))
  in
  let* _oph = Operation.Manager.inject [op_reveal] client in
  let* () = bake_for client in
  return fresh_account

(** [test_dal_rewards_distribution _protocol dal_parameters cryptobox node
    client _dal_node] verifies the correct distribution of DAL rewards among
    delegates based on their participation in DAL attestations activity.

    The test uses the 5 bootstrap accounts and a new account with a small stake
    that has on average one assigned shard per level.

    The main steps of the test are:

    1. Initialize delegates: we assign six accounts to specific roles:
    - **Baker:** Always attests both TenderBake (TB) and all DAL slots.
    - **Attesting DAL Slot 10:** Always attests TB and specifically DAL slot 10.
    - **Not Attesting at All:** Does not attest either TB or DAL slots.
    - **Not Attesting DAL:** Does not attest DAL slots, either by not sending
    any DAL attestation or by sending an empty bitset.
    - **Not Sufficiently Attesting DAL Slot 10:** Attests DAL slot 10 only 25%
    of the time.
    - **Small Baker:** It has very few assigned shards, but does the same as the Baker.

    2. Initial balances snapshot: we capture the initial balances of all
    delegates to compare against post-test balances.

    3. Publish DAL slots and inject DAL attestations: we define a helper
    function to inject DAL attestations based on each delegate's role:
    - we publish a dummy DAL slot at index 10.
    - we inject attestations from each delegate according to their assigned
    behavior.

    4. Blocks production until end of cycle: we bake blocks up to the last
    block of the current cycle, ensuring DAL slot publications and attestations
    are appropriately injected. Each time a new block is produced, we check the
    value of the "dal_attestation" bitset in the block's metadata and count the
    number of times the DAL slot at index 10 is attested.

    5. Second balances snapshot: this is done before TB and DAL rewards are
    distributed at the next block, which marks the end of the current cycle.

    6. Bake the final block of the cycle to trigger the distribution of DAL and
    TB rewards.

    7. Final snapshot of delegates' balances to assess the impact of rewards
    distribution.

    8. Balance Checks:
    - Ensure that delegates who did not attest at all (for both TB & DAL) have
    unchanged balances.
    - Verify that delegates with sufficient DAL participation received the
    expected DAL and TB rewards.
    - Confirm that delegates with insufficient or no DAL participation received
    only the expected TB rewards.

    9. Some extra checks related to /dal_participation RPC:
    - DAL participation status of each delegate matches the expected outcome
    based on their attestation behavior.
    - No delegates are denounced (there is no accuser running actually).
    - The attestable slots field of /dal_participation's result is equal to the
    number of attested slots counted in blocks metadata. *)
let test_dal_rewards_distribution _protocol dal_parameters cryptobox node client
    _dal_node =
  let* proto_params =
    Node.RPC.call node @@ RPC.get_chain_block_context_constants ()
  in
  let blocks_per_cycle = JSON.(proto_params |-> "blocks_per_cycle" |> as_int) in
  assert (blocks_per_cycle >= dal_parameters.Dal.Parameters.attestation_lag) ;
  let consensus_rights_delay =
    JSON.(proto_params |-> "consensus_rights_delay" |> as_int)
  in
  let minimal_stake = JSON.(proto_params |-> "minimal_stake" |> as_int) in
  let nb_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let all_slots = List.init nb_slots Fun.id in
  let number_of_shards =
    dal_parameters.Dal.Parameters.cryptobox.number_of_shards
  in

  (* Compute the stake of the small baker. *)
  let* small_baker_stake =
    let* bootstrap_info =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_delegate
           Constant.bootstrap1.public_key_hash
    in
    let bootstrap_baking_power =
      JSON.(bootstrap_info |-> "baking_power" |> as_string) |> int_of_string
    in
    let total_baking_power =
      bootstrap_baking_power * Array.length Account.Bootstrap.keys
    in
    (* The amount is such that this baker has only one assigned shard per cycles
       (on average). Note that if the baker has no assigned shard, the test
       should still pass, because we just check that it gets its DAL rewards.

       Let [t] be total_baking_power, [s] be [small_baker_stake], and [1/n]
       desired shards fraction for the small baker. We want [s / (t + s) = 1 /
       n], ie [t + s = n * s], ie [t = (n-1) * s] ie, [s = t / (n-1)] *)
    let desired_stake =
      total_baking_power / ((blocks_per_cycle * number_of_shards) - 1)
    in
    let small_baker_stake = max desired_stake minimal_stake in
    Log.info
      "total_baking_power = %d, small_baker_stake = %d"
      total_baking_power
      small_baker_stake ;
    return small_baker_stake
  in

  (* Each of the 5 bootstrap accounts contributes equally to the new baker's
     account. We give it a bit more tez, for it to be able to pay the fees for
     the pk reveal and stake operations. *)
  let to_transfer = (small_baker_stake + 3_000) / 5 in

  let* level = Node.get_level node in
  assert (level < blocks_per_cycle) ;
  let level = ref level in
  let* small_baker =
    create_account_and_reveal ~amount:to_transfer ~alias:"small_baker" client
  in
  incr level ;

  let* () =
    Lwt_list.iter_s
      (fun bootstrap ->
        if bootstrap <> Constant.bootstrap2 then
          let* _oph =
            Operation.Manager.inject_single_transfer
              client
              ~source:bootstrap
              ~dest:small_baker
              ~amount:to_transfer
          in
          unit
        else unit)
      (Array.to_list Account.Bootstrap.keys)
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Register small_baker as a delegate" ;
  let* _small_baker =
    Client.register_delegate ~delegate:small_baker.alias client
  in
  let* () = bake_for client in
  incr level ;

  Log.info "Stake for small_baker" ;
  let* () =
    Client.stake
      (Tez.of_mutez_int small_baker_stake)
      ~staker:small_baker.public_key_hash
      client
  in

  Log.info
    "Bake (almost) %d cycles to activate the delegate"
    consensus_rights_delay ;
  let* () =
    bake_for
      ~count:((blocks_per_cycle * (1 + consensus_rights_delay)) - !level - 1)
      client
  in
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;
  level := blocks_per_cycle * (1 + consensus_rights_delay) ;
  assert (!level = current_level.level) ;

  (* We get our available delegates and assign them different roles they'll play
     during the test. *)
  let ( accounts_list,
        ( baker,
          attesting_dal_slot_10,
          not_attesting_at_all,
          not_attesting_dal,
          not_sufficiently_attesting_dal_slot_10,
          small_baker ) ) =
    match Account.Bootstrap.keys with
    | [|d1; d2; d3; d4; d5|] ->
        ( [
            (d1, "baker");
            (d2, "attesting_dal_slot_10");
            (d3, "not_attesting_at_all");
            (d4, "not_attesting_dal");
            (d5, "not_sufficiently_attesting_dal_slot_10");
            (small_baker, "small_baker");
          ],
          (d1, d2, d3, d4, d5, small_baker) )
    | _ -> Test.fail "Expected exactly 5 bootstrap accounts."
  in

  (* A helper function to get balances of the delegates above. *)
  let snapshot_full_balances_helper () =
    let bal account =
      let* b =
        Client.get_full_balance_for client ~account:account.Account.alias
      in
      return (Tez.mutez_int64 b)
    in
    let* l =
      Lwt.all
        [
          bal baker;
          bal attesting_dal_slot_10;
          bal not_attesting_at_all;
          bal not_attesting_dal;
          bal not_sufficiently_attesting_dal_slot_10;
          bal small_baker;
        ]
    in
    match l with
    | [b1; b2; b3; b4; b5; b6] -> return (b1, b2, b3, b4, b5, b6)
    | _ -> Test.fail "Not reachable."
  in

  (* Snapshot the balances of the accounts at startup. *)
  let* ( _baker_bal0,
         attesting_dal_slot_10_bal0,
         not_attesting_at_all_bal0,
         not_attesting_dal_bal0,
         not_sufficiently_attesting_dal_slot_10_bal0,
         small_baker_bal0 ) =
    snapshot_full_balances_helper ()
  in

  (* This is the main helper function which injects (DAL) attestations for
     delegates depending on their profiles. *)
  let inject_attestations () =
    let count_dal_attesting_bakers = ref 0 in
    (* 1. Baker always attests TB and all DAL slots *)
    let* (_ : Operation_core.t * [`OpHash of peer_id]) =
      (* The baker delegate will miss 1/10 of its DAL attestations and will send
         [No_dal_attestation] in this case. *)
      let baker_attestation =
        if !level mod 10 = 0 then No_dal_attestation
        else (
          incr count_dal_attesting_bakers ;
          Slots all_slots)
      in
      inject_dal_attestation_exn
        ~signer:baker
        ~nb_slots
        baker_attestation
        client
    in
    (* 2. attesting_dal_slot_10 always attests TB and DAL slot 10 *)
    let* (_ : Operation_core.t * [`OpHash of peer_id]) =
      (* The attesting_dal_slot_10 delegate misses 1/11th of its DAL
         attestations and will send Slots [], but it should be fine for its
         rewards. *)
      let attestation =
        if !level mod 11 = 0 then Slots []
        else (
          incr count_dal_attesting_bakers ;
          Slots [10])
      in
      inject_dal_attestation_exn
        ~signer:attesting_dal_slot_10
        ~nb_slots
        attestation
        client
    in
    (* 3. not_attesting_at_all is not attesting neither TB nor DAL slots *)
    (* 4. not_attesting_dal either sends no DAL content or sends bitset 0 *)
    let* (_ : Operation_core.t * [`OpHash of peer_id]) =
      let dal_attestation =
        if !level mod 2 = 0 then No_dal_attestation else Slots []
      in
      inject_dal_attestation_exn
        ~signer:not_attesting_dal
        ~nb_slots
        dal_attestation
        client
    in
    (* 5. not_sufficiently_attesting_dal_slot_10: is attesting DAL slot 10, but only 25%
       of the time. *)
    let* (_ : Operation_core.t * [`OpHash of peer_id]) =
      let slots_to_attest =
        if !level mod 4 = 0 then (
          incr count_dal_attesting_bakers ;
          Slots [10])
        else No_dal_attestation
      in
      inject_dal_attestation_exn
        ~signer:not_sufficiently_attesting_dal_slot_10
        ~nb_slots
        slots_to_attest
        client
    in
    (* 6. small_baker: is always attesting DAL slot 10. *)
    let* () =
      let slots_to_attest = Slots [10] in
      let* res =
        inject_dal_attestation
          ~signer:small_baker
          ~nb_slots
          slots_to_attest
          client
      in
      (match res with
      | None ->
          Log.info
            "At level %d, %s could not TB attest"
            !level
            small_baker.alias
      | Some _ -> incr count_dal_attesting_bakers) ;
      unit
    in
    Log.info
      "At level %d, there are %d bakers that DAL attested"
      !level
      !count_dal_attesting_bakers ;
    unit
  in

  (* We'll count the number of times we see that slot 10 is attested in blocks
     metadata. We'll then check the final value against the
     delegate_attestable_dal_slots field of /dal_participation for each
     delegate. *)
  let count_set_dal_attestation_bitset = ref 0 in

  let count_slot_10_if_attested () =
    let* block_json = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
    let block_dal_attestation_bitset =
      JSON.(block_json |-> "dal_attestation" |> as_int)
    in
    Log.info
      "At level %d, dal_attestation = %d"
      !level
      block_dal_attestation_bitset ;
    (* count when slot 10 is attested *)
    if block_dal_attestation_bitset = 1024 then
      incr count_set_dal_attestation_bitset ;
    unit
  in

  (* This is the main entry of the test: we start by baking a number of blocks
     in which we inject DAL slots publications at slot index 10 and TB/DAL
     attestations. We stop before baking the last block of the current cycle,
     where DAL rewards are distributed. *)
  let* () =
    repeat (blocks_per_cycle - 1) (fun () ->
        let* () = count_slot_10_if_attested () in
        let* (`OpHash _oph1) =
          publish_dummy_slot
            ~source:baker
            ~index:10
            ~message:"hi!"
            cryptobox
            client
        in
        let* () = inject_attestations () in
        let* () =
          (* Do not use [bake_for] because it also injects an attestation
             operation for [baker]. *)
          Client.propose_for_and_wait
            ~key:[baker.Account.public_key_hash]
            client
        in
        incr level ;
        let* manager_ops =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:3
               ()
        in
        Check.(
          (List.length @@ JSON.as_list manager_ops = 1)
            int
            ~__LOC__
            ~error_msg:"expected 1 manager operation in block, got %L") ;
        let* attestations =
          Node.RPC.call node
          @@ RPC.get_chain_block_operations_validation_pass
               ~validation_pass:0
               ()
        in
        let num_attestations = List.length @@ JSON.as_list attestations in
        (* The small baker may not have rights to inject its attestation *)
        let check_num = num_attestations = 4 || num_attestations = 5 in
        Check.is_true
          check_num
          ~__LOC__
          ~error_msg:"expected 4 or 5 attestations in block" ;
        unit)
  in

  (* After this first round of blocks, we snapshot the balances of our delegates
     again. *)
  let* ( _baker_bal1,
         attesting_dal_slot_10_bal1,
         not_attesting_at_all_bal1,
         not_attesting_dal_bal1,
         not_sufficiently_attesting_dal_slot_10_bal1,
         small_baker_bal1 ) =
    snapshot_full_balances_helper ()
  in

  let* () = count_slot_10_if_attested () in

  let* bootstrap_accounts_participation =
    Lwt_list.map_s
      (fun (account, _account_role) ->
        let* dal_participation =
          Node.RPC.call node
          @@ RPC.get_chain_block_context_delegate_dal_participation
               account.Account.public_key_hash
        in
        return (account, dal_participation))
      accounts_list
  in

  (* We use the 'participation' RPC to get the expected Tenderbake rewards of delegate
     who TB-attested sufficiently. *)
  let snapshot_tb_participation () =
    let participation account =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_delegate_participation
           account.Account.public_key_hash
    in
    let* l =
      Lwt.all
        [
          participation baker;
          participation attesting_dal_slot_10;
          participation not_attesting_at_all;
          participation not_attesting_dal;
          participation not_sufficiently_attesting_dal_slot_10;
          participation small_baker;
        ]
    in
    match l with
    | [p1; p2; p3; p4; p5; p6] -> return (p1, p2, p3, p4, p5, p6)
    | _ -> Test.fail "Not reachable."
  in
  let* ( _baker_tb_participation,
         attesting_dal_slot_10_tb_participation,
         _not_attesting_at_all_tb_participation,
         not_attesting_dal_tb_participation,
         not_sufficiently_attesting_dal_slot_10_tb_participation,
         small_baker_tb_participation ) =
    snapshot_tb_participation ()
  in

  (* We now bake the last block of the cycle, which should trigger TB and DAL
     rewards distribution. TB rewards are actually set to 0. *)
  let* () = bake_for ~delegates:(`For [baker.Account.public_key_hash]) client in
  let* _json = Node.RPC.(call node @@ get_chain_block_metadata_raw ()) in
  incr level ;
  let* current_level =
    Node.RPC.call node @@ RPC.get_chain_block_helper_current_level ()
  in
  assert (current_level.cycle_position = blocks_per_cycle - 1) ;

  (* We snapshot the balances of the delegates at the end of the cycle. *)
  let* ( _baker_bal2,
         attesting_dal_slot_10_bal2,
         not_attesting_at_all_bal2,
         not_attesting_dal_bal2,
         not_sufficiently_attesting_dal_slot_10_bal2,
         small_baker_bal2 ) =
    snapshot_full_balances_helper ()
  in

  (* We're now ready to do some checks, both on balances and on
     dal_participation RPC's result. *)
  (* First, we define a function to check expected balances of delegates. *)
  let check_bal_incr ~__LOC__ account bal_before bal_after ~delta =
    let error_msg =
      if delta = 0 then
        "account " ^ account.Account.public_key_hash
        ^ ", expected balance to be unchanged. Got %R, expecting %L"
      else
        "account " ^ account.Account.public_key_hash
        ^ ", unexpected balance: got %R, expecting %L = "
        ^ Int64.to_string bal_before ^ " + " ^ string_of_int delta
    in
    Check.(Int64.(add bal_before (of_int delta)) = bal_after)
      ~__LOC__
      Check.int64
      ~error_msg
  in

  (* Except for the baker, we don't expect the balances of the delegates to
     change before baking the last block of the current cycle. *)
  List.iter
    (fun (account, bal0, bal1) ->
      check_bal_incr ~__LOC__ account bal0 bal1 ~delta:0)
    [
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal0,
        attesting_dal_slot_10_bal1 );
      ( not_attesting_at_all,
        not_attesting_at_all_bal0,
        not_attesting_at_all_bal1 );
      (not_attesting_dal, not_attesting_dal_bal0, not_attesting_dal_bal1);
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal0,
        not_sufficiently_attesting_dal_slot_10_bal1 );
      (small_baker, small_baker_bal0, small_baker_bal1);
    ] ;

  (* As all delegates except [small_baker] have the same stake distribution,
     they're expected to have the same number of the assigned shards, to have
     shards at the same level, to have the same rewards allocated, ... *)
  let () =
    match List.rev bootstrap_accounts_participation with
    | (_small_baker, _) :: (_account, dal_part) :: rest ->
        List.iter
          (fun (_account, dal_participation) ->
            (* We transferred funds from [bootstrap2] to [small_baker], so
               [bootstrap_2] has a smaller baking power than the other bootstrap
               delegates. *)
            Check.(
              dal_part.expected_assigned_shards_per_slot
              = dal_participation.RPC.expected_assigned_shards_per_slot)
              ~__LOC__
              Check.int
              ~error_msg:"expected_assigned_shards_per_slot mismatch" ;
            Check.(
              Tez.to_mutez dal_part.expected_dal_rewards
              = Tez.to_mutez dal_participation.expected_dal_rewards)
              ~__LOC__
              Check.int
              ~error_msg:"expected_dal_rewards mismatch" ;
            Check.(
              dal_part.delegate_attestable_dal_slots
              = dal_participation.delegate_attestable_dal_slots)
              ~__LOC__
              Check.int
              ~error_msg:"delegate_attestable_dal_slots mismatch")
          rest
    | _ -> Test.fail "Not reachable."
  in

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest at all didn't change.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2) ->
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta:0 ;
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.is_false
        dal_participation.sufficient_dal_participation
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", expected to have insufficient DAL participation."))
    [
      ( not_attesting_at_all,
        not_attesting_at_all_bal1,
        not_attesting_at_all_bal2 );
    ] ;

  (* After baking the last block of the cycle, we check that:
     - the balances of the delegates who didn't attest DAL sufficiently or
       didn't attest DAL at all only increased by a delta equal to the expected
       Tenderbake attestation rewards.
     - the participation RPC's result is aligned with the first check. *)
  List.iter
    (fun (account, bal1, bal2, tb_participation, sufficient_dal_participation) ->
      let dal_participation =
        List.assoc account bootstrap_accounts_participation
      in
      Check.(
        (dal_participation.sufficient_dal_participation
       = sufficient_dal_participation)
          ~__LOC__
          bool
          ~error_msg:
            ("account " ^ account.Account.public_key_hash
           ^ ", expected to have sufficient DAL participation? %R, but got %L")) ;
      let expected_dal_rewards =
        if sufficient_dal_participation then
          Tez.to_mutez dal_participation.expected_dal_rewards
        else 0
      in
      let delta =
        Tez.to_mutez tb_participation.RPC.expected_attesting_rewards
        + expected_dal_rewards
      in
      Log.info
        "[check] %s %s: %Ld = %Ld + %d + %d"
        account.Account.alias
        account.public_key_hash
        bal1
        bal2
        (Tez.to_mutez tb_participation.expected_attesting_rewards)
        expected_dal_rewards ;
      check_bal_incr ~__LOC__ account bal1 bal2 ~delta)
    [
      ( not_attesting_dal,
        not_attesting_dal_bal1,
        not_attesting_dal_bal2,
        not_attesting_dal_tb_participation,
        false );
      ( not_sufficiently_attesting_dal_slot_10,
        not_sufficiently_attesting_dal_slot_10_bal1,
        not_sufficiently_attesting_dal_slot_10_bal2,
        not_sufficiently_attesting_dal_slot_10_tb_participation,
        false );
      ( attesting_dal_slot_10,
        attesting_dal_slot_10_bal1,
        attesting_dal_slot_10_bal2,
        attesting_dal_slot_10_tb_participation,
        true );
      ( small_baker,
        small_baker_bal1,
        small_baker_bal2,
        small_baker_tb_participation,
        true );
    ] ;

  (* As a final check, we verify that no delegate is denounced and that we
     report the correct number of attested slots. *)
  List.iter
    (fun (account, dal_participation) ->
      Check.is_false
        dal_participation.RPC.denounced
        ~__LOC__
        ~error_msg:
          ("account " ^ account.Account.public_key_hash
         ^ ", not expected to be denounced.") ;

      (* The number of attestable slots for the small stake baker is smaller
         than for the other bakers and since we did not count them, we don't
         check them. *)
      if not @@ String.equal account.alias small_baker.alias then
        Check.(
          dal_participation.delegate_attestable_dal_slots
          = !count_set_dal_attestation_bitset)
          ~__LOC__
          Check.int
          ~error_msg:
            "Expecting %L attestable DAL slots, but %R were reported in blocks \
             metadata")
    bootstrap_accounts_participation ;
  unit

let use_mockup_node_for_getting_attestable_slots _protocol dal_parameters
    cryptobox l1_node client _bootstrap_key =
  let number_of_slots = dal_parameters.Dal.Parameters.number_of_slots in
  let attestation_lag = dal_parameters.attestation_lag in

  Log.info "Start the mocked DAL node" ;
  let dal_node_mockup =
    let attesters =
      Account.Bootstrap.keys |> Array.to_list
      |> List.map (fun b -> b.Account.public_key_hash)
    in
    let attestable_slots ~attester:_ ~attested_level:_ =
      List.init number_of_slots (fun _0 -> true)
    in
    Dal_node.Mockup_for_baker.make
      ~name:"mock-dal-node"
      ~attestation_lag
      ~attesters
      ~attestable_slots
  in
  let port = Port.fresh () in
  let () = Dal_node.Mockup_for_baker.run dal_node_mockup ~port in
  let dal_node_rpc_endpoint =
    Endpoint.make ~host:"localhost" ~scheme:"http" ~port ()
  in
  let baker = Agnostic_baker.create ~dal_node_rpc_endpoint l1_node client in

  Log.info "Publish a slot" ;
  let* (`OpHash _op_hash) =
    publish_dummy_slot
      ~source:Constant.bootstrap1
      ~index:0
      ~message:"a"
      cryptobox
      client
  in
  let* publish_level =
    let* op_level = Node.get_level l1_node in
    return @@ (op_level + 1)
  in

  Log.info "Start the baker" ;
  let* () = Agnostic_baker.run baker in

  (* +2 blocks for the attested block to be final, +1 for some slack *)
  let* _ = Node.wait_for_level l1_node (publish_level + attestation_lag + 3) in
  let* () = Agnostic_baker.terminate baker in
  let () = Dal_node.Mockup_for_baker.stop dal_node_mockup in

  let attested_level = publish_level + attestation_lag in
  Log.info
    "Check that the slot published at level %d was attested at level %d"
    publish_level
    attested_level ;
  let* {dal_attestation; _} =
    Node.RPC.(
      call l1_node
      @@ get_chain_block_metadata ~block:(string_of_int attested_level) ())
  in
  Check.((Some [|true|] = dal_attestation) (option (array bool)))
    ~error_msg:"Unexpected DAL attestation: expected %L, got %R" ;
  unit

let test_disable_shard_validation_wrong_cli _protocol _parameters _cryptobox
    _node _client dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
         ".* DAL shard validation is disabled but the option \
          '--disable-shard-validation' was not provided.*")

let test_disable_shard_validation_wrong_env _protocol _parameters _cryptobox
    _node _client dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* DAL shard validation is enabled but the environment variable %s \
            was not set.*"
           Dal_node.disable_shard_validation_environment_variable)

let test_ignore_topics_wrong_cli _protocol _parameters _cryptobox _node _client
    dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* The environment variable to ignore topics %s was set, but the \
            option '--ignore-topics' was not provided.*"
           Dal_node.ignore_topics_environment_variable)

let test_ignore_topics_wrong_env _protocol _parameters _cryptobox _node _client
    dal_node =
  Dal_node.check_error
    dal_node
    ~msg:
      (rex
      @@ Format.sprintf
           ".* The option '--ignore-topics' was provided, but the environment \
            variable to ignore topics %s was not set.*"
           Dal_node.ignore_topics_environment_variable)

let register ~protocols =
  (* Tests with Layer1 node only *)
  scenario_with_layer1_node
    ~additional_bootstrap_accounts:1
    "dal basic logic"
    test_slot_management_logic
    protocols ;
  scenario_with_layer1_node
    "attesters receive expected DAL rewards depending on participation"
    test_dal_rewards_distribution
    (List.filter (fun p -> Protocol.number p >= 022) protocols)
    (* We set attestation threshold to 30% because we'll have 2 regular bakers
       who attest sufficiently. *)
    ~attestation_threshold:30
    ~attestation_lag:2
    ~blocks_per_cycle:16
    ~blocks_per_commitment:16 ;
  scenario_with_layer1_node
    ~attestation_lag:5
    "slots attestation operation behavior"
    test_slots_attestation_operation_behavior
    protocols ;
  (* We want to test that the number of slots following mainnet
     parameters can be included into one block. We hard-code the
     mainnet value. It could be extended to higher values if
     desired. *)
  scenario_with_layer1_node
    ~regression:true
    ~number_of_slots:32
    ~additional_bootstrap_accounts:(32 - Array.length Account.Bootstrap.keys)
    "Use all available slots"
    test_all_available_slots
    protocols ;
  scenario_with_layer1_node
    "slots attestation operation dal committee membership check"
    test_slots_attestation_operation_dal_committee_membership_check
    (* We need to set the prevalidator's event level to [`Debug]
       in order to capture the errors thrown in the validation phase. *)
    ~event_sections_levels:[("prevalidator", `Debug)]
    ~consensus_committee_size:1024
    protocols ;
  scenario_with_layer1_node
    "one_committee_per_level"
    test_one_committee_per_level
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    "slot is protocol attested even if attestations are aggregated"
    test_aggregation_required_to_pass_quorum
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~traps_fraction:Q.one
    "inject accusation"
    test_inject_accusation
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject accusation of aggregated attestation"
    (test_inject_accusation_aggregated_attestation 1)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~traps_fraction:Q.one
    ~operator_profiles:[0]
    "inject several accusations for the same aggregated attestation"
    (test_inject_accusation_aggregated_attestation 2)
    (List.filter (fun p -> Protocol.number p >= 023) protocols) ;
  scenario_with_layer1_node
    ~traps_fraction:Q.one
    "inject a duplicated denunciation at different steps"
    test_duplicate_denunciations
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_node
    ~traps_fraction:Q.one
    "inject a denunciation at the next cycle"
    test_denunciation_next_cycle
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  (* Tests with layer1 and dal nodes *)
  scenario_with_layer1_and_dal_nodes
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~traps_fraction:Q.one
    "faulty DAL node entrapment"
    test_e2e_trap_faulty_dal_node
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  test_dal_node_startup protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    "dal node slot management"
    test_dal_node_slot_management
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6]
    "dal node slot headers tracking"
    test_dal_node_slots_headers_tracking
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    "dal node shard fetching and slot reconstruction"
    test_dal_node_rebuild_from_shards
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["rpc"]
    ~regression:true
    ~prover:false
    "dal node list RPCs"
    test_dal_node_rpc_list
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    "dal node POST /slots"
    test_dal_node_test_post_slot
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    "dal node GET /levels/<level>/slots/<index>/content"
    test_dal_node_test_get_level_slot_content
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~prover:false
    "dal node PATCH+GET /profiles"
    test_dal_node_test_patch_profile
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~prover:false
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
    test_dal_node_get_assigned_shard_indices
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "dal node GET \
     /profiles/<public_key_hash>/attested_levels/<level>/attestable_slots"
    ~operator_profiles:[0; 1; 2]
    test_dal_node_get_attestable_slots
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~attestation_threshold:100
    ~number_of_slots:8
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6; 7]
    "dal attester with bake for"
    test_attester_with_bake_for
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~attestation_threshold:100
    ~attestation_lag:16
    ~activation_timestamp:Now
    ~number_of_slots:8
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6; 7]
    "dal attester with baker daemon"
    test_attester_with_daemon
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["snapshot"; "import"]
    ~operator_profiles:[0]
    "dal node import snapshot"
    test_dal_node_import_snapshot
    protocols ;

  (* Tests with layer1 and dal nodes (with p2p/GS) *)
  scenario_with_layer1_and_dal_nodes
    ~prover:false
    ~tags:["gossipsub"]
    "GS/P2P connection and disconnection"
    test_dal_node_p2p_connection_and_disconnection
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~prover:false
    ~tags:["gossipsub"]
    "GS join topic"
    test_dal_node_join_topic
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    ~operator_profiles:[0]
    "GS valid messages exchange"
    test_dal_node_gs_valid_messages_exchange
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"]
    "GS invalid messages exchange"
    ~operator_profiles:[0]
    test_dal_node_gs_invalid_messages_exchange
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    "Attester attests produced slot"
    test_producer_attester
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~attestation_threshold:1
    ~l1_history_mode:Default_with_refutation
    ~event_sections_levels:[("prevalidator", `Debug)]
    "attester_did_not_attest warning is emitted"
    test_attester_did_not_attest
    protocols ;
  scenario_with_layer1_and_dal_nodes
    "baker registers profiles with dal node"
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~activation_timestamp:Now
    ~prover:false
    test_baker_registers_profiles
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["bootstrap"]
    ~bootstrap_profile:true
    ~prover:false
    ~l1_history_mode:Default_with_refutation
    "peer discovery via bootstrap node"
    test_peer_discovery_via_bootstrap_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gossipsub"; "rpc"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    "GS/RPC get_connections"
    test_rpc_get_connections
    protocols ;

  scenario_with_layer1_and_dal_nodes
    ~tags:["bootstrap"; "trusted"; "connection"]
    ~bootstrap_profile:true
    "trusted peers reconnection"
    ~prover:false
    ~l1_history_mode:Default_with_refutation
    test_peers_reconnection
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["operator"; "profile"]
    "operator profile"
    ~prover:false
    test_operator_profile
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["attestation"; "p2p"]
    ~attestation_threshold:100
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    "attestation through p2p"
    test_attestation_through_p2p
    protocols ;
  History_rpcs.test_commitments_history_rpcs protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["amplification"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~redundancy_factor:2
      (* With a redundancy factor of 4 or more, not much luck is
         needed for a bootstrap account (with 1/5 of the stake) to be
         assigned enough shards to reconstruct alone. Since the
         redundancy factor must be a power of 2, we use 2 which means
         that half of the shards are needed to perform a
         reconstruction. *)
    ~number_of_slots:1
    "banned attesters receive their shards thanks to amplification"
    Amplification.test_amplification
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["amplification"; "simple"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
      (* In this test, receiving all shards should happen before amplification
         starts. With [minimal_block_delay = 1], it may start 1 second after the
         first shard is received, which results in flakiness. With
         [minimal_block_delay = 3], it may start only after 4s (see !18139),
         which should be enough time for the observer to receive all the
         shards. *)
    ~minimal_block_delay:"3"
    "observer triggers amplification (without lost shards)"
    Amplification.test_amplification_without_lost_shards
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gc"; "simple"]
    ~operator_profiles:[0]
    ~number_of_slots:1
    "garbage collection of shards for producer"
    Garbage_collection.test_gc_simple_producer
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gc"; "attester"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "garbage collection of shards for producer and attester"
    Garbage_collection.test_gc_producer_and_attester
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["gc"; "multi"]
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "garbage collection of shards for all profiles"
    Garbage_collection.test_gc_with_all_profiles
    protocols ;
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7561
     Adapt the test for the SQLite3 backend. *)
  Garbage_collection.test_gc_skip_list_cells ~protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["crawler"; "reconnection"]
    "DAL node crawler reconnects to L1 without crashing (non-producer case)"
    ~prover:false
    test_dal_node_crawler_reconnects_to_l1
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["crawler"; "reconnection"]
    "DAL node crawler reconnects to L1 without crashing (producer case)"
    ~operator_profiles:[0]
    test_dal_node_crawler_reconnects_to_l1
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~bootstrap_profile:true
    ~l1_history_mode:Default_with_refutation
    ~number_of_slots:1
    "new attester attests"
    test_new_attester_attests
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~number_of_slots:1
    ~operator_profiles:[0]
    ~regression:true
    "attesters receive DAL rewards"
    test_attesters_receive_dal_rewards
    (List.filter (fun p -> Protocol.number p >= 022) protocols) ;
  scenario_with_layer1_and_dal_nodes
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~tags:["restart"]
    ~activation_timestamp:Now
    ~operator_profiles:[0]
    ~l1_history_mode:(Custom (Rolling (Some 5)))
    "restart DAL node (producer)"
    test_restart_dal_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~tags:["restart"]
    ~activation_timestamp:Now
    ~bootstrap_profile:true
    "restart DAL node (bootstrap)"
    test_restart_dal_node
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["http"; "backup"; "retrievability"; Tag.extra; Tag.memory_hungry]
    ~operator_profiles:[0]
    ~l1_history_mode:(Custom Node.Archive)
    ~history_mode:Full
    ~number_of_slots:8
    ~attestation_threshold:0
    "fetching slots from backup sources"
    dal_slots_retrievability
    protocols ;

  (* Tests with all nodes *)
  scenario_with_all_nodes
    ~operator_profiles:[0; 1; 2; 3; 4; 5; 6]
    "rollup_node_applies_dal_pages"
    rollup_node_applies_dal_pages
    protocols ;
  scenario_with_all_nodes
    ~operator_profiles:[0]
    "test reveal_dal_page in fast exec wasm pvm"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel])
    ~pvm_name:"wasm_2_0_0"
    ~number_of_shards:256
    ~slot_size:(1 lsl 15)
    ~redundancy_factor:8
    ~attestation_lag:4
    ~page_size:128
    test_reveal_dal_page_in_fast_exec_wasm_pvm
    protocols ;
  scenario_with_all_nodes
    "test tx_kernel"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.tx_kernel_dal])
    ~pvm_name:"wasm_2_0_0"
    ~operator_profiles:[0]
    ~number_of_shards:256
    ~slot_size:(1 lsl 15)
    ~redundancy_factor:8
    ~page_size:128
    ~attestation_lag:4
    Tx_kernel_e2e.test_tx_kernel_e2e
    protocols ;
  scenario_with_all_nodes
    "test echo_kernel"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel])
    ~pvm_name:"wasm_2_0_0"
    ~slot_size:2048
    ~page_size:256
    ~number_of_shards:64
    ~operator_profiles:[0]
    Tx_kernel_e2e.test_echo_kernel_e2e
    protocols ;
  (* This test only asserts the echo kernel used by tezt-cloud scenarios is
     correct. It isn't meant to be ran by the CI. *)
  scenario_with_all_nodes
    "test echo_kernel_for_bandwidth"
    ~uses:(fun _protocol ->
      [Constant.smart_rollup_installer; Constant.WASM.dal_echo_kernel_bandwidth])
    ~pvm_name:"wasm_2_0_0"
    ~slot_size:2048
    ~page_size:256
    ~number_of_shards:64
    ~operator_profiles:[0]
    ~tags:[Tag.ci_disabled]
    ~regression:false
    Tx_kernel_e2e.test_manual_echo_kernel_for_bandwidth
    protocols ;

  (* Register tutorial test *)
  scenario_tutorial_dal_baker protocols ;

  Refutations
  .register_scenario_with_two_rollups_a_faulty_dal_node_and_a_correct_one
    "Refutation where the faulty node timeouts"
    ~refute_operations_priority:`Faulty_first
    protocols ;

  Refutations
  .register_scenario_with_two_rollups_a_faulty_dal_node_and_a_correct_one
    "Refutation where the honest node makes final move"
    ~refute_operations_priority:`Honest_first
    protocols ;

  scenario_with_all_nodes
    "Rollup injects DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_node_injects_dal_slots
    ~operator_profiles:[0]
      (* It it sufficient for a single baker here to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  scenario_with_all_nodes
    "Rollup batches and injects optimal DAL slots"
    ~regression:false
    ~pvm_name:"wasm_2_0_0"
    ~commitment_period:5
    rollup_batches_and_publishes_optimal_dal_slots
    ~operator_profiles:[0; 1; 2]
      (* It it sufficient for a single baker to receive some shards here to
         declare the slot available. Otherwise the test might be flaky as we
         bake with a timestamp in the past. *)
    ~attestation_threshold:1
    protocols ;

  (* Register end-to-end tests *)
  register_end_to_end_tests ~protocols ;
  dal_crypto_benchmark () ;
  scenario_with_layer1_node
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
    ~activation_timestamp:Now
    "mockup get_attestable_slots"
    use_mockup_node_for_getting_attestable_slots
    protocols ;

  (* Scenarios for disabling shard validation *)
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    ~wait_ready:false
    ~env:
      (String_map.singleton
         Dal_node.disable_shard_validation_environment_variable
         "yes")
    "DAL node disable shard validation wrong CLI"
    test_disable_shard_validation_wrong_cli
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    ~wait_ready:false
    ~disable_shard_validation:true
    "DAL node disable shard validation wrong env"
    test_disable_shard_validation_wrong_env
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    ~wait_ready:true
    ~env:
      (String_map.singleton
         Dal_node.disable_shard_validation_environment_variable
         "yes")
    ~disable_shard_validation:true
    "DAL node disable shard validation correct CLI"
    (fun _protocol _parameters _cryptobox _node _client dal_node ->
      Dal_node.terminate dal_node)
    protocols ;

  (* Scenarios for --ignore-topics *)
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    ~wait_ready:false
    ~env:
      (String_map.singleton Dal_node.ignore_topics_environment_variable "yes")
    "DAL node ignore topics wrong CLI"
    test_ignore_topics_wrong_cli
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~operator_profiles:[0]
    ~wait_ready:false
    ~ignore_pkhs:
      [
        Constant.bootstrap1.Account.public_key_hash;
        Constant.bootstrap2.Account.public_key_hash;
      ]
    "DAL node ignore topics wrong env"
    test_ignore_topics_wrong_env
    protocols ;
  scenario_with_layer1_and_dal_nodes
    ~tags:["amplification"; "ignore_topics"]
    ~bootstrap_profile:true
    ~wait_ready:true
    "Test amplification by ignoring topics"
    Amplification.test_by_ignoring_topics
    protocols

let tests_start_dal_node_around_migration ~migrate_from ~migrate_to =
  let offsets = [-2; -1; 0; 1; 2] in
  let tests ~migrate_from ~migrate_to ~check_rpc =
    List.iter
      (fun offset ->
        test_start_dal_node_around_migration
          ~migrate_from
          ~migrate_to
          ~offset
          ~check_rpc)
      offsets
  in
  tests ~migrate_from ~migrate_to ~check_rpc:true

let register_migration ~migrate_from ~migrate_to =
  test_migration_plugin ~migration_level:4 ~migrate_from ~migrate_to ;
  History_rpcs.test_commitments_history_rpcs_with_migration
    ~migration_level:10
    ~migrate_from
    ~migrate_to ;
  tests_start_dal_node_around_migration ~migrate_from ~migrate_to ;
  test_migration_accuser_issue ~migration_level:4 ~migrate_from ~migrate_to ;
  test_migration_parameters ~migration_level:3 ~migrate_from ~migrate_to

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: debug print store schemas"
    ~tags:["dal"; "store"; "schemas"]
    ~uses:[Constant.octez_dal_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Dal_node.debug_print_store_schemas ~hooks ()

let () =
  Regression.register
    ~__FILE__
    ~title:"DAL Node: P2P message encoding"
    ~tags:["dal"; "gossipsub"; "p2p"]
    ~uses:[Constant.octez_codec]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* output = Codec.describe_binary_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  let* output = Codec.describe_json_schema ~id:"dal_p2p_message" () in
  Regression.capture output ;
  unit
