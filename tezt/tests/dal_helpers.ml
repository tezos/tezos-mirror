(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Shared helpers, types, and scenario builders for DAL integration tests.
   Used by all dal_*.ml test files via [open Dal_helpers]. *)

(* --- Module aliases and constants --- *)

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

(* --- Logging and utilities --- *)

type logger = {log_step : 'a. ('a, Format.formatter, unit, unit) format4 -> 'a}

let init_logger () : logger =
  let counter = ref 1 in
  let log_step fmt =
    let color = Log.Color.(bold ++ FG.blue) in
    let prefix = "step-" ^ string_of_int !counter in
    incr counter ;
    Log.info ~color ~prefix fmt
  in
  {log_step}

(* Returns [i; i+1; ...; j] *)
let rec ( --> ) i j = if i > j then [] else i :: (succ i --> j)

(* --- Store helpers --- *)

let store_path dal_node store_kind =
  Format.sprintf
    "/%s/store/%s_store"
    (Dal_node.data_dir dal_node)
    (match store_kind with
    | `Slots -> "slot"
    | `Shards -> "shard"
    | `Skip_list -> "skip_list")

let data_path store_path store_kind ~slot_size ~published_level ~slot_index =
  match store_kind with
  | `Slots ->
      Format.sprintf
        "%s/%d_%d_%d"
        store_path
        published_level
        slot_index
        slot_size
  | `Shards -> Format.sprintf "%s/%d_%d" store_path published_level slot_index
  | `Skip_list ->
      failwith
        "Skip-list are store in an sqlite DB; cannot access data this way"

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
  let published_levels = ref [] in
  let query = "SELECT published_level FROM skip_list_slots;" in
  let res =
    Sqlite3.exec_not_null_no_headers
      ~cb:(fun row ->
        let published_level = row.(0) in
        published_levels := published_level :: !published_levels)
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
    List.sort_uniq String.compare !published_levels
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

(* --- Event waiting helpers --- *)

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

let wait_for_cached_slot ~shard_index dal_node ~published_level ~slot_index =
  let check_slot_id e =
    JSON.(e |-> "published_level" |> as_int) = published_level
    && JSON.(e |-> "slot_index" |> as_int) = slot_index
  in
  let check_shard_index e =
    JSON.(e |-> "shard_index" |> as_int) = shard_index
  in
  Dal_node.wait_for dal_node "dal_cached_slot_shard.v0" (fun e ->
      if check_slot_id e && check_shard_index e then Some () else None)

let wait_for_stored_slot_shards ~num_stored_shards dal_node ~published_level
    ~slot_index =
  let check_slot_id e =
    JSON.(e |-> "published_level" |> as_int) = published_level
    && JSON.(e |-> "slot_index" |> as_int) = slot_index
  in
  if num_stored_shards <= 0 then Lwt.return_unit
  else
    let num_remaining_shards = ref num_stored_shards in
    Dal_node.wait_for dal_node "dal_stored_slot_shard.v0" (fun e ->
        if check_slot_id e then (
          decr num_remaining_shards ;
          if !num_remaining_shards <= 0 then Some () else None)
        else None)

(* Wait until the given [dal_node] receives all the shards whose
   indices are [shards] for the given published level and slot index. *)
let wait_for_shards_promises ~dal_node ~shards ~published_level ~slot_index
    ~(storage_profile : [`Cache_only | `Disk of int]) =
  let nshards = List.length shards in
  let count = ref 0 in
  let promises =
    List.map
      (fun shard_index ->
        let* () =
          wait_for_cached_slot
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
  let save_on_disk_promise =
    match storage_profile with
    | `Cache_only -> Lwt.return_unit
    | `Disk num_stored_shards ->
        wait_for_stored_slot_shards
          ~num_stored_shards
          dal_node
          ~published_level
          ~slot_index
  in
  Lwt.join (save_on_disk_promise :: promises)

(* --- Baking and block helpers --- *)

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

(* --- Committee and delegate helpers --- *)

let check_in_TB_committee ~__LOC__ ~protocol node ?(inside = true) ?level pkh =
  let* slots =
    Node.RPC.call node
    @@ RPC.get_chain_block_helper_validators ?level ~delegate:pkh ()
  in
  let in_committee =
    if Protocol.number protocol >= 024 then
      JSON.(as_list slots |> List.hd |-> "delegates" |> as_list) <> []
    else JSON.as_list slots <> []
  in
  Check.(
    (in_committee = inside)
      ~__LOC__
      bool
      ~error_msg:"The account is in the TB committee? Expected %R, got %L") ;
  unit

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

let get_delegate client ~protocol ~level ~tb_index =
  let* pkh_to_rounds = Operation.Consensus.get_rounds ~level ~protocol client in
  let pkh =
    Option.get
    @@ List.find_map
         (fun (pkh, rounds) ->
           if List.mem tb_index rounds then Some pkh else None)
         pkh_to_rounds
  in
  return
  @@ List.find
       (fun acc -> acc.Account.public_key_hash = pkh)
       Constant.all_secret_keys

(* --- Protocol parameter builders --- *)

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

(* --- Protocol parameters and node setup --- *)

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

(** Initialize an L1 node with protocol parameters customized for DAL testing.
    Returns the DAL parameters, cryptobox, node, client, and a bootstrap key. *)
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

(** High-level wrapper: set up an L1 node with DAL-enabled protocol parameters,
    then call [f] with the parameters, cryptobox, node, client, and bootstrap key. *)
let with_layer1 ?custom_constants ?additional_bootstrap_accounts
    ?consensus_committee_size ?consensus_threshold_size ?minimal_block_delay
    ?delay_increment_per_round ?attestation_lag ?slot_size ?number_of_slots
    ?page_size ?attestation_threshold ?number_of_shards ?redundancy_factor
    ?commitment_period ?challenge_window ?dal_enable ?incentives_enable
    ?dal_rewards_weight ?traps_fraction ?event_sections_levels ?node_arguments
    ?activation_timestamp ?dal_bootstrap_peers ?(parameters = [])
    ?(prover = true) ?smart_rollup_timeout_period_in_blocks ?l1_history_mode
    ?blocks_per_cycle ?blocks_per_commitment
    ?all_bakers_attest_activation_threshold f ~protocol =
  let parameter_overrides =
    make_int_parameter ["dal_parametric"; "attestation_lag"] attestation_lag
    @ (match attestation_lag with
      | None -> []
      | Some lag ->
          if Protocol.number protocol < 025 then []
          else
            [
              ( ["dal_parametric"; "attestation_lags"],
                `A [`Float (float_of_int lag)] );
            ])
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
    @ make_int_parameter ["consensus_threshold_size"] consensus_threshold_size
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
    @ (if Protocol.(number protocol >= 024) then
         match all_bakers_attest_activation_threshold with
         | None ->
             (* TODO ABAAB: in current version of the tests, the
                "all bakers attest" feature is not active unless a threshold is
                passed explicitly as a parameter override. *)
             [
               ( ["all_bakers_attest_activation_threshold"],
                 `O [("numerator", `Float 2.); ("denominator", `Float 1.)] );
             ]
         | Some Q.{num; den} ->
             [
               ( ["all_bakers_attest_activation_threshold"],
                 `O
                   [
                     ("numerator", `Float (Z.to_float num));
                     ("denominator", `Float (Z.to_float den));
                   ] );
             ]
       else [])
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
  let* () =
    if prover then Helpers.init_prover ~__LOC__ ()
    else (
      Helpers.init_verifier () ;
      unit)
  in
  let* cryptobox = Helpers.make_cryptobox dal_parameters.cryptobox in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f dal_parameters cryptobox node client bootstrap1_key

let noop_wasm_bootsector =
  Octez_smart_rollup_node_test_helpers.Helpers.noop_wasm_boot_sector
  |> Hex.of_string |> Hex.show

let default_boot_sector ~pvm_name =
  match pvm_name with "wasm_2_0_0" -> noop_wasm_bootsector | _ -> ""

(** Originate a fresh smart rollup and call [f] with the rollup address,
    rollup node, and related context. *)
let with_fresh_rollup ?(pvm_name = "arith") ?boot_sector ?dal_node f tezos_node
    tezos_client bootstrap1_key =
  let boot_sector =
    match boot_sector with Some b -> b | None -> default_boot_sector ~pvm_name
  in
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"rollup"
      ~src:bootstrap1_key
      ~kind:pvm_name
      ~boot_sector
      ~parameters_ty:"string"
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ?dal_node
      Operator
      tezos_node
      ~base_dir:(Client.base_dir tezos_client)
      ~kind:pvm_name
      ~default_operator:bootstrap1_key
  in
  let* () = bake_for tezos_client in
  f rollup_address sc_rollup_node

let boot_sector kernel_path =
  let read_kernel ?(suffix = ".wasm") name : string =
    let load_kernel_file name : string =
      let open Tezt.Base in
      let kernel_file = project_root // name in
      read_file kernel_file
    in
    let hex_encode (input : string) : string =
      match Hex.of_string input with `Hex s -> s
    in
    hex_encode (load_kernel_file (name ^ suffix))
  in
  read_kernel ~suffix:"" (Uses.path kernel_path)

let make_dal_node ?name ?peers ?attester_profiles ?operator_profiles
    ?observer_profiles ?bootstrap_profile ?history_mode ?(wait_ready = true)
    ?env ?disable_shard_validation ?(event_level = `Debug) ?slots_backup_uris
    ?trust_slots_backup_uris ?disable_amplification ?ignore_pkhs
    ?batching_time_interval tezos_node =
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
      ?observer_profiles
      ?bootstrap_profile
      ?history_mode
      ?slots_backup_uris
      ?trust_slots_backup_uris
      ?batching_time_interval
      dal_node
  in
  let* () = Dal_node.run ?env ~event_level dal_node ~wait_ready in
  return dal_node

(** Create and run a DAL node connected to [tezos_node], then call [f]
    with the bootstrap key and DAL node. *)
let with_dal_node ?peers ?attester_profiles ?operator_profiles
    ?observer_profiles ?bootstrap_profile ?history_mode ?wait_ready ?env
    ?disable_shard_validation ?disable_amplification ?ignore_pkhs
    ?batching_time_interval tezos_node f key =
  let* dal_node =
    make_dal_node
      ?peers
      ?attester_profiles
      ?operator_profiles
      ?observer_profiles
      ?bootstrap_profile
      ?history_mode
      ?wait_ready
      ?env
      ?disable_shard_validation
      ?disable_amplification
      ?ignore_pkhs
      ?batching_time_interval
      tezos_node
  in
  f key dal_node

(* --- Test registration and scenario builders --- *)

let test ~__FILE__ ?(regression = false) ?(tags = []) ?uses
    ?(supports = Protocol.From_protocol 19) title f =
  let tags = Tag.tezos2 :: "dal" :: tags in
  let register_test =
    if regression then Protocol.register_regression_test
    else Protocol.register_test
  in
  register_test ~__FILE__ ~title ~tags ?uses ~supports f

(* Wrapper scenario functions that should be re-used as much as possible when
   writing tests. *)

(** Register a test that sets up an L1 node only (no DAL node).
    The [scenario] function receives the protocol parameters, cryptobox,
    node, client, and bootstrap key. *)
let scenario_with_layer1_node ~__FILE__ ?attestation_threshold ?regression
    ?(tags = []) ?(uses = fun _ -> []) ?additional_bootstrap_accounts
    ?attestation_lag ?number_of_shards ?number_of_slots ?slot_size
    ?custom_constants ?commitment_period ?challenge_window ?(dal_enable = true)
    ?incentives_enable ?traps_fraction ?dal_rewards_weight
    ?event_sections_levels ?node_arguments ?activation_timestamp
    ?consensus_committee_size ?minimal_block_delay ?delay_increment_per_round
    ?blocks_per_cycle ?blocks_per_commitment variant scenario =
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
        ?slot_size
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

(** Register a test that sets up both an L1 node and a DAL node.
    The [scenario] function receives the protocol, DAL parameters, cryptobox,
    node, client, and DAL node. This is the most commonly used scenario builder. *)
let scenario_with_layer1_and_dal_nodes ~__FILE__ ?regression ?(tags = [])
    ?(uses = fun _ -> []) ?custom_constants ?minimal_block_delay
    ?blocks_per_cycle ?delay_increment_per_round ?consensus_committee_size
    ?consensus_threshold_size ?redundancy_factor ?slot_size ?number_of_shards
    ?number_of_slots ?attestation_lag ?attestation_threshold ?traps_fraction
    ?commitment_period ?challenge_window ?(dal_enable = true) ?incentives_enable
    ?dal_rewards_weight ?activation_timestamp ?bootstrap_profile
    ?event_sections_levels ?operator_profiles ?observer_profiles ?history_mode
    ?prover ?l1_history_mode ?all_bakers_attest_activation_threshold ?wait_ready
    ?env ?disable_shard_validation ?disable_amplification ?ignore_pkhs
    ?batching_time_interval variant scenario =
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
        ?blocks_per_cycle
        ?consensus_committee_size
        ?consensus_threshold_size
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
        ?all_bakers_attest_activation_threshold
      @@ fun parameters cryptobox node client ->
      with_dal_node
        ?bootstrap_profile
        ?operator_profiles
        ?observer_profiles
        ?history_mode
        ?wait_ready
        ?env
        ?disable_shard_validation
        ?disable_amplification
        ?ignore_pkhs
        ?batching_time_interval
        node
      @@ fun _key dal_node ->
      scenario protocol parameters cryptobox node client dal_node)

(* Adapted from sc_rollup.ml *)
let test_l1_migration_scenario ~__FILE__ ?(tags = []) ?(uses = []) ~migrate_from
    ~migrate_to ~migration_level ~scenario ~description ?bootstrap_profile
    ?operator_profiles ?custom_constants ?attestation_lag ?attestation_threshold
    ?number_of_slots ?number_of_shards ?slot_size ?page_size ?redundancy_factor
    ?traps_fraction ?consensus_committee_size ?blocks_per_cycle
    ?minimal_block_delay ?parameter_overrides ?activation_timestamp () =
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
    @ Option.value ~default:[] parameter_overrides
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

  let* dal_node = make_dal_node ?operator_profiles ?bootstrap_profile node in

  scenario ~migration_level dal_parameters client node dal_node

(** Register a test that sets up an L1 node, a DAL node, and a smart rollup
    node. Used for tests that involve DAL page imports in rollup kernels. *)
let scenario_with_all_nodes ~__FILE__ ?custom_constants ?node_arguments
    ?consensus_committee_size ?slot_size ?page_size ?number_of_shards
    ?redundancy_factor ?attestation_lag ?(tags = []) ?(uses = fun _ -> [])
    ?(pvm_name = "arith") ?(dal_enable = true) ?incentives_enable
    ?dal_rewards_weight ?commitment_period ?challenge_window
    ?minimal_block_delay ?delay_increment_per_round ?activation_timestamp
    ?bootstrap_profile ?operator_profiles ?observer_profiles
    ?smart_rollup_timeout_period_in_blocks ?(regression = true) ?prover
    ?attestation_threshold ?l1_history_mode variant ?disable_amplification
    ?batching_time_interval scenario =
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
        ?observer_profiles
        ?disable_amplification
        ?batching_time_interval
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

(* --- Attestation operation helpers --- *)

(* We support two formats for specifying the attested slots: either a
   list of slot ids or a bitset. *)
type attestation_availability =
  | Slots of int list
  | Bitset of bool array
  | No_dal_attestation

(** Craft a DAL attestation operation without injecting it. The [availability]
    specifies which slots to attest (as slot indices, a bitset, or no attestation). *)
let craft_dal_attestation ~protocol ?level ?round ?payload_level ?lag_index
    ~signer availability client dal_parameters node_endpoint =
  let* dal_attestation =
    match availability with
    | Bitset bitset ->
        let* encoded =
          Dal.Attestations.encode_for_one_lag
            protocol
            node_endpoint
            ?lag_index
            dal_parameters
            bitset
        in
        return (Some encoded)
    | Slots availability ->
        let number_of_slots = dal_parameters.number_of_slots in
        let dal_attestation = Array.make number_of_slots false in
        List.iter (fun i -> dal_attestation.(i) <- true) availability ;
        let* encoded =
          Dal.Attestations.encode_for_one_lag
            protocol
            node_endpoint
            ?lag_index
            dal_parameters
            dal_attestation
        in
        return (Some encoded)
    | No_dal_attestation -> return None
  in
  let* level =
    match level with Some level -> return level | None -> Client.level client
  in
  let* block_payload_hash, round =
    let block = Option.value payload_level ~default:level |> string_of_int in
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
  let* slot =
    Operation.Consensus.get_attestation_slot_opt
      ~level
      ~delegate:signer
      ~protocol
      client
  in
  match slot with
  | None -> none
  | Some slot ->
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

let craft_dal_attestation_exn ~protocol ?level ?round ?payload_level ?lag_index
    ~signer availability client dal_parameters node_endpoint =
  let* res =
    craft_dal_attestation
      ~protocol
      ?level
      ?round
      ?payload_level
      ?lag_index
      ~signer
      availability
      client
      dal_parameters
      node_endpoint
  in
  match res with
  | None ->
      Test.fail
        ~__LOC__
        "Unexpected case: pkh %s has no TB slot"
        signer.Account.public_key_hash
  | Some v -> return v

let inject_dal_attestation ~protocol ?level ?round ?payload_level ?lag_index
    ?force ?error ?request ~signer availability client dal_parameters
    node_endpoint =
  let* op_opt =
    craft_dal_attestation
      ~protocol
      ?level
      ?round
      ?payload_level
      ?lag_index
      ~signer
      availability
      client
      dal_parameters
      node_endpoint
  in
  match op_opt with
  | None -> none
  | Some op ->
      let* oph = Operation.inject ?force ?error ?request op client in
      some (op, oph)

let inject_dal_attestation_exn ~protocol ?level ?round ?payload_level ?lag_index
    ?force ?error ?request ~signer availability client dal_parameters
    node_endpoint =
  let* res =
    inject_dal_attestation
      ~protocol
      ?level
      ?round
      ?payload_level
      ?lag_index
      ?force
      ?error
      ?request
      ~signer
      availability
      client
      dal_parameters
      node_endpoint
  in
  match res with
  | None ->
      Test.fail
        ~__LOC__
        "Unexpected case: pkh %s has no TB slot"
        signer.Account.public_key_hash
  | Some v -> return v

let inject_dal_attestations ~protocol ?payload_level ?level ?round ?lag_index
    ?force ?request ?(signers = Array.to_list Account.Bootstrap.keys)
    availability client dal_parameters node_endpoint =
  Lwt_list.filter_map_s
    (fun signer ->
      inject_dal_attestation
        ~protocol
        ?payload_level
        ?level
        ?round
        ?lag_index
        ?force
        ?request
        ~signer
        availability
        client
        dal_parameters
        node_endpoint)
    signers

(** Inject DAL attestations for all bootstrap delegates and bake a block. *)
let inject_dal_attestations_and_bake ~protocol ?lag_index node client indexes
    dal_parameters =
  let* baker =
    let* level = Node.get_level node in
    baker_for_round_zero node ~level:(level + 1)
  in
  let signers = different_delegates baker in
  let* _op_and_op_hash_list =
    inject_dal_attestations
      ~protocol
      ?lag_index
      ~signers
      indexes
      client
      dal_parameters
      (Node.as_rpc_endpoint node)
  in
  bake_for ~delegates:(`For [baker]) client

let wait_for_classified oph node =
  let filter json = if JSON.as_string json = oph then Some () else None in
  Node.wait_for node "operation_classified.v0" filter

(** Create expected attestation array with given slots attested. *)
let expected_attestation dal_parameters attested_slots =
  let arr = Array.make dal_parameters.Dal.Parameters.number_of_slots false in
  List.iter (fun i -> arr.(i) <- true) attested_slots ;
  arr

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

(* --- Slot publication helpers --- *)

let generate_dummy_slot slot_size =
  String.init slot_size (fun i ->
      match i mod 3 with 0 -> 'a' | 1 -> 'b' | _ -> 'c')

let publish_dummy_slot ~source ?error ?fee ~index ~message cryptobox =
  let commitment, proof = Dal.(Commitment.dummy_commitment cryptobox message) in
  Helpers.publish_commitment ~source ?fee ?error ~index ~commitment ~proof

let slot_idx parameters level =
  level mod parameters.Dal.Parameters.number_of_slots

(* We check that publishing a slot header with a proof for a different
   slot leads to a proof-checking error. *)
let publish_dummy_slot_with_wrong_proof_for_same_content ~source ?fee ~index
    cryptobox =
  let commitment, _proof = Dal.(Commitment.dummy_commitment cryptobox "a") in
  let _commitment, proof = Dal.(Commitment.dummy_commitment cryptobox "b") in
  Helpers.publish_commitment ~source ?fee ~index ~commitment ~proof

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

let publish_store_and_attest_slot ~protocol ?counter ?force ?fee client node
    dal_node source ~index ~content dal_parameters =
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
  let* () =
    repeat dal_parameters.Dal.Parameters.attestation_lag (fun () ->
        bake_for client)
  in
  inject_dal_attestations_and_bake
    ~protocol
    node
    client
    (Slots [index])
    dal_parameters

let publish_store_and_attest_slot_at_lag ~protocol ~lag_index ~lag client node
    dal_node source ~index ~content dal_parameters =
  let* _commitment =
    Helpers.publish_and_store_slot client dal_node source ~index content
  in
  let* () = repeat lag (fun () -> bake_for client) in
  inject_dal_attestations_and_bake
    ~protocol
    ~lag_index
    node
    client
    (Slots [index])
    dal_parameters

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

(** For each level in the range [from_level, to_level]:
    1. Publishes a dummy slot with content derived from the level and slot index
    2. Stores the slot in the DAL node
    3. Bakes a new block to include the published slot on L1
    4. Waits for the node to reach the baked level

    The [delegates] parameter is used to specify which delegates should
    participate in baking. *)
let publish_and_bake ?slots ?delegates ~from_level ~to_level parameters
    cryptobox node client dal_node =
  let num_bakers = Array.length Account.Bootstrap.keys in
  let publish source ~index message =
    let* _op_hash =
      publish_dummy_slot ~source ~index ~message cryptobox client
    in
    unit
  in
  let publish_and_store =
    let slot_idx =
      match slots with
      | Some s ->
          let num_slots = List.length s in
          fun level -> List.nth s (slot_idx parameters level mod num_slots)
      | None -> slot_idx parameters
    in
    fun level ->
      let source = Account.Bootstrap.keys.(level mod num_bakers) in
      let index = slot_idx level in
      let slot_content =
        Format.asprintf "content at level %d index %d" level index
      in
      let* predecessor = Node.wait_for_level node (pred level) in
      if predecessor >= level then
        Test.fail
          "Tried to publish a slot header at level %d but the predecessor \
           level is %d"
          level
          predecessor ;
      let* () = publish source ~index slot_content in
      (* Storing the slot is required as the producer/operator does not store
         anything. *)
      let* _commitment, _proof =
        let slot_size = parameters.Dal.Parameters.cryptobox.slot_size in
        Helpers.(
          store_slot dal_node ~slot_index:index
          @@ make_slot ~slot_size slot_content)
      in
      Log.info "Slot with %d index (normally) published at level %d" index level ;
      return (level, index)
  in
  Log.info
    "Publish (inject and bake) a slot header at each level from %d to %d."
    from_level
    to_level ;
  let rec iter acc level =
    if level > to_level then return acc
    else
      let* level, index = publish_and_store level in
      let* () =
        bake_for
          ?delegates
          ~dal_node_endpoint:(Dal_node.rpc_endpoint dal_node)
          client
      in
      let* _ = Node.wait_for_level node level in
      iter ((level, index) :: acc) (level + 1)
  in
  iter [] from_level

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
            (* This value is quite high (way higher than required). But the
             default value of 1200µtez was not sufficient for a publication to
             be validated by the mempool in the test
             [test_migration_accuser_issue]. I do not understand why, since
             publication fees on real networks are less than 1000µtez.
          *)
          ~fee:5000
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

(* --- Status and verification helpers --- *)

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

(** This function checks that the status of the slot at
    [(~slot_level, ~slot_index)] on [dal_node] matches [~expected_status].

    [?check_attested_lag] controls how the lag value is compared when both
    the actual and expected statuses are [Attested]. This is particularly
    relevant for multi-lag configurations where a slot can be attested at
    different lags (e.g., with [attestation_lags = [2; 3; 5]], a slot could be
    attested at lag 2, 3, or 5). The possible values are:

    - [`Exact] (default): The actual lag must equal the expected lag exactly;
    - [`At_most]: The actual lag must be less than or equal to the expected lag.

    For non-[Attested] statuses, [?check_attested_lag] has no effect and exact
    equality is always used. *)
let check_slot_status ~__LOC__ ~expected_status ?(check_attested_lag = `Exact)
    dal_node ~slot_level ~slot_index =
  match expected_status with
  | Dal_RPC.Not_found ->
      (* DAL node has no record for this slot. *)
      let* response =
        Dal_RPC.(
          call_raw dal_node @@ get_level_slot_status ~slot_level ~slot_index)
      in
      RPC_core.check_string_response ~code:404 response ;
      unit
  | _ ->
      let* status =
        Dal_RPC.(call dal_node @@ get_level_slot_status ~slot_level ~slot_index)
      in
      let prefix =
        sf "Unexpected slot status at level %d, index %d " slot_level slot_index
      in
      let status_matches =
        match (status, expected_status, check_attested_lag) with
        | Dal_RPC.Attested lag, Dal_RPC.Attested expected_lag, `At_most ->
            lag <= expected_lag
        | _ -> status = expected_status
      in
      if not status_matches then
        Test.fail
          ~__LOC__
          "%s(got = %a, expected = %a)"
          prefix
          Dal_RPC.pp_slot_id_status
          status
          Dal_RPC.pp_slot_id_status
          expected_status ;
      unit

let check_slots_statuses ~__LOC__ ~expected_status ?check_attested_lag dal_node
    ~slot_level slots_info =
  let test (slot_index, _commitment) =
    check_slot_status
      ~__LOC__
      ~expected_status
      ?check_attested_lag
      dal_node
      ~slot_level
      ~slot_index
  in
  Lwt_list.iter_s test slots_info

let check_profiles ~__LOC__ dal_node ~expected =
  let* profiles = Dal_RPC.(call dal_node @@ get_profiles ()) in
  return
    Check.(
      (profiles = expected)
        Dal.Check.profiles_typ
        ~error_msg:
          (__LOC__ ^ " : Unexpected profiles (Actual: %L <> Expected: %R)"))

(* --- P2P and Gossipsub helpers --- *)

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
