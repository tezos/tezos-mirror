(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_helpers
open Contract_path

type l1_contracts = {
  delayed_transaction_bridge : string;
  exchanger : string;
  bridge : string;
  admin : string;
  sequencer_governance : string;
  ticket_router_tester : string;
}

type sequencer_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_address : string;
  sc_rollup_node : Sc_rollup_node.t;
  observer : Evm_node.t;
  sequencer : Evm_node.t;
  proxy : Evm_node.t;
  l1_contracts : l1_contracts;
  boot_sector : string;
  kernel : Uses.t;
  enable_dal : bool;
}

let uses _protocol =
  [
    Constant.octez_smart_rollup_node;
    Constant.octez_evm_node;
    Constant.smart_rollup_installer;
  ]

let setup_l1_contracts ?(dictator = Constant.bootstrap2) client =
  (* Originates the delayed transaction bridge. *)
  let* delayed_transaction_bridge =
    Client.originate_contract
      ~alias:"evm-seq-delayed-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~prg:(delayed_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the exchanger. *)
  let* exchanger =
    Client.originate_contract
      ~alias:"exchanger"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:(exchanger_path ())
      ~burn_cap:Tez.one
      client
  in
  (* Originates the bridge. *)
  let* bridge =
    Client.originate_contract
      ~alias:"evm-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.public_key_hash
      ~init:(sf "Pair %S None" exchanger)
      ~prg:(bridge_path ())
      ~burn_cap:Tez.one
      client
  (* Originates the administrator contract. *)
  and* admin =
    Client.originate_contract
      ~alias:"evm-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap3.public_key_hash
      ~init:(sf "%S" dictator.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
    (* Originates the administrator contract. *)
  and* sequencer_governance =
    Client.originate_contract
      ~alias:"evm-sequencer-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap4.public_key_hash
      ~init:(sf "%S" dictator.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the ticket router tester (FA bridge) contract. *)
  let* ticket_router_tester =
    Client.originate_contract
      ~alias:"ticket-router-tester"
      ~amount:Tez.zero
      ~src:Constant.bootstrap4.public_key_hash
      ~init:
        "Pair (Pair 0x01000000000000000000000000000000000000000000 (Pair (Left \
         Unit) 0)) {}"
      ~prg:(ticket_router_tester_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  return
    {
      delayed_transaction_bridge;
      exchanger;
      bridge;
      admin;
      sequencer_governance;
      ticket_router_tester;
    }

let run_new_rpc_endpoint evm_node =
  let rpc_node =
    Evm_node.create
      ~data_dir:(Evm_node.data_dir evm_node)
      ~mode:(Rpc Evm_node.(mode evm_node))
      (Evm_node.endpoint evm_node)
  in
  let* () = Evm_node.run rpc_node in
  return rpc_node

let run_new_observer_node ?(patch_config = Fun.id) ~sc_rollup_node evm_node =
  let preimages_dir = Evm_node.preimages_dir evm_node in
  let initial_kernel = Evm_node.initial_kernel evm_node in
  let* observer_mode =
    if Evm_node.supports_threshold_encryption evm_node then
      let bundler =
        Dsn_node.bundler ~endpoint:(Evm_node.endpoint evm_node) ()
      in
      let* () = Dsn_node.start bundler in
      return
        (Evm_node.Threshold_encryption_observer
           {
             initial_kernel;
             preimages_dir;
             rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
             bundler_node_endpoint = Dsn_node.endpoint bundler;
           })
    else
      return
        (Evm_node.Observer
           {
             initial_kernel;
             preimages_dir;
             rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
           })
  in
  let* observer =
    Evm_node.init ~patch_config ~mode:observer_mode (Evm_node.endpoint evm_node)
  in
  return observer

let setup_sequencer ?sequencer_rpc_port ?sequencer_private_rpc_port
    ~mainnet_compat ?genesis_timestamp ?time_between_blocks ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?max_number_of_chunks
    ?commitment_period ?challenge_window
    ?(bootstrap_accounts =
      List.map
        (fun account -> account.Eth_account.address)
        (Array.to_list Eth_account.bootstrap_accounts))
    ?(sequencer = Constant.bootstrap1) ?sequencer_pool_address
    ?(kernel = Constant.WASM.evm_kernel) ?da_fee ?minimum_base_fee_per_gas
    ?preimages_dir ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge
    ?(threshold_encryption = false) ?(drop_duplicate_when_injection = true)
    ?node_transaction_validation ?history_mode ~enable_dal ?dal_slots protocol =
  let* node, client =
    setup_l1
      ?commitment_period
      ?challenge_window
      ?timestamp:genesis_timestamp
      protocol
  in
  let* dal_node =
    if enable_dal then
      let dal_node = Dal_node.create ~node () in
      let* () = Dal_node.init_config ?producer_profiles:dal_slots dal_node in
      let* () = Dal_node.run ~wait_ready:true dal_node in
      some dal_node
    else none
  in
  let client = Client.with_dal_node client ?dal_node in
  let* l1_contracts = setup_l1_contracts client in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ?dal_node
      ?history_mode
  in
  let preimages_dir =
    Option.value
      ~default:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
      preimages_dir
  in
  let output_config = Temp.file "config.yaml" in
  let*! () =
    Evm_node.make_kernel_installer_config
      ~mainnet_compat
      ~sequencer:sequencer.public_key
      ~delayed_bridge:l1_contracts.delayed_transaction_bridge
      ~ticketer:l1_contracts.exchanger
      ~administrator:l1_contracts.admin
      ~sequencer_governance:l1_contracts.sequencer_governance
      ?minimum_base_fee_per_gas
      ?da_fee_per_byte:da_fee
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?sequencer_pool_address
      ?maximum_allowed_ticks
      ?maximum_gas_per_transaction
      ~enable_dal
      ?dal_slots
      ?max_blueprint_lookahead_in_seconds
      ~bootstrap_accounts
      ~output:output_config
      ?enable_fa_bridge
      ()
  in
  let* {output; _} =
    prepare_installer_kernel ~preimages_dir ~config:(`Path output_config) kernel
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:Helpers.evm_type
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let private_rpc_port =
    match sequencer_private_rpc_port with
    | Some p -> Some p
    | None -> Some (Port.fresh ())
  in
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~drop_duplicate_when_injection
      ?node_transaction_validation
      (* When adding new experimental feature please make sure it's a
         good idea to activate it for all test or not. *)
      ()
  in
  let* sequencer_mode =
    if threshold_encryption then
      let sequencer_sidecar = Dsn_node.sequencer () in
      let* () = Dsn_node.start sequencer_sidecar in
      return
      @@ Evm_node.Threshold_encryption_sequencer
           {
             initial_kernel = output;
             preimage_dir = Some preimages_dir;
             private_rpc_port;
             time_between_blocks;
             sequencer = sequencer.alias;
             genesis_timestamp;
             max_blueprints_lag;
             max_blueprints_ahead;
             max_blueprints_catchup;
             catchup_cooldown;
             max_number_of_chunks;
             wallet_dir = Some (Client.base_dir client);
             tx_pool_timeout_limit = None;
             tx_pool_addr_limit = None;
             tx_pool_tx_per_addr_limit = None;
             sequencer_sidecar_endpoint = Dsn_node.endpoint sequencer_sidecar;
             dal_slots;
           }
    else
      return
      @@ Evm_node.Sequencer
           {
             initial_kernel = output;
             preimage_dir = Some preimages_dir;
             private_rpc_port;
             time_between_blocks;
             sequencer = sequencer.alias;
             genesis_timestamp;
             max_blueprints_lag;
             max_blueprints_ahead;
             max_blueprints_catchup;
             catchup_cooldown;
             max_number_of_chunks;
             wallet_dir = Some (Client.base_dir client);
             tx_pool_timeout_limit = None;
             tx_pool_addr_limit = None;
             tx_pool_tx_per_addr_limit = None;
             dal_slots;
           }
  in
  let* sequencer =
    Evm_node.init
      ?rpc_port:sequencer_rpc_port
      ~patch_config
      ~mode:sequencer_mode
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* observer =
    run_new_observer_node ~patch_config ~sc_rollup_node sequencer
  in
  let* proxy =
    Evm_node.init
      ~patch_config
      ~mode:(Proxy {finalized_view = false})
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  return
    {
      node;
      client;
      sequencer;
      proxy;
      observer;
      l1_contracts;
      sc_rollup_address;
      sc_rollup_node;
      boot_sector = output;
      kernel;
      enable_dal;
    }

(* Register a single variant of a test but for all protocols. *)
let register_test ?sequencer_rpc_port ?sequencer_private_rpc_port
    ?genesis_timestamp ?time_between_blocks ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?max_number_of_chunks
    ?bootstrap_accounts ?sequencer ?sequencer_pool_address ~kernel ?da_fee
    ?minimum_base_fee_per_gas ?preimages_dir ?maximum_allowed_ticks
    ?maximum_gas_per_transaction ?max_blueprint_lookahead_in_seconds
    ?enable_fa_bridge ?commitment_period ?challenge_window
    ?(threshold_encryption = false) ?(uses = uses) ?(additional_uses = [])
    ?history_mode ~enable_dal
    ?(dal_slots = if enable_dal then Some [4] else None) body ~title ~tags
    protocols =
  let kernel_tag, kernel_use = Kernel.to_uses_and_tags kernel in
  let tags = kernel_tag :: tags in
  let additional_uses =
    (if threshold_encryption then [Constant.octez_dsn_node] else [])
    @ [kernel_use]
    @ (if enable_dal then [Constant.octez_dal_node] else [])
    @ additional_uses
  in
  let body protocol =
    let* sequencer_setup =
      setup_sequencer
        ?sequencer_rpc_port
        ?sequencer_private_rpc_port
        ~mainnet_compat:false
        ?commitment_period
        ?challenge_window
        ?genesis_timestamp
        ?time_between_blocks
        ?max_blueprints_lag
        ?max_blueprints_ahead
        ?max_blueprints_catchup
        ?catchup_cooldown
        ?delayed_inbox_timeout
        ?delayed_inbox_min_levels
        ?max_number_of_chunks
        ?bootstrap_accounts
        ?sequencer
        ?sequencer_pool_address
        ~kernel:kernel_use
        ?da_fee
        ?minimum_base_fee_per_gas
        ?preimages_dir
        ?maximum_allowed_ticks
        ?maximum_gas_per_transaction
        ?max_blueprint_lookahead_in_seconds
        ?enable_fa_bridge
        ~threshold_encryption
        ?history_mode
        ~enable_dal
        ?dal_slots
        protocol
    in
    body sequencer_setup protocol
  in
  let tags =
    (if threshold_encryption then ["threshold_encryption"] else [])
    @ (if enable_dal then ["dal"] else [])
    @ tags
  in
  let title =
    sf
      "%s (%s, %s, %s)"
      title
      (if threshold_encryption then "te_sequencer" else "sequencer")
      kernel_tag
      (if enable_dal then "with dal" else "without dal")
  in
  (* Only register DAL tests for supporting kernels *)
  if (not enable_dal) || Kernel.supports_dal kernel then
    Protocol.register_test
      ~additional_tags:(function Alpha -> [] | _ -> [Tag.slow])
      ~__FILE__
      ~uses:(fun protocol -> uses protocol @ additional_uses)
      body
      ~title
      ~tags
      protocols

let register_test_for_kernels ?sequencer_rpc_port ?sequencer_private_rpc_port
    ?genesis_timestamp ?time_between_blocks ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?max_number_of_chunks
    ?bootstrap_accounts ?sequencer ?sequencer_pool_address
    ?(kernels = Kernel.all) ?da_fee ?minimum_base_fee_per_gas ?preimages_dir
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ?history_mode
    ?commitment_period ?challenge_window ?additional_uses ~threshold_encryption
    ~enable_dal ~title ~tags body protocols =
  List.iter
    (fun kernel ->
      register_test
        ?sequencer_rpc_port
        ?sequencer_private_rpc_port
        ?commitment_period
        ?challenge_window
        ?genesis_timestamp
        ?time_between_blocks
        ?max_blueprints_lag
        ?max_blueprints_ahead
        ?max_blueprints_catchup
        ?catchup_cooldown
        ?delayed_inbox_timeout
        ?delayed_inbox_min_levels
        ?max_number_of_chunks
        ?bootstrap_accounts
        ?sequencer
        ?sequencer_pool_address
        ~kernel
        ?da_fee
        ?minimum_base_fee_per_gas
        ?preimages_dir
        ?maximum_allowed_ticks
        ?maximum_gas_per_transaction
        ?max_blueprint_lookahead_in_seconds
        ?enable_fa_bridge
        ?additional_uses
        ~threshold_encryption
        ?history_mode
        ~enable_dal
        ~title
        ~tags
        body
        protocols)
    kernels
