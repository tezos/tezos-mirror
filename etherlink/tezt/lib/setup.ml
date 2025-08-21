(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
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

type multichain_sequencer_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_address : string;
  sc_rollup_node : Sc_rollup_node.t;
  observers : Evm_node.t list;
  sequencer : Evm_node.t;
  proxies : Evm_node.t list;
  l1_contracts : l1_contracts;
  boot_sector : string;
  kernel : Uses.t;
  enable_dal : bool;
  evm_version : Evm_version.t;
  enable_multichain : bool;
  l2_chains : Evm_node.l2_setup list;
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
  evm_version : Evm_version.t;
  enable_multichain : bool;
  l2_chains : Evm_node.l2_setup list;
}

let multichain_setup_to_single ~(setup : multichain_sequencer_setup) =
  let observer =
    match setup.observers with [observer] -> observer | _ -> assert false
  in
  let proxy = match setup.proxies with [proxy] -> proxy | _ -> assert false in
  {
    node = setup.node;
    client = setup.client;
    sc_rollup_address = setup.sc_rollup_address;
    sc_rollup_node = setup.sc_rollup_node;
    observer;
    sequencer = setup.sequencer;
    proxy;
    l1_contracts = setup.l1_contracts;
    boot_sector = setup.boot_sector;
    kernel = setup.kernel;
    enable_dal = setup.enable_dal;
    enable_multichain = setup.enable_multichain;
    evm_version = setup.evm_version;
    l2_chains = setup.l2_chains;
  }

let uses _protocol =
  [
    Constant.octez_smart_rollup_node;
    Constant.octez_evm_node;
    Constant.smart_rollup_installer;
  ]

let setup_l1_contracts ?(dictator = Constant.bootstrap2) ~kernel client =
  (* Originates the delayed transaction bridge. *)
  let* delayed_transaction_bridge =
    Client.originate_contract
      ~alias:"evm-seq-delayed-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~prg:(delayed_path ~kernel)
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

let observer_counter =
  (* Counter used to give unique names to config files used by observers *)
  ref 0

let run_new_observer_node ?(finalized_view = false) ?(patch_config = Fun.id)
    ~sc_rollup_node ?rpc_server ?websockets ?history_mode ?enable_tx_queue
    ?l2_chain evm_node =
  let preimages_dir = Evm_node.preimages_dir evm_node in
  let initial_kernel = Evm_node.initial_kernel evm_node in
  let config_file = Temp.file (sf "config-%d.json" !observer_counter) in
  incr observer_counter ;
  let patch_config =
    if finalized_view then
      JSON.(
        fun json ->
          put
            ("finalized_view", annotate ~origin:"" (`Bool true))
            (patch_config json))
    else patch_config
  in
  let patch_config =
    match (rpc_server, enable_tx_queue) with
    | None, None -> patch_config
    | _, _ ->
        fun c ->
          Evm_node.patch_config_with_experimental_feature
            ?l2_chains:
              (match l2_chain with
              | None -> None
              | Some l2_chain -> Some [l2_chain])
            ?rpc_server
            ?enable_tx_queue
            ()
          @@ patch_config c
  in
  let observer_mode =
    Evm_node.Observer
      {
        initial_kernel;
        preimages_dir = Some preimages_dir;
        private_rpc_port = Some (Port.fresh ());
        rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
      }
  in
  let* observer =
    Evm_node.init
      ~patch_config
      ~mode:observer_mode
      ~config_file
      ?history_mode
      ?websockets
      (Evm_node.endpoint evm_node)
  in
  let* () = Evm_node.wait_for_blueprint_applied observer 0 in
  return observer

let setup_kernel_singlechain ~l1_contracts ?max_delayed_inbox_blueprint_length
    ~mainnet_compat ?delayed_inbox_timeout ?delayed_inbox_min_levels
    ?(eth_bootstrap_accounts = Evm_node.eth_default_bootstrap_accounts)
    ?sequencer_pool_address ?da_fee_per_byte ?minimum_base_fee_per_gas
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ~enable_revm
    ?enable_fast_withdrawal ?enable_fast_fa_withdrawal ~enable_dal ?dal_slots
    ?evm_version ~sequencer ~preimages_dir ~kernel () =
  let output_config = Temp.file "config.yaml" in
  let*! () =
    Evm_node.make_kernel_installer_config
      ?max_delayed_inbox_blueprint_length
      ~mainnet_compat
      ~sequencer
      ~delayed_bridge:l1_contracts.delayed_transaction_bridge
      ~ticketer:l1_contracts.exchanger
      ~administrator:l1_contracts.admin
      ~sequencer_governance:l1_contracts.sequencer_governance
      ?minimum_base_fee_per_gas
      ?da_fee_per_byte
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?sequencer_pool_address
      ?maximum_allowed_ticks
      ?maximum_gas_per_transaction
      ~enable_dal
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?dal_slots
      ~enable_multichain:false
      ?max_blueprint_lookahead_in_seconds
      ~eth_bootstrap_accounts
      ~output:output_config
      ?evm_version
      ?enable_fa_bridge
      ~enable_revm
      ()
  in
  let* {output; _} =
    prepare_installer_kernel ~preimages_dir ~config:(`Path output_config) kernel
  in
  return output

let generate_l2_kernel_config (l2_setup : Evm_node.l2_setup) client =
  let l2_config =
    Temp.file (Format.sprintf "l2-%d-config.yaml" l2_setup.l2_chain_id)
  in
  let* tez_bootstrap_contracts =
    match l2_setup.tez_bootstrap_contracts with
    | None -> none
    | Some contracts ->
        let* result =
          Lwt_list.map_s
            (fun Evm_node.{address; path; initial_storage} ->
              let script = Tezt_core.Base.read_file path in
              let* code =
                Client.convert_script
                  ~script
                  ~src_format:`Michelson
                  ~dst_format:`Binary
                  client
              in
              let* initial_storage_binary =
                Client.convert_data
                  ~data:initial_storage
                  ~src_format:`Michelson
                  ~dst_format:`Binary
                  client
              in
              Lwt.return
                (String.trim address ^ "," ^ String.trim code ^ ","
                ^ String.trim initial_storage_binary))
            contracts
        in
        some result
  in
  let*! () =
    Evm_node.make_l2_kernel_installer_config
      ~chain_id:l2_setup.l2_chain_id
      ~chain_family:l2_setup.l2_chain_family
      ?maximum_gas_per_transaction:l2_setup.maximum_gas_per_transaction
      ?sequencer_pool_address:l2_setup.sequencer_pool_address
      ?minimum_base_fee_per_gas:l2_setup.minimum_base_fee_per_gas
      ?da_fee_per_byte:l2_setup.da_fee_per_byte
      ?eth_bootstrap_accounts:l2_setup.eth_bootstrap_accounts
      ?tez_bootstrap_accounts:l2_setup.tez_bootstrap_accounts
      ?tez_bootstrap_contracts
      ?world_state_path:l2_setup.world_state_path
      ~output:l2_config
      ()
  in
  return l2_config

let setup_kernel_multichain ~(l2_setups : Evm_node.l2_setup list) ~l1_contracts
    ?max_delayed_inbox_blueprint_length ~mainnet_compat ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?maximum_allowed_ticks
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ~enable_revm
    ?enable_fast_withdrawal ?enable_fast_fa_withdrawal ~enable_dal ?dal_slots
    ~sequencer ~preimages_dir ?evm_version ~kernel ~client () =
  let l2_chain_ids = List.map (fun l2 -> l2.Evm_node.l2_chain_id) l2_setups in
  let* l2_configs =
    Lwt_list.map_s (fun s -> generate_l2_kernel_config s client) l2_setups
  in
  let rollup_config = Temp.file "rollup-config.yaml" in
  (* To keep backwards compatibility, we also write to the current durable storage
     paths the variables that have been moved if we have a single chain. *)
  let ( minimum_base_fee_per_gas,
        da_fee_per_byte,
        sequencer_pool_address,
        maximum_gas_per_transaction,
        eth_bootstrap_accounts ) =
    match l2_setups with
    | [
     {
       minimum_base_fee_per_gas;
       da_fee_per_byte;
       sequencer_pool_address;
       maximum_gas_per_transaction;
       world_state_path;
       eth_bootstrap_accounts;
       _;
     };
    ] ->
        let eth_bootstrap_accounts =
          (* If `world_state_path` is `/evm/world_state`, it means the bootstrap accounts have already been written to that path.
             To avoid duplicating this information in the configuration file — which would unnecessarily bloat the rollup origination
             operation and cause an error — we skip including them here. *)
          if world_state_path = Some "/evm/world_state" then None
          else eth_bootstrap_accounts
        in

        ( minimum_base_fee_per_gas,
          da_fee_per_byte,
          sequencer_pool_address,
          maximum_gas_per_transaction,
          eth_bootstrap_accounts )
    | _ -> assert false
  in
  (* In the kernel, the multichain notion was not introduced yet. *)
  (* The kernel rely on its chain id to know the chain_family.    *)
  (* We need to keep this as long as we register test as multichain
     just for tezlink (and not real multichain) *)
  let chain_id =
    match l2_chain_ids with [chain_id] -> Some chain_id | _ -> None
  in
  let*! () =
    Evm_node.make_kernel_installer_config
      ?chain_id
      ~l2_chain_ids
      ?max_delayed_inbox_blueprint_length
      ~mainnet_compat
      ~sequencer
      ~delayed_bridge:l1_contracts.delayed_transaction_bridge
      ~ticketer:l1_contracts.exchanger
      ~administrator:l1_contracts.admin
      ~sequencer_governance:l1_contracts.sequencer_governance
      ?minimum_base_fee_per_gas
      ?da_fee_per_byte
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?sequencer_pool_address
      ?maximum_allowed_ticks
      ?maximum_gas_per_transaction
      ~enable_dal
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?dal_slots
      ~enable_multichain:true
      ?max_blueprint_lookahead_in_seconds
      ?eth_bootstrap_accounts
      ~output:rollup_config
      ?enable_fa_bridge
      ~enable_revm
      ?evm_version
      ()
  in
  let* {output; _} =
    prepare_installer_kernel_with_multiple_setup_file
      ~preimages_dir
      ~configs:(rollup_config :: l2_configs)
      (Uses.path kernel)
  in
  return output

let setup_kernel ~enable_multichain ~l2_chains ~l1_contracts
    ?max_delayed_inbox_blueprint_length ~mainnet_compat ~sequencer
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?maximum_allowed_ticks
    ~enable_dal ?enable_fast_withdrawal ?enable_fast_fa_withdrawal ?dal_slots
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ~enable_revm
    ~preimages_dir ~kernel ?evm_version ~client () =
  if not enable_multichain then (
    assert (List.length l2_chains = 1) ;
    let chain_config = List.hd l2_chains in
    setup_kernel_singlechain
      ~l1_contracts
      ?max_delayed_inbox_blueprint_length
      ~mainnet_compat
      ~sequencer:sequencer.Account.public_key
      ?minimum_base_fee_per_gas:chain_config.minimum_base_fee_per_gas
      ?da_fee_per_byte:chain_config.da_fee_per_byte
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?sequencer_pool_address:chain_config.sequencer_pool_address
      ?maximum_allowed_ticks
      ?maximum_gas_per_transaction:chain_config.maximum_gas_per_transaction
      ~enable_dal
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?dal_slots
      ?max_blueprint_lookahead_in_seconds
      ?eth_bootstrap_accounts:chain_config.Evm_node.eth_bootstrap_accounts
      ?enable_fa_bridge
      ~enable_revm
      ?evm_version
      ~preimages_dir
      ~kernel
      ())
  else
    setup_kernel_multichain
      ~l2_setups:l2_chains
      ~l1_contracts
      ?max_delayed_inbox_blueprint_length
      ~mainnet_compat
      ~sequencer:sequencer.Account.public_key
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?maximum_allowed_ticks
      ~enable_dal
      ?enable_fa_bridge
      ~enable_revm
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?dal_slots
      ?max_blueprint_lookahead_in_seconds
      ?evm_version
      ~preimages_dir
      ~kernel
      ~client
      ()

let setup_sequencer_internal ?max_delayed_inbox_blueprint_length
    ?next_wasm_runtime ?sequencer_rpc_port ?sequencer_private_rpc_port
    ~mainnet_compat ?genesis_timestamp ?time_between_blocks ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?max_number_of_chunks
    ?commitment_period ?challenge_window ?(sequencer = Constant.bootstrap1)
    ?(kernel = Constant.WASM.evm_kernel) ?evm_version ?preimages_dir
    ?maximum_allowed_ticks ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge
    ~enable_revm ?enable_fast_withdrawal ?enable_fast_fa_withdrawal
    ?(drop_duplicate_when_injection = true)
    ?(blueprints_publisher_order_enabled = true) ?rollup_history_mode
    ~enable_dal ?dal_slots ~enable_multichain ~l2_chains ?rpc_server ?websockets
    ?history_mode ?enable_tx_queue ?spawn_rpc ?periodic_snapshot_path protocol =
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
      let* () = Dal_node.init_config ?operator_profiles:dal_slots dal_node in
      let* () = Dal_node.run ~wait_ready:true dal_node in
      some dal_node
    else none
  in
  let client = Client.with_dal_node client ?dal_node in
  let* l1_contracts =
    setup_l1_contracts ~kernel:(Kernel.of_tag_use kernel) client
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ?dal_node
      ?history_mode:rollup_history_mode
  in
  let preimages_dir =
    Option.value
      ~default:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
      preimages_dir
  in

  let* output =
    setup_kernel
      ~l1_contracts
      ?max_delayed_inbox_blueprint_length
      ~mainnet_compat
      ~sequencer
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ?maximum_allowed_ticks
      ~enable_dal
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?dal_slots
      ~enable_multichain
      ~l2_chains
      ?evm_version
      ?max_blueprint_lookahead_in_seconds
      ?enable_fa_bridge
      ~enable_revm
      ~preimages_dir
      ~kernel
      ~client
      ()
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:Test_helpers.evm_type
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
  let seq_patch_config =
    Evm_node.patch_config_with_experimental_feature
      ?l2_chains:(if enable_multichain then Some l2_chains else None)
      ~drop_duplicate_when_injection
      ~blueprints_publisher_order_enabled
      ?next_wasm_runtime
      ?rpc_server
      ?spawn_rpc
      ?enable_tx_queue
      (* When adding new experimental feature please make sure it's a
         good idea to activate it for all test or not. *)
      ()
  in
  let obs_patch_config =
    Evm_node.patch_config_with_experimental_feature
      ?l2_chains:(if enable_multichain then Some l2_chains else None)
      ~drop_duplicate_when_injection
      ~blueprints_publisher_order_enabled
      ?next_wasm_runtime
      ?rpc_server
      ?enable_tx_queue
      ?periodic_snapshot_path
      ()
  in
  let proxy_patch_config =
    Evm_node.patch_config_with_experimental_feature
      ?l2_chains:(if enable_multichain then Some l2_chains else None)
      ~drop_duplicate_when_injection
      ~blueprints_publisher_order_enabled
      ?next_wasm_runtime
      ?rpc_server
      ?enable_tx_queue
      ?periodic_snapshot_path
      ()
  in
  let sequencer_mode =
    Evm_node.Sequencer
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
      ~patch_config:seq_patch_config
      ~mode:sequencer_mode
      ?history_mode
      ?spawn_rpc
      ?websockets
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* observers =
    Lwt_list.map_s
      (fun _l2 ->
        run_new_observer_node
          ~patch_config:obs_patch_config
          ~sc_rollup_node
          ?rpc_server
          ?websockets
          ?history_mode
          ?enable_tx_queue
          sequencer)
      l2_chains
  in
  (* Launching the sequencer made it produced the first blueprint, we
     need to bake a block to include it in the inbox, which will
     trigger the installation of the kernel in the rollup. *)
  let* _lvl = Test_helpers.next_rollup_node_level ~sc_rollup_node ~client in
  let* proxies =
    Lwt_list.map_s
      (fun _l2 ->
        Evm_node.init
          ~patch_config:proxy_patch_config
          ~mode:Proxy
          (Sc_rollup_node.endpoint sc_rollup_node))
      l2_chains
  in
  let evm_version =
    Kernel.select_evm_version (Kernel.of_tag_use kernel) ?evm_version
  in
  return
    {
      node;
      client;
      sequencer;
      proxies;
      observers;
      l1_contracts;
      sc_rollup_address;
      sc_rollup_node;
      boot_sector = output;
      kernel;
      enable_dal;
      evm_version;
      enable_multichain;
      l2_chains;
    }

let setup_sequencer ?max_delayed_inbox_blueprint_length ?next_wasm_runtime
    ?sequencer_rpc_port ?sequencer_private_rpc_port ~mainnet_compat
    ?genesis_timestamp ?time_between_blocks ?max_blueprints_lag
    ?max_blueprints_ahead ?max_blueprints_catchup ?catchup_cooldown
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?max_number_of_chunks
    ?commitment_period ?challenge_window
    ?(eth_bootstrap_accounts = Evm_node.eth_default_bootstrap_accounts)
    ?(tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts)
    ?sequencer ?sequencer_pool_address ?kernel ?da_fee ?minimum_base_fee_per_gas
    ?preimages_dir ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ~enable_revm
    ?enable_fast_withdrawal ?enable_fast_fa_withdrawal
    ?drop_duplicate_when_injection ?blueprints_publisher_order_enabled
    ?rollup_history_mode ~enable_dal ?dal_slots ~enable_multichain ?rpc_server
    ?websockets ?history_mode ?enable_tx_queue ?spawn_rpc
    ?periodic_snapshot_path ?l2_chains protocol =
  (* Note that the chain_id is not important (it will become important later) *)
  let l2_chains =
    Option.value
      ~default:
        [
          {
            (Evm_node.default_l2_setup ~l2_chain_id:1) with
            sequencer_pool_address;
            eth_bootstrap_accounts = Some eth_bootstrap_accounts;
            tez_bootstrap_accounts = Some tez_bootstrap_accounts;
            da_fee_per_byte = da_fee;
            minimum_base_fee_per_gas;
            maximum_gas_per_transaction;
          };
        ]
      l2_chains
  in
  let* sequencer_setup =
    setup_sequencer_internal
      ?max_delayed_inbox_blueprint_length
      ?next_wasm_runtime
      ?sequencer_rpc_port
      ?sequencer_private_rpc_port
      ~mainnet_compat
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
      ?sequencer
      ?kernel
      ?preimages_dir
      ?maximum_allowed_ticks
      ?max_blueprint_lookahead_in_seconds
      ?enable_fa_bridge
      ~enable_revm
      ?enable_fast_withdrawal
      ?enable_fast_fa_withdrawal
      ?blueprints_publisher_order_enabled
      ?drop_duplicate_when_injection
      ?rollup_history_mode
      ?websockets
      ?history_mode
      ~enable_dal
      ?dal_slots
      ~enable_multichain
      ~l2_chains
      ?rpc_server
      ?enable_tx_queue
      ?spawn_rpc
      ?periodic_snapshot_path
      protocol
  in
  return (multichain_setup_to_single ~setup:sequencer_setup)

let register_multichain_test ~__FILE__ ?max_delayed_inbox_blueprint_length
    ?sequencer_rpc_port ?sequencer_private_rpc_port ?genesis_timestamp
    ?time_between_blocks ?max_blueprints_lag ?max_blueprints_ahead
    ?max_blueprints_catchup ?catchup_cooldown ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?max_number_of_chunks
    ?(eth_bootstrap_accounts = Evm_node.eth_default_bootstrap_accounts)
    ?(tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts)
    ?tez_bootstrap_contracts ?sequencer ?sequencer_pool_address ~kernel ?da_fee
    ?minimum_base_fee_per_gas ?preimages_dir ?maximum_allowed_ticks
    ?maximum_gas_per_transaction ?max_blueprint_lookahead_in_seconds
    ?enable_fa_bridge ~enable_revm ?enable_fast_withdrawal
    ?enable_fast_fa_withdrawal ?commitment_period ?challenge_window
    ?(uses = uses) ?(additional_uses = []) ?rollup_history_mode ~enable_dal
    ?(dal_slots = if enable_dal then Some [0; 1; 2; 3] else None)
    ~enable_multichain ~l2_setups ?rpc_server ?websockets ?history_mode
    ?enable_tx_queue ?spawn_rpc ?periodic_snapshot_path body ~title ~tags
    protocols =
  let kernel_tag, kernel_use = Kernel.to_uses_and_tags kernel in
  let tags = kernel_tag :: tags in
  let additional_uses =
    [kernel_use]
    @ (if enable_dal then [Constant.octez_dal_node] else [])
    @ additional_uses
  in
  let rpc_server =
    match (rpc_server, kernel) with
    | Some _, _ -> rpc_server
    | _, Mainnet -> None (* default *)
    | _, Latest -> Some Evm_node.Dream (* test with Dream for latest kernel *)
  in
  let l2_chains =
    match l2_setups with
    | None ->
        [
          {
            (Evm_node.default_l2_setup ~l2_chain_id:1337) with
            da_fee_per_byte = da_fee;
            sequencer_pool_address;
            minimum_base_fee_per_gas;
            maximum_gas_per_transaction;
            eth_bootstrap_accounts = Some eth_bootstrap_accounts;
            tez_bootstrap_accounts = Some tez_bootstrap_accounts;
            tez_bootstrap_contracts;
          };
        ]
    | Some l2_chains -> l2_chains
  in
  let body protocol =
    let* sequencer_setup =
      setup_sequencer_internal
        ?max_delayed_inbox_blueprint_length
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
        ?sequencer
        ~kernel:kernel_use
        ?preimages_dir
        ?maximum_allowed_ticks
        ?max_blueprint_lookahead_in_seconds
        ?enable_fa_bridge
        ~enable_revm
        ?enable_fast_withdrawal
        ?enable_fast_fa_withdrawal
        ?rollup_history_mode
        ?websockets
        ?history_mode
        ~enable_dal
        ?dal_slots
        ~enable_multichain
        ~l2_chains
        ?rpc_server
        ?enable_tx_queue
        ?spawn_rpc
        ?periodic_snapshot_path
        protocol
    in
    body sequencer_setup protocol
  in
  let tags =
    (if enable_dal then ["dal"] else [])
    @ (if enable_multichain then ["multichain_enabled"] else [])
    @ (if enable_revm then ["revm"] else [])
    @ tags
  in
  let title =
    sf
      "%s (%s, %s, %s, %s)"
      title
      kernel_tag
      (if enable_dal then "with dal" else "without dal")
      (if enable_multichain then "multichain" else "single chain")
      (if enable_revm then "with revm" else "without revm")
  in
  if
    (* Only register DAL tests for supporting kernels *)
    ((not enable_dal) || Kernel.supports_dal kernel)
    && (* Only register REVM tests for supporting kernels *)
    ((not enable_revm) || Kernel.supports_revm kernel)
  then
    Protocol.register_test
      ~additional_tags:(function
        | Alpha -> []
        | _ ->
            (* There is no point in testing the multichain feature on non-alpha protocols *)
            [(if enable_multichain then Tag.ci_disabled else Tag.extra)])
      ~__FILE__
      ~uses:(fun protocol -> uses protocol @ additional_uses)
      body
      ~title
      ~tags
      protocols

(* Register a single variant of a test but for all protocols. *)
let register_test ~__FILE__ ?max_delayed_inbox_blueprint_length
    ?sequencer_rpc_port ?sequencer_private_rpc_port ?genesis_timestamp
    ?time_between_blocks ?max_blueprints_lag ?max_blueprints_ahead
    ?max_blueprints_catchup ?catchup_cooldown ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?max_number_of_chunks
    ?(eth_bootstrap_accounts = Evm_node.eth_default_bootstrap_accounts)
    ?(tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts)
    ?sequencer ?sequencer_pool_address ~kernel ?da_fee ?minimum_base_fee_per_gas
    ?preimages_dir ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ~enable_revm
    ?enable_fast_withdrawal ?enable_fast_fa_withdrawal ?commitment_period
    ?challenge_window ?uses ?additional_uses ?rollup_history_mode ~enable_dal
    ?dal_slots ~enable_multichain ?rpc_server ?websockets ?history_mode
    ?enable_tx_queue ?spawn_rpc ?periodic_snapshot_path ?l2_setups body ~title
    ~tags protocols =
  let body sequencer_setup =
    body (multichain_setup_to_single ~setup:sequencer_setup)
  in
  register_multichain_test
    ~__FILE__
    ?max_delayed_inbox_blueprint_length
    ?sequencer_rpc_port
    ?sequencer_private_rpc_port
    ?genesis_timestamp
    ?time_between_blocks
    ?max_blueprints_lag
    ?max_blueprints_ahead
    ?max_blueprints_catchup
    ?catchup_cooldown
    ?delayed_inbox_timeout
    ?delayed_inbox_min_levels
    ?max_number_of_chunks
    ~eth_bootstrap_accounts
    ~tez_bootstrap_accounts
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
    ~enable_revm
    ?enable_fast_withdrawal
    ?enable_fast_fa_withdrawal
    ?commitment_period
    ?challenge_window
    ?uses
    ?additional_uses
    ?rollup_history_mode
    ~enable_dal
    ?dal_slots
    ~enable_multichain
    ?rpc_server
    ?websockets
    ?history_mode
    ?enable_tx_queue
    ?spawn_rpc
    ?periodic_snapshot_path
    ~l2_setups
    body
    ~title
    ~tags
    protocols

let register_test_for_kernels ~__FILE__ ?max_delayed_inbox_blueprint_length
    ?sequencer_rpc_port ?sequencer_private_rpc_port ?genesis_timestamp
    ?time_between_blocks ?max_blueprints_lag ?max_blueprints_ahead
    ?max_blueprints_catchup ?catchup_cooldown ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?max_number_of_chunks
    ?(eth_bootstrap_accounts = Evm_node.eth_default_bootstrap_accounts)
    ?(tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts)
    ?sequencer ?sequencer_pool_address ?(kernels = Kernel.all) ?da_fee
    ?minimum_base_fee_per_gas ?preimages_dir ?maximum_allowed_ticks
    ?maximum_gas_per_transaction ?max_blueprint_lookahead_in_seconds
    ?enable_fa_bridge ~enable_revm ?rollup_history_mode ?commitment_period
    ?challenge_window ?additional_uses ~enable_dal ?dal_slots ~enable_multichain
    ?rpc_server ?websockets ?enable_fast_withdrawal ?enable_fast_fa_withdrawal
    ?history_mode ?enable_tx_queue ?spawn_rpc ?periodic_snapshot_path ?l2_setups
    ~title ~tags body protocols =
  List.iter
    (fun kernel ->
      register_test
        ~__FILE__
        ?max_delayed_inbox_blueprint_length
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
        ~eth_bootstrap_accounts
        ~tez_bootstrap_accounts
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
        ~enable_revm
        ?enable_fast_withdrawal
        ?enable_fast_fa_withdrawal
        ?additional_uses
        ?rpc_server
        ?websockets
        ?history_mode
        ?rollup_history_mode
        ~enable_dal
        ?dal_slots
        ~enable_multichain
        ?enable_tx_queue
        ?spawn_rpc
        ?periodic_snapshot_path
        ?l2_setups
        ~title
        ~tags
        body
        protocols)
    kernels

type feature_test_registration =
  | Register_with_feature
  | Register_without_feature
  | Register_both of {
      additional_tags_with : string list;
      additional_tags_without : string list;
    }
[@@warning "-unused-constructor"]

let default_dal_registration =
  Register_both
    {additional_tags_with = [Tag.extra]; additional_tags_without = []}

let ci_enabled_dal_registration =
  Register_both {additional_tags_with = []; additional_tags_without = []}

let default_multichain_registration =
  Register_both
    {additional_tags_with = [Tag.extra]; additional_tags_without = []}

(* By default REVM is completely disabled. *)
let default_revm_registration = Register_without_feature

(* Use this value to register a specific test in the CI. *)
let activate_revm_registration =
  Register_both {additional_tags_with = []; additional_tags_without = []}

(* Register all variants of a test. *)
let register_all ~__FILE__ ?max_delayed_inbox_blueprint_length
    ?sequencer_rpc_port ?sequencer_private_rpc_port ?genesis_timestamp
    ?time_between_blocks ?max_blueprints_lag ?max_blueprints_ahead
    ?max_blueprints_catchup ?catchup_cooldown ?delayed_inbox_timeout
    ?delayed_inbox_min_levels ?max_number_of_chunks ?eth_bootstrap_accounts
    ?tez_bootstrap_accounts ?sequencer ?sequencer_pool_address
    ?(kernels = Kernel.all) ?da_fee ?minimum_base_fee_per_gas ?preimages_dir
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ?rollup_history_mode
    ?commitment_period ?challenge_window ?additional_uses ?rpc_server
    ?websockets ?enable_fast_withdrawal ?enable_fast_fa_withdrawal ?history_mode
    ?(use_dal = default_dal_registration)
    ?(use_multichain = default_multichain_registration)
    ?(use_revm = default_revm_registration) ?enable_tx_queue ?spawn_rpc
    ?periodic_snapshot_path ?l2_setups ~title ~tags body protocols =
  let register_cases = function
    | Register_both {additional_tags_with; additional_tags_without} ->
        [(false, additional_tags_without); (true, additional_tags_with)]
    | Register_with_feature -> [(true, [])]
    | Register_without_feature -> [(false, [])]
  in
  let dal_cases = register_cases use_dal in
  let multichain_cases = register_cases use_multichain in
  let revm_cases = register_cases use_revm in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7367
     Also register the tests with and without FA bridge feature flag. *)
  List.iter
    (fun (enable_dal, dal_tags) ->
      List.iter
        (fun (enable_multichain, multichain_tags) ->
          List.iter
            (fun (enable_revm, revm_tags) ->
              (* Since the set of RPCs the sequencer has access to is restricted in the multichain case,
                 we need the intermediate RPC node to handle the extra RPCs necessary in the tests. *)
              let spawn_rpc =
                match spawn_rpc with
                | None when enable_multichain -> Some (Port.fresh ())
                | _ -> spawn_rpc
              in
              register_test_for_kernels
                ~__FILE__
                ?max_delayed_inbox_blueprint_length
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
                ?eth_bootstrap_accounts
                ?tez_bootstrap_accounts
                ?sequencer
                ?sequencer_pool_address
                ~kernels
                ?da_fee
                ?minimum_base_fee_per_gas
                ?preimages_dir
                ?maximum_allowed_ticks
                ?maximum_gas_per_transaction
                ?max_blueprint_lookahead_in_seconds
                ?enable_fa_bridge
                ~enable_revm
                ?enable_fast_withdrawal
                ?enable_fast_fa_withdrawal
                ?additional_uses
                ?rpc_server
                ?websockets
                ?history_mode
                ?rollup_history_mode
                ~enable_dal
                ~enable_multichain
                ?enable_tx_queue
                ?spawn_rpc
                ?periodic_snapshot_path
                ?l2_setups
                ~title
                ~tags:(dal_tags @ multichain_tags @ revm_tags @ tags)
                body
                protocols)
            revm_cases)
        multichain_cases)
    dal_cases
