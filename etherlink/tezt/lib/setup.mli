(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

type l1_contracts = {
  delayed_transaction_bridge : string;
  exchanger : string;
  bridge : string;
  admin : string;
  sequencer_governance : string;
  ticket_router_tester : string;
  fa_deposit : string;
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
  kernel : Kernel.t;
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
  kernel : Kernel.t;
  enable_dal : bool;
  evm_version : Evm_version.t;
  enable_multichain : bool;
  l2_chains : Evm_node.l2_setup list;
}

type tx_queue_config = {
  max_lifespan : int;
  max_size : int;
  tx_per_addr_limit : int;
}

(** [uses protocol] returns the list of dependencies for the tests. *)
val uses : Protocol.t -> Uses.t list

(** [setup_l1_contracts ~dictator ~kernel client] setups the necessary
    contracts for the rollup. *)
val setup_l1_contracts :
  ?dictator:Account.key -> kernel:Kernel.t -> Client.t -> l1_contracts Lwt.t

(** [run_new_rpc_endpoint node] starts a new rpc node following the setup. *)
val run_new_rpc_endpoint : Evm_node.t -> Evm_node.t Lwt.t

(**[run_new_observer_node ~sc_rollup_node node] starts a new observer following
   the setup. *)
val run_new_observer_node :
  ?finalized_view:bool ->
  ?patch_config:(Tezt_wrapper.JSON.t -> Tezt_wrapper.JSON.t) ->
  ?fail_on_divergence:bool ->
  sc_rollup_node:Sc_rollup_node.t option ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?tx_queue:tx_queue_config ->
  ?l2_chain:Evm_node.l2_setup ->
  Evm_node.t ->
  Evm_node.t Lwt.t

(** [register_test] setups the full sequencer environment for the
    tests and starts the test. For each of the parameters, please refer to
    [Evm_node]. *)
val register_test :
  __FILE__:string ->
  ?max_delayed_inbox_blueprint_length:int ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  ?genesis_timestamp:Client.timestamp ->
  ?time_between_blocks:Evm_node.time_between_blocks ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?max_number_of_chunks:int ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer:Account.key ->
  ?additional_sequencer_keys:Account.key list ->
  ?sequencer_pool_address:string ->
  kernel:Kernel.t ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?uses:(Protocol.t -> Uses.t list) ->
  ?additional_uses:Uses.t list ->
  ?rollup_history_mode:Sc_rollup_node.history_mode ->
  enable_dal:bool ->
  ?dal_slots:int list option ->
  ?dal_publishers_whitelist:string list ->
  enable_multichain:bool ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?tx_queue:tx_queue_config ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?signatory:bool ->
  ?l2_setups:Evm_node.l2_setup list ->
  ?sequencer_sunset_sec:int ->
  ?with_runtimes:Tezosx_runtime.t list ->
  ?instant_confirmations:bool ->
  (sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  title:string ->
  tags:string list ->
  Protocol.t list ->
  unit

val register_multichain_test :
  __FILE__:string ->
  ?max_delayed_inbox_blueprint_length:int ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  ?genesis_timestamp:Client.timestamp ->
  ?time_between_blocks:Evm_node.time_between_blocks ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?max_number_of_chunks:int ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?tez_bootstrap_contracts:Evm_node.tez_contract list ->
  ?sequencer:Account.key ->
  ?additional_sequencer_keys:Account.key list ->
  ?sequencer_pool_address:string ->
  kernel:Kernel.t ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?uses:(Protocol.t -> Uses.t list) ->
  ?additional_uses:Uses.t list ->
  ?rollup_history_mode:Sc_rollup_node.history_mode ->
  enable_dal:bool ->
  ?dal_slots:int list option ->
  ?dal_publishers_whitelist:string list ->
  enable_multichain:bool ->
  l2_setups:Evm_node.l2_setup list option ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?tx_queue:tx_queue_config ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?signatory:bool ->
  ?sequencer_sunset_sec:int ->
  ?with_runtimes:Tezosx_runtime.t list ->
  ?instant_confirmations:bool ->
  (multichain_sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  title:string ->
  tags:string list ->
  Protocol.t list ->
  unit

(** [register_test_for_kernels] setups the full sequencer environment for the
    tests and starts the test. For each of the parameters, please refer to
    [Evm_node]. The test is registered for each kernel. *)
val register_test_for_kernels :
  __FILE__:string ->
  ?max_delayed_inbox_blueprint_length:int ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  ?genesis_timestamp:Client.timestamp ->
  ?time_between_blocks:Evm_node.time_between_blocks ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?max_number_of_chunks:int ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer:Account.key ->
  ?additional_sequencer_keys:Account.key list ->
  ?sequencer_pool_address:string ->
  ?kernels:Kernel.t list ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?rollup_history_mode:Sc_rollup_node.history_mode ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?additional_uses:Tezt_wrapper.Uses.t list ->
  enable_dal:bool ->
  ?dal_slots:int list option ->
  ?dal_publishers_whitelist:string list ->
  enable_multichain:bool ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?tx_queue:tx_queue_config ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?signatory:bool ->
  ?l2_setups:Evm_node.l2_setup list ->
  ?sequencer_sunset_sec:int ->
  ?instant_confirmations:bool ->
  title:string ->
  tags:string list ->
  (sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  Protocol.t list ->
  unit

val setup_sequencer :
  ?max_delayed_inbox_blueprint_length:int ->
  ?next_wasm_runtime:bool ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  ?kernel_compat:string ->
  ?genesis_timestamp:Client.timestamp ->
  ?time_between_blocks:Evm_node.time_between_blocks ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?max_number_of_chunks:int ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer:Account.key ->
  ?additional_sequencer_keys:Account.key list ->
  ?sequencer_pool_address:string ->
  ?kernel:Kernel.t ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?drop_duplicate_when_injection:bool ->
  ?blueprints_publisher_order_enabled:bool ->
  ?rollup_history_mode:Sc_rollup_node.history_mode ->
  enable_dal:bool ->
  ?dal_slots:int list ->
  enable_multichain:bool ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?signatory:bool ->
  ?l2_chains:Evm_node.l2_setup list ->
  ?sequencer_sunset_sec:int ->
  ?with_runtimes:Tezosx_runtime.t list ->
  ?instant_confirmations:bool ->
  Protocol.t ->
  sequencer_setup Lwt.t

(* For each feature (threshold encryption, DAL, FA Bridge), tests may
   registered with the feature enabled, with the feature disabled, or both. *)
type feature_test_registration =
  | Register_with_feature
  | Register_without_feature
  | Register_both of {
      additional_tags_with : string list;
      additional_tags_without : string list;
    }
(* We want at most one variant of the test in MR CI, the
   [additional_tags_with] and [additional_tags_without] fields allow to
   select which one by passing [Tag.ci_disabled] or
   [Tag.extra] to the case which should not run in
   MR CI. *)

val ci_enabled_dal_registration : feature_test_registration

val register_all :
  __FILE__:string ->
  ?max_delayed_inbox_blueprint_length:int ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  ?genesis_timestamp:Client.timestamp ->
  ?time_between_blocks:Evm_node.time_between_blocks ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?max_number_of_chunks:int ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer:Account.key ->
  ?additional_sequencer_keys:Account.key list ->
  ?sequencer_pool_address:string ->
  ?kernels:Kernel.t list ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?rollup_history_mode:Sc_rollup_node.history_mode ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?additional_uses:Uses.t list ->
  ?rpc_server:Evm_node.rpc_server ->
  ?websockets:bool ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?history_mode:Evm_node.history_mode ->
  ?use_dal:feature_test_registration ->
  ?use_multichain:feature_test_registration ->
  ?tx_queue:tx_queue_config ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?signatory:bool ->
  ?l2_setups:Evm_node.l2_setup list ->
  ?sequencer_sunset_sec:int ->
  ?instant_confirmations:bool ->
  title:string ->
  tags:string list ->
  (sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  Protocol.t list ->
  unit
