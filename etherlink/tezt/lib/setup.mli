(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

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

(** [uses protocol] returns the list of dependencies for the tests. *)
val uses : Protocol.t -> Uses.t list

(** [setup_l1_contracts ~dictator client] setups the necessary contracts for the
    rollup. *)
val setup_l1_contracts : ?dictator:Account.key -> Client.t -> l1_contracts Lwt.t

(** [run_new_rpc_endpoint node] starts a new rpc node following the setup. *)
val run_new_rpc_endpoint : Evm_node.t -> Evm_node.t Lwt.t

(**[run_new_rpc_endpoint ~sc_rollup_node node] starts a new observer following
   the setup. *)
val run_new_observer_node :
  ?patch_config:(Tezt_wrapper.JSON.t -> Tezt_wrapper.JSON.t) ->
  sc_rollup_node:Sc_rollup_node.t ->
  Evm_node.t ->
  Evm_node.t Lwt.t

(** [register_test] setups the full sequencer environment for the
    tests and starts the test. For each of the parameters, please refer to
    [Evm_node]. *)
val register_test :
  __FILE__:string ->
  ?block_storage_sqlite3:bool ->
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
  ?bootstrap_accounts:string list ->
  ?sequencer:Account.key ->
  ?sequencer_pool_address:string ->
  kernel:Kernel.t ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?threshold_encryption:bool ->
  ?uses:(Protocol.t -> Uses.t list) ->
  ?additional_uses:Uses.t list ->
  ?history_mode:Sc_rollup_node.history_mode ->
  enable_dal:bool ->
  ?dal_slots:int list option ->
  (sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  title:string ->
  tags:string list ->
  Protocol.t list ->
  unit

(** [register_test_for_kernels] setups the full sequencer environment for the
    tests and starts the test. For each of the parameters, please refer to
    [Evm_node]. The test is registered for each kernel. *)
val register_test_for_kernels :
  __FILE__:string ->
  ?block_storage_sqlite3:bool ->
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
  ?bootstrap_accounts:string list ->
  ?sequencer:Account.key ->
  ?sequencer_pool_address:string ->
  ?kernels:Kernel.t list ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?history_mode:Sc_rollup_node.history_mode ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?additional_uses:Tezt_wrapper.Uses.t list ->
  threshold_encryption:bool ->
  enable_dal:bool ->
  title:string ->
  tags:string list ->
  (sequencer_setup -> Protocol.t -> unit Lwt.t) ->
  Protocol.t list ->
  unit

val setup_sequencer :
  ?block_storage_sqlite3:bool ->
  ?sequencer_rpc_port:int ->
  ?sequencer_private_rpc_port:int ->
  mainnet_compat:bool ->
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
  ?bootstrap_accounts:string list ->
  ?sequencer:Account.key ->
  ?sequencer_pool_address:string ->
  ?kernel:Uses.t ->
  ?da_fee:Wei.t ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?preimages_dir:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?enable_fa_bridge:bool ->
  ?threshold_encryption:bool ->
  ?drop_duplicate_when_injection:bool ->
  ?history_mode:Sc_rollup_node.history_mode ->
  enable_dal:bool ->
  ?dal_slots:int list ->
  Protocol.t ->
  sequencer_setup Lwt.t
