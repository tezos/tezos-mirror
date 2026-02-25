(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

type evm_version = Shanghai | Cancun | Prague | Osaka

(** [make ~boostrap_balance ?bootstrap_accounts ... ~output ()]
    generates a configuration file located at [output], where
    [bootstrap_accounts] are provisioned with [bootstrap_balance]. *)
val make :
  ?kernel_compat:Constants.kernel ->
  eth_bootstrap_balance:Z.t ->
  ?l2_chain_ids:L2_types.chain_id list ->
  ?eth_bootstrap_accounts:Ethereum_types.address list ->
  ?kernel_root_hash:string * string ->
  ?chain_id:string * string ->
  ?sequencer:string * string ->
  ?delayed_bridge:string * string ->
  ?ticketer:string * string ->
  ?admin:string * string ->
  ?sequencer_governance:string * string ->
  ?kernel_governance:string * string ->
  ?kernel_security_governance:string * string ->
  ?minimum_base_fee_per_gas:string * string ->
  ?da_fee_per_byte:string * string ->
  ?delayed_inbox_timeout:string * string ->
  ?delayed_inbox_min_levels:string * string ->
  ?sequencer_pool_address:string * string ->
  ?maximum_allowed_ticks:string * string ->
  ?maximum_gas_per_transaction:string * string ->
  ?max_blueprint_lookahead_in_seconds:string * string ->
  ?remove_whitelist:string * string ->
  ?enable_fa_bridge:string * string ->
  ?enable_revm:string * string ->
  ?enable_dal:string * string ->
  ?dal_slots:string * string ->
  ?dal_publishers_whitelist:string * string ->
  ?disable_legacy_dal_signals:string * string ->
  ?enable_fast_withdrawal:string * string ->
  ?enable_fast_fa_withdrawal:string * string ->
  ?enable_multichain:string * string ->
  ?set_account_code:(string * string) list ->
  ?max_delayed_inbox_blueprint_length:string * string ->
  ?evm_version:evm_version ->
  ?with_runtimes:Tezosx.runtime list ->
  ?tez_bootstrap_accounts:Signature.V2.public_key list ->
  tez_bootstrap_balance:Tezos_types.Tez.t ->
  output:string ->
  unit ->
  unit tzresult Lwt.t

(** [make_l2 ~boostrap_balance ?bootstrap_accounts ... ~l2_chain_id ~output ()]
    generates a configuration file located at [output] for the chain [l2_chain_id],
    where [bootstrap_accounts] are provisioned with [bootstrap_balance]. *)
val make_l2 :
  eth_bootstrap_balance:Z.t ->
  tez_bootstrap_balance:Tezos_types.Tez.t ->
  ?eth_bootstrap_accounts:Ethereum_types.address list ->
  ?tez_bootstrap_accounts:Signature.V2.Public_key.t list ->
  ?tez_bootstrap_contracts:(Tezos_types.Contract.t * string * string) list ->
  ?minimum_base_fee_per_gas:string * string ->
  ?da_fee_per_byte:string * string ->
  ?sequencer_pool_address:string * string ->
  ?maximum_gas_per_transaction:string * string ->
  ?set_account_code:(string * string) list ->
  ?world_state_path:string * string ->
  l2_chain_id:string ->
  l2_chain_family:_ L2_types.chain_family ->
  output:string ->
  unit ->
  unit tzresult Lwt.t
