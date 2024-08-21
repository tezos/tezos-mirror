(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [make ~boostrap_balance ?bootstrap_accounts ... ~output ()]
    generates a configuration file located at [output], where
    [bootstrap_accounts] are provisioned with [bootstrap_balance]. *)
val make :
  mainnet_compat:bool ->
  boostrap_balance:Ethereum_types.NonceMap.key ->
  ?bootstrap_accounts:string list ->
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
  ?enable_dal:string * string ->
  ?dal_slots:string * string ->
  ?set_account_code:(string * string) list ->
  output:string ->
  unit ->
  unit tzresult Lwt.t
