(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

val run :
  scenario:[< `ERC20 | `XTZ] ->
  relay_endpoint:Uri.t ->
  rpc_endpoint:Uri.t ->
  controller:Account.Secret_key.t ->
  max_active_eoa:int ->
  max_transaction_batch_length:int option ->
  spawn_interval:float ->
  tick_interval:float ->
  base_fee_factor:float ->
  initial_balance:Z.t ->
  txs_per_salvo:int ->
  elapsed_time_between_report:float ->
  unit tzresult Lwt.t
