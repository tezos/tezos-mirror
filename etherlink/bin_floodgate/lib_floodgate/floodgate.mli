(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type attempt = Always | Never | Number of int

val run :
  scenario:[< `ERC20 | `XTZ] ->
  relay_endpoint:Uri.t ->
  rpc_endpoint:Uri.t ->
  ws_endpoint:Uri.t option ->
  controller:Signer.t ->
  max_active_eoa:int ->
  max_transaction_batch_length:int option ->
  spawn_interval:float ->
  tick_interval:float ->
  base_fee_factor:float ->
  initial_balance:Z.t ->
  txs_per_salvo:int ->
  elapsed_time_between_report:float ->
  dummy_data_size:int option ->
  retry_attempt:attempt ->
  unit tzresult Lwt.t

val deploy :
  ?nonce:Z.t ->
  ?gas_limit:Z.t ->
  rpc_endpoint:Uri.t ->
  scenario:[< `Custom of string | `ERC20] ->
  Network_info.t ->
  Account.t ->
  string tzresult Lwt.t

val start_new_head_monitor : ws_uri:Uri.t -> unit tzresult Lwt.t

val start_blueprint_follower :
  relay_endpoint:Uri.t -> rpc_endpoint:Uri.t -> 'a tzresult Lwt.t

val get_transaction_receipt :
  Uri.t -> Ethereum_types.hash -> Transaction_receipt.t tzresult Lwt.t
