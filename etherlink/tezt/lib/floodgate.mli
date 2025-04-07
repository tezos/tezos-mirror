(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Spawn floodgate *)
val run :
  ?runner:Runner.t ->
  ?path:string ->
  ?scenario:[< `ERC20 | `XTZ > `XTZ] ->
  rpc_endpoint:string ->
  controller:Eth_account.t ->
  ?relay_endpoint:string ->
  ?max_active_eoa:int ->
  ?max_transaction_batch_length:int ->
  ?spawn_interval:float ->
  ?tick_interval:float ->
  ?base_fee_factor:float ->
  ?initial_balance:Wei.t ->
  unit ->
  unit Lwt.t
