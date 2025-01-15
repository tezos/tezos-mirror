(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val run :
  relay_endpoint:Uri.t ->
  rpc_endpoint:Uri.t ->
  controller:Account.Secret_key.t ->
  max_active_eoa:int ->
  spawn_interval:float ->
  tick_interval:float ->
  base_fee_factor:float ->
  initial_balance:Z.t ->
  unit tzresult Lwt.t
