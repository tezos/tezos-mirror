(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {chain_id : L2_types.chain_id; base_fee_per_gas : Z.t}

val timeout : float

val fetch : rpc_endpoint:Uri.t -> base_fee_factor:float -> t tzresult Lwt.t

val get_gas_limit :
  ?data:Ethereum_types.hash ->
  ?from:Ethereum_types.address ->
  ?to_:Ethereum_types.address ->
  ?value:Z.t ->
  rpc_endpoint:Uri.t ->
  base_fee_per_gas:Z.t ->
  unit ->
  Z.t tzresult Lwt.t
