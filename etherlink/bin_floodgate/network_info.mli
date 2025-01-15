(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  chain_id : Ethereum_types.quantity;
  base_fee_per_gas : Z.t;
  gas_limit : Z.t;
}

val fetch :
  rpc_endpoint:Uri.t -> base_fee_factor:float -> Account.t -> t tzresult Lwt.t
