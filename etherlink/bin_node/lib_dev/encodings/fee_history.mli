(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  oldest_block : Ethereum_types.quantity;
  base_fee_per_gas : Ethereum_types.quantity list;
  gas_used_ratio : float list;
}

val encoding : t Data_encoding.t
