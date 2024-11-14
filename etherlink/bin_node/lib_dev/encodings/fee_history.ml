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

let encoding =
  let open Data_encoding in
  conv
    (fun {oldest_block; base_fee_per_gas; gas_used_ratio} ->
      (oldest_block, base_fee_per_gas, gas_used_ratio))
    (fun (oldest_block, base_fee_per_gas, gas_used_ratio) ->
      {oldest_block; base_fee_per_gas; gas_used_ratio})
    (obj3
       (req "oldestBlock" Ethereum_types.quantity_encoding)
       (req "baseFeePerGas" (list Ethereum_types.quantity_encoding))
       (req "gasUsedRatio" (list float)))
