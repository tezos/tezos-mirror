(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

(** [gas_for_fees ~da_fee_per_byte ~gas_price ?access_list tx_data] returns
      the DA fees, i.e. the gas unit necessary for the data availability.

      The whole point of this function is to avoid an unncessary call
      to the WASM PVM to improve the performances. *)
val gas_for_fees :
  da_fee_per_byte:Ethereum_types.quantity ->
  gas_price:Z.t ->
  ?access_list:Transaction.access_list_item list ->
  bytes ->
  Z.t
