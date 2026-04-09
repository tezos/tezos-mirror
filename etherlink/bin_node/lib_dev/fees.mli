(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

(** Default minimum base fee per gas, set to 1 Gwei.
    Alias for {!Ethereum_types.default_minimum_base_fee_per_gas}. *)
val default_minimum_base_fee_per_gas : Z.t

(** [gas_used_for_da_fees ~da_fee_per_byte ~base_fee_per_gas ?access_list
    ?authorization_list_len tx_data] computes the amount of gas unit taken by
    the kernel to reimburse the sequencer for its L1 costs, given the provided
    [base_fee_per_gas]. [authorization_list_len] is the number of EIP-7702
    authorization entries. *)
val gas_used_for_da_fees :
  da_fee_per_byte:Ethereum_types.quantity ->
  base_fee_per_gas:Z.t ->
  ?access_list:Transaction_object.access list ->
  ?authorization_list_len:int ->
  bytes ->
  Z.t

(** [da_fees_gas_limit_overhead ~da_fee_per_byte ~minimum_base_fee_per_gas
    ?access_list ?authorization_list_len tx_data] returns the overhead (in gas
    unit) to be added to the execution gas limit. This overhead will be used by
    the kernel to reimburse the sequencer for its L1 costs. *)
val da_fees_gas_limit_overhead :
  da_fee_per_byte:Ethereum_types.quantity ->
  minimum_base_fee_per_gas:Z.t ->
  ?access_list:Transaction_object.access list ->
  ?authorization_list_len:int ->
  bytes ->
  Z.t

(** [execution_gas_limit ~da_fee_per_byte ~minimum_base_fee_per_gas
    ?access_list ?authorization_list_len ~gas_limit tx_data] computes the gas
    specifically allocated to executing the transaction from [gas_limit], based
    on the size of [tx_data]. *)
val execution_gas_limit :
  da_fee_per_byte:Ethereum_types.quantity ->
  minimum_base_fee_per_gas:Z.t ->
  ?access_list:Transaction_object.access list ->
  ?authorization_list_len:int ->
  gas_limit:Z.t ->
  bytes ->
  (Z.t, string) result
