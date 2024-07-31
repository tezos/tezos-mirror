(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

let gas_for_fees ?da_fee_per_byte ~gas_price tx_data =
  (* The computation of the gas_for_fees comes from the kernel in fees.rs
     in the gas_for_fees function *)
  (* Constants defined in the kernel: *)
  let assumed_tx_encoded_size = 150 in
  let default_da_fee_per_byte =
    (* 4 * 10^12, 4 mutez *)
    Ethereum_types.quantity_of_z (Z.of_string "4_000_000_000_000")
  in
  let (Qty da_fee_per_byte) =
    Option.value ~default:default_da_fee_per_byte da_fee_per_byte
  in
  (* Computation of da fee based on da fee per byte and variable tx data. *)
  let da_fee da_fee_per_byte tx_data =
    let size = Bytes.length tx_data + assumed_tx_encoded_size |> Z.of_int in
    Z.mul da_fee_per_byte size
  in
  let fees = da_fee da_fee_per_byte tx_data in
  Z.div fees gas_price
