(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

let gas_for_fees ~da_fee_per_byte:(Ethereum_types.Qty da_fee_per_byte)
    ~gas_price ?access_list tx_data =
  (* The computation of the gas_for_fees comes from the kernel in fees.rs
     in the gas_for_fees function *)
  (* Constants defined in the kernel: *)
  let assumed_tx_encoded_size = 150 in
  (* Computation of da fee based on da fee per byte and variable tx data. *)
  let da_fee ?(access_list = []) da_fee_per_byte tx_data =
    (* Compute access_list size for da_fees *)
    let size_address = Z.of_int 20 in
    let size_slot = Z.of_int 32 in
    let access_list_size =
      List.fold_left
        (fun acc (_, slots) ->
          let nb_slots = Z.of_int (List.length slots) in
          let slots_cost = Z.mul size_slot nb_slots in
          Z.add (Z.add acc slots_cost) size_address)
        Z.zero
        access_list
    in
    let size_wo_access_list =
      Bytes.length tx_data + assumed_tx_encoded_size |> Z.of_int
    in
    let size = Z.add size_wo_access_list access_list_size in
    Z.mul da_fee_per_byte size
  in
  let fees = da_fee ?access_list da_fee_per_byte tx_data in
  Z.cdiv fees gas_price
