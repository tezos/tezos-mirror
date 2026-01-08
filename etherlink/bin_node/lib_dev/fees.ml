(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

(*

  On Etherlink, the gas serves two purposes:

    1. It provides a constraints on what can be executed by a given
       transaction.
    2. It accounts for the inclusion fees users pay to the sequencer for their
       transaction to be included on the parent chain.

   Each unit of gas is paid by the user with a base fee per gas. Contrary to
   other EVM chains, Etherlink has a minimum bas fee per gas in order to ensure
   that the sequencer will receive enough inclusion fees. The inclusion fees
   are a fixed cost per byte (in XTZ). So the higher the gas price, the less
   unit of gas are required to pay it.

   When it comes to computation involving fees, we consider the following input

     - [calldata] length from the transaction
     - [access_list] length from the transaction
     - [gas_limit] from the transaction
     - [minimum_base_fee_per_gas] the floor price for a gas unit
     - [base_fee_per_gas] the current price for a gas unit
     - [da_fee_per_byte] the constant price for a byte to include on the L1

   Based on the semantics introduced by Dionysus, we compute four values

     - [gas_used_for_da_fees] which is the number of gas units consumed to
       reimburse the sequencer, given the current [base_fee_per_gas]
     - [da_fees_overhead] is the amount of gas which a transaction needs to
       allocate in order for the sender to pay the inclusion fees when
       [base_fee_per_gas = minimum_base_fee_per_gas]. The kernel will however only
       consume the gas necessary to cover the inclusion fee for the current
       [base_fee_per_gas].
     - [execution_gas_limit = gas_limit - da_fees_overhead] computed by the
       kernel (since Dionysus) as the maximum amount of gas unit the
       transaction will be able to consume to be executed
     - [execution_gas = gas_used - gas_used_for_da_fees] the actual amount of
       gas unit used by the transaction.

 *)

let gas_used_for_da_fees ~da_fee_per_byte:(Ethereum_types.Qty da_fee_per_byte)
    ~base_fee_per_gas ?access_list tx_data =
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
        (fun acc ({address = _; storage_keys} : Transaction_object.access) ->
          let nb_slots = Z.of_int (List.length storage_keys) in
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
  Z.cdiv fees base_fee_per_gas

let da_fees_gas_limit_overhead ~da_fee_per_byte ~minimum_base_fee_per_gas
    ?access_list tx_data =
  gas_used_for_da_fees
    ~da_fee_per_byte
    ~base_fee_per_gas:minimum_base_fee_per_gas
    ?access_list
    tx_data

let execution_gas_limit ~da_fee_per_byte ~minimum_base_fee_per_gas ?access_list
    ~gas_limit tx_data =
  let da_gas =
    da_fees_gas_limit_overhead
      ~da_fee_per_byte
      ~minimum_base_fee_per_gas
      ?access_list
      tx_data
  in
  let result = Z.sub gas_limit da_gas in
  if Compare.Z.(result <= Z.zero) then
    Format.kasprintf
      Result.error
      "Not enough gas for inclusion fees. Provided %a but inclusion consumes %a"
      Z.pp_print
      gas_limit
      Z.pp_print
      da_gas
  else Ok result
