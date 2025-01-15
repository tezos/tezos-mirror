(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let prepare_and_forge_tx ~gas_limit ~base_fee_per_gas ~chain_id ~nonce ~value
    ~sk ~to_ =
  let open Efunc_core in
  let unsigned =
    Eth.
      {
        ti_max_priority_fee = Z.succ base_fee_per_gas;
        ti_max_fee = base_fee_per_gas;
        ti_value = value;
        ti_data = None;
        ti_chain_id = chain_id;
        ti_nonce = nonce;
        ti_gas_limit = Z.to_int gas_limit;
        ti_access_list = [];
        ti_signature = None;
        ti_to = Some to_;
        ti_max_fee_per_blob_gas = None;
        ti_blob_versioned_hashes = [];
        ti_blobs = [];
      }
  in
  let ti_signature =
    Some (Crypto.sign sk (Rope.to_string (Forge.transaction unsigned)))
  in
  Evm.of_rope @@ Forge.transaction {unsigned with ti_signature}

let transfer ?nonce ~infos ~from ~to_ value =
  let nonce = Option.value nonce ~default:from.Account.nonce |> Z.to_int in
  let (Ethereum_types.Qty chain_id) = infos.Network_info.chain_id in
  let txn =
    prepare_and_forge_tx
      ~gas_limit:infos.gas_limit
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~chain_id:(Z.to_int chain_id)
      ~sk:from.secret_key
      ~nonce
      ~to_
      ~value
  in
  Ethereum_types.Hex (txn :> string)
