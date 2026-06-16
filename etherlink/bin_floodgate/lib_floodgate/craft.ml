(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let prepare_and_forge_tx ?to_ ?data ~gas_limit ~base_fee_per_gas ~chain_id
    ~nonce ~value ~signer () =
  let open Efunc_core in
  let open Lwt_result_syntax in
  let unsigned =
    Eth.
      {
        ti_max_priority_fee = Z.succ base_fee_per_gas;
        ti_max_fee = base_fee_per_gas;
        ti_value = value;
        ti_data = data;
        ti_chain_id = chain_id;
        ti_nonce = nonce;
        ti_gas_limit = Z.to_int gas_limit;
        ti_access_list = [];
        ti_signature = None;
        ti_to = to_;
        ti_max_fee_per_blob_gas = None;
        ti_blob_versioned_hashes = [];
        ti_blobs = [];
      }
  in
  let+ ti_signature =
    Signer.sign signer (Rope.to_string (Forge.transaction unsigned))
  in
  Evm.of_rope
  @@ Forge.transaction {unsigned with ti_signature = Some ti_signature}

let transfer ?nonce ?to_ ?data ~value ~gas_limit ~infos ~from () =
  let open Lwt_result_syntax in
  let nonce = Option.value nonce ~default:from.Account.nonce |> Z.to_int in
  let (L2_types.Chain_id chain_id) = infos.Network_info.chain_id in
  let+ txn =
    prepare_and_forge_tx
      ?to_
      ?data
      ~value
      ~gas_limit
      ~base_fee_per_gas:infos.base_fee_per_gas
      ~chain_id:(Z.to_int chain_id)
      ~nonce
      ~signer:from.signer
      ()
  in
  Ethereum_types.Hex (txn :> string)

let transfer_exn ?nonce ?to_ ?data ~value ~gas_limit ~infos ~from () =
  let open Lwt_syntax in
  let* res = transfer ?nonce ?to_ ?data ~value ~gas_limit ~infos ~from () in
  match res with
  | Ok res -> return res
  | Error err ->
      Stdlib.failwith
        (Format.asprintf "Could not craft the transfer: %a" pp_print_trace err)

let transfer_with_obj_exn ?nonce ?to_ ?data ~value ~gas_limit ~infos ~from () =
  let open Lwt_result_syntax in
  let*! raw_tx =
    transfer_exn ?nonce ~infos ~from ?to_ ~gas_limit ~value ?data ()
  in
  let*? transaction_object =
    Transaction_object.decode @@ Ethereum_types.hex_to_bytes raw_tx
  in
  return (raw_tx, transaction_object)
