(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let ( let**? ) v f =
  let open Lwt_result_syntax in
  match v with Ok v -> f v | Error err -> return (Error err)

let is_tx_valid (module Backend_rpc : Services_backend_sig.S) tx_raw =
  let open Lwt_result_syntax in
  let hash = Ethereum_types.hash_raw_tx tx_raw in
  match String.get_uint8 tx_raw 0 with
  | 1 -> Backend_rpc.is_tx_valid tx_raw
  | 2 -> Backend_rpc.is_tx_valid tx_raw
  | _ ->
      let tx_raw = Bytes.unsafe_of_string tx_raw in
      let**? transaction = Transaction.decode_legacy tx_raw in
      let**? transaction_object =
        Transaction.to_transaction_object ~hash transaction
      in
      return (Ok (Either.Left transaction_object))
