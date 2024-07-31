(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let ( let** ) v f =
  let open Lwt_result_syntax in
  let* r = v in
  match r with Ok v -> f v | Error err -> return (Error err)

let ( let**? ) v f =
  let open Lwt_result_syntax in
  match v with Ok v -> f v | Error err -> return (Error err)

let validate_chain_id (module Backend_rpc : Services_backend_sig.S)
    (transaction : Transaction.transaction) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  match transaction.chain_id with
  | None -> return (Ok ())
  | Some transaction_chain_id ->
      let* (Qty chain_id) = Backend_rpc.chain_id () in
      if Z.equal transaction_chain_id chain_id then return (Ok ())
      else return (Error "Invalid chain id")

let validate backend_rpc transaction =
  let open Lwt_result_syntax in
  let** () = validate_chain_id backend_rpc transaction in
  return (Ok ())

let is_tx_valid ((module Backend_rpc : Services_backend_sig.S) as backend_rpc)
    tx_raw =
  let open Lwt_result_syntax in
  let hash = hash_raw_tx tx_raw in
  match String.get_uint8 tx_raw 0 with
  | 1 -> Backend_rpc.is_tx_valid tx_raw
  | 2 -> Backend_rpc.is_tx_valid tx_raw
  | _ ->
      let tx_raw = Bytes.unsafe_of_string tx_raw in
      let**? transaction = Transaction.decode_legacy tx_raw in
      let**? transaction_object =
        Transaction.to_transaction_object ~hash transaction
      in
      let** () = validate backend_rpc transaction in
      return (Ok (Either.Left transaction_object))
