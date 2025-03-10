(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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
      let* (Chain_id chain_id) = Backend_rpc.chain_id () in
      if Z.equal transaction_chain_id chain_id then return (Ok ())
      else return (Error "Invalid chain id")

let validate_nonce ~next_nonce:(Qty next_nonce)
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  if transaction.nonce >= next_nonce then return (Ok ())
  else return (Error "Nonce too low")

let validate_gas_limit (module Backend_rpc : Services_backend_sig.S)
    (transaction : Transaction.transaction) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Constants defined in the kernel: *)
  let gas_limit = transaction.gas_limit in
  let* state = Backend_rpc.Reader.get_state () in
  let* (Qty maximum_gas_limit) =
    Durable_storage.maximum_gas_per_transaction (Backend_rpc.Reader.read state)
  in
  let* da_fee_per_byte =
    Durable_storage.da_fee_per_byte (Backend_rpc.Reader.read state)
  in
  let* (Qty gas_price) =
    Durable_storage.base_fee_per_gas (Backend_rpc.Reader.read state)
  in
  let gas_for_da_fees =
    Fees.gas_for_fees
      ~da_fee_per_byte
      ~access_list:transaction.access_list
      ~gas_price
      transaction.data
  in
  let** execution_gas_limit =
    if Z.gt gas_limit gas_for_da_fees then
      return (Ok (Z.sub gas_limit gas_for_da_fees))
    else return (Error "Invalid gas_limit for da_fees")
  in
  if Z.leq execution_gas_limit maximum_gas_limit then return (Ok ())
  else
    return
      (Error
         (Format.sprintf
            "Gas limit for execution is too high. Maximum limit is %s, \
             transaction has %s"
            (Z.to_string maximum_gas_limit)
            (Z.to_string execution_gas_limit)))

let validate_sender_not_a_contract (module Backend_rpc : Services_backend_sig.S)
    caller : (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* (Hex code) =
    Backend_rpc.code caller Block_parameter.(Block_parameter Latest)
  in
  if code = "" then return (Ok ())
  else return (Error "Sender is a contract which is not possible")

let validate_max_fee_per_gas (module Backend_rpc : Services_backend_sig.S)
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  let* (Qty base_fee_per_gas) = Backend_rpc.base_fee_per_gas () in
  if transaction.max_fee_per_gas >= base_fee_per_gas then return (Ok ())
  else return (Error "Max gas fee too low")

let validate_balance_is_enough (transaction : Transaction.transaction) ~balance
    =
  let open Lwt_result_syntax in
  let gas = transaction.gas_limit in
  let gas_price = transaction.max_fee_per_gas in
  let gas_cost = Z.mul gas gas_price in
  let total_cost =
    let value = transaction.value in
    Z.add gas_cost value
  in
  if gas_cost > balance then return (Error "Cannot prepay transaction.")
  else if total_cost > balance then return (Error "Not enough funds")
  else return (Ok ())

let validate_stateless ~next_nonce backend_rpc transaction ~caller =
  let open Lwt_result_syntax in
  let** () = validate_chain_id backend_rpc transaction in
  let** () = validate_nonce ~next_nonce transaction in
  let** () = validate_sender_not_a_contract backend_rpc caller in
  return (Ok ())

let validate_with_state (module Backend_rpc : Services_backend_sig.S)
    transaction ~caller =
  let open Lwt_result_syntax in
  let* (Qty balance) =
    Backend_rpc.balance caller Block_parameter.(Block_parameter Latest)
  in
  let backend_rpc = (module Backend_rpc : Services_backend_sig.S) in
  let** () = validate_max_fee_per_gas backend_rpc transaction in
  let** () = validate_gas_limit backend_rpc transaction in
  let** () = validate_balance_is_enough transaction ~balance in
  return (Ok ())

type validation_mode = Stateless | With_state | Full

let valid_transaction_object ~backend_rpc ~hash ~mode tx =
  let open Lwt_result_syntax in
  let**? tx_object = Transaction.to_transaction_object ~hash tx in
  let caller = tx_object.from in
  let* next_nonce =
    let (module Backend_rpc : Services_backend_sig.S) = backend_rpc in
    Backend_rpc.nonce caller Block_parameter.(Block_parameter Latest)
  in
  let next_nonce =
    match next_nonce with None -> Qty Z.zero | Some next_nonce -> next_nonce
  in
  let** () =
    match mode with
    | Stateless -> validate_stateless backend_rpc ~next_nonce tx ~caller
    | With_state -> validate_with_state backend_rpc tx ~caller
    | Full ->
        let** () = validate_stateless ~next_nonce backend_rpc tx ~caller in
        let** () = validate_with_state backend_rpc tx ~caller in
        return (Ok ())
  in

  return (Ok (next_nonce, tx_object))

let is_tx_valid ((module Backend_rpc : Services_backend_sig.S) as backend_rpc)
    ~mode tx_raw =
  let hash = Ethereum_types.hash_raw_tx tx_raw in
  let**? tx = Transaction.decode tx_raw in
  valid_transaction_object ~backend_rpc ~hash ~mode tx
