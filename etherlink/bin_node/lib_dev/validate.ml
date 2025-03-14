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

let validate_pay_for_fees (transaction : Transaction.transaction) ~balance =
  let open Lwt_result_syntax in
  let cost = Z.mul transaction.gas_limit transaction.max_fee_per_gas in
  if balance >= cost then return (Ok ())
  else return (Error "Cannot prepay transaction.")

let validate_total_cost (tx_object : legacy_transaction_object) ~balance =
  let open Lwt_result_syntax in
  let total_cost =
    let (Qty gas) = tx_object.gas in
    let (Qty gas_price) = tx_object.gasPrice in
    let (Qty value) = tx_object.value in
    Z.add (Z.mul gas gas_price) value
  in
  if total_cost > balance then return (Error "Not enough funds")
  else return (Ok ())

let validate_stateless ~next_nonce backend_rpc transaction ~caller =
  let open Lwt_result_syntax in
  let** () = validate_chain_id backend_rpc transaction in
  let** () = validate_nonce ~next_nonce transaction in
  let** () = validate_sender_not_a_contract backend_rpc caller in
  return (Ok ())

let validate_with_state (module Backend_rpc : Services_backend_sig.S)
    transaction (tx_object : legacy_transaction_object) =
  let open Lwt_result_syntax in
  let* (Qty balance) =
    Backend_rpc.balance tx_object.from Block_parameter.(Block_parameter Latest)
  in
  let backend_rpc = (module Backend_rpc : Services_backend_sig.S) in
  let** () = validate_max_fee_per_gas backend_rpc transaction in
  let** () = validate_pay_for_fees transaction ~balance in
  let** () = validate_gas_limit backend_rpc transaction in
  let** () = validate_total_cost tx_object ~balance in
  return (Ok ())

type validation_mode = Stateless | With_state | Full

let valid_transaction_object ~backend_rpc ~decode ~hash ~mode tx_raw =
  let open Lwt_result_syntax in
  let tx_raw = Bytes.unsafe_of_string tx_raw in
  let**? tx = decode tx_raw in
  let**? tx_object = Transaction.to_transaction_object ~hash tx in
  let* next_nonce =
    let (module Backend_rpc : Services_backend_sig.S) = backend_rpc in
    Backend_rpc.nonce tx_object.from Block_parameter.(Block_parameter Latest)
  in
  let next_nonce =
    match next_nonce with None -> Qty Z.zero | Some next_nonce -> next_nonce
  in
  let** () =
    match mode with
    | Stateless ->
        validate_stateless backend_rpc ~next_nonce tx ~caller:tx_object.from
    | With_state -> validate_with_state backend_rpc tx tx_object
    | Full ->
        let** () =
          validate_stateless ~next_nonce backend_rpc tx ~caller:tx_object.from
        in
        let** () = validate_with_state backend_rpc tx tx_object in
        return (Ok ())
  in

  return (Ok (next_nonce, tx_object))

let is_tx_valid ((module Backend_rpc : Services_backend_sig.S) as backend_rpc)
    ~mode tx_raw =
  let hash = Ethereum_types.hash_raw_tx tx_raw in
  match String.get_uint8 tx_raw 0 with
  | 1 ->
      let tx_raw = String.sub tx_raw 1 (String.length tx_raw - 1) in
      valid_transaction_object
        ~backend_rpc
        ~decode:Transaction.decode_eip2930
        ~hash
        ~mode
        tx_raw
  | 2 ->
      let tx_raw = String.sub tx_raw 1 (String.length tx_raw - 1) in
      valid_transaction_object
        ~backend_rpc
        ~decode:Transaction.decode_eip1559
        ~hash
        ~mode
        tx_raw
  | _ ->
      valid_transaction_object
        ~backend_rpc
        ~decode:Transaction.decode_legacy
        ~hash
        ~mode
        tx_raw
