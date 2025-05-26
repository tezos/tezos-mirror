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

let validate_gas_limit ~maximum_gas_limit:(Qty maximum_gas_limit)
    ~da_fee_per_byte ~base_fee_per_gas:(Qty gas_price)
    (transaction : Transaction.transaction) :
    (unit, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Constants defined in the kernel: *)
  let gas_limit = transaction.gas_limit in
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
    Backend_rpc.Etherlink.code caller Block_parameter.(Block_parameter Latest)
  in
  if code = "" then return (Ok ())
  else return (Error "Sender is a contract which is not possible")

let validate_max_fee_per_gas ~base_fee_per_gas:(Qty base_fee_per_gas)
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
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
  else return (Ok total_cost)

let tx_data_size_limit_reached ~max_number_of_chunks ~tx_data =
  let max_number_of_chunks =
    Option.value
      max_number_of_chunks
      ~default:Sequencer_blueprint.maximum_chunks_per_l1_level
  in
  Bytes.length tx_data
  > Sequencer_blueprint.maximum_usable_space_in_blueprint
      (* Minus one so that the "rest" of the raw transaction can
         be contained within one of the chunks. *)
      (max_number_of_chunks - 1)

let validate_tx_data_size ~max_number_of_chunks
    (transaction : Transaction.transaction) =
  let open Lwt_result_syntax in
  let tx_data = transaction.data in
  if tx_data_size_limit_reached ~max_number_of_chunks ~tx_data then
    return @@ Error "Transaction data exceeded the allowed size."
  else return (Ok ())

let validate_stateless ~next_nonce ~max_number_of_chunks backend_rpc transaction
    ~caller =
  let open Lwt_result_syntax in
  let** () = validate_chain_id backend_rpc transaction in
  let** () = validate_nonce ~next_nonce transaction in
  let** () = validate_sender_not_a_contract backend_rpc caller in
  let** () = validate_tx_data_size ~max_number_of_chunks transaction in
  return (Ok ())

let validate_balance_and_gas ~base_fee_per_gas ~maximum_gas_limit
    ~da_fee_per_byte ~transaction ~from_balance:(Qty from_balance) =
  let open Lwt_result_syntax in
  let** () = validate_max_fee_per_gas ~base_fee_per_gas transaction in
  let** () =
    validate_gas_limit
      ~maximum_gas_limit
      ~da_fee_per_byte
      ~base_fee_per_gas
      transaction
  in
  let** total_cost =
    validate_balance_is_enough transaction ~balance:from_balance
  in
  return (Ok total_cost)

let validate_with_state_from_backend
    (module Backend_rpc : Services_backend_sig.S) transaction ~caller =
  let open Lwt_result_syntax in
  let* from_balance =
    Backend_rpc.Etherlink.balance
      caller
      Block_parameter.(Block_parameter Latest)
  in
  let* base_fee_per_gas = Backend_rpc.Etherlink.base_fee_per_gas () in
  let* state = Backend_rpc.Reader.get_state () in
  let* maximum_gas_limit =
    Etherlink_durable_storage.maximum_gas_per_transaction
      (Backend_rpc.Reader.read state)
  in
  let* da_fee_per_byte =
    Etherlink_durable_storage.da_fee_per_byte (Backend_rpc.Reader.read state)
  in
  let** _total_cost =
    validate_balance_and_gas
      ~base_fee_per_gas
      ~maximum_gas_limit
      ~da_fee_per_byte
      ~transaction
      ~from_balance
  in
  return (Ok ())

type validation_mode = Stateless | With_state | Full

let valid_transaction_object ?max_number_of_chunks ~backend_rpc ~hash ~mode tx =
  let open Lwt_result_syntax in
  let**? tx_object = Transaction.to_transaction_object ~hash tx in
  let caller = tx_object.from in
  let* next_nonce =
    let (module Backend_rpc : Services_backend_sig.S) = backend_rpc in
    Backend_rpc.Etherlink.nonce caller Block_parameter.(Block_parameter Latest)
  in
  let next_nonce =
    match next_nonce with None -> Qty Z.zero | Some next_nonce -> next_nonce
  in
  let** () =
    match mode with
    | Stateless ->
        validate_stateless
          ~max_number_of_chunks
          backend_rpc
          ~next_nonce
          tx
          ~caller
    | With_state -> validate_with_state_from_backend backend_rpc tx ~caller
    | Full ->
        let** () =
          validate_stateless
            ~max_number_of_chunks
            ~next_nonce
            backend_rpc
            tx
            ~caller
        in
        let** () = validate_with_state_from_backend backend_rpc tx ~caller in
        return (Ok ())
  in

  return (Ok (next_nonce, tx_object))

let is_tx_valid ?max_number_of_chunks
    ((module Backend_rpc : Services_backend_sig.S) as backend_rpc) ~mode tx_raw
    =
  let hash = Ethereum_types.hash_raw_tx tx_raw in
  let**? tx = Transaction.decode tx_raw in
  valid_transaction_object ?max_number_of_chunks ~backend_rpc ~hash ~mode tx

type validation_config = {
  base_fee_per_gas : Ethereum_types.quantity;
  maximum_gas_limit : Ethereum_types.quantity;
  da_fee_per_byte : Ethereum_types.quantity;
  next_nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t;
  balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t;
}

type validation_state = {
  config : validation_config;
  addr_balance : Z.t String.Map.t;
  addr_nonce : Z.t String.Map.t;
}

let validate_balance_gas_nonce_with_validation_state validation_state
    ~(caller : Ethereum_types.address) (transaction : Transaction.transaction) :
    (validation_state, string) result tzresult Lwt.t =
  let open Lwt_result_syntax in
  let (Address (Hex caller_str)) = caller in
  let* next_nonce =
    let nonce = String.Map.find caller_str validation_state.addr_nonce in
    match nonce with
    | Some nonce -> return nonce
    | None -> (
        let* nonce = validation_state.config.next_nonce caller in
        match nonce with
        | Some (Qty nonce) -> return nonce
        | None -> return Z.zero)
  in
  let** () =
    let tx_nonce = transaction.nonce in
    if Z.equal tx_nonce next_nonce then return (Ok ())
    else return (Error "Transaction nonce is not the expected nonce.")
  in
  let* from_balance =
    let from_balance =
      String.Map.find caller_str validation_state.addr_balance
    in
    match from_balance with
    | Some balance -> return balance
    | None ->
        let* (Qty balance) = validation_state.config.balance caller in
        return balance
  in
  let** total_cost =
    validate_balance_and_gas
      ~base_fee_per_gas:validation_state.config.base_fee_per_gas
      ~maximum_gas_limit:validation_state.config.maximum_gas_limit
      ~da_fee_per_byte:validation_state.config.da_fee_per_byte
      ~transaction
      ~from_balance:(Qty from_balance)
  in
  let* addr_balance =
    match transaction.to_ with
    | None -> return validation_state.addr_balance
    | Some to_ ->
        let (`Hex to_) = Hex.of_bytes to_ in
        let to_balance = String.Map.find to_ validation_state.addr_balance in
        let* to_balance =
          match to_balance with
          | Some balance -> return balance
          | None ->
              let* (Qty balance) =
                validation_state.config.balance (Address (Hex to_))
              in
              return balance
        in
        let new_to_balance = Z.add transaction.value to_balance in
        let addr_balance =
          String.Map.add to_ new_to_balance validation_state.addr_balance
        in
        return addr_balance
  in
  let validation_state =
    let new_from_balance = Z.sub from_balance total_cost in
    let addr_balance =
      String.Map.add caller_str new_from_balance addr_balance
    in
    let addr_nonce =
      String.Map.add caller_str (Z.succ next_nonce) validation_state.addr_nonce
    in
    {validation_state with addr_balance; addr_nonce}
  in
  return (Ok validation_state)
