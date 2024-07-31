(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type transaction_type = Legacy | Eip2930 | Eip1559

type transaction = {
  transaction_type : transaction_type;
  chain_id : Z.t option;
  nonce : Z.t;
  max_priority_fee_per_gas : Z.t;
  max_fee_per_gas : Z.t;
  gas_limit : Z.t;
  to_ : bytes option;
  value : Z.t;
  data : bytes;
  access_list : unit list;
  v : Z.t;
  r : bytes;
  s : bytes;
}

let decode_legacy : bytes -> (transaction, string) result =
 fun bytes ->
  let open Result_syntax in
  let open Rlp in
  match decode bytes with
  | Ok
      (List
        [
          Value nonce;
          Value gas_price;
          Value gas_limit;
          Value to_;
          Value value;
          Value data;
          Value v;
          Value r;
          Value s;
        ]) ->
      let (Qty nonce) = decode_number_be nonce in
      let (Qty gas_limit) = decode_number_be gas_limit in
      let (Qty gas_price) = decode_number_be gas_price in
      let to_ = if to_ = Bytes.empty then None else Some to_ in
      let (Qty value) = decode_number_be value in
      let (Qty v) = decode_number_be v in
      let* chain_id =
        let open Z in
        if v > of_int 36 then return (Some (div (v - of_int 35) (of_int 2)))
        else if v = of_int 27 || v = of_int 28 then return None
        else fail "Chain ID cannot be decoded"
      in
      return
        {
          transaction_type = Legacy;
          chain_id;
          nonce;
          max_priority_fee_per_gas = gas_price;
          max_fee_per_gas = gas_price;
          gas_limit;
          to_;
          value;
          data;
          access_list = [];
          v;
          r;
          s;
        }
  | _ -> fail "Legacy transaction is not 9 rlp items"

let encode_legacy_transaction : transaction -> bytes = function
  | {
      transaction_type = Legacy;
      chain_id;
      nonce;
      max_priority_fee_per_gas = _;
      max_fee_per_gas;
      gas_limit;
      to_;
      value;
      data;
      access_list = [];
      v = _;
      r = _;
      s = _;
    } ->
      let open Rlp in
      let rlp =
        let nonce = encode_z nonce in
        let max_fee_per_gas = encode_z max_fee_per_gas in
        let gas_limit = encode_z gas_limit in
        let to_ = Option.value ~default:Bytes.empty to_ in
        let value = encode_z value in
        let signature =
          match chain_id with
          | Some chain_id ->
              (* If the chain id is present we are post EIP155, the chain id
                 is the `v` field of the `v * r * s` where `r` and `s` are
                 empty bytes. *)
              let v = encode_z chain_id in
              [Value v; Value Bytes.empty; Value Bytes.empty]
          | None ->
              (* If we are pre EIP155, there's simply no signature. *)
              []
        in
        List
          ([
             Value nonce;
             Value max_fee_per_gas;
             Value gas_limit;
             Value to_;
             Value value;
             Value data;
           ]
          @ signature)
      in
      encode rlp
  | _ -> invalid_arg "Transaction is not legacy"

(** [message transaction] returns the "message" of the transaction, the
    binary blob that is signed and then to be used in signature recovery. *)
let message : transaction -> bytes =
 fun transaction ->
  let encoded_transaction =
    match transaction.transaction_type with
    | Legacy -> encode_legacy_transaction transaction
    | Eip2930 -> invalid_arg "Eip2930 is not yet supported"
    | Eip1559 -> invalid_arg "Eip1559 is not yet supported"
  in
  Tezos_crypto.Hacl.Hash.Keccak_256.digest encoded_transaction

let legacy_recovery_id ~chain_id ~v =
  let v = Z.to_int v in
  match chain_id with
  | None -> v - 27
  | Some chain_id ->
      let chain_id = Z.to_int chain_id in
      v - ((chain_id * 2) + 35)

let recovery_id : transaction -> (bytes, string) result =
 fun transaction ->
  let ri =
    match transaction.transaction_type with
    | Legacy ->
        legacy_recovery_id ~chain_id:transaction.chain_id ~v:transaction.v
    | Eip2930 -> invalid_arg "Eip2930 is not yet supported"
    | Eip1559 -> invalid_arg "Eip1559 is not yet supported"
  in
  if ri == 0 || ri == 1 then (
    let buffer = Bytes.create 1 in
    Bytes.set_uint8 buffer 0 ri ;
    Ok buffer)
  else Error "Invalid recovery id"

let caller transaction =
  let open Result_syntax in
  let message = message transaction in
  let* recovery_id = recovery_id transaction in
  let sig_ =
    (* TODO: we are missing the s.is_high() check. *)
    Bytes.concat Bytes.empty [transaction.r; transaction.s; recovery_id]
  in
  let+ caller = Tezos_crypto.Signature.Secp256k1.recover sig_ message in
  Address (Hex (Hex.of_bytes caller |> Hex.show))

let to_transaction_object :
    hash:hash ->
    transaction ->
    (Ethereum_types.transaction_object, string) result =
 fun ~hash
     ({
        transaction_type = _;
        chain_id = _;
        nonce;
        max_priority_fee_per_gas = _;
        max_fee_per_gas;
        gas_limit;
        to_;
        value;
        data;
        access_list = _;
        v;
        r;
        s;
      } as transaction) ->
  let open Result_syntax in
  let* from = caller transaction in
  let to_ = Option.map decode_address to_ in

  return
    {
      blockHash = None;
      blockNumber = None;
      from;
      gas = Qty gas_limit;
      gasPrice = Qty max_fee_per_gas;
      hash;
      input = decode_hash data;
      nonce = Qty nonce;
      to_;
      transactionIndex = None;
      value = Qty value;
      v = Qty v;
      r = decode_hash r;
      s = decode_hash s;
    }
