(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types
open L2_types

type transaction_type = Legacy | Eip2930 | Eip1559

type access_list_item = bytes * bytes list

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
  access_list : access_list_item list;
  v : Z.t;
  r : bytes;
  s : bytes;
}

let decode_access_list_item (item : Rlp.item) :
    (access_list_item, string) result =
  let open Rlp in
  let open Result_syntax in
  match item with
  | List [Value address; List storage_slots] ->
      let* storage_slots =
        List.fold_left
          (fun acc -> function
            | Value slot ->
                let* acc in
                return (slot :: acc)
            | _ -> fail "Storage slot should be a Value RLP item")
          return_nil
          storage_slots
      in
      return (address, List.rev storage_slots)
  | _ -> fail "An access list item should be a list of two items"

let decode_access_list_rlp access_list =
  let open Result_syntax in
  let* rev_list =
    List.fold_left
      (fun acc item ->
        let* acc in
        let* item = decode_access_list_item item in
        return (item :: acc))
      return_nil
      access_list
  in
  return (List.rev rev_list)

let encode_access_list_item ((address, storage_slots) : access_list_item) :
    Rlp.item =
  let open Rlp in
  let storage_slots = List.map (fun slot -> Value slot) storage_slots in
  List [Value address; List storage_slots]

let encode_access_list access_list =
  let open Rlp in
  let items = List.map encode_access_list_item access_list in
  List items

let decode_transaction ?chain_id ~tx_type ~nonce ~max_priority_fee_per_gas
    ~max_fee_per_gas ~gas_limit ~to_ ~value ~data ?(access_list = []) (v, r, s)
    =
  let open Result_syntax in
  let (Qty nonce) = decode_number_be nonce in
  let (Qty gas_limit) = decode_number_be gas_limit in
  let (Qty max_priority_fee_per_gas) =
    decode_number_be max_priority_fee_per_gas
  in
  let (Qty max_fee_per_gas) = decode_number_be max_fee_per_gas in
  let to_ = if to_ = Bytes.empty then None else Some to_ in
  let (Qty value) = decode_number_be value in
  let (Qty v) = decode_number_be v in
  let* access_list = decode_access_list_rlp access_list in
  let* chain_id =
    match chain_id with
    | None ->
        let open Z in
        if v > of_int 36 then return (Some (div (v - of_int 35) (of_int 2)))
        else if v = of_int 27 || v = of_int 28 then return None
        else fail "Chain ID cannot be decoded"
    | Some chain_id ->
        let (Chain_id chain_id) = Chain_id.decode_be chain_id in
        return (Some chain_id)
  in
  return
    {
      transaction_type = tx_type;
      chain_id;
      nonce;
      max_priority_fee_per_gas;
      max_fee_per_gas;
      gas_limit;
      to_;
      value;
      data;
      access_list;
      v;
      r;
      s;
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
      decode_transaction
        ~tx_type:Legacy
        ~nonce
        ~max_priority_fee_per_gas:gas_price
        ~max_fee_per_gas:gas_price
        ~gas_limit
        ~to_
        ~value
        ~data
        (v, r, s)
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

let decode_eip1559 : bytes -> (transaction, string) result =
 fun bytes ->
  let open Result_syntax in
  let open Rlp in
  match decode bytes with
  | Ok
      (List
        [
          Value chain_id;
          Value nonce;
          Value max_priority_fee_per_gas;
          Value max_fee_per_gas;
          Value gas_limit;
          Value to_;
          Value value;
          Value data;
          List access_list;
          Value v;
          Value r;
          Value s;
        ]) ->
      decode_transaction
        ~tx_type:Eip1559
        ~chain_id
        ~nonce
        ~max_priority_fee_per_gas
        ~max_fee_per_gas
        ~gas_limit
        ~to_
        ~value
        ~data
        ~access_list
        (v, r, s)
  | _ -> fail "Eip1559 transaction is not 12 rlp items"

let encode_eip1559_transaction : transaction -> bytes = function
  | {
      transaction_type = Eip1559;
      chain_id = Some chain_id;
      nonce;
      max_priority_fee_per_gas;
      max_fee_per_gas;
      gas_limit;
      to_;
      value;
      data;
      access_list;
      v = _;
      r = _;
      s = _;
    } ->
      let open Rlp in
      let rlp =
        let chain_id = encode_z chain_id in
        let nonce = encode_z nonce in
        let max_priority_fee_per_gas = encode_z max_priority_fee_per_gas in
        let max_fee_per_gas = encode_z max_fee_per_gas in
        let gas_limit = encode_z gas_limit in
        let to_ = Option.value ~default:Bytes.empty to_ in
        let value = encode_z value in
        let access_list = encode_access_list access_list in
        List
          [
            Value chain_id;
            Value nonce;
            Value max_priority_fee_per_gas;
            Value max_fee_per_gas;
            Value gas_limit;
            Value to_;
            Value value;
            Value data;
            access_list;
          ]
      in
      let prefix = Bytes.make 1 (Char.chr 2) in
      Bytes.cat prefix (encode rlp)
  | _ -> invalid_arg "Transaction is not eip 1559"

let decode_eip2930 : bytes -> (transaction, string) result =
 fun bytes ->
  let open Result_syntax in
  let open Rlp in
  match decode bytes with
  | Ok
      (List
        [
          Value chain_id;
          Value nonce;
          Value max_fee_per_gas;
          Value gas_limit;
          Value to_;
          Value value;
          Value data;
          List access_list;
          Value v;
          Value r;
          Value s;
        ]) ->
      decode_transaction
        ~tx_type:Eip2930
        ~chain_id
        ~nonce
        ~max_priority_fee_per_gas:max_fee_per_gas
        ~max_fee_per_gas
        ~gas_limit
        ~to_
        ~value
        ~data
        ~access_list
        (v, r, s)
  | _ -> fail "Eip2930 transaction is not 11 rlp items"

let encode_eip2930_transaction : transaction -> bytes = function
  | {
      transaction_type = Eip2930;
      chain_id = Some chain_id;
      nonce;
      max_priority_fee_per_gas = _;
      max_fee_per_gas;
      gas_limit;
      to_;
      value;
      data;
      access_list;
      v = _;
      r = _;
      s = _;
    } ->
      let open Rlp in
      let rlp =
        let chain_id = encode_z chain_id in
        let nonce = encode_z nonce in
        let max_fee_per_gas = encode_z max_fee_per_gas in
        let gas_limit = encode_z gas_limit in
        let to_ = Option.value ~default:Bytes.empty to_ in
        let value = encode_z value in
        let access_list = encode_access_list access_list in
        List
          [
            Value chain_id;
            Value nonce;
            Value max_fee_per_gas;
            Value gas_limit;
            Value to_;
            Value value;
            Value data;
            access_list;
          ]
      in
      let prefix = Bytes.make 1 (Char.chr 1) in
      Bytes.cat prefix (encode rlp)
  | _ -> invalid_arg "Transaction is not eip 2930"

let decode tx_raw =
  let decode, tx_raw =
    match String.get_uint8 tx_raw 0 with
    | 1 ->
        let tx_raw = String.sub tx_raw 1 (String.length tx_raw - 1) in
        (decode_eip2930, tx_raw)
    | 2 ->
        let tx_raw = String.sub tx_raw 1 (String.length tx_raw - 1) in
        (decode_eip1559, tx_raw)
    | _ -> (decode_legacy, tx_raw)
  in
  let tx_raw = Bytes.unsafe_of_string tx_raw in
  decode tx_raw

(** [message transaction] returns the "message" of the transaction, the
    binary blob that is signed and then to be used in signature recovery. *)
let message : transaction -> bytes =
 fun transaction ->
  let encoded_transaction =
    match transaction.transaction_type with
    | Legacy -> encode_legacy_transaction transaction
    | Eip2930 -> encode_eip2930_transaction transaction
    | Eip1559 -> encode_eip1559_transaction transaction
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
    | Eip2930 | Eip1559 -> Z.to_int transaction.v
  in
  if ri == 0 || ri == 1 then (
    let buffer = Bytes.create 1 in
    Bytes.set_uint8 buffer 0 ri ;
    Ok buffer)
  else Error "Invalid recovery id"

let secp256k1n_2 =
  Z.of_string
    "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0"

let is_high s =
  let (Qty s) = decode_number_be s in
  if s > secp256k1n_2 then Error "High s value" else Ok ()

let uncompressed_signature ~r ~s recovery_id =
  let open Result_syntax in
  let uncompressed_32_bytes v =
    let len = Bytes.length v in
    if len < 32 then Bytes.cat (Bytes.make (32 - len) '\000') v else v
  in
  let r = uncompressed_32_bytes r in
  let s = uncompressed_32_bytes s in
  let+ () = is_high s in
  Bytes.concat Bytes.empty [r; s; recovery_id]

let caller ({r; s; _} as transaction) =
  let open Result_syntax in
  let message = message transaction in
  let* recovery_id = recovery_id transaction in
  let* sig_ = uncompressed_signature ~r ~s recovery_id in
  let+ caller = Tezos_crypto.Signature.Secp256k1.recover sig_ message in
  Address (Hex (Hex.of_bytes caller |> Hex.show))

let to_transaction_object :
    hash:hash ->
    transaction ->
    (Ethereum_types.legacy_transaction_object, string) result =
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
      input = decode_hex data;
      nonce = Qty nonce;
      to_;
      transactionIndex = None;
      value = Qty value;
      v = Qty v;
      r = decode_hex r;
      s = decode_hex s;
    }
