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
