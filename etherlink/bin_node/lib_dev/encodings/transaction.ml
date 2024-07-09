(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type t = {
  from : address;
  to_ : address;
  gas : quantity;
  gasPrice : quantity;
  value : quantity option;
  data : hash;
  nonce : quantity option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {from; to_; gas; gasPrice; value; data; nonce} ->
      (from, to_, gas, gasPrice, value, data, nonce))
    (fun (from, to_, gas, gasPrice, value, data, nonce) ->
      {from; to_; gas; gasPrice; value; data; nonce})
    (obj7
       (req "from" address_encoding)
       (req "to" address_encoding)
       (req "gas" quantity_encoding)
       (req "gasPrice" quantity_encoding)
       (opt "value" quantity_encoding)
       (req "data" hash_encoding)
       (opt "nonce" quantity_encoding))

let check_prefix b =
  if String.starts_with ~prefix:"\x01" b then
    `EIP2930 (String.sub b 1 (String.length b - 1) |> Bytes.of_string)
  else if String.starts_with ~prefix:"\x02" b then
    `EIP1559 (String.sub b 1 (String.length b - 1) |> Bytes.of_string)
  else `Legacy (Bytes.of_string b)

let hash_raw_tx raw_tx =
  let hash =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest (String.to_bytes raw_tx)
  in
  Hash (Hex Hex.(of_bytes hash |> show))

let nonce_of_rlp_raw_tx bytes =
  let open Result_syntax in
  match check_prefix bytes with
  | `EIP2930 b -> (
      (* EIP-2930: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2930.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _])
      | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _; _; _]) ->
          let+ nonce = Rlp.decode_z nonce in
          nonce
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements"))
  | `EIP1559 b -> (
      (* EIP-1559: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1559.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _])
      | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _; _; _; _]) ->
          let+ nonce = Rlp.decode_z nonce in
          nonce
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements"))
  | `Legacy b -> (
      (* Legacy: https://eips.ethereum.org/EIPS/eip-2972 *)
      match Rlp.decode b with
      | Ok (Rlp.List [Value nonce; _; _; _; _; _; _; _; _]) ->
          let+ nonce = Rlp.decode_z nonce in
          nonce
      | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 elements"))

let data_of_rlp_raw_tx bytes =
  let open Result_syntax in
  match check_prefix bytes with
  | `EIP2930 b -> (
      (* EIP-2930: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2930.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; _; _; _; Value data; _])
      | Ok (Rlp.List [_; _; _; _; _; _; Value data; _; _; _; _]) ->
          return data
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements"))
  | `EIP1559 b -> (
      (* EIP-1559: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1559.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; _; _; _; _; Value data; _])
      | Ok (Rlp.List [_; _; _; _; _; _; _; Value data; _; _; _; _]) ->
          return data
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements"))
  | `Legacy b -> (
      (* Legacy: https://eips.ethereum.org/EIPS/eip-2972 *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; _; _; Value data; _; _; _]) -> return data
      | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 elements"))

let gas_limit_of_rlp_raw_tx bytes =
  let open Result_syntax in
  match check_prefix bytes with
  | `EIP2930 b -> (
      (* EIP-2930: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2930.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; Value gas_limit; _; _; _; _])
      | Ok (Rlp.List [_; _; _; Value gas_limit; _; _; _; _; _; _; _]) ->
          let+ gas_limit = Rlp.decode_z gas_limit in
          gas_limit
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements"))
  | `EIP1559 b -> (
      (* EIP-1559: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1559.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; _; Value gas_limit; _; _; _; _])
      | Ok (Rlp.List [_; _; _; _; Value gas_limit; _; _; _; _; _; _; _]) ->
          let+ gas_limit = Rlp.decode_z gas_limit in
          gas_limit
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements"))
  | `Legacy b -> (
      (* Legacy: https://eips.ethereum.org/EIPS/eip-2972 *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; Value gas_limit; _; _; _; _; _; _]) ->
          let+ gas_limit = Rlp.decode_z gas_limit in
          gas_limit
      | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 elements"))

let gas_price_of_rlp_raw_tx bytes =
  let open Result_syntax in
  match check_prefix bytes with
  | `EIP2930 b -> (
      (* EIP-2930: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2930.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; Value gas_price; _; _; _; _; _])
      | Ok (Rlp.List [_; _; Value gas_price; _; _; _; _; _; _; _; _]) ->
          let* gas_price = Rlp.decode_z gas_price in
          return gas_price
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements"))
  | `EIP1559 b -> (
      (* EIP-1559: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1559.md *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; _; _; Value max_fee_per_gas; _; _; _; _; _])
      | Ok (Rlp.List [_; _; _; Value max_fee_per_gas; _; _; _; _; _; _; _; _])
        ->
          (* Normally, max_priority_fee_per_gas would also be a fee paid per gas in
             addition to base fee per gas.
             This would incentivise miners to include the transaction.
             More details see here https://eips.ethereum.org/EIPS/eip-1559#abstract

             We choose to ignore this, however, as we actually do not implement EIP-1559
             mechanism exactly. The sequencer is compensated for L1 inclusion cost via
             the data availability fee. *)
          let* max_fee_per_gas = Rlp.decode_z max_fee_per_gas in
          return max_fee_per_gas
      | _ ->
          tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements"))
  | `Legacy b -> (
      (* Legacy: https://eips.ethereum.org/EIPS/eip-2972 *)
      match Rlp.decode b with
      | Ok (Rlp.List [_; Value gas_price; _; _; _; _; _; _; _]) ->
          let* gas_price = Rlp.decode_z gas_price in
          return gas_price
      | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9"))
