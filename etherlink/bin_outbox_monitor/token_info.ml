(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Tokens_table = Hashtbl.Make (struct
  include Ethereum_types.Address

  let equal a b = compare a b = 0

  let hash (Ethereum_types.Address (Hex s)) = Hashtbl.hash s
end)

type t = {symbol : string; decimals : int option}

let tokens = Tokens_table.create 7

(** [keccak_function s] computes the first 4 bytes of the Keccak-256 hash of the
    string [s]. This is used to compute function selectors for Ethereum smart
    contract calls. *)
let keccak_function s =
  let b = Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string s) in
  let b = Bytes.sub b 0 4 in
  let (`Hex h) = Hex.of_bytes b in
  Ethereum_types.Hash (Hex h)

let symbol_keccak = keccak_function "symbol()"

let decimals_keccak = keccak_function "decimals()"

let get_symbol ws_client token_addr =
  let open Lwt_result_syntax in
  let*! resp =
    Websocket_client.send_jsonrpc
      ws_client
      (Call
         ( (module Rpc_encodings.Eth_call),
           ( {
               data = Some symbol_keccak;
               from = None;
               to_ = Some token_addr;
               gas = None;
               gasPrice = None;
               value = None;
             },
             Block_parameter Latest,
             Ethereum_types.AddressMap.empty ) ))
  in
  match resp with
  | Error (Websocket_client.Request_failed _ :: _) ->
      (* Contract has no "symbol" function *)
      return_none
  | Error e -> fail e
  | Ok (Hash (Hex hex)) -> return (Hex.to_string (`Hex hex))

let get_decimals ws_client token_addr =
  let open Lwt_result_syntax in
  let*! resp =
    Websocket_client.send_jsonrpc
      ws_client
      (Call
         ( (module Rpc_encodings.Eth_call),
           ( {
               data = Some decimals_keccak;
               from = None;
               to_ = Some token_addr;
               gas = None;
               gasPrice = None;
               value = None;
             },
             Block_parameter Latest,
             Ethereum_types.AddressMap.empty ) ))
  in
  match resp with
  | Error (Websocket_client.Request_failed _ :: _) ->
      (* Contract has no "decimals" function *)
      return_none
  | Error e -> fail e
  | Ok (Hash (Hex hex)) -> (
      match Hex.to_bytes (`Hex hex) with
      | None -> return_none
      | Some b ->
          let (Qty decimals_z) = Ethereum_types.decode_number_be b in
          return_some (Z.to_int decimals_z))

let get_fa_info ws_client token_addr =
  let open Lwt_result_syntax in
  match Tokens_table.find tokens token_addr with
  | Some info -> return info
  | None ->
      let* symbol = get_symbol ws_client token_addr
      and* decimals = get_decimals ws_client token_addr in
      let symbol =
        Option.value
          symbol
          ~default:(Ethereum_types.Address.to_string token_addr)
      in
      Tokens_table.replace tokens token_addr {symbol; decimals} ;
      return {symbol; decimals}

let get ws_client =
  let open Lwt_result_syntax in
  function
  | Db.Xtz -> return ({symbol = "XTZ"; decimals = Some 18}, false)
  | FA token_addr ->
      let* info = get_fa_info ws_client token_addr in
      return (info, false)
  | Fast_xtz -> return ({symbol = "XTZ"; decimals = Some 18}, true)
  | Fast_FA token_addr ->
      let* info = get_fa_info ws_client token_addr in
      return (info, true)

let get_for_display ws_client token amount =
  let open Lwt_result_syntax in
  let* {symbol; decimals}, fast = get ws_client token in
  let amount =
    let (Ethereum_types.Qty amount) = amount in
    let amount = Z.to_float amount in
    match decimals with
    | None -> amount
    | Some decimals -> amount /. (10. ** float_of_int decimals)
  in
  return (amount, symbol, fast)
