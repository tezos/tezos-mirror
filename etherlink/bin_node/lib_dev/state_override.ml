(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let durable_balance v = v |> Ethereum_types.encode_u256_le |> Bytes.to_string

let durable_nonce v = v |> Ethereum_types.encode_u64_le |> Bytes.to_string

let durable_code = Ethereum_types.hex_to_bytes

let update_storage address state_diff state =
  let open Ethereum_types in
  let open Lwt_result_syntax in
  let update key value state =
    let (Hex key) = key in
    let (Hex value) = value in
    let*! state =
      Evm_state.modify
        ~key:(Durable_storage_path.Accounts.storage address key)
        ~value
        state
    in
    return state
  in
  StorageMap.fold_es update state_diff state

let update_account address {Ethereum_types.balance; nonce; code; state_diff}
    state =
  let open Durable_storage_path in
  let open Lwt_result_syntax in
  let update v_opt key encode state =
    match v_opt with
    | None -> return state
    | Some v ->
        let*! state = Evm_state.modify ~key ~value:(encode v) state in
        return state
  in
  let* state =
    update balance (Accounts.balance address) durable_balance state
  in
  let* state = update nonce (Accounts.nonce address) durable_nonce state in
  let* state = update code (Accounts.code address) durable_code state in
  let* state = update_storage address state_diff state in
  return state

let update_accounts state_override state =
  match state_override with
  | None -> Lwt_result_syntax.return state
  | Some so -> Ethereum_types.AddressMap.fold_es update_account so state
