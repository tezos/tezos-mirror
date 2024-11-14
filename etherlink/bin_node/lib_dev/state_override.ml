(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let durable_balance v = v |> Ethereum_types.encode_u256_le |> Bytes.to_string

let durable_nonce v = v |> Ethereum_types.encode_u64_le |> Bytes.to_string

let durable_code = Ethereum_types.hex_to_bytes

type error += Invalid_storage_value of string | State_and_state_diff

let () =
  register_error_kind
    `Permanent
    ~id:"durable_storage_invalid_value"
    ~title:"Invalid storage value"
    ~description:"Tried to set an invalid value in an account storage."
    ~pp:(fun ppf path ->
      Format.fprintf ppf "%s is not a valid storage value" path)
    Data_encoding.(obj1 (req "durable_storage_invalid_value" string))
    (function Invalid_storage_value path -> Some path | _ -> None)
    (fun path -> Invalid_storage_value path) ;
  register_error_kind
    `Permanent
    ~id:"durable_storage_state_and_state_diff"
    ~title:"Invalid state override"
    ~description:
      "Invalid state override, state and state diff are mutually exclusive."
    Data_encoding.empty
    (function State_and_state_diff -> Some () | _ -> None)
    (fun () -> State_and_state_diff)

let update_storage address state_diff state =
  let open Ethereum_types in
  let open Lwt_result_syntax in
  let update key value state =
    let (Hex key) = key in
    let (Hex value) = value in
    if String.length value = 64 then
      let*? key = Durable_storage_path.Accounts.storage_e address key in
      let*! state =
        Evm_state.modify ~key ~value:(hex_to_bytes (Hex value)) state
      in
      return state
    else tzfail (Invalid_storage_value value)
  in
  StorageMap.fold_es update state_diff state

let replace_storage address state_override state =
  let open Lwt_result_syntax in
  match state_override with
  | None -> return state
  | Some state_override ->
      let*? key = Durable_storage_path.Accounts.storage_dir_e address in
      let*! state =
        Evm_state.delete ~kind:Tezos_scoru_wasm.Durable.Directory state key
      in
      update_storage address state_override state

let is_invalid state_override =
  let open Ethereum_types in
  not
    (Option.is_none state_override.state
    || StorageMap.is_empty state_override.state_diff)

let update_account address state_override evm_state =
  let open Durable_storage_path in
  let open Lwt_result_syntax in
  let update v_opt key encode state =
    match v_opt with
    | None -> return state
    | Some v ->
        let*! state = Evm_state.modify ~key ~value:(encode v) state in
        return state
  in
  if is_invalid state_override then tzfail State_and_state_diff
  else
    let* evm_state =
      update
        state_override.balance
        (Accounts.balance address)
        durable_balance
        evm_state
    in
    let* evm_state =
      update
        state_override.nonce
        (Accounts.nonce address)
        durable_nonce
        evm_state
    in
    let* evm_state =
      update state_override.code (Accounts.code address) durable_code evm_state
    in
    let* evm_state =
      update_storage address state_override.state_diff evm_state
    in
    let* evm_state = replace_storage address state_override.state evm_state in
    return evm_state

let update_accounts state_override state =
  match state_override with
  | None -> Lwt_result_syntax.return state
  | Some so -> Ethereum_types.AddressMap.fold_es update_account so state
