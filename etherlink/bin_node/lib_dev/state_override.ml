(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

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
  if is_invalid state_override then tzfail State_and_state_diff
  else
    let* info = Evm_state.read evm_state (Accounts.info address) in
    let* new_info =
      match info with
      | None ->
          let* old_balance_opt =
            Evm_state.read evm_state (Accounts.balance address)
          in
          let old_balance =
            Option.map
              (fun b -> Ethereum_types.decode_number_le b)
              old_balance_opt
            |> Option.value ~default:Ethereum_types.Qty.zero
          in
          let* old_nonce_opt =
            Evm_state.read evm_state (Accounts.nonce address)
          in
          let old_nonce =
            Option.map
              (fun b -> Ethereum_types.decode_number_le b)
              old_nonce_opt
            |> Option.value ~default:Ethereum_types.Qty.zero
          in
          let* old_code_hash_opt =
            Evm_state.read evm_state (Accounts.code_hash address)
          in
          let old_code_hash =
            Option.map (fun b -> Ethereum_types.decode_hash b) old_code_hash_opt
            |> Option.value
                 ~default:
                   (Bytes.create 0 |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
                  |> Ethereum_types.decode_hash)
          in
          return
            Etherlink_durable_storage.AccountInfo.
              {
                balance =
                  Option.value ~default:old_balance state_override.balance;
                nonce = Option.value ~default:old_nonce state_override.nonce;
                code_hash =
                  Option.fold
                    ~none:old_code_hash
                    ~some:(fun code ->
                      code |> Ethereum_types.hex_to_real_bytes
                      |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
                      |> Ethereum_types.decode_hash)
                    state_override.code;
              }
      | Some info ->
          let old_info =
            Etherlink_durable_storage.AccountInfo.decode_exn info
          in
          return
            Etherlink_durable_storage.AccountInfo.
              {
                balance =
                  Option.value ~default:old_info.balance state_override.balance;
                nonce =
                  Option.value ~default:old_info.nonce state_override.nonce;
                code_hash =
                  Option.fold
                    ~none:old_info.code_hash
                    ~some:(fun code ->
                      code |> Ethereum_types.hex_to_real_bytes
                      |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
                      |> Ethereum_types.decode_hash)
                    state_override.code;
              }
    in
    let*! evm_state =
      Evm_state.modify
        ~key:(Accounts.info address)
        ~value:
          (Etherlink_durable_storage.AccountInfo.encode new_info
          |> Bytes.to_string)
        evm_state
    in
    let* evm_state =
      match state_override.code with
      | None -> return evm_state
      | Some code ->
          let*! evm_state =
            Evm_state.modify
              ~key:(Code.code new_info.code_hash)
              ~value:(Ethereum_types.hex_to_bytes code)
              evm_state
          in
          return evm_state
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
