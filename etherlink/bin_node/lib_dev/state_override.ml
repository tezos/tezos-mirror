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

let update_storage ~storage_version address state_diff state =
  let open Ethereum_types in
  let open Lwt_result_syntax in
  let*? fixed_addr =
    Durable_storage_path.Accounts.fixed_address ~storage_version address
  in
  let update key value state =
    let (Hex key_hex) = key in
    let (Hex value_hex) = value in
    if String.length value_hex = 64 then
      let*? fixed_idx = Durable_storage_path.Accounts.fixed_index key_hex in
      Durable_storage.write
        (Evm_account_storage (fixed_addr, fixed_idx))
        (Hex value_hex)
        state
    else tzfail (Invalid_storage_value value_hex)
  in
  StorageMap.fold_es update state_diff state

let replace_storage ~storage_version address state_override state =
  let open Lwt_result_syntax in
  match state_override with
  | None -> return state
  | Some state_override ->
      let*? fixed_addr =
        Durable_storage_path.Accounts.fixed_address ~storage_version address
      in
      let* state =
        Durable_storage.delete_dir (Evm_account_storage_dir fixed_addr) state
      in
      update_storage ~storage_version address state_override state

let is_invalid state_override =
  let open Ethereum_types in
  not
    (Option.is_none state_override.state
    || StorageMap.is_empty state_override.state_diff)

let update_account address state_override evm_state =
  let open Lwt_result_syntax in
  if is_invalid state_override then tzfail State_and_state_diff
  else
    let* storage_version = Durable_storage.storage_version evm_state in
    let* info = Durable_storage.read_opt (Evm_account_info address) evm_state in
    let* evm_state =
      match info with
      | None ->
          let update v_opt path state =
            match v_opt with
            | None -> return state
            | Some v -> Durable_storage.write path v state
          in
          let* evm_state =
            update
              state_override.nonce
              (Evm_legacy_account_nonce address)
              evm_state
          in
          let* evm_state =
            update
              state_override.balance
              (Evm_legacy_account_balance address)
              evm_state
          in
          let* evm_state =
            update
              state_override.code
              (Evm_legacy_account_code address)
              evm_state
          in
          return evm_state
      | Some old_info -> (
          let new_info =
            Durable_storage.EVM_account_info.
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
                (* Round-trip the classification tag: overriding state
                   must not clobber it nor add a field an older kernel
                   would reject. *)
                origin = old_info.origin;
              }
          in
          let* evm_state =
            Durable_storage.write (Evm_account_info address) new_info evm_state
          in
          match state_override.code with
          | None -> return evm_state
          | Some code ->
              Durable_storage.write
                (Evm_code_by_hash new_info.code_hash)
                code
                evm_state)
    in
    let* evm_state =
      update_storage
        ~storage_version
        address
        state_override.state_diff
        evm_state
    in
    let* evm_state =
      replace_storage ~storage_version address state_override.state evm_state
    in
    return evm_state

let update_accounts state_override state =
  match state_override with
  | None -> Lwt_result_syntax.return state
  | Some so -> Ethereum_types.AddressMap.fold_es update_account so state
