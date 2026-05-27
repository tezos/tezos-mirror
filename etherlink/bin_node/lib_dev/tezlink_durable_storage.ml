(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

type implicit_account_data_model =
  | Rlp
      (** One entry per account, the value is RLP-encoded and contains
          three fields: balance, nonce (aka. counter), and optional
          pkh.

          This data model is used to reduce the number of IOs in the
          kernel at the cost of Tezos compatibility. *)
  | Path
      (** One directory per account, the balance, counter, and manager
          are stored at distinct subpaths. *)

module Path = struct
  (** [to_path encoding value] uses [encoding] to encode [value] in
      hexadecimal *)
  let to_path encoding value =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding value in
    let (`Hex s) = Hex.of_bytes raw_key in
    s

  let accounts_index = Durable_storage_path.michelson_contracts_index

  let big_map = "/tez/tez_accounts/big_map"

  let account contract =
    accounts_index ^ "/" ^ to_path Contract.encoding contract

  (* [balance]/[manager]/[counter] are kept as path helpers because
     [sequencer.ml] uses them as raw keys to seed bootstrap accounts via
     [Evm_context.patch_state] (which takes a raw [key]). They are no
     longer used as read paths inside this module; reads go through the
     typed constructors [Durable_storage.Tezlink_balance / _manager /
     _counter]. *)
  let balance contract = account contract ^ "/balance"

  let manager contract = account contract ^ "/manager"

  let counter contract = account contract ^ "/counter"
end

let contract_of_path = Contract.of_hex

let balance state ~data_model (c : Contract.t) =
  let open Lwt_result_syntax in
  match data_model with
  | Rlp -> (
      match c with
      | Originated _ ->
          Durable_storage.read_or_default
            ~default:Tez.zero
            (Tezlink_balance c)
            state
      | Implicit pkh -> (
          let* info_opt =
            Durable_storage.read_opt (Tezos_account_info pkh) state
          in
          match info_opt with
          | None -> return Tez.zero
          | Some info -> return info.balance))
  | Path ->
      Durable_storage.read_or_default
        ~default:Tezos_types.Tez.zero
        (Tezlink_balance c)
        state

let balance_z state ~data_model c =
  let open Lwt_result_syntax in
  let* b = balance state ~data_model c in
  return @@ Tezos_types.Tez.to_mutez_z b

let manager state ~data_model (c : Contract.t) =
  let open Lwt_result_syntax in
  match data_model with
  | Rlp -> (
      match c with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* info_opt =
            Durable_storage.read_opt (Tezos_account_info pkh) state
          in
          match info_opt with
          | None -> return_none
          | Some info -> (
              match info.public_key with
              | None -> return_none
              | Some public_key -> return_some (Manager.Public_key public_key)))
      )
  | Path -> Durable_storage.read_opt (Tezlink_manager c) state

let counter state ~data_model (c : Contract.t) =
  let open Lwt_result_syntax in
  match data_model with
  | Rlp -> (
      match c with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* info_opt =
            Durable_storage.read_opt (Tezos_account_info pkh) state
          in
          match info_opt with
          | None -> return_none
          | Some info -> return_some (Z.of_int64 info.nonce)))
  | Path -> Durable_storage.read_opt (Tezlink_counter c) state

let big_map_get state id key_hash =
  Durable_storage.read_opt (Tezos_big_map_value (id, key_hash)) state

let big_map_key_type state id =
  Durable_storage.read_opt (Tezos_big_map_key_type id) state

let big_map_value_type state id =
  Durable_storage.read_opt (Tezos_big_map_value_type id) state

let nth_block state n =
  let open Lwt_result_syntax in
  let level = n in
  let* block_hash_opt =
    Durable_storage.read_opt (Tezlink_block_hash_by_number (Nth level)) state
  in
  match block_hash_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block_hash -> (
      let* block_opt =
        Durable_storage.read_opt (Tezlink_block_by_hash block_hash) state
      in
      match block_opt with
      | None ->
          raise
          @@ Durable_storage.Invalid_block_structure "Couldn't decode bytes"
      | Some block -> return block)

let nth_block_hash state n =
  let number = Durable_storage_path.Block.(Nth n) in
  Durable_storage.read_opt (Tezlink_block_hash_by_number number) state

let da_fee_per_byte_mutez state =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty da_fee_per_byte_wei) =
    Durable_storage.read Da_fee_per_byte state
  in
  let*? da_fee_per_byte = Tezos_types.Tez.(of_wei (Wei da_fee_per_byte_wei)) in
  (* DA fee expressed in wei: converting to mutez. *)
  return da_fee_per_byte
