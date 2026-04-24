(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

(* Root for Michelson block storage (indexes/blocks, blocks/{hash}).
   Contracts and big_map live under a different keyspace,
   [/tez/tez_accounts/...], see [Path] below. *)
let root = Durable_storage_path.tezosx_tezos_blocks_root

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

  let balance contract = account contract ^ "/balance"

  let manager contract = account contract ^ "/manager"

  let counter contract = account contract ^ "/counter"

  let storage contract = account contract ^ "/data/storage"

  let code contract = account contract ^ "/data/code"

  (** Path to a specific big_map: /tez/tez_accounts/big_map/{id} *)
  let big_map_id id =
    big_map ^ "/"
    ^ Z.to_string (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)

  (** Path to a big_map value:
      /tez/tez_accounts/big_map/{id}/{key_hash_hex} where
      key_hash_hex is the hex encoding of the raw Script_expr_hash bytes *)
  let big_map_value id key_hash =
    let raw_hash =
      Tezlink_imports.Imported_protocol.Script_expr_hash.to_bytes key_hash
    in
    let (`Hex key_hex) = Hex.of_bytes raw_hash in
    big_map_id id ^ "/" ^ key_hex

  let big_map_key_type id = big_map_id id ^ "/key_type"

  let big_map_value_type id = big_map_id id ^ "/value_type"
end

let contract_of_path = Contract.of_hex

let balance state ~data_model (c : Contract.t) =
  let open Lwt_result_syntax in
  match data_model with
  | Rlp -> (
      match c with
      | Originated _ -> (
          let path = Path.balance c in
          let* bytes_opt = Durable_storage.read_opt (Raw_path path) state in
          match bytes_opt with
          | Some bytes -> (
              match
                Data_encoding.Binary.of_bytes Tezos_types.Tez.encoding bytes
              with
              | Ok balance -> return balance
              | Error e ->
                  failwith
                    "Cannot decode KT1 balance: %a"
                    Data_encoding.Binary.pp_read_error
                    e)
          | None -> return Tez.zero)
      | Implicit pkh -> (
          let* bytes_opt =
            Durable_storage.read_opt
              (Raw_path (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh))
              state
          in
          match bytes_opt with
          | None -> return Tez.zero
          | Some bytes ->
              let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
              return info.balance))
  | Path ->
      Durable_storage.inspect_durable_and_decode_default
        ~default:Tezos_types.Tez.zero
        state
        (Path.balance c)
        (Data_encoding.Binary.of_bytes_exn Tez.encoding)

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
          let* bytes_opt =
            Durable_storage.read_opt
              (Raw_path (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh))
              state
          in
          match bytes_opt with
          | None -> return_none
          | Some bytes -> (
              let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
              match info.public_key with
              | None -> return_none
              | Some public_key -> return_some (Manager.Public_key public_key)))
      )
  | Path ->
      Durable_storage.inspect_durable_and_decode_opt
        state
        (Path.manager c)
        (Data_encoding.Binary.of_bytes_exn Manager.encoding)

let counter state ~data_model (c : Contract.t) =
  let open Lwt_result_syntax in
  match data_model with
  | Rlp -> (
      match c with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* bytes_opt =
            Durable_storage.read_opt
              (Raw_path (Tezosx.Durable_storage_path.Accounts.Tezos.info pkh))
              state
          in
          match bytes_opt with
          | None -> return_none
          | Some bytes ->
              let*? info = Tezosx.Tezos_runtime.decode_account_info bytes in
              return_some (Z.of_int64 info.nonce)))
  | Path ->
      Durable_storage.inspect_durable_and_decode_opt
        state
        (Path.counter c)
        (Data_encoding.Binary.of_bytes_exn Data_encoding.n)

let big_map_get state id key_hash =
  let open Lwt_result_syntax in
  let path = Path.big_map_value id key_hash in
  let decode =
    Data_encoding.Binary.of_bytes_opt
      Tezlink_imports.Imported_context.Script.expr_encoding
  in
  let+ result =
    Durable_storage.inspect_durable_and_decode_opt state path decode
  in
  Option.join result

let big_map_key_type state id =
  let open Lwt_result_syntax in
  let path = Path.big_map_key_type id in
  let decode =
    Data_encoding.Binary.of_bytes_opt
      Tezlink_imports.Imported_context.Script.expr_encoding
  in
  let+ result =
    Durable_storage.inspect_durable_and_decode_opt state path decode
  in
  Option.join result

let big_map_value_type state id =
  let open Lwt_result_syntax in
  let path = Path.big_map_value_type id in
  let decode =
    Data_encoding.Binary.of_bytes_opt
      Tezlink_imports.Imported_context.Script.expr_encoding
  in
  let+ result =
    Durable_storage.inspect_durable_and_decode_opt state path decode
  in
  Option.join result

let nth_block state n =
  let open Lwt_result_syntax in
  let level = n in
  let* block_hash_opt =
    Durable_storage.inspect_durable_and_decode_opt
      state
      (Durable_storage_path.Indexes.block_by_number ~root (Nth level))
      Ethereum_types.decode_block_hash
  in
  match block_hash_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block_hash -> (
      let* block_opt =
        Durable_storage.inspect_durable_and_decode_opt
          state
          (Durable_storage_path.Block.by_hash ~root block_hash)
          L2_types.Tezos_block.block_from_kernel
      in
      match block_opt with
      | None ->
          raise
          @@ Durable_storage.Invalid_block_structure "Couldn't decode bytes"
      | Some block -> return block)

let nth_block_hash state n =
  let number = Durable_storage_path.Block.(Nth n) in
  Durable_storage.inspect_durable_and_decode_opt
    state
    (Durable_storage_path.Indexes.block_by_number ~root number)
    Ethereum_types.decode_block_hash

let da_fee_per_byte_mutez state =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty da_fee_per_byte_wei) =
    Durable_storage.read Da_fee_per_byte state
  in
  let*? da_fee_per_byte = Tezos_types.Tez.(of_wei (Wei da_fee_per_byte_wei)) in
  (* DA fee expressed in wei: converting to mutez. *)
  return da_fee_per_byte
