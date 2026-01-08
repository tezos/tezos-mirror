(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

let root = Durable_storage_path.tezlink_root

module Path = struct
  (** [to_path encoding value] uses [encoding] to encode [value] in
      hexadecimal *)
  let to_path encoding value =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding value in
    let (`Hex s) = Hex.of_bytes raw_key in
    s

  let accounts_index = "/tezlink/context/contracts/index"

  let big_map = "/tezlink/context/big_map"

  let account contract =
    accounts_index ^ "/" ^ to_path Contract.encoding contract

  let balance contract = account contract ^ "/balance"

  let manager contract = account contract ^ "/manager"

  let counter contract = account contract ^ "/counter"

  let storage contract = account contract ^ "/data/storage"

  let code contract = account contract ^ "/data/code"
end

let contract_of_path = Contract.of_hex

let balance read c =
  Durable_storage.inspect_durable_and_decode_default
    ~default:Tezos_types.Tez.zero
    read
    (Path.balance c)
    (Data_encoding.Binary.of_bytes_exn Tez.encoding)

let balance_z read c =
  let open Lwt_result_syntax in
  let* b = balance read c in
  return @@ Tezos_types.Tez.to_mutez_z b

let manager read c =
  Durable_storage.inspect_durable_and_decode_opt
    read
    (Path.manager c)
    (Data_encoding.Binary.of_bytes_exn Manager.encoding)

let counter read c =
  Durable_storage.inspect_durable_and_decode_default
  (* FIXME: #7960
     This default should be the global counter *)
    ~default:Z.one
    read
    (Path.counter c)
    (Data_encoding.Binary.of_bytes_exn Data_encoding.n)

let nth_block read n =
  let open Lwt_result_syntax in
  let number = Durable_storage_path.Block.(Nth n) in
  let* (Ethereum_types.Qty level) =
    Durable_storage.block_number ~root read number
  in
  let* block_hash_opt =
    Durable_storage.inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Indexes.block_by_number ~root (Nth level))
      Ethereum_types.decode_block_hash
  in
  match block_hash_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block_hash -> (
      let* block_opt =
        Durable_storage.inspect_durable_and_decode_opt
          read
          (Durable_storage_path.Block.by_hash ~root block_hash)
          L2_types.Tezos_block.block_from_binary
      in
      match block_opt with
      | None ->
          raise
          @@ Durable_storage.Invalid_block_structure "Couldn't decode bytes"
      | Some block -> return block)

let nth_block_hash read n =
  let number = Durable_storage_path.Block.(Nth n) in
  Durable_storage.inspect_durable_and_decode_opt
    read
    (Durable_storage_path.Indexes.block_by_number ~root number)
    Ethereum_types.decode_block_hash

module Make_block_storage (Reader : Durable_storage.READER) :
  Tezlink_block_storage_sig.S = struct
  let read_with_state () =
    let open Lwt_result_syntax in
    let* state = Reader.get_state () in
    return (Reader.read state)

  let nth_block n =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    nth_block read n

  let nth_block_hash n =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    nth_block_hash read n
end
