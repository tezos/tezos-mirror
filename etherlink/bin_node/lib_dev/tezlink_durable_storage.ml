(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

module Path = struct
  (** [to_path encoding value] uses [encoding] to encode [value] in
      hexadecimal *)
  let to_path encoding value =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding value in
    let (`Hex s) = Hex.of_bytes raw_key in
    s

  let account contract =
    "/tezlink/context/contracts/index/" ^ to_path Contract.encoding contract

  let balance contract = account contract ^ "/balance"

  let manager_key contract = account contract ^ "/manager_key"

  let counter contract = account contract ^ "/counter"
end

let balance read c =
  Durable_storage.inspect_durable_and_decode_default
    ~default:Tezos_types.Tez.zero
    read
    (Path.balance c)
    (Data_encoding.Binary.of_bytes_exn Tez.encoding)

let manager_key read c =
  Durable_storage.inspect_durable_and_decode_opt
    read
    (Path.manager_key c)
    (Data_encoding.Binary.of_bytes_exn Signature.V1.Public_key.encoding)

let counter read c =
  Durable_storage.inspect_durable_and_decode_default
  (* FIXME: #7960
     This default should be the global counter *)
    ~default:Z.one
    read
    (Path.counter c)
    (Data_encoding.Binary.of_bytes_exn Data_encoding.z)

let nth_block read n =
  let open Lwt_result_syntax in
  let number = Durable_storage_path.Block.(Nth n) in
  let* (Ethereum_types.Qty level) = Durable_storage.block_number read number in
  let* block_hash_opt =
    Durable_storage.inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Indexes.block_by_number (Nth level))
      Ethereum_types.decode_block_hash
  in
  match block_hash_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block_hash -> (
      let* block_opt =
        Durable_storage.inspect_durable_and_decode_opt
          read
          (Durable_storage_path.Block.by_hash block_hash)
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
    (Durable_storage_path.Indexes.block_by_number number)
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
