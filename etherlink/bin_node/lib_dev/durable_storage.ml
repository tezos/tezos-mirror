(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module type READER = sig
  val read :
    ?block:Ethereum_types.Block_parameter.extended ->
    Durable_storage_path.path ->
    bytes option tzresult Lwt.t

  val subkeys :
    ?block:Ethereum_types.Block_parameter.extended ->
    Durable_storage_path.path ->
    string list tzresult Lwt.t
end

module Make (Reader : READER) = struct
  let inspect_durable_and_decode_opt path decode =
    let open Lwt_result_syntax in
    let* bytes = Reader.read path in
    match bytes with
    | Some bytes -> return_some (decode bytes)
    | None -> return_none

  let inspect_durable_and_decode path decode =
    let open Lwt_result_syntax in
    let* res_opt = inspect_durable_and_decode_opt path decode in
    match res_opt with Some res -> return res | None -> failwith "null"

  let balance address block =
    let open Lwt_result_syntax in
    let+ answer =
      Reader.read ~block (Durable_storage_path.Accounts.balance address)
    in
    match answer with
    | Some bytes ->
        Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z
    | None -> Ethereum_types.Qty Z.zero

  let nonce address block =
    let open Lwt_result_syntax in
    let+ answer =
      Reader.read ~block (Durable_storage_path.Accounts.nonce address)
    in
    answer
    |> Option.map (fun bytes ->
           bytes |> Bytes.to_string |> Z.of_bits |> Ethereum_types.quantity_of_z)

  let code address block =
    let open Lwt_result_syntax in
    let+ answer =
      Reader.read ~block (Durable_storage_path.Accounts.code address)
    in
    match answer with
    | Some bytes ->
        bytes |> Hex.of_bytes |> Hex.show |> Ethereum_types.hex_of_string
    | None -> Ethereum_types.Hex ""

  exception Invalid_block_structure of string

  let block_number n =
    let open Lwt_result_syntax in
    match n with
    (* This avoids an unecessary service call in case we ask a block's number
       with an already expected/known block number [n]. *)
    | Durable_storage_path.Block.Nth i -> return @@ Ethereum_types.Qty i
    | Durable_storage_path.Block.Current -> (
        let+ answer = Reader.read Durable_storage_path.Block.current_number in
        match answer with
        | Some bytes -> Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits)
        | None ->
            raise
            @@ Invalid_block_structure
                 "Unexpected [None] value for [current_number]'s [answer]")

  let current_block_number () = block_number Durable_storage_path.Block.Current

  let un_qty (Qty z) = z

  let transaction_receipt tx_hash =
    let open Lwt_result_syntax in
    (* We use a mock block hash to decode the rest of the receipt,
       so that we can get the number to query for the actual block
       hash. *)
    let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
    let* opt_receipt =
      inspect_durable_and_decode_opt
        (Durable_storage_path.Transaction_receipt.receipt tx_hash)
        (Transaction_receipt.of_rlp_bytes mock_block_hash)
    in
    match opt_receipt with
    | Some temp_receipt ->
        let+ blockHash =
          inspect_durable_and_decode
            (Durable_storage_path.Indexes.block_by_number
               (Nth (un_qty temp_receipt.blockNumber)))
            decode_block_hash
        in
        let logs =
          List.map
            (fun (log : transaction_log) ->
              {log with blockHash = Some blockHash})
            temp_receipt.logs
        in
        Some {temp_receipt with blockHash; logs}
    | None -> return_none

  let transaction_object tx_hash =
    let open Lwt_result_syntax in
    (* We use a mock block hash to decode the rest of the receipt,
       so that we can get the number to query for the actual block
       hash. *)
    let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
    let* opt_object =
      inspect_durable_and_decode_opt
        (Durable_storage_path.Transaction_object.object_ tx_hash)
        (Ethereum_types.transaction_object_from_rlp (Some mock_block_hash))
    in
    match opt_object with
    | Some temp_object ->
        let*? (blockNumber : quantity) =
          match temp_object.blockNumber with
          | None -> error_with "Unexpected null blockNumber in valid object"
          | Some n -> Ok n
        in
        let+ blockHash =
          inspect_durable_and_decode
            (Durable_storage_path.Indexes.block_by_number
               (Nth (un_qty blockNumber)))
            decode_block_hash
        in
        Some {temp_object with blockHash = Some blockHash}
    | None -> return_none

  let transaction_object_with_block_hash block_hash tx_hash =
    inspect_durable_and_decode_opt
      (Durable_storage_path.Transaction_object.object_ tx_hash)
      (Ethereum_types.transaction_object_from_rlp block_hash)

  let full_transactions block_hash transactions =
    let open Lwt_result_syntax in
    match transactions with
    | TxHash hashes ->
        let+ objects =
          List.filter_map_es
            (transaction_object_with_block_hash block_hash)
            hashes
        in
        TxFull objects
    | TxFull _ -> return transactions

  let populate_tx_objects ~full_transaction_object block =
    let open Lwt_result_syntax in
    if full_transaction_object then
      let* transactions =
        full_transactions (Some block.hash) block.transactions
      in
      return {block with transactions}
    else return block

  let blocks_by_number ~full_transaction_object ~number =
    let open Lwt_result_syntax in
    let* (Ethereum_types.Qty level) = block_number number in
    let* block_hash_opt =
      inspect_durable_and_decode_opt
        (Durable_storage_path.Indexes.block_by_number (Nth level))
        decode_block_hash
    in
    match block_hash_opt with
    | None -> failwith "Unknown block %a" Z.pp_print level
    | Some block_hash -> (
        let* block_opt =
          inspect_durable_and_decode_opt
            (Durable_storage_path.Block.by_hash block_hash)
            Ethereum_types.block_from_rlp
        in
        match block_opt with
        | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
        | Some block -> populate_tx_objects ~full_transaction_object block)

  let current_block ~full_transaction_object =
    blocks_by_number
      ~full_transaction_object
      ~number:Durable_storage_path.Block.Current

  let nth_block ~full_transaction_object n =
    blocks_by_number
      ~full_transaction_object
      ~number:Durable_storage_path.Block.(Nth n)

  let nth_block_hash n =
    let number = Durable_storage_path.Block.(Nth n) in
    inspect_durable_and_decode_opt
      (Durable_storage_path.Indexes.block_by_number number)
      decode_block_hash

  let block_by_hash ~full_transaction_object block_hash =
    let open Lwt_result_syntax in
    let* block_opt =
      inspect_durable_and_decode_opt
        (Durable_storage_path.Block.by_hash block_hash)
        Ethereum_types.block_from_rlp
    in
    match block_opt with
    | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
    | Some block -> populate_tx_objects ~full_transaction_object block

  let block_receipts n =
    let number = Durable_storage_path.Block.(Nth n) in
    let open Lwt_result_syntax in
    let* block = blocks_by_number ~full_transaction_object:false ~number in
    let get_receipt_from_hash tx_hash =
      Lwt.map
        (function Ok receipt -> receipt | _ -> None)
        (transaction_receipt tx_hash)
    in
    let tx_hashes : hash list =
      match block.transactions with
      | TxHash tx_hashes -> tx_hashes
      | TxFull tx_objects ->
          (* This case should never happen, because there is no ways
             to ask for full objects when requestion block receipts. *)
          List.map
            (fun (tx_object : transaction_object) -> tx_object.hash)
            tx_objects
    in
    let*! receipts = Lwt_list.filter_map_s get_receipt_from_hash tx_hashes in
    Lwt.return_ok receipts

  let chain_id () =
    inspect_durable_and_decode Durable_storage_path.chain_id decode_number_le

  let base_fee_per_gas () =
    inspect_durable_and_decode
      Durable_storage_path.base_fee_per_gas
      decode_number_le

  let kernel_version () =
    inspect_durable_and_decode
      Durable_storage_path.kernel_version
      Bytes.to_string

  let kernel_root_hash () =
    inspect_durable_and_decode_opt
      Durable_storage_path.kernel_root_hash
      Bytes.to_string

  let storage_at address (Qty pos) block =
    let open Lwt_result_syntax in
    let pad32left0 s =
      let open Ethereum_types in
      (* Strip 0x *)
      let (Hex s) = hex_of_string s in
      let len = String.length s in
      (* This is a Hex string of 32 bytes, therefore the length is 64 *)
      String.make (64 - len) '0' ^ s
    in
    let index = Z.format "#x" pos |> pad32left0 in
    let+ answer =
      Reader.read ~block (Durable_storage_path.Accounts.storage address index)
    in
    match answer with
    | Some bytes ->
        Bytes.to_string bytes |> Hex.of_string |> Hex.show
        |> Ethereum_types.hex_of_string
    | None -> Ethereum_types.Hex (pad32left0 "0")

  let coinbase () =
    let open Lwt_result_syntax in
    let+ res =
      inspect_durable_and_decode_opt
        Durable_storage_path.sequencer_pool_address
        (fun bytes ->
          Address
            (Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hex_of_string))
    in
    Option.value
      ~default:
        (Address
           (Ethereum_types.hex_of_string
              "0x0000000000000000000000000000000000000000"))
      res
end
