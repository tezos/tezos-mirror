(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Durable_storage
open Ethereum_types

let root = Durable_storage_path.etherlink_root

let post_v41_unsupported_function ~__FUNCTION__ =
  Invalid_argument (__FUNCTION__ ^ " is not supported for storage version >= 41")

module AccountInfo = struct
  type t = {
    balance : Ethereum_types.quantity;
    nonce : Ethereum_types.quantity;
    code_hash : Ethereum_types.hash;
  }

  let encode {balance; nonce; code_hash} =
    let open Rlp in
    Rlp.encode
      (List
         [
           Value (encode_u256_le balance);
           Value (encode_u64_le nonce);
           Value (encode_hash code_hash);
         ])

  let decode_opt b =
    match Rlp.decode b with
    | Ok (List [Value balance; Value nonce; Value code_hash]) ->
        let balance = decode_number_le balance in
        let nonce = decode_number_le nonce in
        let code_hash = decode_hash code_hash in
        Some {balance; nonce; code_hash}
    | _ -> None

  let decode_exn b =
    match decode_opt b with
    | Some x -> x
    | None ->
        Stdlib.failwith
        @@ Format.asprintf "Invalid account info format: %s"
        @@ Bytes.to_string b
end

let inspect_and_decode_opt_account read address =
  inspect_durable_and_decode_opt
    read
    (Durable_storage_path.Accounts.info address)
    AccountInfo.decode_exn

let balance read address =
  let open Lwt_result_syntax in
  let* answer = inspect_and_decode_opt_account read address in
  match answer with
  | Some info -> return info.balance
  | None ->
      inspect_durable_and_decode_default
        ~default:(Ethereum_types.Qty Z.zero)
        read
        (Durable_storage_path.Accounts.balance address)
        decode_number_le

let nonce read address =
  let open Lwt_result_syntax in
  let* answer = inspect_and_decode_opt_account read address in
  match answer with
  | Some info -> return @@ Some info.nonce
  | None ->
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Accounts.nonce address)
        decode_number_le

let code read address =
  let open Lwt_result_syntax in
  let default = Ethereum_types.Hex "" in
  let decode bytes =
    bytes |> Hex.of_bytes |> Hex.show |> Ethereum_types.hex_of_string
  in
  let* account_opt = inspect_and_decode_opt_account read address in
  match account_opt with
  | Some info ->
      inspect_durable_and_decode_default
        ~default
        read
        (Durable_storage_path.Code.code info.code_hash)
        decode
  | None -> (
      let* code_opt =
        inspect_durable_and_decode_opt
          read
          (Durable_storage_path.Accounts.code address)
          decode
      in
      match code_opt with
      | Some code -> return code
      | None -> (
          let* hash_opt =
            inspect_durable_and_decode_opt
              read
              (Durable_storage_path.Accounts.code_hash address)
              (fun bytes ->
                Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hash_of_string)
          in
          match hash_opt with
          | None -> return default
          | Some hash ->
              inspect_durable_and_decode_default
                ~default
                read
                (Durable_storage_path.Code.code hash)
                decode))

let current_block_number read =
  Durable_storage.block_number ~root read Durable_storage_path.Block.Current

let current_block_hash read =
  let open Lwt_result_syntax in
  let+ hash =
    inspect_durable_and_decode
      read
      (Durable_storage_path.Block.current_hash ~root)
      decode_block_hash
  in
  hash

let un_qty (Qty z) = z

let mock_block_hash = Block_hash (Hex (String.make 64 'a'))

let current_transactions_receipts block_hash storage_version read =
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    Durable_storage.inspect_durable_and_decode_default
      read
      (Durable_storage_path.Block.current_receipts
         ~root:Durable_storage_path.etherlink_root)
      ~default:[]
      (function bytes ->
        (match Rlp.decode bytes with
        | Ok (Rlp.List receipts_rlp) ->
            List.map
              (fun rlp -> Transaction_receipt.of_rlp_item block_hash rlp)
              receipts_rlp
        | _ -> raise (Invalid_argument "Transaction receipts should be a list")))
  else raise (post_v41_unsupported_function ~__FUNCTION__)

let transaction_receipt read ?block_hash tx_hash =
  let open Lwt_result_syntax in
  let* storage_version = storage_version read in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_hash = current_block_hash read in
    let* receipts =
      current_transactions_receipts block_hash storage_version read
    in
    return
      (List.find_opt
         (fun (receipt : Transaction_receipt.t) ->
           receipt.transactionHash = tx_hash)
         receipts)
  else
    (* We use a mock block hash to decode the rest of the receipt,
      so that we can get the number to query for the actual block
      hash (only if the block hash isn't already available). *)
    let block = Option.value block_hash ~default:mock_block_hash in
    let* opt_receipt =
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Transaction_receipt.receipt tx_hash)
        (Transaction_receipt.of_rlp_bytes block)
    in
    match block_hash with
    | Some _ ->
        (* Correct receipt *)
        return opt_receipt
    | None -> (
        (* Need to replace with correct block hash *)
        match opt_receipt with
        | Some temp_receipt ->
            let+ blockHash =
              inspect_durable_and_decode
                read
                (Durable_storage_path.Indexes.block_by_number
                   ~root
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
        | None -> return_none)

let current_transactions_objects ?block_hash storage_version read =
  let open Lwt_result_syntax in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_hash =
      match block_hash with
      | Some bh -> return bh
      | None ->
          let+ bh = current_block_hash read in
          bh
    in
    Durable_storage.inspect_durable_and_decode_default
      read
      (Durable_storage_path.Block.current_transactions_objects
         ~root:Durable_storage_path.etherlink_root)
      ~default:[]
      (function bytes ->
        (match Rlp.decode bytes with
        | Ok (Rlp.List objects_rlp) ->
            List.map
              (fun rlp ->
                Ethereum_types.legacy_transaction_object_from_rlp_item
                  (Some block_hash)
                  rlp)
              objects_rlp
        | _ -> raise (Invalid_argument "Transaction objects should be a list")))
  else raise (post_v41_unsupported_function ~__FUNCTION__)

let transaction_object read tx_hash =
  let open Lwt_result_syntax in
  let* storage_version = storage_version read in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* transaction_objects =
      current_transactions_objects storage_version read
    in
    return
      (List.find_opt
         (fun (obj : Ethereum_types.legacy_transaction_object) ->
           obj.hash = tx_hash)
         transaction_objects)
  else
    (* We use a mock block hash to decode the rest of the receipt,
     so that we can get the number to query for the actual block
     hash. *)
    let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
    let* opt_object =
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Transaction_object.object_ tx_hash)
        (Ethereum_types.legacy_transaction_object_from_rlp
           (Some mock_block_hash))
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
            read
            (Durable_storage_path.Indexes.block_by_number
               ~root
               (Nth (un_qty blockNumber)))
            decode_block_hash
        in
        Some {temp_object with blockHash = Some blockHash}
    | None -> return_none

let transaction_object_with_block_hash ?known_storage_version read block_hash
    tx_hash =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> storage_version read
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Transaction_object.object_ tx_hash)
      (Ethereum_types.legacy_transaction_object_from_rlp block_hash)

let full_transactions ?known_storage_version read block_hash transactions =
  let open Lwt_result_syntax in
  match transactions with
  | TxHash hashes ->
      let+ objects =
        List.filter_map_es
          (transaction_object_with_block_hash
             ?known_storage_version
             read
             block_hash)
          hashes
      in
      TxFull objects
  | TxFull l -> return (TxFull l)

let populate_tx_objects ?known_storage_version read ~full_transaction_object
    (block : legacy_transaction_object block) =
  let open Lwt_result_syntax in
  if full_transaction_object then
    let* transactions =
      full_transactions
        ?known_storage_version
        read
        (Some block.hash)
        block.transactions
    in
    return {block with transactions}
  else return block

let block_by_hash ?known_storage_version read ~full_transaction_object
    block_hash =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> storage_version read
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    let* block_opt =
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Block.by_hash ~root block_hash)
        Ethereum_types.block_from_rlp
    in
    match block_opt with
    | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
    | Some block -> populate_tx_objects read ~full_transaction_object block

let current_block ?known_storage_version read ~full_transaction_object =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> storage_version read
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_opt =
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Block.current_block ~root)
        Ethereum_types.block_from_rlp
    in
    let block =
      match block_opt with
      | Some block -> block
      | None ->
          raise
          @@ Invalid_block_structure "Couldn't decode bytes of current block"
    in
    if full_transaction_object then
      let* transaction_objects =
        current_transactions_objects ~block_hash:block.hash storage_version read
      in
      return {block with transactions = TxFull transaction_objects}
    else return block
  else
    let* block_hash = current_block_hash read in
    block_by_hash read ~full_transaction_object block_hash

let blocks_by_number read ~full_transaction_object ~number =
  let open Lwt_result_syntax in
  let* storage_version = storage_version read in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    match number with
    | Durable_storage_path.Block.Current ->
        current_block
          ~known_storage_version:storage_version
          ~full_transaction_object
          read
    | Durable_storage_path.Block.(Nth n) ->
        let* (Qty current_number) = current_block_number read in
        if current_number = n then
          current_block
            ~known_storage_version:storage_version
            ~full_transaction_object
            read
        else raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    let* (Ethereum_types.Qty level) = block_number ~root read number in
    let* block_hash_opt =
      inspect_durable_and_decode_opt
        read
        (Durable_storage_path.Indexes.block_by_number ~root (Nth level))
        decode_block_hash
    in
    match block_hash_opt with
    | None -> failwith "Block %a not found" Z.pp_print level
    | Some block_hash -> (
        let* block_opt =
          inspect_durable_and_decode_opt
            read
            (Durable_storage_path.Block.by_hash ~root block_hash)
            Ethereum_types.block_from_rlp
        in
        match block_opt with
        | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
        | Some block -> populate_tx_objects read ~full_transaction_object block)

let nth_block read ~full_transaction_object n =
  blocks_by_number
    read
    ~full_transaction_object
    ~number:Durable_storage_path.Block.(Nth n)

let block_receipts_of_block read block =
  let get_receipt_from_hash tx_hash =
    Lwt.map
      (function Ok receipt -> receipt | _ -> None)
      (transaction_receipt read ~block_hash:block.hash tx_hash)
  in
  let tx_hashes : hash list =
    match block.transactions with
    | TxHash tx_hashes -> tx_hashes
    | TxFull tx_objects ->
        (* This case should never happen, because there is no ways
           to ask for full objects when requestion block receipts. *)
        List.map (fun (obj : legacy_transaction_object) -> obj.hash) tx_objects
  in
  Lwt_list.filter_map_s get_receipt_from_hash tx_hashes

let block_receipts read n =
  let number = Durable_storage_path.Block.(Nth n) in
  let open Lwt_result_syntax in
  let* block = blocks_by_number read ~full_transaction_object:false ~number in
  let*! receipts = block_receipts_of_block read block in
  Lwt.return_ok receipts

let base_fee_per_gas_opt read =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* block = current_block read ~full_transaction_object:false in
      return block.baseFeePerGas)
    (function
      | Durable_storage.Invalid_block_structure _ -> return_none
      | exn -> Lwt.reraise exn)

let base_fee_per_gas read =
  let open Lwt_result_syntax in
  let* base_fee_per_gas = base_fee_per_gas_opt read in
  match base_fee_per_gas with
  | Some base_fee_per_gas -> return base_fee_per_gas
  | None ->
      Error_monad.failwith
        "Attempted to get the base fee per gas from a block which does not \
         have one."

let backlog read =
  let open Lwt_result_syntax in
  let+ read_result = read Durable_storage_path.backlog in
  match read_result with
  | Some backlog_bytes ->
      Z.of_int64_unsigned
        Data_encoding.(Binary.of_bytes_exn Little_endian.int64 backlog_bytes)
  | None -> Z.zero

let minimum_base_fee_per_gas_opt read =
  inspect_durable_and_decode_opt
    read
    Durable_storage_path.minimum_base_fee_per_gas
    Helpers.decode_z_le

let minimum_base_fee_per_gas read =
  inspect_durable_and_decode
    read
    Durable_storage_path.minimum_base_fee_per_gas
    Helpers.decode_z_le

let storage_at read address (Qty pos) =
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
  let+ answer = read (Durable_storage_path.Accounts.storage address index) in
  match answer with
  | Some bytes ->
      Bytes.to_string bytes |> Hex.of_string |> Hex.show
      |> Ethereum_types.hex_of_string
  | None -> Ethereum_types.Hex (pad32left0 "0")

let coinbase read =
  inspect_durable_and_decode_default
    ~default:
      (Address
         (Ethereum_types.hex_of_string
            "0x0000000000000000000000000000000000000000"))
    read
    Durable_storage_path.sequencer_pool_address
    (fun bytes ->
      Address (Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hex_of_string))

let maximum_gas_per_transaction read =
  (* In future iterations of the kernel, the default value will be
     written to the storage. This default value will no longer need to
     be declared here. *)
  inspect_durable_and_decode_default
    ~default:(Qty (Z.of_string "30_000_000"))
    read
    Durable_storage_path.maximum_gas_per_transaction
    decode_number_le

let da_fee_per_byte read =
  inspect_durable_and_decode
    read
    Durable_storage_path.da_fee_per_byte
    decode_number_le

module Make (Reader : READER) = struct
  let read = Reader.read

  let read_with_state ?block () =
    let open Lwt_result_syntax in
    let* state = Reader.get_state ?block () in
    return (Reader.read state)

  let balance address block =
    let open Lwt_result_syntax in
    let* read = read_with_state ~block () in
    balance read address

  let nonce address block =
    let open Lwt_result_syntax in
    let* read = read_with_state ~block () in
    nonce read address

  let code address block =
    let open Lwt_result_syntax in
    let* read = read_with_state ~block () in
    code read address

  let base_fee_per_gas () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    base_fee_per_gas read

  let base_fee_per_gas_opt () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    base_fee_per_gas_opt read

  let backlog () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    backlog read

  let minimum_base_fee_per_gas () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    minimum_base_fee_per_gas read

  let minimum_base_fee_per_gas_opt () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    minimum_base_fee_per_gas_opt read

  let storage_at address pos block =
    let open Lwt_result_syntax in
    let* read = read_with_state ~block () in
    storage_at read address pos

  let coinbase () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    coinbase read
end

(**
  This module is only used by the proxy mode, which is deprecated and
  scheduled to be removed. Therefore, this module will also be removed in the
  near future.
  For now, after storage version 40, it still support serving data that are available
  in the last block only. It does not support querying historical blocks anymore.
  It would be removed after the proxy mode removal.
  See https://linear.app/tezos/issue/L2-301/drop-the-proxy-mode
  for more details.
*)
module Make_block_storage (Reader : READER) = struct
  let read = Reader.read

  let read_with_state () =
    let open Lwt_result_syntax in
    let* state = Reader.get_state () in
    return (Reader.read state)

  let transaction_receipt tx_hash =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    transaction_receipt read tx_hash

  let current_block_number () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    current_block_number read

  let nth_block ~full_transaction_object n =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    let+ block = nth_block read ~full_transaction_object n in
    Transaction_object.block_from_legacy block

  let block_by_hash ~full_transaction_object block_hash =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    let+ block = block_by_hash read ~full_transaction_object block_hash in
    Transaction_object.block_from_legacy block

  let block_receipts n =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    block_receipts read n

  let block_range_receipts ?mask:_ _n _len = failwith "Not supported"

  let transaction_object tx_hash =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    let+ transaction_object = transaction_object read tx_hash in
    Option.map
      Transaction_object.from_store_transaction_object
      transaction_object
end
