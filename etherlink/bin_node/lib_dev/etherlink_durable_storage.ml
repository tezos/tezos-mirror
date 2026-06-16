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

let inspect_and_decode_opt_account state address =
  inspect_durable_and_decode_opt
    state
    (Durable_storage_path.Accounts.info address)
    AccountInfo.decode_exn

let balance state address =
  let open Lwt_result_syntax in
  let* answer = inspect_and_decode_opt_account state address in
  match answer with
  | Some info -> return info.balance
  | None ->
      inspect_durable_and_decode_default
        ~default:(Ethereum_types.Qty Z.zero)
        state
        (Durable_storage_path.Accounts.balance address)
        decode_number_le

let nonce state address =
  let open Lwt_result_syntax in
  let* answer = inspect_and_decode_opt_account state address in
  match answer with
  | Some info -> return @@ Some info.nonce
  | None ->
      inspect_durable_and_decode_opt
        state
        (Durable_storage_path.Accounts.nonce address)
        decode_number_le

let code state address =
  let open Lwt_result_syntax in
  let default = Ethereum_types.Hex "" in
  let decode bytes =
    bytes |> Hex.of_bytes |> Hex.show |> Ethereum_types.hex_of_string
  in
  let* account_opt = inspect_and_decode_opt_account state address in
  match account_opt with
  | Some info ->
      inspect_durable_and_decode_default
        ~default
        state
        (Durable_storage_path.Code.code info.code_hash)
        decode
  | None -> (
      let* code_opt =
        inspect_durable_and_decode_opt
          state
          (Durable_storage_path.Accounts.code address)
          decode
      in
      match code_opt with
      | Some code -> return code
      | None -> (
          let* hash_opt =
            inspect_durable_and_decode_opt
              state
              (Durable_storage_path.Accounts.code_hash address)
              (fun bytes ->
                Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hash_of_string)
          in
          match hash_opt with
          | None -> return default
          | Some hash ->
              inspect_durable_and_decode_default
                ~default
                state
                (Durable_storage_path.Code.code hash)
                decode))

let current_block_number state =
  Durable_storage.read (Current_block_number L2_types.EVM) state

let current_block_hash state =
  let open Lwt_result_syntax in
  let+ hash =
    inspect_durable_and_decode
      state
      (Durable_storage_path.Block.current_hash ~root)
      decode_block_hash
  in
  hash

let un_qty (Qty z) = z

let mock_block_hash = Block_hash (Hex (String.make 64 'a'))

let current_transactions_receipts block_hash storage_version state =
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    Durable_storage.inspect_durable_and_decode_default
      state
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

let transaction_receipt state ?block_hash tx_hash =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version state in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_hash = current_block_hash state in
    let* receipts =
      current_transactions_receipts block_hash storage_version state
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
        state
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
                state
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

let current_transactions_objects ?block_hash storage_version state =
  let open Lwt_result_syntax in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_hash =
      match block_hash with
      | Some bh -> return bh
      | None ->
          let+ bh = current_block_hash state in
          bh
    in
    Durable_storage.inspect_durable_and_decode_default
      state
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

let transaction_object state tx_hash =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version state in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* transaction_objects =
      current_transactions_objects storage_version state
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
        state
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
            state
            (Durable_storage_path.Indexes.block_by_number
               ~root
               (Nth (un_qty blockNumber)))
            decode_block_hash
        in
        Some {temp_object with blockHash = Some blockHash}
    | None -> return_none

let transaction_object_with_block_hash ?known_storage_version state block_hash
    tx_hash =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> Durable_storage.storage_version state
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    inspect_durable_and_decode_opt
      state
      (Durable_storage_path.Transaction_object.object_ tx_hash)
      (Ethereum_types.legacy_transaction_object_from_rlp block_hash)

let full_transactions ?known_storage_version state block_hash transactions =
  let open Lwt_result_syntax in
  match transactions with
  | TxHash hashes ->
      let+ objects =
        List.filter_map_es
          (transaction_object_with_block_hash
             ?known_storage_version
             state
             block_hash)
          hashes
      in
      TxFull objects
  | TxFull l -> return (TxFull l)

let populate_tx_objects ?known_storage_version state ~full_transaction_object
    (block : legacy_transaction_object block) =
  let open Lwt_result_syntax in
  if full_transaction_object then
    let* transactions =
      full_transactions
        ?known_storage_version
        state
        (Some block.hash)
        block.transactions
    in
    return {block with transactions}
  else return block

let block_by_hash ?known_storage_version state ~full_transaction_object
    block_hash =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> Durable_storage.storage_version state
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    let* block_opt =
      inspect_durable_and_decode_opt
        state
        (Durable_storage_path.Block.by_hash ~root block_hash)
        Ethereum_types.block_from_rlp
    in
    match block_opt with
    | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
    | Some block -> populate_tx_objects state ~full_transaction_object block

let current_block ?known_storage_version state ~full_transaction_object =
  let open Lwt_result_syntax in
  let* storage_version =
    match known_storage_version with
    | Some v -> return v
    | None -> Durable_storage.storage_version state
  in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    let* block_opt =
      inspect_durable_and_decode_opt
        state
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
        current_transactions_objects
          ~block_hash:block.hash
          storage_version
          state
      in
      return {block with transactions = TxFull transaction_objects}
    else return block
  else
    let* block_hash = current_block_hash state in
    block_by_hash state ~full_transaction_object block_hash

let blocks_by_number state ~full_transaction_object ~number =
  let open Lwt_result_syntax in
  let* storage_version = Durable_storage.storage_version state in
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    match number with
    | Durable_storage_path.Block.Current ->
        current_block
          ~known_storage_version:storage_version
          ~full_transaction_object
          state
    | Durable_storage_path.Block.(Nth n) ->
        let* (Qty current_number) = current_block_number state in
        if current_number = n then
          current_block
            ~known_storage_version:storage_version
            ~full_transaction_object
            state
        else raise (post_v41_unsupported_function ~__FUNCTION__)
  else
    let* (Ethereum_types.Qty level) =
      match number with
      | Durable_storage_path.Block.Nth i -> return (Ethereum_types.Qty i)
      | Current -> current_block_number state
    in
    let* block_hash_opt =
      inspect_durable_and_decode_opt
        state
        (Durable_storage_path.Indexes.block_by_number ~root (Nth level))
        decode_block_hash
    in
    match block_hash_opt with
    | None -> failwith "Block %a not found" Z.pp_print level
    | Some block_hash -> (
        let* block_opt =
          inspect_durable_and_decode_opt
            state
            (Durable_storage_path.Block.by_hash ~root block_hash)
            Ethereum_types.block_from_rlp
        in
        match block_opt with
        | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
        | Some block -> populate_tx_objects state ~full_transaction_object block
        )

let nth_block state ~full_transaction_object n =
  blocks_by_number
    state
    ~full_transaction_object
    ~number:Durable_storage_path.Block.(Nth n)

let block_receipts_of_block state block =
  let get_receipt_from_hash tx_hash =
    Lwt.map
      (function Ok receipt -> receipt | _ -> None)
      (transaction_receipt state ~block_hash:block.hash tx_hash)
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

let block_receipts state n =
  let number = Durable_storage_path.Block.(Nth n) in
  let open Lwt_result_syntax in
  let* block = blocks_by_number state ~full_transaction_object:false ~number in
  let*! receipts = block_receipts_of_block state block in
  Lwt.return_ok receipts

let base_fee_per_gas_opt state =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let* block = current_block state ~full_transaction_object:false in
      return block.baseFeePerGas)
    (function
      | Durable_storage.Invalid_block_structure _ -> return_none
      | exn -> Lwt.reraise exn)

let base_fee_per_gas state =
  let open Lwt_result_syntax in
  let* base_fee_per_gas = base_fee_per_gas_opt state in
  match base_fee_per_gas with
  | Some base_fee_per_gas -> return base_fee_per_gas
  | None ->
      Error_monad.failwith
        "Attempted to get the base fee per gas from a block which does not \
         have one."

let michelson_to_evm_gas_multiplier state =
  let open Lwt_result_syntax in
  let+ v_opt = read_opt Michelson_to_evm_gas_multiplier state in
  Option.value v_opt ~default:10L

let backlog state =
  let open Lwt_result_syntax in
  let+ read_result = read_opt Backlog state in
  match read_result with Some i -> Z.of_int64_unsigned i | None -> Z.zero

let minimum_base_fee_per_gas_opt state = read_opt Minimum_base_fee_per_gas state

let minimum_base_fee_per_gas state = read Minimum_base_fee_per_gas state

let storage_at state address (Qty pos) =
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
    inspect_durable_and_decode_opt
      state
      (Durable_storage_path.Accounts.storage address index)
      Fun.id
  in
  match answer with
  | Some bytes ->
      Bytes.to_string bytes |> Hex.of_string |> Hex.show
      |> Ethereum_types.hex_of_string
  | None -> Ethereum_types.Hex (pad32left0 "0")

let coinbase state =
  let open Lwt_result_syntax in
  let+ addr_opt = read_opt Sequencer_pool_address state in
  Option.value
    addr_opt
    ~default:
      (Address
         (Ethereum_types.hex_of_string
            "0x0000000000000000000000000000000000000000"))

let maximum_gas_per_transaction state =
  let open Lwt_result_syntax in
  let+ v_opt = read_opt Maximum_gas_per_transaction state in
  Option.value v_opt ~default:(Qty (Z.of_string "30_000_000"))

let da_fee_per_byte state = read Da_fee_per_byte state
