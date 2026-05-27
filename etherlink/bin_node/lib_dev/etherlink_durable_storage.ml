(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Durable_storage
open Ethereum_types

let post_v41_unsupported_function ~__FUNCTION__ =
  Invalid_argument (__FUNCTION__ ^ " is not supported for storage version >= 41")

let balance state address =
  let open Lwt_result_syntax in
  let* answer = Durable_storage.read_opt (Evm_account_info address) state in
  match answer with
  | Some info -> return info.balance
  | None ->
      Durable_storage.read_or_default
        ~default:(Ethereum_types.Qty Z.zero)
        (Evm_legacy_account_balance address)
        state

let nonce state address =
  let open Lwt_result_syntax in
  let* answer = Durable_storage.read_opt (Evm_account_info address) state in
  match answer with
  | Some info -> return @@ Some info.nonce
  | None -> Durable_storage.read_opt (Evm_legacy_account_nonce address) state

let code state address =
  let open Lwt_result_syntax in
  let default = Ethereum_types.Hex "" in
  let* account_opt =
    Durable_storage.read_opt (Evm_account_info address) state
  in
  match account_opt with
  | Some info ->
      Durable_storage.read_or_default
        ~default
        (Evm_code_by_hash info.code_hash)
        state
  | None -> (
      let* code_opt =
        Durable_storage.read_opt (Evm_legacy_account_code address) state
      in
      match code_opt with
      | Some code -> return code
      | None -> (
          let* hash_opt =
            Durable_storage.read_opt
              (Evm_legacy_account_code_hash address)
              state
          in
          match hash_opt with
          | None -> return default
          | Some hash ->
              Durable_storage.read_or_default
                ~default
                (Evm_code_by_hash hash)
                state))

let current_block_number state =
  Durable_storage.read (Current_block_number L2_types.EVM) state

let current_block_hash state =
  Durable_storage.read (Current_block_hash L2_types.EVM) state

let un_qty (Qty z) = z

let mock_block_hash = Block_hash (Hex (String.make 64 'a'))

let current_transactions_receipts block_hash storage_version state =
  if not (Storage_version.legacy_storage_compatible ~storage_version) then
    Durable_storage.read_or_default
      ~default:[]
      (Evm_current_block_receipts block_hash)
      state
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
      Durable_storage.read_opt
        (Evm_transaction_receipt_by_hash (tx_hash, block))
        state
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
              Durable_storage.read
                (Evm_block_hash_by_number
                   (Nth (un_qty temp_receipt.blockNumber)))
                state
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
    Durable_storage.read_or_default
      ~default:[]
      (Evm_current_block_transactions_objects block_hash)
      state
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
      Durable_storage.read_opt
        (Evm_transaction_object_by_hash (tx_hash, Some mock_block_hash))
        state
    in
    match opt_object with
    | Some temp_object ->
        let*? (blockNumber : quantity) =
          match temp_object.blockNumber with
          | None -> error_with "Unexpected null blockNumber in valid object"
          | Some n -> Ok n
        in
        let+ blockHash =
          Durable_storage.read
            (Evm_block_hash_by_number (Nth (un_qty blockNumber)))
            state
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
    Durable_storage.read_opt
      (Evm_transaction_object_by_hash (tx_hash, block_hash))
      state

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
      Durable_storage.read_opt (Evm_legacy_block_by_hash block_hash) state
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
    let* block_opt = Durable_storage.read_opt Evm_legacy_current_block state in
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
      Durable_storage.read_opt (Evm_block_hash_by_number (Nth level)) state
    in
    match block_hash_opt with
    | None -> failwith "Block %a not found" Z.pp_print level
    | Some block_hash -> (
        let* block_opt =
          Durable_storage.read_opt (Evm_legacy_block_by_hash block_hash) state
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
  let* storage_version = Durable_storage.storage_version state in
  let*? fixed_addr =
    Durable_storage_path.Accounts.fixed_address ~storage_version address
  in
  let*? fixed_idx = Durable_storage_path.Accounts.fixed_index index in
  Durable_storage.read_or_default
    ~default:(Ethereum_types.Hex (pad32left0 "0"))
    (Evm_account_storage (fixed_addr, fixed_idx))
    state

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
