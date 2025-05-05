(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module type READER = sig
  type state

  val get_state :
    ?block:Ethereum_types.Block_parameter.extended ->
    unit ->
    state tzresult Lwt.t

  val read : state -> Durable_storage_path.path -> bytes option tzresult Lwt.t

  val subkeys : state -> Durable_storage_path.path -> string list tzresult Lwt.t
end

let inspect_durable_and_decode_opt read path decode =
  let open Lwt_result_syntax in
  let* bytes = read path in
  match bytes with
  | Some bytes -> return_some (decode bytes)
  | None -> return_none

let inspect_durable_and_decode_default ~default read path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt read path decode in
  match res_opt with Some res -> return res | None -> return default

let inspect_durable_and_decode read path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt read path decode in
  match res_opt with
  | Some res -> return res
  | None -> failwith "No value found under %s" path

let balance read address =
  inspect_durable_and_decode_default
    ~default:(Ethereum_types.Qty Z.zero)
    read
    (Durable_storage_path.Accounts.balance address)
    decode_number_le

let nonce read address =
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
  let* answer =
    inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Accounts.code address)
      decode
  in
  match answer with
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
            decode)

exception Invalid_block_structure of string

let block_number read n =
  let open Lwt_result_syntax in
  match n with
  (* This avoids an unecessary service call in case we ask a block's number
     with an already expected/known block number [n]. *)
  | Durable_storage_path.Block.Nth i -> return @@ Ethereum_types.Qty i
  | Durable_storage_path.Block.Current -> (
      let+ answer = read Durable_storage_path.Block.current_number in
      match answer with
      | Some bytes -> Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits)
      | None ->
          raise
          @@ Invalid_block_structure
               "Unexpected [None] value for [current_number]'s [answer]")

let current_block_number read =
  block_number read Durable_storage_path.Block.Current

let un_qty (Qty z) = z

let mock_block_hash = Block_hash (Hex (String.make 64 'a'))

let transaction_receipt read ?block_hash tx_hash =
  let open Lwt_result_syntax in
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

let transaction_object read tx_hash =
  let open Lwt_result_syntax in
  (* We use a mock block hash to decode the rest of the receipt,
     so that we can get the number to query for the actual block
     hash. *)
  let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
  let* opt_object =
    inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Transaction_object.object_ tx_hash)
      (Ethereum_types.legacy_transaction_object_from_rlp (Some mock_block_hash))
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
             (Nth (un_qty blockNumber)))
          decode_block_hash
      in
      Some {temp_object with blockHash = Some blockHash}
  | None -> return_none

let transaction_object_with_block_hash read block_hash tx_hash =
  inspect_durable_and_decode_opt
    read
    (Durable_storage_path.Transaction_object.object_ tx_hash)
    (Ethereum_types.legacy_transaction_object_from_rlp block_hash)

let full_transactions read block_hash transactions =
  let open Lwt_result_syntax in
  match transactions with
  | TxHash hashes ->
      let+ objects =
        List.filter_map_es
          (transaction_object_with_block_hash read block_hash)
          hashes
      in
      TxFull objects
  | TxFull l -> return (TxFull l)

let populate_tx_objects read ~full_transaction_object
    (block : legacy_transaction_object block) =
  let open Lwt_result_syntax in
  if full_transaction_object then
    let* transactions =
      full_transactions read (Some block.hash) block.transactions
    in
    return {block with transactions}
  else return block

let blocks_by_number read ~full_transaction_object ~number =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty level) = block_number read number in
  let* block_hash_opt =
    inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Indexes.block_by_number (Nth level))
      decode_block_hash
  in
  match block_hash_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block_hash -> (
      let* block_opt =
        inspect_durable_and_decode_opt
          read
          (Durable_storage_path.Block.by_hash block_hash)
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

let nth_block_hash read n =
  let number = Durable_storage_path.Block.(Nth n) in
  inspect_durable_and_decode_opt
    read
    (Durable_storage_path.Indexes.block_by_number number)
    decode_block_hash

let block_by_hash read ~full_transaction_object block_hash =
  let open Lwt_result_syntax in
  let* block_opt =
    inspect_durable_and_decode_opt
      read
      (Durable_storage_path.Block.by_hash block_hash)
      Ethereum_types.block_from_rlp
  in
  match block_opt with
  | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
  | Some block -> populate_tx_objects read ~full_transaction_object block

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

let chain_id read =
  inspect_durable_and_decode
    read
    Durable_storage_path.chain_id
    L2_types.Chain_id.decode_le

let l2_minimum_base_fee_per_gas read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.minimum_base_fee_per_gas chain_id)
    Helpers.decode_z_le

let l2_da_fee_per_byte read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.da_fee_per_byte chain_id)
    Helpers.decode_z_le

let l2_maximum_gas_per_transaction read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.maximum_gas_per_transaction
       chain_id)
    Helpers.decode_z_le

let chain_family read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.chain_family chain_id)
    (fun x -> L2_types.Chain_family.of_string_exn (Bytes.to_string x))

let world_state read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.world_state chain_id)
    Bytes.to_string

let base_fee_per_gas read =
  let open Lwt_result_syntax in
  let* block =
    blocks_by_number
      read
      ~full_transaction_object:false
      ~number:Durable_storage_path.Block.Current
  in
  match block.baseFeePerGas with
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

let minimum_base_fee_per_gas read =
  inspect_durable_and_decode
    read
    Durable_storage_path.minimum_base_fee_per_gas
    Helpers.decode_z_le

let kernel_version read =
  inspect_durable_and_decode
    read
    Durable_storage_path.kernel_version
    Bytes.to_string

let kernel_root_hash read =
  inspect_durable_and_decode_opt
    read
    Durable_storage_path.kernel_root_hash
    Bytes.to_string

let is_multichain_enabled read =
  let open Lwt_result_syntax in
  let* bytes_opt = read Durable_storage_path.Feature_flags.multichain in
  return (Option.is_some bytes_opt)

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

let storage_version read =
  inspect_durable_and_decode_default
    ~default:0
    read
    Durable_storage_path.storage_version
    (fun bytes -> decode_number_le bytes |> un_qty |> Z.to_int)

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

let sequencer read =
  inspect_durable_and_decode
    read
    Durable_storage_path.sequencer_key
    (fun bytes -> Signature.Public_key.of_b58check_exn (String.of_bytes bytes))

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

  let chain_id () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    chain_id read

  let is_multichain_enabled () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    is_multichain_enabled read

  let l2_minimum_base_fee_per_gas chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_minimum_base_fee_per_gas read chain_id

  let l2_da_fee_per_byte chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_da_fee_per_byte read chain_id

  let l2_maximum_gas_per_transaction chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_maximum_gas_per_transaction read chain_id

  let chain_family chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    chain_family read chain_id

  let world_state chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    world_state read chain_id

  let base_fee_per_gas () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    base_fee_per_gas read

  let backlog () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    backlog read

  let minimum_base_fee_per_gas () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    minimum_base_fee_per_gas read

  let storage_version () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    storage_version read

  let kernel_version () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    kernel_version read

  let kernel_root_hash () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    kernel_root_hash read

  let storage_at address pos block =
    let open Lwt_result_syntax in
    let* read = read_with_state ~block () in
    storage_at read address pos

  let coinbase () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    coinbase read
end

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

  let nth_block_hash n =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    nth_block_hash read n

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

  let transaction_object tx_hash =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    let+ transaction_object = transaction_object read tx_hash in
    Option.map
      Transaction_object.from_store_transaction_object
      transaction_object
end
