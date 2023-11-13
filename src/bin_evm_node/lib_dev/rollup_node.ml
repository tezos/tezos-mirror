(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types
open Rollup_node_services

(* The hard limit is 4096 but it needs to add the external message tag. *)
let max_input_size = 4095

let smart_rollup_address_size = 20

let transaction_tag_size = 1

let framing_protocol_tag_size = 1

type transaction =
  | Simple of string
  | NewChunked of (string * int * string)
  | Chunk of string

let encode_transaction ~smart_rollup_address kind =
  let data =
    match kind with
    | Simple data -> "\000" ^ data
    | NewChunked (tx_hash, len, all_chunk_hashes) ->
        let number_of_chunks_bytes = Ethereum_types.u16_to_bytes len in
        "\001" ^ tx_hash ^ number_of_chunks_bytes ^ all_chunk_hashes
    | Chunk data -> "\002" ^ data
  in
  "\000" ^ smart_rollup_address ^ data

let chunk_transaction ~tx_hash ~tx_raw =
  let open Result_syntax in
  let size_per_chunk =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - 2 (* Index as u16 *)
    - (Ethereum_types.transaction_hash_size * 2)
  in
  let* chunks = String.chunk_bytes size_per_chunk (Bytes.of_string tx_raw) in
  let all_chunk_hashes, chunks =
    List.fold_left_i
      (fun i (all_chunk_hashes, chunks) chunk ->
        let chunk_hash = Ethereum_types.hash_raw_tx chunk in
        let all_chunk_hashes = all_chunk_hashes ^ chunk_hash in
        let chunk =
          Chunk (tx_hash ^ Ethereum_types.u16_to_bytes i ^ chunk_hash ^ chunk)
        in
        (all_chunk_hashes, chunk :: chunks))
      ("", [])
      chunks
  in
  let new_chunk_transaction =
    NewChunked (tx_hash, List.length chunks, all_chunk_hashes)
  in
  return (tx_hash, new_chunk_transaction :: chunks)

let make_evm_inbox_transactions tx_raw =
  let open Result_syntax in
  let tx_raw = Ethereum_types.hex_to_bytes tx_raw in
  (* Maximum size describes the maximum size of [tx_raw] to fit
     in a simple transaction. *)
  let maximum_size =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - Ethereum_types.transaction_hash_size
  in
  let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
  if String.length tx_raw <= maximum_size then
    (* Simple transaction, fits in a single input. *)
    let tx = Simple (tx_hash ^ tx_raw) in
    return (tx_hash, [tx])
  else chunk_transaction ~tx_hash ~tx_raw

let make_encoded_messages ~smart_rollup_address tx_raw =
  let open Result_syntax in
  let* tx_hash, messages = make_evm_inbox_transactions tx_raw in
  let messages =
    List.map
      (fun x ->
        x
        |> encode_transaction ~smart_rollup_address
        |> Hex.of_string |> Hex.show)
      messages
  in
  return (tx_hash, messages)

module Durable_storage_path = struct
  module EVM = struct
    let root = "/evm"

    let make s = root ^ s
  end

  let chain_id = EVM.make "/chain_id"

  let base_fee_per_gas = EVM.make "/base_fee_per_gas"

  let kernel_version = EVM.make "/kernel_version"

  module Accounts = struct
    let accounts = EVM.make "/eth_accounts"

    let balance = "/balance"

    let nonce = "/nonce"

    let code = "/code"

    let storage = "/storage"

    let account (Address (Hex s)) = accounts ^ "/" ^ s

    let balance address = account address ^ balance

    let nonce address = account address ^ nonce

    let code address = account address ^ code

    let storage address index = account address ^ storage ^ "/" ^ index
  end

  module Block = struct
    type number = Current | Nth of Z.t

    let blocks = EVM.make "/blocks"

    let hash = "/hash"

    let number = "/number"

    let by_hash (Block_hash (Hex hash)) = blocks ^ "/" ^ hash

    let current_number = blocks ^ "/current" ^ number

    let _current_hash = blocks ^ "/current" ^ hash
  end

  module Indexes = struct
    let indexes = EVM.make "/indexes"

    let blocks = "/blocks"

    let blocks = indexes ^ blocks

    let number_to_string = function
      | Block.Current -> "current"
      | Nth i -> Z.to_string i

    let blocks_by_number number = blocks ^ "/" ^ number_to_string number
  end

  module Transaction_receipt = struct
    let receipts = EVM.make "/transactions_receipts"

    let receipt tx_hash = receipts ^ "/" ^ tx_hash
  end

  module Transaction_object = struct
    let objects = EVM.make "/transactions_objects"

    let object_ tx_hash = objects ^ "/" ^ tx_hash
  end
end

let inspect_durable_and_decode_opt base key decode =
  let open Lwt_result_syntax in
  let* bytes = call_service ~base durable_state_value () {key} () in
  match bytes with
  | Some bytes -> return_some (decode bytes)
  | None -> return_none

let inspect_durable_and_decode base key decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt base key decode in
  match res_opt with Some res -> return res | None -> failwith "null"

let smart_rollup_address base =
  let open Lwt_result_syntax in
  let*! answer =
    call_service
      ~base
      ~media_types:[Media_type.octet_stream]
      smart_rollup_address
      ()
      ()
      ()
  in
  match answer with
  | Ok address -> return (Bytes.to_string address)
  | Error tztrace ->
      failwith
        "Failed to communicate with %a, because %a"
        Uri.pp
        base
        pp_print_trace
        tztrace

let balance base address =
  let open Lwt_result_syntax in
  let key = Durable_storage_path.Accounts.balance address in
  let+ answer = call_service ~base durable_state_value () {key} () in
  match answer with
  | Some bytes ->
      Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z
  | None -> Ethereum_types.Qty Z.zero

let nonce base address =
  let open Lwt_result_syntax in
  let key = Durable_storage_path.Accounts.nonce address in
  let+ answer = call_service ~base durable_state_value () {key} () in
  answer
  |> Option.map (fun bytes ->
         bytes |> Bytes.to_string |> Z.of_bits |> Ethereum_types.quantity_of_z)

let code base address =
  let open Lwt_result_syntax in
  let key = Durable_storage_path.Accounts.code address in
  let+ answer = call_service ~base durable_state_value () {key} () in
  match answer with
  | Some bytes ->
      bytes |> Hex.of_bytes |> Hex.show |> Ethereum_types.hex_of_string
  | None -> Ethereum_types.Hex ""

let inject_raw_transaction base tx =
  let open Lwt_result_syntax in
  (* The injection's service returns a notion of L2 message hash (defined
     by the rollup node) used to track the message's injection in the batcher.
     We do not wish to follow the message's inclusion, and thus, ignore
     the resulted hash. *)
  let* _answer = call_service ~base batcher_injection () () [tx] in
  return_unit

let inject_raw_transaction base ~smart_rollup_address tx_raw =
  let open Lwt_result_syntax in
  let*? tx_hash, messages =
    make_encoded_messages ~smart_rollup_address tx_raw
  in
  let* () = List.iter_es (inject_raw_transaction base) messages in
  return (Ethereum_types.Hash Hex.(of_string tx_hash |> show |> hex_of_string))

exception Invalid_block_structure of string

exception Invalid_block_index of Z.t

let block_number base n =
  let open Lwt_result_syntax in
  match n with
  (* This avoids an unecessary service call in case we ask a block's number
     with an already expected/known block number [n]. *)
  | Durable_storage_path.Block.Nth i -> return @@ Ethereum_types.Block_height i
  | Durable_storage_path.Block.Current -> (
      let key = Durable_storage_path.Block.current_number in
      let+ answer = call_service ~base durable_state_value () {key} () in
      match answer with
      | Some bytes ->
          Ethereum_types.Block_height (Bytes.to_string bytes |> Z.of_bits)
      | None ->
          raise
          @@ Invalid_block_structure
               "Unexpected [None] value for [current_number]'s [answer]")

let current_block_number base () =
  block_number base Durable_storage_path.Block.Current

let un_qty (Qty z) = z

let transaction_receipt base (Hash (Hex tx_hash)) =
  let open Lwt_result_syntax in
  (* We use a mock block hash to decode the rest of the receipt,
     so that we can get the number to query for the actual block
     hash. *)
  let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
  let* opt_receipt =
    inspect_durable_and_decode_opt
      base
      (Durable_storage_path.Transaction_receipt.receipt tx_hash)
      (Ethereum_types.transaction_receipt_from_rlp mock_block_hash)
  in
  match opt_receipt with
  | Some temp_receipt ->
      let+ blockHash =
        inspect_durable_and_decode
          base
          (Durable_storage_path.Indexes.blocks_by_number
             (Nth (un_qty temp_receipt.blockNumber)))
          decode_block_hash
      in
      let logs =
        List.map
          (fun (log : transaction_log) -> {log with blockHash = Some blockHash})
          temp_receipt.logs
      in
      Some {temp_receipt with blockHash; logs}
  | None -> return_none

let transaction_object base (Hash (Hex tx_hash)) =
  let open Lwt_result_syntax in
  (* We use a mock block hash to decode the rest of the receipt,
     so that we can get the number to query for the actual block
     hash. *)
  let mock_block_hash = Block_hash (Hex (String.make 64 'a')) in
  let* opt_object =
    inspect_durable_and_decode_opt
      base
      (Durable_storage_path.Transaction_object.object_ tx_hash)
      (Ethereum_types.transaction_object_from_rlp mock_block_hash)
  in
  match opt_object with
  | Some temp_object ->
      let+ blockHash =
        inspect_durable_and_decode
          base
          (Durable_storage_path.Indexes.blocks_by_number
             (Nth (un_qty temp_object.blockNumber)))
          decode_block_hash
      in
      Some {temp_object with blockHash}
  | None -> return_none

let transaction_object_with_block_hash base block_hash (Hash (Hex tx_hash)) =
  inspect_durable_and_decode_opt
    base
    (Durable_storage_path.Transaction_object.object_ tx_hash)
    (Ethereum_types.transaction_object_from_rlp block_hash)

let full_transactions block_hash transactions base =
  let open Lwt_result_syntax in
  match transactions with
  | TxHash hashes ->
      let+ objects =
        List.filter_map_es
          (transaction_object_with_block_hash base block_hash)
          hashes
      in
      TxFull objects
  | TxFull _ -> return transactions

let populate_tx_objects ~full_transaction_object base block =
  let open Lwt_result_syntax in
  if full_transaction_object then
    let* transactions = full_transactions block.hash block.transactions base in
    return {block with transactions}
  else return block

let blocks_by_number ~full_transaction_object ~number base =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Block_height level) = block_number base number in
  let* block_hash_opt =
    inspect_durable_and_decode_opt
      base
      (Durable_storage_path.Indexes.blocks_by_number (Nth level))
      decode_block_hash
  in
  match block_hash_opt with
  | None -> raise @@ Invalid_block_index level
  | Some block_hash -> (
      let* block_opt =
        inspect_durable_and_decode_opt
          base
          (Durable_storage_path.Block.by_hash block_hash)
          Ethereum_types.block_from_rlp
      in
      match block_opt with
      | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
      | Some block -> populate_tx_objects ~full_transaction_object base block)

let current_block base ~full_transaction_object =
  blocks_by_number
    ~full_transaction_object
    ~number:Durable_storage_path.Block.Current
    base

let nth_block base ~full_transaction_object n =
  blocks_by_number
    ~full_transaction_object
    ~number:Durable_storage_path.Block.(Nth n)
    base

let block_by_hash base ~full_transaction_object block_hash =
  let open Lwt_result_syntax in
  let* block_opt =
    inspect_durable_and_decode_opt
      base
      (Durable_storage_path.Block.by_hash block_hash)
      Ethereum_types.block_from_rlp
  in
  match block_opt with
  | None -> raise @@ Invalid_block_structure "Couldn't decode bytes"
  | Some block -> populate_tx_objects ~full_transaction_object base block

let txpool _ () =
  Lwt.return_ok {pending = AddressMap.empty; queued = AddressMap.empty}

let chain_id base () =
  inspect_durable_and_decode base Durable_storage_path.chain_id decode_number

let base_fee_per_gas base () =
  inspect_durable_and_decode
    base
    Durable_storage_path.base_fee_per_gas
    decode_number

let kernel_version base () =
  inspect_durable_and_decode
    base
    Durable_storage_path.kernel_version
    Bytes.to_string

let simulate_call base call =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode call in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5900
         for now the status is not used but it should be for error handling *)
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "simulate_call";
      }
  in
  Simulation.call_result r

let estimate_gas base call =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode call in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_gas"];
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5900
         for now the status is not used but it should be for error handling *)
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "estimate_gas";
      }
  in
  Simulation.gas_estimation r

let is_tx_valid base (Hex tx_raw) =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode_tx tx_raw in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "tx_validity";
      }
  in
  Simulation.is_tx_valid r

let storage_at base address (Qty pos) =
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
  let key = Durable_storage_path.Accounts.storage address index in
  let+ answer = call_service ~base durable_state_value () {key} () in
  match answer with
  | Some bytes ->
      Bytes.to_string bytes |> Hex.of_string |> Hex.show
      |> Ethereum_types.hex_of_string
  | None -> Ethereum_types.Hex (pad32left0 "0")

module type S = sig
  val smart_rollup_address : string tzresult Lwt.t

  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  val nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t

  val code : Ethereum_types.address -> Ethereum_types.hex tzresult Lwt.t

  val inject_raw_transaction :
    smart_rollup_address:string -> hex -> hash tzresult Lwt.t

  val current_block :
    full_transaction_object:bool -> Ethereum_types.block tzresult Lwt.t

  val current_block_number : unit -> Ethereum_types.block_height tzresult Lwt.t

  val nth_block :
    full_transaction_object:bool -> Z.t -> Ethereum_types.block tzresult Lwt.t

  val block_by_hash :
    full_transaction_object:bool ->
    Ethereum_types.block_hash ->
    Ethereum_types.block tzresult Lwt.t

  val transaction_receipt :
    Ethereum_types.hash ->
    Ethereum_types.transaction_receipt option tzresult Lwt.t

  val transaction_object :
    Ethereum_types.hash ->
    Ethereum_types.transaction_object option tzresult Lwt.t

  val txpool : unit -> Ethereum_types.txpool tzresult Lwt.t

  val chain_id : unit -> Ethereum_types.quantity tzresult Lwt.t

  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  val kernel_version : unit -> string tzresult Lwt.t

  val simulate_call : Ethereum_types.call -> Ethereum_types.hash tzresult Lwt.t

  val estimate_gas :
    Ethereum_types.call -> Ethereum_types.quantity tzresult Lwt.t

  val is_tx_valid :
    Ethereum_types.hex -> (Ethereum_types.address, string) result tzresult Lwt.t

  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.hex tzresult Lwt.t
end

module Make (Base : sig
  val base : Uri.t
end) : S = struct
  let smart_rollup_address = smart_rollup_address Base.base

  let balance = balance Base.base

  let nonce = nonce Base.base

  let code = code Base.base

  let inject_raw_transaction = inject_raw_transaction Base.base

  let current_block = current_block Base.base

  let current_block_number = current_block_number Base.base

  let nth_block = nth_block Base.base

  let block_by_hash = block_by_hash Base.base

  let transaction_receipt = transaction_receipt Base.base

  let transaction_object = transaction_object Base.base

  let txpool = txpool Base.base

  let chain_id = chain_id Base.base

  let base_fee_per_gas = base_fee_per_gas Base.base

  let kernel_version = kernel_version Base.base

  let simulate_call = simulate_call Base.base

  let estimate_gas = estimate_gas Base.base

  let is_tx_valid = is_tx_valid Base.base

  let storage_at = storage_at Base.base
end
