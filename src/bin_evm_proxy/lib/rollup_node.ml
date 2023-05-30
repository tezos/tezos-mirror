(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

(* The hard limit is 4096 but it needs to add the external message tag. *)
let max_input_size = 4095

let smart_rollup_address_size = 20

type transaction =
  | Simple of string
  | NewChunked of (string * int)
  | Chunk of string

let encode_transaction ~smart_rollup_address kind =
  let data =
    match kind with
    | Simple data -> "\000" ^ data
    | NewChunked (hash, len) ->
        let number_of_chunks_bytes = u16_to_bytes len in
        "\001" ^ hash ^ number_of_chunks_bytes
    | Chunk data -> "\002" ^ data
  in
  smart_rollup_address ^ data

let make_evm_inbox_transactions tx_raw =
  let open Result_syntax in
  (* Maximum size describes the maximum size of [tx_raw] to fit
     in a simple transaction. *)
  let transaction_tag_size = 1 in
  let maximum_size =
    max_input_size - smart_rollup_address_size - transaction_tag_size
    - Ethereum_types.transaction_hash_size
  in
  let tx_hash = Tx_hash.hash_to_string tx_raw in
  if String.length tx_raw <= maximum_size then
    (* Simple transaction, fits in a single input. *)
    let tx_hash = Tx_hash.hash_to_string tx_raw in
    let tx = Simple (tx_hash ^ tx_raw) in
    return (tx_hash, [tx])
  else
    let size_per_chunk =
      max_input_size - smart_rollup_address_size - transaction_tag_size - 2
      (* Index as u16 *) - Ethereum_types.transaction_hash_size
    in
    let* chunks = String.chunk_bytes size_per_chunk (Bytes.of_string tx_raw) in
    let new_chunk_transaction = NewChunked (tx_hash, List.length chunks) in
    let chunks =
      List.mapi (fun i chunk -> Chunk (tx_hash ^ u16_to_bytes i ^ chunk)) chunks
    in
    return (tx_hash, new_chunk_transaction :: chunks)

let make_encoded_messages ~smart_rollup_address tx_raw =
  let open Result_syntax in
  let tx_raw = Ethereum_types.hash_to_bytes tx_raw in
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

(** [chunks bytes size] returns [Bytes.length bytes / size] chunks of size
    [size]. *)
let chunks bytes size =
  let n = Bytes.length bytes in
  assert (n mod size = 0) ;
  let nb = n / size in
  let rec collect i acc =
    if i = nb then acc
    else
      let chunk = Bytes.sub_string bytes (i * size) size in
      collect (i + 1) (chunk :: acc)
  in
  collect 0 [] |> List.rev

module Durable_storage_path = struct
  let accounts = "/evm/eth_accounts"

  let balance = "/balance"

  let nonce = "/nonce"

  let code = "/code"

  let account_path (Address s) = accounts ^ "/" ^ s

  let balance_path address = account_path address ^ balance

  let nonce_path address = account_path address ^ nonce

  let code_path address = account_path address ^ code

  let number = "/number"

  let hash = "/hash"

  let transactions = "/transactions"

  module Block = struct
    let blocks = "/evm/blocks"

    type number = Current | Nth of Z.t

    let number_to_string = function
      | Current -> "current"
      | Nth i -> Z.to_string i

    let block_field block_number field =
      blocks ^ "/" ^ number_to_string block_number ^ field

    let hash block_number = block_field block_number hash

    let transactions block_number = block_field block_number transactions

    let current_number = blocks ^ "/current" ^ number
  end

  module Transaction_receipt = struct
    let receipts = "/evm/transactions_receipts"

    let receipt_field tx_hash field = receipts ^ "/" ^ tx_hash ^ "/" ^ field

    let block_hash tx_hash = receipt_field tx_hash "block_hash"

    let block_number tx_hash = receipt_field tx_hash "block_number"

    let from tx_hash = receipt_field tx_hash "from"

    let to_ tx_hash = receipt_field tx_hash "to"

    let index tx_hash = receipt_field tx_hash "index"

    let status tx_hash = receipt_field tx_hash "status"

    let type_ tx_hash = receipt_field tx_hash "type"

    let contract_address tx_hash = receipt_field tx_hash "contract_address"
  end

  module Transaction_object = struct
    let objects = "/evm/transactions_objects"

    let object_field tx_hash field =
      Format.sprintf "%s/%s/%s" objects tx_hash field

    let block_hash tx_hash = object_field tx_hash "block_hash"

    let block_number tx_hash = object_field tx_hash "block_number"

    let from tx_hash = object_field tx_hash "from"

    let gas_used tx_hash = object_field tx_hash "gas_used"

    let gas_price tx_hash = object_field tx_hash "gas_price"

    let input tx_hash = object_field tx_hash "input"

    let nonce tx_hash = object_field tx_hash "nonce"

    let to_ tx_hash = object_field tx_hash "to"

    let index tx_hash = object_field tx_hash "index"

    let value tx_hash = object_field tx_hash "value"

    let v tx_hash = object_field tx_hash "v"

    let r tx_hash = object_field tx_hash "r"

    let s tx_hash = object_field tx_hash "r"
  end
end

module RPC = struct
  open Tezos_rpc
  open Path

  let smart_rollup_address =
    Service.get_service
      ~description:"Smart rollup address"
      ~query:Query.empty
      ~output:(Data_encoding.Fixed.bytes 20)
      (open_root / "global" / "smart_rollup_address")

  type state_value_query = {key : string}

  let state_value_query : state_value_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun key -> {key})
    |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
    |> seal

  let durable_state_value =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve value by key from PVM durable storage. PVM state is taken \
         with respect to the specified block level. Value returned in hex \
         format."
      ~query:state_value_query
      ~output:Data_encoding.(option bytes)
      (open_root / "global" / "block" / "head" / "durable" / "wasm_2_0_0"
     / "value")

  let batcher_injection =
    Tezos_rpc.Service.post_service
      ~description:"Inject messages in the batcher's queue"
      ~query:Tezos_rpc.Query.empty
      ~input:
        Data_encoding.(
          def "messages" ~description:"Messages to inject" (list string))
      ~output:
        Data_encoding.(
          def
            "message_hashes"
            ~description:"Hashes of injected L2 messages"
            (list string))
      (open_root / "local" / "batcher" / "injection")

  let call_service ~base ?(media_types = Media_type.all_media_types) =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_service media_types ~base

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
    let key = Durable_storage_path.balance_path address in
    let+ answer = call_service ~base durable_state_value () {key} () in
    match answer with
    | Some bytes ->
        Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z
    | None -> Ethereum_types.Qty Z.zero

  let nonce base address =
    let open Lwt_result_syntax in
    let key = Durable_storage_path.nonce_path address in
    let+ answer = call_service ~base durable_state_value () {key} () in
    match answer with
    | Some bytes ->
        Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z
    | None -> Ethereum_types.Qty Z.zero

  let code base address =
    let open Lwt_result_syntax in
    let key = Durable_storage_path.code_path address in
    let+ answer = call_service ~base durable_state_value () {key} () in
    match answer with
    | Some bytes ->
        bytes |> Hex.of_bytes |> Hex.show |> Ethereum_types.hash_of_string
    | None -> Ethereum_types.Hash ""

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
    return (Ethereum_types.Hash Hex.(of_string tx_hash |> show))

  exception Invalid_block_structure of string

  let block_number base n =
    let open Lwt_result_syntax in
    match n with
    (* This avoids an unecessary service call in case we ask a block's number
       with an already expected/known block number [n]. *)
    | Durable_storage_path.Block.Nth i ->
        return @@ Ethereum_types.Block_height i
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

  let block_hash base number =
    let open Lwt_result_syntax in
    let* (Block_height block_number) =
      match number with
      | Durable_storage_path.Block.Current -> block_number base number
      | Nth n -> return (Block_height n)
    in
    let key = Durable_storage_path.Block.hash (Nth block_number) in
    let+ hash_answer = call_service ~base durable_state_value () {key} () in
    match hash_answer with
    | Some bytes ->
        Block_hash (Bytes.to_string bytes |> Hex.of_string |> Hex.show)
    | None ->
        raise
        @@ Invalid_block_structure "Unexpected [None] value for [block.hash]"

  let current_block_number base () =
    block_number base Durable_storage_path.Block.Current

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

  let decode_block_hash bytes =
    Block_hash (Bytes.to_string bytes |> Hex.of_string |> Hex.show)

  let decode_address bytes =
    Address (Bytes.to_string bytes |> Hex.of_string |> Hex.show)

  let decode_number bytes =
    Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z

  let decode_hash bytes =
    Hash (Bytes.to_string bytes |> Hex.of_string |> Hex.show)

  let transaction_receipt base (Hash tx_hash) =
    let open Lwt_result_syntax in
    let* block_hash =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.block_hash tx_hash)
        decode_block_hash
    in
    let* block_number =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.block_number tx_hash)
        decode_number
    in
    let* from =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.from tx_hash)
        decode_address
    in
    (* This can be none *)
    let* to_ =
      inspect_durable_and_decode_opt
        base
        (Durable_storage_path.Transaction_receipt.to_ tx_hash)
        decode_address
    in
    let* index =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.index tx_hash)
        decode_number
    in
    let* status =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.status tx_hash)
        decode_number
    in
    let* contract_address =
      inspect_durable_and_decode_opt
        base
        (Durable_storage_path.Transaction_receipt.contract_address tx_hash)
        decode_address
    in
    let+ type_ =
      inspect_durable_and_decode
        base
        (Durable_storage_path.Transaction_receipt.type_ tx_hash)
        decode_number
    in
    {
      transactionHash = Hash tx_hash;
      transactionIndex = index;
      blockHash = block_hash;
      blockNumber = block_number;
      from;
      to_;
      cumulativeGasUsed = Ethereum_types.quantity_of_z Z.zero;
      effectiveGasPrice = Ethereum_types.quantity_of_z Z.zero;
      gasUsed = Ethereum_types.quantity_of_z Z.zero;
      logs = [];
      logsBloom = Hash (String.make 256 'a');
      type_;
      status;
      contractAddress = contract_address;
    }

  let transaction_object base (Hash tx_hash as hash) =
    let open Lwt_result_syntax in
    let* block_hash_opt =
      inspect_durable_and_decode_opt
        base
        (Durable_storage_path.Transaction_object.block_hash tx_hash)
        decode_block_hash
    in
    match block_hash_opt with
    | None ->
        (* If the transaction has no block hash, it was not mined. *)
        return_none
    | Some block_hash ->
        let* block_number =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.block_number tx_hash)
            decode_number
        in
        let* from =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.from tx_hash)
            decode_address
        in
        let* gas_used =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.gas_used tx_hash)
            decode_number
        in
        let* gas_price =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.gas_price tx_hash)
            decode_number
        in
        let* input =
          inspect_durable_and_decode_opt
            base
            (Durable_storage_path.Transaction_object.input tx_hash)
            decode_hash
        in
        let* nonce =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.nonce tx_hash)
            decode_number
        in
        let* to_ =
          inspect_durable_and_decode_opt
            base
            (Durable_storage_path.Transaction_object.to_ tx_hash)
            decode_address
        in
        let* index =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.index tx_hash)
            decode_number
        in
        let* value =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.value tx_hash)
            decode_number
        in
        let* v =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.v tx_hash)
            decode_number
        in
        let* r =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.r tx_hash)
            decode_hash
        in
        let* s =
          inspect_durable_and_decode
            base
            (Durable_storage_path.Transaction_object.s tx_hash)
            decode_hash
        in
        return_some
          {
            blockHash = Some block_hash;
            blockNumber = Some block_number;
            from;
            gas = gas_used;
            gasPrice = gas_price;
            hash;
            input = Option.value ~default:(Hash "") input;
            nonce;
            to_;
            transactionIndex = index;
            value;
            v;
            r;
            s;
          }

  let transactions ~full_transaction_object ~number base =
    let open Lwt_result_syntax in
    let* (Block_height block_number) = block_number base number in
    let key_transactions =
      Durable_storage_path.Block.transactions (Nth block_number)
    in
    let* transactions_answer =
      call_service ~base durable_state_value () {key = key_transactions} ()
    in
    match transactions_answer with
    | Some bytes ->
        let chunks = chunks bytes Ethereum_types.transaction_hash_size in
        if full_transaction_object then
          let+ objects =
            List.filter_map_es
              (fun bytes ->
                transaction_object base (Hash Hex.(of_string bytes |> show)))
              chunks
          in
          TxFull objects
        else
          let hashes =
            List.map (fun bytes -> Hash Hex.(of_string bytes |> show)) chunks
          in
          return (TxHash hashes)
    | None ->
        raise
        @@ Invalid_block_structure
             "Unexpected [None] value for [block.transactions]"

  let block ~full_transaction_object ~number base =
    let open Lwt_result_syntax in
    let* transactions = transactions ~full_transaction_object ~number base in
    let* (Ethereum_types.Block_height level_z as level) =
      block_number base number
    in
    let* hash = block_hash base number in
    let* parent =
      if Z.zero = level_z then
        return (Ethereum_types.Block_hash (String.make 64 'a'))
      else block_hash base (Nth (Z.pred level_z))
    in
    return
      {
        number = Some level;
        hash = Some hash;
        parent;
        nonce = Ethereum_types.Hash (String.make 16 'a');
        sha3Uncles = Ethereum_types.Hash (String.make 64 'a');
        logsBloom = Some (Ethereum_types.Hash (String.make 512 'a'));
        transactionRoot = Ethereum_types.Hash (String.make 64 'a');
        stateRoot = Ethereum_types.Hash (String.make 64 'a');
        receiptRoot = Ethereum_types.Hash (String.make 64 'a');
        (* We need the following dummy value otherwise eth-cli will complain
           that miner's address is not a valid Ethereum address. *)
        miner = Ethereum_types.Hash "6471A723296395CF1Dcc568941AFFd7A390f94CE";
        difficulty = Ethereum_types.Qty Z.zero;
        totalDifficulty = Ethereum_types.Qty Z.zero;
        extraData = "";
        size = Ethereum_types.Qty Z.zero;
        gasLimit = Ethereum_types.Qty Z.zero;
        gasUsed = Ethereum_types.Qty Z.zero;
        timestamp = Ethereum_types.Qty Z.zero;
        transactions;
        uncles = [];
      }

  let current_block base ~full_transaction_object =
    block
      ~full_transaction_object
      ~number:Durable_storage_path.Block.Current
      base

  let nth_block base ~full_transaction_object n =
    block
      ~full_transaction_object
      ~number:Durable_storage_path.Block.(Nth n)
      base

  let txpool _ () =
    Lwt.return_ok {pending = AddressMap.empty; queued = AddressMap.empty}
end

module type S = sig
  val smart_rollup_address : string tzresult Lwt.t

  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  val nonce : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  val code : Ethereum_types.address -> Ethereum_types.hash tzresult Lwt.t

  val inject_raw_transaction :
    smart_rollup_address:string -> hash -> hash tzresult Lwt.t

  val current_block :
    full_transaction_object:bool -> Ethereum_types.block tzresult Lwt.t

  val current_block_number : unit -> Ethereum_types.block_height tzresult Lwt.t

  val nth_block :
    full_transaction_object:bool -> Z.t -> Ethereum_types.block tzresult Lwt.t

  val transaction_receipt :
    Ethereum_types.hash -> Ethereum_types.transaction_receipt tzresult Lwt.t

  val transaction_object :
    Ethereum_types.hash ->
    Ethereum_types.transaction_object option tzresult Lwt.t

  val txpool : unit -> Ethereum_types.txpool tzresult Lwt.t
end

module Make (Base : sig
  val base : Uri.t
end) : S = struct
  let smart_rollup_address = RPC.smart_rollup_address Base.base

  let balance = RPC.balance Base.base

  let nonce = RPC.nonce Base.base

  let code = RPC.code Base.base

  let inject_raw_transaction = RPC.inject_raw_transaction Base.base

  let current_block = RPC.current_block Base.base

  let current_block_number = RPC.current_block_number Base.base

  let nth_block = RPC.nth_block Base.base

  let transaction_receipt = RPC.transaction_receipt Base.base

  let transaction_object = RPC.transaction_object Base.base

  let txpool = RPC.txpool Base.base
end
