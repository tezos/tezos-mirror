(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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

let transaction_hash_size = 32

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

type hex = Hex of string [@@ocaml.unboxed]

(** Appends the [0x] prefix to a string. *)
let hex_to_string (Hex s) = "0x" ^ s

let hex_of_string s =
  if String.starts_with ~prefix:"0x" s then
    let n = String.length s in
    Hex (String.sub s 2 (n - 2))
  else Hex s

let hex_to_bytes (Hex h) = Hex.to_bytes_exn (`Hex h) |> Bytes.to_string

let hex_encoding = Data_encoding.(conv hex_to_string hex_of_string string)

type address = Address of hex [@@ocaml.unboxed]

let address_of_string s = Address (hex_of_string (String.lowercase_ascii s))

let address_to_string (Address a) = hex_to_string a

let address_encoding =
  Data_encoding.(conv address_to_string address_of_string string)

let timestamp_to_bytes timestamp =
  let seconds = Time.Protocol.to_seconds timestamp in
  let buffer = Bytes.make 8 '\000' in
  Bytes.set_int64_le buffer 0 seconds ;
  buffer

let timestamp_of_bytes timestamp_bytes =
  let timestamp_64 = Bytes.get_int64_le timestamp_bytes 0 in
  Time.Protocol.of_seconds timestamp_64

type quantity = Qty of Z.t [@@ocaml.unboxed]

module Qty = struct
  let pred (Qty z) = Qty (Z.pred z)

  let next (Qty z) = Qty (Z.add z Z.one)

  let to_z (Qty z) = z

  let zero = Qty Z.zero
end

let quantity_of_z z = Qty z

let z_to_hexa = Z.format "#x"

let quantity_encoding =
  Data_encoding.conv
    (fun (Qty q) -> z_to_hexa q)
    (fun q -> Qty (Z.of_string q))
    Data_encoding.string

let pp_quantity fmt (Qty q) = Z.pp_print fmt q

type block_hash = Block_hash of hex [@@ocaml.unboxed]

let pp_block_hash fmt (Block_hash (Hex h)) = Format.pp_print_string fmt h

let block_hash_of_string s = Block_hash (hex_of_string s)

let block_hash_encoding =
  Data_encoding.(
    conv (fun (Block_hash h) -> hex_to_string h) block_hash_of_string string)

let block_hash_to_bytes (Block_hash h) = hex_to_bytes h

let genesis_parent_hash = Block_hash (Hex (String.make 64 'f'))

module Block_parameter = struct
  type t = Number of quantity | Earliest | Latest | Pending | Finalized

  let pp fmt = function
    | Number quantity -> pp_quantity fmt quantity
    | Earliest -> Format.pp_print_string fmt "earliest"
    | Latest -> Format.pp_print_string fmt "latest"
    | Pending -> Format.pp_print_string fmt "pending"
    | Finalized -> Format.pp_print_string fmt "finalized"

  let encoding =
    let open Data_encoding in
    union
      [
        (let tag = "hex" in
         case
           ~title:tag
           (Tag 0)
           quantity_encoding
           (function Number n -> Some n | _ -> None)
           (fun n -> Number n));
        (let tag = "earliest" in
         case
           ~title:tag
           (Tag 1)
           (constant tag)
           (function Earliest -> Some () | _ -> None)
           (fun () -> Earliest));
        (let tag = "latest" in
         case
           ~title:tag
           (Tag 2)
           (constant tag)
           (function Latest -> Some () | _ -> None)
           (fun () -> Latest));
        (let tag = "pending" in
         case
           ~title:tag
           (Tag 3)
           (constant tag)
           (function Pending -> Some () | _ -> None)
           (fun () -> Pending));
        (let tag = "finalized" in
         case
           ~title:tag
           (Tag 4)
           (constant tag)
           (function Finalized -> Some () | _ -> None)
           (fun () -> Finalized));
      ]

  type extended =
    | Block_parameter of t
    | Block_hash of {hash : block_hash; require_canonical : bool}

  let pp_extended fmt = function
    | Block_parameter param -> pp fmt param
    | Block_hash {hash; _} -> pp_block_hash fmt hash

  let extended_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"block_parameter"
          (Tag 0)
          encoding
          (function
            | Block_parameter block_param -> Some block_param | _ -> None)
          (fun block_param -> Block_parameter block_param);
        case
          ~title:"block_parameter_hash"
          (Tag 1)
          (obj2
             (req "blockHash" block_hash_encoding)
             (dft "requireCanonical" bool false))
          (function
            | Block_hash {hash; require_canonical} ->
                Some (hash, require_canonical)
            | _ -> None)
          (fun (hash, require_canonical) ->
            Block_hash {hash; require_canonical});
        case
          ~title:"block_parameter_number"
          (Tag 2)
          (obj2
             (req "blockNumber" quantity_encoding)
             (dft "requireCanonical" bool false))
          (function
            | Block_parameter (Number number) -> Some (number, false)
            | _ -> None)
          (fun (number, _require_canonical) -> Block_parameter (Number number));
      ]
end

type hash = Hash of hex [@@ocaml.unboxed]

let hash_of_string s = Hash (hex_of_string s)

let hash_to_string (Hash h) = hex_to_string h

let hash_to_bytes (Hash h) = hex_to_bytes h

let hash_encoding = Data_encoding.(conv hash_to_string hash_of_string string)

let pp_hash fmt (Hash (Hex h)) = Format.pp_print_string fmt h

let pp_block_hash fmt (Block_hash (Hex h)) = Format.pp_print_string fmt h

let decode_hex bytes = Hex Hex.(of_bytes bytes |> show)

let encode_hex (Hex hex) = Hex.to_bytes_exn (`Hex hex)

let decode_block_hash bytes = Block_hash (decode_hex bytes)

let decode_address bytes = Address (decode_hex bytes)

let encode_address (Address address) = encode_hex address

let decode_number_le bytes = Bytes.to_string bytes |> Z.of_bits |> quantity_of_z

let decode_number_be bytes =
  Bytes.fold_left
    (fun acc c ->
      let open Z in
      add (of_int (Char.code c)) (shift_left acc 8))
    Z.zero
    bytes
  |> quantity_of_z

let decode_hash bytes = Hash (decode_hex bytes)

let pad_to_n_bytes_le bytes length =
  let current_length = Bytes.length bytes in
  if current_length >= length then bytes
  else
    let padding_length = length - current_length in
    let padding = Bytes.make padding_length '\x00' in
    Bytes.cat bytes padding

let encode_u256_le (Qty n) =
  let bits = Z.to_bits n |> Bytes.of_string in
  pad_to_n_bytes_le bits 32

type transaction_log = {
  address : address;
  topics : hash list;
  data : hex;
  blockNumber : quantity option;
  transactionHash : hash option;
  transactionIndex : quantity option;
  blockHash : block_hash option;
  logIndex : quantity option;
  removed : bool option;
}

let transaction_log_body_from_rlp = function
  | Rlp.List [List [Value address; List topics; Value data]; Value index] ->
      ( decode_address address,
        List.map
          (function
            | Rlp.Value bytes -> decode_hash bytes
            | _ -> raise (Invalid_argument "Expected hash representing topic"))
          topics,
        decode_hex data,
        decode_number_le index )
  | _ ->
      raise
        (Invalid_argument
           "Expected list of 2 elements representing an indexed log body")

let transaction_log_encoding =
  let open Data_encoding in
  conv
    (fun {
           address;
           topics;
           data;
           blockNumber;
           transactionHash;
           transactionIndex;
           blockHash;
           logIndex;
           removed;
         } ->
      ( address,
        topics,
        data,
        blockNumber,
        transactionHash,
        transactionIndex,
        blockHash,
        logIndex,
        removed ))
    (fun ( address,
           topics,
           data,
           blockNumber,
           transactionHash,
           transactionIndex,
           blockHash,
           logIndex,
           removed ) ->
      {
        address;
        topics;
        data;
        blockNumber;
        transactionHash;
        transactionIndex;
        blockHash;
        logIndex;
        removed;
      })
    (obj9
       (req "address" address_encoding)
       (req "topics" (list hash_encoding))
       (req "data" hex_encoding)
       (req "blockNumber" (option quantity_encoding))
       (req "transactionHash" (option hash_encoding))
       (req "transactionIndex" (option quantity_encoding))
       (req "blockHash" (option block_hash_encoding))
       (req "logIndex" (option quantity_encoding))
       (req "removed" (option bool)))

type transaction_receipt = {
  transactionHash : hash;
  transactionIndex : quantity;
  blockHash : block_hash;
  blockNumber : quantity;
  from : address;
  to_ : address option;
  cumulativeGasUsed : quantity;
  effectiveGasPrice : quantity;
  gasUsed : quantity;
  logs : transaction_log list;
  logsBloom : hex;
  type_ : quantity;
  status : quantity;
  contractAddress : address option;
}

let transaction_receipt_from_rlp block_hash bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value hash;
          Value index;
          Value block_number;
          Value from;
          Value to_;
          Value cumulative_gas_used;
          Value effective_gas_price;
          Value gas_used;
          Value contract_address;
          List logs;
          Value bloom;
          Value type_;
          Value status;
        ]) ->
      let hash = decode_hash hash in
      let index = decode_number_le index in
      let block_number = decode_number_le block_number in
      let from = decode_address from in
      let to_ = if to_ = Bytes.empty then None else Some (decode_address to_) in
      let cumulative_gas_used = decode_number_le cumulative_gas_used in
      let effective_gas_price = decode_number_le effective_gas_price in
      let gas_used = decode_number_le gas_used in
      let contract_address =
        if contract_address = Bytes.empty then None
        else Some (decode_address contract_address)
      in
      let logs_body = List.map transaction_log_body_from_rlp logs in
      let logs_objects =
        List.map
          (fun (address, topics, data, logIndex) ->
            {
              address;
              topics;
              data;
              blockHash = Some block_hash;
              blockNumber = Some block_number;
              transactionHash = Some hash;
              transactionIndex = Some index;
              logIndex = Some logIndex;
              removed = Some false;
            })
          logs_body
      in
      let bloom = decode_hex bloom in
      let type_ = decode_number_le type_ in
      let status = decode_number_le status in
      {
        transactionHash = hash;
        transactionIndex = index;
        blockHash = block_hash;
        blockNumber = block_number;
        from;
        to_;
        cumulativeGasUsed = cumulative_gas_used;
        effectiveGasPrice = effective_gas_price;
        gasUsed = gas_used;
        logs = logs_objects;
        logsBloom = bloom;
        type_;
        status;
        contractAddress = contract_address;
      }
  | _ ->
      raise
        (Invalid_argument
           "Expected a RlpList of 13 elements in transaction receipt")

let transaction_receipt_encoding =
  let open Data_encoding in
  conv
    (fun {
           transactionHash;
           transactionIndex;
           blockHash;
           blockNumber;
           from;
           to_;
           cumulativeGasUsed;
           effectiveGasPrice;
           gasUsed;
           logs;
           logsBloom;
           type_;
           status;
           contractAddress;
         } ->
      ( ( transactionHash,
          transactionIndex,
          blockHash,
          blockNumber,
          from,
          to_,
          cumulativeGasUsed,
          effectiveGasPrice,
          gasUsed,
          logs ),
        (logsBloom, type_, status, contractAddress) ))
    (fun ( ( transactionHash,
             transactionIndex,
             blockHash,
             blockNumber,
             from,
             to_,
             cumulativeGasUsed,
             effectiveGasPrice,
             gasUsed,
             logs ),
           (logsBloom, type_, status, contractAddress) ) ->
      {
        transactionHash;
        transactionIndex;
        blockHash;
        blockNumber;
        from;
        to_;
        cumulativeGasUsed;
        effectiveGasPrice;
        gasUsed;
        logs;
        logsBloom;
        type_;
        status;
        contractAddress;
      })
    (merge_objs
       (obj10
          (req "transactionHash" hash_encoding)
          (req "transactionIndex" quantity_encoding)
          (req "blockHash" block_hash_encoding)
          (req "blockNumber" quantity_encoding)
          (req "from" address_encoding)
          (req "to" (option address_encoding))
          (req "cumulativeGasUsed" quantity_encoding)
          (req "effectiveGasPrice" quantity_encoding)
          (req "gasUsed" quantity_encoding)
          (req "logs" (list transaction_log_encoding)))
       (obj4
          (req "logsBloom" hex_encoding)
          (req "type" quantity_encoding)
          (req "status" quantity_encoding)
          (req "contractAddress" (option address_encoding))))

type transaction_object = {
  blockHash : block_hash option;
  blockNumber : quantity option;
  from : address;
  gas : quantity;
  gasPrice : quantity;
  hash : hash;
  input : hash;
  nonce : quantity;
  to_ : address option;
  transactionIndex : quantity option;
      (* It can be null if it's in a pending block. *)
  value : quantity;
  v : quantity;
  r : hash;
  s : hash;
}

let transaction_object_from_rlp_item block_hash rlp_item =
  let decode_optional_number bytes =
    if block_hash == None then None else Some (decode_number_le bytes)
  in

  match rlp_item with
  | Rlp.List
      [
        Value block_number;
        Value from;
        Value gas_used;
        Value gas_price;
        Value hash;
        Value input;
        Value nonce;
        Value to_;
        Value index;
        Value value;
        Value v;
        Value r;
        Value s;
      ] ->
      let block_number = decode_optional_number block_number in
      let from = decode_address from in
      let gas = decode_number_le gas_used in
      let gas_price = decode_number_le gas_price in
      let hash = decode_hash hash in
      let input = decode_hash input in
      let nonce = decode_number_le nonce in
      let to_ = if to_ = Bytes.empty then None else Some (decode_address to_) in
      let index = decode_optional_number index in
      let value = decode_number_le value in
      (* The signature is taken from the raw transaction, that is encoded in big
         endian. *)
      let v = decode_number_be v in
      let r = decode_hash r in
      let s = decode_hash s in
      {
        blockHash = block_hash;
        blockNumber = block_number;
        from;
        gas;
        gasPrice = gas_price;
        hash;
        input;
        nonce;
        to_;
        transactionIndex = index;
        value;
        v;
        r;
        s;
      }
  | _ -> raise (Invalid_argument "Expected a List of 13 elements")

let transaction_object_from_rlp block_hash bytes =
  match Rlp.decode bytes with
  | Ok rlp_item -> transaction_object_from_rlp_item block_hash rlp_item
  | _ -> raise (Invalid_argument "Expected a List of 13 elements")

let transaction_object_encoding =
  let open Data_encoding in
  conv
    (fun {
           blockHash;
           blockNumber;
           from;
           gas;
           gasPrice;
           hash;
           input;
           nonce;
           to_;
           transactionIndex;
           value;
           v;
           r;
           s;
         } ->
      ( ( blockHash,
          blockNumber,
          from,
          gas,
          gasPrice,
          hash,
          input,
          nonce,
          to_,
          transactionIndex ),
        (value, v, r, s) ))
    (fun ( ( blockHash,
             blockNumber,
             from,
             gas,
             gasPrice,
             hash,
             input,
             nonce,
             to_,
             transactionIndex ),
           (value, v, r, s) ) ->
      {
        blockHash;
        blockNumber;
        from;
        gas;
        gasPrice;
        hash;
        input;
        nonce;
        to_;
        transactionIndex;
        value;
        v;
        r;
        s;
      })
    (merge_objs
       (obj10
          (req "blockHash" (option block_hash_encoding))
          (req "blockNumber" (option quantity_encoding))
          (req "from" address_encoding)
          (req "gas" quantity_encoding)
          (req "gasPrice" quantity_encoding)
          (req "hash" hash_encoding)
          (req "input" hash_encoding)
          (req "nonce" quantity_encoding)
          (req "to" (option address_encoding))
          (req "transactionIndex" (option quantity_encoding)))
       (obj4
          (req "value" quantity_encoding)
          (req "v" quantity_encoding)
          (req "r" hash_encoding)
          (req "s" hash_encoding)))

type block_transactions =
  | TxHash of hash list
  | TxFull of transaction_object list

let block_transactions_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"hash"
        (Tag 0)
        (list hash_encoding)
        (function TxHash hashes -> Some hashes | _ -> None)
        (fun hashes -> TxHash hashes);
      case
        ~title:"full"
        (Tag 1)
        (list transaction_object_encoding)
        (function TxFull txs -> Some txs | _ -> None)
        (fun txs -> TxFull txs);
    ]

type block = {
  number : quantity;
  hash : block_hash;
  parent : block_hash;
  nonce : hex;
  sha3Uncles : hash;
  logsBloom : hex;
  transactionRoot : hash;
  stateRoot : hash;
  receiptRoot : hash;
  miner : hex;
  difficulty : quantity;
  totalDifficulty : quantity;
  extraData : hex;
  size : quantity;
  gasLimit : quantity;
  gasUsed : quantity;
  timestamp : quantity;
  transactions : block_transactions;
  uncles : hash list;
  (* baseFeePerGas and prevRandao are set optionnal because old blocks didn't have
     them*)
  baseFeePerGas : quantity option;
  prevRandao : block_hash option;
}

let decode_list decoder list =
  List.map
    Rlp.(
      function
      | Value r -> decoder r
      | List _ -> raise (Invalid_argument "Expected a list of atomic data"))
    list

let decode_option ~default decoder bytes =
  (if bytes = Bytes.empty then None else Some (decoder bytes))
  |> Option.value ~default

let block_from_rlp_v0 bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value number;
          Value hash;
          Value parent_hash;
          Value logsBloom;
          Value transactionRoot;
          Value stateRoot;
          Value receiptRoot;
          Value miner;
          Value extraData;
          Value gasLimit;
          List transactions;
          Value gasUsed;
          Value timestamp;
        ]) ->
      let (Qty number) = decode_number_le number in
      let hash = decode_block_hash hash in
      let parent = decode_block_hash parent_hash in
      let logsBloom =
        decode_option ~default:(Hex (String.make 512 'a')) decode_hex logsBloom
      in
      (* Post merge: this field is now used for the "fee recipient". We don't
         have that, potentially this could be the sequencer. *)
      let miner =
        decode_option
          ~default:(Hex "0000000000000000000000000000000000000000")
          decode_hex
          miner
      in
      let transactionRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          transactionRoot
      in
      let stateRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          stateRoot
      in
      let receiptRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          receiptRoot
      in
      let extraData = decode_option ~default:(Hex "") decode_hex extraData in
      let gasLimit =
        decode_option ~default:(Qty Z.zero) decode_number_le gasLimit
      in
      let transactions = TxHash (decode_list decode_hash transactions) in
      let gasUsed = decode_number_le gasUsed in
      let timestamp = decode_number_le timestamp in
      {
        number = Qty number;
        hash;
        parent;
        (* Post merge: always 0. *)
        nonce = Hex "0000000000000000";
        (* Post merge: uncles are always empty, therefore this is the "empty"
           hash of these uncles. *)
        sha3Uncles =
          Hash
            (Hex
               "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347");
        logsBloom;
        transactionRoot;
        stateRoot;
        receiptRoot;
        miner;
        (* Post merge: always zero. *)
        difficulty = Qty Z.zero;
        (* Post merge: sum of difficulty will always be zero because difficulty
           has and will always be zero. *)
        totalDifficulty = Qty Z.zero;
        extraData;
        size = Qty (Z.of_int (Bytes.length bytes));
        gasLimit;
        gasUsed;
        timestamp;
        transactions;
        (* Post merge: always empty. *)
        uncles = [];
        baseFeePerGas = None;
        prevRandao = None;
      }
  | _ -> raise (Invalid_argument "Expected a List of 13 elements")

let block_from_rlp_v1 bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value number;
          Value hash;
          Value parent_hash;
          Value logsBloom;
          Value transactionRoot;
          Value stateRoot;
          Value receiptRoot;
          Value miner;
          Value extraData;
          Value gasLimit;
          List transactions;
          Value gasUsed;
          Value timestamp;
          Value baseFeePerGas;
          Value prevRandao;
        ]) ->
      let (Qty number) = decode_number_le number in
      let hash = decode_block_hash hash in
      let parent = decode_block_hash parent_hash in
      let logsBloom =
        decode_option ~default:(Hex (String.make 512 'a')) decode_hex logsBloom
      in
      (* Post merge: this field is now used for the "fee recipient". We don't
         have that, potentially this could be the sequencer. *)
      let miner =
        decode_option
          ~default:(Hex "0000000000000000000000000000000000000000")
          decode_hex
          miner
      in
      let transactionRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          transactionRoot
      in
      let stateRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          stateRoot
      in
      let receiptRoot =
        decode_option
          ~default:(Hash (Hex (String.make 64 'a')))
          decode_hash
          receiptRoot
      in
      let extraData = decode_option ~default:(Hex "") decode_hex extraData in
      let gasLimit =
        decode_option ~default:(Qty Z.zero) decode_number_le gasLimit
      in
      let transactions = TxHash (decode_list decode_hash transactions) in
      let gasUsed = decode_number_le gasUsed in
      let timestamp = decode_number_le timestamp in
      let baseFeePerGas = Some (decode_number_le baseFeePerGas) in
      let prevRandao = Some (decode_block_hash prevRandao) in
      {
        number = Qty number;
        hash;
        parent;
        (* Post merge: always 0. *)
        nonce = Hex "0000000000000000";
        (* Post merge: uncles are always empty, therefore this is the "empty"
           hash of these uncles. *)
        sha3Uncles =
          Hash
            (Hex
               "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347");
        logsBloom;
        transactionRoot;
        stateRoot;
        receiptRoot;
        miner;
        (* Post merge: always zero. *)
        difficulty = Qty Z.zero;
        (* Post merge: sum of difficulty will always be zero because difficulty
           has and will always be zero. *)
        totalDifficulty = Qty Z.zero;
        extraData;
        size = Qty (Z.of_int (Bytes.length bytes));
        gasLimit;
        gasUsed;
        timestamp;
        transactions;
        (* Post merge: always empty. *)
        uncles = [];
        baseFeePerGas;
        prevRandao;
      }
  | _ -> raise (Invalid_argument "Expected a List of 15 elements")

let block_from_rlp bytes =
  let first_byte = Bytes.get bytes 0 in
  if first_byte = Char.chr 1 then
    let length = Bytes.length bytes in
    block_from_rlp_v1 (Bytes.sub bytes 1 (length - 1))
  else block_from_rlp_v0 bytes

let block_encoding =
  let open Data_encoding in
  conv
    (fun {
           number;
           hash;
           parent;
           nonce;
           sha3Uncles;
           logsBloom;
           transactionRoot;
           stateRoot;
           receiptRoot;
           miner;
           difficulty;
           totalDifficulty;
           extraData;
           size;
           gasLimit;
           gasUsed;
           timestamp;
           transactions;
           uncles;
           baseFeePerGas;
           prevRandao;
         } ->
      ( ( ( number,
            hash,
            parent,
            nonce,
            sha3Uncles,
            logsBloom,
            transactionRoot,
            stateRoot,
            receiptRoot,
            miner ),
          ( difficulty,
            totalDifficulty,
            extraData,
            size,
            gasLimit,
            gasUsed,
            timestamp,
            transactions,
            uncles,
            baseFeePerGas ) ),
        prevRandao ))
    (fun ( ( ( number,
               hash,
               parent,
               nonce,
               sha3Uncles,
               logsBloom,
               transactionRoot,
               stateRoot,
               receiptRoot,
               miner ),
             ( difficulty,
               totalDifficulty,
               extraData,
               size,
               gasLimit,
               gasUsed,
               timestamp,
               transactions,
               uncles,
               baseFeePerGas ) ),
           prevRandao ) ->
      {
        number;
        hash;
        parent;
        nonce;
        sha3Uncles;
        logsBloom;
        transactionRoot;
        stateRoot;
        receiptRoot;
        miner;
        difficulty;
        totalDifficulty;
        extraData;
        size;
        gasLimit;
        gasUsed;
        baseFeePerGas;
        timestamp;
        transactions;
        uncles;
        prevRandao;
      })
    (merge_objs
       (merge_objs
          (obj10
             (req "number" quantity_encoding)
             (req "hash" block_hash_encoding)
             (req "parentHash" block_hash_encoding)
             (req "nonce" hex_encoding)
             (req "sha3Uncles" hash_encoding)
             (req "logsBloom" hex_encoding)
             (req "transactionsRoot" hash_encoding)
             (req "stateRoot" hash_encoding)
             (req "receiptsRoot" hash_encoding)
             (req "miner" hex_encoding))
          (obj10
             (req "difficulty" quantity_encoding)
             (req "totalDifficulty" quantity_encoding)
             (req "extraData" hex_encoding)
             (req "size" quantity_encoding)
             (req "gasLimit" quantity_encoding)
             (req "gasUsed" quantity_encoding)
             (req "timestamp" quantity_encoding)
             (req "transactions" block_transactions_encoding)
             (req "uncles" (list hash_encoding))
             (opt "baseFeePerGas" quantity_encoding)))
       (obj1 (opt "prevRandao" block_hash_encoding)))

type call = {
  from : address option;
  to_ : address option;
  gas : quantity option;
  gasPrice : quantity option;
  value : quantity option;
  data : hash option;
}

let call_extendable_encoding =
  let open Data_encoding in
  (* `merge_objs <obj> unit` allows the encoding to accept any number of
      unspecified fields from JSON. *)
  merge_objs
    (conv_with_guard
       (fun {from; to_; gas; gasPrice; value; data} ->
         (from, to_, gas, gasPrice, value, data, None))
       (function
         | from, to_, gas, gasPrice, value, data, None
         | from, to_, gas, gasPrice, value, None, data ->
             Ok {from; to_; gas; gasPrice; value; data}
         | _, _, _, _, _, Some _, Some _ ->
             Error "Cannot specify both data and input")
       (obj7
          (dft "from" (option address_encoding) None)
          (dft "to" (option address_encoding) None)
          (* `call` is also used for estimateGas, which allows all fields to be
             empty, hence `to` can be `null` or absent. *)
          (dft "gas" (option quantity_encoding) None)
          (dft "gasPrice" (option quantity_encoding) None)
          (dft "value" (option quantity_encoding) None)
          (dft "input" (option hash_encoding) None)
          (dft "data" (option hash_encoding) None)))
    unit

let call_encoding =
  Data_encoding.conv
    (fun call -> (call, ()))
    (fun (call, ()) -> call)
    call_extendable_encoding

(** The txpool encoding can be found in
    https://geth.ethereum.org/docs/interacting-with-geth/rpc/ns-txpool#txpool-content.

    Basically, `txpool_content` is a map associating addresses to counters and
    transactions. In JSON, it is encoded as an object associating addresses as
    fields to objects that contain counters as field and transaction objects as
    values. I.e., the `txpool_` encodes it as:

    ```
    {@js[

    { "address1" :
      { "counter1" : <transaction object of counter 1>,
        "counter2" : <transaction object of counter 2>,
        ...
      },
      "address2" :
      { "counter1" : <transaction object of counter 1>,
        "counter2" : <transaction object of counter 2>,
        ...
      },
      ...
    }
    ]}
    ```

    As such, the encoding uses Ezjsonm representation directly to encode and
    decode the txpool.
*)
module MapMake (Key : sig
  include Stdlib.Map.OrderedType

  val to_string : t -> string

  val of_string : string -> t
end) : sig
  include Map.S with type key = Key.t

  val associative_array_encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end = struct
  module Instance = Map.Make (Key)

  let associative_array_encoding value_encoding =
    let open Data_encoding in
    conv
      (fun map ->
        let bindings = Instance.bindings map in
        let fields =
          List.map
            (fun (name, value) ->
              (Key.to_string name, Json.construct value_encoding value))
            bindings
        in
        `O fields)
      (function
        | `O fields ->
            let bindings =
              List.filter_map
                (fun (name, value) ->
                  try
                    Some (Key.of_string name, Json.destruct value_encoding value)
                  with _ -> None)
                fields
              |> List.to_seq
            in
            Instance.of_seq bindings
        | _ -> Instance.empty)
      Json.encoding

  include Instance
end

module NonceMap = MapMake (Z)

module Address = struct
  type t = address

  let compare (Address (Hex h)) (Address (Hex h')) = String.compare h h'

  let to_string = address_to_string

  let of_string = address_of_string
end

module AddressMap = MapMake (Address)

type txpool = {
  pending : transaction_object NonceMap.t AddressMap.t;
  queued : transaction_object NonceMap.t AddressMap.t;
}

let txpool_encoding =
  let open Data_encoding in
  let field_encoding =
    AddressMap.associative_array_encoding
      (NonceMap.associative_array_encoding transaction_object_encoding)
  in
  conv
    (fun {pending; queued} -> (pending, queued))
    (fun (pending, queued) -> {pending; queued})
    (obj2 (req "pending" field_encoding) (req "queued" field_encoding))

type filter_topic = One of hash | Or of hash list

let filter_topic_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"one"
        (Tag 0)
        hash_encoding
        (function One hash -> Some hash | _ -> None)
        (fun hash -> One hash);
      case
        ~title:"or"
        (Tag 1)
        (list hash_encoding)
        (function Or l -> Some l | _ -> None)
        (fun l -> Or l);
    ]

type filter_address = Single of address | Vec of address list

let filter_address_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"single"
        (Tag 0)
        address_encoding
        (function Single address -> Some address | _ -> None)
        (fun address -> Single address);
      case
        ~title:"vec"
        (Tag 1)
        (list address_encoding)
        (function Vec l -> Some l | _ -> None)
        (fun l -> Vec l);
    ]

type filter = {
  from_block : Block_parameter.t option;
  to_block : Block_parameter.t option;
  address : filter_address option;
  topics : filter_topic option list option;
  block_hash : block_hash option;
}

let filter_encoding =
  let open Data_encoding in
  conv
    (fun {from_block; to_block; address; topics; block_hash} ->
      (from_block, to_block, address, topics, block_hash))
    (fun (from_block, to_block, address, topics, block_hash) ->
      {from_block; to_block; address; topics; block_hash})
    (obj5
       (opt "fromBlock" Block_parameter.encoding)
       (opt "toBlock" Block_parameter.encoding)
       (opt "address" filter_address_encoding)
       (opt "topics" (list @@ option filter_topic_encoding))
       (opt "blockHash" block_hash_encoding))

type filter_changes =
  | Block_filter of block_hash
  | Pending_transaction_filter of hash
  | Log of transaction_log

let filter_changes_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"block"
        (Tag 0)
        block_hash_encoding
        (function Block_filter hash -> Some hash | _ -> None)
        (fun hash -> Block_filter hash);
      case
        ~title:"pending_transaction"
        (Tag 1)
        hash_encoding
        (function Pending_transaction_filter hash -> Some hash | _ -> None)
        (fun hash -> Pending_transaction_filter hash);
      case
        ~title:"log"
        (Tag 2)
        transaction_log_encoding
        (function Log f -> Some f | _ -> None)
        (fun f -> Log f);
    ]

type fee_history = {
  oldest_block : quantity;
  base_fee_per_gas : quantity list;
  gas_used_ratio : float list;
}

let fee_history_encoding =
  let open Data_encoding in
  conv
    (fun {oldest_block; base_fee_per_gas; gas_used_ratio} ->
      (oldest_block, base_fee_per_gas, gas_used_ratio))
    (fun (oldest_block, base_fee_per_gas, gas_used_ratio) ->
      {oldest_block; base_fee_per_gas; gas_used_ratio})
    (obj3
       (req "oldestBlock" quantity_encoding)
       (req "baseFeePerGas" (list quantity_encoding))
       (req "gasUsedRatio" (list float)))

module Delayed_transaction = struct
  type kind = Transaction | Deposit | Fa_deposit

  type t = {
    kind : kind;
    hash : hash;
    raw : string;
        (* Binary string, so that it integrates smoothly with the tx-pool. *)
  }

  let encoding_kind =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"transaction"
          (constant "transaction")
          (function Transaction -> Some () | _ -> None)
          (function () -> Transaction);
        case
          (Tag 1)
          ~title:"deposit"
          (constant "deposit")
          (function Deposit -> Some () | _ -> None)
          (function () -> Deposit);
        case
          (Tag 2)
          ~title:"fa_deposit"
          (constant "fa_deposit")
          (function Fa_deposit -> Some () | _ -> None)
          (function () -> Fa_deposit);
      ]

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {kind; hash; raw} -> (kind, hash, raw))
      (fun (kind, hash, raw) -> {kind; hash; raw})
      (tup3 encoding_kind hash_encoding (string' Hex))

  let of_rlp_content ?(transaction_tag = "\x03") ?(fa_deposit_tag = "\x04") hash
      rlp_content =
    match rlp_content with
    | Rlp.(List [Value tag; content]) -> (
        match (Bytes.to_string tag, content) with
        (* The new delayed transaction event actually contains the
           TransactionContent, which is Ethereum|Deposit|DelayedTransaction.
           Transaction cannot be in the delayed inbox by construction, therefore
           we care only about Deposit and DelayedTransaction.

           However, we use this function to decode actual delayed inbox item
           when we initialize from a rollup-node. They contain the same
           payload but have a different tag for transaction.
        *)
        | tag, Rlp.Value raw_tx when tag = transaction_tag ->
            Some {kind = Transaction; hash; raw = Bytes.to_string raw_tx}
        | tag, fa_deposit when tag = fa_deposit_tag ->
            (* Delayed inbox item has tag 3, inbox::transaction has tag 4. Event
               uses the inbox::transaction tag. *)
            let raw = Rlp.encode fa_deposit |> Bytes.to_string in
            Some {kind = Fa_deposit; hash; raw}
        | "\x02", deposit ->
            let raw = Rlp.encode deposit |> Bytes.to_string in
            Some {kind = Deposit; hash; raw}
        | _ -> None)
    | _ -> None

  let to_rlp {kind; raw; hash} =
    let open Rlp in
    let tag =
      (match kind with
      | Transaction -> "\x03"
      | Deposit -> "\x02"
      | Fa_deposit -> "\x04")
      |> Bytes.of_string
    in
    let hash = hash_to_bytes hash |> Bytes.of_string in
    let content =
      match kind with
      | Transaction -> Value (Bytes.of_string raw)
      | Deposit -> decode_exn (Bytes.of_string raw)
      | Fa_deposit -> decode_exn (Bytes.of_string raw)
    in
    let rlp = List [Value hash; List [Value tag; content]] in
    encode rlp

  let pp_kind fmt = function
    | Transaction -> Format.pp_print_string fmt "Transaction"
    | Deposit -> Format.pp_print_string fmt "Deposit"
    | Fa_deposit -> Format.pp_print_string fmt "FA_Deposit"

  let pp fmt {raw; kind; _} =
    Format.fprintf fmt "%a: %a" pp_kind kind Hex.pp (Hex.of_string raw)

  let pp_short fmt {kind; hash; _} =
    Format.fprintf fmt "%a: %a" pp_kind kind pp_hash hash
end

module Upgrade = struct
  type t = {hash : hash; timestamp : Time.Protocol.t}

  let of_rlp = function
    | Rlp.List [Value hash_bytes; Value timestamp] ->
        let hash =
          hash_bytes |> Bytes.to_string |> Hex.of_string |> Hex.show
          |> hash_of_string
        in
        let timestamp = timestamp_of_bytes timestamp in
        Some {hash; timestamp}
    | _ -> None

  let of_bytes bytes =
    match bytes |> Rlp.decode with Ok rlp -> of_rlp rlp | _ -> None

  let to_bytes {hash; timestamp} =
    let hash = hash_to_bytes hash |> String.to_bytes in
    let timestamp = timestamp_to_bytes timestamp in
    Rlp.(encode (List [Value hash; Value timestamp]))

  let encoding =
    let open Data_encoding in
    conv
      (fun {hash = Hash (Hex hash); timestamp} -> (hash, timestamp))
      (fun (hash, timestamp) -> {hash = Hash (Hex hash); timestamp})
      (tup2 string Time.Protocol.encoding)
end

module Sequencer_upgrade = struct
  type t = {
    sequencer : Signature.public_key;
    pool_address : address;
    timestamp : Time.Protocol.t;
  }

  let of_rlp = function
    | Rlp.List [Value sequencer; Value pool_address; Value timestamp] ->
        let sequencer =
          Signature.Public_key.of_b58check_exn (String.of_bytes sequencer)
        in
        let timestamp = timestamp_of_bytes timestamp in
        let pool_address = decode_address pool_address in
        Some {sequencer; pool_address; timestamp}
    | _ -> None

  let to_rlp {sequencer; pool_address; timestamp} =
    let sequencer =
      Signature.Public_key.to_b58check sequencer |> String.to_bytes
    in
    let timestamp = timestamp_to_bytes timestamp in
    let pool_address = encode_address pool_address in
    Rlp.List [Value sequencer; Value pool_address; Value timestamp]

  let of_bytes bytes =
    match bytes |> Rlp.decode with Ok rlp -> of_rlp rlp | _ -> None

  let to_bytes sequencer_upgrade = Rlp.encode @@ to_rlp sequencer_upgrade

  let encoding =
    let open Data_encoding in
    conv
      (fun {sequencer; pool_address = Address (Hex pool_address); timestamp} ->
        (sequencer, pool_address, timestamp))
      (fun (sequencer, pool_address, timestamp) ->
        {sequencer; pool_address = Address (Hex pool_address); timestamp})
      (tup3 Signature.Public_key.encoding string Time.Protocol.encoding)
end

module Blueprint_applied = struct
  type t = {number : quantity; hash : block_hash}

  let of_rlp = function
    | Rlp.List [Value number; Value hash] ->
        let number = decode_number_le number in
        let hash = decode_block_hash hash in
        Some {number; hash}
    | _ -> None

  let encoding =
    let open Data_encoding in
    conv
      (fun {number = Qty number; hash = Block_hash (Hex hash)} ->
        (number, hash))
      (fun (number, hash) ->
        {number = Qty number; hash = Block_hash (Hex hash)})
      (tup2 z string)
end

module Evm_events = struct
  type t =
    | Upgrade_event of Upgrade.t
    | Sequencer_upgrade_event of Sequencer_upgrade.t
    | Blueprint_applied of Blueprint_applied.t
    | New_delayed_transaction of Delayed_transaction.t

  let of_bytes bytes =
    match bytes |> Rlp.decode with
    | Ok (Rlp.List [Value tag; rlp_content]) -> (
        match Bytes.to_string tag with
        | "\x01" ->
            let upgrade = Upgrade.of_rlp rlp_content in
            Option.map (fun u -> Upgrade_event u) upgrade
        | "\x02" ->
            let sequencer_upgrade = Sequencer_upgrade.of_rlp rlp_content in
            Option.map (fun u -> Sequencer_upgrade_event u) sequencer_upgrade
        | "\x03" ->
            let blueprint_applied = Blueprint_applied.of_rlp rlp_content in
            Option.map (fun u -> Blueprint_applied u) blueprint_applied
        | "\x04" -> (
            match rlp_content with
            | List [Value hash; transaction_content] ->
                let hash = decode_hash hash in
                let transaction =
                  Delayed_transaction.of_rlp_content hash transaction_content
                in
                Option.map (fun u -> New_delayed_transaction u) transaction
            | _ -> None)
        | _ -> None)
    | _ -> None

  let pp fmt = function
    | Upgrade_event {hash; timestamp} ->
        Format.fprintf
          fmt
          "Upgrade:@ hash %a,@ timestamp: %a"
          pp_hash
          hash
          Time.Protocol.pp_hum
          timestamp
    | Sequencer_upgrade_event
        {sequencer; pool_address = Address (Hex address); timestamp} ->
        Format.fprintf
          fmt
          "Sequencer upgrade:@ sequencer:@ %a,pool_address %s,@ timestamp: %a"
          Signature.Public_key.pp
          sequencer
          address
          Time.Protocol.pp_hum
          timestamp
    | Blueprint_applied {number = Qty number; hash = Block_hash (Hex hash)} ->
        Format.fprintf
          fmt
          "Blueprint applied:@,number:%a@ hash: %s"
          Z.pp_print
          number
          hash
    | New_delayed_transaction delayed_transaction ->
        Format.fprintf
          fmt
          "New delayed transaction:@ %a"
          Delayed_transaction.pp_short
          delayed_transaction

  let encoding =
    let open Data_encoding in
    let case ~kind ~tag ~event_encoding ~proj ~inj =
      case
        ~title:kind
        (Tag tag)
        (obj2 (req "kind" string) (req "event" event_encoding))
        (fun x -> match proj x with None -> None | Some x -> Some (kind, x))
        (fun (_, x) -> inj x)
    in
    union
      [
        case
          ~kind:"kernel_upgrade"
          ~tag:0
          ~event_encoding:Upgrade.encoding
          ~proj:(function Upgrade_event upgrade -> Some upgrade | _ -> None)
          ~inj:(fun upgrade -> Upgrade_event upgrade);
        case
          ~kind:"sequencer_upgrade"
          ~tag:1
          ~event_encoding:Sequencer_upgrade.encoding
          ~proj:(function
            | Sequencer_upgrade_event upgrade -> Some upgrade | _ -> None)
          ~inj:(fun upgrade -> Sequencer_upgrade_event upgrade);
        case
          ~kind:"blueprint_applied"
          ~tag:2
          ~event_encoding:Blueprint_applied.encoding
          ~proj:(function Blueprint_applied info -> Some info | _ -> None)
          ~inj:(fun info -> Blueprint_applied info);
        case
          ~kind:"new_delayed_transaction"
          ~tag:3
          ~event_encoding:Delayed_transaction.encoding
          ~proj:(function
            | New_delayed_transaction delayed_transaction ->
                Some delayed_transaction
            | _ -> None)
          ~inj:(fun delayed_transaction ->
            New_delayed_transaction delayed_transaction);
      ]
end

(* See bool encoding for RLP: https://docs.rs/ethereum-rlp/latest/src/rlp/impls.rs.html#36-44 *)
let bool_to_rlp_bytes b =
  if b then Rlp.Value (Bytes.make 1 '\001') else Rlp.Value Bytes.empty
