(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** Transaction hash size is 32 bytes. *)
let transaction_hash_size = 32

(** Translate an int in a binary string of two bytes (little endian).
    Ints greater than 2 bytes are truncated. *)
let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

(** Ethereum data, as Hex-encoded strings *)
type hex = Hex of string [@@ocaml.unboxed]

(** Appends the [0x] prefix to a string. *)
let hex_to_string (Hex s) = "0x" ^ s

(** Strips the [0x] prefix of a string. *)
let hex_of_string s =
  if String.starts_with ~prefix:"0x" s then
    let n = String.length s in
    Hex (String.sub s 2 (n - 2))
  else Hex s

(** [hex_to_bytes hex] transforms the [hex] to binary format. *)
let hex_to_bytes (Hex h) = Hex.to_bytes_exn (`Hex h) |> Bytes.to_string

let hex_encoding = Data_encoding.(conv hex_to_string hex_of_string string)

(** Ethereum address (20 bytes) *)
type address = Address of hex [@@ocaml.unboxed]

let address_of_string s = Address (hex_of_string (String.lowercase_ascii s))

let address_to_string (Address a) = hex_to_string a

let address_encoding =
  Data_encoding.(conv address_to_string address_of_string string)

(** [timestamp_to_bytes timestamp] transforms the timestamp to bytes
    compatible with the kernel. *)
let timestamp_to_bytes timestamp =
  let seconds = Time.Protocol.to_seconds timestamp in
  let buffer = Bytes.make 8 '\000' in
  Bytes.set_int64_le buffer 0 seconds ;
  buffer

let timestamp_of_bytes timestamp_bytes =
  let timestamp_64 = Bytes.get_int64_le timestamp_bytes 0 in
  Time.Protocol.of_seconds timestamp_64

(** Ethereum generic quantity, always encoded in hexadecimal. *)
type quantity = Qty of Z.t [@@ocaml.unboxed]

let quantity_of_z z = Qty z

let z_to_hexa = Z.format "#x"

let quantity_encoding =
  Data_encoding.conv
    (fun (Qty q) -> z_to_hexa q)
    (fun q -> Qty (Z.of_string q))
    Data_encoding.string

(** Ethereum block level. *)
type block_height = Block_height of Z.t [@@ocaml.unboxed]

let block_height_of_z z = Block_height z

let block_height_encoding =
  Data_encoding.conv
    (fun (Block_height h) -> z_to_hexa h)
    (fun h -> Block_height (Z.of_string h))
    Data_encoding.string

(** Ethereum block params in RPCs. *)
type block_param = Hash_param of block_height | Earliest | Latest | Pending

let block_param_encoding =
  let open Data_encoding in
  union
    [
      (let tag = "hex" in
       case
         ~title:tag
         (Tag 0)
         block_height_encoding
         (function Hash_param h -> Some h | _ -> None)
         (fun h -> Hash_param h));
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
    ]

(** Ethereum block hash (32 bytes) *)
type block_hash = Block_hash of hex [@@ocaml.unboxed]

let block_hash_of_string s = Block_hash (hex_of_string s)

let block_hash_encoding =
  Data_encoding.(
    conv (fun (Block_hash h) -> hex_to_string h) block_hash_of_string string)

let block_hash_to_bytes (Block_hash h) = hex_to_bytes h

let genesis_parent_hash = Block_hash (Hex (String.make 64 'f'))

(** Ethereum hash, that would encoded with a 0x prefix. *)
type hash = Hash of hex [@@ocaml.unboxed]

(** [hash_of_string s] takes a string [s] representing a hash in
    hexadecimal format, e.g. [0xFFFFFFF]. Strips the prefix and keeps the
    hash value, e.g. [FFFFFFF]. *)
let hash_of_string s = Hash (hex_of_string s)

(** [hash_to_string h] constructs a valid hash encoded in hexadecimal format,
    e.g. [0xFFFFFFF]. *)
let hash_to_string (Hash h) = hex_to_string h

(** [hash_to_bytes hash] transforms the [hash] to binary format. *)
let hash_to_bytes (Hash h) = hex_to_bytes h

let hash_encoding = Data_encoding.(conv hash_to_string hash_of_string string)

let pp_hash fmt (Hash (Hex h)) = Format.pp_print_string fmt h

let empty_hash = Hash (Hex "")

let decode_hex bytes = Hex Hex.(of_bytes bytes |> show)

let encode_hex (Hex hex) = Hex.to_bytes_exn (`Hex hex)

let decode_block_hash bytes = Block_hash (decode_hex bytes)

let decode_address bytes = Address (decode_hex bytes)

let encode_address (Address address) = encode_hex address

let decode_number bytes = Bytes.to_string bytes |> Z.of_bits |> quantity_of_z

let encode_number (Qty v) = Z.to_bits v |> Bytes.of_string

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

let encode_u16_le (Qty n) =
  let bits = Z.to_bits n |> Bytes.of_string in
  pad_to_n_bytes_le bits 2

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
        decode_number index )
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
      let index = decode_number index in
      let block_number = decode_number block_number in
      let from = decode_address from in
      let to_ = if to_ = Bytes.empty then None else Some (decode_address to_) in
      let cumulative_gas_used = decode_number cumulative_gas_used in
      let effective_gas_price = decode_number effective_gas_price in
      let gas_used = decode_number gas_used in
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
      let type_ = decode_number type_ in
      let status = decode_number status in
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
  blockHash : block_hash;
  blockNumber : quantity;
  from : address;
  gas : quantity;
  gasPrice : quantity;
  hash : hash;
  input : hash;
  nonce : quantity;
  to_ : address option;
  transactionIndex : quantity;
      (* It can be null if it's in a pending block, but we don't have a notion of pending. *)
  value : quantity;
  v : quantity;
  r : hash;
  s : hash;
}

let transaction_object_from_rlp block_hash bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
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
        ]) ->
      let block_number = decode_number block_number in
      let from = decode_address from in
      let gas = decode_number gas_used in
      let gas_price = decode_number gas_price in
      let hash = decode_hash hash in
      let input = decode_hash input in
      let nonce = decode_number nonce in
      let to_ = if to_ = Bytes.empty then None else Some (decode_address to_) in
      let index = decode_number index in
      let value = decode_number value in
      let v = decode_number v in
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
          (req "blockHash" block_hash_encoding)
          (req "blockNumber" quantity_encoding)
          (req "from" address_encoding)
          (req "gas" quantity_encoding)
          (req "gasPrice" quantity_encoding)
          (req "hash" hash_encoding)
          (req "input" hash_encoding)
          (req "nonce" quantity_encoding)
          (req "to" (option address_encoding))
          (req "transactionIndex" quantity_encoding))
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

(** Ethereum block hash representation from RPCs. *)
type block = {
  number : block_height;
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

let block_from_rlp bytes =
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
      let (Qty number) = decode_number number in
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
        decode_option ~default:(Qty Z.zero) decode_number gasLimit
      in
      let transactions = TxHash (decode_list decode_hash transactions) in
      let gasUsed = decode_number gasUsed in
      let timestamp = decode_number timestamp in
      {
        number = Block_height number;
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
      }
  | _ -> raise (Invalid_argument "Expected a List of 13 elements")

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
         } ->
      ( ( number,
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
          uncles ) ))
    (fun ( ( number,
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
             uncles ) ) ->
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
        timestamp;
        transactions;
        uncles;
      })
    (merge_objs
       (obj10
          (req "number" block_height_encoding)
          (req "hash" block_hash_encoding)
          (req "parentHash" block_hash_encoding)
          (req "nonce" hex_encoding)
          (req "sha3Uncles" hash_encoding)
          (req "logsBloom" hex_encoding)
          (req "transactionsRoot" hash_encoding)
          (req "stateRoot" hash_encoding)
          (req "receiptsRoot" hash_encoding)
          (req "miner" hex_encoding))
       (obj9
          (req "difficulty" quantity_encoding)
          (req "totalDifficulty" quantity_encoding)
          (req "extraData" hex_encoding)
          (req "size" quantity_encoding)
          (req "gasLimit" quantity_encoding)
          (req "gasUsed" quantity_encoding)
          (req "timestamp" quantity_encoding)
          (req "transactions" block_transactions_encoding)
          (req "uncles" (list hash_encoding))))

type transaction = {
  from : address;
  to_ : address;
  gas : quantity;
  gasPrice : quantity;
  value : quantity option;
  data : hash;
  nonce : quantity option;
}

let transaction_encoding =
  let open Data_encoding in
  conv
    (fun {from; to_; gas; gasPrice; value; data; nonce} ->
      (from, to_, gas, gasPrice, value, data, nonce))
    (fun (from, to_, gas, gasPrice, value, data, nonce) ->
      {from; to_; gas; gasPrice; value; data; nonce})
    (obj7
       (req "from" address_encoding)
       (req "to" address_encoding)
       (req "gas" quantity_encoding)
       (req "gasPrice" quantity_encoding)
       (opt "value" quantity_encoding)
       (req "data" hash_encoding)
       (opt "nonce" quantity_encoding))

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
    (conv
       (fun {from; to_; gas; gasPrice; value; data} ->
         (from, Some to_, gas, gasPrice, value, data))
       (fun (from, to_, gas, gasPrice, value, data) ->
         {from; to_ = Option.join to_; gas; gasPrice; value; data})
       (obj6
          (opt "from" address_encoding)
          (opt "to" (option address_encoding))
          (* `call` is also used for estimateGas, which allows all fields to be
             empty, hence `to` can be `null` or absent. *)
          (opt "gas" quantity_encoding)
          (opt "gasPrice" quantity_encoding)
          (opt "value" quantity_encoding)
          (opt "data" hash_encoding)))
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

  let encoding = address_encoding
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

let hash_raw_tx str =
  str |> Bytes.of_string |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
  |> Bytes.to_string

(** [transaction_nonce bytes] returns the nonce of a given raw transaction. *)
let transaction_nonce bytes =
  let open Result_syntax in
  if String.starts_with ~prefix:"01" bytes then
    (* eip 2930*)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _])
    | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _; _; _; _]) ->
        let+ nonce = Rlp.decode_z nonce in
        Qty nonce
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements")
  else if String.starts_with ~prefix:"02" bytes then
    (* eip 1559*)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _])
    | Ok (Rlp.List [_; Value nonce; _; _; _; _; _; _; _; _; _]) ->
        let+ nonce = Rlp.decode_z nonce in
        Qty nonce
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements")
  else
    (* Legacy *)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok (Rlp.List [Value nonce; _; _; _; _; _; _; _; _]) ->
        let+ nonce = Rlp.decode_z nonce in
        Qty nonce
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 elements")

(** [transaction_gas_price base_fee bytes] returns the maximum gas price the
    user can pay for the tx. *)
let transaction_gas_price base_fee bytes =
  let open Result_syntax in
  if String.starts_with ~prefix:"01" bytes then
    (* eip 2930*)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok (Rlp.List [_; _; Value gas_price; _; _; _; _; _; _])
    | Ok (Rlp.List [_; _; Value gas_price; _; _; _; _; _; _; _; _; _]) ->
        let* gas_price = Rlp.decode_z gas_price in
        return gas_price
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9 or 12 elements")
  else if String.starts_with ~prefix:"02" bytes then
    (* eip 1559*)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok
        (Rlp.List
          [
            _;
            _;
            Value max_priority_fee_per_gas;
            Value max_fee_per_gas;
            _;
            _;
            _;
            _;
          ])
    | Ok
        (Rlp.List
          [
            _;
            _;
            Value max_priority_fee_per_gas;
            Value max_fee_per_gas;
            _;
            _;
            _;
            _;
            _;
            _;
            _;
          ]) ->
        let* max_priority_fee_per_gas = Rlp.decode_z max_priority_fee_per_gas in
        let* max_fee_per_gas = Rlp.decode_z max_fee_per_gas in
        return Z.(min max_fee_per_gas (add base_fee max_priority_fee_per_gas))
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 8 or 11 elements")
  else
    (* Legacy *)
    match bytes |> String.to_bytes |> Rlp.decode with
    | Ok (Rlp.List [_; Value gas_price; _; _; _; _; _; _; _]) ->
        let* gas_price = Rlp.decode_z gas_price in
        return gas_price
    | _ -> tzfail (Rlp.Rlp_decoding_error "Expected a list of 9")

(* Event filter, see
   https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getlogs *)
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
  from_block : block_param option;
  to_block : block_param option;
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
       (opt "fromBlock" block_param_encoding)
       (opt "toBlock" block_param_encoding)
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

module Delayed_transaction = struct
  type kind = Transaction | Deposit

  type t = {
    kind : kind;
    hash : hash;
    raw : string;
        (* Binary string, so that it integrates smoothly with the tx-pool. *)
  }

  let hash t = t.hash

  let encoding_kind =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"transaction"
          unit
          (function Transaction -> Some () | _ -> None)
          (function () -> Transaction);
        case
          (Tag 1)
          ~title:"deposit"
          unit
          (function Deposit -> Some () | _ -> None)
          (function () -> Deposit);
      ]

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {kind; hash; raw} -> (kind, hash, raw))
      (fun (kind, hash, raw) -> {kind; hash; raw})
      (obj3
         (req "kind" encoding_kind)
         (req "hash" hash_encoding)
         (req "raw" string))

  let of_bytes hash bytes =
    match bytes |> Rlp.decode with
    | Ok Rlp.(List [List [Value tag; content]; _timestamp; _level]) -> (
        match (Bytes.to_string tag, content) with
        | "\x01", Rlp.Value raw_tx ->
            let hash =
              raw_tx |> Bytes.to_string |> hash_raw_tx |> Hex.of_string
              |> Hex.show |> hash_of_string
            in
            Some {kind = Transaction; hash; raw = Bytes.to_string raw_tx}
        | "\x02", deposit ->
            let raw = Rlp.encode deposit |> Bytes.to_string in
            Some {kind = Deposit; hash; raw}
        | _ -> None)
    | _ -> None

  let pp_kind fmt = function
    | Transaction -> Format.pp_print_string fmt "Transaction"
    | Deposit -> Format.pp_print_string fmt "Deposit"

  let pp fmt {raw; kind; _} =
    Format.fprintf fmt "%a: %a" pp_kind kind Hex.pp (Hex.of_string raw)

  let pp_short fmt {hash = Hash (Hex h); _} = Format.pp_print_string fmt h
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

module Evm_events = struct
  type t =
    | Upgrade_event of Upgrade.t
    | Sequencer_upgrade_event of Sequencer_upgrade.t

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
        | _ -> None)
    | _ -> None

  let pp fmt = function
    | Upgrade_event {hash; timestamp} ->
        Format.fprintf
          fmt
          "upgrade:@ hash %a,@ timestamp: %a"
          pp_hash
          hash
          Time.Protocol.pp_hum
          timestamp
    | Sequencer_upgrade_event
        {sequencer; pool_address = Address (Hex address); timestamp} ->
        Format.fprintf
          fmt
          "SequencerUpgrade:@ sequencer:@ %a,pool_address %s,@ timestamp: %a"
          Signature.Public_key.pp
          sequencer
          address
          Time.Protocol.pp_hum
          timestamp

  let encoding =
    let open Data_encoding in
    union
      [
        (let tag = "kernel_upgrade" in
         case
           ~title:tag
           (Tag 0)
           (obj2 (req "kind" string) (req "event" Upgrade.encoding))
           (function
             | Upgrade_event upgrade -> Some ("kernel_upgrade", upgrade)
             | _ -> None)
           (fun (_, upgrade) -> Upgrade_event upgrade));
        (let tag = "sequencer_upgrade" in
         case
           ~title:tag
           (Tag 1)
           (obj2 (req "kind" string) (req "event" Sequencer_upgrade.encoding))
           (function
             | Sequencer_upgrade_event sequencer_upgrade ->
                 Some ("sequencer_upgrade", sequencer_upgrade)
             | _ -> None)
           (fun (_, sequencer_upgrade) ->
             Sequencer_upgrade_event sequencer_upgrade));
      ]
end
