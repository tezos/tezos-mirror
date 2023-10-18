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

let empty_hash = Hash (Hex "")

let decode_hex bytes = Hex Hex.(of_bytes bytes |> show)

let decode_block_hash bytes = Block_hash (decode_hex bytes)

let decode_address bytes = Address (decode_hex bytes)

let decode_number bytes = Bytes.to_string bytes |> Z.of_bits |> quantity_of_z

let decode_hash bytes = Hash (decode_hex bytes)

type transaction_log = {
  address : address;
  topics : hash list;
  data : hash;
  blockNumber : quantity;
  transactionHash : hash;
  transactionIndex : quantity;
  blockHash : block_hash;
  logIndex : quantity;
  removed : bool;
}

let transaction_log_body_from_rlp = function
  | Rlp.List [Value address; List topics; Value data] ->
      ( decode_address address,
        List.map
          (function
            | Rlp.Value bytes -> decode_hash bytes
            | _ -> raise (Invalid_argument "Expected hash representing topic"))
          topics,
        decode_hash data )
  | _ ->
      raise
        (Invalid_argument "Expected list of 3 elements representing log body")

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
       (req "data" hash_encoding)
       (req "blockNumber" quantity_encoding)
       (req "transactionHash" hash_encoding)
       (req "transactionIndex" quantity_encoding)
       (req "blockHash" block_hash_encoding)
       (req "logIndex" quantity_encoding)
       (req "removed" bool))

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

let transaction_receipt_from_rlp bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value hash;
          Value index;
          Value block_hash;
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
      let block_hash = decode_block_hash block_hash in
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
        List.mapi
          (fun i (address, topics, data) ->
            {
              address;
              topics;
              data;
              blockHash = block_hash;
              blockNumber = block_number;
              transactionHash = hash;
              transactionIndex = index;
              logIndex = quantity_of_z (Z.of_int i);
              removed = false;
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
           "Expected a RlpList of 14 elements in transaction receipt")

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
  transactionIndex : quantity;
      (* It can be null if it's in a pending block, but we don't have a notion of pending. *)
  value : quantity;
  v : quantity;
  r : hash;
  s : hash;
}

let transaction_object_from_rlp bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value block_hash;
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
      let block_hash = decode_block_hash block_hash in
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
        blockHash = Some block_hash;
        blockNumber = Some block_number;
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
  | _ -> raise (Invalid_argument "Expected a List of 14 elements")

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
  number : block_height option;
  hash : block_hash option;
  parent : block_hash;
  nonce : hex;
  sha3Uncles : hash;
  logsBloom : hex option;
  transactionRoot : hash;
  stateRoot : hash;
  receiptRoot : hash;
  miner : hex;
  difficulty : quantity;
  totalDifficulty : quantity;
  extraData : string;
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

let block_from_rlp bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value number;
          Value hash;
          Value parent_hash;
          List transactions;
          Value gas_used;
          Value timestamp;
        ]) ->
      let (Qty number) = decode_number number in
      let hash = decode_block_hash hash in
      let parent_hash = decode_block_hash parent_hash in
      let transactions = TxHash (decode_list decode_hash transactions) in
      let gas_used = decode_number gas_used in
      let timestamp = decode_number timestamp in
      {
        number = Some (Block_height number);
        hash = Some hash;
        parent = parent_hash;
        nonce = Hex (String.make 16 'a');
        sha3Uncles = Hash (Hex (String.make 64 'a'));
        logsBloom = Some (Hex (String.make 512 'a'));
        transactionRoot = Hash (Hex (String.make 64 'a'));
        stateRoot = Hash (Hex (String.make 64 'a'));
        receiptRoot = Hash (Hex (String.make 64 'a'));
        (* We need the following dummy value otherwise eth-cli will complain
           that miner's address is not a valid Ethereum address. *)
        miner = Hex "6471A723296395CF1Dcc568941AFFd7A390f94CE";
        difficulty = Qty Z.zero;
        totalDifficulty = Qty Z.zero;
        extraData = "";
        size = Qty (Z.of_int (Bytes.length bytes));
        gasLimit = Qty Z.zero;
        gasUsed = gas_used;
        timestamp;
        transactions;
        uncles = [];
      }
  | _ -> raise (Invalid_argument "Expected a List of 6 elements")

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
          (req "number" (option block_height_encoding))
          (req "hash" (option block_hash_encoding))
          (req "parentHash" block_hash_encoding)
          (req "nonce" hex_encoding)
          (req "sha3Uncles" hash_encoding)
          (req "logsBloom" (option hex_encoding))
          (req "transactionsRoot" hash_encoding)
          (req "stateRoot" hash_encoding)
          (req "receiptsRoot" hash_encoding)
          (req "miner" hex_encoding))
       (obj9
          (req "difficulty" quantity_encoding)
          (req "totalDifficulty" quantity_encoding)
          (req "extraData" string)
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
