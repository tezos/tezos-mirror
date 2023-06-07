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

(** Append the [0x] prefix to a string. *)
let append_0x s = "0x" ^ s

(** Strip the [0x] prefix of a string. *)
let strip_0x s =
  if String.starts_with ~prefix:"0x" s then
    let n = String.length s in
    String.sub s 2 (n - 2)
  else s

(** Ethereum address (20 bytes) *)
type address = Address of string [@@ocaml.unboxed]

let address_of_string s = Address (String.lowercase_ascii (strip_0x s))

let address_to_string (Address a) = append_0x a

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
type block_hash = Block_hash of string [@@ocaml.unboxed]

let block_hash_of_string s = Block_hash (strip_0x s)

let block_hash_encoding =
  Data_encoding.(
    conv (fun (Block_hash h) -> append_0x h) block_hash_of_string string)

(** Ethereum hash or data, that would encoded with a 0x prefix. *)
type hash = Hash of string [@@ocaml.unboxed]

(** [hash_of_string s] takes a string [s] representing a hash in
    hexadecimal format, e.g. [0xFFFFFFF]. Strips the prefix and keeps the
    hash value, e.g. [FFFFFFF]. *)
let hash_of_string s =
  if s = "" then
    (* Empty hash, tools can choose to send [""] instead of
       omitting the field, e.g. ["data" : ""]. *)
    Hash ""
  else Hash (strip_0x s)

(** [hash_to_string h] constructs a valid hash encoded in hexadecimal format,
    e.g. [0xFFFFFFF]. *)
let hash_to_string (Hash h) = append_0x h

(** [hash_to_bytes hash] transforms the [hash] to binary format. *)
let hash_to_bytes (Hash h) = Hex.to_bytes_exn (`Hex h) |> Bytes.to_string

let hash_encoding = Data_encoding.(conv hash_to_string hash_of_string string)

let empty_hash = Hash ""

let decode_block_hash bytes = Block_hash Hex.(of_bytes bytes |> show)

let decode_address bytes = Address Hex.(of_bytes bytes |> show)

let decode_number bytes = Bytes.to_string bytes |> Z.of_bits |> quantity_of_z

let decode_hash bytes = Hash Hex.(of_bytes bytes |> show)

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
  logsBloom : hash;
  type_ : quantity;
  status : quantity;
  contractAddress : address option;
}

let rope_to_bytes r = Rope.to_string r |> Bytes.of_string

let decode_rope_block_hash h = decode_block_hash (rope_to_bytes h)

let decode_rope_hash h = decode_hash (rope_to_bytes h)

let decode_rope_address a = decode_address (rope_to_bytes a)

let decode_rope_number n = decode_number (rope_to_bytes n)

let transaction_receipt_from_rlp bytes =
  match Rlp.decode (Rope.of_string bytes) with
  | Rlp.RlpList
      [
        RlpData hash;
        RlpData index;
        RlpData block_hash;
        RlpData block_number;
        RlpData from;
        RlpData to_;
        RlpData cumulative_gas_used;
        RlpData effective_gas_price;
        RlpData gas_used;
        RlpData contract_address;
        RlpData type_;
        RlpData status;
      ] ->
      let hash = decode_rope_hash hash in
      let index = decode_rope_number index in
      let block_hash = decode_rope_block_hash block_hash in
      let block_number = decode_rope_number block_number in
      let from = decode_rope_address from in
      let to_ =
        if Rope.is_empty to_ then None else Some (decode_rope_address to_)
      in
      let cumulative_gas_used = decode_rope_number cumulative_gas_used in
      let effective_gas_price = decode_rope_number effective_gas_price in
      let gas_used = decode_rope_number gas_used in
      let contract_address =
        if Rope.is_empty contract_address then None
        else Some (decode_rope_address contract_address)
      in
      let type_ = decode_rope_number type_ in
      let status = decode_rope_number status in
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
        logs = [];
        logsBloom = Hash (String.make 256 'a');
        type_;
        status;
        contractAddress = contract_address;
      }
  | _ -> raise (Invalid_argument "Expected a RlpList of 12 elements")

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
          (req "logsBloom" hash_encoding)
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
  match Rlp.decode (Rope.of_string bytes) with
  | Rlp.RlpList
      [
        RlpData block_hash;
        RlpData block_number;
        RlpData from;
        RlpData gas_used;
        RlpData gas_price;
        RlpData hash;
        RlpData input;
        RlpData nonce;
        RlpData to_;
        RlpData index;
        RlpData value;
        RlpData v;
        RlpData r;
        RlpData s;
      ] ->
      let block_hash = decode_rope_block_hash block_hash in
      let block_number = decode_rope_number block_number in

      let from = decode_rope_address from in
      let gas = decode_rope_number gas_used in
      let gas_price = decode_rope_number gas_price in
      let hash = decode_rope_hash hash in
      let input = decode_rope_hash input in
      let nonce = decode_rope_number nonce in
      let to_ =
        if Rope.is_empty to_ then None else Some (decode_rope_address to_)
      in
      let index = decode_rope_number index in
      let value = decode_rope_number value in
      let v = decode_rope_number v in
      let r = decode_rope_hash r in
      let s = decode_rope_hash s in
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
  | _ -> raise (Invalid_argument "Expected a RlpList of 14 elements")

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
  nonce : hash;
  sha3Uncles : hash;
  logsBloom : hash option;
  transactionRoot : hash;
  stateRoot : hash;
  receiptRoot : hash;
  miner : hash;
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
          (req "nonce" hash_encoding)
          (req "sha3Uncles" hash_encoding)
          (req "logsBloom" (option hash_encoding))
          (req "transactionsRoot" hash_encoding)
          (req "stateRoot" hash_encoding)
          (req "receiptsRoot" hash_encoding)
          (req "miner" hash_encoding))
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

let call_encoding =
  let open Data_encoding in
  conv
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
       (opt "data" hash_encoding))

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

  let compare (Address h) (Address h') = String.compare h h'

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
