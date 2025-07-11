(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(**
  A bloom filter can be seen as a probabilistic set. As such, the order of
  its elements is not important.

  Blocks contain a bloom filter with the union of all topics in their logs,
  together with the address of the contract that produced them.
  The [getLogs] RPC defines a filter (not to be confused with bloom filter)
  which may contain a pattern of topics to be matched with a log.

  A pattern is defined as: [pattern := TOPIC | NULL | TOPIC list]

  Where [NULL] is a wildcard, and a list of topics matches against any of the
  elements in the list ([Or]).

  To speed up the filtering, we compute a bloom filter corresponding to the
  filter's pattern. The goal is to check if this bloom is contained in the
  block's bloom, and only fetching the block in that case.
  Therefore, our filter must always be included in filters corresponding
  to blocks that have at least a log that matches with the pattern.
  For this reason, we decide to ignore [Or] patterns in the bloom filter
  "heuristic" (including all topics would break the previous property).
  The same is done for addresses, as a filter can match against a list
  of them.
  If this becomes a serious bottleneck, we could keep a collection of bloom
  filters to represent the disjunction.
*)

(** Saner representation of an input filter:
    [from_block] and [to_block] are defined by:
    A filter's [from_block] and [to_block] if provided, or
    a filter's [block_hash] if provided ([from_block = to_block = block_n]), or
    [from_block = to_block = latest block's number].

    A [bloom] filter is computed using the topics and address.
*)
type valid_filter = {
  from_block : quantity;
  to_block : quantity;
  bloom : Ethbloom.t;
  topics : Filter.topic option list;
  address : address list;
}

type bloom_filter = {
  bloom : Ethbloom.t;
  topics : Filter.topic option list;
  address : address list;
}

type error +=
  | Incompatible_block_params
  | Block_range_too_large of {limit : int}
  | Topic_list_too_large
  | Receipt_not_found of hash
  | Too_many_logs of {limit : int}

let valid_range log_filter_config (Qty from) (Qty to_) =
  Z.(
    to_ >= from
    && to_ - from < of_int log_filter_config.Configuration.max_nb_blocks)

let emit_and_return_none event arg =
  let open Lwt_result_syntax in
  let*! () = Internal_event.Simple.emit event arg in
  return_none

(* Parses the [from_block] and [to_block] fields, as described before.  *)
let validate_range log_filter_config
    (module Rollup_node_rpc : Services_backend_sig.S) (filter : Filter.t) =
  let open Lwt_result_syntax in
  match filter with
  | {block_hash = Some _; from_block = Some _; _}
  | {block_hash = Some _; to_block = Some _; _} ->
      tzfail Incompatible_block_params
  | {block_hash = Some block_hash; _} ->
      let* block =
        Rollup_node_rpc.Etherlink_block_storage.block_by_hash
          ~full_transaction_object:false
          block_hash
      in
      return_some (block.number, block.number)
  | {from_block; to_block; _} ->
      let get_block_number block_param =
        Rollup_node_rpc.block_param_to_block_number
          ~chain_family:L2_types.EVM
          (Block_parameter
             (Option.value ~default:Block_parameter.Latest block_param))
      in
      let* from_block = get_block_number from_block in
      let* to_block = get_block_number to_block in
      if from_block > to_block then return_none
      else if valid_range log_filter_config from_block to_block then
        return_some (from_block, to_block)
      else
        tzfail (Block_range_too_large {limit = log_filter_config.max_nb_blocks})

let make_bloom_address_topics address topics =
  let bloom = Ethbloom.make () in
  Option.iter
    (function
      | Filter.Single (Address address) -> Ethbloom.accrue ~input:address bloom
      | _ -> ())
    address ;
  Option.iter
    (List.iter (function
        | Some Filter.(One (Hash topic)) -> Ethbloom.accrue ~input:topic bloom
        | _ -> ()))
    topics ;
  bloom

(* Constructs the bloom filter *)
let make_bloom (filter : Filter.t) =
  make_bloom_address_topics filter.address filter.topics

let validate_topics (filter : Filter.t) =
  let open Lwt_result_syntax in
  match filter.topics with
  | Some topics when List.compare_length_with topics 4 > 0 ->
      tzfail Topic_list_too_large
  | _ -> return_unit

let validate_bloom_filter (filter : Filter.t) =
  let open Lwt_result_syntax in
  let* () = validate_topics filter in
  let bloom = make_bloom filter in
  let topics = Option.value ~default:[] filter.topics in
  let address =
    Option.map (function Filter.Single a -> [a] | Vec l -> l) filter.address
    |> Option.value ~default:[]
  in
  return {bloom; topics; address}

(* Parsing a filter into a simpler representation, this is the
   input validation step *)
let validate_filter log_filter_config
    (module Rollup_node_rpc : Services_backend_sig.S) filter =
  let open Lwt_result_syntax in
  let* range =
    validate_range log_filter_config (module Rollup_node_rpc) filter
  in
  match range with
  | None -> return_none
  | Some (from_block, to_block) ->
      let* () = validate_topics filter in
      let bloom = make_bloom filter in
      let address =
        Option.map
          (function Filter.Single a -> [a] | Vec l -> l)
          filter.address
        |> Option.value ~default:[]
      in
      return_some
        {
          from_block;
          to_block;
          bloom;
          topics = Option.value ~default:[] filter.topics;
          address;
        }

let hex_to_bytes h = hex_to_bytes h |> Bytes.of_string

(* Checks if a filter's topics matches a log's topics, as specified in
   https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getfilterchanges *)
let match_filter_topics (filter : bloom_filter) (log_topics : hash list) : bool
    =
  let match_one_topic (filter_topic : Filter.topic option) (log_topic : hash) =
    match (filter_topic, log_topic) with
    (* Null matches with every topic *)
    | None, _ -> true
    | Some (One ft), lt -> ft = lt
    | Some (Or fts), lt -> List.mem ~equal:( = ) lt fts
  in
  (* A log has at most 4 topics, no need to make it tail-rec *)
  let rec go filter_topics log_topics =
    match (filter_topics, log_topics) with
    (* Empty filter matches with everything *)
    | [], _ -> true
    (* Non-empty filter never matches with empty topics *)
    | _ :: _, [] -> false
    | ft :: fts, lt :: lts -> match_one_topic ft lt && go fts lts
  in
  go filter.topics log_topics

(* Checks if a filter's address matches a log's address *)
let match_filter_address (filter : bloom_filter) (address : address) : bool =
  List.is_empty filter.address || List.mem ~equal:( = ) address filter.address

(* Apply a filter on one log *)
let filter_one_log : bloom_filter -> transaction_log -> Filter.changes option =
 fun filter log ->
  if
    match_filter_address filter log.address
    && match_filter_topics filter log.topics
  then Some (Log log)
  else None

let filter_receipt (filter : bloom_filter) (receipt : Transaction_receipt.t) =
  if Ethbloom.contains_bloom (hex_to_bytes receipt.logsBloom) filter.bloom then
    Some (List.filter_map (filter_one_log filter) receipt.logs)
  else None

(* Apply a filter on one transaction *)
let filter_one_tx (module Rollup_node_rpc : Services_backend_sig.S) :
    bloom_filter -> hash -> Filter.changes list option tzresult Lwt.t =
 fun filter tx_hash ->
  let open Lwt_result_syntax in
  let* receipt =
    Rollup_node_rpc.Etherlink_block_storage.transaction_receipt tx_hash
  in
  match receipt with
  | Some receipt -> return @@ filter_receipt filter receipt
  | None -> tzfail (Receipt_not_found tx_hash)

(* Apply a filter on one block *)
let filter_one_block (module Rollup_node_rpc : Services_backend_sig.S) :
    valid_filter -> Z.t -> Filter.changes list option tzresult Lwt.t =
 fun filter block_number ->
  let open Lwt_result_syntax in
  let* block =
    Rollup_node_rpc.Etherlink_block_storage.nth_block
      ~full_transaction_object:false
      block_number
  in
  let indexed_transaction_hashes =
    match block.transactions with
    | TxHash l -> l
    | TxFull _ ->
        (* Impossible:
           The block is requested without tx objects *)
        assert false
  in
  let filter =
    {bloom = filter.bloom; topics = filter.topics; address = filter.address}
  in
  if Ethbloom.contains_bloom (hex_to_bytes block.logsBloom) filter.bloom then
    let+ changes =
      List.filter_map_ep
        (filter_one_tx (module Rollup_node_rpc) filter)
        indexed_transaction_hashes
    in
    Some (List.concat changes)
  else return_none

(** [split_in_chunks ~chunk_size ~base ~length] returns a list of
    lists (chunks) containing the consecutive numbers from [base]
    to [base + length - 1].
    Each chunk is at most of length [chunk_size]. Only the last
    chunk can be shorter than [chunk_size].

    Example [split_in_chunks ~chunk_size:2 ~base:1 ~length:5] is
    <<1, 2>, <3,4>, <5>>.
 *)
let split_in_chunks ~chunk_size ~base ~length =
  (* nb_chunks = ceil(length / chunk_size)  *)
  let nb_chunks = (length + chunk_size - 1) / chunk_size in
  let rem = length mod chunk_size in
  Stdlib.List.init nb_chunks (fun chunk ->
      let chunk_length =
        if chunk = nb_chunks - 1 && rem <> 0 then (* Last chunk isn't full *)
          rem
        else chunk_size
      in
      let chunk_offset = chunk * chunk_size in
      Stdlib.List.init chunk_length (fun i ->
          Z.(base + of_int chunk_offset + of_int i)))

(* [get_logs (module Rollup_node_rpc) filter] applies the [filter].

   It does so using a chunking mechanism:
   Blocks to be filtered are split in chunks, which will be filtered
   in sequence. Within each chunk, the block filtering is done
   concurrently.

   This design is meant to strike a balance between concurrent
   performace and not exceeding the bound in number of logs.
*)
let get_logs (log_filter_config : Configuration.log_filter_config)
    (module Rollup_node_rpc : Services_backend_sig.S) filter =
  let open Lwt_result_syntax in
  let* filter =
    validate_filter log_filter_config (module Rollup_node_rpc) filter
  in
  match filter with
  | None -> return []
  | Some filter ->
      let (Qty from) = filter.from_block in
      let (Qty to_) = filter.to_block in
      let length = Z.(to_int (to_ - from)) + 1 in
      let block_numbers =
        split_in_chunks
          ~chunk_size:log_filter_config.chunk_size
          ~length
          ~base:from
      in
      let* logs, _n_logs =
        List.fold_left_es
          (function
            | acc_logs, n_logs ->
                fun chunk ->
                  (* Apply the filter to the entire chunk concurrently *)
                  let* new_logs =
                    Lwt_result.map List.concat
                    @@ List.filter_map_ep
                         (filter_one_block (module Rollup_node_rpc) filter)
                         chunk
                  in
                  let n_new_logs = List.length new_logs in
                  if n_logs + n_new_logs > log_filter_config.max_nb_logs then
                    tzfail
                      (Too_many_logs {limit = log_filter_config.max_nb_logs})
                  else return (acc_logs @ new_logs, n_logs + n_new_logs))
          ([], 0)
          block_numbers
      in
      return logs

(* Errors registration *)

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_incompatible_block_params"
    ~title:"Incompatible block parameters"
    ~description:
      "block_hash field cannot be set when from_block and to_block are set"
    Data_encoding.(obj1 (req "incompatible_block_params" unit))
    (function Incompatible_block_params -> Some () | _ -> None)
    (fun () -> Incompatible_block_params) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_block_range_too_large"
    ~title:"Block range is too large"
    ~description:"Block_range is too large"
    ~pp:(fun fmt limit ->
      Format.fprintf fmt "Cannot request logs over more than %d blocks" limit)
    Data_encoding.(
      obj1 (req "block_range_too_large" (obj1 (req "limit" int31))))
    (function Block_range_too_large {limit} -> Some limit | _ -> None)
    (fun limit -> Block_range_too_large {limit}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_topic_list_too_large"
    ~title:"Topic list is too large"
    ~description:"Topic_list is too large"
    Data_encoding.(obj1 (req "topic_list_too_large" unit))
    (function Topic_list_too_large -> Some () | _ -> None)
    (fun () -> Topic_list_too_large) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_receipt_not_found"
    ~title:"Receipt not found"
    ~description:"Could not found requested receipt"
    Data_encoding.(obj1 (req "receipt_not_found" hash_encoding))
    (function Receipt_not_found hash -> Some hash | _ -> None)
    (fun hash -> Receipt_not_found hash) ;
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_too_many_logs"
    ~title:"Too many logs"
    ~description:
      "Result would return too many logs. Request on a smaller block range"
    ~pp:(fun fmt limit ->
      Format.fprintf
        fmt
        "Result would return too many logs, current limit is %d"
        limit)
    Data_encoding.(obj1 (req "too_many_logs" (obj1 (req "limit" int31))))
    (function Too_many_logs {limit} -> Some limit | _ -> None)
    (fun limit -> Too_many_logs {limit})
