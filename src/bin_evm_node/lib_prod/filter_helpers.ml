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
  from_block : block_height;
  to_block : block_height;
  bloom : Ethbloom.t;
  topics : filter_topic option list;
  address : address option;
}

module Event = struct
  let section = ["evm_node"; "prod"; "logs_filter"]

  let incompatible_block_params =
    Internal_event.Simple.declare_0
      ~section
      ~name:"incompatible_block_params"
      ~msg:"block_hash field cannot be set when from_block and to_block are set"
      ~level:Error
      ()

  let block_range_too_large =
    Internal_event.Simple.declare_0
      ~section
      ~name:"block_range_too_large"
      ~msg:"Requested block range is above the maximum"
      ~level:Error
      ()

  let topic_list_too_large =
    Internal_event.Simple.declare_0
      ~section
      ~name:"topic_list_too_large"
      ~msg:"Topic list length should be at most 4"
      ~level:Error
      ()

  let receipt_not_found =
    Internal_event.Simple.declare_1
      ~section
      ~name:"receipt_not_found"
      ~msg:"Receipt not found for {tx_hash}"
      ~level:Error
      ("tx_hash", hash_encoding)

  let too_many_logs =
    Internal_event.Simple.declare_0
      ~section
      ~name:"too_many_logs"
      ~msg:"Too many logs requested"
      ~level:Error
      ()
end

(** [height_from_param (module Rollup_node_rpc) from to_] returns the
    block height for params [from] and [to_] as a tuple.
*)
let height_from_param (module Rollup_node_rpc : Rollup_node.S) from to_ =
  let open Lwt_result_syntax in
  match (from, to_) with
  | Hash_param h1, Hash_param h2 -> return (h1, h2)
  | Hash_param h1, _ ->
      let+ h2 = Rollup_node_rpc.current_block_number () in
      (h1, h2)
  | _, _ ->
      let+ h = Rollup_node_rpc.current_block_number () in
      (h, h)

let valid_range log_filter_config (Block_height from) (Block_height to_) =
  Z.(
    to_ >= from
    && to_ - from < of_int log_filter_config.Configuration.max_nb_blocks)

let emit_and_return_none event arg =
  let open Lwt_result_syntax in
  let*! () = Internal_event.Simple.emit event arg in
  return_none

(* Parses the [from_block] and [to_block] fields, as described before.  *)
let validate_range log_filter_config (module Rollup_node_rpc : Rollup_node.S)
    (filter : filter) =
  let open Lwt_result_syntax in
  match filter with
  | {from_block = Some _; to_block = Some _; block_hash = Some _; _} ->
      emit_and_return_none Event.incompatible_block_params ()
  | {block_hash = Some block_hash; _} ->
      let* block =
        Rollup_node_rpc.block_by_hash ~full_transaction_object:false block_hash
      in
      return_some (block.number, block.number)
  | {from_block; to_block; _} ->
      let from_block = Option.value ~default:Latest from_block in
      let to_block = Option.value ~default:Latest to_block in
      let* from_block, to_block =
        height_from_param (module Rollup_node_rpc) from_block to_block
      in
      if valid_range log_filter_config from_block to_block then
        return_some (from_block, to_block)
      else emit_and_return_none Event.block_range_too_large ()

(* Constructs the bloom filter *)
let make_bloom (filter : filter) =
  let bloom = Ethbloom.make () in
  Option.iter
    (fun (Address address) -> Ethbloom.accrue ~input:address bloom)
    filter.address ;
  Option.iter
    (List.iter (function
        | Some (One (Hash topic)) -> Ethbloom.accrue ~input:topic bloom
        | _ -> ()))
    filter.topics ;
  bloom

let validate_topics (filter : filter) =
  let open Lwt_result_syntax in
  match filter.topics with
  | Some topics when List.compare_length_with topics 4 > 0 ->
      emit_and_return_none Event.topic_list_too_large ()
  | _ -> return_some ()

(* Bind for ['a option tzresult Lwt.t] *)
let ( let*?? ) m f =
  let open Lwt_result_syntax in
  let* opt_x = m in
  match opt_x with Some x -> f x | None -> return_none

(* Parsing a filter into a simpler representation, this is the
   input validation step *)
let validate_filter log_filter_config (module Rollup_node_rpc : Rollup_node.S) :
    filter -> valid_filter option tzresult Lwt.t =
 fun filter ->
  let open Lwt_result_syntax in
  let*?? from_block, to_block =
    validate_range log_filter_config (module Rollup_node_rpc) filter
  in
  let*?? () = validate_topics filter in
  let bloom = make_bloom filter in
  return_some
    {
      from_block;
      to_block;
      bloom;
      topics = Option.value ~default:[] filter.topics;
      address = filter.address;
    }

let hex_to_bytes h = hex_to_bytes h |> Bytes.of_string

(* Checks if a filter's topics matches a log's topics, as specified in
   https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getfilterchanges *)
let match_filter_topics (filter : valid_filter) (log_topics : hash list) : bool
    =
  let match_one_topic (filter_topic : filter_topic option) (log_topic : hash) =
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
let match_filter_address (filter : valid_filter) (address : address) : bool =
  Option.fold ~none:true ~some:(( = ) address) filter.address

(* Apply a filter on one log *)
let filter_one_log : valid_filter -> transaction_log -> filter_changes option =
 fun filter log ->
  if
    match_filter_address filter log.address
    && match_filter_topics filter log.topics
  then Some (Log log)
  else None

(* Apply a filter on one transaction *)
let filter_one_tx (module Rollup_node_rpc : Rollup_node.S) :
    valid_filter -> hash -> filter_changes list option tzresult Lwt.t =
 fun filter tx_hash ->
  let open Lwt_result_syntax in
  let* receipt = Rollup_node_rpc.transaction_receipt tx_hash in
  match receipt with
  | Some receipt ->
      if Ethbloom.contains_bloom (hex_to_bytes receipt.logsBloom) filter.bloom
      then return_some @@ List.filter_map (filter_one_log filter) receipt.logs
      else return_none
  | None -> emit_and_return_none Event.receipt_not_found tx_hash

(* Apply a filter on one block *)
let filter_one_block (module Rollup_node_rpc : Rollup_node.S) :
    valid_filter -> Z.t -> filter_changes list option tzresult Lwt.t =
 fun filter block_number ->
  let open Lwt_result_syntax in
  let* block =
    Rollup_node_rpc.nth_block ~full_transaction_object:false block_number
  in
  let indexed_transaction_hashes =
    match block.transactions with
    | TxHash l -> l
    | TxFull _ ->
        (* Impossible:
           The block is requested without tx objects *)
        assert false
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
    (module Rollup_node_rpc : Rollup_node.S) filter =
  let open Lwt_result_syntax in
  let+ logs =
    let*?? filter =
      validate_filter log_filter_config (module Rollup_node_rpc) filter
    in
    let (Block_height from) = filter.from_block in
    let (Block_height to_) = filter.to_block in
    let length = Z.(to_int (to_ - from)) + 1 in
    let block_numbers =
      split_in_chunks
        ~chunk_size:log_filter_config.chunk_size
        ~length
        ~base:from
    in
    let*?? logs, _n_logs =
      List.fold_left_es
        (function
          | Some (acc_logs, n_logs) ->
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
                  emit_and_return_none Event.too_many_logs ()
                else return_some (acc_logs @ new_logs, n_logs + n_new_logs)
          | None ->
              (* Filtering failed due to a request of too many logs *)
              fun _chunk -> return_none)
        (Some ([], 0))
        block_numbers
    in
    return_some logs
  in
  Option.value ~default:[] logs
