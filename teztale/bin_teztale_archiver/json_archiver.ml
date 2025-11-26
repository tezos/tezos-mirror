(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2020 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_result_syntax

class type t = Tezos_client_base.Client_context.full

let name = "json-archiver"

let levels_per_folder = 4096l

let dirname_of_level prefix level =
  let base = Int32.mul (Int32.div level levels_per_folder) levels_per_folder in
  Filename.concat
    prefix
    (Printf.sprintf
       "%li-%li"
       base
       (Int32.add base (Int32.pred levels_per_folder)))

let filename_of_level prefix level =
  Filename.concat
    (dirname_of_level prefix level)
    (Int32.to_string level ^ ".json")

let load filename encoding empty =
  let*! exists = Lwt_unix.file_exists filename in
  if exists then
    let* json = Lwt_utils_unix.Json.read_file filename in
    try return (Data_encoding.Json.destruct encoding json)
    with exn -> Lwt.return (Error_monad.error_with_exn exn)
  else return empty

let write filename encoding value =
  let*! () = Lwt_utils_unix.create_dir (Filename.dirname filename) in
  Lwt_utils_unix.Json.write_file
    filename
    (Data_encoding.Json.construct encoding value)

module StringMap = Map.Make (Compare.String)

let files_in_use = ref StringMap.empty

let get_file_mutex filename =
  match StringMap.find filename !files_in_use with
  | None ->
      let x = Lwt_mutex.create () in
      let () = files_in_use := StringMap.add filename x !files_in_use in
      x
  | Some x -> x

let drop_file_mutex filename =
  files_in_use :=
    StringMap.filter
      (fun name mutex ->
        not
          (String.equal name filename
          && (not (Lwt_mutex.is_locked mutex))
          && Lwt_mutex.is_empty mutex))
      !files_in_use

(* [add_to_operations block_hash ops_kind ops_round ops] adds the
   preattestations or attestations in [ops] that were included in block
   [block_hash] to the list of operations already known for operation's
   producer. *)
let add_to_operations block_hash ops_hash ops_kind ?ops_round operations =
  match
    List.partition
      (fun Data.Delegate_operations.{kind; round; _} ->
        kind = ops_kind
        &&
        match (round, ops_round) with
        | None, None -> true
        | Some round, Some round' -> Int32.equal round round'
        | _ -> assert false)
      operations
  with
  | ( Data.Delegate_operations.
        {hash; kind; round = _; mempool_inclusion; block_inclusion}
      :: _,
      operations' ) ->
      Data.Delegate_operations.
        {
          hash;
          kind;
          round = ops_round;
          mempool_inclusion;
          block_inclusion = block_hash :: block_inclusion;
        }
      :: operations'
  | [], _ ->
      {
        hash = ops_hash;
        kind = ops_kind;
        round = ops_round;
        mempool_inclusion = [];
        block_inclusion = [block_hash];
      }
      :: operations

(* [validators] are those delegates whose operations (either preattestations or
   attestations) have been included in the given block.*)
let add_inclusion_in_block block_hash validators delegate_operations =
  let updated_known, unknown =
    List.fold_left
      (fun (acc, missing)
           Data.Delegate_operations.(
             {
               delegate;
               first_slot;
               attesting_power;
               operations;
               assigned_shard_indices;
             } as delegate_ops)
         ->
        match
          List.partition
            (fun op ->
              Tezos_crypto.Signature.Public_key_hash.equal
                op.Consensus_ops.delegate
                delegate)
            missing
        with
        | op :: other_ops_by_same_delegate, missing' ->
            assert (other_ops_by_same_delegate = []) ;
            ( Data.Delegate_operations.
                {
                  delegate;
                  first_slot;
                  attesting_power;
                  operations =
                    add_to_operations
                      block_hash
                      op.op.hash
                      op.op.kind
                      ?ops_round:op.op.round
                      operations;
                  assigned_shard_indices;
                }
              :: acc,
              missing' )
        | [], _ -> (delegate_ops :: acc, missing))
      ([], validators)
      delegate_operations
  in
  match unknown with
  | [] -> updated_known
  | _ :: _ ->
      List.fold_left
        (fun acc op ->
          let delegate = op.Consensus_ops.delegate in
          Data.Delegate_operations.
            {
              delegate;
              first_slot = 0;
              attesting_power = op.Consensus_ops.power;
              operations =
                [
                  {
                    hash = op.op.hash;
                    kind = op.op.kind;
                    round = op.op.round;
                    mempool_inclusion = [];
                    block_inclusion = [block_hash];
                  };
                ];
              assigned_shard_indices = [];
            }
          :: acc)
        updated_known
        unknown

type level_file_content = {
  cycle_info : Data.cycle_info option;
  blocks : Data.Block.t list;
  delegate_operations : Data.Delegate_operations.t list;
  baking_rights : Data.baking_right list;
  unaccurate : bool;
}

let level_file_content_empty =
  {
    cycle_info = None;
    blocks = [];
    delegate_operations = [];
    baking_rights = [];
    unaccurate = false;
  }

let level_file_content_encoding =
  let open Data_encoding in
  conv
    (fun {cycle_info; blocks; delegate_operations; baking_rights; unaccurate} ->
      (cycle_info, blocks, delegate_operations, baking_rights, unaccurate))
    (fun (cycle_info, blocks, delegate_operations, baking_rights, unaccurate) ->
      {cycle_info; blocks; delegate_operations; baking_rights; unaccurate})
    (obj5
       (opt "cycle_info" Data.cycle_info_encoding)
       (dft "blocks" (list Data.Block.encoding) [])
       (* TODO: change name? *)
       (dft "endorsements" (list Data.Delegate_operations.encoding) [])
       (dft "baking_rights" (list Data.baking_right_encoding) [])
       (dft "unaccurate" Data_encoding.bool false))

let rec merge_baking_rights (acc : Data.baking_right list) = function
  | [] -> acc
  | hd :: tl ->
      if List.exists (fun x -> x.Data.round = hd.Data.round) acc then
        merge_baking_rights acc tl
      else merge_baking_rights (hd :: acc) tl

(* FIXME: Use to use Data.t instead of Data.block_data? *)
let dump_included_in_block logger path block_level block_hash block_predecessor
    block_round timestamp reception_times baker cycle_info consensus_ops
    baking_rights =
  let delegate_operations_t =
    let attestations_level = Int32.pred block_level in
    let filename = filename_of_level path attestations_level in
    Log.info logger (fun () ->
        Format.asprintf
          "Dumping delegate operations in block %a at level %li."
          Block_hash.pp
          block_hash
          block_level) ;
    let mutex = get_file_mutex filename in
    let*! out =
      Lwt_mutex.with_lock mutex (fun () ->
          let* (info : level_file_content) =
            load filename level_file_content_encoding level_file_content_empty
          in
          let delegate_operations =
            add_inclusion_in_block
              block_hash
              consensus_ops
              info.delegate_operations
          in
          let out =
            {
              cycle_info;
              blocks = info.blocks;
              delegate_operations;
              unaccurate = info.unaccurate;
              baking_rights = info.baking_rights;
            }
          in
          write filename level_file_content_encoding out)
    in
    let () = drop_file_mutex filename in
    match out with
    | Ok () -> Lwt.return_unit
    | Error err ->
        Log.error logger (fun () ->
            Format.asprintf
              "@[Failed to dump delegate operations in block %a at level %li \
               :@ @[%a@]@]"
              Block_hash.pp
              block_hash
              block_level
              Error_monad.pp_print_trace
              err) ;
        Lwt.return_unit
  in
  let blocks_t =
    let filename = filename_of_level path block_level in
    let mutex = get_file_mutex filename in
    let*! out =
      Lwt_mutex.with_lock mutex (fun () ->
          let* infos =
            load filename level_file_content_encoding level_file_content_empty
          in
          let delegate_operations = infos.delegate_operations in
          let blocks =
            Data.Block.
              {
                hash = block_hash;
                predecessor = block_predecessor;
                delegate = baker;
                round = block_round;
                reception_times;
                timestamp;
                nonce = None;
              }
            :: infos.blocks
          in
          write
            filename
            level_file_content_encoding
            {
              cycle_info;
              blocks;
              delegate_operations;
              unaccurate = infos.unaccurate;
              baking_rights =
                merge_baking_rights infos.baking_rights baking_rights;
            })
    in
    let () = drop_file_mutex filename in
    match out with
    | Ok () -> Lwt.return_unit
    | Error err ->
        Log.error logger (fun () ->
            Format.asprintf
              "@[Failed to dump block %a at level %li :@ @[%a@]@]"
              Block_hash.pp
              block_hash
              block_level
              Error_monad.pp_print_trace
              err) ;
        Lwt.return_unit
  in
  Lwt.join [delegate_operations_t; blocks_t]

(* NB: the same operation may be received several times; we only record the
   first reception time. *)
let merge_operations =
  List.fold_left
    (fun acc Consensus_ops.{op = {hash; kind; round}; errors; reception_time} ->
      match
        List.partition
          (fun Data.Delegate_operations.{round = r; kind = k; _} ->
            Option.equal Int32.equal r round && kind = k)
          acc
      with
      | ( Data.Delegate_operations.{block_inclusion; mempool_inclusion = []; _}
          :: _,
          acc' ) ->
          Data.Delegate_operations.
            {
              hash;
              kind;
              round;
              mempool_inclusion =
                [{source = "archiver"; reception_time; errors}];
              block_inclusion;
            }
          :: acc'
      | _ :: _, _ -> acc
      | [], _ ->
          {
            hash;
            kind;
            round;
            mempool_inclusion = [{source = "archiver"; reception_time; errors}];
            block_inclusion = [];
          }
          :: acc)

let dump_received logger path ?unaccurate level received_ops =
  let filename = filename_of_level path level in
  let mutex = get_file_mutex filename in
  let*! out =
    Lwt_mutex.with_lock mutex (fun () ->
        let* infos =
          load filename level_file_content_encoding level_file_content_empty
        in
        let updated_known, unknown =
          List.fold_left
            (fun (acc, missing)
                 Data.Delegate_operations.(
                   {
                     delegate;
                     first_slot;
                     attesting_power;
                     operations;
                     assigned_shard_indices;
                   } as delegate_ops)
               ->
              match
                List.partition
                  (fun (right, _) ->
                    Tezos_crypto.Signature.Public_key_hash.equal
                      right.Consensus_ops.address
                      delegate)
                  missing
              with
              | (_, new_operations) :: other_ops_by_same_delegate, missing' ->
                  assert (other_ops_by_same_delegate = []) ;
                  ( Data.Delegate_operations.
                      {
                        delegate;
                        first_slot;
                        attesting_power;
                        operations = merge_operations operations new_operations;
                        assigned_shard_indices;
                      }
                    :: acc,
                    missing' )
              | [], _ -> (delegate_ops :: acc, missing))
            ([], received_ops)
            infos.delegate_operations
        in
        let* delegate_operations =
          match unknown with
          | [] -> return updated_known
          | _ :: _ ->
              return
                (List.fold_left
                   (fun acc (right, ops) ->
                     Data.Delegate_operations.
                       {
                         delegate = right.Consensus_ops.address;
                         first_slot = right.Consensus_ops.first_slot;
                         attesting_power = right.Consensus_ops.power;
                         operations =
                           List.rev_map
                             (fun Consensus_ops.
                                    {
                                      op = {hash; kind; round};
                                      errors;
                                      reception_time;
                                    }
                                ->
                               {
                                 hash;
                                 kind;
                                 round;
                                 mempool_inclusion =
                                   [
                                     {
                                       source = "archiver";
                                       reception_time;
                                       errors;
                                     };
                                   ];
                                 block_inclusion = [];
                               })
                             ops;
                         assigned_shard_indices = [];
                       }
                     :: acc)
                   updated_known
                   unknown)
        in
        let unaccurate = Option.value ~default:infos.unaccurate unaccurate in
        let out_infos =
          {
            cycle_info = infos.cycle_info;
            blocks = infos.blocks;
            delegate_operations;
            unaccurate;
            baking_rights = infos.baking_rights;
          }
        in
        write filename level_file_content_encoding out_infos)
  in
  let () = drop_file_mutex filename in
  match out with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Log.error logger (fun () ->
          Format.asprintf
            "@[Failed to dump delegate operations at level %li :@ @[%a@]@]"
            level
            Error_monad.pp_print_trace
            err) ;
      Lwt.return_unit

type chunk =
  | Block of
      Int32.t
      * Block_hash.t
      * Block_hash.t option
      * Int32.t
      * Time.Protocol.t
      * Data.Block.reception list
      * Tezos_crypto.Signature.Public_key_hash.t
      * Data.cycle_info option
      * Consensus_ops.block_op list
      * Data.baking_right list
  | Mempool of bool option * Int32.t (* level *) * Consensus_ops.delegate_ops

let chunk_stream, chunk_feeder = Lwt_stream.create ()

let dump prefix chunk =
  let logger = Log.logger () in
  match chunk with
  | Block
      ( level,
        block_hash,
        predecessor,
        round,
        timestamp,
        reception_times,
        baker,
        cycle_info,
        block_info,
        baking_rights ) ->
      dump_included_in_block
        logger
        prefix
        level
        block_hash
        predecessor
        round
        timestamp
        reception_times
        baker
        cycle_info
        block_info
        baking_rights
  | Mempool (unaccurate, level, items) ->
      dump_received logger prefix ?unaccurate level items

let launch _cctxt prefix = Lwt_stream.iter_p (dump prefix) chunk_stream

let stop () = chunk_feeder None

let add_mempool ?unaccurate ~level items =
  chunk_feeder (Some (Mempool (unaccurate, level, items)))

let add_block ~level (block, cycle_info, (endos, preendos), baking_rights) =
  chunk_feeder
    (Some
       (Block
          ( level,
            block.Data.Block.hash,
            block.Data.Block.predecessor,
            block.round,
            block.timestamp,
            block.reception_times,
            block.delegate,
            cycle_info,
            endos @ preendos,
            baking_rights )))

(* not used *)
let add_rights ~level:_ _rights = ()

(* not used *)
let add_dal_shards ~level:_ _shard_assignments = ()
