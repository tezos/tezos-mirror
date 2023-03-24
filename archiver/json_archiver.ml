(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Lwt_result_syntax

class type t = Tezos_client_base.Client_context.full

let name = "json-archiver"

let levels_per_folder = 4096l

module Anomaly = struct
  let rec insert_in_ordered_list (Data.Anomaly.{level; delegate; _} as anomaly)
      = function
    | [] -> [anomaly]
    | head :: tail as l ->
        if Compare.Int32.(head.Data.Anomaly.level < level) then anomaly :: l
        else if
          Int32.equal head.level level
          && Tezos_crypto.Signature.Public_key_hash.equal head.delegate delegate
        then if head.problem = Missed then anomaly :: tail else l
        else head :: insert_in_ordered_list anomaly tail
end

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

let dump_anomalies path level anomalies =
  let filename =
    Filename.concat (dirname_of_level path level) "anomalies.json"
  in
  let mutex = get_file_mutex filename in
  let*! out =
    Lwt_mutex.with_lock mutex (fun () ->
        let* known =
          load filename (Data_encoding.list Data.Anomaly.encoding) []
        in
        write
          filename
          (Data_encoding.list Data.Anomaly.encoding)
          (List.fold_left
             (fun x y -> Anomaly.insert_in_ordered_list y x)
             known
             anomalies))
  in
  let () = drop_file_mutex filename in
  Lwt.return out

let extract_anomalies path level infos =
  if infos.Data.unaccurate then return_unit
  else
    let anomalies =
      List.fold_left
        (fun acc
             Data.Delegate_operations.
               {delegate; delegate_alias; endorsing_power = _; operations} ->
          List.fold_left
            (fun acc
                 Data.Delegate_operations.
                   {kind; round; mempool_inclusion; block_inclusion} ->
              match (kind, mempool_inclusion, block_inclusion) with
              | Endorsement, [], [] ->
                  Data.Anomaly.
                    {
                      level;
                      round;
                      kind;
                      delegate;
                      delegate_alias;
                      problem = Data.Anomaly.Missed;
                    }
                  :: acc
              | Endorsement, Data.Delegate_operations.{errors; _} :: _, [] -> (
                  match errors with
                  | Some (_ :: _) ->
                      Data.Anomaly.
                        {
                          level;
                          round;
                          kind;
                          delegate;
                          delegate_alias;
                          problem = Incorrect;
                        }
                      :: acc
                  | None | Some [] ->
                      Data.Anomaly.
                        {
                          level;
                          round;
                          kind;
                          delegate;
                          delegate_alias;
                          problem = Forgotten;
                        }
                      :: acc)
              | Endorsement, [], _ :: _ ->
                  Data.Anomaly.
                    {
                      level;
                      round;
                      kind;
                      delegate;
                      delegate_alias;
                      problem = Sequestered;
                    }
                  :: acc
              | Endorsement, _ :: _, _ :: _ -> acc
              | Preendorsement, _, _ -> acc)
            acc
            operations)
        []
        infos.Data.delegate_operations
    in
    match anomalies with
    | [] -> return_unit
    | _ :: _ -> dump_anomalies path level anomalies

(* [add_to_operations block_hash ops_kind ops_round ops] adds the
   preendorsements or endorsements in [ops] that were included in block
   [block_hash] to the list of operations already known for operation's
   producer. *)
let add_to_operations block_hash ops_kind ?ops_round operations =
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
        {kind; round = _; mempool_inclusion; block_inclusion}
      :: _,
      operations' ) ->
      Data.Delegate_operations.
        {
          kind;
          round = ops_round;
          mempool_inclusion;
          block_inclusion = block_hash :: block_inclusion;
        }
      :: operations'
  | [], _ ->
      {
        kind = ops_kind;
        round = ops_round;
        mempool_inclusion = [];
        block_inclusion = [block_hash];
      }
      :: operations

(* [validators] are those delegates whose operations (either preendorsements or
   endorsements) have been included in the given block.*)
let add_inclusion_in_block aliases block_hash validators delegate_operations =
  let updated_known, unknown =
    List.fold_left
      (fun (acc, missing)
           Data.Delegate_operations.(
             {delegate; delegate_alias; endorsing_power; operations} as
             delegate_ops) ->
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
                  delegate_alias;
                  endorsing_power;
                  operations =
                    add_to_operations
                      block_hash
                      op.op.kind
                      ?ops_round:op.op.round
                      operations;
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
              delegate_alias = Wallet.alias_of_pkh aliases delegate;
              endorsing_power = op.Consensus_ops.power;
              operations =
                [
                  {
                    kind = op.op.kind;
                    round = op.op.round;
                    mempool_inclusion = [];
                    block_inclusion = [block_hash];
                  };
                ];
            }
          :: acc)
        updated_known
        unknown

let dump_included_in_block cctxt path block_level block_hash block_round
    timestamp reception_time baker consensus_ops =
  let open Lwt.Infix in
  Wallet.of_context cctxt >>= fun aliases_opt ->
  let aliases =
    match aliases_opt with
    | Ok aliases -> aliases
    | Error err ->
        let () = Error_monad.pp_print_trace Format.err_formatter err in
        Wallet.empty
  in
  (let endorsements_level = Int32.pred block_level in
   let filename = filename_of_level path endorsements_level in
   let mutex = get_file_mutex filename in
   Lwt_mutex.with_lock mutex (fun () ->
       let* infos = load filename Data.encoding Data.empty in
       let delegate_operations =
         add_inclusion_in_block
           aliases
           block_hash
           consensus_ops
           infos.Data.delegate_operations
       in
       let out_infos =
         Data.
           {
             blocks = infos.blocks;
             delegate_operations;
             unaccurate = infos.unaccurate;
           }
       in
       let* () = write filename Data.encoding out_infos in
       extract_anomalies path endorsements_level out_infos)
   >>= fun out ->
   let () = drop_file_mutex filename in
   match out with
   | Ok () -> Lwt.return_unit
   | Error err ->
       Lwt_io.printl
         (Format.asprintf
            "@[Failed to dump delegate operations in block %a at level %li :@ \
             @[%a@]@]"
            Block_hash.pp
            block_hash
            block_level
            Error_monad.pp_print_trace
            err))
  <&>
  let filename = filename_of_level path block_level in
  let mutex = get_file_mutex filename in
  Lwt_mutex.with_lock mutex (fun () ->
      let* infos = load filename Data.encoding Data.empty in
      let delegate_operations = infos.Data.delegate_operations in
      let blocks =
        Data.Block.
          {
            hash = block_hash;
            delegate = baker;
            delegate_alias = Wallet.alias_of_pkh aliases baker;
            round = block_round;
            reception_time = Some reception_time;
            timestamp;
            nonce = None;
          }
        :: infos.Data.blocks
      in
      write
        filename
        Data.encoding
        Data.{blocks; delegate_operations; unaccurate = infos.unaccurate})
  >>= fun out ->
  let () = drop_file_mutex filename in
  match out with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Lwt_io.printl
        (Format.asprintf
           "@[Failed to dump block %a at level %li :@ @[%a@]@]"
           Block_hash.pp
           block_hash
           block_level
           Error_monad.pp_print_trace
           err)

(* NB: the same operation may be received several times; we only record the
   first reception time. *)
let merge_operations =
  List.fold_left
    (fun
      acc
      Consensus_ops.{op = {hash = _; kind; round}; errors; reception_time}
    ->
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
            kind;
            round;
            mempool_inclusion = [{source = "archiver"; reception_time; errors}];
            block_inclusion = [];
          }
          :: acc)

let dump_received cctxt path ?unaccurate level received_ops =
  let filename = filename_of_level path level in
  let mutex = get_file_mutex filename in
  let*! out =
    Lwt_mutex.with_lock mutex (fun () ->
        let* infos = load filename Data.encoding Data.empty in
        let updated_known, unknown =
          List.fold_left
            (fun (acc, missing)
                 Data.Delegate_operations.(
                   {delegate; delegate_alias; endorsing_power; operations} as
                   delegate_ops) ->
              match
                List.partition
                  (fun (pkh, _) ->
                    Tezos_crypto.Signature.Public_key_hash.equal pkh delegate)
                  missing
              with
              | (_, new_operations) :: other_ops_by_same_delegate, missing' ->
                  assert (other_ops_by_same_delegate = []) ;
                  ( Data.Delegate_operations.
                      {
                        delegate;
                        delegate_alias;
                        endorsing_power;
                        operations = merge_operations operations new_operations;
                      }
                    :: acc,
                    missing' )
              | [], _ -> (delegate_ops :: acc, missing))
            ([], received_ops)
            infos.Data.delegate_operations
        in
        let* delegate_operations =
          match unknown with
          | [] -> return updated_known
          | _ :: _ ->
              let* aliases = Wallet.of_context cctxt in
              return
                (List.fold_left
                   (fun acc (delegate, ops) ->
                     Data.Delegate_operations.
                       {
                         delegate;
                         delegate_alias = Wallet.alias_of_pkh aliases delegate;
                         endorsing_power = 0;
                         operations =
                           List.rev_map
                             (fun Consensus_ops.
                                    {
                                      op = {hash = _; kind; round};
                                      errors;
                                      reception_time;
                                    } ->
                               {
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
                       }
                     :: acc)
                   updated_known
                   unknown)
        in
        let unaccurate = Option.value ~default:infos.unaccurate unaccurate in
        let out_infos =
          Data.{blocks = infos.blocks; delegate_operations; unaccurate}
        in
        let* () = write filename Data.encoding out_infos in
        if infos.unaccurate then return_unit
        else extract_anomalies path level out_infos)
  in
  let () = drop_file_mutex filename in
  match out with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Lwt_io.printl
        (Format.asprintf
           "@[Failed to dump delegate operations at level %li :@ @[%a@]@]"
           level
           Error_monad.pp_print_trace
           err)

type chunk =
  | Block of
      Int32.t
      * Block_hash.t
      * Int32.t
      * Time.Protocol.t
      * Time.System.t
      * Tezos_crypto.Signature.Public_key_hash.t
      * Consensus_ops.block_op list
  | Mempool of bool option * Int32.t (* level *) * Consensus_ops.delegate_ops

let chunk_stream, chunk_feeder = Lwt_stream.create ()

let launch cctxt prefix =
  Lwt_stream.iter_p
    (function
      | Block
          ( level,
            block_hash,
            round,
            timestamp,
            reception_time,
            baker,
            block_info ) ->
          dump_included_in_block
            cctxt
            prefix
            level
            block_hash
            round
            timestamp
            reception_time
            baker
            block_info
      | Mempool (unaccurate, level, items) ->
          dump_received cctxt prefix ?unaccurate level items)
    chunk_stream

let stop () = chunk_feeder None

let add_mempool ?unaccurate ~level items =
  chunk_feeder (Some (Mempool (unaccurate, level, items)))

let add_block ~level (block, (endos, preendos)) =
  chunk_feeder
    (Some
       (Block
          ( level,
            block.Data.Block.hash,
            block.round,
            block.timestamp,
            Stdlib.Option.get block.reception_time,
            block.delegate,
            endos @ preendos )))

(* not used *)
let add_rights ~level:_ _rights _aliases = ()
