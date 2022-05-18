(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let levels_per_folder = 4096l

module Delegate_operations = struct
  type operation = {
    kind : Consensus_ops.operation_kind;
    round : Int32.t option;
    reception_time : Time.System.t option;
    errors : error list option;
    block_inclusion : Block_hash.t list;
  }

  let operation_encoding =
    let open Data_encoding in
    conv
      (fun {kind; round; reception_time; errors; block_inclusion} ->
        (kind, round, reception_time, errors, block_inclusion))
      (fun (kind, round, reception_time, errors, block_inclusion) ->
        {kind; round; reception_time; errors; block_inclusion})
      (obj5
         (dft
            "kind"
            Consensus_ops.operation_kind_encoding
            Consensus_ops.Endorsement)
         (opt "round" int32)
         (req "reception_time" (option Time.System.encoding))
         (opt "errors" (list error_encoding))
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  type t = {
    delegate : Signature.public_key_hash;
    delegate_alias : string option;
    operations : operation list;
  }

  let legacy_encoding =
    let open Data_encoding in
    conv
      (fun _ -> assert false)
      (fun (delegate, delegate_alias, reception_time, errors, block_inclusion) ->
        match (reception_time, block_inclusion) with
        | (None, []) -> {delegate; delegate_alias; operations = []}
        | (_, _) ->
            {
              delegate;
              delegate_alias;
              operations =
                [
                  {
                    kind = Endorsement;
                    reception_time;
                    errors;
                    round = None;
                    block_inclusion;
                  };
                ];
            })
      (obj5
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (opt "reception_time" Time.System.encoding)
         (opt "errors" (list error_encoding))
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  let encoding =
    let open Data_encoding in
    conv
      (fun {delegate; delegate_alias; operations} ->
        (delegate, delegate_alias, operations))
      (fun (delegate, delegate_alias, operations) ->
        {delegate; delegate_alias; operations})
      (obj3
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (dft "operations" (list operation_encoding) []))

  let encoding =
    let open Data_encoding in
    splitted
      ~json:
        (union
           [
             case ~title:"current" Json_only encoding Option.some (fun x -> x);
             case
               ~title:"legacy"
               Json_only
               legacy_encoding
               (fun _ -> None)
               (fun x -> x);
           ])
      ~binary:encoding
end

module Block = struct
  type t = {
    hash : Block_hash.t;
    delegate : Signature.public_key_hash;
    delegate_alias : string option;
    round : Int32.t;
    timestamp : Time.Protocol.t;
    reception_time : Time.System.t;
    nonce : unit option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             hash;
             delegate;
             delegate_alias;
             round;
             reception_time;
             timestamp;
             nonce;
           } ->
        (hash, delegate, delegate_alias, round, reception_time, timestamp, nonce))
      (fun ( hash,
             delegate,
             delegate_alias,
             round,
             reception_time,
             timestamp,
             nonce ) ->
        {
          hash;
          delegate;
          delegate_alias;
          round;
          reception_time;
          timestamp;
          nonce;
        })
      (obj7
         (req "hash" Block_hash.encoding)
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (dft "round" int32 0l)
         (req "reception_time" Time.System.encoding)
         (req "timestamp" Time.Protocol.encoding)
         (opt "nonce" unit))
end

module Anomaly = struct
  (* only anomalies related to endorsements are considered for now *)
  type problem = Missed | Forgotten | Sequestered | Incorrect

  type t = {
    level : Int32.t;
    round : Int32.t option;
    delegate : Signature.Public_key_hash.t;
    delegate_alias : string option;
    problem : problem;
  }

  let problem_encoding =
    Data_encoding.string_enum
      [
        ("missed", Missed);
        ("forgotten", Forgotten);
        ("sequestered", Sequestered);
        ("incorrect", Incorrect);
      ]

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; round; delegate; delegate_alias; problem} ->
        (level, round, delegate, delegate_alias, problem))
      (fun (level, round, delegate, delegate_alias, problem) ->
        {level; round; delegate; delegate_alias; problem})
      (obj5
         (req "level" int32)
         (opt "round" int32)
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (req "problem" problem_encoding))

  let rec insert_in_ordered_list ({level; delegate; _} as anomaly) = function
    | [] -> [anomaly]
    | head :: tail as l ->
        if Compare.Int32.(head.level < level) then anomaly :: l
        else if
          Int32.equal head.level level
          && Signature.Public_key_hash.equal head.delegate delegate
        then if head.problem = Missed then anomaly :: tail else l
        else head :: insert_in_ordered_list anomaly tail
end

type t = {
  blocks : Block.t list;
  delegate_operations : Delegate_operations.t list;
  unaccurate : bool;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {blocks; delegate_operations; unaccurate} ->
      (blocks, delegate_operations, unaccurate))
    (fun (blocks, delegate_operations, unaccurate) ->
      {blocks; delegate_operations; unaccurate})
    (obj3
       (dft "blocks" (list Block.encoding) [])
       (* TODO: change name? *)
       (dft "endorsements" (list Delegate_operations.encoding) [])
       (dft "unaccurate" bool false))

let empty = {blocks = []; delegate_operations = []; unaccurate = true}

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
        let* known = load filename (Data_encoding.list Anomaly.encoding) [] in
        write
          filename
          (Data_encoding.list Anomaly.encoding)
          (List.fold_left
             (fun x y -> Anomaly.insert_in_ordered_list y x)
             known
             anomalies))
  in
  let () = drop_file_mutex filename in
  Lwt.return out

let extract_anomalies path level infos =
  if infos.unaccurate then return_unit
  else
    let anomalies =
      List.fold_left
        (fun acc Delegate_operations.{delegate; delegate_alias; operations} ->
          List.fold_left
            (fun acc
                 Delegate_operations.
                   {kind; round; reception_time; errors; block_inclusion} ->
              match errors with
              | Some (_ :: _) ->
                  Anomaly.
                    {
                      level;
                      round;
                      delegate;
                      delegate_alias;
                      problem = Anomaly.Incorrect;
                    }
                  :: acc
              | None | Some [] -> (
                  match (kind, reception_time, block_inclusion) with
                  | (Endorsement, None, []) ->
                      Anomaly.
                        {
                          level;
                          round;
                          delegate;
                          delegate_alias;
                          problem = Anomaly.Missed;
                        }
                      :: acc
                  | (Endorsement, Some _, []) ->
                      Anomaly.
                        {
                          level;
                          round;
                          delegate;
                          delegate_alias;
                          problem = Anomaly.Forgotten;
                        }
                      :: acc
                  | (Endorsement, None, _ :: _) ->
                      Anomaly.
                        {
                          level;
                          round;
                          delegate;
                          delegate_alias;
                          problem = Anomaly.Sequestered;
                        }
                      :: acc
                  | (Endorsement, Some _, _ :: _) -> acc
                  | (Preendorsement, _, _) -> acc))
            acc
            operations)
        []
        infos.delegate_operations
    in
    match anomalies with
    | [] -> return_unit
    | _ :: _ -> dump_anomalies path level anomalies

(* [add_to_operations block_hash ops_kind ops_round ops] adds the
   preendorsements or endorsements in ops that were included in block
   [block_hash] to the list of operations already known for operation's
   producer. *)
let add_to_operations block_hash ops_kind ?ops_round operations =
  match
    List.partition
      (fun Delegate_operations.{kind; round; _} ->
        kind = ops_kind
        &&
        match (round, ops_round) with
        | (None, None) -> true
        | (Some round, Some round') -> Int32.equal round round'
        | _ -> assert false)
      operations
  with
  | ( Delegate_operations.
        {kind; round = _; errors; reception_time; block_inclusion}
      :: _,
      operations' ) ->
      Delegate_operations.
        {
          kind;
          round = ops_round;
          errors;
          reception_time;
          block_inclusion = block_hash :: block_inclusion;
        }
      :: operations'
  | ([], _) ->
      {
        kind = ops_kind;
        round = ops_round;
        errors = None;
        reception_time = None;
        block_inclusion = [block_hash];
      }
      :: operations

(* [validators] are those delegates whose operations (either preendorsements or
   endorsements) have been included in the given block.*)
let add_inclusion_in_block aliases block_hash ops_kind ops_round validators
    delegate_operations =
  let (updated_known, unknown) =
    List.fold_left
      (fun (acc, missing)
           Delegate_operations.(
             {delegate; delegate_alias; operations} as delegate_ops) ->
        match
          List.partition
            (fun pkh -> Signature.Public_key_hash.equal pkh delegate)
            missing
        with
        | (_ :: _, missing') ->
            ( Delegate_operations.
                {
                  delegate;
                  delegate_alias;
                  operations =
                    add_to_operations block_hash ops_kind ?ops_round operations;
                }
              :: acc,
              missing' )
        | ([], _) -> (delegate_ops :: acc, missing))
      ([], validators)
      delegate_operations
  in
  match unknown with
  | [] -> updated_known
  | _ :: _ ->
      List.fold_left
        (fun acc delegate ->
          Delegate_operations.
            {
              delegate;
              delegate_alias = Wallet.alias_of_pkh aliases delegate;
              operations =
                [
                  {
                    kind = ops_kind;
                    round = ops_round;
                    errors = None;
                    reception_time = None;
                    block_inclusion = [block_hash];
                  };
                ];
            }
          :: acc)
        updated_known
        unknown

let dump_included_in_block cctxt path block_level block_hash block_round
    timestamp reception_time baker consensus_ops_info =
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
       let* infos = load filename encoding empty in
       let delegate_operations' =
         match consensus_ops_info.Consensus_ops.preendorsers with
         | Some validators ->
             add_inclusion_in_block
               aliases
               block_hash
               Consensus_ops.Preendorsement
               consensus_ops_info.preendorsements_round
               validators
               infos.delegate_operations
         | None -> infos.delegate_operations
       in
       let delegate_operations =
         add_inclusion_in_block
           aliases
           block_hash
           Consensus_ops.Endorsement
           consensus_ops_info.endorsements_round
           consensus_ops_info.endorsers
           delegate_operations'
       in
       let out_infos =
         {
           blocks = infos.blocks;
           delegate_operations;
           unaccurate = infos.unaccurate;
         }
       in
       let* () = write filename encoding out_infos in
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
      let* infos = load filename encoding empty in
      let blocks =
        Block.
          {
            hash = block_hash;
            delegate = baker;
            delegate_alias = Wallet.alias_of_pkh aliases baker;
            round = block_round;
            reception_time;
            timestamp;
            nonce = None;
          }
        :: infos.blocks
      in
      write
        filename
        encoding
        {
          blocks;
          delegate_operations = infos.delegate_operations;
          unaccurate = infos.unaccurate;
        })
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
  List.fold_left (fun acc Consensus_ops.{kind; round; errors; reception_time} ->
      match
        List.partition
          (fun Delegate_operations.{round = r; kind = k; _} ->
            Option.equal Int32.equal r round && kind = k)
          acc
      with
      | ( Delegate_operations.{block_inclusion; reception_time = None; _} :: _,
          acc' ) ->
          Delegate_operations.
            {
              kind;
              round;
              errors;
              reception_time = Some reception_time;
              block_inclusion;
            }
          :: acc'
      | (_ :: _, _) -> acc
      | ([], _) ->
          {
            kind;
            round;
            errors;
            reception_time = Some reception_time;
            block_inclusion = [];
          }
          :: acc)

let dump_received cctxt path ?unaccurate level received_ops =
  let filename = filename_of_level path level in
  let mutex = get_file_mutex filename in
  let*! out =
    Lwt_mutex.with_lock mutex (fun () ->
        let* infos = load filename encoding empty in
        let (updated_known, unknown) =
          List.fold_left
            (fun (acc, missing)
                 Delegate_operations.(
                   {delegate; delegate_alias; operations} as delegate_ops) ->
              match
                List.partition
                  (fun (pkh, _) -> Signature.Public_key_hash.equal pkh delegate)
                  missing
              with
              | ((_, new_operations) :: _, missing') ->
                  ( Delegate_operations.
                      {
                        delegate;
                        delegate_alias;
                        operations = merge_operations operations new_operations;
                      }
                    :: acc,
                    missing' )
              | ([], _) -> (delegate_ops :: acc, missing))
            ([], received_ops)
            infos.delegate_operations
        in
        let* delegate_operations =
          match unknown with
          | [] -> return updated_known
          | _ :: _ ->
              let* aliases = Wallet.of_context cctxt in
              return
                (List.fold_left
                   (fun acc (delegate, ops) ->
                     Delegate_operations.
                       {
                         delegate;
                         delegate_alias = Wallet.alias_of_pkh aliases delegate;
                         operations =
                           List.rev_map
                             (fun Consensus_ops.
                                    {kind; round; errors; reception_time} ->
                               {
                                 kind;
                                 round;
                                 errors;
                                 reception_time = Some reception_time;
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
          {blocks = infos.blocks; delegate_operations; unaccurate}
        in
        let* () = write filename encoding out_infos in
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
      * Signature.Public_key_hash.t
      * Consensus_ops.block_info
  | Mempool of bool option * Int32.t (* level *) * Consensus_ops.delegate_ops

let (chunk_stream, chunk_feeder) = Lwt_stream.create ()

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

let add_received ?unaccurate level items =
  chunk_feeder (Some (Mempool (unaccurate, level, items)))

let add_block ~level block_hash ~round timestamp reception_time baker block_info
    =
  chunk_feeder
    (Some
       (Block
          ( level,
            block_hash,
            round,
            timestamp,
            reception_time,
            baker,
            block_info )))
