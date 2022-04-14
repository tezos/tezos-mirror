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

module Endorsement = struct
  type t = {
    delegate : Signature.public_key_hash;
    delegate_alias : string option;
    reception_time : Time.System.t option;
    errors : error list option;
    block_inclusion : Block_hash.t list;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {delegate; delegate_alias; reception_time; errors; block_inclusion} ->
        (delegate, delegate_alias, reception_time, errors, block_inclusion))
      (fun (delegate, delegate_alias, reception_time, errors, block_inclusion) ->
        {delegate; delegate_alias; reception_time; errors; block_inclusion})
      (obj5
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (opt "reception_time" Time.System.encoding)
         (opt "errors" (list error_encoding))
         (dft "included_in_blocks" (list Block_hash.encoding) []))
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
  type problem = Missed | Forgotten | Sequestered | Incorrect

  type t = {
    level : Int32.t;
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
      (fun {level; delegate; delegate_alias; problem} ->
        (level, delegate, delegate_alias, problem))
      (fun (level, delegate, delegate_alias, problem) ->
        {level; delegate; delegate_alias; problem})
      (obj4
         (req "level" int32)
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
  endorsements : Endorsement.t list;
  unaccurate : bool;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {blocks; endorsements; unaccurate} ->
      (blocks, endorsements, unaccurate))
    (fun (blocks, endorsements, unaccurate) ->
      {blocks; endorsements; unaccurate})
    (obj3
       (dft "blocks" (list Block.encoding) [])
       (dft "endorsements" (list Endorsement.encoding) [])
       (dft "unaccurate" bool false))

let empty = {blocks = []; endorsements = []; unaccurate = true}

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
        (fun acc
             Endorsement.
               {
                 delegate;
                 delegate_alias;
                 reception_time;
                 errors;
                 block_inclusion;
               } ->
          match errors with
          | Some (_ :: _) ->
              Anomaly.
                {level; delegate; delegate_alias; problem = Anomaly.Incorrect}
              :: acc
          | None | Some [] -> (
              match (reception_time, block_inclusion) with
              | (None, []) ->
                  Anomaly.
                    {level; delegate; delegate_alias; problem = Anomaly.Missed}
                  :: acc
              | (Some _, []) ->
                  Anomaly.
                    {
                      level;
                      delegate;
                      delegate_alias;
                      problem = Anomaly.Forgotten;
                    }
                  :: acc
              | (None, _ :: _) ->
                  Anomaly.
                    {
                      level;
                      delegate;
                      delegate_alias;
                      problem = Anomaly.Sequestered;
                    }
                  :: acc
              | (Some _, _ :: _) -> acc))
        []
        infos.endorsements
    in
    match anomalies with
    | [] -> return_unit
    | _ :: _ -> dump_anomalies path level anomalies

let dump_included_in_block cctxt path block_level block_hash block_round
    timestamp reception_time baker endorsers_pkhs =
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
       let (updated_known, unknown) =
         List.fold_left
           (fun (acc, missing)
                Endorsement.(
                  {
                    delegate;
                    delegate_alias;
                    reception_time;
                    errors;
                    block_inclusion;
                  } as en) ->
             match
               List.partition
                 (fun pkh -> Signature.Public_key_hash.equal pkh delegate)
                 missing
             with
             | (_ :: _, missing') ->
                 ( Endorsement.
                     {
                       delegate;
                       delegate_alias;
                       reception_time;
                       errors;
                       block_inclusion = block_hash :: block_inclusion;
                     }
                   :: acc,
                   missing' )
             | ([], _) -> (en :: acc, missing))
           ([], endorsers_pkhs)
           infos.endorsements
       in
       let endorsements =
         match unknown with
         | [] -> updated_known
         | _ :: _ ->
             List.fold_left
               (fun acc delegate ->
                 Endorsement.
                   {
                     delegate;
                     delegate_alias = Wallet.alias_of_pkh aliases delegate;
                     reception_time = None;
                     errors = None;
                     block_inclusion = [block_hash];
                   }
                 :: acc)
               updated_known
               unknown
       in
       let out_infos =
         {blocks = infos.blocks; endorsements; unaccurate = infos.unaccurate}
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
            "@[Failed to dump endorsements in block %a at level %li :@ @[%a@]@]"
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
          endorsements = infos.endorsements;
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

let dump_received cctxt path ?unaccurate level items =
  let filename = filename_of_level path level in
  let mutex = get_file_mutex filename in
  let*! out =
    Lwt_mutex.with_lock mutex (fun () ->
        let* infos = load filename encoding empty in
        let (updated_known, unknown) =
          List.fold_left
            (fun (acc, missing)
                 Endorsement.(
                   {
                     delegate;
                     delegate_alias;
                     reception_time;
                     errors;
                     block_inclusion;
                   } as en) ->
              match
                List.partition
                  (fun (pkh, _, _) ->
                    Signature.Public_key_hash.equal pkh delegate)
                  missing
              with
              | ((_, err, time) :: _, missing') ->
                  let (reception_time, errors) =
                    match reception_time with
                    | Some _ -> (reception_time, errors)
                    | None -> (time, err)
                  in
                  ( Endorsement.
                      {
                        delegate;
                        delegate_alias;
                        reception_time;
                        errors;
                        block_inclusion;
                      }
                    :: acc,
                    missing' )
              | ([], _) -> (en :: acc, missing))
            ([], items)
            infos.endorsements
        in
        let* endorsements =
          match unknown with
          | [] -> return updated_known
          | _ :: _ ->
              let* aliases = Wallet.of_context cctxt in
              (* let aliases = match out with
                 | Ok aliases -> aliases
                  | Error _err -> StringMap.empty in*)
              return
                (List.fold_left
                   (fun acc (delegate, errors, reception_time) ->
                     Endorsement.
                       {
                         delegate;
                         delegate_alias = Wallet.alias_of_pkh aliases delegate;
                         reception_time;
                         errors;
                         block_inclusion = [];
                       }
                     :: acc)
                   updated_known
                   unknown)
        in
        let unaccurate = Option.value ~default:infos.unaccurate unaccurate in
        let out_infos = {blocks = infos.blocks; endorsements; unaccurate} in
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
           "@[Failed to dump endorsements at level %li :@ @[%a@]@]"
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
      * Signature.Public_key_hash.t list
  | Mempool of
      bool option
      * Int32.t
      * (Signature.Public_key_hash.t * error list option * Time.System.t option)
        list

let (chunk_stream, chunk_feeder) = Lwt_stream.create ()

let launch cctxt prefix =
  Lwt_stream.iter_p
    (function
      | Block
          (block_level, block, round, timestamp, reception_time, delegate, pkhs)
        ->
          dump_included_in_block
            cctxt
            prefix
            block_level
            block
            round
            timestamp
            reception_time
            delegate
            pkhs
      | Mempool (unaccurate, level, items) ->
          dump_received cctxt prefix ?unaccurate level items)
    chunk_stream

let stop () = chunk_feeder None

let add_received ?unaccurate level items =
  chunk_feeder (Some (Mempool (unaccurate, level, items)))

let add_block ~level block_hash ~round timestamp reception_time delegate pkhs =
  chunk_feeder
    (Some
       (Block
          (level, block_hash, round, timestamp, reception_time, delegate, pkhs)))
