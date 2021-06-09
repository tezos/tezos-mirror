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

let levels_per_folder = 4096l

module Endorsement = struct
  type t =
    { delegate : Signature.public_key_hash;
      reception_time : Time.System.t option;
      errors : error list;
      block_inclusion : Block_hash.t list
    }

  let encoding =
    let open Data_encoding in
    conv
      (fun { delegate; reception_time; errors; block_inclusion } ->
        (delegate, reception_time, errors, block_inclusion))
      (fun (delegate, reception_time, errors, block_inclusion) ->
        { delegate; reception_time; errors; block_inclusion })
      (obj4
         (req "delegate" Signature.Public_key_hash.encoding)
         (opt "reception_time" Time.System.encoding)
         (dft "errors" (list error_encoding) [])
         (dft "included_in_blocks" (list Block_hash.encoding) []))
end

module Block = struct
  type t =
    { hash : Block_hash.t;
      timestamp : Time.Protocol.t;
      reception_time : Time.System.t;
      nonce : unit option
    }

  let encoding =
    let open Data_encoding in
    conv
      (fun { hash; reception_time; timestamp; nonce } ->
        (hash, reception_time, timestamp, nonce))
      (fun (hash, reception_time, timestamp, nonce) ->
        { hash; reception_time; timestamp; nonce })
      (obj4
         (req "hash" Block_hash.encoding)
         (req "reception_time" Time.System.encoding)
         (req "timestamp" Time.Protocol.encoding)
         (opt "nonce" unit))
end

module Anomaly = struct
  type problem = Missed | Forgotten | Sequestered | Incorrect

  type t =
    { level : Int32.t;
      delegate : Signature.Public_key_hash.t;
      problem : problem
    }

  let problem_encoding =
    Data_encoding.string_enum
      [ ("missed", Missed);
        ("forgotten", Forgotten);
        ("sequestered", Sequestered);
        ("incorrect", Incorrect) ]

  let encoding =
    let open Data_encoding in
    conv
      (fun { level; delegate; problem } -> (level, delegate, problem))
      (fun (level, delegate, problem) -> { level; delegate; problem })
      (obj3
         (req "level" int32)
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "problem" problem_encoding))

  let rec insert_in_ordered_list ({ level; delegate; _ } as anomaly) = function
    | [] -> [anomaly]
    | head :: tail as l ->
        if Compare.Int32.(head.level < level) then anomaly :: l
        else if
          Int32.equal head.level level
          && Signature.Public_key_hash.equal head.delegate delegate
        then if head.problem = Missed then anomaly :: tail else l
        else head :: insert_in_ordered_list anomaly tail
end

type t =
  { blocks : Block.t list;
    endorsements : Endorsement.t list;
    unaccurate : bool
  }

let encoding =
  let open Data_encoding in
  conv
    (fun { blocks; endorsements; unaccurate } ->
      (blocks, endorsements, unaccurate))
    (fun (blocks, endorsements, unaccurate) ->
      { blocks; endorsements; unaccurate })
    (obj3
       (dft "blocks" (list Block.encoding) [])
       (dft "endorsements" (list Endorsement.encoding) [])
       (dft "unaccurate" bool false))

let empty = { blocks = []; endorsements = []; unaccurate = true }

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
  Lwt_unix.file_exists filename >>= function
  | false -> return empty
  | true -> (
      Lwt_utils_unix.Json.read_file filename >>=? fun json ->
      try return (Data_encoding.Json.destruct encoding json)
      with exn -> Lwt.return (Error_monad.error_exn exn) )

let write filename encoding value =
  Lwt_utils_unix.create_dir (Filename.dirname filename) >>= fun () ->
  Lwt_utils_unix.Json.write_file
    filename
    (Data_encoding.Json.construct encoding value)

let files_in_use = ref []

let get_file_mutex filename =
  match List.assoc_opt filename !files_in_use with
  | None ->
      let x = Lwt_mutex.create () in
      let () = files_in_use := (filename, x) :: !files_in_use in
      x
  | Some x -> x

let drop_file_mutex filename =
  files_in_use := List.remove_assoc filename !files_in_use

let dump_anomalies path level anomalies =
  let filename =
    Filename.concat (dirname_of_level path level) "anomalies.json"
  in
  let mutex = get_file_mutex filename in
  Lwt_mutex.with_lock mutex (fun () ->
      load filename (Data_encoding.list Anomaly.encoding) [] >>=? fun known ->
      write
        filename
        (Data_encoding.list Anomaly.encoding)
        (List.fold_left
           (fun x y -> Anomaly.insert_in_ordered_list y x)
           known
           anomalies))
  >|= fun out ->
  let () = drop_file_mutex filename in
  out

let extract_anomalies path level infos =
  if infos.unaccurate then return_unit
  else
    let anomalies =
      List.fold_left
        (fun acc
             Endorsement.{ delegate; reception_time; errors; block_inclusion } ->
          match errors with
          | _ :: _ ->
              Anomaly.{ level; delegate; problem = Anomaly.Incorrect } :: acc
          | [] -> (
              match (reception_time, block_inclusion) with
              | (None, []) ->
                  Anomaly.{ level; delegate; problem = Anomaly.Missed } :: acc
              | (Some _, []) ->
                  Anomaly.{ level; delegate; problem = Anomaly.Forgotten }
                  :: acc
              | (None, _ :: _) ->
                  Anomaly.{ level; delegate; problem = Anomaly.Sequestered }
                  :: acc
              | (Some _, _ :: _) -> acc ))
        []
        infos.endorsements
    in
    match anomalies with
    | [] -> return_unit
    | _ :: _ -> dump_anomalies path level anomalies

let dump_included_in_block path block_level block_hash timestamp reception_time
    endorsers_pkhs =
  let open Lwt.Infix in
  (let endorsements_level = Int32.pred block_level in
   let filename = filename_of_level path endorsements_level in
   let mutex = get_file_mutex filename in
   Lwt_mutex.with_lock mutex (fun () ->
       load filename encoding empty >>=? fun infos ->
       let (updated_known, unknown) =
         List.fold_left
           (fun (acc, missing)
                Endorsement.(
                  { delegate; reception_time; errors; block_inclusion } as en) ->
             match
               List.partition
                 (fun pkh -> Signature.Public_key_hash.equal pkh delegate)
                 missing
             with
             | (_ :: _, missing') ->
                 ( Endorsement.
                     { delegate;
                       reception_time;
                       errors;
                       block_inclusion = block_hash :: block_inclusion
                     }
                   :: acc,
                   missing' )
             | ([], _) -> (en :: acc, missing))
           ([], endorsers_pkhs)
           infos.endorsements
       in
       let endorsements =
         List.fold_left
           (fun acc delegate ->
             Endorsement.
               { delegate;
                 reception_time = None;
                 errors = [];
                 block_inclusion = [block_hash]
               }
             :: acc)
           updated_known
           unknown
       in
       let out_infos =
         { blocks = infos.blocks; endorsements; unaccurate = infos.unaccurate }
       in
       write filename encoding out_infos >>=? fun () ->
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
            Error_monad.pp_print_error
            err))
  <&>
  let filename = filename_of_level path block_level in
  let mutex = get_file_mutex filename in
  Lwt_mutex.with_lock mutex (fun () ->
      load filename encoding empty >>=? fun infos ->
      let blocks =
        Block.{ hash = block_hash; reception_time; timestamp; nonce = None }
        :: infos.blocks
      in
      write
        filename
        encoding
        { blocks;
          endorsements = infos.endorsements;
          unaccurate = infos.unaccurate
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
           Error_monad.pp_print_error
           err)

let dump_received path ?unaccurate level items =
  let filename = filename_of_level path level in
  let mutex = get_file_mutex filename in
  Lwt_mutex.with_lock mutex (fun () ->
      Lwt_io.printlf "Locked %s" filename >>= fun () ->
      load filename encoding empty >>=? fun infos ->
      let (updated_known, unknown) =
        List.fold_left
          (fun (acc, missing)
               Endorsement.(
                 { delegate; reception_time; errors; block_inclusion } as en) ->
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
                    { delegate; reception_time; errors; block_inclusion }
                  :: acc,
                  missing' )
            | ([], _) -> (en :: acc, missing))
          ([], items)
          infos.endorsements
      in
      let endorsements =
        List.fold_left
          (fun acc (delegate, errors, reception_time) ->
            Endorsement.
              { delegate; reception_time; errors; block_inclusion = [] }
            :: acc)
          updated_known
          unknown
      in
      let unaccurate = Option.value ~default:infos.unaccurate unaccurate in
      let out_infos = { blocks = infos.blocks; endorsements; unaccurate } in
      write filename encoding out_infos >>=? fun () ->
      extract_anomalies path level out_infos)
  >>= fun out ->
  let () = drop_file_mutex filename in
  match out with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Lwt_io.printl
        (Format.asprintf
           "@[Failed to dump endorsements at level %li :@ @[%a@]@]"
           level
           Error_monad.pp_print_error
           err)

type chunk =
  | Block of
      Int32.t
      * Block_hash.t
      * Time.Protocol.t
      * Time.System.t
      * Signature.Public_key_hash.t list
  | Mempool of
      bool option
      * Int32.t
      * (Signature.Public_key_hash.t * error list * Time.System.t option) list

let (chunk_stream, chunk_feeder) = Lwt_stream.create ()

let launch prefix =
  Lwt_stream.iter_p
    (function
      | Block (block_level, block, timestamp, reception_time, pkhs) ->
          dump_included_in_block
            prefix
            block_level
            block
            timestamp
            reception_time
            pkhs
      | Mempool (unaccurate, level, items) ->
          dump_received prefix ?unaccurate level items)
    chunk_stream

let stop () = chunk_feeder None

let add_received ?unaccurate level items =
  chunk_feeder (Some (Mempool (unaccurate, level, items)))

let add_block block_level block_hash timestamp reception_time pkhs =
  chunk_feeder
    (Some (Block (block_level, block_hash, timestamp, reception_time, pkhs)))
