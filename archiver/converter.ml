(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let ensure_delegates_are_there db data =
  let addresses =
    List.map
      (fun block ->
        (block.Data.Block.delegate, block.Data.Block.delegate_alias))
      data.Data.blocks
    @ List.map
        (fun ops ->
          ( ops.Data.Delegate_operations.delegate,
            ops.Data.Delegate_operations.delegate_alias ))
        data.Data.delegate_operations
  in
  let query =
    Format.asprintf
      "INSERT OR IGNORE INTO delegates (address,alias) VALUES %a;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (add, al) ->
           Format.fprintf
             f
             "(x'%a',%a)"
             Hex.pp
             (Signature.Public_key_hash.to_hex add)
             (Format.pp_print_option
                ~none:(fun f () -> Format.pp_print_string f "NULL")
                (fun f x -> Format.fprintf f "'%s'" x))
             al))
      addresses
  in
  Db.exec db query

let register_rights db level data =
  (*fake first_slot,fake endorsing_power*)
  let query =
    Format.asprintf
      "INSERT INTO endorsing_rights \
       (level,delegate,first_slot,endorsing_power) SELECT %s,delegates.id,1,1 \
       FROM delegates JOIN (VALUES %a) ON delegates.address = column1;"
      level
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f ops ->
           Format.fprintf
             f
             "(x'%a')"
             Hex.pp
             (Signature.Public_key_hash.to_hex
                ops.Data.Delegate_operations.delegate)))
      data.Data.delegate_operations
  in
  Db.exec db query

let register_block db level data source =
  let query =
    Format.asprintf
      "INSERT OR IGNORE INTO blocks (timestamp,hash,level,round,baker) SELECT \
       column1,column2,%s,column4,delegates.id FROM delegates JOIN (VALUES %a) \
       ON delegates.address = column3;"
      level
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f block ->
           Format.fprintf
             f
             "('%a',x'%a',x'%a',%li)"
             Time.Protocol.pp
             block.Data.Block.timestamp
             Hex.pp
             (Block_hash.to_hex block.Data.Block.hash)
             Hex.pp
             (Signature.Public_key_hash.to_hex block.Data.Block.delegate)
             block.Data.Block.round))
      data.Data.blocks
  in
  Db.exec db query ;
  let query =
    Format.asprintf
      "INSERT INTO blocks_reception (timestamp,block,source) SELECT \
       column1,blocks.id,nodes.id FROM blocks JOIN (VALUES %a) ON blocks.hash \
       = column2 JOIN nodes ON nodes.name = '%s';"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f block ->
           Format.fprintf
             f
             "('%a',x'%a')"
             Time.System.pp_hum
             block.Data.Block.reception_time
             Hex.pp
             (Block_hash.to_hex block.Data.Block.hash)))
      data.Data.blocks
      source
  in
  Db.exec db query

let register_ops =
  (* fake hash *)
  let fake_hash = ref 0 in
  fun db level data source ->
    let known_ops =
      List.rev_concat_map
        (fun del ->
          List.rev_map
            (fun op ->
              Data.Delegate_operations.(op.kind, del.delegate, op.round))
            del.Data.Delegate_operations.operations)
        data.Data.delegate_operations
    in
    let query =
      Format.asprintf
        "INSERT INTO operations (endorsement,level,round,endorser,hash) SELECT \
         column1,%s,column3,delegates.id,column4 FROM delegates JOIN (VALUES \
         %a) ON delegates.address = column2;"
        level
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
           (fun f (kind, delegate, round) ->
             incr fake_hash ;
             Format.fprintf
               f
               "(%d,x'%a',%a,x'%044x')"
               (Sql_requests.bool_to_int (kind = Consensus_ops.Endorsement))
               Hex.pp
               (Signature.Public_key_hash.to_hex delegate)
               (Format.pp_print_option
                  ~none:(fun f () -> Format.pp_print_string f "NULL")
                  (fun f x -> Format.fprintf f "%li" x))
               round
               !fake_hash))
        known_ops
    in
    Db.exec db query ;
    let received_ops =
      List.rev_concat_map
        (fun del ->
          List.rev_filter_map
            (fun op ->
              Option.map
                (fun reception ->
                  Data.Delegate_operations.
                    (reception, op.errors, del.delegate, op.round, op.kind))
                op.Data.Delegate_operations.reception_time)
            del.Data.Delegate_operations.operations)
        data.Data.delegate_operations
    in
    let query =
      Format.asprintf
        "INSERT INTO operations_reception (timestamp,operation,source,errors) \
         SELECT column1,operations.id,nodes.id,column2 FROM operations JOIN \
         delegates ON operations.endorser = delegates.id JOIN (VALUES %a) ON \
         delegates.address = column3 AND ((operations.round IS NULL AND \
         column4 IS NULL) OR operations.round = column4) AND \
         operations.endorsement = column5 JOIN nodes ON nodes.name = '%s' \
         WHERE operations.level = %s;"
        (Format.pp_print_list
           ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
           (fun f (timestamp, errors, delegate, round, kind) ->
             Format.fprintf
               f
               "('%a',%a,x'%a',%a,%d)"
               Time.System.pp_hum
               timestamp
               (Format.pp_print_option
                  ~none:(fun f () -> Format.pp_print_string f "NULL")
                  (fun f x ->
                    Format.fprintf
                      f
                      "x'%a'"
                      Hex.pp
                      (Hex.of_bytes
                         (Data_encoding.Binary.to_bytes_exn
                            (Data_encoding.list Error_monad.error_encoding)
                            x))))
               errors
               Hex.pp
               (Signature.Public_key_hash.to_hex delegate)
               (Format.pp_print_option
                  ~none:(fun f () -> Format.pp_print_string f "NULL")
                  (fun f x -> Format.fprintf f "%li" x))
               round
               (Sql_requests.bool_to_int (kind = Consensus_ops.Endorsement))))
        received_ops
        source
        level
    in
    Db.exec db query

let register_ops_inclusion db level data =
  let included_ops =
    List.rev_concat_map
      (fun del ->
        List.rev_concat_map
          (fun op ->
            List.map
              (fun block ->
                Data.Delegate_operations.(block, del.delegate, op.round, op.kind))
              op.Data.Delegate_operations.block_inclusion)
          del.Data.Delegate_operations.operations)
      data.Data.delegate_operations
  in
  let query =
    Format.asprintf
      "INSERT INTO operations_inclusion (block,operation) SELECT \
       blocks.id,operations.id FROM operations JOIN delegates ON \
       operations.endorser = delegates.id JOIN blocks JOIN (VALUES %a) ON \
       delegates.address = column2 AND ((operations.round IS NULL AND column3 \
       IS NULL) OR operations.round = column3) AND operations.endorsement = \
       column4 AND blocks.hash = column1 WHERE operations.level = %s;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (block, delegate, round, kind) ->
           Format.fprintf
             f
             "(x'%a',x'%a',%a,%d)"
             Hex.pp
             (Block_hash.to_hex block)
             Hex.pp
             (Signature.Public_key_hash.to_hex delegate)
             (Format.pp_print_option
                ~none:(fun f () -> Format.pp_print_string f "NULL")
                (fun f x -> Format.fprintf f "%li" x))
             round
             (Sql_requests.bool_to_int (kind = Consensus_ops.Endorsement))))
      included_ops
      level
  in
  Db.exec db query

let main source prefix db_file =
  let db = Sqlite3.db_open db_file in
  Db.exec db Sql_requests.db_schema ;
  Db.ensure_source_is_there db source ;
  let*! () =
    Array.fold_left
      (fun acc subdir ->
        let subdir = Filename.concat prefix subdir in
        if Sys.is_directory subdir then
          Array.fold_left
            (fun acc filename ->
              if Filename.check_suffix filename "json" then
                let level = Filename.chop_extension filename in
                let filename = Filename.concat subdir filename in
                let*! () = acc in
                let*! data =
                  let* json = Lwt_utils_unix.Json.read_file filename in
                  try return (Data_encoding.Json.destruct Data.encoding json)
                  with exn -> Lwt.return (Error_monad.error_with_exn exn)
                in
                match data with
                | Ok data ->
                    if not data.Data.unaccurate then (
                      ensure_delegates_are_there db data ;
                      register_rights db level data ;
                      register_block db level data source ;
                      register_ops db level data source ;
                      register_ops_inclusion db level data) ;
                    Lwt.return_unit
                | Error err ->
                    Lwt_io.printl
                      (Format.asprintf
                         "@[File %s does not parse :@ @[%a@]@]"
                         filename
                         Error_monad.pp_print_trace
                         err)
              else Lwt.return_unit)
            acc
            (Sys.readdir subdir)
        else Lwt.return_unit)
      Lwt.return_unit
      (Sys.readdir prefix)
  in
  return_unit
