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

open Db
module Sql = Sqlite3

type t = Sqlite3.db

(* TODO: use prepared statements instead of exec (and thus also use Sql.Data) *)

let register_rights db level rights aliases =
  let query =
    Format.asprintf
      "INSERT OR IGNORE INTO delegates (address, alias) VALUES %a;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f r ->
           let alias = Wallet.alias_of_pkh aliases r.Consensus_ops.address in
           Format.fprintf
             f
             "(x'%a', %a)"
             Hex.pp
             (Signature.Public_key_hash.to_hex r.Consensus_ops.address)
             (Format.pp_print_option
                ~none:(fun f () -> Format.pp_print_string f "NULL")
                (fun f x -> Format.fprintf f "'%s'" x))
             alias))
      rights
  in
  exec db query ;
  let query =
    Format.asprintf
      "INSERT OR IGNORE INTO endorsing_rights (level, delegate, first_slot, \
       endorsing_power) SELECT column1, delegates.id, column3, column4 FROM \
       delegates JOIN (VALUES %a) ON delegates.address = column2;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f Consensus_ops.{address; first_slot; power} ->
           Format.fprintf
             f
             "(%ld, x'%a', %d, %d)"
             level
             Hex.pp
             (Signature.Public_key_hash.to_hex address)
             first_slot
             power))
      rights
  in
  exec db query

let register_included_operations db block_hash level round ~endorsements
    operations =
  let query =
    Format.asprintf
      "INSERT INTO operations (hash, endorsement, endorser, level, round) \
       SELECT column1, %d, delegates.id, %ld, %ld FROM delegates JOIN (VALUES \
       %a) ON delegates.address = column2 WHERE column1 NOT IN (SELECT hash \
       FROM operations WHERE level = %ld);"
      (bool_to_int endorsements)
      level
      round
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (op : Consensus_ops.block_op) ->
           Format.fprintf
             f
             "(x'%a', x'%a')"
             Hex.pp
             (Operation_hash.to_hex op.hash)
             Hex.pp
             (Signature.Public_key_hash.to_hex op.delegate)))
      operations
      level
  in
  exec db query ;
  let query =
    Format.asprintf
      "INSERT INTO operations_inclusion (block, operation) SELECT blocks.id, \
       operations.id FROM operations, blocks, (VALUES %a) ON operations.hash = \
       column1 AND blocks.hash = x'%a' WHERE operations.level = %ld;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (op : Consensus_ops.block_op) ->
           Format.fprintf
             f
             "(x'%a')"
             Hex.pp
             (Operation_hash.to_hex op.Consensus_ops.hash)))
      operations
      Hex.pp
      (Block_hash.to_hex block_hash)
      level
  in
  exec db query

let register_received_operations db level
    (operations : Consensus_ops.delegate_ops) source =
  (* flatten the operations, that is, do not group by delegate *)
  let received_ops =
    List.fold_left
      (fun acc (delegate, delegate_ops) ->
        let aux = List.map (fun op -> (delegate, op)) delegate_ops in
        aux @ acc)
      []
      operations
  in
  let query =
    Format.asprintf
      "INSERT INTO operations (hash, endorsement, endorser, level, round) \
       SELECT column1, column2, delegates.id, %ld, column4 FROM delegates JOIN \
       (VALUES %a) ON delegates.address = column3 WHERE column1 NOT IN (SELECT \
       hash FROM operations WHERE level = %ld);"
      level
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (delegate, (op : Consensus_ops.received_operation)) ->
           Format.fprintf
             f
             "(x'%a', %d, x'%a', %a)"
             Hex.pp
             (Operation_hash.to_hex op.hash)
             (bool_to_int (op.kind = Consensus_ops.Endorsement))
             Hex.pp
             (Signature.Public_key_hash.to_hex delegate)
             (Format.pp_print_option
                ~none:(fun f () -> Format.pp_print_string f "NULL")
                (fun f x -> Format.fprintf f "%li" x))
             op.round))
      received_ops
      level
  in
  exec db query ;
  let query =
    Format.asprintf
      "INSERT INTO operations_reception (timestamp, operation, source, errors) \
       SELECT column1, operations.id, nodes.id, column2 FROM operations, \
       nodes, (VALUES %a) ON operations.hash = column3 AND nodes.name = '%s' \
       WHERE operations.level = %ld;"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_text f ", ")
         (fun f (_delegate, (op : Consensus_ops.received_operation)) ->
           Format.fprintf
             f
             "('%a', %a, x'%a')"
             Time.System.pp_hum
             op.reception_time
             (Format.pp_print_option
                ~none:(fun f () -> Format.pp_print_string f "NULL")
                (fun f errors ->
                  Format.fprintf
                    f
                    "x'%a'"
                    Hex.pp
                    (Hex.of_bytes
                       (Data_encoding.Binary.to_bytes_exn
                          (Data_encoding.list Error_monad.error_encoding)
                          errors))))
             op.errors
             Hex.pp
             (Operation_hash.to_hex op.hash)))
      received_ops
      source
      level
  in
  exec db query

let register_block ~db hash ~level ~round timestamp reception_time delegate
    block_info source =
  let query =
    Format.asprintf
      "INSERT INTO blocks (timestamp, hash, level, round, baker) SELECT \
       column1, column2, %ld, column4, delegates.id FROM delegates JOIN \
       (VALUES ('%a', x'%a', x'%a', %ld)) ON delegates.address = column3;"
      level
      Time.Protocol.pp
      timestamp
      Hex.pp
      (Block_hash.to_hex hash)
      Hex.pp
      (Signature.Public_key_hash.to_hex delegate)
      round
  in
  exec db query ;
  let query =
    Format.asprintf
      "INSERT INTO blocks_reception (timestamp, block, source) SELECT column1, \
       blocks.id, nodes.id FROM blocks JOIN (VALUES ('%a', x'%a')) ON \
       blocks.hash = column2 JOIN nodes ON nodes.name = '%s';"
      Time.System.pp_hum
      reception_time
      Hex.pp
      (Block_hash.to_hex hash)
      source
  in
  exec db query ;

  (*** insert into operations and operations_inclusion *)
  (match block_info.Consensus_ops.preendorsements with
  | Some preendorsements ->
      register_included_operations
        db
        hash
        level
        (Stdlib.Option.get block_info.preendorsements_round)
        ~endorsements:false
        preendorsements
  | None -> ()) ;
  register_included_operations
    db
    hash
    (Int32.pred level)
    (Stdlib.Option.get block_info.endorsements_round)
    ~endorsements:true
    block_info.endorsements

type chunk =
  | Block of
      Int32.t (* level *)
      * Block_hash.t
      * Int32.t (* round *)
      * Time.Protocol.t
      * Time.System.t
      * Signature.Public_key_hash.t
      * Consensus_ops.block_info
  | Mempool of bool option * Int32.t (* level *) * Consensus_ops.delegate_ops
  | Rights of (Int32.t (* level *) * Consensus_ops.rights * Wallet.t)

let (chunk_stream, chunk_feeder) = Lwt_stream.create ()

let launch db source =
  Lwt_stream.iter_p
    (fun event ->
      (try
         exec db "BEGIN TRANSACTION" ;
         (match event with
         | Block
             ( level,
               block_hash,
               round,
               timestamp,
               reception_time,
               baker,
               block_info ) ->
             ensure_source_is_there db source ;
             register_block
               ~db
               block_hash
               ~level
               ~round
               timestamp
               reception_time
               baker
               block_info
               source
         | Mempool (_unaccurate, level, ops) ->
             ensure_source_is_there db source ;
             register_received_operations db level ops source
         | Rights (level, rights, aliases) ->
             register_rights db level rights aliases) ;
         exec db "END TRANSACTION"
       with
      | Sql.InternalError str -> prerr_endline str
      | Sql.Error str -> Format.eprintf "Error: %s@." str
      | Sql.DataTypeError str -> Format.eprintf "DataTypeError: %s@." str
      | Sql.RangeError (a, b) -> Format.eprintf "DataTypeError: %d %d@." a b
      | Sql.SqliteError str -> Format.eprintf "SqliteError: %s@." str
      | excp -> prerr_endline (Printexc.to_string excp)) ;
      Lwt.return ())
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

let add_rights ~level rights aliases =
  chunk_feeder (Some (Rights (level, rights, aliases)))
