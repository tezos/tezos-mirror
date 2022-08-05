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
  exec db (Sql_requests.maybe_insert_endorsing_rights ~level rights)

let register_included_operations db block_hash level operations =
  exec db (Sql_requests.maybe_insert_operations_from_block ~level operations) ;
  exec db (Sql_requests.insert_included_operations block_hash ~level operations)

let register_received_operations db level
    (operations : Consensus_ops.delegate_ops) source =
  exec db (Sql_requests.maybe_insert_operations_from_received ~level operations) ;
  exec db (Sql_requests.insert_received_operations ~source ~level operations)

let register_block ~db hash ~level ~round timestamp reception_time delegate
    block_ops source =
  exec db (Sql_requests.insert_block hash ~level ~round timestamp delegate) ;
  exec db (Sql_requests.insert_received_block ~source hash reception_time) ;

  (*** insert into operations and operations_inclusion *)
  register_included_operations db hash (Int32.pred level) block_ops

type chunk =
  | Block of
      Int32.t (* level *)
      * Block_hash.t
      * Int32.t (* round *)
      * Time.Protocol.t
      * Time.System.t
      * Signature.Public_key_hash.t
      * Consensus_ops.block_op list
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
