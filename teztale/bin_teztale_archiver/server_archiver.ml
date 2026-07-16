(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_result_syntax

type endpoint = {auth : string * string; endpoint : Uri.t}

type chunk =
  | Block of
      Int32.t (* level *)
      * (Data.Block.t
        * Data.cycle_info option
        * (Consensus_ops.block_op list (* endos *)
          * Consensus_ops.block_op list (* preendos *))
        * Data.baking_right list)
  | Mempool of Int32.t (* level *) * Consensus_ops.delegate_ops
  | Rights of (Int32.t (* level *) * Consensus_ops.rights)
  | Dal_shards of Int32.t (* level *) * Data.Dal.shard_assignment list

type ctx = {
  cohttp_ctx : Cohttp_lwt_unix.Net.ctx;
  endpoints : endpoint list;
  backup : chunk -> unit Lwt.t;
}

type t = ctx

let name = "remote-archiver"

let extract_auth uri =
  let user = Option.value (Uri.user uri) ~default:"archiver" in
  let pass = Option.value (Uri.password uri) ~default:"secret" in
  {auth = (user, pass); endpoint = Uri.with_uri ~userinfo:None uri}

let send_something =
  let sent = ref 0 in
  let received = ref 0 in
  fun ctx path body ->
    let headers =
      Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
    in
    Lwt_list.iter_p
      (fun {auth; endpoint} ->
        let logger = Log.logger () in
        let headers = Cohttp.Header.add_authorization headers (`Basic auth) in
        let uri = Uri.with_path endpoint (Uri.path endpoint ^ "/" ^ path) in
        incr sent ;
        let*! resp, out =
          Log.debug logger (fun () -> "Sending " ^ path) ;
          Cohttp_lwt_unix.Client.post ~ctx:ctx.cohttp_ctx ~body ~headers uri
        in
        incr received ;
        let*! out = Cohttp_lwt.Body.to_string out in
        let msg =
          Printf.sprintf
            "%s: %s (%d/%d requests treated)"
            (Cohttp.Code.string_of_status resp.status)
            out
            !received
            !sent
        in
        if resp.status = `OK then
          let () = Log.debug logger (fun () -> msg) in
          Lwt.return_unit
        else
          let () = Log.info logger (fun () -> msg) in
          Lwt.fail_with msg)
      ctx.endpoints

(* The request path and JSON body of the POST that ships a given chunk to the
   teztale-server. Shared by [send] (live path) and [backup_post] (on failure)
   so every chunk kind -- rights and dal_shards included -- is handled
   uniformly. *)
let chunk_to_post = function
  | Block (level, block_data) ->
      ( Int32.to_string level ^ "/block",
        Ezjsonm.value_to_string
          (Data_encoding.Json.construct
             Data.Archiver.raw_block_data_encoding
             block_data) )
  | Mempool (level, ops) ->
      ( Int32.to_string level ^ "/mempool",
        Ezjsonm.value_to_string
          (Data_encoding.Json.construct Consensus_ops.delegate_ops_encoding ops)
      )
  | Rights (level, rights) ->
      ( Int32.to_string level ^ "/rights",
        Ezjsonm.value_to_string
          (Data_encoding.Json.construct Consensus_ops.rights_encoding rights) )
  | Dal_shards (level, shard_assignments) ->
      ( Int32.to_string level ^ "/dal_shards",
        Ezjsonm.value_to_string
          (Data_encoding.Json.construct
             Data.Dal.shard_assignments_encoding
             shard_assignments) )

let chunk_stream, chunk_feeder = Lwt_stream.create ()

let send actx chunk =
  let path, body = chunk_to_post chunk in
  send_something actx path (`String body)

(* On send failure, when --backup-dir is set, persist the failed POST verbatim
   (path + JSON body) as one record per file. Unlike the previous json-archiver
   dump, this preserves every chunk kind -- rights and dal_shards included -- so
   no data is silently dropped, and the record can later be replayed as-is. *)
let backup_post dir chunk =
  let logger = Log.logger () in
  let path, body = chunk_to_post chunk in
  Lwt.catch
    (fun () ->
      let*! () = Lwt_utils_unix.create_dir dir in
      let file = Filename.temp_file ~temp_dir:dir "post-" ".json" in
      let*! out =
        Lwt_utils_unix.Json.write_file
          file
          (`O [("path", `String path); ("body", `String body)])
      in
      match out with
      | Ok () -> Lwt.return_unit
      | Error err ->
          Log.error logger (fun () ->
              Format.asprintf
                "Failed to back up %s data: %a"
                path
                Error_monad.pp_print_trace
                err) ;
          Lwt.return_unit)
    (fun exn ->
      Log.error logger (fun () ->
          Format.asprintf
            "Failed to back up %s data: %s"
            path
            (Printexc.to_string exn)) ;
      Lwt.return_unit)

let launch actx _source =
  Lwt_stream.iter_p
    (fun chunk ->
      Lwt.catch (fun () -> send actx chunk) (fun _ -> actx.backup chunk))
    chunk_stream

let stop () = chunk_feeder None

let add_mempool ?unaccurate:(_ : bool option) ~level items =
  chunk_feeder (Some (Mempool (level, items)))

let add_block ~level block = chunk_feeder (Some (Block (level, block)))

let add_rights ~level rights = chunk_feeder (Some (Rights (level, rights)))

let add_dal_shards ~level shard_assignments =
  chunk_feeder (Some (Dal_shards (level, shard_assignments)))
