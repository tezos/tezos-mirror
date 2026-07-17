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

(* Maximum time (seconds) to wait for a single POST (request + response read)
   to a teztale-server before giving up. Without a timeout, a server that
   accepts the connection but then stalls (e.g. blocked on its database) makes
   the send hang forever, leaking the connection; enough such hangs wedge the
   archiver entirely. On timeout we raise, so the chunk goes through the usual
   failure path ([backup]) instead of hanging silently. *)
let request_timeout = ref 30.

let set_request_timeout t = request_timeout := t

(* Upper bound on the number of POSTs in flight at once, bounding how many
   concurrent connections we open to a slow or stalled server. Note this does
   not bound the [chunk_stream] backlog: pending chunks still queue there. *)
let max_concurrent_sends = 8

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
        Lwt_unix.with_timeout !request_timeout @@ fun () ->
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

(* Replay of backed-up POSTs. When --replay-backups is set, a background loop
   periodically re-sends the records left by [backup_post] and deletes each only
   after the server accepts it (HTTP OK). Server inserts are idempotent, so
   re-sending a record the server already stored is harmless; this keeps the
   on-disk backup from growing without bound once the server is reachable. *)

let replay_interval = 60.

let replay_record ctx file =
  let logger = Log.logger () in
  Lwt.catch
    (fun () ->
      let*! json = Lwt_utils_unix.Json.read_file file in
      match json with
      | Error _ -> Lwt.return_unit (* unreadable record: leave it in place *)
      | Ok json ->
          let path = Ezjsonm.get_string (Ezjsonm.find json ["path"]) in
          let body = Ezjsonm.get_string (Ezjsonm.find json ["body"]) in
          (* send_something raises on a non-OK / failed POST *)
          let*! () = send_something ctx path (`String body) in
          let*! () = Lwt_unix.unlink file in
          Log.debug logger (fun () -> "Replayed and purged " ^ path) ;
          Lwt.return_unit)
    (function
      (* Expected failures -- POST rejected ([Failure]), server unreachable
         ([Unix_error]), timeout, or a malformed record ([Not_found] from a
         missing key, [Parse_error] from a wrong-typed value) -- leave the file
         in place for the next sweep. Anything else is unexpected and propagates
         rather than being silently swallowed. *)
      | Lwt_unix.Timeout | Failure _ | Unix.Unix_error _ | Not_found
      | Ezjsonm.Parse_error _ ->
          Lwt.return_unit
      | exn -> Lwt.reraise exn)

let replay_once ctx dir =
  let*! exists = Lwt_unix.file_exists dir in
  if not exists then Lwt.return_unit
  else
    (* [files_of_directory] yields a stream we drain sequentially, which plays
       nicely with the Lwt scheduler (unlike the blocking [Sys.readdir]). *)
    Lwt_stream.iter_s
      (fun f ->
        if Filename.check_suffix f ".json" then
          replay_record ctx (Filename.concat dir f)
        else Lwt.return_unit)
      (Lwt_unix.files_of_directory dir)

let rec replay_backups_loop ctx dir =
  let*! () =
    Lwt.catch (fun () -> replay_once ctx dir) (fun _ -> Lwt.return_unit)
  in
  let*! () = Lwt_unix.sleep replay_interval in
  replay_backups_loop ctx dir

let launch actx _source =
  Lwt_stream.iter_n
    ~max_concurrency:max_concurrent_sends
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
