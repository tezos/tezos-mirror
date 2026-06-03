(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

module Events = Sqlite_compression_migration_events
open Sqlite

(* --- Target description --------------------------------------------------- *)

(* Hardcoded list of tables to migrate. Must mirror the set of columns
   wrapped in zstd_compress / zstd_decompress on the write/read path
   in [Evm_store.Q].

   Note: not every wrapped column needs a migration entry. The
   [blocks.tez_block] column is wrapped on writes/reads but is not
   migrated because it is unused — and therefore empty — on every
   production EVM node, so a row scan of [blocks] for it would just be
   wasted I/O. New writes go through [zstd_compress] regardless, so
   any future use of the column starts compressed.

   To add a new compressed column that needs migration:
   1. Wrap the column in [zstd_compress(?)] on writes and
      [zstd_decompress(col)] on reads in [Evm_store.Q].
   2. Add the table name here (or extend an existing entry if the
      table already appears).
   3. Add a per-table [Q.update_<table>] prepared statement following
      the shape of [update_blueprints] (single column) or
      [update_transactions] (multiple columns).
   4. Wire it into [run_batch_for_table], [min_rowid_query_for], and
      [max_rowid_query_for]. *)
let target_tables = ["blueprints"; "blocks"; "transactions"]

(* A special sentinel row kept in the cursor table to mark "the VACUUM +
   rename step also completed". Once all target tables and this sentinel
   have [done_ = 1], we have a persistent O(1) signal that compression
   is fully finished and startup can skip the whole migration path —
   no per-table linear scan, no redundant VACUUM. *)
let vacuum_marker = "__vacuum__"

let all_rows = vacuum_marker :: target_tables

(* --- Prepared statements -------------------------------------------------- *)

module Q = struct
  open Sqlite.Request

  let table = "compression_migration"

  let create_table =
    (Caqti_type.unit ->. Caqti_type.unit) ~name:__FUNCTION__ ~table
    @@ {|CREATE TABLE IF NOT EXISTS compression_migration (
           table_name TEXT PRIMARY KEY,
           cursor INTEGER NOT NULL DEFAULT 0,
           done_ INTEGER NOT NULL DEFAULT 0
         )|}

  let ensure_row =
    (Caqti_type.string ->. Caqti_type.unit) ~name:__FUNCTION__ ~table
    @@ {|INSERT OR IGNORE INTO compression_migration (table_name)
         VALUES (?)|}

  let get_state =
    (Caqti_type.string ->! Caqti_type.(t2 int bool)) ~name:__FUNCTION__ ~table
    @@ {|SELECT cursor, done_ FROM compression_migration
          WHERE table_name = ?|}

  let set_cursor =
    (Caqti_type.(t2 int string) ->. Caqti_type.unit) ~name:__FUNCTION__ ~table
    @@ {|UPDATE compression_migration SET cursor = ?
          WHERE table_name = ?|}

  let mark_done =
    (Caqti_type.string ->. Caqti_type.unit) ~name:__FUNCTION__ ~table
    @@ {|UPDATE compression_migration SET done_ = 1
          WHERE table_name = ?|}

  (* Count of rows in the cursor table that still have work pending —
     either a target table that isn't fully compressed yet, or the vacuum
     sentinel when the VACUUM + rename step hasn't finished. Used on
     startup to decide in O(1) whether to skip the migration entirely. *)
  let count_pending =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table
    @@ {|SELECT COUNT(*) FROM compression_migration WHERE done_ = 0|}

  (* Count of rows that already have progress recorded — either a
     completed table or one with a non-zero cursor. Non-zero means the
     current invocation is resuming a partially-finished migration from
     a previous run, rather than starting fresh. *)
  let count_in_progress =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table
    @@ {|SELECT COUNT(*) FROM compression_migration WHERE done_ = 1 OR cursor > 0|}

  (* [MIN(rowid)] / [MAX(rowid)] are both O(log N) on SQLite's rowid
     B-tree (no full scan), so we can cheaply bracket the live rowid
     range even on tables with tens of millions of rows. We need both
     because rolling/snapshot stores prune old rows but keep their
     rowids reserved — without [MIN(rowid)], the cursor enters the
     loop already at ~99% of [MAX(rowid)] and the displayed
     percentage is useless. [COALESCE] handles empty tables by
     reporting 0 for both bounds. *)
  let min_rowid_blueprints =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table:"blueprints"
    @@ {|SELECT COALESCE(MIN(rowid), 0) FROM blueprints|}

  let max_rowid_blueprints =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table:"blueprints"
    @@ {|SELECT COALESCE(MAX(rowid), 0) FROM blueprints|}

  let min_rowid_blocks =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table:"blocks"
    @@ {|SELECT COALESCE(MIN(rowid), 0) FROM blocks|}

  let max_rowid_blocks =
    (Caqti_type.unit ->! Caqti_type.int) ~name:__FUNCTION__ ~table:"blocks"
    @@ {|SELECT COALESCE(MAX(rowid), 0) FROM blocks|}

  let min_rowid_transactions =
    (Caqti_type.unit ->! Caqti_type.int)
      ~name:__FUNCTION__
      ~table:"transactions"
    @@ {|SELECT COALESCE(MIN(rowid), 0) FROM transactions|}

  let max_rowid_transactions =
    (Caqti_type.unit ->! Caqti_type.int)
      ~name:__FUNCTION__
      ~table:"transactions"
    @@ {|SELECT COALESCE(MAX(rowid), 0) FROM transactions|}

  (* Batch updates use the 1-arg form [zstd_compress(col)]; the level is
     read from the per-connection user-data slot set at register time. *)

  (* SQL literal for the zstd frame magic (0x28B52FFD, little-endian).
     Mirrors [ZSTD_MAGIC_LE] in [lib_sqlite_zstd/zstd.c] and is
     substituted into the prepared statements below at module load
     time via [Printf.sprintf]. The value is a compile-time constant
     so no injection risk. *)
  let zstd_magic_le_sql = "X'28B52FFD'"

  (* The WHERE filters skip rows whose target column is at most 16
     bytes. The minimum zstd frame size (magic + frame header + block
     header + optional checksum) is around 12-16 bytes, so any input
     at or below that threshold is mathematically guaranteed to expand
     under [zstd_compress]: there is no payload that could compress
     into fewer bytes than the frame envelope itself. Skipping these
     rows avoids wasted I/O and prevents the migration from inflating
     storage on stores where many rows are short or empty.

     The byte-count check is written as [length(CAST(col AS BLOB)) > 16]
     rather than [length(col) > 16] because legacy rows may have been
     written with TEXT affinity even though the payload is binary, and
     [length(X)] on a TEXT value with embedded 0x00 or 0xFF bytes is
     explicitly undefined per the SQLite spec. The CAST forces BLOB
     interpretation so [length] returns byte count regardless of how
     the cell was originally stored. *)

  let update_blueprints =
    (Caqti_type.(t2 int int) ->* Caqti_type.int)
      ~name:__FUNCTION__
      ~table:"blueprints"
    @@ Printf.sprintf
         {|UPDATE blueprints
          SET payload = zstd_compress(payload)
          WHERE rowid IN (
            SELECT rowid FROM blueprints
             WHERE rowid > ?
               AND length(CAST(payload AS BLOB)) > 16
               AND substr(payload, 1, 4) != %s
             ORDER BY rowid
             LIMIT ?)
          RETURNING rowid|}
         zstd_magic_le_sql

  let update_blocks =
    (Caqti_type.(t2 int int) ->* Caqti_type.int)
      ~name:__FUNCTION__
      ~table:"blocks"
    @@ Printf.sprintf
         {|UPDATE blocks
          SET block = zstd_compress(block)
          WHERE rowid IN (
            SELECT rowid FROM blocks
             WHERE rowid > ?
               AND length(CAST(block AS BLOB)) > 16
               AND substr(block, 1, 4) != %s
             ORDER BY rowid
             LIMIT ?)
          RETURNING rowid|}
         zstd_magic_le_sql

  (* The per-column [CASE] in the SET clause is load-bearing here, unlike
     in the single-column updates above. The subquery selects rows where
     [receipt_fields OR object_fields] needs work, so the SET runs against
     both columns; the [CASE] protects whichever column did not trigger
     the row's selection from being re-compressed (the C extension does
     not auto-detect existing zstd frames).

     The [length(CAST(... AS BLOB)) <= 16] guard mirrors the WHERE-clause
     length filter on each column so the migration's "don't compress
     sub-threshold inputs" invariant is enforced by this SQL alone,
     rather than relying on the zstd extension's small-input
     pass-through behaviour. *)
  let update_transactions =
    (Caqti_type.(t2 int int) ->* Caqti_type.int)
      ~name:__FUNCTION__
      ~table:"transactions"
    @@ Printf.sprintf
         {|UPDATE transactions
          SET receipt_fields =
                CASE
                  WHEN receipt_fields IS NULL
                    OR length(CAST(receipt_fields AS BLOB)) <= 16
                    OR substr(receipt_fields, 1, 4) = %s
                  THEN receipt_fields
                  ELSE zstd_compress(receipt_fields)
                END,
              object_fields =
                CASE
                  WHEN object_fields IS NULL
                    OR length(CAST(object_fields AS BLOB)) <= 16
                    OR substr(object_fields, 1, 4) = %s
                  THEN object_fields
                  ELSE zstd_compress(object_fields)
                END
          WHERE rowid IN (
            SELECT rowid FROM transactions
             WHERE rowid > ?
               AND ((length(CAST(receipt_fields AS BLOB)) > 16
                     AND substr(receipt_fields, 1, 4) != %s)
                 OR (length(CAST(object_fields AS BLOB)) > 16
                     AND substr(object_fields, 1, 4) != %s))
             ORDER BY rowid
             LIMIT ?)
          RETURNING rowid|}
         zstd_magic_le_sql
         zstd_magic_le_sql
         zstd_magic_le_sql
         zstd_magic_le_sql
end

(* --- Helpers -------------------------------------------------------------- *)

let run_batch_for_table db_conn ~table ~cursor ~batch_size =
  match table with
  | "blueprints" ->
      Db.collect_list db_conn Q.update_blueprints (cursor, batch_size)
  | "blocks" -> Db.collect_list db_conn Q.update_blocks (cursor, batch_size)
  | "transactions" ->
      Db.collect_list db_conn Q.update_transactions (cursor, batch_size)
  | other -> failwith "unknown migration table %s" other

let min_rowid_query_for = function
  | "blueprints" -> Q.min_rowid_blueprints
  | "blocks" -> Q.min_rowid_blocks
  | "transactions" -> Q.min_rowid_transactions
  | other ->
      invalid_arg
        (Printf.sprintf "sqlite_compression_migration: unknown table %s" other)

let max_rowid_query_for = function
  | "blueprints" -> Q.max_rowid_blueprints
  | "blocks" -> Q.max_rowid_blocks
  | "transactions" -> Q.max_rowid_transactions
  | other ->
      invalid_arg
        (Printf.sprintf "sqlite_compression_migration: unknown table %s" other)

let max_rowid_of_batch = function
  | [] -> None
  | hd :: tl -> Some (List.fold_left max hd tl)

(* Reports cursor advancement within the live rowid range
   [[min_rowid, max_rowid]]. Anchoring on [min_rowid] (rather than 0)
   matters on rolling/snapshot stores: pruned rows leave large gaps
   below the live range, so [cursor / max_rowid] would jump to ~99%
   on the very first batch. The clamp to [[0, 100]] is defensive — a
   resumed migration can have [cursor < min_rowid] briefly if the
   table was further pruned between runs, and writes during the loop
   could in principle push [cursor] past [max_rowid]. *)
let percent_of ~cursor ~min_rowid ~max_rowid =
  let denom = max_rowid - min_rowid in
  if denom <= 0 then 100.
  else
    let p = float_of_int (cursor - min_rowid) *. 100. /. float_of_int denom in
    Float.min 100. (Float.max 0. p)

(* Holds the store's only writable connection for the entire loop
   (the store is configured with a pool size of 1 in [Evm_store.init]).
   The migration runs synchronously before any client work in
   [Evm_store.init], so this is safe; a stray background task acquiring
   the pool while this is running would deadlock until the table scan
   finishes. *)
let compress_table_loop store ~table ~batch_size ~initial_cursor =
  let open Lwt_result_syntax in
  let started_at = Time.System.now () in
  Sqlite.use store @@ fun conn ->
  (* Snapshot the live rowid range once before the loop starts. The
     migration is a one-shot startup step so no concurrent writer can
     extend the table during the loop. Both bounds are needed for an
     accurate progress percentage on rolling stores where pruned rows
     leave gaps below the live range. *)
  let* min_rowid =
    with_connection conn @@ fun db_conn ->
    Db.find db_conn (min_rowid_query_for table) ()
  in
  let* max_rowid =
    with_connection conn @@ fun db_conn ->
    Db.find db_conn (max_rowid_query_for table) ()
  in
  let rec loop ~rows_done ~cursor =
    (* The batch UPDATE and the cursor/done write share a transaction so a
       crash between them cannot leave the cursor stale: either both land
       or neither. *)
    let* batch_result =
      Sqlite.with_transaction conn @@ fun conn ->
      with_connection conn @@ fun db_conn ->
      let* rowids = run_batch_for_table db_conn ~table ~cursor ~batch_size in
      match max_rowid_of_batch rowids with
      | None ->
          let* () = Db.exec db_conn Q.mark_done table in
          return `Done
      | Some new_cursor ->
          let* () = Db.exec db_conn Q.set_cursor (new_cursor, table) in
          return (`More (new_cursor, List.length rowids))
    in
    match batch_result with
    | `Done ->
        let elapsed = Ptime.diff (Time.System.now ()) started_at in
        let*! () = Events.(emit table_done) (table, elapsed) in
        return_unit
    | `More (new_cursor, batch_rows) ->
        let rows_done = rows_done + batch_rows in
        let percent = percent_of ~cursor:new_cursor ~min_rowid ~max_rowid in
        let*! () = Events.(emit table_progress) (table, rows_done, percent) in
        loop ~rows_done ~cursor:new_cursor
  in
  let*! () = Events.(emit table_start) table in
  loop ~rows_done:0 ~cursor:initial_cursor

let compress_all_tables_if_pending store
    (cfg : Configuration.sqlite_compression_config) =
  let open Lwt_result_syntax in
  List.iter_es
    (fun table ->
      let* () =
        Sqlite.use store @@ fun conn ->
        with_connection conn @@ fun db_conn ->
        Db.exec db_conn Q.ensure_row table
      in
      let* cursor, done_ =
        Sqlite.use store @@ fun conn ->
        with_connection conn @@ fun db_conn -> Db.find db_conn Q.get_state table
      in
      if done_ then return_unit
      else
        compress_table_loop
          store
          ~table
          ~batch_size:cfg.batch_size
          ~initial_cursor:cursor)
    target_tables

let file_size_bytes path =
  try Some (Unix.LargeFile.stat path).st_size with Unix.Unix_error _ -> None

(* Remove [path] if it exists. [ENOENT] is expected; anything else is
   re-raised so we don't silently paper over permission or filesystem
   errors. *)
let unlink_if_present path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let run_vacuum_into_and_rename store ~path =
  let open Lwt_result_syntax in
  let tmp_path = path ^ ".compact" in
  (* Clean up a stale tmp from a previous aborted run before we start. *)
  unlink_if_present tmp_path ;
  let*! () = Events.(emit vacuum_start) () in
  let started_at = Time.System.now () in
  let size_before = Option.value (file_size_bytes path) ~default:0L in
  let*! vacuum_res =
    Sqlite.use store @@ fun conn -> Sqlite.vacuum ~conn ~output_db_file:tmp_path
  in
  let cleanup_tmp () =
    unlink_if_present tmp_path ;
    unlink_if_present (tmp_path ^ "-wal") ;
    unlink_if_present (tmp_path ^ "-shm")
  in
  match vacuum_res with
  | Error err ->
      cleanup_tmp () ;
      let*! () =
        Events.(emit vacuum_failed) (Format.asprintf "%a" pp_print_trace err)
      in
      Lwt.return (Error err)
  | Ok () -> (
      let*! () = Sqlite.close store in
      (* The freshly-vacuumed [tmp_path] is a clean SQLite file. Nuke
         the pre-vacuum WAL/SHM sidecars so SQLite never re-attaches
         them to the new inode after rename. *)
      unlink_if_present (path ^ "-wal") ;
      unlink_if_present (path ^ "-shm") ;
      (* On rename failure the source DB at [path] is intact (its main
         file is unchanged; SQLite auto-checkpoints on last-connection
         close, so the deleted [-wal]/[-shm] sidecars hold nothing
         un-flushed). Per-table [done_=1] is durable in the cursor
         table because each batch's UPDATE shares a transaction with
         its [mark_done] write, so a restart re-opens the original
         path and only the VACUUM + rename phase runs again — the
         multi-hour row-compression scan does not repeat. *)
      match Unix.rename tmp_path path with
      | exception Unix.Unix_error (e, _, _) ->
          let msg = Unix.error_message e in
          cleanup_tmp () ;
          let*! () = Events.(emit vacuum_failed) msg in
          failwith "sqlite_compression_migration: rename failed (%s)" msg
      | () ->
          (* [Sqlite.vacuum] may have left WAL/SHM sidecars next to the
             tmp file; they're not carried by [Unix.rename tmp_path path]
             and would remain as orphans at the old basename. *)
          unlink_if_present (tmp_path ^ "-wal") ;
          unlink_if_present (tmp_path ^ "-shm") ;
          let size_after = Option.value (file_size_bytes path) ~default:0L in
          let elapsed = Ptime.diff (Time.System.now ()) started_at in
          (* Positive reduction = the file shrank, negative = compression
             paradoxically grew the DB (possible on tiny or incompressible
             content). [size_before] of 0 is reported as 0% to avoid a
             division by zero; the file size is logged anyway so operators
             can see the raw numbers. *)
          let reduction =
            if Int64.compare size_before 0L <= 0 then 0.
            else
              let before = Int64.to_float size_before in
              let after = Int64.to_float size_after in
              (before -. after) *. 100. /. before
          in
          let*! () =
            Events.(emit vacuum_done)
              (elapsed, size_before, size_after, reduction)
          in
          return_unit)

(* Ensure the cursor table exists and contains a row for each target
   table plus the [__vacuum__] sentinel. Idempotent — safe to run on
   every startup, costs a handful of cheap INSERT OR IGNOREs. *)
let ensure_cursor_table_and_rows store =
  let open Lwt_result_syntax in
  Sqlite.use store @@ fun conn ->
  Sqlite.with_transaction conn @@ fun conn ->
  with_connection conn @@ fun db_conn ->
  let* () = Db.exec db_conn Q.create_table () in
  List.iter_es (fun row -> Db.exec db_conn Q.ensure_row row) all_rows

let run_if_needed ~store ~path ~open_store
    (cfg : Configuration.sqlite_compression_config) =
  let open Lwt_result_syntax in
  let* () = ensure_cursor_table_and_rows store in
  (* Fast path: if every row is [done_ = 1], the previous run compressed
     every target column AND the VACUUM + rename step succeeded. This is
     an O(1) check and is what keeps steady-state startups cheap. *)
  let* pending =
    Sqlite.use store @@ fun conn ->
    with_connection conn @@ fun db_conn -> Db.find db_conn Q.count_pending ()
  in
  if pending = 0 then return store
  else
    let* in_progress =
      Sqlite.use store @@ fun conn ->
      with_connection conn @@ fun db_conn ->
      Db.find db_conn Q.count_in_progress ()
    in
    let*! () =
      if in_progress > 0 then Events.(emit migration_resume) ()
      else Events.(emit migration_start) ()
    in
    let started_at = Time.System.now () in
    let* () = compress_all_tables_if_pending store cfg in
    (* VACUUM only if the sentinel hasn't been marked done yet — this
       covers both a fresh run and the "crashed between compression and
       vacuum" resume case. *)
    let* _, vacuum_done =
      Sqlite.use store @@ fun conn ->
      with_connection conn @@ fun db_conn ->
      Db.find db_conn Q.get_state vacuum_marker
    in
    let* final_store =
      if vacuum_done then return store
      else
        let* () = run_vacuum_into_and_rename store ~path in
        (* Narrow crash window: if the process dies between the rename
           above and the [mark_done vacuum_marker] below, the next run
           sees the sentinel still at 0 and re-executes [VACUUM INTO]
           on the already-compressed file. The per-table cursors are
           all [done_ = 1] so the row-compression scan is skipped;
           only the vacuum (idempotent, same output) is repeated. See
           the mli for the full rationale on why we accept this rather
           than closing the window. *)
        let* fresh_store = open_store () in
        let* () =
          Sqlite.use fresh_store @@ fun conn ->
          with_connection conn @@ fun db_conn ->
          Db.exec db_conn Q.mark_done vacuum_marker
        in
        return fresh_store
    in
    let elapsed = Ptime.diff (Time.System.now ()) started_at in
    let*! () = Events.(emit migration_done) elapsed in
    return final_store
