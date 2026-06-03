(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** One-shot startup migration that zstd-compresses the legacy uncompressed
    rows of the target BLOB columns listed below, then reclaims disk
    space via [VACUUM INTO] followed by a file rename. Interruptible:
    progress is persisted in a lazily-created [compression_migration]
    cursor table (one row per target table plus a [__vacuum__]
    sentinel for the vacuum + rename step), so a crash or restart
    resumes where the previous run left off.

    The migrated columns are:

    - [blueprints.payload]
    - [blocks.block]
    - [transactions.receipt_fields]
    - [transactions.object_fields]

    [blocks.tez_block] is also wrapped in [zstd_compress] /
    [zstd_decompress] on the write/read path in [Evm_store.Q], but is
    intentionally excluded from the migration scan: the column is
    unused — and therefore empty — on every production EVM node, so a
    row scan would be wasted I/O. New writes go through
    [zstd_compress] regardless, so any future use of the column starts
    compressed.

    On steady-state startups (every cursor row has [done_ = 1]), the
    function exits in O(1) without touching any target table.

    On [VACUUM INTO] / rename failure, the cursor table is preserved
    with [done_ = 1] for every fully compressed table and [done_ = 0]
    on the [__vacuum__] sentinel — so the next startup retries only
    the vacuum + rename phase, never the multi-hour row scan. The
    error is propagated to the caller.

    A narrow crash window exists between [Unix.rename] succeeding and
    the [__vacuum__] sentinel being marked [done_ = 1] (essentially
    the time it takes to reopen the store and run a single UPDATE).
    A crash in that window leaves the file fully migrated and
    vacuumed but with the sentinel still at 0; the next run skips
    every table loop (each target is already [done_ = 1]) and
    re-runs [VACUUM INTO] once on the already-compressed file. That
    second vacuum is idempotent — output is identical — but costs the
    same wall time as the first. We accept the cost: closing the
    window would require writing the sentinel into the tmp DB before
    the rename, which is more machinery than the rarity of the
    scenario justifies. *)

(** [run_if_needed ~store ~path ~open_store cfg] runs the compression
    migration and returns the (possibly reopened) store. [~open_store]
    is used to reopen after [VACUUM INTO] + rename. Must be called after
    the store has been opened and before any client work runs against
    it. *)
val run_if_needed :
  store:Sqlite.t ->
  path:string ->
  open_store:(unit -> Sqlite.t tzresult Lwt.t) ->
  Configuration.sqlite_compression_config ->
  Sqlite.t tzresult Lwt.t
