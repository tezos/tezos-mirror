(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Compression mode requested at registration time. Captured per
    connection in the C extension's user-data slot, so the 1-arg form
    of [zstd_compress] resolves it without any SQL-level binding.
    [Level n] with [n <= 0] is treated as [No_compression] (the
    1-arg form passes the input through unchanged). *)
type compression = No_compression | Level of int

(** Register user-defined SQLite functions on the given connection:

    - [zstd_compress(blob)] (1-arg): compresses [blob] using the
      [compression] mode chosen at registration time.
      [No_compression] returns [blob] unchanged (storage class
      preserved); [Level n] produces a zstd frame at level [n].
    - [zstd_compress(blob, level INTEGER)] (2-arg): compresses [blob]
      using the explicit [level], independent of registration mode.
      [level <= 0] is a pass-through.
    - [zstd_decompress(blob)]: inverse of [zstd_compress]. If the
      input does not start with the zstd magic number ([0x28B52FFD]
      little-endian), the input is returned unchanged — this
      backward-compatibility property lets the same query work on
      both compressed and uncompressed rows.

    [zstd_compress] additionally passes inputs of 16 bytes or fewer
    through unchanged: the minimum zstd frame envelope (magic + frame
    header + block header + optional checksum) is around 12-16 bytes,
    so any payload up to that threshold is guaranteed to grow when
    compressed. Pass-through keeps storage tight on tiny values and
    round-trips through [zstd_decompress]. *)
val register : compression -> Sqlite3.db -> unit
