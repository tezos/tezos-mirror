(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

type compression = No_compression | Level of int

(* The C side stores the level as an [int] in per-connection user data.
   The convention: 0 means "pass through unchanged" (as in
   [No_compression]); any positive integer is a real zstd compression
   level. We never pass [Level 0] across the FFI because that's the
   ambiguous case (zstd reads it as "library default"). *)
external register_with_level : Sqlite3.db -> int -> unit
  = "caml_sqlite3_register_zstd"

let register compression db =
  let level = match compression with No_compression -> 0 | Level n -> n in
  register_with_level db level
