(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section

(* Pretty-print a byte count with an appropriate SI-binary unit, signed.
   Produces outputs like "12.34 MiB", "-5.67 GiB", or "+123 B" (with an
   explicit positive sign where useful). The cap at TiB matches the
   units table above. *)
let pp_human_bytes fmt (n : int64) =
  let sign, v =
    if Int64.compare n 0L < 0 then ("-", Int64.to_float (Int64.neg n))
    else ("", Int64.to_float n)
  in
  let units = [|"B"; "KiB"; "MiB"; "GiB"; "TiB"|] in
  let rec go i v =
    if i = Array.length units - 1 || v < 1024. then
      Format.fprintf fmt "%s%.2f %s" sign v units.(i)
    else go (i + 1) (v /. 1024.)
  in
  go 0 v

let pp_percent fmt f = Format.fprintf fmt "%.2f%%" f

let migration_start =
  declare_0
    ~section
    ~name:"sqlite_compression_migration_start"
    ~msg:"starting sqlite zstd-compression migration"
    ~level:Notice
    ()

let migration_resume =
  declare_0
    ~section
    ~name:"sqlite_compression_migration_resume"
    ~msg:"resuming sqlite zstd-compression migration from previous run"
    ~level:Notice
    ()

let table_start =
  declare_1
    ~section
    ~name:"sqlite_compression_migration_table_start"
    ~msg:"compressing table {table}"
    ~level:Notice
    ("table", Data_encoding.string)

let table_progress =
  declare_3
    ~section
    ~name:"sqlite_compression_migration_table_progress"
    ~msg:"compressing {table}: {rows_done} rows processed ({percent})"
    ~level:Info
    ~pp3:pp_percent
    ("table", Data_encoding.string)
    ("rows_done", Data_encoding.int31)
    ("percent", Data_encoding.float)

let table_done =
  declare_2
    ~section
    ~name:"sqlite_compression_migration_table_done"
    ~msg:"finished compressing {table} in {elapsed}"
    ~level:Notice
    ~pp2:Time.System.Span.pp_hum
    ("table", Data_encoding.string)
    ("elapsed", Time.System.Span.encoding)

let vacuum_start =
  declare_0
    ~section
    ~name:"sqlite_compression_migration_vacuum_start"
    ~msg:"starting VACUUM INTO after compression migration"
    ~level:Notice
    ()

let vacuum_done =
  declare_4
    ~section
    ~name:"sqlite_compression_migration_vacuum_done"
    ~msg:
      "VACUUM INTO finished in {elapsed}: {before_bytes} -> {after_bytes} \
       ({reduction})"
    ~level:Notice
    ~pp1:Time.System.Span.pp_hum
    ~pp2:pp_human_bytes
    ~pp3:pp_human_bytes
    ~pp4:pp_percent
    ("elapsed", Time.System.Span.encoding)
    ("before_bytes", Data_encoding.int64)
    ("after_bytes", Data_encoding.int64)
    ("reduction", Data_encoding.float)

let vacuum_failed =
  declare_1
    ~section
    ~name:"sqlite_compression_migration_vacuum_failed"
    ~msg:
      "VACUUM INTO failed after row compression: {error}. Migration state is \
       preserved and will be retried on the next invocation of 'compress \
       store'."
    ~level:Error
    ("error", Data_encoding.string)

let migration_done =
  declare_1
    ~section
    ~name:"sqlite_compression_migration_done"
    ~msg:"sqlite zstd-compression migration complete in {elapsed}"
    ~level:Notice
    ~pp1:Time.System.Span.pp_hum
    ("elapsed", Time.System.Span.encoding)
