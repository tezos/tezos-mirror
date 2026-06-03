(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Sqlite_zstd
    Invocation:   dune exec src/lib_sqlite_zstd/test/main.exe
    Subject:      Tests for the sqlite_zstd SQLite extension
                  (scalar functions zstd_compress / zstd_decompress).
*)

let test_register = Test.register ~__FILE__

let with_db ?(compression = Sqlite_zstd.No_compression) f =
  let db = Sqlite3.db_open ":memory:" in
  Sqlite_zstd.register compression db ;
  let r = try Ok (f db) with exn -> Error exn in
  let (_ : bool) = Sqlite3.db_close db in
  match r with Ok v -> v | Error exn -> raise exn

let fail_rc ~loc rc =
  Test.fail ~__LOC__:loc "sqlite3 error: %s" (Sqlite3.Rc.to_string rc)

let ok_or_fail ~loc rc =
  match rc with Sqlite3.Rc.OK | Sqlite3.Rc.DONE -> () | _ -> fail_rc ~loc rc

(* Bind [input] as a blob parameter and read back the first column as a blob.
   Returns [None] if the column was NULL, [Some bytes] otherwise. *)
let run_blob_unary db ~sql ~input =
  let stmt = Sqlite3.prepare db sql in
  ok_or_fail ~loc:__LOC__ (Sqlite3.bind_blob stmt 1 input) ;
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.NULL -> None
        | Sqlite3.Data.BLOB s -> Some s
        | Sqlite3.Data.TEXT s -> Some s
        | other ->
            Test.fail
              "unexpected column data: %s"
              (Sqlite3.Data.to_string_debug other))
    | rc -> fail_rc ~loc:__LOC__ rc
  in
  ok_or_fail ~loc:__LOC__ (Sqlite3.finalize stmt) ;
  result

let run_blob_nullary db ~sql =
  let stmt = Sqlite3.prepare db sql in
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.NULL -> None
        | Sqlite3.Data.BLOB s -> Some s
        | Sqlite3.Data.TEXT s -> Some s
        | other ->
            Test.fail
              "unexpected column data: %s"
              (Sqlite3.Data.to_string_debug other))
    | rc -> fail_rc ~loc:__LOC__ rc
  in
  ok_or_fail ~loc:__LOC__ (Sqlite3.finalize stmt) ;
  result

let check_blob_eq ~__LOC__ ~found ~expected =
  Check.(
    (found = expected) string ~__LOC__ ~error_msg:"expected blob %R, got %L")

let check_some_blob_eq ~__LOC__ ~found ~expected =
  match found with
  | None -> Test.fail ~__LOC__ "expected Some blob, got NULL"
  | Some s -> check_blob_eq ~__LOC__ ~found:s ~expected

let zstd_magic = "\x28\xB5\x2F\xFD"

let highly_compressible n =
  (* A very repetitive blob that should compress to well below [n] bytes. *)
  String.make n 'a'

let test_roundtrip_small =
  test_register
    ~title:"sqlite_zstd: zstd_compress/zstd_decompress round-trip (small)"
    ~tags:["sqlite"; "zstd"; "roundtrip"]
  @@ fun () ->
  with_db @@ fun db ->
  let input = "hello, world" in
  let found =
    run_blob_unary db ~sql:"SELECT zstd_decompress(zstd_compress(?, 3))" ~input
  in
  check_some_blob_eq ~__LOC__ ~found ~expected:input ;
  unit

let test_roundtrip_empty =
  test_register
    ~title:"sqlite_zstd: round-trip of an empty blob"
    ~tags:["sqlite"; "zstd"; "roundtrip"; "empty"]
  @@ fun () ->
  with_db @@ fun db ->
  let found =
    run_blob_unary
      db
      ~sql:"SELECT zstd_decompress(zstd_compress(?, 3))"
      ~input:""
  in
  check_some_blob_eq ~__LOC__ ~found ~expected:"" ;
  unit

let test_roundtrip_large =
  test_register
    ~title:"sqlite_zstd: round-trip of a 1 MiB highly-compressible blob"
    ~tags:["sqlite"; "zstd"; "roundtrip"; "large"]
  @@ fun () ->
  with_db @@ fun db ->
  let input = highly_compressible (1024 * 1024) in
  let found =
    run_blob_unary db ~sql:"SELECT zstd_decompress(zstd_compress(?, 3))" ~input
  in
  check_some_blob_eq ~__LOC__ ~found ~expected:input ;
  unit

let test_compression_shrinks =
  test_register
    ~title:"sqlite_zstd: compression of a repetitive blob shrinks it"
    ~tags:["sqlite"; "zstd"; "size"]
  @@ fun () ->
  with_db @@ fun db ->
  let input = highly_compressible (64 * 1024) in
  let compressed =
    match run_blob_unary db ~sql:"SELECT zstd_compress(?, 3)" ~input with
    | Some s -> s
    | None -> Test.fail ~__LOC__ "unexpected NULL result"
  in
  Check.(
    (String.length compressed < String.length input / 10)
      int
      ~__LOC__
      ~error_msg:
        "expected compressed size (%L) to be much smaller than input (%R)") ;
  (* First four bytes must be the zstd magic number. *)
  Check.(
    (String.sub compressed 0 4 = zstd_magic)
      string
      ~__LOC__
      ~error_msg:"expected zstd magic at start of compressed blob, got %L") ;
  unit

let test_decompress_passthrough_on_raw =
  test_register
    ~title:
      "sqlite_zstd: zstd_decompress is a no-op on inputs without zstd magic"
    ~tags:["sqlite"; "zstd"; "passthrough"]
  @@ fun () ->
  with_db @@ fun db ->
  let inputs =
    [
      (* An arbitrary blob not starting with the zstd magic. *)
      "plain legacy bytes, not compressed";
      (* A 3-byte blob is too short to contain a zstd magic at all. *)
      "\x00\x01\x02";
      (* 4 bytes but wrong magic. *)
      "\xDE\xAD\xBE\xEF";
      (* A blob whose body happens to contain the magic, but not at offset 0. *)
      "prefix" ^ zstd_magic ^ "suffix";
    ]
  in
  List.iter
    (fun input ->
      let found = run_blob_unary db ~sql:"SELECT zstd_decompress(?)" ~input in
      check_some_blob_eq ~__LOC__ ~found ~expected:input)
    inputs ;
  unit

let test_decompress_empty_is_empty =
  test_register
    ~title:"sqlite_zstd: zstd_decompress of an empty blob is empty"
    ~tags:["sqlite"; "zstd"; "passthrough"; "empty"]
  @@ fun () ->
  with_db @@ fun db ->
  let found = run_blob_unary db ~sql:"SELECT zstd_decompress(?)" ~input:"" in
  check_some_blob_eq ~__LOC__ ~found ~expected:"" ;
  unit

let test_null_in_null_out =
  test_register
    ~title:"sqlite_zstd: NULL inputs produce NULL outputs"
    ~tags:["sqlite"; "zstd"; "null"]
  @@ fun () ->
  with_db @@ fun db ->
  let check_null ~loc sql =
    match run_blob_nullary db ~sql with
    | None -> ()
    | Some s ->
        Test.fail
          ~__LOC__:loc
          "expected NULL for %s, got blob of length %d"
          sql
          (String.length s)
  in
  check_null ~loc:__LOC__ "SELECT zstd_compress(NULL, 3)" ;
  check_null ~loc:__LOC__ "SELECT zstd_decompress(NULL)" ;
  check_null ~loc:__LOC__ "SELECT zstd_decompress(zstd_compress(NULL, 3))" ;
  unit

let test_invalid_frame_errors =
  test_register
    ~title:"sqlite_zstd: zstd_decompress errors on a corrupt zstd frame"
    ~tags:["sqlite"; "zstd"; "error"]
  @@ fun () ->
  with_db @@ fun db ->
  (* Starts with the magic but the rest is nonsense. *)
  let bad = zstd_magic ^ String.make 16 '\xff' in
  let stmt = Sqlite3.prepare db "SELECT zstd_decompress(?)" in
  ok_or_fail ~loc:__LOC__ (Sqlite3.bind_blob stmt 1 bad) ;
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.ERROR -> ()
  | other ->
      Test.fail
        ~__LOC__
        "expected Rc.ERROR, got %s"
        (Sqlite3.Rc.to_string other)) ;
  (* After a step() error, finalize propagates the same error code. *)
  let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
  unit

(* Exercises the CASE-based UPDATE idiom used by
   [Sqlite_compression_migration]. Mixes a plain blob and an
   already-compressed blob in the same table; after the UPDATE, both
   must be compressed exactly once (no double compression). *)
let test_migration_update_preserves_compressed =
  test_register
    ~title:
      "sqlite_zstd: migration CASE-UPDATE compresses only uncompressed rows"
    ~tags:["sqlite"; "zstd"; "migration"]
  @@ fun () ->
  with_db @@ fun db ->
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload BLOB)") ;
  (* Row 1: plain payload. Row 2: pre-compressed payload. *)
  let plain = "the quick brown fox" in
  let precompressed =
    match run_blob_unary db ~sql:"SELECT zstd_compress(?, 3)" ~input:plain with
    | Some s -> s
    | None -> Test.fail ~__LOC__ "expected Some blob"
  in
  let insert_row ~id ~payload =
    let stmt = Sqlite3.prepare db "INSERT INTO t (id, payload) VALUES (?, ?)" in
    ok_or_fail ~loc:__LOC__ (Sqlite3.bind_int stmt 1 id) ;
    ok_or_fail ~loc:__LOC__ (Sqlite3.bind_blob stmt 2 payload) ;
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE ->
        let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
        ()
    | rc -> fail_rc ~loc:__LOC__ rc
  in
  insert_row ~id:1 ~payload:plain ;
  insert_row ~id:2 ~payload:precompressed ;
  (* Run the same CASE-based UPDATE the migration module emits. *)
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec
       db
       {|UPDATE t SET payload =
         CASE
           WHEN payload IS NULL
             OR substr(payload, 1, 4) = X'28B52FFD'
           THEN payload
           ELSE zstd_compress(payload, 3)
         END|}) ;
  (* Both rows should now decompress back to the original plain text. *)
  let fetch id =
    let stmt =
      Sqlite3.prepare db "SELECT zstd_decompress(payload) FROM t WHERE id = ?"
    in
    ok_or_fail ~loc:__LOC__ (Sqlite3.bind_int stmt 1 id) ;
    let r =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.BLOB s | Sqlite3.Data.TEXT s -> s
          | _ -> Test.fail ~__LOC__ "expected blob")
      | rc -> fail_rc ~loc:__LOC__ rc
    in
    let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
    r
  in
  check_blob_eq ~__LOC__ ~found:(fetch 1) ~expected:plain ;
  check_blob_eq ~__LOC__ ~found:(fetch 2) ~expected:plain ;
  (* Row 2 must still be the same zstd frame we inserted — proving the
     CASE branch did not re-compress. *)
  let raw_row2 =
    let stmt = Sqlite3.prepare db "SELECT payload FROM t WHERE id = 2" in
    let r =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.BLOB s | Sqlite3.Data.TEXT s -> s
          | _ -> Test.fail ~__LOC__ "expected blob")
      | rc -> fail_rc ~loc:__LOC__ rc
    in
    let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
    r
  in
  check_blob_eq ~__LOC__ ~found:raw_row2 ~expected:precompressed ;
  unit

(* Legacy rows in the EVM store are sometimes stored with TEXT storage
   class instead of BLOB (see the [CAST(col AS BLOB)] comments in
   [evm_store.ml]). The migration's CASE guard uses [substr(col, 1, 4) =
   X'28B52FFD'] which SQLite evaluates as FALSE on any TEXT row (BLOB
   never equals TEXT), so TEXT rows always take the [ELSE] branch and
   get compressed. This test pins that behaviour. *)
let test_migration_update_compresses_text_rows =
  test_register
    ~title:"sqlite_zstd: migration CASE-UPDATE compresses TEXT rows"
    ~tags:["sqlite"; "zstd"; "migration"; "text"]
  @@ fun () ->
  with_db @@ fun db ->
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload BLOB)") ;
  (* Insert as an explicit TEXT value — SQLite's flexible typing allows
     a TEXT value in a BLOB-affinity column. The payload is kept above
     the 16-byte minimum-frame threshold so [zstd_compress] actually
     emits a frame instead of passing the value through unchanged. *)
  let plain = "plain text row that is comfortably above 16 bytes" in
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec
       db
       (Printf.sprintf "INSERT INTO t (id, payload) VALUES (1, '%s')" plain)) ;
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec
       db
       {|UPDATE t SET payload =
         CASE
           WHEN payload IS NULL
             OR substr(payload, 1, 4) = X'28B52FFD'
           THEN payload
           ELSE zstd_compress(payload, 3)
         END|}) ;
  (* Verify the row was compressed (starts with the zstd magic) and
     decompresses back to the original plain text. *)
  let stmt = Sqlite3.prepare db "SELECT payload FROM t WHERE id = 1" in
  let raw =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.BLOB s | Sqlite3.Data.TEXT s -> s
        | _ -> Test.fail ~__LOC__ "expected blob")
    | rc -> fail_rc ~loc:__LOC__ rc
  in
  let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
  Check.(
    (String.sub raw 0 4 = zstd_magic)
      string
      ~__LOC__
      ~error_msg:"expected migrated TEXT row to be a zstd frame, got prefix %L") ;
  let roundtrip =
    run_blob_unary db ~sql:"SELECT zstd_decompress(?)" ~input:raw
  in
  check_some_blob_eq ~__LOC__ ~found:roundtrip ~expected:plain ;
  unit

(* NULL payloads must survive the CASE-UPDATE unchanged. *)
let test_migration_update_leaves_null =
  test_register
    ~title:"sqlite_zstd: migration CASE-UPDATE leaves NULL payloads alone"
    ~tags:["sqlite"; "zstd"; "migration"; "null"]
  @@ fun () ->
  with_db @@ fun db ->
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload BLOB)") ;
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec db "INSERT INTO t (id, payload) VALUES (1, NULL)") ;
  ok_or_fail
    ~loc:__LOC__
    (Sqlite3.exec
       db
       {|UPDATE t SET payload =
         CASE
           WHEN payload IS NULL
             OR substr(payload, 1, 4) = X'28B52FFD'
           THEN payload
           ELSE zstd_compress(payload, 3)
         END|}) ;
  let stmt = Sqlite3.prepare db "SELECT payload FROM t WHERE id = 1" in
  (match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.NULL -> ()
      | other ->
          Test.fail
            ~__LOC__
            "expected NULL, got %s"
            (Sqlite3.Data.to_string_debug other))
  | rc -> fail_rc ~loc:__LOC__ rc) ;
  let (_ : Sqlite3.Rc.t) = Sqlite3.finalize stmt in
  unit

(* The 1-arg [zstd_compress(blob)] form reads its level from the
   per-connection state set at registration. With [No_compression] it
   must be a pass-through. *)
let test_unary_no_compression_passthrough =
  test_register
    ~title:
      "sqlite_zstd: 1-arg zstd_compress is a pass-through under No_compression"
    ~tags:["sqlite"; "zstd"; "unary"; "passthrough"]
  @@ fun () ->
  with_db ~compression:Sqlite_zstd.No_compression @@ fun db ->
  let inputs =
    ["plain bytes"; ""; "\x00\x01\x02\x03\x04"; "BLOB-ish \xDE\xAD"]
  in
  List.iter
    (fun input ->
      let found = run_blob_unary db ~sql:"SELECT zstd_compress(?)" ~input in
      check_some_blob_eq ~__LOC__ ~found ~expected:input)
    inputs ;
  unit

(* With [Level n] the 1-arg form behaves like the 2-arg form at level n. *)
let test_unary_level_compresses =
  test_register
    ~title:"sqlite_zstd: 1-arg zstd_compress emits a zstd frame under Level n"
    ~tags:["sqlite"; "zstd"; "unary"; "level"]
  @@ fun () ->
  with_db ~compression:(Sqlite_zstd.Level 3) @@ fun db ->
  let input = highly_compressible (8 * 1024) in
  let compressed =
    match run_blob_unary db ~sql:"SELECT zstd_compress(?)" ~input with
    | Some s -> s
    | None -> Test.fail ~__LOC__ "unexpected NULL result"
  in
  Check.(
    (String.sub compressed 0 4 = zstd_magic)
      string
      ~__LOC__
      ~error_msg:"expected zstd magic at start of compressed blob, got %L") ;
  Check.(
    (String.length compressed < String.length input / 4)
      int
      ~__LOC__
      ~error_msg:
        "expected compressed size (%L) to be much smaller than input (%R)") ;
  unit

(* End-to-end: round-trip via the 1-arg form at Level 3 returns the
   original input. *)
let test_unary_round_trip =
  test_register
    ~title:"sqlite_zstd: 1-arg zstd_compress + zstd_decompress round-trips"
    ~tags:["sqlite"; "zstd"; "unary"; "roundtrip"]
  @@ fun () ->
  with_db ~compression:(Sqlite_zstd.Level 3) @@ fun db ->
  let input = "the quick brown fox jumps over the lazy dog" in
  let found =
    run_blob_unary db ~sql:"SELECT zstd_decompress(zstd_compress(?))" ~input
  in
  check_some_blob_eq ~__LOC__ ~found ~expected:input ;
  unit

(* Inputs at or below the minimum-frame threshold are passed through
   unchanged regardless of compression level. The threshold matches
   [ZSTD_EXT_MIN_COMPRESS_SIZE = 16] in [zstd.c]. *)
let test_compress_passes_through_at_threshold =
  test_register
    ~title:"sqlite_zstd: zstd_compress is a pass-through for inputs ≤ 16 bytes"
    ~tags:["sqlite"; "zstd"; "threshold"]
  @@ fun () ->
  with_db @@ fun db ->
  (* 1, 8, 15, 16 — all ≤ threshold, all should round-trip byte-for-byte
     without ever producing a zstd frame. *)
  let inputs =
    [
      "x";
      "abcdefgh";
      String.make 15 'a';
      String.make 16 'a';
      (* Includes bytes that would otherwise look like start of a zstd
         frame; the size check fires first. *)
      zstd_magic ^ "\x00\x01\x02";
    ]
  in
  List.iter
    (fun input ->
      let found = run_blob_unary db ~sql:"SELECT zstd_compress(?, 3)" ~input in
      check_some_blob_eq ~__LOC__ ~found ~expected:input)
    inputs ;
  unit

(* Just above the threshold, [zstd_compress] does emit a real zstd
   frame (verified via the magic prefix). *)
let test_compress_compresses_above_threshold =
  test_register
    ~title:"sqlite_zstd: zstd_compress emits a frame for inputs > 16 bytes"
    ~tags:["sqlite"; "zstd"; "threshold"]
  @@ fun () ->
  with_db @@ fun db ->
  let input = String.make 17 'a' in
  let found =
    match run_blob_unary db ~sql:"SELECT zstd_compress(?, 3)" ~input with
    | Some s -> s
    | None -> Test.fail ~__LOC__ "unexpected NULL result"
  in
  Check.(
    (String.sub found 0 4 = zstd_magic)
      string
      ~__LOC__
      ~error_msg:
        "expected zstd magic at start of compressed blob (input was 17 bytes), \
         got %L") ;
  unit

(* The pass-through is round-trip-safe via [zstd_decompress]'s existing
   non-magic-pass-through behavior. *)
let test_compress_decompress_round_trip_at_threshold =
  test_register
    ~title:
      "sqlite_zstd: round-trip through zstd_decompress at and just above the \
       16-byte threshold"
    ~tags:["sqlite"; "zstd"; "threshold"; "roundtrip"]
  @@ fun () ->
  with_db @@ fun db ->
  let inputs = [String.make 16 'a'; String.make 17 'a'] in
  List.iter
    (fun input ->
      let found =
        run_blob_unary
          db
          ~sql:"SELECT zstd_decompress(zstd_compress(?, 3))"
          ~input
      in
      check_some_blob_eq ~__LOC__ ~found ~expected:input)
    inputs ;
  unit

let () =
  test_roundtrip_small ;
  test_roundtrip_empty ;
  test_roundtrip_large ;
  test_compression_shrinks ;
  test_decompress_passthrough_on_raw ;
  test_decompress_empty_is_empty ;
  test_null_in_null_out ;
  test_invalid_frame_errors ;
  test_migration_update_preserves_compressed ;
  test_migration_update_compresses_text_rows ;
  test_migration_update_leaves_null ;
  test_unary_no_compression_passthrough ;
  test_unary_level_compresses ;
  test_unary_round_trip ;
  test_compress_passes_through_at_threshold ;
  test_compress_compresses_above_threshold ;
  test_compress_decompress_round_trip_at_threshold
