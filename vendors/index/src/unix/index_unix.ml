(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

let src = Logs.Src.create "index_unix" ~doc:"Index_unix"

module Log = (val Logs.src_log src : Logs.LOG)

exception RO_not_allowed

let current_version = "00000001"

module Stats = Index.Stats

module IO : Index.IO = struct
  let ( ++ ) = Int64.add

  let ( -- ) = Int64.sub

  external set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

  external get_64 : string -> int -> int64 = "%caml_string_get64"

  external swap64 : int64 -> int64 = "%bswap_int64"

  let encode_int64 i =
    let set_uint64 s off v =
      if not Sys.big_endian then set_64 s off (swap64 v) else set_64 s off v
    in
    let b = Bytes.create 8 in
    set_uint64 b 0 i;
    Bytes.unsafe_to_string b

  let decode_int64 buf =
    let get_uint64 s off =
      if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off
    in
    get_uint64 buf 0

  module Raw = struct
    type t = { fd : Unix.file_descr; mutable cursor : int64 }

    let v fd = { fd; cursor = 0L }

    external pread : Unix.file_descr -> int64 -> bytes -> int -> int -> int
      = "caml_pread"

    external pwrite : Unix.file_descr -> int64 -> bytes -> int -> int -> int
      = "caml_pwrite"

    external unix_fsync : Unix.file_descr -> unit = "unix_fsync"

    let really_write fd off buf =
      let rec aux fd_off buf_off len =
        let w = pwrite fd fd_off buf buf_off len in
        if w = 0 || w = len then ()
        else (aux [@tailcall]) (fd_off ++ Int64.of_int w) (buf_off + w) (len - w)
      in
      (aux [@tailcall]) off 0 (Bytes.length buf)

    let really_read fd off len buf =
      let rec aux fd_off buf_off len =
        let r = pread fd fd_off buf buf_off len in
        if r = 0 then buf_off (* end of file *)
        else if r = len then buf_off + r
        else (aux [@tailcall]) (fd_off ++ Int64.of_int r) (buf_off + r) (len - r)
      in
      (aux [@tailcall]) off 0 len

    let fsync t = unix_fsync t.fd

    let unsafe_write t ~off buf =
      let buf = Bytes.unsafe_of_string buf in
      really_write t.fd off buf;
      t.cursor <- off ++ Int64.of_int (Bytes.length buf);
      Stats.add_write (Bytes.length buf)

    let unsafe_read t ~off ~len buf =
      let n = really_read t.fd off len buf in
      t.cursor <- off ++ Int64.of_int n;
      Stats.add_read n;
      n

    module Offset = struct
      let set t n =
        let buf = encode_int64 n in
        unsafe_write t ~off:0L buf

      let get t =
        let buf = Bytes.create 8 in
        let n = unsafe_read t ~off:0L ~len:8 buf in
        assert (n = 8);
        decode_int64 (Bytes.unsafe_to_string buf)
    end

    module Version = struct
      let get t =
        let buf = Bytes.create 8 in
        let n = unsafe_read t ~off:8L ~len:8 buf in
        assert (n = 8);
        Bytes.unsafe_to_string buf

      let set t = unsafe_write t ~off:8L current_version
    end

    module Generation = struct
      let get t =
        let buf = Bytes.create 8 in
        let n = unsafe_read t ~off:16L ~len:8 buf in
        assert (n = 8);
        decode_int64 (Bytes.unsafe_to_string buf)

      let set t gen =
        let buf = encode_int64 gen in
        unsafe_write t ~off:16L buf
    end

    module Fan = struct
      let set t buf =
        let size = encode_int64 (Int64.of_int (String.length buf)) in
        unsafe_write t ~off:24L size;
        if buf <> "" then unsafe_write t ~off:(24L ++ 8L) buf

      let get_size t =
        let size_buf = Bytes.create 8 in
        let n = unsafe_read t ~off:24L ~len:8 size_buf in
        assert (n = 8);
        decode_int64 (Bytes.unsafe_to_string size_buf)

      let set_size t size =
        let buf = encode_int64 size in
        unsafe_write t ~off:24L buf

      let get t =
        let size = Int64.to_int (get_size t) in
        let buf = Bytes.create size in
        let n = unsafe_read t ~off:(24L ++ 8L) ~len:size buf in
        assert (n = size);
        Bytes.unsafe_to_string buf
    end
  end

  type t = {
    file : string;
    mutable header : int64;
    mutable raw : Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    mutable fan_size : int64;
    readonly : bool;
    version : string;
    buf : Buffer.t;
  }

  let sync ?(with_fsync = false) t =
    if t.readonly then raise RO_not_allowed;
    let buf = Buffer.contents t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if buf = "" then ()
    else (
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      Raw.Offset.set t.raw offset;
      assert (t.flushed ++ Int64.of_int (String.length buf) = t.header ++ offset);
      t.flushed <- offset ++ t.header );
    if with_fsync then Raw.fsync t.raw

  let name t = t.file

  let rename ~src ~dst =
    sync ~with_fsync:true src;
    Unix.close dst.raw.fd;
    Unix.rename src.file dst.file;
    Buffer.clear dst.buf;
    dst.header <- src.header;
    dst.fan_size <- src.fan_size;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let close t =
    if not t.readonly then Buffer.clear t.buf;
    Unix.close t.raw.fd

  let auto_flush_limit = 1_000_000L

  let append t buf =
    if t.readonly then raise RO_not_allowed;
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then sync t

  let read t ~off ~len buf =
    if not t.readonly then assert (t.header ++ off <= t.flushed);
    Raw.unsafe_read t.raw ~off:(t.header ++ off) ~len buf

  let offset t = t.offset

  let force_offset t =
    t.offset <- Raw.Offset.get t.raw;
    t.offset

  let version t = t.version

  let get_generation t =
    let i = Raw.Generation.get t.raw in
    Log.debug (fun m -> m "get_generation: %Ld" i);
    i

  let set_generation t i =
    Log.debug (fun m -> m "set_generation: %Ld" i);
    Raw.Generation.set t.raw i

  let get_fanout t = Raw.Fan.get t.raw

  let set_fanout t buf =
    assert (Int64.equal (Int64.of_int (String.length buf)) t.fan_size);
    Raw.Fan.set t.raw buf

  let readonly t = t.readonly

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with e -> protect_unix_exn e

  let safe f x = try f x with e -> ignore_enoent e

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then safe Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        protect (Unix.mkdir dir) 0o755;
        k () )
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let clear ?(keep_generation = false) t =
    t.offset <- 0L;
    t.flushed <- t.header;
    if not keep_generation then Raw.Generation.set t.raw 0L;
    Raw.Offset.set t.raw t.offset;
    Raw.Fan.set t.raw "";
    Buffer.clear t.buf

  let buffers = Hashtbl.create 256

  let buffer file =
    try Hashtbl.find buffers file
    with Not_found ->
      let buf = Buffer.create (4 * 1024) in
      Hashtbl.add buffers file buf;
      buf

  let () = assert (String.length current_version = 8)

  let v ~readonly ~fresh ~generation ~fan_size file =
    let v ~fan_size ~offset ~version raw =
      let header = 8L ++ 8L ++ 8L ++ 8L ++ fan_size in
      {
        version;
        header;
        file;
        offset;
        raw;
        readonly;
        fan_size;
        buf = buffer file;
        flushed = header ++ offset;
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let x = Unix.openfile file Unix.[ O_CREAT; O_CLOEXEC; mode ] 0o644 in
        let raw = Raw.v x in
        Raw.Offset.set raw 0L;
        Raw.Fan.set_size raw fan_size;
        Raw.Version.set raw;
        Raw.Generation.set raw generation;
        v ~fan_size ~offset:0L ~version:current_version raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; O_CLOEXEC; mode ] 0o644 in
        let raw = Raw.v x in
        if readonly && fresh then
          Fmt.failwith "IO.v: cannot reset a readonly file"
        else if fresh then (
          Raw.Offset.set raw 0L;
          Raw.Fan.set_size raw fan_size;
          Raw.Version.set raw;
          Raw.Generation.set raw generation;
          v ~fan_size ~offset:0L ~version:current_version raw )
        else
          let offset = Raw.Offset.get raw in
          let version = Raw.Version.get raw in
          let fan_size = Raw.Fan.get_size raw in
          v ~fan_size ~offset ~version raw

  type lock = { path : string; fd : Unix.file_descr }

  exception Locked of string

  let unsafe_lock op f =
    mkdir (Filename.dirname f);
    let fd = Unix.openfile f [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
    and pid = string_of_int (Unix.getpid ()) in
    let pid_len = String.length pid in
    try
      Unix.lockf fd op 0;
      if Unix.single_write_substring fd pid 0 pid_len <> pid_len then (
        Unix.close fd;
        failwith "Unable to write PID to lock file" )
      else Some fd
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _) ->
        Unix.close fd;
        None
    | e ->
        Unix.close fd;
        raise e

  let err_rw_lock path =
    let ic = open_in path in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Log.err (fun l ->
        l
          "Cannot lock %s: index is already opened in write mode by PID %d. \
           Current PID is %d."
          path pid (Unix.getpid ()));
    raise (Locked path)

  let lock path =
    Log.debug (fun l -> l "Locking %s" path);
    match unsafe_lock Unix.F_TLOCK path with
    | Some fd -> { path; fd }
    | None -> err_rw_lock path

  let unlock { path; fd } =
    Log.debug (fun l -> l "Unlocking %s" path);
    Unix.close fd
end

module Mutex = struct
  include Mutex

  let with_lock t f =
    Mutex.lock t;
    try
      let ans = f () in
      Mutex.unlock t;
      ans
    with e ->
      Mutex.unlock t;
      raise e
end

module Thread = struct
  type t = Thread.t option

  let async f = Some (Thread.create f ())

  let yield = Thread.yield

  let return () = None

  let await t = match t with None -> () | Some t -> Thread.join t
end

module Make (K : Index.Key) (V : Index.Value) =
  Index.Make (K) (V) (IO) (Mutex) (Thread)

module Private = struct
  module IO = IO
  module Make (K : Index.Key) (V : Index.Value) =
    Index.Private.Make (K) (V) (IO) (Mutex) (Thread)
end
