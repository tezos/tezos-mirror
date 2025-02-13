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

open! Import

let src = Logs.Src.create "index_unix" ~doc:"Index_unix"

module Log = (val Logs.src_log src : Logs.LOG)

exception RO_not_allowed

let current_version = "00000001"

module Stats = Brassaia_index.Index.Stats

module IO : Brassaia_index.Index.Platform.IO = struct
  let ( ++ ) = Int63.add
  let ( -- ) = Int63.sub

  type t = {
    mutable file : string;
    mutable header : int63;
    mutable raw : Raw.t;
    mutable offset : int63;
    mutable flushed : int63;
    mutable fan_size : int63;
    readonly : bool;
    buf : Buffer.t;
    flush_callback : unit -> unit;
  }

  let flush ?no_callback ?(with_fsync = false) t =
    if t.readonly then raise RO_not_allowed;
    if not (Buffer.is_empty t.buf) then (
      let buf_len = Buffer.length t.buf in
      let offset = t.offset in
      (match no_callback with Some () -> () | None -> t.flush_callback ());
      Log.debug (fun l -> l "[%s] flushing %d bytes" t.file buf_len);
      Buffer.write_with (Raw.unsafe_write t.raw ~off:t.flushed) t.buf;
      Buffer.clear t.buf;
      Raw.Offset.set t.raw offset;
      assert (t.flushed ++ Int63.of_int buf_len = t.header ++ offset);
      t.flushed <- offset ++ t.header);
    if with_fsync then Raw.fsync t.raw

  let rename ~src ~dst =
    flush ~with_fsync:true src;
    Raw.close dst.raw;
    Unix.rename src.file dst.file;
    Buffer.clear dst.buf;
    src.file <- dst.file;
    dst.header <- src.header;
    dst.fan_size <- src.fan_size;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let close t =
    if not t.readonly then Buffer.clear t.buf;
    Raw.close t.raw

  let auto_flush_limit = Int63.of_int 1_000_000

  let append_substring t buf ~off ~len =
    if t.readonly then raise RO_not_allowed;
    Buffer.add_substring t.buf buf ~off ~len;
    let len = Int63.of_int len in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then flush t

  let append t buf = append_substring t buf ~off:0 ~len:(String.length buf)

  let read t ~off ~len buf =
    let off = t.header ++ off in
    let end_of_value = off ++ Int63.of_int len in
    if not t.readonly then
      assert (
        let total_length = t.flushed ++ Int63.of_int (Buffer.length t.buf) in
        (* NOTE: we don't require that [end_of_value <= total_length] in order
           to support short reads on read-write handles (see comment about this
           case below). *)
        off <= total_length);

    if t.readonly || end_of_value <= t.flushed then
      (* Value is entirely on disk *)
      Raw.unsafe_read t.raw ~off ~len buf
    else
      (* Must read some data not yet flushed to disk *)
      let requested_from_disk = max 0 (Int63.to_int (t.flushed -- off)) in
      let requested_from_buffer = len - requested_from_disk in
      let read_from_disk =
        if requested_from_disk > 0 then (
          let read = Raw.unsafe_read t.raw ~off ~len:requested_from_disk buf in
          assert (read = requested_from_disk);
          read)
        else 0
      in
      let read_from_buffer =
        let src_off = max 0 (Int63.to_int (off -- t.flushed)) in
        let len =
          (* The user may request more bytes than actually exist, in which case
             we read to the end of the write buffer and return a size less than
             [len]. *)
          let available_length = Buffer.length t.buf - src_off in
          min available_length requested_from_buffer
        in
        Buffer.blit ~src:t.buf ~src_off ~dst:buf ~dst_off:requested_from_disk
          ~len;
        len
      in
      read_from_disk + read_from_buffer

  let offset t = t.offset

  let get_generation t =
    let i = Raw.Generation.get t.raw in
    Log.debug (fun m -> m "get_generation: %a" Int63.pp i);
    i

  let get_fanout t = Raw.Fan.get t.raw
  let get_fanout_size t = Raw.Fan.get_size t.raw

  let set_fanout t buf =
    assert (Int63.(equal (of_int (String.length buf)) t.fan_size));
    Raw.Fan.set t.raw buf

  module Header = struct
    type header = { offset : int63; generation : int63 }

    let pp ppf { offset; generation } =
      Format.fprintf ppf "{ offset = %a; generation = %a }" Int63.pp offset
        Int63.pp generation

    let get t =
      let Raw.Header.{ offset; generation; _ } = Raw.Header.get t.raw in
      t.offset <- offset;
      let headers = { offset; generation } in
      Log.debug (fun m -> m "[%s] get_headers: %a" t.file pp headers);
      headers

    let set t { offset; generation } =
      let version = current_version in
      Log.debug (fun m ->
          m "[%s] set_header %a" t.file pp { offset; generation });
      Raw.Header.(set t.raw { offset; version; generation })
  end

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
        (aux [@tailcall]) (Filename.dirname dir) (fun () ->
            protect (Unix.mkdir dir) 0o755;
            k ()))
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let raw_file ~flags ~version ~offset ~generation file =
    let x = Unix.openfile file flags 0o644 in
    let raw = Raw.v x in
    let header = { Raw.Header.offset; version; generation } in
    Log.debug (fun m ->
        m "[%s] raw set_header %a" file Header.pp { offset; generation });
    Raw.Header.set raw header;
    Raw.Fan.set raw "";
    Raw.fsync raw;
    raw

  let clear ~generation ?(hook = fun () -> ()) ~reopen t =
    t.offset <- Int63.zero;
    t.flushed <- t.header;
    Buffer.clear t.buf;
    let old = t.raw in

    if reopen then (
      (* Open a fresh file and rename it to ensure atomicity:
         concurrent readers should never see the file disapearing. *)
      let tmp_file = t.file ^ "_tmp" in
      t.raw <-
        raw_file ~version:current_version ~generation ~offset:Int63.zero
          ~flags:Unix.[ O_CREAT; O_RDWR; O_CLOEXEC ]
          tmp_file;
      Unix.rename tmp_file t.file)
    else
      (* Remove the file current file. This allows a fresh file to be
         created, before writing the new generation in the old file. *)
      Unix.unlink t.file;

    hook ();

    (* Set new generation in the old file. *)
    Raw.Header.set old
      { Raw.Header.offset = Int63.zero; generation; version = current_version };
    Raw.close old

  let () = assert (String.length current_version = 8)

  let v_instance ?(flush_callback = fun () -> ()) ~readonly ~fan_size ~offset
      file raw =
    let eight = Int63.of_int 8 in
    let header = eight ++ eight ++ eight ++ eight ++ fan_size in
    {
      header;
      file;
      offset;
      raw;
      readonly;
      fan_size;
      buf = Buffer.create (if readonly then 0 else 4 * 1024);
      flushed = header ++ offset;
      flush_callback;
    }

  let v ?flush_callback ~fresh ~generation ~fan_size file =
    let v = v_instance ?flush_callback ~readonly:false file in
    mkdir (Filename.dirname file);
    let header =
      { Raw.Header.offset = Int63.zero; version = current_version; generation }
    in
    match Sys.file_exists file with
    | false ->
        let x = Unix.openfile file Unix.[ O_CREAT; O_CLOEXEC; O_RDWR ] 0o644 in
        let raw = Raw.v x in
        Raw.Header.set raw header;
        Raw.Fan.set_size raw fan_size;
        v ~fan_size ~offset:Int63.zero raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; O_CLOEXEC; O_RDWR ] 0o644 in
        let raw = Raw.v x in
        if fresh then (
          Raw.Header.set raw header;
          Raw.Fan.set_size raw fan_size;
          Raw.fsync raw;
          v ~fan_size ~offset:Int63.zero raw)
        else
          let version = Raw.Version.get raw in
          if version <> current_version then
            Fmt.failwith "Io.v: unsupported version %s (current version is %s)"
              version current_version;

          let offset = Raw.Offset.get raw in
          let fan_size = Raw.Fan.get_size raw in
          v ~fan_size ~offset raw

  let v_readonly file =
    let v = v_instance ~readonly:true file in
    mkdir (Filename.dirname file);
    try
      let x = Unix.openfile file Unix.[ O_EXCL; O_CLOEXEC; O_RDONLY ] 0o644 in
      let raw = Raw.v x in
      try
        let version = Raw.Version.get raw in
        if version <> current_version then
          Fmt.failwith "Io.v: unsupported version %s (current version is %s)"
            version current_version;
        let offset = Raw.Offset.get raw in
        let fan_size = Raw.Fan.get_size raw in
        Ok (v ~fan_size ~offset raw)
      with Raw.Not_written ->
        (* The readonly instance cannot read a file that does not have a
           header.*)
        Raw.close raw;
        Error `No_file_on_disk
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        (* The readonly instance cannot open a non existing file. *)
        Error `No_file_on_disk
    | e -> raise e

  let exists = Sys.file_exists
  let size { raw; _ } = (Raw.fstat raw).st_size
  let size_header t = t.header |> Int63.to_int

  module Lock = struct
    type t = { path : string; fd : Unix.file_descr }

    exception Locked of string

    let unsafe_lock op f =
      mkdir (Filename.dirname f);
      let fd =
        Unix.openfile f [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600
      and pid = string_of_int (Unix.getpid ()) in
      let pid_len = String.length pid in
      try
        Unix.lockf fd op 0;
        if Unix.single_write_substring fd pid 0 pid_len <> pid_len then (
          Unix.close fd;
          failwith "Unable to write PID to lock file")
        else Some fd
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          Unix.close fd;
          None
      | e ->
          Unix.close fd;
          raise e

    let with_ic path f =
      let ic = open_in path in
      let a = f ic in
      close_in ic;
      a

    let err_rw_lock path =
      let line = with_ic path input_line in
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

    let pp_dump path =
      match Sys.file_exists path with
      | false -> None
      | true ->
          let contents =
            with_ic path (fun ic ->
                really_input_string ic (in_channel_length ic))
          in
          Some (fun ppf -> Fmt.string ppf contents)
  end
end

module Semaphore = struct
  module S = Semaphore_compat.Semaphore.Binary

  let is_held t =
    let acquired = S.try_acquire t in
    if acquired then S.release t;
    not acquired

  include S

  let acquire n t =
    let x = Mtime_clock.counter () in
    S.acquire t;
    let y = Mtime_clock.count x in
    if Mtime.span_to_s y > 1. then
      Log.warn (fun l -> l "Semaphore %s was blocked for %a" n Mtime.Span.pp y)

  let with_acquire n t f =
    acquire n t;
    Fun.protect ~finally:(fun () -> S.release t) f
end

module Thread = struct
  type 'a t =
    | Async of { thread : Thread.t; result : ('a, exn) result option ref }
    | Value of 'a

  let async f =
    let result = ref None in
    let protected_f x =
      try result := Some (Ok (f x))
      with exn ->
        result := Some (Error exn);
        raise exn
    in
    let thread = Thread.create protected_f () in
    Async { thread; result }

  let yield = Thread.yield
  let return a = Value a

  let await t =
    match t with
    | Value v -> Ok v
    | Async { thread; result } -> (
        let () = Thread.join thread in
        match !result with
        | Some (Ok _ as o) -> o
        | Some (Error exn) -> Error (`Async_exn exn)
        | None -> assert false)
end

module Platform = struct
  module IO = IO
  module Semaphore = Semaphore
  module Thread = Thread
  module Clock = Mtime_clock
  module Progress = Progress
  module Fmt_tty = Fmt_tty
end

module Make (K : Brassaia_index.Index.Key.S) (V : Brassaia_index.Index.Value.S) =
  Brassaia_index.Index.Make (K) (V) (Platform)

module Syscalls = Syscalls

module Private = struct
  module Platform = Platform
  module IO = IO
  module Raw = Raw

  module Make
      (K : Brassaia_index.Index.Key.S)
      (V : Brassaia_index.Index.Value.S) =
    Brassaia_index.Index.Private.Make (K) (V) (Platform)
end
