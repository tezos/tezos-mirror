exception RO_Not_Allowed

let current_version = "00000001"

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
    Bytes.to_string b

  let decode_int64 buf =
    let get_uint64 s off =
      if not Sys.big_endian then swap64 (get_64 s off) else get_64 s off
    in
    get_uint64 buf 0

  module Raw = struct
    type t = { fd : Unix.file_descr; mutable cursor : int64 }

    let v fd = { fd; cursor = 0L }

    let really_write fd buf =
      let rec aux off len =
        let w = Unix.write fd buf off len in
        if w = 0 then () else (aux [@tailcall]) (off + w) (len - w)
      in
      (aux [@tailcall]) 0 (Bytes.length buf)

    let really_read fd len buf =
      let rec aux off len =
        let r = Unix.read fd buf off len in
        if r = 0 then off (* end of file *)
        else if r = len then off + r
        else (aux [@tailcall]) (off + r) (len - r)
      in
      (aux [@tailcall]) 0 len

    let lseek t off =
      if off = t.cursor then ()
      else
        let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
        t.cursor <- off

    let unsafe_write t ~off buf =
      lseek t off;
      let buf = Bytes.unsafe_of_string buf in
      really_write t.fd buf;
      t.cursor <- off ++ Int64.of_int (Bytes.length buf)

    let unsafe_read t ~off ~len buf =
      lseek t off;
      let n = really_read t.fd len buf in
      t.cursor <- off ++ Int64.of_int n;
      n

    let unsafe_set_offset t n =
      let buf = encode_int64 n in
      unsafe_write t ~off:0L buf

    let int64_buf = Bytes.create 8

    let unsafe_get_offset t =
      let n = unsafe_read t ~off:0L ~len:8 int64_buf in
      assert (n = 8);
      decode_int64 (Bytes.unsafe_to_string int64_buf)

    let version_buf = Bytes.create 8

    let generation_buf = Bytes.create 8

    let unsafe_get_version t =
      let n = unsafe_read t ~off:8L ~len:8 version_buf in
      assert (n = 8);
      Bytes.to_string version_buf

    let unsafe_set_version t = unsafe_write t ~off:8L current_version

    let unsafe_get_generation t =
      let n = unsafe_read t ~off:16L ~len:8 generation_buf in
      assert (n = 8);
      decode_int64 (Bytes.unsafe_to_string generation_buf)

    let unsafe_set_generation t gen =
      let buf = encode_int64 gen in
      unsafe_write t ~off:16L buf
  end

  type t = {
    file : string;
    mutable raw : Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    readonly : bool;
    version : string;
    buf : Buffer.t;
  }

  let header = 24L (* offset + version + generation *)

  let sync t =
    if t.readonly then raise RO_Not_Allowed;
    let buf = Buffer.contents t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if buf = "" then ()
    else (
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      Raw.unsafe_set_offset t.raw offset;

      (* concurrent append might happen so here t.offset might differ
         from offset *)
      if not (t.flushed ++ Int64.of_int (String.length buf) = header ++ offset)
      then
        Fmt.failwith "sync error: %s flushed=%Ld buf=%Ld offset+header=%Ld\n%!"
          t.file t.flushed
          (Int64.of_int (String.length buf))
          (offset ++ header);
      t.flushed <- offset ++ header )

  let name t = t.file

  let rename ~src ~dst =
    sync src;
    Unix.close dst.raw.fd;
    Unix.rename src.file dst.file;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let auto_flush_limit = 1_000_000L

  let append t buf =
    if t.readonly then raise RO_Not_Allowed;
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then sync t

  let read t ~off buf =
    if not t.readonly then assert (header ++ off <= t.flushed);
    Raw.unsafe_read t.raw ~off:(header ++ off) ~len:(Bytes.length buf) buf

  let offset t = t.offset

  let force_offset t =
    t.offset <- Raw.unsafe_get_offset t.raw;
    t.offset

  let version t = t.version

  let get_generation t = Raw.unsafe_get_generation t.raw

  let set_generation t gen = Raw.unsafe_set_generation t.raw gen

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

  let clear t =
    t.offset <- 0L;
    t.flushed <- header;
    Raw.unsafe_set_offset t.raw t.offset;
    Buffer.clear t.buf

  let buffers = Hashtbl.create 256

  let buffer file =
    try
      let buf = Hashtbl.find buffers file in
      Buffer.clear buf;
      buf
    with Not_found ->
      let buf = Buffer.create (4 * 1024) in
      Hashtbl.add buffers file buf;
      buf

  let () = assert (String.length current_version = 8)

  let v ~readonly ~fresh ~generation file =
    let v ~offset ~version raw =
      {
        version;
        file;
        offset;
        raw;
        readonly;
        buf = buffer file;
        flushed = header ++ offset;
      }
    in
    let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        if readonly then raise RO_Not_Allowed;
        let x = Unix.openfile file Unix.[ O_CREAT; mode ] 0o644 in
        let raw = Raw.v x in
        Raw.unsafe_set_offset raw 0L;
        Raw.unsafe_set_version raw;
        Raw.unsafe_set_generation raw generation;
        v ~offset:0L ~version:current_version raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; mode ] 0o644 in
        let raw = Raw.v x in
        if readonly && fresh then
          Fmt.failwith "IO.v: cannot reset a readonly file"
        else if fresh then (
          Raw.unsafe_set_offset raw 0L;
          Raw.unsafe_set_version raw;
          Raw.unsafe_set_generation raw generation;
          v ~offset:0L ~version:current_version raw )
        else
          let offset = Raw.unsafe_get_offset raw in
          let version = Raw.unsafe_get_version raw in
          v ~offset ~version raw
end

module Make (K : Index.Key) (V : Index.Value) = Index.Make (K) (V) (IO)
