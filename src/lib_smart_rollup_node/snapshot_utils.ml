(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module type READER = sig
  type in_channel

  val open_in : string -> in_channel

  val really_input : in_channel -> bytes -> int -> int -> unit

  val input : in_channel -> bytes -> int -> int -> int

  val close_in : in_channel -> unit
end

module type WRITER = sig
  type out_channel

  val open_out : string -> out_channel

  val output : out_channel -> bytes -> int -> int -> unit

  val flush_continue : out_channel -> unit

  val close_out : out_channel -> unit
end

module type READER_INPUT = sig
  include READER

  val in_chan : in_channel
end

module type WRITER_OUTPUT = sig
  include WRITER

  val out_chan : out_channel
end

module Stdlib_reader : READER with type in_channel = Stdlib.in_channel = Stdlib

module Stdlib_writer : WRITER with type out_channel = Stdlib.out_channel =
struct
  include Stdlib

  let flush_continue = flush
end

module Gzip_reader : READER with type in_channel = Gzip.in_channel = Gzip

module Gzip_writer : WRITER with type out_channel = Gzip.out_channel = struct
  include Gzip

  let open_out f = open_out f
end

type reader = (module READER)

type writer = (module WRITER)

let stdlib_reader : reader = (module Stdlib_reader)

let stdlib_writer : writer = (module Stdlib_writer)

let gzip_reader : reader = (module Gzip_reader)

let gzip_writer : writer = (module Gzip_writer)

type snapshot_version = V0

type snapshot_metadata = {
  history_mode : Configuration.history_mode;
  address : Address.t;
  head_level : int32;
  last_commitment : Commitment.Hash.t;
}

let snapshot_version_encoding =
  let open Data_encoding in
  conv_with_guard
    (function V0 -> 0)
    (function
      | 0 -> Ok V0 | x -> Error ("Invalid snapshot version " ^ string_of_int x))
    int8

let snaphsot_metadata_encoding =
  let open Data_encoding in
  conv
    (fun {history_mode; address; head_level; last_commitment} ->
      (history_mode, address, head_level, last_commitment))
    (fun (history_mode, address, head_level, last_commitment) ->
      {history_mode; address; head_level; last_commitment})
  @@ obj4
       (req "history_mode" Configuration.history_mode_encoding)
       (req "address" Address.encoding)
       (req "head_level" int32)
       (req "last_commitment" Commitment.Hash.encoding)

let snapshot_metadata_size =
  Data_encoding.Binary.fixed_length snaphsot_metadata_encoding
  |> WithExceptions.Option.get ~loc:__LOC__

let version = V0

let write_snapshot_metadata (module Writer : WRITER_OUTPUT) metadata =
  let version_bytes =
    Data_encoding.Binary.to_bytes_exn snapshot_version_encoding version
  in
  let metadata_bytes =
    Data_encoding.Binary.to_bytes_exn snaphsot_metadata_encoding metadata
  in
  Writer.output Writer.out_chan version_bytes 0 (Bytes.length version_bytes) ;
  Writer.output Writer.out_chan metadata_bytes 0 (Bytes.length metadata_bytes)

let read_snapshot_metadata (module Reader : READER_INPUT) =
  let version_bytes = Bytes.create 1 in
  let metadata_bytes = Bytes.create snapshot_metadata_size in
  Reader.really_input Reader.in_chan version_bytes 0 1 ;
  Reader.really_input Reader.in_chan metadata_bytes 0 snapshot_metadata_size ;
  let snapshot_version =
    Data_encoding.Binary.of_bytes_exn snapshot_version_encoding version_bytes
  in
  assert (snapshot_version = version) ;
  Data_encoding.Binary.of_bytes_exn snaphsot_metadata_encoding metadata_bytes

let create (module Reader : READER) (module Writer : WRITER) metadata ~dir
    ~include_file ~dest =
  let module Archive_writer = Tar.Make (struct
    include Reader
    include Writer
  end) in
  let total =
    Tezos_stdlib_unix.Utils.directory_contents_size dir ~include_file
  in
  let progress_bar =
    Progress_bar.progress_bar
      ~counter:`Bytes
      ~message:"Exporting snapshot  "
      ~color:(Terminal.Color.rgb 3 132 252)
      total
  in
  Progress_bar.with_reporter progress_bar @@ fun count_progress ->
  let write_file file (out_chan : Writer.out_channel) =
    let in_chan = Reader.open_in file in
    try
      let buffer_size = 64 * 1024 in
      let buf = Bytes.create buffer_size in
      let rec copy () =
        let read_bytes = Reader.input in_chan buf 0 buffer_size in
        Writer.output out_chan buf 0 read_bytes ;
        count_progress read_bytes ;
        if read_bytes > 0 then copy ()
      in
      copy () ;
      Writer.flush_continue out_chan ;
      Reader.close_in in_chan
    with e ->
      Reader.close_in in_chan ;
      raise e
  in
  let file_stream =
    Tezos_stdlib_unix.Utils.list_files dir ~include_file
    @@ fun ~full_path ~relative_path ->
    let {Unix.st_perm; st_size; st_mtime; _} = Unix.lstat full_path in
    let header =
      Tar.Header.make
        ~file_mode:st_perm
        ~mod_time:(Int64.of_float st_mtime)
        relative_path
        (Int64.of_int st_size)
    in
    let writer = write_file full_path in
    (header, writer)
  in
  let out_chan = Writer.open_out dest in
  try
    write_snapshot_metadata
      (module struct
        include Writer

        let out_chan = out_chan
      end)
      metadata ;
    Archive_writer.Archive.create_gen file_stream out_chan ;
    Writer.close_out out_chan
  with e ->
    Writer.close_out out_chan ;
    raise e

let extract (module Reader : READER) (module Writer : WRITER) metadata_check
    ~snapshot_file ~dest =
  let open Lwt_result_syntax in
  let module Writer = struct
    include Writer

    let count_progress = ref (fun _ -> ())

    let output oc b p l =
      !count_progress 1 ;
      output oc b p l
  end in
  let module Archive_reader = Tar.Make (struct
    include Reader
    include Writer
  end) in
  let out_channel_of_header (header : Tar.Header.t) =
    let path = Filename.concat dest header.file_name in
    Tezos_stdlib_unix.Utils.create_dir (Filename.dirname path) ;
    Writer.open_out path
  in
  let in_chan = Reader.open_in snapshot_file in
  let reader_input : (module READER_INPUT) =
    (module struct
      include Reader

      let in_chan = in_chan
    end)
  in
  Lwt.finalize
    (fun () ->
      let metadata = read_snapshot_metadata reader_input in
      let* check_result = metadata_check metadata in
      let spinner = Progress_bar.spinner ~message:"Extracting snapshot" in
      Progress_bar.with_reporter spinner @@ fun count_progress ->
      Writer.count_progress := count_progress ;
      Archive_reader.Archive.extract_gen out_channel_of_header in_chan ;
      return (metadata, check_result))
    (fun () ->
      Reader.close_in in_chan ;
      Lwt.return_unit)

let compress ~snapshot_file =
  let Unix.{st_size = total; _} = Unix.stat snapshot_file in
  let progress_bar =
    Progress_bar.progress_bar
      ~counter:`Bytes
      ~message:"Compressing snapshot"
      ~color:(Terminal.Color.rgb 3 198 252)
      total
  in
  Progress_bar.with_reporter progress_bar @@ fun count_progress ->
  let snapshot_file_gz = Filename.chop_suffix snapshot_file ".uncompressed" in
  let in_chan = open_in snapshot_file in
  let out_chan = Gzip.open_out snapshot_file_gz in
  try
    let buffer_size = 64 * 1024 in
    let buf = Bytes.create buffer_size in
    let rec copy () =
      let read_bytes = input in_chan buf 0 buffer_size in
      Gzip.output out_chan buf 0 read_bytes ;
      count_progress read_bytes ;
      if read_bytes > 0 then copy ()
    in
    copy () ;
    Gzip.close_out out_chan ;
    close_in in_chan ;
    Unix.unlink snapshot_file ;
    snapshot_file_gz
  with e ->
    Gzip.close_out out_chan ;
    close_in in_chan ;
    raise e

let read_metadata (module Reader : READER) ~snapshot_file =
  let in_chan = Reader.open_in snapshot_file in
  let reader_input : (module READER_INPUT) =
    (module struct
      include Reader

      let in_chan = in_chan
    end)
  in
  try
    let metadata = read_snapshot_metadata reader_input in
    Reader.close_in in_chan ;
    metadata
  with e ->
    Reader.close_in in_chan ;
    raise e
