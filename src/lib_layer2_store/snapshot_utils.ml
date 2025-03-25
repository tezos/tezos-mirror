(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023-2024 Functori <contact@functori.com>         *)
(*                                                                           *)
(*****************************************************************************)

module type READER = sig
  type in_channel

  val format : [`Compressed | `Uncompressed]

  val open_in_chan : Stdlib.in_channel -> in_channel

  val really_input : in_channel -> bytes -> int -> int -> unit

  val input_char : in_channel -> char

  val input : in_channel -> bytes -> int -> int -> int

  val close_in : in_channel -> unit
end

module type WRITER = sig
  type out_channel

  val format : [`Compressed | `Uncompressed]

  val open_out : string -> out_channel

  val output : out_channel -> bytes -> int -> int -> unit

  val flush_continue : out_channel -> unit

  val close_out : out_channel -> unit
end

module type READER_INPUT = sig
  include READER

  val in_chan : in_channel

  val source : [`Local of string | `Remote of string]
end

module type WRITER_OUTPUT = sig
  include WRITER

  val out_chan : out_channel
end

module Stdlib_reader : READER with type in_channel = Stdlib.in_channel = struct
  include Stdlib

  let format = `Uncompressed

  let open_in_chan ic = ic
end

module Stdlib_writer : WRITER with type out_channel = Stdlib.out_channel =
struct
  include Stdlib

  let format = `Uncompressed

  let flush_continue = flush
end

module Gzip_reader : READER with type in_channel = Gzip.in_channel = struct
  include Gzip

  let format = `Compressed
end

module Gzip_writer : WRITER with type out_channel = Gzip.out_channel = struct
  include Gzip

  let format = `Compressed

  let open_out f = open_out f
end

type reader = (module READER)

type reader_input = (module READER_INPUT)

type writer = (module WRITER)

let stdlib_reader : reader = (module Stdlib_reader)

let stdlib_writer : writer = (module Stdlib_writer)

let gzip_reader : reader = (module Gzip_reader)

let gzip_writer : writer = (module Gzip_writer)

let reader_format (module Reader : READER) = Reader.format

let input_format (module Reader : READER_INPUT) = Reader.format

let input_source (module Reader : READER_INPUT) = Reader.source

let run ~cancellable k =
  if cancellable then
    (* [Lwt_preemptive] does not yet provide a way to cancel a detached
       computation.

       As a temporary fix, we use [Lwt.wrap_in_cancelable]. The promise
       created by [detach] is cancelled. The detached computation keeps
       running, which is less than ideal, but it is reasonable because
       our use case for cancellation leads to the program exiting. *)
    Lwt.wrap_in_cancelable (Lwt_preemptive.detach k ())
  else Lwt.return (k ())

(* Magic bytes for gzip files is 1f8b. *)
let is_compressed_snapshot ic =
  let pos = pos_in ic in
  try
    let c1 = input_byte ic in
    let c2 = input_byte ic in
    let ok = c1 = 0x1f && c2 = 0x8b in
    (* [seek_in] does not work in general for non regular file channels. However
       we abuse it a little to rely on the IO buffer because we're only reading
       the first two bytes so we're guaranteed to only move in this buffer (see
       https://github.com/ocaml/ocaml/blob/trunk/runtime/caml/misc.h#L806) and
       not actually call lseek (see
       https://github.com/ocaml/ocaml/blob/trunk/runtime/io.c#L464.) *)
    seek_in ic pos ;
    ok
  with End_of_file ->
    seek_in ic pos ;
    false

let periodic_report event =
  let open Lwt_syntax in
  let start_time = Ptime_clock.now () in
  let rec aux () =
    let* () = Lwt_unix.sleep 60. in
    let elapsed_time = Ptime.diff (Ptime_clock.now ()) start_time in
    let* () = event elapsed_time in
    aux ()
  in
  aux ()

let create_reader_input (type a) ~src
    (module Reader : READER with type in_channel = a)
    (in_chan : Stdlib.in_channel) :
    (module READER_INPUT with type in_channel = a) =
  (module struct
    include Reader

    let in_chan = open_in_chan in_chan

    let source = src
  end)

let create_reader_input : src:_ -> reader -> in_channel -> reader_input =
 fun ~src (module R) s ->
  let r = create_reader_input ~src (module R) s in
  let module R : READER_INPUT = (val r) in
  (module R)

module Make (Header : sig
  type t

  val encoding : t Data_encoding.t
end) =
struct
  let write_snapshot_header (module Writer : WRITER_OUTPUT) header =
    let header_bytes =
      Data_encoding.Binary.to_bytes_exn Header.encoding header
    in
    Writer.output Writer.out_chan header_bytes 0 (Bytes.length header_bytes)

  let read_snapshot_header (module Reader : READER_INPUT) =
    let read_char () =
      let c = Reader.input_char Reader.in_chan in
      Bytes.init 1 (fun _ -> c)
    in
    let rec loop = function
      | Data_encoding.Binary.Success {result; size = _; stream = _} -> result
      | Await k -> loop (k (read_char ()))
      | Error e ->
          Format.kasprintf
            Stdlib.failwith
            "Error reading snapshot header: %a"
            Data_encoding.Binary.pp_read_error
            e
    in
    loop (Data_encoding.Binary.read_stream Header.encoding)

  let run_progress ~display_progress k =
    match display_progress with
    | `Bar progress_bar -> Progress_bar.with_reporter progress_bar k
    | `Periodic_event mk_event ->
        let progress = ref 0 in
        Lwt.pick
          [
            k (fun i -> progress := !progress + i);
            periodic_report (fun elapsed_time ->
                mk_event ~progress:!progress elapsed_time);
          ]

  let create (module Writer : WRITER) header ~cancellable ~display_progress
      ~files ~dest () =
    let module Archive_writer = Tar.Make (struct
      include Stdlib_reader
      include Writer
    end) in
    let total =
      List.fold_left
        (fun total (file, _) ->
          let {Unix.st_size; _} = Unix.lstat file in
          total + st_size)
        0
        files
    in
    let display_progress =
      match display_progress with
      | `Bar ->
          `Bar
            (Progress_bar.progress_bar
               ~counter:`Bytes
               ~message:"Exporting snapshot  "
               ~color:(Terminal.Color.rgb 3 132 252)
               total)
      | `Periodic_event mk_event -> `Periodic_event (mk_event ~total)
    in
    run_progress ~display_progress @@ fun count_progress ->
    let write_file file (out_chan : Writer.out_channel) =
      let in_chan = open_in file in
      try
        let buffer_size = 64 * 1024 in
        let buf = Bytes.create buffer_size in
        let rec copy () =
          let read_bytes = input in_chan buf 0 buffer_size in
          Writer.output out_chan buf 0 read_bytes ;
          count_progress read_bytes ;
          if read_bytes > 0 then copy ()
        in
        copy () ;
        Writer.flush_continue out_chan ;
        close_in in_chan
      with e ->
        close_in in_chan ;
        raise e
    in
    let file_stream =
      List.rev_map
        (fun (full_path, path_in_snapshot) ->
          let {Unix.st_perm; st_size; st_mtime; _} = Unix.lstat full_path in
          let header =
            Tar.Header.make
              ~file_mode:st_perm
              ~mod_time:(Int64.of_float st_mtime)
              path_in_snapshot
              (Int64.of_int st_size)
          in
          let writer = write_file full_path in
          (header, writer))
        files
      |> Stream.of_list
    in
    let out_chan = Writer.open_out dest in
    run ~cancellable @@ fun () ->
    try
      write_snapshot_header
        (module struct
          include Writer

          let out_chan = out_chan
        end)
        header ;
      Archive_writer.Archive.create_gen file_stream out_chan ;
      Writer.close_out out_chan
    with e ->
      Writer.close_out out_chan ;
      raise e

  let extract (reader_input : (module READER_INPUT)) ~cancellable
      ~display_progress ~dest =
    let open Lwt_syntax in
    let module Writer = struct
      type out_channel = Stdlib.out_channel

      let count_progress = ref (fun _ -> ())

      let output oc b p l =
        !count_progress 1 ;
        output oc b p l

      let close_out = Stdlib.close_out
    end in
    let module Reader = (val reader_input) in
    let module Archive_reader = Tar.Make (struct
      include Reader
      include Writer
    end) in
    let out_channel_of_header (header : Tar.Header.t) =
      let path = Filename.concat dest header.file_name in
      Tezos_stdlib_unix.Utils.create_dir (Filename.dirname path) ;
      open_out path
    in
    let display_progress =
      match display_progress with
      | `Bar -> `Bar (Progress_bar.spinner ~message:"Extracting snapshot")
      | `Periodic_event mk_event ->
          `Periodic_event (fun ~progress:_ -> mk_event)
    in
    Lwt.finalize
      (fun () ->
        run_progress ~display_progress @@ fun count_progress ->
        Writer.count_progress := count_progress ;
        run ~cancellable (fun () ->
            Archive_reader.Archive.extract_gen
              out_channel_of_header
              Reader.in_chan))
      (fun () ->
        Reader.close_in Reader.in_chan ;
        return_unit)

  let compress ~cancellable ~display_progress ~snapshot_file () =
    let Unix.{st_size = total; _} = Unix.stat snapshot_file in
    let display_progress =
      match display_progress with
      | `Bar ->
          `Bar
            (Progress_bar.progress_bar
               ~counter:`Bytes
               ~message:"Compressing snapshot"
               ~color:(Terminal.Color.rgb 3 198 252)
               total)
      | `Periodic_event mk_event -> `Periodic_event (mk_event ~total)
    in
    run_progress ~display_progress @@ fun count_progress ->
    let snapshot_file_gz = Filename.chop_suffix snapshot_file ".uncompressed" in
    let in_chan = open_in snapshot_file in
    let out_chan = Gzip.open_out snapshot_file_gz in
    run ~cancellable @@ fun () ->
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

  let with_open_snapshot file f =
    let open Lwt_result_syntax in
    let in_chan = open_in file in
    let reader =
      if is_compressed_snapshot in_chan then gzip_reader else stdlib_reader
    in
    let reader_input = create_reader_input reader in_chan ~src:(`Local file) in
    protect
      ~on_error:(fun error ->
        let module Reader = (val reader_input) in
        Reader.close_in Reader.in_chan ;
        fail error)
      (fun () ->
        let header = read_snapshot_header reader_input in
        f header reader_input)
end
