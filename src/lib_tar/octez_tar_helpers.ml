(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2020-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

include Tar

let rw_file_perm = 0o644

let ro_file_perm = 0o444

module Reader = struct
  type in_channel = Lwt_unix.file_descr

  type 'a t = 'a Lwt.t

  let really_read fd = Lwt_cstruct.(complete (read fd))

  let skip (ifd : Lwt_unix.file_descr) (n : int) =
    let open Lwt_syntax in
    let buffer_size = 32768 in
    let buffer = Cstruct.create buffer_size in
    let rec loop (n : int) =
      if n <= 0 then Lwt.return_unit
      else
        let amount = min n buffer_size in
        let block = Cstruct.sub buffer 0 amount in
        let* () = really_read ifd block in
        loop (n - amount)
    in
    loop n
end

module Writer = struct
  type out_channel = Lwt_unix.file_descr

  type 'a t = 'a Lwt.t

  let really_write fd = Lwt_cstruct.(complete (write fd))
end

module HR = struct
  include Tar.HeaderReader (Lwt) (Reader)

  let read ic = read ~level:Posix ic
end

module HW = struct
  include Tar.HeaderWriter (Lwt) (Writer)

  let write oc = write ~level:Posix oc
end

type file = {header : Tar.Header.t; data_ofs : Int64.t}

type o = {
  mutable current_pos : Int64.t;
  mutable data_pos : Int64.t;
  fd : Lwt_unix.file_descr;
}

let open_out ~file =
  let open Lwt_syntax in
  let* fd = Lwt_unix.openfile file Unix.[O_WRONLY; O_CREAT] rw_file_perm in
  let data_pos = Int64.of_int (Header.length * 3) in
  let* _ = Lwt_unix.LargeFile.lseek fd data_pos SEEK_SET in
  Lwt.return {current_pos = 0L; data_pos; fd}

(* Writes the double zero blocks to close the archive, as it is
     defined in the RFC.*)
let close_out t =
  let open Lwt_syntax in
  let* _eof = Lwt_unix.LargeFile.lseek t.fd t.current_pos SEEK_SET in
  let* () = Writer.really_write t.fd Tar.Header.zero_block in
  let* () = Writer.really_write t.fd Tar.Header.zero_block in
  Lwt_unix.close t.fd

(* Builds a tar header for the given sequence of bytes *)
let header_of_bytes ~filename ~data_size (file : Lwt_unix.file_descr) :
    Header.t Lwt.t =
  let open Lwt_syntax in
  let* stat = Lwt_unix.LargeFile.fstat file in
  let file_mode = stat.Lwt_unix.LargeFile.st_perm in
  let user_id = stat.Lwt_unix.LargeFile.st_uid in
  let group_id = stat.Lwt_unix.LargeFile.st_gid in
  let mod_time = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
  let link_indicator = Tar.Header.Link.Normal in
  let link_name = "" in
  let devmajor = 0 in
  let devminor = 0 in
  (* Enforce the extended header version (Posix aka pax). All tar
       headers are then expected to be of size [Tar.Header.length * 3
       = 512B x 3]. It is only necessary to set a single field to
       trigger this behavior in the [Tar] library. *)
  let extended =
    Some
      {
        Tar.Header.Extended.access_time = None;
        charset = None;
        comment = None;
        group_id = None;
        gname = None;
        header_charset = None;
        link_path = None;
        mod_time = None;
        path = None;
        file_size = Some data_size;
        user_id = None;
        uname = None;
      }
  in
  let header =
    Tar.Header.make
      ~file_mode
      ~user_id
      ~group_id
      ~mod_time
      ~link_indicator
      ~link_name
      ~devmajor
      ~devminor
      filename
      data_size
  in
  let header = {header with extended} in
  Lwt.return header

(* [finalize tar ~bytes_written ~filename] writes the header
     corresponding to the quantity of data given through
     [bytes_written] in the [tar]. Then, it finalizes the file and returns a new
     handle. The file descriptor of that handle is positioned to allow
     writing data. *)
let finalize t ~bytes_written ~filename =
  let open Lwt_syntax in
  (* Build the header based on the bytes_written *)
  let* header = header_of_bytes ~filename ~data_size:bytes_written t.fd in
  (* We are building extended headers which are 512B x 3. *)
  let header_length = Int64.of_int (Header.length * 3) in
  (* Compute and write the adequate padding for finalizing a data block *)
  let c = Tar.Header.zero_padding header in
  let zero_padding = Cstruct.to_bytes c in
  let zero_padding_length = Bytes.length zero_padding in
  (* Make sure that the fd position is after the written data *)
  let* _ =
    Lwt_unix.LargeFile.lseek t.fd (Int64.add t.data_pos bytes_written) SEEK_SET
  in
  let* _ = Lwt_unix.write t.fd zero_padding 0 zero_padding_length in
  (* Go back to the header position to write it *)
  let* _ = Lwt_unix.LargeFile.lseek t.fd t.current_pos SEEK_SET in
  let* () = HW.write header t.fd in
  let next_block_start =
    Int64.(
      add
        (add t.current_pos header_length)
        (add bytes_written (of_int zero_padding_length)))
  in
  let next_data_pos = Int64.(add next_block_start header_length) in
  (* Set fd position to be ready for next data write *)
  let* _ = Lwt_unix.LargeFile.lseek t.fd next_data_pos SEEK_SET in
  t.current_pos <- next_block_start ;
  t.data_pos <- next_data_pos ;
  Lwt.return_unit

let add_raw_and_finalize t ~f ~filename =
  let open Lwt_syntax in
  let* res =
    Lwt.catch
      (fun () -> f t.fd)
      (function
        | exn ->
        (* Rewind file descriptor to the start of the current data
                 slot. Then, the next write will overwrite the corrupted
                 data. *)
        let* _ = Lwt_unix.LargeFile.lseek t.fd t.data_pos SEEK_SET in
        Lwt.fail exn)
  in
  let* eor = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
  let bytes_written = Int64.sub eor t.data_pos in
  let* () = finalize t ~bytes_written ~filename in
  Lwt.return res

let copy_n ifd ofd n ~buffer_size =
  let open Lwt_syntax in
  let buffer = Cstruct.create buffer_size in
  let rec loop remaining =
    if remaining = 0L then Lwt.return_unit
    else
      let this = Int64.(to_int (min (of_int buffer_size) remaining)) in
      let block = Cstruct.sub buffer 0 this in
      let* () = Reader.really_read ifd block in
      let* () = Writer.really_write ofd block in
      loop Int64.(sub remaining (of_int this))
  in
  loop n

let add_file_and_finalize tar ~file ~filename ~buffer_size =
  let open Lwt_syntax in
  let* fd = Lwt_unix.openfile file [Unix.O_RDONLY] ro_file_perm in
  let* stat = Lwt_unix.LargeFile.fstat fd in
  let file_size = stat.st_size in
  let* () = copy_n fd tar.fd file_size ~buffer_size in
  let* () = finalize tar ~bytes_written:file_size ~filename in
  let* () = Lwt_unix.close fd in
  Lwt.return_unit

let rec readdir dir_handler =
  let open Lwt_syntax in
  Option.catch_os
    ~catch_only:(function End_of_file -> true | _ -> false)
    (fun () ->
      let* d = Lwt_unix.readdir dir_handler in
      match d with
      | filename
        when filename = Filename.current_dir_name
             || filename = Filename.parent_dir_name ->
          readdir dir_handler
      | any -> Lwt.return_some any)

let enumerate path =
  let open Lwt_syntax in
  let rec aux prefix dir_handler acc =
    let* o = readdir dir_handler in
    match o with
    | Some any ->
        let full_path = Filename.concat prefix any in
        if Sys.is_directory full_path then
          let* new_dir_handler = Lwt_unix.opendir full_path in
          let* sub_folder = aux full_path new_dir_handler [] in
          let* () = Lwt_unix.closedir new_dir_handler in
          aux prefix dir_handler (sub_folder @ acc)
        else aux prefix dir_handler (full_path :: acc)
    | None -> Lwt.return acc
  in
  let* dir_handler = Lwt_unix.opendir path in
  let* res = aux path dir_handler [] in
  let* () = Lwt_unix.closedir dir_handler in
  Lwt.return res

let add_directory_and_finalize ?archive_prefix tar ~dir_path ~buffer_size =
  let open Lwt_syntax in
  let dir_prefix = Filename.dirname dir_path in
  let* file_paths = enumerate dir_path in
  let archive_prefix = Option.value archive_prefix ~default:dir_prefix in
  let files =
    let dir_length = String.length dir_prefix in
    List.map
      (fun file_path ->
        let filename =
          String.sub
            file_path
            (dir_length + 1)
            String.(length file_path - dir_length - 1)
        in
        (file_path, filename))
      file_paths
  in
  List.iter_s
    (fun (file, filename) ->
      add_file_and_finalize
        tar
        ~file
        ~filename:Filename.(concat archive_prefix filename)
        ~buffer_size)
    files

type i = {
  mutable current_pos : Int64.t;
  mutable data_pos : Int64.t;
  fd : Lwt_unix.file_descr;
  mutable files : file list option;
}

let open_in ~file =
  let open Lwt_syntax in
  let* fd = Lwt_unix.openfile file Unix.[O_RDONLY] ro_file_perm in
  (* We need to retrieve the first header's length. [Tar] will shift
     the offset to the data location in the file: we can infer the
     length from it. *)
  let* _header = HR.read fd in
  let* data_pos = Lwt_unix.LargeFile.lseek fd 0L SEEK_CUR in
  let* _ = Lwt_unix.LargeFile.lseek fd 0L SEEK_SET in
  let files = None in
  Lwt.return {current_pos = 0L; data_pos; fd; files}

let close_in t =
  Lwt.catch
    (fun () -> Lwt_unix.close t.fd)
    (function
      | Unix.(Unix_error (EBADF, _, _)) -> Lwt.return_unit | exn -> Lwt.fail exn)

(*[list_files tar] returns the list of files contained in the
     [tar]. *)
let list_files t =
  let open Lwt_syntax in
  let* _ = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_SET in
  (* This implementation is way faster than the one implemented in
       Tar_lwt_unix.Archive.list function which reads the whole file
    *)
  let rec loop pos acc =
    let* _ = Lwt_unix.LargeFile.lseek t.fd pos SEEK_SET in
    let* _ = Lwt_unix.lseek t.fd 0 SEEK_CUR in
    let* r = HR.read t.fd in
    match r with
    | Error `Eof -> Lwt.return (List.rev acc)
    | Ok hdr ->
        (* Header length can be 1024 if extended *)
        let* data_pos = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
        let header_length = Int64.sub data_pos pos in
        let file_size = hdr.Tar.Header.file_size in
        let padding =
          Int64.of_int (Tar.Header.compute_zero_padding_length hdr)
        in
        let next_header = Int64.(add (add file_size padding) header_length) in
        let* _ = Lwt_unix.LargeFile.lseek t.fd next_header SEEK_SET in
        let h = {header = hdr; data_ofs = data_pos} in
        loop (Int64.add pos next_header) (h :: acc)
  in
  loop 0L []

let update_files t files = t.files <- Some files

let may_update_files t files =
  match t.files with Some _ -> () | None -> update_files t files

let get_files t =
  let open Lwt_syntax in
  match t.files with
  | Some files -> Lwt.return files
  | None ->
      let* files = list_files t in
      update_files t files ;
      Lwt.return files

let get_file tar ~filename =
  let open Lwt_syntax in
  let* files = get_files tar in
  Lwt.return
    (List.find_opt (fun {header; _} -> header.file_name = filename) files)

let get_filename {header; _} = header.Tar.Header.file_name

let get_file_size {header; _} = header.Tar.Header.file_size

(*[get_raw tar file] loads the [file] from [tar] in memory *)
let get_raw t {header; data_ofs} =
  let open Lwt_syntax in
  let* _ = Lwt_unix.LargeFile.lseek t.fd data_ofs SEEK_SET in
  let data_size = Int64.to_int header.file_size in
  let buf = Bytes.create data_size in
  let* _ = Lwt_unix.read t.fd buf 0 data_size in
  Lwt.return (Bytes.unsafe_to_string buf)

let get_raw_input_fd {fd; _} = fd

let get_raw_file_ofs {data_ofs; _} = data_ofs

let find_file t ~filename =
  let open Lwt_syntax in
  (* If the files were already listed, there is no need to read the whole tar archive.*)
  match t.files with
  | Some _ -> get_file t ~filename
  | None ->
      let* _ = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_SET in
      let rec loop pos acc =
        let* _ = Lwt_unix.LargeFile.lseek t.fd pos SEEK_SET in
        let* _ = Lwt_unix.lseek t.fd 0 SEEK_CUR in
        let* r = HR.read t.fd in
        match r with
        | Error `Eof ->
            (* If the end of file is reached, all the files were
                 enumerated without finding the expected one. In this case,
                 the files are updated. *)
            may_update_files t acc ;
            Lwt.return_none
        | Ok hdr ->
            (* Header length is 512B x 3 when extended (which is
                 now the default). *)
            let* data_pos = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
            if hdr.file_name = filename then
              Lwt.return_some {header = hdr; data_ofs = data_pos}
            else
              let header_length = Int64.(sub data_pos pos) in
              let file_size = hdr.Tar.Header.file_size in
              let padding =
                Int64.of_int (Tar.Header.compute_zero_padding_length hdr)
              in
              let next_header_pos =
                Int64.(add pos (add (add file_size padding) header_length))
              in
              let h = {header = hdr; data_ofs = data_pos} in
              loop next_header_pos (h :: acc)
      in
      loop 0L []

let find_files_with_common_path t ~pattern =
  let open Lwt_syntax in
  let* files = get_files t in
  let pattern = Re.compile (Re.Perl.re pattern) in
  Lwt.return
    (List.filter
       (fun {header; _} -> Re.execp pattern header.Tar.Header.file_name)
       files)

let load_file t file = get_raw t file

let load_from_filename t ~filename =
  let open Lwt_syntax in
  let* o = get_file t ~filename in
  match o with
  | Some hd ->
      let* str = get_raw t hd in
      Lwt.return_some str
  | None -> Lwt.return_none

let copy_to_file tar {header; data_ofs} ~dst ~buffer_size =
  let open Lwt_syntax in
  let* _ = Lwt_unix.LargeFile.lseek tar.fd data_ofs SEEK_SET in
  let* fd =
    Lwt_unix.openfile dst Unix.[O_WRONLY; O_CREAT; O_TRUNC] rw_file_perm
  in
  Lwt.finalize
    (fun () -> copy_n tar.fd fd header.Tar.Header.file_size ~buffer_size)
    (fun () -> Lwt_unix.close fd)
