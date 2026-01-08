(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type chunk_size = {min : int; avg : int; max : int}

(* Default for desync is 16:64:256 but we use larger values to have less chunks
   in order to not reach the inodes limit. *)
let default_chunk_size = {min = 256; avg = 1024; max = 4096}

let chunk_size_arg {min; avg; max} = Format.sprintf "%d:%d:%d" min avg max

let default_chunk_size_arg = chunk_size_arg default_chunk_size

let metadata_filename = ".snapshot_metadata.json"

let metadata_file dir = Filename.concat dir metadata_filename

let concurrency = Domain.recommended_domain_count () |> string_of_int

let check_desync_available =
  let memo = String.Hashtbl.create 2 in
  fun ~desync_path ->
    let open Lwt_result_syntax in
    match String.Hashtbl.find memo desync_path with
    | Some check -> check
    | None ->
        let check =
          let*! status =
            Process_manager.with_process_full
              ("which", [|"which"; desync_path|])
              (fun pc -> pc#status)
          in
          match status with
          | Unix.WEXITED 0 -> return_unit
          | _ ->
              failwith
                "The desync tool is not available. Please install it from \
                 https://github.com/folbricht/desync."
        in
        String.Hashtbl.add memo desync_path check ;
        check

let create_tmp_hardlinks ~tmpdir ~src items =
  let open Lwt_syntax in
  List.iter_s
    (fun f ->
      let src_f = Filename.concat src f in
      let dst_f = Filename.concat tmpdir f in
      Lwt.catch
        (fun () ->
          let* src_dir = Lwt_utils_unix.is_directory src_f in
          match src_dir with
          | false -> Lwt_unix.link src_f dst_f
          | true -> Lwt_utils_unix.hardlink_dir src_f dst_f)
        (function
          | Unix.Unix_error _ ->
              (* File dose not exist, skip *)
              Lwt.return_unit
          | e -> Lwt.reraise e))
    items

let pp_status cmd_name fmt = function
  | Unix.WEXITED i -> Format.fprintf fmt "%s exited with code %d" cmd_name i
  | WSIGNALED i -> Format.fprintf fmt "%s was killed by signal %d" cmd_name i
  | WSTOPPED i -> Format.fprintf fmt "%s stopped by signal %d" cmd_name i

let check_status cmd_name pc =
  let open Lwt_result_syntax in
  let*! status = pc#status in
  when_ (status <> Unix.WEXITED 0) @@ fun () ->
  failwith "%a" (pp_status cmd_name) status

let desync_tar ?(desync_path = "desync") ?(chunk_size = default_chunk_size)
    ~target_store ~index_file dir =
  let open Lwt_result_syntax in
  let* () = check_desync_available ~desync_path in
  let*! () = Lwt_utils_unix.create_dir target_store in
  Process_manager.with_process_none
    ( desync_path,
      [|
        "desync";
        "tar";
        "--concurrency";
        concurrency;
        "--chunk-size";
        chunk_size_arg chunk_size;
        "-i";
        "-s";
        target_store;
        index_file;
        dir;
      |] )
    (check_status "desync tar")

let desync_untar ?(desync_path = "desync") ~source_store ~index_file dest =
  let open Lwt_result_syntax in
  let* () = check_desync_available ~desync_path in
  let*! () = Lwt_utils_unix.create_dir dest in
  Process_manager.with_process_none
    ( desync_path,
      [|
        "desync";
        "untar";
        "--concurrency";
        concurrency;
        "--no-same-owner";
        "-s";
        source_store;
        "-i";
        index_file;
        dest;
      |] )
    (check_status "desync untar")

let info ?(desync_path = "desync") index_file =
  let open Lwt_result_syntax in
  let* () = check_desync_available ~desync_path in
  Process_manager.with_process_in (desync_path, [|"desync"; "info"; index_file|])
  @@ fun pc ->
  let*! info = Lwt_io.read pc#stdout in
  let* () = check_status "desync info" pc in
  return (Ezjsonm.value_from_string info)

(** [read_tar_header ic] reads a tar header from the input channel [ic]. It
    partially parses the header to identify the type of the entry. For normal
    files, it also extracts the name and size. This function is necessary
    because {!Tar_lwt_unix.get_next_header} does not work on the tar files
    produced by desync.
    For details on the tar header format, see the GNU tar manual:
    https://www.gnu.org/software/tar/manual/html_node/Standard.html *)
let read_tar_header ic =
  let open Lwt_result_syntax in
  (* A (non-extended) tar header is 512 bytes long. *)
  let*! header = Lwt_io.read ic ~count:512 in
  (* The type of the file is stored at offset 156. *)
  let type_ = header.[156] in
  match type_ with
  | '5' -> return `Directory
  | '0' | '\000' ->
      (* '0' or a null byte indicates a normal file. *)
      (* The filename is a null-terminated string of at most 100 bytes at the
         beginning of the header. *)
      let name_len =
        String.index_opt header '\000' |> Option.value ~default:100 |> min 100
      in
      let name = String.sub header 0 name_len in
      (* The size is a 12-bytes, null-terminated, octal string at offset 124. *)
      let size = String.sub header 124 12 in
      let*? size =
        match String.index_opt size '\000' with
        | None -> Ok size
        | Some 0 -> error_with "Invalid tar header, no size."
        | Some len -> Ok (String.sub size 0 len)
      in
      (* OCaml's [int_of_string] can parse octal if prefixed with "0o". *)
      let size = int_of_string ("0o" ^ size) in
      return (`Normal (name, size))
  | c -> return (`Other c)

(** [read_first_file ~source_store index_file] reads the first file from a
    desync archive. It starts streaming the tar archive from desync and reads
    entries until a file is found. It returns the name and content of that
    file. This is used to read the metadata from a snapshot without extracting
    the whole archive. *)
let read_first_file ?(desync_path = "desync") ~source_store index_file =
  let open Lwt_result_syntax in
  let* () = check_desync_available ~desync_path in
  Process_manager.with_process_full
    ( desync_path,
      [|
        "desync";
        "untar";
        "--no-same-owner";
        "-s";
        source_store;
        "-i";
        index_file;
        "--output-format";
        "gnu-tar";
        "-";
      |] )
  @@ fun pc ->
  let rec find () =
    let* header_info = read_tar_header pc#stdout in
    match header_info with
    | `Directory -> (* skip *) find ()
    | `Other c ->
        failwith "Could not read first file of indexed archive (type %C)." c
    | `Normal (name, size) ->
        let*! contents = Lwt_io.read pc#stdout ~count:size in
        return (name, contents)
  in
  let* res =
    protect find ~on_error:(fun error ->
        let*! err_msg = Lwt_io.read pc#stderr in
        pc#kill Sys.sigterm ;
        let*! status = pc#status in
        let top_err =
          error_of_fmt
            "Could not read metadata from snapshot.\n%a\n%s"
            (pp_status "desync untar")
            status
            (String.trim err_msg)
        in
        fail @@ TzTrace.cons top_err error)
  in
  pc#kill Sys.sigterm ;
  return res

let with_spinner ~progress promise =
  if not progress then promise
  else
    let open Lwt_syntax in
    let with_progress =
      let progress_bar = Progress_bar.spinner "Exporting desync snapshot" in
      Progress_bar.Lwt.with_reporter progress_bar
    in
    let spinning =
      with_progress @@ fun count_progress ->
      let rec loop () =
        let* () = count_progress () in
        let* () = Lwt_unix.sleep 0.25 in
        loop ()
      in
      loop ()
    in
    Lwt.pick [promise; spinning]

let export ?desync_path ?chunk_size ?metadata ?metadata_encoding
    ?(progress = true) ~target_store ~index_file ~backup_items data_dir =
  let open Lwt_result_syntax in
  let metadata_json =
    match (metadata, metadata_encoding) with
    | Some m, Some e -> Some (Data_encoding.Json.construct e m)
    | _ -> None
  in
  Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".snapshot_export_"
  @@ fun tmpdir ->
  let*! () = create_tmp_hardlinks ~tmpdir ~src:data_dir backup_items in
  let* () =
    match metadata_json with
    | None -> return_unit
    | Some json -> Lwt_utils_unix.Json.write_file (metadata_file tmpdir) json
  in
  with_spinner ~progress
  @@ desync_tar ?desync_path ?chunk_size ~target_store ~index_file tmpdir

let import ?desync_path ?metadata_encoding ~source_store ~index_file dest =
  let open Lwt_result_syntax in
  let* () = desync_untar ?desync_path ~source_store ~index_file dest in
  let metadata_f = metadata_file dest in
  let*! metadata_exists = Lwt_unix.file_exists metadata_f in
  let* metadata =
    if not metadata_exists then return_none
    else
      let* metadata =
        match metadata_encoding with
        | None -> return_none
        | Some e ->
            let+ m = Lwt_utils_unix.Json.read_file metadata_f in
            Some (Data_encoding.Json.destruct e m)
      in
      let*! () = Lwt_unix.unlink metadata_f in
      return metadata
  in
  return metadata

let read_metadata ?desync_path ~source_store ~index_file metadata_encoding =
  let open Lwt_result_syntax in
  let* filename, contents =
    read_first_file ?desync_path ~source_store index_file
  in
  let* () =
    unless (filename = metadata_filename) @@ fun () ->
    failwith
      "Malformed snapshot. First file of snapshot %s is not the metadata file"
      index_file
  in
  Ezjsonm.value_from_string contents
  |> Data_encoding.Json.destruct metadata_encoding
  |> return
