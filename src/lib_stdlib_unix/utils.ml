(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018-2024 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

let hide_progress_line s =
  let len = String.length s in
  if len > 0 then Printf.eprintf "\r%*s\r" len ""

let display_progress ?(refresh_rate = (1, 1)) msgf =
  if Unix.isatty Unix.stderr then
    let index, rate = refresh_rate in
    if index mod rate == 0 then
      msgf
        (Format.kasprintf (fun msg ->
             hide_progress_line msg ;
             Format.eprintf "%s%!" msg))

let display_progress_end () =
  if Unix.isatty Unix.stderr then Format.eprintf "@."

let fold_files dir f acc =
  let rec list_files_in_dir acc
      ((dir, relative_dir, dir_handle) as current_dir_info) =
    match Unix.readdir dir_handle with
    | "." | ".." -> list_files_in_dir acc current_dir_info
    | basename ->
        let full_path = Filename.concat dir basename in
        let relative_path = Filename.concat relative_dir basename in
        let acc =
          if Sys.is_directory full_path then
            let sub_dir_handle = Unix.opendir full_path in
            list_files_in_dir acc (full_path, relative_path, sub_dir_handle)
          else f relative_path acc
        in
        list_files_in_dir acc current_dir_info
    | exception End_of_file ->
        Unix.closedir dir_handle ;
        acc
  in
  let dir_handle = Unix.opendir dir in
  list_files_in_dir acc (dir, "", dir_handle)

let list_files dir = fold_files dir List.cons [] |> List.rev

let directory_contents_size dir =
  fold_files
    dir
    (fun relative_path total ->
      let {Unix.st_size; _} = Unix.lstat (Filename.concat dir relative_path) in
      total + st_size)
    0

let rec create_dir ?(perm = 0o755) dir =
  let stat =
    try Some (Unix.stat dir) with Unix.Unix_error (ENOENT, _, _) -> None
  in
  match stat with
  | Some {st_kind = S_DIR; _} -> ()
  | Some _ -> Stdlib.failwith "Not a directory"
  | None -> (
      create_dir ~perm (Filename.dirname dir) ;
      try Unix.mkdir dir perm
      with Unix.Unix_error (EEXIST, _, _) ->
        (* This is the case where the directory has been created at the same
           time. *)
        ())

let copy_file ~count_progress ~src ~dst =
  let in_chan = open_in src in
  let out_chan = open_out dst in
  try
    let buffer_size = 64 * 1024 in
    let buf = Bytes.create buffer_size in
    let rec copy () =
      let read_bytes = input in_chan buf 0 buffer_size in
      output out_chan buf 0 read_bytes ;
      count_progress read_bytes ;
      if read_bytes > 0 then copy ()
    in
    copy () ;
    flush out_chan ;
    close_in in_chan ;
    close_out out_chan
  with e ->
    close_in in_chan ;
    close_out out_chan ;
    raise e

let copy_dir ?(perm = 0o755) ?progress src dst =
  create_dir ~perm dst ;
  let maybe_report_progress =
    match progress with
    | None -> fun f -> f (fun _ -> ())
    | Some (message, color) ->
        let total = directory_contents_size src in
        let progress_bar =
          Progress_bar.progress_bar ~counter:`Bytes ~message ~color total
        in
        fun f -> Progress_bar.with_reporter progress_bar f
  in
  maybe_report_progress @@ fun count_progress ->
  fold_files
    src
    (fun relative_path () ->
      let src = Filename.concat src relative_path in
      let dst = Filename.concat dst relative_path in
      let dst_dir = Filename.dirname dst in
      create_dir ~perm dst_dir ;
      copy_file ~count_progress ~src ~dst)
    ()

let copy_file = copy_file ~count_progress:(fun _ -> ())

let hardlink_dir ?(perm = 0o755) src dst =
  create_dir ~perm dst ;
  fold_files
    src
    (fun relative_path () ->
      let src = Filename.concat src relative_path in
      let dst = Filename.concat dst relative_path in
      let dst_dir = Filename.dirname dst in
      create_dir ~perm dst_dir ;
      Unix.link src dst)
    ()

let rec retry ?max_delay ~delay ~factor ?tries ~is_error ~emit
    ?(msg = fun _ -> "") f x =
  let open Lwt.Syntax in
  let* result = f x in
  let should_retry = match tries with None -> true | Some i -> i > 0 in
  match result with
  | Ok _ as r -> Lwt.return r
  | Error (err :: _ as errs) when should_retry && is_error err -> (
      let* () =
        emit
          (Format.sprintf
             "%sRetrying in %.2f seconds%s..."
             (msg errs)
             delay
             (match tries with
             | None -> ""
             | Some i -> Format.sprintf ", %d attempts left" i))
      in
      let* result =
        Lwt.pick
          [
            (let* () = Lwt_unix.sleep delay in
             Lwt.return `Continue);
            (let* _ = Lwt_exit.clean_up_starts in
             Lwt.return `Killed);
          ]
      in
      match result with
      | `Killed -> Lwt.return_error errs
      | `Continue ->
          let next_delay = delay *. factor in
          let delay =
            Option.fold
              ~none:next_delay
              ~some:(fun max_delay -> Float.min next_delay max_delay)
              max_delay
          in
          retry
            ?max_delay
            ~delay
            ~factor
            ~msg
            ?tries:(Option.map pred tries)
            ~is_error
            ~emit
            f
            x)
  | Error errs as err ->
      let* () = emit (Format.sprintf "%sNo attempts left." (msg errs)) in
      Lwt.return err

let event_on_stalling_promise ?max_delay ?(factor = 1.2) ?(initial_delay = 2.)
    ~event f =
  let open Lwt.Syntax in
  let rec timeout_warn sum ~delay =
    let next_delay = delay *. factor in
    let delay =
      Option.fold ~none:next_delay ~some:(Float.min next_delay) max_delay
    in
    let* () = Lwt_unix.sleep delay in
    let sum = sum +. delay in
    let* () = event sum in
    timeout_warn sum ~delay
  in
  (* [timeout_warn] will never stop but will emit an event after a delay each time *)
  Lwt.pick [f; timeout_warn 0. ~delay:initial_delay]
