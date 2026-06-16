(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type day = int * int * int

let current_day () =
  let today =
    match Ptime.of_float_s (Unix.gettimeofday ()) with
    | Some s -> s
    | None -> Ptime.min
  in
  let date, _ = Ptime.to_date_time today in
  date

let day_of_ptime ptime =
  let date, _ = Ptime.to_date_time ptime in
  date

let string_of_day (y, m, d) = Format.sprintf "%d%02d%02d" y m d

let filename_insert_before_ext ~path s =
  let ext = Filename.extension path in
  let chopped = if ext = "" then path else Filename.chop_extension path in
  Format.asprintf "%s-%s%s" chopped s ext

let%expect_test _ =
  print_endline (filename_insert_before_ext ~path:"foo.bar" "baz") ;
  [%expect {| foo-baz.bar |}] ;
  print_endline (filename_insert_before_ext ~path:"/tmp/log.out" "11") ;
  [%expect {| /tmp/log-11.out |}] ;
  print_endline (filename_insert_before_ext ~path:"/dev/null" "XXX") ;
  [%expect {| /dev/null-XXX |}] ;
  ()

let check_file_format_with_date base_filename s =
  let name_no_ext = Filename.remove_extension base_filename in
  let ext = Filename.extension base_filename in
  let open Re.Perl in
  let re_ext = "(." ^ ext ^ ")?" in
  let re_date = "-\\d{4}\\d{2}\\d{2}" in
  let re = compile @@ re (name_no_ext ^ re_date ^ re_ext) in
  Re.execp re s

let%expect_test _ =
  print_endline (Bool.to_string (check_file_format_with_date ".out" "a.out")) ;
  [%expect {| false |}] ;
  print_endline
    (Bool.to_string
       (check_file_format_with_date "some-name.log" "some-name-19991231.log")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date "hello." "hello-19991231.")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date ".log" "19991231.log")) ;
  [%expect {| false |}] ;
  print_endline
    (Bool.to_string
       (check_file_format_with_date ".log.log" ".log-19991231.log")) ;
  [%expect {| true |}] ;
  print_endline
    (Bool.to_string (check_file_format_with_date "file" "file-19991231")) ;
  [%expect {| true |}] ;
  ()

let list_rotation_files base_path =
  let dirname = Filename.dirname base_path in
  let base_filename = Filename.basename base_path in
  try
    let files = Sys.readdir dirname in
    Array.to_list files
    |> List.filter (check_file_format_with_date base_filename)
  with Sys_error _ -> []

let remove_older_files base_path ~days_kept =
  let dirname = Filename.dirname base_path in
  let files = list_rotation_files base_path in
  let sorted = List.sort (fun x y -> -compare x y) files in
  List.iteri
    (fun i file ->
      if i >= days_kept then
        try Sys.remove (Filename.concat dirname file) with Sys_error _ -> ())
    sorted

let list_rotation_files_lwt base_path =
  let open Lwt_syntax in
  let dirname = Filename.dirname base_path in
  let base_filename = Filename.basename base_path in
  let file_stream = Lwt_unix.files_of_directory dirname in
  let rec explore acc =
    let* filename = Lwt_stream.get file_stream in
    match filename with
    | None -> Lwt.return acc
    | Some filename ->
        if check_file_format_with_date base_filename filename then
          explore (filename :: acc)
        else explore acc
  in
  explore []

let remove_older_files_lwt base_path ~days_kept =
  let open Lwt_syntax in
  let dirname = Filename.dirname base_path in
  let* files = list_rotation_files_lwt base_path in
  let sorted = List.sort (fun x y -> -compare x y) files in
  List.iteri_s
    (fun i file ->
      if i >= days_kept then Lwt_unix.unlink (Filename.concat dirname file)
      else Lwt.return_unit)
    sorted
