(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Base

(* [parents] contains the list of parent directories that were created.
   [cleanup] will delete them only if they are empty.
   If [a/b] and [a/b/c] needed to be created, then [a/b] is after [a/b/c] in this list. *)
let parents = ref []

let dirs = ref []

let files = ref []

let next_name = ref 0

let base_main_dir = "tezt-" ^ string_of_int (Unix.getpid ())

let fresh_main_dir () =
  let index = !next_name in
  incr next_name ;
  Filename.get_temp_dir_name () // base_main_dir // string_of_int index

let main_dir_ref = ref @@ fresh_main_dir ()

let main_dir () = !main_dir_ref

let file_aux ?(perms = 0o755) base_name =
  let filename = main_dir () // base_name in
  let rec create_parent filename =
    let parent = Filename.dirname filename in
    if String.length parent < String.length filename then (
      create_parent parent ;
      if not (Sys.file_exists parent) then (
        Unix.mkdir parent perms ;
        parents := parent :: !parents ) )
  in
  create_parent filename ; filename

let allowed = ref false

let check_allowed fname arg =
  if not !allowed then (
    Printf.eprintf
      "Error: Temp.%s %S: not allowed outside of Test.run\n%!"
      fname
      arg ;
    exit 1 )

let file ?perms base_name =
  check_allowed "file" base_name ;
  let filename = file_aux ?perms base_name in
  files := filename :: !files ;
  filename

let dir ?(perms = 0o755) base_name =
  check_allowed "dir" base_name ;
  let filename = file_aux ~perms base_name in
  if not (Sys.file_exists filename) then (
    Unix.mkdir filename perms ;
    dirs := filename :: !dirs ) ;
  filename

let rec remove_recursively filename =
  match (Unix.stat filename).st_kind with
  | exception Unix.Unix_error (error, _, _) ->
      Log.warn
        "Failed to read file type for %s: %s"
        filename
        (Unix.error_message error)
  | S_REG | S_LNK | S_FIFO | S_SOCK -> (
    (* It is particularly important to not recursively delete symbolic links
       to directories but to remove the links instead. *)
    try Sys.remove filename
    with Sys_error error -> Log.warn "Failed to remove %s: %s" filename error )
  | S_DIR -> (
    match Sys.readdir filename with
    | exception Sys_error error ->
        Log.warn "Failed to read %s: %s" filename error
    | contents -> (
        let contents = Array.map (Filename.concat filename) contents in
        Array.iter remove_recursively contents ;
        try Unix.rmdir filename
        with Unix.Unix_error (error, _, _) ->
          Log.warn
            "Failed to remove directory %s: %s"
            filename
            (Unix.error_message error) ) )
  | S_CHR ->
      Log.warn "Will not remove character device: %s" filename
  | S_BLK ->
      Log.warn "Will not remove block device: %s" filename

let start () =
  if !allowed then invalid_arg "Temp.start: a test is already running" ;
  main_dir_ref := fresh_main_dir () ;
  allowed := true ;
  !main_dir_ref

let stop () = allowed := false

let clean_up () =
  stop () ;
  List.iter
    (fun filename -> if Sys.file_exists filename then Sys.remove filename)
    !files ;
  files := [] ;
  List.iter
    (fun dirname ->
      if Sys.file_exists dirname && Sys.is_directory dirname then
        remove_recursively dirname)
    !dirs ;
  dirs := [] ;
  List.iter
    (fun dirname ->
      match Sys.readdir dirname with
      | [||] ->
          Unix.rmdir dirname
      | _ ->
          Log.warn "Directory is not empty: %s" dirname)
    !parents ;
  parents := []

let check () =
  let tmp_dir = Filename.get_temp_dir_name () in
  match Sys.readdir tmp_dir with
  | exception Sys_error _ ->
      ()
  | contents ->
      let tezt_rex = rex "^tezt-[0-9]+$" in
      let check_file filename =
        if filename =~ tezt_rex then
          Log.warn
            "Leftover temporary file from previous run: %s"
            (tmp_dir // filename)
      in
      Array.iter check_file contents

let () = check ()
