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

(* Private type to represent a file system. It's used by [clean_up].

   [parents] contains the list of parent directories that were created.
   [cleanup] will delete them only if they are empty.
   If [a/b] and [a/b/c] needed to be created, then [a/b] is after [a/b/c] in
   this list, i.e. descendants come before parents. *)
type file_system = {
  parents : string list;
  dirs : string list;
  files : string list;
}

(* Associate one runner with its remote file system. *)
module Runner_map = Map.Make (struct
  type t = Runner.t option

  let compare = Option.compare compare
end)

let filesystems = ref Runner_map.empty

let get_fs ?runner () =
  let fs = Runner_map.find_opt runner !filesystems in
  match fs with None -> {parents = []; dirs = []; files = []} | Some fs -> fs

let next_name = ref 0

let base_main_dir () = "tezt-" ^ string_of_int (Unix.getpid ())

(* [add_file], [add_dir] and [add_parent] select the file system to use.*)
let add_file ?runner file =
  let fs = get_fs ?runner () in
  let old_files = fs.files in
  filesystems :=
    Runner_map.add runner {fs with files = file :: old_files} !filesystems

let add_dir ?runner dir =
  let fs = get_fs ?runner () in
  let old_dirs = fs.dirs in
  filesystems :=
    Runner_map.add runner {fs with dirs = dir :: old_dirs} !filesystems

let add_parent ?runner parent =
  let fs = get_fs ?runner () in
  let old_parents = fs.parents in
  filesystems :=
    Runner_map.add runner {fs with parents = parent :: old_parents} !filesystems

let fresh_main_dir () =
  let index = !next_name in
  incr next_name ;
  Filename.get_temp_dir_name () // base_main_dir () // string_of_int index

let main_dir_ref = ref @@ fresh_main_dir ()

let main_dir () = !main_dir_ref

let file_aux ?runner ?(perms = 0o755) base_name =
  let filename = main_dir () // base_name in
  let rec create_parent filename =
    let parent = Filename.dirname filename in
    if String.length parent < String.length filename then (
      create_parent parent ;
      if not (Runner.Sys.file_exists ?runner parent) then (
        Runner.Sys.mkdir ?runner ~perms parent ;
        add_parent ?runner parent))
  in
  create_parent filename ;
  filename

let allowed = ref false

let check_allowed fname arg =
  if not !allowed then (
    Printf.eprintf
      "Error: Temp.%s %S: not allowed outside of Test.run\n%!"
      fname
      arg ;
    exit 1)

let file ?runner ?perms base_name =
  check_allowed "file" base_name ;
  let filename = file_aux ?runner ?perms base_name in
  add_file ?runner filename ;
  filename

let dir ?runner ?(perms = 0o755) base_name =
  check_allowed "dir" base_name ;
  let filename = file_aux ?runner ~perms base_name in
  if not (Runner.Sys.file_exists ?runner filename) then (
    Runner.Sys.mkdir ?runner ~perms filename ;
    add_dir ?runner filename) ;
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
      with Sys_error error ->
        Log.warn "Failed to remove %s: %s" filename error)
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
              (Unix.error_message error)))
  | S_CHR -> Log.warn "Will not remove character device: %s" filename
  | S_BLK -> Log.warn "Will not remove block device: %s" filename

let start () =
  if !allowed then invalid_arg "Temp.start: a test is already running" ;
  main_dir_ref := fresh_main_dir () ;
  allowed := true ;
  !main_dir_ref

let stop () = allowed := false

let clean_up_aux (runner : Runner.t option) runner_fs =
  List.iter
    (fun filename ->
      if Runner.Sys.file_exists ?runner filename then
        Runner.Sys.remove ?runner filename)
    runner_fs.files ;
  List.iter
    (fun dirname ->
      if
        Runner.Sys.file_exists ?runner dirname
        && Runner.Sys.is_directory ?runner dirname
      then
        match runner with
        | None -> remove_recursively dirname
        | Some runner -> Runner.Sys.rm_rf runner dirname)
    runner_fs.dirs ;
  List.iter
    (fun dirname ->
      match Runner.Sys.readdir ?runner dirname with
      | [||] -> Runner.Sys.rmdir ?runner dirname
      | _ ->
          let dirtype =
            if Option.is_some runner then "Remote directory" else "Directory"
          in
          Log.warn "%s directory is not empty %s" dirtype dirname)
    runner_fs.parents

let clean_up () =
  stop () ;
  let filesystems_to_clean = !filesystems in
  filesystems := Runner_map.empty ;
  Runner_map.iter clean_up_aux filesystems_to_clean

let check () =
  let tmp_dir = Filename.get_temp_dir_name () in
  match Sys.readdir tmp_dir with
  | exception Sys_error _ -> ()
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

(* FIXME: Add a remote check to verify the existence of a tezt
   repository on the remote machine. *)
