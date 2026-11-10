(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Lwt_utils_unix.remove_dir
   Invocation:   dune exec src/lib_stdlib_unix/test/main.exe -- \
                   --file test_remove_dir.ml
   Subject:      [remove_dir] removes symbolic links and does not modify
                 their targets.
*)

open Error_monad

(* Creates [dir/file] with some content. *)
let make_dir_with_file dir =
  Unix.mkdir dir 0o700 ;
  let file = Filename.concat dir "file" in
  write_file file ~contents:"content" ;
  file

let is_present path =
  match Unix.lstat path with
  | _ -> true
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let check_removed path =
  Check.is_false
    (is_present path)
    ~error_msg:(path ^ " was not removed")
    ~__LOC__

let check_kept path =
  Check.is_true (is_present path) ~error_msg:(path ^ " was removed") ~__LOC__

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"remove_dir does not follow a symbolic link in the directory"
    ~tags:["lwt_utils_unix"; "remove_dir"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_remove_dir" @@ fun root ->
  let outside = Filename.concat root "outside" in
  let outside_file = make_dir_with_file outside in
  let dir = Filename.concat root "dir" in
  let (_ : string) = make_dir_with_file dir in
  Unix.symlink outside (Filename.concat dir "link") ;
  let* () = Lwt_utils_unix.remove_dir dir in
  check_removed dir ;
  check_kept outside_file ;
  return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"remove_dir with a symbolic link given as the directory"
    ~tags:["lwt_utils_unix"; "remove_dir"]
  @@ fun () ->
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "tezos_remove_dir" @@ fun root ->
  let target = Filename.concat root "target" in
  let target_file = make_dir_with_file target in
  let link = Filename.concat root "link" in
  Unix.symlink target link ;
  let* () = Lwt_utils_unix.remove_dir link in
  check_removed link ;
  check_kept target_file ;
  let dangling = Filename.concat root "dangling" in
  Unix.symlink (Filename.concat root "missing") dangling ;
  let* () = Lwt_utils_unix.remove_dir dangling in
  check_kept dangling ;
  return_unit
