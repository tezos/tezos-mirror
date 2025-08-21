(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let warn x = Printf.ksprintf (echo "Warning: %s") x

(* Implements [rm -rf]. *)
(* Copied and adapted from Tezt. *)
let rec remove_recursively filename =
  match (Unix.lstat filename).st_kind with
  | exception Unix.Unix_error (ENOENT, _, _) ->
      (* Already does not exist. *)
      ()
  | exception Unix.Unix_error (error, _, _) ->
      warn
        "failed to read file type for %s: %s"
        filename
        (Unix.error_message error)
  | S_REG | S_LNK | S_FIFO | S_SOCK -> (
      (* It is particularly important to not recursively delete symbolic links
         to directories but to remove the links instead. *)
      try Sys.remove filename
      with Sys_error error -> warn "failed to remove %s: %s" filename error)
  | S_DIR -> (
      match Sys.readdir filename with
      | exception Sys_error error -> warn "failed to read %s: %s" filename error
      | contents -> (
          let contents = Array.map (Filename.concat filename) contents in
          Array.iter remove_recursively contents ;
          try Unix.rmdir filename
          with Unix.Unix_error (error, _, _) ->
            warn
              "failed to remove directory %s: %s"
              filename
              (Unix.error_message error)))
  | S_CHR -> warn "will not remove character device: %s" filename
  | S_BLK -> warn "will not remove block device: %s" filename

type checkout_result = {
  tmp_worktree : string;
  tmp_path : string;
  cleanup : unit -> unit;
}

let checkout_into_tmp ~git_reference ~path ?(other_paths = []) () =
  (* Find a free temporary directory name. *)
  let* tmp_worktree =
    let tmp = Filename.get_temp_dir_name () in
    let rec attempt n =
      if n >= 1000 then
        fail "failed to find an unused temporary directory name in %s" tmp
      else
        let candidate = tmp // ("tobi" ^ string_of_int n) in
        match Unix.mkdir candidate 0o700 with
        | exception Unix.Unix_error (EEXIST, _, _) -> attempt (n + 1)
        | () -> Ok candidate
    in
    attempt 0
  in
  (* Cleanup function to remove temporary directories.
     To be called if something goes wrong. *)
  let cleanup () = remove_recursively tmp_worktree in
  (* Run [git restore]. *)
  match
    Run.command
      "git"
      ("--work-tree" :: tmp_worktree :: "restore" :: "--source" :: git_reference
     :: "--" :: path :: other_paths)
  with
  | Ok () ->
      (* Check that the requested paths were checked out correctly. *)
      let* () =
        list_iter_r (path :: other_paths) @@ fun path ->
        if Sys.file_exists (tmp_worktree // path) then unit
        else (
          cleanup () ;
          fail "%S does not appear to exist in %S" path git_reference)
      in
      (* All good; keep the temporary files and return. *)
      Ok {tmp_worktree; tmp_path = tmp_worktree // path; cleanup}
  | Error _ as x ->
      cleanup () ;
      x

let with_checkout_into_tmp ~git_reference ~path ?other_paths
    ?(keep_temp = false) f =
  let* checkout_result =
    checkout_into_tmp ~git_reference ~path ?other_paths ()
  in
  Fun.protect ~finally:(fun () ->
      if not keep_temp then checkout_result.cleanup ())
  @@ fun () -> f checkout_result

let is_a_commit_hash str =
  String.length str = 40
  && String.for_all
       (function 'a' .. 'f' | '0' .. '9' -> true | _ -> false)
       str

let rev_parse =
  (* Memoization makes it easy to query Git only once per reference.
     It also makes sure that the meaning of references stays consistent.
     For instance, even if someone commits to a branch, [Git.rev_parse] will
     always return the same commit hash for this branch.
     Although this is a bit limited because if one asks, say, [HEAD] and then [HEAD^],
     there is no guarantee that the result of [Git.rev_parse] for [HEAD^]
     is actually the parent of the result of [Git.rev_parse] for [HEAD].
     Just don't call Git while Tobi is running and you'll be fine. *)
  memoize @@ fun git_reference ->
  let* lines = Run.command_lines "git" ["rev-parse"; git_reference] in
  match List.filter (fun line -> String.trim line <> "") lines with
  | [] -> fail "'git rev-parse %s' returned nothing" git_reference
  | _ :: _ :: _ ->
      fail "'git rev-parse %s' returned more than one value" git_reference
  | [line] ->
      if not (is_a_commit_hash line) then
        fail
          "'git rev-parse %s' returned something that does not look like a \
           commit hash: %S"
          git_reference
          line
      else Ok line
