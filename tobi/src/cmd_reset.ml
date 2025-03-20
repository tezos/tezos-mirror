(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let run ~verbose ~dry_run =
  (* We'll look for links which start with [tobi_prefix].
     This assumes that the links were created with absolute targets by [Cmd_install]. *)
  let tobi_prefix = Sys.getcwd () // "_tobi/" in
  (* Recursively browse [_opam] to find links to [tobi_prefix] and remove those links. *)
  let rec browse path =
    (* It is important to use [lstat] and not just [stat].
       Otherwise links would not have kind [S_LNK]. *)
    match Unix.lstat path with
    | exception Unix.Unix_error (ENOENT, _, _) ->
        (* Most likely case this could happen is if [_opam] does not exist.
           In any case, we do not care about traversing paths that do not exist. *)
        unit
    | exception Unix.Unix_error (code, _, _) ->
        fail "failed to browse %s" path ~reason:[Unix.error_message code]
    | stat -> (
        match stat.st_kind with
        | S_DIR -> (
            (* List the contents of the directory to recursively traverse it. *)
            match Unix.opendir path with
            | exception Unix.Unix_error (ENOENT, _, _) ->
                (* Maybe the directory was deleted by another process.
                   We don't care about it anymore. *)
                unit
            | exception Unix.Unix_error (code, _, _) ->
                fail
                  "failed to open directory: %s"
                  path
                  ~reason:[Unix.error_message code]
            | dir ->
                Fun.protect ~finally:(fun () -> closedir dir) @@ fun () ->
                let rec browse_dir () =
                  match Unix.readdir dir with
                  | exception End_of_file -> unit
                  | exception Unix.Unix_error (code, _, _) ->
                      fail
                        "failed to read directory: %s"
                        path
                        ~reason:[Unix.error_message code]
                  | filename ->
                      (* Traversing [.] and [..] would be a bad idea. *)
                      if
                        filename = Filename.current_dir_name
                        || filename = Filename.parent_dir_name
                      then browse_dir ()
                      else
                        let* () = browse (path // filename) in
                        browse_dir ()
                in
                browse_dir ())
        | S_LNK -> (
            (* Found a link. Check its target. *)
            match Unix.readlink path with
            | exception Unix.Unix_error (ENOENT, _, _) ->
                (* Maybe the link was deleted by another process.
                   We don't care about it anymore. *)
                unit
            | exception Unix.Unix_error (EINVAL, _, _) ->
                (* Not a symbolic link after all.
                   The link was maybe overritten with another file by another process. *)
                unit
            | exception Unix.Unix_error (code, _, _) ->
                fail
                  "failed to read link: %s"
                  path
                  ~reason:[Unix.error_message code]
            | link_target_path ->
                if not (String.starts_with link_target_path ~prefix:tobi_prefix)
                then unit
                else (
                  (* Found a link that leads inside _tobi: remove it. *)
                  if verbose then echo "rm %s" path ;
                  if dry_run then unit
                  else
                    match Unix.unlink path with
                    | exception Unix.Unix_error (ENOENT, _, _) ->
                        (* Maybe the link was deleted by another process.
                           We don't care about it anymore. *)
                        unit
                    | exception Unix.Unix_error (code, _, _) ->
                        fail
                          "failed to remove: %s"
                          path
                          ~reason:[Unix.error_message code]
                    | () -> unit))
        | S_REG | S_CHR | S_BLK | S_FIFO | S_SOCK ->
            (* We don't care about regular files, character devices, block devices,
               named pipes and sockets. They are not links and they cannot contain links.
               Well I guess devices could, but not in the way we care about. *)
            unit)
  in
  browse "_opam"
