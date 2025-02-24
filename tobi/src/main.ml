(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(* Find the first parent directory which contains [.git]. *)
let find_project_root () =
  let rec find from =
    if try Sys.file_exists (from // ".git") with Sys_error _ -> false then
      Ok from
    else
      let parent = Filename.dirname from in
      if String.length parent < String.length from then find parent
      else
        fail
          "failed to find project root: no .git in current directory or its \
           parents"
  in
  find (Sys.getcwd ())

(* Command-line options. *)
module CLI = struct
  (* Custom argument types. *)
  module Type = struct
    let version =
      Clap.typ
        ~name:"version"
        ~dummy:Version.Dev
        ~parse:(fun s -> Some (Version.parse s))
        ~show:Version.show
  end

  let verbose =
    Clap.flag
      ~set_long:"verbose"
      ~set_short:'v'
      ~description:"Be more talkative."
      false

  (* Subcommands. *)
  module Command = struct
    let list =
      Clap.case "list" ~description:"List components." @@ fun () ->
      let version =
        Clap.default
          Type.version
          ~long:"version"
          ~placeholder:"VERSION"
          ~description:
            "List components that are available in VERSION, which can be 'dev' \
             or a Git commit hash, branch name or tag. 'dev' denotes the \
             version in the working directory."
          Dev
      in
      `list version
  end

  let command = Clap.subcommand Command.[list]

  let () = Clap.close ()
end

let main () =
  (* Set directory to the project's root.
     This can fail so this should only be performed by commands that require it,
     but for now all commands require it. *)
  let* project_root = find_project_root () in
  Sys.chdir project_root ;

  (* Dispatch commands. *)
  match CLI.command with
  | `list version -> Cmd_list.run ~verbose:CLI.verbose version

(* Entrypoint: call [main] and handle errors. *)
let () =
  match main () with
  | Ok () -> ()
  | Error {code = `failed; message} -> (
      match message with
      | [] -> Printf.eprintf "Error (no error message)\n"
      | head :: tail ->
          Printf.eprintf "Error: %s\n" head ;
          List.iter (Printf.eprintf "=> %s\n") tail ;
          exit 1)
