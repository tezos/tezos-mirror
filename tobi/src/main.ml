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

    let component_and_version =
      let parse str =
        match str_split_once str '.' with
        | None -> Some (str, Version.head)
        | Some (name, version) -> Some (name, Version.parse version)
      in
      let show (name, version) = name ^ "." ^ Version.show version in
      Clap.typ ~name:"component.version" ~dummy:("", Version.head) ~parse ~show
  end

  let verbose =
    Clap.flag
      ~set_long:"verbose"
      ~set_short:'v'
      ~description:"Be more talkative."
      false

  let dry_run =
    Clap.flag
      ~set_long:"dry-run"
      ~description:"Do not write any file; only print what would be done."
      false

  let jobs =
    Clap.optional_int
      ~long:"jobs"
      ~short:'j'
      ~description:
        "Maximum number of processes to spawn when stuff can be done in \
         parallel."
      ()

  (* Subcommands. *)
  module Command = struct
    let list =
      Clap.case "list" ~description:"List components." @@ fun () ->
      let installed =
        Clap.flag
          ~set_long:"installed"
          ~set_short:'i'
          ~description:"Only list installed components."
          false
      in
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
      `list (version, `installed installed)

    let install =
      Clap.case "install" ~description:"Install some component(s)." @@ fun () ->
      let keep_temp =
        Clap.flag
          ~set_long:"keep-temp"
          ~description:"Do not remove temporary directories."
          false
      in
      let components =
        Clap.list
          Type.component_and_version
          ~placeholder:"COMPONENT"
          ~description:
            "Name of a component to install. COMPONENT can be of the form NAME \
             or NAME.VERSION, where VERSION is a Git commit hash, branch name \
             or tag. It cannot be 'dev'. If VERSION is omitted, it defaults to \
             HEAD."
          ()
      in
      `install (components, `keep_temp keep_temp)

    let reset =
      Clap.case "reset" ~description:"Uninstall all installed components."
      @@ fun () -> `reset

    let build =
      Clap.case "build" ~description:"Build some component(s)." @@ fun () ->
      let components =
        Clap.list_string
          ~placeholder:"COMPONENT"
          ~description:"Name of a component to build."
          ()
      in
      `build components
  end

  let command = Clap.subcommand Command.[list; install; reset; build]

  let () = Clap.close ()
end

let main () =
  (* Set directory to the project's root.
     This can fail so this should only be performed by commands that require it,
     but for now all commands require it. *)
  let* project_root = find_project_root () in
  Sys.chdir project_root ;

  (* Only read [/proc/cpuinfo] if needed, as it can emit a warning. *)
  let jobs () =
    match CLI.jobs with Some jobs -> jobs | None -> Cpuinfo.get_cpu_count ()
  in

  (* Dispatch commands. *)
  match CLI.command with
  | `list (version, `installed installed) ->
      Cmd_list.run ~verbose:CLI.verbose ~installed version
  | `install (components, `keep_temp keep_temp) ->
      Cmd_install.run
        ~verbose:CLI.verbose
        ~dry_run:CLI.dry_run
        ~jobs:(jobs ())
        ~keep_temp
        components
  | `reset -> Cmd_reset.run ~verbose:CLI.verbose ~dry_run:CLI.dry_run
  | `build components ->
      Cmd_build.run
        ~verbose:CLI.verbose
        ~dry_run:CLI.dry_run
        ~jobs:(jobs ())
        components

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
