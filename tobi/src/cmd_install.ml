(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(* Implementation of [mkdir -p]. *)
let rec create_parent_directory path ~permissions =
  let parent = Filename.dirname path in
  let* () =
    if String.length parent >= String.length path then unit
    else create_parent_directory parent ~permissions
  in
  try
    Unix.mkdir parent permissions ;
    unit
  with
  | Unix.Unix_error (EEXIST, _, _) -> unit
  | Unix.Unix_error (code, _, _) ->
      fail
        "failed to create directory: %s"
        path
        ~reason:[Unix.error_message code]

(* Implementation of [cp] for a single file.
   Also creates parent directories if needed. *)
let copy_file ~source_path ~target_path ~new_permissions =
  let* () = create_parent_directory target_path ~permissions:0o755 in
  wrap_errors (sf "failed to copy %s into %s" source_path target_path)
  @@
  (* Open the source file for reading. *)
  match Unix.openfile source_path [O_RDONLY; O_CLOEXEC] new_permissions with
  | exception Unix.Unix_error (code, _, _) ->
      fail "failed to open %s: %s" source_path (Unix.error_message code)
  | source_file -> (
      Fun.protect ~finally:(fun () -> close source_file) @@ fun () ->
      (* Open the target file for writing, creating it if necessary. *)
      match
        Unix.openfile target_path [O_WRONLY; O_CREAT; O_CLOEXEC] new_permissions
      with
      | exception Unix.Unix_error (code, _, _) ->
          fail "failed to open %s: %s" target_path (Unix.error_message code)
      | target_file ->
          Fun.protect ~finally:(fun () -> close target_file) @@ fun () ->
          (* Prepare a 4kB buffer.
             Copying in chunks of 4kB should be rather fast with common file systems. *)
          let bytes = Bytes.create 4096 in
          (* Loop until end of file. *)
          let rec copy () =
            match Unix.read source_file bytes 0 (Bytes.length bytes) with
            | exception Unix.Unix_error (code, _, _) ->
                fail
                  "failed to read from %s: %s"
                  source_path
                  (Unix.error_message code)
            | len -> (
                if len <= 0 then (* End of file. *)
                  unit
                else
                  match Unix.write target_file bytes 0 len with
                  | exception Unix.Unix_error (code, _, _) ->
                      fail
                        "failed to write into %s: %s"
                        target_path
                        (Unix.error_message code)
                  | (_ : int) -> copy ())
          in
          copy ())

(* Checkout a component into a temporary directory, call [continue],
   then clean up temporary files. *)
let checkout_component_into_tmp ~keep_temp (component : Component.t)
    (continue : Git.checkout_result -> (_, [> `failed]) r) =
  match component.version with
  | Dev ->
      (* Technically we could allow this, but we couldn't store installed files
         in the cache because there is no commit hash. *)
      fail "dev versions cannot be installed"
  | Old git_reference ->
      (* Call Git to checkout the relevant paths. *)
      let* config = Config.load component.version in
      let path, other_paths = component.paths in
      Git.with_checkout_into_tmp
        ~git_reference
        ~path
        ~other_paths:(Config.pervasive_paths config @ other_paths)
        ~keep_temp
      @@ fun checkout ->
      (* Make a symbolic link to _opam so that external libraries are available. *)
      let* () =
        let target = Sys.getcwd () // "_opam" in
        let link = checkout.tmp_worktree // "_opam" in
        match Unix.symlink target link ~to_dir:true with
        | exception Unix.Unix_error (code, _, _) ->
            fail
              "failed to create symbolic link to %s in %s"
              target
              link
              ~reason:[Unix.error_message code]
        | () -> unit
      in
      continue checkout

(* Evaluate a term from Opam build instructions. *)
let eval_command_item ~jobs (component : Component.t) (item : Opam.command_item)
    =
  match item with
  | Const s -> Ok s
  | Var "name" -> Ok component.name
  | Var "jobs" -> Ok (string_of_int jobs)
  | Var var ->
      fail
        "unknown variable in build instructions of %s.%s: %s"
        component.name
        (Version.show component.version)
        var

(* Run Opam build instructions in a given working directory. *)
let run_build_instructions ~verbose ~jobs (component : Component.t)
    ~working_directory =
  list_iter_r component.build @@ fun {command; arguments; condition} ->
  match condition with
  | With_test -> unit
  | True ->
      (* Evaluate terms. *)
      let* command = eval_command_item ~jobs component command in
      let* arguments =
        list_map_r arguments (eval_command_item ~jobs component)
      in
      (* Replace [rm -r] with [rm -rf], because we may not have checked out the files
         and directories that need to be deleted. *)
      let arguments =
        match (command, arguments) with
        | "rm", "-r" :: other_arguments -> "-rf" :: other_arguments
        | _ -> arguments
      in
      (* Run the command. *)
      if verbose then echo "%s" (quote_command command arguments) ;
      Run.command command arguments ~working_directory

let build_component_into_cache ~verbose ~dry_run ~jobs ~keep_temp
    (component : Component.t) =
  if dry_run then unit
  else
    (* Checkout the component into a temporary directory. *)
    checkout_component_into_tmp ~keep_temp component @@ fun checkout ->
    (* Build the component in this temporary directory. *)
    let* () =
      run_build_instructions
        ~verbose
        ~jobs
        component
        ~working_directory:checkout.tmp_worktree
    in
    (* Parse the resulting [.install] file. *)
    let* files =
      Opam.Install.parse_file
        ~package_name:component.name
        ~filename:(checkout.tmp_worktree // (component.name ^ ".install"))
    in
    (* Copy the [.install] file into the cache. *)
    let* relative_cache_dir = Component.relative_cache_dir component in
    (* TODO: name the directory with .part, and then rename it once all files are copied,
       to avoid having a broken cache. *)
    let dot_install_base_name = component.name ^ ".install" in
    let* () =
      copy_file
        ~source_path:(checkout.tmp_worktree // dot_install_base_name)
        ~target_path:(relative_cache_dir // dot_install_base_name)
        ~new_permissions:0o644
    in
    (* Copy all files listed in the [.install] file into the cache. *)
    list_iter_r files @@ fun {source_path; permissions; _} ->
    let target_path = relative_cache_dir // source_path in
    copy_file
      ~source_path:(checkout.tmp_worktree // source_path)
      ~target_path
      ~new_permissions:permissions

(* TODO: if a file failed to be installed, uninstall other files
   (and delete directories that become empty). *)
let install_component_from_cache ~verbose ~dry_run (component : Component.t) =
  if dry_run then unit
  else
    let* relative_cache_dir = Component.relative_cache_dir component in
    let* files =
      Opam.Install.parse_file
        ~package_name:component.name
        ~filename:(relative_cache_dir // (component.name ^ ".install"))
    in
    list_iter_r files
    @@ fun {directory; source_path; target_path_relative_to_prefix; _} ->
    let target_path = "_opam" // target_path_relative_to_prefix in
    match directory with
    | Man ->
        if verbose then echo "%s: ignored (man)" target_path ;
        unit
    | Misc ->
        if verbose then echo "%s: ignored (misc)" target_path ;
        unit
    | Lib | Lib_root | Libexec | Libexec_root | Bin | Sbin | Toplevel | Share
    | Share_root | Etc | Doc | Stublibs ->
        (* Ideally we would ignore [Doc] files.
           But some .mld files can be installed there.
           And those .mld files may be needed to build other stuff.
           This is the case for src/lib_protocol_compiler/dune for instance. *)
        if verbose then echo "-> %s" target_path ;
        if dry_run then unit
        else
          let* () = create_parent_directory target_path ~permissions:0o755 in
          (* Delete symbolic link if it exists so that we can replace it.
             Note that Sys.file_exists can return "false" if the link exists
             but targets something that doesn't.
             TODO: no longer needed if we correctly uninstall first. *)
          (match Unix.lstat target_path with
          | exception Unix.Unix_error _ -> ()
          | _ -> Unix.unlink target_path) ;
          Unix.symlink
            (Sys.getcwd () // (relative_cache_dir // source_path))
            target_path ;
          unit

(* TODO: uninstall component if already installed *)
let install_component ~verbose ~dry_run ~jobs ~keep_temp
    {Solver.component; reasons} =
  if verbose then
    echo
      "Installing: %s.%s (%s)"
      component.name
      (Version.show component.version)
      (Solver.show_reasons reasons)
  else echo "Installing: %s" component.name ;
  let* available_in_cache = Component.available_in_cache component in
  let* () =
    if available_in_cache then (
      echo "Found in cache." ;
      unit)
    else build_component_into_cache ~verbose ~dry_run ~jobs ~keep_temp component
  in
  install_component_from_cache ~verbose ~dry_run component

let run ~verbose ~dry_run ~jobs ~keep_temp components =
  if components = [] then (
    echo "Nothing to do (no component was specified on the command-line)." ;
    unit)
  else
    let* plan = Solver.solve components in
    list_iter_r plan (install_component ~verbose ~dry_run ~jobs ~keep_temp)
