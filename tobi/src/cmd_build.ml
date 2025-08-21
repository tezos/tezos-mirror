(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

let run ~verbose ~dry_run ~jobs components =
  if components = [] then (
    echo "Nothing to do (no component was specified on the command-line)." ;
    unit)
  else
    (* Load the configuration of the components that we want to build. *)
    let* components =
      list_map_r components @@ fun name -> Component.load name Dev
    in
    (* We need the list of dependencies to know which packages to make visible to dune. *)
    let* plan =
      Solver.solve
        (List.map
           (fun (component : Component.t) ->
             (component.name, component.version))
           components)
    in
    (* Compute the value of the [--only-packages] argument that we will pass to [dune].
       This is [plan] minus components that have been installed by Tobi.
       Also gather those installed components so that we can display the list to the user. *)
    let* only_packages, installed_components =
      let rec loop acc_only_packages acc_installed_components list =
        match list with
        | [] ->
            Ok (List.rev acc_only_packages, List.rev acc_installed_components)
        | {Solver.component; _} :: tail -> (
            let* installed_version =
              Component.get_installed_commit_hash component.name
            in
            match installed_version with
            | None ->
                loop
                  (component.name :: acc_only_packages)
                  acc_installed_components
                  tail
            | Some commit_hash ->
                loop
                  acc_only_packages
                  ((component.name, commit_hash) :: acc_installed_components)
                  tail)
      in
      loop [] [] plan
    in
    (* Make the user aware that some relevant components will not be rebuilt. *)
    if installed_components <> [] then (
      echo "The following components are installed and will not be built:" ;
      Fun.flip List.iter installed_components @@ fun (name, commit_hash) ->
      if verbose then echo "- %s.%s" name commit_hash else echo "- %s" name) ;
    (* If [only_packages] is empty, it means that all requested components
       are already installed. *)
    if only_packages = [] then (
      echo "Nothing to build." ;
      unit)
    else
      (* Compute the list of paths to ask dune to build.
         This is the paths of all components that were explicitly requested. *)
      let paths =
        List.fold_left
          (fun acc (component : Component.t) ->
            List.fold_left
              (fun acc path -> String_set.add path acc)
              acc
              (let head, tail = component.paths in
               head :: tail))
          String_set.empty
          components
        |> String_set.elements
      in
      (* Run [dune build].
         Force the progress bar with [--display=progress],
         and use [Unix.execvp] instead of [Run.command],
         otherwise Dune thinks the output is not a terminal. *)
      (* Note about -j: it seems like when executed by Tobi,
         dune's default number of jobs is 1 and not the number of CPU cores.
         So it is important to specify -j. *)
      let command = "dune" in
      let arguments =
        "build" :: "--only-packages"
        :: String.concat "," only_packages
        :: "--display=progress" :: "-j" :: string_of_int jobs :: paths
      in
      if verbose then echo "%s" (quote_command command arguments) ;
      if dry_run then unit
      else
        try Unix.execvp command (Array.of_list (command :: arguments))
        with Unix.Unix_error (code, _, _) ->
          fail "failed to run %s" command ~reason:[Unix.error_message code]
