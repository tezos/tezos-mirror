(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base.Version

let create_version_from_args ?major ?minor ?announcement ?rc () =
  match (major, minor) with
  | None, _ -> failwith "Missing argument for [--major]"
  | _, None -> failwith "Missing argument for [--minor]"
  | Some major, Some minor ->
      make ?announcement ?rc ~major ~minor ~latest:false ()

let () =
  Clap.description
    "Manage versions.json files for Octez release pages stored in S3. This \
     tool provides commands to list, add, remove, and manage version entries." ;
  let s3_path =
    Clap.mandatory_string
      ~long:"path"
      ~short:'p'
      ~description:"S3 path where versions.json is stored."
      ~placeholder:"PATH"
      ()
  in
  let major =
    Clap.optional_int
      ~long:"major"
      ~short:'M'
      ~placeholder:"MAJOR"
      ~description:"Major version number"
      ()
  in
  let minor =
    Clap.optional_int
      ~long:"minor"
      ~short:'m'
      ~placeholder:"MINOR"
      ~description:"Minor version number"
      ()
  in
  let rc =
    Clap.optional_int
      ~long:"rc"
      ~short:'r'
      ~placeholder:"RC"
      ~description:"RC number"
      ()
  in
  let announcement =
    Clap.optional_string
      ~long:"announcement"
      ~short:'a'
      ~placeholder:"ANNOUNCEMENT"
      ~description:"Link to the announcement"
      ()
  in
  let list_versions =
    Clap.case ~description:"Display all versions" "list" @@ fun () -> `list
  in
  let add =
    Clap.case ~description:"Add a new version" "add" @@ fun () -> `add
  in
  let set_latest =
    Clap.case ~description:"Set a version as latest" "set-latest" @@ fun () ->
    `set_latest
  in
  let command = Clap.subcommand [list_versions; add; set_latest] in
  Clap.close () ;
  match command with
  | `list ->
      let versions = load_from_storage ~path:s3_path in
      Format.printf "Versions in s3://%s/versions.json:@." s3_path ;
      List.iter
        (fun version ->
          let latest_mark = if version.latest then " (latest)" else "" in
          Format.printf "  %s%s@." (to_string version) latest_mark)
        versions
  | `add ->
      let new_version =
        create_version_from_args ?announcement ?rc ?major ?minor ()
      in
      ignore @@ update_in_storage ~path:s3_path (add_version new_version) ;
      Format.printf "Added %s@." (to_string new_version)
  | `set_latest -> (
      let target_version =
        create_version_from_args ?announcement ?rc ?major ?minor ()
      in
      let updated =
        update_in_storage ~path:s3_path (Base.Version.set_latest target_version)
      in
      match find_latest updated with
      | Some latest -> Format.printf "Set %s as latest@." (to_string latest)
      | None -> Format.printf "Warning: No version marked as latest@.")
