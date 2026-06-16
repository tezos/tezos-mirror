(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base
open Base.Version

type rc = Rc of int | Stable

let parse_rc_opt = function
  | "stable" -> Some Stable
  | rc -> (
      match int_of_string_opt rc with Some rc -> Some (Rc rc) | None -> None)

let show_rc = function Stable -> "stable" | Rc rc -> sf "%i" rc

let create_version_from_args ?major ?minor ?(active = false) ?announcement ?rc
    ?(publication_date = 0.) () =
  match (major, minor) with
  | None, _ -> failwith "Missing argument for [--major]"
  | _, None -> failwith "Missing argument for [--minor]"
  | Some major, Some minor ->
      let rc =
        match rc with None | Some Stable -> None | Some (Rc rc) -> Some rc
      in
      make
        ?announcement
        ~major
        ~minor
        ?rc
        ~latest:false
        ~active
        ~publication_date
        ()

let predicate_from_args ?major ?minor ?rc () =
 fun version ->
  let check x = function None -> true | Some y -> x = y in
  check version.major major && check version.minor minor
  &&
  match rc with
  | None -> true
  | Some Stable -> Option.is_none version.rc
  | Some (Rc rc) -> (
      match version.rc with None -> false | Some version_rc -> version_rc = rc)

(* [version_to_rss_items ~component version] converts a [version] of [component]
   to a list of RSS items. The base release item is always included. An
   additional item is generated for each packaging revision in
   [version.revisions], preserving the full revision history in the feed. *)
let version_to_rss_items ~component version =
  let version_str = Version.to_string version in
  (* TODO: Deduce from command line arguments. *)
  let link = Uri.of_string "https://octez.tezos.com/releases" in
  let rc_suffix =
    match version.rc with Some rc -> sf "-rc%d" rc | None -> ""
  in
  let base_item =
    let title =
      sf "%s %s" (String.capitalize_ascii component.name) version_str
    in
    let guid = Rss.Guid_name (sf "%s-%s" component.name version_str) in
    let pubdate = version.publication_date |> Ptime.of_float_s in
    let desc =
      sf
        "%s version %d.%d%s"
        (String.capitalize_ascii component.name)
        version.major
        version.minor
        rc_suffix
    in
    Rss.item ~title ~desc ~guid ~link ?pubdate ()
  in
  let revision_items =
    List.map
      (fun (rev : revision) ->
        let title =
          sf
            "%s %s (packaging revision %d)"
            (String.capitalize_ascii component.name)
            version_str
            rev.build_number
        in
        let guid =
          Rss.Guid_name
            (sf "%s-%s-%d" component.name version_str rev.build_number)
        in
        let pubdate = Ptime.of_float_s rev.revision_date in
        let desc =
          sf
            "%s version %d.%d%s - packaging revision %d"
            (String.capitalize_ascii component.name)
            version.major
            version.minor
            rc_suffix
            rev.build_number
        in
        Rss.item ~title ~desc ~guid ~link ?pubdate ())
      version.revisions
  in
  base_item :: revision_items

let () =
  Clap.description
    "Manage versions.json files for Octez release pages. This tool provides \
     commands to list, add, remove, and manage version entries." ;
  let file =
    Clap.default_string
      ~long:"file"
      ~short:'f'
      ~description:"Path to local versions.json file."
      ~placeholder:"FILE"
      "versions.json"
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
  let rc_typ =
    Clap.typ ~name:"rc" ~dummy:Stable ~parse:parse_rc_opt ~show:show_rc
  in
  let rc =
    Clap.optional
      rc_typ
      ~long:"rc"
      ~short:'r'
      ~placeholder:"RC"
      ~description:
        "RC value.\n\
         Its value is \"stable\" if the version is not a Release Candidate."
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
  let build_number =
    Clap.optional_int
      ~long:"build-number"
      ~short:'b'
      ~placeholder:"BUILD_NUMBER"
      ~description:"Build number for packaging revision"
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
  let set_active =
    Clap.case ~description:"Set version(s) as active" "set-active" @@ fun () ->
    `set_active
  in
  let set_inactive =
    Clap.case ~description:"Set version(s) as inactive " "set-inactive"
    @@ fun () -> `set_inactive
  in
  let generate_rss =
    Clap.case ~description:"Generate RSS feed for releases" "generate-rss"
    @@ fun () -> `generate_rss
  in
  let download =
    Clap.case
      ~description:"Download versions.json from remote storage"
      "download"
    @@ fun () -> `download
  in
  let upload =
    Clap.case ~description:"Upload versions.json to remote storage" "upload"
    @@ fun () -> `upload
  in
  let update_build_number =
    Clap.case
      ~description:"Update the build number of an existing version"
      "update-build-number"
    @@ fun () -> `update_build_number
  in
  let command =
    Clap.subcommand
      [
        list_versions;
        add;
        set_latest;
        set_active;
        set_inactive;
        generate_rss;
        download;
        upload;
        update_build_number;
      ]
  in
  let remote_path =
    Clap.optional_string
      ~long:"path"
      ~short:'p'
      ~description:
        "Remote storage path (used by download, upload and generate-rss \
         commands)."
      ~placeholder:"PATH"
      ()
  in
  let base_url =
    Clap.optional_string
      ~long:"base-url"
      ~description:
        "Base URL for RSS feed links (used with generate-rss command). \
         Defaults to: https://<PATH>"
      ~placeholder:"URL"
      ()
  in
  let output =
    Clap.default_string
      ~long:"output"
      ~short:'o'
      ~description:
        "Output file path for the RSS feed (used with generate-rss command)."
      ~placeholder:"FILE"
      "feed.xml"
  in
  Clap.close () ;
  let require_path () =
    match remote_path with
    | Some path -> path
    | None -> failwith "The --path option is required for this command."
  in
  match command with
  | `list ->
      let versions = load_from_file file in
      Format.printf "Versions in %s:@." file ;
      List.iter
        (fun version ->
          let latest_mark = if version.latest then " (latest)" else "" in
          let build_mark =
            match version.revisions with
            | [] -> ""
            | revs ->
                let latest_bn =
                  List.fold_left
                    (fun acc (r : revision) -> max acc r.build_number)
                    0
                    revs
                in
                sf " [build %d]" latest_bn
          in
          Format.printf "  %s%s%s@." (to_string version) latest_mark build_mark)
        versions
  | `add ->
      let publication_date = Unix.time () in
      let new_version =
        create_version_from_args
          ?announcement
          ?rc
          ?major
          ?minor
          ~publication_date
          ()
      in
      let updated = add_version new_version (load_from_file file) in
      save_to_file updated file ;
      Format.printf "Added %s@." (to_string new_version)
  | `set_latest -> (
      let target_version =
        create_version_from_args ?announcement ?rc ?major ?minor ()
      in
      let updated =
        Base.Version.set_latest target_version (load_from_file file)
      in
      save_to_file updated file ;
      match find_latest updated with
      | Some latest -> Format.printf "Set %s as latest@." (to_string latest)
      | None -> Format.printf "Warning: No version marked as latest@.")
  | `set_active ->
      let predicate = predicate_from_args ?major ?minor ?rc () in
      let updated = Base.Version.set_active predicate (load_from_file file) in
      save_to_file updated file ;
      let active_versions = find_active updated in
      if List.is_empty active_versions then
        Format.printf "No versions marked as active.@."
      else (
        Format.printf "Marked the following version(s) as active:@." ;
        List.iter
          (fun v -> Format.printf "  - %s@." (to_string v))
          active_versions)
  | `set_inactive ->
      let predicate = predicate_from_args ?major ?minor ?rc () in
      let updated = Base.Version.set_inactive predicate (load_from_file file) in
      save_to_file updated file ;
      let still_active = find_active updated in
      let now_inactive =
        List.filter
          (fun v -> not (List.exists (fun a -> a = v) still_active))
          updated
        |> List.filter predicate
      in
      if List.is_empty now_inactive then
        Format.printf "No version was set as inactive.@."
      else (
        Format.printf "The following version(s) are now inactive:@." ;
        List.iter (fun v -> Format.printf "  - %s@." (to_string v)) now_inactive) ;
      if List.is_empty still_active then
        Format.printf "No version is active anymore.@."
      else (
        Format.printf "The following version(s) remain active:@." ;
        List.iter (fun v -> Format.printf "  - %s@." (to_string v)) still_active)
  | `update_build_number ->
      let major =
        match major with Some m -> m | None -> failwith "--major is required"
      in
      let minor =
        match minor with Some m -> m | None -> failwith "--minor is required"
      in
      let bn =
        match build_number with
        | Some bn -> bn
        | None -> failwith "--build-number is required"
      in
      let rc =
        match rc with None | Some Stable -> None | Some (Rc rc) -> Some rc
      in
      let revision_date = Unix.time () in
      let updated =
        Base.Version.add_build_number
          ~major
          ~minor
          ?rc
          ~build_number:bn
          ~revision_date
        @@ load_from_file file
      in
      save_to_file updated file ;
      Format.printf "Updated build number of v%d.%d to %d@." major minor bn
  | `generate_rss -> (
      let component_name = "octez" in
      let path = require_path () in
      let url = Option.value base_url ~default:("https://" ^ path) in
      let component =
        {
          name = component_name;
          path;
          asset_path = (fun _ _ -> "");
          (* [asset_path] is not used for RSS generation *)
          url;
        }
      in
      let title = String.capitalize_ascii component.name in
      let desc = sf "%s releases" component.name in
      (* TODO: Deduce from command line arguments. *)
      let link = Uri.of_string "https://octez.tezos.com/releases" in
      let last_build_date = Unix.time () |> Ptime.of_float_s in
      let versions = load_from_file file in
      let items =
        List.concat_map
          (fun version -> version_to_rss_items ~component version)
          versions
      in
      let channel = Rss.channel ~title ~desc ~link ?last_build_date items in
      let output_file = output in
      Format.printf "Generating RSS feed...@." ;
      try
        Rss.print_file ~indent:2 ~encoding:"UTF-8" output_file channel ;
        Printf.printf "RSS feed generated: %s\n%!" output_file
      with Sys_error msg ->
        failwith (sf "System error writing %s: %s" output_file msg))
  | `download ->
      let path = require_path () in
      Storage.download_file ~path ~remote_file:"versions.json" ~local_file:file
  | `upload ->
      let path = require_path () in
      Storage.upload_file ~path ~local_file:file ~remote_file:"versions.json"
