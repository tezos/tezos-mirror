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

(* [version_to_rss_item ~component version] converts a [version] of [component]
   to an RSS item. *)
let version_to_rss_item ~component version =
  let version_str = Version.to_string version in
  let title = sf "%s %s" (String.capitalize_ascii component.name) version_str in
  let guid = Rss.Guid_name (sf "%s-%s" component.name version_str) in
  (* TODO: Deduce from command line arguments. *)
  let link = Uri.of_string "https://octez.tezos.com/releases" in
  let pubdate = version.publication_date |> Ptime.of_float_s in
  let desc =
    sf
      "%s version %d.%d%s"
      (String.capitalize_ascii component.name)
      version.major
      version.minor
      (match version.rc with Some rc -> sf "-rc%d" rc | None -> "")
  in
  Rss.item ~title ~desc ~guid ~link ?pubdate ()

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
  let command =
    Clap.subcommand
      [list_versions; add; set_latest; set_active; set_inactive; generate_rss]
  in
  let base_url =
    Clap.optional_string
      ~long:"base-url"
      ~description:
        "Base URL for RSS feed links (used with generate-rss command). \
         Defaults to: https://<S3_PATH>"
      ~placeholder:"URL"
      ()
  in
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
  | `set_active ->
      let predicate = predicate_from_args ?major ?minor ?rc () in
      let updated =
        update_in_storage ~path:s3_path (Base.Version.set_active predicate)
      in
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
      let updated =
        update_in_storage ~path:s3_path (Base.Version.set_inactive predicate)
      in
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
  | `generate_rss -> (
      let component_name = "octez" in
      let url = Option.value base_url ~default:("https://" ^ s3_path) in
      let component =
        {
          name = component_name;
          path = s3_path;
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
      let versions = Base.Version.load_from_storage ~path:component.path in
      let items =
        List.map
          (fun version -> version_to_rss_item ~component version)
          versions
      in
      let channel = Rss.channel ~title ~desc ~link ?last_build_date items in
      let output_file = "feed.xml" in
      Format.printf "Generating RSS feed...@." ;
      try
        Rss.print_file ~indent:2 ~encoding:"UTF-8" output_file channel ;
        Printf.printf "RSS feed generated: %s\n%!" output_file
      with Sys_error msg ->
        failwith (sf "System error writing %s: %s" output_file msg))
