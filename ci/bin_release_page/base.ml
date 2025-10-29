(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let sf = Printf.sprintf

type arch = X86_64 | Arm64

let string_of_arch = function X86_64 -> "x86_64" | Arm64 -> "arm64"

(* [get_field n line] returns the [n]th field of [line].
     Fields are non-empty space-separated values. *)
let get_field n line =
  let fields =
    String.split_on_char ' ' line
    |> List.map String.trim
    |> List.filter (( <> ) "")
  in
  if n >= 0 then List.nth_opt fields n
  else List.nth_opt fields (List.length fields + n)

(* Storage module *)
module Storage = struct
  (* Temporary directory for downloaded files *)
  let temp_dir = Filename.temp_dir "release_page" ""

  (* [get_path ~path ?target file] copies the [file] from [path] from the S3 bucket to
     [temp_dir]/target. If [target] is not provided [file] is used instead.
     Fails if the file does not exist. *)
  let get_file ~path ?target file =
    let target = Option.value ~default:file target in
    Format.printf "Getting %s from %s@." file path ;
    let command =
      sf
        "aws s3 cp \"s3://%s/%s\" \"%s\""
        path
        file
        (Filename.concat temp_dir target)
    in
    match Sys.command command with
    | 0 ->
        Format.printf
          "File %s stored in %s@."
          file
          (Filename.concat temp_dir target)
    | _ -> failwith (sf "Unable to get %s in %s" file path)

  (* [upload_file ~path ~local_file ~remote_file] uploads [local_file] to [path]/[remote_file] in the S3 bucket.
     Fails if the upload is unsuccessful. *)
  let upload_file ~path ~local_file ~remote_file =
    Format.printf "Uploading %s to %s/%s@." local_file path remote_file ;
    let command =
      sf "aws s3 cp \"%s\" \"s3://%s/%s\"" local_file path remote_file
    in
    match Sys.command command with
    | 0 ->
        Format.printf
          "File %s uploaded to s3://%s/%s@."
          local_file
          path
          remote_file
    | _ ->
        failwith
          (sf "Failed to upload %s to s3://%s/%s" local_file path remote_file)

  (* [get_folder_content ~path] returns the list of files in [path]. *)
  let get_folder_content ~path =
    let command = sf "aws s3 ls \"s3://%s\" --recursive" path in
    let ic = Unix.open_process_in command in
    let result = ref [] in
    (try
       while true do
         let line = input_line ic in
         match get_field (-1) line with
         | Some field -> result := field :: !result
         | None -> ()
       done
     with End_of_file -> ()) ;
    let _ = Unix.close_process_in ic in
    !result
end

(* Release types and functions *)
type asset_type = Binaries | Dashboards | Packages | Changelog

let string_of_asset_type = function
  | Binaries -> "Binaries"
  | Dashboards -> "Dashboards"
  | Packages -> "Packages"
  | Changelog -> "Changelog"

let asset_type_of_string_opt asset_type =
  match String.lowercase_ascii asset_type with
  | "binaries" -> Some Binaries
  | "dashboards" -> Some Dashboards
  | "packages" -> Some Packages
  | "changelog" -> Some Changelog
  | _ -> None

module Content = struct
  type t =
    | Text of string
    | Link of {text : string; url : string}
    | Concat of t list
    | Section of section
    | Items of t list

  and section = {title : string; content : t}

  type page = section list

  let section title content = Section {title; content}

  let link text url = Link {text; url}

  (* Defines a [Text text] but with a new line. *)
  let text_line text = Concat [Text text; Text "\n"]

  (* Defines a [Text text] but followed by an empty line. *)
  let text_empty_line text = Concat [Text text; Text "\n\n"]
end

(* Version management operations and types *)
module Version = struct
  type t = {
    major : int;
    minor : int;
    rc : int option;
    latest : bool;
    active : bool;
    announcement : string option;
    publication_date : float;
  }

  let make ~major ~minor ?rc ~latest ~active ?announcement ~publication_date ()
      =
    {major; minor; rc; latest; active; announcement; publication_date}

  let to_string {major; minor; rc; _} =
    match rc with
    | Some rc -> sf "v%d.%d-rc%d" major minor rc
    | None -> sf "v%d.%d" major minor

  (* [of_json json] converts a JSON object to a version record. *)
  let of_json json =
    let open JSON in
    try
      {
        major = json |-> "major" |> as_int;
        minor = json |-> "minor" |> as_int;
        rc = json |-> "rc" |> as_int_opt;
        latest = json |-> "latest" |> as_bool_opt |> Option.value ~default:false;
        active = json |-> "active" |> as_bool_opt |> Option.value ~default:false;
        announcement = json |-> "announcement" |> as_string_opt;
        publication_date =
          json |-> "pubDate" |> as_float_opt |> Option.value ~default:0.;
      }
    with
    | Error error ->
        failwith (sf "Failed to parse version from JSON: %s" (show_error error))
    | exn ->
        failwith
          (sf "Unexpected error parsing version: %s" (Printexc.to_string exn))

  (* [to_json version] converts a version record to a JSON object. *)
  let to_json {major; minor; rc; latest; active; announcement; publication_date}
      : JSON.u =
    `O
      ([
         ("major", `Float (Int.to_float major));
         ("minor", `Float (Int.to_float minor));
         ("latest", `Bool latest);
         ("active", `Bool active);
         ("pubDate", `Float publication_date);
       ]
      @ (match rc with
        | None -> []
        | Some rc -> [("rc", `Float (Int.to_float rc))])
      @
      match announcement with
      | None -> []
      | Some url -> [("announcement", `String url)])

  (* [load_from_file file_path] loads a list of versions from a JSON file. *)
  let load_from_file file_path =
    let open JSON in
    try
      let json = parse_file file_path in
      json |> as_list |> List.map of_json
    with
    | Error error ->
        failwith
          (sf
             "Failed to read versions.json from %s: %s"
             file_path
             (show_error error))
    | Sys_error msg -> failwith (sf "System error reading %s: %s" file_path msg)

  (* [save_to_file versions file_path] saves a list of versions to a JSON file. *)
  let save_to_file versions file_path =
    try
      let oc = open_out file_path in
      Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
      let json_versions = `A (List.map to_json versions) in
      Printf.fprintf oc "%s" (JSON.encode_u json_versions) ;
      Format.printf "Successfully saved to %s@." file_path
    with Sys_error msg ->
      failwith (sf "System error writing %s: %s" file_path msg)

  (* [add_version new_version versions] adds a new version to the list, ensuring uniqueness. *)
  let add_version new_version versions =
    let version_exists version =
      version.major = new_version.major
      && version.minor = new_version.minor
      && version.rc = new_version.rc
    in
    if List.exists version_exists versions then
      failwith (sf "Version %s already exists" (to_string new_version))
    else versions @ [new_version]

  (* [set_latest target_version versions] sets a specific version as latest, unset all others. *)
  let set_latest target_version versions =
    List.map
      (fun version ->
        if
          version.major = target_version.major
          && version.minor = target_version.minor
          && version.rc = target_version.rc
        then {version with latest = true}
        else {version with latest = false})
      versions

  (* [find_latest versions] finds the latest version in the list. *)
  let find_latest versions =
    List.find_opt (fun version -> version.latest) versions

  (* [set_active predicate versions] sets all versions where predicate returns true as active. *)
  let set_active predicate versions =
    List.map
      (fun version ->
        if predicate version then {version with active = true} else version)
      versions

  (* [set_inactive predicate versions] sets all versions where predicate returns true as inactive. *)
  let set_inactive predicate versions =
    List.map
      (fun version ->
        if predicate version then {version with active = false} else version)
      versions

  (* [find_active versions] finds all active versions in the list. *)
  let find_active versions =
    List.filter (fun version -> version.active) versions

  (* [load_from_storage ~path] loads versions.json from a storage path. *)
  let load_from_storage ~path =
    try
      Storage.get_file ~path "versions.json" ;
      let local_file = Filename.concat Storage.temp_dir "versions.json" in
      load_from_file local_file
    with
    | Sys_error msg ->
        Format.printf
          "Warning: Failed to load versions.json from %s: %s@."
          path
          msg ;
        Format.printf "Starting with empty version list.@." ;
        []
    | Failure msg ->
        Format.printf
          "Warning: Failed to parse versions.json from %s: %s@."
          path
          msg ;
        Format.printf "Starting with empty version list.@." ;
        []

  (* [save_to_storage ~path versions] saves [versions].json to a storage path (S3). *)
  let save_to_storage ~path versions =
    try
      let local_file = Filename.concat Storage.temp_dir "versions.json" in
      save_to_file versions local_file ;
      Format.printf "Versions saved locally to %s@." local_file ;
      Storage.upload_file ~path ~local_file ~remote_file:"versions.json"
    with exn ->
      failwith
        (sf
           "Failed to save versions.json to storage %s: %s"
           path
           (Printexc.to_string exn))

  (* [update_in_storage ~path update_fn] performs a complete workflow:
     load from storage, modify, save back to storage. *)
  let update_in_storage ~path update_fn =
    let versions = load_from_storage ~path in
    let updated_versions = update_fn versions in
    save_to_storage ~path updated_versions ;
    updated_versions
end

type component = {
  name : string;
  path : string;
  asset_path : Version.t -> asset_type -> string;
  url : string;
}
