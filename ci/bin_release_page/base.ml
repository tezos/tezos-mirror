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

type version = {
  major : int;
  minor : int;
  rc : int option;
  latest : bool;
  announcement : string option;
}

type component = {
  name : string;
  path : string;
  asset_path : version -> asset_type -> string;
  url : string;
}

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

let version_string version =
  match version.rc with
  | Some rc -> sf "v%d.%d-rc%d" version.major version.minor rc
  | None -> sf "v%d.%d" version.major version.minor

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
