(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let temp_dir = Filename.temp_dir "release_page" ""

(* Module to handle the storage (S3 bucket).
   All used files are stored in temporary directoy. *)
module Storage = struct
  (* [get_path ~path ?target file] copies the [file] from [path] from the S3 bucket to
     [temp_dir]/target. If [target] is not provided [file] is used instead.
     Fails if the file does not exist. *)
  let get_file ~path ?target file =
    let target = Option.value ~default:file target in
    Format.printf "Getting %s from %s@." file path ;
    let command =
      Format.sprintf
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
    | _ -> failwith (Format.sprintf "Unable to get %s in %s" file path)

  (* [get_folder_content ~path] returns the list of files in [path]. *)
  let get_folder_content ~path =
    let command =
      (* awk is used to print only the last field from each line. *)
      Format.sprintf
        "aws s3 ls \"s3://%s\" --recursive | awk '{print $NF}'"
        path
    in
    let ic = Unix.open_process_in command in
    let result = ref [] in
    (try
       while true do
         let line = input_line ic in
         result := line :: !result
       done
     with End_of_file -> ()) ;
    let _ = Unix.close_process_in ic in
    !result
end

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
  binaries_path : version -> string;
  url : string;
}

type section = {title : string; content : content}

and content =
  | Section of section
  | Concat of content list
  | Items of content list
  | Link of {text : string; url : string}
  | Text of string

let section title content = Section {title; content}

let link text url = Link {text; url}

(* Defines a [Text text] but with a new line. *)
let text_line text = Concat [Text text; Text "\n"]

(* Defines a [Text text] but followed by an empty line. *)
let text_empty_line text = Concat [Text text; Text "\n\n"]

type page = section list

type arch = X86_64 | Arm64

let string_of_arch = function X86_64 -> "x86_64" | Arm64 -> "arm64"

(* [show_markdown content] returns a string that contains the
   markdown associated with [content]. *)
let rec show_markdown ?(level = 1) = function
  | Concat list ->
      String.concat ""
      @@ List.map (fun content -> show_markdown ~level content) list
  | Items list ->
      (* List are preceeded by an empty line,
         and followed by an empty line. *)
      ("\n\n" ^ String.concat "\n"
      @@ List.map (fun content -> "- " ^ show_markdown ~level content) list)
      ^ "\n\n"
  | Link {text; url} -> Format.sprintf "[%s](%s)" text url
  | Text text -> text
  | Section {title; content} ->
      (String.make level '#' ^ " " ^ title ^ "\n")
      ^ show_markdown ~level:(level + 1) content
      ^ "\n"

(* [markdown_of_page page] returns a string that contains the
   markdown associated with the [page]. *)
let markdown_of_page (page : page) =
  List.fold_left
    (fun acc section -> acc ^ show_markdown (Section section))
    ""
    page

type asset_type = Binaries | Packages | Changelog

let string_of_asset_type asset_type =
  match asset_type with
  | Binaries -> "Binaries"
  | Packages -> "Packages"
  | Changelog -> "Changelog"

let asset_type_of_string_opt asset_type =
  match String.lowercase_ascii asset_type with
  | "binaries" -> Some Binaries
  | "packages" -> Some Packages
  | "changelog" -> Some Changelog
  | _ -> None

(* [get_checksum_file ~component  ~version arch] downloads the checksum
   file for [component] with [version] and [arch].

   The checksum file is stored in [temp_dir]/ as [arch_sha256sums.txt]. *)
let get_checksum_file ~component ~version arch =
  let arch = string_of_arch arch in
  let path = Filename.concat (component.binaries_path version) arch in
  let target = Format.sprintf "%s_sha256sums.txt" arch in
  let file = "sha256sums.txt" in
  Storage.get_file ~path ~target file

(* [get_versions ~component] returns the list of [versions] of [component]
   listed in versions.json stored in s3 [component.path]. *)
let get_versions ~component =
  let open JSON in
  try
    Storage.get_file ~path:component.path "versions.json" ;
    parse_file (Filename.concat temp_dir "versions.json")
    |> as_list
    |> List.map (fun version ->
           {
             major = version |-> "major" |> as_int;
             minor = version |-> "minor" |> as_int;
             rc = version |-> "rc" |> as_int_opt;
             latest =
               version |-> "latest" |> as_bool_opt
               |> Option.value ~default:false;
             announcement = version |-> "announcement" |> as_string_opt;
           })
  with Error error ->
    failwith
      ("Failed to read versions.json in " ^ component.path ^ ": "
     ^ show_error error)

(* [get_binaries ~component ~version arch] gets from the s3 [bucket]
   the [arch] binaries of [version] of [component].
   They are returned as a list of string. *)
let get_binaries ~component ~version arch =
  let arch = string_of_arch arch in
  Format.printf
    "Getting binaries list from %s for version %i.%i and architecture %s@."
    component.path
    version.major
    version.minor
    arch ;
  let path = Filename.concat (component.binaries_path version) arch in
  Storage.get_folder_content ~path

let binaries_links ~component ~arch binaries =
  let binaries_links =
    List.map
      (fun binary ->
        if Filename.basename binary = "sha256sums.txt" then
          link
            (Filename.basename binary)
            ("https://" ^ component.url ^ "/" ^ binary)
        else
          let command =
            Format.asprintf
              "grep '%s' %s | awk '{print $1}'"
              (Filename.basename binary)
              (Filename.concat temp_dir arch ^ "_sha256sums.txt")
          in
          let ic = Unix.open_process_in command in
          let checksum = input_line ic in
          let _ = Unix.close_process_in ic in
          Concat
            [
              link
                (Filename.basename binary)
                ("https://" ^ component.url ^ "/" ^ binary);
              Text " ";
              Text
                (Format.sprintf
                   "<span class=\"sha256\">(**sha256:** `%s`)</span>"
                   checksum);
            ])
      binaries
  in
  Items binaries_links

(* [asset_content ~component ~version asset] returns the content for [asset]
   associated to its [version] of [component]. *)
let asset_content ~component ~version = function
  | Binaries ->
      get_checksum_file ~component ~version X86_64 ;
      get_checksum_file ~component ~version Arm64 ;
      let binaries_x86 = get_binaries ~component ~version X86_64 in
      let binaries_arm = get_binaries ~component ~version Arm64 in
      let section_x86 =
        section "x86_64"
        @@ binaries_links ~component ~arch:"x86_64" binaries_x86
      in
      let section_arm64 =
        section "arm64" @@ binaries_links ~component ~arch:"arm64" binaries_arm
      in
      section "Static Binaries" @@ Concat [section_x86; section_arm64]
  | Packages ->
      Concat
        [
          section "Debian packages"
          @@ text_line
               "For installation instructions, refer to the [Octez Debian \
                Packages \
                Guide](https://octez.tezos.com/docs/introduction/howtoget.html#ubuntu-and-debian-octez-packages)";
          section "RPM packages"
          @@ text_line
               "For installation instructions, refer to the [Octez RPM \
                Packages \
                Guide](https://tezos.gitlab.io/introduction/howtoget.html#fedora-octez-packages)";
        ]
  | Changelog -> (
      match version.announcement with
      | None ->
          (* If no announcement is provided for [versions], we just return an empty string.*)
          Text ""
      | Some announcement ->
          text_empty_line
            (Format.sprintf
               "Details and changelogs available in [the documentation](%s)"
               announcement))

(* [version_section ~component ~asset_types ~version] returns a section corresponding to
   the [version] of [component] in the release page,
   containing the assets of [asset_types]. *)
let version_section ~component ~asset_types ~version =
  let {major; minor; rc; latest; _} = version in
  let title =
    match rc with
    | Some rcn ->
        Format.sprintf
          "%s Release Candidate %i.%i~rc%i\n"
          (String.capitalize_ascii component.name)
          major
          minor
          rcn
    | None ->
        if latest then
          Format.sprintf
            "%s %i.%i (latest)\n"
            (String.capitalize_ascii component.name)
            major
            minor
        else
          Format.sprintf
            "%s %i.%i\n"
            (String.capitalize_ascii component.name)
            major
            minor
  in
  {
    title;
    content = Concat (List.map (asset_content ~component ~version) asset_types);
  }

(* [generate_md ~component ~versions ~asset_types] generates a markdown file
   corresponding to a release page for the [versions] of [component] containing
   all the assets of [asset_types].

   This markdown is stored in [temp_dir/release_page]. *)
let generate_md ~component ~versions ~asset_types =
  let page =
    List.map
      (fun version -> version_section ~component ~asset_types ~version)
      (List.rev versions)
  in
  let md = markdown_of_page page in
  let index = Filename.concat temp_dir "index.md" in
  if Sys.file_exists index then
    Format.printf "Warning: %s already exists. It will be erased@." index ;
  let index_file = open_out index in
  Fun.protect ~finally:(fun () -> close_out index_file) @@ fun () ->
  Printf.fprintf index_file "%s" md ;
  Format.printf "Generated %s@." index

(* [generate_html ~template ~title ~path index] generates
   an index.html, titled [title], file out of [temp_dir/index.md] markdown file
   and the [template.html] file.

   As this file is supposed to be used for a release page,
   a [path] is required as a metadata used for the
   assets. *)
let generate_html ~template ~title ~path =
  let index = Filename.concat temp_dir "index.md" in
  let command =
    Format.sprintf
      "pandoc %s -s --template=\"%s\" --metadata=title=\"%s\" \
       --metadata=path=\"%s\" --css=%s/style.css -o index.html"
      index
      template
      title
      path
      path
  in
  match Sys.command command with
  | 0 -> Format.printf "File index.html generated from %s@." index
  | n -> failwith ("Failed to generate release page: Error " ^ Int.to_string n)

(* This script takes a [component] name, page [title], a [bucket] name,
   a [path] and a list of [asset_type] as arguments.

   It will create a release page, titled [title], associated to that [component].
   For each versions found in [bucket/path/versions.json], the page will contain the list
   of assets for each [asset_type] specified.
*)
let () =
  Clap.description
    "Creates a release page for the given COMPONENT. The page will contain \
     sections for each asset type given as arguments. The assets listed are \
     those stored in [BUCKET]/[PATH]." ;
  let component =
    Clap.mandatory_string
      ~long:"component"
      ~description:"Component name"
      ~placeholder:"COMPONENT"
      ~short:'c'
      ()
  in
  let title =
    Clap.optional_string
      ~long:"title"
      ~description:"Title fo the release page"
      ~short:'t'
      ~placeholder:"TITLE"
      ()
    (* We use Option.value instead of CLap.default_string because
       the default value would not be correctly displayed in the man page. *)
    |> Option.value ~default:(String.capitalize_ascii component ^ " releases")
  in
  let bucket =
    Clap.mandatory_string
      ~long:"bucket"
      ~description:"Bucket name"
      ~placeholder:"BUCKET"
      ()
  in
  let path =
    Clap.default_string
      ~long:"path"
      ~description:"Path of the component in the bucket"
      ~placeholder:"PATH"
      ""
  in
  let url =
    Clap.default_string
      ~long:"url"
      ~description:
        "URL of the bucket. `https://` should not be included.\n\
         For example, to build the tezos/tezos release page the URL value \
         should be `octez.tezos.com`"
      ~placeholder:"URL"
      bucket
  in
  let asset_types =
    Clap.(
      list
        (typ
           ~name:"asset_type"
           ~dummy:Changelog
           ~parse:asset_type_of_string_opt
           ~show:string_of_asset_type)
        ~description:
          "List of assets types to display in the page. Possible values are \
           \"changelog\", \"binaries\" and \"packages\"."
        ~placeholder:"ASSET_TYPE"
        ())
  in
  Clap.close () ;
  let component =
    {
      name = component;
      path =
        (* For octez, the path is root of the bucket. *)
        (if component = "octez" then Format.sprintf "%s%s" bucket path
         else Format.sprintf "%s%s/%s" bucket path component);
      binaries_path =
        (fun version ->
          if component = "octez" then
            Format.sprintf
              "%s%s/%s-v%i.%i%s/binaries"
              bucket
              path
              component
              version.major
              version.minor
              (match version.rc with
              | Some n -> Format.sprintf "-rc%s" @@ string_of_int n
              | None -> "")
          else
            Format.sprintf
              "%s%s/%s/%s-v%i.%i%s/binaries"
              bucket
              path
              component
              component
              version.major
              version.minor
              (match version.rc with
              | Some n -> Format.sprintf "-rc%s" @@ string_of_int n
              | None -> ""));
      url;
    }
  in
  let versions = get_versions ~component in
  generate_md ~component ~versions ~asset_types ;
  generate_html ~template:"./docs/release_page/template.html" ~title ~path
