(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Base
open Base.Version

type active_filter = Active | Inactive | All

(* [show_markdown content] returns a string that contains the
   markdown associated with [content]. *)
let show_markdown ?(level = 1) content =
  let buf = Buffer.create 0 in
  let pp string = Buffer.add_string buf string in
  let rec aux ~level = function
    | Content.Concat list -> List.iter (fun content -> aux ~level content) list
    | Content.Items list ->
        (* List are preceeded by an empty line,
         and followed by an empty line. *)
        pp "\n" ;
        List.iter
          (fun content ->
            pp "- " ;
            aux ~level content ;
            pp "\n")
          list ;
        pp "\n\n"
    | Content.Link {text; url} -> pp @@ sf "[%s](%s)" text url
    | Content.Text text -> pp text
    | Content.Section {title; content} ->
        pp @@ String.make level '#' ;
        pp " " ;
        pp title ;
        pp "\n" ;
        aux ~level:(level + 1) content ;
        pp "\n"
  in
  aux ~level content ;
  Buffer.contents buf

(* [markdown_of_page page] returns a string that contains the
   markdown associated with the [page]. *)
let markdown_of_page (page : Content.page) =
  List.fold_left
    (fun acc section -> acc ^ show_markdown (Content.Section section))
    ""
    page

(* [get_checksum_file ?arch ~component ~version ~asset ()]
   Download the checksum file for [asset] of [component] at [version],
   optionally scoped to [arch].

   The file is stored in [temp_dir]/ as:
   - [<arch>_sha256sums.txt], if [arch] is provided
   - [sha256sums.txt], otherwise. *)
let get_checksum_file ?arch ~component ~version ~asset () =
  let path, target =
    match arch with
    | Some arch ->
        let arch = string_of_arch arch in
        ( Filename.concat (component.asset_path version asset) arch,
          sf "%s_sha256sums.txt" arch )
    | None -> (component.asset_path version asset, "sha256sums.txt")
  in
  let file = "sha256sums.txt" in
  Storage.get_file ~path ~target file

(* [get_versions ~component] returns the list of [versions] of [component]
   listed in versions.json stored in s3 [component.path]. *)
let get_versions ~component =
  let open JSON in
  try
    Storage.get_file ~path:component.path "versions.json" ;
    parse_file (Filename.concat Storage.temp_dir "versions.json")
    |> as_list |> List.map of_json
  with Error error ->
    failwith
      ("Failed to read versions.json in " ^ component.path ^ ": "
     ^ show_error error)

(* [get_assets ?arch ~component ~version ~asset ()]
   Retrieve from S3 the list of files for [asset] of [component] at [version],
   optionally filtered by [arch].

   The list is returned as a list of strings. *)
let get_assets ?arch ~component ~version ~asset () =
  let path =
    match arch with
    | None ->
        Format.printf
          "Getting %s list from %s for version %i.%i@."
          (string_of_asset_type asset)
          component.path
          version.major
          version.minor ;
        Format.printf "%s@." (component.asset_path version asset) ;
        component.asset_path version asset
    | Some arch ->
        let arch = string_of_arch arch in
        Format.printf
          "Getting %s list from %s for version %i.%i and architecture %s@."
          (string_of_asset_type asset)
          component.path
          version.major
          version.minor
          arch ;
        Filename.concat (component.asset_path version asset) arch
  in
  Storage.get_folder_content ~path

let make_links ?arch ~component assets =
  let open Content in
  let assets_links =
    List.map
      (fun asset ->
        if Filename.basename asset = "sha256sums.txt" then
          Content.link
            (Filename.basename asset)
            ("https://" ^ component.url ^ "/" ^ asset)
        else
          let checksum_path =
            match arch with
            | Some arch ->
                Filename.concat Storage.temp_dir arch ^ "_sha256sums.txt"
            | None -> Filename.concat Storage.temp_dir "sha256sums.txt"
          in
          let command =
            Format.asprintf
              "grep '%s' %s"
              (Filename.basename asset)
              checksum_path
          in
          let ic = Unix.open_process_in command in
          let checksum =
            input_line ic |> get_field 0 |> Option.value ~default:""
          in
          let _ = Unix.close_process_in ic in
          Concat
            [
              link
                (Filename.basename asset)
                ("https://" ^ component.url ^ "/" ^ asset);
              Text " ";
              Text
                (sf "<span class=\"sha256\">(**sha256:** `%s`)</span>" checksum);
            ])
      assets
  in
  Items assets_links

(* [asset_content ~component ~version asset] returns the content for [asset]
   associated to its [version] of [component]. *)
let asset_content ~component ~version = function
  | Binaries ->
      let open Content in
      get_checksum_file ~arch:X86_64 ~component ~version ~asset:Binaries () ;
      get_checksum_file ~arch:Arm64 ~component ~version ~asset:Binaries () ;
      let binaries_x86 =
        get_assets ~component ~version ~asset:Binaries ~arch:X86_64 ()
      in
      let binaries_arm =
        get_assets ~component ~version ~asset:Binaries ~arch:Arm64 ()
      in
      let section_x86 =
        section "x86_64" @@ make_links ~component ~arch:"x86_64" binaries_x86
      in
      let section_arm64 =
        section "arm64" @@ make_links ~component ~arch:"arm64" binaries_arm
      in
      section "Static Binaries" @@ Concat [section_x86; section_arm64]
  | Dashboards ->
      get_checksum_file ~component ~version ~asset:Dashboards () ;
      let dashboards = get_assets ~component ~version ~asset:Dashboards () in
      let dashboards_links = make_links ~component dashboards in
      Content.section "Dashboards" dashboards_links
  | Packages ->
      let open Content in
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
          Content.Text ""
      | Some announcement ->
          Content.text_empty_line
            (sf
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
        sf
          "%s Release Candidate %i.%i~rc%i\n"
          (String.capitalize_ascii component.name)
          major
          minor
          rcn
    | None ->
        if latest then
          sf
            "%s %i.%i (latest)\n"
            (String.capitalize_ascii component.name)
            major
            minor
        else
          sf "%s %i.%i\n" (String.capitalize_ascii component.name) major minor
  in
  {
    Content.title;
    content = Concat (List.map (asset_content ~component ~version) asset_types);
  }

(* [generate_md ~component ~versions ~asset_types ~filter_active] generates a markdown file
   corresponding to a release page for the [versions] of [component] containing
   all the assets of [asset_types].

   This markdown is stored in [temp_dir/release_page]. *)
let generate_md ~component ~versions ~asset_types ~filter_active =
  let page =
    List.map
      (fun version -> version_section ~component ~asset_types ~version)
      (List.rev versions)
  in
  let header_note =
    match filter_active with
    | Some Active ->
        [
          {
            Content.title = "";
            content =
              Content.text_line
                "Looking for older releases? Visit our [older releases \
                 page](older_releases.html).\n";
          };
        ]
    | Some Inactive ->
        [
          {
            Content.title = "";
            content =
              Content.text_line "Return to [current releases](index.html).\n";
          };
        ]
    | _ -> []
  in
  let full_page = header_note @ page in
  let md = markdown_of_page full_page in
  let index = Filename.concat Storage.temp_dir "index.md" in
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
  let index = Filename.concat Storage.temp_dir "index.md" in
  let command =
    sf
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
      ~description:
        "Name of the component for which you are building the release page.\n\
         Choose this carefully, as it will be used for paths and titles.\n\
         In case the component is \"octez\", the release assets will be pulled \
         from toplevel directory, instead of COMPONENT/COMPONENT-vX.Y as for \
         any other component."
      ~placeholder:"COMPONENT"
      ~short:'c'
      ()
  in
  let title =
    Clap.optional_string
      ~long:"title"
      ~description:
        "Title of the generated the release page. By default, it will be set \
         to `COMPONENT releases`"
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
      ~description:
        "The name of the S3 bucket where the assets for the release page are \
         stored. All assets listed on the release page are read from this \
         bucket.\n\n\
         Note that `BUCKET/PATH` must contain a `COMPONENT/versions.json` file \
         that is readable, along with the assets associated with each version. \
         For more details, see the Storage section."
      ~placeholder:"BUCKET"
      ()
  in
  let path =
    Clap.default_string
      ~long:"path"
      ~description:
        "subpath of the releases assets the S3 bucket.\n\
         Useful in case the bucket is not used only for the release page. For \
         instance, the `site-prod.octez.tezos.com` S3 bucket is also used for \
         the documentation. Thus, the release assets are stored in \
         `octez.tezos.com/releases/`. In that case <PATH> is `/releases`."
      ~placeholder:"PATH"
      ""
  in
  let url =
    Clap.default_string
      ~long:"url"
      ~description:
        "The URL of the bucket. By default it is the name of the bucket.\n\
         This should be used in case the URL of the web page is not the same \
         as the S3 bucket name.\n\
         For instance, the `site-prod.octez.tezos.com` bucket should be used \
         with `octez.tezos.com` URL."
      ~placeholder:"URL"
      bucket
  in
  let filter_active =
    Clap.optional
      ~long:"filter-active"
      ~description:
        "Filter versions by active status.\n\
         Possible values are:\n\
         - \"active\": Show only active versions (adds banner link to older \
         releases)\n\
         - \"inactive\": Show only inactive versions (adds banner link to \
         current releases)\n\
         - \"all\" or omitted: Show all versions"
      ~placeholder:"FILTER"
      (Clap.enum
         "filter-active"
         [("active", Active); ("inactive", Inactive); ("all", All)])
      ()
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
          "List of assets types to display in the page.\n\
           Possible values are:\n\
           - \"changelog\"\n\
           - \"binaries\"\n\
           - \"dashboards\"\n\
           - \"packages\"."
        ~placeholder:"ASSET_TYPE"
        ())
  in
  Clap.close () ;
  let component =
    {
      name = component;
      path =
        (* For octez, the path is root of the bucket. *)
        (if component = "octez" then sf "%s%s" bucket path
         else sf "%s%s/%s" bucket path component);
      asset_path =
        (fun version asset_type ->
          if component = "octez" then
            sf
              "%s%s/%s-v%i.%i%s/%s"
              bucket
              path
              component
              version.major
              version.minor
              (match version.rc with
              | Some n -> sf "-rc%s" @@ string_of_int n
              | None -> "")
              (string_of_asset_type asset_type |> String.lowercase_ascii)
          else
            sf
              "%s%s/%s/%s-v%i.%i%s/%s"
              bucket
              path
              component
              component
              version.major
              version.minor
              (match version.rc with
              | Some n -> sf "-rc%s" @@ string_of_int n
              | None -> "")
              (string_of_asset_type asset_type |> String.lowercase_ascii));
      url;
    }
  in
  let all_versions = get_versions ~component in
  let versions =
    match filter_active with
    | Some Active -> List.filter (fun v -> v.active) all_versions
    | Some Inactive -> List.filter (fun v -> not v.active) all_versions
    | Some All -> all_versions
    | None ->
        Format.printf
          "Warning: no [--filter-active] argument. No filter will be applied." ;
        all_versions
  in
  generate_md ~component ~versions ~asset_types ~filter_active ;
  generate_html ~template:"./docs/release_page/template.html" ~title ~path
