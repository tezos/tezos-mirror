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

(* [get_versions ~component ~file] returns the list of [versions] of [component].
   If [file] is [Some path], reads from that local path instead of downloading
   from S3. *)
let get_versions ~component ~file =
  let open JSON in
  let path =
    match file with
    | Some f -> f
    | None ->
        Storage.get_file ~path:component.path "versions.json" ;
        Filename.concat Storage.temp_dir "versions.json"
  in
  try parse_file path |> as_list |> List.map of_json
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
          "Getting %s list from %s for version %s@."
          (string_of_asset_type asset)
          component.path
          (Version.to_string version) ;
        Format.printf "%s@." (component.asset_path version asset) ;
        component.asset_path version asset
    | Some arch ->
        let arch = string_of_arch arch in
        Format.printf
          "Getting %s list from %s for version %s and architecture %s@."
          (string_of_asset_type asset)
          component.path
          (Version.to_string version)
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
  let {number; prerelease; latest; _} = version in
  let number = Version.number_to_string number in
  let name = String.capitalize_ascii component.name in
  let title =
    match prerelease with
    | Some (RC rcn) -> sf "%s Release Candidate %s~rc%i\n" name number rcn
    | Some (Beta bn) -> sf "%s Beta %s~beta%i\n" name number bn
    | None ->
        if latest then sf "%s %s (latest)\n" name number
        else sf "%s %s\n" name number
  in
  {
    Content.title;
    content = Concat (List.map (asset_content ~component ~version) asset_types);
  }

(* [generate_md ~component ~versions ~asset_types ~filter_active] generates a markdown file
   corresponding to a release page for the [versions] of [component] containing
   all the assets of [asset_types].

   This markdown is stored in [temp_dir/release_page] and its path is returned. *)
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
  Format.printf "Generated %s@." index ;
  index

(* [generate_html ~template ~title ~path md_path] generates
   an index.html, titled [title], file out of the markdown file at [md_path]
   and the [template.html] file.

   As this file is supposed to be used for a release page,
   a [path] is required as a metadata used for the
   assets.

   Returns the path of the generated HTML file. *)
let generate_html ~template ~title ~path md_path =
  let html_path = Filename.concat Storage.temp_dir "index.html" in
  let command =
    sf
      "pandoc %s -s --template=\"%s\" --metadata=title=\"%s\" \
       --metadata=path=\"%s\" --css=%s/style.css -o %s"
      md_path
      template
      title
      path
      path
      html_path
  in
  match Sys.command command with
  | 0 ->
      Format.printf "File %s generated from %s@." html_path md_path ;
      html_path
  | n -> failwith ("Failed to generate release page: Error " ^ Int.to_string n)

(* [make_component ~name ~bucket ~path ~url] builds the [component] record
   describing where [name]'s versions.json and assets live in [bucket]/[path].

   The "octez" component is special: its assets are stored at the root of
   [bucket]/[path] rather than under a [name]/ subdirectory. *)
let make_component ~name ~bucket ~path ~url =
  {
    name;
    path =
      (* For octez, the path is the root of the bucket. *)
      (if name = "octez" then sf "%s%s" bucket path
       else sf "%s%s/%s" bucket path name);
    asset_path =
      (fun version asset_type ->
        let version_tag = Version.to_string version in
        let asset = string_of_asset_type asset_type |> String.lowercase_ascii in
        if name = "octez" then
          sf "%s%s/%s-%s/%s" bucket path name version_tag asset
        else sf "%s%s/%s/%s-%s/%s" bucket path name name version_tag asset);
    url;
  }

(* [render_page ~component ~asset_types ~filter_active ~title ~path ~file ~output]
   renders a single release page for [component]: it writes the markdown to
   [output] and the HTML alongside it (with a [.html] extension). Versions are
   read from S3 unless [file] provides a local versions.json. *)
let render_page ~component ~asset_types ~filter_active ~title ~path ~file
    ~output =
  let all_versions = get_versions ~component ~file in
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
  let md_path = generate_md ~component ~versions ~asset_types ~filter_active in
  let html_path =
    generate_html
      ~template:"./docs/release_page/template.html"
      ~title
      ~path
      md_path
  in
  let move src dst =
    try Sys.rename src dst
    with Sys_error _ ->
      (* Fallback for cross-device moves (e.g. /tmp to working directory) *)
      let content = In_channel.(with_open_bin src input_all) in
      Out_channel.(with_open_bin dst (fun oc -> output_string oc content)) ;
      Sys.remove src
  in
  let dst_html = Filename.remove_extension output ^ ".html" in
  move md_path output ;
  move html_path dst_html
