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
  let {major; minor; prerelease; latest; _} = version in
  let title =
    match prerelease with
    | Some (RC rcn) ->
        sf
          "%s Release Candidate %i.%i~rc%i\n"
          (String.capitalize_ascii component.name)
          major
          minor
          rcn
    | Some (Beta bn) ->
        sf
          "%s Beta %i.%i~beta%i\n"
          (String.capitalize_ascii component.name)
          major
          minor
          bn
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
        let variant_suffix =
          match version.prerelease with
          | Some (RC n) -> sf "-rc%d" n
          | Some (Beta n) -> sf "-beta%d" n
          | None -> ""
        in
        if name = "octez" then
          sf
            "%s%s/%s-v%i.%i%s/%s"
            bucket
            path
            name
            version.major
            version.minor
            variant_suffix
            (string_of_asset_type asset_type |> String.lowercase_ascii)
        else
          sf
            "%s%s/%s/%s-v%i.%i%s/%s"
            bucket
            path
            name
            name
            version.major
            version.minor
            variant_suffix
            (string_of_asset_type asset_type |> String.lowercase_ascii));
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

(* A page to render for a component in [--site] mode. *)
type site_page = {
  filter : active_filter option;
  page_title : string;
  basename : string; (* e.g. "index" or "older_releases" *)
}

(* A component of the release site, together with how to render its tab. *)
type site_component = {
  component_name : string;
  subpath : string; (* location under [bucket]/[path]; "" for octez (root) *)
  asset_types : asset_type list;
  pages : site_page list;
}

(* The catalog of components that make up the release site: the single source of
   truth used by [--site] to re-render the whole site from every component's
   published versions.json. Keep it in sync with the per-component
   [deploy_release_page_assets.sh] scripts, which decide the asset types and
   subpaths on the deploy side. *)
let site_catalog =
  [
    {
      component_name = "octez";
      subpath = "";
      asset_types = [Changelog; Binaries; Packages];
      pages =
        [
          {
            filter = Some Active;
            page_title = "Octez releases";
            basename = "index";
          };
          {
            filter = Some Inactive;
            page_title = "Octez older releases";
            basename = "older_releases";
          };
        ];
    };
    {
      component_name = "grafazos";
      subpath = "grafazos";
      asset_types = [Dashboards];
      pages =
        [
          {
            filter = Some All;
            page_title = "Grafazos releases";
            basename = "index";
          };
        ];
    };
    {
      component_name = "teztale";
      subpath = "teztale";
      asset_types = [Binaries; Packages];
      pages =
        [
          {
            filter = Some All;
            page_title = "Teztale releases";
            basename = "index";
          };
        ];
    };
    {
      component_name = "octez-smart-rollup-node";
      subpath = "octez-smart-rollup-node";
      asset_types = [Binaries];
      pages =
        [
          {
            filter = Some All;
            page_title = "Octez Smart Rollup node releases";
            basename = "index";
          };
        ];
    };
  ]

(* [render_site ~bucket ~path ~url ~output_dir] renders every component in
   [site_catalog] from its published versions.json into [output_dir], mirroring
   the site layout: octez at the root, each other component under its subpath. *)
let render_site ~bucket ~path ~url ~output_dir =
  List.iter
    (fun {component_name; subpath; asset_types; pages} ->
      let component = make_component ~name:component_name ~bucket ~path ~url in
      let dir =
        if subpath = "" then output_dir else Filename.concat output_dir subpath
      in
      if Sys.command (sf "mkdir -p %s" (Filename.quote dir)) <> 0 then
        failwith (sf "Failed to create output directory %s" dir) ;
      List.iter
        (fun {filter; page_title; basename} ->
          Format.printf
            "Rendering %s page for component %s@."
            basename
            component_name ;
          render_page
            ~component
            ~asset_types
            ~filter_active:filter
            ~title:page_title
            ~path
            ~file:None
            ~output:(Filename.concat dir (basename ^ ".md")))
        pages)
    site_catalog

(* This script takes a [component] name, page [title], a [bucket] name,
   a [path] and a list of [asset_type] as arguments.

   It will create a release page, titled [title], associated to that [component].
   For each versions found in [bucket/path/versions.json], the page will contain the list
   of assets for each [asset_type] specified.

   Alternatively, with [--site], it re-renders the whole release site from
   [site_catalog] instead of a single component.
*)
let () =
  Clap.description
    "Creates a release page for the given COMPONENT. The page will contain \
     sections for each asset type given as arguments. The assets listed are \
     those stored in [BUCKET]/[PATH]." ;
  let component =
    Clap.optional_string
      ~long:"component"
      ~description:
        "Name of the component for which you are building the release page.\n\
         Required unless [--site] is given.\n\
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
  let file =
    Clap.optional_string
      ~long:"file"
      ~short:'f'
      ~description:
        "Path to a local versions.json file. If provided, skips the S3 \
         download."
      ~placeholder:"FILE"
      ()
  in
  let output =
    Clap.default_string
      ~long:"output"
      ~description:
        "Path where the generated markdown file is written (e.g. [index.md]). \
         The corresponding HTML file is written alongside it (e.g. \
         [index.html])."
      ~placeholder:"FILE"
      "./index.md"
  in
  let site =
    Clap.flag
      ~set_long:"site"
      ~description:
        "Render the whole release site from the built-in catalog instead of a \
         single component. Every component's page is rendered from its \
         published versions.json into [--output-dir], mirroring the site \
         layout. Ignores [--component], [--title], [--filter-active], \
         [--file], [--output] and the asset-type arguments."
      false
  in
  let output_dir =
    Clap.default_string
      ~long:"output-dir"
      ~description:
        "Directory into which [--site] writes the rendered site (default: \
         current directory). octez is written at the root, each other \
         component under its own subdirectory. Ignored without [--site]."
      ~placeholder:"DIR"
      "."
  in
  Clap.close () ;
  if site then render_site ~bucket ~path ~url ~output_dir
  else
    let component_name =
      match component with
      | Some component_name -> component_name
      | None ->
          Printf.eprintf
            "Error: the --component option is required unless --site.\n" ;
          exit 1
    in
    let title =
      Option.value
        title
        ~default:(String.capitalize_ascii component_name ^ " releases")
    in
    let component = make_component ~name:component_name ~bucket ~path ~url in
    render_page
      ~component
      ~asset_types
      ~filter_active
      ~title
      ~path
      ~file
      ~output
