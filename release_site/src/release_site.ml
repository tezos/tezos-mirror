(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025-2026 Nomadic Labs. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

open Base
open Release_page

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
      asset_types = [Changelog; Binaries];
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
