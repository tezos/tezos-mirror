(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025-2026 Nomadic Labs. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

open Base
open Release_page

(* A page to render for a component's tab. *)
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
  emits_rss : bool;
      (* If [true], an RSS [feed.xml] is emitted in the component's directory,
         with the component's own page URL as its channel and item link. *)
}

(* The catalog of components that make up the release site: the single source of
   truth used to re-render the whole site from every component's published
   versions.json. Keep it in sync with the per-component
   [deploy_release_page_assets.sh] scripts, which decide the asset types and
   subpaths on the deploy side. *)
let site_catalog =
  [
    {
      component_name = "octez";
      subpath = "";
      asset_types = [Changelog; Binaries; Packages];
      emits_rss = true;
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
      emits_rss = false;
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
      emits_rss = false;
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
      emits_rss = false;
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

(* [needs_storage_listing asset_type] is [true] for asset types whose content
   is obtained by listing the assets (and downloading their checksums) from the
   storage bucket, as opposed to being derived from versions.json alone. *)
let needs_storage_listing = function
  | Binaries | Dashboards -> true
  | Changelog | Packages -> false

(* [render_site ?only ?versions_dir ~no_remote_assets ~bucket ~path ~url
   ~output_dir ()] renders the release site from [site_catalog] into
   [output_dir], mirroring the site layout: octez at the root, each other
   component under its subpath. If [only] is given, only that component's tab
   is (re-)rendered instead of the whole site.

   If [versions_dir] is given, each component's versions.json is read from
   [versions_dir], using the same layout as [output_dir], instead of being
   downloaded from the storage bucket. If [no_remote_assets] is [true], asset
   types that require listing the storage bucket are omitted. Together, they
   allow rendering the site without any access to the storage bucket. *)
let render_site ?only ?versions_dir ~no_remote_assets ~bucket ~path ~url
    ~output_dir () =
  let components =
    match only with
    | None -> site_catalog
    | Some name -> (
        match List.find_opt (fun c -> c.component_name = name) site_catalog with
        | Some c -> [c]
        | None ->
            Printf.eprintf
              "Error: unknown component %S: not in the release site catalog.\n"
              name ;
            exit 1)
  in
  List.iter
    (fun {component_name; subpath; asset_types; pages; emits_rss} ->
      let component = make_component ~name:component_name ~bucket ~path ~url in
      let in_site_layout root =
        if subpath = "" then root else Filename.concat root subpath
      in
      let dir = in_site_layout output_dir in
      let file =
        Option.map
          (fun versions_dir ->
            Filename.concat (in_site_layout versions_dir) "versions.json")
          versions_dir
      in
      let asset_types =
        if no_remote_assets then
          List.filter (fun a -> not (needs_storage_listing a)) asset_types
        else asset_types
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
            ~file
            ~output:(Filename.concat dir (basename ^ ".md")))
        pages ;
      if emits_rss then
        let versions = get_versions ~component ~file in
        let page_url =
          if subpath = "" then sf "https://%s%s" url path
          else sf "https://%s%s/%s" url path subpath
        in
        generate_rss
          ~component
          ~link_url:page_url
          ~versions
          ~output:(Filename.concat dir "feed.xml"))
    components

(* Renders the release site from the built-in [site_catalog]: every component's
   tab is rendered from its published versions.json in [BUCKET]/[PATH], written
   into [--output-dir] mirroring the site layout (octez at the root, each other
   component under its own subdirectory). With [--only COMPONENT], only that
   component's tab is re-rendered. With [--versions-dir] and
   [--no-remote-assets], the site can be rendered without storage access. *)
let () =
  Clap.description
    "Renders the whole Octez release site from every component's published \
     versions.json in [BUCKET]/[PATH]." ;
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
  let output_dir =
    Clap.default_string
      ~long:"output-dir"
      ~description:
        "Directory into which the rendered site is written (default: current \
         directory). octez is written at the root, each other component under \
         its own subdirectory."
      ~placeholder:"DIR"
      "."
  in
  let only =
    Clap.optional_string
      ~long:"only"
      ~description:
        "Render only the tab of the given COMPONENT from the catalog, instead \
         of the whole site. The component must be one of the site catalog \
         entries (e.g. \"octez\", \"grafazos\", \"teztale\", \
         \"octez-smart-rollup-node\")."
      ~placeholder:"COMPONENT"
      ()
  in
  let versions_dir =
    Clap.optional_string
      ~long:"versions-dir"
      ~description:
        "Read each component's versions.json from DIR instead of downloading \
         it from `BUCKET/PATH`. DIR uses the same layout as the rendered site: \
         `DIR/versions.json` for octez and `DIR/COMPONENT/versions.json` for \
         each other component. Useful to render the site locally, together \
         with [--no-remote-assets]."
      ~placeholder:"DIR"
      ()
  in
  let no_remote_assets =
    Clap.flag
      ~set_long:"no-remote-assets"
      ~description:
        "Omit the asset types that require listing `BUCKET/PATH` (binaries and \
         dashboards). Together with [--versions-dir], this renders the site \
         without any access to the storage bucket."
      false
  in
  Clap.close () ;
  render_site
    ?only
    ?versions_dir
    ~no_remote_assets
    ~bucket
    ~path
    ~url
    ~output_dir
    ()
