(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

(** {2 Changesets} *)

(** Modifying these files will unconditionally execute all conditional jobs.

    If the CI configuration of [before_merging] or [merge_train]
    pipelines change, we execute all jobs of these merge request
    pipelines. (We cannot currently have a finer grain and run only
    the jobs that are modified.)

    As Changesets should only be present in merge request pipelines,
    other pipelines' files need not be in the changeset.

    [changeset_base] should be included in all Changesets below, any
    exceptions should be explained. *)
let changeset_base =
  Changeset.make
    [
      ".gitlab/ci/pipelines/merge_train.yml";
      ".gitlab/ci/pipelines/before_merging.yml";
      ".gitlab-ci.yml";
    ]

let changeset_images_rust_toolchain =
  Changeset.make
    [
      "images/rust-toolchain/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

let changeset_images_rust_sdk_bindings =
  Changeset.make
    [
      "images/rust-sdk-bindings/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

(** Only if octez source code has changed *)
let changeset_octez =
  let octez_source_content =
    List.map
      (fun path -> if Sys.is_directory path then path ^ "/**/*" else path)
      (read_lines_from_file "script-inputs/octez-source-content")
    |> List.filter (fun f -> f <> "CHANGES.rst" && f <> "LICENSES/**/*")
    |> Changeset.make
  in
  Changeset.(
    changeset_base @ octez_source_content
    @ make
        [
          "etherlink/**/*";
          "michelson_test_scripts/**/*";
          "tzt_reference_test_suite/**/*";
        ])

(** Only if documentation has changed *)

let octez_docs_base_folders =
  [
    "src";
    "tezt";
    "brassaia";
    "irmin";
    "client-libs";
    "etherlink";
    "data-encoding";
    "vendors";
  ]

let changeset_octez_docs =
  Changeset.(
    changeset_base
    (* TODO refine scripts *)
    @ make ["scripts/**/*/"; "script-inputs/**/*/"]
    @ make
        (octez_docs_base_folders |> List.map (fun x -> String.cat x "/**/*.ml*"))
    @ make
        [
          "dune";
          "dune-project";
          "dune-workspace";
          "**/*.rst";
          (* Nota: stays as it is, many non-rst files in this folder *)
          "docs/**/*";
          "grafazos/doc/**/*";
        ])

(* Job [documentation:manuals] requires the build jobs, because it needs
   to run Octez executables to generate the man pages.
   So the build jobs need to be included if the documentation changes. *)
let changeset_octez_or_doc = Changeset.(changeset_octez @ changeset_octez_docs)

let changeset_docker_files = Changeset.make ["build.Dockerfile"; "Dockerfile"]

let changeset_debian_packages =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/debian_repository_partial_auto.yml";
        "scripts/packaging/build-deb-local.sh";
        "scripts/packaging/Release.conf";
        "scripts/packaging/octez/debian/*";
        "debian-deps-build.Dockerfile";
        "scripts/ci/build-debian-packages_current.sh";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-debian-packages.sh";
        "scripts/ci/prepare-apt-repo.sh";
        "scripts/ci/create_debian_repo.sh";
        "docs/introduction/install-bin-deb.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_rpm_packages =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/rpm_repository_partial_auto.yml";
        "scripts/packaging/build-rpm-local.sh";
        "scripts/packaging/octez/rpm/*";
        "scripts/packaging/tests/rpm/*";
        "rpm-deps-build.Dockerfile";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-rpm-packages.sh";
        "scripts/ci/prepare-apt-rpm-repo.sh";
        "scripts/ci/create_rpm_repo.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_homebrew =
  Changeset.(
    make
      [
        ".gitlab/ci/pipelines/homebrew_auto.yml";
        "scripts/packaging/test_homebrew_install.sh";
        "scripts/packaging/homebrew_release.sh";
        "images/scripts/install-gcloud-apt.sh";
        "scripts/packaging/homebrew_install.sh";
        "scripts/packaging/octez/homebrew/Formula/*";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

(* The linting job runs over the set of [source_directories]
   defined in [scripts/lint.sh] that must be included here: *)
let changeset_lint_files =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "tezt/**/*";
          "devtools/**/*";
          "scripts/**/*";
          "docs/**/*";
          "contrib/**/*";
          "client-libs/**/*";
          "etherlink/**/*";
        ])

(** Set of Rust files for formatting ([cargo fmt --check]). *)
let changeset_rust_fmt_files = Changeset.(changeset_base @ make ["**/*.rs"])

(** Set of Jsonnet files for formatting ([jsonnetfmt --test]). *)
let changeset_jsonnet_fmt_files = Changeset.(make ["**/*.jsonnet"])

let changeset_test_sdk_rust =
  Changeset.(
    changeset_base
    @ changeset_images_rust_toolchain
      (* Run if the [rust-toolchain] image is updated *)
    @ make ["sdk/rust/**/*"])

let changeset_test_sdk_bindings =
  Changeset.(
    changeset_base
    @ changeset_images_rust_sdk_bindings
      (* Run if the [rust-sdk-bindings] image is updated *)
    @ Sdk_bindings_ci.changeset)
