(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

(** {2 Changesets} *)

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
    octez_source_content
    @ make
        [
          "etherlink/**/*";
          "michelson_test_scripts/**/*";
          "tzt_reference_test_suite/**/*";
        ])

let changeset_debian_packages =
  Changeset.(
    make
      [
        "scripts/packaging/build-deb-local.sh";
        "scripts/packaging/Release.conf";
        "scripts/packaging/octez/debian/*";
        "scripts/ci/build-debian-packages_current.sh";
        "scripts/ci/build-debian-packages.sh";
        "scripts/ci/prepare-apt-repo.sh";
        "scripts/ci/create_debian_repo.sh";
        "scripts/packaging/octez-archive-keyring/**/*";
        "scripts/ci/build-keyring-deb.sh";
        "scripts/packaging/tests/deb/test-keyring*";
        "docs/introduction/install-bin-deb.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_homebrew =
  Changeset.(
    make
      [
        "scripts/packaging/test_homebrew_install.sh";
        "scripts/packaging/homebrew_release.sh";
        "images/scripts/install-gcloud.sh";
        "scripts/packaging/homebrew_install.sh";
        "scripts/packaging/octez/homebrew/Formula/*";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])
