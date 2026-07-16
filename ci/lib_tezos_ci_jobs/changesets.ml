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
