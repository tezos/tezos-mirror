(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines sanity jobs that are not component-specific.
   As such, the jobs are defined in the [Shared] component.

   Sanity jobs are fast jobs that trigger without the need
   to click on the manual [trigger] job.
   Their purpose is to quickly detect some common mistakes
   before adding jobs to the merge train.
   Most of them also run in [schedule_extended_test]. *)

module CI = Cacio.Shared

let job_sanity_ci =
  CI.job
    "sanity_ci"
    ~__POS__
    ~description:
      "Check that generated dune, .opam and .yml files are up-to-date."
    ~image:Tezos_ci.Images.CI.build_master
    ~stage:Test
    ~only_if_changed:
      [
        "**/manifest/**/*";
        "**/dune";
        "opam/**/*";
        "**/ci/**/*";
        ".gitlab-ci.yml";
        ".gitlab/ci/pipelines/*.yml";
      ]
    [
      "./scripts/ci/take_ownership.sh";
      "eval $(opam env)";
      "make --silent -C manifest check";
      "make --silent -C ci check";
    ]

let job_docker_hadolint =
  let files_to_lint = ["build.Dockerfile"; "Dockerfile"] in
  CI.job
    "docker:hadolint"
    ~__POS__
    ~description:"Run hadolint on some Docker files."
    ~image:Tezos_ci.Images.hadolint
    ~stage:Test
    ~only_if_changed:files_to_lint
    (List.map (( ^ ) "hadolint ") files_to_lint)

let job_oc_ocaml_fmt =
  CI.job
    "oc.ocaml_fmt"
    ~__POS__
    ~description:
      "Check that .ocamlformat files are all the same, and check that OCaml \
       source files are correctly formatted using ocamlformat."
    ~image:Tezos_ci.Images.CI.build_master
    ~stage:Test
    ~only_if_changed:["**/.ocamlformat"; "**/*.ml"; "**/*.mli"]
    ~dune_cache:(Cacio.dune_cache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      (* Check .ocamlformat files. *)
      "scripts/lint.sh --check-ocamlformat";
      (* Check actual formatting. *)
      "scripts/ci/dune.sh build --profile=dev @fmt";
    ]

let job_semgrep =
  CI.job
    "oc.semgrep"
    ~__POS__
    ~description:"Run a linter on OCaml code."
    ~image:Tezos_ci.Images.semgrep_agent
    ~stage:Test
    ~only_if_changed:
      ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "scripts/semgrep/**/*"]
    [
      "echo \"OCaml code linting. For information on how to reproduce locally, \
       check out scripts/semgrep/README.md\"";
      "sh ./scripts/semgrep/lint-all-ocaml-sources.sh";
    ]

let register () =
  CI.register_before_merging_jobs
    [
      (Immediate, job_sanity_ci);
      (Immediate, job_docker_hadolint);
      (Immediate, job_oc_ocaml_fmt);
      (Immediate, job_semgrep);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Immediate, job_sanity_ci);
      (Immediate, job_docker_hadolint);
      (Immediate, job_oc_ocaml_fmt);
      (Immediate, job_semgrep);
    ] ;
  ()
