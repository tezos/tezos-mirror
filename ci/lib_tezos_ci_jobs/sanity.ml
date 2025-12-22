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
    ~dune_cache:true
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

let job_oc_misc_checks =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "oc.misc_checks"
    ~__POS__
    ~description:
      "Perform miscellaneous checks: lint, check WASM PVM regressions, check \
       EVM store migrations, check rollup node SQL migrations, check DAL store \
       migrations, check licences."
    ~image:Tezos_ci.Images.CI.test_master
    ~stage:Test
    ~only_if_changed:
      [
        "src/**/*";
        "tezt/**/*";
        "devtools/**/*";
        "scripts/**/*";
        "docs/**/*";
        "contrib/**/*";
        "client-libs/**/*";
        "etherlink/**/*";
      ]
    (List.flatten
       [
         (* Setup the environment. *)
         [
           "./scripts/ci/take_ownership.sh";
           ". ./scripts/version.sh";
           "eval $(opam env)";
           ". $HOME/.venv/bin/activate";
         ];
         (* Perform the checks. *)
         [
           "./scripts/ci/lint_misc_check.sh";
           "scripts/check_wasm_pvm_regressions.sh check";
           "etherlink/scripts/check_evm_store_migrations.sh check";
           "./scripts/check_rollup_node_sql_migrations.sh check";
           "./src/lib_dal_node/scripts/check_dal_store_migrations.sh check";
         ];
         (* The license check only applies to new files (in the sense of [git add]),
            so can only run in merge request pipelines. *)
         (match mode with
         | `full -> ["./scripts/ci/lint_check_licenses.sh"]
         | `no_license_check -> []);
       ])

(* Note: this job's script includes a copy-paste of the script of [grafazos.build]. *)
let job_check_jsonnet =
  CI.job
    "check_jsonnet"
    ~__POS__
    ~image:Tezos_ci.Images.jsonnet_master
    ~description:"Check jsonnet format and lint."
    ~stage:Test
    ~only_if_changed:["**/*.jsonnet"]
    [
      "cd grafazos/";
      (* For security, we explicitly install v11.1.0
           which corresponds to commit [1ce5aec]. *)
      "jb install github.com/grafana/grafonnet/gen/grafonnet-v11.1.0@1ce5aec";
      "cd ../";
      "scripts/lint.sh --check-jsonnet-format";
      "scripts/lint.sh --check-jsonnet-lint";
    ]

let job_check_rust_fmt =
  CI.job
    "check_rust_fmt"
    ~__POS__
    ~description:"Check formatting on Rust source files."
    ~image:Tezos_ci.Images.rust_toolchain_master
    ~stage:Test
    ~only_if_changed:["**/*.rs"]
    ["scripts/check-format-rust.sh"]

(* Necromantic nix-related rites. *)
let job_nix =
  CI.job
    "nix"
    ~__POS__
    ~description:"Check that the Nix lock file is up-to-date."
    ~image:Tezos_ci.Images.nix
    ~stage:Test
    ~artifacts:(Gitlab_ci.Util.artifacts ~when_:On_failure ["flake.lock"])
    ~only_if_changed:["**/*.nix"; "flake.lock"; "scripts/version.sh"]
    ~cache:[Gitlab_ci.Util.cache ~key:"nix-store" ["/nix/store"]]
    [
      "mkdir -p ~/.config/nix";
      "echo 'extra-experimental-features = flakes nix-command' > \
       ~/.config/nix/nix.conf";
      "nix run .#ci-check-version-sh-lock";
    ]

(* Note: checking commit titles only makes sense in merge request pipelines.
   In scheduled pipelines, it is too late to change commit titles. *)
let job_commit_titles =
  Cacio.parameterize @@ fun mode ->
  CI.job
    "commit_titles"
    ~__POS__
    ~description:"Check that commit titles match the developer guidelines."
    ~image:Tezos_ci.Images.CI.prebuild_master
    ~stage:Test
    ~allow_failure:
      (match mode with
      | `strict -> No
      | `lenient ->
          (* ./scripts/ci/check_commit_messages.sh exits with code 65
             when a git history contains invalid commits titles
             in situations where that is allowed. *)
          With_exit_codes [65])
    [
      (* "|| exit $?" might seem like a noop but is in fact necessary
         to please his majesty GitLab.
         For more info, see:
         - https://gitlab.com/tezos/tezos/-/merge_requests/9923#note_1538894754;
         - https://gitlab.com/tezos/tezos/-/merge_requests/12141; and
         - https://gitlab.com/groups/gitlab-org/-/epics/6074
         TODO: replace this with [FF_USE_NEW_BASH_EVAL_STRATEGY=true], see
         {{:https://docs.gitlab.com/runner/configuration/feature-flags.html}GitLab
         Runner feature flags}. *)
      "./scripts/ci/check_commit_messages.sh || exit $?";
    ]

let register () =
  CI.register_merge_request_jobs
    [
      (Immediate, job_sanity_ci);
      (Immediate, job_docker_hadolint);
      (Immediate, job_oc_ocaml_fmt);
      (Immediate, job_semgrep);
      (Immediate, job_oc_misc_checks `full);
      (Immediate, job_check_jsonnet);
      (Immediate, job_check_rust_fmt);
      (Immediate, job_nix);
    ] ;
  CI.register_before_merging_jobs [(Immediate, job_commit_titles `lenient)] ;
  CI.register_merge_train_jobs [(Immediate, job_commit_titles `strict)] ;
  CI.register_schedule_extended_test_jobs
    [
      (Immediate, job_sanity_ci);
      (Immediate, job_docker_hadolint);
      (Immediate, job_oc_ocaml_fmt);
      (Immediate, job_semgrep);
      (Immediate, job_oc_misc_checks `no_license_check);
      (Immediate, job_check_jsonnet);
      (Immediate, job_check_rust_fmt);
    ] ;
  ()
