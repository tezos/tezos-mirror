(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines jobs that were migrated to Cacio,
   but which are not yet part of a component.

   As such, the jobs are defined in the [Shared] component,
   and are added to the [scheduled_extended_test] pipeline. *)

module CI = Cacio.Shared

let job_check_lift_limits_patch =
  CI.job
    "oc.check_lift_limits_patch"
    ~__POS__
    ~stage:Test
    ~description:
      "Check that src/bin_tps_evaluation/lift_limits.patch still applies."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:
      [
        "src/bin_tps_evaluation/lift_limits.patch";
        "src/proto_alpha/lib_protocol/main.ml";
      ]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      (* Check that the patch only modifies the src/proto_alpha/lib_protocol. *)
      "[ $(git apply --numstat src/bin_tps_evaluation/lift_limits.patch | cut \
       -f3) = \"src/proto_alpha/lib_protocol/main.ml\" ]";
      "git apply src/bin_tps_evaluation/lift_limits.patch";
      "dune build @src/proto_alpha/lib_protocol/check";
    ]

let job_python_check =
  CI.job
    "oc.python_check"
    ~__POS__
    ~stage:Test
    ~description:
      "Run Python checks (environment in sync. with the image, lint, \
       typecheck)."
    ~image:Tezos_ci.Images.CI.test
    ~only_if_changed:["poetry.lock"; "pyproject.toml"; "**/*.py"]
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      ". $HOME/.venv/bin/activate";
      "./scripts/ci/lint_misc_python_check.sh";
    ]

let job_integration_compiler_rejections =
  CI.job
    "oc.integration:compiler-rejections"
    ~__POS__
    ~stage:Test
    ~description:"Run the tests defined under dune alias @runtest_rejections."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:(Changesets.changeset_octez |> Tezos_ci.Changeset.encode)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "dune build @runtest_rejections";
    ]

let job_script_test_gen_genesis =
  CI.job
    "oc.script:test-gen-genesis"
    ~__POS__
    ~stage:Test
    ~description:"Check that scripts/gen-genesis/gen_genesis.exe still builds."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:(Changesets.changeset_octez |> Tezos_ci.Changeset.encode)
    ["eval $(opam env)"; "dune build scripts/gen-genesis/gen_genesis.exe"]

let dune_cache_pull_from_pipeline =
  Cacio.dune_cache ~key:Pipeline ~policy:Pull ()

(* Add these jobs to the [needs_legacy] of jobs that use [dune_cache_pull_from_pipeline]
   so that the cache is actually available. *)
let needs_cache_from_x86_64_build_jobs =
  [
    (* We don't need the artifacts, but we do want the cache from the build jobs. *)
    (Cacio.Job, Code_verification.job_build_x86_64_release Before_merging);
    (Job, Code_verification.job_build_x86_64_exp Before_merging);
    (Job, Code_verification.job_build_x86_64_extra_dev Before_merging);
  ]

let job_script_snapshot_alpha_and_link =
  CI.job
    "oc.script:snapshot_alpha_and_link"
    ~__POS__
    ~stage:Test
    ~description:
      "Test that Alpha can be snapshotted using snapshot_alpha_and_link.sh."
    ~image:Tezos_ci.Images.CI.build
    ~cpu:Very_high
    ~only_if_changed:
      [
        "src/proto_alpha/**/*";
        "scripts/snapshot_alpha_and_link.sh";
        "scripts/snapshot_alpha.sh";
        "scripts/user_activated_upgrade.sh";
      ]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~dune_cache:(Cacio.dune_cache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/script:snapshot_alpha_and_link.sh";
    ]

(* Requires Python.
   Can be changed to a python image, but using the build docker image
   to keep in sync with the python version used for the tests. *)
let job_script_b58_prefix =
  CI.job
    "oc.script:b58_prefix"
    ~__POS__
    ~stage:Test
    ~description:""
    ~image:Tezos_ci.Images.CI.test
    ~only_if_changed:
      [
        "scripts/b58_prefix/b58_prefix.py";
        "scripts/b58_prefix/test_b58_prefix.py";
      ]
    [
      ". ./scripts/version.sh";
      ". $HOME/.venv/bin/activate";
      "poetry run pylint scripts/b58_prefix/b58_prefix.py \
       --disable=missing-docstring --disable=invalid-name";
      "poetry run pytest scripts/b58_prefix/test_b58_prefix.py";
    ]

let job_test_liquidity_baking_scripts =
  CI.job
    "oc.test-liquidity-baking-scripts"
    ~__POS__
    ~stage:Test
    ~description:"Test the liquidity baking scripts."
    ~image:Tezos_ci.Images.CI.build
    ~needs_legacy:
      [
        (Artifacts, Code_verification.job_build_x86_64_release Before_merging);
        (Artifacts, Code_verification.job_build_x86_64_exp Before_merging);
        (Artifacts, Code_verification.job_build_x86_64_extra_dev Before_merging);
      ]
    ~only_if_changed:
      [
        "src/**/*";
        "scripts/ci/test_liquidity_baking_scripts.sh";
        "scripts/check-liquidity-baking-scripts.sh";
      ]
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/test_liquidity_baking_scripts.sh";
    ]

let job_oc_script_test_release_versions =
  CI.job
    "oc.script:test_octez_release_versions"
    ~__POS__
    ~stage:Test
    ~description:"Test how src/lib_version parses Git tags."
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:
      ["scripts/test_octez_release_version.sh"; "src/lib_version/**/*"]
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/test_octez_release_version.sh";
    ]

let job_test_release_versions =
  CI.job
    "oc:scripts:release_script_values"
    ~__POS__
    ~description:
      "Test the values defined in scripts/ci/octez-packages-version.sh."
    ~image:Tezos_ci.Images.CI.prebuild
    ~stage:Test
    ~only_if_changed:
      [
        "scripts/releases/octez-release.sh";
        "scripts/ci/octez-packages-version.sh";
        "scripts/ci/test_release_values.sh";
      ]
    ["scripts/ci/test_release_values.sh"]

let job_resto_unit =
  Cacio.parameterize @@ fun arch ->
  CI.job
    ("resto.unit:" ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ~__POS__
    ~description:"Run unit tests for resto."
    ~arch
    ?storage:(match arch with Arm64 -> Some Ramfs | _ -> None)
    ~image:Tezos_ci.Images.CI.test
    ~stage:Test
    ~timeout:(Minutes 10)
    ~only_if_changed:["resto/**"]
    ["eval $(opam env)"; "dune runtest resto"]

(* "de" stands for "data-encoding". *)
let job_de_unit =
  Cacio.parameterize @@ fun arch ->
  CI.job
    ("de.unit:" ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ~__POS__
    ~description:"Run unit tests for data-encoding."
    ~arch
    ?storage:(match arch with Arm64 -> Some Ramfs | _ -> None)
    ~image:Tezos_ci.Images.CI.test
    ~stage:Test
    ~only_if_changed:["data-encoding/**"]
    ["eval $(opam env)"; "dune runtest data-encoding"]

let job_oc_unit_protocol_compiles =
  CI.job
    "oc.unit:protocol_compiles"
    ~__POS__
    ~description:
      "Check that all protocols can still be compiled by \
       octez-protocol-compiler."
    ~arch:Amd64
    ~cpu:Very_high
    ~image:Tezos_ci.Images.CI.build
    ~stage:Test
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "dune build @runtest_compile_protocol";
    ]

let job_oc_unit_webassembly_x86_64 =
  CI.job
    "oc.unit:webassembly-x86_64"
    ~__POS__
    ~description:"Run the tests for WASM."
    ~arch:Amd64 (* The wasm tests are written in Python *)
    ~image:Tezos_ci.Images.CI.test
    ~stage:Test
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~timeout:(Minutes 20)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4663
         This test takes around 2 to 4min to complete, but it sometimes
         hangs. We use a timeout to retry the test in this case. The
         underlying issue should be fixed eventually, turning this timeout
         unnecessary. *)
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-webassembly"]

(* Artifacts that are generated by test_wrapper.sh. *)
let artifacts_test_results_xml arch =
  Gitlab_ci.Util.artifacts
    ~name:
      ("$CI_JOB_NAME-$CI_COMMIT_SHA-"
      ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ["test_results"]
    ~reports:(Gitlab_ci.Util.reports ~junit:"test_results/*.xml" ())
    ~expire_in:(Duration (Days 1))
    ~when_:Always

let job_oc_unit_non_proto_x86_64 =
  CI.job
    "oc.unit:non-proto-x86_64"
    ~__POS__
    ~description:
      "Run unit tests for the non-protocol parts of Octez (on amd64)."
    ~stage:Test
    ~retry:Gitlab_ci.Types.{max = 2; when_ = []}
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~image:Tezos_ci.Images.CI.test
      (* use the test image because [lib_benchmark] require Python *)
    ~arch:Amd64
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~artifacts:(artifacts_test_results_xml Amd64)
    ~dune_cache:(Cacio.dune_cache ())
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      (* [test-webassembly] is only tested on arm64. *)
      "make test-nonproto-unit";
    ]

(* Note: this job does not depend on build jobs because:
   - it is not using their artefacts;
   - it is not using their cache.
   It only uses the cargo cache and sccache which are keyed on the job name. *)
let job_oc_unit_non_proto_arm64 =
  CI.job
    "oc.unit:non-proto-arm64"
    ~__POS__
    ~description:
      "Run unit tests for the non-protocol parts of Octez (on arm64)."
    ~stage:Test
    ~retry:Gitlab_ci.Types.{max = 2; when_ = []}
    ~parallel:(Vector 2)
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~image:Tezos_ci.Images.CI.test
      (* use the test image because [lib_benchmark] require Python *)
    ~arch:Arm64
    ~storage:Ramfs
    ~variables:
      [
        (* Make sure that [scripts/test_wrapper.sh] partitions
           the set of @runtest targets to build. *)
        ("DISTRIBUTE_TESTS_TO_PARALLELS", "true");
        ("DUNE_ARGS", "-j 12");
      ]
    ~artifacts:(artifacts_test_results_xml Arm64)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make test-nonproto-unit test-webassembly";
    ]

let job_oc_unit_proto_x86_64 =
  CI.job
    "oc.unit:proto-x86_64"
    ~__POS__
    ~description:"Run unit tests for the protocol parts of Octez."
    ~stage:Test
    ~retry:Gitlab_ci.Types.{max = 2; when_ = []}
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~image:Tezos_ci.Images.CI.build
    ~arch:Amd64
    ~cpu:Very_high
    ~needs_legacy:needs_cache_from_x86_64_build_jobs
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~artifacts:(artifacts_test_results_xml Amd64)
    ~dune_cache:dune_cache_pull_from_pipeline
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-proto-unit"]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())

(* TODO: the changeset is silly since this tests non-Octez parts of the repository. *)
let job_oc_unit_other_x86_64 =
  CI.job
    "oc.unit:other-x86_64"
    ~__POS__
    ~description:"Run unit tests for some non-Octez parts of the repository."
    ~stage:Test
    ~retry:Gitlab_ci.Types.{max = 2; when_ = []}
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~image:Tezos_ci.Images.CI.build
    ~arch:Amd64
    ~cpu:High
    ~needs_legacy:needs_cache_from_x86_64_build_jobs
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~artifacts:(artifacts_test_results_xml Amd64)
    ~dune_cache:dune_cache_pull_from_pipeline
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-other-unit"]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())

let job_ocaml_check =
  CI.job
    "ocaml-check"
    ~__POS__
    ~description:"Typecheck all OCaml code with 'dune build @check'."
    ~cpu:Very_high
    ~image:Tezos_ci.Images.CI.build
    ~stage:Test
    ~only_if_changed:
      ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "**/*.ml"; "**/*.mli"]
    ~variables:[("CARGO_NET_OFFLINE", "false")]
    ~dune_cache:(Cacio.dune_cache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      (* Stops on first error for easier detection of problems in
         the log and to reduce time to merge of other MRs further
         down the merge train. *)
      "scripts/ci/dune.sh build @check --stop-on-first-error";
    ]

let register () =
  CI.register_merge_request_jobs
    [
      (Auto, job_check_lift_limits_patch);
      (Auto, job_python_check);
      (Auto, job_integration_compiler_rejections);
      (Auto, job_script_test_gen_genesis);
      (Auto, job_script_snapshot_alpha_and_link);
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_oc_script_test_release_versions);
      (Auto, job_test_release_versions);
      (Auto, job_resto_unit Amd64);
      (Auto, job_resto_unit Arm64);
      (Auto, job_de_unit Amd64);
      (Auto, job_de_unit Arm64);
      (Auto, job_oc_unit_protocol_compiles);
      (Auto, job_oc_unit_webassembly_x86_64);
      (Auto, job_oc_unit_non_proto_x86_64);
      (Auto, job_oc_unit_non_proto_arm64);
      (Auto, job_oc_unit_proto_x86_64);
      (Auto, job_oc_unit_other_x86_64);
      (Auto, job_ocaml_check);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_check_lift_limits_patch);
      (Auto, job_python_check);
      (Auto, job_integration_compiler_rejections);
      (Auto, job_script_test_gen_genesis);
      (Auto, job_script_snapshot_alpha_and_link);
      (Auto, job_script_b58_prefix);
      (Auto, job_test_liquidity_baking_scripts);
      (Auto, job_oc_script_test_release_versions);
      (Auto, job_test_release_versions);
      (Auto, job_resto_unit Amd64);
      (Auto, job_resto_unit Arm64);
      (Auto, job_de_unit Amd64);
      (Auto, job_de_unit Arm64);
      (Auto, job_oc_unit_protocol_compiles);
      (Auto, job_oc_unit_webassembly_x86_64);
      (Auto, job_oc_unit_non_proto_x86_64);
      (Auto, job_oc_unit_non_proto_arm64);
      (Auto, job_oc_unit_proto_x86_64);
      (Auto, job_oc_unit_other_x86_64);
      (Auto, job_ocaml_check);
    ] ;
  ()
