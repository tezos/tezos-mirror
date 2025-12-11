(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

module Files = struct
  let sdks = ["src/kernel_sdk/**/*"; "sdk/rust/**/*"]

  let rust_toolchain_image =
    [
      "images/rust-toolchain/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

  let lib_wasm_runtime_rust = ["src/lib_wasm_runtime/**/*.rs"]

  let node = ["etherlink/**/*"]

  let kernel = ["etherlink.mk"; "etherlink/**/*.rs"]

  let firehose =
    [
      "etherlink/firehose/**/*";
      "etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*";
    ]

  let evm_compatibility =
    [
      "etherlink.mk";
      "etherlink/kernel_latest/revm/**/*";
      "etherlink/kernel_latest/evm_evaluation/**/*";
    ]

  let revm_compatibility =
    [
      "etherlink.mk";
      "etherlink/kernel_latest/revm/**/*";
      "etherlink/kernel_latest/revm_evaluation/**/*";
    ]

  let mir =
    [
      "contrib/mir/**/*.rs";
      "contrib/mir/**/*.lalrpop";
      (* Cargo.toml, clippy.toml *)
      "contrib/mir/**/*.toml";
      "contrib/mir/**/Cargo.lock";
    ]

  let tzt = ["tzt_reference_test_suite/**/*"]

  (* [firehose], [evm_compatibility] and [revm_compatibility] are already included
     in [node @ kernel] *)
  let all =
    sdks @ rust_toolchain_image @ lib_wasm_runtime_rust @ node @ kernel @ mir
    @ tzt
end

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = Files.all
end)

type purpose = Release | Test

let octez_evm_node_release_tag_re =
  "/^octez-evm-node-v\\d+\\.\\d+(?:\\-rc\\d+)?$/"

(** Creates a Docker build job of the given [arch]. *)
let job_docker_build =
  Cacio.parameterize @@ fun arch ->
  Cacio.parameterize @@ fun test ->
  let arch_string = Runner.Arch.show_uniform arch in
  CI.job
    ("docker:" ^ arch_string)
    ~image_dependencies:[Images.CI.runtime]
    ~__POS__
    ~stage:Build
    ~image:Images_external.docker
    ~arch
    ?storage:(if arch = Arm64 then Some Ramfs else None)
    ~variables:
      [
        ("DOCKER_BUILD_TARGET", "without-evm-artifacts");
        ("DOCKER_VERSION", "24.0.7");
        ("CI_DOCKER_HUB", match test with `test -> "false" | `real -> "true");
        ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
        ("EXECUTABLE_FILES", "script-inputs/octez-evm-node-executable");
      ]
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~description:(sf "Build EVM node docker image for %s." arch_string)
    ["./scripts/ci/docker_initialize.sh"; "./scripts/ci/docker_release.sh"]

let job_build_evm_node_static =
  Cacio.parameterize @@ fun arch ->
  Cacio.parameterize @@ fun purpose ->
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  CI.job
    ("build_evm_node_static_" ^ arch_string)
    ~__POS__
    ~stage:(match purpose with Release -> Build | Test -> Test)
    ~description:"Build the Etherlink executables (statically linked)."
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ?storage:
      (match (purpose, arch) with
      | Release, _ | Test, Arm64 -> Some Ramfs
      | Test, Amd64 -> None)
    ~image:Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"octez-binaries"
         ?expire_in:
           (match purpose with
           | Release -> Some (Duration (Days 90))
           | Test -> None)
         ~when_:On_success
         ["octez-binaries/$ARCH/*"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~variables:
      ([("ARCH", arch_string); ("VERSION_EXECUTABLE", "octez-evm-node")]
      @
      match purpose with
      | Release ->
          [
            ("DUNE_BUILD_JOBS", "-j 12");
            ("EXECUTABLE_FILES", "script-inputs/octez-evm-node-executable");
          ]
      | Test ->
          [
            ( "EXECUTABLE_FILES",
              "script-inputs/etherlink-experimental-executables" );
          ])
    [
      "./scripts/ci/take_ownership.sh";
      "eval $(opam env)";
      "./scripts/ci/build_static_binaries.sh";
    ]

let job_lint_wasm_runtime =
  CI.job
    "lint_wasm_runtime"
    ~__POS__
    ~stage:Test
    ~description:"Run the linter on lib_wasm_runtime."
    ~image:Images.CI.build
    ~only_if_changed:Files.lib_wasm_runtime_rust
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "etherlink/lib_wasm_runtime/lint.sh";
    ]

let job_unit_tests =
  CI.job
    "unit_tests"
    ~__POS__
    ~stage:Test
    ~description:"Etherlink unit tests."
    ~image:Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"$CI_JOB_NAME-$CI_COMMIT_SHA-x86_64"
         ["test_results"]
         ~reports:(Gitlab_ci.Util.reports ~junit:"test_results/*.xml" ())
         ~expire_in:(Duration (Days 1))
         ~when_:Always)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~dune_cache:(Cacio.dune_cache ())
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~retry:{max = 2; when_ = []}
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-etherlink-unit"]

let job_test_kernel =
  CI.job
    "test_kernel"
    ~__POS__
    ~stage:Test
    ~description:"Check and test the etherlink kernel."
    ~image:Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ kernel @ sdks @ mir)
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -f etherlink.mk check"; "make -f etherlink.mk test"]

let job_test_firehose =
  CI.job
    "test_firehose"
    ~__POS__
    ~stage:Test
    ~description:"Check and test etherlink firehose."
    ~image:Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ firehose)
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -C etherlink/firehose check"]

let job_test_evm_compatibility =
  CI.job
    "test_evm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test EVM compatibility."
    ~image:Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ evm_compatibility)
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
       evm-evaluation-assessor";
      "git clone --depth 1 --branch v14.1@etherlink \
       https://github.com/functori/tests ethereum_tests";
      "./evm-evaluation-assessor --eth-tests ./ethereum_tests/ --resources \
       ./etherlink/kernel_latest/evm_evaluation/resources/ -c";
    ]

let job_test_revm_compatibility =
  CI.job
    "test_revm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test REVM compatibility."
    ~image:Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ revm_compatibility)
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
       revm-evaluation-assessor";
      "git clone --depth 1 https://github.com/functori/evm-fixtures \
       evm_fixtures";
      "./revm-evaluation-assessor --test-cases ./evm_fixtures/";
    ]

let job_mir_unit =
  CI.job
    "mir_unit"
    ~__POS__
    ~description:"Run unit tests for MIR."
    ~image:Images.CI.test
    ~stage:Test
    ~only_if_changed:Files.mir
    ~cargo_cache:true
    ["cargo test --manifest-path contrib/mir/Cargo.toml"]

let job_mir_tzt =
  CI.job
    "mir_tzt"
    ~__POS__
    ~description:"Run MIR's tzt_runner on the tzt reference test suite."
    ~image:Images.CI.test
    ~stage:Test
    ~only_if_changed:Files.(mir @ tzt)
    ~cargo_cache:true
    [
      "cargo run --manifest-path contrib/mir/Cargo.toml --bin tzt_runner \
       tzt_reference_test_suite/*.tzt";
    ]

let job_build_tezt =
  CI.job
    "build_tezt"
    ~__POS__
    ~stage:Build
    ~description:"Build the Etherlink Tezt executable."
    ~image:Images.CI.build
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"etherlink_tezt_exe"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         ["_build/default/etherlink/tezt/tests/main.exe"])
    ~dune_cache:(Cacio.dune_cache ~key:Pipeline ())
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "dune build etherlink/tezt/tests/main.exe";
    ]

(* Specialization of Cacio's [tezt_job] with defaults that are specific to this component. *)
(* Note: for now the changeset is the same as the one for regular Tezt jobs,
   but to follow the vision for the monorepo, it should be limited to Etherlink.
   This is something that can be done later, once we feel ready. *)
let tezt_job ?(retry_tests = 1) =
  CI.tezt_job
    ~tezt_exe:"etherlink/tezt/tests/main.exe"
    ~only_if_changed:(Changeset.encode Tezos_ci_jobs.Changesets.changeset_octez)
    ~needs:
      [
        (Artifacts, job_build_tezt);
        (Artifacts, Tezos_ci_jobs.Kernels.job_build_kernels);
      ]
    ~needs_legacy:
      [
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_x86_64_release
            Before_merging );
        ( Artifacts,
          Tezos_ci_jobs.Code_verification.job_build_x86_64_exp Before_merging );
      ]
    ~retry_tests

let job_tezt =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    ""
    ~__POS__
    ~pipeline
    ~description:"Run normal Etherlink Tezt tests."
    ~test_selection:
      (Tezos_ci_jobs.Tezt.tests_tag_selector [Not (Has_tag "flaky")])
    ~parallel_jobs:18
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_slow =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "slow"
    ~__POS__
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as slow."
    ~test_selection:(Tezos_ci_jobs.Tezt.tests_tag_selector ~slow:true [])
    ~test_timeout:No_timeout
    ~parallel_jobs:3
    ~parallel_tests:3
    ~retry_jobs:2

let job_tezt_extra =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "extra"
    ~__POS__
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as extra and not flaky."
    ~test_selection:
      (Tezos_ci_jobs.Tezt.tests_tag_selector
         ~extra:true
         [Not (Has_tag "flaky")])
    ~parallel_jobs:12
    ~parallel_tests:6
    ~retry_jobs:2

let job_tezt_flaky =
  Cacio.parameterize @@ fun pipeline ->
  tezt_job
    "flaky"
    ~__POS__
    ~pipeline
    ~description:"Run Etherlink Tezt tests tagged as flaky."
    ~allow_failure:Yes
    ~test_selection:(Tezos_ci_jobs.Tezt.tests_tag_selector [Has_tag "flaky"])
    ~parallel_jobs:2
    ~retry_jobs:2
    ~retry_tests:3

let job_gitlab_release =
  CI.job
    ~__POS__
    "gitlab:octez-evm-node-release"
    ~image:Images.ci_release
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_evm_node_static Amd64 Release);
        (Artifacts, job_build_evm_node_static Arm64 Release);
      ]
    ~description:"Create a GitLab release for Etherlink."
    ["./scripts/ci/create_gitlab_octez_evm_node_release.sh"]

let job_docker_merge =
  Cacio.parameterize @@ fun test ->
  CI.job
    "docker:merge_manifests"
    ~__POS__
    ~stage:Publish
    ~image:Images_external.docker
      (* This job merges the images produced in the jobs
         [docker:{amd64,arm64}] into a single multi-architecture image, and
         so must be run after these jobs. *)
    ~needs:
      [(Job, job_docker_build Amd64 test); (Job, job_docker_build Arm64 test)]
    ~variables:
      [
        ("DOCKER_VERSION", "24.0.7");
        ("CI_DOCKER_HUB", match test with `real -> "true" | `test -> "false");
      ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~description:"Merge manifest for arm64 and arm64 docker images."
    [
      "./scripts/ci/docker_initialize.sh";
      "./scripts/ci/docker_merge_manifests.sh";
    ]

let job_docker_promote_to_latest =
  Cacio.parameterize @@ fun test ->
  CI.job
    "docker:promote_to_latest"
    ~__POS__
    ~needs:[(Job, job_docker_merge test)]
    ~stage:Publish
    ~image:Images_external.docker
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~variables:
      [
        ("DOCKER_VERSION", "24.0.7");
        ("CI_DOCKER_HUB", match test with `real -> "true" | `test -> "false");
      ]
    ~description:"Promote the docker images to octez-evm-node-latest."
    [
      "./scripts/ci/docker_initialize.sh";
      "./scripts/ci/docker_promote_to_latest.sh octez-evm-node-latest \
       ./scripts/ci/octez-evm-node-release.sh";
    ]

let job_republish_docker_image =
  Cacio.parameterize @@ fun arch ->
  Cacio.parameterize @@ fun test ->
  CI.job
    ("republish-docker-image:" ^ Runner.Arch.(show_easy_to_distinguish arch))
    ~__POS__
    ~description:
      ("Republish the latest released docker image for arch "
      ^ Runner.Arch.(show_easy_to_distinguish arch))
    ~arch
    ~stage:Build
    ~image:Tezos_ci.Images_external.docker
    ~variables:
      [
        ("DOCKER_VERSION", "24.0.7");
        ("CI_DOCKER_HUB", match test with `test -> "false" | `real -> "true");
      ]
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    [
      "./scripts/ci/docker_initialize.sh";
      "VERSION=$(etherlink/scripts/get_latest_release_version.sh)";
      "BUILT_IMAGE=$(etherlink/scripts/build_docker_release.sh "
      ^ Runner.Arch.(show_easy_to_distinguish arch)
      ^ " $VERSION)";
      "export CI_COMMIT_TAG=\"octez-evm-node-v$VERSION\"";
      (* TODO comment *)
      "git tag octez-evm-node-v$VERSION";
      "./scripts/ci/docker_promote_to_latest.sh octez-evm-node-latest \
       ./scripts/ci/octez-evm-node-release.sh";
    ]

let register () =
  let open Runner.Arch in
  CI.register_before_merging_jobs
    [
      (Manual, job_build_evm_node_static Amd64 Test);
      (Manual, job_build_evm_node_static Arm64 Test);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (* We rely on the fact that [Tezos_ci_pipelines.Code_verification.job_build_kernels]
         returns an equivalent job for [Before_merging] and [Merge_train]. *)
      (Auto, job_test_kernel);
      (Auto, job_test_firehose);
      (Auto, job_test_evm_compatibility);
      (Auto, job_test_revm_compatibility);
      (Auto, job_mir_unit);
      (Auto, job_mir_tzt);
      (Auto, job_tezt `merge_request);
      (Manual, job_tezt_slow `merge_request);
      (Manual, job_tezt_extra `merge_request);
      (Manual, job_tezt_flaky `merge_request);
    ] ;
  CI.register_dedicated_release_pipeline
    ~tag_rex:octez_evm_node_release_tag_re
    [(Auto, job_docker_promote_to_latest `real); (Auto, job_gitlab_release)] ;
  CI.register_dedicated_test_release_pipeline
    ~tag_rex:octez_evm_node_release_tag_re
    [(Auto, job_docker_promote_to_latest `test); (Auto, job_gitlab_release)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    ~legacy_jobs:
      [
        Tezos_ci_jobs.Code_verification.job_build_x86_64_release
          Schedule_extended_test;
        Tezos_ci_jobs.Code_verification.job_build_x86_64_exp
          Schedule_extended_test;
      ]
    [
      (Auto, job_build_evm_node_static Amd64 Test);
      (Auto, job_build_evm_node_static Arm64 Test);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (Auto, job_test_kernel);
      (Auto, job_test_firehose);
      (Auto, job_test_evm_compatibility);
      (Auto, job_test_revm_compatibility);
      (Auto, job_tezt `scheduled);
      (Auto, job_tezt_slow `scheduled);
      (Auto, job_tezt_extra `scheduled);
      (Auto, job_tezt_flaky `scheduled);
      (Auto, job_mir_unit);
      (Auto, job_mir_tzt);
    ] ;
  CI.register_scheduled_pipeline
    "rebuild-released-docker-images"
    ~description:
      "Pipeline to rebuild and republish the latest released Docker images."
    [
      (Manual, job_republish_docker_image Amd64 `test);
      (Manual, job_republish_docker_image Arm64 `test);
    ] ;
  ()
