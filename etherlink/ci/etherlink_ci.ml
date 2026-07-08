(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

module Files = struct
  let sdks = ["src/kernel_sdk/**/*"; "sdk/rust/**/*"]

  let lib_wasm_runtime_rust = ["src/lib_wasm_runtime/**/*.rs"]

  let node = ["etherlink/**/*"]

  let kernel = ["etherlink.mk"; "etherlink/**/*.rs"]

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
      "etherlink/kernel_latest/mir/**/*.rs";
      "etherlink/kernel_latest/mir/**/*.lalrpop";
      (* Cargo.toml, clippy.toml *)
      "etherlink/kernel_latest/mir/**/*.toml";
      "etherlink/kernel_latest/mir/**/Cargo.lock";
    ]

  let tzt = ["tzt_reference_test_suite/**/*"]

  (* [evm_compatibility] and [revm_compatibility] are already included
     in [node @ kernel] *)
  let all = sdks @ lib_wasm_runtime_rust @ node @ kernel @ mir @ tzt
end

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = Files.all
end)

type purpose = Release | Test

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
    ~image:Images.Base_images.alpine_docker_ci
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ?storage:(if arch = Arm64 then Some Ramfs else None)
    ~variables:
      [
        ("DOCKER_BUILD_TARGET", "without-evm-artifacts");
        ("DOCKER_VERSION", Images.Base_images.docker_version);
        ("CI_DOCKER_HUB", match test with `test -> "false" | `real -> "true");
        ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
        ("EXECUTABLE_FILES", "script-inputs/octez-evm-node-executable");
      ]
      (* Docker Hub credentials (CI_DOCKER_AUTH) are scoped to the
       [docker-publish] environment; only [`real] jobs (CI_DOCKER_HUB=true)
       authenticate and thus need access. *)
    ?environment:
      (match test with
      | `real ->
          Some Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
      | `test -> None)
    ~services:[{name = Images.Base_images.dind_service}]
    ~description:(sf "Build EVM node docker image for %s." arch_string)
    ~script:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        "./scripts/ci/docker_release.sh";
      ]

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
    ~script:
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
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
        "etherlink/lib_wasm_runtime/lint.sh";
      ]

let job_lint_solidity_artifacts =
  CI.job
    "lint_solidity_artifacts"
    ~__POS__
    ~stage:Test
    ~description:"Check committed bytecode are up to date."
    ~image:Images.CI.e2etest
    ~only_if_changed:
      [
        "etherlink/kernel_latest/revm/contracts/predeployed/*.sol";
        "etherlink/kernel_latest/revm/contracts/predeployed/*.bin";
      ]
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "make -C etherlink/kernel_latest/revm/contracts/predeployed bytecode";
        "forge --version";
        "git status";
        "git diff-index --quiet HEAD --";
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
    ~dune_cache:true
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~retry:{max = 2; when_ = []}
    ~script:
      [". ./scripts/version.sh"; "eval $(opam env)"; "make test-etherlink-unit"]

let job_test_kernel =
  CI.job
    "test_kernel"
    ~__POS__
    ~stage:Test
    ~description:"Check and test the etherlink kernel."
    ~image:Images.Base_images.debian_rust_trixie
    ~only_if_changed:Files.(kernel @ sdks @ mir)
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:["make -f etherlink.mk check"; "make -f etherlink.mk test"]

let job_test_evm_compatibility =
  CI.job
    "test_evm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test EVM compatibility."
    ~image:Images.Base_images.debian_rust_trixie
    ~only_if_changed:Files.evm_compatibility
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:
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
    ~image:Images.Base_images.debian_rust_trixie
    ~only_if_changed:Files.revm_compatibility
    ~needs:[(Job, Tezos_ci_jobs.Kernels.job_build_kernels)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:
      [
        "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
         revm-evaluation-assessor";
        "git clone --depth 1 https://github.com/functori/evm-fixtures \
         evm_fixtures";
        "./revm-evaluation-assessor --test-cases ./evm_fixtures/";
      ]

let job_mir_tzt =
  CI.job
    "mir_tzt"
    ~__POS__
    ~description:"Run MIR's tzt_runner on the tzt reference test suite."
    ~image:Images.CI.test
    ~stage:Test
    ~only_if_changed:Files.(mir @ tzt)
    ~cargo_cache:true
    ~script:
      [
        "cargo run --manifest-path etherlink/kernel_latest/mir/Cargo.toml \
         --bin tzt_runner tzt_reference_test_suite/*.tzt";
      ]

(* Tickets are enabled in MIR's default feature set but disabled in the shipped
   Tezos X / etherlink kernel, which depends on MIR with [default-features =
   false] (see L2-1680). The default MIR builds (e.g. [job_mir_tzt]) therefore
   exercise the ticket implementation, but nothing otherwise checks the
   tickets-disabled configuration the kernel actually ships. This job checks and
   tests MIR with that configuration so the disabled path (rejection at
   typechecking) stays clean and tested. *)
let job_mir_no_tickets =
  CI.job
    "mir_no_tickets"
    ~__POS__
    ~stage:Test
    ~description:
      "Check and test MIR with tickets disabled, as shipped in the kernel \
       (L2-1680)."
    ~image:Images.Base_images.debian_rust_trixie
    ~only_if_changed:Files.mir
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:
      [
        "cargo clippy --manifest-path etherlink/kernel_latest/mir/Cargo.toml \
         --no-default-features --features bls,allow_lazy_storage_transfer \
         --all-targets -- --deny warnings";
        "RUST_MIN_STACK=104857600 RUST_TEST_THREADS=1 cargo test \
         --manifest-path etherlink/kernel_latest/mir/Cargo.toml \
         --no-default-features --features bls,allow_lazy_storage_transfer";
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
    ~dune_cache:true
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
        "scripts/ci/dune.sh build etherlink/tezt/tests/main.exe";
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
        (Artifacts, Tezos_ci_jobs.Build.job_build_released Amd64);
        (Artifacts, Tezos_ci_jobs.Build.job_build_exp Amd64);
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
    ~parallel_jobs:36
    ~parallel_tests:3
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
  Cacio.parameterize @@ fun mode ->
  CI.job
    ~__POS__
    "gitlab:octez-evm-node-release"
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_evm_node_static Amd64 Release);
        (Artifacts, job_build_evm_node_static Arm64 Release);
      ]
    ?variables:
      (match mode with
      (* Scheduled test release pipelines are not triggered by a tag, so there
         is no [CI_COMMIT_TAG]. Fake one so that the release scripts can compute
         a version, just like the Octez scheduled test release does. *)
      | `scheduled_test -> Some [("CI_COMMIT_TAG", "octez-evm-node-v0.0")]
      | `real -> None)
    ~description:"Create a GitLab release for Etherlink."
    ~script:
      ((match mode with
       | `scheduled_test -> ["git tag octez-evm-node-v0.0"]
       | `real -> [])
      @ [
          ("./scripts/ci/create_gitlab_octez_evm_node_release.sh"
          ^ match mode with `scheduled_test -> " --dry-run" | `real -> "");
        ])

let job_docker_merge =
  Cacio.parameterize @@ fun test ->
  CI.job
    "docker:merge_manifests"
    ~__POS__
    ~stage:Publish
    ~image:Images.Base_images.alpine_docker_ci
      (* This job merges the images produced in the jobs
         [docker:{amd64,arm64}] into a single multi-architecture image, and
         so must be run after these jobs. *)
    ~needs:
      [(Job, job_docker_build Amd64 test); (Job, job_docker_build Arm64 test)]
    ~variables:
      [
        ("DOCKER_VERSION", Images.Base_images.docker_version);
        ("CI_DOCKER_HUB", match test with `real -> "true" | `test -> "false");
      ]
      (* Docker Hub credentials (CI_DOCKER_AUTH) are scoped to the
       [docker-publish] environment; only [`real] jobs (CI_DOCKER_HUB=true)
       authenticate and thus need access. *)
    ?environment:
      (match test with
      | `real ->
          Some Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
      | `test -> None)
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~services:[{name = Images.Base_images.dind_service}]
    ~description:"Merge manifest for arm64 and arm64 docker images."
    ~script:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        "./scripts/ci/docker_merge_manifests.sh";
      ]

let job_docker_promote_to_latest =
  Cacio.parameterize @@ fun test ->
  CI.job
    "docker:promote_to_latest"
    ~__POS__
    ~needs:[(Job, job_docker_merge test)]
    ~stage:Publish
    ~image:Images.Base_images.alpine_docker_ci
    ~services:[{name = Images.Base_images.dind_service}]
    ~variables:
      [
        ("DOCKER_VERSION", Images.Base_images.docker_version);
        ("CI_DOCKER_HUB", match test with `real -> "true" | `test -> "false");
      ]
      (* Docker Hub credentials (CI_DOCKER_AUTH) are scoped to the
       [docker-publish] environment; only [`real] jobs (CI_DOCKER_HUB=true)
       authenticate and thus need access. *)
    ?environment:
      (match test with
      | `real ->
          Some Gitlab_ci.Types.{name = "docker-publish"; action = Some Access}
      | `test -> None)
    ~description:"Promote the docker images to octez-evm-node-latest."
    ~script:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        "./scripts/ci/docker_promote_to_latest.sh octez-evm-node-latest \
         ./scripts/ci/octez-evm-node-release.sh";
      ]

let register () =
  let open Runner.Arch in
  Cacio.register_merge_request_jobs
    [
      (* In theory there should be no manual jobs in merge trains.
         But we're not sure if the build jobs are still used. *)
      (Manual, job_build_evm_node_static Amd64 Test);
      (Manual, job_build_evm_node_static Arm64 Test);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_lint_solidity_artifacts);
      (Auto, job_unit_tests);
      (* We rely on the fact that [Tezos_ci_pipelines.Code_verification.job_build_kernels]
         returns an equivalent job for [Before_merging] and [Merge_train]. *)
      (Auto, job_test_kernel);
      (Auto, job_test_evm_compatibility);
      (Auto, job_test_revm_compatibility);
      (Auto, job_mir_tzt);
      (Auto, job_mir_no_tickets);
      (Auto, job_tezt `merge_request);
    ] ;
  Cacio.register_jobs
    Before_merging
    [
      (Manual, job_tezt_slow `merge_request);
      (Manual, job_tezt_extra `merge_request);
      (Manual, job_tezt_flaky `merge_request);
    ] ;
  let octez_evm_node_release_tag_re = "/^octez-evm-node-v\\d+\\.\\d+$/" in
  let octez_evm_node_prerelease_tag_re =
    "/^octez-evm-node-v\\d+\\.\\d+-(rc|pre)\\d+$/"
  in
  CI.register_dedicated_release_pipeline
    ~tag_rex:octez_evm_node_release_tag_re
    [
      (Auto, job_docker_promote_to_latest `real);
      (Auto, job_gitlab_release `real);
    ] ;
  CI.register_dedicated_test_release_pipeline
    ~tag_rex:octez_evm_node_release_tag_re
    [
      (Auto, job_docker_promote_to_latest `test);
      (Auto, job_gitlab_release `real);
    ] ;
  CI.register_dedicated_prerelease_pipeline
    ~tag_rex:octez_evm_node_prerelease_tag_re
    [(Auto, job_docker_merge `real); (Auto, job_gitlab_release `real)] ;
  CI.register_dedicated_test_prerelease_pipeline
    ~tag_rex:octez_evm_node_prerelease_tag_re
    [(Auto, job_docker_merge `test); (Auto, job_gitlab_release `real)] ;
  (* Test release pipeline that does not require pushing a tag: it is triggered
     as a scheduled pipeline (see [ci/run_pipeline/schedule_test_release_evm_node.sh]).
     Its purpose is to test changes to the release pipeline (e.g. the 'ci-release'
     image) without publishing anything, and it must be safe even when run on
     tezos/tezos (this pipeline is not restricted to forks). Therefore:
     - it uses [job_docker_merge] rather than [job_docker_promote_to_latest], so
       the 'octez-evm-node-latest' tag is never moved (the built images are only
       pushed to the GCP registry under the branch name, in [`test] mode which
       also disables any Docker Hub push);
     - the GitLab release job runs in [`scheduled_test] mode, which fakes the
       release tag and passes --dry-run, so no package is uploaded and no GitLab
       release is created. *)
  CI.register_scheduled_pipeline
    "scheduled_test_release"
    ~description:
      "Test release pipeline for the EVM node, triggered as a scheduled \
       pipeline instead of by pushing a tag. Does not publish anything."
    [(Auto, job_docker_merge `test); (Auto, job_gitlab_release `scheduled_test)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    [
      (Auto, job_build_evm_node_static Amd64 Test);
      (Auto, job_build_evm_node_static Arm64 Test);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_lint_solidity_artifacts);
      (Auto, job_unit_tests);
      (Auto, job_test_kernel);
      (Auto, job_test_evm_compatibility);
      (Auto, job_test_revm_compatibility);
      (Auto, job_tezt `scheduled);
      (Auto, job_tezt_slow `scheduled);
      (Auto, job_tezt_extra `scheduled);
      (Auto, job_tezt_flaky `scheduled);
      (Auto, job_mir_tzt);
      (Auto, job_mir_no_tickets);
    ] ;
  ()
