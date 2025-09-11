(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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
      "etherlink/kernel_latest/evm_execution/**/*";
      "etherlink/kernel_latest/evm_evaluation/**/*";
    ]

  (* [firehose] and [evm_compatibility] are already included in [node @ kernel] *)
  let all = sdks @ rust_toolchain_image @ lib_wasm_runtime_rust @ node @ kernel
end

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = Files.all
end)

let job_build_evm_node_static =
  Cacio.parameterize @@ fun arch ->
  CI.job
    ("build_evm_node_static_"
    ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ~__POS__
    ~stage:Test
    ~description:"Build the EVM node (statically linked)."
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ?storage:(match arch with Arm64 -> Some Ramfs | Amd64 -> None)
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"evm-binaries"
         ~when_:On_success
         ["octez-evm-*"; "etherlink-*"])
    ~cargo_cache:true
    ~cargo_target_caches:true
    ~sccache:(Cacio.sccache ~cache_size:"2G" ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make evm-node-static";
    ]

let job_lint_wasm_runtime =
  CI.job
    "lint_wasm_runtime"
    ~__POS__
    ~stage:Test
    ~description:"Run the linter on lib_wasm_runtime."
    ~image:Tezos_ci.Images.CI.build
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
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:Files.(node @ sdks)
    ~artifacts:
      ((* Note: the [~name] is actually overridden by the one computed
           by [Tezos_ci.Coverage.enable_output_artifact].
           We set it anyway for consistency with how the job
           was previously declared using [job_unit_test] in [code_verification.ml]. *)
       Gitlab_ci.Util.artifacts
         ~name:"$CI_JOB_NAME-$CI_COMMIT_SHA-x86_64"
         ["test_results"]
         ~reports:(Gitlab_ci.Util.reports ~junit:"test_results/*.xml" ())
         ~expire_in:(Duration (Days 1))
         ~when_:Always)
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~dune_cache:(Cacio.dune_cache ())
    ~test_coverage:true
    ~variables:[("DUNE_ARGS", "-j 12")]
    ~retry:{max = 2; when_ = []}
    [". ./scripts/version.sh"; "eval $(opam env)"; "make test-etherlink-unit"]

let job_test_kernel =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_kernel"
    ~__POS__
    ~stage:Test
    ~description:"Check and test the etherlink kernel."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ kernel @ sdks)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -f etherlink.mk check"; "make -f etherlink.mk test"]

let job_test_firehose =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_firehose"
    ~__POS__
    ~stage:Test
    ~description:"Check and test etherlink firehose."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ firehose)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ["make -C etherlink/firehose check"]

let job_test_evm_compatibility =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "test_evm_compatibility"
    ~__POS__
    ~stage:Test
    ~description:"Check and test EVM compatibility."
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:Files.(rust_toolchain_image @ evm_compatibility)
    ~needs_legacy:
      [(Job, Tezos_ci_jobs.Code_verification.job_build_kernels pipeline_type)]
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

let register () =
  CI.register_before_merging_jobs
    [
      (Manual, job_build_evm_node_static Amd64);
      (Manual, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (* We rely on the fact that [Tezos_ci_pipelines.Code_verification.job_build_kernels]
         returns an equivalent job for [Before_merging] and [Merge_train]. *)
      (Auto, job_test_kernel Before_merging);
      (Auto, job_test_firehose Before_merging);
      (Auto, job_test_evm_compatibility Before_merging);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    ~legacy_jobs:
      [Tezos_ci_jobs.Code_verification.job_build_kernels Schedule_extended_test]
    [
      (Auto, job_build_evm_node_static Amd64);
      (Auto, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
      (Auto, job_test_kernel Schedule_extended_test);
      (Auto, job_test_firehose Schedule_extended_test);
      (Auto, job_test_evm_compatibility Before_merging);
    ] ;
  ()
