(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = ["etherlink/**/*"; "src/kernel_sdk/**/*"; "sdk/rust/**/*"]
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
    ~only_if_changed:["src/lib_wasm_runtime/**/*.rs"]
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

let register () =
  CI.register_before_merging_jobs
    [
      (Manual, job_build_evm_node_static Amd64);
      (Manual, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    [
      (Auto, job_build_evm_node_static Amd64);
      (Auto, job_build_evm_node_static Arm64);
      (Auto, job_lint_wasm_runtime);
      (Auto, job_unit_tests);
    ] ;
  ()
