(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* This module contains the definition of stages and Docker
   images used by the Octez CI pipelines.

   It also defines:
    - helpers for defining jobs;
    - changesets shared by jobs;
    - helpers for making jobs;
    - jobs shared between pipelines *)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache

(* types for the repositories pipelines.
   - Release: we run all the release jobs, but no tests
   - Partial: we run only a subset of the tests jobs
   - Full: we run the complete test matrix
*)
type repository_pipeline = Full | Partial | Release

let retry_default_config =
  Gitlab_ci.Types.
    {
      image = None;
      interruptible = Some true;
      retry = Some {max = 2; when_ = [Script_failure; Runner_system_failure]};
    }

let cargo_home =
  (* Note:
     - We want [CARGO_HOME] to be in a sub-folder of
       {!ci_project_dir} to enable GitLab CI caching.
     - We want [CARGO_HOME] to be hidden from dune
       (thus the dot-prefix). *)
  Gitlab_ci.Predefined_vars.(show ci_project_dir) // ".cargo"

(** The default [before_script:] section.

    In general, the result of this script should be used as the
    default value for [~before_merging] for all jobs. Each boolean flag
    of this function enables a specific functionality before the job's
    [script:] runs. In detail:

    - [take_ownership]: all files in the working directory of the
      job are [chown]'d by the job's user. This requires that either
      sudo is installed in the job's image or that the job's user has
      sufficient privileges. (default: [false])
    - [source_version]: the script [scripts/version.sh] is sourced. (default: [false])
    - [eval_opam]: runs [eval $(opam env)], activating any opam switch
      if present in the image. (default: [false])
    - [init_python_venv]: runs [.venv/bin/activate], activating any
      python vinv if present in the image. (default: [false])
    - [install_js_deps]: runs, and sources,
      [./scripts/install_build_deps.js.sh] installing JavaScript
      dependencies and [node], [nvm] and [npm] available in the
      environment. (default: [false])

   The unnamed argument of the function is appended to the end of the
   [before_script:] section, after any of the additions caused by the
   optional arguments. *)
let before_script ?(take_ownership = false) ?(source_version = false)
    ?(eval_opam = false) ?(init_python_venv = false) ?(install_js_deps = false)
    before_script =
  let toggle t x = if t then [x] else [] in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2865 *)
  toggle take_ownership "./scripts/ci/take_ownership.sh"
  @ toggle source_version ". ./scripts/version.sh"
    (* TODO: this must run in the before_script of all jobs that use the opam environment.
       how to enforce? *)
  @ toggle eval_opam "eval $(opam env)"
  (* Load the environment poetry previously created in the docker image.
     Give access to the Python dependencies/executables *)
  @ toggle init_python_venv ". $HOME/.venv/bin/activate"
  @ toggle install_js_deps ". ./scripts/install_build_deps.js.sh"
  @ before_script

let opt_var name f = function Some value -> [(name, f value)] | None -> []

(** {2 Child repositories pipelines} *)

(** Return a tuple (ARCHITECTURES, <archs>) based on the type
    of repository pipeline. *)
let archs_variables pipeline =
  let archs : Runner.Arch.t list =
    match pipeline with Partial -> [Amd64] | Full | Release -> [Amd64; Arm64]
  in
  [
    ( "ARCHITECTURES",
      String.concat " " (List.map Runner.Arch.show_uniform archs) );
  ]

let make_job_build_packages ~__POS__ ~name ~matrix ~script ~dependencies
    ~variables ~image =
  job
    ~__POS__
    ~name
    ~image
    ~stage:Stages.build
    ~variables
    ~parallel:(Matrix matrix)
    ~dependencies
    ~tag:Dynamic
    ~retry:Gitlab_ci.Types.{max = 1; when_ = [Stuck_or_timeout_failure]}
    ~artifacts:(artifacts ["packages/$DISTRIBUTION/$RELEASE"])
    [
      (* This is a hack to enable Cargo networking for jobs in child pipelines.

         Global variables of the parent pipeline
         are passed to the child pipeline. Inside the child
         pipeline, variables received from the parent pipeline take
         precedence over job-level variables. It's bit strange. So
         to override the default [CARGO_NET_OFFLINE=true], we cannot
         just set it in the job-level variables of this job.

         [enable_sccache] adds the cache directive for [$CI_PROJECT_DIR/_sccache].

         See
         {{:https://docs.gitlab.com/ee/ci/variables/index.html#cicd-variable-precedence}here}
         for more info. *)
      "export CARGO_NET_OFFLINE=false";
      script;
    ]
  |> enable_sccache ~idle_timeout:"0"

(* This version of the job builds both released and experimental executables.
   It is used in the following pipelines:
   - Before merging: check whether static executables still compile,
     i.e. that we do pass the -static flag and that when we do it does compile
   - Master branch: executables (including experimental ones) are used in some test networks
   Variants:
   - an arm64 variant exist, but is only used in the master branch pipeline
     (no need to test that we pass the -static flag twice)
   - released variants exist, that are used in release tag pipelines
     (they do not build experimental executables) *)
let job_build_static_binaries ~__POS__ ~arch ?(cpu = Runner.CPU.Normal) ?storage
    ?(executable_files = "script-inputs/octez-released-executables")
    ?(experimental_executables = "script-inputs/octez-experimental-executables")
    ?version_executable ?(release = false) ?rules ?dependencies ?retry () :
    tezos_job =
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  let name = "oc.build:static-" ^ arch_string ^ "-linux-binaries" in
  let artifacts =
    (* Extend the lifespan to prevent failure for external tools using artifacts. *)
    let expire_in = if release then Some (Duration (Days 90)) else None in
    artifacts ?expire_in ["octez-binaries/$ARCH/*"]
  in
  let executable_files =
    executable_files
    ^ if not release then " " ^ experimental_executables else ""
  in
  let version_executable =
    match version_executable with
    | Some exe -> [("VERSION_EXECUTABLE", exe)]
    | None -> []
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage:Stages.build
    ~arch
    ~cpu
    ?storage
    ~name
    ?retry
    ~image:Images.CI.build
    ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
    ~variables:
      ([
         ("ARCH", arch_string);
         ("EXECUTABLE_FILES", executable_files);
         ("DUNE_BUILD_JOBS", "-j 12");
       ]
      @ version_executable)
    ~artifacts
    ["./scripts/ci/build_static_binaries.sh"]
  |> enable_cargo_cache |> enable_sccache |> enable_cargo_target_caches

(** Type of Docker build jobs.

    The semantics of the type is summed up in this table:

    |                       | Release    | Experimental | Test   | Test_manual |
    |-----------------------+------------+--------------+--------+-------------|
    | Image registry        | Docker hub | Docker hub   | GitLab | GitLab      |
    | Experimental binaries | no         | yes          | yes    | yes         |
    | EVM Kernels           | no         | On amd64     | no     | On amd64    |
    | Manual job            | no         | no           | no     | yes         |

    - [Release] Docker builds include only released executables whereas other
      types also includes experimental ones.
    - [Octez_evm_node_release] Docker build include only `octez-evm-node`
    - [Test_manual] and [Experimental] Docker builds include the EVM kernels in
      amd64 builds.
    - [Release] and [Experimental] Docker builds are pushed to Docker hub,
      whereas other types are pushed to the GitLab registry.
    - [Test_manual] Docker builds are started manually, put in the stage
      [manual] and their failure is allowed. The other types are in the build
      stage, run [on_success] and are not allowed to fail. *)
type docker_build_type =
  | Experimental
  | Release
  | Octez_evm_node_release
  | Test
  | Test_manual

(** Creates a Docker build job of the given [arch] and [docker_build_type]. *)
let job_docker_build ?rules ?dependencies ~__POS__ ~arch ?storage
    docker_build_type : tezos_job =
  let arch_string = Runner.Arch.show_uniform arch in
  let ci_docker_hub =
    match docker_build_type with
    | Release | Octez_evm_node_release | Experimental -> true
    | Test | Test_manual -> false
  in
  (* Whether to include evm artifacts.
     Including these artifacts requires the rust-toolchain image. *)
  let with_evm_artifacts =
    match (arch, docker_build_type) with
    | Amd64, (Test_manual | Experimental) -> true
    | _ -> false
  in
  let image_dependencies =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7293

       In reality, we actually require both
       {!Images.CI.runtime} and
       {!Images.CI.build}. But these two images are
       created by the same job, and depending on them both will create
       a duplicated dependency on that single job, which GitLab CI
       does not allow. This should be somehow handled by CIAO. *)
    [Images.CI.runtime]
    @ if with_evm_artifacts then [Images.rust_toolchain] else []
  in
  let variables =
    [
      ( "DOCKER_BUILD_TARGET",
        if with_evm_artifacts then "with-evm-artifacts"
        else "without-evm-artifacts" );
      ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
      ( "EXECUTABLE_FILES",
        match docker_build_type with
        | Release -> "script-inputs/released-executables"
        | Octez_evm_node_release -> "script-inputs/octez-evm-node-executable"
        | Test | Test_manual | Experimental ->
            "script-inputs/released-executables \
             script-inputs/experimental-executables" );
    ]
  in
  let stage =
    match docker_build_type with
    | Test_manual -> Stages.manual
    | _ -> Stages.build
  in
  let name = "oc.docker:" ^ arch_string in
  job_docker_authenticated
    ?rules
    ?dependencies
    ~image_dependencies
    ~ci_docker_hub
    ~__POS__
    ~stage
    ~arch
    ?storage
    ~name
    ~variables
    ["./scripts/ci/docker_release.sh"]

let job_docker_merge_manifests ~__POS__ ~ci_docker_hub ~job_docker_amd64
    ~job_docker_arm64 : tezos_job =
  job_docker_authenticated
    ~__POS__
    ~stage:Stages.publish
    ~name:"docker:merge_manifests"
    ~tag:Gcp_not_interruptible
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      (* This job merges the images produced in the jobs
         [docker:{amd64,arm64}] into a single multi-architecture image, and
         so must be run after these jobs. *)
    ~dependencies:(Dependent [Job job_docker_amd64; Job job_docker_arm64])
    ~ci_docker_hub
    ["./scripts/ci/docker_merge_manifests.sh"]

let job_docker_promote_to_latest ?dependencies ~ci_docker_hub () : tezos_job =
  job_docker_authenticated
    ~__POS__
    ?dependencies
    ~stage:Stages.publish
    ~name:"docker:promote_to_latest"
    ~ci_docker_hub
    ["./scripts/ci/docker_promote_to_latest.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~tag:Gcp_not_interruptible

type bin_package_target = Rpm

type bin_package_group = A | B

let bin_package_image = Image.mk_external ~image_path:"$DISTRIBUTION"

let job_build_released_binaries ?rules ~__POS__ ~arch ?retry ?cpu ?storage
    ?dependencies () =
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  let name = sf "oc.build_%s-released" arch_string in
  let executable_files = "script-inputs/released-executables" in
  let variables =
    [("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
  in
  let artifacts =
    artifacts
      ~name:"build-$ARCH-$CI_COMMIT_REF_SLUG"
      ~when_:On_success
      ~expire_in:(Duration (Days 1))
      ["octez-*"; "src/proto_*/parameters/*.json"]
  in
  let job =
    job
      ?rules
      ?dependencies
      ~__POS__
      ~stage:Stages.build
      ~arch
      ?retry
      ?cpu
      ?storage
      ~name
      ~image:Images.CI.build
      ~before_script:
        (before_script
           ~take_ownership:true
           ~source_version:true
           ~eval_opam:true
           [])
      ~variables
      ~artifacts
      ["./scripts/ci/build_full_unreleased.sh"]
    |> enable_cargo_cache |> enable_sccache |> enable_cargo_target_caches
  in
  (* Disable coverage for arm64 *)
  if arch = Amd64 then Coverage.enable_instrumentation job else job

let job_build_dynamic_binaries ?rules ~__POS__ ~arch ?retry ?cpu ?storage
    ?dependencies ~name executable_files =
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  let build_extra =
    match arch with
    | Amd64 ->
        [
          "src/bin_tps_evaluation/main_tps_evaluation.exe";
          "src/bin_octogram/octogram_main.exe";
          "tezt/tests/main.exe";
          "contrib/octez_injector_server/octez_injector_server.exe";
        ]
    | Arm64 ->
        [
          "src/bin_tps_evaluation/main_tps_evaluation.exe";
          "src/bin_octogram/octogram_main.exe tezt/tests/main.exe";
        ]
  in
  let variables =
    [("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
    @ [("BUILD_EXTRA", String.concat " " build_extra)]
  in
  let artifacts =
    artifacts
      ~name:"build-$ARCH-$CI_COMMIT_REF_SLUG"
      ~when_:On_success
      ~expire_in:(Duration (Days 1))
      (* TODO: [paths] can be refined based on [release] *)
      [
        "octez-*";
        "octez-teztale-*";
        "src/proto_*/parameters/*.json";
        "_build/default/src/lib_protocol_compiler/bin/main_native.exe";
        "_build/default/tezt/tests/main.exe";
        "_build/default/contrib/octez_injector_server/octez_injector_server.exe";
        "etherlink-governance-observer";
      ]
  in
  let job =
    job
      ?rules
      ?dependencies
      ~__POS__
      ~stage:Stages.build
      ~arch
      ?retry
      ?cpu
      ?storage
      ~name
      ~image:Images.CI.build
      ~before_script:
        (before_script
           ~take_ownership:true
           ~source_version:true
           ~eval_opam:true
           [])
      ~variables
      ~artifacts
      ["./scripts/ci/build_full_unreleased.sh"]
    |> enable_cargo_cache |> enable_sccache |> enable_cargo_target_caches
  in
  (* Disable coverage for arm64 *)
  if arch = Amd64 then Coverage.enable_instrumentation job else job

(** {2 Shared jobs} *)

let job_build_arm64_release ?rules () : tezos_job =
  job_build_released_binaries ?rules ~__POS__ ~arch:Arm64 ~storage:Ramfs ()

let job_build_arm64_extra_dev ?rules () : tezos_job =
  job_build_dynamic_binaries
    ~name:"oc.build_arm64-extra-dev"
    ?rules
    ~__POS__
    ~arch:Arm64
    ~storage:Ramfs
    "script-inputs/dev-executables"

let job_build_arm64_extra_exp ?rules () : tezos_job =
  job_build_dynamic_binaries
    ~name:"oc.build_arm64-extra-exp"
    ?rules
    ~__POS__
    ~arch:Arm64
    ~storage:Ramfs
    "script-inputs/experimental-executables"

let job_build_kernels ?rules () : tezos_job =
  job
    ~__POS__
    ~name:"oc.build_kernels"
    ~image:Images.rust_toolchain
    ~stage:Stages.build
    ?rules
    ["make -f kernels.mk build"; "make -f etherlink.mk evm_kernel.wasm"]
    ~artifacts:
      (artifacts
         ~name:"build-kernels-$CI_COMMIT_REF_SLUG"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         [
           "evm_kernel.wasm";
           "smart-rollup-installer";
           "sequenced_kernel.wasm";
           "tx_kernel.wasm";
           "tx_kernel_dal.wasm";
           "dal_echo_kernel.wasm";
         ])
  |> enable_kernels
  |> enable_sccache ~key:"kernels-sccache"
  |> enable_cargo_cache

let job_build_layer1_profiling ?rules ?(expire_in = Duration (Days 1)) () =
  job
    ~__POS__
    ~stage:Stages.build
    ~image:Images.CI.build
    ?rules
    ~name:"build-layer1-profiling"
    ~cpu:Very_high
    ~artifacts:
      (artifacts
         ~expire_in
         [
           "./octez-binaries/x86_64/octez-node";
           "./octez-binaries/x86_64/octez-client";
         ])
    ~before_script:
      (before_script
         ~take_ownership:true
         ~source_version:true
         ~eval_opam:true
         [])
    ~variables:[("PROFILE", "static")]
    [
      "scripts/slim-mode.sh on";
      (* turn on -opaque for all subsequent builds *)
      "scripts/custom-flags.sh set -opaque";
      (* 1) compile with PPX profiling *)
      "TEZOS_PPX_PROFILER=profiling make build OCTEZ_EXECUTABLES?=\"octez-node \
       octez-client\"";
      (* 2) compile with OpenTelemetry PPX (overwrites binaries) *)
      "TEZOS_PPX_PROFILER=opentelemetry make build \
       OCTEZ_EXECUTABLES?=\"octez-node octez-client\"";
      "mkdir -p octez-binaries/x86_64/";
      "mv octez-node octez-binaries/x86_64/";
      "mv octez-client octez-binaries/x86_64/";
    ]
  |> enable_cargo_cache |> enable_sccache
