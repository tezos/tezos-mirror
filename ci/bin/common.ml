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

(* types for the repositories pipelines.
   - Release: we run all the release jobs, but no tests
   - Partial: we run only a subset of the tests jobs
   - Full: we run the complete test matrix
*)
type repository_pipeline = Full | Partial | Release

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

(** A [script:] that executes [script] and propagates its exit code.

    This might seem like a noop but is in fact necessary to please his
    majesty GitLab.

    For more info, see:
     - https://gitlab.com/tezos/tezos/-/merge_requests/9923#note_1538894754;
     - https://gitlab.com/tezos/tezos/-/merge_requests/12141; and
     - https://gitlab.com/groups/gitlab-org/-/epics/6074

   TODO: replace this with [FF_USE_NEW_BASH_EVAL_STRATEGY=true], see
   {{:https://docs.gitlab.com/runner/configuration/feature-flags.html}GitLab
   Runner feature flags}. *)
let script_propagate_exit_code script = [script ^ " || exit $?"]

let opt_var name f = function Some value -> [(name, f value)] | None -> []

(** Add variable for bisect_ppx instrumentation.

    This template should be extended by jobs that build OCaml targets
    that should be instrumented for coverage output. This set of job
    includes build jobs (like [oc.build_x86_64_*]). It also includes
    OCaml unit test jobs like [oc.unit:*-x86_64] as they build the test
    runners before their execution. *)
let enable_coverage_instrumentation : tezos_job -> tezos_job =
  Tezos_ci.append_variables
    [("COVERAGE_OPTIONS", "--instrument-with bisect_ppx")]

(** Add variable specifying coverage trace storage.

    This function should be applied to jobs that either produce (like
    test jobs) or consume (like the [unified_coverage] job) coverage
    traces. In addition to specifying the location of traces, setting
    this variable also _enables_ coverage trace output for
    instrumented binaries. *)
let enable_coverage_location : tezos_job -> tezos_job =
  Tezos_ci.append_variables
    [("BISECT_FILE", "$CI_PROJECT_DIR/_coverage_output/")]

let enable_coverage_report job : tezos_job =
  job
  |> Tezos_ci.add_artifacts
       ~expose_as:"Coverage report"
       ~reports:
         (reports
            ~coverage_report:
              {
                coverage_format = Cobertura;
                path = "_coverage_report/cobertura.xml";
              }
            ())
       ~expire_in:(Duration (Days 15))
       ~when_:Always
       ["_coverage_report/"; "$BISECT_FILE"]
  |> Tezos_ci.append_variables [("SLACK_COVERAGE_CHANNEL", "C02PHBE7W73")]

(** Add common variables used by jobs compiling kernels *)
let enable_kernels =
  Tezos_ci.append_variables
    [("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]

(** {2 Caches} *)

(* Common GitLab CI caches *)

(** Enable caching of Cargo's target folder which stores files which
    can speed up subsequent compilation passes.

    All folders are stored in a single cacheable directory to work
    around GitLab's restriction on the number caches a job may have. *)
let enable_cargo_target_caches ?key job =
  let key =
    Option.value
      ~default:
        ("rust-targets-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
      key
  in
  let cache_dir = "$CI_PROJECT_DIR" // "_target" in
  job
  |> append_variables
       [
         ("OCTEZ_RUST_DEPS_TARGET_DIR", cache_dir // "rust_deps");
         ("OCTEZ_RUSTZCASH_DEPS_TARGET_DIR", cache_dir // "rustzcash_deps");
         ( "OCTEZ_ETHERLINK_WASM_RUNTIME_TARGET_DIR",
           cache_dir // "etherlink_wasm_runtime" );
       ]
  |> append_cache (cache ~key [cache_dir])

(** Add variable enabling dune cache.

    This function can be applied to jobs that run dune.

    - [key] and [path] configure the key under which the cache is
    stored, and the path that will be cached. By default, the [key]
    contains the name of the job, thus scoping the cache to all
    instances of that job. By default, [path] is the folder
    ["$CI_PROJECT_DIR/_dune_cache"], and this function also sets the
    environment dir [DUNE_CACHE_ROOT] such that dune stores its caches
    there.

    - [cache_size] sets the maximum size of the cache.

    - [copy_mode], if [true] (default is [false]) sets
    {{:https://dune.readthedocs.io/en/stable/caching.html#cache-storage-mode}Dune
    Cache Storage Mode} to [copy]. If [false], [hardlink] mode is
    used, which is typically more performant but requires that the
    build and cache folder be on the same volume. *)
let enable_dune_cache ?key ?(path = "$CI_PROJECT_DIR/_dune_cache")
    ?(cache_size = "5GB") ?(copy_mode = false) ?policy job =
  let key =
    Option.value
      ~default:
        ("dune_cache-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
      key
  in
  job
  |> append_variables
       [
         ("DUNE_CACHE", "enabled");
         ("DUNE_CACHE_STORAGE_MODE", if copy_mode then "copy" else "hardlink");
         ("DUNE_CACHE_ROOT", path);
       ]
  |> append_cache (cache ?policy ~key [path])
  |> append_after_script
       ["eval $(opam env)"; "dune cache trim --size=" ^ cache_size]

(** {2 Child repositories pipelines} *)

(** Return a tuple (ARCHITECTURES, <archs>) based on the type
    of repository pipeline. *)
let archs_variables pipeline =
  let amd64 = List.map Tezos_ci.arch_to_string_alt [Amd64] in
  let all = List.map Tezos_ci.arch_to_string_alt [Amd64; Arm64] in
  match pipeline with
  | Partial -> [("ARCHITECTURES", String.concat " " amd64)]
  | Full | Release -> [("ARCHITECTURES", String.concat " " all)]

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

(** {2 Changesets} *)

(** Modifying these files will unconditionally execute all conditional jobs.

    If the CI configuration change, we execute all
    jobs. [changeset_base] should be included in all changesets below,
    any exceptions should be explained. *)
let changeset_base = Changeset.make [".gitlab/**/*"; ".gitlab-ci.yml"]

let changeset_images = Changeset.make ["images/**/*"]

(** Only if octez source code has changed *)
let changeset_octez =
  let octez_source_content =
    List.map
      (fun path -> if Sys.is_directory path then path ^ "/**/*" else path)
      (read_lines_from_file "script-inputs/octez-source-content")
    |> Changeset.make
  in
  Changeset.(
    changeset_base @ octez_source_content
    @ make
        [
          "etherlink/**/*";
          "michelson_test_scripts/**/*";
          "tzt_reference_test_suite/**/*";
        ])

(* Only if Etherlink has changed *)
let changeset_etherlink =
  Changeset.(
    changeset_base
    @ make
        ["etherlink/**/*"; "Makefile"; "src/kernel_sdk/**/*"; "sdk/rust/**/*"])

(** Only if octez source code has changed, if the images has changed or
    if kernels.mk changed. *)
let changeset_octez_or_kernels =
  Changeset.(
    changeset_base @ changeset_octez @ changeset_images
    @ make ["scripts/ci/**/*"; "kernels.mk"; "etherlink.mk"])

(** Only if documentation has changed *)
let changeset_octez_docs =
  Changeset.(
    changeset_base
    @ make
        [
          "scripts/**/*/";
          "script-inputs/**/*/";
          "src/**/*";
          "tezt/**/*";
          "brassaia/**/*";
          "irmin/**/*";
          "client-libs/**/*";
          "etherlink/**/*";
          "data-encoding/**/*";
          "vendors/**/*";
          "dune";
          "dune-project";
          "dune-workspace";
          "docs/**/*";
        ])

(** Only if reStructured Text files have changed *)
let changeset_octez_docs_rst = Changeset.(changeset_base @ make ["**/*.rst"])

let changeset_octez_docker_changes_or_master =
  Changeset.(
    changeset_base
    @ make
        [
          "scripts/**/*";
          "script-inputs/**/*";
          "src/**/*";
          "tezt/**/*";
          "vendors/**/*";
          "dune";
          "dune-project";
          "dune-workspace";
          "opam";
          "Makefile";
          "kernels.mk";
          "build.Dockerfile";
          "Dockerfile";
        ])

let changeset_docker_files = Changeset.make ["build.Dockerfile"; "Dockerfile"]

let changeset_debian_packages =
  Changeset.(
    make
      [
        "scripts/packaging/build-deb-local.sh";
        "scripts/packaging/Release.conf";
        "scripts/packaging/octez/debian/*";
        "debian-deps-build.Dockerfile";
        "scripts/ci/build-debian-packages_current.sh";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-debian-packages.sh";
        "scripts/ci/prepare-apt-repo.sh";
        "scripts/ci/create_debian_repo.sh";
        "docs/introduction/install-bin-deb.sh";
        "docs/introduction/upgrade-bin-deb.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_rpm_packages =
  Changeset.(
    make
      [
        "scripts/packaging/build-deb-local.sh";
        "scripts/packaging/octez/rpm/*";
        "scripts/packaging/tests/rpm/*";
        "rpm-deps-build.Dockerfile";
        "scripts/ci/build-packages-dependencies.sh";
        "scripts/ci/build-rpm-packages.sh";
        "scripts/ci/prepare-apt-rpm-repo.sh";
        "scripts/ci/create_rpm_repo.sh";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

let changeset_homebrew =
  Changeset.(
    make
      [
        "scripts/packaging/test_homebrew_install.sh";
        "scripts/packaging/homebrew_release.sh";
        "scripts/ci/install-gsutil.sh";
        "scripts/packaging/homebrew_install.sh";
        "scripts/packaging/Formula/*";
        "scripts/version.sh";
        "manifest/**/*.ml*";
      ])

(** The set of [changes:] that select opam jobs.

    Note: unlike all other changesets, this one does not include {!changeset_base}.
    This is to avoid running these costly jobs too often. *)
let changeset_opam_jobs =
  Changeset.(
    make
      [
        "**/dune";
        "**/dune.inc";
        "**/*.dune.inc";
        "**/dune-project";
        "**/dune-workspace";
        "**/*.opam";
        ".gitlab/ci/jobs/packaging/opam:prepare.yml";
        ".gitlab/ci/jobs/packaging/opam_package.yml";
        "manifest/**/*.ml*";
        "scripts/opam-prepare-repo.sh";
        "scripts/version.sh";
      ])

let changeset_kaitai_e2e_files =
  Changeset.(
    changeset_base @ changeset_images
    @ make
        [
          (* Regenerate the client-libs-dependencies image when the CI
             scripts change. *)
          "scripts/ci/**/*";
          "src/**/*";
          "client-libs/*kaitai*/**/*";
        ])

(** Set of OCaml files for type checking ([dune build @check]). *)
let changeset_ocaml_check_files =
  Changeset.(
    changeset_base
    @ make ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "**/*.ml"; "**/*.mli"])

(** Set of files for checking the WASM Runtime bindings. *)
let changeset_wasm_runtime_check_files =
  Changeset.(changeset_base @ make ["src/lib_wasm_runtime/**/*.rs"])

let changeset_lift_limits_patch =
  Changeset.(
    changeset_base
    @ make
        [
          "src/bin_tps_evaluation/lift_limits.patch";
          "src/proto_alpha/lib_protocol/main.ml";
        ])

(* The linting job runs over the set of [source_directories]
   defined in [scripts/lint.sh] that must be included here: *)
let changeset_lint_files =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "tezt/**/*";
          "devtools/**/*";
          "scripts/**/*";
          "docs/**/*";
          "contrib/**/*";
          "client-libs/**/*";
          "etherlink/**/*";
        ])

(** Set of Python files. *)
let changeset_python_files =
  Changeset.(changeset_base @ make ["poetry.lock"; "pyproject.toml"; "**/*.py"])

(** Set of Rust files for formatting ([cargo fmt --check]). *)
let changeset_rust_fmt_files = Changeset.(changeset_base @ make ["**/*.rs"])

(** Set of OCaml files for formatting ([dune build @fmt]). *)
let changeset_ocaml_fmt_files =
  Changeset.(changeset_base @ make ["**/.ocamlformat"; "**/*.ml"; "**/*.mli"])

let changeset_semgrep_files =
  Changeset.(
    changeset_base
    @ make ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "scripts/semgrep/**/*"])

(** Set of Jsonnet files for formatting ([jsonnetfmt --test]). *)
let changeset_jsonnet_fmt_files = Changeset.(make ["**/*.jsonnet"])

(* We only need to run the [oc.script:snapshot_alpha_and_link] job if
   protocol Alpha or if the scripts changed. *)
let changeset_script_snapshot_alpha_and_link =
  Changeset.(
    changeset_base
    @ make
        [
          "src/proto_alpha/**/*";
          "scripts/snapshot_alpha_and_link.sh";
          "scripts/snapshot_alpha.sh";
          "scripts/user_activated_upgrade.sh";
        ])

let changeset_script_b58_prefix =
  Changeset.(
    changeset_base
    @ make
        [
          "scripts/b58_prefix/b58_prefix.py";
          "scripts/b58_prefix/test_b58_prefix.py";
        ])

let changeset_test_liquidity_baking_scripts =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "scripts/ci/test_liquidity_baking_scripts.sh";
          "scripts/check-liquidity-baking-scripts.sh";
        ])

let changeset_test_sdk_rust =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["sdk/rust/**/*"])

(* TODO: add sdk/rust to the changesets once the bindings code depends
   on the Rust SDK: https://linear.app/tezos/issue/SDK-59. *)
let changeset_test_sdk_bindings =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["sdk/rust/**/*"]
    @ make ["contrib/sdk-bindings"])

let changeset_test_kernels =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make
        ["kernels.mk"; "src/kernel_*/**/*"; "src/riscv/**/*"; "sdk/rust/**/*"])

let changeset_test_etherlink_kernel =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make
        [
          "etherlink.mk";
          "etherlink/**/*.rs";
          "src/kernel_sdk/**/*";
          "sdk/rust/**/*";
        ])

let changeset_test_etherlink_firehose =
  Changeset.(
    changeset_base @ changeset_images
    @ make
        [
          "etherlink/firehose/**/*";
          "etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*";
        ])

let changeset_riscv_kernels =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["sdk/rust/**/*"; "src/kernel_sdk/**/*"; "src/riscv/**/*"])

let changeset_test_evm_compatibility =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make
        [
          "etherlink.mk";
          "etherlink/kernel_latest/evm_execution/**/*";
          "etherlink/kernel_latest/evm_evaluation/**/*";
        ])

let changeset_mir =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["contrib/mir/**/*"])

let changeset_mir_tzt =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["contrib/mir/**/*"; "tzt_reference_test_suite/**/*"])

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
let job_build_static_binaries ~__POS__ ~arch ?(cpu = Normal)
    ?(executable_files = "script-inputs/octez-released-executables")
    ?(experimental_executables = "script-inputs/octez-experimental-executables")
    ?version_executable ?(release = false) ?rules ?dependencies ?retry () :
    tezos_job =
  let arch_string = arch_to_string arch in
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
    ~name
    ?retry
    ~image:Images.CI.build
    ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
    ~variables:
      ([("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
      @ version_executable)
    ~artifacts
    ["./scripts/ci/build_static_binaries.sh"]
  |> enable_cargo_cache
  |> enable_sccache ~cache_size:"2G"
  |> enable_cargo_target_caches

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
let job_docker_build ?rules ?dependencies ~__POS__ ~arch docker_build_type :
    tezos_job =
  let arch_string = arch_to_string_alt arch in
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
    ~name
    ~variables
    ["./scripts/ci/docker_release.sh"]

let job_docker_merge_manifests ~__POS__ ~ci_docker_hub ~job_docker_amd64
    ~job_docker_arm64 : tezos_job =
  job_docker_authenticated
    ~__POS__
    ~stage:Stages.prepare_release
    ~name:"docker:merge_manifests"
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
    ~stage:Stages.publish_release
    ~name:"docker:promote_to_latest"
    ~ci_docker_hub
    ["./scripts/ci/docker_promote_to_latest.sh"]

type bin_package_target = Rpm

type bin_package_group = A | B

let bin_package_image = Image.mk_external ~image_path:"$DISTRIBUTION"

let job_build_dynamic_binaries ?rules ~__POS__ ~arch ?retry ?cpu
    ?(release = false) ?dependencies () =
  let arch_string = arch_to_string arch in
  let name =
    sf
      "oc.build_%s-%s"
      arch_string
      (if release then "released" else "exp-dev-extra")
  in
  let executable_files =
    if release then "script-inputs/released-executables"
    else "script-inputs/experimental-executables script-inputs/dev-executables"
  in
  let build_extra =
    match (release, arch) with
    | true, _ -> None
    | false, Amd64 ->
        Some
          [
            "src/bin_tps_evaluation/main_tps_evaluation.exe";
            "src/bin_octogram/octogram_main.exe";
            "tezt/tests/main.exe";
            "contrib/octez_injector_server/octez_injector_server.exe";
          ]
    | false, Arm64 ->
        Some
          [
            "src/bin_tps_evaluation/main_tps_evaluation.exe";
            "src/bin_octogram/octogram_main.exe tezt/tests/main.exe";
          ]
  in
  let variables =
    [("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
    @
    match build_extra with
    | Some build_extra -> [("BUILD_EXTRA", String.concat " " build_extra)]
    | None -> []
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
  if arch = Amd64 then enable_coverage_instrumentation job else job

(** {2 Shared jobs} *)

let job_build_arm64_release ?rules () : tezos_job =
  job_build_dynamic_binaries ?rules ~__POS__ ~arch:Arm64 ~release:true ()

let job_build_arm64_exp_dev_extra ?rules () : tezos_job =
  job_build_dynamic_binaries ?rules ~__POS__ ~arch:Arm64 ~release:false ()

let job_build_kernels ?rules () : tezos_job =
  job
    ~__POS__
    ~name:"oc.build_kernels"
    ~image:Images.rust_toolchain
    ~stage:Stages.build
    ?rules
    [
      "make -f kernels.mk build";
      "make -f etherlink.mk evm_kernel.wasm";
      "make -C src/riscv riscv-dummy.elf";
    ]
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
           "src/riscv/riscv-dummy.elf";
         ])
  |> enable_kernels
  |> enable_sccache ~key:"kernels-sccache" ~path:"$CI_PROJECT_DIR/_sccache"
  |> enable_cargo_cache

let job_build_dsn_node ?rules () : tezos_job =
  job
    ~__POS__
    ~name:"oc.build_dsn_node"
    ~image:Images.rust_toolchain
    ~stage:Stages.build
    ?rules
    ["make -f etherlink.mk octez-dsn-node"]
    ~artifacts:
      (artifacts
         ~name:"build-dsn-node-$CI_COMMIT_REF_SLUG"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         ["octez-dsn-node"])
  |> enable_kernels |> enable_sccache |> enable_cargo_cache

let job_datadog_pipeline_trace : tezos_job =
  job
    ~__POS__
    ~allow_failure:Yes
    ~name:"datadog_pipeline_trace"
    ~image:Images.datadog_ci
    ~stage:Stages.start
    [
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

let job_build_layer1_profiling ?(expire_in = Duration (Days 1)) () =
  job
    ~__POS__
    ~stage:Stages.build
    ~image:Images.CI.build
    ~name:"build-layer1-profiling"
    ~cpu:Very_high
    ~retry:{max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
    ~artifacts:(artifacts ~expire_in ["./octez-binaries/x86_64/octez-node"])
    ~before_script:
      (before_script
         ~take_ownership:true
         ~source_version:true
         ~eval_opam:true
         [])
    ~variables:[("TEZOS_PPX_PROFILER", "profiling"); ("PROFILE", "static")]
    [
      "scripts/slim-mode.sh on";
      "make build OCTEZ_EXECUTABLES?=octez-node";
      "mkdir -p octez-binaries/x86_64/";
      "mv octez-node octez-binaries/x86_64/";
    ]
  |> enable_cargo_cache |> enable_sccache

module Tezt = struct
  (** Create a tezt job.

      To enable tezt selection via manifest, pass a job as constructed
      by {!job_select_tezts} as [~job_select_tezts]. Then the
      constructed tezt job will only run the tezts selected by the
      selection job. *)
  let job ~__POS__ ?rules ?parallel ?(tag = Gcp_tezt) ~name
      ~(tezt_tests : Tezt_core.TSL_AST.t) ?(retry = 2) ?(tezt_retry = 1)
      ?(tezt_parallel = 1) ?(tezt_variant = "")
      ?(before_script = before_script ~source_version:true ~eval_opam:false [])
      ?timeout ?job_select_tezts ~dependencies ?allow_failure () : tezos_job =
    let variables =
      [
        ("JUNIT", "tezt-junit.xml");
        ("TEZT_VARIANT", tezt_variant);
        ("TESTS", Tezt_core.TSL.show tezt_tests);
        ("TEZT_RETRY", string_of_int tezt_retry);
        ("TEZT_PARALLEL", string_of_int tezt_parallel);
        ("TEZT_NO_NPX", "true");
      ]
    in
    let artifacts =
      artifacts
        ~reports:(reports ~junit:"$JUNIT" ())
        [
          "selected_tezts.tsv";
          "tezt.log";
          "tezt-*.log";
          "tezt-results-${CI_NODE_INDEX:-1}${TEZT_VARIANT}.json";
          "$JUNIT";
        ]
        (* The record artifacts [tezt-results-$CI_NODE_INDEX.json]
           should be stored for as long as a given commit on master is
           expected to be HEAD in order to support auto-balancing. At
           the time of writing, we have approximately 6 merges per day,
           so 1 day should more than enough. However, we set it to 3
           days to keep records over the weekend. The tezt artifacts
           (including records and coverage) take up roughly 2MB /
           job. Total artifact storage becomes [N*P*T*W] where [N] is
           the days of retention (7 atm), [P] the number of pipelines
           per day (~200 atm), [T] the number of Tezt jobs per pipeline
           (100) and [W] the artifact size per tezt job (2MB). This
           makes 280GB which is ~4% of our
           {{:https://gitlab.com/tezos/tezos/-/artifacts}total artifact
           usage}. *)
        ~expire_in:(Duration (Days 7))
        ~when_:Always
    in
    let print_variables =
      [
        "TESTS";
        "JUNIT";
        "CI_NODE_INDEX";
        "CI_NODE_TOTAL";
        "TEZT_PARALLEL";
        "TEZT_VARIANT";
      ]
    in
    let with_or_without_select_tezts, dependencies =
      match job_select_tezts with
      | Some job_select_tezts ->
          let dependencies =
            Tezos_ci.dependencies_add_artifact_dependency
              dependencies
              job_select_tezts
          in
          ("--with-select-tezts", dependencies)
      | None -> ("--without-select-tezts", dependencies)
    in
    let retry = if retry = 0 then None else Some {max = retry; when_ = []} in
    job
      ?timeout
      ~__POS__
      ~image:Images.CI.e2etest
      ~name
      ?parallel
      ~tag
      ~stage:Stages.test
      ?rules
      ~artifacts
      ~variables
      ~dependencies
      ?retry
      ~before_script
      ?allow_failure
      [
        (* Print [print_variables] in a shell-friendly manner for easier debugging *)
        "echo \""
        ^ String.concat
            " "
            (List.map (fun var -> sf {|%s=\"${%s}\"|} var var) print_variables)
        ^ "\"";
        (* Store the list of tests that have been scheduled for execution for later debugging.
           It is imperative this this first call to tezt receives any flags passed to the
           second call that affect test selection.Note that TESTS must be quoted (here and below)
           since it will contain e.g. '&&' which we want to interpreted as TSL and not shell
           syntax. *)
        "./scripts/ci/tezt.sh " ^ with_or_without_select_tezts
        ^ " \"${TESTS}\" --from-record tezt/records --job \
           ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --list-tsv > \
           selected_tezts.tsv";
        (* For Tezt tests, there are multiple timeouts:
           - --global-timeout is the internal timeout of Tezt, which only works if tests
             are cooperative;
           - the "timeout" command, which we set to send SIGTERM to Tezt 60s after --global-timeout
             in case tests are not cooperative;
           - the "timeout" command also sends SIGKILL 60s after having sent SIGTERM in case
             Tezt is still stuck;
           - the CI timeout.
           The use of the "timeout" command is to make sure that Tezt eventually exits,
           because if the CI timeout is reached, there are no artefacts,
           and thus no logs to investigate.
           See also: https://gitlab.com/gitlab-org/gitlab/-/issues/19818 *)
        (* To observe memory usage of tests, we use the following options:
           - --record-mem-peak causes Tezt to measure memory usage
             (it is implied by --mem-warn so we could omit it);
           - --junit-mem-peak tells Tezt to store peak memory usage
             in a <property> named dd_tags[memory.peak] in JUnit reports,
             which makes DataDog aware of it
             (see https://docs.datadoghq.com/tests/setup/junit_xml/?tab=linux#providing-metadata-through-property-elements);
           - --mem-warn causes Tezt to warn if a test uses more than the specified
             amount of memory (in bytes). We set the threshold to 5 GB. *)
        "./scripts/ci/exit_code.sh timeout -k 60 1860 ./scripts/ci/tezt.sh \
         --send-junit " ^ with_or_without_select_tezts
        ^ " \"${TESTS}\" --color --log-buffer-size 5000 --log-file tezt.log \
           --global-timeout 1800 --on-unknown-regression-files fail --junit \
           ${JUNIT} --junit-mem-peak 'dd_tags[memory.peak]' --from-record \
           tezt/records --job ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --record \
           tezt-results-${CI_NODE_INDEX:-1}${TEZT_VARIANT}.json --job-count \
           ${TEZT_PARALLEL} --retry ${TEZT_RETRY} --record-mem-peak --mem-warn \
           5_000_000_000";
      ]

  (** Tezt tag selector string.

    It returns a TSL expression that:
    - always deselects tags with [ci_disabled];
    - selects, respectively deselects, the tests with the tags
      [time_sensitive], [slow] or [cloud],
      depending on the value of the corresponding function argument.
      These arguments all default to false.

    See [src/lib_test/tag.mli] for a description of the above tags.

    The list of TSL expressions [and_] are appended to the final
    selector, allowing to modify the selection further. *)
  let tests_tag_selector ?(time_sensitive = false) ?(slow = false)
      ?(cloud = false) (and_ : Tezt_core.TSL_AST.t list) : Tezt_core.TSL_AST.t =
    let tags =
      [
        (false, "ci_disabled");
        (time_sensitive, "time_sensitive");
        (slow, "slow");
        (cloud, "cloud");
      ]
    in
    let positive, negative = List.partition fst tags in
    let positive = List.map snd positive in
    let negative = List.map snd negative in
    Tezt_core.(
      TSL.conjunction
      @@ List.map (fun tag -> TSL_AST.Has_tag tag) positive
      @ List.map (fun tag -> TSL_AST.Not (Has_tag tag)) negative
      @ and_)

  (** Selects tezt tests based on merge request diff.

      This job only makes sense in merge request pipelines. To enable
      tezt selection, it should be passed to the [~job_select_tezts]
      argument to {!Tezt.job}. *)
  let job_select_tezts ?rules () : tezos_job =
    Tezos_ci.job
      ~__POS__
      ~name:"select_tezts"
        (* We need:
           - Git (to run git diff)
           - ocamlyacc, ocamllex and ocamlc (to build manifest/manifest) *)
      ?rules
      ~image:Images.CI.prebuild
      ~stage:Stages.build
      ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
      (script_propagate_exit_code "scripts/ci/select_tezts.sh")
      ~allow_failure:(With_exit_codes [17])
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Days 3))
           ~when_:Always
           ["selected_tezts.tsl"])

  (* Fetch records for Tezt generated on the last merge request
     pipeline on the most recently merged MR and makes them available
     in artifacts for future merge request pipelines. *)
  let job_tezt_fetch_records ?rules () : tezos_job =
    Tezos_ci.job
      ~__POS__
      ~name:"oc.tezt:fetch-records"
      ~image:Images.CI.build
      ~stage:Stages.build
      ~before_script:
        (before_script
           ~take_ownership:true
           ~source_version:true
           ~eval_opam:true
           [])
      ?rules
      [
        "dune exec scripts/ci/update_records/update.exe -- --log-file \
         tezt-fetch-records.log --from last-successful-schedule-extended-test \
         --info";
      ]
      ~after_script:["./scripts/ci/filter_corrupted_records.sh"]
        (* Allow failure of this job, since Tezt can use the records
           stored in the repo as backup for balancing. *)
      ~allow_failure:Yes
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Hours 4))
           ~when_:Always
           [
             "tezt-fetch-records.log";
             "tezt/records/*.json";
             (* Keep broken records for debugging *)
             "tezt/records/*.json.broken";
           ])
end

module Documentation = struct
  let mk_artifact_dependencies ?(dependencies = Dependent []) jobs =
    List.fold_right
      (fun job dependencies ->
        dependencies_add_artifact_dependency dependencies job)
      jobs
      dependencies

  (** Create an odoc job.

      The job will build the target [odoc] (resp. [odoc-lite]) in the
      directory [docs] if lite is [false] (resp. [true]), which is the
      default.

      This job is one of the prerequisites to {!job_build_all}. *)
  let job_odoc ?(lite = false) ?rules ?dependencies () : tezos_job =
    let target = if lite then "odoc-lite" else "odoc" in
    job
      ~__POS__
      ~name:"documentation:odoc"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ?dependencies
      ?rules
      ~before_script:(before_script ~eval_opam:true [])
      ~artifacts:
        (artifacts
           ~when_:Always
           ~expire_in:(Duration (Hours 1))
           (* Path must be terminated with / to expose artifact (gitlab-org/gitlab#/36706) *)
           ["docs/_build/api/odoc/"; "docs/odoc.log"])
      ["make -C docs " ^ target]
    |> enable_cargo_cache |> enable_sccache

  (** Create the manuals job.

      This job builds the command-line interface manuals (i.e [man])
      of octez binaries, for inclusion in the documentation.

      This job is one of the prerequisites to {!job_build_all}. *)
  let job_manuals ?rules ?dependencies ~use_static_executables () : tezos_job =
    job
      ~__POS__
      ~name:"documentation:manuals"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ?dependencies
      ?rules
      ~before_script:(before_script ~eval_opam:true [])
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Weeks 1))
           [
             "docs/*/octez-*.html";
             "docs/api/octez-*.txt";
             "docs/developer/metrics.csv";
             "docs/developer/rollup_metrics.csv";
             "docs/user/node-config.json";
           ])
      (if use_static_executables then
         ["scripts/ci/documentation:manuals_static.sh"]
       else ["make -C docs -j octez-gen"])

  (** Create the docgen job.

      This job builds various generated reference material, for
      inclusion in the documentation. This includes the RPC, P2p and
      error reference.

      This job is one of the prerequisites to {!job_build_all}. *)
  let job_docgen ?rules ?dependencies () : tezos_job =
    job
      ~__POS__
      ~name:"documentation:docgen"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ?dependencies
      ?rules
      ~before_script:(before_script ~eval_opam:true [])
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Weeks 1))
           [
             "docs/alpha/rpc.rst";
             "docs/shell/rpc.rst";
             "docs/user/default-acl.json";
             "docs/api/errors.rst";
             "docs/shell/p2p_api.rst";
           ])
      ["make -C docs -j docexes-gen"]
    |> enable_cargo_cache |> enable_sccache

  (** Create the [documentation:build_all] job.

      This jobs builds the RST sources in docs, which will include the
      the material from the prerequisite jobs: {!job_odoc},
      {!job_manuals}, {!job_docgen}, on whose artifacts the
      [documentation:build_all] job will depend. *)
  let job_build_all ~job_odoc ~job_manuals ~job_docgen ?dependencies ?rules () :
      tezos_job =
    let dependencies =
      mk_artifact_dependencies ?dependencies [job_odoc; job_manuals; job_docgen]
    in
    job
      ~__POS__
      ~name:"documentation:build_all"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ~dependencies
      ?rules
      ~before_script:(before_script ~eval_opam:true ~init_python_venv:true [])
      ~artifacts:
        (artifacts
           ~expose_as:"Documentation - excluding old protocols"
           ~expire_in:(Duration (Weeks 1))
           (* Path must be terminated with / to expose artifact (gitlab-org/gitlab#/36706) *)
           ["docs/_build/"])
      ["make -C docs -j sphinx"; "make -C docs -j _build/octezdoc.txt"]

  (** Create a [documentation:linkcheck] job. *)
  let job_linkcheck ~job_manuals ~job_docgen ~job_build_all ?dependencies ?rules
      () : tezos_job =
    let dependencies =
      mk_artifact_dependencies
        ?dependencies
        [job_manuals; job_docgen; job_build_all]
    in
    job
      ~__POS__
      ~name:"documentation:linkcheck"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ~dependencies
        (* Warning: the [documentation:linkcheck] job must have at least the same
           restrictions in the rules as [documentation:build_all], otherwise the CI
           may complain that [documentation:linkcheck] depends on [documentation:build_all]
           which does not exist. *)
      ?rules
      ~allow_failure:Yes
      ~before_script:
        (before_script
           ~source_version:true
           ~eval_opam:true
           ~init_python_venv:true
           [])
      ["make -C docs redirectcheck"; "make -C docs linkcheck"]

  (** Create a [publish:documentation] job.

      This job, for inclusion on [master_branch] pipelines, publishes
      built documentation received as artifact from a {!job_build_all}
      to [tezos.gitlab.io]. *)
  let job_publish_documentation ~job_build_all ?dependencies ?rules () :
      tezos_job =
    let dependencies = mk_artifact_dependencies ?dependencies [job_build_all] in
    job
      ~__POS__
      ~name:"publish:documentation"
      ~image:Images.CI.test
      ~stage:Stages.doc
      ~dependencies
      ~before_script:
        (before_script
           ~eval_opam:true
             (* Load the environment poetry previously created in the docker image.
                Give access to the Python dependencies/executables. *)
           ~init_python_venv:true
           (* [CI_PK_GITLAB_DOC] and [CI_KH] are set in the projects
              GitLab setting and are only available on protected
              refs. [CI_PK_GITLAB_DOC] contains the private key used
              as deploy key for the Octez documentation on
              [tezos.gitlab.io]. [CI_KH] contains the host key of
              [gitlab.com], which is not really a secret (it's
              public!). *)
           [
             {|echo "${CI_PK_GITLAB_DOC}" > ~/.ssh/id_ed25519|};
             {|echo "${CI_KH}" > ~/.ssh/known_hosts|};
             {|chmod 400 ~/.ssh/id_ed25519|};
           ])
      ~interruptible:false
      ?rules
      ["./scripts/ci/doc_publish.sh"]
    |> enable_cargo_cache |> enable_sccache
end
