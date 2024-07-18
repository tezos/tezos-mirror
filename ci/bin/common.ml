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

(* types for the debian repository pipelines.
   - Release: we run all the release jobs, but no tests
   - Partial: we run only a subset of the tests jobs
   - Full: we run the complete test matrix
*)
type debian_repository_pipeline = Full | Partial | Release

(* Encodes the conditional [before_merging] pipeline and its
   unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline = Before_merging | Schedule_extended_test

let cargo_home =
  (* Note:
     - We want [CARGO_HOME] to be in a sub-folder of
       {!ci_project_dir} to enable GitLab CI caching.
     - We want [CARGO_HOME] to be hidden from dune
       (thus the dot-prefix). *)
  Gitlab_ci.Predefined_vars.(show ci_project_dir) // ".cargo"

(* Define [stages:]

   The "manual" stage exists to fix a UI problem that occurs when mixing
   manual and non-manual jobs. *)
module Stages = struct
  let start = Stage.register "start"

  (* All automatic image creation is done in the stage [images]. *)
  let images = Stage.register "images"

  let sanity = Stage.register "sanity"

  let build = Stage.register "build"

  let test = Stage.register "test"

  let test_coverage = Stage.register "test_coverage"

  let packaging = Stage.register "packaging"

  let publishing = Stage.register "publishing"

  let publishing_tests = Stage.register "publishing_tests"

  let doc = Stage.register "doc"

  let prepare_release = Stage.register "prepare_release"

  let publish_release_gitlab = Stage.register "publish_release_gitlab"

  let publish_release = Stage.register "publish_release"

  let publish_package_gitlab = Stage.register "publish_package_gitlab"

  let manual = Stage.register "manual"
end

(* Get the [alpine_version] from the environment, which is typically
   set by sourcing [scripts/version.sh]. This is used to set the tag
   of the image {!Images.alpine}. *)
let alpine_version =
  match Sys.getenv_opt "alpine_version" with
  | None ->
      failwith
        "Please set the environment variable [alpine_version], by e.g. \
         sourcing [scripts/version.sh] before running."
  | Some v -> v

(* Register external images.

   Use this module to register images that are as built outside the
   [tezos/tezos] CI. *)
module Images_external = struct
  let nix = Image.mk_external ~image_path:"nixos/nix:2.22.1"

  (* Match GitLab executors version and directly use the Docker socket
     The Docker daemon is already configured, experimental features are enabled
     The following environment variables are already set:
     - [BUILDKIT_PROGRESS]
     - [DOCKER_DRIVER]
     - [DOCKER_VERSION]
     For more info, see {{:https://docs.gitlab.com/ee/ci/docker/using_docker_build.html#use-docker-socket-binding}} here.

     This image is defined in {{:https://gitlab.com/tezos/docker-images/ci-docker}tezos/docker-images/ci-docker}. *)
  let docker =
    Image.mk_external
      ~image_path:"${GCP_REGISTRY}/tezos/docker-images/ci-docker:v1.12.0"

  (* The Alpine version should be kept up to date with the version
     used for the [ci_image_name] images and specified in the
     variable [alpine_version] in [scripts/version.sh]. This is
     checked by the jobs [start] and [sanity_ci]. *)
  let alpine = Image.mk_external ~image_path:("alpine:" ^ alpine_version)

  let debian_bookworm = Image.mk_external ~image_path:"debian:bookworm"

  let ubuntu_focal =
    Image.mk_external ~image_path:"public.ecr.aws/lts/ubuntu:20.04_stable"

  let ubuntu_jammy =
    Image.mk_external ~image_path:"public.ecr.aws/lts/ubuntu:22.04_stable"

  let fedora_37 = Image.mk_external ~image_path:"fedora:37"

  let fedora_39 = Image.mk_external ~image_path:"fedora:39"

  let opam_ubuntu_jammy =
    Image.mk_external ~image_path:"ocaml/opam:ubuntu-22.04"

  let opam_ubuntu_mantic =
    Image.mk_external ~image_path:"ocaml/opam:ubuntu-23.10"

  let opam_debian_bookworm =
    Image.mk_external ~image_path:"ocaml/opam:debian-12"

  let ci_release =
    Image.mk_external
      ~image_path:"${GCP_REGISTRY}/tezos/docker-images/ci-release:v1.6.0"

  let hadolint = Image.mk_external ~image_path:"hadolint/hadolint:2.9.3-debian"

  (* We specify the semgrep image by hash to avoid flakiness. Indeed, if we took the
     latest release, then an update in the parser or analyser could result in new
     errors being found even if the code doesn't change. This would place the
     burden for fixing the code on the wrong dev (the devs who happen to open an
     MR coinciding with the semgrep update rather than the dev who wrote the
     infringing code in the first place).
     Update the hash in scripts/semgrep/README.md too when updating it here
     Last update: 2022-01-03 *)
  let semgrep_agent =
    Image.mk_external ~image_path:"returntocorp/semgrep-agent:sha-c6cd7cf"
end

(** {2 Helpers} *)

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

(** Add variable enabling sccache.

    This function should be applied to jobs that build rust files and
    which has a configured sccache Gitlab CI cache.

    - [key] and [path] configure the key under which the cache is
    stored, and the path that will be cached. By default, the [key]
    contains the name of the job, thus scoping the cache to all
    instances of that job. By default, [path] is the folder
    ["$CI_PROJECT_DIR/_sccache"], and this function also sets the
    environment dir [SCCACHE_DIR] such that sccache stores its caches
    there.

    - [error_log], [idle_timeout] and [log] sets the environment
    variables [SCCACHE_ERROR_LOG], [SCCACHE_IDLE_TIMEOUT] and
    [SCCACHE_LOG] respectively. See the sccache documentation for more
    information on these variables. *)
let enable_sccache ?key ?error_log ?idle_timeout ?log
    ?(path = "$CI_PROJECT_DIR/_sccache") job =
  let key =
    Option.value
      ~default:("sccache-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
      key
  in
  job
  |> append_variables
       ([("SCCACHE_DIR", path)]
       @ opt_var "SCCACHE_ERROR_LOG" Fun.id error_log
       @ opt_var "SCCACHE_IDLE_TIMEOUT" Fun.id idle_timeout
       @ opt_var "SCCACHE_LOG" Fun.id log)
  |> append_cache (cache ~key [path])
  (* Starts sccache and sets [RUSTC_WRAPPER] *)
  |> append_before_script [". ./scripts/ci/sccache-start.sh"]
  |> append_after_script ["sccache --stop-server || true"]

(** Allow cargo to access the network by setting [CARGO_NET_OFFLINE=false].

    This function should only be applied to jobs that have a GitLab CI
    cache for [CARGO_HOME], as enabled through [enable_cache_cargo] (that
    function calls this function, so there is no need to apply both).
    Exceptions can be made for jobs that must have CARGO_HOME set to
    something different than {!cargo_home}. *)
let enable_networked_cargo = append_variables [("CARGO_NET_OFFLINE", "false")]

(** Adds a GitLab CI cache for the CARGO_HOME folder.

    More precisely, we only cache the non-SCM dependencies in the
    sub-directory [registry/cache]. *)
let enable_cargo_cache job =
  job
  |> append_cache
       (cache
          ~key:("cargo-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
          [cargo_home // "registry/cache"])
  (* Allow Cargo to access the network *)
  |> enable_networked_cargo

(** Add variable enabling dune cache.

    This function can be applied to jobs that run dune.

    - [key] and [path] configure the key under which the cache is
    stored, and the path that will be cached. By default, the [key]
    contains the name of the job, thus scoping the cache to all
    instances of that job. By default, [path] is the folder
    ["$CI_PROJECT_DIR/_dune_cache"], and this function also sets the
    environment dir [DUNE_CACHE_ROOT] such that dune stores its caches
    there.

    - [copy_mode], if [true] (default is [false]) sets
    {{:https://dune.readthedocs.io/en/stable/caching.html#cache-storage-mode}Dune
    Cache Storage Mode} to [copy]. If [false], [hardlink] mode is
    used, which is typically more performant but requires that the
    build and cache folder be on the same volume. *)
let enable_dune_cache ?key ?(path = "$CI_PROJECT_DIR/_dune_cache")
    ?(copy_mode = false) ?policy job =
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

(** {2 Changesets} *)

(** Modifying these files will unconditionally execute all conditional jobs. *)
let changeset_base = Changeset.make [".gitlab/**/*"; ".gitlab-ci.yml"]

let changeset_images = Changeset.make ["images/**/*"]

(** Only if octez source code has changed *)
let changeset_octez =
  Changeset.(
    changeset_base
    @ make
        [
          "src/**/*";
          "data-encoding/**/*";
          "etherlink/**/*";
          "resto/**/*";
          "tezt/**/*";
          "michelson_test_scripts/**/*";
          "tzt_reference_test_suite/**/*";
          "irmin/**/*";
          "brassaia/**/*";
        ])

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
          "vendors/**/*";
          "dune";
          "dune-project";
          "dune-workspace";
          "docs/**/*";
        ])

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

let changeset_hadolint_docker_files =
  Changeset.make ["build.Dockerfile"; "Dockerfile"]

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

(** Set of OCaml files for formatting ([dune build @fmt]). *)
let changeset_ocaml_fmt_files =
  Changeset.(changeset_base @ make ["**/.ocamlformat"; "**/*.ml"; "**/*.mli"])

let changeset_semgrep_files =
  Changeset.(
    changeset_base
    @ make ["src/**/*"; "tezt/**/*"; "devtools/**/*"; "scripts/semgrep/**/*"])

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

let changeset_test_kernels =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["kernels.mk"; "src/kernel_*/**/*"])

let changeset_test_etherlink_kernel =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["etherlink.mk"; "etherlink/**/*.rs"; "src/kernel_sdk/**/*"])

let changeset_test_etherlink_firehose =
  Changeset.(
    changeset_base @ changeset_images
    @ make
        [
          "etherlink/firehose/**/*";
          "etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*";
        ])

let changeset_test_riscv_kernels =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make ["src/kernel_sdk/**/*"; "src/riscv/**/*"])

let changeset_test_evm_compatibility =
  Changeset.(
    changeset_base
    @ changeset_images (* Run if the [rust-toolchain] image is updated *)
    @ make
        [
          "etherlink.mk";
          "etherlink/kernel_evm/evm_execution/**/*";
          "etherlink/kernel_evm/evm_evaluation/**/*";
        ])

(** {2 Job makers} *)

(** Helper to create jobs that uses the Docker daemon.

    It sets the appropriate image. Furthermore, unless
    [skip_docker_initialization] is [true], it:
    - activates the Docker daemon as a service;
    - sets up authentification with Docker registries
    in the job's [before_script] section.

    If [ci_docker_hub] is set to [true], then the job will
    authenticate with Docker Hub provided the environment variable
    [CI_DOCKER_AUTH] contains the appropriate credentials. *)
let job_docker_authenticated ?(skip_docker_initialization = false)
    ?ci_docker_hub ?artifacts ?(variables = []) ?rules ?dependencies
    ?image_dependencies ?arch ?tag ?allow_failure ?parallel ~__POS__ ~stage
    ~name script : tezos_job =
  let docker_version = "24.0.6" in
  job
    ?rules
    ?dependencies
    ?image_dependencies
    ?artifacts
    ?arch
    ?tag
    ?allow_failure
    ?parallel
    ~__POS__
    ~image:Images_external.docker
    ~variables:
      ([("DOCKER_VERSION", docker_version)]
      @ opt_var "CI_DOCKER_HUB" Bool.to_string ci_docker_hub
      @ variables)
    ~before_script:
      (if not skip_docker_initialization then
         ["./scripts/ci/docker_initialize.sh"]
       else [])
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~stage
    ~name
    script

(** A set of internally and externally built images.

    Use this module to register images built in the CI of
    [tezos/tezos] that are also used in the same pipelines.See
    {!Images_external} for external images.

    To make the distinction between internal and external images
    transparent to job definitions, this module also includes
    {!Images_external}.

    For documentation on the [CI] images and the [rust_toolchain]
    images, refer to [images/README.md]. *)
module Images = struct
  (* Include external images here for convenience. *)
  include Images_external

  let client_libs_dependencies =
    let image_builder_amd64 =
      job_docker_authenticated
        ~__POS__
        ~stage:Stages.build
        ~name:"oc.docker:client-libs-dependencies"
          (* These image are not built for external use. *)
        ~ci_docker_hub:false
          (* Handle docker initialization, if necessary, in [./scripts/ci/docker_client_libs_dependencies_build.sh]. *)
        ~skip_docker_initialization:true
        ["./scripts/ci/docker_client_libs_dependencies_build.sh"]
        ~artifacts:
          (artifacts
             ~reports:
               (reports ~dotenv:"client_libs_dependencies_image_tag.env" ())
             [])
    in
    let image_path =
      "${client_libs_dependencies_image_name}:${client_libs_dependencies_image_tag}"
    in
    Image.mk_internal ~image_builder_amd64 ~image_path ()

  (** The rust toolchain image *)
  let rust_toolchain =
    (* The job that builds the rust_toolchain image.
       This job is automatically included in any pipeline that uses this image. *)
    let image_builder arch =
      job_docker_authenticated
        ~__POS__
        ~arch
        ~skip_docker_initialization:true
        ~stage:Stages.images
        ~name:("oc.docker:rust-toolchain:" ^ arch_to_string_alt arch)
        ~ci_docker_hub:false
        ~artifacts:
          (artifacts
             ~reports:(reports ~dotenv:"rust_toolchain_image_tag.env" ())
             [])
        ["./scripts/ci/docker_rust_toolchain_build.sh"]
    in
    let image_path =
      "${rust_toolchain_image_name}:${rust_toolchain_image_tag}"
    in
    Image.mk_internal
      ~image_builder_amd64:(image_builder Amd64)
      ~image_builder_arm64:(image_builder Arm64)
      ~image_path
      ()

  module CI = struct
    (* The job that builds the CI images.
       This job is automatically included in any pipeline that uses any of these images. *)
    let job_docker_ci arch =
      let variables = Some [("ARCH", arch_to_string_alt arch)] in
      job_docker_authenticated
        ?variables
        ~__POS__
        ~arch
        ~skip_docker_initialization:true
        ~stage:Stages.images
        ~name:("oc.docker:ci:" ^ arch_to_string_alt arch)
        ~ci_docker_hub:false
        ~artifacts:
          (artifacts ~reports:(reports ~dotenv:"ci_image_tag.env" ()) [])
        ["./images/ci_create_ci_images.sh"]

    let mk_ci_image ~image_path =
      Image.mk_internal
        ~image_builder_amd64:(job_docker_ci Amd64)
        ~image_builder_arm64:(job_docker_ci Arm64)
        ~image_path
        ()

    (* Reuse the same image_builder job [job_docker_ci] for all
       the below images, since they're all produced in that same job.

       Depending on any of these images ensures that the job
       [job_docker_ci] is included exactly once in the pipeline. *)
    let runtime =
      mk_ci_image ~image_path:"${ci_image_name}/runtime:${ci_image_tag}"

    let prebuild =
      mk_ci_image ~image_path:"${ci_image_name}/prebuild:${ci_image_tag}"

    let build = mk_ci_image ~image_path:"${ci_image_name}/build:${ci_image_tag}"

    let test = mk_ci_image ~image_path:"${ci_image_name}/test:${ci_image_tag}"

    let e2etest =
      mk_ci_image ~image_path:"${ci_image_name}/e2etest:${ci_image_tag}"
  end
end

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
let job_build_static_binaries ~__POS__ ~arch ?(release = false) ?rules
    ?dependencies () : tezos_job =
  let arch_string = arch_to_string arch in
  let name = "oc.build:static-" ^ arch_string ^ "-linux-binaries" in
  let artifacts =
    (* Extend the lifespan to prevent failure for external tools using artifacts. *)
    let expire_in = if release then Some (Duration (Days 90)) else None in
    artifacts ?expire_in ["octez-binaries/$ARCH/*"]
  in
  let executable_files =
    "script-inputs/released-executables"
    ^ if not release then " script-inputs/experimental-executables" else ""
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage:Stages.build
    ~arch
    ~name
    ~image:Images.CI.build
    ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
    ~variables:[("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
    ~artifacts
    ["./scripts/ci/build_static_binaries.sh"]
  |> enable_cargo_cache |> enable_sccache

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
    - [Test_manual] and [Experimental] Docker builds include the EVM kernels in
      amd64 builds.
    - [Release] and [Experimental] Docker builds are pushed to Docker hub,
      whereas other types are pushed to the GitLab registry.
    - [Test_manual] Docker builds are started manually, put in the stage
      [manual] and their failure is allowed. The other types are in the build
      stage, run [on_success] and are not allowed to fail. *)
type docker_build_type = Experimental | Release | Test | Test_manual

(** Creates a Docker build job of the given [arch] and [docker_build_type]. *)
let job_docker_build ?rules ?dependencies ~__POS__ ~arch docker_build_type :
    tezos_job =
  let arch_string = arch_to_string_alt arch in
  let ci_docker_hub =
    match docker_build_type with
    | Release | Experimental -> true
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

type bin_package_target = Dpkg | Rpm

let bin_package_image = Image.mk_external ~image_path:"$DISTRIBUTION"

let job_build_bin_package ?dependencies ?rules ~__POS__ ~name
    ?(stage = Stages.build) ~arch ~target () : tezos_job =
  let arch_string = arch_to_string_alt arch in
  let target_string = match target with Dpkg -> "dpkg" | Rpm -> "rpm" in
  let image = bin_package_image in
  let parallel =
    let distributions =
      match target with
      | Dpkg -> ["debian:bookworm"; "ubuntu:focal"; "ubuntu:jammy"]
      | Rpm -> ["fedora:39"; "rockylinux:9.3"]
    in
    Matrix [[("DISTRIBUTION", distributions)]]
  in
  let artifacts =
    artifacts
      ~expire_in:(Duration (Days 1))
      ~when_:On_success
      ~name:"${TARGET}-$ARCH-$CI_COMMIT_REF_SLUG"
      ["packages/"]
  in
  let before_script =
    before_script
      ~source_version:true
      (match target with
      | Dpkg -> ["./scripts/ci/bin_packages_deb_dependencies.sh"]
      | Rpm -> ["./scripts/ci/bin_packages_rpm_dependencies.sh"])
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~name
    ~arch
    ~image
    ~stage
    ~variables:
      [
        ("TARGET", target_string);
        ("OCTEZ_PKGMAINTAINER", "nomadic-labs");
        ("BLST_PORTABLE", "yes");
        ("ARCH", arch_string);
        ("CARGO_HOME", "/root/.cargo");
      ]
    ~artifacts
    ~parallel
    ~before_script
    [
      "wget https://sh.rustup.rs/rustup-init.sh";
      "chmod +x rustup-init.sh";
      "./rustup-init.sh --profile minimal --default-toolchain  \
       $recommended_rust_version -y";
      ". $HOME/.cargo/env";
      "export OPAMYES=\"true\"";
      "opam init --bare --disable-sandboxing";
      "make build-deps";
      "eval $(opam env)";
      "make $TARGET";
      "DISTRO=$(echo \"$DISTRIBUTION\" | cut -d':' -f1)";
      "RELEASE=$(echo \"$DISTRIBUTION\" | cut -d':' -f2)";
      "mkdir -p packages/$DISTRO/$RELEASE";
      "mv octez-*.* packages/$DISTRO/$RELEASE/";
    ]
  |> enable_networked_cargo

let job_build_dpkg_amd64 : unit -> tezos_job =
  job_build_bin_package
    ~__POS__
    ~name:"oc.build:dpkg:amd64"
    ~target:Dpkg
    ~arch:Amd64
    ~dependencies:(Dependent [])

let job_build_rpm_amd64 : unit -> tezos_job =
  job_build_bin_package
    ~__POS__
    ~name:"oc.build:rpm:amd64"
    ~target:Rpm
    ~arch:Amd64
    ~dependencies:(Dependent [])

let job_build_homebrew ?rules ~__POS__ ~name ?(stage = Stages.build)
    ?dependencies () : tezos_job =
  let image = Images.debian_bookworm in
  job
    ?rules
    ~__POS__
    ~name
    ~arch:Amd64
    ?dependencies
    ~image
    ~stage
    ~before_script:
      [
        "apt update && apt install -y curl git build-essential";
        "./scripts/packaging/homebrew_install.sh";
        "eval \"$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)\"";
        "eval $(scripts/active_protocols.sh)";
        "sed \"s|%%VERSION%%|0.0.0-dev| ; \
         s|%%CI_MERGE_REQUEST_SOURCE_PROJECT_URL%%|$CI_MERGE_REQUEST_SOURCE_PROJECT_URL|; \
         s|%%CI_COMMIT_REF_NAME%%|$CI_COMMIT_REF_NAME|; \
         s|%%CI_PROJECT_NAMESPACE%%|$CI_PROJECT_NAMESPACE|; \
         s|%%PROTO_CURRENT%%|$PROTO_CURRENT|; s|%%PROTO_NEXT%%|$PROTO_NEXT|\" \
         scripts/packaging/Formula/octez.rb.template > \
         scripts/packaging/Formula/octez.rb";
      ]
    [
      (* These packages are needed on Linux. For macOS, Homebrew will
         make those available locally. *)
      "apt install -y autoconf cmake libev-dev libffi-dev libgmp-dev \
       libprotobuf-dev libsqlite3-dev protobuf-compiler libhidapi-dev \
       pkg-config zlib1g-dev";
      "brew install -v scripts/packaging/Formula/octez.rb";
    ]
  |> enable_networked_cargo

let job_build_dynamic_binaries ?rules ~__POS__ ~arch ?(release = false)
    ?dependencies () =
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
        "src/proto_*/parameters/*.json";
        "_build/default/src/lib_protocol_compiler/bin/main_native.exe";
        "_build/default/tezt/tests/main.exe";
        "_build/default/contrib/octez_injector_server/octez_injector_server.exe";
      ]
  in
  let job =
    job
      ?rules
      ?dependencies
      ~__POS__
      ~stage:Stages.build
      ~arch
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
    |> enable_cargo_cache |> enable_sccache
  in
  (* Disable coverage for arm64 *)
  if arch = Amd64 then enable_coverage_instrumentation job else job

(** {2 Shared jobs} *)

let job_build_arm64_release ?rules () : tezos_job =
  job_build_dynamic_binaries ?rules ~__POS__ ~arch:Arm64 ~release:true ()

let job_build_arm64_exp_dev_extra ?rules () : tezos_job =
  job_build_dynamic_binaries ?rules ~__POS__ ~arch:Arm64 ~release:false ()
