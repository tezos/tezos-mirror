(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint of CI-in-OCaml.

   Here we register the set of pipelines, stages and images and
   generate the GitLab CI configuration file. *)

open Gitlab_ci
open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

(* Sets up the [default:] top-level configuration element. *)
let default = default ~interruptible:true ()

(* Define [stages:]

   The "manual" stage exists to fix a UI problem that occurs when mixing
   manual and non-manual jobs. *)
module Stages = struct
  let trigger = Stage.register "trigger"

  let _sanity = Stage.register "sanity"

  let build = Stage.register "build"

  let _test = Stage.register "test"

  let _test_coverage = Stage.register "test_coverage"

  let _packaging = Stage.register "packaging"

  let _doc = Stage.register "doc"

  let prepare_release = Stage.register "prepare_release"

  let publish_release_gitlab = Stage.register "publish_release_gitlab"

  let publish_release = Stage.register "publish_release"

  let publish_package_gitlab = Stage.register "publish_package_gitlab"

  let manual = Stage.register "manual"
end

(* Get the [build_deps_image_version] from the environment, which is
   typically set by sourcing [scripts/version.sh]. This is used to write
   [build_deps_image_version] in the top-level [variables:], used to
   specify the versions of the [build_deps] images. *)
let build_deps_image_version =
  match Sys.getenv_opt "opam_repository_tag" with
  | None ->
      failwith
        "Please set the environment variable [opam_repository_tag], by e.g. \
         sourcing [scripts/version.sh] before running."
  | Some v -> v

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

(* Top-level [variables:] *)
let variables : variables =
  [
    (* /!\ CI_REGISTRY is overriden to use a private Docker registry mirror in AWS ECR
       in GitLab namespaces `nomadic-labs` and `tezos`
       /!\ This value MUST be the same as `opam_repository_tag` in `scripts/version.sh` *)
    ("build_deps_image_version", build_deps_image_version);
    ("build_deps_image_name", "${CI_REGISTRY}/tezos/opam-repository");
    ( "rust_toolchain_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/rust-toolchain" );
    ( "client_libs_dependencies_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/client-libs-dependencies" );
    ("GIT_STRATEGY", "fetch");
    ("GIT_DEPTH", "1");
    ("GET_SOURCES_ATTEMPTS", "2");
    ("ARTIFACT_DOWNLOAD_ATTEMPTS", "2");
    (* Sets the number of tries before failing opam downloads. *)
    ("OPAMRETRIES", "5");
    (* An addition to working around a bug in gitlab-runner's default
       unzipping implementation
       (https://gitlab.com/gitlab-org/gitlab-runner/-/issues/27496),
       this setting cuts cache creation time. *)
    ("FF_USE_FASTZIP", "true");
    (* If RUNTEZTALIAS is true, then Tezt tests are included in the
       @runtest alias. We set it to false to deactivate these tests in
       the unit test jobs, as they already run in the Tezt jobs. It is
       set to true in the opam jobs where we want to run the tests
       --with-test. *)
    ("RUNTEZTALIAS", "false");
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6764
       "false" is the GitLab default but we've overridden it in the runner settings.
       This should be fixed at the runner level but we reset it to the
       default here in the meantime. *)
    ("FF_KUBERNETES_HONOR_ENTRYPOINT", "false");
  ]

(* Register images.

   The set of registered images are written to
   [.gitlab/ci/jobs/shared/images.yml] for interoperability with
   hand-written .yml files.

   For documentation on the [runtime_X_dependencies] and the
   [rust_toolchain] images, refer to
   {{:https://gitlab.com/tezos/opam-repository/}
   tezos/opam-repository}. *)
module Images = struct
  let _runtime_e2etest_dependencies =
    Image.register
      ~name:"runtime_e2etest_dependencies"
      ~image_path:
        "${build_deps_image_name}:runtime-e2etest-dependencies--${build_deps_image_version}"

  let runtime_build_test_dependencies =
    Image.register
      ~name:"runtime_build_test_dependencies"
      ~image_path:
        "${build_deps_image_name}:runtime-build-test-dependencies--${build_deps_image_version}"

  let runtime_build_dependencies =
    Image.register
      ~name:"runtime_build_dependencies"
      ~image_path:
        "${build_deps_image_name}:runtime-build-dependencies--${build_deps_image_version}"

  let _runtime_prebuild_dependencies =
    Image.register
      ~name:"runtime_prebuild_dependencies"
      ~image_path:
        "${build_deps_image_name}:runtime-prebuild-dependencies--${build_deps_image_version}"

  let _client_libs_dependencies =
    Image.register
      ~name:"client_libs_dependencies"
      ~image_path:
        "${client_libs_dependencies_image_name}:${client_libs_dependencies_image_tag}"

  let _rust_toolchain =
    (* Warning: we are relying on ill-specified behavior from GitLab that allows
       the expansion of dotenv variables (here: $rust_toolchain_image_tag) in
       the image field.
       See: https://gitlab.com/gitlab-org/gitlab-runner/-/issues/37361. *)
    Image.register
      ~name:"rust_toolchain"
      ~image_path:"${rust_toolchain_image_name}:${rust_toolchain_image_tag}"

  (* Match GitLab executors version and directly use the Docker socket
     The Docker daemon is already configured, experimental features are enabled
     The following environment variables are already set:
     - [BUILDKIT_PROGRESS]
     - [DOCKER_DRIVER]
     - [DOCKER_VERSION]
     For more info, see {{:https://docs.gitlab.com/ee/ci/docker/using_docker_build.html#use-docker-socket-binding}} here.

     This image is defined in {{:https://gitlab.com/tezos/docker-images/ci-docker}tezos/docker-images/ci-docker}. *)
  let docker =
    Image.register
      ~name:"docker"
      ~image_path:"${CI_REGISTRY}/tezos/docker-images/ci-docker:v1.9.0"

  (* The Alpine version should be kept up to date with the version
     used for the [build_deps_image_name] images and specified in the
     variable [alpine_version] in [scripts/version.sh]. This is
     checked by the jobs [trigger] and [sanity_ci]. *)
  let alpine =
    Image.register ~name:"alpine" ~image_path:("alpine:" ^ alpine_version)

  let debian_bookworm =
    Image.register ~name:"debian_bookworm" ~image_path:"debian:bookworm"

  let fedora_39 = Image.register ~name:"fedora_39" ~image_path:"fedora:39"

  let ci_release =
    Image.register
      ~name:"ci_release"
      ~image_path:"${CI_REGISTRY}/tezos/docker-images/ci-release:v1.1.0"
end

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

let changeset_octez =
  [
    "src/**/*";
    "etherlink/**/*";
    "tezt/**/*";
    ".gitlab/**/*";
    ".gitlab-ci.yml";
    "michelson_test_scripts/**/*";
    "tzt_reference_test_suite/**/*";
  ]

let changeset_octez_or_kernels =
  ["images/**/*"; "scripts/ci/**/*"; "kernels.mk"; "etherlink.mk"]
  @ changeset_octez

(* Dummy job.

   This fixes the "configuration must contain at least one
   visible job" error in GitLab when using includes.

   For more info, see: https://gitlab.com/gitlab-org/gitlab/-/issues/341693 *)
let job_dummy : job =
  Util.job
    ~rules:
      [job_rule ~if_:If.(var "foo" == str "bar" && var "foo" != str "bar") ()]
    ~name:"dummy_job"
    ~script:[{|echo "This job will never execute"|}]
    ()

(* Define the [trigger] job

   §1: The purpose of this job is to launch the CI manually in certain cases.
   The objective is not to run computing when it is not
   necessary and the decision to do so belongs to the developer

   §2: We also perform some fast sanity checks. *)
let trigger =
  job_external
  @@ job
       ~image:Images.alpine
       ~stage:Stages.trigger
       ~allow_failure:No
       ~rules:
         [
           job_rule
             ~if_:(If.not Rules.assigned_to_marge_bot)
             ~allow_failure:No
             ~when_:Manual
             ();
           job_rule ~when_:Always ();
         ]
       ~timeout:(Minutes 10)
       ~name:"trigger"
       [
         "echo 'Trigger pipeline!'";
         (* Check that [.gitlab-ci.yml]'s [build_deps_image_version] and
            [scripts/version.sh]'s [opam_repository_tag] are the same. *)
         "./scripts/ci/check_opam_repository_tag.sh";
         (* Check that the Alpine version of the trigger job's image
            corresponds to the value in scripts/version.sh. *)
         "./scripts/ci/check_alpine_version.sh";
       ]

(** Helper to create jobs that uses the docker deamon.

    It:
    - Sets the appropriate image.
    - Activates the Docker daemon as a service.
    - It sets up authentification with docker registries *)
let job_docker_authenticated ?(skip_docker_initialization = false) ?artifacts
    ?variables ?rules ?dependencies ?arch ?when_ ?allow_failure ~stage ~name
    script : Tezos_ci.tezos_job =
  let docker_version = "24.0.6" in
  job
    ?rules
    ?dependencies
    ?artifacts
    ?arch
    ?when_
    ?allow_failure
    ~image:Images.docker
    ~variables:
      ([("DOCKER_VERSION", docker_version)] @ Option.value ~default:[] variables)
    ~before_script:
      (if not skip_docker_initialization then
       ["./scripts/ci/docker_initialize.sh"]
      else [])
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~stage
    ~name
    script

let job_docker_promote_to_latest ~ci_docker_hub : tezos_job =
  job_docker_authenticated
    ~stage:Stages.publish_release
    ~name:"docker:promote_to_latest"
    ~variables:[("CI_DOCKER_HUB", Bool.to_string ci_docker_hub)]
    ["./scripts/ci/docker_promote_to_latest.sh"]

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
let job_build_static_binaries ~arch ?(release = false) ?(needs_trigger = false)
    ?rules () : Tezos_ci.tezos_job =
  let arch_string =
    match arch with Tezos_ci.Amd64 -> "x86_64" | Arm64 -> "arm64"
  in
  let name = "oc.build:static-" ^ arch_string ^ "-linux-binaries" in
  let artifacts =
    (* Extend the lifespan to prevent failure for external tools using artifacts. *)
    let expire_in = if release then Some (Days 90) else None in
    artifacts ?expire_in ["octez-binaries/$ARCH/*"]
  in
  let executable_files =
    "script-inputs/released-executables"
    ^ if not release then " script-inputs/experimental-executables" else ""
  in
  let dependencies =
    (* Even though not many tests depend on static executables, some
       of those that do are limiting factors in the total duration of
       pipelines. So when requested through [needs_trigger] we start
       this job as early as possible, without waiting for
       sanity_ci. *)
    if needs_trigger then Dependent [Optional trigger] else Staged []
  in
  job
    ?rules
    ~stage:Stages.build
    ~arch
    ~name
    ~image:Images.runtime_build_dependencies
    ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
    ~variables:[("ARCH", arch_string); ("EXECUTABLE_FILES", executable_files)]
    ~dependencies
    ~artifacts
    ["./scripts/ci/build_static_binaries.sh"]

let rules_static_build_master = [job_rule ~when_:Always ()]

let rules_static_build_other = [job_rule ~changes:changeset_octez ()]

let _job_static_arm64_experimental =
  job_external ~filename_suffix:"experimental"
  @@ job_build_static_binaries ~arch:Arm64 ~rules:rules_static_build_other ()

let _job_static_arm64_master =
  job_external ~filename_suffix:"master"
  @@ job_build_static_binaries ~arch:Arm64 ~rules:rules_static_build_master ()

let _job_static_x86_64_experimental =
  job_external ~filename_suffix:"experimental"
  @@ job_build_static_binaries
       ~arch:Amd64
       ~needs_trigger:true
       ~rules:rules_static_build_other
       ()

let _job_static_x86_64_master =
  job_external ~filename_suffix:"master"
  @@ job_build_static_binaries
       ~arch:Amd64
         (* TODO: this job doesn't actually need trigger and there is no
            need to set it optional since we know this job is only on the master branch. *)
       ~needs_trigger:true
       ~rules:rules_static_build_master
       ()

let job_docker_rust_toolchain ?rules ?dependencies () =
  job_docker_authenticated
    ?rules
    ?dependencies
    ~skip_docker_initialization:true
    ~stage:Stages.build
    ~name:"oc.docker:rust-toolchain"
    ~variables:[("CI_DOCKER_HUB", "false")]
    ~artifacts:
      (artifacts
         ~reports:(reports ~dotenv:"rust_toolchain_image_tag.env" ())
         [])
    ["./scripts/ci/docker_rust_toolchain_build.sh"]

let _job_docker_rust_toolchain_before_merging =
  job_external ~filename_suffix:"before_merging"
  @@ job_docker_rust_toolchain
       ~dependencies:(Dependent [Optional trigger])
       ~rules:
         [
           job_rule ~changes:changeset_octez_or_kernels ~when_:On_success ();
           job_rule ~when_:Manual ();
         ]
       ()

let _job_docker_rust_toolchain_master =
  job_external ~filename_suffix:"master"
  @@ job_docker_rust_toolchain ~rules:[job_rule ~when_:Always ()] ()

let _job_docker_rust_toolchain_other =
  job_external ~filename_suffix:"other" @@ job_docker_rust_toolchain ()

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
    - [Test_manual] Docker builds are triggered manually, put in the stage
      [manual] and their failure is allowed. The other types are in the build
      stage, run [on_success] and are not allowed to fail. *)
type docker_build_type = Experimental | Release | Test | Test_manual

(** Creates a Docker build job of the given [arch] and [docker_build_type].

    If [external_] is set to true (default [false]), then the job is
    also written to an external file. *)
let job_docker_build ?rules ?dependencies ~arch ?(external_ = false)
    docker_build_type : Tezos_ci.tezos_job =
  let arch_string =
    match arch with Tezos_ci.Amd64 -> "amd64" | Arm64 -> "arm64"
  in
  let variables =
    [
      ( "DOCKER_BUILD_TARGET",
        match (arch, docker_build_type) with
        | Amd64, (Test_manual | Experimental) -> "with-evm-artifacts"
        | _ -> "without-evm-artifacts" );
      ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
      ( "CI_DOCKER_HUB",
        Bool.to_string
          (match docker_build_type with
          | Release | Experimental -> true
          | Test | Test_manual -> false) );
      ( "EXECUTABLE_FILES",
        match docker_build_type with
        | Release -> "script-inputs/released-executables"
        | Test | Test_manual | Experimental ->
            "script-inputs/released-executables \
             script-inputs/experimental-executables" );
    ]
  in
  let stage, when_, (allow_failure : allow_failure_job option) =
    match docker_build_type with
    | Test_manual -> (Stages.manual, Some Manual, Some Yes)
    | _ -> (Stages.build, None, None)
  in
  let name = "oc.docker:" ^ arch_string in
  let filename_suffix =
    match docker_build_type with
    | Release -> "release"
    | Experimental -> "experimental"
    | Test -> "test"
    | Test_manual -> "test_manual"
  in
  let job =
    job_docker_authenticated
      ?when_
      ?allow_failure
      ?rules
      ?dependencies
      ~stage
      ~arch
      ~name
      ~variables
      ["./scripts/ci/docker_release.sh"]
  in
  if external_ then job_external ~directory:"build" ~filename_suffix job
  else job

let changeset_octez_docker_changes_or_master =
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
    ".gitlab/**/*";
    ".gitlab-ci.yml";
  ]

let rules_octez_docker_changes_or_master =
  [
    job_rule ~if_:Rules.on_master ~when_:Always ();
    job_rule ~changes:changeset_octez_docker_changes_or_master ();
  ]

let job_docker_amd64_experimental : Tezos_ci.tezos_job =
  job_docker_build
    ~external_:true
      (* TODO: when this is generated for a given pipeline, then the correct variant of [_job_docker_rust_toolchain_*] must be set.
         For now we can set any variant to get the correct name of the need. *)
    ~dependencies:(Dependent [Artifacts _job_docker_rust_toolchain_master])
    ~rules:rules_octez_docker_changes_or_master
    ~arch:Amd64
    Experimental

let _job_docker_amd64_test_manual : Tezos_ci.tezos_job =
  job_docker_build
    ~external_:true (* TODO: see above *)
    ~dependencies:
      (Dependent [Artifacts _job_docker_rust_toolchain_before_merging])
    ~arch:Amd64
    Test_manual

let job_docker_arm64_experimental : Tezos_ci.tezos_job =
  job_docker_build
    ~external_:true (* TODO: see above *)
    ~dependencies:(Dependent [Artifacts _job_docker_rust_toolchain_master])
    ~rules:rules_octez_docker_changes_or_master
    ~arch:Arm64
    Experimental

let _job_docker_arm64_test_manual : Tezos_ci.tezos_job =
  job_docker_build
    ~external_:true (* TODO: see above *)
    ~dependencies:
      (Dependent [Artifacts _job_docker_rust_toolchain_before_merging])
    ~arch:Arm64
    Test_manual

(* Note: here we rely on [$IMAGE_ARCH_PREFIX] to be empty.
   Otherwise, [$DOCKER_IMAGE_TAG] would contain [$IMAGE_ARCH_PREFIX] too.
   [$IMAGE_ARCH_PREFIX] is only used when building Docker images,
   here we handle all architectures so there is no such variable. *)
let job_docker_merge_manifests ~ci_docker_hub ~job_docker_amd64
    ~job_docker_arm64 : Tezos_ci.tezos_job =
  job_docker_authenticated
    ~stage:Stages.prepare_release
    ~name:"docker:merge_manifests"
      (* This job merges the images produced in the jobs
         [docker:{amd64,arm64}] into a single multi-architecture image, and
         so must be run after these jobs. *)
    ~dependencies:(Dependent [Job job_docker_amd64; Job job_docker_arm64])
    ~variables:[("CI_DOCKER_HUB", Bool.to_string ci_docker_hub)]
    ["./scripts/ci/docker_merge_manifests.sh"]

let _job_docker_merge_manifests_release =
  job_external ~filename_suffix:"release"
  @@ job_docker_merge_manifests
       ~ci_docker_hub:true
         (* TODO: In theory, actually uses either release or
            experimental variant of docker jobs depending on
            pipeline. In practice, this does not matter as these jobs
            have the same name in the generated files
            ([oc.build:ARCH]). However, when the merge_manifest jobs
            are created directly in the appropriate pipeline, the
            correcty variant must be used. *)
       ~job_docker_amd64:job_docker_amd64_experimental
       ~job_docker_arm64:job_docker_arm64_experimental

type bin_package_target = Dpkg | Rpm

let job_build_bin_package ?rules ~name ?(stage = Stages.build) ~arch ~target ()
    : Tezos_ci.tezos_job =
  let arch_string =
    match arch with Tezos_ci.Amd64 -> "amd64" | Arm64 -> "arm64"
  in
  let target_string = match target with Dpkg -> "dpkg" | Rpm -> "rpm" in
  let image =
    match target with Dpkg -> Images.debian_bookworm | Rpm -> Images.fedora_39
  in
  let artifacts =
    let artifact_path =
      "octez-*." ^ match target with Dpkg -> "deb" | Rpm -> "rpm"
    in
    artifacts
      ~expire_in:(Days 1)
      ~when_:On_success
      ~name:"${TARGET}-$ARCH-$CI_COMMIT_REF_SLUG"
      [artifact_path]
  in
  let before_script =
    ". ./scripts/version.sh"
    ::
    (match target with
    | Dpkg ->
        [
          "apt update";
          "apt-get install -y rsync git m4 build-essential patch unzip wget \
           opam jq bc autoconf cmake libev-dev libffi-dev libgmp-dev \
           libhidapi-dev pkg-config zlib1g-dev libprotobuf-dev \
           protobuf-compiler libsqlite3-dev";
        ]
    | Rpm ->
        [
          "dnf update -y";
          "dnf install -y libev-devel gmp-devel hidapi-devel libffi-devel \
           zlib-devel libpq-devel m4 perl git pkg-config rpmdevtools \
           python3-devel python3-setuptools wget opam rsync which cargo \
           autoconf mock systemd systemd-rpm-macros cmake python3-wheel \
           python3-tox-current-env gcc-c++ protobuf-compiler protobuf-devel \
           sqlite-devel";
        ])
  in
  job
    ?rules
    ~name
    ~arch
    ~image
    ~stage
    ~dependencies:(Dependent [])
    ~variables:
      [
        ("TARGET", target_string);
        ("OCTEZ_PKGMAINTAINER", "nomadic-labs");
        ("BLST_PORTABLE", "yes");
        ("ARCH", arch_string);
      ]
    ~artifacts
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
    ]

let job_build_dpkg_amd64 =
  job_external
  @@ job_build_bin_package
       ~name:"oc.build:dpkg:amd64"
       ~target:Dpkg
       ~arch:Tezos_ci.Amd64
       ()

let job_build_rpm_amd64 =
  job_external
  @@ job_build_bin_package
       ~name:"oc.build:rpm:amd64"
       ~target:Rpm
       ~arch:Tezos_ci.Amd64
       ()

let _job_build_dpkg_amd64_manual =
  job_external ~directory:"build" ~filename_suffix:"manual"
  @@ job_build_bin_package
       ~name:"oc.build:dpkg:amd64"
       ~target:Dpkg
       ~arch:Tezos_ci.Amd64
       ~rules:[job_rule ~when_:Manual ()]
       ~stage:Stages.manual
       ()

let _job_build_rpm_amd64_manual =
  job_external ~directory:"build" ~filename_suffix:"manual"
  @@ job_build_bin_package
       ~rules:[job_rule ~when_:Manual ()]
       ~name:"oc.build:rpm:amd64"
       ~target:Rpm
       ~arch:Tezos_ci.Amd64
       ~stage:Stages.manual
       ()

(** Type of release tag pipelines.

    The semantics of the type is summed up in this table:

   |                       | Release_tag | Beta_release_tag | Non_release_tag |
   |-----------------------+-------------+------------------+-----------------|
   | GitLab release type   | Release     | Release          | Create          |
   | Experimental binaries | No          | No               | No              |
   | Docker build type     | Release     | Release          | Release         |
   | Publishes to opam     | Yes         | No               | No              |

    - All release tag pipelines types publish [Release] type Docker builds.
    - No release tag pipelines include experimental binaries.
    - [Release_tag] and [Beta_release_tag] pipelines creates GitLab
    and publishes releases. [Non_release_tag] pipelines creates the
    GitLab release but do not publish them.
    - Only [Release_tag] pipelines publish to opam. *)
type release_tag_pipeline_type =
  | Release_tag
  | Beta_release_tag
  | Non_release_tag

(** Create a release tag pipeline of type {!release_tag_pipeline_type}.

    If [test] is true (default is [false]), then the Docker images are
    built of the [Test] type and are published to the GitLab registry
    instead of Docker hub. *)
let release_tag_pipeline ?(test = false) release_tag_pipeline_type =
  let job_docker_rust_toolchain = job_docker_rust_toolchain () in
  let job_docker_amd64 =
    job_docker_build
      ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
      ~arch:Amd64
      (if test then Test else Release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
      ~arch:Arm64
      (if test then Test else Release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  let job_static_arm64_release =
    job_build_static_binaries ~arch:Arm64 ~release:true ()
  in
  let job_static_x86_64_release =
    job_build_static_binaries ~arch:Amd64 ~release:true ~needs_trigger:true ()
  in
  let job_gitlab_release ~dependencies : Tezos_ci.tezos_job =
    job
      ~image:Images.ci_release
      ~stage:Stages.publish_release_gitlab
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:release"
      [
        "./scripts/ci/restrict_export_to_octez_source.sh";
        "./scripts/ci/gitlab-release.sh";
      ]
  in
  let job_gitlab_publish ~dependencies : Tezos_ci.tezos_job =
    job
      ~image:Images.ci_release
      ~stage:Stages.publish_package_gitlab
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:publish"
      ["${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"]
  in
  let job_gitlab_release_or_publish =
    let dependencies =
      Dependent
        [
          Artifacts job_static_x86_64_release;
          Artifacts job_static_arm64_release;
          Artifacts job_build_dpkg_amd64;
          Artifacts job_build_rpm_amd64;
        ]
    in
    match release_tag_pipeline_type with
    | Non_release_tag -> job_gitlab_publish ~dependencies
    | _ -> job_gitlab_release ~dependencies
  in
  let job_opam_release : Tezos_ci.tezos_job =
    job
      ~image:Images.runtime_build_test_dependencies
      ~stage:Stages.publish_release
      ~interruptible:false
      ~name:"opam:release"
      ["./scripts/ci/opam-release.sh"]
  in
  [
    job_docker_rust_toolchain;
    job_static_x86_64_release;
    job_static_arm64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_build_dpkg_amd64;
    job_build_rpm_amd64;
    job_docker_merge;
    job_gitlab_release_or_publish;
  ]
  @
  match (test, release_tag_pipeline_type) with
  | false, Release_tag -> [job_opam_release]
  | _ -> []

(* Register pipelines types. Pipelines types are used to generate
   workflow rules and includes of the files where the jobs of the
   pipeline is defined. At the moment, all these pipelines are defined
   manually in .yml, but will eventually be generated. *)
let () =
  (* Matches release tags, e.g. [v1.2.3] or [v1.2.3-rc4]. *)
  let release_tag_re = "/^v\\d+\\.\\d+(?:\\-rc\\d+)?$/" in
  (* Matches beta release tags, e.g. [v1.2.3-beta5]. *)
  let beta_release_tag_re = "/^v\\d+\\.\\d+\\-beta\\d*$/" in
  let open Rules in
  let open Pipeline in
  (* Matches either release tags or beta release tags, e.g. [v1.2.3],
     [v1.2.3-rc4] or [v1.2.3-beta5]. *)
  let has_any_release_tag =
    If.(has_tag_match release_tag_re || has_tag_match beta_release_tag_re)
  in
  let has_non_release_tag =
    If.(Predefined_vars.ci_commit_tag != null && not has_any_release_tag)
  in
  register "before_merging" If.(on_tezos_namespace && merge_request) ;
  register
    "latest_release"
    ~jobs:[job_docker_promote_to_latest ~ci_docker_hub:true]
    If.(on_tezos_namespace && push && on_branch "latest-release") ;
  register
    "latest_release_test"
    If.(not_on_tezos_namespace && push && on_branch "latest-release-test")
    ~jobs:[job_docker_promote_to_latest ~ci_docker_hub:false] ;
  register "master_branch" If.(on_tezos_namespace && push && on_branch "master") ;
  register
    "release_tag"
    If.(on_tezos_namespace && push && has_tag_match release_tag_re)
    ~jobs:(release_tag_pipeline Release_tag) ;
  register
    "beta_release_tag"
    If.(on_tezos_namespace && push && has_tag_match beta_release_tag_re)
    ~jobs:(release_tag_pipeline Beta_release_tag) ;
  register
    "release_tag_test"
    If.(not_on_tezos_namespace && push && has_any_release_tag)
    ~jobs:(release_tag_pipeline ~test:true Release_tag) ;
  register
    "non_release_tag"
    If.(on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(release_tag_pipeline Non_release_tag) ;
  register
    "non_release_tag_test"
    If.(not_on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(release_tag_pipeline ~test:true Non_release_tag) ;
  register "schedule_extended_test" schedule_extended_tests

(* Split pipelines and writes image templates *)
let config =
  (* Split pipelines types into workflow and includes *)
  let workflow, includes = Pipeline.workflow_includes () in
  (* Write image templates.

     This is a temporary stop-gap and only necessary for jobs that are
     not define in OCaml. Once all jobs have been migrated, this can
     be removed. *)
  let image_templates_include =
    let filename = ".gitlab/ci/jobs/shared/images.yml" in
    let image_template (name, image_path) : string * Yaml.value =
      let name = ".image_template__" ^ name in
      (name, `O [("image", `String (Image.name image_path))])
    in
    let config : Yaml.value = `O (List.map image_template (Image.all ())) in
    Base.write_yaml ~header:Tezos_ci.header filename config ;
    {local = filename; rules = []}
  in
  let includes =
    image_templates_include
    :: {local = ".gitlab/ci/jobs/shared/templates.yml"; rules = []}
    :: includes
  in
  Pipeline.write () ;
  [
    Workflow workflow;
    Default default;
    Variables variables;
    Stages (Stage.to_string_list ());
    Job job_dummy;
    Include includes;
  ]

let () =
  let filename = Base.(project_root // ".gitlab-ci.yml") in
  To_yaml.to_file ~header:Tezos_ci.header ~filename config
