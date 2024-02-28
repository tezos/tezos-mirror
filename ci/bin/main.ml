(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint of CI-in-OCaml.

   Here we register the set of pipelines, stages and images and
   generate the top-level [.gitlab-ci.yml] file. *)

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
  let _trigger = Stage.register "trigger"

  let _sanity = Stage.register "sanity"

  let _build = Stage.register "build"

  let _test = Stage.register "test"

  let _test_coverage = Stage.register "test_coverage"

  let _packaging = Stage.register "packaging"

  let _doc = Stage.register "doc"

  let _prepare_release = Stage.register "prepare_release"

  let _publish_release_gitlab = Stage.register "publish_release_gitlab"

  let publish_release = Stage.register "publish_release"

  let _publish_package_gitlab = Stage.register "publish_package_gitlab"

  let _manual = Stage.register "manual"
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
      "registry.gitlab.com/${CI_PROJECT_PATH}/rust-toolchain" );
    ( "client_libs_dependencies_image_name",
      "registry.gitlab.com/${CI_PROJECT_PATH}/client-libs-dependencies" );
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

  let _runtime_build_test_dependencies =
    Image.register
      ~name:"runtime_build_test_dependencies"
      ~image_path:
        "${build_deps_image_name}:runtime-build-test-dependencies--${build_deps_image_version}"

  let _runtime_build_dependencies =
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
  let _alpine =
    Image.register ~name:"alpine" ~image_path:("alpine:" ^ alpine_version)
end

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

(** Helper to create jobs that uses the docker deamon.

    It:
    - Sets the appropriate image.
    - Activates the Docker daemon as a service.
    - It sets up authentification with docker registries *)
let job_docker_authenticated ?variables ~stage ~name script : job =
  let docker_version = "24.0.6" in
  job
    ~image:Images.docker
    ~variables:
      ([("DOCKER_VERSION", docker_version)] @ Option.value ~default:[] variables)
    ~before_script:["./scripts/ci/docker_initialize.sh"]
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~stage
    ~name
    script

let job_docker_promote_to_latest ~ci_docker_hub : job =
  job_docker_authenticated
    ~stage:Stages.publish_release
    ~name:"docker:promote_to_latest"
    ~variables:[("CI_DOCKER_HUB", Bool.to_string ci_docker_hub)]
    ["./scripts/ci/docker_promote_to_latest.sh"]

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
    If.(on_tezos_namespace && push && has_tag_match release_tag_re) ;
  register
    "beta_release_tag"
    If.(on_tezos_namespace && push && has_tag_match beta_release_tag_re) ;
  register
    "release_tag_test"
    If.(not_on_tezos_namespace && push && has_any_release_tag) ;
  register
    "non_release_tag"
    If.(on_tezos_namespace && push && has_non_release_tag) ;
  register
    "non_release_tag_test"
    If.(not_on_tezos_namespace && push && has_non_release_tag) ;
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
