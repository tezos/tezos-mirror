(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

module CI = Cacio.Make (struct
  let name = "octez-smart-rollup-node"

  let paths = ["src/**/*"]
end)

let octez_smart_rollup_node_release_tag_re =
  "/^octez-smart-rollup-node-v\\d+(\\.\\d+)?(?:\\-(rc|beta)\\d+)?$/"

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
    ?storage:(if arch = Arm64 then Some Ramfs else None)
    ~variables:
      [
        ("DOCKER_VERSION", Images.Base_images.docker_version);
        ("CI_DOCKER_HUB", match test with `test -> "false" | `real -> "true");
        ("DOCKER_BUILD_TARGET", "without-evm-artifacts");
        ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
        ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
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
    ~description:
      (sf "Build Octez Smart Rollup docker image for %s." arch_string)
    ~script:
      [
        "./scripts/ci/docker_initialize.sh --image-names";
        Tezos_ci_jobs.Docker.docker_release_script;
      ]

let job_build_static_binaries =
  Cacio.parameterize @@ fun arch ->
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  CI.job
    ("build:static-" ^ arch_string ^ "-binaries")
    ~__POS__
    ~description:
      (sf
         "Build the Octez smart rollup node static binaries for %s."
         arch_string)
    ~stage:Build
    ~arch
    ?cpu:(if arch = Amd64 then Some Very_high else None)
    ?storage:(if arch = Arm64 then Some Ramfs else None)
    ~image:Images.CI.build
    ~variables:
      [
        ("ARCH", arch_string);
        ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
        ("VERSION_EXECUTABLE", "octez-smart-rollup-node");
      ]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~artifacts:
      ((* Extend the lifespan to prevent failure for external tools using artifacts. *)
       artifacts
         ~expire_in:(Duration (Days 90))
         ["octez-binaries/$ARCH/*"])
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        "eval $(opam env)";
        "./scripts/ci/build_static_binaries.sh";
      ]

let job_docker_merge_manifests =
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

let release_page_variables = function
  | `test ->
      (* The S3_BUCKET and DISTRIBUTION_ID depend on the release type (tests
         or not). *)
      [
        ("S3_BUCKET", "release-page-test.nomadic-labs.com");
        ("DISTRIBUTION_ID", "E19JF46UG3Z747");
      ]
  | `real ->
      [
        ("S3_BUCKET", "site-prod.octez.tezos.com");
        ("URL", "octez.tezos.com");
        ("BUCKET_PATH", "/releases");
        ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
      ]

let job_deploy_release_page_assets =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "release-page-deploy-assets"
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Deploy the Octez Smart Rollup node release assets and versions.json. If \
       running in a test pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]."
    ~needs:
      [
        (Artifacts, job_build_static_binaries Amd64);
        (Artifacts, job_build_static_binaries Arm64);
      ]
    ~variables:(release_page_variables pipeline_type)
    ~script:
      [
        "eval $(opam env)";
        "./scripts/rollup_node/releases/deploy_release_page_assets.sh";
      ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

let job_release_page =
  Cacio.parameterize @@ fun pipeline_type ->
  Cacio.parameterize @@ fun wait_for ->
  CI.job
    "release-page-publish"
    ~__POS__
    ~image:Images.CI.release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Publish the Octez Smart Rollup node release page: regenerate \
       [index.html] from the published versions.json and upload it. Reflects \
       what has been deployed by \
       [octez-smart-rollup-node.release-page-deploy-assets]."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.md"; "index.html"])
    ~needs:
      (match wait_for with
      | `wait_for_nothing -> []
      | `wait_for_deploy ->
          [(Job, job_deploy_release_page_assets pipeline_type)])
    ~variables:(release_page_variables pipeline_type)
    ~script:
      [
        "eval $(opam env)";
        "./scripts/rollup_node/releases/publish_release_page.sh";
      ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

let job_gitlab_release =
  CI.job
    "gitlab:octez-smart-rollup-node-release"
    ~__POS__
    ~image:Images.Base_images.ci_release
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_static_binaries Amd64);
        (Artifacts, job_build_static_binaries Arm64);
      ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~description:"Create the gitlab release for the Octez Smart Rollup."
    ~script:["./scripts/rollup_node/releases/create_gitlab_release.sh"]

let register () =
  CI.register_dedicated_test_release_pipeline
    ~tag_rex:octez_smart_rollup_node_release_tag_re
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `test);
      (Auto, job_release_page `test `wait_for_deploy);
    ] ;
  CI.register_dedicated_release_pipeline
    ~tag_rex:octez_smart_rollup_node_release_tag_re
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `real);
      (Auto, job_release_page `real `wait_for_deploy);
    ] ;
  Cacio.register_release_jobs
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `real);
      (Manual, job_deploy_release_page_assets `real);
      (Auto, job_release_page `real `wait_for_deploy);
    ] ;
  Cacio.register_test_release_jobs
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `test);
      (Manual, job_deploy_release_page_assets `test);
      (Auto, job_release_page `test `wait_for_deploy);
    ] ;
  Cacio.register_jobs
    Non_release_tag
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `real);
    ] ;
  Cacio.register_jobs
    Non_release_tag_test
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `test);
    ] ;
  Cacio.register_jobs
    Publish_release_page
    [
      ( Manual,
        (* [wait_for_nothing]: this pipeline only regenerates the page from the
           already-published versions.json, so it neither deploys assets nor
           depends on the build jobs. *)
        job_release_page `real `wait_for_nothing );
    ] ;
  Cacio.register_jobs
    Test_publish_release_page
    [
      ( Manual,
        (* [wait_for_nothing]: this pipeline only regenerates the page from the
           already-published versions.json, so it neither deploys assets nor
           depends on the build jobs. *)
        job_release_page `test `wait_for_nothing );
    ]
