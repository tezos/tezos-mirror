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
    ~image:Images_external.docker
    ~arch
    ?storage:(if arch = Arm64 then Some Ramfs else None)
    ~variables:
      [
        ("DOCKER_VERSION", "24.0.7");
        ("CI_DOCKER_HUB", match test with `test -> "false" | `real -> "true");
        ("DOCKER_BUILD_TARGET", "without-evm-artifacts");
        ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
        ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
      ]
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~description:
      (sf "Build Octez Smart Rollup docker image for %s." arch_string)
    ["./scripts/ci/docker_initialize.sh"; "./scripts/ci/docker_release.sh"]

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
    ~sccache:(Cacio.sccache ~cache_size:"2G" ())
    ~artifacts:
      ((* Extend the lifespan to prevent failure for external tools using artifacts. *)
       artifacts
         ~expire_in:(Duration (Days 90))
         ["octez-binaries/$ARCH/*"])
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

let job_release_page =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "release-page"
    ~__POS__
    ~image:Images.CI.build
    ~stage:Publish
    ~description:
      "A job to update the rollup node release page. If running in a test \
       pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]. Then its [index.html] is updated \
       accordingly."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.md"; "index.html"])
    ~needs:
      [
        (Artifacts, job_build_static_binaries Amd64);
        (Artifacts, job_build_static_binaries Arm64);
      ]
    ~variables:
      (match pipeline_type with
      | `test ->
          (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
          [
            ("S3_BUCKET", "release-page-test.nomadic-labs.com");
            ("DISTRIBUTION_ID", "E19JF46UG3Z747");
            ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
            ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
          ]
      | `real ->
          [
            ("S3_BUCKET", "site-prod.octez.tezos.com/releases");
            ("URL", "octez.tezos.com");
            ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
          ])
    [
      (* Required for creating the release page.*)
      "sudo apk add aws-cli pandoc";
      "eval $(opam env)";
      "./scripts/rollup_node/releases/publish_release_page.sh";
      (* Prepare artifacts. *)
      "cp /tmp/release_page*/index.md ./index.md";
    ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

let job_gitlab_release =
  CI.job
    "gitlab:octez-smart-rollup-node-release"
    ~__POS__
    ~image:Images.ci_release
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_static_binaries Amd64);
        (Artifacts, job_build_static_binaries Arm64);
      ]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~description:"Create the gitlab release for the Octez Smart Rollup."
    ["./scripts/rollup_node/releases/create_gitlab_release.sh"]

let register () =
  CI.register_dedicated_test_release_pipeline
    ~tag_rex:octez_smart_rollup_node_release_tag_re
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `test);
      (Auto, job_gitlab_release);
      (Manual, job_release_page `test);
    ] ;
  CI.register_dedicated_release_pipeline
    ~tag_rex:octez_smart_rollup_node_release_tag_re
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `real);
      (Auto, job_gitlab_release);
      (Manual, job_release_page `real);
    ] ;
  CI.register_global_release_jobs
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `real);
      (Manual, job_release_page `real);
    ] ;
  CI.register_global_test_release_jobs
    [
      (Auto, job_build_static_binaries Arm64);
      (Auto, job_build_static_binaries Amd64);
      (Auto, job_docker_merge_manifests `test);
      (Manual, job_release_page `test);
    ]
