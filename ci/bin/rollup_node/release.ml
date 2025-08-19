(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache

let prefix = "rollup-node"

let job ~name = job ~name:(prefix ^ "." ^ name)

let job_docker_authenticated ~name =
  job_docker_authenticated ~name:(prefix ^ "." ^ name)

(** TODO: These jobs should be definied in [lib_tezos_ci]. *)

(** Creates a Docker build job of the given [arch]. *)
let job_docker_build ~__POS__ ~arch ~test ?storage () : tezos_job =
  let arch_string = Runner.Arch.show_uniform arch in
  let variables =
    [
      ("DOCKER_BUILD_TARGET", "without-evm-artifacts");
      ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
      ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
    ]
  in
  let name = "docker:" ^ arch_string in
  job_docker_authenticated
    ~ci_docker_hub:(not test)
    ~image_dependencies:[Images.CI.runtime]
    ~__POS__
    ~stage:Stages.build
    ~arch
    ?storage
    ~name
    ~variables
    ["./scripts/ci/docker_release.sh"]

let job_build_static_binaries ~__POS__ ~arch ?cpu ?storage () : tezos_job =
  let arch_string = Runner.Arch.show_easy_to_distinguish arch in
  let name = "build:static-" ^ arch_string ^ "-binaries" in
  let artifacts =
    (* Extend the lifespan to prevent failure for external tools using artifacts. *)
    let expire_in = Some (Duration (Days 90)) in
    artifacts ?expire_in ["octez-binaries/$ARCH/*"]
  in
  job
    ~__POS__
    ~stage:Stages.build
    ~arch
    ?cpu
    ?storage
    ~name
    ~image:Images.CI.build
    ~before_script:["./scripts/ci/take_ownership.sh"; "eval $(opam env)"]
    ~variables:
      [
        ("ARCH", arch_string);
        ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
        ("VERSION_EXECUTABLE", "octez-smart-rollup-node");
      ]
    ~artifacts
    ["./scripts/ci/build_static_binaries.sh"]
  |> enable_cargo_cache
  |> enable_sccache ~cache_size:"2G"
  |> enable_cargo_target_caches

let job_docker_merge_manifests ~__POS__ ~ci_docker_hub ~job_docker_amd64
    ~job_docker_arm64 : tezos_job =
  job_docker_authenticated
    ~__POS__
    ~stage:Stages.publish
    ~name:"docker:merge_manifests"
      (* This job merges the images produced in the jobs
         [docker:{amd64,arm64}] into a single multi-architecture image, and
         so must be run after these jobs. *)
    ~dependencies:(Dependent [Job job_docker_amd64; Job job_docker_arm64])
    ~ci_docker_hub
    ["./scripts/ci/docker_merge_manifests.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~tag:Gcp_not_interruptible

let job_static_x86_64_release =
  job_build_static_binaries ~__POS__ ~arch:Amd64 ~cpu:Very_high ()

let job_static_arm64_release =
  job_build_static_binaries ~__POS__ ~arch:Arm64 ~storage:Ramfs ()

let job_release_page ~test () =
  job
    ~__POS__
    ~image:Images.CI.build
    ~stage:Stages.publish
    ~description:
      "A job to update the rollup node release page. If running in a test \
       pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]. Then its [index.html] is updated \
       accordingly."
    ~name:"publish:release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["index.md"; "index.html"])
    ~dependencies:
      (Dependent
         [
           Artifacts job_static_x86_64_release;
           Artifacts job_static_arm64_release;
         ])
    ~before_script:
      [
        (* Required for creating the release page.*)
        "sudo apk add aws-cli pandoc";
        "eval $(opam env)";
      ]
    ~after_script:["cp /tmp/release_page*/index.md ./index.md"]
    ~variables:
      (if test then
         (* The S3_BUCKET, AWS keys and DISTRIBUTION_ID
            depends on the release type (tests or not). *)
         [
           ("S3_BUCKET", "release-page-test.nomadic-labs.com");
           ("DISTRIBUTION_ID", "E19JF46UG3Z747");
           ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
           ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
         ]
       else
         [
           ("S3_BUCKET", "site-prod.octez.tezos.com/releases");
           ("URL", "octez.tezos.com");
           ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
         ])
    ["./scripts/rollup_node/releases/publish_release_page.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

let jobs ?(test = false) () =
  let job_docker_amd64 = job_docker_build ~__POS__ ~arch:Amd64 ~test () in
  let job_docker_arm64 =
    job_docker_build ~__POS__ ~arch:Arm64 ~storage:Ramfs ~test ()
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  let job_gitlab_release : Tezos_ci.tezos_job =
    let dependencies =
      Dependent
        [
          Artifacts job_static_x86_64_release; Artifacts job_static_arm64_release;
        ]
    in
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:octez-smart-rollup-node-release"
      ["./scripts/rollup_node/releases/create_gitlab_release.sh"]
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ~tag:Gcp_not_interruptible
  in
  [
    Tezos_ci.job_datadog_pipeline_trace;
    job_static_arm64_release;
    job_static_x86_64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_docker_merge;
    job_gitlab_release;
    job_release_page ~test ();
  ]
