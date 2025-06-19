(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

let prefix = "rollup"

(** TODO: These jobs should be definied in [lib_tezos_ci]. *)

(** Creates a Docker build job of the given [arch]. *)
let job_docker_build ~__POS__ ~arch ~test ?storage () : tezos_job =
  let arch_string = arch_to_string_alt arch in
  let variables =
    [
      ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
      ("EXECUTABLE_FILES", "script-inputs/smart-rollup-node-executable");
    ]
  in
  let name = "docker:" ^ arch_string in
  job_docker_authenticated
    ~ci_docker_hub:(not test)
    ~__POS__
    ~stage:Stages.build
    ~arch
    ?storage
    ~name
    ~variables
    ["./scripts/ci/docker_release.sh"]

let job_build_static_binaries ~__POS__ ~arch ?storage () : tezos_job =
  let arch_string = arch_to_string arch in
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
    ?storage
    ~name
    ~image:Images.CI.build
    ~before_script:["./scripts/ci/take_ownership.sh"; "eval $(opam env)"]
    ~variables:[("ARCH", arch_string)]
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
  let job_static_x86_64_release =
    job_build_static_binaries ~__POS__ ~arch:Amd64 ()
  in
  let job_static_arm64_release =
    job_build_static_binaries ~__POS__ ~arch:Arm64 ~storage:Ramfs ()
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
      ["./scripts/ci/create_gitlab_octez_smart_rollup_node_release.sh"]
  in
  [
    (* Stage: build *)
    job_static_arm64_release;
    job_static_x86_64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_docker_merge;
    job_gitlab_release;
  ]
