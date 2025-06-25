(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Tezos_ci

let debian_matrix = [[("RELEASE", ["unstable"; "bookworm"])]]

let ubuntu_matrix = [[("RELEASE", ["noble"; "jammy"])]]

let jobs =
  let make_job_base_images ~__POS__ ~name ~matrix ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:[("DISTRIBUTION", distribution)]
      ~parallel:(Matrix matrix)
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
      ~tag:Gcp_very_high_cpu
      ["scripts/ci/build-base-images.sh images/base-images/Dockerfile.debian"]
  in
  let job_debian_based_images =
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.debian"
      ~distribution:"debian"
      ~matrix:debian_matrix
  in
  let job_ubuntu_based_images =
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.ubuntu"
      ~distribution:"ubuntu"
      ~matrix:ubuntu_matrix
  in
  [job_debian_based_images; job_ubuntu_based_images]

let child_pipeline =
  Pipeline.register_child
    "base_images"
    ~description:"Build CI base images"
    ~jobs:(job_datadog_pipeline_trace :: jobs)
