(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the pipelines scanning Docker
   images for vulnerability detection.

   These pipelines run daily on the [master] branch. *)

open Tezos_ci

(* Scans a Docker image to detect CVEs. Based on a template provided by Gitlab. *)
let job_container_scanning ?(dockerfile_path = "build.Dockerfile") docker_image
    : tezos_job =
  job
    ~__POS__
    ~name:"container_scanning"
    ~stage:Stages.scan
    ~template:Jobs_container_scanning
    ~variables:
      [
        ("CS_IMAGE", docker_image);
        ("SECURE_LOG_LEVEL", "debug");
        ("CS_DOCKERFILE_PATH", dockerfile_path);
      ]
    ~description:(Format.sprintf "Scanning image %s" docker_image)
    ~git_strategy:Fetch
    ["gtcs scan"]

let jobs ?dockerfile_path docker_image : tezos_job list =
  [
    Common.job_datadog_pipeline_trace;
    job_container_scanning ?dockerfile_path docker_image;
  ]
