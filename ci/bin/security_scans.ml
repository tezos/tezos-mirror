(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs related to security scans.
   - Vulnerability detection in Docker images. *)

open Gitlab_ci.Util
open Tezos_ci

let jobs : tezos_job list =
  let image_tags = ["latest"; "octez-evm-node-latest"; "master"] in

  (* Scans [docker_image:docker_tag] image. A scanning report artifact
     is produced.
     We adapt the template provided by Trivy:
     https://trivy.dev/v0.63/tutorials/integrations/gitlab-ci/#gitlab-ci-using-trivy-container *)
  let job_container_scanning ?(docker_image = "tezos/tezos") docker_tag :
      tezos_job =
    let full_image_name = docker_image ^ ":" ^ docker_tag in
    let report = "gl-container-scanning-report-" ^ docker_tag ^ ".json" in
    job
      ~__POS__
      ~name:("container_scanning_" ^ docker_tag)
      ~description:("Container scanning of [" ^ full_image_name ^ "]")
      ~stage:Stages.test
      ~image:Images.trivy
      ~dependencies:(Dependent [])
      ~artifacts:(artifacts [report])
      ~cache:[cache ~key:"trivy" [".trivycache/"]]
      ~variables:
        [
          ("TRIVY_NO_PROGRESS", "true");
          ("TRIVY_CACHE_DIR", ".trivycache/");
          ("FULL_IMAGE_NAME", full_image_name);
          ("REPORT", report);
        ]
      [". ./scripts/ci/container_scanning_generate_reports.sh"]
  in

  Common.job_datadog_pipeline_trace
  :: List.map job_container_scanning image_tags
