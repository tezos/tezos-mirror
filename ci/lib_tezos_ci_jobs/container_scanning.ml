(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the pipelines scanning Docker
   images for vulnerability detection.

   These pipelines run daily on the [master] branch. *)

open Gitlab_ci.Util
open Tezos_ci

type docker_image = {
  name : string; (* the name of the image like tezos/tezos *)
  tag : string; (* the tag like master *)
  dockerfile : string;
      (* the docker file associated to this image like build.dockerfile *)
  job_name : string;
      (* the unique friendly job name associate to the scanning job *)
}

let image_ref image = image.name ^ ":" ^ image.tag

(* Scans a Docker image to detect CVEs. Based on a template provided by Gitlab. *)
let job_container_scanning image : tezos_job =
  job
    ~__POS__
    ~name:"container_scanning"
    ~stage:Stages.test
    ~image:Images.trivy
    ~variables:
      [
        ("TRIVY_CACHE_DIR", ".trivycache/");
        ("REPORT", "gl-container-scanning-report.json");
        ("FULL_IMAGE_NAME", image_ref image);
      ]
    ~description:(Format.sprintf "Scanning image %s" (image_ref image))
    ~cache:[cache ~key:"trivy" [".trivycache/"]]
    ~artifacts:(artifacts ["gl-container-scanning-report.json"])
    [". ./scripts/ci/container_scanning_generate_reports.sh"]

let job_container_scanning_slack_notification image : tezos_job =
  job
    ~__POS__
    ~name:"container_scanning_slack_notification"
    ~description:"Report on Slack the results of the scan"
    ~dependencies:(Dependent [Artifacts (job_container_scanning image)])
    ~stage:Stages.test
    ~image:Images.CI.monitoring
    [
      ". ./scripts/ci/container_scanning_slack_notification.sh "
      ^ image_ref image;
    ]

let jobs image : tezos_job list =
  [
    Tezos_ci.job_datadog_pipeline_trace;
    job_container_scanning image;
    job_container_scanning_slack_notification image;
  ]
