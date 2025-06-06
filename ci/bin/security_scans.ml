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

  let job_list_container_scanning =
    List.map job_container_scanning image_tags
  in

  (* Merges reports from individual image scans into a single report
     compatible with Gitlab's vulnerability report.
     https://gitlab.com/tezos/tezos/-/security/vulnerability_report *)
  let job_container_scanning_merge_reports : tezos_job =
    job
      ~__POS__
      ~name:"container_scanning_merge_reports"
      ~description:
        "Merge container scanning reports in a single one fitted for Gitlab \
         Vulnerability report"
        (* needs the reports produced in previous [container_scanning] jobs *)
      ~dependencies:
        (Dependent
           (List.map (fun job -> Artifacts job) job_list_container_scanning))
      ~stage:Stages.test
      ~image:Images.CI.monitoring
      ~artifacts:
        (artifacts
           ~reports:
             (reports
                ~container_scanning:"gl-container-scanning-report.json"
                ())
           [])
      ((* Merge of all container scanning reports: the vulnerability
          arrays are merged into a single one. *)
       let jq_slurp_filter =
         "'{ version: .[0].version, scan: .[0].scan, vulnerabilities: \
          map(.vulnerabilities[]), remediations: map(.remediations[])}'"
       in
       (* String of all individual image scan reports *)
       let input_scan_reports =
         let concat_reports tag report_name : string =
           Format.sprintf " gl-container-scanning-report-"
           ^ tag ^ ".json" ^ report_name
         in
         List.fold_right concat_reports image_tags " "
       in
       [
         "jq --slurp " ^ jq_slurp_filter ^ input_scan_reports
         ^ "> gl-container-scanning-report.json";
       ])
  in

  (Common.job_datadog_pipeline_trace :: job_list_container_scanning)
  @ [job_container_scanning_merge_reports]
