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
  let build_images =
    List.map
      (fun tag ->
        {
          Container_scanning.name = "tezos/tezos";
          tag;
          dockerfile = "build.Dockerfile";
          job_name = "tezos-tezos-" ^ tag;
        })
      ["latest"; "octez-evm-node-latest"; "master"]
  in

  let base_images =
    List.map
      (fun release ->
        {
          Container_scanning.name =
            "${GCP_PROTECTED_REGISTRY}/tezos/tezos/debian";
          tag =
            Images.Base_images.(Format.asprintf "%s-%s" release debian_version);
          dockerfile = "images/base-images/Dockerfile.debian";
          job_name =
            Images.Base_images.(
              Format.asprintf "tezos-debian-%s-%s" release debian_version);
        })
      (Base_images.Distribution.releases Debian)
    @ List.map
        (fun release ->
          {
            Container_scanning.name =
              "${GCP_PROTECTED_REGISTRY}/tezos/tezos/ubuntu";
            tag =
              Images.Base_images.(
                Format.asprintf "%s-%s" release debian_version);
            dockerfile = "images/base-images/Dockerfile.debian";
            job_name =
              Images.Base_images.(
                Format.asprintf "tezos-ubuntu-%s-%s" release debian_version);
          })
        (Base_images.Distribution.releases Ubuntu)
    @
    if Base_images.enable_rpm_images then
      List.map
        (fun release ->
          {
            Container_scanning.name =
              "${GCP_PROTECTED_REGISTRY}/tezos/tezos/fedora";
            tag =
              Images.Base_images.(Format.asprintf "%s-%s" release rpm_version);
            dockerfile = "images/base-images/Dockerfile.rpm";
            job_name =
              Images.Base_images.(
                Format.asprintf "tezos-fedora-%s-%s" release rpm_version);
          })
        (Base_images.Distribution.releases Fedora)
      @ List.map
          (fun release ->
            {
              Container_scanning.name =
                "${GCP_PROTECTED_REGISTRY}/tezos/tezos/rockylinux";
              tag =
                Images.Base_images.(Format.asprintf "%s-%s" release rpm_version);
              dockerfile = "images/base-images/Dockerfile.rpm";
              job_name =
                Images.Base_images.(
                  Format.asprintf "tezos-rockylinux-%s-%s" release rpm_version);
            })
          (Base_images.Distribution.releases Rockylinux)
    else []
  in
  (* Scans [docker_image:docker_tag] image. A scanning report artifact
     is produced.
     The source image is pulled, pushed to GCP Artifact Registry at
     [us-central1-docker.pkg.dev/nl-gitlab-runner/image-scanning], and
     scanned via the GCP AR On-Demand Scanning API. *)
  let job_container_scanning image : tezos_job =
    let full_image_name = Container_scanning.(image_ref image) in
    let report = "gl-container-scanning-report-" ^ image.job_name ^ ".json" in
    job
      ~__POS__
      ~name:("container_scanning_" ^ image.job_name)
      ~description:("Container scanning of [" ^ full_image_name ^ "]")
      ~stage:Stages.test
      ~image:Images_external.docker
      ~dependencies:(Dependent [])
      ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
      ~artifacts:(artifacts [report])
      ~variables:
        [
          ("DOCKER_VERSION", "24.0.7");
          ("FULL_IMAGE_NAME", full_image_name);
          ("REPORT", report);
        ]
      [". ./scripts/ci/container_scanning_generate_reports_with_gcp.sh"]
  in

  let job_list_container_scanning =
    List.map job_container_scanning (build_images @ base_images)
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
           ["gl-container-scanning-report.json"])
      ((* Merge of all container scanning reports: the vulnerability
          arrays are merged into a single one. *)
       let jq_slurp_filter =
         "'{ version: .[0].version, scan: .[0].scan, vulnerabilities: \
          map((.vulnerabilities // [])[]), remediations: map((.remediations // \
          [])[])}'"
       in
       (* String of all individual image scan reports *)
       [
         "jq --slurp " ^ jq_slurp_filter ^ " gl-container-scanning-report-*"
         ^ "> gl-container-scanning-report.json";
       ])
  in

  (* For each image listed here, emit a dedicated Slack notification job
     that reports the per-image scan result. *)
  let job_container_scanning_slack_notification full_image_name : tezos_job =
    let image =
      match
        List.find_opt
          (fun img -> Container_scanning.image_ref img = full_image_name)
          build_images
      with
      | Some img -> img
      | None ->
          failwith
            (Printf.sprintf
               "[security_scans.ml] No build image matches %S. Slack \
                notification jobs can only be emitted for images declared in \
                [build_images]."
               full_image_name)
    in
    let expected_job_name = "container_scanning_" ^ image.job_name in
    let scanning_job =
      match
        List.find_opt
          (fun j -> Tezos_ci.name_of_tezos_job j = expected_job_name)
          job_list_container_scanning
      with
      | Some j -> j
      | None ->
          failwith
            (Printf.sprintf
               "[security_scans.ml] No scanning job named %S found in \
                [job_list_container_scanning]."
               expected_job_name)
    in
    let report = "gl-container-scanning-report-" ^ image.job_name ^ ".json" in
    job
      ~__POS__
      ~name:("container_scanning_slack_notification_" ^ image.job_name)
      ~description:
        ("Report on Slack the results of the scan for [" ^ full_image_name ^ "]")
      ~stage:Stages.test
      ~image:Images.CI.monitoring
      ~dependencies:(Dependent [Artifacts scanning_job])
      ~variables:[("REPORT", report)]
      [
        ". ./scripts/ci/container_scanning_slack_notification.sh "
        ^ full_image_name;
      ]
  in
  (Tezos_ci.job_datadog_pipeline_trace :: job_list_container_scanning)
  @ job_container_scanning_merge_reports
    :: List.map
         job_container_scanning_slack_notification
         [
           "tezos/tezos:octez-evm-node-latest";
           "tezos/tezos:master";
           "tezos/tezos:latest";
         ]

let child_pipeline =
  Pipeline.register_child
    ~description:
      "A child pipeline of 'before_merging' to launch the security scans for \
       the images on the master branch"
    ~jobs:
      (if Tezos_ci.container_scanning_flag then jobs
       else [Tezos_ci.job_datadog_pipeline_trace])
    "security-scans-master"
