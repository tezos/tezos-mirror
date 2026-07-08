(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs related to security scans.
   - Vulnerability detection in Docker images. *)

open Gitlab_ci.Util
module Images = Tezos_ci.Images
module CI = Cacio.Shared

type docker_image = {
  name : string; (* the name of the image like tezos/tezos *)
  tag : string; (* the tag like master *)
  dockerfile : string;
      (* the docker file associated to this image like build.dockerfile *)
  job_name : string;
      (* the unique friendly job name associate to the scanning job *)
}

let image_ref image = image.name ^ ":" ^ image.tag

let build_images : docker_image list =
  List.map
    (fun tag ->
      {
        name = "tezos/tezos";
        tag;
        dockerfile = "build.Dockerfile";
        job_name = "tezos-tezos-" ^ tag;
      })
    ["latest"; "octez-evm-node-latest"; "master"]

let base_images : docker_image list =
  List.map
    (fun release ->
      {
        name = "${GCP_PROTECTED_REGISTRY}/tezos/tezos/debian";
        tag =
          Images.Base_images.(Format.asprintf "%s-%s" release base_images_tag);
        dockerfile = "images/base-images/Dockerfile.debian";
        job_name =
          Images.Base_images.(
            Format.asprintf "tezos-debian-%s-%s" release base_images_tag);
      })
    (Base_images.Distribution.releases Debian)
  @ List.map
      (fun release ->
        {
          name = "${GCP_PROTECTED_REGISTRY}/tezos/tezos/ubuntu";
          tag =
            Images.Base_images.(Format.asprintf "%s-%s" release base_images_tag);
          dockerfile = "images/base-images/Dockerfile.debian";
          job_name =
            Images.Base_images.(
              Format.asprintf "tezos-ubuntu-%s-%s" release base_images_tag);
        })
      (Base_images.Distribution.releases Ubuntu)

(* Scans [docker_image:docker_tag] image. A scanning report artifact is produced.
   The source image is pulled, pushed to GCP Artifact Registry at
   [us-central1-docker.pkg.dev/nl-gitlab-runner/image-scanning], and
   scanned via the GCP AR On-Demand Scanning API.

   This is a parameterized job family: each unique [image] argument yields a
   distinct CI job. [Cacio.parameterize] ensures that calling this function
   multiple times with the same argument returns the same Cacio job object,
   which is required for correct [~needs] references (e.g. from
   [job_container_scanning_slack_notification] and from
   [job_container_scanning_merge_reports]). *)
let job_container_scanning =
  Cacio.parameterize @@ fun image ->
  let full_image_name = image_ref image in
  let report = "gl-container-scanning-report-" ^ image.job_name ^ ".json" in
  CI.job
    ("container_scanning_" ^ image.job_name)
    ~__POS__
    ~description:("Container scanning of [" ^ full_image_name ^ "]")
    ~stage:Test
    ~image:Images.Base_images.alpine_docker_ci
    ~services:[{name = Images.Base_images.dind_service}]
    ~artifacts:(artifacts [report])
    ~variables:[("FULL_IMAGE_NAME", full_image_name); ("REPORT", report)]
    ~script:
      [
        (* Wait for the Docker-in-Docker daemon to be ready before the
           script issues [docker pull]. *)
        ". ./scripts/ci/docker_wait_for_daemon.sh";
        ". ./scripts/ci/container_scanning_generate_reports_with_gcp.sh";
      ]

(* For each image listed in [build_images], emits a dedicated Slack notification
   job that reports the per-image scan result.
   By calling [job_container_scanning image] internally, this function
   automatically depends on the correct scanning job for the given image. *)
let job_container_scanning_slack_notification =
  Cacio.parameterize @@ fun image ->
  let full_image_name = image_ref image in
  let report = "gl-container-scanning-report-" ^ image.job_name ^ ".json" in
  CI.job
    ("container_scanning_slack_notification_" ^ image.job_name)
    ~__POS__
    ~description:
      ("Report on Slack the results of the scan for [" ^ full_image_name ^ "]")
    ~stage:Test
    ~image:Images.CI.monitoring
    ~needs:[(Cacio.Artifacts, job_container_scanning image)]
    ~variables:[("REPORT", report)]
    ~script:
      [
        ". ./scripts/ci/container_scanning_slack_notification.sh "
        ^ full_image_name;
      ]

(* Merges reports from individual image scans into a single report
   compatible with Gitlab's vulnerability report.
   https://gitlab.com/tezos/tezos/-/security/vulnerability_report *)
let all_scanning_jobs =
  List.map job_container_scanning (build_images @ base_images)

let job_container_scanning_merge_reports =
  let jq_slurp_filter =
    "'{ version: .[0].version, scan: .[0].scan, vulnerabilities: \
     map((.vulnerabilities // [])[]), remediations: map((.remediations // \
     [])[])}'"
  in
  CI.job
    "container_scanning_merge_reports"
    ~__POS__
    ~description:
      "Merge container scanning reports in a single one fitted for Gitlab \
       Vulnerability report"
    ~stage:Test
    ~image:Images.CI.monitoring
    ~needs:(List.map (fun j -> (Cacio.Artifacts, j)) all_scanning_jobs)
    ~artifacts:
      (artifacts
         ~reports:
           (reports ~container_scanning:"gl-container-scanning-report.json" ())
         ["gl-container-scanning-report.json"])
    ~script:
      [
        (* Merge of all container scanning reports: the vulnerability
           arrays are merged into a single one. *)
        "jq --slurp " ^ jq_slurp_filter
        ^ " gl-container-scanning-report-* > gl-container-scanning-report.json";
      ]

let slack_jobs = List.map job_container_scanning_slack_notification build_images

(* We only explicitly register the reporting jobs.
   Cacio pulls in the scan jobs automatically via the transitive closure
   of the dependency graph. This reflects the intent: the scans are an
   implementation detail of producing the reports. *)
let register () =
  Cacio.register_jobs
    Schedule_security_scans
    (List.map
       (fun j -> (Cacio.Auto, j))
       (job_container_scanning_merge_reports :: slack_jobs))
