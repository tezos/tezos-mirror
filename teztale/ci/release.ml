(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

let job_datadog_pipeline_trace : tezos_job =
  job
    ~__POS__
    ~allow_failure:Yes
    ~name:"datadog_pipeline_trace"
    ~image:Images.datadog_ci
    ~before_script:[". ./scripts/ci/datadog_send_job_info.sh"]
    ~stage:Stages.start
    [
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

let job_gitlab_release =
  job
    ~__POS__
    ~image:Images.ci_release
    ~stage:Stages.publish_release_gitlab
    ~interruptible:false
    ~dependencies:
      (Dependent
         [
           Artifacts (Common.job_build ~arch:Amd64 ());
           Artifacts (Common.job_build ~arch:Arm64 ());
         ])
    ~name:"gitlab:release"
    ["./teztale/scripts/releases/create_gitlab_release.sh"]

let jobs ~test () =
  (if test then [] else [job_datadog_pipeline_trace])
  @ [
      Common.job_build ~expire_in:Never ~arch:Amd64 ();
      Common.job_build ~expire_in:Never ~arch:Arm64 ();
      job_gitlab_release;
    ]
