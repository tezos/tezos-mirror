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
    ~stage:Stages.publish
    ~interruptible:false
    ~dependencies:(Dependent [Artifacts (Common.job_build_grafazos ())])
    ~name:"gitlab:release"
    ["./grafazos/scripts/releases/create_gitlab_release.sh"]

let job_release_page ~test () =
  job
    ~__POS__
    ~image:Images.ci_release
    ~stage:Stages.publish_release
    ~description:
      "A job  to update the Grafazos release page. If running in a test \
       pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]. Then its [index.html] is updated \
       accordingly."
    ~name:"publish:release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~dependencies:(Dependent [Artifacts (Common.job_build_grafazos ())])
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
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
    ["./grafazos/scripts/releases/publish_release_page.sh"]

let jobs ~test () =
  (if test then [] else [job_datadog_pipeline_trace])
  @ [
      Common.job_build_grafazos (); job_gitlab_release; job_release_page ~test ();
    ]
