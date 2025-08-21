(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Common

let job_gitlab_release =
  job
    ~__POS__
    ~image:Images.ci_release
    ~stage:Stages.publish
    ~interruptible:false
    ~dependencies:(Dependent [Artifacts (Common.job_build ())])
    ~name:"gitlab:release"
    ["./grafazos/scripts/releases/create_gitlab_release.sh"]

let job_release_page ~test () =
  job
    ~__POS__
    ~image:Images.ci_release
    ~stage:Stages.publish
    ~description:
      "A job  to update the Grafazos release page. If running in a test \
       pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]. Then its [index.html] is updated \
       accordingly."
    ~name:"publish:release-page"
    ~rules:[Gitlab_ci.Util.job_rule ~when_:Manual ()]
    ~dependencies:(Dependent [Artifacts (Common.job_build ())])
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

let jobs ~test ?(dry_run = false) () =
  (* If the release is a dry run, we do not publish a gitlab release page. *)
  (if dry_run then [] else [job_gitlab_release])
  @ [Common.job_build (); job_release_page ~test ()]
