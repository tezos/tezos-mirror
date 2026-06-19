(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "grafazos"

  let paths = ["grafazos/**/*"]
end)

let job_build =
  Cacio.parameterize @@ fun stage ->
  CI.job
    "build"
    ~__POS__
    ~image:Tezos_ci.Images.Base_images.debian_jsonnet_trixie
    ~stage
    ~description:"Build the Grafazos dashboards."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"grafazos-dashboards"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         ["grafazos/output/**/*.json"])
    ~script:
      [
        "cd grafazos/";
        (* For security, we explicitly install v11.1.0
           which corresponds to commit [1ce5aec]. *)
        "jb install \
         github.com/grafana/grafonnet/gen/grafonnet-v11.1.0@1ce5aec95ce32336fe47c8881361847c475b5254";
        "make";
      ]

let job_gitlab_release =
  CI.job
    "gitlab_release"
    ~__POS__
    ~image:Tezos_ci.Images.Base_images.ci_release
    ~stage:Publish
    ~description:"Create a GitLab release for Grafazos."
    ~needs:[(Artifacts, job_build Build)]
    ~script:["./grafazos/scripts/releases/create_gitlab_release.sh"]

let release_page_variables = function
  | `test ->
      [
        ("S3_BUCKET", "release-page-test.nomadic-labs.com");
        ("DISTRIBUTION_ID", "E19JF46UG3Z747");
      ]
  | `real ->
      [
        ("S3_BUCKET", "site-prod.octez.tezos.com");
        ("URL", "octez.tezos.com");
        ("BUCKET_PATH", "/releases");
        ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
      ]

let job_deploy_release_page_assets =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "release-page-deploy-assets"
    ~__POS__
    ~image:Tezos_ci.Images.CI.release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Deploy the Grafazos release assets and versions.json. If running in a \
       test pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]."
    ~needs:[(Artifacts, job_build Build)]
    ~variables:(release_page_variables pipeline_type)
    ~script:
      [
        "eval $(opam env)";
        "./grafazos/scripts/releases/deploy_release_page_assets.sh";
      ]

let job_release_page =
  Cacio.parameterize @@ fun pipeline_type ->
  Cacio.parameterize @@ fun wait_for ->
  CI.job
    "release-page-publish"
    ~__POS__
    ~image:Tezos_ci.Images.CI.release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Publish the Grafazos release page: regenerate [index.html] from the \
       published versions.json and upload it. Reflects what has been deployed \
       by [grafazos.release-page-deploy-assets]."
    ~needs:
      (match wait_for with
      | `wait_for_nothing -> []
      | `wait_for_deploy ->
          [(Job, job_deploy_release_page_assets pipeline_type)])
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
    ~variables:(release_page_variables pipeline_type)
    ~script:
      [
        "eval $(opam env)"; "./grafazos/scripts/releases/publish_release_page.sh";
      ]

let register () =
  Cacio.register_merge_request_jobs [(Auto, job_build Test)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Grafazos."
    [(Auto, job_build Test)] ;
  Cacio.register_release_jobs
    [
      (Manual, job_deploy_release_page_assets `real);
      (Auto, job_release_page `real `wait_for_deploy);
    ] ;
  Cacio.register_test_release_jobs
    [
      (Manual, job_deploy_release_page_assets `test);
      (Auto, job_release_page `test `wait_for_deploy);
    ] ;
  Cacio.register_jobs Non_release_tag [(Auto, job_build Build)] ;
  Cacio.register_jobs Non_release_tag_test [(Auto, job_build Build)] ;
  Cacio.register_jobs
    Scheduled_test_release
    [
      (* The build job runs to exercise it during the scheduled test release.
         No manual job is registered here: it is not relevant to have manual
         jobs in a pipeline that is triggered automatically on a schedule. The
         release page is tested using test release tags instead. *)
      (Auto, job_build Build);
    ] ;
  Cacio.register_jobs
    Publish_release_page
    [
      ( Manual,
        (* [wait_for_nothing]: this pipeline only regenerates the page from the
           already-published versions.json, so it neither deploys assets nor
           depends on the build job. *)
        job_release_page `real `wait_for_nothing );
    ] ;
  Cacio.register_jobs
    Test_publish_release_page
    [
      ( Manual,
        (* [wait_for_nothing]: this pipeline only regenerates the page from the
           already-published versions.json, so it neither deploys assets nor
           depends on the build job. *)
        job_release_page `test `wait_for_nothing );
    ] ;
  CI.register_dedicated_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `real);
      (Auto, job_release_page `real `wait_for_deploy);
    ] ;
  CI.register_dedicated_test_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `test);
      (Auto, job_release_page `test `wait_for_deploy);
    ] ;
  ()
