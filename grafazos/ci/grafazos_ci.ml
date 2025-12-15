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
    ~image:Tezos_ci.Images.jsonnet
    ~stage
    ~description:"Build the Grafazos dashboards."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"grafazos-dashboards"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         ["grafazos/output/**/*.json"])
    [
      "cd grafazos/";
      (* For security, we explicitly install v11.1.0
         which corresponds to commit [1ce5aec]. *)
      "jb install github.com/grafana/grafonnet/gen/grafonnet-v11.1.0@1ce5aec";
      "make";
    ]

let job_gitlab_release =
  CI.job
    "gitlab_release"
    ~__POS__
    ~image:Tezos_ci.Images.ci_release
    ~stage:Publish
    ~description:"Create a GitLab release for Grafazos."
    ~needs:[(Artifacts, job_build Build)]
    ["./grafazos/scripts/releases/create_gitlab_release.sh"]

let job_release_page =
  Cacio.parameterize @@ fun pipeline_type ->
  Cacio.parameterize @@ fun needs ->
  CI.job
    "release_page"
    ~__POS__
    ~image:Tezos_ci.Images.CI.build
    ~stage:Publish
    ~description:
      "Update the Grafazos release page. If running in a test pipeline, the \
       assets are pushed in the [release-page-test.nomadic-labs.com] bucket. \
       Otherwise they are pushed in [site.prod.octez.tezos.com]. Then its \
       [index.html] is updated accordingly."
    ~needs:
      (match needs with
      | `build_dependencies -> [(Artifacts, job_build Build)]
      | `no_build_dependencies -> [])
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["./index.md"; "index.html"])
    ~variables:
      (match pipeline_type with
      | `test ->
          [
            ("S3_BUCKET", "release-page-test.nomadic-labs.com");
            ("DISTRIBUTION_ID", "E19JF46UG3Z747");
            ("AWS_ACCESS_KEY_ID", "${AWS_KEY_RELEASE_PUBLISH}");
            ("AWS_SECRET_ACCESS_KEY", "${AWS_SECRET_RELEASE_PUBLISH}");
          ]
      | `real ->
          [
            ("S3_BUCKET", "site-prod.octez.tezos.com");
            ("URL", "octez.tezos.com");
            ("BUCKET_PATH", "/releases");
            ("DISTRIBUTION_ID", "${CLOUDFRONT_DISTRIBUTION_ID}");
          ])
    [
      "eval $(opam env)";
      "sudo apk add aws-cli pandoc";
      "./grafazos/scripts/releases/publish_release_page.sh";
    ]

let register () =
  CI.register_merge_request_jobs [(Auto, job_build Test)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Grafazos."
    [(Auto, job_build Test)] ;
  CI.register_global_release_jobs
    [(Manual, job_release_page `real `build_dependencies)] ;
  CI.register_global_test_release_jobs
    [(Manual, job_release_page `test `build_dependencies)] ;
  CI.register_global_scheduled_test_release_jobs
    [
      (* Explicitly include the build job so that it has trigger [Auto]. *)
      (Auto, job_build Build);
      (Manual, job_release_page `test `build_dependencies);
    ] ;
  CI.register_global_publish_release_page_jobs
    [
      ( Manual,
        (* [no_build_dependencies] because we don't want the build job to run
           as their artifacts are not needed to update the release page. *)
        job_release_page `real `no_build_dependencies );
    ] ;
  CI.register_global_test_publish_release_page_jobs
    [
      ( Manual,
        (* [no_build_dependencies] because we don't want the build job to run
           as their artifacts are not needed to update the release page. *)
        job_release_page `test `no_build_dependencies );
    ] ;
  CI.register_dedicated_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_release_page `real `build_dependencies);
    ] ;
  CI.register_dedicated_test_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_release_page `test `build_dependencies);
    ] ;
  ()
