(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "teztale"

  let paths = ["teztale/**/*"]
end)

let job_build =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun arch ->
  let arch_string = Tezos_ci.Runner.Arch.show_easy_to_distinguish arch in
  CI.job
    ("build-" ^ arch_string)
    ~__POS__
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ~image:Tezos_ci.Images.Base_images.alpine_build
    ~stage:(match mode with `test -> Test | `release -> Build)
    ~description:"Build Teztale."
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"teztale-binaries"
         ~expire_in:
           (match mode with
           | `test -> Gitlab_ci.Types.(Duration (Days 1))
           | `release -> Gitlab_ci.Types.Never)
         ~when_:On_success
         ["teztale-binaries/" ^ arch_string ^ "/octez-teztale-*"])
    ~variables:[("PROFILE", "static")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    ~script:
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
        "make teztale";
        "mkdir -p ./teztale-binaries/" ^ arch_string;
        "mv octez-teztale-* ./teztale-binaries/" ^ arch_string ^ "/";
      ]

let job_gitlab_release =
  CI.job
    "gitlab_release"
    ~__POS__
    ~image:Tezos_ci.Images.Base_images.ci_release
    ~stage:Publish
    ~description:"Create a GitLab release for Teztale."
    ~needs:
      [
        (Artifacts, job_build `release Amd64);
        (Artifacts, job_build `release Arm64);
      ]
    ~script:["./teztale/scripts/releases/create_gitlab_release.sh"]

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
    ~image:Tezos_ci.Images.Base_images.alpine_release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Deploy the Teztale release assets and versions.json. If running in a \
       test pipeline, the assets are pushed in the \
       [release-page-test.nomadic-labs.com] bucket. Otherwise they are pushed \
       in [site.prod.octez.tezos.com]."
    ~needs:
      [
        (Artifacts, job_build `release Amd64);
        (Artifacts, job_build `release Arm64);
      ]
    ~variables:(release_page_variables pipeline_type)
    ~script:
      [
        "eval $(opam env)";
        "./teztale/scripts/releases/deploy_release_page_assets.sh";
      ]

let register () =
  Cacio.register_merge_request_jobs
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Teztale."
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  Cacio.register_release_jobs [(Manual, job_deploy_release_page_assets `real)] ;
  Cacio.register_test_release_jobs
    [(Manual, job_deploy_release_page_assets `test)] ;
  Cacio.register_jobs
    Non_release_tag
    [(Auto, job_build `release Amd64); (Auto, job_build `release Arm64)] ;
  Cacio.register_jobs
    Non_release_tag_test
    [(Auto, job_build `release Amd64); (Auto, job_build `release Arm64)] ;
  Cacio.register_jobs
    Tezos_ci_pipelines.schedule_test_release
    [
      (* The build jobs run to exercise them during the scheduled test release.
         No manual job is registered here: it is not relevant to have manual
         jobs in a pipeline that is triggered automatically on a schedule. The
         release page is tested using test release tags instead. *)
      (Auto, job_build `release Amd64);
      (Auto, job_build `release Arm64);
    ] ;
  CI.register_dedicated_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `real);
      (* Re-render the whole site once Teztale's assets are deployed. *)
      ( Auto,
        Release_site_ci.job_render
          `real
          ~needs:[(Job, job_deploy_release_page_assets `real)] );
    ] ;
  CI.register_dedicated_test_release_pipeline
    [
      (Auto, job_gitlab_release);
      (Manual, job_deploy_release_page_assets `test);
      ( Auto,
        Release_site_ci.job_render
          `test
          ~needs:[(Job, job_deploy_release_page_assets `test)] );
    ] ;
  ()
