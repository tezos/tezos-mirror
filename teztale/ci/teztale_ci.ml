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
    ~storage:Ramfs
    ~image:Tezos_ci.Images.CI.build
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
    ~image:Tezos_ci.Images.ci_release
    ~stage:Publish
    ~description:"Create a GitLab release for Teztale."
    ~needs:
      [
        (Artifacts, job_build `release Amd64);
        (Artifacts, job_build `release Arm64);
      ]
    ["./teztale/scripts/releases/create_gitlab_release.sh"]

let job_release_page =
  Cacio.parameterize @@ fun pipeline_type ->
  Cacio.parameterize @@ fun needs ->
  CI.job
    "release_page"
    ~__POS__
    ~image:Tezos_ci.Images.CI.build
    ~stage:Publish
    ~description:
      "Update the Teztale release page. If running in a test pipeline, the \
       assets are pushed in the [release-page-test.nomadic-labs.com] bucket. \
       Otherwise they are pushed in [site.prod.octez.tezos.com]. Then its \
       [index.html] is updated accordingly."
    ~needs:
      (match needs with
      | `build_dependencies ->
          [
            (Artifacts, job_build `release Amd64);
            (Artifacts, job_build `release Arm64);
          ]
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
      "./teztale/scripts/releases/publish_release_page.sh";
    ]

let register () =
  CI.register_merge_request_jobs
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Teztale."
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  CI.register_global_release_jobs
    [(Manual, job_release_page `real `build_dependencies)] ;
  CI.register_global_test_release_jobs
    [(Manual, job_release_page `test `build_dependencies)] ;
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
  CI.register_global_scheduled_test_release_jobs
    [
      (* Explicitly include the build jobs so that they have trigger [Auto]. *)
      (Auto, job_build `release Amd64);
      (Auto, job_build `release Arm64);
      (Manual, job_release_page `test `build_dependencies);
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
