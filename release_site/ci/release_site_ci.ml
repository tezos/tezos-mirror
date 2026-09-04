(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

module CI = Cacio.Make (struct
  let name = "release_site"

  let paths = ["release_site/**/*"]
end)

let job_build =
  CI.job
    "build"
    ~__POS__
    ~stage:Build
    ~description:"Build the Release-page executable."
    ~image:Images.Base_images.alpine_release_page
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"release_page_exe"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         [
           "_build/default/release_site/src/version_manager.exe";
           "_build/default/release_site/src/release_page.exe";
         ])
    ~script:["eval $(opam env)"; "make -C release_site/ build"]

let job_build_tezt =
  CI.job
    "build_tezt"
    ~__POS__
    ~stage:Build
    ~description:"Build the Release-page Tezt executable."
    ~image:Images.Base_images.alpine_release_page
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"release_page_tezt_exe"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         ["_build/default/release_site/tezt/main.exe"])
    ~sccache:(Cacio.sccache ())
    ~script:["eval $(opam env)"; "dune build release_site/tezt/"]

let job_test =
  CI.tezt_job
    ""
    ~__POS__
    ~description:"Test the release page tools."
    ~pipeline:`merge_request
    ~needs:[(Artifacts, job_build); (Artifacts, job_build_tezt)]
    ~select_tezts:false
    ~tezt_exe:"release_site/tezt/main.exe"

let release_site_variables = function
  | `test ->
      (* The S3_BUCKET and DISTRIBUTION_ID depend on the release type (tests
         or not). *)
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

let job_render ?(needs = []) pipeline_type =
  CI.job
    "render"
    ~__POS__
    ~image:Images.Base_images.alpine_release_page
    ~stage:Publish
    ~environment:Gitlab_ci.Types.{name = "release-page"; action = Some Access}
    ~description:
      "Render the whole release site: regenerate every component's page from \
       its published versions.json and upload the site. Reflects what has been \
       deployed by the [deploy-assets] jobs, so it neither deploys assets nor \
       depends on any build job."
    ~needs
    ~variables:(release_site_variables pipeline_type)
    ~script:
      ["eval $(opam env)"; "./release_site/scripts/render_release_site.sh"]
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}

let register () =
  Cacio.register_merge_request_jobs [(Auto, job_test)] ;
  Cacio.register_jobs Schedule_extended_test [(Auto, job_test)] ;
  Cacio.register_jobs Publish_release_page [(Manual, job_render `real)] ;
  Cacio.register_jobs Test_publish_release_page [(Manual, job_render `test)]
