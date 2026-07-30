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
    ~image:Images.CI.release_page
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
    ~image:Images.CI.release_page
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

let register () =
  Cacio.register_merge_request_jobs [(Auto, job_test)] ;
  Cacio.register_jobs Schedule_extended_test [(Auto, job_test)]
