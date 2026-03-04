(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

module CI = Cacio.Make (struct
  let name = "release_page"

  let paths = ["ci/bin_release_page/**/*"]
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
         ["_build/default/ci/bin_release_page/src/version_manager.exe"])
    ["eval $(opam env)"; "make -C ci/bin_release_page/ build"]

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
         ["_build/default/ci/bin_release_page/tezt/main.exe"])
    ~sccache:(Cacio.sccache ())
    ["eval $(opam env)"; "dune build ci/bin_release_page/tezt/"]

let job_test =
  CI.tezt_job
    ""
    ~__POS__
    ~description:"Test the release page tools."
    ~pipeline:`merge_request
    ~needs:[(Artifacts, job_build); (Artifacts, job_build_tezt)]
    ~select_tezts:false
    ~tezt_exe:"ci/bin_release_page/tezt/main.exe"

let register () = CI.register_merge_request_jobs [(Auto, job_test)]
