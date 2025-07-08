(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Will be removed later in this MR. *)
module Common = Common
module Release = Release

module CI = Cacio.Make (struct
  let name = "teztale"

  let paths = ["teztale/**/*"]
end)

let job_build =
  Cacio.parameterize @@ fun mode ->
  Cacio.parameterize @@ fun arch ->
  let arch_string = Tezos_ci.arch_to_string arch in
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
    ~sccache:(Cacio.sccache ~cache_size:"2G" ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make teztale";
      "mkdir -p ./teztale-binaries/" ^ arch_string;
      "mv octez-teztale-* ./teztale-binaries/" ^ arch_string ^ "/";
    ]

let register () =
  CI.register_before_merging_jobs
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Teztale."
    [(Auto, job_build `test Amd64); (Auto, job_build `test Arm64)] ;
  ()
