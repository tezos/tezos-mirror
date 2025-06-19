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

let register () =
  CI.register_before_merging_jobs [(Auto, job_build Test)] ;
  ()
