(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Util
open Tezos_ci
module String_set = Set.Make (String)

(** Prefix used for the jobs name. *)
let prefix = "grafazos"

let job ~name = job ~name:(prefix ^ "." ^ name)

(** Set of Grafazos files *)
let changeset_grafazos = Changeset.(make ["grafazos/**/*"])

(** Job that builds the Grafazos dashboards *)
let job_build ?rules () =
  job
    ~__POS__
    ~name:"build_dashboards"
    ~image:Images.jsonnet
    ~stage:Stages.build
    ?rules
    ~artifacts:
      (artifacts
         ~name:"grafazos-dashboards"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         ["grafazos/output/**/*.json"])
    ~before_script:
      [
        "cd grafazos/";
        (* For security, we explicitly install v11.1.0
           which corresponds to commit [1ce5aec]. *)
        "jb install github.com/grafana/grafonnet/gen/grafonnet-v11.1.0@1ce5aec";
      ]
    ["make"]
