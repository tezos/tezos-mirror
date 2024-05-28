(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [latest_release] pipeline.

   This pipeline runs on each push to the [latest_release] branch. The
   goal of this pipeline to update the [latest] Docker tag of the
   Octez Docker distribution. *)

open Tezos_ci
open Common

let job_docker_promote_to_latest ~ci_docker_hub : tezos_job =
  job_docker_authenticated
    ~__POS__
    ~stage:Stages.publish_release
    ~name:"docker:promote_to_latest"
    ~ci_docker_hub
    ["./scripts/ci/docker_promote_to_latest.sh"]

let jobs ?(test = false) () =
  [job_docker_promote_to_latest ~ci_docker_hub:(not test)]
