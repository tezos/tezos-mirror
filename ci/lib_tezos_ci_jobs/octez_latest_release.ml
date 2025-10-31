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

let jobs ?(test = false) () =
  [
    (* Stage: start *)
    Tezos_ci.job_datadog_pipeline_trace;
    (* main job *)
    Common.Docker.job_docker_promote_to_latest ~ci_docker_hub:(not test) ();
  ]
