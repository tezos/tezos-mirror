(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [job_deploy_release_page_assets pipeline_type] deploys Teztale's release
    assets and [versions.json]. *)
val job_deploy_release_page_assets : [`real | `test] -> Cacio.job

(** Register Teztale jobs and pipelines. *)
val register : unit -> unit
