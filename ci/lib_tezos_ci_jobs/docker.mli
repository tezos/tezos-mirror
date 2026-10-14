(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The version of Docker to use. *)
val version : string

(** Jobs [oc.docker:$ARCH]. *)
val job_docker :
  [`released | `experimental] ->
  [`real | `test] ->
  Tezos_ci.Runner.Arch.t ->
  Cacio.job

(** Job [docker:merge_manifests]. *)
val job_docker_merge_manifests :
  [`released | `experimental] -> [`real | `test] -> Cacio.job

(** Job [docker:mirror_to_gitlab_registry]: copies the tags published by
    [docker:merge_manifests] into the project's GitLab container registry.
    Should be set to [Manual], and must run after that job. *)
val job_docker_mirror_to_gitlab_registry :
  [`released | `experimental] -> [`real | `test] -> Cacio.job

(** Register Docker build and related jobs. *)
val register : unit -> unit
