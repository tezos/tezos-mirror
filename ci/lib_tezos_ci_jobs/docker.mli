(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The version of Docker to use. *)
val version : string

(** [docker_release.sh] invocation stating the CI images the Docker
    distribution jobs build FROM, passed explicitly as full references. The
    tags are resolved at runtime by the job's shell from the
    [Images.CI.runtime] dependency dotenv, exactly as the jobs' [image:]
    fields already reference them. *)
val docker_release_script : string

(** Jobs [oc.docker:$ARCH]. *)
val job_docker :
  [`released | `experimental | `experimental_with_evm] ->
  [`real | `test] ->
  Tezos_ci.Runner.Arch.t ->
  Cacio.job

(** Job [docker:merge_manifests]. *)
val job_docker_merge_manifests :
  [`released | `experimental | `experimental_with_evm] ->
  [`real | `test] ->
  Cacio.job

(** Register Docker build and related jobs. *)
val register : unit -> unit
