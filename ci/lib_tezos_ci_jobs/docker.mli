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
