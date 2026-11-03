(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Docker build and related jobs.

    They are registered when this module is initialized,
    i.e. when this library is linked. *)

(** The version of Docker to use. *)
val version : string

(** The [CI_DOCKER_HUB] variable for jobs that handle Docker images
    built in a given mode (see {!job_docker}). *)
val docker_hub_variable : [`docker_hub | `test | `release] -> string * string

(** The environment for jobs that handle Docker images
    built in a given mode (see {!job_docker}). *)
val docker_publish_environment :
  [`docker_hub | `test | `release] -> Gitlab_ci.Types.environment option

(** Jobs [oc.docker:$ARCH].

    The mode specifies where images are pushed:
    - [`docker_hub]: to Docker Hub on protected refs of tezos/tezos,
      to the GCP registry otherwise;
    - [`test]: to the GCP registry, signed with the dev-only signer;
    - [`release]: to the GCP registry, signed with the production signer
      on protected refs, so that they can be copied to Docker Hub. *)
val job_docker :
  [`released | `experimental] ->
  [`docker_hub | `test | `release] ->
  Tezos_ci.Runner.Arch.t ->
  Cacio.job

(** Job [docker:merge_manifests].

    See {!job_docker} for the meaning of the mode. *)
val job_docker_merge_manifests :
  [`released | `experimental] -> [`docker_hub | `test | `release] -> Cacio.job

(** Job [docker:mirror_to_gitlab_registry]: copies the tags published by
    [docker:merge_manifests] into the project's GitLab container registry.
    Should be set to [Manual], and must run after that job. *)
val job_docker_mirror_to_gitlab_registry :
  [`released | `experimental] -> [`docker_hub | `test | `release] -> Cacio.job
